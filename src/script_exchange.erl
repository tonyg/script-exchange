%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is script-exchange.
%%
%% The Initial Developers of the Original Code are Rabbit Technologies
%% Ltd and Tony Garnock-Jones.
%%
%% Portions created by Rabbit Technologies Ltd or by Tony Garnock-Jones
%% are Copyright (C) 2010 Rabbit Technologies Ltd and Tony Garnock-Jones.
%%
%% All Rights Reserved.
%%
%% Contributor(s): ______________________________________.
-module(script_exchange).
-include_lib("rfc4627_jsonrpc/include/rfc4627.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-define(EXCHANGE_TYPE_BIN, <<"x-script">>).

-rabbit_boot_step({?MODULE,
                   [{mfa, {rabbit_exchange_type_registry, register, [?EXCHANGE_TYPE_BIN, ?MODULE]}},
                    {requires, rabbit_exchange_type_registry},
                    {enables, exchange_recovery}]}).

-rabbit_boot_step({script_exchange_key_check,
                   [{mfa, {?MODULE, check_permitted_keys, []}},
                    {requires, ?MODULE}]}).

-behaviour(rabbit_exchange_type).

-export([description/0, publish/2]).
-export([validate/1, create/1, recover/2, delete/2, add_binding/2, remove_bindings/2]).

-export([check_permitted_keys/0]).

description() ->
    [{description, <<"Experimental Javascript exchange">>}].

check_permitted_keys() ->
    PermittedKeyIds = permitted_key_ids(),
    if
        PermittedKeyIds == [] ->
            exit({?MODULE, no_permitted_key_ids_configured});
        true ->
            ok
    end,
    case lists:filter(
           fun (PermittedKeyId) ->
                   Output = os:cmd("gpg --list-keys --with-colons " ++ PermittedKeyId),
                   case [L || "pub:" ++ L <- string:tokens(Output, "\n")] of
                       [] -> true;
                       [_, _ | _] -> true;
                       [_Line] -> false
                   end
           end, permitted_key_ids()) of
        [] ->
            error_logger:info_msg("script_exchange permitted_key_ids: ~p~n", [PermittedKeyIds]),
            ok;
        ProblemKeys ->
            %% Either they're unknown, or multiply-defined (!)
            exit({?MODULE, problematic_permitted_key_ids, ProblemKeys})
    end.

name_to_js(#resource{virtual_host = VHostBin, name = NameBin}) ->
    [VHostBin, NameBin].

%% table_to_js(Table) ->
%%     {obj, [table_field_to_js(Name, Type, Value) || {Name, Type, Value} <- Table]}.
%%
%% table_field_to_js(Name, table, Value) ->
%%     {Name, table_to_js(Value)};
%% table_field_to_js(Name, _Type, Value) ->
%%     {Name, Value}.

js_to_content(OldContent, NewProps, NewBody) ->
    OldContent#content{
      properties = ?RFC4627_TO_RECORD('P_basic', NewProps),
      properties_bin = none,
      payload_fragments_rev = [NewBody]}.

update_delivery_rk(Delivery = #delivery{message = Message}, RK) ->
    Delivery#delivery{message = Message#basic_message{routing_key = RK}}.

do_routing_action(#exchange{name = Name}, Delivery, <<"fanout">>) ->
    rabbit_router:deliver(rabbit_router:match_routing_key(Name, '_'),
                          update_delivery_rk(Delivery, <<>>));
do_routing_action(#exchange{name = Name}, Delivery, [<<"direct">>, RK]) ->
    rabbit_router:deliver(rabbit_router:match_routing_key(Name, RK),
                          update_delivery_rk(Delivery, RK));
do_routing_action(Exchange, Delivery, [<<"topic">>, RK]) ->
    rabbit_exchange_type_topic:publish(Exchange, update_delivery_rk(Delivery, RK));
do_routing_action(Exchange, _Delivery, Action) ->
    error_logger:error_report({bad_script_exchange_routing_action, Exchange, Action}),
    {unroutable, []}.

merge_pieces([]) ->
    %% TODO FIXME by exposing check_delivery or similar from rabbit_router
    {unroutable, []};
merge_pieces([{Result, Pids} | Pieces]) ->
    merge_pieces({Result, lists:usort(Pids)}, Pieces).

%% This is gross. Need to expose more fine-grained aspects of delivery and dedup
merge_pieces(Result, []) ->
    Result;
merge_pieces({OldResult, OldPids}, [{NewResult, NewPids} | Rest]) ->
    merge_pieces({choose_result(OldResult, NewResult),
                  lists:umerge(OldPids, lists:usort(NewPids))},
                 Rest).

choose_result(routed, _) -> routed; 
choose_result(_, routed) -> routed;
choose_result(V, _) -> V.

required_arg(Args, ArgNameBin, ArgTypeAtom) ->
    case lists:keysearch(ArgNameBin, 1, Args) of
        {value, {_, ArgTypeAtom, Value}} ->
            Value;
        {value, {_, _OtherTypeAtom, _}} ->
            rabbit_misc:protocol_error(not_allowed,
                                       "Required argument '~s' must be of type ~s",
                                       [ArgNameBin, ArgTypeAtom]);
        false ->
            rabbit_misc:protocol_error(not_allowed,
                                       "Required argument '~s' not present",
                                       [ArgNameBin])
    end.

script_manager_pid(#exchange{arguments = Args}) ->
    MimeTypeBin = required_arg(Args, <<"type">>, longstr),
    {ok, Pid} = script_manager_sup:lookup(MimeTypeBin),
    Pid.

with_temp_files(Contents, F) ->
    Pairs = [{"/tmp/"
              ++ atom_to_list(?MODULE) ++ "-"
              ++ atom_to_list(node()) ++ "-"
              ++ integer_to_list(erlang:phash2(make_ref()))
              ++ ".bin", C} || C <- Contents],
    [ok = file:write_file(N, C) || {N, C} <- Pairs],
    Result = F([N || {N, _} <- Pairs]),
    [ok = file:delete(N) || {N, _} <- Pairs],
    Result.

process_gpg_output(XName, PermittedKeyIds, FullOutput) ->
    error_logger:info_msg("GPG verification output for ~s:~n~s~n",
                          [rabbit_misc:rs(XName),
                           FullOutput]),
    case [L || "[GNUPG:] VALIDSIG " ++ L <- string:tokens(FullOutput, "\n")] of
        [] ->
            rabbit_misc:protocol_error(not_allowed,
                                       "No valid signatures found",
                                       []);
        [Line] ->
            [FullKeyId | _] = string:tokens(Line, " "),
            case lists:any(fun (PermittedKeyId) ->
                                   string:right(FullKeyId, length(PermittedKeyId))
                                       == PermittedKeyId
                           end, PermittedKeyIds) of
                true ->
                    ok;
                false ->
                    rabbit_misc:protocol_error(not_allowed,
                                               "Valid signature found, but using unauthorised key",
                                               [])
            end
    end.

verify_signature(XName, ContentBin, DetachedSigBin, PermittedKeyIds) ->
    with_temp_files([ContentBin, DetachedSigBin],
                    fun ([ContentFilename, SigFilename]) ->
                            process_gpg_output(XName,
                                               PermittedKeyIds,
                                               os:cmd("gpg --verify --status-fd 1 "
                                                      ++ "\"" ++ SigFilename ++ "\" \""
                                                      ++ ContentFilename ++ "\""))
                    end).

permitted_key_ids() ->
    {ok, PermittedKeyIds} = application:get_env(rabbit_script_exchange, permitted_key_ids),
    PermittedKeyIds.

publish(Exchange = #exchange{name = Name},
        Delivery = #delivery{message = Message0 = #basic_message{
                               routing_key = RK,
                               content = Content0
                              }}) ->
    #content{
         properties = Properties,
         payload_fragments_rev = PayloadRev
        } = rabbit_binary_parser:ensure_content_decoded(Content0),
    case script_instance_manager:call(
           script_manager_pid(Exchange), <<"Exchange">>, <<"publish">>,
           [name_to_js(Name),
            RK,
            ?RFC4627_FROM_RECORD('P_basic', Properties),
            list_to_binary(lists:reverse(PayloadRev))]) of
        {ok, [NewProps, NewBody, Actions]} ->
            NewContent = js_to_content(Content0, NewProps, NewBody),
            NewDelivery = Delivery#delivery{message = Message0#basic_message{content = NewContent}},
            merge_pieces([do_routing_action(Exchange, NewDelivery, Action)
                          || Action <- Actions]);
        Other ->
            error_logger:error_report({bad_reply_from_script_exchange_publish, Other}),
            %% TODO FIXME do something more sensible here
            []
    end.

validate(X = #exchange{name = Name, arguments = Args}) ->
    DefinitionBin = required_arg(Args, <<"definition">>, longstr),
    DetachedSigBin = required_arg(Args, <<"signature">>, longstr),
    ok = verify_signature(Name, DefinitionBin, DetachedSigBin, permitted_key_ids()),
    {ok, true} = script_instance_manager:call(script_manager_pid(X), <<"Exchange">>, <<"validate">>,
                                              [name_to_js(Name), DefinitionBin]),
    ok.

create(X = #exchange{name = Name, arguments = Args}) ->
    DefinitionBin = required_arg(Args, <<"definition">>, longstr),
    {ok, _} = script_instance_manager:call(script_manager_pid(X), <<"Exchange">>, <<"create">>,
                                           [name_to_js(Name), DefinitionBin]),
    ok.

recover(X, _Bs) ->
    create(X).

delete(_X, _Bs) ->
    ok.

add_binding(_X, _B) ->
    ok.

remove_bindings(_X, _Bs) ->
    ok.
