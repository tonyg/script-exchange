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

-behaviour(rabbit_exchange_type).

-export([description/0, publish/2]).
-export([validate/1, create/1, recover/2, delete/2, add_binding/2, remove_bindings/2]).

description() ->
    [{description, <<"Experimental Javascript exchange">>}].

name_to_js(#resource{virtual_host = VHostBin, name = NameBin}) ->
    [VHostBin, NameBin].

table_to_js(Table) ->
    {obj, [table_field_to_js(Name, Type, Value) || {Name, Type, Value} <- Table]}.

table_field_to_js(Name, table, Value) ->
    {Name, table_to_js(Value)};
table_field_to_js(Name, _Type, Value) ->
    {Name, Value}.

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

script_manager_pid(#exchange{arguments = Args}) ->
    {value, {_, _, MimeTypeBin}} = lists:keysearch(<<"type">>, 1, Args),
    {ok, Pid} = script_manager_sup:lookup(MimeTypeBin),
    Pid.

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
    {ok, true} = script_instance_manager:call(script_manager_pid(X), <<"Exchange">>, <<"validate">>,
                                              [name_to_js(Name), table_to_js(Args)]),
    ok.

create(X = #exchange{name = Name, arguments = Args}) ->
    {ok, _} = script_instance_manager:call(script_manager_pid(X), <<"Exchange">>, <<"create">>,
                                           [name_to_js(Name), table_to_js(Args)]),
    ok.

recover(X, _Bs) ->
    create(X).

delete(_X, _Bs) ->
    ok.

add_binding(_X, _B) ->
    ok.

remove_bindings(_X, _Bs) ->
    ok.
