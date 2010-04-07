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
-module(script_instance).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([call/4, cast/4]).

%%---------------------------------------------------------------------------

start_link(CommandLine) ->
    gen_server:start_link(?MODULE, [CommandLine], []).

call(Pid, M, F, A) ->
    gen_server:call(Pid, {call, M, F, A}).

cast(Pid, M, F, A) ->
    gen_server:cast(Pid, {cast, M, F, A}).

%%---------------------------------------------------------------------------

-record(state, {port, fragments_rev, requests, next_request}).

send_obj(Id, Op, Payload, #state{port = Port}) ->
    port_command(Port,
                 [rfc4627:encode({obj, [{id, Id}, {op, Op}, {payload, Payload}]}), "\n"]),
    ok.

bad_script_message(Message, State) ->
    {stop, {bad_script_message, Message}, State}.

handle_msg(Message, State = #state{requests = OldReq}) ->
    Id = rfc4627:get_field(Message, "id", null),
    Op = rfc4627:get_field(Message, "op", null),
    Payload = rfc4627:get_field(Message, "payload", null),
    case Op of
        <<"reply">> ->
            case dict:find(Id, OldReq) of
                error ->
                    bad_script_message(Message, State);
                {ok, From} ->
                    gen_server:reply(From, {ok, Payload}),
                    {noreply, State#state{requests = dict:erase(Id, OldReq)}}
            end;
        <<"cast">> ->
            [MBin, FBin, A] = Payload,
            apply(list_to_atom(binary_to_list(MBin)), list_to_atom(binary_to_list(FBin)), A),
            {noreply, State};
        _Other ->
            bad_script_message(Message, State)
    end.

%%---------------------------------------------------------------------------

init([CommandLine]) ->
    ScriptDir = script_instance_manager:script_dir(),
    error_logger:info_report({starting_script, ScriptDir, CommandLine}),
    Port = open_port({spawn, CommandLine},
                     [{line, 1024}, {cd, ScriptDir}, use_stdio, eof]),
    {ok, #state{port = Port,
                fragments_rev = [],
                requests = dict:new(),
                next_request = 0}}.

handle_call({call, M, F, A}, From, State = #state{requests = OldReq,
                                                  next_request = RequestId}) ->
    ok = send_obj(RequestId, <<"call">>, [M, F, A], State),
    {noreply, State#state{requests = dict:store(RequestId, From, OldReq),
                          next_request = RequestId + 1}};
handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast({cast, M, F, A}, State) ->
    ok = send_obj(null, <<"cast">>, [M, F, A], State),
    {noreply, State};
handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info({Port, {data, Datum}}, State = #state{port = Port,
                                                  fragments_rev = OldFrag}) ->
    case Datum of
        {noeol, Fragment} ->
            {noreply, State#state{fragments_rev = [Fragment | OldFrag]}};
        {eol, Fragment} ->
            {ok, Message, []} =
                rfc4627:decode(lists:flatten(lists:reverse([Fragment | OldFrag]))),
            handle_msg(Message, State#state{fragments_rev = []})
    end;
handle_info({Port, eof}, State = #state{port = Port, requests = OldReq}) ->
    ok = dict:fold(fun (_ReqId, From, ok) ->
                           gen_server:reply(From, {error, eof}),
                           ok
                   end, ok, OldReq),
    {stop, normal, State#state{requests = dict:new()}};
handle_info(Message, State) ->
    {stop, {unhandled_info, Message}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
