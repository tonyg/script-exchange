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
-module(script_instance_manager).

-behaviour(gen_server).

-export([start_link/3]).
-export([script_dir/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([call/4]).

%%---------------------------------------------------------------------------

start_link(ServerName, CommandLine, MaxInstanceCount) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [CommandLine, MaxInstanceCount], []).

script_dir() ->
    %% Lifted from rabbit_mochiweb:static_context_handler/3. Why
    %% doesn't code:priv_dir work?
    {file, Here} = code:is_loaded(?MODULE),
    ModuleRoot = filename:dirname(filename:dirname(Here)),
    LocalPath = filename:join(ModuleRoot, "priv"),
    LocalPath.

call(Pid, M, F, A) ->
    gen_server:call(Pid, {call, M, F, A}).

%%---------------------------------------------------------------------------

-record(state, {command_line, max_instance_count, current_instance_count, free_instances, waiters}).

call_and_release(InstancePid, {M, F, A, From}) ->
    Manager = self(),
    spawn(fun () ->
                  link(InstancePid),
                  Reply = script_instance:call(InstancePid, M, F, A),
                  gen_server:reply(From, Reply),
                  Manager ! {instance_idle, InstancePid}
          end),
    ok.

maybe_proceed(State = #state{command_line = CommandLine,
                             free_instances = OldFree,
                             waiters = OldWaiters,
                             current_instance_count = OldCurrent,
                             max_instance_count = Max}) ->
    case queue:out(OldWaiters) of
        {{value, Waiter}, NewWaiters} ->
            case queue:out(OldFree) of
                {{value, FreePid}, NewFree} ->
                    ok = call_and_release(FreePid, Waiter),
                    State#state{free_instances = NewFree,
                                waiters = NewWaiters};
                {empty, _} ->
                    if
                        OldCurrent < Max ->
                            {ok, NewPid} = script_instance:start_link(CommandLine),
                            ok = call_and_release(NewPid, Waiter),
                            State#state{current_instance_count = OldCurrent + 1,
                                        waiters = NewWaiters};
                        true ->
                            State
                    end
            end;
        {empty, _} ->
            State
    end.

%%---------------------------------------------------------------------------

init([CommandLine, MaxInstanceCount]) ->
    process_flag(trap_exit, true),
    {ok, #state{command_line = CommandLine,
                max_instance_count = MaxInstanceCount,
                current_instance_count = 0,
                free_instances = queue:new(),
                waiters = queue:new()}}.

handle_call({call, M, F, A}, From, State = #state{waiters = OldWaiters}) ->
    {noreply, maybe_proceed(State#state{waiters = queue:in({M, F, A, From}, OldWaiters)})};
handle_call(Request, _From, State) ->
    {stop, {unhandled_call, Request}, State}.

handle_cast(Request, State) ->
    {stop, {unhandled_cast, Request}, State}.

handle_info({instance_idle, InstancePid}, State = #state{free_instances = OldFree}) ->
    {noreply, maybe_proceed(State#state{free_instances = queue:in(InstancePid, OldFree)})};
handle_info(Message, State) ->
    {stop, {unhandled_info, Message}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
