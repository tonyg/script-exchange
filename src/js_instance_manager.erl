-module(js_instance_manager).

-behaviour(gen_server).

-export([start_link/0]).
-export([script_dir/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([call/3, cast/3]).

-define(SERVER, ?MODULE).

-rabbit_boot_step({?MODULE,
                   [{mfa, {rabbit_sup, start_child, [?MODULE]}},
                    {enables, rabbit_exchange_type_js}]}).

%%---------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

script_dir() ->
    %% Lifted from rabbit_mochiweb:static_context_handler/3. Why
    %% doesn't code:priv_dir work?
    {file, Here} = code:is_loaded(?MODULE),
    ModuleRoot = filename:dirname(filename:dirname(Here)),
    LocalPath = filename:join(ModuleRoot, "priv"),
    LocalPath.

call(M, F, A) ->
    gen_server:call(?SERVER, {call, M, F, A}).

cast(M, F, A) ->
    gen_server:cast(?SERVER, {cast, M, F, A}).

%%---------------------------------------------------------------------------

-record(state, {script_name, max_instance_count, current_instance_count, free_instances, waiters}).

call_and_release(InstancePid, {M, F, A, From}) ->
    Manager = self(),
    spawn(fun () ->
                  link(InstancePid),
                  Reply = js_instance:call(InstancePid, M, F, A),
                  gen_server:reply(From, Reply),
                  Manager ! {instance_idle, InstancePid}
          end),
    ok.

maybe_proceed(State = #state{script_name = ScriptName,
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
                            {ok, NewPid} = js_instance:start_link(ScriptName),
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

init([]) ->
    process_flag(trap_exit, true),
    {ok, MaxInstanceCount} = application:get_env(rabbit_js_exchange, max_instance_count),
    {ok, ScriptName} = application:get_env(rabbit_js_exchange, script_name),
    {ok, #state{script_name = ScriptName,
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
