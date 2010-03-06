-module(js_instance).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([call/4, cast/4]).

%%---------------------------------------------------------------------------

start_link(ScriptName) ->
    gen_server:start_link(?MODULE, [ScriptName], []).

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

bad_js_message(Message, State) ->
    {stop, {bad_js_message, Message}, State}.

handle_msg(Message, State = #state{requests = OldReq}) ->
    Id = rfc4627:get_field(Message, "id", null),
    Op = rfc4627:get_field(Message, "op", null),
    Payload = rfc4627:get_field(Message, "payload", null),
    case Op of
        <<"reply">> ->
            case dict:find(Id, OldReq) of
                error ->
                    bad_js_message(Message, State);
                {ok, From} ->
                    gen_server:reply(From, {ok, Payload}),
                    {noreply, State#state{requests = dict:erase(Id, OldReq)}}
            end;
        <<"cast">> ->
            [MBin, FBin, A] = Payload,
            apply(list_to_atom(binary_to_list(MBin)), list_to_atom(binary_to_list(FBin)), A),
            {noreply, State};
        _Other ->
            bad_js_message(Message, State)
    end.

%%---------------------------------------------------------------------------

init([ScriptName]) ->
    ScriptDir = js_instance_manager:script_dir(),
    error_logger:info_report({starting_js, ScriptDir, ScriptName}),
    Port = open_port({spawn, "js " ++ ScriptName},
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
