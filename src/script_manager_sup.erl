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
-module(script_manager_sup).

-behaviour(supervisor).

-export([start_link/0, lookup/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

-rabbit_boot_step({?MODULE,
                   [{mfa, {rabbit_sup, start_child, [?MODULE]}},
                    {enables, script_exchange}]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

lookup(Name) ->
    case ets:lookup(?SERVER, Name) of
        [{_Name, ServerName}] -> {ok, ServerName};
        []                    -> {error, not_found}
    end.

init([]) ->
    ?SERVER = ets:new(?SERVER, [named_table, protected]),

    {ok, MaxInstanceCount} = application:get_env(rabbit_script_exchange, max_instance_count),
    {ok, LanguageDefs} = application:get_env(rabbit_script_exchange, languages),

    error_logger:info_msg("script_exchange supported_languages: ~p~n",
                          [[binary_to_list(MT) || {MT, _} <- LanguageDefs]]),

    {ok, {{one_for_one, 10, 10},
          [begin
               {value, {_, CommandLine}} = lists:keysearch(command_line, 1, Attributes),
               ServerName = list_to_atom("script_instance " ++ CommandLine),
               true = ets:insert(?SERVER, {MimeTypeBin, ServerName}),
               {ServerName, {script_instance_manager, start_link, [ServerName,
                                                                   CommandLine,
                                                                   MaxInstanceCount]},
                transient, 100, worker, [script_instance_manager, script_instance]}
           end
           || {MimeTypeBin, Attributes} <- LanguageDefs]}}.
