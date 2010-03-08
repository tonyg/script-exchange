{application, rabbit_script_exchange,
 [{description, "RabbitMQ Script Exchange Plugin"},
  {vsn, "0.01"},
  {modules, [script_exchange,
             script_instance,
             script_instance_manager,
	     script_manager_sup]},
  {registered, []},
  {env, [{languages, [
                      %% Commented out because of securability problems:
                      %% {<<"text/x-python">>, [{command_line, "python py_exchange_boot.py"}]},

                      {<<"text/javascript">>, [{command_line, "js js_exchange_boot.js"}]}
                     ]},
         {max_instance_count, 3}]},
  {applications, [kernel, stdlib, rabbit, mnesia]}]}.
