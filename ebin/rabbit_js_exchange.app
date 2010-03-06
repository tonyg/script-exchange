{application, rabbit_js_exchange,
 [{description, "RabbitMQ Javascript exchange plugin"},
  {vsn, "0.01"},
  {modules, [js_instance,
             js_instance_manager,
             rabbit_exchange_type_js
            ]},
  {registered, []},
  {env, [{max_instance_count, 3},
         {script_name, "js_exchange_boot.js"}]},
  {applications, [kernel, stdlib, rabbit, mnesia]}]}.
