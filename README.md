# RabbitMQ "Script Exchange" Plugin

Extends RabbitMQ Server with support for a new experimental exchange
type, `x-script`. An exchange of type `x-script` has an attached
program in a language such as Python, Ruby or Javascript, which is run
for each message travelling through the exchange.

Each script can:

 - **Filter and Route** messages: It's up to the script which messages
   are passed on, and if so how they are routed on to their
   destinations. Scripts can choose any combination of `fanout`,
   `direct` and `topic`-style message routing, on a message-by-message
   basis. If no routing selection is made, the message is dropped.

 - **Transform** messages: Scripts can alter content properties,
   headers and message bodies as they travel past. Arbitrary
   transformation is possible.

The current implementation does not expose the actual bindings
themselves to the scripts, but this is planned for a future
revision. Once the bindings are available for individual selection by
scripts, implementing exchanges such as an "XPath exchange" becomes
possible, where bindings contain XPath expressions for selecting
messages.

## Declaring `x-script` exchanges

Use AMQP's `Exchange.Declare` operation as usual, with type `x-script`
and the following arguments:

 - `type`: the MIME-type of the language to be used to interpret the
   attached script. Currently, the following language MIME-types are
   supported:

     - `text/javascript`: Requires Spidermonkey to be installed. I've
       been using version "1.8.0 pre-release 1 2007-10-03".
     - `text/x-python`: Requires the default `python` interpreter to
       have a `simplejson` module installed. Python v2.6 and newer
       include `simplejson` by default.

 - `definition`: the source-code for the script itself.

As an example, here's an exchange declaration containing a simple
Javascript script, written for the
[Pika](http://github.com/tonyg/pika) Python AMQP client library:

    script_source = r"""
      return function (msg) {
        // filter, transform and route the message here to your taste.
        msg.fanout();
      }
    """

    ch.exchange_declare(exchange = 'my-exchange-name',
                        type = 'x-script',
                        arguments = {"type": "text/javascript",
                                     "definition": script_source})

## Writing scripts

### Javascript

Supply a sequence of statements, ending in the `return` of a function
of one argument.

    arguments = {
      "type": "text/javascript",
      "definition": "return function (msg) { ... }"
    }

The message is passed in to the function as an object with the
following fields and methods:

 - `routing_key`: the routing key given when the message was published.
 - `properties`: a dictionary containing the content properties and
   headers, as per the AMQP specification.
 - `body`: the body of the message, as a string.
 - `fanout()`: call this method to cause the message to be routed as
   if the defined exchange were a fanout exchange.
 - `direct(key)`: call this method to cause the message to be routed
   as if the defined exchange were a direct exchange and as if the
   message had routing-key `key` attached to it.
 - `topic(key)`: call this method to cause the message to be routed as
   if the defined exchange were a topic exchange and as if the message
   had routing-key `key` attached to it.

If `properties` or `body` is altered before the function returns, the
changes will be sent on to the queues to which the message is routed.

Each call to `fanout`, `direct`, or `topic` causes a single delivery
of the message. For example, calling `fanout` twice will cause each
bound queue to receive *two* copies of the message.

### Python

Defining a Python exchange script is very similar to defining a
Javascript script. The main difference is that because the script is
evaluated with Python's
[exec](http://docs.python.org/reference/simple_stmts.html#the-exec-statement)
statement, `return`ing an anonymous function is not possible: instead,
a function named `handler` should be defined.

    arguments = {
      "type": "text/x-python",
      "definition": "def handler(msg): ..."
    }

Make sure to get the indentation right in the `definition` argument to
`Exchange.Declare`.

The `msg` argument to `handler` is a message object, very similar to
that given to a Javascript function, with identically-named fields and
methods. See the Javascript section for details.

## Managing state in scripts

While handler functions *can* be made stateful, it's not a good
idea. Try to keep your handler functions stateless.

The system is free to start (and shut down!) language-specific virtual
machines any time it needs to (up to the `max_instance_count` setting;
see below), so state held in memory can be lost at any time. If the
handler function returned from the `definition` program is a
closure---if it captures any mutable state---the lifetime of that
state is not guaranteed to persist for longer than a single call to
the handler function.

Furthermore, once more than a single virtual machine has been started
for a given language, all free instances are kept in a pool, so any
one virtual machine instance will see a different stream of messages
from the others.

If a stateful script is absolutely required, augment the
language-specific virtual machine boot script with a means of sharing
state between instances, and use that. For example, CouchDB, Redis,
Riak or even memcached could be used to share state between all
instances of a given exchange.

## Configuring the plugin and defining new languages

Define the following Erlang application configuration variables to
alter the settings for the plugin from their defaults:

 - application `rabbit_script_exchange`, variable
   `max_instance_count`: set this to the maximum number of instances
   of each language's virtual machine to start up. If more than one
   concurrent instance of a language is required to honor requests
   sent through `x-script` exchanges, another will be created until
   `max_instance_count` has been reached, after which the requesting
   party will be entered into a queue waiting for a busy instance to
   be freed up.

 - application `rabbit_script_exchange`, variable `languages`: set
   this to a list of tuples describing the available language
   MIME-types and virtual-machine command-line invocations required to
   start instances. This variable defaults to

   <pre>
       {languages, [{<<"text/javascript">>, [{command_line, "js js_exchange_boot.js"}]},
                    {<<"text/x-python">>,   [{command_line, "python py_exchange_boot.py"}]}]}</pre>

   Command lines given are run with their current-working-directory
   set to the `priv` directory of the unpacked plugin.

## Interactions between RabbitMQ and script language virtual machines

The protocol is not documented yet, as it may still change; read
`priv/js_exchange_boot.js` and `priv/py_exchange_boot.py` in
conjunction with `src/script_instance.erl` for details of its
operation.

## Known bugs

 - Unicode, UTF-8 and 8-bit-clean message processing has not yet been
   thoroughly tested.

 - The error handling in the vm-rabbit protocol is very poorly
   defined; error handling in the language-specific boot scripts is
   similarly awful. This can make debugging problems very
   difficult. Keep an eye on the RabbitMQ logs: it's the only place
   any useful information appears when problems occur.

## Licensing

This plugin is licensed under the MPL. The full license text is
included with the source code for the package. If you have any
questions regarding licensing, please contact us at
<info@rabbitmq.com>.
