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

## Security

Scripts must be signed (using [GnuPG](http://www.gnupg.org/)), and the
server is configured with a list of trusted GnuPG key IDs. When a
script is uploaded to the server, its signature is checked, and it is
only executed if the signature is good and is made by one of the
configured trusted keys.

The plugin **will not run** unless a list of trusted GnuPG key IDs is
configured; see below for details of the configuration variables
required.

The virtual machines for each supported scripting language run as the
same user that runs the RabbitMQ server, meaning that unless extra
steps are taken, scripts uploaded by AMQP **clients** can run code as
the AMQP **server user** on the server's operating system if their
signatures verify successfully.

For this reason, *do not enable this plugin until you have fully
analyzed the security consequences of doing so*. You may compromise a
large chunk of your network.

By default, the plugin is configured with only the
Spidermonkey[^spiderversion] Javascript interpreter
enabled. Spidermonkey is a reasonably tightly sandboxed environment
for client-supplied programs to run in. While Python is also
supported, the difficulty of securing the Python virtual machine makes
it unwise to enable Python support in anything other than the most
tightly locked-down environments.

Besides the existing support for signing of uploaded scripts, another
potential (but to date untried) approach to securing script
interpreters for the plugin is to use
[Jailkit](http://olivier.sessink.nl/jailkit/) (or similar) to set up
`chroot` jails for them. Jailkit runs on Linux, Solaris and various
BSDs including Mac OS X.

## Declaring `x-script` exchanges

Use AMQP's `Exchange.Declare` operation as usual, with type `x-script`
and the following three required arguments:

 - `type` (string): the MIME-type of the language to be used to
   interpret the attached script. Currently, the following language
   MIME-types are supported:

     - `text/javascript`: Requires Spidermonkey to be installed. I've
       been using version "1.8.0 pre-release 1
       2007-10-03".[^spiderversion]

     - `text/x-python`: Requires the default `python` interpreter to
       have a `simplejson` module installed. Python v2.6 and newer
       include `simplejson` by default.

 - `definition` (string): the source-code for the script itself.

 - `signature` (string): a detached GPG signature, covering the bytes
   in the `definition` argument.

As an example, here's an exchange declaration containing a simple
Javascript script, written for the
[Pika](http://github.com/tonyg/pika) Python AMQP client library:

    script_source = r"""
      return function (msg) {
        // filter, transform and route the message here to your taste.
        msg.fanout();
      }
    """

    signature = r"""
    -----BEGIN PGP SIGNATURE-----
    Version: GnuPG v1.4.10 (Darwin)

    iQEcBAABAgAGBQJLzVuBAAoJEJQm9mH+Pp+mcWIH/Ru1NsMXEur1TkBqInaf7o05
    6PI/okXUNmF9cXJwpA7inzLXYJTFSi6kVxPZ0LAxm/9bUUauFR4K5tXKDHOmRN/t
    k3hFm8HjFURz6wZNNuRUXxern4BJDu/7qx/fs0JvD7kizGjwHCfPOMq4oOo//Ov7
    BXGYEm3jAZD9tCt6K3prHVOHFnhacvlkRqvLQZxDa+ukhn9EfgHZU0QIKDfsj7ki
    uS1Ax+vRgLlT2BLABuVLEu2qsa5ABsr3V7VkPK/7wCw6SDf+x6IpzX5kRCiTe1lD
    6im6TIQNOiH0H9lTWBjo1XqQM3vaZAo5Dc/Hkq/taisK0xW5D6W8wUutL0XQWmY=
    =pkGp
    -----END PGP SIGNATURE-----
    """

    ch.exchange_declare(exchange = 'my-exchange-name',
                        type = 'x-script',
                        arguments = {"type": "text/javascript",
                                     "definition": script_source,
                                     "signature": signature})

Given a script definition in a variable `script_source`, and a key ID
(e.g. `"58914A52"`) in a variable `signingkey`, you can compute the
signature by shelling out to the `gpg` command-line tool. Here's a
python example, which leaves the computed signature in a variable
`signature`:

    (po, pi) = os.popen2("gpg -u %s -a --detach-sign" % (signingkey,))
    po.write(script_source)
    po.close()
    signature = pi.read()
    pi.close()

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

Note that the plugin ships with Python support disabled, for the
reasons given above in the section on security.

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

 - application `rabbit_script_exchange`, variable `permitted_key_ids`:
   set this to a list of strings, identifying the public-keys that
   will be accepted as valid signatures for uploaded script
   definitions. This variable defaults to the empty list. For an
   example of creating a keypair and configuring this variable, please
   see below.

 - application `rabbit_script_exchange`, variable `languages`: set
   this to a list of tuples describing the available language
   MIME-types and virtual-machine command-line invocations required to
   start instances. This variable defaults to

   <pre>
       {languages, [{<<"text/javascript">>, [{command_line, "js js_exchange_boot.js"}]}]}</pre>

   Command lines given are run with their current-working-directory
   set to the `priv` directory of the unpacked plugin.

 - application `rabbit_script_exchange`, variable
   `max_instance_count`: set this to the maximum number of instances
   of each language's virtual machine to start up. If more than one
   concurrent instance of a language is required to honor requests
   sent through `x-script` exchanges, another will be created until
   `max_instance_count` has been reached, after which the requesting
   party will be entered into a queue waiting for a busy instance to
   be freed up.

For the plugin to work, GnuPG (the `gpg` executable) needs to be on
the path, and the `permitted_key_ids` need to be available in the
`rabbitmq` user's GnuPG public keyring.

### Generating a keypair and configuring `permitted_key_ids`

In this example, we will generate and configure a GnuPG key that can
only be used for signing.

    $ gpg --gen-key
    gpg (GnuPG) 1.4.10; Copyright (C) 2008 Free Software Foundation, Inc.
    This is free software: you are free to change and redistribute it.
    There is NO WARRANTY, to the extent permitted by law.

    Please select what kind of key you want:
       (1) RSA and RSA (default)
       (2) DSA and Elgamal
       (3) DSA (sign only)
       (4) RSA (sign only)
    Your selection? 4
    RSA keys may be between 1024 and 4096 bits long.
    What keysize do you want? (2048)
    Requested keysize is 2048 bits
    Please specify how long the key should be valid.
             0 = key does not expire
          <n>  = key expires in n days
          <n>w = key expires in n weeks
          <n>m = key expires in n months
          <n>y = key expires in n years
    Key is valid for? (0) 1w
    Key expires at Tue Apr 27 19:54:33 2010 NZST
    Is this correct? (y/N) y

    You need a user ID to identify your key; the software constructs the user ID
    from the Real Name, Comment and Email Address in this form:
        "Heinrich Heine (Der Dichter) <heinrichh@duesseldorf.de>"

    Real name: Test key for script-exchange
    Email address: script-exchange@example.org
    Comment:
    You selected this USER-ID:
        "Test key for script-exchange <script-exchange@example.org>"

    Change (N)ame, (C)omment, (E)mail or (O)kay/(Q)uit? o
    You need a Passphrase to protect your secret key.

    You don't want a passphrase - this is probably a *bad* idea!
    I will do it anyway.  You can change your passphrase at any time,
    using this program with the option "--edit-key".

    We need to generate a lot of random bytes. It is a good idea to perform
    some other action (type on the keyboard, move the mouse, utilize the
    disks) during the prime generation; this gives the random number
    generator a better chance to gain enough entropy.
    +++++
    .+++++
    gpg: key 58914A52 marked as ultimately trusted
    public and secret key created and signed.

    gpg: checking the trustdb
    gpg: public key FBBB8AB1 is 58138 seconds newer than the signature
    gpg: 3 marginal(s) needed, 1 complete(s) needed, classic trust model
    gpg: depth: 0  valid:   4  signed:   3  trust: 0-, 0q, 0n, 0m, 0f, 4u
    gpg: depth: 1  valid:   3  signed:   1  trust: 3-, 0q, 0n, 0m, 0f, 0u
    gpg: next trustdb check due at 2010-04-27
    pub   2048R/58914A52 2010-04-20 [expires: 2010-04-27]
          Key fingerprint = 1F4E A37F 1657 2A50 DC77  E755 0D3D C0A9 5891 4A52
    uid                  Test key for script-exchange <script-exchange@example.org>

    Note that this key cannot be used for encryption.  You may want to use
    the command "--edit-key" to generate a subkey for this purpose.

Check that the key is present in the keyring of the `rabbitmq` user:

    $ gpg --list-keys script-exchange@example.org
    pub   2048R/58914A52 2010-04-20 [expires: 2010-04-27]
    uid                  Test key for script-exchange <script-exchange@example.org>

This lets us know that we should configure `permitted_key_ids` with
the string `"58914A52"`. There are many ways of doing this, but a good
choice is to add a stanza to your `rabbitmq.config` file:

    [
            {rabbit_script_exchange, [{permitted_key_ids, ["58914A52"]}]}
    ].

If more than one key is to be authorised for uploaded scripts, the
`permitted_key_ids` variable should contain more than one string:

    [
            {rabbit_script_exchange, [{permitted_key_ids, ["58914A52",
                                                           "12345678",
                                                           "98765432"]}]}
    ].

Both the short ("58914A52") form and the long ("0D3DC0A958914A52")
forms of key IDs are acceptable for use in the `permitted_key_ids`
variable.

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

[^spiderversion]: **N.B.**: version 1.7.0 of spidermonkey lacks a
significant `fflush()` call in its `print()` primitive which prevents
it from working with the script exchange. Version 1.8.0rc1 seems to be
fine, though.
