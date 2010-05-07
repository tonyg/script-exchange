PACKAGE=rabbit_script_exchange
DEPS=rabbitmq-server rabbitmq-erlang-client erlang-rfc4627
RUNTIME_DEPS=rfc4627_jsonrpc
EXTRA_PACKAGE_DIRS=priv

include ../include.mk

runjs:
	(cd priv; js js_exchange_boot.js)
