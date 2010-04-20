## The contents of this file are subject to the Mozilla Public License
## Version 1.1 (the "License"); you may not use this file except in
## compliance with the License. You may obtain a copy of the License at
## http:##www.mozilla.org/MPL/
##
## Software distributed under the License is distributed on an "AS IS"
## basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
## the License for the specific language governing rights and limitations
## under the License.
##
## The Original Code is script-exchange.
##
## The Initial Developers of the Original Code are Rabbit Technologies
## Ltd and Tony Garnock-Jones.
##
## Portions created by Rabbit Technologies Ltd or by Tony Garnock-Jones
## are Copyright (C) 2010 Rabbit Technologies Ltd and Tony Garnock-Jones.
##
## All Rights Reserved.
##
## Contributor(s): ______________________________________.

import simplejson
import sys

modules = {}

def send_obj(id, op, payload):
    print simplejson.dumps({'id': id, 'op': op, 'payload': payload})
    sys.stdout.flush()

def dbg(v):
    send_obj(None, "cast", ["io", "format", ["~p~n", [v]]])

def info(v):
    send_obj(None, "cast", ["error_logger", "info_report", [v]])

def log_error(report):
    send_obj(None, "cast", ["error_logger", "error_report", [report]])

def do_cast(payload):
    (m, f, a) = payload
    m = modules[m]
    f = getattr(m, f)
    return f(*a)

def mainloop1():
    try:
        line = raw_input()
    except EOFError:
        return False
    req = simplejson.loads(line)
    return dispatch_request(req)

def dispatch_request(req):
    op = req['op']
    if op == "call":
	send_obj(req['id'], "reply", do_cast(req['payload']))
    elif op == "cast":
        do_cast(req['payload'])
    else:
	log_error({"message": "Invalid request sent to js instance", "req": req})
    return True

def mainloop():
    try:
        while mainloop1():
            pass
    except:
        import traceback
	log_error({"message": "Exception in mainloop", "e": traceback.format_exc()})

###########################################################################

class Message(object):
    def __init__(self, rk, props, body):
        self.routing_key = rk
        self.properties = props
        self.body = body
        self.routing_actions = []

    def fanout(self):
        self.routing_actions.append("fanout")

    def direct(self, rk):
        self.routing_actions.append(["direct", rk])

    def topic(self, rk):
        self.routing_actions.append(["topic", rk])

class Exchange:
    def __init__(self):
        self.handlers = {}

    def validate(self, xname, definition):
        return True

    def create(self, xname, definition):
        vars = {}
        exec definition in globals(), vars
        self.handlers[','.join(xname)] = vars['handler']

    def publish(self, xname, rk, props, body):
	handler = self.handlers[','.join(xname)]
	msg = Message(rk, props, body)
	handler(msg)
	return [msg.properties, msg.body, msg.routing_actions]

modules['Exchange'] = Exchange()

info("Python instance started OK")
mainloop()
