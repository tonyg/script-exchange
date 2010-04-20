// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
// the License for the specific language governing rights and limitations
// under the License.
//
// The Original Code is script-exchange.
//
// The Initial Developers of the Original Code are Rabbit Technologies
// Ltd and Tony Garnock-Jones.
//
// Portions created by Rabbit Technologies Ltd or by Tony Garnock-Jones
// are Copyright (C) 2010 Rabbit Technologies Ltd and Tony Garnock-Jones.
//
// All Rights Reserved.
//
// Contributor(s): ______________________________________.

load("json2.js");

///////////////////////////////////////////////////////////////////////////

var Modules = {};

function send_obj(id, op, payload) {
    print(JSON.stringify({id: id, op: op, payload: payload}));
}

function dbg(v) {
    send_obj(null, "cast", ["io", "format", ["~p~n", [v]]]);
}

function info(v) {
    send_obj(null, "cast", ["error_logger", "info_report", [v]]);
}

function log_error(report) {
    send_obj(null, "cast", ["error_logger", "error_report", [report]]);
}

function do_cast(payload) {
    var m = Modules[payload[0]];
    if (!m) {
	throw {message: "No such module", module: payload[0], payload: payload};
    }
    var f = m[payload[1]];
    if (!f) {
	throw {message: "No such function", "function": payload[1], payload: payload};
    }
    return f.apply(m, payload[2]);
}

function mainloop1() {
    var line = readline();
    if (line === null) return false;

    var req = JSON.parse(line);
    return dispatch_request(req);
}

function dispatch_request(req) {
    switch (req.op) {
    case "call":
	send_obj(req.id, "reply", do_cast(req.payload));
	break;

    case "cast":
	do_cast(req.payload);
	break;

    default:
	log_error({message: "Invalid request sent to js instance", req: req});
	break;
    }
    return true;
}

function mainloop() {
    try {
	while (mainloop1()) {
	}
    } catch (e) {
	log_error({message: "Exception in mainloop", e: e});
    }
}

///////////////////////////////////////////////////////////////////////////

Modules.Exchange = {
    handlers: {},

    Message: function(rk, props, body) {
	this.routing_key = rk;
	this.properties = props;
	this.body = body;
	this.routing_actions = [];
    },

    validate: function (xname, definition) {
	if (!definition) {
	    return false;
	}
	return true;
    },

    create: function (xname, definition) {
	this.handlers[xname.join(',')] = eval("(function () { " + definition + "})()");
    },

    publish: function (xname, rk, props, body) {
	var handler = this.handlers[xname.join(',')];
	if (!handler) return null;

	var msg = new this.Message(rk, props, body);
	handler(msg);
	return [msg.properties, msg.body, msg.routing_actions];
    }
};

Modules.Exchange.Message.prototype.fanout = function () {
    this.routing_actions.push("fanout");
};

Modules.Exchange.Message.prototype.direct = function (rk) {
    this.routing_actions.push(["direct", rk]);
};

Modules.Exchange.Message.prototype.topic = function (rk) {
    this.routing_actions.push(["topic", rk]);
};

///////////////////////////////////////////////////////////////////////////

info("JS instance started OK");
mainloop();
