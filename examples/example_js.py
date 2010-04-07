#!/usr/bin/env python
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

import sys
sys.path.append('../pika')
import pika
import asyncore

conn = pika.AsyncoreConnection(pika.ConnectionParameters(
    '127.0.0.1',
    credentials=pika.PlainCredentials('guest', 'guest')))

ch = conn.channel()

# try:
#     print ch.exchange_delete(exchange='x-script')
# except pika.exceptions.ChannelClosed:
#     ch = conn.channel()

print ch.exchange_declare(exchange='x-script', type='x-script', arguments={
    "type": "text/javascript",
    "definition": r"""
    var counter = 0;
    return function (msg) {
      msg.fanout();
      msg.direct("hi");
      msg.topic("x.y." + msg.routing_key);
      msg.properties.user_id = "THISISTHEUSER";
      msg.body = counter + "\n" + msg.body;
      counter++;
    }
    """
    })

print ch.queue_declare(queue='x-script-q', auto_delete=True)
print ch.queue_bind(queue='x-script-q', exchange='x-script', routing_key='hi')
print ch.queue_bind(queue='x-script-q', exchange='x-script', routing_key='*.y.#')
print ch.queue_bind(queue='x-script-q', exchange='x-script', routing_key='x.#')
print ch.basic_publish(exchange='x-script', routing_key='testrk', body='testbody',
                       properties=pika.BasicProperties(content_type='text/plain'))

conn.close()
asyncore.loop()
