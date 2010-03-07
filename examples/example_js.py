#!/usr/bin/env python
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
    function (msg) {
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
