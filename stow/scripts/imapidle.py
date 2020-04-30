# Copyright (c) 2012 Mathieu Lecarme
# This code is licensed under the MIT license (see LICENSE for details)

# Copied from https://raw.githubusercontent.com/gumblex/imapidle/master/src/imapidle.py

import imaplib


def idle(connection):
    tag = connection._new_tag()
    name = bytes('IDLE', 'ASCII')
    data = tag + b' ' + name
    connection.send(data + imaplib.CRLF)
    response = connection.readline()
    if response != b'+ idling\r\n':
        raise Exception("IDLE not handled? : %s" % response)
    connection.loop = True
    while connection.loop:
        try:
            resp = connection._get_response()
        except connection.abort:
            connection.done()
        else:
            uid, message = resp.split(maxsplit=2)[1:]
            if uid.isdigit():
                yield uid, message
            elif uid != b'OK':
                raise imaplib.error(
                    'IDLE command error: %s %s' % (uid.decode(), message))
            # we have * OK still here


def done(connection):
    connection.send(b'DONE\r\n')
    response = connection.readline()
    connection.loop = False

imaplib.IMAP4.idle = idle
imaplib.IMAP4.done = done
