#!/usr/bin/env python3

# Using imapidle from https://github.com/gumblex/imapidle/

from imapidle import imaplib
import passpy
from passpy_dict import password_dict
import os

runcmd = ["emacsclient -e '(mu4e-update-mail-and-index 1)'"]




# ["notify-send 'Email received' 'Ping for email received",
if __name__ == '__main__':
    pass_store = passpy.Store()  # Assumes GPG key is unlocked already
    pwdict = password_dict("jiby.tech/fastmail/emacs_hally", pass_store)
    m = imaplib.IMAP4_SSL(pwdict["host"])
    m.login(pwdict["user"], pwdict["password"])
    m.select()
    for uid, msg in m.idle():
        if msg == b"EXISTS":
            print("Ding!")
            for cmd in runcmd:
                os.system(cmd)
