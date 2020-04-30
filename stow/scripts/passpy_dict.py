#!/usr/bin/env python3

"""
Parsing config files stored in `pass` (https://passwordstore.org)
that follow a recutils style key-value.
Files are assumed to be formatted as `MYPASSWORD\nkey1: value1\nk2:v2`

Use with

    pass_store = passpy.Store()  # Assumes GPG key is unlocked already
    pwdict = password_dict("bank/rbs", pass_store)

Returned as a dict:

    {"password" :`MYPASSWORD`,
     "key1": "value1",
     "k2": "v2"}
"""

import passpy


def password_dict(pass_path, store_obj):
    """ Fetches the password entry for given pass_path from store_obj"""
    pass_file_text = store_obj.get_key(pass_path)
    # then parse it from string `MYPASSWORD\nkey1: value1\nk2:v2` to dict
    return format_pass_file(pass_file_text)


def format_pass_file(pass_file):
    """ Parses passpy file to dict"""
    # take the first line of the file = password
    pass_lines = pass_file.split('\n')
    pass_dict = {"password": pass_lines[0]}
    # the rest is `key: value` formatted
    for pass_line in pass_lines[1:-1]:
        k, v = pass_line.split(":")
        # remove trailing whitespace for values
        pass_dict[k] = v.strip()
    return pass_dict
