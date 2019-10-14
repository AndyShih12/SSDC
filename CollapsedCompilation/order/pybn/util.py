#!/usr/bin/env python

import os

def get_username():
    userpath = os.path.expanduser('~')
    username = os.path.basename(userpath)
    return username

def get_hostname():
    return os.uname()[1]
