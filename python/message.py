#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import traceback
from enum import Enum

import utils
from utils import logger

class MESSAGE_TYPE(Enum):
    INVALID = 0
    NOTIFICATION = 1
    REQUEST = 2
    RESPONSE = 3
    
class Message:
    def __init__(self, msg=""):
        self.msg = msg
        self.json = None
        self.msg_type = MESSAGE_TYPE.INVALID
        if len(self.msg) > 0:
            self._parse()

    def _parse(self):
        try:
            self.json = utils.json_deserialize(self.msg)
            if self.json.get("id") and self.json.get("method"):
                self.msg_type = MESSAGE_TYPE.REQUEST
            elif self.json.get("method") is None:
                self.msg_type = MESSAGE_TYPE.NOTIFICATION
            else:
                self.msg_type = MESSAGE_TYPE.RESPONSE
        except:
            logger.error(traceback.format_exc())
