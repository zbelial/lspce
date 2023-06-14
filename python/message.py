#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import traceback
from enum import Enum, auto

import utils
from utils import logger

class MESSAGE_TYPE(Enum):
    INVALID = auto()
    NOTIFICATION = auto()
    REQUEST = auto()
    RESPONSE = auto()
    
class Message:
    def __init__(self, msg):
        self.msg = msg
        self.json = None
        self.type = MESSAGE_TYPE.INVALID

        self._parse()

    def _parse(self):
        try:
            self.json = utils.json_deserialize(self.msg)
            if self.json.get("id") and self.json.get("method"):
                self.type = MESSAGE_TYPE.REQUEST
            elif self.json.get("method") is None:
                self.type = MESSAGE_TYPE.NOTIFICATION
            else:
                self.type = MESSAGE_TYPE.RESPONSE
        except:
            logger.error(traceback.format_exc())
