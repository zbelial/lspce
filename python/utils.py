#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import logging

try:
    import orjson as json_parser
except:
    import json as json_parser

logger = logging.getLogger("lspce")
logger.setLevel(logging.INFO)
logger.addHandler(logging.StreamHandler())


def json_deserialize(str):
    return json_parser.loads(str)

def json_serialize(obj):
    return json_parser.dumps(obj)
