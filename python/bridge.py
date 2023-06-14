#!/usr/bin/env python3
# -*- coding: utf-8 -*-


# 跟lsp server的桥接通道，一个lsp server对应一个bridge。
# lsp client通过bridge发送消息到lsp server，并从bridge读请求/响应/通知等。
# 有两个队列，分别保存发往lsp server的数据jsonrpc message和从lsp server读取的jsonrpc message。有两个线程，分别处理上面的两个队列。
# 诊断通知单独存储，单独有供lsp server调用的接口。
class Bridge:
    def __init__(self, args):
        pass
