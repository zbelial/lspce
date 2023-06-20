#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import threading
import traceback
import lsp_server
import message
import queue
from utils import logger

# 跟lsp server的桥接通道，一个lsp server对应一个bridge。
# lsp client通过bridge发送消息到lsp server，并从bridge读请求/响应/通知等。
# 诊断通知单独存储，单独有供lsp server调用的接口。
class Bridge:
    def __init__(self, server: lsp_server.LspServer, ws_port: int):
        self.ws_port = ws_port # websocket server的端口号
        self.lsp_server = server
        self.server_responses = dict[str, message.Message]() # key: method, value: response message from server
        self.server_requests = queue.Queue()
        self.server_notifications = queue.Queue() # 除了诊断外的所有通知
        self.file_diagnostics = dict[str, message.Message]() # key: file, value: diagnostics
        self.server_disptch_thread = threading.Thread(target=self._server_disptch)
        self.server_disptch_thread.start()

    def send(self, msg):
        self.lsp_server.send(msg)

    def diagnostics(self, file: str):
        return self.file_diagnostics.get(file)

    def receive(self, method = None, id = None):
        pass

    def _server_disptch(self):
        while self.lsp_server is not None and not self.lsp_server.terminated():
            try:
                msg : message.Message = self.lsp_server.receive()
                if msg.msg_type is message.MESSAGE_TYPE.INVALID:
                    continue
    
                if msg.msg_type is message.MESSAGE_TYPE.NOTIFICATION:
                    if msg.json is not None:
                        method = msg.json['method']
                        if method == "textDocument/publishDiagnostics":
                            uri = msg.json['params']['uri']
                            self.file_diagnostics[uri] = msg
                        else:
                            self.server_notifications.put(msg)
                if msg.msg_type is message.MESSAGE_TYPE.REQUEST:
                    self.server_requests.put(msg)
                if msg.msg_type is message.MESSAGE_TYPE.RESPONSE:
                    if msg.json is not None:
                        method = msg.json['method']
                        self.server_responses[method] = msg
            except:
                logger.error(traceback.format_exc())
