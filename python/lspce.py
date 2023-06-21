#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import websocket
import traceback
import message
from utils import logger
from lsp_server import LspServer, ProjectId
from enum import Enum


# for a project that contains different languages
lsp_servers = dict[ProjectId, LspServer]()  # key: ProjectId, value: LspServer
clients = dict() # key: ProjectId, value: websocket
server_notification_enabled = dict[ProjectId, bool]
server_request_enabled = dict[ProjectId, bool]

class EventType(Enum):
    CLIENT_START_SERVER = 0
    CLIENT_REQUEST = 1
    CLIENT_RESPONSE = 2
    CLIENT_NOTIFICATION = 3
    CLIENT_CHANGE_LOGLEVEL = 4
    CLIENT_TOGGLE_SERVER_NOTIFICATION = 5
    CLIENT_TOGGLE_SERVER_REQUEST = 6
    CLIENT_STOP_SERVER = 7
    SERVER_REQUEST = 20
    SERVER_RESPONSE = 21
    SERVER_NOTIFICATION = 22

class Lspce:
    def __init__(self, project_root: str, language_id: str, server_name: str, ws_port: int):
        self.project_root = project_root
        self.language_id = language_id
        self.server_name = server_name
        self.ws_port = ws_port # websocket server的端口号
        self.ws_client = None
        self.lsp_server = None
        self.initialized = False
        self.file_diagnostics = dict[str, message.Message]() # key: file, value: diagnostics
        self.stop_flag = False

    def _connect_emacs(self):
        try:
            self.ws_client = websocket.WebSocket(enable_multithread=True, skip_utf8_validation=False)
            self.ws_client.connect("ws://127.0.0.1:{}".format(self.ws_port), timeout = 0.005)
        except:
            logger.error(traceback.format_exc())

    def _deal_server_msg(self):
        try:
            if self.lsp_server is not None and not self.lsp_server.terminated():
                msg : message.Message = self.lsp_server.receive()
                if msg.msg_type is message.MESSAGE_TYPE.INVALID:
                    return
    
                if msg.msg_type is message.MESSAGE_TYPE.NOTIFICATION:
                    if msg.json is not None:
                        method = msg.json['method']
                        if method == "textDocument/publishDiagnostics":
                            uri = msg.json['params']['uri']
                            self.file_diagnostics[uri] = msg
                        else:
                            self._send_notification_to_emacs(msg)
                if msg.msg_type is message.MESSAGE_TYPE.REQUEST:
                    self._send_request_to_emacs(msg)
                if msg.msg_type is message.MESSAGE_TYPE.RESPONSE:
                    self._send_response_to_emacs(msg)
        except:
            logger.error(traceback.format_exc())

    def _deal_emacs_msg(self, msg):
        try:
            # TODO
            pass
        except:
            logger.error(traceback.format_exc())

    def _event_loop(self):
        while not self.stop_flag:
            try:
                ws_msg = self.ws_client.recv()
                self._deal_emacs_msg(ws_msg)
            except websocket.WebSocketTimeoutException:
                self._deal_server_msg()
            except:
                logger.error(traceback.format_exc())
                self.ws_client.close()
                self.stop_flag = True

    def _send_request_to_emacs(self, msg: message.Message):
        # TODO
        pass

    def _send_notification_to_emacs(self, msg: message.Message):
        # TODO
        pass

    def _send_response_to_emacs(self, msg: message.Message):
        # TODO
        pass

    def _send_diagnostic_to_emacs(self, msg: message.Message):
        # TODO
        pass

    def send(self, msg):
        if self.lsp_server is not None:
            self.lsp_server.send(msg)

    def start_server(self, project_root: str, language_id: str, server_name: str, command=None, ip=None, port=None, force=False) -> int:
        try:
            global lsp_servers
            project_id = ProjectId(project_root, language_id, server_name)
            server: LspServer = lsp_servers[project_id]
    
            start_flag = False
            if server is None:
                start_flag = True
            elif force:
                start_flag = True
    
            if start_flag:
                server = LspServer(project_root, language_id, server_name, command, ip, port)
                server.establish()
                lsp_servers[project_id] = server
    
            return server.pid()
        except:
            logger.error(traceback.format_exc())
            return -1
    
    def stop_server(project_root: str, language_id = None, server_name = None):
        try:
            global lsp_servers
            project_ids = lsp_servers.keys()
            dels = list()
            for project_id in project_ids:
                if project_id.project_root != project_root:
                    continue
                if language_id is not None:
                    if project_id.language_id != language_id:
                        continue
                    else:
                        if server_name is not None:
                            if project_id.server_name != server_name:
                                continue
                            else:
                                dels.append(project_id)
                        else:
                            pass
                else:
                    pass
    
            for project_id in dels:
                server: LspServer = lsp_servers[project_id]
                if server is not None:
                    server.terminate()
                    del lsp_servers[project_id]
        except:
            logger.error(traceback.format_exc())

if __name__ == "__main__":
    if len(sys.argv) != 5:
        logger.error("Invalid arguments")
    else:
        ws_port = int(sys.argv[1])
        project_root = sys.argv[2]
        language_id = sys.argv[3]
        server_name = sys.argv[4]
