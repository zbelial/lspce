#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import subprocess
import queue
import threading
import shlex
import sys
import re
import traceback
from sys import stderr
from enum import Enum, auto
from subprocess import PIPE

import utils
from utils import logger
from message import Message, MESSAGE_TYPE

BUFFER_SIZE = 1024 * 1024

class PROCESS_TYPE(Enum):
    UNKNOWN = auto()
    STDIO = auto()
    SOCKET = auto()

class PROCESS_STATUS(Enum):
    INVALID = auto()
    RUNNING = auto()
    QUITING = auto()
    FINISHED = auto()
    

# 代表一个lsp server实例
class LspServer:
    def __init__(self, project_root, language_id, server_name, command, ip=None, port=None):
        self.project_root = project_root
        self.language_id = language_id
        self.server_name = server_name
        self.command = command
        self.ip = ip
        self.port = port
        self.lsp_process = None
        self.lsp_process_type = PROCESS_TYPE.UNKNOWN
        self.lsp_process_status = PROCESS_STATUS.INVALID
        self.established = False
        self.send_queue = queue.Queue() # 保存待向lsp server写的数据
        self.receive_queue = queue.Queue() # 保存从lsp server读的数据
        self.sender_thread = threading.Thread(target=self._write_to_server)
        self.receiver_thread = threading.Thread(target=self._read_from_server)

        # stdio优先
        if self.ip is not None and self.port is not None:
            self.lsp_process_type = PROCESS_TYPE.SOCKET
        if self.command is not None:
            self.lsp_process_type = PROCESS_TYPE.STDIO
            self.command = shlex.split(self.command)


    # 创建本地lsp server进程或者连接到lsp server服务
    def establish(self):
        if self.lsp_process_type is PROCESS_TYPE.UNKNOWN:
            pass
        elif self.lsp_process_type is PROCESS_TYPE.STDIO:
            self._create_process()
        elif self.lsp_process_type is PROCESS_TYPE.SOCKET:
            self._connect_remote()
        else:
            pass

        if self.lsp_process is None:
            raise RuntimeError("Failed to establish lsp server.")

        self.established = True
        self.lsp_process_status = PROCESS_STATUS.RUNNING
        
        self.sender_thread.start()
        self.receiver_thread.start()

        logger.info("established")
        
    def send(self, msg):
        self.send_queue.put(msg)

    def receive(self, timeout = 0.005):
        try:
            msg = self.receive_queue.get(block=True, timeout=timeout)
            return msg
        except:
            logger.error(traceback.format_exc())
            return None

    def pop_send_queue(self, timeout=0.005):
        try:
            msg = self.send_queue.get(block=True, timeout=timeout)
            return msg
        except:
            return None

    def _write_to_server(self):
        try:
            if self.lsp_process_type is PROCESS_TYPE.STDIO:
                while self.lsp_process.poll() is None:
                    msg = self.pop_send_queue()
                    if msg is not None:
                        self.lsp_process.stdin.write(msg.encode("utf-8"))
                        self.lsp_process.stdin.flush()
                        logger.info("_write_to_server msg {}".format(msg))
            else: # socket
                # TODO socket _write_to_server
                pass
        except:
            logger.error(traceback.format_exc())

    def _read_from_server(self):
        try:
            if self.lsp_process_type is PROCESS_TYPE.STDIO:
                content_length = None
                buffer = bytearray()
                while self.lsp_process.poll() is None:
                    if content_length is None:
                        match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                        if match is not None:
                            end = match.end()
                            parts = match.group(0).decode("utf-8").strip().split(": ")
                            content_length = int(parts[1])
                            buffer = buffer[end:]
                        else:
                            line = self.lsp_process.stdout.readline()
                            if re.search(b"Content-Type", line) is None:
                                buffer = buffer + line
                    else:
                        if len(buffer) < content_length:
                            match = re.search(b"Content-Length: [0-9]+\r\n\r\n", buffer)
                            if match is not None:
                                start = match.start()
                                msg = buffer[0:start]
                                buffer = buffer[start:]
                                content_length = None
                                self.queue_msg(msg.decode("utf-8"))
                            else:
                                line = self.lsp_process.stdout.readline(content_length - len(buffer))
                                buffer = buffer + line
                        else:
                            msg = buffer[0: content_length]
                            buffer = buffer[content_length:]
                            content_length = None
                            logger.info("_read_from_server msg {}".format(msg))
                            self.queue_msg(msg.decode("utf-8"))
                    if self.lsp_process.stderr:
                        logger.info(self.lsp_process.stderr.read())
                logger.info("LSP server {} exited with code {}".format(self.server_name, self.lsp_process.returncode))
            else: # socket
                # TODO socket _read_from_server
                pass
        except:
            logger.error(traceback.format_exc())

    def queue_msg(self, msg):
        if not msg:
            return
        try:
            m = Message(msg)
            if m.type is not MESSAGE_TYPE.INVALID:
                self.receive_queue.put(m)
        except:
            logger.error(traceback.format_exc())

    # 创建lsp server进程
    def _create_process(self):
        try:
            self.lsp_process = subprocess.Popen(self.command,
                                                bufsize=BUFFER_SIZE,
                                                stdin=PIPE,
                                                stdout=PIPE,
                                                stderr=stderr)
            logger.info("lsp_process pid {}".format(self.lsp_process.pid))
        except:
            self.lsp_process = None
            logger.error(traceback.format_exc())

    # 连接到lsp server服务
    def _connect_remote(self):
        # TODO _connect_remote
        pass


if __name__ == "__main__":
    if len(sys.argv) < 5:
        print("arguments not enough")
    else:
        lsp_server = LspServer(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
        lsp_server.establish()

        json_content = utils.json_serialize({"id":1, "method":"initialize", "params":"abc"})
        msg = "Content-Length: {}\r\n\r\n{}".format(len(json_content), json_content)
        lsp_server.send(msg)
