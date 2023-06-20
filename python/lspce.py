#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import asyncio
import websockets
import traceback
import utils
from utils import logger
from lsp_server import LspServer, ProjectId
from enum import Enum
from websockets.server import serve


# for a project that contains different languages
lsp_servers = dict[ProjectId, LspServer]()  # key: ProjectId, value: LspServer
clients = dict() # key: ProjectId, value: websocket
server_notification_enabled = dict[ProjectId, bool]
server_request_enabled = dict[ProjectId, bool]

def start_server(project_root: str, language_id: str, server_name: str, command=None, ip=None, port=None, force=False) -> int:
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

class EventType(Enum):
    CLIENT_INIT = 0
    CLIENT_REQUEST = 1
    CLIENT_RESPONSE = 2
    CLIENT_NOTIFICATION = 3
    CLIENT_CHANGE_LOGLEVEL = 4
    CLIENT_TOGGLE_SERVER_NOTIFICATION = 5
    CLIENT_TOGGLE_SERVER_REQUEST = 6
    CLIENT_CLOSE = 7
    SERVER_REQUEST = 20
    SERVER_RESPONSE = 21
    SERVER_NOTIFICATION = 22

async def handler(websocket):
    async for message in websocket:
        try:
            logger.debug("event {}".format(message))
    
            event = utils.json_deserialize(message)
            event_type = event['event_type']
            event_data = event['event_data']
            if event_type == EventType.CLIENT_INIT.value:
                pass
            elif event_type == EventType.CLIENT_REQUEST.value:
                pass
            elif event_type == EventType.CLIENT_RESPONSE.value:
                pass
            elif event_type == EventType.CLIENT_NOTIFICATION.value:
                pass
            elif event_type == EventType.CLIENT_CHANGE_LOGLEVEL.value:
                pass
            elif event_type == EventType.CLIENT_TOGGLE_SERVER_NOTIFICATION.value:
                pass
            elif event_type == EventType.CLIENT_TOGGLE_SERVER_REQUEST.value:
                pass
            elif event_type == EventType.CLIENT_CLOSE.value:
                logger.info("websocket client closing")
                break
            elif event_type == EventType.SERVER_REQUEST.value:
                pass
            elif event_type == EventType.SERVER_RESPONSE.value:
                pass
            elif event_type == EventType.SERVER_NOTIFICATION.value:
                pass
            else:
                logger.error("Unknown event {}".format(event))
        except:
            logger.error(traceback.format_exc())

async def main(port):
    async with serve(handler, "", port):
        await asyncio.Future() # run forevor

if __name__ == "__main__":
    if len(sys.argv) != 2:
        logger.error("Invalid arguments")
    else:
        asyncio.run(main(int(sys.argv[1])))
