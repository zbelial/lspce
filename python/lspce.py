#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import traceback
from utils import logger
from lsp_server import LspServer

class ProjectId:
    def __init__(self, project_root, language_id, server_name):
        self.project_root = project_root
        self.language_id = language_id
        self.server_name = server_name

    def project_root(self):
        return self.project_root

    def language_id(self):
        return self.language_id

    def server_name(self):
        return self.server_name

class Lspce:
    def __init__(self):
        # for a project that contains different languages
        self.servers = dict() # key: ProjectId, value: LspServer

    def start_server(self, project_root: str, language_id: str, server_name: str, command=None, ip=None, port=None, force=False) -> int:
        try:
            project_id = ProjectId(project_root, language_id, server_name)
            server: LspServer = self.servers[project_id]

            start_flag = False
            if server is None:
                start_flag = True
            elif force:
                start_flag = True

            if start_flag:
                server = LspServer(project_root, language_id, server_name, command, ip, port)
                server.establish()
                self.servers[project_id] = server

            return server.pid()
        except:
            logger.error(traceback.format_exc())
            return -1
        

    def stop_server(self, project_root: str, language_id = None, server_name = None):
        try:
            project_ids = self.servers.keys()
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
                server: LspServer = self.servers[project_id]
                if server is not None:
                    server.terminate()
                    del self.servers[project_id]
        except:
            logger.error(traceback.format_exc())

if __name__ == "__main__":
    ps = dict()
    p1 = ProjectId("/mnt/Personal/Sources/lspce", "python", "pylsp")
    p1v = "test"
    p2 = ProjectId("/mnt/Personal/Sources/lspce2", "python", "pyright")
    p2v = "test2"

    ps[p1] = p1v
    ps[p2] = p2v

    print(ps[p1])
    print(ps[p2])
