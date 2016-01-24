# This module patches the asyncore module to use the drivers select loop
# instead of asyncore.loop() (don't call that anymore).

import asyncore, select
import ldmud

class LDMudSocketMap(dict):
    def __setitem__(self, fd, obj):
        def on_event(events):
            asyncore.readwrite(obj, events)

        def get_events():
            events = 0
            if obj.readable():
                events |= select.POLLIN | select.POLLPRI
            if obj.writable() and not obj.accepting:
                events |= select.POLLOUT | select.POLLPRI
            return events

        ldmud.register_socket(fd, on_event, get_events)
        dict.__setitem__(self, fd, obj)

    def __delitem__(self, fd):
        ldmud.unregister_socket(fd)
        dict.__delitem__(self, fd)

def replace_socket_map():
    oldmap = asyncore.socket_map
    asyncore.socket_map = LDMudSocketMap()
    for (fd, obj) in oldmap.items():
        asyncore.socket_map[fd] = obj

replace_socket_map()
