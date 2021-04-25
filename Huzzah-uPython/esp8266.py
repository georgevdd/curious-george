#!/usr/bin/env micropython

import json
import network
import tinyweb
import gc

import state


# Create web server
app = tinyweb.server.webserver()


# RESTAPI: System status
class Status():

    def get(self, data):
        mem = {'mem_alloc': gc.mem_alloc(),
               'mem_free': gc.mem_free(),
               'mem_total': gc.mem_alloc() + gc.mem_free()}
        sta_if = network.WLAN(network.STA_IF)
        ifconfig = sta_if.ifconfig()
        net = {'ip': ifconfig[0],
               'netmask': ifconfig[1],
               'gateway': ifconfig[2],
               'dns': ifconfig[3]
               }
        return {'memory': mem, 'network': net}


# Reset

class Reset():

    def put(self, data):
        import machine
        machine.soft_reset()


# State

class StateList():

    def get(self, data):
        return {
            k: getattr(state, k)
            for k in dir(state)
            if not k.startswith('_')
        }


class State():

    def put(self, data, key):
        new_value = data.get('value')
        if new_value is None:
            return {'message': '"value" is required'}, 400
        try:
            old_value = getattr(state, key)
        except AttributeError as e:
            return {'message': "Unknown setting '%s'" % key}, 404
        new_value = json.loads(new_value)
        if type(new_value) != type(old_value):
            raise TypeError(
                "Type mismatch: '%s' is a %s but existing value is a %s." %
                (new_value, type(new_value), type(old_value)))
        setattr(state, key, new_value)

gc.collect()


app.add_resource(Status, '/api/system')
app.add_resource(StateList, '/api/state')
app.add_resource(State, '/api/state/<key>')
app.add_resource(Reset, '/api/reset')


def run():
    app.run(host='0.0.0.0', port=8081)


if __name__ == '__main__':
    run()
