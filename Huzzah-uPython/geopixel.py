# Based on:
#   NeoPixel driver for MicroPython on ESP8266
#   MIT license; Copyright (c) 2016 Damien P. George
#
# Rewritten to use more memory and many fewer CPU cycles,
# especially when filling.

from esp import neopixel_write


def colour(r, g, b, w):
    return bytes((g, r, b, w))


class GeoPixel:

    def __init__(self, pin, n, timing=1):
        self.pin = pin
        self.n = n
        self.buf = bytearray(n << 2)
        self.pin.init(pin.OUT)
        self.timing = timing

    def __setitem__(self, index, val):
        if index < 0: index += self.n

        assert 0 <= index < self.n, "bad index: %d" % index
        assert len(val) == 4, val

        offset = index << 2
        self.buf[offset:offset+4] = bytearray(val)

    def __getitem__(self, index):
        offset = index << 2
        return self.buf[offset:offset+4]

    def fill(self, colour):
        self.buf[:] = colour * self.n

    def write(self):
        neopixel_write(self.pin, self.buf, self.timing)
