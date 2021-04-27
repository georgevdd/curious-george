## Background

Pandemic lockdowns made everyone a bit weird, didn't they? In my case, I somehow thought it would be a good idea to try to put together some programmable LED strips to customise ... a bookcase. Since I had already fitted the bookcase with an acrylic mirror backing, I could fit LEDs around the back of the face frame and have them reflected in it. I hoped that the overall result would be like a cross between an infinity mirror and ... a bookcase. When I found that the shelves were cut slightly shorter than the width of the case, leaving just enough room to thread a strip of NeoPixels or similar down either side, I couldn't resist making it happen.

## Requirements

Overall, the job would need a little under 5m of LED strip. At 60 LEDs/m that would be near 300 LEDs. The bookcase has a crown of about 10cm - ideal for concealing a box of tricks, from which power and control for LED strips could be fed through small holes drilled in the front corners of the top. Since a strip of LEDs is controlled from one end, that would mean controlling two separate strips of different lengths - one for one side and one for the top and the other side.

I wanted to be able to reprogram the controller remotely, without having to get it down from its hiding place. Connecting it to the local network would be the natural way to do that - by WiFi, to avoid more cabling. I wasn't too worried about availability of programming languages; I could always implement [half of Common Lisp](https://en.wikipedia.org/wiki/Greenspun%27s_tenth_rule) if necessary.

I was keen to avoid saturated red/green/blue colours and be able to produce warm, full whites and pastel shades. Luckily, RGBW strips are just as available as RGB ones.

Each LED draws about 20mA per colour at full brightness, so the full set would draw nearly `20mA × 4 × 300 = 24A`. Full power for all lights at once isn't necessary, though, so a 10A supply should be enough.

Summary of controller requirements:

- Can be run off a 5V power supply;
- Drive two strips of 5V LEDs via GPIO pins;
- WiFi connectivity.

I chose an [Adafruit Feather Huzzah](https://learn.adafruit.com/adafruit-feather-huzzah-esp8266) and [5m of RGBW NeoPixel-compatible LEDs](https://shop.pimoroni.com/products/flexible-rgbw-led-strip-neopixel-ws2812-sk6812-compatible?variant=30260032700499) from Pimoroni (which sounds Italian but is British). For a power supply I used [one of these](https://www.amazon.co.uk/gp/product/B07DQKM9P7).

## Detailed Design

### Powering the Controller

[Adafruit's instructions](https://learn.adafruit.com/adafruit-feather-huzzah-esp8266/power-management) recommend against powering the board other than from a battery (~4V) or from USB (5V). So I cut apart a micro USB cable and extracted its power and ground lines for connection to the 5V power supply.

### Powering the LEDs

[Adafruit's instructions](https://learn.adafruit.com/adafruit-neopixel-uberguide/powering-neopixels) recommend supplying power to every metre separately. Luckily I had a bunch of ancient speaker cable (thanks, Dad!) that was begging to be upcycled and more than fat enough for the job. I included a 1000µF, 35V capacitor at each power point.

### Controlling the LEDs

One small difficulty with the ESP8266-based Feather Huzzah is that it runs at 3.3V, while the LEDs (especially in long runs) need 5V control. [It turns out](https://learn.adafruit.com/neopixel-levelshifter) that there's a chip for that. The chip would need a 5V power supply, but it's low-power enough to be supplied from the USB power pin of the Feather itself.

I also included 470Ω resistors on each 5V data line, as recommended.

## Assembly

### Controller: Mounting and Packaging

I mounted the controller and its accompanying level shifter on a breadboard (also second-hand - thanks, Freddie!).

An old shoebox was big enough to contain the power supply and breadboard, and sported a convenient finger hole through which to pass all the cables. Inside, I glued some wooden blocks onto which to screw the power supply, and some 5mm wooden strips to hold the breadboard neatly against one wall. I cut it down to 8cm tall so it wouldn't show over the crown of the bookcase.

### LEDs: Installation

LEDs were held in place with double-sided tape.

Power wiring was done with standard terminal blocks that could be screwed onto the inside of the bookcase. Connections to LED strips were (badly!) soldered. Capacitors were fitted across the nearest terminal blocks.

Control was via jumper wires.
