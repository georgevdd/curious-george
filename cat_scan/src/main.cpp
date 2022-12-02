#include <Arduino.h>
#include "SoftwareSerial.h"

auto rfid_serial = SoftwareSerial(
  /*receivePin=*/2,
  /*transmitPin=*/3,
  /*inverseLogic=*/false
);

auto& usb_serial = Serial;

static union {
struct {
  uint8_t start_marker;
  uint8_t fixed_code[3];
  uint8_t country_code[2];
  uint8_t animal_id[5];
  uint8_t checksum;
  uint8_t end_marker;
} fields;
char buffer[sizeof(fields)];
};

static_assert(sizeof(fields) == 13);

int bytes_read;  // Index of next free byte in buffer.
int total_bytes_read = 0;

void decode_and_print_buffer() {
  uint16_t country_code = 0;
  for (int i = 0; i < 2; ++i) {
    country_code = (country_code << 8) + fields.country_code[i];
  }
  uint64_t animal_id = 0;
  for (int i = 0; i < 5; ++i) {
    animal_id = (animal_id << 8) + fields.animal_id[i];
  }
  char decimal_buffer[16];
  snprintf(
      decimal_buffer, 16, "%03u%06lu%06lu",
      country_code,
      // Printing 64-bit integers seems not to work at all, so instead
      // we print the two halves of the animal ID separately.
      static_cast<uint32_t>(animal_id / 1000000),
      static_cast<uint32_t>(animal_id % 1000000)
  );
  usb_serial.print("15-digit identifier: ");
  usb_serial.println(decimal_buffer);
}

void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
  usb_serial.begin(9600);
  usb_serial.print("Initialising ... ");

  rfid_serial.begin(9600);
  if (!rfid_serial.isListening()) {
    usb_serial.println("Failed to initialise software serial port.");
    return;
  }

  digitalWrite(LED_BUILTIN, LOW);
  usb_serial.println("Ready.");
}

void loop() {
  if (rfid_serial.overflow()) {
    usb_serial.println("Overflow!");
  }
  while (rfid_serial.available()) {
    int available_byte;
    if ((available_byte = rfid_serial.read()) != -1) {
      usb_serial.print(available_byte, HEX);
      usb_serial.print(' ');
      ++total_bytes_read;

      // Don't start filling the read buffer unless this byte is a start
      // marker. That way, if we ever get into some sort of unaligned state
      // then it will be possible to recover eventually.
      if (bytes_read == 0 && available_byte != 0xAA) {
        usb_serial.println("Ignoring non-start byte.");
        continue;
      }

      buffer[bytes_read++] = static_cast<char>(available_byte);
      if (bytes_read == sizeof(fields)) {
        usb_serial.println();
        decode_and_print_buffer();
        memset(buffer, 0, sizeof(fields));
        bytes_read = 0;
      }
    } else {
      usb_serial.println("That's odd. An available byte could not be read.");
    }
  }

  // Flash the LED till we've read something, to show that the loop is
  // still running.
  digitalWrite(LED_BUILTIN,
               total_bytes_read > 0 || ((millis() / 500) % 2 == 0));
}
