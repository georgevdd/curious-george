#include <iostream>
#include <sstream>

#include "Arduino.h"

#include <ESP8266WiFi.h>
#include <WiFiClient.h>
#include <ESP8266WiFiMulti.h>
#include <ESP8266mDNS.h>
#include <ESP8266WebServer.h>

#include "WifiCredentials.h"
#if !defined(WIFI_SSID) || !defined(WIFI_PASSWORD)
#  error WIFI_SSID, WIFI_PASSWORD must both be defined
#endif
#define stringify_(x) #x
#define stringify(x) stringify_(x)

ESP8266WiFiMulti wifiMulti;
ESP8266WebServer server(80);

void handleRoot();
void handleNotFound();

void connectToWiFi() {
  wifiMulti.addAP(stringify(WIFI_SSID), stringify(WIFI_PASSWORD));

  Serial.println("Connecting ...");
  while (wifiMulti.run() != WL_CONNECTED) {
	delay(250);
	Serial.print('.');
  }
  Serial.println('\n');
  Serial.print("Connected to ");
  Serial.println(WiFi.SSID());              // Tell us what network we're connected to
  Serial.print("IP address:\t");
  Serial.println(WiFi.localIP());           // Send the IP address of the ESP8266 to the computer
}

void startMdns() {
  if (MDNS.begin("esp8266")) {              // Start the mDNS responder for esp8266.local
	Serial.println("mDNS responder started");
  } else {
	Serial.println("Error setting up MDNS responder!");
  }
}

void startWebServer() {
  server.on("/", handleRoot);               // Call the 'handleRoot' function when a client requests URI "/"
  server.onNotFound(handleNotFound);        // When a client requests an unknown URI (i.e. something other than "/"), call function "handleNotFound"

  server.begin();                           // Actually start the server
  Serial.println("HTTP server started");
}

void setup(void){
  Serial.begin(9600);
  delay(10);
  Serial.println('\n');

  pinMode(0, OUTPUT);

  connectToWiFi();
  startMdns();
  startWebServer();
}

std::array<long, sizeof(unsigned long) * 8> loopDurationMicrosBuckets;

void showTimings(std::ostream& o) {
  unsigned long label = 1;
  for (const auto& count : loopDurationMicrosBuckets) {
    o << label << ' ' << count << std::endl;
    label <<= 1;
  }
}

void progress() {
  static int state = LOW;
  state = (state == LOW) ? HIGH : LOW;
  digitalWrite(0, state);

  static unsigned long previousNowMicros = 0;
  auto nowMicros = micros();
  if (previousNowMicros) {
    auto loopDuration = nowMicros - previousNowMicros;
    ++loopDurationMicrosBuckets[log2(loopDuration)];
  }
  previousNowMicros = nowMicros;
}

void loop(void){
  progress();
  server.handleClient();                    // Listen for HTTP requests from clients
}

void handleRoot() {
  std::ostringstream o;
  o << "Hello world!" << std::endl;
  showTimings(o);
  server.send(200, "text/plain", o.str().c_str());   // Send HTTP status 200 (Ok) and send some text to the browser/client
}

void handleNotFound(){
  server.send(404, "text/plain", "404: Not found"); // Send HTTP status 404 (Not Found) when there's no handler for the URI in the request
}
