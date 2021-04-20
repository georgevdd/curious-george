#include <iostream>
#include <sstream>

#include "Arduino.h"

#include <ESP8266WiFi.h>
#include <WiFiClient.h>
#include <ESP8266HTTPClient.h>
#include <ESP8266WiFiMulti.h>
#include <ESP8266mDNS.h>
#include <ESP8266WebServer.h>

#include <LittleFS.h>

#include "WifiCredentials.h"
#if !defined(WIFI_SSID) || !defined(WIFI_PASSWORD)
#  error WIFI_SSID, WIFI_PASSWORD must both be defined
#endif
#define stringify_(x) #x
#define stringify(x) stringify_(x)

ESP8266WiFiMulti wifiMulti;
ESP8266WebServer server(80);

void handleRoot();
void handleFileGet();
void handleFilePost();
void handleStats();
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
  Serial.println(WiFi.SSID());
  Serial.print("IP address:\t");
  Serial.println(WiFi.localIP());
}

void startMdns() {
  if (MDNS.begin("esp8266")) {              // Start the mDNS responder for esp8266.local
	Serial.println("mDNS responder started");
  } else {
	Serial.println("Error setting up MDNS responder!");
  }
}

void startWebServer() {
  server.on("/", handleRoot);
  server.on("/file", HTTPMethod::HTTP_GET, handleFileGet);
  server.on("/file",
      HTTPMethod::HTTP_POST,
      [](){ Serial.println("WTAF?!"); server.send(HTTP_CODE_OK); },
      handleFilePost);
  server.on("/stats", handleStats);
  server.onNotFound(handleNotFound);

  server.begin();
  Serial.println("HTTP server started");
}

void setup(void){
  Serial.begin(9600);
  delay(10);
  Serial.println('\n');

  pinMode(0, OUTPUT);

  LittleFS.begin();
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
  server.handleClient();
}

void handleFileGet() {
  Serial.println("Sending file ...");
  auto file = LittleFS.open("/file", "r");
  if (!file) {
    Serial.println("Unable to open file.");
    server.send(HTTP_CODE_NOT_FOUND, "text/plain", "Couldn't find that.");
  } else {
    auto bytesSent = server.streamFile(file, "text/plain");
    file.close();
    Serial.print("Sent "); Serial.print(bytesSent); Serial.print(" bytes.");
  }
}

void handleFilePost() {
  Serial.println("handleFilePost1!!");

  static File currentFile;

  const auto& upload = server.upload();

  switch (upload.status) {
  case UPLOAD_FILE_START: {
    Serial.println("Receiving file ...");
    delay(200);
    currentFile = LittleFS.open("/file", "w");
    if (!currentFile) {
      server.send(
          HTTP_CODE_INTERNAL_SERVER_ERROR,
          "text/plain",
          "Unable to open file.");
    }
    break;
  }
  case UPLOAD_FILE_WRITE: {
    Serial.println("Still receiving file ...");
    auto bytesWritten = currentFile.write(upload.buf, upload.currentSize);
    if (bytesWritten != upload.contentLength) {
      server.send(
          HTTP_CODE_INSUFFICIENT_STORAGE,
          "text/plain",
          "Unable to write all the data.");
    }
    break;
  }
  case UPLOAD_FILE_END: {
    Serial.println("Done receiving file.");
    currentFile.close();
    server.send(HTTP_CODE_OK, "text/plain", "All done.");
    break;
  }
  case UPLOAD_FILE_ABORTED: {
    Serial.println("Aborted receiving file.");
    server.send(HTTP_CODE_OK, "text/plain", "Aborted.");
    break;
  }
  default: {
    Serial.print("WTF?! Code "); Serial.println(upload.status);
  }
  }

}

void handleStats() {
  std::ostringstream o;
  o << "floor(log2(micros)) count" << std::endl;
  showTimings(o);
  server.send(200, "text/plain", o.str().c_str());}

void handleRoot() {
  std::ostringstream o;
  o << "Hello world!" << std::endl;
  server.send(200, "text/plain", o.str().c_str());
}

void handleNotFound(){
  server.send(404, "text/plain", "404: Not found");
}
