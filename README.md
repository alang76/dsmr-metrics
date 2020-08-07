# dsmr-metrics

## intro

Reads DSMR telegrams from the P1 port of a smart energy meter over the serial port. It publishes these as a Prometheus metrics scrape target. This works for me (Landis+Gyr , likely your meter outputs different metrics and the parser might fail. It does not implement the full DSMR spec. Feel free to add anything missing/incorrect. Also the manufacturer specific stuff (mainly the telegram header) probably needs to be less picky. 

## 