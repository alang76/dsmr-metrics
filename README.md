# dsmr-metrics

## intro

Utility to read DSMR telegrams from the P1 port of a smart energy meter over a serial port. It publishes these as a Prometheus metrics scrape target. This works for me (Landis+Gyr E350). It is likely that other type of meter will produce outputs that might cause the parser might fail. The application does not implement the full DSMR spec. 

Feel free to add anything missing/incorrect. Also the manufacturer specific stuff (mainly the telegram header) probably needs to be less picky. 

Mainly used as a learning vehicle for how to use these libraries:

* [Polysemy](https://hackage.haskell.org/package/polysemy) as the effect system
* [Hedgehog](https://hackage.haskell.org/package/hedgehog) as the property based testing tool
