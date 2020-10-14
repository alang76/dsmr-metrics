# dsmr-metrics

Utility to read DSMR telegrams from the P1 port of a smart energy meter over a serial port. It publishes these as a Prometheus metrics scrape target. This works for me (Landis+Gyr E350). It is likely that other types of meter will produce outputs that will cause the parser to fail. The application does not implement the full DSMR spec. 

Note: the manufacturer specific stuff (mainly the telegram header) probably needs to be less picky. 

Mainly used as a learning vehicle for getting acquinted with these libraries:

* [Polysemy](https://hackage.haskell.org/package/polysemy) as the effect system
* [Hedgehog](https://hackage.haskell.org/package/hedgehog) as the property based testing tool

Here's my dash:

![My Dash](images/dsmr-metrics-dash.png?raw=true "My Dash")