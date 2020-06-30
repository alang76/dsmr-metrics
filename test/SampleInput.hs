module SampleInput where

telegram :: String
telegram = 
    "/XMX5LGBBFFB231215493\n\
    \\n\
    \1-3:0.2.8(42)\n\
    \0-0:1.0.0(200529163319S)\n\
    \0-0:96.1.1(4530303034303031353934373534343134)\n\
    \1-0:1.8.1(014765.683*kWh)\n\
    \1-0:2.8.1(000000.000*kWh)\n\
    \1-0:1.8.2(014628.043*kWh)\n\
    \1-0:2.8.2(000000.000*kWh)\n\
    \0-0:96.14.0(0002)\n\
    \1-0:1.7.0(03.877*kW)\n\
    \1-0:2.7.0(00.000*kW)\n\
    \0-0:96.7.21(00003)\n\
    \0-0:96.7.9(00002)\n\
    \1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s)\n\
    \1-0:32.32.0(00000)\n\
    \1-0:32.36.0(00000)\n\
    \0-0:96.13.1()\n\
    \0-0:96.13.0()\n\
    \1-0:31.7.0(017*A)\n\
    \1-0:21.7.0(03.877*kW)\n\
    \1-0:22.7.0(00.000*kW)\n\
    \0-1:24.1.0(003)\n\
    \0-1:96.1.0(4730303137353931323139313130333134)\n\
    \0-1:24.2.1(200529160000S)(05277.053*m3)\n\
    \!7667\n"

telegram2 :: String 
telegram2 = 
    "/XMX5LGBBFFB231215493\n\
    \\n\
    \1-3:0.2.8(42)\n\
    \0-0:1.0.0(200625210415S)\n\
    \0-0:96.1.1(4530303034303031353934373534343134)\n\
    \1-0:1.8.1(014966.198*kWh)\n\
    \1-0:2.8.1(000000.000*kWh)\n\
    \1-0:1.8.2(014891.988*kWh)\n\
    \1-0:2.8.2(000000.000*kWh)\n\
    \0-0:96.14.0(0002)\n\
    \1-0:1.7.0(00.454*kW)\n\
    \1-0:2.7.0(00.000*kW)\n\
    \0-0:96.7.21(00003)\n\
    \0-0:96.7.9(00002)\n\
    \1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s)\n\
    \1-0:32.32.0(00000)\n\
    \1-0:32.36.0(00000)\n\
    \0-0:96.13.1()\n\
    \0-0:96.13.0()\n\
    \1-0:31.7.0(003*A)\n\
    \1-0:21.7.0(00.454*kW)\n\
    \1-0:22.7.0(00.000*kW)\n\
    \0-1:24.1.0(003)\n\
    \0-1:96.1.0(4730303137353931323139313130333134)\n\
    \0-1:24.2.1(200625210000S)(05292.965*m3)\n\
    \!6DB2"



{-
Meter ID: LGBBFFB231215493
1-3:0.2.8(42) <- Version number (4.2)
0-0:1.0.0(200529163319S) <- Timestamp of the message in TST (YYMMDDhhmmssX)  (DST active (summer time): X=S, DST not active (winter time): X=W)
0-0:96.1.1(4530303034303031353934373534343134) <- Equipment identifier
1-0:1.8.1(014765.683*kWh) <- Meter Reading electricity delivered *to* client (Tariff 1) in 0,001 kWh
1-0:2.8.1(000000.000*kWh) <- Meter Reading electricity delivered *by* client (Tariff 1) in 0,001 kWh
1-0:1.8.2(014628.043*kWh) <- Meter Reading electricity delivered *to* client (Tariff 2) in 0,001 kWh
1-0:2.8.2(000000.000*kWh) <- Meter Reading electricity delivered *by* client (Tariff 2) in 0,001 kWh
0-0:96.14.0(0002) <- Current tariff indicator electricity.
1-0:1.7.0(03.877*kW) <- Actual electricity power consumed by client  (+P) in 1 Watt resolution (all phases)
1-0:2.7.0(00.000*kW) <- Actual electricity power returned to net (-P) in 1 Watt resolution (all phases)
0-0:96.7.21(00003) <- Number of power failures in any phase
0-0:96.7.9(00002) <- Number of long power failures in any phase
1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s) <- Power Failure Event Log (long power failures), Format: Timestamp (YYMMDDhhmmssX), duration in seconds.. out for a year in 2016 and 2017?? weird..
1-0:32.32.0(00000) <- Number of voltage sags in phase L1
1-0:32.36.0(00000) <- Number of voltage sags in phase L2
0-0:96.13.1() <- ??
0-0:96.13.0() <- ??
1-0:31.7.0(017*A) <- Actual current consumption
1-0:21.7.0(03.877*kW) <- Actual electricity power consumed by client (+P) in 1 Watt resolution (phase L1)
1-0:22.7.0(00.000*kW) <- Actual electricity power returned to net (+P) in 1 Watt resolution (phase L1)
0-1:24.1.0(003) <- Slave (Gas meter) device type
0-1:96.1.0(4730303137353931323139313130333134) <-  Gas meter serial number
0-1:24.2.1(200529160000S)(05277.053*m3) <- Gas meter time stamp + value
!7667 <- CRC16 checksum of entire telegram (from / to !)
-}
