:- style_check(-singleton).
:- use_module(sampler).

0.1::burglary(true); 0.9::burglary(false) <--- .     % burglary happens 10%
0.2::earthquake(true); 0.8::earthquake(false) <--- . % earthquake happens 20%

1.0::alarm(true) <--- burglary(true), earthquake(true). % alarm always goes off for both
0.8::alarm(true); 0.2::alarm(false) <--- burglary(true), earthquake(false). % alarm 80% when only burglary
0.8::alarm(true); 0.2::alarm(false) <--- burglary(false), earthquake(true). % alarm 80% when only earthquake
0.1::alarm(true); 0.9::alarm(false) <--- burglary(false), earthquake(false). % alarm misfires 10%
