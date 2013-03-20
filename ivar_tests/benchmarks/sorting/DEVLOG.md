

[2013.03.20] {Perf results from IU Mine machines}
-------------------------------------------------

The rather small 4.1M element benchmark runs in 33ms and 9ms on 1 and
4 threads respectively.  3.6X

For 2^25 (33M) elements it takes 0.835s vs. 3.09s.  3.7X.

So there is NOT noticable worse parallel efficiency in
larger-than-cache array sizes at this four-core scale.

The out of place version even with NO memory reclamation takes 1.247s
to 3.702s on 2^25 elements (2.98 speedup).  It uses over 2GB of memory.  
It's surprising it does even that well.



