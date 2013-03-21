

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

Switching to 64 bit
-------------------

Things get quite a bit slower.  For four threads, 2^25:

 * inplace - 0.92s
 * outofplace - 1.77s

Wow, the gap is up to almost 2X!  And it holds even at 2^24.
Why is the out-of-place version so disproportionately affected by switching to 64 bit ops?

[2013.03.21] {Wait... were those prev numbers right?}

Now it's much slower 2^24 = 16777216... is taking over two seconds.
WAIT... is this a big slowdown with the concurrent/cilk runtime?
Irrespective of which version of the benchmark?

 * inplace (1.594147 s)
 * outofplace (2.202818 s)
 * ivars (2.308277 s)

 * inplace plain cilk: (1.589038 s)    [WRONG]
 * outofplace plain cilk: (2.206963 s) [WRONG]

No apparent difference on that run... I feel like somethings weird
about the build state.  

Wait... it IS THE CONCURRENT CILK RUNTIME.  I must be catching the
concurrent Cilk version EVEN in my "plainicc" builds.  If I BREAK the
concurrent cilk build... then I get a proper (plain icc) run for 2^24:

 * inplace TRUE plain icc (0.439702 s)  [3X BETTER THAN OTHER RUNTIME]
 * outof place, TRUE plain icc (1.000862 s)

Now that's consistent(ish) with the above.

