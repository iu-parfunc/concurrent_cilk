Devlog for pause/unpause VS pthread context switch microbenchmark

                      {22/2013/2012:}
-----------------------------------------------------------------------------------------------
- On Hive, current cycle counts (in terms of CPU cycles) are:
  * pause/unpause averaged over 5 runs: 9263.2 ticks
  * Pthread context switch averaged over 5 runs: 9216.375383 ticks 

- On Beetle, with no one else on, current cycle counts are:
  * pause/unpause averaged over 5 runs: 3045.6 ticks
  * Pthread context switch averaged over 5 runs: 2911.3980036 ticks


                            **** Future work/ideas ****
- One thing we might want to consider is benchmarking these with arbitrary loads (in terms of data).
  Since in this  [paper][1] The authors talk about the data size versus the amount of time that it 
  takes to do a context switch (pg 2 figure1), seems that it might be in our favor 


                      {23/2013/2012:}
-----------------------------------------------------------------------------------------------

- I was stupid, and was not taking the average of the runs, 
  I went and changed it so that we take the average of calling pause 1000 times. The running times follow:
    * on Hive: 3243 




[1]:http://www.cs.rochester.edu/u/cli/research/switch.pdf
