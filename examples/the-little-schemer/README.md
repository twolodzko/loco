
The code example come from *The Little Schemer* book by Friedmann and Felleisen (MIT, 1996).

The unit tests are adapted from the code found in the https://github.com/bmitc/the-little-schemer repository.

I used this code to run a benchmark against MIT Scheme:

```shell
$ hyperfine -m 100 --warmup 1 'gosch run-all.scm' 'scheme --quiet < run-all.scm' '../../loco run-all.scm'  
Benchmark 1: gosch run-all.scm
  Time (mean ± σ):      73.6 ms ±   3.3 ms    [User: 87.8 ms, System: 7.4 ms]
  Range (min … max):    66.7 ms …  81.7 ms    100 runs
 
Benchmark 2: scheme --quiet < run-all.scm
  Time (mean ± σ):     211.9 ms ±   2.8 ms    [User: 165.3 ms, System: 46.5 ms]
  Range (min … max):   207.2 ms … 228.6 ms    100 runs
 
Benchmark 3: ../../loco run-all.scm
  Time (mean ± σ):      14.5 ms ±   1.5 ms    [User: 13.5 ms, System: 1.3 ms]
  Range (min … max):    13.2 ms …  24.1 ms    167 runs
 
  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet PC without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.
 
Summary
  '../../loco run-all.scm' ran
    5.08 ± 0.57 times faster than 'gosch run-all.scm'
   14.64 ± 1.52 times faster than 'scheme --quiet < run-all.scm'
```
