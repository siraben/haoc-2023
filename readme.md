# Haskell Advent of Code 2023
My solutions for Advent of Code 2023 in Haskell.  Here's some goals I
set to make the most out of it:

1. No looking up solutions/discussing with others before completion.
2. No use of libraries outside of the [GHC bootstrap
  libraries](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/index.html)
  for the solutions.
3. If time permits, use
  [Criterion](https://hackage.haskell.org/package/criterion) to
  benchmark and improve solutions.
4. No unsafe Haskell.

The reason for (2) is that many online competitive programming sites
that support Haskell (CodeForces, Google Code Jam) do not have
libraries beyond the bootstrap list.  Plus, there's already a wealth
of competitive libraries in there, such as `bytestring`, `text`,
`parsec`, `containers` (which contains maps, sets, int sets, graphs,
sequences) and more.

I might do a full writeup after my semester is over, but here's my
rough procedure on how to tackle the problems.

1. Write the most naive thing that could possibly work.  If it works,
   submit the answers!
2. Focus on algorithmic improvements.  I use a combination of
   techniques:
   - use equational reasoning to fuse folds, traversals
   - use more efficient data structures
3. Focus on empirical improvements.  I make heavy use of Criterion,
   though [AutoBench](https://github.com/mathandley/AutoBench) seems
   interesting.
   - manually inlining helper functions and equational reasoning, tail
     recursion
   - convert `foldr` to `foldl` when possible
   - using strict versions of functions, bang patterns
   - explicit type annotations
   - faster types: `Int` instead of `Integer`, `ByteString` instead of
     `String`, `Sequence` or `Vector` instead of `List`

Of course with (3) one could continue shaving off more and more time,
though these heuristics in practice have given me most of the gains.

## Writing Haskell for AoC quickly
AoC is all about solving the problem quickly via any means, so

> a naive (but possibly inefficient) solution that is quick to write
> and produces the answer is better than an optimized one that is slow
> to write

From this principle, it informs my choices on how to write Haskell
quickly for the initial solve.

- put everything in main, variables/expressions used more than once go
  in a `let` expression, use a short, random identifier unless you can
  think of a good one within 3 seconds
- for mapping over lists, use `<$>`, but if you need to filter, use
  `let` expressions or iterate over a cartesian product of lists, list
  comprehensions/monadic syntax is better
- never discard parts of input even if it makes part 1 faster to
  solve, since almost always part 2 will use that information and
  you'll have to adjust the `String -> data` step again
- avoid explicit recursion when possible, or at the least use tail
  recursion with a function named `go`
- print intermediate results as you process data, especially if the
  transformation is complex, so `do { let a = f x; print a; let b = g
  a; ... }` over `do { print (f (g x)) }`
- if using the state monad (should be a last resort), use a record for
  the state

## Best benchmarks so far
<details>
<summary>CPU details</summary>

```
Apple M1 Pro, 32 GB RAM, 10 Threads
```
</details>

### Day 1
<details>

```
benchmarking day1/part1
time                 119.3 μs   (119.2 μs .. 119.5 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 119.4 μs   (119.2 μs .. 119.5 μs)
std dev              532.6 ns   (424.8 ns .. 700.4 ns)

benchmarking day1/part2
time                 732.2 μs   (731.3 μs .. 733.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 732.5 μs   (731.1 μs .. 733.9 μs)
std dev              4.863 μs   (3.902 μs .. 6.437 μs)
```
</details>

### Day 2
<details>

```
benchmarking day2/part1
time                 1.049 μs   (1.042 μs .. 1.055 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.048 μs   (1.046 μs .. 1.051 μs)
std dev              8.988 ns   (7.142 ns .. 11.40 ns)

benchmarking day2/part2
time                 1.894 μs   (1.890 μs .. 1.898 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.894 μs   (1.891 μs .. 1.900 μs)
std dev              13.86 ns   (7.967 ns .. 22.19 ns)

```
</details>

### Day 3
<details>

```
benchmarking day3/part1
time                 1.613 ms   (1.611 ms .. 1.617 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.635 ms   (1.630 ms .. 1.640 ms)
std dev              18.89 μs   (15.61 μs .. 23.14 μs)

benchmarking day3/part2
time                 776.8 μs   (771.6 μs .. 783.0 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 774.0 μs   (772.1 μs .. 777.1 μs)
std dev              8.316 μs   (6.077 μs .. 12.97 μs)
```
</details>

### Day 4
<details>

```
benchmarking day4/part1
time                 5.101 μs   (5.094 μs .. 5.109 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.112 μs   (5.108 μs .. 5.116 μs)
std dev              12.92 ns   (10.65 ns .. 15.63 ns)

benchmarking day4/part2
time                 15.88 μs   (15.85 μs .. 15.91 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 15.91 μs   (15.89 μs .. 15.92 μs)
std dev              37.82 ns   (24.82 ns .. 55.72 ns)
```
</details>


### Day 6
<details>

```
benchmarking day6/part1
time                 32.44 ns   (32.38 ns .. 32.50 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 32.48 ns   (32.45 ns .. 32.52 ns)
std dev              117.6 ps   (87.24 ps .. 181.8 ps)

benchmarking day6/part2
time                 11.49 ns   (11.46 ns .. 11.52 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.46 ns   (11.45 ns .. 11.47 ns)
std dev              43.81 ps   (31.63 ps .. 59.76 ps)
```
</details>

### Day 7
<details>

```
benchmarking day7/part1
time                 2.707 ms   (2.700 ms .. 2.718 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.703 ms   (2.700 ms .. 2.708 ms)
std dev              12.46 μs   (7.554 μs .. 19.53 μs)

benchmarking day7/part2
time                 56.08 ms   (55.94 ms .. 56.22 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 56.04 ms   (55.98 ms .. 56.11 ms)
std dev              123.4 μs   (95.78 μs .. 155.8 μs)
```
</details>

### Day 8
<details>

```
benchmarking day8/part1
time                 890.1 μs   (888.2 μs .. 892.4 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 888.9 μs   (888.0 μs .. 890.5 μs)
std dev              3.874 μs   (2.642 μs .. 6.049 μs)

benchmarking day8/part2
time                 5.338 ms   (5.331 ms .. 5.345 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.337 ms   (5.333 ms .. 5.345 ms)
std dev              16.39 μs   (9.575 μs .. 28.87 μs)
```
</details>
