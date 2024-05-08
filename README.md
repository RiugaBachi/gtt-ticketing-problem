# GTT Ticketing Problem

"Find all combinations" branch!

Original jsfiddle: https://jsfiddle.net/m2rz06te/

# Assumptions

1. Users prefer single-row adjacent seats (Condition 1) wherever possible.  This is not explicitly stated, but it is an observation I've made in real life. Perhaps it's just me, but seating the group on the same row is generally preferred even if you're marginally farther from an end.
2. If two data rows in the CSV declare duplicate seating, the first (topmost) row is kept and all subsequent duplicates are discarded. A duplicate is defined as a seat with the same section + row + number; different pricing is irrelevant.
3. Some CSV rows declare, for instance "startingSeat 1, ..., even"; when the consecutivity of a row is even, for instance, we only expand out the even seat numbers declared in that row. So in the case of "startingSeat 1, endingSeat 1, even", the whole row would be discarded. For "startingSeat 1, endingSeat 3, even", we would only expand out a single seat with number=2. This is in line with the common understand of how even/odd seating works in actual theaters and arenas.

# Limitations

1. Row size is unknown; as such we can't truly measure for the 'closest seat from [both] ends'. As such 'closest to the ends' for the purposes of this solution is defined as "being as close as possible to seat number 0". There are ways to extend this to measure from both ends if we do know row length however, and I'm happy to discuss it.

# Optimization

I don't believe in premature optimization. This algorithm is for business, and business is rooted in reality.

I have benchmarked this version of the algorithm against models of AT&T Stadium (regarded as the largest in NA) and Yokohama Arena (no particular reason, just the largest that I've been to personally), and overall performance seems reasonable for the purposes of a webapp.


A large part of this solution relies on culling entire sections based on cheap criteria. I took inspiration from how collision detection is implemented in most games (hierarchy of boxes/cubes).

The advantage of this optimization is we can easily extend it to further subsect the individual sections, much like in collision detection, if in practice they become too large and performance is unacceptable.

Worst case, profile for hotspots, and optimize accordingly.

(Speaking of subsecting, Limitation #1 could potentially be implemented by subsecting a section in half vertically, and running the same algorithms on each subsection in an alternating fashion. We could then measure closeness to the ends as the inverse of distance from the center of a section. It seems to be a concept that's cropping up in more than one way!)

# Benchmarks

The models I used are as follows:

ATT Stadium:
- Capacity: 100k
- Based on some rough estimation let's say the average section holds 500 seats
- This would imply there are 200 sections total (this seems roughly accurate)
- We can roughly 'cube' the sections and say it's 20 rows of 25 seats, which seems realistic.

Yokohama Arena:
- Capacity: 17k
- Eyeballing the floorplan, I chose 20 rows x 25 seats x 34 sections.

Test machine specs: 5800X3D, 32GB DDR4 RAM 3200mhz

My memory never shot past a marginal ~0.06GB increase while running the benchmark so I didn't see the need to profile for space leaks.

The benchmark data was randomized per run; namely: 
- Price (randomized within a set range)
- Available of seats (a fixed % were unavailable per run)

For AT&T:
- All 3 benchmark scenarios ran between 134 and 151ms per iteration

For Yokohama:
- Both benchmark scenarios ran on the order of ~21ms per iteration

Assuming the intended usage is in a web application, this is an acceptable wait time as much of the web runs on a much slower timescale and will likely feel indistinguishable to end users, even in the worst case (AT&T Stadium). Worst case we can add an async loading widget or somesuch.

There are ways we can optimize further of course (subsecting sections); we can even profile for specific hotspots and see what we can do there. But overall memory usage seems good and I feel confident that this will suffice for an MVP.

# Testing

I have not yet implemented a test suite for this; but if I were to approach it in the future I would:

1. Property test that the seat combination is always within budget
2. Property test that the seat combination is always equal to the party size
3. Unit test simple adjacent and piggyback cases, as well as certain edge cases (below)

# Noteworthy Edge Cases

Compilation of some noteworthy edge cases that are worth unit testing later on.

```
<Select 5>
..ooo..ooo.
.ooo....ooo

<Select 6>
ooooo
ooooo

<Select 5>
..oooooo..
.ooo..ooo.

<Select 6>
.oooo..oooo.
..oooooooo..
```
