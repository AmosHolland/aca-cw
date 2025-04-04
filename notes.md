# Memory Hierarchy part 1:

Why have a hierarchy?
- main memory is slow, hundreds of cycles to access
- smaller, on chip can be faster, a few cycles per access
- but we can't fit everything in these
- we can do quite well by exploiting two types of locality
    - spacial locality: programs use data in regions, so it clumps together
    - temporal locality: if you've used data once, you're likely to use it again

Memory hierarchy example
- each cpu core has registers and L1
    - 10s of KiB, 1-4 cycles to access
- each core also has an L2
    - 100s of KiB, 8-12 cycles to access
- then L3 - which is shared between cores
    - 10s of MiB, 20-30 cycles to access
- then main memory
    - 10s of GiB, 200-400 cycles to access
- in general as you go out from the CPU memory gets bigger and slower

Characteristics of caches
- most caches are organsied into cache lines, each of which stores multiple words
    - common to be 32 or 64 bytes long
    - helps for exploiting spactial locality
- allocation policy - where should memory go in the cache
- replacement policy - how do we decide which line to replace with a new one
- how do we keep our copy in the cache, and keep the memory hierarchy consistent when we modify cache

Direct Mapped Cache
- allocation: each line in memory maps to a specific location in cache
    - this usually uses a simple MOD on the addresses in memory to generate
    lines to map to (this works because things will likely be different by a power of 2)
- cache lines also include
    - tag to indicate where in memory it came from
    - a valid bit to indicate if it holds data or not
    - a dirty but to indicate if its been written to
- advantages
    - simple
    - little hardware
    - fast, small, low power
    - easy to understand
- disadvantages
    - suffers from addrsses in memory colliding a lot
    - causes unnecessary data eviction (cache thrashing)
    - highest miss rate
    - difficult to understand behaviour

Fully associative caches
- A line at address A can map to anywhere in cache
- advantages
    - most efficient use of space
    - relatively easy to understand
- disadvantages
    - lots of hardware to implement
    - requires a CAM or similar
    - largest performance overhead, hard to make it go fast
    - how do you decide evictions?? lots of options make it harder

Set associative caches
- line at address A can only map to one set, but then to any of S lines in the set
- set calculated as A mod (N/S)
- advantages
    - best trade-off
    - performs well, not too hard to implement, good efficiency
- disadvantages
    - harder to understand
- most modern caches have 4, 8, 16 way set associativity

Cache misses
- Compulsory: To bring lines into the cache for the first time, these would be misses even in an infinite cache
- Capacity: Cache is not large enough such that some lines are discarded and later retrieved - misses even in a fully associative cache
- Conflict: For set associative or direct mapped cache, lines can be discarded and later retrieved if too many cache lines map to a given set. Also called collision misses or interference misses. These would be misses even in N-way set associative cache
- 4th C is misses caused by cache coherence

Trends as cache size increases
- compulsory misses insignificant
- capacity misses srhink as cache grows
- 2:1 cache rule
    - Miss rate for 1-way set associative cache of size X ~= miss rate for 2-way set associative cache of size X/2

# Memory hierarchy part 2

Replacement policies
- new line mapping to a full set, what do we replace?
- three common options:
    - Least recently used (LRU): if not used in a long time, maybe we don't need it!
    - Least recently replaced (LRR): oldest in the set, might be used still, but is simpler to implement
    - Random: simplest to implement, ideally pseudo random

Maintaining cache consistency
- what happens on write?
    - 1 in 4 is a read, 1 in 10 is a write
    - the current cache is more up-to-date than elsewhere
    - we need to maintain consistency
- two common approaches
    - write-through: write is passed on to next level of memory hierarchy
    - write-back: data is only updated in the next level when the line is evicted

Inclusive vs Exclusive caches
- should level n cache's contents be included in level n + 1
- advantages
    - spatial and temporal cohenerence benefits
    - easy to check if a core has a copy of data, just look in its (e.g.) L2
- disadvantages
    - duplication of data
    - reduced unique capacity of level n+1
    - expensive to maintain for shared caches with lots of cores
- common to be inclusive, trend towards exclusive tho

Cache coherency for multi-core
- if multiple cores access shared array, where does that live?
- whichever core most recently updated data, they have most up to date copy
- when other cores want to access the same data, they need to discover the most up-to-date version
- Chap 5 onwards ahs this (snooping, directory methods)

BC4 Cache
- 35 MiB L3 cache per CPU
- 256 KiB L2 cache per core
- 32 KiB L1 data cache per core
- 32 KiB L1 instruction cache per core
- all caches are 8-way set associative, and write-back
- connected via a ring bus
- L3 is distruvurted arround the ring, slice of LLC per core
- 2.5 MiB per core, 14x2.5 = 35MiB

Cache trends
- On chip cache has grown!
- L3 256 MiB per CPU for one of them!!
- Per core has 32 KiB L1 I and D caches, 512 KiB L2

Common cache optimisation tech
- Hardware prefetch - get close stuff at the same time, complicated tech for this
- Hit under miss - out of order-ish when waiting for a miss to fulfill, 10-20% speedup
- Critical word first - bring the part we care about first
- Merging write buffers - merge multiple updates
- Compiler optimisations - loop interchange, tiling, etc.
- More about these in H&P 78-96

- This will be my main, so hop on the H&P frfr








