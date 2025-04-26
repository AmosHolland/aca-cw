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

# Trends #1

Moore's law
- is dead, lol  
- 2000-2010 still held, for both advanced chips, and cost-performance chips (in phones etc.)
- Is not fully dead, but is slowing donw
- now doubling every 3 years instead of every 2
- still similar in big and small chips

Other trends
- no more free lunch
- can't just keep adding transistors and seeing higher clock speeds any more
- (clock speed is not increasing at the same rate as transistors, has levelled out)
- Power usage also increases with transistors, which isn't sustainable, not for conusmer use, not for cooling reqs
- So power usage has capped at ~100W
- which caps clock speed

- outside of clock speed plateau, instruction level parallelism also caps performance
- at a certain point there's nothing you can do about this
- this lead to the rise of multi-core parallelism


Tech trends
- Transistors improve ~55% per annum
- Memory capacity improves ~49% per annum
- Over time we will end up with less memory per core
- Memory bandwidth even slower, ~30% per annum
- Over time memory will get slower relative to how quickly we can work with it
- Memory Latency improves less than 30% per annum (speed of light bound)

Power-limited regimes
- power consumption has an upper bound, may reduce over time but eh
- Power consumption correlaees with clock frequency, number of trasistors, number of cores, Voltage^2
- If power has an upper bound then performance per watt = performance, have to care about being power efficient

# Trends 2

What do we do now?
- Lots more cores on-chip
- Core designs will be similar / the same, we will just add more
- Power use cannot increase exp, so:
    - Chip voltage cannot go down (0.7V threshhold with sillicon)
    - Clock speeds will ~decrease
    - Memory bandwidth per core will ~decrease
    - Memory per core will tend to decrease
    - Dark silicon challenge (more transistors than we can power, what do?)
- Different types of cores
    - Heterogeneous computing
    - A few heavyweight cores, with a lot of lightweight GPU cores
    - Liekly to go same way as vector units (mainstream)

Future architectures
- Hundreds of thousands of cores per chip
- lower clock speeds
- Highly integrated (mainstream)

- AMD's first fusion chip was 2010
- Everyone is doing it now

Mobile supercomputing
- Almost all mobile devices are very parallel, and need parallel programs
- Very GPU very integrated very wow
- 8 way superscalar
- 7 integer, 4 FP vector units, 2 branch, 4 mem
- Loads of cache
- Similar single thread performance to desktop CPUs from intel and AMD
- Apple can do this because their vertical, can make sure their firmware and software takes advantage of these features

Disruptive tech
- mobile tech disrupting the laptop and desktop space
- mobile has more money, funds cutting edge development

Modern ARM supercomputer node
- 48 cores
- 2.7 T FLOP/s
- 1 TB/s mian mem bandwidth
- ~170 Watts
- High speeed interconnect
- 512-bit wide vectors
- Fugaku #1 supercomputer uses it
- 8.7B transistors, 7nm each

GPGPUs
- 1990s GPUs appeared
- 2003 became programmable
- 2006 dropped fix functionality, we code CUDA etc.
- 2009 first truly general purpose GPU

- Specialised designs for other single tasks (e.g. bitcoin mining lol) have cropped uo
- Some chips being done for ML specifically
- These will outperform general purpose chips
- How big does the market need to be to justify this?

Pie in the sky stuff
- 3D semiconductors (these are here)
- Spintronics
- Carbon nanotube transistors (single atom??)
- Post silicon tech
- If we don't get these Moore's law will be dead ~2030

Overall
- Transistor counts will keep going up, but this will bring challenges
- On chip-parallelism, and depths of memory hierarchies will increase
- Increase heterogeneity of processors also expected

# ARM case study
Embedded space
- we'll look at real arch features, highlighting interestign approaches
- Mainly nor processor specific
- Not application specific
- Micro-arch focused
- Not exhaustive

- ARM generally operate in embedded space

Arm history
- Founded UK 1990, Acorn Computers, Apple Computer, VLSI tech
- Sell IP rather than hardware - main market is lowe power, general purpose embedded processors
- based off of ARM 7/8/9 instructions sets
- ARM7 and ARM9 cores are main products, used extensively in hand-held comupting devices
- DEC produced StrongARM, Intel did XScale, today beeg licensees are apple, amazon, samsung, huawei

Arm arch family
- Arm has range of arch designs and implementations
    - 16 32-bit registers, R13-16 have special purposes
    - ARMv4 arch, ARM7 processors
        - 3-stage pipeline
        - no (or small) cache
        - < 100 MHz
    - ARMv5 arch, ARM9
        - 5-stage pipeline (F, D, E, Mem, Writeback)
        - cache
        - more tha  100 MHz
    -ARMv7, v8 are more pipelined, more HPC features (e.g. SIMD, DSP) over 1 GHz

- Does a lot in each stage to increase performance
- Slow but using very little power

- Armv6 added CFPv2, Jazelle
- 7 added SIMD, Thumb-2, TrustZone
- 8 added 64 bit, advanced SIMD, compatible with ARMv-A

Predicated (Guarded) Execution
- Common arch feature
- 32-bit status register, called CPSR (R16)
- ALU based instructions update CPSR, supressed using a flag
- 32-bit inst had 4-bit code which corresponds to a predicate based on flags in CPSR
- Using these we can do predicated execution
    - when inst is fetched, predicate evaluated
    - If result is true, instr continues to be decoded and executed, otheriwse it is discarded, made into a NOP
    - 1 cycle to conditionally exclude an instruction, 3 cycles or more to hadnle a branch
    - Good for short branches
- Discarded instructions take up pipeline slots which is a waste
- So doing a longer branch this way is not worthwhile

Scaling operands
- ARM doesn't have a dedicated shift instruction, instead all arithmetic inst can scale their second operand
- Uses a shift unit inline with ALU
- Very useful as a scaled addressing mode
- Also useful to do multiplication by a constant (e.g. x * 17 can become a shift of 4 + x)
- General mult takes 4 cycles, but add and shift is 1 cycle

Arm branch prediction
- ARM7 and ARM9 had no branch prediction (always predicting not taken)
- ARM 11 (8-stage)
    - Dynamic branch prediction
        - Branch target address cache is 128 entries, and is directly mapped cache structure of virtual addresses
        - Each entry in BTAC stores branch address, branch target address, prediction using a 2-bit saturating counter (strongly taken, weakly taken, weakly not taken, strongly not taken etc.)
        - Once a branch is allocated in BTAC, only evicted by a capacity clash, or due to address mapping conflicts
        - If a BTAC hit occurs then branch has no pipeline stalls
        - If this is very good it kinda beats predicated instructions
    - Static branch prediction
        - with a return stack
        - all unconditional branches predicted as always taken
        - All conditional backward branches predicted as always taken, conditional forward as always not taken
    - Why both?
        - when a branch is first encountered we don't have a BTAC, so follow static prediction

Arm Thumb
- Arm uses 36 diff instruction formats, all are 32-bits long
- fixed length makes decoding easier, but code density can suffer, and bandwidth is expensive
- To combat this are uses the thumb extension
- Essentially this is a processor mode, programmer can switch between thumb and arm modes using the bx branch instruction, indicated by T flag in CSPR

- We want to compress instructions then
- We want to reduce by a consistent amount, e.g. half, or reduce by a number of bytes
- This is to keep instructions aligned with cache lines etc. (half is best)
- How can we do that for, e.g. a 3 operand inst, or a load/store inst
- features have to be restricted
    - A sub-set of registers is available (need less bits to specify)
    - A sub-set of features (e.g. get rid of predicated execution)
    - Some inst not available (use less bits in the opcode)
    - Each can then be encoded with 16-bits

- Most ISAs use similar tech (MIPS has MIPS16E)
- IB CodePack actually compresses instructions (lossless), then decompressed at runtime

- Thumb2 allows mixing 2 byte and 4-byte (so where 4-byte is needed for perf / makes better use of space we can use it)

- no longer 3 operand register to register
- instead overwrite one of them
- smaller consts in immediates as well
- don't get a perfect double in code density, sometimes need to do multiple instructions to do the same thing

- if you care about code density, use thumb to gain 30% to 70% (thumb vs thumb2) code size reudction
- if you really need the features, use Arm mode

- impacts energy efficiency
    - better code desnsity means less bytes to move around
    - greater proportion of working set held in less on-chip memory (which is power hungry)
    - And mem transactions less expensive, fewere of them, more can be on-chip

- Choice comes down to a mixture: perf critical sections can use Arm-mode, size critical can use Thumb mode (or use thumb 2)



Jazelle - optimising for running JVMs
- Java code runs on a JVM, which can cause perf issues
- Self-modification can help this
    - JVM monitors hot-spots
    - hot-spots are aggresively re-compiled, modified so more-efficient versions replace the hot-spots

- Some some options for optimising
    - Hosted compilation: re-compiled program still Java (just more efficient), and executed via the JVM
    - Native compilation: recompiled code is executed by hardware
- Jazelle uses ideas from the second of these at a hardware level

- Jazelle mode enter and exit with bxj branch
- J flag in CPSR
- instructions are no longer Arm or Thumb, but native Java bytecode
- Bytecode expanded into one or more Arm instructions by the decoder
- 140 go into hardware, 94 are emulated
    - 95% of executed instructions are native
- Some GPR will need to have special roles in Jazelle mode

- Advantages
    - bytecode becomes very low overhead vs true native Java
    - Bytecode instructions executed via Jazelle can exploit the instruction cache
    - Much lower start-up penalty compared to JVM
    - Translation between bytecode and native instructions is dynamic, no memory overhead

- 64-bit instr set means benefits of Jazelle have shrunk
- Jazelle not used as much anymore

Arm64
-  How to change 32-bit to 64
    - registers get bigger
    - get more registers (more room to address them)
    - SIMD inst set grows in number of registers
    - FP becomes guaranteed
    - SIMD becomes guaranteed
    - Bigger physical memory support as well (40-bit -> 48-bit)
    - Simplified the instruction set
        - remove predicated exec
        - no thumb
        - no load multiple / store multiple CISC-y instructions

- 64-bit can be a mode, so still compatible with old code
- currently moving away from backwards compatibility (if you don't want it, don't implement it)

Further changes
- 2014 added atomic read/write
- extended SIMD ISA
- 2016 Half precision support, RAS, Scalable Vector Extension
- 2016 Pointer auth, SIMD complex number support, Improved JS datatype conv support
- 2017 Crypto, Dot product inst
- 2018 - memory tagging, branch target indicators, RNG
- 2019 General mat mul, Float16 support (for deep learning)

Approaches are not unique
- NEON is good for SIMD etc.
- TrustZone is interesting for security
- Arm is good example of an evolving arch (which has evolved over time)

Features are driven by a range of things
- Generic issues, compatibility, application demands, issues of manafacture, marketing


# Arm SIMD / SVE

Features we haven't seen yet
- coprocessor interface for communicating with tightly-coupled blocks on a System on Chip
- extensions for SIMD/vec instructions
- security stuff

Coprocessor interfaces
- Common for processors sold in IP form to include configurable interfaces as a way to talk to other blocks on a SoC

- Arm provides an interface to 16 coprocessors
- Several used for current extensions - debug, SIMD/VFP, some reserved for future use too
- 0-7 available for "vendor specific features"

- 3 types of operations
    - initiate co-processor data processing op
    -  transfer data between GPR and co-processor reg
    - load/store directly between co-processor reg and memory

- Coprocessors execute same stream of inst as processor, ignoring all but their own instructions
- coprocessor isntructions that cannot be executed by any coprocessor hardware cause an undefined instruction exception


SIMD/Vector extension
- many processors use these to exploit data parallelism
    - implemented as coprocessor extendsion in Arm
- usually in the form of SIMD instructions
- sometimes more DSP like (for embedded systems)
    - multiply accumulate, auto-increment / decerement load and store, rounding / sturating arithmetic, zero overhead loops
- Arm had two SIMD/Vector extensions
    - NEON: integer SIMD for media applications
    - VFP: Vector Floating Point instructions for 3D games

SIMD
- Multiple data items grouped into vectors
- Items processed in lanes
- e.g. adding together two 32-pit pixels with 4 4-bits components
- instead of doing parts separate, add separate parts at the same time independently
- in one cycle
- better use of memory, ALUs
- in 32 bit example there were 4 lanes

Arm SIMD/VFP instruction set
- typical of embedded proc
- 64 or 128-bit SIMD
    - supports packed 1, 2, 4, or 8 byte data items
    - Integer, fixed point, and floatiing point are supported
        - fixed can be useful for particular signal processing apps
- used for
    - integers, multimedia and signal algos
        - video encode / decode, 2D/3D graphics, voice processing, image processing
    - VFP
        - Auto controls, 3D graphcis, game consoles etc. etc.
- have a separate coprocessor pipeline and register file
    - adds 16 or 32 64-bit reg
    - can also be viewed as 16 128-bit registers
- only have it when you need it
- Adds support for unaligned data accesses
- powerful load/store instructions with interleave capability
- adds an extra status reg, FPSCR

SIMD inst mnemonics
- Insts can operate on different data types specified in instruction encoding
- software indicates size required by appending a suffix to instruction mnemonic
- Number of elements operated on is indicated by the specified refister size
- e.g. VADD.I16 Q0, Q1, Q2
- i.e. the things in Q0, Q1, Q2 are 16-bit integers
- some instructions can have different input and output reg
- e.g. VMULL.S16 Q0, D2, D3
- four 16-bit lanes in parallel produce 32-bit products in a 128-bit destination

Common SIMD instructions
- Conversions
- Comparisons
- Arithmetic
- Newton-Raphson reciprocal estimation & step
- SQRT, reciprocal SQRT
- Saturating arithmetic (videos)
- Polynomial arithmetic
- Arm advanced SIMD allows MP3 audio decoding on CPUs running at just 10MHz, and GSM adaptive multi-rate speech codec at just 13 MHz

Using ISA extensiosn via intrinsics
- uses high level language headers to specify SIMD stuff
- using the right compiler these can go straight into SIMD, no need for the compiler to guess
- C++ compiler can let you do similar with operator overloading

- Some compilers have automatic vectorisation, though it may need hints etc. to do this
- e.g. __restrict keyword, const keywords etc.
- and some other operations which ensure that numbers are of certain shapes
- just there to give hints to the compiler


# ARM SVE
- SIMD has fixed widths encoded in instructions, and also in user code
    - can expand the architecutre to have bigger ones, but old code may not be able to use it (using a specific reg)
- traditional vector architectures let you haver variable length vectors
- Arm added SVE to address this (scalable vector extension)

- vectors 128 - 2048 bits, incremented in 128 bit chunks
- vector length agnostic
- nice compiler target
- extensive support for predication etc.
- can vectorise loops that are not exact multiples of vector width without the need for peel loops
    - i.e. extra info for different stages of loop are no longer needed
    - avoid these with predication, use same length for all, just ignore empty lanes

- A bunch of new Vector registers, DP & SP, all vector max len x 128 bits
- allow DP, SP floating
- 64, 32, 16, 8-bit integers

- Scalable predicate registers
    - lane masks (info for lanes), 16 x len in length (16 bits per lane)
    - also somespare predicate registers
    - first fault register

- scalable vector control registers
- e.g. vector length, exception and privelge level

- Predicates in SVE
    - crucial to SVE loop control, reduce overhead for managing vectorsied loops
    - use NZCV condition flags
        - N - set if first element is active
        - Z - set if no element is active
        - C - set if last element is not active
        - V - scalarised loop state, else zero
    - can have conditional branches based on these
        - e.g. branch if none (tells you when to stop a loop)

    - C helps know if full or not


- Vector partioning and speculation
    - vector partitioning uses predication to allow speculative vectorisation
    - operate on a partition of safe elements in response to dynamic conditions
    - partitions inherited by nested conditons and loops

    - uncounted loops with data dependent exists
        - ops with side effects following a break must not be architecturally performed
        - operate in a before-break vector partition, then exit loop if break was detected
    
    - speculative loads
        - Vector loads required to detect break condition might fault on lanes following it
        - operate ob before-faul partition, iterate until break is detected

    - vector length agnostic is a special case of partitions

- speedup from SVE is only in particular cases

- Arm TrustZone
    - adds secure and non-secure states
    - and a secure monitor mode for switching between the two


# VLIW 1

VLIW
- coined in 1980s, but first happened in 1970s
- ILP processors have two methods - parallel operation, and pipelined operation
- most of our things exist in both

VLIW Basics
- exploits ILP and uses multiple EUs in parallel
- Big differences
    - how instructions are formulated
    - how scheduling is carried out

VLIW Instr formulation
- in superscalar, multiple instructions per cycle, but from the same sequential instructions stream
- VLIWs have a single long instruction word per cycle (100s of bits long)
- Each inst has multiple control fields, one per EU in target system
- Length of VLIW depends on
    - number of EUs available
    - number of bits to control each EU
- 5-40 EUs per VLIW processor, 16-32 bits for control, VLIW instruction 100-1000 bits long
- No real decode / issue stage

VLIW scheduling
- essentially, VLIW processors are scheduling instructions statically
- superscalar schedule dynamically
- don't need all the logic to discover and exploit ILP at runtime
    - don;t need ROB, reservation station, dependency checking logic etc.
    - Should lead to higher clocks speeds, lower power, cheaper processors
- depend on the compiler to schedule instructions in advance
    - Requires knowing all the details about the microarch
    - requires removing / avoiding all dependencies
    - Also relies on all instruction latencies being fixed and constant

VLIW drawbacks
    - Latencies cannot be guaranteed in advance always
        - branches (predicted or mispredicted)
        - loads and stores (cache hit or miss)
        - long instructions with early outs (divide, etc.)
    - Compilers are rocket science
    - Insts need a field for every EU, but if we can't usefully employ every EU on each cycle, we need to include NOPs in VLIW instruction, wasting space and bandwidth
    - Reply heavily on degree of ILP in programs
        - Fisher needed a new way to schedule instructions to solve this - trace scheduling (can schedule across basic block boundaries)

# VLIW Itanium

- Intel wanted their own architecure, clean ownership and IP

- Hence Intel did IA-64 (EPIC) arch
    - introduced in 2001 with Merced
    - initially co-developed with HP
    - Designed for enterprise-class systems
        - reliability, availability, serviceability
        - 64-bit addresing
        - Big caches, lots of memory bandwidth
- IA-64 had a reputation for being
    - Late
    - Underperforming
    - Expensive
- Itanium referred to as "Itanic"

- Part of the problem was VLIW arch
    - the compilers needed were basically impossible to write
- revised in 2006 (Montecito)
    - Dropped support for x86
    - added support for coarse-grained, two-way, Simultaneoous multi-threading
    - hardware support for virtualisation

IA-64 Arch
- 64-bit from the ground up
- Register rich
    - 128 integere regs (64-bit)
    - 128 FP registers (82-bit)
    - 64 predicate registers (1-bit)
    - 8 branch registers
- Explicitly parallel
    - 128-bt VLIW
        - 3 inst in each VLIW
    - Predication, speculation, branch prediction
        - all under compiler control
- dual issue, in-order (mostly) for VLIWs

Explicitly parallel instruction computing
- Each word constains multiple instructions
- instructio scheduling is static
    - ILP discovered at compile time

Microarch Overview (IA-64)
- 30 EUs, in 11 groups
    - each could do 1 inst per cycle, fully pipelined
- Groups
    - 6 General purpose ALUs, 2 integer, 1 shifter
    - 4 load/store
    - 6 multimedia, 2 parallel shifts, 1 parallel mult, 1 popcount
    - 2 82-bit FP MACs, 2 32-bit SIMD MACs
    - 3 branch units
- Compiler groups instructions into VLIW
    - ideally in bundles that can be executed in parallel
    - Max 4 FLOPS per cycle

Memory
- 16KByte inst and dara caches
- 1MB L2 instruction cache
- 256KB L2 Data on chip
    - some logic to do simple arithmetic
    - enables atomic semaphore operations
- 24MB on chip L3 unified cache

Actual VLIW
- 128 bits
- 3 41 bit insts, 5 bit template
- template contains compiler grouping info about parallelism between contained instructions
- compilers use template to tell CPU which inst should be issued together
- tempalte also contains end-of-bundle bit. This bit lets compiler indicates whether or not the bundle is finished after first 3 inst, or if CPU should chain more bundles of inst

Template
- 5-bit template field within each bundle
    - presence of any stops associated with bundle
    - execution unit type required by each inst in the bundle
- bundle formats can specify only a subset of all possible combinations of instruction types and stops
- so only certain instruction bundles can be used

# VLIW Itanium 2

Simple instruction format
- Each 41 bit inst is
    - 3 reg addresses (7 bits)
    - predicate register (6 bits)
    - opcode (10 bits)
    - 4 bits define type of instruction

- predicates specify 64 combinations of predicates, helps eliminate branches
- critical to replace branches with predicated code for good performance

Predicates 
- Conditional branches implemented as predicated unconditional branches
- Predicate registers are set using compare or test insts
- compare inst specifies one of ten diff compariosn tests and two predicate registers as destinations
- predicate registers will be written with
    - result of comparison (0 or 1), and the complement, or
    - some logical function that combines the two testsm and the complement. allows multiple comparisons with one inst

Instruction latencies
- all instructions are fully pipelined - one per unit per cycle
    - integer ALU operations, 0 cycles
    - FP arithmetic, 4 cycles
    - integer load, 1 cycle
    - FP load 5-9 cycles (bypass L1 cache)
    - correctly predicted taken branch, 0-3 cycles
    - mispredict, 6 cycles
- latency is defined as smallest number of intervening instructions between two dependent instructions

Not strictly in order
- why?
    - some are always unpredictable, cache misses, mispredictons will always happen
    - IA-64 allows unstalled instructions in VLIW to continue on through pipeline
    - needs scoreboarding
    - and we're back at dynamic scheduling!!!
        - oops, strict adherence to in order was too bad for performance

Software pipelining
    - important for VLIW
    - software pipelining reorganises loops such that each iteration in pipelined code is made from instructions chosen from different iterations of the original loop
    - compiler's scheduler interleaves instructions from different loops, separates the dependent instructions that occur within one iteration
    - Essentially re-rolled loop unrolling

IA-64 pros
    - easy decoding, shorter pipeline
    - in-order issue and exec -> dispatch hardware simpler -> shorter pipeline, fewer transistors
    - conditional branches and program wide scheduling -> more ILP
    - 128 registers + load store model -> less memory / cache accesses (compared to x86)

IA-64 cons
    - no OoO -> cache miss and stalls more costly
    - 128 reg & bundle / group system -> instructions are much larger than x86
    - Code density suffers: x86 24 bits pert inst, IA-64 needs 42.6 (128/3)

IA-64 supercomputers
- Thunder
- Columbia

IA-64 progression
- no.3 in 2010
- quad core
- 65nm (behind the time)
- 24 MB/s on-chip caches

- kittson (32nm) (no. 4) 2017

- Itanium stopped shipping July 29 2021

- even Intel doesn't get its way
- "Gotchas" kill the upside
- hard to establish new architectures
- VLIW hard to apply to general purpose computing, but successful in specialist areas
    - embedded, or DSP
- Texas Instruments has these, AMD and Arm have used VLIW in GPUs


    






 







