This project can be built using cargo, the rust crate manager.

Cargo (and rust) can be installed from https://www.rust-lang.org/tools/install.

Having installed cargo and rust, this project can be built and run just by using the command "cargo run" while in the root folder of this project. 

This will bring up a simple repl which can be used to run or debug a suite of test programs (these can be found in the examples directory).

While in the starting repl run:
- "run <program_name>", to run one of the example programs all the way through (just the name of the file is fine)
- "debug <>program_name>", to run an example program in debug mode


While in debug mode:
- the state of the pipeline will be printed every cycle
- "s" can be used to step forward a cycle
- "r" can be used to print the current register state
- "r <n>" can be used to print the nth register 
- "m <n1> <n2>" can be used to print memory from word n1 to word n2
- "pl" can be used to print the pipeline again
- "pc" can be used to print the program counter
- "b" can be used to print the current state of the branch predictor
- "j" can be used to print the current state of the return address cache



