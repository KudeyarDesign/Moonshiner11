# Moonshiner11
 Moonshiner 11: DEC PDP-11 fast CISC core for Retro-computing

DEC PDP-11 fast CISC core for Retro-computing and proof-of-concept about architectural superiority of classic
CISC core architectures relatively to RISC cores from 1980+ years

This core is about 100x faster than classic predecessors :
One order of magnitude due to microarchitecture improvements
One order of magnitude due to clock frequency

Microarchitecture:
7-stage In-order Supercalar Pipeline

Fast Hardware Instruction Decoder (up to 4 microinstruction per clock), opposite to classic microcode decoders
or combined PLA/microcode decoder from J-11

Up to 4 micro instructions per clock:
Memory read
Address register predecrement/postincrement
ALU operation
Memory write

External Bus - Altera Avalon, QBUS through Avalon-to-QBUS bridge

Separate Data and Instruction Caches (D-Cache & I-Cache)

Interrupt Controller

Hardware Debug Unit

Trace Unit

Instruction Timing Unit for compatibility mode with classic (old) processor implementations
(emulate different instruction delays and slowdowns pipeline accordingly)

Most instructions executed during 1 cycle:
Register-Register      ADD R0,R1
Memory with offset     ADD R0, 2(R1)   ,  ADD 2(R0), R1    
Predecrement/Postincrement memory   ADD R0, (R1)+   ,  ADD (R0)+, R1

Maximum instruction execution time - 7 cycles , memory indirect mode for both operands
(includes 4 memory reads and memory write)
ADD @2(R1), @4(R2)

Maximum FPGA clock frequency - 90-100 MHz for venerable Altera Cyclone III (Quartus 11.0, Balanced Mode synthesis configuration)
 
Some tests for early version this core from Sergey Vakulenko (MIPS Inc.)  - https://github.com/sergev/bk0012

Directory structure:
/rtl - source code
/doc - pipeline structure

Author:

Alexey Shistko     alexey@kudeyar.com
