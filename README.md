# Final Cartridge III 101%

This project is an 21th century improved firmware for the Final Cartridge III, an utility cartridge for the Commodore 64.  It is based on reverse engineering work by Michael Steil and development work is being done by me, Daniël Mantione. For more information about the Final Cartridge III 101% and information about the redesigned cartridge hardware, please visit the [Final Cartridge 101% web page](https://www.freepascal.org/~daniel/fc3/).

## What is contained?

Bank 0 contains the BASIC and editor extensions, the floppy and tape speeder, fast format, the centronics printer driver, and the monitor.

Bank 3 contains the freezer.

## Building

You need the following tools in order to be able to build the firmware:

* [CC65](https://cc65.github.io/)
* [Ucon64](https://ucon64.sourceforge.io/)
* [Petcat](https://vice-emu.sourceforge.io/)
* [Cartconv](https://vice-emu.sourceforge.io/)

All development is done on a Linux host system, therefore you are recommended to build on a Linux system. Building on other operating systems may work, but it not being tested.

Create fc3.bin:

    make

Regression test: create fc3.bin and make sure it's identical to the original ROM:

    make test

## Why?

### Reusing Components

The FC3 contained some great components, like the editor and BASIC extensions, and the excellent machine code monitor.

The source was separated into files with minimal dependencies, so they can be included in other projects.

For example:

    make clean PROJECT=monitor monitor.prg

builds a standalone version of the monitor that can be started with

    sys 16384

The monitor has also been adapted to work with the TED series (C16, C116, Plus/4):

    make clean MACHINE=ted PROJECT=monitor monitor.prg

In this version, the `O` command takes a single hex character argument for the 16 banks. Since `OD` clashes with switching to disk, bank `D` can be reached with `O<SHIFT>D`.

The standalone version can optionally be built with support for illegal opcodes by adding `CPU=6502ill` to the make command like, i.e.

    make clean CPU=6502ill PROJECT=monitor monitor.prg # C64
    make clean CPU=6502ill MACHINE=ted PROJECT=monitor monitor.prg # TED

### Creating Derivatives

The existing code is a great starting point to create an improved Final Cartridge. Some ideas:

* Replace the floppy speeder with a faster one.
* Disable speeder when it detects non-1541 drives or a high-level emulated drive.
* Add a fast directory function.
* Replace PACK and UNPACK with a different engine.
* Adding disk copy functionality.
* Add "clear all memory" functionality (like "Zero Fill" in freezer)
* Monitor: Support illegal opcodes.
* Monitor: Support screen code.
* Monitor: Optimize "OD" mode for speed.
* Monitor: Allow transfering data between computer and drive.
* Monitor: Allow loading and saving below I/O area.
* Bundle a user-defined set of standard $0801-based programs in ROM.

This can be done without overflowing bank 0, since it contains plenty of code that can be removed, either because it is of little use these days (printer.s, tape code in speeder.s, format.s) or it is only there to support other banks (freezer.s, vectors.s, desktop_helper.s).

Please be aware of the [The Final Replay](http://www.oxyron.de/html/freplay.html) project, which has similar goals, but is based on a clean reimplementation.

## Copyright

The original code is (c) Uwe Stahl and Riska B.V. Home & Personal Computers. The assembly version, the comments, the partitioning into separate files and the linker configuration was done by Michael Steil <mist64@mac.com>; this work is in the public domain. The improvements of the Final Cartridge III 101% were developed by Daniël Mantione <daniel.mantione@freepascal.org>, and are in the public domain as well.
