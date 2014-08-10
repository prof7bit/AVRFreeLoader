#AVRFreeLoader

This aims to become a free implementation of the proprietary AVRootLoader 
protocol originally invented by Hagen Reddmann and used in his legendary
AVR bootloader.

The bootloader assembly source code itself was released by Hagen as freeware,
however, there was only a propriatery Windows GUI application to connect
to the bootloader, no source code was provided, no builds for other operating
systems besides Windows 32 bit, no command line utility to integrate it into
own production code, not even a formal protocol specification and virtually
no documentation at all.

Hagen's bootloader is ingenious and nothing even remotely comparable exists,
in order to prevent it from fading into oblivion due to a lack of available 
tools, knowledge and community support and to increase its usefulness even 
further I have decided to completely reverse engineer the protocol and write 
a free cross platform client and library from scratch, released under (L)GPL.

The code is written in Free Pascal (GPL, LGPL) to ensure maximum cross 
platform compatibility and easily readable code, it will compile with current
FPC stable release (2.6.x at the time of this writing) on all supported 
Windows, Linux, Mac and other Unix platforms and CPU architectures. Source 
(and later also binary releases) of a command line client and a simple GUI 
tool that shows how to use the library in own projects will be provided.

#How to build it from source

##Prerequisites
You will need Free Pascal and Lazarus installed. Both are available at 
http://www.lazarus.freepascal.org/ 

Windows users just download and install the Lazarus installer wich already 
comes bundled with the neeeded FPC compiler and everything will be set up
to work out of the box, it cannot become any simpler.

Linux users download the packages for fpc, fpc-src and lazarus separately
and install them all. **Do not use the packages from your distributiuon
repository, they will most likely be broken or outdated, on Ubuntu they
are outright broken**. If you accidentally did then you must first remove 
them again, make sure you remove all traces of these packages and also 
remove all remaining configuration files, there should not be any 
/etc/fpc.cfg and no /usr/lib/fpc/ left over anymore and no fpc or lazarus
related files in /usr/bin, use the purge option in apt-get to remove all
system wide config files that were left over (or whatever option your
packet manager has to remove configuration files after uninstall). Then 
install the packages fpc, fpc-src and lazarus from sourceforge, install 
them in exactly that order.

Alternatively you can also install it with with the fpcup tool (google it) 
which will automatically download and compile everything needed from source, 
this sounds scary but works astonishingly well on Windows and also on Linux
and will also give you a complete and up-to-date FPC/Lazarus installation.

##Build
Start Lazarus, open any one of the *.lpi files (Menu: Project -> Open Project...)
and then just build them (Run -> Build) which will produce the executable
of the project in the same directory a few seconds later. The executables
are linked statically, so you don't need any .so or .dll installed on the
production machine, they are entirely self-contained.

##Bugs, Problems
Please let me know about it through the github issue tracker.