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

