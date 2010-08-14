## Process this file with automake to produce Makefile.in
##
## Makefile for hircules
##
## Author : Jens Petersen <petersen@haskell.org>
## Created: 2003-05-14
##
## Version $Revision: 1.4 $ from $Date: 2003/07/03 21:30:47 $
##
## Copyright (c) 2003 Jens-Ulrik Holger Petersen
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation; either
## version 2 of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

LIBMODULES = Debug.hs Directories.hs Display.hs GdkKeys.hs GtkKeymap.hs MaybeDo.hs Threads.hs Version.hs.in WordString.hs
PACKAGES = gtk2 net
LAMBDABOT = @LAMBDABOT@

bin_PROGRAMS = hircules
hircules_SOURCES = Main.hs GUI.hs IRC.hs CTCP.hs Config.hs LambdaBot.hsc $(addprefix lib/,$(LIBMODULES))
EXTRA_DIST = index.html hircules.spec Screenshot-Hircules.png

HCFLAGS = $(addprefix -package ,$(PACKAGES)) -ilib

#ifneq ($(LAMBDABOT), "")
HCFLAGS += -i$(LAMBDABOT) -i$(LAMBDABOT)/EvalModule -DLAMBDABOT
#endif

hircules$(EXEEXT): $(hircules_SOURCES:.hs.in=.hs) LambdaBot.hs
	$(HC) --make -o $@ $(HCFLAGS) Main.hs
	strip $@

%.hs: %.hsc
	hsc2hs -DLAMBDABOT $<

LambdaBot.hs: config.status

ghci:
	ghci -ilib $(HCFLAGS) $<

RPMARCH = i386

rpm: dist 
	rpmbuild -ta --define "_sourcedir ." --define "_srcrpmdir ." --define "_rpmdir ." $(distdir).tar.gz

-include Makefile.local
