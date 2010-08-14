## Process this file with automake to produce Makefile.in
##
## Makefile for hircules
##
## Author : Jens Petersen <petersen@haskell.org>
## Created: 2003-05-14
##
## Version $Revision: 1.1 $ from $Date: 2003/05/15 20:43:43 $
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

LIBMODULES = GdkKeys.hs GtkKeymap.hs Threads.hs Version.hs.in
PACKAGES = gtk2 net
LAMBDABOT = lambdabot

bin_PROGRAMS = hircules
hircules_SOURCES = Main.hs GUI.hs IRC.hs $(addprefix lib/,$(LIBMODULES))
EXTRA_DIST = index.html hircules.spec Screenshot-Hircules.png

HCFLAGS = $(addprefix -package ,$(PACKAGES)) -ilib -i$(LAMBDABOT) -i$(LAMBDABOT)/EvalModule 

hircules$(EXEEXT): $(hircules_SOURCES:.hs.in=.hs)
	$(HC) --make -o $@ $(HCFLAGS) Main.hs
	strip $@

ghci:
	ghci -ilib $(HCFLAGS) $<

RPMARCH = i386

rpm: dist 
	rpmbuild -ta --define "_sourcedir ." --define "_srcrpmdir ." --define "_rpmdir ." $(distdir).tar.gz

-include Makefile.local
