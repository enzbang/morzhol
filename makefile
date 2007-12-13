##############################################################################
##                                Morzhol                                   ##
##                                                                          ##
##                           Copyright (C) 2007                             ##
##                     Pascal Obry - Olivier Ramonat                        ##
##                                                                          ##
##  This library is free software; you can redistribute it and/or modify    ##
##  it under the terms of the GNU General Public License as published by    ##
##  the Free Software Foundation; either version 2 of the License, or (at   ##
##  your option) any later version.                                         ##
##                                                                          ##
##  This library is distributed in the hope that it will be useful, but     ##
##  WITHOUT ANY WARRANTY; without even the implied warranty of              ##
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       ##
##  General Public License for more details.                                ##
##                                                                          ##
##  You should have received a copy of the GNU General Public License       ##
##  along with this library; if not, write to the Free Software Foundation, ##
##  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       ##
##############################################################################

GNAT=gnat
MKDIR=mkdir
CP=cp
MODE=Release

GNAT_ROOT=$(dir $(shell which gnatls))..
INSTALL = $(GNAT_ROOT)

# Required for Windows to find the shared library
ifeq ($(OS),Windows_NT)
export PATH:=$(shell pwd)/lib:$(PATH)
endif

all: build

build:
ifneq ($(INSTALL), "")
# Write INSTALL target into mk.install (see install target)
	$(shell echo INSTALL = $(INSTALL) > mk.install)
endif
	$(GNAT) make -p -XPRJ_BUILD=$(MODE) -Pmorzhol

setup:

check:
	$(GNAT) check -dd -Pmorzhol -rules -from=morzhol.check

regtests:
	$(GNAT) make -Ptest/test
	./test/test

clean:
	-$(GNAT) clean -XPRJ_BUILD=$(MODE) -q -Pmorzhol
	-$(GNAT) clean -XPRJ_BUILD=$(MODE) -q -Ptest/test


-include mk.install

force:

# Set BDIR to .build/#lowercase_mode#
install: BDIR = .build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])
install: force
ifeq ("$(INSTALL)", "")
	$(error "Wrong install path : empty INSTALL var")
endif
	$(MKDIR) -p $(INSTALL)/include/morzhol
	$(CP) src/*.ad[sb] $(INSTALL)/include/morzhol
	$(CP) $(BDIR)/lib/* $(INSTALL)/lib/morzhol
	$(CP) install.gpr $(INSTALL)/lib/gnat/morzhol.gpr
ifeq ($(OS), Windows_NT)
	$(CP) $(BDIR)/lib/*$(SOEXT) $(INSTALL)/lib
endif
