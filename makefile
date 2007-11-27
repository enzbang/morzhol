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

# Required for Windows to find the shared library
ifeq ($(OS),Windows_NT)
export PATH:=$(shell pwd)/lib:$(PATH)
endif

all:
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

I_MORZ     = $(INSTALL)/include/morzhol
I_LIB_MORZ = $(INSTALL)/lib/morzhol
I_GPR	   = $(INSTALL)/lib/gnat
BDIR       = ".build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])"

install:
ifeq ("$(INSTALL)", "..")
	$(error "Wrong install path : INSTALL='$(INSTALL)'")
else
ifeq ("$(INSTALL)", "")
	$(error "Wrong install path : empty INSTALL var")
endif
endif
	$(MKDIR) -p $(I_MORZ)
	$(CP) src/*.ad[sb] $(I_MORZ)
	$(CP) $(BDIR)/lib/* $(I_LIB_MORZ)
	$(CP) install.gpr $(I_GPR)/morzhol.gpr