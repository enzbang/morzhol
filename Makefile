##############################################################################
##                                Morzhol                                   ##
##                                                                          ##
##                        Copyright (C) 2007-2008                           ##
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

all::

# Root dir
MODE              = Release
JOBS              = 1
BUILD_DIR         = .build/$(shell echo $(MODE) | tr [[:upper:]] [[:lower:]])
GNATMAKE_OPTIONS  = -XPRJ_BUILD=$(MODE)
VALGRIND          =
TAR_DIR           = tar czf

# Install prefix
GNAT_ROOT         = $(dir $(shell which gnatls))..
ARGWIAD_ROOT      =
prefix	          =

ifeq (,$(prefix))
	prefix = $(shell cat $(BUILD_DIR)/gnat.root 2>/dev/null)
endif

ifeq (,$(ARGWIAD_ROOT))
	ARGWIAD_ROOT = $(shell echo $$ARGWIAD_ROOT)
endif

GNAT              = gnat
GNATMAKE          = $(GNAT) make -p -j$(JOBS) $(GNATMAKE_OPTIONS)
GNATCLEAN         = $(GNAT) clean $(GNATMAKE_OPTIONS)
GNATCHECK         = $(GNAT) check $(GNATMAKE_OPTIONS) -rules -from=morzhol.check
RM                = rm -f
LN                = ln
CP                = cp -p
MKDIR             = mkdir -p

LIBRARIES =
LIBRARIES += morzhol

GPR =
GPR += morzhol
GPR += test/test

BLD_GPR := $(addprefix bld-, $(GPR))
CLN_GPR := $(addprefix cln-, $(GPR))
CHK_GPR := $(addprefix chk-, $(GPR))

ifeq (${OS},Windows_NT)
	LIBEXT = .dll
	EXEEXT = .exe
	export PATH:=$(shell pwd)/lib:$(PATH) # Required to find the shared library
else
	LIBEXT = .so
endif

VERSION_ALL = $(shell git describe 2>/dev/null)

$(BLD_GPR): bld-% :
	$(GNATMAKE) -P$*

$(CLN_GPR): cln-% :
	$(GNATCLEAN) -P$*

$(CHK_GPR): chk-% :
	$(GNATCHECK) -P$*

morzhol build: prepare_install bld-morzhol

all:: prepare_install $(LIBRARIES)

clean: $(CLN_GPR)

clean_all:
	$(RM) -rf $(BUILD_DIR)

regtests:: bld-test/test
	./test/test
	./test/test_compose

gnatcheck: $(CHK_GPR)

install::
	$(MKDIR) $(prefix)/include/morzhol
	$(MKDIR) $(prefix)/lib/morzhol
	$(CP) src/*.ad[sb] $(prefix)/include/morzhol
	$(CP) $(BUILD_DIR)/lib/*$(LIBEXT) $(prefix)/lib/morzhol
	$(CP) $(BUILD_DIR)/lib/*.ali $(prefix)/lib/morzhol
	$(CP) install.gpr $(prefix)/lib/gnat/morzhol.gpr
ifeq ($(OS), Windows_NT)
	$(CP) $(BUILD_DIR)/lib/*$(LIBEXT) $(prefix)/bin
endif

prepare_install::
	$(shell echo $(GNAT_ROOT) > $(BUILD_DIR)/gnat.root)

install-clean::
	$(RM) -rf $(prefix)/include/morzhol
	$(RM) -rf $(prefix)/lib/morzhol

dist:
	git archive --prefix="$(shell basename $(PWD))/" --format=tar HEAD \
		 | gzip > "$(shell basename $(PWD))-$(VERSION_ALL)-src.tgz"


.PHONY: all
