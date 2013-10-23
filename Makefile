############################################################################
# Copyright (c) 2010-13,                                                   #
#                        Foivos    Zakkak          <zakkak@ics.forth.gr>   #
#                                                                          #
#                        FORTH-ICS / CARV                                  #
#                        (Foundation for Research & Technology -- Hellas,  #
#                         Institute of Computer Science,                   #
#                         Computer Architecture & VLSI Systems Laboratory) #
#                                                                          #
#                                                                          #
#                                                                          #
# Licensed under the Apache License, Version 2.0 (the "License");          #
# you may not use this file except in compliance with the License.         #
# You may obtain a copy of the License at                                  #
#                                                                          #
#     http://www.apache.org/licenses/LICENSE-2.0                           #
#                                                                          #
# Unless required by applicable law or agreed to in writing, software      #
# distributed under the License is distributed on an "AS IS" BASIS,        #
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. #
# See the License for the specific language governing permissions and      #
# limitations under the License.                                           #
############################################################################

################################################################################
# Copyright (c) 2004-2010                                                      #
#  Polyvios Pratikakis <polyvios@cs.umd.edu> <polyvios@ics.forth.gr>           #
#  Jeffrey Foster      <jfoster@cs.umd.edu>                                    #
#  Michael Hicks       <mwh@cs.umd.edu>                                        #
# All rights reserved.                                                         #
#                                                                              #
#                                                                              #
# Redistribution and use in source and binary forms, with or without           #
# modification, are permitted provided that the following conditions are met:  #
#                                                                              #
# 1. Redistributions of source code must retain the above copyright notice,    #
# this list of conditions and the following disclaimer.                        #
#                                                                              #
# 2. Redistributions in binary form must reproduce the above copyright notice, #
# this list of conditions and the following disclaimer in the documentation    #
# and/or other materials provided with the distribution.                       #
#                                                                              #
# 3. The names of the contributors may not be used to endorse or promote       #
# products derived from this software without specific prior written           #
# permission.                                                                  #
#                                                                              #
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  #
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    #
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   #
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE     #
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          #
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         #
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     #
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      #
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      #
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   #
# POSSIBILITY OF SUCH DAMAGE.                                                  #
#                                                                              #
#                                                                              #
# (See http://www.opensource.org/licenses/bsd-license.php)                     #
################################################################################

AT               ?= @
LOCKSMITH_MODULES =     \
  rmalias               \
  lockutil              \
  worklist              \
  dotpretty             \
  bansheemlifc          \
  falsecfl              \
  mycfl                 \
  livevars              \
  locksettings          \
  uniqueness            \
  labelname             \
  labelflow             \
  controlflow           \
  lockprofile           \
  lockstate             \
  shared                \
  correlation           \
  semiunification       \
  locktype              \
  lockalloc
SCOOP_MODULES     =     \
  ptatypes              \
  sdam                  \
  scoop_util            \
  barrierstate          \
  loopa                 \
  ptatype               \
  ptdepa                \
  scoop_codegen         \
  scoop_make_exec       \
  scoop_cell            \
  scoop_cellgod         \
  scoop_bddt            \
  scoop_XPPFX           \
  scoop_nesting         \
  scoop_myrmics

LOCKSMITH_CMODULES = bansheeifc
CP4S              += locksettings.p4

BANSHEE            = $(PWD)/banshee
DYCKCFL_DIR        = $(BANSHEE)/dyckcfl
ENGINE_DIR         = $(BANSHEE)/engine
REGION_DIR         = $(BANSHEE)/libcompat
LINKFLAGS          = \
       $(DYCKCFL_DIR)/dyckcfl.o $(DYCKCFL_DIR)/mr_dyckcfl.o \
       $(REGION_DIR)/libregions.a \
       $(ENGINE_DIR)/libnsengine.a

       #-ccopt '-static'

CAML_CFLAGS := -ccopt -I$(DYCKCFL_DIR) -ccopt -I$(REGION_DIR) -ccopt -I$(ENGINE_DIR)


ifndef ARCHOS
   ARCHOS=x86_LINUX
endif

# Put here all the byproducts of make
OBJDIR      := cil/obj/$(ARCHOS)

DOCDIR      := doc

export LOCKSMITH_MODULES
export SCOOP_MODULES
export LOCKSMITH_CMODULES
#export LINKFLAGS
export CAML_CFLAGS
export CP4S

.PHONY: default profile all doc alldoc clean distclean install uninstall

defualt: scoop
	$(AT)$(MAKE) -C $(BANSHEE) NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 all
	$(AT)LINKFLAGS="$(LINKFLAGS)" $(MAKE) -C cil

profile: scoop
	$(AT)$(MAKE) -C $(BANSHEE) NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 DEBUG=1 DEBUG_RALLOC=1 all
	$(AT)LINKFLAGS="$(LINKFLAGS)" $(MAKE) -C cil PROFILE=1

all: default doc alldoc

# this is performed by configure
config:
	$(AT)cd cil; ./configure EXTRASRCDIRS="$(PWD)/src" EXTRAFEATURES="scoop"

scoop: scoop.in
	$(AT)sed 's.ROOT.'$(PWD)'.g' $< > $@
	$(AT)chmod a+x $@

doc:
	$(AT)$(MAKE) -C $(DOCDIR) manual.pdf

alldoc: default doc
	$(AT)$(MAKE) -C $(DOCDIR) scoop_api.pdf
	$(AT)$(MAKE) -C $(DOCDIR) html

install: scoop default
	$(AT)cp $< /usr/local/bin/

uninstall:
	$(AT)rm -f /usr/local/bin/scoop

clean:
	$(AT)$(MAKE) -C $(BANSHEE) clean
	$(AT)$(MAKE) -C cil clean

distclean: clean
	$(AT)rm -rf scoop cil/obj
	$(AT)$(MAKE) -C cil distclean
