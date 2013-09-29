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

AT          ?= @
OBJDIR      := ../cil/obj/$(ARCHOS)
ODOC_FILES   = ../src/scoop*.ml

.PHONY: html pdf

manual.pdf: src/manual.tex
	$(AT)cd src; echo "\def\scoopversion{1.5.5}" > scoop.version.tex
	$(AT)cd src; pdflatex manual.tex; pdflatex manual.tex
	$(AT)cd src; mv manual.pdf ../manual.pdf
	$(AT)cd src; rm -f *.aux *.log scoop.version.tex


html scoop_api.tex: $(OBJDIR)/pretty.cmi $(OBJDIR)/cil.cmi

html:
	$(AT)mkdir -p html
	$(AT)rm -f ocamldoc.sty
	$(AT)ocamldoc -d html/ -v -stars -html -t "SCOOP Documentation" \
	              -I $(OBJDIR) -hide Pervasives,Scoop_alter $(ODOC_FILES)

scoop_api.tex:
	$(AT)ocamldoc -o src/scoop_api.tex -v -stars -latex \
	              -t "SCOOP Documentation" \
	              -I $(OBJDIR) -hide Pervasives,Scoop_alter $(ODOC_FILES)
	$(AT)sed -i \
	         -e 's/\\usepackage\[T1\]{fontenc}/\\setlength{\\pdfpagewidth}{\\paperwidth} \\setlength{\\pdfpageheight}{\\paperheight}/' \
	         src/scoop_api.tex

scoop_api.pdf: scoop_api.tex
	$(AT)cd src; \
	  TEXINPUTS="$$TEXINPUTS:/usr/local/lib/ocaml/ocamldoc:/usr/lib/ocaml/ocamldoc"\
	  latexmk scoop_api.tex

clean:
	$(AT)cd src; rm -rf *.aux *.log scoop_api.tex *.toc *.bcf *.out *.run.xml auto

distclean:
	$(AT)rm -rf scoop_api.pdf html
