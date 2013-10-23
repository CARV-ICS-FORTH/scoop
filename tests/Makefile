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

AT=@

TESTS = \
  simple01.c \
  simple02.c
# $(wildcard *.c)

# Why are we doing bytecode?  We run the real thing with native, that's what should be here!
#CILLY=../cil/bin/cilly --bytecode --save-graph --list-shared --list-guardedby -lpthread
SCOOP=../scoop --save-graph
OPTS=

all: init runall

runall: runintro $(TESTS:%.c=out/%.out)
	$(AT)echo "</table></html>" >> out/report.html

runintro:
	$(AT)echo "running all tests"

init:
	$(AT)$(RM) -rf out
	$(AT)mkdir -p out
	$(AT)echo "<html><table border=1pt>" > out/report.html
	$(AT)$(RM) -f core

cleanok:
	$(AT)$(RM) ok/*.out

clean:
	$(AT)$(RM) -f core a.out graph*.dot cf-graph.dot su?.dot *graph.ps a.out graph.txt *~
	$(AT)$(RM) -f out/* diffs/*

out/%.out: %.c
	$(AT)echo -n $<
	$(AT)echo -n "<tr> <td> $< <td> " >> out/report.html
	$(AT)$(SCOOP) --save-profile-data=out/$(basename $<).txt $(OPTS) $< 2>&1 | \
		awk -f filter.awk | \
		sed -e 's/#[0-9]*\\n//g' > $@; \
	if test -f core || test 0 -ne `ls core.* 2>/dev/null|wc -l`; \
	then \
		/bin/echo -e "\\033[30G \\033[1;31m core dump \\033[0;39m"; \
		echo " core dump" >> out/report.html; \
		$(RM) core core.*; false; \
	fi
	$(AT) \
	if test -f ok/$(basename $<).dot;\
	then \
		cat graph.dot | sed -e 's/#[0-9]*\\n//g' > out/$(basename $<).dot; \
		sort out/$(basename $<).dot > out/$(basename $<).dot.sorted; \
		sort ok/$(basename $<).dot > ok/$(basename $<).dot.sorted; \
		if diff -Bb \
			ok/$(basename $<).dot.sorted out/$(basename $<).dot.sorted > /dev/null; \
			then true; \
			else \
				/bin/echo -e "\\033[30G \\033[1;31m graphs differ \\033[0;39m"; \
				echo " graphs differ" >> out/report.html; \
				true; \
		fi; \
	fi && \
	if diff -Bb ok/$(basename $<).out $@ > /tmp/out$$ 2>&1; \
		then $(RM) /tmp/out$$; true; \
		else \
			/bin/echo -e "\\033[30G \\033[1;31m output differs \\033[0;39m";  \
			/bin/echo -e "\\033[30G \\033[1;31m old: " `grep "Possible data race" ok/$(basename $<).out | wc -l`"  new: " `grep "Possible data race" out/$(basename $<).out | wc -l`" \\033[0;39m"; \
			/bin/echo -e " output differs <br>" >> out/report.html; \
			echo "old: " `grep "Possible data race" ok/$(basename $<).out | wc -l`"  new: " `grep "Possible data race" out/$(basename $<).out | wc -l`" <br> " >> out/report.html; \
			mkdir -p diffs; mv /tmp/out$$ diffs/$(basename $<).diff; true; \
	fi && \
	/bin/echo -e "\\033[30G \\033[1;32m ok \\033[0;39m"; \
	echo "ok" >> out/report.html; \
