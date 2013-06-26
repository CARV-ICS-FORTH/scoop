AT = @
LOCKSMITH_MODULES = \
  rmalias \
	lockutil \
	worklist \
	dotpretty \
	bansheemlifc \
	falsecfl \
	mycfl \
	livevars \
	locksettings \
	uniqueness \
	labelname \
	labelflow \
	controlflow \
	lockprofile \
	lockstate \
	shared \
	correlation \
	semiunification \
	locktype \
	lockalloc \
	ptatypes \
	sdam \
	scoop_util \
	barrierstate \
	loopa \
	ptatype \
	ptdepa \
	scoop_cell \
	scoop_cellgod \
	scoop_adam \
	scoop_bddt \
	scoop_XPPFX \
	scoop_nesting \
	scoop_myrmics \
	scoop_make_exec

LOCKSMITH_CMODULES = bansheeifc
CP4S        += locksettings.p4

BANSHEE = $(PWD)/banshee
DYCKCFL_DIR = $(BANSHEE)/dyckcfl
ENGINE_DIR = $(BANSHEE)/engine
REGION_DIR = $(BANSHEE)/libcompat
LINKFLAGS = \
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

ODOC_FILES = src/scoop*.ml

export LOCKSMITH_MODULES
export LOCKSMITH_CMODULES
#export LINKFLAGS
export CAML_CFLAGS
export CP4S

all:
	$(AT)$(MAKE) -C banshee NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 all
	$(AT)LINKFLAGS="$(LINKFLAGS)" $(MAKE) -C cil

doc: pdfdoc htmldoc

pdfdoc: $(DOCDIR)/src/manual.tex $(OBJDIR)/pretty.cmi $(OBJDIR)/cil.cmi
	@cd doc/src; echo "\def\scoopversion{1.5.1}" > scoop.version.tex
	@cd $(DOCDIR)/src; pdflatex manual.tex; pdflatex manual.tex
	@cd $(DOCDIR)/src; mv manual.pdf ../manual.pdf
	@ocamldoc -o $(DOCDIR)/scoop-api.tex.tmp -v -stars\
             -latex \
             -t "SCOOP Documentation" \
	     -I $(OBJDIR) -hide Pervasives,Scoop_alter $(ODOC_FILES)
	@sed -e 's/\\usepackage\[T1\]{fontenc}/\\setlength{\\pdfpagewidth}{\\paperwidth} \\setlength{\\pdfpageheight}{\\paperheight}/' $(DOCDIR)/scoop-api.tex.tmp >$(DOCDIR)/scoop-api.tex
	@rm $(DOCDIR)/scoop-api.tex.tmp

	@cd $(DOCDIR); TEXINPUTS="$$TEXINPUTS:/usr/local/lib/ocaml/ocamldoc:/usr/lib/ocaml/ocamldoc" pdflatex scoop-api.tex
	@cd $(DOCDIR); rm -f *.aux *.log src/*.aux src/*.log src/scoop.version.tex

htmldoc: $(OBJDIR)/pretty.cmi $(OBJDIR)/cil.cmi
	-rm -rf $(DOCDIR)/html/
	-mkdir -p $(DOCDIR)/html/
	-rm -f $(DOCDIR)/ocamldoc.sty
	ocamldoc -d $(DOCDIR)/html/ -v -stars \
             -html \
             -t "SCOOP Documentation" \
	     -I $(OBJDIR) -hide Pervasives,Scoop_alter $(ODOC_FILES)

profile:
	$(AT)$(MAKE) -C banshee NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 DEBUG=1 DEBUG_RALLOC=1 all
	$(AT)LINKFLAGS="$(LINKFLAGS)" $(MAKE) -C cil PROFILE=1

clean:
	$(AT)$(MAKE) -C banshee clean
	$(AT)$(MAKE) -C cil clean
