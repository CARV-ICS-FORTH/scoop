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
	sdam \
	barrierstate \
	ptatype \
	ptdepa \
	scoop_util \
	scoop_x86 \
	scoop_cell \
	scoop_cellgod \
#	scoop_rmtmps \
#	lockpick \
#	locksmith \
#	stmizer

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
CAML_CFLAGS := -ccopt -I$(DYCKCFL_DIR) -ccopt -I$(REGION_DIR) -ccopt -I$(ENGINE_DIR)


ifndef ARCHOS
   ARCHOS=x86_LINUX
endif

# Put here all the byproducts of make
OBJDIR      := cil/obj/$(ARCHOS)
DOCDIR      := doc

ODOC_FILES =        src/scoop*.ml src/scoop*.mli

export LOCKSMITH_MODULES
export LOCKSMITH_CMODULES
#export LINKFLAGS
export CAML_CFLAGS
export CP4S

all:
	$(AT)$(MAKE) -C banshee NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 all
	$(AT)LINKFLAGS="$(LINKFLAGS)" $(MAKE) -C cil

doc: pdfdoc

pdfdoc: $(DOCDIR)/in/scoopman.tex $(OBJDIR)/pretty.cmi $(OBJDIR)/cil.cmi
	-rm -rf $(DOCDIR)/scoop
	-mkdir -p $(DOCDIR)/scoop
	cd doc/in; echo "\def\scoopversion{1.0.0}" >scoop.version.tex
	cd $(DOCDIR)/in; pdflatex scoopman.tex; pdflatex scoopman.tex
	cd $(DOCDIR)/in; mv scoopman.pdf ../scoop/SCOOP.pdf
	ocamldoc -o $(DOCDIR)/scoop-api.tex.tmp -v -stars \
             -latex \
             -t "SCOOP Documentation" \
	     -I $(OBJDIR) -hide Pervasives $(ODOC_FILES)
	sed -e 's/\\usepackage\[T1\]{fontenc}/\\setlength{\\pdfpagewidth}{\\paperwidth} \\setlength{\\pdfpageheight}{\\paperheight}/' $(DOCDIR)/scoop-api.tex.tmp >$(DOCDIR)/scoop-api.tex
	rm $(DOCDIR)/scoop-api.tex.tmp

	cd $(DOCDIR) ; TEXINPUTS="$$TEXINPUTS:/usr/local/lib/ocaml/ocamldoc:/usr/lib/ocaml/ocamldoc" pdflatex scoop-api.tex
	cd $(DOCDIR) ; mv scoop-api.pdf scoop/SCOOP-API.pdf
	-rm -f $(DOCDIR)/*


profile:
	$(AT)$(MAKE) -C banshee NO_BANSHEE_ROLLBACK=1 NO_HASH_BOUNDS=1 DEBUG=1 DEBUG_RALLOC=1 all
	$(AT)LINKFLAGS="$(LINKFLAGS)" $(MAKE) -C cil PROFILE=1

clean:
	$(AT)$(MAKE) -C banshee clean
	$(AT)$(MAKE) -C cil clean
