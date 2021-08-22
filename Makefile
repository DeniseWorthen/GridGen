# GNU Makefile template for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. Following the ESMF User's Guide, a complete ESMF     ##
## installation should ensure that a single environment variable "ESMFMKFILE" ##
## is made available on the system. This variable should point to the         ##
## "esmf.mk" file.                                                            ##
##                                                                            ##
## This example Makefile uses the "ESMFMKFILE" environment variable.          ##
##                                                                            ##
## If you notice that this Makefile cannot find variable ESMFMKFILE then      ##
## please contact the person responsible for the ESMF installation on your    ##
## system.                                                                    ##
## As a work-around you can simply hardcode the path to "esmf.mk" in the      ##
## include line below. However, doing so will render this Makefile a lot less ##
## flexible and non-portable.                                                 ##
################################################################################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

localFopt = -C -O0 -fPIC

%.o : %.f90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<
        
%.o : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

%.o : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

OBJs=scripdefs.o scripgrid.o inputnml.o vartypedefs.o tripolegrid.o cicegrid.o angles.o physcon.o debugprint.o vertices.o grdvars.o charstrings.o gen_fixgrid.o

gengrid: $(OBJs)
	$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS)

grdvars.o:
charstrings.o:
vartypedefs.o:
scripdefs.o:
physcon.o:
tripolegrid.o:
angles.o : grdvars.o
debugprint.o: grdvars.o
inputnml.o: grdvars.o charstrings.o
cicegrid.o: grdvars.o charstrings.o vartypedefs.o
scripgrid.o: grdvars.o charstrings.o vartypedefs.o
tripolgrid.o: grdvars.o charstrings.o vartypedefs.o
vertices.o: grdvars.o
gen_fixgrid.o: inputnml.o grdvars.o angles.o vertices.o vartypedefs.o tripolgrid.o cicegrid.o scripgrid.o physcon.o charstrings.o debugprint.o

# -----------------------------------------------------------------------------
#.PRECIOUS: %.so
#%.mk : %.so
#       @echo "# ESMF self-describing build dependency makefile fragment" > $@
#       @echo >> $@
#       @echo "ESMF_DEP_FRONT = kiss"                   >> $@
#       @echo "ESMF_DEP_INCPATH = 'pwd'"                >> $@
#       @echo "ESMF_DEP_CMPL_OBJS = "                   >> $@
#       @echo "ESMF_DEP_LINK_OBJS = "                   >> $@
#       @echo "ESMF_SHRD_PATH = 'pwd'"                  >> $@
#       @echo "ESMF_SHRD_LIBS = "$*                     >> $@
# -----------------------------------------------------------------------------
.PHONY: dust clean distclean
dust:
	rm -f PET*.ESMF_LogFile
clean:
	rm -f gengrid *.i *.o *.mod *.so *.mk PET*.ESMF_LogFile
distclean: dust clean

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================
