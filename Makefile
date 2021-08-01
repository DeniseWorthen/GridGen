CDF=/apps/netcdf/4.7.0/intel/18.0.5.274
#####################################################################
# compiler options
# #####################################################################
#FOPT = -C -O0
#fPIC reqd for 008 and is b4b when used on smaller grids
FOPT = -C -O0 -fPIC
#FOPT = -C -warn

F90 = ifort

######################################################################
#
#####################################################################
OBJS = grdvars.o charstrings.o scripdefs.o scripgrid.o inputnml.o fixgriddefs.o tripolegrid.o icegriddefs.o cicegrid.o angles.o physcon.o debugprint.o vertices.o gen_fixgrid.o

gengrid: $(OBJS)
	$(F90) $(FOPT) -o gengrid $(OBJS) -L$(CDF)/lib -lnetcdff -lnetcdf

%.o: %.F90
	$(F90) $(FOPT) -c -I$(CDF)/include $<
	cpp -I$(CDF)/include $*.F90>$*.i

clean:
	/bin/rm -f gengrid *.o *.i *.mod
