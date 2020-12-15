CDF=/apps/netcdf/4.7.0/intel/18.0.5.274
#####################################################################
# compiler options
# #####################################################################
FOPT = -C -O0
#FOPT = -C -warn

F90 = ifort

opt1 = -Doutput_grid_qdeg
#opt1 = -Doutput_grid_hdeg
#opt1 = -Doutput_grid_1deg
#opt1 = -Doutput_grid_3deg

optall = $(opt1) $(opt2)
######################################################################
#
#####################################################################
OBJS = param.o charstrings.o grdvars.o angles.o physcon.o debugprint.o fixgriddefs.o gen_fixgrid.o vertices.o write_tripolegrid.o icedefs.o write_cicegrid.o

gengrid: $(OBJS)
	$(F90) $(FOPT) -o gengrid $(OBJS) -L$(CDF)/lib -lnetcdff -lnetcdf

%.o: %.F90
	$(F90) $(FOPT) $(optall) -c -I$(CDF)/include $<
	cpp $(optall) -I$(CDF)/include $*.F90>$*.i

clean:
	/bin/rm -f gengrid *.o *.i *.mod
