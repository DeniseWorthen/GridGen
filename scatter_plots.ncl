; ***********************************************
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************

  grdsrc = "/scratch2/NCEPDEV/climate/Denise.Worthen/grids-20210727/"

  ; rad 2 degree
  rad2deg = 180.0d0/acos(-1.0d0)

   ; arrays of characterstrings to set locations and filenames
   ;  fsrcs => tripole grid file
   ;  csrcs => cice restart file
    fsrcs = (/ "mx100",  "mx050",  "mx025", "mx072", "mx400"/)
    csrcs = (/  "1.00",   "0.50",   "0.25",  "0.72",  "4.00"/)
   ; seam longitude for each res
     lons = (/    60.0,     60.0,     60.0,   73.75,   80.00/)
   ; min lat to plot for each res
  minlats = (/    89.0,     89.5,     89.7,   89.0,   80.00/)

  ;ires = 0 ; 1deg
  ;ires = 1 ; 1/2deg
  ;ires = 2 ; 1/4deg
  ;ires = 3 ; 072deg
  ires = 4 ; 4deg

;************************************************
; read in netCDF file
;************************************************
       gr = addfile(grdsrc+"tripole."+fsrcs(ires)+".nc","r")
  rfactor = rad2deg

   cornersuffix = "Bu"
   ugridsuffix  = "Cu"
   vgridsuffix  = "Cv"
   tgridsuffix  = "Ct"

  cornerlatname="lat"+cornersuffix
  cornerlonname="lon"+cornersuffix
 
  centerlatname="lat"+tgridsuffix
  centerlonname="lon"+tgridsuffix

;************************************************
;
;************************************************

  ; retrieve a variable to set the dimensions
   tmp = gr->$centerlatname$
  dims = dimsizes(tmp)
    ny = dims(0)
    nx = dims(1)
  lats = new((/nx/), typeof(tmp))
  lats = tmp(ny-1,:)
  ; fortran style
  ipole = (/maxind(lats)+1, maxind(lats)+1/)
  print("using pole locations ipole = "+ipole)

   imax = nx
   jmax = (/ny,ny/)
   ioff = 0

;************************************************
; load the grid file
;************************************************

   plottitle = "tripole."+fsrcs(ires)+".nc"

  centerlat = gr->$centerlatname$(:,:)
  centerlon = gr->$centerlonname$(:,:)

  cornerlat = gr->$cornerlatname$(:,:)
  cornerlon = gr->$cornerlonname$(:,:)

;************************************************
; ipL = i index of Pole on "Left" side
; ipR = i index of Pole on "Right" side
;************************************************

  centerjmax = jmax(0)
  cornerjmax = jmax(1)
  center_ipL = ipole(0)
  corner_ipL = ipole(1)
  corneroff  = ioff

  center_ipR=(imax - center_ipL +1)
  corner_ipR=(imax - corner_ipL + corneroff)

  print("corner "+corner_ipL+","+corner_ipR)
  print("center "+center_ipL+","+center_ipR)

  ; ncl indexing
  center_ipL=center_ipL-1
  center_ipR=center_ipR-1
  corner_ipL=corner_ipL-1
  corner_ipR=corner_ipR-1

  print ("Center Lon,Lat on  Left "+centerlon(centerjmax-1,center_ipL)+" "+centerlat(centerjmax-1,center_ipL))
  print ("Center Lon,Lat on Right "+centerlon(centerjmax-1,center_ipR)+" "+centerlat(centerjmax-1,center_ipR))
  deltalon=centerlon(centerjmax-1,center_ipR) - centerlon(centerjmax-1,center_ipL)
  print ("Delta Lon :"+deltalon)
  print (" ")
  print ("Corner Lon,Lat on  Left "+cornerlon(cornerjmax-1,corner_ipL)+" "+cornerlat(cornerjmax-1,corner_ipL))
  print ("Corner Lon,Lat on Right "+cornerlon(cornerjmax-1,corner_ipR)+" "+cornerlat(cornerjmax-1,corner_ipR))
  deltalon=cornerlon(cornerjmax-1,corner_ipR) - cornerlon(cornerjmax-1,corner_ipL)
  print ("Delta Lon :"+deltalon)

;************************************************
; extract out points which cross the polar seam
;  ==> +/- 1 the pole i location
;************************************************

  center_xlon = (/centerlon(:,center_ipL-1),centerlon(:,center_ipL),centerlon(:,center_ipL+1), \
                  centerlon(:,center_ipR-1),centerlon(:,center_ipR),centerlon(:,center_ipR+1)/)
  center_xlat = (/centerlat(:,center_ipL-1),centerlat(:,center_ipL),centerlat(:,center_ipL+1), \
                  centerlat(:,center_ipR-1),centerlat(:,center_ipR),centerlat(:,center_ipR+1)/)

  corner_xlon = (/cornerlon(:,corner_ipL-1),cornerlon(:,corner_ipL),cornerlon(:,corner_ipL+1), \
                  cornerlon(:,corner_ipR-1),cornerlon(:,corner_ipR),cornerlon(:,corner_ipR+1)/)
  corner_xlat = (/cornerlat(:,corner_ipL-1),cornerlat(:,corner_ipL),cornerlat(:,corner_ipL+1), \
                  cornerlat(:,corner_ipR-1),cornerlat(:,corner_ipR),cornerlat(:,corner_ipR+1)/)

  print(dimsizes(corner_xlon))
;************************************************
; extract out points which parallel the polar seam
;************************************************

  center_ylon = (/centerlon(centerjmax-2:centerjmax-1,:)/)
  center_ylat = (/centerlat(centerjmax-2:centerjmax-1,:)/)

  corner_ylon = (/cornerlon(cornerjmax-2:cornerjmax-1,:)/)
  corner_ylat = (/cornerlat(cornerjmax-2:cornerjmax-1,:)/)

  ; index rel 0
  jrows=(/cornerjmax-2,cornerjmax-1/)
  jrows=jrows+1
  print("parallel rows: "+jrows(0)+" , "+jrows(1))

  print(dimsizes(corner_ylon))

;************************************************
; define the map
;************************************************

  wkstype="X11"
  ;wkstype="png"
  wkstype@wkWidth=1200
  wkstype@wkHeight=1200
  wks   = gsn_open_wks (wkstype,"scatt_"+fsrcs(ires))              ; send graphics to X11 window

    map = new(4,graphic)
  plotr = new(4,graphic)
  plotb = new(4,graphic)
    dum = new(4,graphic)

  res                      = True        ; plot mods desired
  res@gsnDraw              = False       ; do not draw until the end
  res@gsnFrame             = False       ; do not automatically advance 'frame'

  res@gsnPolar     = "NH"                  ; specify the hemisphere
  res@mpMinLatF    = minlats(ires)         ; minimum lat to plot
  res@mpCenterLonF = -45

  res@gsnStringFontHeightF = 0.05
;************************************************
; add a poly line showing the polar seem along
; great circle path from (lon1,lat1) to (lon2,lat2)
;************************************************

 lon1 =  lons(ires)
  lon2 =  lon1+180.0
  lat1 =  60.00
  lat2 =  60.00

  print("Great Circle "+lon1+" "+lon2)

; ***************************************************
; Resources for the polyline
; ***************************************************

  res@mpGreatCircleLinesOn = True
  pres                  = True           ; polyline mods desired
  pres@gsLineThicknessF = 4.0            ; line thickness
  pres@gsLineColor      = "green"         ; color of lines
  pres@gsLineDashPattern =  1            ; dashes

; ***************************************************
; create panel
; **************************************************

  resP                       = True                 ; modify the panel plot
  resP@gsnMaximize           = True                 ; maximize in frame
  resP@gsnFrame              = False                ; don't advance frame yet
  resP@gsnPanelYWhiteSpacePercent = 10
  resP@gsnPanelYWhiteSpacePercent = 10

  ;resP@gsnPanelMainString=plottitle
  ;resP@txString=plottitle

  resP@gsnStringFontHeightF = 0.30
;************************************************
; resources for polymarkers
;************************************************

  mkres               = True
  mkres@gsMarkerIndex = 2
  mkres@gsMarkerSizeF = 0.012

;************************************************
; the scatt plot call
;************************************************

begin
function scattplot(ii,xr,yr,xb,yb)
begin
  map(ii) = gsn_csm_map_polar(wks,res)                ; Create the map plot, don't draw
  dum(ii) = gsn_add_polyline(wks,map(ii),(/lon1, lon2/) , (/lat1, lat2/) ,pres)
  mkres@gsMarkerColor = "Red"
  plotr(ii) = gsn_add_polymarker(wks,map(ii),xr,yr,mkres)
  mkres@gsMarkerColor = "Blue"
  plotb(ii) = gsn_add_polymarker(wks,map(ii),xb,yb,mkres)
  return(map(ii))
end

;************************************************
;
;************************************************

  ii=0
  res@gsnLeftString = "Center ("+str_upper(tgridsuffix)+")"
  map(ii) = scattplot(ii,center_xlon(0:2,:),center_xlat(0:2,:),center_xlon(3:5,:),center_xlat(3:5,:))
  ;map(ii) = scattplot(ii,center_xlon(1,:),center_xlat(1,:),center_xlon(4,:),center_xlat(4,:))

  ii=1
  res@gsnLeftString = "Corner ("+str_upper(cornersuffix)+")"
  map(ii) = scattplot(ii,corner_xlon(0:2,:),corner_xlat(0:2,:),corner_xlon(3:5,:),corner_xlat(3:5,:))
  ;map(ii) = scattplot(ii,corner_xlon(1,:),corner_xlat(1,:),corner_xlon(4,:),corner_xlat(4,:))

  ii=2
  res@gsnLeftString = "Center ("+str_upper(tgridsuffix)+")"
  map(ii) = scattplot(ii,center_ylon(0,:),center_ylat(0,:),center_ylon(1,:),center_ylat(1,:))

  ii=3
  res@gsnLeftString = "Corner ("+str_upper(cornersuffix)+")"
  map(ii) = scattplot(ii,corner_ylon(0,:),corner_ylat(0,:),corner_ylon(1,:),corner_ylat(1,:))

;************************************************
; make the panel
;************************************************
  resP@gsnPanelMainString  = plottitle
  resP@gsnPanelMainFontHeightF = 0.010            ; new resource added in NCL V6.4.0

  gsn_panel(wks,map,(/2,2/),resP)
  frame(wks)

exit
end
