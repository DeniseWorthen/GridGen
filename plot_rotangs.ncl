;************************************************
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

  load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"
;************************************************
begin

  grdsrc = "/scratch2/NCEPDEV/climate/Denise.Worthen/GRIDS/"

  ; rad 2 degree
  rad2deg = 180.0d0/acos(-1.0d0)

   ; arrays of characterstrings to set locations and filenames
   ;  fsrcs => tripole grid file
   ;  csrcs => cice restart file
    fsrcs = (/ "mx100",  "mx050",  "mx025", "mx072", "mx300"/)
    csrcs = (/  "1.00",   "0.50",   "0.25",  "0.72",  "3.00"/)
   ; seam longitude for each res
     lons = (/    60.0,     60.0,     60.0,   73.75,   75.00/)
   ; min lat to plot for each res
  minlats = (/    86.0,     87.0,     89.0,   86.00,   80.00/)

  ;ires = 0 ; 1deg
  ;ires = 1 ; 1/2deg
  ires = 2 ; 1/4deg
  ;ires = 3 ; 072deg
  ;ires = 4 ; 3deg

  levs = (/-1.6, 1.6, 0.1/)

  plottitle = "rotangs_"+fsrcs(ires)+"_"+minlats(ires)+"N"

;************************************************
; add a poly line showing the polar seem along
; great circle path from (lon1,lat1) to (lon2,lat2)
;************************************************

  lon1 =  lons(ires)
  lon2 =  lon1+180.0
  lat1 =  60.00
  lat2 =  60.00

  print("Great Circle "+lon1+" "+lon2)

;************************************************
; define the map
;************************************************

  wkstype="X11"
  ;wkstype="png"
  wkstype@wkWidth=800
  wkstype@wkHeight=800
  wks   = gsn_open_wks (wkstype,plottitle)              ; send graphics to X11 window

  npanels = 2
     plot = new(npanels,graphic)
      dum = new(npanels,graphic)

  res                      = True        ; plot mods desired
  res@gsnDraw              = False       ; do not draw until the end
  res@gsnFrame             = False       ; do not automatically advance 'frame'
  res@mpFillOn             = False
  res@cnFillMode           = "RasterFill"

  res@cnFillDrawOrder    = "PreDraw"         ; draw contours first
  res@cnFillOn           = True              ; color fill
  res@cnLinesOn          = False             ; no contour lines
  res@lbLabelBarOn       = False             ; turn off individual colorbars
  res@lbLabelFontHeightF = 0.015

  res@gsnPolar     = "NH"                   ; specify the hemisphere
  res@mpMinLatF    =  minlats(ires)         ; minimum lat to plot
  res@mpCenterLonF = -45

  ;res@cnFillPalette   =  "BlueWhiteOrangeRed"                      ; choose colormap
  res@cnFillPalette   =  "WhBlReWh"                      ; choose colormap

  res@cnLevelSelectionMode = "ManualLevels"     ; set the contour levels with the following 3 resources
  res@cnMinLevelValF  = levs(0)                      ; set the minimum contour level
  res@cnMaxLevelValF  = levs(1)                      ; set the maximum contour level
  res@cnLevelSpacingF = levs(2)                      ; set the interval between contours

  res@gsnLeftStringFontHeightF = 0.015

;************************************************
; Resources for the polyline
;************************************************

  res@mpGreatCircleLinesOn = True
  pres                  = True           ; polyline mods desired
  pres@gsLineThicknessF = 2.0            ; line thickness
  pres@gsLineColor      = "black"         ; color of lines
  pres@gsLineDashPattern =  1            ; dashes
  ;pres@tfPolyDrawOrder = "postDraw"

;************************************************
;
;************************************************

   ; plot angle on t-grid
      angname  = "anglet"
    anglonname = "lonCt"
    anglatname = "latCt"
       rfactor = 1.0
            gr = addfile(grdsrc+"tripole."+fsrcs(ires)+".nc","r")

          ii = 0
         ang = gr->$angname$(:,:)
   ang@lon2d = gr->$anglonname$*rfactor
   ang@lat2d = gr->$anglatname$*rfactor
   ang@units = " "
   res@gsnCenterString = "angle on Ct-grid"

  plot(ii) = gsn_csm_contour_map_polar(wks,ang,res)    ; create the plot
   dum(ii) = gsn_add_polyline(wks,plot(ii),(/lon1, lon2/) , (/lat1, lat2/) ,pres)
   delete([/gr,ang,rfactor/])


   ; plot angle on q-grid
      angname = "angle"
   anglonname = "ulon"
   anglatname = "ulat"
       rfactor = rad2deg
         levs = (/-1.6, 1.6, 0.1/)
           gr = addfile(grdsrc+"grid_cice_NEMS_"+fsrcs(ires)+".nc","r")

          ii = 1
         ang = gr->$angname$(:,:)
   ang@lon2d = gr->$anglonname$*rfactor
   ang@lat2d = gr->$anglatname$*rfactor
   ang@units = " "
   ang@long_name = " "
   res@gsnCenterString = "angle on Bu-grid"

  plot(ii) = gsn_csm_contour_map_polar(wks,ang,res)    ; create the plot
   dum(ii) = gsn_add_polyline(wks,plot(ii),(/lon1, lon2/) , (/lat1, lat2/) ,pres)

;************************************************
;
;************************************************

  resP                     = True                ; modify the panel plot

  ;resP@gsnPanelMainString  = plottitle
  resP@gsnPanelMainFontHeightF = 0.010            ; new resource added in NCL V6.4.0

  gsn_panel(wks,plot,(/2,1/),resP)               ; now draw as one plot

exit
end
