;************************************************
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

  load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"
;************************************************
begin
;************************************************
; read in netCDF file
;************************************************

  rtsrc = "/scratch1/NCEPDEV/climate/Denise.Worthen/grids-20240311/"
  ;rtsrc = "/scratch1/NCEPDEV/climate/Denise.Worthen/addangvar-20240313/"

  grdfile = "tripole.mx025.nc"
  grdsrc = ""
  rad2deg = 180.d0/acos(-1.0d0)

;************************************************
; ice
;************************************************

  ; load the grid file
  gr =addfile(rtsrc+grdsrc+grdfile,"r")
  print(rtsrc+grdfile)

  ;varname="htn"
  ;latname="latCv"
  ;lonname="lonCv"
  ;levs=(/6400.0, 6600.0, 5.0/)
  ;minlat=88.0

  ;varname="hte"
  ;latname="latCu"
  ;lonname="lonCu"
  ;levs=(/9200.0, 12200.0, 100.0/)
  ;minlat=82.0

  levs=(/-1.6,1.6,.1/)
  ;minlat = 89.5
  minlat = 85.0

;************************************************
; add a poly line showing the polar seem along
; great circle path from (lon1,lat1) to (lon2,lat2)
;************************************************

  lon1=60.00
  lon2=-120.00
  lat1=60
  lat2=60

;************************************************
; define the map
;************************************************
  wkstype="X11"
  ;wkstype="png"
  wkstitle=grdfile
  wkstype@wkWidth=800
  wkstype@wkHeight=800
  wks   = gsn_open_wks (wkstype,wkstitle)              ; send graphics to X11 window

  plot = new(2,graphic)
  dum = new(2,graphic)

  res                      = True        ; plot mods desired
  res@gsnDraw              = False       ; do not draw until the end
  res@gsnFrame             = False       ; do not automatically advance 'frame'

  res@cnFillMode = "RasterFill"
  res@mpFillOn   = True

  res@gsnStringFontHeightF = 0.05

  res@gsnPolar   = "NH"                          ; specify the hemisphere
  res@mpMinLatF  = minlat                            ; minimum lat to plot
  res@mpCenterLonF = -45

  res@cnFillDrawOrder   = "PreDraw"             ; draw contours first
  res@cnFillOn          = True                  ; color fill
  res@cnLinesOn         = False                 ; no contour lines
  res@cnLineLabelsOn    = False
  res@lbLabelBarOn      = False

  res@cnLevelSelectionMode = "ManualLevels"     ; set the contour levels with the following 3 resources
  res@cnMinLevelValF  = levs(0)                      ; set the minimum contour level
  res@cnMaxLevelValF  = levs(1)                      ; set the maximum contour level
  res@cnLevelSpacingF = levs(2)                      ; set the interval between contours

  ;res@cnFillPalette        = "NCV_jet"              ; choose colormap
  res@cnFillPalette        = "BlueWhiteOrangeRed"              ; choose colormap

  res@gsnCenterStringOrthogonalPosF = .15	    ; raise the CenterString slightly
  res@gsnStringFontHeightF = 0.02
  ;res@gsnCenterString = cd_string(times(lstep),"%d %F")

;************************************************
; Resources for the polyline
;************************************************

  res@mpGreatCircleLinesOn = True
  pres                  = True           ; polyline mods desired
  pres@gsLineThicknessF = 2.0            ; line thickness
  pres@gsLineColor      = "black"         ; color of lines
  pres@gsLineDashPattern =  1            ; dashes
  ;pres@tfPolyDrawOrder = "postDraw"

; ***************************************************
; panel resources
; **************************************************

  resP                       = True                 ; modify the panel plot
  resP@gsnMaximize           = True                 ; maximize in frame
  resP@gsnFrame              = False                ; don't advance frame yet
  resP@gsnPanelYWhiteSpacePercent = 10
  resP@gsnPanelYWhiteSpacePercent = 10
  resP@gsnPanelLabelBar      = True                 ; turn on a common labelbar for the entire panel plot

  resP@gsnPanelMainFontHeightF = 0.015           ; new resource added in NCL V6.4.0
  resP@gsnPanelMainString = grdfile
  resP@gsnPanelFigureStrings = (/"angle on Ct","angle on Bu"/)
;************************************************
;
;************************************************

  ; in tripole file, anglet=(-1)*angle
  ii=0
  varname="anglet"
  lonname="lonCt"
  latname="latCt"
  var = gr->$varname$
  var = -1.0*var
  var@lon2d=gr->$lonname$
  var@lat2d=gr->$latname$
  var@units=" "
  plot(ii) = gsn_csm_contour_map_polar(wks,var,res)    ; create the plot
  dum(ii) = gsn_add_polyline(wks,plot(ii),(/lon1, lon2/) , (/lat1, lat2/) ,pres)


  ii=1
  varname="angle"
  lonname="lonBu"
  latname="latBu"
  var = gr->$varname$
  var@lon2d=gr->$lonname$
  var@lat2d=gr->$latname$
  var@units=" "
  plot(ii) = gsn_csm_contour_map_polar(wks,var,res)    ; create the plot
  dum(ii) = gsn_add_polyline(wks,plot(ii),(/lon1, lon2/) , (/lat1, lat2/) ,pres)

;************************************************
; make the panel
;************************************************

  gsn_panel(wks,plot,(/1,2/),resP)

  frame(wks)

  if(wkstype .eq. "png")then
   imcmd = "convert -trim "+wkstitle+".png "+wkstitle+".jpg"
   system(imcmd)
  end if

exit
end
