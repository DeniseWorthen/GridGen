;------------------------------------------------------------------
; Denise.Worthen@noaa.gov (Feb 2019)
;
; This is a version of ocnpost.ncl which remaps only the
; daily SST files
;
  load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"

;----------------------------------------------------------------------
begin

  ;wgtsdir="/scratch1/NCEPDEV/climate/Denise.Worthen/grids-20240218/"
  ;wgtsdir="/scratch1/NCEPDEV/global/glopara/fix/mom6/20220805/post/mx500/"
  wgtsdir="/scratch1/NCEPDEV/global/glopara/fix/mom6/20220805/post/mx100/"

  ;ocnf = addfile("/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT/rt_79026/cpld_control_c48_intel/SST_2021_03_22.nc","r")
  ocnf = addfile("/scratch1/NCEPDEV/stmp2/Denise.Worthen/FV3_RT/rt_31222/cpld_control_nowave_noaero_p8/SST_2021_03_22.nc","r")
  ;printVarSummary(ocnf)
  vname="sst"
  ; get a 2 dimensional field for creating the interpolation masks
  ; the mask2d contains 1's on land and 0's at valid points.
  mask2d = where(ismissing(ocnf->$vname$),  1.0, 0.0)
  ;printVarSummary(mask2d)

;----------------------------------------------------------------------
; loop over the output resolutions
;----------------------------------------------------------------------

      jj = 1
      ii = 0

   ; ; regrid a field to obtain the output xy dimensions
   ;  wgtsfile = wgtsdir+"tripole.mx500.Ct.to.rect.5p00.bilinear.nc"
   ;        tt = ESMF_regrid_with_weights(mask2d(0,:,:),wgtsfile,False)
   ;      tt!0 = "lat"
   ;      tt!1 = "lon"
   ;       lat = tt&lat
   ;       lon = tt&lon
   ;      dims = dimsizes(tt)
   ;      nlat = dims(0)
   ;      nlon = dims(1)
   ;   print("fields will be remapped to destination grid size "\
   ;         +nlon+"  "+nlat)

   ;   delete(tt)
   ;   delete(dims)

    ; regrid the masks to obtain the interpolation masks.
    ; the mask2d contain 1's on land and 0's at valid points.
    ; when remapped any mask value > 0, identifies land values that
    ; have crept into the field. remapped model fields are then
    ; masked with this interpolation mask

    ;wgtsfile = wgtsdir+"tripole.mx500.Ct.to.rect.5p00.bilinear.nc"
      wgtsfile = wgtsdir+"tripole.mx100.Ct.to.rect.1p00.bilinear.nc"
    ;rgmask2d = ESMF_regrid_with_weights(mask2d, wgtsfile,False)

    ; create the interpolation mask
    ;rgmask2d = where(rgmask2d .gt. 0.0, rgmask2d@_FillValue, 1.0)

    ocnvar=ocnf->$vname$(0,:,:)
    rgtt = ESMF_regrid_with_weights(ocnvar,wgtsfile,False)
    ;rgtt = where(ismissing(rgmask2d),ocnvar@_FillValue,rgtt)

    levs = (/-2.0,10.0,1.0/)

    wkstype="X11"
                                ;wkstype="png"
    wkstype@wkWidth=1200
    wkstype@wkHeight=1200
    wks   = gsn_open_wks (wkstype,"cice5")

    res                      = True        ; plot mods desired
    res@cnFillMode = "MeshFill"
    res@mpFillOn   = False
    res@mpOutlineOn   = False

    res@cnFillOn          = True                  ; color fill
    res@cnLinesOn         = False                 ; no contour lines
    res@cnLineLabelsOn    = False
    res@lbLabelBarOn      = True

    ;res@gsnPolar   =  "NH"                         ; specify the hemisphere
    ;res@mpMinLatF    =  60                          ; minimum lat to plot
    ;res@mpCenterLonF = -45

    res@gsnStringFontHeightF = 0.05
    res@cnLevelSelectionMode = "ManualLevels"     ; set the contour levels with the following 3 resources
    res@cnMinLevelValF  = levs(0)                      ; set the minimum contour level
    res@cnMaxLevelValF  = levs(1)                      ; set the maximum contour level
    res@cnLevelSpacingF = levs(2)                      ; set the interval between contours

    ;plot = gsn_csm_contour_map_polar(wks,rgtt,res)
    plot = gsn_csm_contour_map(wks,rgtt,res)
exit
end
