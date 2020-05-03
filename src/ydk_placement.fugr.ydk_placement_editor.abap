FUNCTION ydk_placement_editor.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(REPORT) TYPE  REPID OPTIONAL
*"     REFERENCE(HANDLE) TYPE  YDK_PLACEMENT_VR-HANDLE OPTIONAL
*"     REFERENCE(VARIANT) TYPE  YDK_PLACEMENT_VR-VARIANT OPTIONAL
*"     REFERENCE(AREAS_TAB) TYPE  SDYDO_OPTION_TAB
*"     REFERENCE(LEFT) TYPE  I OPTIONAL
*"     REFERENCE(TOP) TYPE  I OPTIONAL
*"     REFERENCE(WIDTH) TYPE  I OPTIONAL
*"     REFERENCE(HEIGHT) TYPE  I OPTIONAL
*"  EXPORTING
*"     REFERENCE(RET_VARIANT) TYPE  YDK_PLACEMENT_VR-VARIANT
*"  EXCEPTIONS
*"      INVALID_PARAMS
*"----------------------------------------------------------------------

  CLEAR ret_variant.

  gv_report  = report.
  gv_handle  = handle.
  gv_variant = variant.
  gt_areas   = areas_tab.

  IF gv_report IS INITIAL.
    CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD gv_report.    "#EC CI_CCALL
  ENDIF.

  DATA: right TYPE i.
  DATA: bottom TYPE i.

  IF left = 0 AND top = 0.
    CALL SCREEN 100.
  ELSE.
    gv_dialog = abap_true.
    right = left + width.
    bottom = top + height.
    CALL SCREEN 100 STARTING AT left top ENDING AT right bottom.
  ENDIF.

  ret_variant = gv_variant.
ENDFUNCTION.
