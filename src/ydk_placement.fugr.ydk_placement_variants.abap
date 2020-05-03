FUNCTION ydk_placement_variants .
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     VALUE(REPORT) TYPE  REPID OPTIONAL
*"     REFERENCE(HANDLE) TYPE  YDK_PLACEMENT_VR-HANDLE OPTIONAL
*"     REFERENCE(AREAS_TAB) TYPE  SDYDO_OPTION_TAB OPTIONAL
*"     REFERENCE(LEFT) TYPE  I DEFAULT 10
*"     REFERENCE(TOP) TYPE  I DEFAULT 10
*"     REFERENCE(CAN_SELECT) TYPE  ABAP_BOOL OPTIONAL
*"     REFERENCE(CAN_CREATE) TYPE  ABAP_BOOL OPTIONAL
*"     REFERENCE(CAN_EDIT) TYPE  ABAP_BOOL OPTIONAL
*"     REFERENCE(CAN_DELETE) TYPE  ABAP_BOOL OPTIONAL
*"     REFERENCE(CAN_SET_DEFAULT_VAR) TYPE  ABAP_BOOL OPTIONAL
*"  EXPORTING
*"     REFERENCE(VARIANT) TYPE  YDK_PLACEMENT_VR-VARIANT
*"  EXCEPTIONS
*"      CANCEL
*"      INVALID_PARAMS
*"----------------------------------------------------------------------

  CLEAR variant.

  gv_report  = report.
  gv_handle  = handle.
  gv_variant = variant.
  gt_areas   = areas_tab.

  gs_can-select      = can_select.
  gs_can-create      = can_create.
  gs_can-edit        = can_edit.
  gs_can-delete      = can_delete.
  gs_can-set_default = can_set_default_var.

  IF NOT areas_tab IS SUPPLIED AND ( gs_can-create = abap_true OR gs_can-edit = abap_true ).
    RAISE invalid_params.
  ENDIF.

  IF gv_report IS INITIAL.
    CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD gv_report.    "#EC CI_CCALL
  ENDIF.

  SELECT variant text isdef INTO CORRESPONDING FIELDS OF TABLE gt_var
    FROM ydk_placement_vr
   WHERE report = gv_report
     AND handle = gv_handle.

  DATA: fc      TYPE lvc_t_fcat.
  DATA: repid   TYPE sy-repid.
  DATA: right   TYPE i.
  DATA: bottom  TYPE i.
  DATA: us_exit TYPE slis_exit_by_user.

  FIELD-SYMBOLS <var> LIKE LINE OF gt_var.

  fc = VALUE #( ( fieldname = 'VARIANT' ref_field = 'VARIANT' ref_table = 'YDK_PLACEMENT_VR' )
                ( fieldname = 'TEXT'    ref_field = 'TEXT'    ref_table = 'YDK_PLACEMENT_VR')
                ( fieldname = 'ISDEF'   ref_field = 'ISDEF'   ref_table = 'YDK_PLACEMENT_VR')
              ).

  IF gs_can-select = abap_true.
    fc[ fieldname = 'VARIANT' ]-hotspot = abap_true.
  ENDIF.

  IF gs_can-set_default IS INITIAL.
    fc[ fieldname = 'ISDEF' ]-tech = abap_true.
  ELSE.
    fc[ fieldname = 'ISDEF' ]-icon    = abap_true.
    fc[ fieldname = 'ISDEF' ]-hotspot = abap_true.

    LOOP AT gt_var ASSIGNING <var>.
      IF <var>-isdef = 'X'.
        <var>-isdef = icon_okay.
      ELSE.
        <var>-isdef = icon_wd_radio_button_empty.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DATA: lv_lines TYPE i.
  lv_lines = lines( gt_var ).
  IF lv_lines > 15.
    lv_lines = 15.
  ENDIF.

  repid = sy-repid.
  right = left + 60.
  bottom = top + lv_lines + 3.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       =
*     I_BUFFER_ACTIVE          =
      i_callback_program       = repid
      i_callback_pf_status_set = 'ALV_STATUS_SET'
      i_callback_user_command  = 'ALV_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
*     is_layout_lvc            = layout
      it_fieldcat_lvc          = fc[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS_LVC    =
*     IT_SORT_LVC              =
*     IT_FILTER_LVC            =
*     IT_HYPERLINK             =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'A'
*     IS_VARIANT               =
*     IT_EVENTS                =
*     IT_EVENT_EXIT            =
*     IS_PRINT_LVC             =
*     IS_REPREP_ID_LVC         =
      i_screen_start_column    = left
      i_screen_start_line      = top
      i_screen_end_column      = right
      i_screen_end_line        = bottom
*     I_HTML_HEIGHT_TOP        =
*     I_HTML_HEIGHT_END        =
*     IT_ALV_GRAPHICS          =
*     IT_EXCEPT_QINFO_LVC      =
*     IR_SALV_FULLSCREEN_ADAPTER        =
    IMPORTING
*     e_exit_caused_by_caller  =
      es_exit_caused_by_user   = us_exit
    TABLES
      t_outtab                 = gt_var
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF gs_can-select = abap_true.
    IF us_exit-back = abap_true.
      RAISE cancel.
    ENDIF.

    variant = gv_variant.
  ENDIF.
ENDFUNCTION.

FORM alv_status_set USING extab TYPE slis_t_extab.
  IF gs_can-edit IS INITIAL.
    APPEND 'EDIT' TO extab.
  ENDIF.
  IF gs_can-create IS INITIAL.
    APPEND 'CREATE' TO extab.
  ENDIF.
  IF gs_can-delete IS INITIAL.
    APPEND 'DELETE' TO extab.
  ENDIF.

  SET PF-STATUS 'ALV' EXCLUDING extab.
ENDFORM.                    "ALV_STATUS

FORM alv_user_command USING ucomm    TYPE sy-ucomm
                            selfield TYPE slis_selfield.

  selfield-refresh    = abap_true.
  selfield-col_stable = abap_true.
  selfield-row_stable = abap_true.

  FIELD-SYMBOLS <var> LIKE LINE OF gt_var.

  IF NOT selfield-tabindex IS INITIAL.
    READ TABLE gt_var ASSIGNING <var> INDEX selfield-tabindex.
  ENDIF.

  CASE ucomm.
    WHEN '&IC1'. " Double click
      IF selfield-fieldname = 'ISDEF'.
        LOOP AT gt_var ASSIGNING <var>.
          <var>-isdef = icon_wd_radio_button_empty.
        ENDLOOP.

        UPDATE ydk_placement_vr SET isdef = abap_false
         WHERE report = gv_report
           AND handle = gv_handle.

        IF selfield-value = icon_wd_radio_button_empty.
          READ TABLE gt_var ASSIGNING <var> INDEX selfield-tabindex.
          <var>-isdef = icon_okay.

          UPDATE ydk_placement_vr SET isdef = abap_true
           WHERE report = gv_report
             AND handle = gv_handle
             AND variant = <var>-variant.
        ENDIF.

        COMMIT WORK.

        RETURN.
      ENDIF.

      IF gs_can-select = abap_true.
        gv_variant = <var>-variant.
        selfield-exit = abap_true.
        RETURN.
      ENDIF.

      IF gs_can-edit = abap_true.
        CALL FUNCTION 'YDK_PLACEMENT_EDITOR'
          EXPORTING
            report    = gv_report
            handle    = gv_handle
            variant   = <var>-variant
            areas_tab = gt_areas.

        <var>-text = gv_vartext.

        RETURN.
      ENDIF.
    WHEN 'EDIT'.
      CHECK <var> IS ASSIGNED.

      CALL FUNCTION 'YDK_PLACEMENT_EDITOR'
        EXPORTING
          report    = gv_report
          handle    = gv_handle
          variant   = <var>-variant
          areas_tab = gt_areas.

      <var>-text = gv_vartext.
    WHEN 'CREATE'.
      CALL FUNCTION 'YDK_PLACEMENT_EDITOR'
        EXPORTING
          report    = gv_report
          handle    = gv_handle
*         variant   =
          areas_tab = gt_areas.

      IF NOT gv_variant IS INITIAL AND NOT line_exists( gt_var[ variant = gv_variant ] ).
        APPEND INITIAL LINE TO gt_var ASSIGNING <var>.
        <var>-variant = gv_variant.
        <var>-text    = gv_vartext.
      ENDIF.
    WHEN 'DELETE'.
      DATA: answer TYPE c LENGTH 1.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
*         DEFAULTOPTION  = 'Y'
          textline1      = |{ 'Delete variant'(dv1) } { <var>-variant }|
*         TEXTLINE2      = ' '
          titel          = 'Deleting a varian'(dv2)
*         START_COLUMN   = 25
*         START_ROW      = 6
          cancel_display = ' '
        IMPORTING
          answer         = answer.
      CHECK answer = 'J'.

      CALL METHOD ydk_cl_placement=>variant_delete
        EXPORTING
          report  = gv_report
          handle  = gv_handle
          variant = <var>-variant.

      DELETE gt_var INDEX selfield-tabindex.
  ENDCASE.
ENDFORM.                    "ALV_COMMAND
