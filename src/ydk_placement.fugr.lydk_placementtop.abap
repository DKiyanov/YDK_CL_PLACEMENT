FUNCTION-POOL ydk_placement.                "MESSAGE-ID ..

TYPE-POOLS: sdydo.

DATA: gv_report  TYPE sy-repid.
DATA: gv_handle  TYPE ydk_placement_vr-handle.
DATA: gv_variant TYPE ydk_placement_vr-variant.
DATA: gv_vartext TYPE ydk_placement_vr-text.
DATA: gt_areas   TYPE sdydo_option_tab.
DATA: gv_dialog  TYPE abap_bool.

DATA: go_container TYPE REF TO cl_gui_docking_container.
DATA: go_placement TYPE REF TO ydk_cl_placement.

DATA: gv_cname_num TYPE n LENGTH 2.
DATA: gv_variant_dlg TYPE ydk_placement_vr-variant.
DATA: gv_vartext_dlg TYPE ydk_placement_vr-text.
DATA: gv_changed TYPE abap_bool.

TYPES: BEGIN OF ty_var,
         variant TYPE ydk_placement_vr-variant,
         text    TYPE ydk_placement_vr-text,
         isdef   TYPE icon-id,
       END   OF ty_var.
TYPES: ty_var_tab TYPE STANDARD TABLE OF ty_var.

DATA: gt_var TYPE ty_var_tab.

DATA: BEGIN OF gs_can,
        select      TYPE abap_bool,
        create      TYPE abap_bool,
        edit        TYPE abap_bool,
        delete      TYPE abap_bool,
        set_default TYPE abap_bool,
      END   OF gs_can.

CLASS lcl_tile DEFINITION DEFERRED.
DATA: gt_tiles TYPE STANDARD TABLE OF REF TO lcl_tile.

CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA: go_event_receiver TYPE REF TO lcl_event_receiver.

MODULE status_0100 OUTPUT.
  SET TITLEBAR 'MAIN' WITH gv_variant.
  IF gv_dialog = abap_false.
    SET PF-STATUS 'FULLSCR'.
  ELSE.
    SET PF-STATUS 'DIALOG'.
  ENDIF.

  PERFORM create_objects.
ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE user_command_0100 INPUT.
  PERFORM user_command_0100.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

FORM user_command_0100.
  DATA: answer TYPE c LENGTH 1.
  CASE sy-ucomm.
    WHEN 'CANC'.
      sy-subrc = 4.
      LEAVE TO SCREEN 0.
    WHEN 'OKAY'.
      PERFORM save.
      sy-subrc = 0.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      IF gv_changed = abap_true.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
*           DEFAULTOPTION  = 'Y'
            textline1      = 'Save the changes'(svc)
*           TEXTLINE2      = ' '
            titel          = 'Exit'(ext)
*           START_COLUMN   = 25
*           START_ROW      = 6
            cancel_display = 'X'
          IMPORTING
            answer         = answer.

        CASE answer.
          WHEN 'A'.
            EXIT.
          WHEN 'J'.
            PERFORM save.
        ENDCASE.
      ENDIF.

      sy-subrc = 0.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM save.
  ENDCASE.
ENDFORM.


CLASS lcl_tile DEFINITION
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA parent TYPE REF TO cl_gui_container.
    DATA doc TYPE REF TO cl_dd_document.
    DATA select   TYPE REF TO cl_dd_select_element.
    DATA button_l TYPE REF TO cl_dd_button_element.
    DATA button_r TYPE REF TO cl_dd_button_element.
    DATA button_t TYPE REF TO cl_dd_button_element.
    DATA button_b TYPE REF TO cl_dd_button_element.

    METHODS constructor
      IMPORTING
        opt_tab TYPE sdydo_option_tab
        opt_sel TYPE clike.

    METHODS display
      IMPORTING parent TYPE REF TO cl_gui_container.

    EVENTS button_click EXPORTING VALUE(kind) TYPE char1.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS handle_button_click
          FOR EVENT clicked OF cl_dd_button_element
      IMPORTING
          sender.
ENDCLASS.

CLASS lcl_tile IMPLEMENTATION.
  METHOD constructor.
    DATA ta TYPE REF TO cl_dd_table_element.
    DATA col TYPE REF TO cl_dd_area.
    DATA hform  TYPE REF TO cl_dd_form_area.

    CREATE OBJECT doc.

    CALL METHOD doc->initialize_document.

    CALL METHOD doc->add_table
      EXPORTING
        no_of_columns = 1
        width         = '100%'
        border        = '0'
      IMPORTING
        table         = ta.

    CALL METHOD ta->add_column
      IMPORTING
        column = col.

    CALL METHOD ta->set_column_style
      EXPORTING
        col_no     = 1
        sap_align  = 'CENTER'
        sap_valign = 'CENTER'.

    CALL METHOD col->add_form
      IMPORTING
        formarea = hform.

*--------
    CALL METHOD hform->line_with_layout
      EXPORTING
        start = 'X'.

    CALL METHOD hform->add_button
      EXPORTING
        sap_icon = 'ICON_TOTAL_UP'
      IMPORTING
        button   = button_t.
    SET HANDLER me->handle_button_click FOR button_t.

    CALL METHOD hform->line_with_layout
      EXPORTING
        end = 'X'.
*---------------

*---------------
    CALL METHOD hform->line_with_layout
      EXPORTING
        start = 'X'.

    CALL METHOD hform->add_button
      EXPORTING
        sap_icon = 'ICON_TOTAL_LEFT'
      IMPORTING
        button   = button_l.
    SET HANDLER me->handle_button_click FOR button_l.

    CALL METHOD hform->add_gap
      EXPORTING
        width = 2.

    CALL METHOD hform->add_text
      EXPORTING
        text = 'Area'(are).

    CALL METHOD hform->add_gap
      EXPORTING
        width = 2.

    CALL METHOD hform->add_select_element
      EXPORTING
        options        = opt_tab
        value          = CONV #( opt_sel )
      IMPORTING
        select_element = select.

    CALL METHOD hform->add_gap
      EXPORTING
        width = 2.

    CALL METHOD hform->add_button
      EXPORTING
        sap_icon = 'ICON_TOTAL_RIGHT'
      IMPORTING
        button   = button_r.
    SET HANDLER me->handle_button_click FOR button_r.

    CALL METHOD hform->line_with_layout
      EXPORTING
        end = 'X'.
*----------------

*----------------
    CALL METHOD hform->line_with_layout
      EXPORTING
        start = 'X'.

    CALL METHOD hform->add_button
      EXPORTING
        sap_icon = 'ICON_TOTAL_DOWN'
      IMPORTING
        button   = button_b.
    SET HANDLER me->handle_button_click FOR button_b.

    CALL METHOD hform->line_with_layout
      EXPORTING
        end = 'X'.
*--------------

    CALL METHOD doc->merge_document.

    LOOP AT doc->html_table ASSIGNING FIELD-SYMBOL(<html>) WHERE line CP '*<table*id="A1T1"*'.
      CONCATENATE <html>-line 'height="100&#x25;"' INTO <html>-line SEPARATED BY space.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

  METHOD display.
    me->parent = parent.
    CALL METHOD doc->display_document
      EXPORTING
        reuse_control = 'X'
        parent        = me->parent.
  ENDMETHOD.

  METHOD handle_button_click.
*      FOR EVENT clicked OF cl_dd_button_element
*  IMPORTING
*      sender.
    DATA: kind TYPE char1.

    CASE sender.
      WHEN button_l. kind = 'L'.
      WHEN button_r. kind = 'R'.
      WHEN button_t. kind = 'T'.
      WHEN button_b. kind = 'B'.
    ENDCASE.

    RAISE EVENT button_click
      EXPORTING
        kind = kind.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_button_click
          FOR EVENT button_click OF lcl_tile
      IMPORTING
          kind
          sender.
ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_button_click.
*    FOR EVENT button_click OF lcl_tile
*      IMPORTING
*        kind
*        sender.
    DATA: cname TYPE string.
    DATA: side TYPE ydk_placement_side.
    DATA: ncname TYPE string.
    DATA: ncontainer TYPE REF TO cl_gui_container.
    FIELD-SYMBOLS <cont> LIKE LINE OF go_placement->itcont.
    DATA: sender_tile TYPE REF TO lcl_tile.
    DATA: o_sel_area TYPE string.

    sender_tile ?= sender.

    side = kind.

    LOOP AT go_placement->itcont ASSIGNING <cont> WHERE container = sender_tile->parent AND NOT cname IS INITIAL.
      cname = <cont>-cname.
      EXIT.
    ENDLOOP.
    CHECK NOT cname IS INITIAL.

    o_sel_area = sender_tile->select->value.
    DELETE gt_tiles WHERE table_line = sender_tile.

    LOOP AT <cont>-container->children ASSIGNING FIELD-SYMBOL(<child>).
      <child>->free( ).
    ENDLOOP.

    ADD 1 TO gv_cname_num.
    CONCATENATE 'C' gv_cname_num INTO ncname.

    ncontainer = go_placement->add_container( cname = cname side = side ncname = ncname ).

* Создаём новую плитку для старого контейнера
    READ TABLE go_placement->itcont ASSIGNING <cont> WITH KEY cname = cname.
    PERFORM show_tile USING <cont>-container o_sel_area.

* Создаём новую плитку для нового контейнера
    PERFORM show_tile USING ncontainer ''.

    gv_changed = abap_true.
  ENDMETHOD.
ENDCLASS.

FORM create_objects.
  CHECK go_container IS INITIAL.

  CREATE OBJECT go_container
    EXPORTING
      side      = cl_gui_docking_container=>dock_at_bottom
      extension = cl_gui_docking_container=>ws_maximizebox
      metric    = cl_gui_control=>metric_pixel.

  CREATE OBJECT go_placement
    EXPORTING
      report = gv_report
      handle = gv_handle.

  DATA: ltplacment TYPE ydk_cl_placement=>ty_placment_tab.
  DATA: ncname TYPE string.

  IF NOT gv_variant IS INITIAL.
    SELECT SINGLE text INTO gv_vartext
      FROM ydk_placement_vr
     WHERE variant = gv_variant.
    IF sy-subrc = 0.
      go_placement->load_placement( root = go_container variant = gv_variant ).
    ENDIF.
  ENDIF.

  IF go_placement->itplacment IS INITIAL.
    ADD 1 TO gv_cname_num.
    CONCATENATE 'C' gv_cname_num INTO ncname.

    ltplacment = VALUE #( ( ncname = ncname ) ).
    go_placement->create_placement( root = go_container placment = ltplacment ).
  ENDIF.

  CREATE OBJECT go_event_receiver.

  DATA: lv_sel_area TYPE string.

  INSERT INITIAL LINE INTO gt_areas INDEX 1.

  LOOP AT go_placement->itcont ASSIGNING FIELD-SYMBOL(<cont>) WHERE NOT cname IS INITIAL.
    CLEAR lv_sel_area.
    READ TABLE gt_areas ASSIGNING FIELD-SYMBOL(<area>) WITH KEY value = <cont>-cname.
    IF sy-subrc = 0.
      lv_sel_area = <area>-value.
    ENDIF.

    PERFORM show_tile USING <cont>-container lv_sel_area.
  ENDLOOP.
ENDFORM.

FORM show_tile USING container TYPE REF TO cl_gui_container sel_area.
  DATA: lo_tile TYPE REF TO lcl_tile.

  CREATE OBJECT lo_tile
    EXPORTING
      opt_tab = gt_areas
      opt_sel = sel_area.
  APPEND lo_tile TO gt_tiles.
  SET HANDLER go_event_receiver->handle_button_click FOR lo_tile.
  lo_tile->display( container ).
ENDFORM.

FORM save.
  gv_variant_dlg = gv_variant.
  gv_vartext_dlg = gv_vartext.

  CALL SCREEN 200 STARTING AT 10 10.
  CHECK sy-subrc = 0.

  PERFORM save_var USING gv_variant_dlg gv_vartext_dlg.
ENDFORM.

FORM save_var USING variant text.
  DATA: ltplacment TYPE ydk_cl_placement=>ty_placment_tab.
  FIELD-SYMBOLS <placment> LIKE LINE OF ltplacment.
  FIELD-SYMBOLS <cont> LIKE LINE OF go_placement->itcont.

  ltplacment = go_placement->itplacment.

  LOOP AT gt_tiles ASSIGNING FIELD-SYMBOL(<tile>).
    READ TABLE go_placement->itcont ASSIGNING <cont> WITH KEY container = <tile>->parent.
    CHECK sy-subrc = 0.

    LOOP AT ltplacment ASSIGNING <placment> WHERE cname = <cont>-cname.
      <placment>-cname = <tile>->select->value.
    ENDLOOP.
    LOOP AT ltplacment ASSIGNING <placment> WHERE ncname = <cont>-cname.
      <placment>-ncname = <tile>->select->value.
    ENDLOOP.
  ENDLOOP.

  go_placement->save_placement( placment = ltplacment variant = CONV #( variant ) text = text ).
  go_placement->save_sizes( for_user = abap_false ).

  gv_variant = gv_variant_dlg.
  gv_vartext = gv_vartext_dlg.
  gv_changed = abap_false.
ENDFORM.

MODULE status_0200 OUTPUT.
  SET PF-STATUS 'DIALOG'.
  SET TITLEBAR  'SAVE'.

  IF NOT gv_variant IS INITIAL.
    LOOP AT SCREEN.
      CHECK screen-name = 'GV_VARIANT_DLG'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " STATUS_0200  OUTPUT

MODULE user_command_0200 INPUT.
  PERFORM user_command_0200.
ENDMODULE.                 " USER_COMMAND_0200  INPUT

FORM user_command_0200.
  DATA: answer TYPE c LENGTH 1.
  CASE sy-ucomm.
    WHEN 'OKAY'.
      IF gv_variant_dlg <> gv_variant.
        SELECT SINGLE variant INTO gv_variant_dlg
          FROM ydk_placement_vr
         WHERE report = gv_report
           AND handle = gv_handle
           AND variant = gv_variant_dlg.
        IF sy-subrc = 0.
          CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
            EXPORTING
*             DEFAULTOPTION  = 'Y'
              textline1      = 'The placement variant already exit'(001)
              textline2      = 'Rewrite?'(002)
              titel          = 'Saving the placement variant'(003)
*             START_COLUMN   = 25
*             START_ROW      = 6
              cancel_display = ' '
            IMPORTING
              answer         = answer.
          CHECK answer = 'J'.
        ENDIF.
      ENDIF.

      CLEAR sy-subrc.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.

MODULE at_exit_command INPUT.
  CASE sy-ucomm.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " AT_EXIT_COMMAND  INPUT
