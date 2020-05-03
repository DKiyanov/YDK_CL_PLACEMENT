*&---------------------------------------------------------------------*
*& Report  Z_PLACEMENT_EDITOR_TEST
*& ydk_cl_placement test and demo
*&---------------------------------------------------------------------*

REPORT ydk_placement_editor_test.

PARAMETERS: pplacvar TYPE ydk_placement_vr-variant.

DATA: go_placment TYPE REF TO ydk_cl_placement.
DATA: go_root TYPE REF TO cl_gui_docking_container.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pplacvar.
  PERFORM select_variant CHANGING pplacvar.

START-OF-SELECTION.
  IF pplacvar IS INITIAL.
    PERFORM select_variant CHANGING pplacvar.
  ENDIF.

  IF pplacvar IS INITIAL.
    MESSAGE 'Placement variant not selected' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  CALL SCREEN 100.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'MAIN'.

  PERFORM create_objects.
ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      go_placment->save_sizes( ).
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

FORM create_objects.
  CHECK go_root IS INITIAL.

  CREATE OBJECT go_root
    EXPORTING
      side      = cl_gui_docking_container=>dock_at_bottom
      extension = cl_gui_docking_container=>ws_maximizebox.

  CREATE OBJECT go_placment.

  CALL METHOD go_placment->load_placement
    EXPORTING
      root    = go_root
      variant = pplacvar.

  LOOP AT go_placment->itcont ASSIGNING FIELD-SYMBOL(<cont>).
    CASE <cont>-cname.
      WHEN 'AR1'. PERFORM create_object USING <cont>-container 'First object'.
      WHEN 'AR2'. PERFORM create_object USING <cont>-container 'Second object'.
      WHEN 'AR3'. PERFORM create_object USING <cont>-container 'Third object'.
    ENDCASE.
  ENDLOOP.
ENDFORM.

FORM create_object USING container text.
  DATA: lo_textedit TYPE REF TO cl_gui_textedit.

  CREATE OBJECT lo_textedit
    EXPORTING
      parent = container
    EXCEPTIONS
      OTHERS = 1.

  lo_textedit->set_textstream( text = text ).
ENDFORM.

FORM select_variant CHANGING variant TYPE ydk_placement_vr-variant.
  DATA: lt_areas TYPE sdydo_option_tab.

  lt_areas = VALUE #(
    ( value = 'AR1' text = 'First container'  )
    ( value = 'AR2' text = 'Second container' )
    ( value = 'AR3' text = 'Third container'  )
  ).

  CALL METHOD ydk_cl_placement=>variants_dialog
    EXPORTING
      areas_tab           = lt_areas
      left                = 5
      top                 = 3
      can_select          = abap_true
      can_create          = abap_true
      can_edit            = abap_true
      can_delete          = abap_true
      can_set_default_var = abap_true
    RECEIVING
      variant             = variant
    EXCEPTIONS
      cancel              = 1
      OTHERS              = 2.
ENDFORM.
