class YDK_CL_PLACEMENT definition
  public
  final
  create public .

public section.

  types:
* itplacment - это запись порядка действий для создания требуемого размещения контейнеров
* каждая строка это действие по вставке CL_GUI_SPLITTER_CONTAINER
* после вставки CL_GUI_SPLITTER_CONTAINER к его контейнерам присваются имена
* исходного и нового в зависимоти от стороны вставки по отношению к исходному
    BEGIN OF ty_placment,
        cname  TYPE string,     " имя контейнера в который будет встален CL_GUI_EASY_SPLITTER_CONTAINER
        side   TYPE c LENGTH 1, " сторона с которой будет размещён новый контейнер
        ncname TYPE string,     " имя нового контейнера
        nsize  TYPE i,          " размер %
      END   OF ty_placment .
  types:
    ty_placment_tab TYPE STANDARD TABLE OF ty_placment .
  types:
    BEGIN OF ty_cont,
        cname     TYPE string,
        splitter  TYPE REF TO cl_gui_splitter_container,
        row       TYPE i,
        col       TYPE i,
        container TYPE REF TO cl_gui_container,
        size      TYPE i,
      END   OF ty_cont .
  types:
    ty_cont_tab TYPE STANDARD TABLE OF ty_cont .
  types:
    BEGIN OF ty_cont_size,
             cname TYPE string,
             size  TYPE i,
           END   OF ty_cont_size .
  types:
    ty_cont_size_tab TYPE STANDARD TABLE OF ty_cont_size .

  data REPORT type REPID read-only .
  data HANDLE type YDK_PLACEMENT_VR-HANDLE read-only .
  data VARIANT type YDK_PLACEMENT_VR-VARIANT read-only .
  data ITPLACMENT type TY_PLACMENT_TAB read-only .
  data ITCONT type TY_CONT_TAB read-only .

  methods CONSTRUCTOR
    importing
      value(REPORT) type REPID optional
      !HANDLE type YDK_PLACEMENT_VR-HANDLE optional .
  type-pools ABAP .
  methods LOAD_PLACEMENT
    importing
      !ROOT type ref to CL_GUI_CONTAINER
      !VARIANT type YDK_PLACEMENT_VR-VARIANT
      !LOAD_SIZES type ABAP_BOOL default ABAP_TRUE .
  methods CREATE_PLACEMENT
    importing
      !ROOT type ref to CL_GUI_CONTAINER
      !PLACMENT type TY_PLACMENT_TAB .
  methods SAVE_PLACEMENT
    importing
      !PLACMENT type TY_PLACMENT_TAB optional
      !VARIANT type YDK_PLACEMENT_VR-VARIANT
      !TEXT type CLIKE .
  methods SAVE_SIZES
    importing
      !FOR_USER type ABAP_BOOL default ABAP_TRUE .
  methods ADD_CONTAINER
    importing
      !CNAME type CLIKE
      !SIDE type YDK_PLACEMENT_SIDE
      !NCNAME type CLIKE
      !NSIZE type I optional
    returning
      value(N_CONTAINER) type ref to CL_GUI_CONTAINER .
  methods LOAD_SIZES .
  class-methods VARIANT_EXISTS
    importing
      value(REPORT) type REPID optional
      !HANDLE type YDK_PLACEMENT_VR-HANDLE optional
      !VARIANT type YDK_PLACEMENT_VR-VARIANT
    returning
      value(EXISTS) type ABAP_BOOL .
  type-pools SDYDO .
  class-methods VARIANT_EDIT
    importing
      value(REPORT) type REPID optional
      !HANDLE type YDK_PLACEMENT_VR-HANDLE optional
      !VARIANT type YDK_PLACEMENT_VR-VARIANT optional
      !AREAS_TAB type SDYDO_OPTION_TAB
      !LEFT type I optional
      !TOP type I optional
      !WIDTH type I optional
      !HEIGHT type I optional
    returning
      value(RET_VARIANT) type YDK_PLACEMENT_VR-VARIANT .
  class-methods VARIANT_DELETE
    importing
      value(REPORT) type REPID optional
      !HANDLE type YDK_PLACEMENT_VR-HANDLE optional
      !VARIANT type YDK_PLACEMENT_VR-VARIANT optional .
  class-methods VARIANT_GET_DEFAULT
    importing
      value(REPORT) type REPID optional
      !HANDLE type YDK_PLACEMENT_VR-HANDLE optional
    returning
      value(VARIANT) type YDK_PLACEMENT_VR-VARIANT
    exceptions
      NOT_FOUND .
  class-methods VARIANTS_DIALOG
    importing
      value(REPORT) type REPID optional
      !HANDLE type YDK_PLACEMENT_VR-HANDLE optional
      !AREAS_TAB type SDYDO_OPTION_TAB optional
      !LEFT type I default 10
      !TOP type I default 10
      !CAN_SELECT type ABAP_BOOL optional
      !CAN_CREATE type ABAP_BOOL optional
      !CAN_EDIT type ABAP_BOOL optional
      !CAN_DELETE type ABAP_BOOL optional
      !CAN_SET_DEFAULT_VAR type ABAP_BOOL optional
    returning
      value(VARIANT) type YDK_PLACEMENT_VR-VARIANT
    exceptions
      CANCEL .
protected section.
private section.
ENDCLASS.



CLASS YDK_CL_PLACEMENT IMPLEMENTATION.


  METHOD add_container.
    FIELD-SYMBOLS <placment> LIKE LINE OF itplacment.
    READ TABLE itplacment WITH KEY cname = cname ncname = ncname TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO itplacment ASSIGNING <placment>.
      <placment>-cname  = cname.
      <placment>-side   = side.
      <placment>-ncname = ncname.
      <placment>-nsize  = nsize.
    ENDIF.

    DATA: n_splitter  TYPE REF TO cl_gui_splitter_container.
    FIELD-SYMBOLS <cont> LIKE LINE OF itcont.

    IF cname IS INITIAL. " добавляется первый (корневой элемент)
      READ TABLE itcont ASSIGNING <cont> WITH KEY cname = '@ROOT@'.
      CHECK sy-subrc = 0.

      CLEAR <cont>-cname.

      n_container = <cont>-container.

      APPEND INITIAL LINE TO itcont ASSIGNING <cont>.
      <cont>-cname     = ncname.
      <cont>-container = n_container.

      RETURN.
    ENDIF.

    READ TABLE itcont ASSIGNING <cont> WITH KEY cname = cname.
    CHECK sy-subrc = 0.

    DATA: o_row TYPE i VALUE 1.
    DATA: n_row TYPE i VALUE 1.
    DATA: o_col TYPE i VALUE 1.
    DATA: n_col TYPE i VALUE 1.

    CASE side.
      WHEN 'L'. o_col = 2.
      WHEN 'R'. n_col = 2.
      WHEN 'T'. o_row = 2.
      WHEN 'B'. n_row = 2.
    ENDCASE.

    CASE side.
      WHEN 'L' OR 'R'.
        CREATE OBJECT n_splitter
          EXPORTING
            parent  = <cont>-container
            rows    = 1
            columns = 2.

        IF NOT nsize IS INITIAL.
          CALL METHOD n_splitter->set_column_width
            EXPORTING
              id    = n_col
              width = nsize.
        ENDIF.
      WHEN 'T' OR 'B'.
        CREATE OBJECT n_splitter
          EXPORTING
            parent  = <cont>-container
            rows    = 2
            columns = 1.

        IF NOT nsize IS INITIAL.
          CALL METHOD n_splitter->set_row_height
            EXPORTING
              id     = n_row
              height = nsize.
        ENDIF.
    ENDCASE.

    CALL METHOD n_splitter->set_border
      EXPORTING
        border = cl_gui_cfw=>false.

    CLEAR <cont>-cname.

    DATA: o_container TYPE REF TO  cl_gui_container.

    CALL METHOD n_splitter->get_container
      EXPORTING
        row       = o_row
        column    = o_col
      RECEIVING
        container = o_container.

    APPEND INITIAL LINE TO itcont ASSIGNING <cont>.
    <cont>-cname     = cname.
    <cont>-splitter  = n_splitter.
    <cont>-row       = o_row.
    <cont>-col       = o_col.
    <cont>-container = o_container.

    CALL METHOD n_splitter->get_container
      EXPORTING
        row       = n_row
        column    = n_col
      RECEIVING
        container = n_container.

    APPEND INITIAL LINE TO itcont ASSIGNING <cont>.
    <cont>-cname     = ncname.
    <cont>-splitter  = n_splitter.
    <cont>-row       = n_row.
    <cont>-col       = n_col.
    <cont>-container = n_container.
  ENDMETHOD.


  METHOD constructor.
    me->report = report.
    IF me->report IS INITIAL.
      CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD me->report. "#EC CI_CCALL
    ENDIF.

    me->handle = handle.
  ENDMETHOD.


  METHOD create_placement.
    REFRESH: itcont.

    itplacment = placment.

    itcont = VALUE #( ( cname = '@ROOT@' container = root ) ).

    FIELD-SYMBOLS <placment> TYPE ty_placment.

    LOOP AT itplacment ASSIGNING <placment>.
      add_container( cname = <placment>-cname side = <placment>-side ncname = <placment>-ncname nsize = <placment>-nsize ).
    ENDLOOP.
  ENDMETHOD.


  METHOD load_placement.
    DATA: rawdata TYPE ydk_placement_vr-rawdata.

    SELECT SINGLE rawdata INTO rawdata
      FROM ydk_placement_vr
     WHERE report  = report
       AND handle  = handle
       AND variant = variant.
    CHECK sy-subrc = 0.

    IMPORT placment TO itplacment FROM DATA BUFFER rawdata.

    create_placement( root = root placment = itplacment ).

    me->variant = variant.

    IF load_sizes = abap_true.
      load_sizes( ).
    ENDIF.
  ENDMETHOD.


  METHOD load_sizes.
    DATA: rawdata TYPE ydk_placement_sz-rawdata.
    DATA: itcs TYPE ty_cont_size_tab.
    FIELD-SYMBOLS <cs> LIKE LINE OF itcs.

    SELECT SINGLE rawdata INTO rawdata
      FROM ydk_placement_sz
     WHERE report  = report
       AND handle  = handle
       AND variant = variant
       AND uname   = sy-uname.
    IF sy-subrc <> 0.
      SELECT SINGLE rawdata INTO rawdata
        FROM ydk_placement_sz
       WHERE report  = report
         AND handle  = handle
         AND variant = variant
         AND uname   = ''.
    ENDIF.

    CHECK sy-subrc = 0.

    IMPORT sizes TO itcs FROM DATA BUFFER rawdata.

    DATA: lv_index TYPE i.

    LOOP AT itcont ASSIGNING FIELD-SYMBOL(<cont>) WHERE row =  2 OR col = 2.
      ADD 1 TO lv_index.
      READ TABLE itcs ASSIGNING <cs> INDEX lv_index.
      CHECK sy-subrc = 0.
      CHECK <cs>-cname = <cont>-cname.

      IF <cont>-row = 2.
        CALL METHOD <cont>-splitter->set_row_height
          EXPORTING
            id     = 2
            height = <cs>-size.
      ELSE.
        CALL METHOD <cont>-splitter->set_column_width
          EXPORTING
            id    = 2
            width = <cs>-size.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD save_placement.

    DATA: pvr TYPE ydk_placement_vr.
    FIELD-SYMBOLS <placment> TYPE ty_placment_tab.

    IF placment IS SUPPLIED.
      ASSIGN placment TO <placment>.
    ELSE.
      ASSIGN me->itplacment TO <placment>.
    ENDIF.

    pvr-report  = report.
    pvr-handle  = handle.
    pvr-variant = variant.
    pvr-text    = text.
    EXPORT placment FROM <placment> TO DATA BUFFER pvr-rawdata.

    MODIFY ydk_placement_vr FROM pvr.

    me->variant = variant.
  ENDMETHOD.


  METHOD save_sizes.
* report и handle оперделяются при создании объекта
* variant определяется  при загрузке или при сохранении

    DATA: itcs TYPE ty_cont_size_tab.
    FIELD-SYMBOLS <cs> LIKE LINE OF itcs.

    LOOP AT itcont ASSIGNING FIELD-SYMBOL(<cont>) WHERE row =  2 OR col = 2.
      APPEND INITIAL LINE TO itcs ASSIGNING <cs>.
      <cs>-cname = <cont>-cname.

      IF <cont>-row = 2.
        CALL METHOD <cont>-splitter->get_row_height
          EXPORTING
            id     = 2
          IMPORTING
            result = <cs>-size.
      ELSE.
        CALL METHOD <cont>-splitter->get_column_width
          EXPORTING
            id     = 2
          IMPORTING
            result = <cs>-size.
      ENDIF.

      cl_gui_cfw=>flush( ).
    ENDLOOP.

    DATA: psz TYPE ydk_placement_sz.

    psz-report  = report.
    psz-handle  = handle.
    psz-variant = variant.
    IF for_user = abap_true.
      psz-uname   = sy-uname.
    ENDIF.
    EXPORT sizes FROM itcs TO DATA BUFFER psz-rawdata.

    MODIFY ydk_placement_sz FROM psz.
  ENDMETHOD.


  METHOD variants_dialog.
    IF report IS INITIAL.
      CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD report.     "#EC CI_CCALL
    ENDIF.

    CALL FUNCTION 'YDK_PLACEMENT_VARIANTS'
      EXPORTING
        report              = report
        handle              = handle
        areas_tab           = areas_tab
        left                = left
        top                 = top
        can_select          = can_select
        can_create          = can_create
        can_edit            = can_edit
        can_delete          = can_delete
        can_set_default_var = can_set_default_var
      IMPORTING
        variant             = variant
      EXCEPTIONS
        cancel              = 1.

    IF sy-subrc = 1.
      RAISE cancel.
    ENDIF.
  ENDMETHOD.


  METHOD variant_delete.
    IF report IS INITIAL.
      CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD report.     "#EC CI_CCALL
    ENDIF.

    DELETE FROM ydk_placement_vr
     WHERE report  = report
       AND handle  = handle
       AND variant = variant.

    DELETE FROM ydk_placement_sz
     WHERE report  = report
       AND handle  = handle
       AND variant = variant.
  ENDMETHOD.


  METHOD variant_edit.
    IF report IS INITIAL.
      CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD report.     "#EC CI_CCALL
    ENDIF.

    CALL FUNCTION 'YDK_PLACEMENT_EDITOR'
      EXPORTING
        report      = report
        handle      = handle
        variant     = variant
        areas_tab   = areas_tab
        left        = left
        top         = top
        width       = width
        height      = height
      IMPORTING
        ret_variant = ret_variant.
  ENDMETHOD.


  METHOD variant_exists.
    IF report IS INITIAL.
      CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD report.     "#EC CI_CCALL
    ENDIF.

    exists = abap_false.

    SELECT SINGLE @abap_true INTO @exists
      FROM ydk_placement_vr
     WHERE report  = @report
       AND handle  = @handle
       AND variant = @variant.
  ENDMETHOD.


  METHOD variant_get_default.
    IF report IS INITIAL.
      CALL 'AB_GET_CALLER' ID 'PROGRAM' FIELD report.     "#EC CI_CCALL
    ENDIF.

    SELECT SINGLE variant INTO variant
      FROM ydk_placement_vr
     WHERE report  = report
       AND handle  = handle
       AND isdef   = abap_true.
    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
