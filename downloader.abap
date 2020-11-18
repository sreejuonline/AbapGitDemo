*&---------------------------------------------------------------------*
*& Report ZAPPSCAN_DOWNLOADER
*&---------------------------------------------------------------------*
*& Program to download the source code for AppScan to consume and scan *
*  the source code for security vulnerability issues                   *
*  The program has option to downlad source code at package level or   *
*  at a transport request level                                        *
*  Also possible to download source code at individual object level    *
*  The proram downloads a package as a zip file containing individual  *
*  .ABAP files containing the programs, FM or classes                  *
*&---------------------------------------------------------------------*
REPORT zappscan_downloader.

TABLES:
  seoclasstx,
  tadir,
  tlibt,
  d020s,
  trdir.

DATA lv_trkorr TYPE trkorr.
DATA lv_trdnam TYPE trdir-name.
DATA lv_subc TYPE trdir-subc.
DATA lv_dynn TYPE d020s-dnum.
DATA lv_cnam TYPE trdir-cnam.
DATA lv_unam TYPE trdir-unam.
DATA lv_appl    TYPE taplt-appl.
DATA lv_devcls TYPE devclass.

SELECTION-SCREEN  BEGIN OF BLOCK: b01 WITH FRAME TITLE TEXT-b01.
SELECT-OPTIONS    s_tr     FOR lv_trkorr NO INTERVALS .
SELECT-OPTIONS    s_devcla   FOR lv_devcls NO INTERVALS.
SELECTION-SCREEN SKIP.
PARAMETERS        p_modify   TYPE sy-datum .
SELECTION-SCREEN: END OF BLOCK b01.

SELECTION-SCREEN  BEGIN OF BLOCK: b02 WITH FRAME TITLE TEXT-b02.

SELECT-OPTIONS:   repname  FOR trdir-name MEMORY ID rs_scan_repid,
                  dynnr    FOR d020s-dnum,
                  subc     FOR trdir-subc,
                  appl     FOR lv_appl,
                  cnam     FOR trdir-cnam MATCHCODE OBJECT user_addr,
                  unam     FOR trdir-unam MATCHCODE OBJECT user_addr.

SELECTION-SCREEN SKIP.
SELECT-OPTIONS:   funcgrp  FOR tlibt-area.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:   p_class  FOR seoclasstx-clsname.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.

PARAMETERS: plusminu(2) TYPE n DEFAULT 2 NO-DISPLAY,
            inclu       TYPE xfeld AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.

PARAMETERS: rb_code RADIOBUTTON GROUP r10,
            rb_dyn  RADIOBUTTON GROUP r10,
            rb_all  RADIOBUTTON GROUP r10,
            p_vers  TYPE xfeld AS CHECKBOX.

SELECTION-SCREEN: END OF BLOCK b02.

CLASS lcl_source_code DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:
      constructor ,
      start IMPORTING iv_tr TYPE char1,
      free_memory.

  PROTECTED SECTION.

    TYPES: BEGIN OF ty_dynpro,
             repname LIKE d020s-prog,
             dynnr   LIKE d020s-dnum,
           END OF ty_dynpro,
           BEGIN OF ty_object,
             name TYPE tadir-obj_name,
           END OF ty_object.

    DATA gr_zip TYPE REF TO cl_abap_zip.


    DATA gt_object     TYPE STANDARD TABLE OF ty_object.
    DATA gt_dynpro     TYPE STANDARD TABLE OF ty_dynpro.
    DATA gt_prog     TYPE STANDARD TABLE OF tadir-obj_name.
    DATA gt_clas     TYPE STANDARD TABLE OF tadir-obj_name.
    DATA gt_intf     TYPE STANDARD TABLE OF tadir-obj_name.
    DATA gt_fugr     TYPE STANDARD TABLE OF tadir-obj_name.

    DATA :gv_report TYPE syrepid,
          gv_tr     TYPE char1,
          gt_source TYPE abaptxt255_tab,
          gv_dynpro TYPE sydynnr.
    METHODS:

      get_source_names,
      get_dynpros,
      get_report_names,
      get_function_names,
      get_class_names,
      get_interface_names,
      get_includes,
      get_abap_source_code,
      get_objects_from_tr,
      apply_modification_date,
      zip_files,
      download_file.



ENDCLASS.

CLASS lcl_source_code IMPLEMENTATION.

  METHOD  free_memory.
    FREE gr_zip.
    REFRESH : gt_object, gt_prog,gt_fugr,gt_clas,gt_intf, gt_source.
    CLEAR: gv_report, gv_tr, gv_dynpro.
  ENDMETHOD.

  METHOD apply_modification_date.
    DATA lt_object     TYPE STANDARD TABLE OF tadir-obj_name.

    IF gt_object IS NOT INITIAL.
      SELECT name FROM trdir INTO TABLE lt_object
        FOR ALL ENTRIES IN gt_object
        WHERE name EQ gt_object-name AND
        udat GE p_modify.

      IF sy-subrc EQ 0.
        REFRESH gt_object.
        gt_object[] = lt_object[].
      ENDIF.
    ENDIF.



  ENDMETHOD.
  METHOD constructor.

  ENDMETHOD.

  METHOD zip_files.

    TYPES: BEGIN OF ps_bin_file,
             name TYPE string,
             size TYPE i,
             data TYPE solix_tab,
           END OF ps_bin_file.

    TYPES: BEGIN OF ly_bin,
             line TYPE so_raw255,
           END OF ly_bin.
    DATA: lt_bindata        TYPE STANDARD TABLE OF ly_bin.

    DATA lv_length TYPE i.
    DATA lv_xstring      TYPE xstring.
    DATA lv_report TYPE string.

    CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
*     EXPORTING
*       FIRST_LINE            = 0
*       LAST_LINE             = 0
*       APPEND_TO_TABLE       = ' '
*       MIMETYPE              = ' '
*       ENCODING              =
      IMPORTING
        output_length = lv_length
      TABLES
        text_tab      = gt_source
        binary_tab    = lt_bindata
      EXCEPTIONS
        failed        = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF gr_zip IS NOT BOUND.
      CREATE OBJECT gr_zip.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = lv_length
*       FIRST_LINE   = 0
*       LAST_LINE    = 0
      IMPORTING
        buffer       = lv_xstring
      TABLES
        binary_tab   = lt_bindata
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here

    ELSE.
      CONCATENATE gv_report '.abap' INTO lv_report  .
      gr_zip->add(  name    = lv_report
                    content = lv_xstring ).
    ENDIF.

  ENDMETHOD.

  METHOD download_file.

    DATA lv_zip_xstring TYPE xstring.
    DATA lv_zip_size TYPE i.
    DATA lt_zip_bin_data   TYPE STANDARD TABLE OF raw255.
*   Get the binary stream for ZIP file

    CHECK gr_zip IS BOUND.
    lv_zip_xstring = gr_zip->save( ).

*   Convert the XSTRING to Binary table
    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_zip_xstring
      IMPORTING
        output_length = lv_zip_size
      TABLES
        binary_tab    = lt_zip_bin_data.


    DATA: lv_filename      TYPE string,
          lv_dest_filepath TYPE string,
          lv_path          TYPE string,
          lv_filesize      TYPE i.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title         = 'Select the File Save Location'
        file_filter = '(*.zip)|*.zip|'
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = lv_dest_filepath
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4
           ).
    IF sy-subrc <> 0.
*     SUBRC check is not reqd.
    ENDIF.

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = lv_zip_size
        filename                  = lv_dest_filepath
        filetype                  = 'BIN'
      IMPORTING
        filelength                = lv_filesize
      CHANGING
        data_tab                  = lt_zip_bin_data
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24
           ).
    IF sy-subrc NE 0.
      MESSAGE 'File download failed' TYPE 'E'.
    ELSE.
      MESSAGE 'ZIP File downloaded successfully' TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD start.
    gv_tr = iv_tr.

    IF gv_tr IS NOT INITIAL.
      get_objects_from_tr( ).
    ELSE.
      get_source_names( ).
    ENDIF.

    get_includes( ).
    apply_modification_date( ).
    get_abap_source_code( ).

  ENDMETHOD.

  METHOD get_objects_from_tr.

    TYPES : BEGIN OF ly_tr,
              trkorr TYPE trkorr,
            END OF ly_tr,

            BEGIN OF ly_e071,
              trkorr   TYPE trkorr,
              pgmid    TYPE pgmid,
              object   TYPE trobjtype,
              obj_name TYPE trobj_name,
            END OF ly_e071.


    DATA lt_tr TYPE STANDARD TABLE OF ly_tr.
    DATA lt_e071 TYPE STANDARD TABLE OF ly_e071.
    DATA lwa_e071 TYPE ly_e071.

    SELECT trkorr FROM e070 INTO TABLE lt_tr
      WHERE trkorr IN s_tr.

    SELECT trkorr FROM e070 APPENDING TABLE lt_tr
      WHERE strkorr IN s_tr.


    IF lt_tr IS NOT INITIAL.

      SELECT trkorr pgmid object obj_name
        FROM e071
        INTO TABLE lt_e071
        FOR ALL ENTRIES IN lt_tr
        WHERE trkorr = lt_tr-trkorr.

      IF sy-subrc EQ 0.
        SORT lt_e071.
        DELETE ADJACENT DUPLICATES FROM lt_e071.
      ENDIF.

    ENDIF.

    LOOP AT lt_e071 INTO lwa_e071.
      CASE lwa_e071-object.

        WHEN 'PROG'.
          APPEND lwa_e071-obj_name TO gt_prog.
        WHEN 'CLAS'.
          APPEND lwa_e071-obj_name TO gt_clas.
        WHEN 'INTF'.
          APPEND lwa_e071-obj_name TO gt_intf.
        WHEN 'FUGR'.
          APPEND lwa_e071-obj_name TO gt_fugr.

      ENDCASE.

    ENDLOOP.

    get_report_names( ).
    get_function_names( ).
    get_class_names( ).
    get_interface_names( ).

  ENDMETHOD.


  METHOD get_source_names.

    IF s_devcla[] IS NOT INITIAL.
      get_report_names( ).
      get_function_names( ).
      get_class_names( ).
      get_interface_names( ).
    ENDIF.


    IF repname[] IS NOT INITIAL OR
       cnam[]    IS NOT INITIAL OR
       unam[]    IS NOT INITIAL OR
       subc[]    IS NOT INITIAL OR
       appl[]    IS NOT INITIAL.

      SELECT name APPENDING TABLE gt_object
        FROM trdir
        WHERE name IN repname
        AND   cnam IN cnam
        AND   unam IN unam
        AND   subc IN subc
        AND   appl IN appl.
    ENDIF.


    IF funcgrp[] IS NOT INITIAL.
      get_function_names( ).
    ENDIF.


    IF p_class[] IS NOT INITIAL.
      get_class_names( ).
      get_interface_names( ).
    ENDIF.

    IF rb_code IS INITIAL.
      get_dynpros( ).
    ENDIF.


  ENDMETHOD.

  METHOD get_dynpros.
    CHECK gt_object IS NOT INITIAL.

    SELECT prog dnum INTO TABLE gt_dynpro
      FROM d020s FOR ALL ENTRIES IN gt_object
      WHERE prog = gt_object-name
      AND   dnum IN dynnr.
  ENDMETHOD.

  METHOD get_report_names.

    IF me->gv_tr IS NOT INITIAL.
      APPEND LINES OF gt_prog TO gt_object.
    ELSE.
      SELECT obj_name INTO TABLE gt_object
        FROM tadir
        WHERE pgmid  = 'R3TR'
        AND   object = 'PROG'
        AND   devclass IN s_devcla.                     "#EC CI_GENBUFF
    ENDIF.
  ENDMETHOD.                    "get_report_names

  METHOD get_function_names.
    DATA:
      lt_obj     TYPE STANDARD TABLE OF tadir-obj_name,
      lv_obj     TYPE tadir-obj_name,
      lv_fgroup  TYPE rs38l-area,
      lv_program TYPE progname.

    FIELD-SYMBOLS:
      <lv_obj> LIKE LINE OF lt_obj.

    IF me->gv_tr IS NOT INITIAL.
      APPEND LINES OF gt_fugr TO lt_obj.
    ELSE.
      SELECT obj_name INTO TABLE lt_obj
        FROM tadir
        WHERE pgmid  = 'R3TR'
        AND   object = 'FUGR'
        AND   devclass IN s_devcla
        AND   obj_name IN funcgrp.                      "#EC CI_GENBUFF
    ENDIF.
    LOOP AT lt_obj ASSIGNING <lv_obj>.
      lv_fgroup = <lv_obj>.
      CLEAR lv_program.

      CALL FUNCTION 'FUNCTION_INCLUDE_CONCATENATE'
        CHANGING
          program       = lv_program
          complete_area = lv_fgroup
        EXCEPTIONS
          OTHERS        = 1.

      CHECK sy-subrc IS INITIAL AND lv_program IS NOT INITIAL.

      lv_obj = lv_program.
      APPEND lv_obj TO gt_object.
    ENDLOOP.
  ENDMETHOD.                    "get_function_names

  METHOD get_class_names.
    DATA lt_obj TYPE STANDARD TABLE OF tadir-obj_name.
    DATA ls_obj TYPE tadir-obj_name.
    IF me->gv_tr IS NOT INITIAL.
      APPEND LINES OF gt_clas TO lt_obj.
    ELSE.
      SELECT obj_name INTO TABLE lt_obj
        FROM tadir
        WHERE pgmid  = 'R3TR'
        AND   object = 'CLAS'
        AND   devclass IN s_devcla
        AND   obj_name IN p_class.                      "#EC CI_GENBUFF
    ENDIF.

    LOOP AT lt_obj INTO ls_obj.
      APPEND cl_oo_classname_service=>get_classpool_name( |{ ls_obj }| ) TO gt_object.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_interface_names.
    DATA lt_obj TYPE STANDARD TABLE OF tadir-obj_name.
    DATA ls_obj TYPE tadir-obj_name.
    IF me->gv_tr IS NOT INITIAL.
      APPEND LINES OF gt_intf TO lt_obj.
    ELSE.
      SELECT obj_name INTO TABLE lt_obj
        FROM tadir
        WHERE pgmid  = 'R3TR'
        AND   object = 'INTF'
        AND   devclass IN s_devcla
        AND   obj_name IN p_class.                      "#EC CI_GENBUFF
    ENDIF.
    LOOP AT lt_obj INTO ls_obj.
      APPEND cl_oo_classname_service=>get_interfacepool_name( |{ ls_obj }| ) TO gt_object.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_includes.
    DATA:
      lt_inc         TYPE STANDARD TABLE OF tadir-obj_name,
      lt_inc_tmp     LIKE lt_inc,
      lv_program     TYPE sy-repid,
      lv_obj         TYPE tadir-obj_name,
      class_name     TYPE seoclsname,
      class_includes TYPE seoincl_t.

    CHECK inclu IS NOT INITIAL.

    LOOP AT gt_object INTO lv_obj.    "for classes we already have the includes

      IF lv_obj+30(2) = 'CP'. "Class Pool
        DELETE gt_object INDEX sy-tabix.

        class_name = lv_obj(30).
        TRANSLATE class_name USING '= '.

        cl_oo_classname_service=>get_all_class_includes(
          EXPORTING class_name = class_name
          RECEIVING result     = class_includes
          EXCEPTIONS OTHERS    = 0
        ).
        DELETE class_includes WHERE table_line+30(2) = 'CS' OR table_line+30(2) = 'CP'.
        APPEND LINES OF class_includes TO lt_inc.

      ELSEIF lv_obj+30(2) = 'IP'. "Interface Pool
        DELETE gt_object INDEX sy-tabix.

        lv_obj+31(1) = 'U'.
        APPEND lv_obj TO lt_inc.
      ENDIF.

      REFRESH lt_inc_tmp.
      lv_program = lv_obj.

      CALL FUNCTION 'RS_GET_ALL_INCLUDES'
        EXPORTING
          program    = lv_program
        TABLES
          includetab = lt_inc_tmp
        EXCEPTIONS
          OTHERS     = 0.

      APPEND LINES OF lt_inc_tmp TO lt_inc.
    ENDLOOP.

    SORT lt_inc.
    DELETE ADJACENT DUPLICATES FROM lt_inc.

    APPEND LINES OF lt_inc TO gt_object.

  ENDMETHOD.

  METHOD get_abap_source_code.
    DATA:
      percentage     TYPE i,
      old_percentage TYPE i VALUE -1,
      text           TYPE c LENGTH 150.

    LOOP AT gt_object INTO gv_report.

      IF sy-batch IS INITIAL.
        percentage = sy-tabix * 100 / lines( gt_object ).
        text = |READING ABAP SOURCES ({ sy-tabix }/{ lines( gt_object ) })...|.

        IF old_percentage <> percentage.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              percentage = percentage
              text       = text.
          old_percentage = percentage.
        ENDIF.
      ENDIF.

      READ REPORT gv_report INTO gt_source.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      zip_files( ).

    ENDLOOP.

    download_file( ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA go_main TYPE REF TO lcl_source_code.
  DATA lv_tr TYPE char1.

  IF s_tr[] IS NOT INITIAL.
    lv_tr = 'X'.
  ELSE.
    CLEAR lv_tr.
  ENDIF.


  CREATE OBJECT go_main .

  go_main->start( lv_tr ).
  go_main->free_memory( ).