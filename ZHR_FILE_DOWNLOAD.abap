CLASS zhr_file_download DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS download
      IMPORTING
        !iv_filename       TYPE string OPTIONAL
        !iv_save_on_server TYPE abap_bool DEFAULT abap_false
        !io_data           TYPE REF TO data
        !iv_separador      TYPE char1 DEFAULT ';'
        !iv_header         TYPE zshr_header_tce OPTIONAL
      EXCEPTIONS
        filename_mandatory
        conversion_error
        save_error .
protected section.
private section.

  class-methods GET_FILENAME
    returning
      value(RV_FILENAME) type STRING .
  class-methods CONVERT
    importing
      !IO_DATA type ref to DATA
      !IV_SEPARADOR type CHAR1
      !IV_HEADER type ZSHR_header_TCE
    changing
      !CT_DATA type TRUXS_T_TEXT_DATA
    exceptions
      TABLETYPE_MISMATCH
      CONVERSION_ERROR .
  class-methods SAVE_ON_SERVER
    importing
      !IV_FILENAME type STRING
      !IT_DATA type TRUXS_T_TEXT_DATA
    exceptions
      SAVE_ERROR .
  class-methods SAVE_ON_LOCAL
    importing
      !IV_FILENAME type STRING
    changing
      !CT_DATA type TRUXS_T_TEXT_DATA
    exceptions
      SAVE_ERROR .
  class-methods INSERT_HEADER
    importing
      !IV_HEADER type ZSHR_header_TCE
    changing
      !CT_DATA type TRUXS_T_TEXT_DATA .
ENDCLASS.



CLASS ZHR_FILE_DOWNLOAD IMPLEMENTATION.


  METHOD convert.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN io_data->* TO <table>.

    IF sy-subrc = 0.
      CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
        TABLES
          i_tab_sap_data       = <table>
        CHANGING
          i_tab_converted_data = ct_data
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.

      IF sy-subrc <> 0.
        RAISE conversion_error.

      ELSE.
        insert_header( EXPORTING iv_header = iv_header
                       CHANGING  ct_data   = ct_data ).

        IF iv_separador <> ';'.
          REPLACE ALL OCCURRENCES OF ';' IN TABLE ct_data WITH iv_separador IN CHARACTER MODE.
        ENDIF.
      ENDIF.

    ELSE.
      RAISE tabletype_mismatch.
    ENDIF.
  ENDMETHOD.


  METHOD download.

    DATA: lv_filename       TYPE string,
          lt_converted_data TYPE truxs_t_text_data.

*** Nome do Arquivo
    IF iv_filename IS INITIAL.
      IF iv_save_on_server = abap_true.
        RAISE filename_mandatory.
        RETURN.
      ENDIF.

      lv_filename = get_filename( ).

    ELSE.
      lv_filename = iv_filename.
    ENDIF.
    IF lv_filename IS INITIAL.
      RAISE filename_mandatory.
    ENDIF.

*** Converter Arquivo
    convert( EXPORTING io_data      = io_data
                       iv_separador = iv_separador
                       iv_header    = iv_header
             CHANGING ct_data  = lt_converted_data ).
    IF sy-subrc <> 0.
      RAISE conversion_error.
      RETURN.
    ENDIF.

*** Gravar Arquivo
    IF iv_save_on_server = abap_true.
      save_on_server( EXPORTING iv_filename = lv_filename
                                it_data     = lt_converted_data
                      EXCEPTIONS save_error = 1 ).

    ELSE.
      save_on_local( EXPORTING iv_filename = lv_filename
                     CHANGING  ct_data     = lt_converted_data
                     EXCEPTIONS save_error = 1 ).
    ENDIF.

    IF sy-subrc <> 0.
      RAISE save_error.
    ENDIF.
  ENDMETHOD.


  METHOD get_filename.

    DATA: lv_filename TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        prompt_on_overwrite       = 'X'
      CHANGING
        filename                  = lv_filename
        path                      = lv_path
        fullpath                  = lv_fullpath
      EXCEPTIONS
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        OTHERS                    = 5.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_filename = lv_fullpath.
  ENDMETHOD.


  METHOD insert_header.

    DATA: lt_header TYPE TABLE OF zshr_header_tce .
    DATA lt_convert TYPE truxs_t_text_data.

    APPEND iv_header TO lt_header.

    CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
      TABLES
        i_tab_sap_data       = lt_header
      CHANGING
        i_tab_converted_data = lt_convert
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

***** Inserir Cabeçalho
**    LOOP AT it_header ASSIGNING FIELD-SYMBOL(<header>).
**      lv_header = lv_header && <header>.
**      IF lines( it_header ) > sy-tabix.
**        lv_header = lv_header && ';'.
**      ENDIF.
**    ENDLOOP.

    IF sy-subrc = 0.
      INSERT lt_convert[ 1 ] INTO ct_data INDEX 1.
    ENDIF.


  ENDMETHOD.


  METHOD save_on_local.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = iv_filename
        filetype                = 'ASC'
      CHANGING
        data_tab                = ct_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.
    IF sy-subrc <> 0.
      RAISE save_error.
    ENDIF.
  ENDMETHOD.


  METHOD save_on_server.

    OPEN DATASET iv_filename FOR OUTPUT IN TEXT MODE ENCODING UTF-8.

    IF sy-subrc EQ 0.
      LOOP AT it_data ASSIGNING FIELD-SYMBOL(<data>).
        TRANSFER <data> TO iv_filename.
      ENDLOOP.

      CLOSE DATASET iv_filename.

    ELSE.
      RAISE save_error.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
