CLASS zclca_excel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA gv_quant TYPE abap_bool .

    "! Inicializa dados
    "! @parameter iv_filename | Nome do arquivo
    "! @parameter iv_file | Arquivo em binário
    METHODS constructor
      IMPORTING
        !iv_filename TYPE any
        !iv_file     TYPE xstring OPTIONAL .
    "! Converte excel para formato tabela interna
    "! @parameter iv_sheetnumber | Número da aba
    "! @parameter iv_headerline | Linha com o cabeçalho (informar 0 caso não exista)
    "! @parameter et_return | Mensagens de retorno
    "! @parameter ct_table | Tabela de retorno
    METHODS get_sheet
      IMPORTING
        !iv_sheetnumber TYPE i DEFAULT 1
        !iv_headerline  TYPE i DEFAULT 1
      EXPORTING
        !et_return      TYPE bapiret2_t
      CHANGING
        !ct_table       TYPE ANY TABLE .
    "! Cria novo arquivo excel
    "! @parameter it_table | Tabela com os dados
    "! @parameter iv_convert | Aplica conversões e tratamento de dados
    "! @parameter ev_file | Arquivo em binário
    "! @parameter et_return | Mensagens de retorno
    METHODS create_document
      IMPORTING
        !it_table   TYPE ANY TABLE
        !iv_convert TYPE flag DEFAULT abap_true
      EXPORTING
        !ev_file    TYPE xstring
        !et_return  TYPE bapiret2_t .
    "! Converte tipo referência para tabela
    "! @parameter iv_headerline | Linha com o cabeçalho (informar 0 caso não exista)
    "! @parameter io_data | Referência com os dados
    "! @parameter ct_table | Tabela de retorno
    METHODS convert_data_to_table
      IMPORTING
        !iv_headerline TYPE i DEFAULT 1
        !io_data       TYPE REF TO data
      EXPORTING
        !et_return     TYPE bapiret2_t
      CHANGING
        !ct_table      TYPE ANY TABLE .
    "! Converte tabela para tabela de arquivo
    "! @parameter it_table | Tabela com os dados orioginais
    "! @parameter eo_file | Tabela com os dados convertidos
    "! @parameter et_return | Mensagens de retorno
    METHODS convert_table_to_file
      IMPORTING
        !it_table  TYPE ANY TABLE
      EXPORTING
        !eo_file   TYPE REF TO data
        !et_return TYPE bapiret2_t .
    "! Converte texto para data
    "! @parameter iv_value | Valor original
    "! @parameter ev_date | Valor no formato data
    "! @parameter rv_date | Valor no formato data
    METHODS convert_text_to_date
      IMPORTING
        !iv_value      TYPE any
      EXPORTING
        !ev_date       TYPE dats
        !es_return     TYPE bapiret2
      RETURNING
        VALUE(rv_date) TYPE dats .
    "! Converte data para texto
    "! @parameter iv_value | Valor original
    "! @parameter ev_date | Valor no formato texto
    "! @parameter rv_date | Valor no formato texto
    METHODS convert_date_to_text
      IMPORTING
        !iv_value      TYPE any
      EXPORTING
        !ev_date       TYPE string
      RETURNING
        VALUE(rv_date) TYPE string .
    "! Converte texto para numérico
    "! @parameter iv_value | Valor original
    "! @parameter ev_currency | Valor no formato numérico
    "! @parameter rv_currency | Valor no formato numérico
    METHODS convert_text_to_number
      IMPORTING
        !iv_value          TYPE any
      EXPORTING
        !ev_currency       TYPE string
      RETURNING
        VALUE(rv_currency) TYPE string .
    "! Converte texto para numérico
    "! @parameter iv_value | Valor original
    "! @parameter ev_value | Valor no formato numérico
    "! @parameter rv_value | Valor no formato numérico
    METHODS convert_text_to_numeric
      IMPORTING
        !iv_value       TYPE any
      EXPORTING
        !ev_value       TYPE string
        !es_return      TYPE bapiret2
      RETURNING
        VALUE(rv_value) TYPE string .
    "! Converte número para texto
    "! @parameter iv_value | Valor original
    "! @parameter ev_currency | Valor no formato texto
    "! @parameter rv_currency | Valor no formato texto
    METHODS convert_number_to_text
      IMPORTING
        !iv_value          TYPE any
      EXPORTING
        !ev_currency       TYPE string
      RETURNING
        VALUE(rv_currency) TYPE string .
    "! Converte texto para hora
    "! @parameter iv_value | Valor original
    "! @parameter ev_time | Valor no formato hora
    "! @parameter rv_time | Valor no formato hora
    METHODS convert_text_to_time
      IMPORTING
        !iv_value      TYPE any
      EXPORTING
        !ev_time       TYPE dats
      RETURNING
        VALUE(rv_time) TYPE dats .
    "! Converte hora para texto
    "! @parameter iv_value | Valor original
    "! @parameter ev_time | Valor no formato hora
    "! @parameter rv_time | Valor no formato hora
    METHODS convert_time_to_text
      IMPORTING
        !iv_value      TYPE any
      EXPORTING
        !ev_time       TYPE string
      RETURNING
        VALUE(rv_time) TYPE string .
    "! Aplica exit de conversão (input)
    "! @parameter iv_convexit | Rotina de conversão
    "! @parameter cv_value | Valor atualizado
    METHODS conversion_exit_input
      IMPORTING
        !iv_convexit TYPE dfies-convexit
      EXPORTING
        !es_return   TYPE bapiret2
      CHANGING
        !cv_value    TYPE any .
    "! Aplica exit de conversão (output)
    "! @parameter iv_convexit | Rotina de conversão
    "! @parameter cv_value | Valor atualizado
    METHODS conversion_exit_output
      IMPORTING
        !iv_convexit TYPE dfies-convexit
      EXPORTING
        !es_return   TYPE bapiret2
      CHANGING
        !cv_value    TYPE any .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: gv_filename TYPE string,
          gv_file     TYPE xstring.

ENDCLASS.



CLASS zclca_excel IMPLEMENTATION.


  METHOD constructor.
    me->gv_filename = iv_filename.
    me->gv_file     = iv_file.
  ENDMETHOD.


  METHOD get_sheet.

* ----------------------------------------------------------------------
* Abre arquivo excel
* ----------------------------------------------------------------------
    TRY.
        DATA(lo_excel) = NEW cl_fdt_xl_spreadsheet( document_name = me->gv_filename
                                                    xdocument     = me->gv_file ).
      CATCH cx_root INTO DATA(lo_root).

        " Falha ao abrir arquivo &1.
        et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '001'
                                                message_v1 = me->gv_filename ) ).

        " Mensagem de erro
        DATA(lv_message) = CONV bapi_msg( lo_root->get_longtext( ) ).
        et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '000'
                                                message_v1 = lv_message+0(50)
                                                message_v2 = lv_message+50(50)
                                                message_v3 = lv_message+100(50)
                                                message_v4 = lv_message+150(50) ) ).
        RETURN.
    ENDTRY.

* ----------------------------------------------------------------------
* Recupera lista de abas do excel
* ----------------------------------------------------------------------
    lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_worksheets) ).

    IF lt_worksheets IS INITIAL.
      " Nenhuma aba encontrada no arquivo &1.
      et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '002'
                                              message_v1 = me->gv_filename ) ).
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
* Verifica se aba solicitada existe
* ----------------------------------------------------------------------
    TRY.
        DATA(lv_worksheet_name) = lt_worksheets[ iv_sheetnumber ].

      CATCH cx_root INTO lo_root.
        " Aba &1 não existe no arquivo &2.
        et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '003'
                                                message_v1 = |{ CONV string( iv_sheetnumber ) }|
                                                message_v2 = me->gv_filename ) ).
    ENDTRY.

* ----------------------------------------------------------------------
* Recupera aba do excel
* ----------------------------------------------------------------------
    DATA(lo_data) = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_worksheet_name ).

    IF lo_data IS INITIAL.
      " Não foi possível ler a aba '$1'.
      et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '005' message_v1 = lv_worksheet_name ) ).
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
* Converte dados para tabela interna
* ----------------------------------------------------------------------
    me->convert_data_to_table( EXPORTING iv_headerline  = iv_headerline
                                         io_data        = lo_data
                               IMPORTING et_return      = DATA(lt_return)
                               CHANGING  ct_table       = ct_table ).

    et_return[] = VALUE #( BASE et_return FOR ls_return IN lt_return ( ls_return ) ).

  ENDMETHOD.


  METHOD convert_data_to_table.

    DATA: lo_describe TYPE REF TO cl_abap_structdescr,
          ls_table    TYPE REF TO data,
          ls_return   TYPE bapiret2.

    DATA(lv_lower_abcde) = to_lower( sy-abcde ).

    FIELD-SYMBOLS: <fs_t_data>        TYPE STANDARD TABLE.

    FREE: et_return.

    DATA(lo_data) = io_data.

* ----------------------------------------------------------------------
* Recupera campos da tabela de destino e seus tipos
* ----------------------------------------------------------------------
    CREATE DATA ls_table  LIKE LINE OF ct_table.
    lo_describe ?= cl_abap_structdescr=>describe_by_data_ref( ls_table ).
    DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr( lo_describe ).

* ----------------------------------------------------------------------
* Converte dados para tabela interna
* ----------------------------------------------------------------------
    ASSIGN ls_table->* TO FIELD-SYMBOL(<fs_s_table>).
    ASSIGN lo_data->* TO <fs_t_data>.

    LOOP AT <fs_t_data> ASSIGNING FIELD-SYMBOL(<fs_s_data>) FROM iv_headerline + 1.

      DATA(lv_index) = sy-tabix.

      LOOP AT lt_dfies REFERENCE INTO DATA(ls_dfies).    "#EC CI_NESTED

        CLEAR:ls_return.

        " Recupera campo da tabela de Origem
        ASSIGN COMPONENT sy-tabix OF STRUCTURE <fs_s_data> TO FIELD-SYMBOL(<fs_v_data_field>).
        CHECK sy-subrc EQ 0.

        " Recupera campo da tabela de Destino
        ASSIGN COMPONENT ls_dfies->fieldname OF STRUCTURE <fs_s_table> TO FIELD-SYMBOL(<fs_v_table_field>).
        CHECK sy-subrc EQ 0.

        " Verifica se o campo quantidade está preenchido.
        IF gv_quant EQ abap_true AND ls_dfies->datatype = 'QUAN' AND <fs_v_data_field> EQ space.
          " Campo '&1'está vazio.
          et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '008' message_v1 = ls_dfies->fieldtext ) ).
          RETURN.
        ENDIF.
        IF gv_quant EQ abap_true AND ls_dfies->datatype = 'QUAN' AND ##TEXT_POOL
        ( <fs_v_data_field> CA TEXT-001 OR <fs_v_data_field> CA sy-abcde OR <fs_v_data_field> CA lv_lower_abcde ).
          " Campo '&1' possui caracteres não numéricos.
          et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '009' message_v1 = ls_dfies->fieldtext ) ).
          RETURN.
        ENDIF.

        TRY.
            " Transfere dados
            <fs_v_table_field> = COND string( WHEN ls_dfies->inttype = 'D' THEN me->convert_text_to_date( EXPORTING iv_value   = <fs_v_data_field>
                                                                                                          IMPORTING es_return  = ls_return   )
                                              WHEN ls_dfies->inttype = 'T' THEN me->convert_text_to_time( <fs_v_data_field> )
                                              WHEN ls_dfies->inttype = 'P' THEN me->convert_text_to_number( <fs_v_data_field> )
                                              WHEN ls_dfies->inttype = 'N' THEN me->convert_text_to_numeric( EXPORTING iv_value   = <fs_v_data_field>
                                                                                                             IMPORTING es_return  = ls_return   )
                                                                           ELSE <fs_v_data_field> ).

            IF ls_return IS INITIAL.
              " Aplica exit de conversão
              me->conversion_exit_input( EXPORTING iv_convexit = ls_dfies->convexit
                                         IMPORTING es_return   = ls_return
                                         CHANGING  cv_value    = <fs_v_table_field> ).
            ENDIF.

            IF ls_return IS NOT INITIAL.
              " Erro durante conversão na linha '&1', campo '&2', valor '&3'.
              et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '004'
                                                      message_v1 = lv_index
                                                      message_v2 = ls_dfies->fieldname
                                                      message_v3 = <fs_v_data_field> ) ).

              " Mensagem de erro
              et_return[] = VALUE #( BASE et_return ( ls_return ) ).
            ENDIF.

          CATCH cx_root INTO DATA(lo_root).

            " Erro durante conversão na linha '&1', campo '&2', valor '&3'.
            et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '004'
                                                    message_v1 = lv_index
                                                    message_v2 = ls_dfies->fieldname
                                                    message_v3 = <fs_v_table_field> ) ).

            " Mensagem de erro
            DATA(lv_message) = CONV bapi_msg( lo_root->get_longtext( ) ).
            et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '000'
                                                    message_v1  = lv_message+0(50)
                                                    message_v2  = lv_message+50(50)
                                                    message_v3  = lv_message+100(50)
                                                    message_v4  = lv_message+150(50) ) ).
        ENDTRY.

      ENDLOOP.

      INSERT <fs_s_table> INTO TABLE ct_table[].
      CLEAR <fs_s_table>.

    ENDLOOP.

  ENDMETHOD.


  METHOD convert_text_to_date.

    FREE: ev_date.

    IF iv_value IS INITIAL.
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
* Converte texto para data
* ----------------------------------------------------------------------
    DATA(lv_value) = CONV char100( iv_value ).
    REPLACE ALL OCCURRENCES OF PCRE '[^0-9]' IN lv_value WITH ''. CONDENSE lv_value NO-GAPS.
    rv_date = ev_date = |{ lv_value+4(4) }{ lv_value+2(2) }{ lv_value+0(2) }|.

    IF ev_date IS NOT INITIAL.
      CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
        EXPORTING
          date                      = rv_date
        EXCEPTIONS
          plausibility_check_failed = 1
          OTHERS                    = 2.

      IF sy-subrc <> 0.
        " Data '&1' possui formato incorreto. Utilizar DD.MM.YYYY.
        es_return = VALUE #( type = 'E' id = 'ZCA_EXCEL'  number = '006' message_v1 = iv_value ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD convert_date_to_text.

    FREE: ev_date.

* ----------------------------------------------------------------------
* Converte data para texto
* ----------------------------------------------------------------------
    rv_date = ev_date = |{ iv_value+6(2) }.{ iv_value+4(2) }.{ iv_value+0(4) }|.

  ENDMETHOD.


  METHOD convert_text_to_number.

    DATA: lv_decimal TYPE decfloat34.

    FREE: ev_currency.

    DATA(lv_value) = CONV char100( iv_value ).


    IF lv_value CS '+'
    OR lv_value CS 'E'.
      CONDENSE lv_value.
      SHIFT lv_value LEFT DELETING LEADING space.
      MOVE lv_value TO lv_decimal.
      lv_value = lv_decimal.
    ENDIF.

    FIND ALL OCCURRENCES OF PCRE '[,.]' IN lv_value RESULTS DATA(lt_result).

    IF sy-subrc EQ 0.
      " Recupera último registro
      DATA(ls_result) = lt_result[ lines( lt_result ) ].

      CASE lv_value+ls_result-offset(ls_result-length).
        WHEN ','.
          TRANSLATE lv_value USING '. '.
          TRANSLATE lv_value USING ',.'.
          CONDENSE  lv_value NO-GAPS.
        WHEN '.'.
          TRANSLATE lv_value USING ', '.
          CONDENSE  lv_value NO-GAPS.
      ENDCASE.
    ENDIF.

    TRY.
        rv_currency = ev_currency = lv_value.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD convert_number_to_text.

    DATA: lv_value TYPE char200.

    FREE: ev_currency.

* ----------------------------------------------------------------------
* Converte texto para numérico
* ----------------------------------------------------------------------
    WRITE  iv_value TO lv_value.
    CONDENSE lv_value NO-GAPS.

    rv_currency = ev_currency = lv_value.

  ENDMETHOD.


  METHOD convert_text_to_time.

    FREE: ev_time.

* ----------------------------------------------------------------------
* Converte texto para data
* ----------------------------------------------------------------------
    DATA(lv_value) = CONV char100( iv_value ).
    REPLACE ALL OCCURRENCES OF PCRE '[^0-9]' IN lv_value WITH ''. CONDENSE lv_value NO-GAPS.
    rv_time = ev_time = |{ lv_value+0(6) }|.

  ENDMETHOD.


  METHOD convert_time_to_text.

    FREE: ev_time.

* ----------------------------------------------------------------------
* Converte texto para data
* ----------------------------------------------------------------------
    rv_time = ev_time = |{ iv_value+0(2) }:{ iv_value+2(2) }:{ iv_value+4(2) }|.

  ENDMETHOD.


  METHOD conversion_exit_input.

    FREE: es_return.

    CHECK iv_convexit IS NOT INITIAL.

* --------------------------------------------------------------------
* Prepara nome da função e aplica conversão
* --------------------------------------------------------------------
    DATA(lv_funcname) = |CONVERSION_EXIT_{ iv_convexit }_INPUT|.

    TRY.
        CALL FUNCTION lv_funcname
          EXPORTING
            input  = cv_value
          IMPORTING
            output = cv_value
                     EXCEPTIONS
                     OTHERS.

        IF sy-subrc NE 0.
          es_return-type       = sy-msgty.
          es_return-id         = sy-msgid.
          es_return-number     = sy-msgno.
          es_return-message_v1 = sy-msgv1.
          es_return-message_v2 = sy-msgv2.
          es_return-message_v3 = sy-msgv3.
          es_return-message_v4 = sy-msgv4.
          RETURN.
        ENDIF.

      CATCH cx_root INTO DATA(lo_root).
        DATA(lv_message) = CONV bapi_msg( lo_root->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD conversion_exit_output.

    FREE: es_return.

    CHECK iv_convexit IS NOT INITIAL.

* --------------------------------------------------------------------
* Prepara nome da função e aplica conversão
* --------------------------------------------------------------------
    DATA(lv_funcname) = |CONVERSION_EXIT_{ iv_convexit }_OUTPUT|.

    TRY.
        CALL FUNCTION lv_funcname
          EXPORTING
            input  = cv_value
          IMPORTING
            output = cv_value
                     EXCEPTIONS
                     OTHERS.

        IF sy-subrc NE 0.
          es_return-type       = sy-msgty.
          es_return-id         = sy-msgid.
          es_return-number     = sy-msgno.
          es_return-message_v1 = sy-msgv1.
          es_return-message_v2 = sy-msgv2.
          es_return-message_v3 = sy-msgv3.
          es_return-message_v4 = sy-msgv4.
          RETURN.
        ENDIF.

      CATCH cx_root INTO DATA(lo_root).
        DATA(lv_message) = CONV bapi_msg( lo_root->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD convert_table_to_file.

    DATA: lo_describe TYPE REF TO cl_abap_structdescr,
          ls_table    TYPE REF TO data,
          ls_file     TYPE REF TO data.

    FIELD-SYMBOLS: <fs_t_file> TYPE STANDARD TABLE.

    FREE: eo_file, et_return.

* ----------------------------------------------------------------------
* Recupera campos da tabela de destino e seus tipos
* ----------------------------------------------------------------------
    CREATE DATA ls_table LIKE LINE OF it_table.
    lo_describe ?= cl_abap_structdescr=>describe_by_data_ref( ls_table ).
    DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr( lo_describe ).

* ----------------------------------------------------------------------
* Monta colunas da nova tabela dinâmica (STRING)
* ----------------------------------------------------------------------
    DATA(lt_fcat) = VALUE lvc_t_fcat( FOR ls_field IN lt_dfies ( tabname      = ls_field-tabname
                                                                 fieldname    = ls_field-fieldname
                                                                 checktable   = ls_field-checktable
                                                                 scrtext_s    = ls_field-scrtext_s
                                                                 scrtext_m    = ls_field-scrtext_m
                                                                 scrtext_l    = ls_field-scrtext_l
                                                                 reptext      = ls_field-reptext
                                                                 inttype      = 'g'
                                                                 intlen       = 8
                                                                 key          = ls_field-keyflag ) ).

* ----------------------------------------------------------------------
* Cria tabela dinâmica
* ----------------------------------------------------------------------
    TRY.
        CALL METHOD cl_alv_table_create=>create_dynamic_table
          EXPORTING
            it_fieldcatalog           = lt_fcat
            i_length_in_byte          = abap_true
          IMPORTING
            ep_table                  = eo_file
          EXCEPTIONS
            generate_subpool_dir_full = 1
            OTHERS                    = 2.

        ASSIGN eo_file->* TO <fs_t_file>.
        CREATE DATA ls_file LIKE LINE OF <fs_t_file>.
        ASSIGN ls_file->* TO FIELD-SYMBOL(<fs_s_file>).
      CATCH cx_root.
    ENDTRY.

* ----------------------------------------------------------------------
* Converte dados para tabela interna
* ----------------------------------------------------------------------
    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<fs_s_table>).

      DATA(lv_index) = sy-tabix.

      LOOP AT lt_dfies REFERENCE INTO DATA(ls_dfies).    "#EC CI_NESTED

        " Recupera campo da tabela de Origem
        ASSIGN COMPONENT ls_dfies->fieldname OF STRUCTURE <fs_s_table> TO FIELD-SYMBOL(<fs_v_table_field>).
        CHECK sy-subrc EQ 0.

        " Recupera campo da tabela de Destino
        ASSIGN COMPONENT ls_dfies->fieldname OF STRUCTURE <fs_s_file> TO FIELD-SYMBOL(<fs_v_file_field>).
        CHECK sy-subrc EQ 0.

        TRY.
            " Transfere dados
            <fs_v_file_field> = COND string( WHEN ls_dfies->inttype = 'D' THEN me->convert_date_to_text( <fs_v_table_field> )
                                             WHEN ls_dfies->inttype = 'T' THEN me->convert_time_to_text( <fs_v_table_field> )
                                             WHEN ls_dfies->inttype = 'P' THEN me->convert_number_to_text( <fs_v_table_field> )
                                                                           ELSE <fs_v_table_field> ).
            " Aplica exit de conversão
            me->conversion_exit_output( EXPORTING iv_convexit = ls_dfies->convexit
                                        IMPORTING es_return   = DATA(ls_return)
                                        CHANGING  cv_value    = <fs_v_file_field> ).

            IF ls_return IS NOT INITIAL.
              " Erro durante conversão na linha '&1', campo '&2', valor '&3'.
              et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '004'
                                                      message_v1 = lv_index
                                                      message_v2 = ls_dfies->fieldname
                                                      message_v3 = <fs_v_table_field> ) ).

              " Mensagem de erro
              et_return[] = VALUE #( BASE et_return ( ls_return ) ).
            ENDIF.

          CATCH cx_root INTO DATA(lo_root).

            " Erro durante conversão na linha '&1', campo '&2', valor '&3'.
            et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '004'
                                                    message_v1 = lv_index
                                                    message_v2 = ls_dfies->fieldname
                                                    message_v3 = <fs_v_table_field> ) ).

            " Mensagem de erro
            DATA(lv_message) = CONV bapi_msg( lo_root->get_longtext( ) ).
            et_return[] = VALUE #( BASE et_return ( type = 'E' id = 'ZCA_EXCEL' number = '000'
                                                    message_v1  = lv_message+0(50)
                                                    message_v2  = lv_message+50(50)
                                                    message_v3  = lv_message+100(50)
                                                    message_v4  = lv_message+150(50) ) ).
        ENDTRY.

      ENDLOOP.

      INSERT <fs_s_file> INTO TABLE <fs_t_file>.
      CLEAR <fs_s_file>.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_document.

    DATA:
      lo_describe TYPE REF TO cl_abap_structdescr,
      lt_columns  TYPE if_fdt_doc_spreadsheet=>t_column,
      ls_column   TYPE if_fdt_doc_spreadsheet=>s_column,
      ls_table    TYPE REF TO data,
      ls_file     TYPE REF TO data.

    FIELD-SYMBOLS: <fs_t_file> TYPE STANDARD TABLE.

    FREE: ev_file, et_return.

* ----------------------------------------------------------------------
* Converte dados
* ----------------------------------------------------------------------
    me->convert_table_to_file( EXPORTING it_table  = it_table
                               IMPORTING eo_file   = DATA(lo_file)
                                         et_return = et_return ).

    IF et_return IS NOT INITIAL.
      RETURN.
    ENDIF.

* ----------------------------------------------------------------------
* Recupera campos da tabela de destino e seus tipos
* ----------------------------------------------------------------------
    CREATE DATA ls_table LIKE LINE OF it_table.
    lo_describe ?= cl_abap_structdescr=>describe_by_data_ref( ls_table ).
    DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr( lo_describe ).

    ASSIGN lo_file->* TO <fs_t_file>.
    CREATE DATA ls_file LIKE LINE OF <fs_t_file>.
    ASSIGN ls_file->* TO FIELD-SYMBOL(<fs_s_file>).

* ----------------------------------------------------------------------
* Monta colunas do excel
* ----------------------------------------------------------------------
    LOOP AT lt_dfies INTO DATA(ls_dfies).

      CLEAR ls_column.
      ls_column-id             = space.
      ls_column-name           = ls_dfies-fieldname.
      ls_column-display_name   = COND #( WHEN ls_dfies-fieldtext IS NOT INITIAL
                                         THEN ls_dfies-fieldtext
                                         ELSE ls_dfies-fieldname ).
      ls_column-is_result      = abap_false.

      ASSIGN COMPONENT ls_dfies-fieldname OF STRUCTURE <fs_s_file> TO FIELD-SYMBOL(<fs_v_field>).

      IF sy-subrc EQ 0.
        ls_column-type           ?= cl_abap_structdescr=>describe_by_data( <fs_v_field> ).
      ENDIF.

      APPEND ls_column TO lt_columns[].
    ENDLOOP.

* ----------------------------------------------------------------------
* Converte dados para tabela interna
* ----------------------------------------------------------------------
    TRY.
        cl_fdt_xl_spreadsheet=>if_fdt_doc_spreadsheet~create_document(
          EXPORTING name               = gv_filename
                    columns            = lt_columns
                    itab               = lo_file
                    iv_call_type       = 0
           RECEIVING xdocument         = ev_file ).
      CATCH cx_fdt_excel_core.
    ENDTRY.

  ENDMETHOD.


  METHOD convert_text_to_numeric.

    IF iv_value CO '0123456789'.
      rv_value = iv_value.
    ELSE.
      " Valor '&1' não é numérico
      es_return = VALUE #( type = 'E' id = 'ZCA_EXCEL'  number = '007' message_v1 = iv_value ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
