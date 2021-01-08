TABLES: bseg, regup.

CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF tp_refdata,
        laufd TYPE REF TO data,
        bukrs TYPE REF TO data,
        belnr TYPE REF TO data,
        gjahr TYPE REF TO data,
      END OF tp_refdata .

    TYPES:
      BEGIN OF tp_param,
        all    TYPE REF TO boolean,
        dentro TYPE REF TO boolean,
        fora   TYPE REF TO boolean,
      END OF tp_param .

    DATA refdata TYPE tp_refdata.
    DATA param TYPE tp_param.

    CONSTANTS: c_dentro TYPE zstatus_prazo VALUE 'Dentro',
               c_fora   TYPE zstatus_prazo VALUE 'Fora'.


    METHODS:
      main .

  PRIVATE SECTION.

    TYPES:
      BEGIN OF tp_selopt,
        laufd TYPE RANGE OF regup-laufd,
        bukrs TYPE RANGE OF bseg-bukrs,
        belnr TYPE RANGE OF bseg-belnr,
        gjahr TYPE RANGE OF bseg-gjahr,
      END OF tp_selopt ,

      BEGIN OF tp_dados,
        lifnr     TYPE bseg-lifnr,
        name1     TYPE lfa1-name1,
        belnr     TYPE bseg-belnr,
        zuonr     TYPE bseg-zuonr,
        h_blart   TYPE bseg-h_blart,
        wrbtr     TYPE bseg-wrbtr,
        h_bldat   TYPE bseg-h_bldat,
        augdt     TYPE bseg-augdt,
        prazo     TYPE i,
        prazo_sts TYPE zstatus_prazo,
        percent   TYPE zpercent_indicador,
      END OF tp_dados,

      BEGIN OF tp_soma,
        h_blart TYPE bseg-h_blart,
        wrbtr   TYPE bseg-wrbtr,
      END OF tp_soma,

      BEGIN OF tp_perc,
        h_blart TYPE bseg-h_blart,
        percent TYPE zpercent_indicador,
      END OF tp_perc.


    DATA selopt TYPE tp_selopt .

    DATA:
      gt_data        TYPE STANDARD TABLE OF tp_dados,
      gs_data        TYPE tp_dados,
      gt_soma        TYPE STANDARD TABLE OF tp_soma,
      gs_soma        TYPE tp_soma,
      gt_perc        TYPE STANDARD TABLE OF tp_perc,
      gs_perc        TYPE tp_perc,
      gv_valor_total TYPE bseg-wrbtr,
      gv_percent     TYPE zpercent_indicador.

    METHODS:
      get_data EXCEPTIONS not_found,

      process,

      alv ,

      selection_options.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD main.

    DATA: lo_content TYPE REF TO data.

    "Transforma selection options
    selection_options(  ).

***** Ler e Processar as informações
    get_data( EXCEPTIONS not_found      = 1 ).
    IF sy-subrc = 1.
      MESSAGE TEXT-m04 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    process( ).

    alv( ).

  ENDMETHOD.


  METHOD get_data.

    DATA:lv_days TYPE i.


    SELECT belnr, gjahr
      FROM regup
      INTO TABLE @DATA(lt_regup)
      WHERE laufd IN @selopt-laufd
         AND xvorl = @space
         AND lifnr <> @space
         AND bukrs IN @selopt-bukrs
         AND belnr IN @selopt-belnr
         AND gjahr IN @selopt-gjahr.

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.

    CHECK lt_regup IS NOT INITIAL.

    SELECT belnr, h_bldat, lifnr, zuonr, h_blart, wrbtr, augdt
      FROM bseg
      INTO TABLE @DATA(lt_bseg)
      FOR ALL ENTRIES IN @lt_regup
      WHERE belnr = @lt_regup-belnr
        AND gjahr = @lt_regup-gjahr
        AND lifnr <> @space.

    IF sy-subrc <> 0.
      RAISE not_found.
    ELSE.
      SELECT lifnr, name1
      FROM lfa1
      INTO TABLE @DATA(lt_lfa1)
      FOR ALL ENTRIES IN @lt_bseg
      WHERE lifnr <> @lt_bseg-lifnr.

      SORT: lt_lfa1 BY lifnr.
    ENDIF.

    CLEAR: gv_valor_total.
    CLEAR: gt_soma, gt_data.
    LOOP AT lt_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg>).



      lv_days = <fs_bseg>-augdt - <fs_bseg>-h_bldat.
      IF lv_days < 3.
        IF param-fora->* = abap_true.
          CONTINUE.
        ENDIF.
      ELSE.
        IF <fs_bseg>-h_blart = 'ZB'.
          CONTINUE.
        ENDIF.
        IF param-dentro->* = abap_true.
          CONTINUE.
        ENDIF.
      ENDIF.


      READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>) WITH KEY lifnr = <fs_bseg>-lifnr
                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        gs_data-name1 = <fs_lfa1>-name1.
      ENDIF.

      MOVE-CORRESPONDING <fs_bseg> TO gs_data.
      COLLECT gs_data INTO gt_data.

      CLEAR: gs_soma.
      MOVE-CORRESPONDING <fs_bseg> TO gs_soma.
      COLLECT gs_soma INTO gt_soma.


      gv_valor_total = gv_valor_total + <fs_bseg>-wrbtr.
    ENDLOOP.

    SORT: gt_soma BY h_blart.


  ENDMETHOD.

  METHOD process.



    CLEAR: gv_percent.
    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).

      DATA(lv_qtd) = <fs_data>-augdt - <fs_data>-h_bldat.

      <fs_data>-prazo = lv_qtd.

      IF <fs_data>-prazo < 3.
        <fs_data>-prazo_sts = c_dentro.
      ELSE.
        IF <fs_data>-h_blart = 'ZB'.
          <fs_data>-prazo_sts = c_dentro.
        ELSE.
          <fs_data>-prazo_sts = c_fora.
        ENDIF.
      ENDIF.

      "Percentual por tipo de documento
      READ TABLE gt_soma ASSIGNING FIELD-SYMBOL(<fs_soma>) WITH KEY h_blart = <fs_data>-h_blart
                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-percent = <fs_data>-wrbtr / <fs_soma>-wrbtr.

        READ TABLE gt_perc ASSIGNING FIELD-SYMBOL(<fs_perc>) WITH KEY h_blart = <fs_data>-h_blart.
        IF sy-subrc = 0.
          <fs_perc>-percent = <fs_perc>-percent + <fs_data>-percent.
        ELSE.
          "Inseri a primeira linha
          APPEND INITIAL LINE TO gt_perc ASSIGNING FIELD-SYMBOL(<fs_perc2>).
          <fs_perc2>-percent = <fs_data>-percent.
          <fs_perc2>-h_blart = <fs_data>-h_blart.
        ENDIF.
      ENDIF.

    ENDLOOP.

    "Ajuste para valor final ser 100% do total
    LOOP AT gt_perc ASSIGNING FIELD-SYMBOL(<fs_perc3>).
      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<fs_data_ajust>) WITH KEY h_blart = <fs_perc3>-h_blart.
      IF sy-subrc = 0.
        <fs_perc3>-percent = 1 - <fs_perc3>-percent.
        <fs_data_ajust>-percent = <fs_data_ajust>-percent + <fs_perc3>-percent.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.



  METHOD selection_options.

    DATA: ol_ref_descr   TYPE REF TO cl_abap_structdescr.
    DATA: tl_detail      TYPE abap_compdescr_tab.

    FIELD-SYMBOLS: <fs_table> TYPE STANDARD TABLE,
                   <fs_ref>   TYPE REF TO data.

    ol_ref_descr ?= cl_abap_typedescr=>describe_by_data( me->refdata ).
    tl_detail[] = ol_ref_descr->components.

    LOOP AT tl_detail INTO DATA(wl_det).

      ASSIGN COMPONENT wl_det-name OF STRUCTURE me->refdata TO <fs_ref>.
      ASSIGN COMPONENT wl_det-name OF STRUCTURE me->selopt  TO FIELD-SYMBOL(<fs_selopt>).

      ASSIGN <fs_ref>->* TO <fs_table>.
      IF <fs_table> IS ASSIGNED.
        <fs_selopt> = <fs_table>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD alv.
    DATA: o_alv TYPE REF TO cl_salv_table.

    DATA: lr_columns TYPE REF TO cl_salv_columns_table,
          lr_column  TYPE REF TO cl_salv_column.

    TRY.


        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = o_alv
          CHANGING
            t_table      = gt_data.

        o_alv->get_functions( )->set_all( abap_true ).

        lr_columns = o_alv->get_columns( ).
        lr_columns->set_optimize( ).


        lr_column = lr_columns->get_column( 'PRAZO' ).
        lr_column->set_long_text( 'Prazo' ).
        lr_column->set_medium_text( 'Prazo' ).
        lr_column->set_short_text( 'Prazo' ).

        lr_column = lr_columns->get_column( 'PRAZO_STS' ).
        lr_column->set_long_text( 'Status Prazo' ).
        lr_column->set_medium_text( 'Status Prazo' ).
        lr_column->set_short_text( 'Status ' ).

      CATCH cx_root INTO DATA(lo_error).
    ENDTRY.

    o_alv->display( ).

  ENDMETHOD.


ENDCLASS.

**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.
SELECT-OPTIONS: s_data FOR regup-laufd,
                s_bukrs FOR bseg-bukrs,
                s_belnr FOR bseg-belnr,
                s_gjahr FOR bseg-gjahr.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_all    RADIOBUTTON GROUP rb2 DEFAULT 'X' MODIF ID r1,
            p_dentro RADIOBUTTON GROUP rb2 MODIF ID r1,
            p_fora   RADIOBUTTON GROUP rb2 MODIF ID r1.
SELECTION-SCREEN END OF BLOCK b02.


INITIALIZATION.

  DATA(o_lcl_report) = NEW lcl_report( ).

  GET REFERENCE OF s_data[] INTO o_lcl_report->refdata-laufd.
  GET REFERENCE OF s_bukrs[] INTO o_lcl_report->refdata-bukrs.
  GET REFERENCE OF s_belnr[] INTO o_lcl_report->refdata-belnr.
  GET REFERENCE OF s_gjahr[] INTO o_lcl_report->refdata-gjahr.
  GET REFERENCE OF p_all INTO o_lcl_report->param-all.
  GET REFERENCE OF p_dentro INTO o_lcl_report->param-dentro.
  GET REFERENCE OF p_fora INTO o_lcl_report->param-fora.

**********************************************************************
START-OF-SELECTION.

  o_lcl_report->main( ).
