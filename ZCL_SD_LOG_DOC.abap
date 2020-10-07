class ZCL_SD_LOG_DOC definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods MOD_DOC_LOG
    importing
      !VBAK_NEW type VBAK
      !VBAK_OLD type VBAK
      !VBPA_NEW type TT_VBPAVB
      !VBPA_OLD type TT_VBPAVB
      !VBAP_NEW type TAB_XYVBAP
      !VBAP_OLD type TAB_XYVBAP
      !VEDA_NEW type ZVEDAVB_TT
      !VEDA_OLD type ZVEDAVB_TT
      !VBKD_NEW type TAB_XYVBKD
      !VBKD_OLD type TAB_XYVBKD
      !KOMV type KOMV_T
      !KOMV_OLD type MEOUT_T_UKONVC
      !FPLA type VA_FPLAVB_T
      !FPLA_OLD type VA_FPLAVB_T
      !FPLT type VA_FPLTVB_T
      !FPLT_OLD type VA_FPLTVB_T .
  methods NEW_DOC_LOG
    importing
      value(VBAK) type VBAK
      value(VBAP) type TAB_XYVBAP
      value(VBPA) type TT_VBPAVB
      value(VEDA) type ZVEDAVB_TT
      value(VBKD) type TAB_XYVBKD
      value(KOMV) type KOMV_T
      value(FPLA) type VA_FPLAVB_T
      value(FPLT) type VA_FPLTVB_T .
protected section.
private section.

  data T_LOG type ZSDT0029_T .
  data T_VBKD type VBKD_T .
  data ITENS_DEL type POSNR_TAB .
  data ITENS_INS type POSNR_TAB .

  methods FILL_VBPA
    importing
      !IM_VBPAVB type VBPAVB
    exporting
      !EX_FIELD type ZZED_LOG_CAMPO
      !EX_VALUE type ZZED_LOG_OLD .
  methods CHECK_TYPE_WRITE
    importing
      !IM_VALUE type ANY
    returning
      value(RE_VALUE) type ZZED_LOG_NEW .
  methods GET_CONFIG_CAMPOS
    importing
      !IM_TAB type ZETABNAME
    returning
      value(RE_ZSDT0028_T) type ZSDT0028_T .
  methods GET_FPLT_VALUE
    importing
      !IM_FPLT type FPLTVB
      !IM_VALUE type ZZED_LOG_NEW
    returning
      value(RE_VALUE) type ZZED_LOG_NEW .
  methods GET_ITENS_PROCESS
    importing
      !VBAP_NEW type TAB_XYVBAP
      !VBAP_OLD type TAB_XYVBAP .
  methods GET_POSNR_VBKD
    importing
      !IM_FPLNR type ANY
    returning
      value(RE_POSNR) type POSNR .
  methods GET_VBELN_VBKD
    importing
      !IM_FPLNR type ANY
    returning
      value(RE_VBELN) type VBELN .
  methods INSERT_LOG
    importing
      !IM_LOG type ZSDT0029 .
  methods PROCESS_DEL_KOMV
    importing
      !IM_TAB type ZETABNAME
      !IM_KOMV type KOMV_T
      !IM_KOMV_OLD type MEOUT_T_UKONVC
      !IM_VBELN type VBELN_VA .
  methods PROCESS_INS_KOMV
    importing
      !IM_TAB type ZETABNAME
      !IM_KOMV type KOMV_T
      !IM_KOMV_OLD type MEOUT_T_UKONVC
      !IM_VBELN type VBELN_VA .
  methods PROCESS_VBPA
    importing
      !IM_VBPA_OLD type TT_VBPAVB optional
      !IM_VBPA type TT_VBPAVB
      !IM_VBELN type VBELN_VA .
  methods PROCESS_MOD_FLPA
    importing
      !FPLA_OLD type VA_FPLAVB_T
      !FPLA_NEW type VA_FPLAVB_T .
  methods PROCESS_MOD_FLPT
    importing
      !FPLT_OLD type VA_FPLTVB_T
      !FPLT_NEW type VA_FPLTVB_T .
  methods PROCESS_MOD_KOMV
    importing
      !IM_TAB type ZETABNAME
      !IM_KOMV type KOMV_T
      !IM_KOMV_OLD type MEOUT_T_UKONVC optional
      !IM_VBELN type VBELN_VA .
  methods PROCESS_MOD_VBAP
    importing
      !VBAP_OLD type TAB_XYVBAP
      !VBAP_NEW type TAB_XYVBAP .
  methods PROCESS_MOD_VBKD
    importing
      !VBKD_OLD type TAB_XYVBKD
      !VBKD_NEW type TAB_XYVBKD .
  methods PROCESS_MOD_VEDA
    importing
      !VEDA_OLD type ZVEDAVB_TT
      !VEDA_NEW type ZVEDAVB_TT .
  methods PROCESS_TAB_CAMPO
    importing
      !IM_TAB type ZETABNAME
      !IM_VALUE type ANY
      !IM_ACAO type ZACAO .
  methods PROCESS_TAB_CAMPO_DEL
    importing
      !IM_TAB type ZETABNAME
      !IM_OLD_VALUE type ANY .
  methods PROCESS_TAB_CAMPO_MOD
    importing
      !IM_TAB type ZETABNAME
      !IM_NEW_VALUE type ANY
      !IM_OLD_VALUE type ANY .
  methods SAVE_LOG .
ENDCLASS.



CLASS ZCL_SD_LOG_DOC IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->CHECK_TYPE_WRITE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_VALUE                       TYPE        ANY
* | [<-()] RE_VALUE                       TYPE        ZZED_LOG_NEW
* +--------------------------------------------------------------------------------------</SIGNATURE>
method check_type_write.

  data: lv_valeu type datum.
  data: descr_ref type ref to cl_abap_typedescr.
**********************************************************************
***   check is Date
**********************************************************************

  check im_value is not initial.

  descr_ref = cl_abap_typedescr=>describe_by_data( im_value ).

  "Date
  if descr_ref->type_kind = 'D'.

    call function 'RP_CHECK_DATE'
      exporting
        date         = im_value
      exceptions
        date_invalid = 1
        others       = 2.

    if sy-subrc = 0.
      write im_value dd/mm/yyyy to re_value.
    else.
      re_value = im_value.
    endif.

    "TIME
  elseif descr_ref->type_kind = 'T'.

    write im_value to re_value using edit mask '__:__:__'.


  elseif descr_ref->type_kind = 'P'.

    write im_value to re_value currency 'BRL'.

  else.
    re_value = im_value.
  endif.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SD_LOG_DOC->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method constructor.

  clear: t_log.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->FILL_VBPA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_VBPAVB                      TYPE        VBPAVB
* | [<---] EX_FIELD                       TYPE        ZZED_LOG_CAMPO
* | [<---] EX_VALUE                       TYPE        ZZED_LOG_OLD
* +--------------------------------------------------------------------------------------</SIGNATURE>
method fill_vbpa.

  case im_vbpavb-parvw.

    when 'AG' or 'RE' or 'RG' or 'WE'.

      concatenate im_vbpavb-parvw '-' im_vbpavb-kunnr into ex_value separated by space.
      ex_field = 'KUNNR'.

    when 'Z2' .

      concatenate im_vbpavb-parvw '-' im_vbpavb-pernr into ex_value separated by space.
      ex_field = 'PERNR'.

    when others.
      clear: ex_field , ex_value.

  endcase.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->GET_CONFIG_CAMPOS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TAB                         TYPE        ZETABNAME
* | [<-()] RE_ZSDT0028_T                  TYPE        ZSDT0028_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_config_campos.

  select *
    from zsdt0028
    into table re_zsdt0028_t
    where tab = im_tab.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->GET_FPLT_VALUE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_FPLT                        TYPE        FPLTVB
* | [--->] IM_VALUE                       TYPE        ZZED_LOG_NEW
* | [<-()] RE_VALUE                       TYPE        ZZED_LOG_NEW
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_fplt_value.

  data: lv_data type char10,
        lv_data2 type char10,
        lv_data3 type char10,
        lv_value type char10,
        lv_value_log type zzed_log_new.

**********************************************************************
** FIELD-SYMBOLS
**********************************************************************
  field-symbols: <fs_value> type any,
                 <fs_value2> type any,
                 <fs_value3> type any.

  assign component 'NFDAT' of structure im_fplt to <fs_value>.
  assign component 'FKDAT' of structure im_fplt to <fs_value2>.
  assign component 'AFDAT' of structure im_fplt to <fs_value3>.

  check <fs_value> is assigned and <fs_value2> is assigned.

  lv_value_log = im_value.
  condense lv_value_log.

  write lv_value_log to lv_value left-justified.
  write <fs_value> dd/mm/yyyy to lv_data.
  write <fs_value2> dd/mm/yyyy to lv_data2.
  write <fs_value3> dd/mm/yyyy to lv_data3.


  concatenate lv_data 'até' lv_data2 '-' lv_data3 ':' lv_value into re_value
              separated by space.

***  •  Campo “NEW_VALUE” com o valor do campo. Concatenar o valor do
***   campo “XFPLT-NFDAT” + “ – “ com o valor do campo. EX. 01.02.2018 – 01.05.2018.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->GET_ITENS_PROCESS
* +-------------------------------------------------------------------------------------------------+
* | [--->] VBAP_NEW                       TYPE        TAB_XYVBAP
* | [--->] VBAP_OLD                       TYPE        TAB_XYVBAP
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_itens_process.

  data: lt_vbap_new type tab_xyvbap.

  field-symbols: <fs_vbap> like line of lt_vbap_new.

  clear: itens_del, itens_ins.

  lt_vbap_new = vbap_new.

  delete lt_vbap_new where updkz = space.

  loop at lt_vbap_new assigning <fs_vbap>.

    if <fs_vbap>-updkz = 'D'.
      append <fs_vbap>-posnr to itens_del.
    elseif <fs_vbap>-updkz = 'I'.
      append <fs_vbap>-posnr to itens_ins.
    endif.

  endloop.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->GET_POSNR_VBKD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_FPLNR                       TYPE        ANY
* | [<-()] RE_POSNR                       TYPE        POSNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_posnr_vbkd.

  data: lv_fplnr type fplnr.

  field-symbols: <fs_vbkd> type vbkd.

  sort: t_vbkd by fplnr.

  lv_fplnr = im_fplnr.

  read table t_vbkd assigning <fs_vbkd> with key fplnr = im_fplnr binary search.
  if sy-subrc = 0.
    re_posnr = <fs_vbkd>-posnr.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->GET_VBELN_VBKD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_FPLNR                       TYPE        ANY
* | [<-()] RE_VBELN                       TYPE        VBELN
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_vbeln_vbkd.

  data: lv_fplnr type fplnr.

  field-symbols: <fs_vbkd> type vbkd.

  sort: t_vbkd by fplnr.

  lv_fplnr = im_fplnr.

  read table t_vbkd assigning <fs_vbkd> with key fplnr = lv_fplnr binary search.
  if sy-subrc = 0.
    re_vbeln = <fs_vbkd>-vbeln.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->INSERT_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_LOG                         TYPE        ZSDT0029
* +--------------------------------------------------------------------------------------</SIGNATURE>
method insert_log.

  data: ls_log type zsdt0029.

  move-corresponding im_log to ls_log.

  "Preencher dados basicos
  ls_log-aedtm = sy-datum.
  ls_log-uzeit = sy-uzeit.
  ls_log-uname = sy-uname.
  ls_log-tcode = sy-tcode.

  append ls_log to t_log.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SD_LOG_DOC->MOD_DOC_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] VBAK_NEW                       TYPE        VBAK
* | [--->] VBAK_OLD                       TYPE        VBAK
* | [--->] VBPA_NEW                       TYPE        TT_VBPAVB
* | [--->] VBPA_OLD                       TYPE        TT_VBPAVB
* | [--->] VBAP_NEW                       TYPE        TAB_XYVBAP
* | [--->] VBAP_OLD                       TYPE        TAB_XYVBAP
* | [--->] VEDA_NEW                       TYPE        ZVEDAVB_TT
* | [--->] VEDA_OLD                       TYPE        ZVEDAVB_TT
* | [--->] VBKD_NEW                       TYPE        TAB_XYVBKD
* | [--->] VBKD_OLD                       TYPE        TAB_XYVBKD
* | [--->] KOMV                           TYPE        KOMV_T
* | [--->] KOMV_OLD                       TYPE        MEOUT_T_UKONVC
* | [--->] FPLA                           TYPE        VA_FPLAVB_T
* | [--->] FPLA_OLD                       TYPE        VA_FPLAVB_T
* | [--->] FPLT                           TYPE        VA_FPLTVB_T
* | [--->] FPLT_OLD                       TYPE        VA_FPLTVB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
method mod_doc_log.

  "Global table
  t_vbkd = vbkd_new.

**********************************************************************
  "VBAK
  if vbpa_new is initial.

    process_tab_campo_del(
      exporting
        im_tab       = 'VBAK'
        im_old_value = vbak_old    ).

  else.

    process_tab_campo_mod(
      exporting
        im_tab   = 'VBAK'
        im_new_value = vbak_new
        im_old_value = vbak_old  ).

  endif.

**********************************************************************
  "VBAP
  process_mod_vbap(
    exporting
      vbap_old = vbap_old
      vbap_new = vbap_new  ).

**********************************************************************
  "VBPA
  process_vbpa( im_vbpa = vbpa_new
                im_vbpa_old = vbpa_old
                im_vbeln = vbak_new-vbeln ).

************************************************************************

  "VEDA
  process_mod_veda(
    exporting
      veda_old = veda_old
      veda_new = veda_new  ).


************************************************************************
  "VBKD
  process_mod_vbkd(
    exporting
      vbkd_old = vbkd_old
      vbkd_new = vbkd_new   ).

************************************************************************
  "KOMV
  process_mod_komv(
    exporting
      im_tab      = 'KOMV'
      im_komv     = komv
      im_komv_old = komv_old
      im_vbeln    = vbak_new-vbeln  ).

************************************************************************
**  "FPLA

  process_mod_flpa(
    exporting
      fpla_old = fpla_old
      fpla_new = fpla  ).


************************************************************************
**  "FPLT

  process_mod_flpt(
    exporting
      fplt_old = fplt_old
      fplt_new = fplt  ).

  "Save DB
  save_log( ).


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_SD_LOG_DOC->NEW_DOC_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] VBAK                           TYPE        VBAK
* | [--->] VBAP                           TYPE        TAB_XYVBAP
* | [--->] VBPA                           TYPE        TT_VBPAVB
* | [--->] VEDA                           TYPE        ZVEDAVB_TT
* | [--->] VBKD                           TYPE        TAB_XYVBKD
* | [--->] KOMV                           TYPE        KOMV_T
* | [--->] FPLA                           TYPE        VA_FPLAVB_T
* | [--->] FPLT                           TYPE        VA_FPLTVB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
method new_doc_log.

  data: ls_vbkd like line of t_vbkd.


  field-symbols: <fs_vbap> like line of vbap,
                 <fs_vbkd> like line of vbkd,
                 <fs_fpla> like line of fpla,
                 <fs_veda> like line of veda,
                 <fs_fplt> like line of fplt.


  "Global table
  loop at vbkd assigning <fs_vbkd>.
    move-corresponding <fs_vbkd> to ls_vbkd.
    ls_vbkd-vbeln = vbak-vbeln.
    <fs_vbkd>-vbeln = vbak-vbeln.
    append ls_vbkd to t_vbkd.
  endloop.

**********************************************************************
  "VBAK
  process_tab_campo(
    exporting
      im_tab   = 'VBAK'
      im_value = vbak
      im_acao  = 'I' ).
**********************************************************************
  "VBAP
  loop at vbap assigning <fs_vbap>.
    <fs_vbap>-vbeln = vbak-vbeln.
    process_tab_campo(
    exporting
      im_tab   = 'VBAP'
      im_value = <fs_vbap>
      im_acao  = 'I' ).
  endloop.

**********************************************************************
  "VBPA
  process_vbpa( im_vbpa = vbpa
                im_vbeln = vbak-vbeln ).

**********************************************************************
  "VEDA

  loop at veda assigning <fs_veda>.
    <fs_veda>-vbeln = vbak-vbeln.
    process_tab_campo(
    exporting
      im_tab   = 'VEDA'
      im_value = <fs_veda>
      im_acao  = 'I' ).
  endloop.

**********************************************************************
  "VBKD
  loop at vbkd assigning <fs_vbkd>.
    <fs_vbkd>-vbeln = vbak-vbeln.
    process_tab_campo(
    exporting
      im_tab   = 'VBKD'
      im_value = <fs_vbkd>
      im_acao  = 'I' ).
  endloop.
**********************************************************************
  "KOMV
  process_mod_komv(
    exporting
      im_tab  = 'KOMV'
      im_komv = komv
      im_vbeln = vbak-vbeln ).

**********************************************************************
  "FPLA
  loop at fpla assigning <fs_fpla>.
    <fs_fpla>-vbeln = vbak-vbeln.
    process_tab_campo(
    exporting
      im_tab   = 'FPLA'
      im_value = <fs_fpla>
      im_acao  = 'I'  ).
  endloop.
**********************************************************************
  "FPLT
  loop at fplt assigning <fs_fplt>.
    process_tab_campo(
    exporting
      im_tab   = 'FPLT'
      im_value = <fs_fplt>
      im_acao  = 'I' ).
  endloop.

  "Save DB
  save_log( ).


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_DEL_KOMV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TAB                         TYPE        ZETABNAME
* | [--->] IM_KOMV                        TYPE        KOMV_T
* | [--->] IM_KOMV_OLD                    TYPE        MEOUT_T_UKONVC
* | [--->] IM_VBELN                       TYPE        VBELN_VA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_del_komv.

  data: lv_krech type krech,
        lv_kbetr type kbetr.
**********************************************************************
** LOCAL TABLE
**********************************************************************
  data: lt_zsdt0028 type table of zsdt0028,
        lt_komv     type komv_t,
        lt_komv_old type meout_t_ukonvc.

**********************************************************************
** LOCAL STRUCTURE
**********************************************************************
  data: ls_log type zsdt0029.

**********************************************************************
** FIELD-SYMBOLS
**********************************************************************
  field-symbols: <fs_zsdt0028> type zsdt0028,
                 <fs_komv> like line of lt_komv,
                 <fs_komv_old> like line of lt_komv_old.


  lt_komv[] = im_komv[].
  sort: lt_komv by kposn kschl.

  lt_komv_old[] = im_komv_old[].
  sort: lt_komv_old by kschl.



  "Busca campos configurados do LOG
  lt_zsdt0028 = get_config_campos( im_tab = im_tab ).

  loop at lt_zsdt0028 assigning <fs_zsdt0028>.

    "Localiza index da condição
    read table lt_komv_old transporting no fields with key kschl = <fs_zsdt0028>-campo binary search.
    check sy-subrc = 0.
    "Passar por todos itens
    loop at lt_komv_old assigning <fs_komv_old> from sy-tabix.

      "Condição saida
      if <fs_komv_old>-kschl <> <fs_zsdt0028>-campo.
        exit.
      endif.


      read table lt_komv assigning <fs_komv> with key kposn = <fs_komv_old>-kposn
                                                      kschl = <fs_komv_old>-kschl
                                                      binary search.
      if sy-subrc = 0.

        if <fs_komv_old>-kbetr <> <fs_komv>-kbetr.

          lv_kbetr = <fs_komv>-kbetr.
          write lv_kbetr to ls_log-new_value currency 'BRL'.
          lv_kbetr = <fs_komv_old>-kbetr.
          write lv_kbetr to ls_log-old_value currency 'BRL'.
          ls_log-tp_act = 'U'.

        else.
          continue.
        endif.

        "Item excluido
      else.

        "Verifica se a condição é percentual
        select single krech
          from t685a
          into lv_krech
          where kschl = <fs_komv_old>-kschl.
        if sy-subrc = 0 and lv_krech = 'A'.
          <fs_komv_old>-kbetr = <fs_komv_old>-kbetr / 10.
        endif.

        lv_kbetr = <fs_komv_old>-kbetr.
        write lv_kbetr to ls_log-old_value currency 'BRL'.
        ls_log-tp_act = 'E'.

      endif.


      condense: ls_log-new_value, ls_log-old_value.

      ls_log-field = <fs_zsdt0028>-campo.
      ls_log-tab = im_tab.
      ls_log-posnr = <fs_komv_old>-kposn.
      ls_log-vbeln = im_vbeln.

      insert_log( im_log = ls_log ).

      clear: ls_log.
      unassign <fs_komv>.

    endloop.

  endloop.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_INS_KOMV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TAB                         TYPE        ZETABNAME
* | [--->] IM_KOMV                        TYPE        KOMV_T
* | [--->] IM_KOMV_OLD                    TYPE        MEOUT_T_UKONVC
* | [--->] IM_VBELN                       TYPE        VBELN_VA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_ins_komv.

  data: lv_kbetr type kbetr.
**********************************************************************
** LOCAL TABLE
**********************************************************************
  data: lt_zsdt0028 type table of zsdt0028,
        lt_komv     type komv_t,
        lt_komv_old type meout_t_ukonvc.

**********************************************************************
** LOCAL STRUCTURE
**********************************************************************
  data: ls_log type zsdt0029.

**********************************************************************
** FIELD-SYMBOLS
**********************************************************************
  field-symbols: <fs_zsdt0028> type zsdt0028,
                 <fs_komv> like line of lt_komv,
                 <fs_komv_old> like line of lt_komv_old.


  lt_komv[] = im_komv[].
  sort: lt_komv by kschl.

  lt_komv_old[] = im_komv_old[].
  sort: lt_komv_old by kposn kschl.



  "Busca campos configurados do LOG
  lt_zsdt0028 = get_config_campos( im_tab = im_tab ).

  loop at lt_zsdt0028 assigning <fs_zsdt0028>.

    "Localiza index da condição
    read table lt_komv transporting no fields with key kschl = <fs_zsdt0028>-campo binary search.
    check sy-subrc = 0.
    "Passar por todos itens
    loop at lt_komv assigning <fs_komv> from sy-tabix.

      "Condição saida
      if <fs_komv>-kschl <> <fs_zsdt0028>-campo.
        exit.
      endif.


      read table lt_komv_old assigning <fs_komv_old> with key kposn = <fs_komv>-kposn
                                                              kschl = <fs_komv>-kschl
                                                              binary search.
      if sy-subrc = 0.

        if <fs_komv_old>-kbetr <> <fs_komv>-kbetr.

          lv_kbetr = <fs_komv>-kbetr.
          if <fs_komv>-krech = 'A'.
            lv_kbetr = lv_kbetr / 10.
          endif.
          write lv_kbetr to ls_log-new_value. " currency 'BRL'.
          lv_kbetr = <fs_komv_old>-kbetr.
          if <fs_komv>-krech = 'A'.
            lv_kbetr = lv_kbetr / 10.
          endif.
          write lv_kbetr to ls_log-old_value. " currency 'BRL'.
          ls_log-tp_act = 'U'.

        else.
          continue.
        endif.

        "Item novo
      else.
        lv_kbetr = <fs_komv>-kbetr.
        if <fs_komv>-krech = 'A'.
          lv_kbetr = lv_kbetr / 10.
        endif.
        write lv_kbetr to ls_log-new_value currency 'BRL'.
        ls_log-tp_act = 'I'. "Inserir
      endif.



      if <fs_komv>-krech = 'A'.
        lv_kbetr = <fs_komv>-kbetr / 10.
        write lv_kbetr to ls_log-new_value currency 'BRL'.
      endif.

      condense: ls_log-new_value, ls_log-old_value.

      ls_log-field = <fs_zsdt0028>-campo.
      ls_log-tab = im_tab.
      ls_log-posnr = <fs_komv>-kposn.
      ls_log-vbeln = im_vbeln.

      insert_log( im_log = ls_log ).

      clear: ls_log.

    endloop.

  endloop.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_MOD_FLPA
* +-------------------------------------------------------------------------------------------------+
* | [--->] FPLA_OLD                       TYPE        VA_FPLAVB_T
* | [--->] FPLA_NEW                       TYPE        VA_FPLAVB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_mod_flpa.

  data: lt_fpla_new type va_fplavb_t,
        lt_fpla_old type va_fplavb_t,
        lt_fpla_del type va_fplavb_t.

  field-symbols: <fs_fpla_new> like line of fpla_new,
                 <fs_fpla_old> like line of fpla_old.

  lt_fpla_new = fpla_new.
  lt_fpla_old = fpla_old.
  lt_fpla_del = fpla_old.

  delete lt_fpla_new where updkz = space.
  delete lt_fpla_del where updkz <> 'D'.

  sort: lt_fpla_old by fplnr.

  loop at lt_fpla_new assigning <fs_fpla_new>.

    "Verifica se item foi modificado
    if <fs_fpla_new>-updkz = 'U'.

      read table lt_fpla_old assigning <fs_fpla_old> with key fplnr = <fs_fpla_new>-fplnr
                                                          binary search.
      if sy-subrc = 0.

        process_tab_campo_mod(
          exporting
            im_tab       = 'FPLA'
            im_new_value = <fs_fpla_new>
            im_old_value = <fs_fpla_old>  ).

      endif.

      "Verifica se item foi incluido
    elseif <fs_fpla_new>-updkz = 'I'.

      process_tab_campo(
          exporting
            im_tab   = 'FPLA'
            im_value = <fs_fpla_new>
            im_acao  = 'I'   ).

    endif.

  endloop.


  "Itens excluidos
  loop at lt_fpla_del assigning <fs_fpla_old>.

    process_tab_campo(
      exporting
        im_tab   = 'FPLA'
        im_value = <fs_fpla_old>
        im_acao  = 'E'   ).

  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_MOD_FLPT
* +-------------------------------------------------------------------------------------------------+
* | [--->] FPLT_OLD                       TYPE        VA_FPLTVB_T
* | [--->] FPLT_NEW                       TYPE        VA_FPLTVB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_mod_flpt.

  data: lt_fplt_new type va_fpltvb_t,
        lt_fplt_old type va_fpltvb_t,
        lt_fplt_del type va_fpltvb_t.

  field-symbols: <fs_fplt_new> like line of fplt_new,
                 <fs_fplt_old> like line of fplt_old.


  lt_fplt_new = fplt_new.
  lt_fplt_old = fplt_old.
  lt_fplt_del = fplt_old.

  delete lt_fplt_new where updkz = space.
  delete lt_fplt_del where updkz <> 'D'.

  sort lt_fplt_old by fplnr fpltr.

  loop at lt_fplt_new assigning <fs_fplt_new>.

    "Verifica se item foi modificado
    if <fs_fplt_new>-updkz = 'U'.

      read table lt_fplt_old assigning <fs_fplt_old> with key fplnr = <fs_fplt_new>-fplnr
                                                              fpltr = <fs_fplt_new>-fpltr
                                                              binary search.
      if sy-subrc = 0.

        process_tab_campo_mod(
          exporting
            im_tab       = 'FPLT'
            im_new_value = <fs_fplt_new>
            im_old_value = <fs_fplt_old>  ).

      endif.

      "Verifica se item foi incluido
    elseif <fs_fplt_new>-updkz = 'I'.

      process_tab_campo(
          exporting
            im_tab   = 'FPLT'
            im_value = <fs_fplt_new>
            im_acao  = 'I'   ).

    endif.

  endloop.


  loop at lt_fplt_del assigning <fs_fplt_old>.

    process_tab_campo(
      exporting
        im_tab   = 'FPLT'
        im_value = <fs_fplt_old>
        im_acao  = 'E'   ).

  endloop.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_MOD_KOMV
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TAB                         TYPE        ZETABNAME
* | [--->] IM_KOMV                        TYPE        KOMV_T
* | [--->] IM_KOMV_OLD                    TYPE        MEOUT_T_UKONVC(optional)
* | [--->] IM_VBELN                       TYPE        VBELN_VA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_mod_komv.

  data: lv_qtd_new   type i,
        lv_qtd_old   type i.
**********************************************************************
** LOCAL TABLE
**********************************************************************
  data:   lt_komv     type komv_t,
          lt_komv_old type meout_t_ukonvc.
**********************************************************************
** LOCAL Range
**********************************************************************
  data: lr_posnr type range of posnr,
        ls_posnr like line of lr_posnr.
**********************************************************************
** FIELD-SYMBOLS
**********************************************************************
  field-symbols: <fs_zsdt0028> type zsdt0028,
                 <fs_komv> like line of lt_komv,
                 <fs_komv_old> like line of lt_komv_old,
                 <fs_posnr> like line of itens_del.



  lt_komv[] = im_komv[].
  describe table lt_komv lines lv_qtd_new.

  lt_komv_old[] = im_komv_old[].
  describe table lt_komv_old lines lv_qtd_old.

  "Foram inseridos novos itens
  if lv_qtd_new > lv_qtd_old.

    process_ins_komv(
      exporting
        im_tab      = im_tab
        im_komv     = im_komv
        im_komv_old = im_komv_old
        im_vbeln    = im_vbeln   ).

    "Foram excluidos itens
  elseif lv_qtd_old > lv_qtd_new.

    process_del_komv(
      exporting
        im_tab      = im_tab
        im_komv     = im_komv
        im_komv_old = im_komv_old
        im_vbeln    = im_vbeln   ).

  else.


    if itens_del is not initial.

      ls_posnr-sign = 'I'.
      ls_posnr-option = 'EQ'.

      loop at itens_del assigning <fs_posnr>.
        ls_posnr-low = <fs_posnr>.
        append ls_posnr to lr_posnr.
      endloop.


      delete lt_komv where kposn in lr_posnr.
      delete lt_komv_old where kposn not in lr_posnr.
      clear: lr_posnr.

      process_del_komv(
      exporting
        im_tab      = im_tab
        im_komv     = lt_komv
        im_komv_old = lt_komv_old
        im_vbeln    = im_vbeln   ).

    endif.

    lt_komv = im_komv.
    lt_komv_old = im_komv_old.

    if itens_ins is not initial.

      ls_posnr-sign = 'I'.
      ls_posnr-option = 'EQ'.

      loop at itens_ins assigning <fs_posnr>.
        ls_posnr-low = <fs_posnr>.
        append ls_posnr to lr_posnr.
      endloop.

      delete lt_komv_old where kposn in lr_posnr.
      delete lt_komv where kposn not in lr_posnr.


      process_ins_komv(
        exporting
          im_tab      = im_tab
          im_komv     = lt_komv
          im_komv_old = lt_komv_old
          im_vbeln    = im_vbeln   ).

    endif.

    if itens_del is initial and
       itens_ins is initial.

      process_ins_komv(
      exporting
        im_tab      = im_tab
        im_komv     = im_komv
        im_komv_old = im_komv_old
        im_vbeln    = im_vbeln   ).

    endif.

  endif.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_MOD_VBAP
* +-------------------------------------------------------------------------------------------------+
* | [--->] VBAP_OLD                       TYPE        TAB_XYVBAP
* | [--->] VBAP_NEW                       TYPE        TAB_XYVBAP
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_mod_vbap.

  data: lt_vbap type tab_xyvbap.

  field-symbols: <fs_vbap_new> like line of vbap_new,
                 <fs_vbap_old> like line of vbap_old.

  lt_vbap = vbap_old.
  sort lt_vbap by vbeln posnr.

  loop at vbap_new assigning <fs_vbap_new>.

    "Verifica se item foi modificado
    if <fs_vbap_new>-updkz = 'U'.

      read table lt_vbap assigning <fs_vbap_old> with key vbeln = <fs_vbap_new>-vbeln
                                                          posnr = <fs_vbap_new>-posnr
                                                          binary search.
      if sy-subrc = 0.

        process_tab_campo_mod(
          exporting
            im_tab       = 'VBAP'
            im_new_value = <fs_vbap_new>
            im_old_value = <fs_vbap_old>  ).

      endif.

      "Verifica se item foi deletado
    elseif <fs_vbap_new>-updkz = 'D'.

      process_tab_campo(
        exporting
          im_tab   = 'VBAP'
          im_value = <fs_vbap_new>
          im_acao  = 'E'   ).

      "Verifica se item foi incluido
    elseif <fs_vbap_new>-updkz = 'I'.

      process_tab_campo(
          exporting
            im_tab   = 'VBAP'
            im_value = <fs_vbap_new>
            im_acao  = 'I'   ).

    endif.

  endloop.



  get_itens_process(
    exporting
      vbap_new = vbap_new
      vbap_old = vbap_old    ).


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_MOD_VBKD
* +-------------------------------------------------------------------------------------------------+
* | [--->] VBKD_OLD                       TYPE        TAB_XYVBKD
* | [--->] VBKD_NEW                       TYPE        TAB_XYVBKD
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_mod_vbkd.

  data: lt_vbkd type tab_xyvbkd.

  field-symbols: <fs_vbkd_new> like line of vbkd_new,
                 <fs_vbkd_old> like line of vbkd_old.

  lt_vbkd = vbkd_old.
  sort lt_vbkd by vbeln posnr.

  loop at vbkd_new assigning <fs_vbkd_new>.

    "Verifica se item foi modificado
    if <fs_vbkd_new>-updkz = 'U'.

      read table lt_vbkd assigning <fs_vbkd_old> with key vbeln = <fs_vbkd_new>-vbeln
                                                          posnr = <fs_vbkd_new>-posnr
                                                          binary search.
      if sy-subrc = 0.

        process_tab_campo_mod(
          exporting
            im_tab       = 'VBKD'
            im_new_value = <fs_vbkd_new>
            im_old_value = <fs_vbkd_old>  ).

      endif.

      "Verifica se item foi deletado
    elseif <fs_vbkd_new>-updkz = 'D'.

      process_tab_campo(
        exporting
          im_tab   = 'VBKD'
          im_value = <fs_vbkd_new>
          im_acao  = 'E'   ).

      "Verifica se item foi incluido
    elseif <fs_vbkd_new>-updkz = 'I'.

      process_tab_campo(
          exporting
            im_tab   = 'VBKD'
            im_value = <fs_vbkd_new>
            im_acao  = 'I'   ).

    endif.

  endloop.



endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_MOD_VEDA
* +-------------------------------------------------------------------------------------------------+
* | [--->] VEDA_OLD                       TYPE        ZVEDAVB_TT
* | [--->] VEDA_NEW                       TYPE        ZVEDAVB_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_mod_veda.

  data: lt_veda type zvedavb_tt,
        lt_veda_del type zvedavb_tt.

  field-symbols: <fs_veda_new> like line of veda_new,
                 <fs_veda_old> like line of veda_old.

  lt_veda = veda_old.
  sort lt_veda by vbeln vposn.

  loop at veda_new assigning <fs_veda_new>.

    "Verifica se item foi modificado
    if <fs_veda_new>-updkz = 'U'.

      read table lt_veda assigning <fs_veda_old> with key vbeln = <fs_veda_new>-vbeln
                                                          vposn = <fs_veda_new>-vposn
                                                          binary search.
      if sy-subrc = 0.

        process_tab_campo_mod(
          exporting
            im_tab       = 'VEDA'
            im_new_value = <fs_veda_new>
            im_old_value = <fs_veda_old>  ).

      endif.

      "Verifica se item foi incluido
    elseif <fs_veda_new>-updkz = 'I'.

      process_tab_campo(
          exporting
            im_tab   = 'VEDA'
            im_value = <fs_veda_new>
            im_acao  = 'I'   ).

    endif.

  endloop.


  lt_veda_del = veda_old.
  delete lt_veda_del where updkz <> 'D'.

  loop at lt_veda_del assigning <fs_veda_old>.

    process_tab_campo(
      exporting
        im_tab   = 'VEDA'
        im_value = <fs_veda_old>
        im_acao  = 'E'   ).

  endloop.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_TAB_CAMPO
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TAB                         TYPE        ZETABNAME
* | [--->] IM_VALUE                       TYPE        ANY
* | [--->] IM_ACAO                        TYPE        ZACAO
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_tab_campo.

**********************************************************************
** LOCAL TABLE
**********************************************************************
  data: lt_zsdt0028 type table of zsdt0028.

**********************************************************************
** LOCAL STRUCTURE
**********************************************************************
  data: ls_log type zsdt0029,
        ls_data type ref to data.

**********************************************************************
** FIELD-SYMBOLS
**********************************************************************
  field-symbols: <fs_zsdt0028> type zsdt0028,
                 <fs_value> type any,
                 <fs_value2> type any,
                 <fs_data> type any.


  create data ls_data like im_value.
  assign ls_data->* to <fs_data>.

  move-corresponding im_value to <fs_data>.

  "Busca campos configurados do LOG
  lt_zsdt0028 = get_config_campos( im_tab = im_tab ).

  loop at lt_zsdt0028 assigning <fs_zsdt0028>.

    clear: ls_log.
    unassign <fs_value>.

**********************************************************************
** VBELN
    assign component 'VBELN' of structure <fs_data> to <fs_value>.
    if <fs_value> is assigned.
      ls_log-vbeln = <fs_value>.
      unassign <fs_value>.

    elseif im_tab = 'FPLT'.
      "Obtem valor da VBKD
      assign component 'FPLNR' of structure <fs_data> to <fs_value>.
      ls_log-vbeln = get_vbeln_vbkd( <fs_value> ).
      unassign <fs_value>.
    endif.

**********************************************************************
** POSNR
    assign component 'POSNR' of structure <fs_data> to <fs_value>.
    if <fs_value> is assigned.

      ls_log-posnr = <fs_value>.
      unassign <fs_value>.

    elseif im_tab = 'FPLA' or im_tab = 'FPLT'.
      "Obtem valor da VBKD
      assign component 'FPLNR' of structure <fs_data> to <fs_value>.
      ls_log-posnr = get_posnr_vbkd( <fs_value> ).
      unassign <fs_value>.

    elseif im_tab = 'VEDA'.
      assign component 'VPOSN' of structure <fs_data> to <fs_value>.
      ls_log-posnr = <fs_value>.
      unassign <fs_value>.
    endif.


    "Verifica se esta processando item ou cabeçalho
    if ls_log-posnr is not initial and <fs_zsdt0028>-tipo <> 'I'.
      continue.
    elseif ls_log-posnr is initial and <fs_zsdt0028>-tipo = 'I'.
      continue.
    endif.


    assign component <fs_zsdt0028>-campo of structure <fs_data> to <fs_value>.
    check <fs_value> is assigned.



    case im_acao.

        "Inserir Item
      when 'I' .
        ls_log-new_value = check_type_write( im_value = <fs_value> ).
        clear: ls_log-old_value.

        if im_tab = 'FPLT'.
          ls_log-new_value = get_fplt_value( im_fplt  = <fs_data>
                                             im_value = ls_log-new_value ).
        endif.

        "Excluir Item
      when 'E'.

        ls_log-old_value = check_type_write( im_value =  <fs_value> ).
        clear: ls_log-new_value.

        if im_tab = 'FPLT'.
          ls_log-old_value = get_fplt_value( im_fplt  = <fs_data>
                                             im_value = ls_log-old_value ).
        endif.

      when others.
    endcase.

    ls_log-field = <fs_zsdt0028>-campo.
    ls_log-tp_act = im_acao.
    ls_log-tab = im_tab.


    insert_log( im_log = ls_log ).

    clear: ls_log.
    unassign <fs_value>.


  endloop.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_TAB_CAMPO_DEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TAB                         TYPE        ZETABNAME
* | [--->] IM_OLD_VALUE                   TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_tab_campo_del.

**********************************************************************
** LOCAL TABLE
**********************************************************************
  data: lt_zsdt0028 type table of zsdt0028.

**********************************************************************
** LOCAL STRUCTURE
**********************************************************************
  data: ls_log type zsdt0029,
        ls_old_data type ref to data.

**********************************************************************
** FIELD-SYMBOLS
**********************************************************************
  field-symbols: <fs_zsdt0028> type zsdt0028,
                 <fs_old_value> type any,
                 <fs_old_data> type any.

  create data ls_old_data like im_old_value.
  assign ls_old_data->* to <fs_old_data>.
  move-corresponding im_old_value to <fs_old_data>.

  "Busca campos configurados do LOG
  lt_zsdt0028 = get_config_campos( im_tab = im_tab ).

  loop at lt_zsdt0028 assigning <fs_zsdt0028>.

    clear: ls_log.
**********************************************************************
** VBELN
    assign component 'VBELN' of structure <fs_old_data> to <fs_old_value>.
    if <fs_old_value> is assigned.
      ls_log-vbeln = <fs_old_value>.
      unassign <fs_old_value>.
    endif.

**********************************************************************
** POSNR
    assign component 'POSNR' of structure <fs_old_data> to <fs_old_value>.
    if <fs_old_value> is assigned.
      ls_log-posnr = <fs_old_value>.
      unassign <fs_old_value>.
    endif.


    "Verifica se esta processando item ou cabeçalho
    if ls_log-posnr is not initial and <fs_zsdt0028>-tipo <> 'I'.
      continue.
    elseif ls_log-posnr is initial and <fs_zsdt0028>-tipo = 'I'.
      continue.
    endif.


    assign component <fs_zsdt0028>-campo of structure <fs_old_data> to <fs_old_value>.
    check <fs_old_value> is assigned.

    ls_log-old_value = check_type_write( im_value =  <fs_old_value> ).

    ls_log-field = <fs_zsdt0028>-campo.
    ls_log-tp_act = 'E'. "Inserir
    ls_log-tab = im_tab.

    insert_log( im_log = ls_log ).

    unassign <fs_old_value>.

  endloop.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_TAB_CAMPO_MOD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_TAB                         TYPE        ZETABNAME
* | [--->] IM_NEW_VALUE                   TYPE        ANY
* | [--->] IM_OLD_VALUE                   TYPE        ANY
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_tab_campo_mod.

**********************************************************************
** LOCAL TABLE
**********************************************************************
  data: lt_zsdt0028 type table of zsdt0028.

**********************************************************************
** LOCAL STRUCTURE
**********************************************************************
  data: ls_log type zsdt0029,
        ls_new_data type ref to data,
        ls_old_data type ref to data.

**********************************************************************
** FIELD-SYMBOLS
**********************************************************************
  field-symbols: <fs_zsdt0028> type zsdt0028,
                 <fs_new_value> type any,
                 <fs_old_value> type any,
                 <fs_new_data> type any,
                 <fs_old_data> type any.

  create data ls_new_data like im_new_value.
  assign ls_new_data->* to <fs_new_data>.
  move-corresponding im_new_value to <fs_new_data>.

  create data ls_old_data like im_old_value.
  assign ls_old_data->* to <fs_old_data>.
  move-corresponding im_old_value to <fs_old_data>.

  "Busca campos configurados do LOG
  lt_zsdt0028 = get_config_campos( im_tab = im_tab ).

  loop at lt_zsdt0028 assigning <fs_zsdt0028>.

    clear: ls_log.
    unassign: <fs_new_value>, <fs_old_value>.
**********************************************************************
** VBELN
    assign component 'VBELN' of structure <fs_new_data> to <fs_new_value>.
    if <fs_new_value> is assigned.
      ls_log-vbeln = <fs_new_value>.
      unassign <fs_new_value>.

    elseif im_tab = 'FPLT'.
      "Obtem valor da VBKD
      assign component 'FPLNR' of structure <fs_new_data> to <fs_new_value>.
      ls_log-vbeln = get_vbeln_vbkd( <fs_new_value> ).
      unassign <fs_new_value>.
    endif.

**********************************************************************
** POSNR
    assign component 'POSNR' of structure <fs_new_data> to <fs_new_value>.
    if <fs_new_value> is assigned.

      ls_log-posnr = <fs_new_value>.
      unassign <fs_new_value>.

    elseif im_tab = 'FPLA' or im_tab = 'FPLT'.
      "Obtem valor da VBKD
      assign component 'FPLNR' of structure <fs_new_data> to <fs_new_value>.
      ls_log-posnr = get_posnr_vbkd( <fs_new_value> ).
      unassign <fs_new_value>.

    elseif im_tab = 'VEDA'.
      assign component 'VPOSN' of structure <fs_new_data> to <fs_new_value>.
      ls_log-posnr = <fs_new_value>.
      unassign <fs_new_value>.
    endif.


    "Verifica se esta processando item ou cabeçalho
    if ls_log-posnr is not initial and <fs_zsdt0028>-tipo <> 'I'.
      continue.
    elseif ls_log-posnr is initial and <fs_zsdt0028>-tipo = 'I'.
      continue.
    endif.


    assign component <fs_zsdt0028>-campo of structure <fs_new_data> to <fs_new_value>.
    check <fs_new_value> is assigned.

    assign component <fs_zsdt0028>-campo of structure <fs_old_data> to <fs_old_value>.
    check <fs_old_value> is assigned.

    "Se valor modificado para gravar log
    check <fs_old_value> <> <fs_new_value>.


    ls_log-new_value = check_type_write( im_value =  <fs_new_value> ).

    if im_tab = 'FPLT'.
      ls_log-new_value = get_fplt_value( im_fplt  = <fs_new_data>
                                         im_value = ls_log-new_value   ).
    endif.

    ls_log-old_value = check_type_write( im_value =  <fs_old_value> ).

    if im_tab = 'FPLT'.
      ls_log-old_value = get_fplt_value( im_fplt  = <fs_old_data>
                                       im_value = ls_log-old_value   ).
    endif.



    ls_log-field = <fs_zsdt0028>-campo.
    ls_log-tp_act = 'U'. "Inserir
    ls_log-tab = im_tab.

    insert_log( im_log = ls_log ).


    unassign <fs_new_value>.
    unassign <fs_old_value>.


  endloop.


endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->PROCESS_VBPA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_VBPA_OLD                    TYPE        TT_VBPAVB(optional)
* | [--->] IM_VBPA                        TYPE        TT_VBPAVB
* | [--->] IM_VBELN                       TYPE        VBELN_VA
* +--------------------------------------------------------------------------------------</SIGNATURE>
method process_vbpa.
**********************************************************************
** LOCAL STRUCTURE
**********************************************************************
  data: ls_log type zsdt0029.

**********************************************************************
** LOCAL STRUCTURE
**********************************************************************
  data: lt_vbpa	type tt_vbpavb,
        lt_vbpa_old type tt_vbpavb.

**********************************************************************
** FIELD-SYMBOLS
**********************************************************************
  field-symbols : <fs_vbpa> like line of im_vbpa,
                  <fs_vbpa_aux> like line of im_vbpa,
                  <fs_posnr>    like line of itens_ins.

  lt_vbpa = im_vbpa.
  lt_vbpa_old = im_vbpa_old.
  sort: lt_vbpa by parvw updkz,
        lt_vbpa_old by parvw updkz.

  data: lr_posnr type range of posnr,
        ls_posnr like line of lr_posnr.

  "Itens inseridos
  if itens_ins is not initial.
    ls_posnr-sign = 'I'.
    ls_posnr-option = 'EQ'.

    loop at itens_ins assigning <fs_posnr>.
      ls_posnr-low = <fs_posnr>.
      append ls_posnr to lr_posnr.
    endloop.

  endif.



  loop at im_vbpa assigning <fs_vbpa>.

    check <fs_vbpa>-updkz is not initial.

    fill_vbpa(
      exporting
        im_vbpavb = <fs_vbpa>
      importing
        ex_field  = ls_log-field
        ex_value  = ls_log-new_value    ).

    if ls_log-field is initial and ls_log-new_value is initial.
      continue.
    endif.


    ls_log-tp_act = 'I'.
    ls_log-tab = 'VBPA'.
    ls_log-posnr = <fs_vbpa>-posnr.
    ls_log-vbeln = im_vbeln.


    if <fs_vbpa>-updkz = 'I'.

      if  <fs_vbpa>-posnr not in lr_posnr.

        "Verifica se item foi alterado do cabeçalho
        read table lt_vbpa assigning <fs_vbpa_aux> with key parvw = <fs_vbpa>-parvw
                                                            updkz = space
                                                            binary search.
        if sy-subrc = 0.

          fill_vbpa(
            exporting
              im_vbpavb = <fs_vbpa_aux>
            importing
              ex_field  = ls_log-field
              ex_value  = ls_log-old_value    ).

          ls_log-tp_act = 'U'.

        endif.
      endif.


    elseif <fs_vbpa>-updkz = 'U'.

      "Verifica se item foi alterado do valor antigo
      read table lt_vbpa_old assigning <fs_vbpa_aux> with key parvw = <fs_vbpa>-parvw
                                                              updkz = space
                                                              binary search.
      if sy-subrc = 0.

        fill_vbpa(
          exporting
            im_vbpavb = <fs_vbpa_aux>
          importing
            ex_field  = ls_log-field
            ex_value  = ls_log-old_value    ).

        ls_log-tp_act = 'U'.

      endif.

    endif.

    insert_log( im_log = ls_log ).

    clear: ls_log.

  endloop.



  "Itens deletados
  loop at lt_vbpa_old assigning <fs_vbpa> where updkz = 'D'.

    fill_vbpa(
    exporting
      im_vbpavb = <fs_vbpa>
    importing
      ex_field  = ls_log-field
      ex_value  = ls_log-old_value    ).

    if ls_log-field is initial and ls_log-old_value is initial.
      continue.
    endif.

    ls_log-tp_act = 'E'.
    ls_log-tab = 'VBPA'.
    ls_log-posnr = <fs_vbpa>-posnr.
    ls_log-vbeln = im_vbeln.

    insert_log( im_log = ls_log ).
    clear: ls_log.

  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_SD_LOG_DOC->SAVE_LOG
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method save_log.

**********************************************************************
** PROCESSAMENTO EM USER-EXIT - NÃO UTILIZAR COMMIT

  check t_log is not initial.

  modify zsdt0029 from table t_log.

  clear: t_log.

endmethod.
ENDCLASS.
