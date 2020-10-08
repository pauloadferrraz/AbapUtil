REPORT zhrr010.

TABLES: pa0000.


CLASS lcl_report DEFINITION.

  PUBLIC SECTION.

    CONSTANTS: c_cnpj_empresa TYPE char16 VALUE 'CNPJ_EMPRESA',
               c_nome_empresa TYPE char16 VALUE 'NOME_EMPRESA'.

    TYPES:
      BEGIN OF tp_refdata,
        pernr TYPE REF TO data,
      END OF tp_refdata .

    DATA refdata TYPE tp_refdata .

    TYPES: BEGIN OF ty_layout,
             dt_atulizacao   TYPE c LENGTH 10,            "Data de atualização cadastro
             codigo          TYPE n LENGTH 15,              "Código do Tipo de folha
             cpf             TYPE n LENGTH 15,             "CPF
             nome            TYPE c LENGTH 150,            "Nome funcionario
             dt_nasci        TYPE c LENGTH 10,             "Data Nascimento
             dt_admissao     TYPE c LENGTH 10,             "Data Admissão
             dt_demissao     TYPE c LENGTH 10,             "Data Demissão
             cod_cargo       TYPE n LENGTH 12,             "Código Cargo
             nome_cargo      TYPE c LENGTH 150,            "Descrição Cargo
             cod_setor       TYPE n LENGTH 10,             "Codigo Setor
             nome_setor      TYPE c LENGTH 150,            "Descrição setor
             sexo            TYPE c LENGTH 1,              "Sexo
             dependentes     TYPE n LENGTH 3,              "Dependentes IRF
             status          TYPE c LENGTH 1,              "Situação funcionaior Ativa/Inativo
             reg_juridico    TYPE c LENGTH 1,              "Regime juridico
             natureza_cargo  TYPE c LENGTH 1,              "Natureza do cargo
             previdencia     TYPE c LENGTH 1,              "Tipo de previdencia
             rg              TYPE n LENGTH 15,             "Rg funcionario
             cbo             TYPE n LENGTH 10,             "CBO - Ministerio do trabalho
             nit             TYPE n LENGTH 11,             "NIT (PIS/PASEP/SI/SUS)
             categoria       TYPE c LENGTH 1,              "Categoria do trabalhor
             endereco        TYPE c LENGTH 150,            "Endereço
             cidade          TYPE c LENGTH 150,            "Cidade
             uf              TYPE c LENGTH 2,              "UF
             cep             TYPE n LENGTH 10,             "CEP
             observacao      TYPE c LENGTH 256,            "Observações
             carga_horario   TYPE n LENGTH 10,              "Carga Horária de Trabalho
             unidade_horario TYPE c LENGTH 1,              "Unidade de Carga Horária
           END OF ty_layout,


           BEGIN OF ty_pa0000,
             pernr TYPE pa0000-pernr,
             begda TYPE pa0000-begda,
             endda TYPE pa0000-endda,
             massn TYPE pa0000-massg,
           END OF ty_pa0000,

           BEGIN OF ty_dados_empresa,
             cnpj TYPE c LENGTH 14,
             nome TYPE c LENGTH 40,
           END OF ty_dados_empresa.

    METHODS:
      main IMPORTING iv_save_on_server TYPE abap_bool
                     iv_filename       TYPE string.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF tp_selopt,
        pernr TYPE RANGE OF pernr_d,
      END OF tp_selopt .

    DATA selopt TYPE tp_selopt .

    DATA:
      gt_data   TYPE STANDARD TABLE OF ty_layout,
      gt_pa0000 TYPE STANDARD TABLE OF ty_pa0000.

    DATA: gv_begda TYPE datum,
          gv_endda TYPE datum.


    METHODS:
      get_data EXCEPTIONS not_found,

      fill_header_file CHANGING cs_header TYPE zshr_header_tce,

      selection_options,

      count_dependentes_irf IMPORTING iv_pernr TYPE pernr_d
                            EXPORTING ex_count TYPE ty_layout-dependentes,

      get_regime_juridico IMPORTING iv_persg   TYPE persg
                                    iv_persk   TYPE persk
                          CHANGING  cv_reg_jur TYPE ty_layout-reg_juridico
                                    cv_obs     TYPE ty_layout-observacao,

      get_categoria IMPORTING iv_cattr TYPE pbr_cattr
                    CHANGING  cv_categ TYPE ty_layout-categoria
                              cv_obs   TYPE ty_layout-observacao,

      get_datas_funcionario IMPORTING iv_pernr    TYPE pernr_d
                            CHANGING  cv_admissao TYPE ty_layout-dt_admissao
                                      cv_demissao TYPE ty_layout-dt_demissao.



ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD main.

    DATA: lo_content TYPE REF TO data,
          ls_header  TYPE zshr_header_tce.

    "Transforma selection options
    selection_options(  ).

***** Ler e Processar as informações
    me->get_data( EXCEPTIONS not_found      = 1 ).
    IF sy-subrc = 1.
      MESSAGE TEXT-m04 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

*   Montar o cabeçalho de linha
    fill_header_file( CHANGING cs_header = ls_header ).


*   Realizar o download:
    GET REFERENCE OF gt_data INTO lo_content.
    zhr_file_download=>download( EXPORTING io_data           = lo_content
                                           iv_separador      = '|'
                                           iv_save_on_server = iv_save_on_server
                                           iv_filename       = iv_filename
                                           iv_header         = ls_header
                                  EXCEPTIONS save_error         = 1
                                             conversion_error   = 2
                                             filename_mandatory = 3 ).

    CASE sy-subrc.
      WHEN 0.
        MESSAGE TEXT-m01 TYPE 'S'.

      WHEN 3.
        MESSAGE TEXT-m03 TYPE 'S' DISPLAY LIKE 'E'.

      WHEN OTHERS.
        MESSAGE TEXT-m02 TYPE 'S' DISPLAY LIKE 'E'.
    ENDCASE.
  ENDMETHOD.


  METHOD get_data.


    "@@ Monta lista de funcionarios
    SELECT a~pernr,  "matricula
           b~cpf_nr, "CPF
           c~cname,  "Nome funcionario
           c~gbdat,  "data nascimento
                ( CASE c~gesch
                    WHEN '1' THEN 'M'
                    ELSE 'F' END ) AS sexo, "sexo
           d~stell,   "cargo
           e~stltx,   "denominacao cargo
           d~orgeh,   "Código setor
           f~orgtx,   "Denominacao setor
           ( CASE a~stat2
                WHEN '3' THEN 'A'
                WHEN '0' THEN 'I'
                END ) AS status,
           d~persg,
           d~persk,
           ( CASE g~hilfm
                  WHEN '005' THEN 'C'
                  ELSE 'E' END ) AS natureza_cargo,
           ( CASE h~codemp
                  WHEN '30' THEN 'P'
                  ELSE 'G' END ) AS regime_prev,
            i~ident_nr, "RG funcionario
            j~cbo_code,
            k~pis_nr,     "PIS
            h~cattr,       "Categoria do trabalhador
            ( l~stras && ' ' && l~hsnmr && ' ' &&  l~posta ) AS endereco,
            l~ort01 AS cidade,
            l~state AS uf,
            l~pstlz AS cep,
            m~mostd AS cargahora
      FROM pa0000 AS a
      LEFT JOIN pa0465 AS b
       ON a~pernr = b~pernr
         AND b~subty = '0001'
         AND b~begda <= @sy-datum
         AND b~endda >= @sy-datum
      LEFT JOIN pa0002 AS c
       ON a~pernr = c~pernr
       AND c~begda <= @sy-datum
       AND c~endda >= @sy-datum
      LEFT JOIN pa0001 AS d
       ON a~pernr = d~pernr
       AND d~begda <= @sy-datum
       AND d~endda >= @sy-datum
       AND ( d~persk <> '1C' AND
             d~persk <> '1G' AND
             d~persk <> '1H' AND
             d~persk <> '2I')
       LEFT JOIN t513s AS e
       ON d~stell = e~stell
       AND e~sprsl = 'P'
       AND e~begda <= @sy-datum
       AND e~endda >= @sy-datum
       LEFT JOIN t527x AS f
       ON f~orgeh = d~orgeh
       AND e~sprsl = 'P'
       AND e~begda <= @sy-datum
       AND e~endda >= @sy-datum
       LEFT JOIN hrp1010 AS g
       ON g~objid = d~stell
       AND g~begda <= @sy-datum
       AND g~endda >= @sy-datum
       AND g~subty = '0001'
       LEFT JOIN pa0398 AS h
       ON h~pernr = a~pernr
       AND h~begda <= @sy-datum
       AND h~endda >= @sy-datum
       LEFT JOIN pa0465 AS i
       ON a~pernr = i~pernr
         AND i~subty = '0002'
         AND i~begda <= @sy-datum
         AND i~endda >= @sy-datum
       LEFT JOIN zhrst_efd_e1030 AS j
       ON j~objid = d~stell
        AND j~begda <= @sy-datum
        AND j~endda >= @sy-datum
       LEFT JOIN pa0465 AS k
       ON a~pernr = k~pernr
         AND k~subty = '0006'
         AND k~begda <= @sy-datum
         AND k~endda >= @sy-datum
        LEFT JOIN pa0006 AS l
        ON l~pernr = a~pernr
         AND l~begda <= @sy-datum
         AND l~endda >= @sy-datum
       LEFT JOIN pa0007 AS m
       ON m~pernr = a~pernr
         AND m~begda <= @sy-datum
         AND m~endda >= @sy-datum
      INTO TABLE @DATA(lt_pernr_list)
      WHERE a~pernr IN @me->selopt-pernr AND
            a~endda >= @sy-datum AND
            a~begda <= @sy-datum
    ORDER BY a~pernr.

    IF sy-subrc = 0.


*      CLEAR: gt_pa0000.
*      SELECT pernr , begda , endda, massn
*          FROM pa0000
*          INTO TABLE @gt_pa0000
*          FOR ALL ENTRIES IN @lt_pernr_list
*          WHERE pernr = @lt_pernr_list-pernr.


*   Montar dados de saída
      LOOP AT lt_pernr_list ASSIGNING FIELD-SYMBOL(<list>).

        APPEND INITIAL LINE TO gt_data ASSIGNING FIELD-SYMBOL(<data>).

        "Data atualização
        <data>-dt_atulizacao = |{ sy-datum DATE = USER }|.

        "Matricula funcionario
        <data>-codigo = <list>-pernr.

        "CPF
        <data>-cpf = <list>-cpf_nr.

        "Nome funcionario
        <data>-nome = <list>-cname.

        "Data Nascimento
        <data>-dt_nasci = |{ <list>-gbdat DATE = USER }|.

        "Data admissão / data demissão
        get_datas_funcionario(
          EXPORTING
            iv_pernr    = <list>-pernr
          CHANGING
            cv_admissao = <data>-dt_admissao
            cv_demissao = <data>-dt_demissao  ).

        "Cargo
        <data>-cod_cargo = <list>-stell.

        "Denominação Cargo
        <data>-nome_cargo = <list>-stltx.

        "Setor
        <data>-cod_setor = <list>-orgeh.

        "Denominação Setor
        <data>-nome_setor = <list>-orgtx.

        "Sexo
        <data>-sexo = <list>-sexo.

        "Quantidade Dependetes
        count_dependentes_irf( EXPORTING iv_pernr = <list>-pernr
                               IMPORTING ex_count = <data>-dependentes ).

        "Status
        <data>-status = <list>-status.

        "Regime juridico
        get_regime_juridico(
          EXPORTING
            iv_persg   = <list>-persg
            iv_persk   = <list>-persk
          CHANGING
            cv_reg_jur = <data>-reg_juridico
            cv_obs     = <data>-observacao  ).

        "Natureza do Cargo
        <data>-natureza_cargo = <list>-natureza_cargo.

        "Regime Previdenciario
        <data>-previdencia = <list>-regime_prev.

        "RG funcionario
        <data>-rg = <list>-ident_nr.

        "CBO
        <data>-cbo = <list>-cbo_code.

        "NIT (PIS/PASEP/SI/SUS)
        <data>-nit = <list>-pis_nr.

        "Categoria
        get_categoria(
          EXPORTING
            iv_cattr = <list>-cattr
          CHANGING
            cv_categ = <data>-categoria
            cv_obs   = <data>-observacao ).


        "Endereco
        <data>-endereco = <list>-endereco.

        "cidade
        <data>-cidade = <list>-cidade.

        "UF
        <data>-uf = <list>-uf.

        "UF
        <data>-cep = <list>-cep.

        "Carga Horaria
        <data>-carga_horario = <list>-cargahora.

        "Carga Horaria
        <data>-unidade_horario = 'M'.

      ENDLOOP.

    ELSE.
      RAISE not_found.
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_file.

    SELECT low FROM tvarvc
    INTO @cs_header-cnpj
    UP TO 1 ROWS
    WHERE name = @c_cnpj_empresa AND
          type = 'P'.
    ENDSELECT.

    SELECT low FROM tvarvc
    INTO @cs_header-nome_empresa
    UP TO 1 ROWS
    WHERE name = @c_nome_empresa AND
          type = 'P'.
    ENDSELECT.

    cs_header-data_inicial = |{ sy-datum DATE = USER }|.
    cs_header-data_final = |{ sy-datum DATE = USER }|.
    cs_header-data_geracao = |{ sy-datum DATE = USER }|.
    cs_header-qtd_linhas = lines( gt_data ) + 1 .

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

  METHOD count_dependentes_irf.

    SELECT COUNT( * )
    FROM pa0397
    INTO ex_count
    WHERE pernr = iv_pernr
      AND begda <= sy-datum
      AND endda >= sy-datum
      AND irflg = abap_true.

  ENDMETHOD.

  METHOD get_regime_juridico.

    CONSTANTS: c_c TYPE char1 VALUE 'C',
               c_o TYPE char1 VALUE 'O'.

    IF iv_persg = '1' OR
       iv_persg = '4'.
      cv_reg_jur = c_c.

    ELSEIF iv_persg = '2'.

      IF iv_persk = '2D' OR
         iv_persk = '2E'.
        cv_reg_jur = c_o.
        cv_obs = 'Diretor'.
      ELSEIF iv_persk = '2F' OR
             iv_persk = '2G' OR
             iv_persk = '2I'.
        cv_reg_jur = c_c.
      ENDIF.

    ELSEIF iv_persg = '3'.

      cv_reg_jur = c_o.
      cv_obs = 'Estagiário'.

    ELSEIF iv_persg = '5' AND
           iv_persk = '5B'.

      cv_reg_jur = c_c.

    ENDIF.


  ENDMETHOD.

  METHOD get_categoria.

    DATA: lv_text TYPE string.

    IF iv_cattr = '01'.
      cv_categ = 'E'.
    ELSE.
      cv_categ = 'O'.

      CASE iv_cattr.
        WHEN '03'.
          lv_text = 'Trab. Não v. ao RGPS, mas com dir. FGTS'.

        WHEN '04'.
          lv_text = 'Empreg. Contrato trab. Prazo determinado'.

        WHEN '05'.
          lv_text = 'Diretor não-empregado com FGTS (Empres.)'.

        WHEN '06'.
          lv_text = 'empregado doméstico'.

        WHEN '07'.
          lv_text = 'menor aprendiz (lei 10.097/2000)'.

        WHEN '11'.
          lv_text = 'diretor não-empregado sem fgts (empres.)'.

        WHEN '12'.
          lv_text = 'agente público'.

        WHEN '13'.
          lv_text = 'trab.autônomo/equip. contr. sobre remun.'.

        WHEN '14'.
          lv_text = 'trab.autônomo/equip. contr. sobre sal-base'.

        WHEN '15'.
          lv_text = 'transp.autônomo contr. sobre remuneração'.

        WHEN '16'.
          lv_text = 'transp.autônomo contr. sobre salário-base'.

        WHEN '20'.
          lv_text = 'serv.públ.ocupante, exclusiv.cargo comiss'.

        WHEN '99'.
          lv_text = 'sem vínculo empregatício'.

      ENDCASE.

      IF lv_text IS NOT INITIAL.
        CONCATENATE cv_obs lv_text INTO cv_obs SEPARATED BY space.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_datas_funcionario.

    DATA: lv_data   TYPE datum.


    "Data admissão
    CALL FUNCTION 'RP_GET_HIRE_DATE'
      EXPORTING
        persnr          = iv_pernr
        check_infotypes = '0001'
      IMPORTING
        hiredate        = lv_data.

    cv_admissao = |{ lv_data DATE = USER }|.


    "Data Demissão
    CALL FUNCTION 'RP_GET_FIRE_DATE'
      EXPORTING
        persnr   = iv_pernr
      IMPORTING
        firedate = lv_data.
    cv_demissao = |{ lv_data DATE = USER }|.


  ENDMETHOD.

ENDCLASS.

**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.
SELECT-OPTIONS: s_pernr FOR pa0000-pernr.
PARAMETERS: p_local  TYPE char1 RADIOBUTTON GROUP b1 USER-COMMAND radio DEFAULT 'X',
            p_server TYPE char1 RADIOBUTTON GROUP b1.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-h02.
PARAMETERS: p_file TYPE fileextern MODIF ID r1.
SELECTION-SCREEN END OF BLOCK b2.


AT SELECTION-SCREEN OUTPUT.

  IF p_local = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'R1'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.

    LOOP AT SCREEN.
      IF screen-group1 = 'R1'.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

INITIALIZATION.

  DATA(o_lcl_report) = NEW lcl_report( ).

  GET REFERENCE OF s_pernr[] INTO o_lcl_report->refdata-pernr.

**********************************************************************
START-OF-SELECTION.

  o_lcl_report->main( EXPORTING iv_save_on_server = p_server
                                     iv_filename       = CONV #( p_file )   ).
