FUNCTION zmm_envia_email_requisitante.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_BANFN) TYPE  BANFN OPTIONAL
*"     VALUE(I_BNFPO) TYPE  BNFPO OPTIONAL
*"     VALUE(I_ERNAM) TYPE  ERNAM OPTIONAL
*"----------------------------------------------------------------------

  DATA: gr_send_request  TYPE REF TO cl_bcs,
        gr_bcs_exception TYPE REF TO cx_bcs,
        gr_recipient     TYPE REF TO if_recipient_bcs,
        gr_cc            TYPE REF TO if_recipient_bcs,
        gr_sender        TYPE REF TO if_sender_bcs,
        gr_document      TYPE REF TO cl_document_bcs.

  CONSTANTS:
    gc_subject TYPE bcs_subject VALUE 'Limite de tolerância excedido entre RC e PC',
    gc_raw     TYPE char03 VALUE 'RAW'.


  DATA: lv_string      TYPE char100,
        lv_sent_to_all TYPE os_boolean.

  DATA: lt_mail   TYPE  bcsy_text.


*============= Buscar email do requisitante ===================================================
  DATA wa_user_address    TYPE usaddress.

  CALL FUNCTION 'SUSR_USER_READ'
    EXPORTING
      user_name            = i_ernam
    IMPORTING
      user_address         = wa_user_address
    EXCEPTIONS
      user_name_not_exists = 1
      internal_error       = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  SELECT SINGLE smtp_addr
  FROM adr6
  INTO @DATA(lv_email)
  WHERE addrnumber = @wa_user_address-addrnumber
  AND   persnumber = @wa_user_address-persnumber.

  CHECK sy-subrc = 0.



*================================= Corpo do email ==============================================

  DATA: lv_body TYPE string.

  CONCATENATE '<p>Prezado Requisitante,</p>'
              '<p>N&atilde;o foi poss&iacute;vel converter sua requisi&ccedil;&atilde;o de compra n&ordm;' INTO lv_body.
  CONCATENATE lv_body i_banfn 'e item' i_bnfpo INTO lv_body SEPARATED BY space.
  CONCATENATE lv_body ' em pedido devido o valor exceder a pol&iacute;tica de limite '
  ' toler&acirc;ncia de pre&ccedil;os da &aacute;rea de Suprimentos. Favor verificar a tabela abaixo:</p>'
  '<table border="1" cellspacing="2" cellpadding="1">'
  '<thead><tr><th>Valor</th>'
  '<th>Limite de toler&acirc;ncia entre RC e PC</th>'
  '</tr><tr><td>&le; 500.000,00 USD</td>'
  '<td align="center">10%</td></tr><tr>'
  '<td>&gt; 500.000,00 USD</td>'
  '<td align="center">5%</td></tr></thead></table>'
  '<p><br /><b>O que fazer? </b><br /><br /> Revisar o pre&ccedil;o informado na'
  ' requisi&ccedil;&atilde;o e solicitar novamente a aprova&ccedil;&atilde;o do documento. '
  ' Em caso de dúvidas, favor entrar em contato a &aacute;rea de Suprimentos. </p>' INTO lv_body.


  TRY.
      DATA(msg) = NEW cl_bcs_message( ).
      msg->set_subject( gc_subject  ).

      DATA: v_adr TYPE bcs_address.
      v_adr = lv_email.

      msg->add_recipient(
        EXPORTING
          iv_address      = v_adr         ).

      msg->set_sender(
        EXPORTING
          iv_address      = 'suprimentos.brasil@testes.com'
           iv_visible_name = 'Do not reply '        ).

      msg->set_main_doc(
        EXPORTING
          iv_contents_txt = lv_body
          iv_doctype      = 'htm'    " Document Category
      ).
      msg->set_send_immediately( abap_true ).
      msg->send( ).

    CATCH cx_bcs_send INTO DATA(ex).
      MESSAGE ex->get_text( ) TYPE 'E'.
  ENDTRY.


ENDFUNCTION.
