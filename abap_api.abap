*&---------------------------------------------------------------------*
*& Report ZTEST_ZENLOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztest_zenlog.


    SELECT SINGLE
        b~maktx AS descricao,
        b~maktg AS descricao_reduzida,
        a~meins AS codigo_unidade_medida,
        a~ntgew AS peso_unitario,
        a~hoehe AS altura,
        a~breit AS largura,
        a~laeng AS comprimento,
        a~ean11 AS codigo_barras,
        a~/sapmp/aho AS camada,
        a~mfrnr AS lastro,
        c~steuc AS classificacao_fiscal_ncm,
        d~umrez AS qtd_item_sku,
        t~mseht as descricao_embalagem,
        a~brgew as peso_sku,
        a~maxh as altura_sku,
        a~maxb as largura_sku,
        a~maxl as comprimento_sku
      INTO @DATA(t_teste)
      FROM mara AS a
     INNER JOIN makt AS b
        ON a~matnr = b~matnr
      LEFT JOIN marc AS c
        ON a~matnr = c~matnr
      LEFT JOIN marm AS d
        ON a~matnr = d~matnr
     INNER JOIN t006a AS t
        ON a~meins = t~msehi
       AND b~spras = t~spras
     WHERE b~spras = @sy-langu.

BREAK-POINT.

LEAVE PROGRAM.


DATA: lo_http_client TYPE REF TO if_http_client,
      lo_rest_client TYPE REF TO cl_rest_http_client,
      lv_url         TYPE        string,
      http_status    TYPE        string,
      lv_body        TYPE        string.


DATA lo_teste TYPE REF TO zcl_api_services.


START-OF-SELECTION.

  TRY .

      CREATE OBJECT lo_teste.

      lo_teste->autenticate_pat( ).

    CATCH cx_root INTO DATA(lr_err).

  ENDTRY.

  BREAK-POINT.

  LEAVE PROGRAM.

  cl_http_client=>create_by_url(
    EXPORTING
     url                = 'http://api.zenlog.com.br/api/TokenAuth/AuthenticatePAT' "api/services/app/Estado/GetAll'
*    proxy_host         = 'http://api.zenlog.com.br' " proxy_host
*    proxy_service      = 'api/services/app/Estado/GetAll' " proxy_service
*    ssl_id             = ssl_id
*    sap_username       = sap_username
*    sap_client         = sap_client
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4
  ).

  IF sy-subrc <> 0.
* MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


* If you are using cl_http_client=>create_by_url use this code to supress and pass your
* http basic authenication
*  lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
*  data l_username type string.
*  data l_password type string.
*  l_username = 'user'.
*  l_password = 'password'.
*  call method lo_http_client->authenticate
*    exporting
*      username = l_username
*      password = l_password.

  lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_0 ).
  lo_http_client->request->set_content_type( 'application/json' ).

  CREATE OBJECT lo_rest_client
    EXPORTING
      io_http_client = lo_http_client.

*  IF lo_http_client IS BOUND AND lo_rest_client IS BOUND.
*    lv_url = '/api/services/app/Produto/Create'.
*    cl_http_utility=>set_request_uri(
*      EXPORTING
*        request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
*        uri     = lv_url                     " URI String (in the Form of /path?query-string)
*    ).


** ABAP to JSON
*    TYPES: BEGIN OF ty_json_req,
*             salesorder TYPE string,
*             type       TYPE string,
*           END OF ty_json_req.
*
*    DATA: json_req TYPE ty_json_req.
*    json_req-salesorder = '25252525'.
*    json_req-type = 'Direct'.
*
*    DATA lr_json_serializer   TYPE REF TO cl_trex_json_serializer.
*
*    CREATE OBJECT lr_json_serializer EXPORTING data = json_req.
*    lr_json_serializer->serialize( ).
*    lv_body = lr_json_serializer->get_data( ).


* Or you can use this class as well
* lv_body = /ui2/cl_json=>serialize( data = json_req ).
* Converted JSON should look like this
* lv_body = '{ "salesorder":"25252525", "type":"Direct"}'.
  DATA: lo_json        TYPE REF TO cl_clb_parse_json,
        lo_response    TYPE REF TO if_rest_entity,
        lo_request     TYPE REF TO if_rest_entity,
        lo_sql         TYPE REF TO cx_sy_open_sql_db,
        status         TYPE  string,
        reason         TYPE  string,
        response       TYPE  string,
        content_length TYPE  string,
        location       TYPE  string,
        content_type   TYPE  string,
        lv_status      TYPE  i.

* Set Payload or body ( JSON or XML)
  lo_request = lo_rest_client->if_rest_client~create_request_entity( ).
  lo_request->set_string_data( iv_data = '{ "personalAccessToken": "TJz1rG9DAzEsb15f/iKdXkLwU85QbH19ZiAu7lly3Dcv+5FEmBys", "tenantName": "apoioqas" }' ).

  lo_rest_client->if_rest_resource~post( lo_request ).
  lo_response = lo_rest_client->if_rest_client~get_response_entity( ).
  http_status = lv_status = lo_response->get_header_field( '~status_code' ).
  reason = lo_response->get_header_field( '~status_reason' ).
  content_length = lo_response->get_header_field( 'content-length' ).
  location = lo_response->get_header_field( 'location' ).
  content_type = lo_response->get_header_field( 'content-type' ).
  response = lo_response->get_string_data( ).

  BREAK-POINT.

**  lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
**  lo_request->set_string_data( lv_body ).

**  CALL METHOD lo_rest_client->if_rest_client~set_request_header
**    EXPORTING
**      iv_name  = 'auth-token'
**      iv_value = token number. "Set your header .

* POST
  lo_rest_client->if_rest_resource~post( lo_request )."( lo_request ).

* Collect response
  lo_response = lo_rest_client->if_rest_client~get_response_entity( ).
  http_status = lv_status = lo_response->get_header_field( '~status_code' ).
  reason = lo_response->get_header_field( '~status_reason' ).
  content_length = lo_response->get_header_field( 'content-length' ).
  location = lo_response->get_header_field( 'location' ).
  content_type = lo_response->get_header_field( 'content-type' ).
  response = lo_response->get_string_data( ).

  BREAK-POINT.

* JSON to ABAP
*    DATA lr_json_deserializer TYPE REF TO cl_trex_json_deserializer.
*    TYPES: BEGIN OF ty_json_res,
*             token TYPE string,
*           END OF ty_json_res.
*    DATA: json_res TYPE ty_json_res.
*
*    CREATE OBJECT lr_json_deserializer.
*    lr_json_deserializer->deserialize( EXPORTING json = response IMPORTING abap = json_res ).

*  ENDIF.
