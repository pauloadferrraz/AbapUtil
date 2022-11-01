*&---------------------------------------------------------------------*
*& Report  YFIORI_CLEAR_CACHE.
*&---------------------------------------------------------------------*
REPORT yfiori_clear_cache.

SELECTION-SCREEN BEGIN OF BLOCK prog WITH FRAME TITLE TEXT-bpr.

  PARAMETERS:
    p_1 AS CHECKBOX DEFAULT abap_true,
    p_2 AS CHECKBOX DEFAULT abap_true,
    p_3 AS CHECKBOX DEFAULT abap_true,
    p_4 AS CHECKBOX DEFAULT abap_true,
    p_5 AS CHECKBOX DEFAULT abap_true.

SELECTION-SCREEN END OF BLOCK prog.

DATA v_uname TYPE syuname.
SELECTION-SCREEN BEGIN OF BLOCK para WITH FRAME TITLE TEXT-bpa.

  SELECT-OPTIONS s_users FOR v_uname.
  PARAMETERS p_allusr AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK para.


INTERFACE lif_program_parameters.

ENDINTERFACE.

CLASS lcl_program_parameters_base DEFINITION.

  PUBLIC SECTION.

    INTERFACES lif_program_parameters.

    TYPES tt_ra_user TYPE RANGE OF syuname.
    TYPES tt_user TYPE SORTED TABLE OF syuname WITH UNIQUE KEY table_line.

    CLASS-METHODS add_all_users.

    CLASS-METHODS add_users
      IMPORTING
        it_ra_user TYPE tt_ra_user.

    CLASS-DATA t_user TYPE tt_user READ-ONLY.
    CLASS-DATA v_is_all_users TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_program_parameters_base IMPLEMENTATION.

  METHOD add_all_users.
    " TODO: show confirmation
    SELECT
      bname
      FROM usr21
      INTO TABLE t_user.

    v_is_all_users = abap_true.
  ENDMETHOD.

  METHOD add_users.
    SELECT
      bname
      FROM usr21
      INTO TABLE t_user
      WHERE
        bname IN it_ra_user.
    v_is_all_users = abap_false.
  ENDMETHOD.

ENDCLASS.

INTERFACE lif_program_launcher.

  METHODS run.

ENDINTERFACE.

CLASS lcl_program_launcher_base DEFINITION ABSTRACT.

  PUBLIC SECTION.
    INTERFACES lif_program_launcher ALL METHODS ABSTRACT.

    METHODS constructor
      IMPORTING
        ir_params TYPE REF TO lif_program_parameters.


  PROTECTED SECTION.
    DATA r_params TYPE REF TO lif_program_parameters.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_program_launcher_base IMPLEMENTATION.
  METHOD constructor.
    me->r_params = ir_params.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_delete_cache DEFINITION INHERITING FROM lcl_program_launcher_base.

  PUBLIC SECTION.
    METHODS lif_program_launcher~run REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_delete_cache IMPLEMENTATION.

  METHOD lif_program_launcher~run.
    LOOP AT CAST lcl_program_parameters_base( me->r_params )->t_user ASSIGNING FIELD-SYMBOL(<ls_user>).
      SUBMIT /ui2/delete_cache
              WITH p_allcli = abap_true
              WITH p_output = abap_true
*        WITH P_URI = space
              WITH p_user = <ls_user>
              EXPORTING LIST TO MEMORY
              AND RETURN.
    ENDLOOP.
*
  ENDMETHOD.

ENDCLASS.

CLASS lcl_delete_cache_after_imp DEFINITION INHERITING FROM lcl_program_launcher_base.

  PUBLIC SECTION.
    METHODS lif_program_launcher~run REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_delete_cache_after_imp IMPLEMENTATION.

  METHOD lif_program_launcher~run.
    SUBMIT /ui2/delete_cache_after_imp
      EXPORTING LIST TO MEMORY
      AND RETURN.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_chip_synchronize_cache DEFINITION INHERITING FROM lcl_program_launcher_base.

  PUBLIC SECTION.
    METHODS lif_program_launcher~run REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_chip_synchronize_cache IMPLEMENTATION.

  METHOD lif_program_launcher~run.
    SUBMIT /ui2/chip_synchronize_cache
      EXPORTING LIST TO MEMORY
      AND RETURN.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_invalidate_client_caches DEFINITION INHERITING FROM lcl_program_launcher_base.

  PUBLIC SECTION.
    METHODS lif_program_launcher~run REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_invalidate_client_caches IMPLEMENTATION.

  METHOD lif_program_launcher~run.

    IF  CAST lcl_program_parameters_base( me->r_params )->v_is_all_users = abap_true.
      SUBMIT /ui2/invalidate_client_caches
          WITH gv_all = abap_true
          WITH gv_user = abap_false
          EXPORTING LIST TO MEMORY
          AND RETURN.
    ELSE.

      LOOP AT CAST lcl_program_parameters_base( me->r_params )->t_user ASSIGNING FIELD-SYMBOL(<ls_user>).
        SUBMIT /ui2/invalidate_client_caches
          WITH gv_all = abap_false
          WITH gv_user = abap_true
          WITH g_uname = <ls_user>
          EXPORTING LIST TO MEMORY
          AND RETURN.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_invalidate_global_caches DEFINITION INHERITING FROM lcl_program_launcher_base.

  PUBLIC SECTION.
    METHODS lif_program_launcher~run REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_invalidate_global_caches IMPLEMENTATION.

  METHOD lif_program_launcher~run.
    SUBMIT /ui2/invalidate_global_caches
        WITH gv_exe = abap_true
        WITH gv_test = abap_false
        EXPORTING LIST TO MEMORY
        AND RETURN.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_program_set_launcher DEFINITION INHERITING FROM lcl_program_launcher_base.

  PUBLIC SECTION.
    TYPES: tt_launcher TYPE SORTED TABLE OF REF TO lif_program_launcher WITH UNIQUE KEY table_line.

    METHODS lif_program_launcher~run REDEFINITION.

    METHODS add_launcher
      IMPORTING
        it_launcher TYPE tt_launcher.

  PRIVATE SECTION.
    DATA t_launcher TYPE tt_launcher.

ENDCLASS.

CLASS lcl_program_set_launcher IMPLEMENTATION.

  METHOD lif_program_launcher~run.
    LOOP AT me->t_launcher INTO DATA(lr_launcher).
      lr_launcher->run( ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_launcher.
    me->t_launcher = it_launcher.
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  s_users = VALUE #( sign = 'I' option = 'EQ' low = sy-uname ).

START-OF-SELECTION.

  DATA r_program_parameters TYPE REF TO lif_program_parameters.
  DATA t_launcher TYPE lcl_program_set_launcher=>tt_launcher.

**********************************************************************
* Common parameters
**********************************************************************

  IF p_allusr = abap_true.
    lcl_program_parameters_base=>add_all_users( ).
  ELSE.
    lcl_program_parameters_base=>add_users( it_ra_user = s_users[] ).
  ENDIF.

  r_program_parameters = NEW lcl_program_parameters_base( ).

  IF p_1 IS NOT INITIAL.
    INSERT NEW lcl_delete_cache( r_program_parameters ) INTO TABLE t_launcher.
  ENDIF.

  IF p_2 IS NOT INITIAL.
    INSERT NEW lcl_delete_cache_after_imp( r_program_parameters ) INTO TABLE t_launcher.
  ENDIF.

  IF p_3 IS NOT INITIAL.
    INSERT NEW lcl_chip_synchronize_cache( r_program_parameters ) INTO TABLE t_launcher.
  ENDIF.

  IF p_4 IS NOT INITIAL.
    INSERT NEW lcl_invalidate_client_caches( r_program_parameters ) INTO TABLE t_launcher.
  ENDIF.

  IF p_5 IS NOT INITIAL.
    INSERT NEW lcl_invalidate_global_caches( r_program_parameters ) INTO TABLE t_launcher.
  ENDIF.

  DATA(r_set_launcher) = NEW lcl_program_set_launcher( r_program_parameters ).

  r_set_launcher->add_launcher( t_launcher ).

  r_set_launcher->lif_program_launcher~run( ).

  MESSAGE 'Execução finalizada' TYPE 'S'.
