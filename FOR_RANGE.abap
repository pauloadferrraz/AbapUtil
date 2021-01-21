
  DATA: r_tipo_remessa       TYPE RANGE OF lfart,
  
  r_tipo_remessa = VALUE #(
                            FOR ls_tvarvc IN gt_tvarvc_ind WHERE ( name EQ 'ZSD_TERC_TIPO_REM' )
                            ( sign   = ls_tvarvc-sign
                              option = ls_tvarvc-opti
                              low    = ls_tvarvc-low
                              high   = ls_tvarvc-high ) ).
