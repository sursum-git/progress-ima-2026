/*FOR EACH cta_mdo_efp:
    IF cod_rh_cta_ctbl_db = '410314' AND cod_rh_ccusto_db = '' THEN DO:
       ASSIGN  cod_rh_cta_ctbl_db = '19000001'
               cod_rh_ccusto_db =  'xxxxxxxx'.
    END.
END.
*/

FOR EACH cta_mdo_efp:
    IF cod_rh_cta_ctbl_db = '410314'  THEN DO:
       ASSIGN  cod_rh_cta_ctbl_db = '41301001'.
    END.
END.

/*
  10  cdn_empresa                      char        im
   20 cdn_estab                        char        im
   30 cdn_event_fp                     char        im
   40 cod_tip_mdo                      char        im
   50 cod_rh_cta_ctbl_db               char
   60 cod_rh_ccusto_db                 char
   70 cod_rh_cta_ctbl_cr               char
   80 cod_rh_ccusto_cr                 char
   90 cod_cta_db_variac                char
  100 cod_rh_ccusto_db_variac          char
  110 cod_cta_cr_variac                char
  120 cod_rh_ccusto_cr_variac          char
  130 cod_usuar_ult_atualiz            char
  140 dat_ult_atualiz                  date
  150 hra_ult_atualiz                  char        m
  160 cod_livre_1                      char
  170 cod_livre_2                      char
  180 dat_livre_1                      date
  190 dat_livre_2                      date
  200 log_livre_1                      logi
  210 log_livre_2                      logi
  220 num_livre_1                      inte
  230 num_livre_2                      inte
  240 val_livre_1                      deci-4
  250 val_livre_2                      deci-4

*/



