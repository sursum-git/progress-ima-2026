/*FOR EACH talon_cheq:
    UPDATE talon_cheq WITH 1 COL WIDTH 550.


   
END.
*/
FOR EACH ems5.cheque
    WHERE num_cheque = 243345:
    FOR EACH cheq_ap OF cheque:
        DELETE cheq_ap.
    END.
    DELETE cheque.
    /*UPDATE cheque WITH 1 COL WIDTH 550.*/

END.


    /* talon cheq
    --- -------------------------------- ----------- -----
   10 cod_cta_corren                   char        im
   20 num_talon_cheq                   inte        im
   30 cod_banco                        char        im
   40 cod_tip_cheq                     char        im
   50 cod_ser_talon_cheq               char
   60 num_inic_talon_cheq              inte        m
   70 num_fim_talon_cheq               inte        m
   80 num_ult_cheq_emitid              inte        m
   90 num_prox_cheq                    inte        m
  100 ind_sit_talon_cheq               char        m
*/


    /*
   cheque
    
   10 cod_empresa                      char        im
   20 cod_tip_cheq                     char        m
   30 cod_modul_dtsul                  char        m
   40 cod_usuar_impres_cheq            char        m
   50 cod_finalid_econ                 char        m
   60 cod_cta_corren                   char        im
   70 num_talon_cheq                   inte        im
   80 num_cheque                       inte        im
   90 num_cop_cheq                     inte        m
  100 num_id_cheq                      inte        im
  110 val_cheque                       deci-2      m
  120 dat_emis_cheq                    date        im
  130 dat_impres_cheq                  date        m
  140 ind_sit_cheq                     char        m
  150 ind_favorec_cheq                 char        m
  160 nom_favorec_cheq                 char        m
  170 nom_cidad_emit_cheq              char        m
  180 log_impres_cheq_sist             logi        m
  190 hra_impres_cheq                  char        m
  200 cod_livre_1                      char
  210 des_text_histor                  char        m
  220 num_id_cheq_valido               inte
  230 cod_livre_2                      char
  240 dat_livre_1                      date
  250 dat_livre_2                      date
  260 log_livre_1                      logi
  270 log_livre_2                      logi
  280 num_livre_1                      inte
  290 num_livre_2                      inte
  300 val_livre_1                      deci-4
  310 val_livre_2                      deci-4

    
    */
