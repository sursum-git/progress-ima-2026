OUTPUT TO c:\temp\cheques.txt.
FOR EACH ems5.cheque
    WHERE cheque.num_cheque = 243489:
    /*DISP cheque WITH 1 COL WIDTH 550.*/
    EXPORT DELIMITER "|" cheque.

    /*FOR EACH relacto_cheq_acr_ap
        WHERE /*relacto_cheq_acr_ap.cod_banco       =  cheque.cod_banco      
        AND   relacto_cheq_acr_ap.cod_agenc_bcia  =  cheque.cod_agenc_bcia 
        AND*/   relacto_cheq_acr_ap.cod_cta_corren  =  cheque.cod_cta_corren 
        AND   relacto_cheq_acr_ap.num_cheque      =  cheque.num_cheque     .
        MESSAGE 'achei'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        DISP  relacto_cheq_acr_ap WITH 1 COL WIDTH 550.
    END.*/
    FOR EACH cheq_ap OF ems5.cheque:
        DISP cheq_ap WITH 1 COL WIDTH 550.
        EXPORT DELIMITER "|" cheq_ap.
        DELETE cheq_ap.
    END.
    DELETE cheque.
END.











/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
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




cheque_ap

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        m
   20 cod_estab_cheq                   char        im
   30 cod_estab_refer                  char        im
   40 cod_portador                     char        im
   50 cod_cart_bcia                    char        im
   60 cod_finalid_econ                 char        im
   70 cod_cta_corren                   char        im
   80 cod_banco                        char        im
   90 cod_agenc_bcia                   char        im
  100 cod_cta_corren_bco               char        im
  110 cod_estab_bord                   char        i
  120 cod_portad_bord                  char        i
  130 cod_usuar_emis_cheq              char        m
  140 cod_usuar_impres_cheq            char
  150 nom_favorec_cheq                 char        im
  160 nom_usuar_termo_respde           char
  170 val_cheque                       deci-2      m
  180 num_seq_cheq                     inte        im
  190 num_talon_cheq                   inte        im
  200 num_cheque                       inte        im
  210 num_cop_cheq                     inte        m
  220 num_id_cheq_ap                   inte        im
  230 num_bord_ap                      inte        i
  240 log_cheq_emitid                  logi        m
  250 log_cop_cheq_emitid              logi        m
  260 log_cheq_cancdo                  logi        m
  270 log_cheq_confdo                  logi        im
  280 log_impres_cheq_sist             logi        m
  290 log_cheq_administ                logi        m
  300 log_bord_ap_escrit               logi        m
  310 hra_impres_cheq                  char
  320 ind_sit_cheq_administ            char        m
  330 ind_localiz_cheq_administ        char
  340 ind_favorec_cheq                 char        m
  350 dat_emis_cheq                    date        im
  360 dat_impres_cheq                  date
  370 dat_confir_cheq_ap               date        m
  380 dat_emis_cheq_administ           date
  390 dat_retir_cheq_administ          date
  400 dat_cancel_cheq_administ         date
  410 dat_termo_respde_cheq            date
  420 log_cheq_parcte_atlzdo           logi        im
  430 cod_livre_1                      char
  440 log_livre_1                      logi
  450 des_text_histor                  char        m
  460 num_id_cheq_valido               inte
  470 cod_livre_2                      char
  480 dat_livre_1                      date
  490 dat_livre_2                      date
  500 log_livre_2                      logi
  510 num_livre_1                      inte
  520 num_livre_2                      inte
  530 val_livre_1                      deci-4
  540 val_livre_2                      deci-4
  550 dat_vencto                       date
  560 cod_workflow                     char
*/
