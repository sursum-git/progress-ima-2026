
FOR EACH cheq_acr NO-LOCK
    WHERE cod_banco = '001'
    AND   cod_agenc_bcia = '1089'
    AND   cod_cta_corren_bco = '21454'
    AND   num_cheque = 850130:
    DISP cheq_acr WITH 1 COL WIDTH 550.

END.

/*

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_banco                        char        im
   20 cod_agenc_bcia                   char        im
   30 cod_cta_corren_bco               char        im
   40 cod_empresa                      char        im
   50 cod_estab                        char        im
   60 cod_id_feder                     char
   70 cod_motiv_devol_cheq             char
   80 cod_indic_econ                   char        m
   90 cod_usuar_cheq_acr_terc          char        m
  100 cod_espec_docto_cheq_acr         char        m
  110 cod_portador                     char        m
  120 cod_pais                         char        m
  130 dat_transacao                    date        im
  140 dat_emis_cheq                    date        m
  150 dat_apres_cheq_acr               date
  160 dat_cr_cheq_acr                  date
  170 dat_renegoc_cheq_acr             date
  180 dat_prev_apres_cheq_acr          date        m
  190 dat_prev_cr_cheq_acr             date
  200 val_cheque                       deci-2      m
  210 val_tot_vincul_cheq_acr          deci-2      m
  220 nom_emit                         char
  230 nom_cidad_emit                   char
  240 log_pend_cheq_acr                logi        m
  250 log_cheq_terc                    logi        m
  260 log_cheq_acr_renegoc             logi        m
  270 log_cheq_acr_devolv              logi        m
  280 num_cheque                       inte        im
  290 num_agrup_cheq_acr               inte        i
  300 num_pessoa                       inte        m
  310 num_id_cheq_acr                  inte        im
  320 num_id_movto_cta_corren          inte
  330 num_id_tit_acr                   inte        i
  340 ind_orig_cheq_acr                char        m
  350 ind_sit_cheq_acr                 char        m
  360 ind_dest_cheq_acr                char        m
  370 cdn_cliente                      inte        im
  380 cod_livre_1                      char
  390 log_livre_1                      logi
  400 dat_livre_1                      date
  410 cod_livre_2                      char
  420 dat_livre_2                      date
  430 log_livre_2                      logi
  440 num_livre_1                      inte
  450 num_livre_2                      inte
  460 val_livre_1                      deci-4
  470 val_livre_2                      deci-4
*/
