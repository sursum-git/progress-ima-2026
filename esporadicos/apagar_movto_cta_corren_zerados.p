FOR EACH movto_cta_corren
   /*WHERE  cod_cta_corren = '602170-0'
   AND dat_transacao = 12.30.2015
   AND num_seq_movto_cta_corren = 110*/ 
    WHERE  movto_cta_corren.val_movto_cta_corren  = 0 :
   DISP  dat_transacao dat_movto_cta_corren cod_tip_trans_cx  WITH 1 COL WIDTH 550.
   DELETE movto_cta_corren.
END.

/*
   10 cod_cta_corren                   char        im
   20 dat_movto_cta_corren             date        im
   30 num_seq_movto_cta_corren         inte        im
   40 cod_cenar_ctbl                   char        m
   50 cod_indic_econ                   char        m
   60 cod_tip_trans_cx                 char        im
   70 cod_histor_padr                  char        m
   80 cod_docto_movto_cta_bco          char        im
   90 cod_usuar_ult_atualiz            char        m
  100 cod_usuar_emis_aviso_lancto      char        m
  110 cod_modul_dtsul                  char        m
  120 dat_transacao                    date        m
  130 dat_emis_aviso_lancto            date        m
  140 dat_ult_atualiz                  date
  150 des_histor_movto_cta_corren      char        im
  160 des_histor_movto_concil          char
  170 ind_fluxo_movto_cta_corren       char        m
  180 ind_tip_movto_cta_corren         char        im
  190 ind_orig_impto_movto_financ      char        m
  200 ind_sit_concil_movto_cta         char        m
  210 ind_ligac_concil_cta_corren      char        m
  220 ind_emis_docto_transf_bcia       char        m
  230 val_movto_cta_corren             deci-2      m
  240 val_impto_movto_cta_corren       deci-2      m
  250 val_pend_concil_cta_corren       deci-2      m
  260 log_movto_cta_corren_autom       logi        m
  270 log_concil_cta_corren            logi        im
  280 log_ctbz_movto_cta_corren        logi        im
  290 num_id_movto_cta_transf          inte        im
  300 num_id_movto_cta_corren          inte        im
  310 num_id_movto_cta_impto           inte        im
  320 num_id_movto_orig_mutuo          inte        im
  330 num_id_cheq                      inte        im
  340 num_id_movto_cta_estorn          inte        im
  350 num_calc_encargo_cta_corren      inte        im
  360 num_aviso_lancto_cta_corren      inte        m
  370 hra_emis_aviso_lancto            char        m
  380 hra_ult_atualiz                  char        m
  390 cod_livre_1                      char
  400 num_id_movto_despes_bcia         inte        m
  410 dat_prev_orig                    date
  420 cod_estab_bord                   char        m
  430 log_sdo_bco_histor               logi        m
  440 cod_usuar_gerac_ctbz             char        m
  450 dat_gerac_ctbz                   date        m
  460 hra_gerac_ctbz                   char        m
  470 cod_livre_2                      char
  480 dat_livre_1                      date
  490 dat_livre_2                      date
  500 log_livre_1                      logi
  510 log_livre_2                      logi
  520 num_livre_1                      inte
  530 num_livre_2                      inte
  540 val_livre_1                      deci-4
  550 val_livre_2                      deci-4
*/
