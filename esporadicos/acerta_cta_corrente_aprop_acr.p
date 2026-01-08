DEFINE VARIABLE d AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c AS DECIMAL     NO-UNDO.
ASSIGN d = 0
       c = 0.
OUTPUT TO c:\temp\acerta_movto_acr_cmg_ctbl.txt.
FOR EACH movto_tit_acr NO-LOCK
    WHERE dat_transacao >= 12/01/2014
    AND   dat_transacao <= 12/31/2014
    AND  cod_empresa = '500':
    
    FOR EACH aprop_ctbl_acr OF movto_tit_acr NO-LOCK
        WHERE cod_cta_ctbl = '11102341'.
        FIND FIRST movto_cta_corren OF movto_tit_acr
            WHERE cod_cta_corren = '71537-6' NO-LOCK NO-ERROR.
        IF AVAIL movto_cta_corren THEN DO:
            IF ind_natur_lancto_ctbl = 'db' THEN
               ASSIGN d = d  + val_movto_tit_acr.
            ELSE
               ASSIGN c = c + val_movto_tit_acr.

         DISP aprop_ctbl_acr WITH 1 COL WIDTH 550.
         ASSIGN aprop_ctbl_acr.cod_cta_ctbl = '21103341'.
        END.                                         
    END.
END.       

   
DISP d c.

OUTPUT CLOSE.
/* 
   MOVIMENTO CONTAS A RECEBER
   
      10 cod_empresa                      char        m
   20 cod_estab                        char        im
   30 cod_refer                        char        im
   40 cod_espec_docto                  char        im
   50 cod_estab_tit_acr_pai            char        i
   60 cod_portador                     char        m
   70 cod_cart_bcia                    char        m
   80 cod_motiv_movto_tit_acr          char        i
   90 cod_finalid_econ_motiv           char
  100 cod_usuario                      char        m
  110 cod_usuar_gerac_ctbz             char
  120 cod_autoriz_bco                  char
  130 cod_instruc_bcia_1_movto         char
  140 cod_instruc_bcia_2_movto         char
  150 cod_estab_enctro_cta             char
  160 dat_transacao                    date        im
  170 dat_cr_movto_tit_acr             date
  180 dat_vencto_tit_acr               date
  190 dat_vencto_ant_tit_acr           date
  200 dat_apurac_variac_val_ant        date
  210 dat_gerac_ctbz                   date
  220 dat_gerac_movto                  date        im
  230 dat_liquidac_tit_acr             date
  240 dat_fluxo_cx_movto               date        im
  250 val_movto_tit_acr                deci-2      m
  260 val_movto_fluxo_cx_bco           deci-2      m
  270 val_desconto                     deci-2      m
  280 val_abat_tit_acr                 deci-2      m
  290 val_juros                        deci-2      m
  300 val_multa_tit_acr                deci-2      m
  310 val_despes_bcia                  deci-2
  320 val_cm_tit_acr                   deci-2      m
  330 val_impto_operac_financ          deci-2      m
  340 val_motiv_movto_acr              deci-2
  350 val_despes_financ                deci-2      m
  360 num_id_movto_tit_acr             inte        im
  370 num_id_movto_tit_acr_pai         inte        i
  380 num_id_movto_cta_corren          inte        im
  390 num_id_tit_acr                   inte        im
  400 num_id_movto_bxa                 inte
  410 num_fatur_acr                    inte
  420 num_renegoc_cobr_acr             inte        m
  430 num_id_enctro_cta                inte
  440 log_ctbz_aprop_ctbl              logi        im
  450 log_aprop_ctbl_ctbzda            logi        im
  460 log_movto_estordo                logi        im
  470 log_ndebito_gerad                logi        m
  480 log_antecip_gerad                logi        m
  490 log_liquidac_contra_antecip      logi        m
  500 log_movto_comis_estordo          logi        m
  510 log_movto_envdo_bco              logi        m
  520 cdn_cliente                      inte        im
  530 cdn_clien_fatur                  inte
  540 ind_trans_acr                    char        m
  550 ind_motiv_acerto_val             char
  560 ind_trans_acr_abrev              char        i
  570 hra_gerac_movto                  char        im
  580 hra_gerac_ctbz                   char
  590 cod_livre_1                      char
  600 log_integr_cfl_atlzdo            logi        im
  610 val_sdo_tit_acr                  deci-2      m
  620 val_multa_tit_acr_calc           deci-2      m
  630 val_juros_calc                   deci-2      m
  640 log_recuper_perda                logi
  650 ind_orig_alter_portad            char
  660 cod_contrat_cambio               char
  670 dat_contrat_cambio_export        date
  680 num_contrat_id_cambio            inte
  690 cod_estab_contrat_cambio         char
  700 cod_refer_contrat_cambio         char
  710 dat_refer_contrat_cambio         date
  720 cod_estab_reembol                char
  730 cod_estab_proces_bxa             char
  740 dat_vincul_contrat_cambio        date
  750 log_retenc_impto_liq             logi        m
  760 val_retenc_pis                   deci-2      m
  770 val_retenc_cofins                deci-2      m
  780 val_retenc_csll                  deci-2      m
  790 cod_livre_2                      char
  800 dat_livre_1                      date
  810 dat_livre_2                      date
  820 log_livre_1                      logi
  830 log_livre_2                      logi
  840 num_livre_1                      inte
  850 num_livre_2                      inte
  860 val_livre_1                      deci-4
  870 val_livre_2                      deci-4
  880 val_iva_retid                    deci-2      m
  890 log_import_tit_sdo               logi







   APROPIACAO ACR


  Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        m
   20 cod_estab                        char        im
   30 cod_estab_aprop_ctbl             char        im
   40 cod_plano_cta_ctbl               char        im
   50 cod_cta_ctbl                     char        im
   60 cod_unid_negoc                   char        im
   70 cod_plano_ccusto                 char        im
   80 cod_ccusto                       char        im
   90 cod_indic_econ                   char        m
  100 ind_natur_lancto_ctbl            char        im
  110 ind_tip_aprop_ctbl               char        m
  120 ind_gera_val_aprop_ctbl_acr      char        m
  130 val_aprop_ctbl                   deci-2      m
  140 num_id_movto_tit_acr             inte        im
  150 num_id_aprop_ctbl_acr            inte        im
  160 dat_transacao                    date        im
  170 log_ctbz_aprop_ctbl              logi        m
  180 log_impto_val_agreg              logi        m
  190 log_aprop_ctbl_ctbzda            logi        im
  200 cod_livre_1                      char
  210 cod_livre_2                      char
  220 dat_livre_1                      date
  230 dat_livre_2                      date
  240 log_livre_1                      logi
  250 log_livre_2                      logi
  260 num_livre_1                      inte
  270 num_livre_2                      inte
  280 val_livre_1                      deci-4
  290 val_livre_2                      deci-4 










   
   MOVTO CONTA CORRENTE
    
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
