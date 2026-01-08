/*FOR EACH movto_calcul_normal:
    DISP movto_calcul_normal WITH 1 COL WIDTH 550.
END.*/

/*FOR EACH movto_calcul_func:
    DISP movto_calcul_func WITH 1 COL WIDTH 550 NO-ERROR.
END.*/
DEFINE TEMP-TABLE tt
    FIELD ano           AS INT
    FIELD mes           AS INT
    FIELD cdn_func      AS INT
    FIELD nome          AS CHAR FORMAT 'x(150)'
    FIELD cdn_event     LIKE EVENT_fp.cdn_event_fp
    FIELD des_event     LIKE EVENT_fp.des_event_fp
    FIELD tipo_event    AS CHAR FORMAT 'x(15)'
    FIELD qtEvento      AS DECIMAL
    FIELD vlEvento      AS DECIMAL
    FIELD vlBaseEvento  AS DECIMAL.

    
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
FOR EACH movto_calcul_func
     WHERE movto_calcul_func.num_ano_refer_fp >= 2017
     USE-INDEX mvtclclf_hbltclcf:
    bloco:
    REPEAT iCont = 1 TO 30:
        CREATE tt.
        ASSIGN tt.ano       =  movto_calcul_func.num_ano_refer_fp
               tt.mes       = num_mes_refer_fp
               tt.qtEvento  = movto_calcul_func.qtd_unid_event_fp[iCont] 
               tt.VlEvento  = movto_calcul_func.val_calcul_efp[iCont]
               tt.cdn_func  = movto_calcul_func.cdn_funcionario
               tt.cdn_event = movto_calcul_func.cdn_event_fp[iCont]
               tt.vlBaseEvento =  movto_calcul_func.val_base_calc_fp[iCont]  .
    END.
END.
OUTPUT TO c:\temp\eventos_utilizados.txt.
FOR EACH tt:
    FIND FIRST EVENT_fp
        WHERE EVENT_fp.cdn_event_fp = tt.cdn_event
        NO-LOCK NO-ERROR.
    FIND FIRST funcionario 
        WHERE funcionario.cdn_funcionario = tt.cdn_func NO-LOCK NO-ERROR.
    FIND FIRST rh_pessoa_fisic OF funcionario NO-LOCK NO-ERROR.
    IF AVAIL rh_pessoa_fisic THEN DO:
       ASSIGN tt.nome = rh_pessoa_fisic.nom_pessoa_fisic.
    END.
    IF AVAIL EVENT_fp THEN
       ASSIGN tt.des_event = EVENT_fp.des_event_fp
              tt.tipo_event = {database/inpy/i03py067.i 4 EVENT_fp.idi_ident_efp } .
    IF tt.tipo_event = 'desconto' THEN
       ASSIGN tt.VlEvento = tt.VlEvento * -1
              tt.qtEvento = tt.qtEvento * -1.
    EXPORT DELIMITER "|"  tt.
END.

/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cdn_empresa                      char        im
   20 cdn_estab                        char        im
   30 cdn_funcionario                  inte        im
   40 num_ano_refer_fp                 inte        im
   50 num_mes_refer_fp                 inte        im
   60 idi_tip_fp                       inte        im
   70 cod_rh_ccusto                    char        im
   80 qti_parc_habilit_calc_fp         inte        im
   90 num_seq_movto_calcul_fp          inte        im
  100 log_existe_prox_seq              logi        m
  110 dat_inic_parc_calcula_fp         date        m
  120 dat_term_parc_calcula            date        m
  130 dat_pagto_salario                date        m
  140 val_salario_hora                 deci-4
  150 log_cta_mes_13o                  logi        m
  160 qti_efp                          inte        m
  170 cdn_event_fp                     char[30]    m
  180 cdn_idx_efp_funcao_espcif        inte[30]    m
  190 qtd_unid_event_fp                deci-3[30]
  200 qtd_hrs_demonst_efp              deci-3[30]  m
  210 val_base_calc_fp                 deci-2[30]  m
  220 val_calcul_efp                   deci-2[30]  m
  230 idi_sit_fasb_cmcac               inte
  240 log_integr_liq_bco_fp            logi        m
  250 cod_usuar_ult_atualiz            char
  260 dat_ult_atualiz                  date
  270 hra_ult_atualiz                  char        m
  280 cod_livre_1                      char
  290 cod_livre_2                      char
  300 dat_livre_1                      date
  310 dat_livre_2                      date
  320 log_livre_1                      logi
  330 log_livre_2                      logi
  340 num_livre_1                      inte
  350 num_livre_2                      inte
  360 val_livre_1                      deci-4
  370 val_livre_2                      deci-4
  380 val_calc_func_praz_indterm       deci-2      m
  390 num_seq_calc_geral               inte        m
  400 val_salario_atual                deci-4      m
  410 log_consid_calc_folha_compl      logi        m
  420 cdn_categ_sal                    inte        im
  430 log_ppr_desligto_pago            logi        m
  440 cdn_tip_calc_rescis              inte        m
  450 cod_empres_event                 char[30]    m
  460 num_seq_alter_event              inte[30]    m
  470 idi_orig_event                   inte[30]    m
  480 log_recolhe_fgts                 logi        m
  490 log_recolhe_inss                 logi        m
  500 log_func_sindlz                  logi        m
  510 log_recolhe_irf                  logi
  520 log_consid_rais                  logi        m
  530 log_consid_calc_ppr              logi        m
  540 log_consid_carg_turno_trab       logi        m
  550 log_recebe_ferias                logi        m
  560 log_contrat_desativ              logi        m
  570 log_consid_movto_agric           logi
  580 log_recebe_adiant_normal         logi        m
  590 val_perc_adiant                  deci-2      m
  600 log_recebe_13o_salario           logi        m
  610 log_recebe_pericul               logi        m
  620 num_niv_pericul                  inte        m
  630 log_recebe_insal                 logi        m
  640 num_niv_insal                    inte        m
  650 log_descta_contrib_sindic        logi        m
  660 log_contrib_sindic_em_dia        logi
  670 log_descta_revers_sindic         logi
  680 qti_depend_irf                   inte        m
  690 qti_depend_salfam                inte        m
  700 dat_vencto_salfam                date
  710 qti_dom_perdido_mes_seguinte     inte
  720 val_compcao_mes                  deci-5
  730 idi_tip_vinc_empregat            inte        m
  740 idi_multiplo_vinc_inss_func      inte        m
  750 log_consid_calc_ptoelet          logi        m
  760 log_comis_rais                   logi        m
  770 num_seq_movto                    inte        im
*/
