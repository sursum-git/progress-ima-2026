/*
  Programa:esbofin490a
  Objetivo: Retornar os titulos a receber com saldo atual que sÆo de especies que atualizam fluxo de caixa.
  O retorno ser  por temp-table
*/

/* variaveis gerais da BO e temp-table*/
{esbo/esbofin490a.i}
DEFINE VARIABLE empresaIni AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE empresaFim AS CHARACTER   NO-UNDO INIT 'zzz'.
DEFINE VARIABLE estabIni AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE estabFim AS CHARACTER   NO-UNDO INIT 'zzz'.
DEFINE VARIABLE dataLimite  AS DATE        NO-UNDO.

PROCEDURE definirEmpresa:
    DEFINE INPUT PARAMETER pEmpresa AS CHARACTER   NO-UNDO.
    ASSIGN empresaIni =  pEmpresa
           empresaFim =  pEmpresa.
END.

PROCEDURE definirEstab:
    DEFINE INPUT PARAMETER pEstabIni  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEstabFim AS CHARACTER   NO-UNDO.
    ASSIGN estabIni =  pEstabIni
           estabFim =  pEstabFim .
END.


PROCEDURE definirDataLimite:
   DEFINE INPUT  PARAMETER pDataLimite AS DATE        NO-UNDO.
   ASSIGN dataLimite = pDataLimite.
END.

PROCEDURE limparDados:
EMPTY TEMP-TABLE ttTitulo.

END.
PROCEDURE buscarTitulos:
    DEFINE VARIABLE lVencido AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iSinal   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE logVal   AS LOGICAL     NO-UNDO.
    OUTPUT TO c:\temp\titulos_sem_val.txt.
    FOR EACH tit_acr NO-LOCK
        WHERE tit_acr.cod_empresa >= empresaIni
        AND tit_acr.cod_empresa <= empresaFim
        AND tit_acr.cod_estab >= estabIni
        AND tit_acr.cod_estab <= estabFim
        AND  tit_acr.log_sdo_tit_acr = YES
        AND tit_acr.dat_vencto_tit_acr <= dataLimite,
        EACH espec_docto_financ_acr OF tit_acr NO-LOCK
        WHERE espec_docto_financ_acr.LOG_atualiz_fluxo_cx = YES.
        ASSIGN logVal = NO.
        FOR EACH val_tit_acr OF tit_acr NO-LOCK:
            ASSIGN logVal = YES.
            /**regra para desconsiderar titulos da ima para med ***/                                                 
            IF tit_acr.cod_empresa = '500' AND tit_acr.cdn_cliente = 1  THEN NEXT.                                   
                                                                                                                     
            /**regra para desconsiderar titulos da med para ima ***/                                                 
            IF tit_acr.cod_empresa = '100' AND tit_acr.cdn_cliente = 10535 THEN NEXT.                                
                                                                                                                     
            ASSIGN lVencido = tit_acr.dat_vencto_tit_acr < TODAY.                                                    
            FIND FIRST ttTitulo                                                                                      
                WHERE ttTitulo.cod_empresa      = tit_acr.cod_empresa                                                
                AND   ttTitulo.cod_estab        = tit_acr.cod_estab                                                  
                AND   ttTitulo.cod_tit_acr      = tit_acr.cod_tit_acr                                                
                AND   ttTitulo.cod_parcela      = tit_acr.cod_parcela                                                
                AND   ttTitulo.cod_ser_docto    = tit_acr.cod_ser_docto                                              
                AND   ttTitulo.cod_espec_docto  = tit_acr.cod_espec_docto                                            
                AND   ttTitulo.cdn_cliente      = tit_acr.cdn_cliente                                                
                NO-LOCK NO-ERROR.                                                                                    
            IF NOT AVAIL ttTitulo THEN DO:                                                                           
               FIND FIRST ems5.espec_docto OF tit_acr NO-LOCK NO-ERROR.                                   
               IF AVAIL espec_docto  THEN DO:                                                                        
                    IF espec_docto.ind_tip_espec_docto = 'Antecipa‡Æo' THEN DO:                                      
                        ASSIGN iSinal = -1.                                                                          
                    END.                                                                                             
                    ELSE DO: 
                        PUT 'especie nÆo encontrada:' tit_acr.cod_espec_docto SKIP.
                        ASSIGN iSinal = 1.                                                                           
                    END.                                                                                             
               END.                                                                                                  
               ELSE DO:                                                                                              
                ASSIGN iSinal = 1.                                                                                   
               END.     
               ASSIGN lVencido = tit_acr.dat_vencto_tit_acr < TODAY.
               CREATE ttTitulo.                                                                                      
               ASSIGN ttTitulo.cod_empresa          = tit_acr.cod_empresa                                            
                      ttTitulo.cod_estab            = tit_acr.cod_estab                                              
                      ttTitulo.cod_tit_acr          = tit_acr.cod_tit_acr                                            
                      ttTitulo.cod_parcela          = tit_acr.cod_parcela                                            
                      ttTitulo.cod_ser_docto        = tit_acr.cod_ser_docto                                          
                      ttTitulo.cod_espec_docto      = tit_acr.cod_espec_docto                                        
                      ttTitulo.cdn_cliente          = tit_acr.cdn_cliente                                            
                      ttTitulo.nom_abrev            = tit_acr.nom_abrev                                              
                      ttTitulo.dat_emis_docto       = tit_acr.dat_emis_docto                                         
                      ttTitulo.dat_vencto_tit_acr   = tit_acr.dat_vencto_tit_acr                                     
                      ttTitulo.dat_fluxo_tit_acr    = tit_acr.dat_fluxo_tit_acr                                      
                      ttTitulo.val_origin_tit_acr   = val_tit_acr.val_origin_tit_acr  * iSinal                           
                      ttTitulo.val_sdo_tit_acr      = val_tit_acr.val_sdo_tit_acr     * iSinal                           
                      ttTitulo.situacao             = lVencido
                      ttTitulo.tipo_fluxo           = val_tit_acr.cod_tip_fluxo_financ .                                                      
            END.
        END.
        IF logVal = NO THEN DO:
           EXPORT DELIMITER "|" tit_acr.cod_empresa tit_acr.cod_estab tit_acr.cod_parcela tit_acr.cod_ser_docto tit_acr.cdn_cliente tit_acr.nom_abrev 
               tit_acr.dat_emis_docto tit_acr.dat_vencto_tit_acr tit_acr.val_origin_tit_acr tit_acr.val_sdo_tit_acr.
        END.
        IF tit_acr.cod_espec_docto = 'CH' THEN DO:

        END.
    END.
    OUTPUT CLOSE.

END PROCEDURE.


PROCEDURE retornarRegistros:
    DEFINE OUTPUT PARAM TABLE FOR ttTitulo.
END.



/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        im
   20 cod_estab                        char        im
   30 cod_espec_docto                  char        im
   40 cod_ser_docto                    char        im
   50 cod_tit_acr                      char        im
   60 cod_parcela                      char        im
   70 cdn_cliente                      inte        im
   80 cdn_clien_matriz                 inte        m
   90 cdn_repres                       inte        i
  100 nom_abrev                        char        im
  110 nom_abrev_contat                 char
  120 num_pessoa                       inte        im
  130 num_fatur_acr                    inte
  140 num_id_movto_tit_acr_ult         inte
  150 num_id_tit_acr                   inte        im
  160 num_id_movto_cta_corren          inte        im
  170 num_bord_acr                     inte        im
  180 num_renegoc_cobr_acr             inte        m
  190 ind_orig_tit_acr                 char        m
  200 ind_sit_tit_acr                  char        m
  210 ind_tip_espec_docto              char        im
  220 ind_sit_bcia_tit_acr             char        m
  230 ind_tip_cobr_acr                 char
  240 ind_ender_cobr                   char        m
  250 dat_transacao                    date        im
  260 dat_emis_docto                   date        im
  270 dat_vencto_tit_acr               date        im
  280 dat_desconto                     date
  290 dat_prev_liquidac                date        im
  300 dat_fluxo_tit_acr                date        im
  310 dat_ult_liquidac_tit_acr         date
  320 dat_ult_apurac_variac_val        date
  330 dat_liquidac_tit_acr             date        im
  340 dat_abat_tit_acr                 date
  350 dat_vencto_origin_tit_acr        date        m
  360 dat_ult_aprop_despes_financ      date        m
  370 dat_alter_portad                 date        m
  380 dat_indcao_perda_dedut           date        im
  390 val_perc_juros_dia_atraso        deci-6      m
  400 val_perc_multa_atraso            deci-2      m
  410 val_perc_desc                    deci-4      m
  420 val_perc_abat_acr                deci-4      m
  430 val_origin_tit_acr               deci-2      m
  440 val_liq_tit_acr                  deci-2      m
  450 val_abat_negocdo                 deci-2
  460 val_desc_negocdo                 deci-2
  470 val_ajust_val_tit_acr            deci-2      m
  480 val_cm_tit_acr                   deci-2      m
  490 val_juros                        deci-2      m
  500 val_multa_tit_acr                deci-2      m
  510 val_desc_tit_acr                 deci-2      m
  520 val_abat_tit_acr                 deci-2      m
  530 val_transf_estab                 deci-2      m
  540 val_saida_subst_nf_dupl          deci-2
  550 val_sdo_tit_acr                  deci-2      m
  560 val_despes_bcia                  deci-2      m
  570 val_despes_financ                deci-2      m
  580 val_impto_operac_financ          deci-2      m
  590 log_tit_acr_cobr_bcia            logi        m
  600 log_dupl_emitid                  logi        m
  610 log_emis_boleto                  logi        m
  620 log_aviso_db_emitid              logi        m
  630 log_recibo_emitid                logi        m
  640 log_npromis_emitid               logi        m
  650 log_tit_acr_estordo              logi        m
  660 log_tit_acr_destndo              logi        m
  670 log_sdo_tit_acr                  logi        im
  680 log_db_autom                     logi        m
  690 log_tip_cr_perda_dedut_tit       logi        m
  700 cod_estab_bord                   char        i
  710 cod_refer                        char        m
  720 cod_grp_clien                    char
  730 cod_portador                     char        im
  740 cod_cart_bcia                    char        m
  750 cod_indic_econ                   char        m
  760 cod_tit_acr_bco                  char        i
  770 cod_cond_cobr                    char        i
  780 cod_cond_pagto                   char
  790 cod_contrat_vda                  char
  800 cod_banco                        char
  810 cod_agenc_bcia                   char
  820 cod_cta_corren                   char
  830 cod_cta_corren_bco               char
  840 cod_digito_cta_corren            char
  850 cod_agenc_cobr_bcia              char
  860 cod_movto_operac_financ          char        im
  870 cod_usuar_indcao_perda           char        m
  880 cod_indic_econ_juros             char        m
  890 qtd_dias_carenc_juros_acr        deci-0      m
  900 qtd_dias_carenc_multa_acr        deci-0      m
  910 qtd_dias_float_cobr              deci-0      m
  920 qtd_dias_atraso_liquidac         deci-4      m
  930 hra_indcao_perda_dedut           char        m
  940 cod_livre_1                      char
  950 des_obs_cobr                     char        m
  960 des_observacao                   char        m
  970 log_integr_cfl_atlzdo            logi        im
  980 cod_instruc_bcia_1_acr           char        m
  990 cod_instruc_bcia_2_acr           char        m
 1000 val_abat_tit_acr_infor           deci-2      m
 1010 log_tit_agrup_especial           logi        m
 1020 ind_tip_calc_juros               char        m
 1030 cod_proces_export                char        i
 1040 val_cr_pis                       deci-2
 1050 val_cr_cofins                    deci-2
 1060 val_cr_csll                      deci-2
 1070 val_base_calc_impto              deci-2
 1080 log_retenc_impto_impl            logi
 1090 cod_livre_2                      char
 1100 dat_livre_1                      date
 1110 dat_livre_2                      date
 1120 log_livre_1                      logi
 1130 log_livre_2                      logi
 1140 num_livre_1                      inte
 1150 num_livre_2                      inte
 1160 val_livre_1                      deci-4
 1170 val_livre_2                      deci-4
 1180 cod_nota_fisc_faturam            char
 1190 cod_parcela_faturam              char
 1200 val_perc_desc_antecip            deci-4

*/
