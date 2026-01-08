DEFINE TEMP-TABLE tt-erros-integr NO-UNDO
    FIELDS nr-linha AS INTEGER    /*** Numero da Linha ***/
    FIELDS msg-erro AS CHARACTER  /*** Mensagem indicano o error da linha (existir erro) ***/ 
    FIELDS chave    AS CHARACTER  /*** Chave unica do registro, se chave composta separar valores por | ***/
    FIELDS cod-erro AS INTEGER.   /*** C¢digo do erro retornado pelo Datasul ***/

/**** Definiá∆o de temp-tables ****************************************/
{prgfin/acr/acr900zi.i}

DEF VAR h_hdl_programa AS HANDLE.
DEF VAR c_cod_matriz_trad_org_ext AS CHAR.
DEF VAR l_log_atualiza_refer_acr  AS LOG  NO-UNDO.
DEF VAR l_log_assume_dat_emis     AS LOG  NO-UNDO.
DEF VAR i-seq AS INT.

DEF TEMP-TABLE tt_tit_acr LIKE tit_acr.
DEF TEMP-TABLE tt_aprop_ctbl_acr LIKE aprop_ctbl_acr.

DEF INPUT PARAMETER TABLE FOR tt_tit_acr.
DEF INPUT PARAMETER TABLE FOR tt_aprop_ctbl_acr.

ASSIGN l_log_assume_dat_emis     = NO
       c_cod_matriz_trad_org_ext = "EMS".
       l_log_atualiza_refer_acr  = YES. /* Atualiza lote sim ou n∆o */

FOR EACH tt_tit_acr BREAK BY tt_tit_acr.cod_estab
                          BY tt_tit_acr.cod_refer.

    FIND tit_acr WHERE
         tit_acr.cod_estab        = tt_tit_acr.cod_estab      AND
         tit_acr.cod_espec_docto  = tt_tit_acr.cod_espec_docto AND
         tit_acr.cod_ser_docto    = tt_tit_acr.cod_ser_docto AND 
         tit_acr.cod_tit_acr      = tt_tit_acr.cod_tit_acr AND   
         tit_acr.cod_parcela      = tt_tit_acr.cod_parcela 
         NO-LOCK NO-ERROR.
    IF AVAIL tit_acr THEN NEXT.

    IF FIRST-OF(tt_tit_acr.cod_refer) THEN DO.
       EMPTY TEMP-TABLE tt_integr_acr_lote_impl.
       EMPTY TEMP-TABLE tt_integr_acr_item_lote_impl_8.
       EMPTY TEMP-TABLE tt_integr_acr_aprop_ctbl_pend.

       ASSIGN i-seq = 0.

       FIND tt_integr_acr_lote_impl WHERE
            tt_integr_acr_lote_impl.tta_cod_estab   = tt_tit_acr.cod_estab AND
            tt_integr_acr_lote_impl.tta_cod_refer   = tt_tit_acr.cod_refer
            NO-ERROR.
       IF NOT AVAIL tt_integr_acr_lote_impl THEN DO.
          CREATE tt_integr_acr_lote_impl.

          ASSIGN tt_integr_acr_lote_impl.tta_cod_empresa = tt_tit_acr.cod_empresa
                 tt_integr_acr_lote_impl.tta_cod_estab   = tt_tit_acr.cod_estab
                 tt_integr_acr_lote_impl.tta_cod_refer   = tt_tit_acr.cod_refer.
       END.

       ASSIGN tt_integr_acr_lote_impl.tta_cod_indic_econ             = tt_tit_acr.cod_indic_econ
              tt_integr_acr_lote_impl.tta_dat_transacao              = tt_tit_acr.dat_transacao
              tt_integr_acr_lote_impl.tta_ind_orig_tit_acr           = tt_tit_acr.ind_orig_tit_acr
              tt_integr_acr_lote_impl.tta_val_tot_lote_impl_tit_acr  = tt_tit_acr.val_origin_tit_acr
              tt_integr_acr_lote_impl.tta_val_tot_lote_infor_tit_acr = tt_tit_acr.val_origin_tit_acr
              tt_integr_acr_lote_impl.tta_ind_tip_cobr_acr           = "Normal"
              tt_integr_acr_lote_impl.tta_log_liquidac_autom         = NO.
    END.

    FIND tt_integr_acr_lote_impl WHERE
         tt_integr_acr_lote_impl.tta_cod_estab   = tt_tit_acr.cod_estab AND
         tt_integr_acr_lote_impl.tta_cod_refer   = tt_tit_acr.cod_refer
         NO-ERROR.

    ASSIGN i-seq = i-seq + 1.

    CREATE tt_integr_acr_item_lote_impl_8.
    ASSIGN tt_integr_acr_item_lote_impl_8.ttv_rec_lote_impl_tit_acr      = RECID(tt_integr_acr_lote_impl)
           tt_integr_acr_item_lote_impl_8.tta_num_seq_refer              = i-seq 
           tt_integr_acr_item_lote_impl_8.tta_cdn_cliente                = tt_tit_acr.cdn_cliente              
           tt_integr_acr_item_lote_impl_8.tta_cod_espec_docto            = tt_tit_acr.cod_espec_docto          
           tt_integr_acr_item_lote_impl_8.tta_cod_ser_docto              = tt_tit_acr.cod_ser_docto
           tt_integr_acr_item_lote_impl_8.tta_cod_tit_acr                = tt_tit_acr.cod_tit_acr
           tt_integr_acr_item_lote_impl_8.tta_cod_parcela                = tt_tit_acr.cod_parcela              
           tt_integr_acr_item_lote_impl_8.tta_cod_indic_econ             = tt_tit_acr.cod_indic_econ           
           tt_integr_acr_item_lote_impl_8.tta_cod_portador               = tt_tit_acr.cod_portador             
           tt_integr_acr_item_lote_impl_8.tta_cod_cart_bcia              = tt_tit_acr.cod_cart_bcia            
           tt_integr_acr_item_lote_impl_8.tta_cdn_repres                 = tt_tit_acr.cdn_repres
           tt_integr_acr_item_lote_impl_8.tta_dat_vencto_tit_acr         = tt_tit_acr.dat_vencto_tit_acr
           tt_integr_acr_item_lote_impl_8.tta_dat_prev_liquidac          = tt_tit_acr.dat_prev_liquidac 
           tt_integr_acr_item_lote_impl_8.tta_dat_emis_docto             = tt_tit_acr.dat_emis_docto
           tt_integr_acr_item_lote_impl_8.tta_val_tit_acr                = tt_tit_acr.val_origin_tit_acr
           tt_integr_acr_item_lote_impl_8.tta_val_perc_juros_dia_atraso  = tt_tit_acr.val_perc_juros_dia_atraso
           tt_integr_acr_item_lote_impl_8.tta_val_perc_multa_atraso      = tt_tit_acr.val_perc_multa_atraso
           tt_integr_acr_item_lote_impl_8.tta_val_liq_tit_acr            = tt_tit_acr.val_liq_tit_acr
           tt_integr_acr_item_lote_impl_8.tta_ind_tip_espec_docto        = tt_tit_acr.ind_tip_espec_docto
           tt_integr_acr_item_lote_impl_8.tta_cod_refer                  = tt_integr_acr_lote_impl.tta_cod_refer
           tt_integr_acr_item_lote_impl_8.tta_cod_motiv_movto_tit_acr    = '01'.

    ASSIGN tt_integr_acr_item_lote_impl_8.ttv_rec_item_lote_impl_tit_acr = RECID(tt_integr_acr_item_lote_impl_8).

    IF LAST-OF(tt_tit_acr.cod_refer) THEN DO.

       RUN prgfin/acr/acr900zi.py persistent set h_hdl_programa.
       RUN pi_main_code_integr_acr_new_11 IN h_hdl_programa (INPUT 11,
                                                             INPUT c_cod_matriz_trad_org_ext,
                                                             INPUT l_log_atualiza_refer_acr,
                                                             INPUT l_log_assume_dat_emis,
                                                             INPUT TABLE tt_integr_acr_repres_comis_2, /*N∆o est† sendo preenchida*/
                                                             INPUT-OUTPUT TABLE tt_integr_acr_item_lote_impl_8, /*Est† sendo preenchida*/
                                                             INPUT TABLE tt_integr_acr_aprop_relacto_2,
                                                             INPUT-OUTPUT TABLE tt_params_generic_api,
                                                             INPUT TABLE tt_integr_acr_relacto_pend_aux).
                                                             
       DELETE PROCEDURE h_hdl_programa.

       FOR EACH tt_log_erros_atualiz:
           MESSAGE tt_log_erros_atualiz.ttv_num_mensagem SKIP
                   tt_log_erros_atualiz.ttv_des_msg_erro SKIP
                   tt_log_erros_atualiz.ttv_des_msg_ajuda SKIP
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
    END.
END.

