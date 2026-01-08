DEFINE STREAM s1.
OUTPUT STREAM s1 TO c:\temp\teste01.ret.
DEFINE VARIABLE cLinha      AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cTitulo     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParcela    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cChave      AS CHARACTER   NO-UNDO FORMAT 'X(19)'.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
INPUT FROM c:\temp\teste02.ret.

 REPEAT:
     ASSIGN iCont = iCont + 1.
     IMPORT UNFORM cLinha.
     IF iCont > 1 THEN DO:
        /*MESSAGE  'linha:' iCont
                 'conteudo:'cLinha SKIP
                'titulo:' SUBSTR(cLinha,117,7)
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        ASSIGN cTitulo  = SUBSTR(cLinha,117,7)
               cParcela = SUBSTR(cLinha,125,2).
        FIND FIRST  tit_acr
            WHERE   tit_acr.cod_estab = '501'
            AND     tit_acr.cod_ser_docto = '3'
            AND     tit_acr.cod_espec_docto = 'dp'
            AND     tit_acr.cod_tit_acr = cTitulo
            AND     tit_acr.cod_parcela = cParcela
            NO-LOCK NO-ERROR.
        IF AVAIL tit_acr THEN DO:
           RUN retornarChave(OUTPUT cChave).
           ASSIGN substr(cLinha,39,19) = cChave. 
        END.
           
     END.
     PUT STREAM s1 UNFORM cLinha SKIP.


 END.


INPUT CLOSE.
OUTPUT STREAM s1 CLOSE     .

        
PROCEDURE retornarChave.
    DEFINE OUTPUT PARAMETER cChave AS CHARACTER   NO-UNDO.

    IF AVAIL tit_acr THEN DO:
        //DISP tit_acr WITH 1 COL WIDTH 550 TITLE "titulo" NO-ERROR .
       FOR EACH movto_tit_acr OF tit_acr NO-LOCK.
           FOR EACH movto_ocor_bcia
           WHERE  movto_tit_acr.cod_estab                = movto_ocor_bcia.cod_estab
           AND    movto_tit_acr.num_id_movto_tit_acr     = movto_ocor_bcia.num_id_movto_tit_acr
           NO-LOCK.
               ASSIGN cChave = tit_acr.cod_estab + ";" + string(movto_ocor_bcia.num_id_movto_tit_acr) + ";" +   string(movto_ocor_bcia.num_id_movto_ocor_bcia)  + ";" . 
               /*DISP
                movto_ocor_bcia.num_id_tit_acr            
                movto_ocor_bcia.num_id_movto_tit_acr           
                movto_ocor_bcia.num_id_movto_ocor_bcia         
                movto_ocor_bcia.num_seq_movto_ocor_bcia        
                movto_ocor_bcia.num_id_movto_ocor_confir       
                movto_ocor_bcia.nom_arq_movimen_bcia           
                movto_ocor_bcia.dat_movto_ocor_bcia            
                movto_ocor_bcia.hra_movto_ocor_bcia            
                movto_ocor_bcia.ind_ocor_bcia_remes_ret        
                movto_ocor_bcia.ind_tip_ocor_bcia   
                WITH 1 COL WIDTH 550 TITLE "ocorr" NO-ERROR.   */

           END.
       END.        
    END.

END PROCEDURE.
            /*
FOR EACH tit_acr NO-LOCK
    WHERE tit_acr.cod_estab = '501'
    AND   tit_acr.cod_ser_docto = '3'
    AND  tit_acr.cod_tit_acr = '0139061'
    AND  tit_acr.cod_parcela = '03'
    AND  tit_acr.cdn_cliente = 35959 .
    DISP tit_acr WITH 1 COL WIDTH 550 NO-ERROR.
    FOR EACH movto_tit_acr OF tit_acr NO-LOCK.
        DISP movto_tit_acr WITH 1 COL WIDTH 550 TITLE "MOVTO" NO-ERROR.
        FOR EACH compl_movto_tit_acr
            WHERE  movto_tit_acr.cod_estab = compl_movto_tit_Acr.cod_estab
            AND    movto_tit_acr.num_id_movto_tit_acr  = compl_movto_tit_acr.num_id_movto_tit_acr
            NO-LOCK.
            DISP movto_tit_acr WITH 1 COL WIDTH 550 TITLE "MOVTO_COMPL" NO-ERROR.


            FOR EACH movto_ocor_bcia OF compl_movto_tit_acr NO-LOCK:
            //DISP movto_ocor_bcia WITH 1 COL WIDTH 550 TITLE "OCORR" NO-ERROR.
            DISP
                num_id_tit_acr            
                num_id_movto_tit_acr           
                num_id_movto_ocor_bcia         
                num_seq_movto_ocor_bcia        
                num_id_movto_ocor_confir       
                nom_arq_movimen_bcia           
                dat_movto_ocor_bcia            
                hra_movto_ocor_bcia            
                ind_ocor_bcia_remes_ret        
                ind_tip_ocor_bcia   WITH 1 COL WIDTH 550 TITLE "ocorr" NO-ERROR.           
            

            END.
        END.
    END.
    FOR EACH ext_movto_ocor_bcia OF tit_acr NO-LOCK:
        DISP tit_acr WITH 1 COL WIDTH 550 TITLE "EXT" NO-ERROR.

    END.
END.*/

/*
tit_acr:
  abat_antecip_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_ti
  abat_prev_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_tit_a
  aprop_liquidac_antecip OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,
  argext_val_tit_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_
  arg_comprov_solicit_cae OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto
  arg_histor_ndebito_mora OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto
  arg_impto_acr_elimdos OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,c
  arg_tit_acr_elimdos OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod
  chl_tit_acr_elimdos OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod
  demonst_ctbl_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_ti
  docto_recibo OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_tit_ac
  gerac_antecip OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_tit_a
  his_nota_devol_tit_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,
  his_tit_acr_histor OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_
  item_agenda_inadimp OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod
  item_bord_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_tit_a
  item_liquidac_vendor OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,co
  item_lote_impl_tit_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,
  item_lote_liquidac_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,
  item_operac_techfin OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod
  item_renegoc_inadimp OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,co
  movto_tit_acr_fechado OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,c
  nota_devol_tit_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_
  pryext_impto_liquidac_cr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_doct
  pry_impto_liq_recibo OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,co
  pry_tit_certif_retenc_cr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_doct
  renegoc_acr OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_tit_acr
  repres_tit_acr_fechado OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,
  tit_calc_renegoc_inadimp OF tit_acr (cod_estab,cod_espec_docto,cod_ser_doct
  tit_carta_anuencia OF tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_
  aprop_ctbl_pend_acr OF tit_acr (cod_estab,num_id_tit_acr)
  aprop_despes_recta_acr OF tit_acr (cod_estab,num_id_tit_acr)
  aprop_despes_recta_pend OF tit_acr (cod_estab,num_id_tit_acr)
  arg_recibo_tit_vincul OF tit_acr (cod_estab,num_id_tit_acr)
  cheq_acr OF tit_acr (cod_estab,num_id_tit_acr)
  compl_movto_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  dupl_vendor OF tit_acr (cod_estab,num_id_tit_acr)
  estorn_cobr_especial OF tit_acr (cod_estab,num_id_tit_acr)
  ext_movto_ocor_bcia OF tit_acr (cod_estab,num_id_tit_acr)
  ext_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  histor_cobr_escrit OF tit_acr (cod_estab,num_id_tit_acr)
  histor_despes_recta OF tit_acr (cod_estab,num_id_tit_acr)
  histor_movto_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  histor_tit_cobr_especial OF tit_acr (cod_estab,num_id_tit_acr)
  impto_vincul_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  item_lote_mktplace OF tit_acr (cod_estab,num_id_tit_acr)
  item_renegoc_acr OF tit_acr (cod_estab,num_id_tit_acr)
  item_renegoc_terc OF tit_acr (cod_estab,num_id_tit_acr)
  movto_cobr_especial_acr OF tit_acr (cod_estab,num_id_tit_acr)
  movto_cobr_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  movto_ocor_bcia OF tit_acr (cod_estab,num_id_tit_acr)
  movto_spool_acr_vendor OF tit_acr (cod_estab,num_id_tit_acr)
  movto_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  movto_tit_acr_avp OF tit_acr (cod_estab,num_id_tit_acr)
  movto_tit_envio_serasa OF tit_acr (cod_estab,num_id_tit_acr)
  orig_sales_tax_tit_ap OF tit_acr (cod_estab,num_id_tit_acr)
  parc_vendor OF tit_acr (cod_estab,num_id_tit_acr)
  ped_vda_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  perext_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  portad_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  pryext_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  relacto_cheq_acr OF tit_acr (cod_estab,num_id_tit_acr)
  relacto_parc_mais_negoc OF tit_acr (cod_estab,num_id_tit_acr)
  relacto_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  relac_movto_cobr_inadimp OF tit_acr (cod_estab,num_id_tit_acr)
  relac_movto_cobr_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  relac_movto_tit_acr_avp OF tit_acr (cod_estab,num_id_tit_acr)
  relac_parc_cartcred OF tit_acr (cod_estab,num_id_tit_acr)
  repres_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  sales_tax_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  tit_acr_cobr_ativ OF tit_acr (cod_estab,num_id_tit_acr)
  tit_acr_msg_financ OF tit_acr (cod_estab,num_id_tit_acr)
  tit_acr_terc OF tit_acr (cod_estab,num_id_tit_acr)
  val_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  tit_acr OF adiant_emprest_produt (cod_estab,cod_refer)
  tit_acr OF antecip_pef_pend (cod_estab,cod_refer)
  tit_acr OF agenc_bcia (cod_banco,cod_agenc_bcia)
  tit_acr OF argext_bord_acr (cod_estab_bord,num_bord_acr)
  tit_acr OF argext_clien_financ (cod_empresa,cdn_cliente)
  tit_acr OF argext_cta_corren (cod_cta_corren)
  tit_acr OF argext_estab (cod_estab)
  tit_acr OF argext_ext_cta_corren (cod_cta_corren)
  tit_acr OF argext_indic_econ (cod_indic_econ)
  tit_acr OF argext_lote_impl_tit_ap (cod_estab,cod_refer)
  tit_acr OF argext_portador (cod_portador)
  tit_acr OF arg_clas_ser_afip (cod_espec_docto,cod_ser_docto)
  tit_acr OF arg_cobr_extra_clien (cod_estab,cod_refer,cod_empresa)
  tit_acr OF arg_ord_pagto (cod_estab,cod_refer)
  tit_acr OF arg_param_livro_iva (cod_empresa)
  tit_acr OF arg_portad_vta_cheq (cod_portador)
  tit_acr OF arg_recibo (cod_estab,cod_refer)
  tit_acr OF arg_tit_acr_elimdos (cod_estab,cod_espec_docto,cod_ser_docto,cod
  tit_acr OF banco (cod_banco)
  tit_acr OF bord_acr (cod_estab_bord,num_bord_acr)
  tit_acr OF cart_bcia (cod_cart_bcia)
  tit_acr OF chlext_espec_docto (cod_espec_docto)
  tit_acr OF chlext_estab (cod_estab)
  tit_acr OF chl_tit_acr_elimdos (cod_estab,cod_espec_docto,cod_ser_docto,cod
  tit_acr OF clien_analis_cr (cod_empresa,cdn_cliente)
  tit_acr OF clien_cobr_ativ (cod_empresa,cdn_cliente,cod_estab)
  tit_acr OF clien_financ (cod_empresa,cdn_cliente)
  tit_acr OF clien_inadimp (cod_empresa,cdn_cliente)
  tit_acr OF clien_portad (cod_empresa,cdn_cliente,cod_portador)
  tit_acr OF cobr_especial_acr (cod_estab,cod_refer,cod_portador)
  tit_acr OF colext_clien_financ (cod_empresa,cdn_cliente)
  tit_acr OF colext_cta_corren (cod_cta_corren)
  tit_acr OF colext_estab (cod_estab)
  tit_acr OF conven_cart_bcia (cod_portador)
  tit_acr OF cta_corren (cod_cta_corren)
  tit_acr OF empresa (cod_empresa)
  tit_acr OF enctro_cta (cod_estab,cod_refer)
  tit_acr OF espec_bco (cod_espec_docto,cod_banco)
  tit_acr OF espec_docto (cod_espec_docto)
  tit_acr OF espec_docto_financ (cod_espec_docto)
  tit_acr OF espec_docto_financ_acr (cod_espec_docto)
  tit_acr OF espec_docto_financ_apf (cod_espec_docto,cod_empresa)
  tit_acr OF espec_vdr_db (cod_espec_docto)
  tit_acr OF extnam_antecip_pef_pend (cod_estab,cod_refer)
  tit_acr OF extnam_empres (cod_empresa)
  tit_acr OF ext_tit_acr (cod_estab,num_id_tit_acr)
  tit_acr OF fatur_acr (cod_estab,num_fatur_acr)
  tit_acr OF fiador_clien_inadimp (cod_empresa,cdn_cliente,num_pessoa)
  tit_acr OF fiador_renegoc_inadimp (cod_refer,num_pessoa)
  tit_acr OF fornecedor (cod_empresa,nom_abrev)
  tit_acr OF grp_clien (cod_grp_clien)
  tit_acr OF histor_acum_repres (cod_empresa,cdn_repres)
  tit_acr OF his_bord_acr (cod_estab_bord,num_bord_acr)
  tit_acr OF his_enctro_cta (cod_estab,cod_refer)
  tit_acr OF his_fatur_acr (cod_estab,num_fatur_acr)
  tit_acr OF his_movto_tit_acr_histor (cod_estab,cod_refer)
  tit_acr OF his_movto_tit_ap_histor (cod_estab,cod_refer)
  tit_acr OF his_nota_devol_tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,
  tit_acr OF his_operac_financ_acr (cod_estab,cod_movto_operac_financ)
  tit_acr OF his_renegoc_acr (cod_estab,num_renegoc_cobr_acr)
  tit_acr OF his_tit_acr_cobr_esp (cod_estab,cod_espec_docto,cod_ser_docto,co
  tit_acr OF his_tit_acr_histor (cod_estab,cod_espec_docto,cod_ser_docto,cod_
  tit_acr OF indic_econ (cod_indic_econ)
  tit_acr OF int_sdo_consolid_param (cod_empresa)
  tit_acr OF lote_impl_tit_acr (cod_estab,cod_refer)
  tit_acr OF lote_impl_tit_ap (cod_estab,cod_refer)
  tit_acr OF mexext_clien_financ (cod_empresa,cdn_cliente)
  tit_acr OF mexext_estab (cod_estab)
  tit_acr OF movto_cta_corren (num_id_movto_cta_corren)
  tit_acr OF nota_devol_tit_acr (cod_estab,cod_espec_docto,cod_ser_docto,cod_
  tit_acr OF operac_financ_acr (cod_estab,cod_movto_operac_financ)
  tit_acr OF ord_busca_orcto (cod_empresa)
  tit_acr OF param_cobr_ativ (cod_empresa)
  tit_acr OF param_empres_shc (cod_empresa)
  tit_acr OF param_pat_internac (cod_empresa)
  tit_acr OF perext_bco (cod_banco)
  tit_acr OF perext_espec_docto (cod_espec_docto)
  tit_acr OF portador (cod_portador)
  tit_acr OF portad_estab (cod_estab,cod_portador)
  tit_acr OF pryext_antecip_pef_pend (cod_estab,cod_refer)
  tit_acr OF pryext_estab (cod_estab)
  tit_acr OF pryext_tit_acr (cod_estab,num_id_tit_acr)
  tit_acr OF pry_param_livro_iva (cod_empresa)
  tit_acr OF relac_parc_cartcred (cod_estab,num_id_tit_acr)
  tit_acr OF renegoc_acr (cod_estab,num_renegoc_cobr_acr)
  tit_acr OF renegoc_inadimp (cod_refer)
  tit_acr OF repres_financ (cod_empresa,cdn_repres)
  tit_acr OF repres_tit_acr (cod_estab,num_id_tit_acr,cdn_repres)
  tit_acr OF repres_tit_cancdo_acr (cod_empresa,cod_estab,cod_espec_docto,cod
  tit_acr OF tip_trans_cx_nreal_apb (cod_espec_docto)
  tit_acr OF tit_acr_avp (cod_estab,num_id_tit_acr)
  tit_acr OF tit_acr_cobr_ativ (cod_estab,num_id_tit_acr)
  tit_acr OF tit_acr_cobr_especial (cod_estab,cod_espec_docto,cod_ser_docto,c
  tit_acr OF tit_acr_envio_serasa (cod_estab,num_id_tit_acr)
  tit_acr OF tit_acr_fechado (cod_empresa,cod_estab,cdn_cliente,cod_espec_doc
  tit_acr OF tit_cancdo_acr (cod_empresa,cod_estab,cod_espec_docto,cod_ser_do
  tit_acr OF tit_inadimp (cod_estab,cod_espec_docto,cod_ser_docto,cod_tit_acr
*/

/*
movto_tit_acr:
  aprop_ctbl_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  argext_impto_iva_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  col_movto_recibo OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  extnam_aprop_ctbl_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  extnam_impto_iva_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  impto_val_agreg_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  livro_fisc_movto_tit_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  mexext_movto_tit_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  movto_impto_tit_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  movto_tit_acr_fechado OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  rat_movto_tit_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  techfin_process OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  val_movto_acr_correc_val OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  val_movto_tit_acr OF movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  movto_tit_acr OF adiant_emprest_produt (cod_estab,cod_refer)
  movto_tit_acr OF antecip_pef_pend (cod_estab,cod_refer)
  movto_tit_acr OF argext_clien_financ (cod_empresa,cdn_cliente)
  movto_tit_acr OF argext_estab (cod_estab)
  movto_tit_acr OF argext_lote_impl_tit_ap (cod_estab,cod_refer)
  movto_tit_acr OF argext_portador (cod_portador)
  movto_tit_acr OF arg_cobr_extra_clien (cod_estab,cod_refer,cod_empresa)
  movto_tit_acr OF arg_ord_pagto (cod_estab,cod_refer)
  movto_tit_acr OF arg_param_livro_iva (cod_empresa)
  movto_tit_acr OF arg_portad_vta_cheq (cod_portador)
  movto_tit_acr OF arg_recibo (cod_estab,cod_refer)
  movto_tit_acr OF cart_bcia (cod_cart_bcia)
  movto_tit_acr OF chlext_espec_docto (cod_espec_docto)
  movto_tit_acr OF chlext_estab (cod_estab)
  movto_tit_acr OF cliente (cod_empresa,cdn_cliente)
  movto_tit_acr OF clien_analis_cr (cod_empresa,cdn_cliente)
  movto_tit_acr OF clien_cobr_ativ (cod_empresa,cdn_cliente,cod_estab)
  movto_tit_acr OF clien_financ (cod_empresa,cdn_cliente)
  movto_tit_acr OF clien_inadimp (cod_empresa,cdn_cliente)
  movto_tit_acr OF clien_portad (cod_empresa,cdn_cliente,cod_portador)
  movto_tit_acr OF cobr_especial_acr (cod_estab,cod_refer,cod_portador)
  movto_tit_acr OF colext_clien_financ (cod_empresa,cdn_cliente)
  movto_tit_acr OF colext_estab (cod_estab)
  movto_tit_acr OF compl_movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  movto_tit_acr OF conven_cart_bcia (cod_portador)
  movto_tit_acr OF empresa (cod_empresa)
  movto_tit_acr OF enctro_cta (cod_estab,cod_refer)
  movto_tit_acr OF espec_docto (cod_espec_docto)
  movto_tit_acr OF espec_docto_financ (cod_espec_docto)
  movto_tit_acr OF espec_docto_financ_acr (cod_espec_docto)
  movto_tit_acr OF espec_docto_financ_apf (cod_espec_docto,cod_empresa)
  movto_tit_acr OF espec_vdr_db (cod_espec_docto)
  movto_tit_acr OF estabelecimento (cod_estab)
  movto_tit_acr OF extnam_antecip_pef_pend (cod_estab,cod_refer)
  movto_tit_acr OF extnam_empres (cod_empresa)
  movto_tit_acr OF ext_tit_acr (cod_estab,num_id_tit_acr)
  movto_tit_acr OF fatur_acr (cod_estab,num_fatur_acr)
  movto_tit_acr OF his_enctro_cta (cod_estab,cod_refer)
  movto_tit_acr OF his_fatur_acr (cod_estab,num_fatur_acr)
  movto_tit_acr OF his_movto_tit_acr_histor (cod_estab,cod_refer)
  movto_tit_acr OF his_movto_tit_ap_histor (cod_estab,cod_refer)
  movto_tit_acr OF his_renegoc_acr (cod_estab,num_renegoc_cobr_acr)
  movto_tit_acr OF int_sdo_consolid_param (cod_empresa)
  movto_tit_acr OF lote_impl_tit_acr (cod_estab,cod_refer)
  movto_tit_acr OF lote_impl_tit_ap (cod_estab,cod_refer)
  movto_tit_acr OF mexext_clien_financ (cod_empresa,cdn_cliente)
  movto_tit_acr OF mexext_estab (cod_estab)
  movto_tit_acr OF mexext_movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  movto_tit_acr OF motiv_movto_inadimp (cod_estab,cod_motiv_movto_tit_acr)
  movto_tit_acr OF motiv_movto_tit_acr (cod_estab,cod_motiv_movto_tit_acr)
  movto_tit_acr OF movto_cta_corren (num_id_movto_cta_corren)
  movto_tit_acr OF movto_tit_acr_fechado (num_id_movto_tit_acr)
  movto_tit_acr OF movto_tit_envio_serasa (cod_estab,num_id_tit_acr,num_id_mo
  movto_tit_acr OF ord_busca_orcto (cod_empresa)
  movto_tit_acr OF param_cobr_ativ (cod_empresa)
  movto_tit_acr OF param_empres_shc (cod_empresa)
  movto_tit_acr OF param_pat_internac (cod_empresa)
  movto_tit_acr OF perext_espec_docto (cod_espec_docto)
  movto_tit_acr OF perf_usuar_inadimp (cod_usuario)
  movto_tit_acr OF portador (cod_portador)
  movto_tit_acr OF portad_estab (cod_estab,cod_portador)
  movto_tit_acr OF prefer_usuar_cfl (cod_usuario)
  movto_tit_acr OF prefer_usuar_cmg (cod_usuario)
  movto_tit_acr OF pryext_antecip_pef_pend (cod_estab,cod_refer)
  movto_tit_acr OF pryext_estab (cod_estab)
  movto_tit_acr OF pryext_tit_acr (cod_estab,num_id_tit_acr)
  movto_tit_acr OF pry_param_livro_iva (cod_empresa)
  movto_tit_acr OF relac_movto_tit_acr_avp (cod_estab,num_id_tit_acr,num_id_m
  movto_tit_acr OF relac_parc_cartcred (cod_estab,num_id_tit_acr)
  movto_tit_acr OF renegoc_acr (cod_estab,num_renegoc_cobr_acr)
  movto_tit_acr OF renegoc_inadimp (cod_refer)
  movto_tit_acr OF tip_trans_cx_nreal_apb (cod_espec_docto)
  movto_tit_acr OF tit_acr (cod_estab,num_id_tit_acr)
  movto_tit_acr OF tit_acr_avp (cod_estab,num_id_tit_acr)
  movto_tit_acr OF tit_acr_cobr_ativ (cod_estab,num_id_tit_acr)
  movto_tit_acr OF tit_acr_envio_serasa (cod_estab,num_id_tit_acr)
  movto_tit_acr OF usuar_apl (cod_empresa,cod_usuario)
  movto_tit_acr OF usuar_financ (cod_usuario)
  movto_tit_acr OF usuar_financ_estab_acr (cod_estab,cod_usuario)
  movto_tit_acr OF usuar_financ_estab_apb (cod_usuario,cod_estab)
  movto_tit_acr OF usuar_inadimp (cod_usuario)
  movto_tit_acr OF usuar_univ (cod_usuario,cod_empresa)

*/

/*

 ext_movto_ocor_bcia OF movto_ocor_bcia (cod_estab,num_id_tit_acr,num_seq_mo
  histor_cobr_escrit OF movto_ocor_bcia (cod_estab,num_id_movto_ocor_bcia)
  movto_ocor_bcia OF adiant_emprest_produt (cod_estab,cod_refer)
  movto_ocor_bcia OF antecip_pef_pend (cod_estab,cod_refer)
  movto_ocor_bcia OF argext_clien_financ (cod_empresa,cdn_cliente)
  movto_ocor_bcia OF argext_estab (cod_estab)
  movto_ocor_bcia OF argext_indic_econ (cod_indic_econ)
  movto_ocor_bcia OF argext_lote_impl_tit_ap (cod_estab,cod_refer)
  movto_ocor_bcia OF argext_portador (cod_portador)
  movto_ocor_bcia OF arg_cobr_extra_clien (cod_estab,cod_refer,cod_empresa)
  movto_ocor_bcia OF arg_ord_pagto (cod_estab,cod_refer)
  movto_ocor_bcia OF arg_param_livro_iva (cod_empresa)
  movto_ocor_bcia OF arg_portad_vta_cheq (cod_portador)
  movto_ocor_bcia OF arg_recibo (cod_estab,cod_refer)
  movto_ocor_bcia OF cart_bcia (cod_cart_bcia)
  movto_ocor_bcia OF chlext_estab (cod_estab)
  movto_ocor_bcia OF cliente (cod_empresa,cdn_cliente)
  movto_ocor_bcia OF clien_analis_cr (cod_empresa,cdn_cliente)
  movto_ocor_bcia OF clien_cobr_ativ (cod_empresa,cdn_cliente,cod_estab)
  movto_ocor_bcia OF clien_financ (cod_empresa,cdn_cliente)
  movto_ocor_bcia OF clien_inadimp (cod_empresa,cdn_cliente)
  movto_ocor_bcia OF clien_portad (cod_empresa,cdn_cliente,cod_portador)
  movto_ocor_bcia OF cobr_especial_acr (cod_estab,cod_refer,cod_portador)
  movto_ocor_bcia OF colext_clien_financ (cod_empresa,cdn_cliente)
  movto_ocor_bcia OF colext_estab (cod_estab)
  movto_ocor_bcia OF compl_movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  movto_ocor_bcia OF conven_cart_bcia (cod_portador)
  movto_ocor_bcia OF empresa (cod_empresa)
  movto_ocor_bcia OF enctro_cta (cod_estab,cod_refer)
  movto_ocor_bcia OF estabelecimento (cod_estab)
  movto_ocor_bcia OF extnam_antecip_pef_pend (cod_estab,cod_refer)
  movto_ocor_bcia OF extnam_empres (cod_empresa)
  movto_ocor_bcia OF ext_tit_acr (cod_estab,num_id_tit_acr)
  movto_ocor_bcia OF his_enctro_cta (cod_estab,cod_refer)
  movto_ocor_bcia OF his_movto_tit_acr_histor (cod_estab,cod_refer)
  movto_ocor_bcia OF his_movto_tit_ap_histor (cod_estab,cod_refer)
  movto_ocor_bcia OF his_operac_financ_acr (cod_estab,cod_movto_operac_financ
  movto_ocor_bcia OF indic_econ (cod_indic_econ)
  movto_ocor_bcia OF int_sdo_consolid_param (cod_empresa)
  movto_ocor_bcia OF lote_impl_tit_acr (cod_estab,cod_refer)
  movto_ocor_bcia OF lote_impl_tit_ap (cod_estab,cod_refer)
  movto_ocor_bcia OF mexext_clien_financ (cod_empresa,cdn_cliente)
  movto_ocor_bcia OF mexext_estab (cod_estab)
  movto_ocor_bcia OF mexext_movto_tit_acr (cod_estab,num_id_movto_tit_acr)
  movto_ocor_bcia OF movto_tit_acr_fechado (num_id_movto_tit_acr)
  movto_ocor_bcia OF movto_tit_envio_serasa (cod_estab,num_id_tit_acr,num_id_
  movto_ocor_bcia OF operac_financ_acr (cod_estab,cod_movto_operac_financ)
  movto_ocor_bcia OF ord_busca_orcto (cod_empresa)
  movto_ocor_bcia OF param_cobr_ativ (cod_empresa)
  movto_ocor_bcia OF param_empres_shc (cod_empresa)
  movto_ocor_bcia OF param_pat_internac (cod_empresa)
  movto_ocor_bcia OF perf_usuar_inadimp (cod_usuario)
  movto_ocor_bcia OF portador (cod_portador)
  movto_ocor_bcia OF portad_estab (cod_estab,cod_portador)
  movto_ocor_bcia OF prefer_usuar_cfl (cod_usuario)
  movto_ocor_bcia OF prefer_usuar_cmg (cod_usuario)
  movto_ocor_bcia OF proces_edi (cdn_proces_edi)
  movto_ocor_bcia OF pryext_antecip_pef_pend (cod_estab,cod_refer)
  movto_ocor_bcia OF pryext_estab (cod_estab)
  movto_ocor_bcia OF pryext_tit_acr (cod_estab,num_id_tit_acr)
  movto_ocor_bcia OF pry_param_livro_iva (cod_empresa)
  movto_ocor_bcia OF relac_movto_tit_acr_avp (cod_estab,num_id_tit_acr,num_id
  movto_ocor_bcia OF relac_parc_cartcred (cod_estab,num_id_tit_acr)
  movto_ocor_bcia OF renegoc_inadimp (cod_refer)
  movto_ocor_bcia OF tit_acr (cod_estab,num_id_tit_acr)
  movto_ocor_bcia OF tit_acr_avp (cod_estab,num_id_tit_acr)
  movto_ocor_bcia OF tit_acr_cobr_ativ (cod_estab,num_id_tit_acr)
  movto_ocor_bcia OF tit_acr_envio_serasa (cod_estab,num_id_tit_acr)
  movto_ocor_bcia OF usuar_apl (cod_empresa,cod_usuario)
  movto_ocor_bcia OF usuar_financ (cod_usuario)
  movto_ocor_bcia OF usuar_financ_estab_acr (cod_estab,cod_usuario)
  movto_ocor_bcia OF usuar_financ_estab_apb (cod_usuario,cod_estab)
  movto_ocor_bcia OF usuar_inadimp (cod_usuario)
  movto_ocor_bcia OF usuar_univ (cod_usuario,cod_empresa)




*/
