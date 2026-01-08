DISABLE TRIGGERS FOR LOAD OF tit_ap.

DEFINE TEMP-TABLE tt
     FIELD cod_empresa      AS CHAR
     FIELD cod_estab        AS CHAR
     FIELD cdn_fornecedor   AS INT
     FIELD cod_ser          AS CHAR
     FIELD cod_espec_docto  AS CHAR 
     FIELD cod_parcela      AS CHAR
     FIELD cod_tit_ap       AS CHAR FORMAT 'x(15)'.
INPUT FROM p:\tadeu\titulos.csv.
REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.

INPUT CLOSE.
OUTPUT TO c:\temp\tits_del.txt.
FOR EACH tt:
    EXPORT DELIMITER '|' tt.
    FOR EACH tit_ap
    WHERE tit_ap.cod_empresa       = tt.cod_empresa
    AND   tit_ap.cod_estab         = tt.cod_estab
    AND   tit_ap.cod_ser           = tt.cod_ser
    AND   tit_ap.cod_tit_ap        = tt.cod_tit_ap
    AND   tit_ap.cod_espec_docto   = tt.cod_espec_docto
    AND   tit_ap.cod_parcela       = tt.cod_parcela
    AND   tit_ap.cdn_fornec        = tt.cdn_fornecedor .
        EXPORT DELIMITER "|" tit_ap.

        /*FOR EACH item_lote_impl_ap OF tit_ap.
            DELETE ITEM_lote_impl_ap.

        END.*/
        FOR EACH movto_tit_ap OF tit_ap.
            DELETE movto_tit_ap.
        END.
        FOR EACH relacto_tit_ap  OF tit_ap:
            DELETE relacto_tit_ap.
        END.
        FOR EACH proces_pagto OF tit_ap:
            DELETE proces_pagto.
        END.
        FOR EACH compl_impto_retid_ap OF tit_ap.
         DELETE compl_impto_retid_ap.
        END.
        FOR EACH compl_retenc_impto_pagto OF tit_ap.
            DELETE compl_retenc_impto_pagto.
        END.
        FOR EACH movto_tit_ap_avp OF tit_ap.
            DELETE movto_tit_ap_avp.
        END.
        FOR EACH relacto_tit_em_bco OF tit_ap.
            DELETE  relacto_tit_em_bco.
        END.
    
        FOR EACH relac_movto_tit_ap_avp OF tit_ap.
           DELETE relac_movto_tit_ap_avp.
        END.
        
        FOR EACH tit_ap_bcio OF tit_ap.
            DELETE tit_ap_bcio.
        END.
    
        FOR EACH val_tit_ap OF tit_ap.
            DELETE val_tit_ap.
        END.
        
        FOR EACH impto_nao_retid_ap OF tit_ap.
            DELETE impto_nao_retid_ap.
        END.
        DELETE tit_ap.                                 
    END. 
END.

/*tit_ap:
  abat_prev_provis_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod
  abat_antecip_vouch OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_
  antecip_pef_pend OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_se
  argext_val_tit_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_s
  arg_despch_tit_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_s
  arg_nf_vincul_cm OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_se
  arg_tit_ap_impto_retenc OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto
  demonst_ctbl_apb OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_se
  docto_estoq_esoc OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_se
  ext_proces_pagto OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_se
  his_tit_ap_transf_histor OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_doct
  item_bord_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser_do
  item_cheq_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser_do
  item_lote_impl_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_s
  item_lote_pagto OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser
  mexext_tit_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser_d
  movto_tit_ap_fechado OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,co
  nota_devol_tit_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_s
  nota_pend_cartcred OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_
  proces_pagto OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser_do
  pryext_tit_ap OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser_d
  tit_ap_fechado OF tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser_
  adiant_prestac_cta_eec OF tit_ap (cod_estab,num_id_tit_ap)
  acerto_cta_eec OF tit_ap (cod_estab,num_id_tit_ap)
  arg_relacto_cheq_terc OF tit_ap (cod_estab,num_id_tit_ap)
  compl_impto_retid_ap OF tit_ap (cod_estab,num_id_tit_ap)
  compl_pagto_cartcred OF tit_ap (cod_estab,num_id_tit_ap)
  compl_retenc_impto_pagto OF tit_ap (cod_estab,num_id_tit_ap)
  extnam_movto_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  extnam_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  fechto_event_tit OF tit_ap (cod_estab,num_id_tit_ap)
  histor_tit_movto_ap OF tit_ap (cod_estab,num_id_tit_ap)
  ext_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  mex_const_retenc OF tit_ap (cod_estab,num_id_tit_ap)
  movto_comis_repres OF tit_ap (cod_estab,num_id_tit_ap)
  movto_conven_financ OF tit_ap (cod_estab,num_id_tit_ap)
  movto_event_tit OF tit_ap (cod_estab,num_id_tit_ap)
  movto_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  movto_tit_ap_avp OF tit_ap (cod_estab,num_id_tit_ap)
  ord_compra_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  orig_sales_tax_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  parc_pagto_conven OF tit_ap (cod_estab,num_id_tit_ap)
  tit_control_xml_e_social OF tit_ap (cod_estab,num_id_tit_ap)
  pry_certif_retenc_iva OF tit_ap (cod_estab,num_id_tit_ap)
  relacto_process_integr OF tit_ap (cod_estab,num_id_tit_ap)
  relacto_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  relacto_tit_em_bco OF tit_ap (cod_estab,num_id_tit_ap)
  relac_movto_tit_ap_avp OF tit_ap (cod_estab,num_id_tit_ap)
  sales_tax_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  tit_ap_bcio OF tit_ap (cod_estab,num_id_tit_ap)
  tit_ap_liber OF tit_ap (cod_estab,num_id_tit_ap)
  tit_contra_empres_em_bco OF tit_ap (cod_estab,num_id_tit_ap)
  tit_darf_ap OF tit_ap (cod_estab,num_id_tit_ap)
  val_tit_ap OF tit_ap (cod_estab,num_id_tit_ap)
  impto_nao_retid_ap OF tit_ap (cod_estab,num_id_tit_ap)
  tit_ap OF apol_seguro (cod_seguradora,cod_apol_seguro)
  tit_ap OF antecip_pef_pend (cod_estab,cod_refer)
  tit_ap OF argext_estab (cod_estab)
  tit_ap OF argext_fornec_cheq (cod_empresa,cdn_fornecedor)
  tit_ap OF argext_fornec_financ (cod_empresa,cdn_fornecedor)
  tit_ap OF argext_indic_econ (cod_indic_econ)
  tit_ap OF argext_lote_impl_tit_ap (cod_estab,cod_refer)
  tit_ap OF argext_portador (cod_portador)
  tit_ap OF argext_val_tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_s
  tit_ap OF argext_val_tit_pend_acr (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF argext_val_tit_pend_ap (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF arg_clas_ser_afip (cod_espec_docto,cod_ser_docto)
  tit_ap OF arg_cobr_extra_clien (cod_estab,cod_refer,cod_empresa)
  tit_ap OF arg_cta_fornec_datanet (cod_empresa,cdn_fornecedor,cod_portador)
  tit_ap OF arg_fornec_despa (cod_empresa,cdn_fornecedor)
  tit_ap OF arg_ord_pagto (cod_estab,cod_refer)
  tit_ap OF arg_param_livro_iva (cod_empresa)
  tit_ap OF arg_portad_vta_cheq (cod_portador)
  tit_ap OF arg_recibo (cod_estab,cod_refer)
  tit_ap OF arrendador (cod_arrendador)
  tit_ap OF chlext_espec_docto (cod_espec_docto)
  tit_ap OF chlext_estab (cod_estab)
  tit_ap OF cobr_especial_acr (cod_estab,cod_refer,cod_portador)
  tit_ap OF colext_estab (cod_estab)
  tit_ap OF colext_fornec_financ (cod_empresa,cdn_fornecedor)
  tit_ap OF compl_impto_retid_ap (cod_estab,num_id_tit_ap)
  tit_ap OF compl_pagto_cartcred (cod_estab,num_id_tit_ap)
  tit_ap OF contrat_leas (cod_arrendador,cod_contrat_leas)
  tit_ap OF conven_cart_bcia (cod_portador)
  tit_ap OF empresa (cod_empresa)
  tit_ap OF enctro_cta (cod_estab,cod_refer)
  tit_ap OF espec_docto (cod_espec_docto)
  tit_ap OF espec_docto_financ (cod_espec_docto)
  tit_ap OF espec_docto_financ_acr (cod_espec_docto)
  tit_ap OF espec_docto_financ_apf (cod_espec_docto,cod_empresa)
  tit_ap OF espec_vdr_db (cod_espec_docto)
  tit_ap OF estabelecimento (cod_estab)
  tit_ap OF extnam_antecip_pef_pend (cod_estab,cod_refer)
  tit_ap OF extnam_empres (cod_empresa)
  tit_ap OF extnam_fornec_financ (cod_empresa,cdn_fornecedor)
  tit_ap OF extnam_item_impl_acr (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF extnam_item_lote_impl_ap (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF extnam_item_lote_pagto (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF extnam_tit_ap (cod_estab,num_id_tit_ap)
  tit_ap OF fiador_renegoc_inadimp (cod_refer,num_pessoa)
  tit_ap OF forma_pagto (cod_forma_pagto)
  tit_ap OF fornecedor (cod_empresa,cdn_fornecedor)
  tit_ap OF fornec_financ (cod_empresa,cdn_fornecedor)
  tit_ap OF grp_fornec (cod_grp_fornec)
  tit_ap OF histor_acum_fornec (cod_empresa,cdn_fornecedor)
  tit_ap OF his_enctro_cta (cod_estab,cod_refer)
  tit_ap OF his_movto_tit_acr_histor (cod_estab,cod_refer)
  tit_ap OF his_movto_tit_ap_histor (cod_estab,cod_refer)
  tit_ap OF his_tit_ap_transf_histor (cod_estab,cdn_fornecedor,cod_espec_doct
  tit_ap OF indic_econ (cod_indic_econ)
  tit_ap OF int_sdo_consolid_param (cod_empresa)
  tit_ap OF item_lote_impl_ap (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF item_lote_impl_tit_acr (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF lote_impl_tit_acr (cod_estab,cod_refer)
  tit_ap OF lote_impl_tit_ap (cod_estab,cod_refer)
  tit_ap OF ext_tit_ap (cod_estab,num_id_tit_ap)
  tit_ap OF mexext_estab (cod_estab)
  tit_ap OF mexext_fornec_financ (cod_empresa,cdn_fornecedor)
  tit_ap OF mexext_tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser_d
  tit_ap OF movto_cta_corren (num_id_movto_cta_corren)
  tit_ap OF ord_busca_orcto (cod_empresa)
  tit_ap OF param_cobr_ativ (cod_empresa)
  tit_ap OF param_empres_shc (cod_empresa)
  tit_ap OF portador (cod_portador)
  tit_ap OF portad_estab (cod_estab,cod_portador)
  tit_ap OF pryext_antecip_pef_pend (cod_estab,cod_refer)
  tit_ap OF pryext_estab (cod_estab)
  tit_ap OF pryext_fornec_financ (cod_empresa,cdn_fornecedor)
  tit_ap OF pryext_item_lote_impl_ap (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF pryext_item_lote_impl_cr (cod_estab,cod_refer,num_seq_refer)
  tit_ap OF pryext_tit_ap (cod_estab,cdn_fornecedor,cod_espec_docto,cod_ser_d
  tit_ap OF pry_param_livro_iva (cod_empresa)
  tit_ap OF renegoc_inadimp (cod_refer)
  tit_ap OF seguradora (cod_seguradora)
  tit_ap OF tip_trans_cx_nreal_apb (cod_espec_docto)
  tit_ap OF tit_ap_avp (cod_estab,num_id_tit_ap)
  tit_ap OF tit_ap_fechado (cod_empresa,cod_estab,cdn_fornecedor,cod_espec_do
  tit_ap OF tit_cancdo_apb (cod_empresa,cod_estab,cdn_fornecedor,cod_espec_do
  tit_ap OF perext_espec_docto (cod_espec_docto)
  tit_ap OF perext_item_impl_tit_acr (cod_estab,cod_refer,num_seq_refer)


*/
