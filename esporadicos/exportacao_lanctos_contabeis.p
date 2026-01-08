OUTPUT TO c:\temp\itens_lancto_ctbl.
FOR EACH lancto_ctbl NO-LOCK.
    FOR EACH ITEM_lancto_ctbl OF lancto_ctbl
        WHERE ITEM_lancto_ctbl.ind_sit_lancto_ctbl = 'ctbz' NO-LOCK.
        FIND cta_ctbl OF ITEM_lancto_ctbl NO-LOCK NO-ERROR.
        FIND ems5.ccusto OF ITEM_lancto_ctbl NO-LOCK NO-ERROR.
        FIND tip_lancto_ctbl OF ITEM_lancto_ctbl NO-LOCK NO-ERROR.
        FIND ems5.unid_negoc OF ITEM_lancto_ctbl NO-LOCK NO-ERROR.
        EXPORT DELIMITER "|" 
        lancto_ctbl.dat_lancto 
        ITEM_lancto_ctbl.cod_cta_ctbl 
        cta_ctbl.des_tit_ctbl
        item_lancto_ctbl.des_histor_lancto_ctbl
        item_lancto_ctbl.num_lote_ctbl
        ITEM_lancto_ctbl.num_lancto_ctbl
        ITEM_lancto_ctbl.num_seq_lancto_ctbl
        item_lancto_ctbl.ind_natur_lancto_ctbl
        ITEM_lancto_ctbl.cod_ccusto
        ITEM_lancto_ctbl.cod_empresa
        ITEM_lancto_ctbl.cod_unid_negoc 
        unid_negoc.des_unid_negoc 
        lancto_ctbl.ind_tip_lancto
        ITEM_lancto_ctbl.cod_tip_lancto_ctbl
        tip_lancto_ctbl.des_tip_lancto_ctbl
        ITEM_lancto_ctbl.val_lancto
        ITEM_lancto_ctbl.log_lancto_apurac_restdo .

    END.
END.
OUTPUT CLOSE.

/*
Data  

Conta

Desc Conta

Hist¢rico

Lote/Lancto/Seq

Orig

Estab

Un Neg

Movto D‚bito

Movto Cr‚dito

Contra Partida

Ccusto

Ccusto CPar





lancto_ctbl

ind_tipo_lancto        ->"Normal","Estornado","Estorno"
ind_sit_lancto_ctbl    -> "Pend","Conf","Ctbz"
ind_natur_lancto_ctbl  -> "DB", "CR"

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_modul_dtsul                  char        m
   20 num_lote_ctbl                    inte        im
   30 num_lancto_ctbl                  inte        im
   40 dat_lancto_ctbl                  date        im
   50 cod_cenar_ctbl                   char        i
   60 ind_sit_lancto_ctbl              char        im
   70 log_sdo_batch_atlzdo             logi        im
   80 log_lancto_conver                logi        m
   90 log_lancto_apurac_restdo         logi        m
  100 log_lancto_apurac_variac         logi        m
  110 cod_empresa                      char        im
  120 cod_lancto_ctbl_padr             char        im
  130 cod_rat_ctbl                     char        im
  140 dat_estorn_lancto_ctbl           date
  150 cod_histor_padr_estorn           char        im
  160 num_lote_ctbl_orig_cop           inte        im
  170 num_lancto_ctbl_orig_cop         inte        im
  180 cod_livre_1                      char
  190 ind_tip_lancto                   char
  200 num_lote_estordo                 inte
  210 num_lancto_estordo               inte        m
  220 cod_livre_2                      char
  230 dat_livre_1                      date
  240 dat_livre_2                      date
  250 log_livre_1                      logi
  260 log_livre_2                      logi
  270 num_livre_1                      inte
  280 num_livre_2                      inte
  290 val_livre_1                      deci-4
  300 val_livre_2                      deci-4
  310 log_rat_exec_orctaria            logi


item_lancto_ctbl
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 num_lote_ctbl                    inte        im
   20 num_lancto_ctbl                  inte        im
   30 num_seq_lancto_ctbl              inte        im
   40 ind_natur_lancto_ctbl            char        m
   50 cod_empresa                      char        im
   60 cod_plano_cta_ctbl               char        im
   70 cod_cta_ctbl                     char        im
   80 cod_plano_ccusto                 char        im
   90 cod_ccusto                       char        i
  100 cod_estab                        char        im
  110 cod_unid_negoc                   char        i
  120 cod_proj_financ                  char        im
  130 cod_histor_padr                  char        i
  140 cod_espec_docto                  char        i
  150 dat_docto                        date
  160 des_docto                        char
  170 cod_imagem                       char        i
  180 cod_indic_econ                   char        im
  190 dat_lancto_ctbl                  date        im
  200 qtd_unid_lancto_ctbl             deci-2      m
  210 val_lancto_ctbl                  deci-2      im
  220 des_histor_lancto_ctbl           char        im
  230 num_seq_lancto_ctbl_cpart        inte        m
  240 cod_tip_lancto_ctbl              char        im
  250 cod_cenar_ctbl                   char        m
  260 ind_sit_lancto_ctbl              char        im
  270 log_lancto_apurac_restdo         logi        m
  280 cod_livre_1                      char
  290 val_seq_entry_number             deci-0      i
  300 cod_livre_2                      char
  310 dat_livre_1                      date
  320 dat_livre_2                      date
  330 log_livre_1                      logi
  340 log_livre_2                      logi
  350 num_livre_1                      inte
  360 num_livre_2                      inte
  370 val_livre_1                      deci-4
  380 val_livre_2                      deci-4


ccusto

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        im
   20 cod_plano_ccusto                 char        im
   30 cod_ccusto                       char        im
   40 des_tit_ctbl                     char        im
   50 dat_inic_valid                   date        m
   60 dat_fim_valid                    date        m
   70 cod_usuar_respons                char        m
   80 log_permit_lancto_ctbl           logi        m
   90 des_anot_tab                     char        m
  100 cod_livre_1                      char
  110 log_livre_1                      logi
  120 num_livre_1                      inte
  130 val_livre_1                      deci-4
  140 dat_livre_1                      date
  150 log_sumar_agro                   logi
  160 cod_livre_2                      char
  170 dat_livre_2                      date
  180 log_livre_2                      logi
  190 num_livre_2                      inte
  200 val_livre_2                      deci-4
  210 ind_tip_ccusto                   char
  220 cdd_version                      deci-0
  230 cdn_id_reserve                   inte

tipo_lancto_ctbl
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_tip_lancto_ctbl              char        im
   20 des_tip_lancto_ctbl              char        m
   30 cod_livre_1                      char
   40 log_livre_1                      logi
   50 num_livre_1                      inte
   60 val_livre_1                      deci-4
   70 dat_livre_1                      date
   80 cod_livre_2                      char
   90 dat_livre_2                      date
  100 log_livre_2                      logi
  110 num_livre_2                      inte
  120 val_livre_2                      deci-4




*/







/*
aprop_lancto_ctbl OF item_lancto_ctbl (num_lote_ctbl,num_lancto_ctbl,num_se
  colext_item_lancto_ctbl OF item_lancto_ctbl (num_lote_ctbl,num_lancto_ctbl,
  perext_item_lancto_ctbl OF item_lancto_ctbl (num_lote_ctbl,num_lancto_ctbl,
  item_lancto_ctbl OF argext_estab (cod_estab)
  item_lancto_ctbl OF argext_indic_econ (cod_indic_econ)
  item_lancto_ctbl OF arg_param_livro_iva (cod_empresa)
  item_lancto_ctbl OF ccusto (cod_empresa,cod_plano_ccusto,cod_ccusto)
  item_lancto_ctbl OF ccusto_unid_negoc (cod_empresa,cod_plano_ccusto,cod_ccu
  item_lancto_ctbl OF cenar_ctbl (cod_cenar_ctbl)
  item_lancto_ctbl OF chlext_espec_docto (cod_espec_docto)
  item_lancto_ctbl OF chlext_estab (cod_estab)
  item_lancto_ctbl OF colext_cta_ctbl (cod_plano_cta_ctbl,cod_cta_ctbl)
  item_lancto_ctbl OF colext_estab (cod_estab)
  item_lancto_ctbl OF colext_item_lancto_ctbl (num_lote_ctbl,num_lancto_ctbl,
  item_lancto_ctbl OF colext_lancto_ctbl (num_lote_ctbl,num_lancto_ctbl)
  item_lancto_ctbl OF cta_ctbl (cod_plano_cta_ctbl,cod_cta_ctbl)
  item_lancto_ctbl OF cta_ctbl_intracompany (cod_unid_negoc)
  item_lancto_ctbl OF cta_ctbl_tip_fluxo (cod_empresa,cod_plano_cta_ctbl,cod_
  item_lancto_ctbl OF empresa (cod_empresa)
  item_lancto_ctbl OF espec_docto (cod_espec_docto)
  item_lancto_ctbl OF espec_docto_financ (cod_espec_docto)
  item_lancto_ctbl OF espec_docto_financ_acr (cod_espec_docto)
  item_lancto_ctbl OF espec_docto_financ_apf (cod_espec_docto,cod_empresa)
  item_lancto_ctbl OF espec_vdr_db (cod_espec_docto)
  item_lancto_ctbl OF estabelecimento (cod_estab)
  item_lancto_ctbl OF estab_unid_negoc (cod_estab,cod_unid_negoc)
  item_lancto_ctbl OF estrut_ctbl_movto_analit (cod_empresa,cod_plano_cta_ctb
  item_lancto_ctbl OF estrut_ctbl_movto_sint (cod_empresa,cod_plano_cta_ctbl,
  item_lancto_ctbl OF extnam_empres (cod_empresa)
  item_lancto_ctbl OF grp_exc_cta_ctbl (cod_empresa,cod_plano_ccusto,cod_ccus
  item_lancto_ctbl OF grp_segur_det_orcto (cod_empresa,cod_estab,cod_plano_cc
  item_lancto_ctbl OF histor_padr (cod_histor_padr)
  item_lancto_ctbl OF his_lote_ctbl (num_lote_ctbl)
  item_lancto_ctbl OF indic_econ (cod_indic_econ)
  item_lancto_ctbl OF int_sdo_consolid_param (cod_empresa)
  item_lancto_ctbl OF lancto_ctbl (num_lote_ctbl,num_lancto_ctbl)
  item_lancto_ctbl OF lote_ctbl (num_lote_ctbl)
  item_lancto_ctbl OF mexext_estab (cod_estab)
  item_lancto_ctbl OF ord_busca_orcto (cod_empresa)
  item_lancto_ctbl OF param_cenar_ctbl_apl (cod_cenar_ctbl)
  item_lancto_ctbl OF param_cobr_ativ (cod_empresa)
  item_lancto_ctbl OF param_empres_shc (cod_empresa)
  item_lancto_ctbl OF plano_ccusto (cod_empresa,cod_plano_ccusto)
  item_lancto_ctbl OF plano_cta_ctbl (cod_plano_cta_ctbl)
  item_lancto_ctbl OF proj_financ (cod_proj_financ)
  item_lancto_ctbl OF proj_financ_estab (cod_proj_financ,cod_estab)
  item_lancto_ctbl OF pryext_estab (cod_estab)
  item_lancto_ctbl OF pry_param_livro_iva (cod_empresa)
  item_lancto_ctbl OF restric_ccusto (cod_empresa,cod_plano_ccusto,cod_ccusto
  item_lancto_ctbl OF tip_lancto_ctbl (cod_tip_lancto_ctbl)
  item_lancto_ctbl OF tip_trans_cx_nreal_apb (cod_espec_docto)
  item_lancto_ctbl OF unid_negoc (cod_unid_negoc)
  item_lancto_ctbl OF utiliz_cenar_ctbl (cod_cenar_ctbl,cod_empresa)
  item_lancto_ctbl OF perext_plano_cta_ctbl (cod_plano_cta_ctbl)
  item_lancto_ctbl OF perext_espec_docto (cod_espec_docto)
  item_lancto_ctbl OF perext_item_lancto_ctbl (num_lote_ctbl,num_lancto_ctbl

*/

