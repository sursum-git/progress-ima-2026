DEFINE VARIABLE iCheque AS INTEGER     NO-UNDO.
DEFINE VARIABLE conta AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTalonario AS INTEGER     NO-UNDO.
UPDATE conta iTalonario iCheque.
FOR EACH ems5.cheque
WHERE cheque.cod_empresa = '500'
AND    cheque.num_cheque = iCheque
       AND cheque.cod_cta_corren = conta
       AND  cheque.num_talon_cheq = iTalonario  :
    DISP cheque WITH 1 COL WIDTH 550.
    FOR EACH histor_cheq OF cheque:
        DISP histor_cheq WITH 1 COL WIDTH 550.
    END.
    FOR EACH his_cheq OF cheque:
        DISP his_cheq WITH 1 COL WIDTH 550.
    END.
    FOR EACH compl_movto_pagto OF cheque :
        DISP compl_movto_pagto WITH 1 COL  WIDTH 550.

    END.
    FOR EACH  movto_cta_corren OF cheque:
        DISP movto_cta_corren WITH 1 COL WIDTH 550.
    END.

    FOR EACH cheq_ap OF cheque:
        DISP cheq_ap WITH 1 COL WIDTH 550 TITLE "cheq.ap".

        FOR EACH compl_movto_pagto OF cheq_ap:
            DISP compl_movto_pagto WITH 1 COL WIDTH 550 TITLE "compl.Movto.Pagto cheq.ap". 
            DELETE compl_movto_pagto.
        END.

        FOR EACH item_bord_ap OF cheq_ap:
            DISP item_bord_ap WITH 1 COL WIDTH 550 TITLE "item bordero ap cheq.ap".
            DELETE ITEM_bord_ap.
        END.
        
        FOR EACH item_cheq_ap OF cheq_ap:
            DISP item_cheq_ap WITH 1 COL WIDTH 550 TITLE "item cheq.ap".
            DELETE ITEM_cheq_ap.
        END.
        DELETE cheq_ap.
    END.
    FIND FIRST talon_cheq
        WHERE talon_cheq.cod_cta_corren = conta
        AND   talon_cheq.num_talon_cheq = iTalonario NO-LOCK NO-ERROR.
    IF AVAIL talon_cheq THEN DO:
       ASSIGN talon_cheq.num_ult_cheq_emitid = iCheque - 1
              talon_cheq.num_prox_cheq = iCheque.
    END.
    DELETE cheque.


END.
/*relacionamentos
cheq_ap OF cheque (cod_cta_corren,num_talon_cheq,num_cheque)
  histor_cheq OF cheque (cod_cta_corren,num_talon_cheq,num_cheque)
  his_cheq OF cheque (cod_cta_corren,num_talon_cheq,num_cheque)
  compl_movto_pagto OF cheque (num_id_cheq)
  movto_cta_corren OF cheque (num_id_cheq)
  cheque OF argext_cta_corren (cod_cta_corren)
  cheque OF argext_ext_cta_corren (cod_cta_corren)
  cheque OF arg_param_livro_iva (cod_empresa)
  cheque OF bco_histor_period (cod_empresa,cod_modul_dtsul)
  cheque OF colext_cta_corren (cod_cta_corren)
  cheque OF cta_corren (cod_cta_corren)
  cheque OF empresa (cod_empresa)
  cheque OF extnam_empres (cod_empresa)
  cheque OF finalid_econ (cod_finalid_econ)
  cheque OF his_cheq (cod_cta_corren,num_talon_cheq,num_cheque)
  cheque OF int_sdo_consolid_param (cod_empresa)
  cheque OF ord_busca_orcto (cod_empresa)
  cheque OF param_cobr_ativ (cod_empresa)
  cheque OF param_ctbz (cod_modul_dtsul,cod_empresa)
  cheque OF param_empres_shc (cod_empresa)
  cheque OF pry_param_livro_iva (cod_empresa)
  cheque OF talon_cheq (cod_cta_corren,num_talon_cheq)
  cheque OF tip_cheq (cod_tip_cheq)

    
    
  compl_movto_pagto OF cheq_ap (cod_estab_cheq,num_id_cheq_ap)
  item_bord_ap OF cheq_ap (cod_estab_cheq,num_id_cheq_ap)
  item_bord_ap_agrup OF cheq_ap (cod_estab_cheq,num_id_cheq_ap)
  item_cheq_ap OF cheq_ap (cod_estab_cheq,num_id_cheq_ap)
  cheq_ap OF agenc_bcia (cod_banco,cod_agenc_bcia)
  cheq_ap OF argext_cta_corren (cod_cta_corren)
  cheq_ap OF argext_ext_cta_corren (cod_cta_corren)
  cheq_ap OF argext_portador (cod_portador)
  cheq_ap OF arg_param_livro_iva (cod_empresa)
  cheq_ap OF arg_portad_vta_cheq (cod_portador)
  cheq_ap OF banco (cod_banco)
  cheq_ap OF bord_ap (cod_estab_bord,cod_portador,num_bord_ap)
  cheq_ap OF cart_bcia (cod_cart_bcia)
  cheq_ap OF cheque (cod_cta_corren,num_talon_cheq,num_cheque)
  cheq_ap OF cheq_acr (cod_banco,cod_agenc_bcia,cod_cta_corren_bco,num_cheque
  cheq_ap OF colext_cta_corren (cod_cta_corren)
  cheq_ap OF conven_cart_bcia (cod_portador)
  cheq_ap OF cta_corren (cod_cta_corren)
  cheq_ap OF empresa (cod_empresa)
  cheq_ap OF extnam_empres (cod_empresa)
  cheq_ap OF finalid_econ (cod_finalid_econ)
  cheq_ap OF his_bord_ap (cod_estab_bord,cod_portador,num_bord_ap)
  cheq_ap OF his_cheq (cod_cta_corren,num_talon_cheq,num_cheque)
  cheq_ap OF his_cheq_acr (cod_banco,cod_agenc_bcia,cod_cta_corren_bco,num_ch
  cheq_ap OF int_sdo_consolid_param (cod_empresa)
  cheq_ap OF ord_busca_orcto (cod_empresa)
  cheq_ap OF param_cobr_ativ (cod_empresa)
  cheq_ap OF param_empres_shc (cod_empresa)
  cheq_ap OF portador (cod_portador)
  cheq_ap OF pry_param_livro_iva (cod_empresa)
  cheq_ap OF talon_cheq (cod_cta_corren,num_talon_cheq)
  cheq_ap OF perext_bco (cod_banco)  

*/

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
*/
