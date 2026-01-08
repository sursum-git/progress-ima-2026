DISABLE TRIGGERS FOR LOAD OF tit_ap.
FOR EACH  tit_ap EXCLUSIVE-LOCK 
    /*WHERE tit_ap.cod_espec_docto = 'pr' 
    AND  tit_ap.dat_ven  >= 01.01.2020 BY tit_ap.dat_emis .*/
    WHERE tit_ap.cod_estab = '501'
    AND   tit_ap.cdn_fornecedor = 11513
    AND   tit_ap.cod_espec_docto =  'Ct'
    AND   tit_ap.cod_ser_docto = '1'
    AND   tit_ap.cod_tit_ap = '0825478'
    AND   tit_ap.cod_parcela = '01'
    .
    
    IF AVAIL tit_ap THEN DO:
        FIND FIRST docum-est
            WHERE docum-est.cod-estab = '5'
            AND   docum-est.cod-emitente = tit_ap.cdn_fornecedor
          //  AND   docum-est.esp-docto = string(tit_ap.cod_espec_docto)
            AND   docum-est.serie   = tit_ap.cod_ser_docto
            AND   docum-est.nro-docto = tit_ap.cod_tit_ap
            AND   docum-est.dt-trans = tit_ap.dat_emis_
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL docum-est THEN
           ASSIGN docum-est.ap-atual = NO
                  docum-est.ce-atual = NO.
        DISP tit_ap WITH 1 COL WIDTH 550.
       FOR EACH movto_tit_ap OF tit_ap.
           FOR EACH aprop_ctbl_ap OF movto_tit_ap:
               DELETE aprop_ctbl_ap.
           END.
           FOR EACH compl_movto_pagto OF movto_tit_ap:
               DELETE compl_movto_pagto.
           END.
           FOR EACH impto_pagto OF movto_tit_ap:
               DELETE impto_pagto.
           END.
    
           FOR EACH rat_movto_tit_ap OF movto_tit_ap:
               DELETE rat_movto_tit_ap.
           END.
    
           FOR EACH relacto_acum_pagto OF movto_tit_ap:
               DELETE relacto_acum_pagto.
           END.
           FOR EACH val_movto_ap OF movto_tit_ap:
               DELETE val_movto_ap.
           END.
    
           FOR EACH val_movto_ap_correc_val OF movto_tit_ap:
               DELETE val_movto_ap_correc_val.
           END.
    
           FOR EACH item_cheq_ap OF movto_tit_ap:
               DELETE ITEM_cheq_ap.
           END.
           FOR EACH impto_val_agreg_movto_ap OF movto_tit_ap:
               DELETE impto_val_agreg_movto_ap.
           END.
           DELETE movto_tit_ap.
       END.
       FOR EACH relacto_tit_ap  OF tit_ap:
           DELETE relacto_tit_ap.
       END.
       FOR EACH proces_pagto OF tit_ap:
           FOR EACH ext_proces_pagto OF proces_pagto:
               DELETE ext_proces_pagto.
           END.
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
