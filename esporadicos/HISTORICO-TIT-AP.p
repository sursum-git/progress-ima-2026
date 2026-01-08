FOR EACH  tit_ap NO-LOCK
    WHERE   tit_ap.cod_empresa = '500'
    AND    dat_emis_docto > 12.01.2014,
    EACH  movto_tit_ap OF tit_ap
    WHERE    movto_tit_ap.ind_trans_ap = 'IMPLANTA€ÇO',
    EACH histor_tit_movto_ap 
    WHERE  histor_tit_movto_ap.num_id_tit_ap = tit_ap.num_id_tit_ap   
    AND    histor_tit_movto_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap:
   
    
    DISP
    tit_ap.cod_empresa
    tit_ap.cod_estab         
    tit_ap.cod_espec_docto
    tit_ap.cod_ser_docto     
    tit_ap.cdn_fornecedor     
    tit_ap.cod_tit_ap         
    tit_ap.cod_parcela        
    tit_ap.log_sdo_tit_ap     
    tit_ap.cod_portador       
    tit_ap.dat_transacao      
    tit_ap.dat_emis_docto     
    tit_ap.dat_vencto_tit_ap  
    tit_ap.dat_desconto     
    tit_ap.dat_prev_pagto     
    tit_ap.dat_ult_pagto      
    tit_ap.dat_liquidac_tit_ap
    tit_ap.val_origin_tit_ap  
    tit_ap.val_sdo_tit_ap
        movto_tit_ap.ind_trans_ap
    des_text_histor
        ind_orig_histor_ap
        num_seq_histor_movto_ap
      WITH 1 COL WIDTH 550.









END.


/*
SELECT 
    PUB.tit_ap.cod_empresa,
    PUB.tit_ap.cod_estab,
    PUB.tit_ap.cod_espec_docto,
    PUB.tit_ap.cod_ser_docto,
    PUB.tit_ap.cdn_fornecedor,
    PUB.tit_ap.cod_tit_ap,
    PUB.tit_ap.cod_parcela,
    PUB.tit_ap.log_sdo_tit_ap,
    PUB.tit_ap.cod_portador,
    PUB.tit_ap.dat_transacao,
    PUB.tit_ap.dat_emis_docto,
    PUB.tit_ap.dat_vencto_tit_ap,
    PUB.tit_ap.dat_desconto,
    PUB.tit_ap.dat_prev_pagto,
    PUB.tit_ap.dat_ult_pagto,
    PUB.tit_ap.dat_liquidac_tit_ap,
    PUB.tit_ap.val_origin_tit_ap,
    PUB.tit_ap.val_sdo_tit_ap,
    des_text_histor     
FROM 
    PUB.tit_ap, pub.histor_tit_movto_ap, pub.movto_tit_ap 
where pub.histor_tit_movto_ap.num_id_tit_ap = pub.tit_ap.num_id_tit_ap 
and pub.tit_ap.cod_estab = pub.movto_tit_ap.cod_estab 
and pub.tit_ap.num_id_tit_ap = pub.movto_tit_ap.num_id_tit_ap and pub.histor_tit_movto_ap.num_id_movto_tit_ap = pub.movto_tit_ap.num_id_movto_tit_ap and pub.movto_tit_ap.ind_trans_ap = 'implanta‡Æo' and ( PUB.tit_ap.cod_empresa = '500' and PUB.tit_ap.dat_emis_docto between '2014-12-01' and '2014-12-31' )
*/
