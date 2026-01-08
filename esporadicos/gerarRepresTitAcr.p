FOR EACH tit_acr NO-LOCK
    WHERE 
    /*tit_acr.cod_estab = '505'
    AND tit_acr.cod_espec_docto = 'dp'
    AND tit_acr.cod_ser_doc = '2'
    AND tit_acr.cod_tit_acr = '0000851'*/
    tit_acr.dat_emis_docto >= 09.22.2023
    .
     
    IF CAN-FIND(FIRST repres_tit_acr 
                WHERE repres_tit_acr.cod_Estab =  tit_acr.cod_estab
                AND   repres_tit_acr.num_id_tit_acr = tit_Acr.num_id_tit_acr ) 
       THEN NEXT.

    DISP cod_tit_acr.
    RUN esporadicos/criarComissaoTitacr.p(ROWID(tit_acr)).


END.
