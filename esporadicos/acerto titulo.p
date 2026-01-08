OUTPUT TO c:\temp\tit_ap.txt.
FOR EACH tit_ap   WHERE LOG_sdo_tit_ap = YES 
/*WHERE cod_tit_ap = '1352/15'*/:   
    FIND FIRST ems5.fornecedor OF tit_ap NO-LOCK NO-ERROR.
    IF fornecedor.num_pessoa <> tit_ap.num_pessoa THEN DO:
       DISP cod_tit_ap dat_transacao tit_ap.cdn_fornec tit_ap.num_pessoa fornecedor.num_pessoa.
       ASSIGN tit_ap.num_pessoa = fornecedor.num_pessoa.
    END.
       

END.
