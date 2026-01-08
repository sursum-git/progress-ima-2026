DEFINE VARIABLE lok AS LOGICAL     NO-UNDO.
OUTPUT TO p:\titulosdivergentes.txt.
FOR EACH tit_acr
    WHERE tit_acr.dat_emis_docto >= 01.01.2016
    AND cod_tit_acr = '0000238'
    AND cod_estab = '101'.
    
    FIND FIRST ems5.cliente 
        WHERE cliente.cdn_cliente = tit_acr.cdn_cliente
        AND  tit_Acr.num_pessoa <> cliente.num_pessoa NO-LOCK NO-ERROR.
    IF AVAIL cliente THEN DO:
       ASSIGN lok = NO.
       ASSIGN tit_acr.num_pessoa = cliente.num_pessoa
           tit_acr.nom_abrev  = cliente.nom_abrev
           tit_acr.nom_abrev_contat = IF tit_acr.nom_abrev_contat <> '' THEN cliente.nom_abrev ELSE tit_acr.nom_abrev_contat.
    END. 
    ELSE DO:
        FIND FIRST ems5.cliente 
        WHERE cliente.cdn_cliente = tit_acr.cdn_cliente
        NO-LOCK NO-ERROR.
        ASSIGN lok = YES.
    END.
    EXPORT DELIMITER "|" lok tit_acr.cod_estab
        tit_acr.cod_tit_Acr 
        tit_acr.cod_ser_docto 
        tit_acr.cod_parcela
        tit_acr.cod_espec_docto 
        tit_Acr.dat_emis_docto 
        tit_acr.val_origin_tit_acr 
        tit_acr.val_sdo_tit_acr 
        tit_Acr.cdn_cliente 
        tit_acr.num_pessoa 
        cliente.num_pessoa .
    
END.
OUTPUT CLOSE.


             
