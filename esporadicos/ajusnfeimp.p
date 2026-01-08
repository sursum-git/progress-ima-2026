FOR EACH nota-fiscal
    WHERE nota-fiscal.cod-estabel = '505'
    AND nota-fiscal.serie = '1'
    AND nota-fiscal.nr-nota-fis = '0000029'.
    UPDATE 
        vl-tot-nota
        vl-tot-nota-me
        val-desp-outros
        val-desp-outros-inf
        
        /*nota-fiscal EXCEPT ind-orig-entrada ind-via-envio char-1 char-2 idi-sit-nf-eletro 
        cod-cond-pag
        WITH 1 COL WIDTH 550 NO-ERROR*/
        .
END.
