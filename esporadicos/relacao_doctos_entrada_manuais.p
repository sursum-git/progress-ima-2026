OUTPUT TO c:\temp\docs_manuais.txt.
FOR EACH natur-oper NO-LOCK
    WHERE natur-oper.cod-model-nf-eletro <> '55'
    AND natur-oper.cod-model-nf-eletro <> '57':
    FOR EACH docum-est OF natur-oper
        WHERE docum-est.dt-trans >= 09.01.2017
        AND   docum-est.dt-trans <= 09.08.2017 NO-LOCK.
        DISP docum-est.nro-docto docum-est.serie-docto docum-est.cod-emitente
             docum-est.dt-trans.
        

    END.

END.
