FOR EACH docum-est
    WHERE docum-est.nat-operacao = '31201'
    AND   docum-est.cod-emitente = 26715
    AND   docum-est.nro-docto    = '0054736'
    AND   docum-est.serie-docto  = '3':
    FOR EACH dupli-apagar OF docum-est:
        DISP dupli-apagar WITH 1 COL WIDTH 550.
    END.


END.
