FOR EACH nota-fiscal FIELDS(dt-emis-nota dt-cancela) NO-LOCK
    /*WHERE nota-fiscal.cod-estabel   = '5'
    AND  nota-fiscal.serie          = '3'
    AND  nota-fiscal.nr-nota-fis    = '0181789'*/
    WHERE nota-fiscal.dt-emis-nota >= 04.01.2025
    AND nota-fiscal.dt-cancela = ?  
    AND nota-fiscal.cod-estabel = '5':
    RUN esapi/imprime-nf-527.p(ROWID(nota-fiscal),NO).
END.


