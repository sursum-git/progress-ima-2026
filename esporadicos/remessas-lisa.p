OUTPUT TO c:\temp\remessas-lisa.csv.
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.nat-operacao = '59207i'
    AND nota-fiscal.cod-estabel = '505'
    AND nota-fiscal.dt-cancela  = ?.    
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
       
        EXPORT DELIMITER ";"  nota-fiscal.nr-nota-fis it-nota-fisc.it-codigo it-nota-fisc.cod-refer it-nota-fisc.qt-faturada[1].
    
    END.
    
END.
