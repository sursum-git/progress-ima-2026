FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.serie = '3'
    AND nota-fiscal.cod-estabel = '1'
    AND nota-fiscal.cod-emitente = 10535
    AND nota-fiscal.nr-nota-fis = '0044259'.
    FOR EACH fat-duplic 
        WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel
        AND   fat-duplic.serie       = nota-fiscal.serie
        AND   fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis EXCLUSIVE-LOCK:
        UPDATE fat-duplic.cod-esp.
    END.

END.
