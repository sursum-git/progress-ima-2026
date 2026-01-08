
 OUTPUT TO c:\temp\ditdoc.txt.
 FOR EACH doc-fisc
    WHERE doc-fisc.cod-estabel = '5'
    AND   doc-fisc.serie = '3'
    AND   doc-fisc.cod-emitente = 31379
    AND   doc-fisc.nat-operacao = '61201m'
    AND nr-doc-fis = '0096176'
     NO-LOCK:
    FOR EACH it-doc-fisc OF doc-fisc NO-LOCK.
        EXPORT it-doc-fisc.

    END.
END.
