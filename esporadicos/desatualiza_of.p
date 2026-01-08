 OUTPUT TO c:\temp\desatualiza_of.txt.
 FOR EACH nota-fiscal
    WHERE dt-emis-nota >= 02.01.2016
    AND   dt-emis-nota <= 02.28.2016:
    FIND FIRST doc-fiscal
        WHERE doc-fiscal.cod-estabel = nota-fiscal.cod-estabel
        AND   doc-fiscal.nat-operacao = nota-fiscal.nat-operacao
        AND   doc-fiscal.cod-emitente = nota-fiscal.cod-emitente
        AND   doc-fiscal.serie = nota-fiscal.serie
        AND   doc-fiscal.dt-emis-doc = nota-fiscal.dt-emis-nota
        
         NO-LOCK NO-ERROR.
    IF NOT AVAIL doc-fiscal THEN DO:
       DISP nota-fiscal.nr-nota-fis.
       ASSIGN nota-fiscal.dt-at-ofest = ?.
    END.
       
END.
