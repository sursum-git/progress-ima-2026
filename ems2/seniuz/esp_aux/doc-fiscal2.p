DEF BUFFER b-doc-fiscal FOR doc-fiscal.
FOR EACH doc-fiscal WHERE doc-fiscal.serie = "u"
                       OR doc-fiscal.serie = "b1".
    ACCUMULATE doc-fiscal.serie(COUNT).
    IF doc-fiscal.cod-estabel <> "2" THEN NEXT.
    IF CAN-FIND (b-doc-fiscal WHERE b-doc-fiscal.cod-estabel   = doc-fiscal.cod-estabel
                                AND b-doc-fiscal.serie         = ""
                                AND b-doc-fiscal.nr-doc-fis    = doc-fiscal.nr-doc-fis  
                                AND b-doc-fiscal.cod-emitente  = doc-fiscal.cod-emitente
                                AND b-doc-fiscal.nat-operacao  = doc-fiscal.nat-operacao) THEN DO:
       DISPLAY doc-fiscal.cod-estabel 
               doc-fiscal.serie       
               doc-fiscal.nr-doc-fis  
               doc-fiscal.cod-emitente
               doc-fiscal.nat-operacao.
    END.
    ELSE DO:
       FOR EACH it-doc-fisc OF doc-fiscal.
           ASSIGN it-doc-fisc.serie = "".
       END.
       ASSIGN doc-fiscal.serie = "".
    END.
END.
DISP (ACCUM COUNT doc-fiscal.serie).

