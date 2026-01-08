DEF VAR de-quantidade AS DEC.
DEF VAR l-falta-fator AS LOG.

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel = "2"
                   AND nota-fiscal.serie       = "1"
                   AND nota-fiscal.nr-nota-fis = "0160525"
                 NO-LOCK,
EACH it-nota-fisc OF nota-fiscal:

    find item where item.it-codigo = it-nota-fisc.it-codigo
         no-lock no-error.
    
    
    /*------ Conversao de M para Kg ------- */
    IF item.un <> "m" THEN DO:
       FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                     NO-LOCK NO-ERROR.
       IF AVAIL item-ext AND item-ext.fator-conv <> 0 THEN
          ASSIGN de-quantidade = it-nota-fisc.qt-faturada[1] * item-ext.fator-conv.
       ELSE DO:
          IF ITEM.peso-liquido <> 0 THEN
             ASSIGN de-quantidade = it-nota-fisc.qt-faturada[1] * (1 / ITEM.peso-liquido).
          ELSE DO:
             ASSIGN l-falta-fator = YES
                    de-quantidade = it-nota-fisc.qt-faturada[1].
             /*
             FIND FIRST tt-work WHERE tt-work.it-codigo = it-nota-fisc.it-codigo
                                NO-LOCK NO-ERROR.
             IF NOT AVAIL tt-work THEN DO:
                CREATE tt-work.
                ASSIGN tt-work.it-codigo = it-nota-fisc.it-codigo.
             END.
             */
          END.
       END.   
    END.
    ELSE
       ASSIGN de-quantidade = it-nota-fisc.qt-faturada[1].

    DISP it-nota-fisc.it-codigo
         item-ext.fator-conv
         ITEM.peso-liquido
         it-nota-fisc.qt-faturada[1]
         de-quantidade.
END.
