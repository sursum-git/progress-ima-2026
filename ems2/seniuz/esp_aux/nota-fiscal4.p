DEF VAR de-vl-tot-item AS DEC.
FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  = "2"
                       AND nota-fiscal.serie        = "1"
                       AND nota-fiscal.dt-emis-nota >= 07/21/2002
                       AND nota-fiscal.dt-emis-nota <= 07/31/2002
                       AND nota-fiscal.dt-cancela   =  ?
                     NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal NO-LOCK
    BREAK BY it-nota-fisc.it-codigo:

    ASSIGN de-vl-tot-item = de-vl-tot-item + it-nota-fisc.vl-tot-item.
           
    IF LAST-OF(it-nota-fisc.it-codigo) THEN DO:
       FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
       DISP ITEM.it-codigo
            ITEM.desc-item
            de-vl-tot-item.
       ASSIGN de-vl-tot-item = 0.
    END.
END.      
