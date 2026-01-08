DEF VAR de-tot1 AS DEC.
DEF VAR de-tot2 AS DEC.

FOR EACH mov-est-acbm WHERE mov-est-acbm.data-mov >= 10/01/2006 
                        AND mov-est-acbm.data-mov <= 10/24/2006
                      NO-LOCK:
    ASSIGN de-tot1 = de-tot1 + mov-est-acbm.qtd-tot-def.
END.

FOR EACH mov-est-acbd WHERE mov-est-acbd.data-mov >= 10/01/2006  
                        AND mov-est-acbd.data-mov <= 10/24/2006  
                      NO-LOCK:
    ASSIGN de-tot2 = de-tot2 + mov-est-acbd.qtd-defeit.
END.
DISP de-tot1 de-tot2.
