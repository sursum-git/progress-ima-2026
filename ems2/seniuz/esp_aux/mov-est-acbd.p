FOR EACH mov-est-acbd WHERE mov-est-acbd.data-mov >= 06/01/2006
                        AND mov-est-acbd.data-mov <= 06/30/2006
                      NO-LOCK.
    FIND tipo-def OF mov-est-acbd NO-LOCK NO-ERROR.
    FIND defeito OF mov-est-acbd NO-LOCK NO-ERROR.

    IF NOT AVAIL tipo-def OR
       NOT AVAIL defeito THEN
    DISP mov-est-acbd.data-mov
         mov-est-acbd.num-lote
         mov-est-acbd.it-codigo FORMAT "x(6)"
         mov-est-acbd.cod-refer
         mov-est-acbd.num-revis
         mov-est-acbd.num-maq
         mov-est-acbd.cod-acond
         mov-est-acbd.num-acond
         mov-est-acbd.classific 
         mov-est-acbd.cod-tipo-def
         mov-est-acbd.cod-defeito
         mov-est-acbd.qtd-defeit(TOTAL).
END.

