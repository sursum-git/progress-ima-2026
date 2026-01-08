fOR EACH mov-est-acbm WHERE mov-est-acbm.it-codigo = "599908" 
                        AND mov-est-acbm.data-mov  > 12/31/2004
                      NO-LOCK.
    DISP mov-est-acbm.data-mov
         mov-est-acbm.num-lote
         mov-est-acbm.it-codigo FORMAT "x(6)"
         mov-est-acbm.cod-refer
         mov-est-acbm.qtd-tot-def(TOTAL).
END.

FOR EACH mov-est-acbd WHERE mov-est-acbd.it-codigo = "599908"
                        AND mov-est-acbd.data-mov  > 12/31/2004
                      NO-LOCK.
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
