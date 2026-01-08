FOR EACH mov-est-acbm WHERE mov-est-acbm.num-lote = 136784 
                      SHARE-LOCK.
    DISP mov-est-acbm.data-mov
         mov-est-acbm.num-lote
         mov-est-acbm.it-codigo FORMAT "x(6)"
         mov-est-acbm.cod-refer
         mov-est-acbm.tipo-tear.
    /*ASSIGN mov-est-acbm.tipo-tear = "TOYOTA".*/
END.
