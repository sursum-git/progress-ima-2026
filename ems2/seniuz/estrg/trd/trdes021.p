TRIGGER PROCEDURE FOR DELETE OF mov-est-acbm.
FOR EACH mov-est-acbd where mov-est-acbd.cod-estabel = mov-est-acbm.cod-estabel
                        and mov-est-acbd.data-mov    = mov-est-acbm.data-mov
                        and mov-est-acbd.num-lote    = mov-est-acbm.num-lote
                        and mov-est-acbd.it-codigo   = mov-est-acbm.it-codigo
                        and mov-est-acbd.cod-refer   = mov-est-acbm.cod-refer:
    DELETE mov-est-acbd.
END.
