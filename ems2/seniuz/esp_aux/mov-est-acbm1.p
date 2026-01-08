FOR EACH mov-est-acbm WHERE /*mov-est-acbm.it-codigo = "501821" 
                        AND */mov-est-acbm.data-mov  >= 06/01/2006
                        AND mov-est-acbm.data-mov  <= 06/30/2006
                      NO-LOCK.

    FOR EACH mov-est-acbd OF mov-est-acbm NO-LOCK.
        ACCUMULATE mov-est-acbd.qtd-defeit(TOTAL).
    END.

    IF (ACCUM TOTAL mov-est-acbd.qtd-defeit) <>
                    mov-est-acbm.qtd-tot-def THEN
       DISP mov-est-acbm.data-mov
            mov-est-acbm.num-lote
            mov-est-acbm.it-codigo FORMAT "x(6)"
            mov-est-acbm.cod-refer
            mov-est-acbm.qtd-tot-def
            ACCUM TOTAL mov-est-acbd.qtd-defeit.
END.
