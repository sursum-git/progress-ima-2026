FOR EACH ref-item-ext WHERE ref-item-ext.it-codigo    = '504613'
                        /*  
                        AND ref-item-ext.cod-refer    >= '0201371'
                        AND ref-item-ext.cod-refer    <= '0201407'
                        */
                        AND ref-item-ext.cod-obsoleto = '1'
                      EXCLUSIVE-LOCK.
    ASSIGN ref-item-ext.cod-obsoleto = '2'. 
    ACCUMULATE ref-item-ext.it-codigo(COUNT).
END.
DISPLAY (ACCUM COUNT ref-item-ext.it-codigo).

