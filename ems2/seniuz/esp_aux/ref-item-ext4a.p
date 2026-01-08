FOR EACH ref-item-ext WHERE ref-item-ext.it-codigo = '504163'
                        AND ref-item-ext.cod-refer BEGINS '05'
                        AND (SUBSTR(ref-item-ext.cod-refer,3,4) = '4038' OR
                             SUBSTR(ref-item-ext.cod-refer,3,4) = '0212' OR
                             SUBSTR(ref-item-ext.cod-refer,3,4) = '0214' OR
                             SUBSTR(ref-item-ext.cod-refer,3,4) = '0215' OR
                             SUBSTR(ref-item-ext.cod-refer,3,4) = '0216' OR
                             SUBSTR(ref-item-ext.cod-refer,3,4) = '0211')
                        AND SUBSTR(ref-item-ext.cod-refer,7,1) >= '1'
                        AND SUBSTR(ref-item-ext.cod-refer,7,1) <= '4'
                        AND ref-item-ext.cod-obsoleto = '2'
                      NO-LOCK.
    DISP ref-item-ext.it-codigo
         ref-item-ext.cod-refer
         ref-item-ext.cod-obsoleto.
/*     ASSIGN ref-item-ext.cod-obsoleto = '1'.  */
END.

