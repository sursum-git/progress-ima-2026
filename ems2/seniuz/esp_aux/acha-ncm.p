DEF VAR c-ncm-est LIKE ITEM.class-fiscal.
DEF VAR c-ncm-bco LIKE ITEM.class-fiscal.
DEF VAR c-ncm-tto LIKE ITEM.class-fiscal.

FOR EACH ITEM WHERE ITEM.ge-codigo >= 50
                AND ITEM.ge-codigo <= 59
             /*   AND ITEM.it-codigo = "501771" */
              NO-LOCK,
    EACH item-ext OF ITEM:

    ASSIGN c-ncm-est = "00000000"
           c-ncm-bco = "00000000"
           c-ncm-tto = "00000000".

    IF item-ext.indigo = YES OR
       SUBSTR(ITEM.it-codigo,6,1) = "0" THEN DO: /* Cru ou Indigo */
       
    END.
    ELSE DO:
       FIND FIRST ref-item WHERE ref-item.it-codigo = ITEM.it-codigo
                             AND SUBSTR(ref-item.cod-refer,7,1) <> "0"
                           NO-LOCK NO-ERROR.
       IF AVAIL ref-item THEN DO:
          FIND ref-item-ext WHERE ref-item-ext.it-codigo = ref-item.it-codigo
                              AND ref-item-ext.cod-refer = ref-item.cod-refer
                            NO-LOCK.
          ASSIGN c-ncm-est = ref-item-ext.cod-ncm.
       END.
       
       FIND FIRST ref-item WHERE ref-item.it-codigo = ITEM.it-codigo
                             AND SUBSTR(ref-item.cod-refer,7,1) = "0"
                             AND (SUBSTR(ref-item.cod-refer,3,4) = "0003" OR
                                  SUBSTR(ref-item.cod-refer,3,4) = "0101" OR
                                  SUBSTR(ref-item.cod-refer,3,4) = "0102" OR
                                  SUBSTR(ref-item.cod-refer,3,4) = "0103" OR
                                  SUBSTR(ref-item.cod-refer,3,4) = "0104" OR
                                  SUBSTR(ref-item.cod-refer,3,4) = "0105" OR
                                  SUBSTR(ref-item.cod-refer,3,4) = "0106" OR
                                  SUBSTR(ref-item.cod-refer,3,4) = "0107" OR
                                  SUBSTR(ref-item.cod-refer,3,4) = "0108")
                           NO-LOCK NO-ERROR.
       IF AVAIL ref-item THEN DO:
          FIND ref-item-ext WHERE ref-item-ext.it-codigo = ref-item.it-codigo
                              AND ref-item-ext.cod-refer = ref-item.cod-refer
                            NO-LOCK.
          ASSIGN c-ncm-bco = ref-item-ext.cod-ncm.
       END.

       FIND FIRST ref-item WHERE ref-item.it-codigo = ITEM.it-codigo
                             AND SUBSTR(ref-item.cod-refer,7,1) = "0"
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0003" 
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0101" 
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0102" 
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0103" 
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0104" 
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0105" 
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0106" 
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0107" 
                             AND SUBSTR(ref-item.cod-refer,3,4) <> "0108"
                           NO-LOCK NO-ERROR.
       IF AVAIL ref-item THEN DO:
          FIND ref-item-ext WHERE ref-item-ext.it-codigo = ref-item.it-codigo
                              AND ref-item-ext.cod-refer = ref-item.cod-refer
                            NO-LOCK.
          ASSIGN c-ncm-tto = ref-item-ext.cod-ncm.
       END.
    END.
    
    IF c-ncm-est = "00000000" THEN
       ASSIGN c-ncm-est = ITEM.class-fiscal.
    IF c-ncm-bco = "00000000" THEN
       ASSIGN c-ncm-bco = ITEM.class-fiscal.
    IF c-ncm-tto = "00000000" THEN
       ASSIGN c-ncm-tto = ITEM.class-fiscal.

    ASSIGN item-ext.cod-ncm-est = c-ncm-est
           item-ext.cod-ncm-bco = c-ncm-bco
           item-ext.cod-ncm-tto = c-ncm-tto.
END.
