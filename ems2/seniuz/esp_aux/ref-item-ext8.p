FOR EACH ref-item-ext NO-LOCK BREAK BY ref-item-ext.cod-obsoleto.
    
    ACCUMULATE ref-item-ext.it-codigo (COUNT BY ref-item-ext.cod-obsoleto).

    IF LAST-OF(ref-item-ext.cod-obsoleto) THEN
       DISP ref-item-ext.cod-obsoleto
            (ACCUM COUNT BY ref-item-ext.cod-obsoleto ref-item-ext.it-codigo).
END.

