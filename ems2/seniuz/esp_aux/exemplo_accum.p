DEF VAR i-ct AS INT.
FOR EACH ped-item-res WHERE ped-item-res.it-codigo BEGINS "5"
                      NO-LOCK
                      BREAK BY ped-item-res.it-codigo
                            BY ped-item-res.cod-refer:
    
    ACCUMULATE ped-item-res.qt-pedida (TOTAL BY ped-item-res.cod-refer).
    ACCUMULATE ped-item-res.qt-pedida (AVERAGE BY ped-item-res.cod-refer).
    ACCUMULATE ped-item-res.qt-pedida (MAXIMUM BY ped-item-res.cod-refer).
    ACCUMULATE ped-item-res.qt-pedida (MINIMUM BY ped-item-res.cod-refer).
    ACCUMULATE ped-item-res.cod-refer (COUNT BY ped-item-res.cod-refer).
    ASSIGN i-ct = i-ct + 1.


    IF LAST-OF(ped-item-res.cod-refer) THEN DO.
       DISPLAY ped-item-res.it-codigo FORMAT "x(6)"
               ped-item-res.cod-refer 
               (ACCUM COUNT BY ped-item-res.cod-refer ped-item-res.cod-refer)
               i-ct LABEL "MANUAL"
               (ACCUM TOTAL BY ped-item-res.cod-refer ped-item-res.qt-pedida)
               (ACCUM AVERAGE BY ped-item-res.cod-refer ped-item-res.qt-pedida)
               (ACCUM MAXIMUM BY ped-item-res.cod-refer ped-item-res.qt-pedida)
               (ACCUM MINIMUM BY ped-item-res.cod-refer ped-item-res.qt-pedida)
        WITH 1 COL WIDTH 550.
       ASSIGN i-ct = 0.
    END.
END.
