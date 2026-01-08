DEF VAR i-ct AS INT.
DEF VAR de-tot AS DEC.
FOR EACH ob-etiqueta WHERE
         ob-etiqueta.localiz <> '' NO-LOCK.
    ASSIGN i-ct = i-ct + 1
           de-tot = de-tot + ob-etiqueta.quantidade.
END.

DISP i-ct de-tot.
