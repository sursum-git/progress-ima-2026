OUTPUT TO PRINTER.
FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 2 NO-LOCK.

    IF ob-etiqueta.nr-lote BEGINS 'p' THEN NEXT.
    IF ob-etiqueta.nr-lote BEGINS 's' THEN NEXT.

    IF ob-etiqueta.dt-emis = TODAY THEN NEXT.

    DISP ob-etiqueta.num-etiqueta
         ob-etiqueta.dt-emis (COUNT).
         
END.
