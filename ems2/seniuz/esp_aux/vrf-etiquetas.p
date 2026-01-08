/* Incia processo de reporte */
FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao     >= 3 AND
         ob-etiqueta.nr-reporte    = 0 AND
         ob-etiqueta.quantidade    > 0 NO-LOCK
         BY ob-etiqueta.dt-emis
         BY ob-etiqueta.hr-emis.

    IF ob-etiqueta.tipo-ordem = 4 THEN NEXT.

    DISP ob-etiqueta.num-etiqueta
         ob-etiqueta.it-codigo
         ob-etiqueta.nr-lote
         ob-etiqueta.dt-emiss
         ob-etiqueta.hr-emis FORMAT "x(5)" COLUMN-LABEL 'Hora'
         ob-etiqueta.quantidade (TOTAL)
         WITH WIDTH 550.

END.

