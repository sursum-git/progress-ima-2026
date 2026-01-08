OUTPUT TO PRINTER.
FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 2 AND
         ob-etiqueta.quantidade > 0 AND
         ob-etiqueta.localiz <> ''.

    IF ob-etiqueta.dt-emissao = TODAY THEN NEXT.


    DISP ob-etiqueta.num-etiqueta
         ob-etiqueta.localiz
         ob-etiqueta.it-codigo
         ob-etiqueta.cod-refer
         ob-etiqueta.nr-lote.
END.
