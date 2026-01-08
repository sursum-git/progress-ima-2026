OUTPUT TO PRINTER.
PUT "PECAS RESERVADAS NÇO LIDAS NO INVENTARIO"
     SKIP.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 4 AND
         ob-etiqueta.dt-emissao <= 12.31.2006 NO-LOCK
         BY ob-etiqueta.localiz.

    FIND inv-acab WHERE
         inv-acab.data-invent = 12.31.2006 AND
         inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta
         NO-LOCK NO-ERROR.

    IF NOT AVAIL inv-acab THEN
       DISP ob-etiqueta.num-etiqueta
            ob-etiqueta.localizacao
            ob-etiqueta.it-codigo
            ob-etiqueta.cod-refer
            ob-etiqueta.nr-lote
            ob-etiqueta.quantidade
            WITH WIDTH 550.
END.




