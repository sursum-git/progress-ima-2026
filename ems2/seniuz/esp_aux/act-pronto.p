DEF TEMP-TABLE tt-reporte
    FIELD it-codigo    LIKE ord-prod.it-codigo
    FIELD cod-refer    LIKE ord-prod.cod-refer
    FIELD nr-lote      LIKE ord-prod.lote
    FIELD dt-emissao   LIKE ord-prod.dt-emissao
    FIELD qtd-acabado  LIKE ord-prod.qt-produzida
    FIELD qtd-retalho  LIKE ord-prod.qt-produzida
    FIELD rw-etiquetas AS CHAR
    INDEX ch-item IS PRIMARY it-codigo cod-refer nr-lote dt-emissao.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.dt-emissao   >= 09.01.2005  AND  
         ob-etiqueta.dt-emissao   <= 09.05.2005  AND  
         ob-etiqueta.situacao      = 2 NO-LOCK,
    FIRST ordem-benefic OF ob-etiqueta NO-LOCK.

    FIND FIRST tt-reporte WHERE
               tt-reporte.it-codigo = ordem-benefic.it-codigo AND
               tt-reporte.cod-refer = ordem-benefic.cod-refer AND
               tt-reporte.nr-lote   = ob-etiqueta.nr-lote + ordem-benefic.cod-refer AND
               tt-reporte.dt-emissao = ob-etiqueta.dt-emissao 
               NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-reporte THEN DO.
       CREATE tt-reporte.
       ASSIGN tt-reporte.it-codigo = ordem-benefic.it-codigo 
              tt-reporte.cod-refer = ordem-benefic.cod-refer
              tt-reporte.nr-lote   = ob-etiqueta.nr-lote + ordem-benefic.cod-refer
              tt-reporte.dt-emissao = ob-etiqueta.dt-emissao.
              
    END.
    ASSIGN tt-reporte.rw-etiquetas = IF tt-reporte.rw-etiquetas = ""
                                     THEN STRING(ROWID(ob-etiqueta))  
                                     ELSE tt-reporte.rw-etiquetas + ";" + STRING(ROWID(ob-etiqueta))
          tt-reporte.qtd-acabado = tt-reporte.qtd-acabado + ob-etiqueta.quantidade.
END.

FOR EACH tt-reporte.
    FIND ref-item-ext WHERE 
         ref-item-ext.it-codigo = tt-reporte.it-codigo AND 
         ref-item-ext.cod-refer = tt-reporte.cod-refer NO-ERROR.

    IF AVAIL ref-item-ext THEN DO.
       IF ref-item-ext.qtd-pron > tt-reporte.qtd-acabado THEN 
          ASSIGN ref-item-ext.qtd-pron = ref-item-ext.qtd-pron - tt-reporte.qtd-acabado.
       ELSE
          ASSIGN ref-item-ext.qtd-pron = 0.
    END.
END.

