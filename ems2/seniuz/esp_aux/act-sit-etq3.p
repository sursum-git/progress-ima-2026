DEF VAR i-ct AS INT.
OUTPUT TO "M:\EMS206\especificos\Seniuz\Seguran‡a\etq-sem-saldo.txt".
FOR EACH saldo-estoq WHERE
         saldo-estoq.cod-estab = '5' AND
         saldo-estoq.cod-depos = 'ARM' AND
         saldo-estoq.qtidade-atu = 0 NO-LOCK,
    FIRST ITEM OF saldo-estoq WHERE
          ITEM.ge-codigo >= 50 AND
          ITEM.ge-codigo <= 60 NO-LOCK
    BY saldo-estoq.it-codigo
    BY saldo-estoq.cod-refer
    BY saldo-estoq.lote.

    FOR EACH bc-etiqueta WHERE
             bc-etiqueta.it-codigo = saldo-estoq.it-codigo AND
             bc-etiqueta.referencia = saldo-estoq.cod-refer AND
             bc-etiqueta.lote = saldo-estoq.lote AND
             bc-etiqueta.cod-estado = 2 SHARE-LOCK.

        DISP bc-etiqueta.progressivo
             bc-etiqueta.qt-item
             WITH WIDTH 550.

        PAUSE 0.
        ASSIGN bc-etiqueta.cod-estado = 9.
    END.
END.

OUTPUT CLOSE.



