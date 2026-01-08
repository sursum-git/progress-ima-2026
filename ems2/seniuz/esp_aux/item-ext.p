DEF VAR de-fator AS DEC.
FOR EACH item-ext WHERE item-ext.it-codigo BEGINS "5" NO-LOCK.
    FIND ITEM WHERE ITEM.it-codigo = item-ext.it-codigo NO-LOCK.
    IF item.un <> "m" THEN DO:
       ASSIGN de-fator = ROUND(1 / ITEM.peso-liquido,4).
       DISP item.it-codigo
            ITEM.un
            ITEM.peso-liquido
            item-ext.fator-conv
            de-fator
            item-ext.fator-conv = de-fator.
    END.
END.
