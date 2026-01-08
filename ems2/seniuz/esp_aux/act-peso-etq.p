DEF VAR de-media-peso AS DEC.
DEF VAR de-peso-calc AS DEC.

DEF VAR i-ct AS INT.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 4 AND
         ob-etiqueta.peso-bruto = 0
         NO-LOCK. 
/*
    FIND ITEM WHERE
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
    FIND item-ext WHERE
         item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    ASSIGN de-media-peso = 0.
    FIND corte-comerc WHERE
         corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
    IF item-ext.indigo THEN
       ASSIGN de-media-peso = corte-comerc.peso-emb-indigo.
    ELSE
       ASSIGN de-media-peso = corte-comerc.peso-emb-outros.

    ASSIGN de-peso-calc = (ITEM.peso-liquido * ob-etiqueta.quantidade) + de-media-peso.

    ASSIGN ob-etiqueta.peso-bruto = de-peso-calc.
  */
    ASSIGN i-ct = i-ct + 1.
    
END.

DISP i-ct.
