DEF VAR i-ct AS INT.
DEF VAR i-embal AS INT.
DEF VAR i-tp-embal AS INT.

FOR EACH bc-etiqueta WHERE
         bc-etiqueta.cod-estado = 2 NO-LOCK.

    FIND ob-etiqueta WHERE
         ob-etiqueta.progressivo = bc-etiqueta.progressivo NO-ERROR.

    IF AVAIL ob-etiqueta THEN DO.
       ASSIGN ob-etiqueta.situacao = 3.
       NEXT. 
    END.

    IF AMBIGUOUS ob-etiqueta THEN NEXT.

    ASSIGN i-ct = i-ct + 1.

    FIND ITEM WHERE
         ITEM.it-codigo = bc-etiqueta.it-codigo NO-LOCK NO-ERROR.

    ASSIGN i-tp-embal = 1.
    IF ITEM.un = 'kg' THEN 
       ASSIGN i-tp-embal = 5.

    FIND corte-comerc WHERE
         corte-comerc.compr-min <= bc-etiqueta.qt-item AND
         corte-comerc.compr-max >= bc-etiqueta.qt-item AND
         corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

    CREATE ob-etiqueta.
    ASSIGN ob-etiqueta.cod-estabel     = bc-etiqueta.cod-estabel
           ob-etiqueta.dt-emissao      = TODAY
           ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
           ob-etiqueta.acondic         = ""
           ob-etiqueta.it-codigo       = bc-etiqueta.it-codigo
           ob-etiqueta.cod-refer       = bc-etiqueta.referencia
           ob-etiqueta.nr-lote         = IF bc-etiqueta.lote = '888'
                                         THEN 'RD' ELSE 'RP'
           ob-etiqueta.cod-qualid      = IF bc-etiqueta.lote = '888'
                                         THEN 'D' ELSE 'B'
           ob-etiqueta.corte-comerc    = IF AVAIL corte-comerc
                                         THEN corte-comerc.codigo
                                         ELSE ''
           ob-etiqueta.localizacao     = ''
           ob-etiqueta.situacao        = 3
           ob-etiqueta.quantidade      = bc-etiqueta.qt-item
           ob-etiqueta.num-etiqueta    = NEXT-VALUE(seq-etq-estoq-med)
           ob-etiqueta.progressivo     = bc-etiqueta.progressivo.

    DISP i-ct.
    PAUSE 0.

END.

