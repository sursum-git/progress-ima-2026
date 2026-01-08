FIND bc-etiqueta No-lock
     Where bc-etiqueta.progressivo = '11002031908' No-error.

FIND ob-etiqueta WHERE
     ob-etiqueta.progressivo = bc-etiqueta.progressivo
     NO-LOCK NO-ERROR.
IF NOT AVAIL ob-etiqueta THEN DO.
   RUN pi-cria-etiqueta.

   MESSAGE ob-etiqueta.num-etiqueta
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

PROCEDURE pi-cria-etiqueta.

    FIND ITEM WHERE
         ITEM.it-codigo = bc-etiqueta.it-codigo NO-LOCK NO-ERROR.

    FIND corte-comerc WHERE
         corte-comerc.codigo = 'Z' NO-LOCK NO-ERROR.

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
           ob-etiqueta.quantidade      = bc-etiqueta.qt-item
           ob-etiqueta.situacao        = 3
           ob-etiqueta.num-etiqueta    = NEXT-VALUE(seq-etq-estoq-ima)
           ob-etiqueta.progressivo     = bc-etiqueta.progressivo.
END PROCEDURE.

