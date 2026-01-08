DEF NEW GLOBAL SHARED VAR wh-coligada AS WIDGET-HANDLE NO-UNDO.

DEF VAR i-ct AS INTEGER.

IF wh-coligada:SCREEN-VALUE <> '' THEN DO.
   DO i-ct = 1 TO NUM-ENTRIES(wh-coligada:SCREEN-VALUE).
      FIND emitente WHERE
           emitente.nome-abrev = ENTRY(i-ct,wh-coligada:SCREEN-VALUE)
           NO-LOCK NO-ERROR.
      IF AVAIL emitente THEN DO.
         ASSIGN wh-coligada:SCREEN-VALUE = REPLACE(wh-coligada:SCREEN-VALUE,
                                                   ENTRY(i-ct,wh-coligada:SCREEN-VALUE),
                                                   STRING(emitente.cod-emit)).
      END.
      ELSE DO.
          FIND emitente WHERE
               emitente.cod-emitente = INTEGER(ENTRY(i-ct,wh-coligada:SCREEN-VALUE))
               NO-LOCK NO-ERROR.
          IF NOT AVAIL emitente THEN DO.
             MESSAGE 'Empresa Coligada ' ENTRY(i-ct,wh-coligada:SCREEN-VALUE) ' n∆o Cadastrada...'
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'ENTRY' TO wh-coligada.
             RETURN NO-APPLY.
          END.
      END.
   END.
END.

