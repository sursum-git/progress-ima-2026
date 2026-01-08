TRIGGER PROCEDURE FOR DELETE OF mp-tipo.
FIND FIRST mp-fardo WHERE mp-fardo.cd-tipo = mp-tipo.codigo
                    NO-LOCK NO-ERROR.
IF AVAIL mp-fardo THEN DO:
   MESSAGE "Tipo est  sendo usado em Fardo. NÆo pode ser eliminado."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN ERROR.
END.

