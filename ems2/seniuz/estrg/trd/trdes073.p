TRIGGER PROCEDURE FOR DELETE OF ob-etq-trf.

FIND ob-trf WHERE ob-trf.num-trf = ob-etq-trf.num-trf NO-LOCK NO-ERROR.
IF AVAIL ob-trf AND ob-trf.situacao = 2 THEN DO:
   MESSAGE "Exclus∆o n∆o permitida ! Transformaá∆o esta revisada . . ."  VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.
FIND ob-etiqueta WHERE ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-ERROR.
IF AVAIL ob-etiqueta THEN
   ASSIGN ob-etiqueta.situacao = 3. /* Volta Etiqueta para o Estoque */
