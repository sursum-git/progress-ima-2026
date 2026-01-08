TRIGGER PROCEDURE FOR DELETE OF ob-trf.
IF ob-trf.situacao = 2 THEN DO. /*  REVISADO */
   MESSAGE "Exclus∆o n∆o permitida ! Transformaá∆o esta revisada . . ."  VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.

FOR EACH ob-etq-trf WHERE ob-etq-trf.num-trf = ob-trf.num-trf.
    FIND ob-etiqueta WHERE 
         ob-etiqueta.cod-estabel = ob-etq-trf.cod-estabel AND
         ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-ERROR.
    IF AVAIL ob-etiqueta THEN
       ASSIGN ob-etiqueta.situacao = 3. /* Volta Etiqueta para o Estoque */

    DELETE ob-etq-trf.
END.
