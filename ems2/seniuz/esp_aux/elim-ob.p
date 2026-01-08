DISABLE TRIGGERS FOR LOAD OF ordem-benefic.

FIND ordem-benefic WHERE
     ordem-benefic.nr-ob = 97150 AND
     ordem-benefic.nr-carro = '09H'.

FIND FIRST ob-etiqueta OF ordem-benefic
     WHERE ob-etiqueta.situacao <> 1 NO-LOCK NO-ERROR.
IF AVAIL ob-etiqueta THEN DO:
   MESSAGE "Ordem possui etiqueta. N∆o pode ser eliminada."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN ERROR.
END.

IF ordem-benefic.situacao <> 1 THEN DO.
   MESSAGE "Situaá∆o da OB, n∆o permite eliminaá∆o...."  VIEW-AS ALERT-BOX.
   RETURN ERROR.
END.

FOR EACH ob-acondic OF ordem-benefic.
    DELETE ob-acondic.
END.

FOR EACH ob-etiqueta OF ordem-benefic.
    DELETE ob-etiqueta.
END.


DELETE ordem-benefic.     
