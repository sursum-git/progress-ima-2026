TRIGGER PROCEDURE FOR DELETE OF mp-entr-cam.
IF mp-entr-cam.tipo-mov = 2 THEN DO. /* DESCARGA DE ALGOD«O */
   FIND FIRST mp-fardo WHERE mp-fardo.nr-cdr = mp-entr-cam.nr-cdr NO-LOCK NO-ERROR.
   IF AVAIL mp-fardo THEN DO:
      MESSAGE "Exclus∆o n∆o permitida - Existem Fardos para esta movimentaá∆o...."  VIEW-AS ALERT-BOX.
      RETURN ERROR.
   END.
END.
