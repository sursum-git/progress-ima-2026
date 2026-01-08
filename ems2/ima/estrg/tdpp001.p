TRIGGER PROCEDURE FOR DELETE OF pp-container.

IF pp-container.situacao = 3 THEN DO:
   MESSAGE "Container j† Fechado.Situaá∆o n∆o permite exclus∆o!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN ERROR.
END.   
FIND FIRST pp-ped-venda OF pp-container NO-LOCK NO-ERROR.
IF AVAIL pp-ped-venda THEN DO:
   MESSAGE "Container Possui Pedidos de Venda.Exclus∆o n∆o permitida!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN ERROR.
END.

FOR EACH pp-it-container OF pp-container.
     DELETE pp-it-container.
END.


