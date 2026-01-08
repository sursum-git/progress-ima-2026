TRIGGER PROCEDURE FOR CREATE OF pp-it-container.
/*N∆o permite exlcus∆o de itens do container no caso do container j† ~
estiver fechado*/
FIND pp-container OF pp-it-container NO-LOCK NO-ERROR.
IF AVAIL pp-container THEN DO:
   IF pp-container.situacao = 3  THEN DO:
      MESSAGE "Container Fechado. Situaá∆o do Container n∆o permite exclus∆o do item!! "
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN ERROR.
   END.
   ELSE DO:
       /*N∆o permite exclus∆o de itens do container no caso de haver pedidos de venda para esse item*/    
        FOR FIRST pp-ped-item WHERE
                  pp-ped-item.nr-container = pp-it-container.nr-container AND
                  pp-ped-item.it-codigo    = pp-it-container.it-comprado  AND
                  pp-ped-item.cod-refer    = pp-it-container.ref-comprada  NO-LOCK,
            FIRST pp-ped-venda WHERE
                  pp-ped-venda.nr-pedcli = pp-ped-item.nr-pedcli AND
                  pp-ped-venda.nome-abrev = pp-ped-item.nome-abrev AND
                  pp-ped-venda.cod-sit-ped = 1 NO-LOCK.
            IF AVAIL pp-ped-item THEN DO:
               MESSAGE "Item " pp-ped-item.it-codigo " x referància " pp-ped-item.cod-refer " possui pedidos de venda." SKIP
                        "Exclua primeiro os Pedidos de venda!"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
               RETURN ERROR.
            END.
        END.
   END.
END.


  
  
  


