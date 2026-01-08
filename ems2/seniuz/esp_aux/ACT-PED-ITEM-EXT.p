DEF VAR i-ct AS INT.
FIND ped-venda WHERE
     ped-venda.nr-pedcli = '120538' NO-LOCK NO-ERROR.
FOR EACH ped-item OF ped-venda.
    FIND ped-item-ext WHERE
         ped-item-ext.nome-abrev   = ped-item.nome-abrev   AND
         ped-item-ext.nr-pedcli    = ped-item.nr-pedcli    AND
         ped-item-ext.nr-sequencia = ped-item.nr-sequencia AND
         ped-item-ext.it-codigo    = ped-item.it-codigo    AND
         ped-item-ext.cod-refer    = ped-item.cod-refer NO-ERROR.

    IF NOT AVAIL ped-item-ext THEN DO:
       CREATE ped-item-ext.
       ASSIGN ped-item-ext.nome-abrev   = ped-item.nome-abrev   
              ped-item-ext.nr-pedcli    = ped-item.nr-pedcli    
              ped-item-ext.nr-sequencia = ped-item.nr-sequencia 
              ped-item-ext.it-codigo    = ped-item.it-codigo    
              ped-item-ext.cod-refer    = ped-item.cod-refer
              ped-item-ext.lote         = 'CA'
              ped-item-ext.corte-comerc = 'I'.
    END.

END.
