DEF VAR c-nr-pedcli AS CHAR.
DEF VAR c-pre-pedido AS CHAR.

ASSIGN c-nr-pedcli = '318994'
       c-pre-pedido = '018084'.

FOR EACH lisa-integra WHERE
         lisa-integra.cod-trans = 'ISF' AND
         lisa-integra.chave BEGINS c-nr-pedcli SHARE-LOCK.

     ASSIGN lisa-integra.val-livre-1 = c-pre-pedido.

     FIND ped-venda WHERE
          ped-venda.nr-pedcli = ENTRY(1,lisa-integra.chave,"|") AND
          ped-venda.nome-abrev = ENTRY(2,lisa-integra.chave,"|") NO-LOCK NO-ERROR.

     FIND ped-venda-ext WHERE
          ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
          ped-venda-ext.nr-pedido = ped-venda.nr-pedido
          SHARE-LOCK NO-ERROR.
     IF AVAIL ped-venda-ext THEN
        ASSIGN ped-venda-ext.dt-isf = TODAY
               ped-venda-ext.nr-pedext = c-pre-pedido.

END.
