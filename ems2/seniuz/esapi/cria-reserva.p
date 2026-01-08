DEF INPUT PARAMETER p-row-ped-item  AS ROWID.
DEF INPUT PARAMETER p-row-ob-etiqueta AS ROWID.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

FIND ped-item WHERE
     ROWID(ped-item) = p-row-ped-item NO-LOCK NO-ERROR.

FIND ped-venda OF ped-item NO-LOCK NO-ERROR.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido
     SHARE-LOCK NO-ERROR.

IF NOT AVAIL ped-venda-ext THEN DO.
   MESSAGE ped-venda.nr-pedcli SKIP ' 3-pedido com erro'
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  NEXT.
END.


ASSIGN ped-venda-ext.qt-fardos = ped-venda-ext.qt-fardos + 1.

FIND ob-etiqueta WHERE
     ROWID(ob-etiqueta) = p-row-ob-etiqueta SHARE-LOCK NO-ERROR.
ASSIGN ob-etiqueta.situacao = 4
       ob-etiqueta.ob-origem = "".

FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.

FIND ped-item-res WHERE
     ped-item-res.cod-estabel  = ped-venda.cod-estabel  AND 
     ped-item-res.nome-abrev   = ped-venda.nome-abrev   AND
     ped-item-res.nr-pedcli    = ped-venda.nr-pedcli    AND
     ped-item-res.nr-sequencia = ped-item.nr-sequencia
     SHARE-LOCK NO-ERROR.    
        
IF NOT AVAIL ped-item-res THEN DO.
   CREATE ped-item-res.
   ASSIGN ped-item-res.cod-estabel  = ped-venda.cod-estabel
          ped-item-res.nome-abrev   = ped-venda.nome-abrev
          ped-item-res.nr-pedcli    = ped-venda.nr-pedcli
          ped-item-res.nr-sequencia = ped-item.nr-sequencia
          ped-item-res.it-codigo    = ped-item.it-codigo
          ped-item-res.cod-refer    = ped-item.cod-refer
          ped-item-res.nome-transp  = ped-venda.nome-transp
          ped-item-res.sigla-emb    = ob-etiqueta.nr-lote
          ped-item-res.dt-trans     = TODAY
          ped-item-res.hr-trans     = STRING(TIME,"HH:MM:SS") + " Separa‡ao Autom tica esapi/cria-reserva.w " + c-seg-usuario
          ped-item-res.lote         = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer.
END.
ASSIGN ped-item-res.qt-pedida = ped-item-res.qt-pedida + ob-etiqueta.quantidade. 

FIND CURRENT ped-item-res NO-LOCK NO-ERROR.

CREATE ped-item-rom.
ASSIGN ped-item-rom.cod-estabel = ped-venda.cod-estabel
       ped-item-rom.nome-abrev = ped-item.nome-abrev
       ped-item-rom.nr-pedcli = ped-item.nr-pedcli
       ped-item-rom.nr-sequencia = ped-item.nr-sequencia
       ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
       ped-item-rom.nr-ob = ob-etiqueta.nr-ob
       ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia
       ped-item-rom.quantidade = ob-etiqueta.quantidade.

RETURN 'ADM-OK'.
