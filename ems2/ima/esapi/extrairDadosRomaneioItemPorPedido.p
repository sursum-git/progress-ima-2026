{esapi/extrairDadosRomaneioItemPorPedido.i}

DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pItem     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRef      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttEtq.


FIND ped-venda NO-LOCK
    WHERE ped-venda.nr-pedido  = pNrPedido NO-ERROR.
IF NOT AVAIL ped-venda THEN DO:
   RETURN 'nok'.
END.

FOR EACH ped-item-res NO-LOCK
  WHERE ped-item-res.nome-abrev     = ped-venda.nome-abrev 
  AND   ped-item-res.nr-pedcli      = ped-venda.nr-pedcli  
  AND   ped-item-res.cod-estabel    = ped-venda.cod-estabel
  AND   ped-item-res.it-codigo      = pItem
  AND   ped-item-res.cod-refer      = pRef .

   FOR EACH ped-item-rom WHERE
       ped-item-rom.nome-abrev   = ped-venda.nome-abrev AND
       ped-item-rom.nr-pedcli    = ped-venda.nr-pedcli  AND
       ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
      FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
              ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
              NO-LOCK NO-ERROR.
      IF AVAIL ob-etiqueta THEN DO:
         CREATE ttEtq.
         ASSIGN ttEtq.numEtq        = ob-etiqueta.num-etiqueta
                ttEtq.quantidade    = ob-etiqueta.quantidade.
         RUN esapi/getDescrSitEtq.p(ob-etiqueta.situacao, OUTPUT ttEtq.descrSit).
      END.  
   END.
END.     
OUTPUT TO c:\temp\ttEtq.txt.
FOR EACH ttEtq.
    DISP ttEtq.
END.

OUTPUT CLOSE.
