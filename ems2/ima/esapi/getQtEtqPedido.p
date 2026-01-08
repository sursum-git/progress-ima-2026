/*****************************************************************
programa:esapi/getQtEtqPedido.p
objetivo: retornar a soma das quantidades das etiquetas romaneadas
para o pedido passado por parametro


*****************************************************************/

DEFINE INPUT  PARAMETER pNrPedido  AS INT         NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER qtAlocada  AS DECIMAL     NO-UNDO.

FIND ped-venda NO-LOCK
     WHERE  ped-venda.nr-pedido   = pNrPedido  NO-ERROR.
IF NOT AVAIL ped-venda THEN RETURN 'nok'.
FOR EACH ped-item-res NO-LOCK
    WHERE ped-item-res.cod-estabel = ped-venda.cod-estabel
    AND   ped-item-res.nome-abrev  = ped-venda.nome-abrev
    AND   ped-item-res.nr-pedcli   = ped-venda.nr-pedcli
    AND   ped-item-res.it-codigo   = pitCodigo
    AND   ped-item-res.cod-refer   = pCodRefer .
    FOR EACH ped-item-rom no-lock
        WHERE ped-item-rom.nome-abrev =  ped-item-res.nome-abrev
        AND   ped-item-rom.nr-pedcli  =  ped-item-res.nr-pedcli
        AND   ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia.
        FIND ob-etiqueta NO-LOCK
             WHERE ob-etiqueta.cod-estabel  = ped-venda.cod-estabel
             AND   ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
             NO-ERROR.
        IF AVAIL ob-etiqueta THEN DO:
           ASSIGN qtAlocada = qtAlocada + ob-etiqueta.quantidade .
        END.                                                      
    END.
END.



         
