DEFINE VARIABLE cItem AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRef AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iContainer AS INTEGER     NO-UNDO.
UPDATE cItem cref iContainer.    
FOR EACH ped-item
    WHERE  ped-item.it-codigo = cItem
    AND    ped-item.cod-refer = cRef
    AND ped-item.cod-sit-item = 1.
    FIND FIRST ped-venda OF ped-item NO-LOCK NO-ERROR.
    FIND FIRST ped-venda-ext
        WHERE ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda-ext THEN NEXT.
    IF ped-venda-ext.nr-container <> iContainer THEN NEXT.
    DISP ped-item.qt-pedida ped-item.nr-pedcli .
END.

FOR EACH itens_ped_web
    WHERE itens_ped_web.it_codigo = cItem
    AND   itens_ped_web.cod_refer = cRef,
    EACH peds_web 
    WHERE peds_web.ped_web_id = itens_ped_web.ped_web_id
    AND   lookup(string(peds_web.ind_sit_ped_web),'1,2,5') > 0
    AND   peds_web.nr_container = icontainer:
    DISP itens_ped_web.qt_pedida itens_ped_web.ped_web_id peds_web.ind_sit_ped_web.


END.


