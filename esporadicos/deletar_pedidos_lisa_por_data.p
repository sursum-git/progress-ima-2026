FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >= 01.01.2024.
    FIND pedidos_lisa 
        WHERE pedidos_lisa.nr_pedido = int(nota-fiscal.nr-pedcli)
        NO-ERROR.
    IF AVAIL pedidos_lisa THEN DO:
        DISP pedidos_lisa.nr_pedido. PAUSE 0 .
       FOR EACH itens_pedido_lisa
           WHERE itens_pedido_lisa.pedido_lisa_id = pedidos_lisa.pedido_lisa_id:
           FOR EACH etq_item_pedido_lisa
               WHERE etq_item_pedido_lisa.ITEM_pedido_lisa_id = itens_pedido_lisa.ITEM_pedido_lisa_id :
               DELETE etq_item_pedido_lisa.
           END.
           DELETE itens_pedido_lisa.
       END.
       DELETE pedidos_lisa.
    END.                   
END.
