FOR EACH itens_pedido_lisa
    WHERE itens_pedido_lisa.nf_origem = '' .
    DISP ITEM_pedido_lisa_id.
    FOR EACH etq_item_pedido_lisa
        WHERE etq_item_pedido_lisa.ITEM_pedido_lisa_id = itens_pedido_lisa.ITEM_pedido_lisa_id .
        DELETE etq_item_pedido_lisa.
    END.
    FIND pedidos_lisa
        WHERE pedidos_lisa.pedido_lisa_id = itens_pedido_lisa.pedido_lisa_id 
        NO-ERROR.
    IF AVAIL pedidos_lisa THEN DO:
       DELETE pedidos_lisa.
    END.
    DELETE itens_pedido_lisa. 
END.
