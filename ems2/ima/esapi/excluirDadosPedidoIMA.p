DEFINE INPUT  PARAMETER pNrPedido AS INTEGER    NO-UNDO.
    

FOR EACH pedidos_lisa EXCLUSIVE-LOCK
    WHERE pedidos_lisa.nr_pedido = pNrPedido .
    FOR EACH itens_pedido_lisa EXCLUSIVE-LOCK
        WHERE itens_pedido_lisa.pedido_lisa_id = pedidos_lisa.pedido_lisa_id.
        FOR EACH etq_item_pedido_lisa EXCLUSIVE-LOCK
            WHERE etq_item_pedido_lisa.ITEM_pedido_lisa_id = itens_pedido_lisa.ITEM_pedido_lisa_id.
            DELETE etq_item_pedido_lisa.
        END.
        RELEASE etq_item_pedido_lisa.
        DELETE itens_pedido_lisa.
    END.
    RELEASE itens_pedido_lisa.
    DELETE pedidos_lisa.
END.
RELEASE pedidos_lisa.


