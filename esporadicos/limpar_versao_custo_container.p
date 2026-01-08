DEFINE VARIABLE iContainer AS INTEGER     NO-UNDO.
UPDATE iContainer.

FOR EACH pp-container 
  WHERE nr-container = iContainer:
    MESSAGE nr-container
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    FOR EACH versoes_item_custo_container
        WHERE versoes_item_custo_container.container_id = pp-container.nr-container:
        FOR EACH ITEM_container_custos OF versoes_item_custo_container .
            DELETE ITEM_container_custos.
        END.
        FOR EACH ITEM_container_custo_descontos OF versoes_item_custo_container.
            DELETE ITEM_container_custo_descontos.
        END.

        FOR EACH ITEM_container_custo_despesas OF versoes_item_custo_container.
            DELETE ITEM_container_custo_despesas.
        END.
        FOR EACH ITEM_container_custo_nfs OF versoes_item_custo_container.
            DELETE ITEM_container_custo_nfs.
        END.
        FOR EACH ITEM_container_custo_pgtos OF versoes_item_custo_container.
            DELETE ITEM_container_custo_pgtos.
        END.
        DELETE versoes_item_custo_container.
    END.
END.
