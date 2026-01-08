FOR EACH versoes_item_custo_container
WHERE container_id = 184518:
    FOR EACH ITEM_container_custos
        WHERE ITEM_container_custos.versao_id = versoes_item_custo_container.versao_id.
        DELETE ITEM_container_custos.
    END.
     FOR EACH ITEM_container_custo_despesas
        WHERE ITEM_container_custo_despesas.versao_id = versoes_item_custo_container.versao_id.
        DELETE ITEM_container_custo_despesas.
    END.
     FOR EACH ITEM_container_custo_descontos
        WHERE ITEM_container_custo_descontos.versao_id = versoes_item_custo_container.versao_id.
        DELETE ITEM_container_custo_descontos.
    END.
     FOR EACH ITEM_container_custo_pgtos
        WHERE ITEM_container_custo_pgtos.versao_id = versoes_item_custo_container.versao_id.
        DELETE ITEM_container_custo_pgtos.
    END.
     FOR EACH ITEM_container_custo_nfs
        WHERE ITEM_container_custo_nfs.versao_id = versoes_item_custo_container.versao_id.
        DELETE ITEM_container_custo_nfs.
    END.
DELETE versoes_item_custo_container.
END.
