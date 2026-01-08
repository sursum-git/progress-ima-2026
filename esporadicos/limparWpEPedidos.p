FOR EACH wp:
    DELETE wp.
END.
FOR EACH wp_nota_fiscal:
    DELETE wp_nota_fiscal.
END.
FOR EACH  wp_estoque_preco:
    DELETE wp_estoque_preco.
END.

FOR EACH peds_web:
    FOR EACH itens_ped_web
        WHERE itens_ped_web.ped_web_id =  peds_web.ped_web_id :
        DELETE itens_ped_web.
    END.
    DELETE peds_web.
END.
