FOR EACH retornos_lisa
WHERE date(dt_hr_registro) >= 01.01.2026:
DISP retorno_lisa_id.
    FOR EACH itens_retorno_lisa 
        WHERE itens_retorno_lisa.nr_pedido = int(retornos_lisa.nr_pedido):
        DELETE itens_retorno_lisa.
    END.
    
    FOR EACH romaneios_retorno_lisa OF retornos_lisa:
        DELETE romaneios_retorno_lisa.
    END.
    DELETE retornos_lisa.
END.
