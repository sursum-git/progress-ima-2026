FOR EACH fats_repres
    WHERE fats_repres.ano = 2024
    AND  fats_repres.mes  = 7.
    DELETE fats_repres.
END.

FOR EACH fats_repres_clientes
    WHERE fats_repres_clientes.ano = 2024
    AND  fats_repres_clientes.mes  = 7.
    DELETE fats_repres_clientes.
END.


FOR EACH fats_repres_clientes_prod
    WHERE fats_repres_clientes_prod.ano = 2024
    AND  fats_repres_clientes_prod.mes  = 7.
    DELETE fats_repres_clientes_prod.
END.


FOR EACH fats_repres_clientes_prod_data
    WHERE fats_repres_clientes_prod_data.data >= 07.01.2024
    AND  fats_repres_clientes_prod_data.data  <= 07.03.2024.
    DELETE fats_repres_clientes_prod_data.
END.


FOR EACH fats_04
    WHERE fats_04.data >= 07.01.2024
    AND  fats_04.data  <= 07.03.2024.
    
    DELETE fats_04.
END.



