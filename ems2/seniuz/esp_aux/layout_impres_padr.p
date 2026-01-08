FOR EACH layout_impres_padr WHERE layout_impres_padr.cod_proced = "*"
                              AND layout_impres_padr.cod_layout_impres = "Padrao_080_R".
    DISP layout_impres_padr.cod_usuario
         layout_impres_padr.nom_impressora
         layout_impres_padr.cod_layout_impres.
    UPDATE layout_impres_padr.cod_layout_impres.
END.
