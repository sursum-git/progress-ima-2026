FOR EACH hist_corte_separacao
    WHERE num_etiqueta = 209306 :
    //DISP hist_corte_separacao EXCEPT historico WITH 1 COL WIDTH 550.
    FOR EACH etiqueta_lisa
        WHERE etiqueta_lisa.num_etiqueta = hist_corte_separacao.num_etiqueta:
        DISP etiqueta_lisa WITH 1 COL WIDTH 550.
    END.
END.
