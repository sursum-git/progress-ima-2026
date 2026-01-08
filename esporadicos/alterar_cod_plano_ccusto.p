FOR EACH ITEM_lancto_ctbl
    WHERE cod_plano_ccusto = ''
    AND   cod_ccusto <> ''
    AND   cod_empresa = '100':
    DISP ITEM_lancto_ctbl.dat_lancto_ctbl
         ITEM_lancto_ctbl.cod_cta_ctbl .
    ASSIGN cod_plano_ccusto = 'ima'.
   

END.
