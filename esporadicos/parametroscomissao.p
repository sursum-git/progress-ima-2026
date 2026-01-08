
DEFINE TEMP-TABLE tt_param_estab_comis LIKE PARAM_estab_comis.

FOR EACH PARAM_estab_comis WHERE cod_empresa = '100':
    CREATE tt_param_Estab_comis.
    BUFFER-COPY param_estab_comis TO tt_param_estab_comis.
    ASSIGN tt_param_estab_comis.cod_empresa = '600'
           tt_param_estab_comis.cod_estab = '601'.
    
END.

FOR EACH tt_param_estab_comis:
    CREATE PARAM_estab_comis.
    BUFFER-COPY tt_PARAM_estab_comis TO PARAM_estab_comis.
END.
