
FOR EACH controle_preco
    WHERE dt_final = ?.
    ASSIGN dt_final = 12.31.9999 .
END.
FOR EACH controle_preco
    WHERE tb_preco_id = 0.
    ASSIGN tb_preco_id = 1.
END.

FOR EACH controle_preco:
    IF controle_preco.cod_refer <> ''  THEN
       ASSIGN num_nivel = 2.
    ELSE
      ASSIGN num_nivel = 1.
END.

FOR EACH controle_preco
    WHERE /*controle_preco.nr_container = 224220
    AND controle_preco.it_codigo = '525083'
    AND*/  controle_preco.vl_real = 0
    AND controle_preco.vl_dolar = 0 .
    //DISP controle_preco WITH 1 COL WIDTH 550. PAUSE 0.
    DELETE controle_preco.
END.
