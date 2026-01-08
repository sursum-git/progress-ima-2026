 DEFINE BUFFER bf FOR controle_preco.
 FOR EACH controle_preco
    WHERE controle_preco.campanha_id >= 5.
     DISP it_codigo.
    FOR EACH bf
        WHERE bf.it_codigo = controle_preco.it_codigo
        AND bf.tp_preco = 3
        AND bf.cod_refer <> ''.
        DISP bf.cod_refer. 
        ASSIGN bf.LOG_vencido = YES
               bf.dt_hr_alteracao = NOW
               bf.cod_usuario_alteracao = 'rrocha'.
    END.
END.
