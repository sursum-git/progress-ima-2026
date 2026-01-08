DEFINE TEMP-TABLE tt
    FIELD itCodigo AS CHAR.

/*FOR EACH referencia
    WHERE cod-refer >='l  ' 
    AND cod-refer <= 'lzz'.
    DISP cod-refer.
END.*/

INPUT FROM c:\temp\itens.txt.

REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";"  tt.
END.
INPUT CLOSE.

FOR EACH tt:
    FOR EACH controle_preco
        WHERE tp_preco = 3
    AND dt_final >= TODAY
    AND controle_preco.it_Codigo = tt.itCodigo
    AND (controle_preco.cod_refer = 'LED' OR
         controle_preco.cod_refer = 'LEG' ).
    ASSIGN LOG_vencido = YES.
    DISP it_codigo cod_refer.           





    END.
END.
