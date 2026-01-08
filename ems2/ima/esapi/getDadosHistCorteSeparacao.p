/**************************************************************************** 
programa:esapi/getDadosHistCorteSeparacao.p
objetivo:Retornar a tabela temporaria filtrando pelo numero de pedido
autor: tadeu silva
data: 09/2025
***************************************************************************/
{esapi/getDadosHistCorteSeparacao.i}
DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR   ttLog . 


FUNCTION getDescrTipo RETURNS CHAR(pTipo AS INT):

CASE pTipo:
    WHEN 1 THEN DO:
        RETURN 'Inclus∆o'.
    END.
    WHEN 2 THEN DO:
        RETURN 'Alteraá∆o'.        
    END.
    WHEN 3 THEN DO:
        RETURN 'Erro - Falta de Etiqueta'.        
    END.
    WHEN 4 THEN DO:
        RETURN 'Desconsiderado - Etiqueta j† Criada'.        
    END.
    WHEN 5 THEN DO:
        RETURN 'Desconsiderado - Etiqueta j† Ajustada'.        
    END.

END CASE.

    


END FUNCTION.



FOR EACH hist_corte_separacao NO-LOCK
    WHERE hist_corte_separacao.nr_pedido = pNrPedido:
    CREATE ttLog.
    BUFFER-COPY hist_corte_separacao TO ttLog.
    ASSIGN ttLog.descTipo =  getDescrTipo(hist_corte_separacao.num_tipo).
    
END.


 

