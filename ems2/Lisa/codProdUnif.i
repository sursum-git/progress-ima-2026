DEFINE VARIABLE lCodigoProdUnificado  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE paramCodProdUnificado AS CHARACTER   NO-UNDO.

ASSIGN paramCodProdUnificado = getCodigoProdutoUnificadoLisa().   
ASSIGN lCodigoProdUnificado =  IF paramCodProdUnificado  = '1' THEN YES ELSE NO.
