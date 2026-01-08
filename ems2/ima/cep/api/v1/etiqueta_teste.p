/*
programa: cep/v1/etiqueta.p
objetivo: API a ser utilizada para atualiza‡Æo das quantidades das etiquetas.
data: 12/2022                                                               
*/


{esp/utilAPI.i} //include especifica que inclui a utp/ut-api.i dentro dela
{utp/ut-api-action.i  atualizarEtiqueta     GET ~* }
{esapi/analisarJsonObject2.i}



PROCEDURE atualizarEtiqueta:

DEF INPUT PARAM jsonInput       AS JsonObject NO-UNDO. 
DEF OUTPUT PARAM jsonOutput     AS JsonObject NO-UNDO.

OUTPUT TO c:\temp\tst\LOG.txt.

    PUT "b" SKIP.


OUTPUT CLOSE.

END PROCEDURE.
