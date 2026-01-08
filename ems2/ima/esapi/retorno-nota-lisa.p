{esapi/analisarJsonObject2.i}
{esp/ttChave.i}

DEF TEMP-TABLE ttNotaRetorno
    FIELD cod-estabel  AS CHAR
    FIELD serie AS CHAR
    FIELD nr-nota-fis AS CHAR.

DEF INPUT  PARAMETER TABLE FOR ttJson.
DEF OUTPUT PARAMETER TABLE FOR ttChave.

DEF VAR c-tabela AS CHAR.
DEF VAR c-nr-pedcli AS CHAR.
DEF VAR c-pre-pedido AS CHAR.
DEF VAR c-chave AS CHAR.
DEF VAR c-msg AS CHAR.

FOR EACH ttJson NO-LOCK USE-INDEX pri.
    IF ttJson.tag_pai = 'payload' AND 
       ttJson.tag = "pedidoCliente" THEN
       ASSIGN c-nr-pedcli = ttJson.valor.

    IF ttJson.tag_pai = 'payload' AND 
       ttJson.tag = "prePedido" THEN
       ASSIGN c-pre-pedido = ttJson.valor.

    /*
    IF ttJson.tag_pai = 'Separacao' THEN DO.
       CASE ttJson.tag:
          WHEN 'NotaVenda' THEN DO.
              CREATE ttNotaRetorno.
              ASSIGN ttNotaRetorno.cod-estabel = 
                     ttNotaRetorno.serie = 
                     ttNotaRetorno.nr-nota-fis = .
          END.
       END CASE.
    END.
    */
END.

// Validar Qualquer coisa
FOR EACH ttNotaRetorno.
    /*
    ASSIGN c-msg = 'Etiqueta n∆o est† em Estoque' + CHR(10) + 
                   'Dados Retornados ' + CHR(10) +
                   '  Container: ' +  ttReservas.nr-container + CHR(10) +
                   '       Item: ' +  ttReservas.it-codigo + CHR(10) +
                   ' Referància: ' +  ttReservas.cod-refer + CHR(10) +
                   '       Rolo: ' +  ttReservas.num-rolo-imp. 

    RUN pi-cria-retorno (INPUT ttNotaRetorno.,
                         INPUT c-msg).
    NEXT.
    */
END.

FIND FIRST ttChave NO-ERROR.
IF AVAIL ttChave THEN DO.
   //RUN criarTTRetorno('RetornoISF', 500).
   RETURN 'ADM-ERROR'.
END.


FOR EACH ttNotaRetorno NO-LOCK.

    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel = ttNotaRetorno.cod-estabel AND
         nota-fiscal.serie = ttNotaRetorno.serie AND
         nota-fiscal.nr-nota-fis = ttNotaRetorno.nr-nota-fis 
         NO-LOCK NO-ERROR.

    ASSIGN c-chave = nota-fiscal.cod-estabel + "|" + 
                     nota-fiscal.serie + "|" + 
                     nota-fiscal.nr-nota-fis.

    FIND lisa-integra WHERE
         lisa-integra.cod-trans = "RetornoNotaVenda"  AND    
         lisa-integra.chave = c-chave SHARE-LOCK.
    ASSIGN lisa-integra.acao = 'Retorno'.
END.


//--------------- Procedures 
PROCEDURE pi-cria-retorno.
    DEF INPUT PARAMETER p-chave AS CHAR.
    DEF INPUT PARAMETER p-valor AS CHAR.

    CREATE ttChave.
    ASSIGN ttChave.chave = p-chave
           ttChave.valor = p-valor.
END.
