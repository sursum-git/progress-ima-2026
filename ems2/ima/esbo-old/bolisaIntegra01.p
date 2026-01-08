/*
* programa: esbo/boLisaIntegra01.p
* Objetivo: Bo para manipula‡Æo da tabela lisa-integra para processamento da SEPARA€ÇO
* Autor: Tadeu Silva
* Data: 10/2023
*/
USING Progress.Json.ObjectModel.ObjectModelParser.
USING progress.Json.*.
USING progress.Json.ObjectModel.*.



DEFINE VARIABLE cArquivo    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrPedido    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDtInclusao AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cHrInclusao AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cDtIncPed   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cHrIncPed   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE oParser      AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oJsonObject  AS JsonObject        NO-UNDO.
DEFINE VARIABLE lAchouArq    AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cDtHr        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lErroRetIsf  AS LOGICAL     NO-UNDO.


DEFINE VARIABLE TRANSACAO AS CHARACTER   NO-UNDO INIT "arqSeparacao".

{esapi/analisarJsonObject2.i}
{esp/utiljson.i}
{esp/ttChave.i}
PROCEDURE setArquivo:

    DEFINE INPUT  PARAMETER pArq AS CHARACTER   NO-UNDO.
    ASSIGN cArquivo = pArq.

END PROCEDURE.

PROCEDURE setPedido:

    DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
    ASSIGN nrPedido = pNrPedido.

END PROCEDURE.

PROCEDURE setDtInclusaoPedido:

    DEFINE INPUT  PARAMETER pDt AS CHARACTER   NO-UNDO.
    ASSIGN cDtInclusao = pDt.


END PROCEDURE.

PROCEDURE setHrInclusaoPedido:

    DEFINE INPUT  PARAMETER pHr AS CHARACTER   NO-UNDO.
    ASSIGN cHrInclusao = pHr.



END PROCEDURE.


PROCEDURE sincrArq:

    RUN vencer.
    RUN incluir.

END PROCEDURE.

PROCEDURE processar:
    DEFINE VARIABLE cArq AS CHARACTER   NO-UNDO.
    ASSIGN lAchouArq = NO.
    RUN getArq(OUTPUT cArq ).
    IF cArq <> '' THEN DO:
       IF search(cArq) <> ? THEN DO:
          ASSIGN lAchouArq = YES.
          RUN convFileToJson(cArq,OUTPUT oJsonObject).
          RUN esapi/analisarJsonObject2.p(INPUT oJsonObject , OUTPUT TABLE ttJson).
          CREATE ttJson.
          ASSIGN ttjson.tag_pai = 'arquivo'
                 ttJson.tag     = 'separacao'
                 ttJson.valor   = cArq.
          RUN esapi/retorno-Isf-Lisa.p(INPUT TABLE ttJson,OUTPUT TABLE ttChave) NO-ERROR.
          ASSIGN lErroRetIsf = RETURN-VALUE  = 'nok' .
       END.
    END.  
END PROCEDURE.

PROCEDURE incluir:

    CREATE lisa-integra.
    ASSIGN lisa-integra.cod-trans       = TRANSACAO
           lisa-integra.chave           = string(nrPedido)
           lisa-integra.conteudo        = cArquivo
           lisa-integra.ind-situacao    = 1
           lisa-integra.val-livre-1     = STRING(NOW,"99/99/9999 hh:mm:ss")
           lisa-integra.val-livre-2     = SUBSTR(cDtInclusao,11,2) + '/' +
                                          SUBSTR(cDtInclusao,7,2) + '/' +
                                          SUBSTR(cDtInclusao,1,4)    
           lisa-integra.val-livre-3     = cHrInclusao

           lisa-integra.acao            = 'criar'.
END PROCEDURE.

PROCEDURE vencer:

    FOR EACH lisa-integra
        WHERE lisa-integra.cod-trans    = TRANSACAO
        AND   lisa-integra.acao         = 'criar'
        AND   lisa-integra.chave        = string(nrPedido)
        AND   lisa-integra.ind-situacao = 1.
        ASSIGN lisa-integra.ind-situacao = 2.
    END.

END PROCEDURE.

PROCEDURE getArq:

    DEFINE OUTPUT PARAMETER cArq AS CHARACTER   NO-UNDO.

    FIND LAST lisa-integra NO-LOCK
        WHERE lisa-integra.cod-trans   = TRANSACAO
        AND   lisa-integra.chave       = STRING(nrPedido)
        AND   lisa-integra.ind-situacao = 1
        NO-ERROR.
    IF AVAIL lisa-integra THEN
       ASSIGN cArq      = lisa-integra.conteudo
              cDtHr     = lisa-integra.val-livre-1
              cDtIncPed = lisa-integra.val-livre-2
              cHrIncPed = lisa-integra.val-livre-3 
              .


END PROCEDURE.

PROCEDURE getAchouArq:

    DEFINE OUTPUT PARAMETER lAchou AS LOGICAL     NO-UNDO.
    ASSIGN lAchou = lAchouArq .


END PROCEDURE.

PROCEDURE getDescrArq:

    DEFINE OUTPUT PARAMETER cDesc AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cArq AS CHARACTER   NO-UNDO.
    RUN getArq(OUTPUT cArq).
    IF cArq <> '' THEN
       ASSIGN cDesc = "Arquivo De Separa‡Æo:" + cArq + " recebido em " + cDtHr + " - Pedido Criado na LISA em " +  cDtIncPed + " " + cHrIncPed.
    ELSE 
       ASSIGN cDesc = "Arquivo De Separa‡Æo NÆo Recebido".


END PROCEDURE.


PROCEDURE getErroRetIsf:

    DEFINE OUTPUT PARAMETER lRet AS LOGICAL     NO-UNDO.
    ASSIGN lRet = lErroRetIsf.

END PROCEDURE.
/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-trans                        char
   20 chave                            char        i
   30 conteudo                         char
   40 val-livre-1                      char
   50 val-livre-2                      char
   60 val-livre-3                      char
   70 val-livre-4                      char
   80 val-livre-5                      char
   90 ind-situacao                     inte
  100 acao                             char        i

*/


