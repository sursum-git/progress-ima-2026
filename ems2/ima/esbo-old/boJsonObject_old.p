/*
programa:boJsonObject
objetivo: Extrair os metadados e dados de um json e 
colocar em tabelas temporarias 
autor:Tadeu Silva
data: 11/2022
*/


USING Progress.Json.ObjectModel.ObjectModelParser.
USING progress.Json.*.
USING progress.Json.ObjectModel.*.
{esbo/bojsonObject.i}
DEFINE VARIABLE iNivel          AS INTEGER     NO-UNDO.
DEFINE VARIABLE idPaiCorrente   AS INTEGER     NO-UNDO.
DEFINE VARIABLE oJson  AS jsonobject NO-UNDO.


FUNCTION _getDescrTipoDado RETURNS CHAR(iTipo AS INT):

         CASE iTipo:
             WHEN 1 THEN
                 RETURN 'texto'.
             WHEN 2 THEN
                 RETURN 'numero'.
             WHEN 3 THEN
                 RETURN 'logico'.
             WHEN 4 THEN
                 RETURN 'objeto'.
             WHEN 5 THEN
                 RETURN 'array'.
             WHEN 6 THEN
                 RETURN 'nulo'.
             OTHERWISE
                 RETURN ''.
         END CASE.

END FUNCTION.


PROCEDURE setJsonObject:
    
    DEFINE INPUT PARAMETER pJson AS jsonObject .
    ASSIGN oJson = pJson.

END PROCEDURE.


PROCEDURE zerarNivel:

    ASSIGN iNivel = 0.

END PROCEDURE.


PROCEDURE incrNivel:

    ASSIGN iNivel = iNivel + 1 .

END PROCEDURE.

PROCEDURE setIdPaiCorrente:

    DEFINE INPUT  PARAMETER pId AS INTEGER     NO-UNDO.
    ASSIGN idPaiCorrente = pId.

END PROCEDURE.

PROCEDURE zerarIdPai:

    ASSIGN idPaiCorrente = 0.

END PROCEDURE.



PROCEDURE extrairDadosJson:
    
    DEFINE VARIABLE aProps           AS CHARACTER   EXTENT NO-UNDO.
    DEFINE VARIABLE i                AS INTEGER     NO-UNDO.
    DEFINE VARIABLE idNovo           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTipo            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iAgrup           AS INTEGER     NO-UNDO.

    ASSIGN iAgrup = INT(TIME).
    //busca os nomes das propriedades de primeiro nivel
    ASSIGN aProps = oJson:getNames()
           cTipo  = _getDescrTipoDado(oJson:getType(aProps[i])).
    DO i = 1 TO EXTENT(aProps):
        RUN inserirMetaDado(iNivel,aProps[i], cTipo,idPaiCorrente, iAgrup,OUTPUT idNovo).
        IF cTipo = 'array' THEN DO:
           RUN incrNivel.
           RUN setIdPaiCorrente(idNovo).
        END.                            
    END.



END PROCEDURE.

PROCEDURE extrairDadosJsonAux:

    DEFINE VARIABLE aProps           AS CHARACTER   EXTENT NO-UNDO.
    DEFINE VARIABLE i                AS INTEGER     NO-UNDO.
    DEFINE VARIABLE idNovo           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTipo            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iAgrup           AS INTEGER     NO-UNDO.

    ASSIGN iAgrup = INT(TIME).
    //busca os nomes das propriedades de primeiro nivel
    ASSIGN aProps = oJson:getNames()
           cTipo  = _getDescrTipoDado(oJson:getType(aProps[i])).
    DO i = 1 TO EXTENT(aProps):
        RUN inserirMetaDado(iNivel,aProps[i], cTipo,idPaiCorrente, iAgrup,OUTPUT idNovo).
        IF cTipo = 'array' THEN DO:
           RUN incrNivel.
           RUN setIdPaiCorrente(idNovo).
        END.                            
    END.
    FOR EACH ttmetaJson
        WHERE ttmetaJson.agrup = iAGrup:


    END.






END.



PROCEDURE gerarArquivoMetaDados:

    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.

    OUTPUT TO VALUE(pArquivo).                              
        FOR EACH ttMetaJson:
            EXPORT DELIMITER "|" ttMetaJson.
        END.
    OUTPUT CLOSE.

END PROCEDURE.


/*

USING Progress.Json.ObjectModel.JsonDataType .

MESSAGE  JsonDataType:STRING  SKIP
         JsonDataType:NUMBER SKIP
         JsonDataType:boolean SKIP
         JsonDataType:OBJECT SKIP
         JsonDataType:array  SKIP
         JsonDataType:NULL   SKIP
        
        
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
 */
