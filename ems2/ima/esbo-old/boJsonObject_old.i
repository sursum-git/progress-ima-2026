USING progress.Json.*.
DEFINE TEMP-TABLE  ttMetaJson NO-UNDO
    FIELD id            AS INT
    FIELD nivel         AS INT 
    FIELD chave         AS CHAR
    FIELD tipo          AS CHAR
    FIELD idPai         AS INT 
    FIELD logPercorrido AS LOGICAL
    FIELD agrup         AS INT
    INDEX pri nivel idpai id
    INDEX sec logPercorrido nivel idpai id
    INDEX ch chave
    INDEX terc agrup.

DEFINE TEMP-TABLE ttDadosJson NO-UNDO
    FIELD id            AS INT
    FIELD idMetaDado    AS INT
    FIELD valor         AS CHAR
    FIELD chaveBusca    AS CHAR
    INDEX pri AS PRIMARY chavebusca idmetadado.


PROCEDURE inserirMetaDado:
    DEFINE INPUT  PARAMETER pNivel AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pChave AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pidPai AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pAgrup AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER idNovo AS INTEGER     NO-UNDO.
    DEFINE BUFFER bf FOR ttMetaJson.
    FIND LAST bf NO-ERROR.
    CREATE ttMetaJson.
    ASSIGN ttMetaJson.id        = IF AVAIL bf THEN bf.id + 1 ELSE 1   
           ttMetaJson.nivel     = pNivel 
           ttMetaJson.chave     = pChave 
           ttMetaJson.tipo      = pTipo  
           ttMetaJson.idPai     = pidPai 
           ttMetaJson.agrup     = pAgrup
           idNovo               = ttMetaJson.id 
           .

END PROCEDURE.

PROCEDURE inserirDado:

    DEFINE INPUT  PARAMETER pIdMetaDado  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pValor      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChaveBusca AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER idNovo      AS INTEGER     NO-UNDO.
    DEFINE BUFFER bf FOR ttDadosJson.
    FIND LAST bf NO-ERROR.

    CREATE ttDadosJson.
    ASSIGN ttDadosJson.id           = IF AVAIL bf THEN bf.id + 1 ELSE 1   
           ttDadosJson.idMetaDado   = pIdMetaDado 
           ttDadosJson.valor        = pValor
           ttDadosJson.chaveBusca   = pChaveBusca
           idNovo                   = ttDadosJson.id 
           .



END PROCEDURE.

    
