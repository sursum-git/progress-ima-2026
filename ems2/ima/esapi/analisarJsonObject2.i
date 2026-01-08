DEFINE TEMP-TABLE ttJson NO-UNDO
    FIELD id            AS INT
    FIELD tag_pai       AS CHAR
    FIELD tag           AS CHAR
    FIELD valor         AS CHAR
    FIELD agrup         AS INT
    FIELD logArquivo    AS LOGICAL
    FIELD nivel         AS INT
    FIELD agrupJson     AS INT
    INDEX pri AS PRIMARY id 
    INDEX indtag tag_pai tag
    INDEX indagrup agrup agrupjson
    INDEX indagrup2 agrupjson agrup
    INDEX indnivel nivel tag_pai tag
    INDEX indparte tag_pai tag agrupJson
    .
    //INDEX pri AS PRIMARY tag_pai agrup.

DEFINE TEMP-TABLE ttNos
    FIELD id AS INT.

PROCEDURE geTtNos:

   DEFINE INPUT  PARAMETER pTagPai AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER pTag    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER pTipo   AS CHARACTER   NO-UNDO. //json ou jsonarray
   DEFINE OUTPUT PARAMETER TABLE FOR ttNos.
   EMPTY TEMP-TABLE ttNos.
   FOR EACH  ttJson
       where ttJson.tag_pai = pTagPai
       AND   ttJson.tag     = pTag
       AND   ttJson.valor   = pTipo.
       CREATE ttNos.
       ASSIGN ttNos.id = ttJson.id.
   END.

END PROCEDURE.




PROCEDURE insertTtJson:

    DEFINE BUFFER bf FOR ttJson.
    DEFINE INPUT  PARAMETER pTagPai     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTag        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pAgrup      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pLogArquivo AS LOGICAL     NO-UNDO.

    FIND LAST bf NO-ERROR.
    CREATE ttJson.
    ASSIGN ttJson.id = IF AVAIL bf THEN bf.id + 1 ELSE 1
           ttJson.tag_pai       = pTagPai
           ttJson.tag           = pTag
           ttJson.valor         = pValor
           ttJson.agrup         = pAgrup
           ttJson.logArquivo    = pLogArquivo
           .
    

END PROCEDURE.

PROCEDURE setAgrupTtJson:
    
    DEFINE INPUT  PARAMETER pCampoPaiIni AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCampoIni    AS CHARACTER   NO-UNDO.
    //DEFINE OUTPUT PARAMETER cCampos      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iAgrupCont           AS INTEGER     NO-UNDO.
    
    IF pCampoIni = '' THEN DO:
       FIND FIRST ttJson
           WHERE ttJson.tag_pai = pCampoPaiIni
           NO-ERROR.
       IF AVAIL ttJson THEN
          ASSIGN pCampoIni = ttjson.tag .
    END.
    //tenta buscar o primeiro caso n∆o exista a combinaá∆o tagpai e tag informada
    //evitando a quebra do programa no caso de mudanáa do json
    IF NOT CAN-FIND( FIRST ttJson WHERE ttJson.tag_pai = pCampoPaiIni
           AND ttJson.tag = pCampoIni) THEN DO:
        FIND FIRST ttJson
           WHERE ttJson.tag_pai = pCampoPaiIni
           NO-ERROR.
       IF AVAIL ttJson THEN
          ASSIGN pCampoIni = ttjson.tag .

    END.

    FOR EACH ttJson :
        IF ttJson.tag_pai = pCampoPaiIni
           AND ttJson.tag = pCampoIni THEN DO:
           ASSIGN iAgrupCont = iAgrupCont + 1. 
        END.
        ASSIGN ttJson.agrup = iAgrupCont.
    END.


END PROCEDURE.

PROCEDURE transporTTJson:

// implementar futuramente.
END PROCEDURE.

 FUNCTION getChaveTTJsonTag RETURNS CHARACTER ( cTag AS CHAR):

    FIND LAST ttJson 
        WHERE ttJson.tag  = cTag NO-ERROR.
    IF AVAIL ttJson THEN
       RETURN ttJson.valor .
    ELSE 
      RETURN ''.

END FUNCTION.
  
FUNCTION getChaveTTJson RETURNS CHARACTER (tagPai AS CHAR, cTag AS CHAR):


    FIND LAST ttJson 
        WHERE ttJson.tag_pai = tagPai
        AND   ttJson.tag     = cTag        
        NO-ERROR.
    IF AVAIL ttJson THEN
       RETURN ttJson.valor .
    ELSE 
      RETURN ''.


END FUNCTION.






/*DEFINE TEMP-TABLE  ttMetaJson NO-UNDO
    FIELD id    AS INT
    FIELD nivel AS INT 
    FIELD chave AS CHAR
    FIELD tipo  AS CHAR
    FIELD idPai AS INT 
    INDEX pri nivel idpai id
    INDEX ch chave.

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
    DEFINE OUTPUT PARAMETER idNovo AS INTEGER     NO-UNDO.
    DEFINE BUFFER bf FOR ttMetaJson.
    FIND LAST bf NO-ERROR.
    CREATE ttMetaJson.
    ASSIGN ttMetaJson.id        = IF AVAIL bf THEN bf.id + 1 ELSE 1   
           ttMetaJson.nivel     = pNivel 
           ttMetaJson.chave     = pChave 
           ttMetaJson.tipo      = pTipo  
           ttMetaJson.idPai     = pidPai 
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
*/
    

