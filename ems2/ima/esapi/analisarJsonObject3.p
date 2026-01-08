
USING Progress.Json.ObjectModel.ObjectModelParser.
USING progress.Json.*.
USING progress.Json.ObjectModel.* .
{esapi/analisarJsonObject2.i}
{esp/util.i}
{esp/utiljson.i}

DEFINE INPUT PARAMETER oJsonObject AS JsonObject NO-UNDO.

DEFINE OUTPUT PARAMETER TABLE FOR ttJson .

DEFINE VARIABLE oParser         AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oJsonArray      AS JsonArray         NO-UNDO.
DEFINE VARIABLE myParser        AS ObjectModelParser NO-UNDO. 
DEFINE VARIABLE txtjson         AS LONGCHAR          NO-UNDO.
//DEFINE VARIABLE aNivel       AS CHARACTER  EXTENT 20 NO-UNDO.
//*DEFINE VARIABLE nivelCorrente   AS CHARACTER         NO-UNDO.
//DEFINE VARIABLE oJsonObject  AS JsonObject        NO-UNDO.
//DEFINE VARIABLE oJsonObject2 AS JsonObject        NO-UNDO.

DEFINE VARIABLE cText            AS CHARACTER         NO-UNDO.
DEFINE VARIABLE myArray          AS CHARACTER         EXTENT NO-UNDO.
DEFINE VARIABLE i1               AS INTEGER           NO-UNDO.
DEFINE VARIABLE cParent          AS CHARACTER         NO-UNDO.
DEFINE VARIABLE iMsg             AS INTEGER           NO-UNDO.
DEFINE VARIABLE cTag             AS CHARACTER         NO-UNDO.
DEFINE VARIABLE agrupJsonCor     AS INTEGER           NO-UNDO.
DEFINE VARIABLE agrupArrayCor    AS INTEGER           NO-UNDO.
DEFINE VARIABLE iTipoObj         AS INTEGER           NO-UNDO.

DEFINE VARIABLE idCorrente       AS INTEGER           NO-UNDO.


DEFINE BUFFER   bfJson FOR ttJson.



FUNCTION getDescrTipoDadoJson RETURNS CHAR(iTipo AS INT):


    CASE iTipo: 
        WHEN 1 THEN
            RETURN 'TEXTO'.
        WHEN 2 THEN
            RETURN 'NUMERO'.
        WHEN 3 THEN
            RETURN 'LOGICO'.
        WHEN 4 THEN
            RETURN 'OBJETO'.
        WHEN 5 THEN
            RETURN 'ARRAY'.
        WHEN 6 THEN
            RETURN 'NULO'.


    END CASE.    
                 
END FUNCTION.


/* Parse the JSON file into a JSON object */
/*oParser = NEW ObjectModelParser().
oJsonObject = CAST(oParser:ParseFile("c:\temp\result.json"), JsonObject).  */


/*MESSAGE 'tipo jsonobject?' TYPE-OF(oJsonObject,jsonObject)
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/


/* Fill the array with the names of the objects in the JSON */
myArray = oJsonObject:GetNames().


/* Process each JSON object in the file */
OUTPUT TO c:\temp\json_analise.txt.
DO i1 = 1 TO EXTENT(myArray):
    ASSIGN cTag     = myArray[i1]
           itipoObj = oJsonObject:getType(cTag).

    PUT UNFORM 'indice principal' "->" cTag  '- tipo obj:' getDescrTipoDadoJson(iTipoObj)  SKIP.
    
    CASE iTipoObj:
      
        WHEN 5 THEN DO:
                                

            RUN criarTtJson(INPUT ''   ,
                            INPUT cTag,
                            'jsonArray').
            PUT UNFORM 'eh um json array - foi criada tag' SKIP.
            RUN ProcessArray (INPUT oJsonObject:getjsonArray(INPUT cTag),INPUT cTag). /* Process the JSON Array */
            
        END.
        WHEN 4 THEN DO:
            
            RUN criarTtJson(INPUT '',
                            INPUT cTag,
                            'json').
            PUT UNFORM 'eh um json - foi criada tag' SKIP.
            RUN ProcessObject (INPUT oJsonObject:getJsonObject(INPUT cTag) , INPUT cTag). /* Process the JSON Object */
            
        END.
        OTHERWISE DO:
            PUT UNFORM 'NAO eh um json NEM array - foi criada tag' SKIP.
            ASSIGN txtJson = oJsonObject:getJsonText( INPUT cTag).
            
            RUN criarTtJson(INPUT cParent,
                            INPUT cTag, 
                            INPUT txtJson).

        END.                           
    END CASE.
END.
OUTPUT CLOSE.



/*OUTPUT TO c:\temp\tt.txt.
FOR EACH ttJson:
    EXPORT DELIMITER "|" ttJson .
END.*/

OUTPUT CLOSE.
//DELETE OBJECT oParser    NO-ERROR.



PROCEDURE ProcessObject:
    DEFINE INPUT PARAMETER poJsonObject AS JsonObject        NO-UNDO.
    DEFINE INPUT  PARAMETER pParente    AS CHARACTER         NO-UNDO.
    //DEFINE INPUT  PARAMETER pAgrup      AS INTEGER           NO-UNDO.
    
    DEFINE VARIABLE myArray AS CHARACTER EXTENT NO-UNDO.
    DEFINE VARIABLE i       AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE lcText  AS LONGCHAR  NO-UNDO.
    //DEFINE VARIABLE memText AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE iTam    AS INTEGER   NO-UNDO.

    myArray = poJsonObject:GetNames().
   
    ASSIGN iTam          = EXTENT(myArray).

    PUT UNFORM 'entrei dentro do objeto - ' cTag ' - pai:' pParente   SKIP. 
    DO i = 1 TO iTam:
        ASSIGN lcText = "".
        fix-codepage(lcText) = "utf-8":U.
        ASSIGN lcText = poJsonObject:getJsonText(INPUT myArray[i])
         //ASSIGN lcText = poJsonObject:getLongChar(INPUT myArray[i],"iso8859-1")
               cTag  = myArray[i].        
        
        

        CASE poJsonObject:getType(INPUT cTag):
            WHEN 5 THEN DO: 
                RUN criarTtJson(INPUT pParente,
                                INPUT cTag,
                                INPUT 'jsonArray').
                PUT UNFORM 'selecionei o tipo de objeto array - ' cTag   SKIP.
                RUN ProcessArray (INPUT poJsonObject:getJsonArray(cTag),INPUT cTag). /* Process the JSON Array */     
            END.
            WHEN 4 THEN DO:
                RUN criarTtJson(INPUT pParente,
                                INPUT cTag,
                                INPUT 'json').
                PUT UNFORM 'selecionei o tipo de objeto json - ' cTag   SKIP.
                RUN ProcessObject(INPUT poJsonObject:getjsonObject(cTag),INPUT cTag). /* Process the JSON Array */     
            END.

                

            OTHERWISE DO:
                RUN criarTtJson(INPUT pParente,
                                INPUT cTag,
                                INPUT lcText).
                PUT UNFORM 'selecionei o tipo <> de  objeto array e json - ' cTag ' - tipo:'  poJsonObject:getType(INPUT cTag)  SKIP.
            END.                                 
        END CASE.
    END.
END.


PROCEDURE ProcessArray:
    DEFINE INPUT PARAMETER oJsonArray AS JsonArray        NO-UNDO.
    DEFINE INPUT  PARAMETER pParente  AS CHARACTER   NO-UNDO.
    DEFINE BUFFER bf FOR ttJson.
    
    DEFINE VARIABLE i               AS INTEGER     NO-UNDO.  
    DEFINE VARIABLE cValor          AS LONGCHAR    NO-UNDO.  
    DEFINE VARIABLE cTagPai         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE oJsonObject     AS JsonObject  NO-UNDO.
    DEFINE VARIABLE iAgrup          AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE iTipoDadoJson   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iTam            AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iITam2          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lCriarRegistro  AS LOGICAL     NO-UNDO.

    DEFINE VARIABLE cTag            AS CHARACTER   NO-UNDO.
    
    
    
    ASSIGN iTam = oJsonArray:LENGTH .
    DO i = 1 TO iTam :
         ASSIGN agrupArrayCor  = 0 .
         ASSIGN iTipoDadoJson  =  oJsonArray:gettype(i).
         ASSIGN lCriarRegistro = NO.
         
         CASE iTipoDadoJson:
             WHEN 1 THEN 
               ASSIGN cValor = oJsonArray:getCharacter(i)
                      lCriarRegistro = YES. 
             WHEN 2 THEN 
               ASSIGN cValor = string(oJsonArray:getDecimal(i))
                      lCriarRegistro = YES.
             WHEN 3 THEN
               ASSIGN cValor = string(oJsonArray:getLogical(i))
                      lCriarRegistro = YES.
             WHEN 6 THEN
               ASSIGN cValor = ''
                      lCriarRegistro = YES.
             WHEN 4 THEN DO:
                RUN criarTtJson(INPUT pParente,
                                INPUT '',
                                INPUT 'json').
                oJsonObject = oJsonArray:getJsonObject(i).
                RUN ProcessObject (INPUT oJsonObject, INPUT pParente). 
             END.
             WHEN 5 THEN DO:
                RUN criarTtJson(INPUT pParente,
                                INPUT '',
                                INPUT 'jsonArray').
                RUN processArray(oJsonArray:getJsonArray(i),pParente).
             END. 
         END CASE.
        PUT UNFORM 'entrei dentro do objeto array - ' string(cvalor) '- tipo:' getDescrTipoDadoJson(iTipoDadoJson) ' -  pai -' pParente SKIP.

        

         IF lCriarRegistro THEN DO:
            FIND LAST ttJson
                WHERE ttJson.tag = pParente NO-ERROR.
            IF AVAIL ttJson THEN
               ASSIGN cTagPai = ttJson.tag_Pai . 
            ELSE
               ASSIGN ctagPai = ''.
            
            RUN criarTtJson(INPUT cTagPai,
                            INPUT pParente,
                            INPUT cValor).
         END.
        
    END.  
END.

PROCEDURE criarTtJson:

   DEFINE INPUT  PARAMETER pTagPai AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER pTag    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER pvalor  AS LONGCHAR    NO-UNDO.
   DEFINE VARIABLE idNivel         AS INTEGER     NO-UNDO.
   
   DEFINE VARIABLE cArqLc          AS CHARACTER   NO-UNDO.
   DEFINE BUFFER bf FOR ttJson.

  

   FIND LAST bf NO-ERROR.

   CREATE ttJson.
   ASSIGN ttJson.id         = IF AVAIL bf THEN bf.id + 1 ELSE 1
          ttJson.tag_pai    = pTagPai
          ttJson.tag        = pTag
          ttJson.agrup      = agrupArrayCor
          ttJson.agrupJson  = IF pTagPai = '' THEN 0 ELSE agrupJsonCor
          .

  PUT UNFORM 'criartTjson - agruparraycor:' agrupArrayCor  ' - agrupJsonCor:' agrupJsonCor  SKIP. 
  //agrupamento de array zerado e agrupamento json maior que zero
  IF agrupArrayCor = 0 AND agrupJsonCor > 0 THEN DO:
     ASSIGN idNivel = agrupJsonCor. 
  END.

  //agrupamento de array maior que zero e agrupamento json igual a zero
  IF agrupArrayCor > 0 AND agrupJsonCor = 0 THEN DO:
     ASSIGN idNivel = agrupJsonCor. 
  END.

  //comparacao do agrupamento de array com o agrupamento de json
  IF agrupArrayCor > agrupJsonCor THEN
     ASSIGN idNivel = agrupArrayCor.
  ELSE
     ASSIGN idNivel = agrupJsonCor.


  FIND bfJson 
      WHERE bfJson.id = idNivel NO-ERROR.
  IF AVAIL bfJson  THEN
     ASSIGN ttJson.nivel = bfJson.nivel + 1.
  ELSE
     ASSIGN ttJson.nivel = 0.
  
  IF pValor = 'json' THEN DO:
     ASSIGN agrupjsonCor = ttJson.Id.
  END.
     

  IF pValor = 'jsonarray' THEN DO:
     ASSIGN agrupArrayCor = ttJson.Id.
  END.


 
     

  
  IF LENGTH(pValor) <= 32000 THEN DO:
      ASSIGN ttJson.valor    = string(pValor)
             ttJson.logArquivo = NO.
  END.
  ELSE DO: // se o conteudo eh maior que o comportado em um varchar, eh criado um arquivo com o conteudo e na tag eh colocado o arquivo gerado e eh marcado o campo logarquivo como yes.
    
      
      ASSIGN cArqLc = cArqLc + "\" + STRING(TIME) + ".lc".
      RUN convLongChar2File(pvalor, INPUT cArqLc) .
      ASSIGN ttJson.valor   = cArqLc
             ttJson.logArquivo = YES .
  END.

  PUT UNFORM 'criartTjson - NOVA ATRIBUICAO agruparraycor:' agrupArrayCor  ' - agrupJsonCor:' agrupJsonCor ' - arquivo?' ttJson.logArquivo  SKIP. 





END PROCEDURE.

 
