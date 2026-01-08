
USING Progress.Json.ObjectModel.ObjectModelParser.
USING progress.Json.*.
USING progress.Json.ObjectModel.* .
{esapi/analisarJsonObject.i}
{esp/util.i}
{esp/utiljson.i}

DEFINE INPUT PARAMETER oJsonObject AS JsonObject NO-UNDO.

DEFINE OUTPUT PARAMETER TABLE FOR ttJson .

DEFINE VARIABLE oParser         AS ObjectModelParser NO-UNDO.
DEFINE VARIABLE oJsonArray      AS JsonArray         NO-UNDO.
DEFINE VARIABLE myParser        AS ObjectModelParser NO-UNDO. 
DEFINE VARIABLE txtjson         AS LONGCHAR          NO-UNDO.
//DEFINE VARIABLE aNivel       AS CHARACTER  EXTENT 20 NO-UNDO.
DEFINE VARIABLE nivelCorrente   AS CHARACTER         NO-UNDO.
//DEFINE VARIABLE oJsonObject  AS JsonObject        NO-UNDO.
//DEFINE VARIABLE oJsonObject2 AS JsonObject        NO-UNDO.

DEFINE VARIABLE cText        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE myArray      AS CHARACTER         EXTENT NO-UNDO.
DEFINE VARIABLE i1           AS INTEGER           NO-UNDO.
DEFINE VARIABLE cParent      AS CHARACTER         NO-UNDO.
DEFINE VARIABLE iMsg         AS INTEGER           NO-UNDO.
DEFINE VARIABLE iNivel       AS INTEGER           NO-UNDO.
DEFINE VARIABLE iNivelFinal  AS INTEGER           NO-UNDO.
DEFINE VARIABLE cTag         AS CHARACTER         NO-UNDO.
DEFINE VARIABLE hBoApiLisa   AS HANDLE            NO-UNDO.


RUN esp/instProg.p('esbo/boAPisLisa.p', OUTPUT hBoApiLisa).



FUNCTION getUltNivel RETURNS CHAR():

    /*DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
    REPEAT iCont = 1 TO extent(aNivel):
        IF aNivel[iCont] = '' THEN DO:
           IF iCont = 1 THEN
              RETURN ''.
           ELSE
              RETURN aNivel[iCont - 1] .
        END.
    END.*/
    RETURN nivelCorrente.

END FUNCTION.

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
    PUT UNFORM 'indice' i1 "->" myArray[i1] SKIP.
    ASSIGN iNivel = iNivel + 1.
    ASSIGN cTag = myArray[i1].
           /*aNivel[iNivel] = myArray[i1] .*/
    /*MESSAGE  'nome tag:' myArray[i1] SKIP
             'tipo json:'getDescrTipoDadoJson( oJsonObject:getType(myArray[i1]) )
             'nivel:' iNivel SKIP
             'pai:' cParent
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    
    ASSIGN iNivelFinal = i1 * iNivel .
    CASE oJsonObject:getType(cTag):

       /* WHEN 1 THEN DO:
            ASSIGN txtJson = oJsonObject:getJsonText(myArray[i1]).
            //COPY-LOB txtjson TO  FILE 'c:\temp\txtjson.txt' .
            
            RUN limparTxtJson(INPUT-OUTPUT txtJson).

            myParser = NEW ObjectModelParser(). 
            RUN ProcessObject (INPUT cast(myParser:Parse(txtJson), JsonObject),myArray[i1],i1 * iNivel). /* Process the JSON Object */
        END.*/
        WHEN 5 THEN DO:
            RUN ProcessArray (INPUT oJsonObject:getjsonArray(INPUT cTag),INPUT cTag). /* Process the JSON Array */     
        END.
        WHEN 4 THEN DO:
            RUN ProcessObject (INPUT oJsonObject:getJsonObject(INPUT cTag) , INPUT cTag, INPUT iNivelFinal). /* Process the JSON Object */
        END.
        OTHERWISE DO:
            ASSIGN txtJson = oJsonObject:getJsonText( INPUT cTag).
            /*MESSAGE 'criei registro na raiz:'
                 'pai:' cParent SKIP
                 'tag:' cTag SKIP
                 'valor:' string(txtJson) SKIP
                 'agrup:' iNivelFinal 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            RUN criarTtJson(INPUT cParent,INPUT cTag, INPUT txtJson, INPUT iNivelFinal).
            /*CREATE ttJson.
            ASSIGN ttJson.tag_pai  = nivelCorrente
                   ttJson.tag      = cTag
                   ttJson.valor    = txtJson
                   ttJson.agrup    = iNivelFinal .*/


        END.                           
    END CASE.
END.
OUTPUT CLOSE.

OUTPUT TO c:\temp\tt.txt.
FOR EACH ttJson:
    EXPORT DELIMITER "|" ttJson .
END.

OUTPUT CLOSE.
//DELETE OBJECT oParser    NO-ERROR.

IF VALID-HANDLE(hBoApiLisa) THEN
   DELETE PROCEDURE hBoApiLisa.

PROCEDURE ProcessObject:
    DEFINE INPUT PARAMETER poJsonObject AS JsonObject        NO-UNDO.
    DEFINE INPUT  PARAMETER pParente    AS CHARACTER         NO-UNDO.
    DEFINE INPUT  PARAMETER pAgrup      AS INTEGER           NO-UNDO.
    
    DEFINE VARIABLE myArray AS CHARACTER EXTENT NO-UNDO.
    DEFINE VARIABLE i       AS INTEGER   NO-UNDO.  
    DEFINE VARIABLE cText   AS LONGCHAR  NO-UNDO.

    myArray = poJsonObject:GetNames().
    PUT UNFORM 'entrei dentro do objeto de segundo n¡vel' SKIP.
    ASSIGN nivelCorrente = pParente .
    ASSIGN iNivel = iNivel + 1.
    DO i = 1 TO EXTENT(myArray):
        ASSIGN cText = poJsonObject:getJsonText(INPUT myArray[i])
               cTag  = myArray[i].

        /*MESSAGE 'pai:' pParente SKIP
                'tag:' cTag SKIP
                'tipo:' poJsonObject:getType(cTag) SKIP
                'texto:' string(cText) SKIP
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

        CASE poJsonObject:getType(INPUT cTag):
            WHEN 5 THEN DO: 
                /*MESSAGE 'vou passar o texto para process array:' string(cText) SKIP
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                RUN ProcessArray (INPUT poJsonObject:getJsonArray(cTag),INPUT cTag). /* Process the JSON Array */     
            END.
            WHEN 4 THEN
                RUN ProcessObject(INPUT poJsonObject:getjsonObject(cTag),INPUT cTag, INPUT pAgrup). /* Process the JSON Array */     
            OTHERWISE DO:
                /*MESSAGE 'criei registro:' SKIP
                        'pai' pParente
                        'tag:' ctag SKIP
                        'valor:' string(ctext) SKIP
                         'agrup:' pagrup
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
                RUN criarTtJson(INPUT pParente,INPUT cTag,INPUT cText,INPUT pAgrup).
                /*CREATE ttJson.
                ASSIGN ttJson.tag_pai  = nivelCorrente
                       ttJson.tag      = cTag
                       ttJson.valor    = cText
                       ttJson.agrup    = pAgrup  .*/
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
    DEFINE VARIABLE iNivelFinal     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iTipoDadoJson   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lCriarRegistro  AS LOGICAL     NO-UNDO.
    
    ASSIGN nivelCorrente = pParente .
    ASSIGN iNivel = iNivel + 1
           iNivelFinal = i * iNivel.
    /* Process each object in the JSON array */
    /*MESSAGE 'tamanho jsonarray:' oJsonArray:LENGTH
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    DO i = 1 TO oJsonArray:LENGTH :
         //ASSIGN iAgrup = INT(TIME). 
         ASSIGN iTipoDadoJson =  oJsonArray:gettype(i).
         /*MESSAGE 'processArray:' pParente SKIP
                 'quantidade de filhos:' oJsonArray:LENGTH SKIP
                 //'objetojson:'  oJsonArray:getJsonObject(i):getJsonText() SKIP
                 'tipo dado:'   iTipoDadoJson
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

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
                oJsonObject = oJsonArray:getJsonObject(i).
                RUN ProcessObject (INPUT oJsonObject, INPUT pParente,INPUT iNivelFinal). /* Process the JSON Object */
             END.
             WHEN 5 THEN DO:
                RUN processArray(oJsonArray:getJsonArray(i),pParente).
             END. 
         END CASE.
         IF lCriarRegistro THEN DO:
            FIND LAST ttJson
                WHERE ttJson.tag = pParente NO-ERROR.
            IF AVAIL ttJson THEN
               ASSIGN cTagPai = ttJson.tag_Pai . 
            ELSE
               ASSIGN ctagPai = ''.
            /*MESSAGE 'vou criar registro dentro do process array:' SKIP
                  'pai:' cTagPai  SKIP
                  'tag:' pParente SKIP
                  'valor:' string(cValor) SKIP
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            RUN criarTtJson(INPUT cTagPai   ,INPUT pParente,INPUT cValor,INPUT iNivelFinal).
         END.
            
         
         
        /* Cast the JSON to an object of type JsonObject - Assumes there are no more JSON arrays */           
        //oJsonObject = CAST(oParser:Parse(cText), JsonObject) NO-ERROR.
        
    END.  
END.

PROCEDURE criarTtJson:

   DEFINE INPUT  PARAMETER pTagPai AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER pTag    AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER pvalor  AS LONGCHAR    NO-UNDO.
   DEFINE INPUT  PARAMETER pAgrup  AS INTEGER     NO-UNDO.
   DEFINE VARIABLE cArqLc          AS CHARACTER   NO-UNDO.
   DEFINE BUFFER bf FOR ttJson.

  /* 
      PUT UNFORM "Parent Object : "  pTagPai SKIP(1)  //cParent SKIP(1) 
            "Property : "pTag " : " pvalor SKIP(1)
            //"Number of Properties in the object : " EXTENT(myArray) SKIP
            //"Current Property : " i SKIP                
            "================================================================" SKIP.
  */

   FIND LAST bf NO-ERROR.

   CREATE ttJson.
   ASSIGN ttJson.id       = IF AVAIL bf THEN bf.id + 1 ELSE 1
          ttJson.tag_pai  = pTagPai
          ttJson.tag      = pTag
          ttJson.agrup    = pAgrup  . 

  

  IF LENGTH(pValor) <= 32000 THEN DO:
      ASSIGN ttJson.valor    = pValor
             ttJson.logArquivo = NO.
  END.
  ELSE DO: // se o conteudo ‚ maior que o comportado em um varchar, ‚ criado um arquivo com o conteudo e na tag ‚ colocado o arquivo gerado e ‚ marcado o campo logarquivo como yes.
    
      RUN getDirJsonLisa IN hBoApiLisa(OUTPUT cArqLc).
      ASSIGN cArqLc = cArqLc + "\" + STRING(TIME) + ".lc".
      RUN convLongChar2File(pvalor, INPUT cArqLc) .
      ASSIGN ttJson.valor   = cArqLc
             ttJson.logArquivo = YES .
  END.

END PROCEDURE.

/*
PROCEDURE extrairDadosJson:

    DEFINE INPUT PARAMETER pOJson    AS JsonObject .
    DEFINE VARIABLE aProps           AS CHARACTER   EXTENT NO-UNDO.
    DEFINE VARIABLE i                AS INTEGER     NO-UNDO.
    //busca os nomes das propriedades de primeiro nivel
    ASSIGN aProps = pOJson:getNames().
    DO i = 1 TO EXTENT(pProps):
        RUN inserirMetaDado(1,aProps[i], pOJson:getType(aProps[i]),0).
    END.
END PROCEDURE.
*/


/* String      =  1 /* = JsonDataType:STRING */
                      Number   /* =  2 = JsonDataType:NUMBER */    
                      Boolean  /* =  3 = JsonDataType:BOOLEAN */
                      Object   /* =  4 = JsonDataType:OBJECT */
                      Array    /* =  5 = JsonDataType:ARRAY */
                      Null     /* =  6 = JsonDataType:NULL */*/
