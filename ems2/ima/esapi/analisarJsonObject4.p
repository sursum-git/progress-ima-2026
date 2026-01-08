USING Progress.Json.ObjectModel.ObjectModelParser.
USING progress.Json.*.
USING progress.Json.ObjectModel.*.
{esapi/analisarJsonObject2.i}
{esp/util.i}
{esp/utiljson.i}



DEFINE INPUT PARAMETER oJsonObject AS JsonObject NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttJson .

/* vari†veis globais */
DEFINE VARIABLE iNivel        AS INTEGER NO-UNDO.
DEFINE VARIABLE agrupJsonCor  AS INTEGER NO-UNDO.
DEFINE VARIABLE agrupArrayCor AS INTEGER NO-UNDO.

/* entrada do script: oJsonObject e ttJson j† vàm definidos pelo include */
  
/* inicializa */
ASSIGN 
  iNivel        = 0
  agrupJsonCor  = 0
  agrupArrayCor = 0
.


/* comeáa o processamento */
RUN ProcessObject(
  INPUT oJsonObject,  /* objeto raiz */
  INPUT "",           /* tagPai vazio */
  INPUT 0,            /* agrupJsonCor inicial */
  INPUT 0             /* agrupArrayCor inicial */
).

/* ==== ProcessObject ==== */
PROCEDURE ProcessObject:
  DEFINE INPUT  PARAMETER poJsonObject AS JsonObject NO-UNDO.
  DEFINE INPUT  PARAMETER pParente     AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pAgrupJson   AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pAgrupArray  AS INTEGER    NO-UNDO.

  DEFINE VARIABLE aNames        AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER           NO-UNDO.
  DEFINE VARIABLE cTag          AS CHARACTER         NO-UNDO.
  DEFINE VARIABLE newAgrupJson  AS INTEGER           NO-UNDO.
  DEFINE VARIABLE newAgrupArray AS INTEGER           NO-UNDO.

  /* sobe um n°vel */
  ASSIGN iNivel = iNivel + 1.

  aNames = poJsonObject:GetNames().
  DO i = 1 TO EXTENT(aNames):
    ASSIGN cTag = aNames[i].

    CASE poJsonObject:getType(cTag):

      WHEN JsonDataType:OBJECT THEN DO:
        RUN criarTtJson(
          INPUT  pParente, 
          INPUT  cTag, 
          INPUT  "json",
          INPUT  pAgrupJson, 
          INPUT  pAgrupArray,
          OUTPUT newAgrupJson, 
          OUTPUT newAgrupArray
        ).
        RUN ProcessObject(
          INPUT poJsonObject:getJsonObject(cTag),
          INPUT cTag,
          INPUT newAgrupJson,
          INPUT newAgrupArray
        ).
      END.

      WHEN JsonDataType:ARRAY THEN DO:
        RUN criarTtJson(
          INPUT  pParente, 
          INPUT  cTag, 
          INPUT  "jsonArray",
          INPUT  pAgrupJson, 
          INPUT  pAgrupArray,
          OUTPUT newAgrupJson, 
          OUTPUT newAgrupArray
        ).
        RUN ProcessArray(
          INPUT poJsonObject:getJsonArray(cTag),
          INPUT cTag,
          INPUT newAgrupJson,
          INPUT newAgrupArray
        ).
      END.

      OTHERWISE DO:
        RUN criarTtJson(
          INPUT  pParente, 
          INPUT  cTag, 
          INPUT  poJsonObject:getJsonText(cTag),
          INPUT  pAgrupJson, 
          INPUT  pAgrupArray,
          OUTPUT newAgrupJson, 
          OUTPUT newAgrupArray
        ).
      END.

    END CASE.
  END.

  /* desce um n°vel */
  ASSIGN iNivel = iNivel - 1.
END PROCEDURE.

/* ==== ProcessArray ==== */
PROCEDURE ProcessArray:
  DEFINE INPUT  PARAMETER poJsonArray  AS JsonArray NO-UNDO.
  DEFINE INPUT  PARAMETER pParente     AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pAgrupJson   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER pAgrupArray  AS INTEGER   NO-UNDO.

  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
  DEFINE VARIABLE tTipo         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cValor        AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE newAgrupJson  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE newAgrupArray AS INTEGER   NO-UNDO.

  /* sobe um n°vel */
  ASSIGN iNivel = iNivel + 1.

  DO i = 1 TO poJsonArray:LENGTH:
    ASSIGN tTipo = poJsonArray:getType(i).

    CASE tTipo:

      WHEN JsonDataType:OBJECT THEN DO:
        RUN criarTtJson(
          INPUT  pParente, 
          INPUT  "", 
          INPUT  "json",
          INPUT  pAgrupJson, 
          INPUT  pAgrupArray,
          OUTPUT newAgrupJson, 
          OUTPUT newAgrupArray
        ).
        RUN ProcessObject(
          INPUT poJsonArray:getJsonObject(i),
          INPUT pParente,
          INPUT newAgrupJson,
          INPUT newAgrupArray
        ).
      END.

      WHEN JsonDataType:ARRAY THEN DO:
        RUN criarTtJson(
          INPUT  pParente, 
          INPUT  "", 
          INPUT  "jsonArray",
          INPUT  pAgrupJson, 
          INPUT  pAgrupArray,
          OUTPUT newAgrupJson, 
          OUTPUT newAgrupArray
        ).
        RUN ProcessArray(
          INPUT poJsonArray:getJsonArray(i),
          INPUT pParente,
          INPUT newAgrupJson,
          INPUT newAgrupArray
        ).
      END.

      OTHERWISE DO:
        /* valor primitivo */
        IF tTipo = JsonDataType:STRING  THEN cValor = poJsonArray:getCharacter(i).
        IF tTipo = JsonDataType:NUMBER  THEN cValor = STRING(poJsonArray:getDecimal(i)).
        IF tTipo = JsonDataType:BOOLEAN THEN cValor = STRING(poJsonArray:getLogical(i)).
        IF tTipo = JsonDataType:NULL    THEN cValor = "".

        RUN criarTtJson(
          INPUT  pParente, 
          INPUT  pParente,  /* tag filha = tag do pai para valores */
          INPUT  cValor,
          INPUT  pAgrupJson, 
          INPUT  pAgrupArray,
          OUTPUT newAgrupJson, 
          OUTPUT newAgrupArray
        ).
      END.

    END CASE.
  END.

  /* desce um n°vel */
  ASSIGN iNivel = iNivel - 1.
END PROCEDURE.

/* ==== criarTtJson ==== */
PROCEDURE criarTtJson:
  DEFINE INPUT  PARAMETER pTagPai     AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pTag        AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER pValor      AS LONGCHAR   NO-UNDO.
  DEFINE INPUT  PARAMETER pAgrupJson  AS INTEGER    NO-UNDO.
  DEFINE INPUT  PARAMETER pAgrupArray AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER oAgrupJson  AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER oAgrupArray AS INTEGER   NO-UNDO.
  DEFINE BUFFER bfJson FOR ttJson.

  DEFINE VARIABLE novoId AS INTEGER NO-UNDO.

  /* gera ID sequencial */
  FIND LAST bfJson NO-ERROR.
  ASSIGN novoId = IF AVAILABLE bfJson THEN bfJson.id + 1 ELSE 1.

  /* cria registro com n°vel atual e agrupadores vindos do pai */
  CREATE ttJson.
  ASSIGN
    ttJson.id         = novoId
    ttJson.tag_pai    = pTagPai
    ttJson.tag        = pTag
    ttJson.valor      = REPLACE(pValor, CHR(146), "")
    ttJson.nivel      = iNivel
    ttJson.agrupJson  = pAgrupJson
    ttJson.agrup      = pAgrupArray
    ttJson.logArquivo = NO
  .

  /* ajusta quais agrupadores ser∆o repassados aos filhos */
  ASSIGN
    oAgrupJson  = IF pValor = "json" THEN novoId ELSE pAgrupJson
    oAgrupArray = IF pValor = "jsonArray" THEN  novoId ELSE pAgrupArray.
END PROCEDURE.
