{esapi/analisarJsonObject2.i}

LOG-MANAGER:LOGFILE-NAME = 'C:\temp\consultaEtq_' + STRING(TIME) + '.txt'.
LOG-MANAGER:LOGGING-LEVEL = 5.
 
 DEFINE INPUT  PARAMETER pItCodigo          AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER pCodRefer          AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER pLogAtuEnd         AS LOGICAL     NO-UNDO.
 DEFINE INPUT  PARAMETER pDtAtuEndIni       AS DATE        NO-UNDO.
 DEFINE INPUT  PARAMETER pDtAtuEndFim       AS DATE        NO-UNDO.
 
 DEFINE OUTPUT PARAMETER TABLE FOR ttJson.
 DEFINE OUTPUT PARAMETER cErro          AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER logSoEmEstoq   AS LOGICAL     NO-UNDO.
 DEFINE VARIABLE iPagina         AS INTEGER     NO-UNDO INIT 1.
 DEFINE VARIABLE cChaveBusca AS CHARACTER   NO-UNDO.
 
 DEFINE TEMP-TABLE ttJsonAux LIKE ttJson.
 DEFINE VARIABLE hTtJson    AS HANDLE      NO-UNDO.
 DEFINE VARIABLE hTtJsonAux AS HANDLE      NO-UNDO.

 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.

 ASSIGN hTTJson    = TEMP-TABLE ttJson:HANDLE
        hTTJsonAux = TEMP-TABLE ttJsonAux:HANDLE.


 IF pItCodigo = '' AND pCodRefer = '' THEN DO:
    ASSIGN cChaveBusca = 'Todos'.
 END.
 ELSE DO:
    ASSIGN cChaveBusca = pItCodigo  +  IF pCodRefer <> '' THEN "-" + pCodRefer
                                  ELSE ''.

 END.
 
 {lisa/propsComuns.i}
 ASSIGN iCalc = 62.
 RUN incluirErroRespBody   IN hBo('errorcode' , '400', '').
 RUN setSufixoJson         IN hBo('consultar_etiquetas').
 RUN setMetodo             IN hBO('GET').
 RUN setPathPrinc          IN hBO('rest/wms/v2/Rolo/query').
 IF pItCodigo <> '' THEN DO:
    RUN sincrParamUrl         IN hBo('produto',pITCodigo).
 END.
    
 IF pCodRefer <> '' THEN DO:
    RUN sincrParamUrl         IN hBo('lote',pCodRefer).
 END.
 IF logSoEmEstoq THEN DO:
    RUN sincrParamUrl         IN hBo('mostrarUsados','NO').
 END.
 
 
  
 RUN sincrParamUrl         IN hBo('pageSize',1000000).
 RUN sincrParamUrl         IN hBo('status','P').
 RUN sincrParamUrl         IN hBo('codigoCliente',cChave). //variavel cChave na include propsComuns.i

 IF pLogAtuEnd THEN
 DO:
    RUN sincrParamUrl         IN hBo('dataEnderecoDe',STRING(YEAR(pDtAtuEndIni)) + STRING(MONTH(pDtAtuEndIni),'99') + string(DAY(pDtAtuEndIni),'99')).
    RUN sincrParamUrl         IN hBo('dataEnderecoAte',STRING(YEAR(pDtAtuEndFim)) + STRING(MONTH(pDtAtuEndFim)) + string(DAY(pDtAtuEndFim))).
     
 END.
 
 REPEAT:
     RUN sincrParamUrl         IN hBo('page',iPagina).
     RUN exec                  IN hBo(iCalc,cChaveBusca).
     RUN getErros              IN hBo(OUTPUT cErro).    
     RUN getTTRetorno          IN hBo( OUTPUT TABLE ttJsonAux).
     hTTJson:COPY-TEMP-TABLE(httJsonAux,YES,?,?).
     IF cErro <> '' THEN LEAVE.
     ELSE DO:
         FIND FIRST ttJson
             WHERE ttJson.tag = 'hasNext' NO-LOCK NO-ERROR.
         IF NOT AVAIL ttJson THEN LEAVE.
         IF ttJson.valor <> 'TRUE' THEN LEAVE.
         ELSE DO:
             ASSIGN iPagina = iPagina + 1.
         END.
     END.
 END.

 RUN finalizar IN hBo.
 

