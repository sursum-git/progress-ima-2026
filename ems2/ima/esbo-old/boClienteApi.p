
/*------------------------------------------------------------------------
    File        : boClienteApi.p
    Purpose     : Possibilitar a integraá∆o de dado do progress com API's via cliente http

    Syntax      :

    Description : 

    Author(s)   : Tadeu Silva Parreiras
    Created     : Mon Mar 27 16:29:43 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


BLOCK-LEVEL ON ERROR UNDO, THROW.
USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING OpenEdge.Core.STRING FROM PROPATH.
USING OpenEdge.Net.HTTP.ClientBuilder FROM PROPATH.
USING OpenEdge.Net.HTTP.IHttpClient.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.RequestBuilder.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING Progress.Json.ObjectModel.JsonObject.
USING OpenEdge.Net.HTTP.Credentials FROM PROPATH.
USING OpenEdge.Net.URI FROM PROPATH.
//{esp/utilAPI.i}

{esp/utilJson.i}
{esp/util.i}
{esapi/extrairTextoEmPartes.i}
{esapi/analisarJsonObject2.i}
{utp/ut-glob.i}
{esbo/boMsg.i}

DEFINE TEMP-TABLE ttCodSitResp
FIELD codIni AS INT
FIELD codFim AS INT
FIELD tipo     AS INT  //1-sucesso, 2- erro requisicao, 3-erro servidor, 4- erro negocio
FIELD logErro  AS LOGICAL 
INDEX ind AS PRIMARY codIni codFim.

DEFINE TEMP-TABLE ttCondErroRespBody
    FIELD chave AS CHAR
    FIELD valor AS CHAR
    FIELD msgErro AS CHAR.

DEFINE VARIABLE iPorta              AS INTEGER          NO-UNDO.
DEFINE VARIABLE iCodSitResp         AS INTEGER          NO-UNDO.
DEFINE VARIABLE idTransacao         AS INTEGER          NO-UNDO.
DEFINE VARIABLE numCalculo          AS INTEGER          NO-UNDO.
DEFINE VARIABLE iPosIniJson         AS INTEGER          NO-UNDO.
DEFINE VARIABLE iPosFimJson         AS INTEGER     NO-UNDO.
DEFINE VARIABLE cMetodo             AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cDominio            AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cPathPrinc          AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cTipoConteudo       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cNomeCertificado    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaChavesSemColchetes AS CHARACTER   NO-UNDO.

DEFINE VARIABLE oResponse           AS IHttpResponse    NO-UNDO.
DEFINE VARIABLE oJsonResp           AS jsonObject       NO-UNDO.
DEFINE VARIABLE logHttps            AS LOGICAL          NO-UNDO.
DEFINE VARIABLE cScheme             AS CHARACTER        NO-UNDO.
DEFINE VARIABLE ojBody              AS jsonObject       NO-UNDO.
DEFINE VARIABLE lJsonDireto         AS LOGICAL          NO-UNDO.
DEFINE VARIABLE lRetirarNoRoot      AS LOGICAL          NO-UNDO.
DEFINE VARIABLE hboTransacao        AS HANDLE           NO-UNDO.
//DEFINE VARIABLE hBoLogCalc          AS HANDLE           NO-UNDO.
DEFINE VARIABLE hBoMsgCliApi        AS HANDLE           NO-UNDO.

DEFINE VARIABLE cDirJson        AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cSufixoJson     AS CHARACTER        NO-UNDO.
DEFINE VARIABLE cUrlJson        AS CHARACTER        NO-UNDO.

DEFINE VARIABLE opcaoTesteAPi   AS CHARACTER   NO-UNDO.


define temp-table ttParamsCab NO-UNDO
    field chave AS CHAR 
    field valor AS CHAR 
    index ind   AS PRIMARY chave.

define temp-table ttParamsUrl NO-UNDO
    field chave AS CHAR 
    field valor AS CHAR 
    index ind   AS PRIMARY  chave.
    
    
define temp-table ttParamsPath NO-UNDO
  field cId as char   .

DEFINE TEMP-TABLE ttArquivos  NO-UNDO
    FIELD arquivo       AS CHAR
    FIELD nomeArquivo   AS CHAR
    INDEX ind AS PRIMARY arquivo .

DEFINE TEMP-TABLE ttHandles NO-UNDO
    FIELD handleObj     AS HANDLE
    FIELD ordem         AS INT
    FIELD tipo          AS CHAR 
    INDEX ind AS PRIMARY handleObj 
    INDEX ind-ordem ordem .

PROCEDURE iniciar:

     RUN esbo/boTransacoes.p PERSISTENT SET hBoTransacao.
     //RUN esbo/boLogsCalculos.p PERSISTENT SET hBoLogCalc.
     //RUN iniciarBos IN hBoLogCalc.
     RUN esbo/boMsg.p PERSISTENT SET hBoMsgCliApi.
     RUN _criarFaixasPadroesCodSitResp.
     ASSIGN iPosIniJson = 10
            iPosFimJson = 15 .
     

END PROCEDURE.


PROCEDURE finalizar:


   RUN gravarLogCalculo IN hBoMsgCliApi(NumCalculo).
   IF VALID-HANDLE(hBoTransacao) THEN
      DELETE PROCEDURE hboTransacao.
   /*IF VALID-HANDLE(hBoLogCalc) THEN DO:
      RUN finalizarBos IN hBoLogCalc.
      DELETE PROCEDURE hBoLogCalc.
   END.*/
   IF VALID-HANDLE(hBoMsgCliApi) THEN
      DELETE PROCEDURE hBoMsgCliApi .

   DELETE PROCEDURE THIS-PROCEDURE.


END PROCEDURE.
/*
PROCEDURE setChaveTransacao:

    DEFINE INPUT  PARAMETER pCalculo AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pChave   AS CHARACTER   NO-UNDO.
    ASSIGN iCalculo = pCalculo.

    RUN setChave            IN hBoTransacao('').

END PROCEDURE.
*/

procedure setMetodo: //POST,GET etc
    
    DEFINE INPUT  PARAMETER pMetodo AS CHARACTER   NO-UNDO.
    ASSIGN cMetodo = pMetodo.
    
end PROCEDURE.

PROCEDURE setTipoConteudo:

    DEFINE INPUT  PARAMETER pTipo AS CHARACTER   NO-UNDO.
    ASSIGN cTipoConteudo = pTipo.

END PROCEDURE.

procedure setDominio:

    DEFINE INPUT  PARAMETER pDominio AS CHARACTER   NO-UNDO.
    ASSIGN cDominio = pDominio.
        
    
end procedure.

PROCEDURE setPorta:

    DEFINE INPUT  PARAMETER pPorta AS INTEGER     NO-UNDO.
    ASSIGN iPorta = pPorta.

END PROCEDURE.

PROCEDURE setPathPrinc:

    DEFINE INPUT  PARAMETER pPathPrinc AS CHARACTER   NO-UNDO.
    ASSIGN cPathPrinc = pPathPrinc .

END PROCEDURE.


procedure sincrChaveCab:

    DEFINE INPUT  PARAMETER pChave AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor AS CHARACTER   NO-UNDO.

    FIND FIRST ttParamsCab
        WHERE ttParamsCab.chave = pChave NO-ERROR.
    IF NOT AVAIL ttParamsCab THEN DO:
       CREATE ttParamsCab.
       ASSIGN ttParamsCab.chave = pChave
              ttParamsCab.valor = pValor. 
    END.
    ELSE DO:
        ASSIGN ttParamsCab.valor = pValor.
    END.

END PROCEDURE.

PROCEDURE sincrParamUrl:
    
    DEFINE INPUT  PARAMETER pChave AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER pValor AS CHARACTER NO-UNDO.

    FIND FIRST ttParamsUrl
        WHERE ttParamsUrl.chave = pChave NO-ERROR.
    IF NOT AVAIL ttParamsUrl THEN DO:
       CREATE ttParamsUrl.
       ASSIGN ttParamsUrl.chave = pChave
              ttParamsUrl.valor = pValor. 
    END.  
    ELSE DO:
       ASSIGN ttParamsUrl.valor = pValor .
    END.

END PROCEDURE.

procedure sincrParamPath:
    
    DEFINE INPUT  PARAMETER pChave AS CHARACTER NO-UNDO. 
    FIND FIRST ttParamsPath
        WHERE ttParamsPath.cId = pChave NO-ERROR.
    IF NOT AVAIL ttParamsPath THEN DO:
       CREATE ttParamsPath.
       ASSIGN ttParamsPath.cId = pChave. 
    END. 
    ELSE DO:
       ASSIGN ttParamsPath.cId = pChave. 
    END.
END PROCEDURE.  

                

PROCEDURE  sincrArqMultiPart:

    DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNome    AS CHARACTER   NO-UNDO.

    FIND FIRST ttArquivos
        WHERE ttArquivos.arquivo = pArquivo 
        NO-LOCK NO-ERROR.

    CREATE ttArquivos.
    ASSIGN ttArquivos.arquivo       = pArquivo
           ttArquivos.nomeArquivo   = pNome .


END PROCEDURE.

PROCEDURE  sincrHandle:

    DEFINE INPUT  PARAMETER pHandle AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER pOrdem  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo   AS CHARACTER   NO-UNDO.

    CREATE ttHandles .
    ASSIGN ttHandles.handleObj = pHandle 
           ttHandles.ordem     = pOrdem
           ttHandles.tipo      = pTipo  .


END PROCEDURE.

PROCEDURE aplicarPathTT:
    DEFINE INPUT-OUTPUT  PARAMETER oUri AS URI   NO-UNDO.
    DEFINE VARIABLE cLog AS CHARACTER   NO-UNDO.
    FOR EACH ttParamsPath :
        //RUN incrValor(cCaminho,ttParamsPath.cId,"/").
        oURI:addPathSegment(ttParamsPath.cId) .
        RUN incrValor(INPUT-OUTPUT cLog,ttParamsPath.cId, "/"  ).
    END.                                                         
    RUN setMsg       IN hBoMsgCliApi(1,'Path:' + cLog, 'log').
END PROCEDURE.

PROCEDURE aplicarQueryParamsTT:

    DEFINE INPUT-OUTPUT  PARAMETER oUri AS URI   NO-UNDO.
    DEFINE VARIABLE cLog AS CHARACTER   NO-UNDO.
    
    FOR EACH ttParamsUrl :
        //RUN incrValor(cCaminho,ttParamsPath.cId,"/").
        oURI:addQuery(ttParamsUrl.chave,ttParamsUrl.valor) .
        RUN incrValor(INPUT-OUTPUT cLog,ttParamsUrl.chave + "->" + ttParamsUrl.valor , ","  ).
    END.

    RUN setMsg       IN hBoMsgCliApi(2,'Parametros URL:' + cLog, 'log').

END PROCEDURE.

PROCEDURE aplicarParamsCab:

    DEFINE INPUT-OUTPUT PARAMETER oRequest AS RequestBuilder NO-UNDO.
    DEFINE VARIABLE cLog AS CHARACTER   NO-UNDO.

    /*MESSAGE 'passei no cab'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    FOR EACH ttParamsCab:
        /*MESSAGE ttParamsCab.chave SKIP
                ttParamsCab.valor
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        oRequest:addHeader(ttParamsCab.chave, ttParamsCab.valor).
        RUN incrValor(INPUT-OUTPUT cLog,ttParamsCab.chave + "->" + ttParamsCab.valor , ","  ).
    END.
    RUN setMsg       IN hBoMsgCliApi(3,'Parametros Cabeáalho:' + cLog,'log').

END PROCEDURE.

PROCEDURE aplicarArqMultiPart:

    //T O D O

END PROCEDURE.

PROCEDURE setObjJsonBody:
    DEFINE INPUT  PARAMETER pObj AS jsonobject   NO-UNDO.
    DEFINE VARIABLE cJson AS CHARACTER   NO-UNDO.

    ASSIGN ojBody           = pObj
           lJsonDireto      = YES.
    
    RUN convJson2Char(INPUT pObj, OUTPUT cJson).

    /*ASSIGN lcJson   = pObj:GetJsonText()
           cJson    = lcJson .*/
    RUN setMsg       IN hBoMsgCliApi(4,'JSON:' + cJson, 'log').
    
END PROCEDURE.



PROCEDURE aplicarBody:

    DEFINE INPUT-OUTPUT PARAMETER oRequest  AS RequestBuilder NO-UNDO.
    DEFINE VARIABLE oJsonArrayBody          AS JsonArray      NO-UNDO.
    DEFINE VARIABLE oJsonObjectBody         AS JsonObject     NO-UNDO.
    //DEFINE VARIABLE oJsonObjectTST          AS JsonObject     NO-UNDO.
    DEFINE VARIABLE lAchou                  AS LOGICAL        NO-UNDO.
    DEFINE VARIABLE lcJson                  AS LONGCHAR       NO-UNDO.
    DEFINE VARIABLE cJson                   AS CHARACTER      NO-UNDO.
    DEFINE VARIABLE cNomeArq                AS CHARACTER   NO-UNDO.
    //DEFINE VARIABLE myParser                AS ObjectModelParser NO-UNDO. 

    /*oJsonObjectTST = NEW jsonObject().
    oJsonObjectTST:ADD('nome','adriano').
    oJsonObjectTST:ADD('aitens','').*/
    oJsonArrayBody = NEW jsonArray().
    IF lJsonDireto = FALSE THEN
        FOR EACH ttHandles BY ttHandles.ordem .
            oJsonObjectBody = NEW jsonObject().
            CASE ttHandles.tipo:
                WHEN 'tt' THEN DO:
                    ttHandles.handleObj:WRITE-JSON(  'jsonobject',
                                                     oJsonObjectBody,
                                                     FALSE,
                                                     'utf-8',FALSE,FALSE).
                END.
                WHEN 'dataset' THEN DO:
                    ttHandles.handleObj:WRITE-JSON(  'jsonobject',
                                                     oJsonObjectBody,
                                                     FALSE, 
                                                     'utf-8',FALSE,YES).
                END.
                /**WHEN 'STRING' THEN DO:
                    myParser        = NEW ObjectModelParser(). 
                    oJsonObjectBody = CAST(myParser:Parse(myJs ),JsonObject).
                END.**/
    
            END CASE.                                 
    
            oJsonArrayBody:ADD(oJsonObjectBody).
            ASSIGN lAchou = YES.
        END.
    ELSE DO:
       //RUN _retirarNoRoot(INPUT-OUTPUT ojBody).
       ASSIGN lAchou = NO.
       oRequest:withData(ojBody).
       RUN gravarArqJson(ojBody,'envio', OUTPUT cNomeArq).           
       RUN setMsg       IN hBoMsgCliApi(5,"JSON ENVIO DIRETO:"  + cNomeArq, 'log').

    END.
    IF lAchou THEN DO:
       RUN _retirarNoRoot(INPUT-OUTPUT oJsonObjectBody).
       oRequest:withData(oJsonObjectBody).
       RUN gravarArqJson(oJsonObjectBody,'envio', OUTPUT cNomeArq).
       RUN setMsg       IN hBoMsgCliApi(6,"Nome Arquivo Json Envio:"  + cNomeArq, 'log').
    END.
    
END PROCEDURE.

PROCEDURE setHttps:
    
    DEFINE INPUT  PARAMETER lParam AS LOGICAL     NO-UNDO.
    ASSIGN logHttps = lParam.
    IF logHttps THEN
       ASSIGN cScheme = 'https'.
    ELSE
       ASSIGN cScheme = 'http'.
    

END PROCEDURE.


PROCEDURE retirarNoRoot:

    DEFINE INPUT  PARAMETER lParam AS LOGICAL     NO-UNDO.

    ASSIGN lRetirarNoRoot = lParam .



END PROCEDURE.




PROCEDURE  _aplicarCertHttps:

    DEFINE VARIABLE cDir            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hBoCertHttps    AS HANDLE      NO-UNDO.
    DEFINE VARIABLE cErro           AS CHARACTER   NO-UNDO.
    RUN _getDirCert(OUTPUT cDir).

    RUN esbo/boCertHttps.p PERSIST SET hBoCertHttps.
    RUN setDirCertificados IN hBoCertHttps(cDir).
    RUN setNomeCertificado IN hBoCertHttps(cNomeCertificado).
    RUN exec    IN hBoCertHttps NO-ERROR .
    RUN getErro IN hBoCertHttps(OUTPUT cErro).
    RUN setMsg  IN hBoMsgCliApi(99,"Erro na Aplicaá∆o do certificado: " + cErro,'erro').





END PROCEDURE.
    
    
PROCEDURE setSomenteGerarJsonSaida:



END PROCEDURE.

PROCEDURE exec:

    DEFINE INPUT  PARAMETER pNumCalculo   AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pChave        AS CHARACTER   NO-UNDO.


    DEFINE VARIABLE oRequest        AS RequestBuilder   NO-UNDO.
    DEFINE VARIABLE oHttpRequest    AS IHttpRequest     NO-UNDO.
    DEFINE VARIABLE oURI            AS URI              NO-UNDO.
    DEFINE VARIABLE cCaminho        AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE oClient         AS IHttpClient      NO-UNDO.
    //DEFINE VARIABLE lcJsonResp      AS LONGCHAR         NO-UNDO.
    DEFINE VARIABLE cNomeArq         AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE iSitTransacao    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cDescrMsg       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iTipoCalc        AS INTEGER     NO-UNDO.

    
    IF opcaoTesteAPI <> '' THEN
       RUN _setVarsTestarAPI(opcaoTesteAPI).

    IF logHttps THEN DO:
       IF cNomeCertificado = '' THEN DO:
          RUN setMsg  IN hBoMsgCliApi(98,"Nome do Certificado n∆o informado para uma conex∆o https. Utilize a procedure setNomeCert para informar o nome do certificado",'erro').  
       END.
       ELSE DO:
          RUN _aplicarCertHttps.
       END.
    END.

    

    RUN gerarTransacao IN hBoTransacao(
        INPUT 'boClienteAPI',
        INPUT c-seg-usuario,
        INPUT pNumCalculo,
        INPUT pChave,
        OUTPUT idTransacao
    ).

    

    RUN setTransacaoLogCalculo IN hBoMsgCliApi(idTransacao).
    ASSIGN numcalculo = pNumCalculo.
    
    RUN aplicarUrl(OUTPUT oURI).
    RUN aplicarPathTT(INPUT-OUTPUT oURI).
    RUN aplicarQueryParamsTT(INPUT-OUTPUT oURI).

    /*MESSAGE 'cheguei ate aqui'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

    
    oRequest = RequestBuilder:build(cMetodo,oURI):AcceptJson().
    RUN aplicarParamsCab(INPUT-OUTPUT oRequest).            
    RUN setMsg       IN hBoMsgCliApi(7,'Metodo:' + cMetodo, 'log').
    
    
    /*CASE cMetodo:
        WHEN 'post' THEN DO:
            oRequest =  RequestBuilder:POST(oURI:toSTring()):AcceptJson():AddHeader('Authorization','Basic YXBpd21zOmFwaXdtcyMyNDVA' ).
            

        END.
        WHEN 'get' THEN DO:
            oRequest =  RequestBuilder:GET(oURI:toSTring()):AcceptJson():AddHeader('Authorization','Basic YXBpd21zOmFwaXdtcyMyNDVA' ).

        END.

    END CASE.*/

    //n∆o est† aplicando os headers nos objetos abrir chamado
    //RUN aplicarParamsCab(INPUT-OUTPUT oRequest).

    //RUN aplicarArqMultiPart(INPUT-OUTPUT oRequest).

    RUN aplicarBody(INPUT-OUTPUT oRequest).




   /* MESSAGE "request:"  oRequest:toString()
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

    /* n∆o precisa 
    IF cTipoConteudo <> '' THEN
       oRequest:AcceptContentType(cTipoConteudo).*/
    
    
    DO TRANSACTION:
    
        oHttpRequest = oRequest:REQUEST .
    
        oClient = ClientBuilder:Build():Client.  
        oResponse = oClient:Execute(oHttpRequest).
    
        ASSIGN iCodSitResp = oResponse:statuscode.
       
    
    
        //implementar condiá∆o de contentType para convers∆o
        IF oResponse:ContentLength > 0 THEN DO:
           oJsonResp = cast(oResponse:Entity,jsonobject ).
           
           RUN gravarArqJson(oJsonResp,'retorno', OUTPUT cNomeArq).
        END.
    
        RUN setMsg       IN hBoMsgCliApi(8,"Nome Arquivo Json Retorno: " + cNomeArq,'log') NO-ERROR.
    
        RUN esapi/analisarJsonObject2.p(oJsonResp, OUTPUT TABLE ttJson) NO-ERROR.
        
        RUN getTpCalcEMsgCodSitResp(iCodSitResp, OUTPUT iTipoCalc, OUTPUT cDescrmsg, OUTPUT iSitTransacao) NO-ERROR.
    
        RUN setMsg       IN hBoMsgCliApi(0,cDescrMsg + ":" +  string(iCodSitResp), 'aviso') NO-ERROR.
        
        RUN setErroRespBody NO-ERROR.
    
    
        IF VALID-HANDLE(hBoTransacao) THEN
           RUN finalizarTransacao  IN hBoTransacao(iSitTransacao) NO-ERROR.
        
        
        CATCH oError AS Progress.Lang.Error :
           
    
            RUN setErroRespBody.
    
            IF VALID-HANDLE(hboTransacao) THEN
               RUN finalizarTransacao IN hBoTransacao(3).
            IF VALID-HANDLE(hBoMsgCliApi) THEN DO:
               RUN setMsg       IN hBoMsgCliApi(9,'Erro:' + oError:GetMessage(1), 'erro').
               RUN setMsg       IN hBoMsgCliApi(10,'Detalhe Erro:' + oError:CallStack, 'erro').
            END.
    
            /*MESSAGE 'Erro:' + oError:GetMessage(1) SKIP
                    'Detalhe Erro:' + oError:CallStack
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            
            RUN finalizar .                
        END CATCH.
   END.

END PROCEDURE.

PROCEDURE getTpCalcEmsgCodSitResp:

    DEFINE INPUT  PARAMETER pcodSitResp     AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iTipoCalc       AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cDescrMsg       AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iSitTransacao   AS INTEGER     NO-UNDO.
     
    

    FIND FIRST ttCodSitResp
        WHERE ttCodsitResp.codIni <= pcodSitResp
        AND   ttCodSitResp.codFim >= pcodSitResp
        NO-ERROR.
    IF AVAIL ttCodSitResp THEN DO:
       IF ttCodSitResp.logErro THEN DO:
          ASSIGN itipoCalc = 2.
          // ,3-redirecionamento   4- erro cliente, 5-erro servidor
          CASE ttCodsitResp.tipo:
              WHEN 4 THEN DO:
                 ASSIGN iSitTransacao  = 4 // erro de negocio
                         cDescrMsg     = ' Erro de Neg¢cio'.
              END.
              WHEN 5 THEN DO:
                  ASSIGN iSitTransacao  = 2 //cancelada
                         cDescrMsg     = ' Erro de Servidor'.
              END.  
              OTHERWISE DO:
                  ASSIGN iSitTransacao = 9 // erro desconhecido
                         cDescrMsg    = 'Erro n∆o mapeado'.
              END.
          END CASE.   
       END.
       ELSE DO:
         ASSIGN iTipoCalc     = 3.
         CASE ttCodsitResp.tipo:  
             WHEN 1 THEN DO: //1-informacao
                  ASSIGN iSitTransacao  = 1 // transacao concluida
                         cDescrMsg     = ' Informaá∆o de Requisiá∆o'.
              END.
             WHEN 2 THEN DO: //2-sucesso
             ASSIGN  iSitTransacao = 1
                     cDescrMsg = 'Sucesso na Requisiá∆o' .
             END.
             WHEN 3 THEN DO: //2-Redirecionamento
             ASSIGN  iSitTransacao = 1
                     cDescrMsg = 'Sucesso no Redirecionamento  ' .
             END.
              OTHERWISE DO: // codigo desconhecido
                  ASSIGN iSitTransacao = 1 
                         cDescrMsg    = 'C¢digo Desconhecido'.
              END.
         END CASE.
       END.

    END.
    ELSE DO:
        ASSIGN  iSitTransacao = 1
                cDescrMsg = 'ATENÄ«O  - Sem Codigos de Situaá∆o Cadastrados - N∆o foi poss°vel definir se a requisiá∆o teve ou n∆o sucesso' 
                iTipoCalc     = 3. 
    END.

END PROCEDURE.



PROCEDURE setLogLongChar:
    DEFINE INPUT  PARAMETER pTexto AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE cParte AS CHARACTER   NO-UNDO.
    RUN esapi/extrairTextoEmPartes.p(pTexto,31999,OUTPUT TABLE ttTexto).

    FOR EACH ttTexto.
        RUN setMsg       IN hBoMsgCliApi(11,'JSON(PARTE ' + STRING(ttTexto.ordem) +  '):' + ttTexto.texto, 'log').
    END.
END PROCEDURE.


PROCEDURE getStatusRetorno:

    DEFINE OUTPUT PARAMETER iRetorno AS INTEGER     NO-UNDO.
    ASSIGN iRetorno = oResponse:StatusCode.


END PROCEDURE.

 
PROCEDURE getTipoConteudoRetorno:

    DEFINE OUTPUT PARAMETER cRetorno AS CHARACTER NO-UNDO.
    ASSIGN cRetorno = oResponse:ContentType.

END PROCEDURE.


PROCEDURE getTTRetorno:

    DEFINE OUTPUT PARAMETER TABLE FOR ttJson.


END PROCEDURE.

PROCEDURE limparTTs:

    EMPTY TEMP-TABLE ttJson       .
    EMPTY TEMP-TABLE ttParamsCab  .
    EMPTY TEMP-TABLE ttParamsPath .
    EMPTY TEMP-TABLE ttParamsUrl  .
    EMPTY TEMP-TABLE ttArquivos   .
    EMPTY TEMP-TABLE ttHandles    .
    EMPTY TEMP-TABLE ttCondErroRespBody.


END PROCEDURE.

PROCEDURE limparTtCodSitResp .

    EMPTY TEMP-TABLE ttCodSitResp .

END PROCEDURE.


PROCEDURE setPosIniJson:
    DEFINE INPUT  PARAMETER pPos AS INTEGER     NO-UNDO.
    ASSIGN iPosIniJson = pPos.

END PROCEDURE.


PROCEDURE setPosFimJson:
    DEFINE INPUT  PARAMETER pPos AS INTEGER     NO-UNDO.
    ASSIGN iPosFimJson = pPos.

END PROCEDURE.


PROCEDURE setNomeCert:

    DEFINE INPUT  PARAMETER pCert AS CHARACTER   NO-UNDO.
    ASSIGN cNomeCertificado = pCert.

END PROCEDURE.

PROCEDURE _getDirCert:

    DEFINE OUTPUT PARAMETER cDir AS CHARACTER   NO-UNDO.

    RUN getVlParametro('dir-cert-https',OUTPUT cDir).
    IF cDir = '' THEN
       ASSIGN cDir = 't:\certs'.

END PROCEDURE.


PROCEDURE setListaChavesSemColchete:

    DEFINE INPUT  PARAMETER pLista AS CHARACTER   NO-UNDO.
    ASSIGN cListaChavesSemColchetes = pLista .

END PROCEDURE.

PROCEDURE _retirarNoRoot.

    DEFINE INPUT-OUTPUT  PARAMETER pObj AS jsonObject  NO-UNDO.
    DEFINE VARIABLE iRand        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lcPostData   AS LONGCHAR    NO-UNDO. 
    DEFINE VARIABLE mvar         AS MEMPTR      NO-UNDO.
    DEFINE VARIABLE cNomeArq     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE myParser     AS ObjectModelParser NO-UNDO.
    DEFINE VARIABLE cInicio      AS CHARACTER   NO-UNDO.

    ASSIGN iRand    = RANDOM ( 1 , 99999999 )
           cNomeArq = SESSION:TEMP-DIRECTORY + STRING(iRand) + ".json" .
    
    pObj:Write(INPUT-OUTPUT lcPostData,TRUE,'utf-8':U  ).

    /*
    lcPostData = left-trim(lcPostData, chr(13)). 
    lcPostData = right-trim(lcPostdata, chr(13)). 
    lcPostData = left-trim(lcPostData, chr(10)). 
    lcPostData = right-trim(lcPostdata, chr(10)). 
    lcPostData = left-trim(lcPostData, '[ ':U). 
    lcPostData = right-trim(lcPostdata, ' ]':U).
    */
    ASSIGN lcPostData = substr(lcPostData,iPosIniJson,LENGTH(lcPostData) - iPosFimJson  ).
    IF cListaChavesSemColchetes <> '' THEN DO:
       RUN _retirarColchetePorListaDeChaves(INPUT-OUTPUT lcPostData).
    END.
       

    /*MESSAGE LENGTH(lcPostData) SKIP
            '-' + cInicio '-' SKIP
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    COPY-LOB lcPostData TO FILE cNomeArq .
    
    myParser = NEW ObjectModelParser().

    pObj = CAST(myParser:Parse(lcPostData), JsonObject) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
       MESSAGE 'n∆o foi possivel converter para um objeto json'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

    /*IF(SEARCH(cNomeArq)) <> ?  THEN DO:
        COPY-LOB FILE cNomeArq TO mvar.
        COPY-LOB mvar TO lcPostData.


    END.  */


END PROCEDURE.

PROCEDURE _retirarColchetePorListaDeChaves:

    DEFINE INPUT-OUTPUT PARAMETER  pLcJson        AS LONGCHAR .
    DEFINE VARIABLE lcNovoJson     AS LONGCHAR    NO-UNDO.
    DEFINE VARIABLE iCont           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cChaveCorrente  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cParte          AS CHARACTER   NO-UNDO.
    REPEAT iCont = 1 TO NUM-ENTRIES(pLcJson,','):
        ASSIGN cParte = ENTRY(1,plcJson,',')
               cChaveCorrente = ENTRY(1,cParte,":")
               cChaveCorrente = REPLACE(cChaveCorrente,'"','').
        IF LOOKUP(cChaveCorrente,cListaChavesSemColchetes) > 0 THEN DO:
           ASSIGN cParte = REPLACE(cParte,'[','')
                  cParte = REPLACE(cParte,']','').
        END.
        RUN incrValor(INPUT-OUTPUT lcNovoJson,cParte,"," ).
    END.
    ASSIGN pLcJson = lcNovoJson .
END PROCEDURE.


PROCEDURE aplicarUrl:

    DEFINE OUTPUT PARAMETER oURI AS URI . 

    oUri        = NEW URI(cscheme,cDominio).    

    IF iPorta > 0 THEN
       oUri:port   = iPorta .
    IF cPathPrinc <> '' THEN
       oUri:Path   = cPathPrinc .   

    RUN setMsg       IN hBoMsgCliApi(12,'URL:'    + cscheme + "://" + cDominio, 'log').
    RUN setMsg       IN hBoMsgCliApi(13,'porta:'  + STRING(iPorta), 'log').  
    RUN setMsg       IN hBoMsgCliApi(14,'path Principal:'  +  oURI:Path, 'log').  
    


END PROCEDURE.

PROCEDURE gravarArqJson:
    DEFINE INPUT  PARAMETER pJson     AS jsonobject   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo     AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cLinkArq  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cArqPuro AS CHARACTER   NO-UNDO.
    RUN getNomeArqJson(pTipo,OUTPUT cArquivo).
    /*MESSAGE cArquivo
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    RUN saveJson2File(pJson,cArquivo).
    IF cUrlJson <> '' THEN DO:
       ASSIGN cArqPuro =  getNomeArqPuro(cArquivo).
       ASSIGN cLinkArq =  cUrlJson + "/" + cArqPuro.
    END.
    ELSE 
       ASSIGN cLinkArq = string(IdTransacao) + "-" +  cArquivo.

END PROCEDURE.



PROCEDURE getNomeArqJson:
    DEFINE INPUT  PARAMETER pTipo    AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.            
    DEFINE VARIABLE cSufixoLocal     AS CHARACTER   NO-UNDO.

   RUN getVlParametro(cSufixoJson + '_json', OUTPUT cVlParam ).
   IF cVlParam = '' THEN
      ASSIGN cSufixoLocal = cSufixoJson + '_'.
   ELSE 
      ASSIGN cSufixoLocal = cVlParam.


    ASSIGN cVlParam = cDirJson + "/" + STRING(idTransacao) + "-" + cSufixoLocal +  pTipo  +  '.json'.
END.


PROCEDURE setUrlJson:
    
    DEFINE INPUT  PARAMETER pUrl AS CHARACTER   NO-UNDO.
    ASSIGN cUrlJson = pUrl.

END PROCEDURE.

PROCEDURE setSufixoJson:

    DEFINE INPUT  PARAMETER pSufixo AS CHARACTER   NO-UNDO.
    ASSIGN cSufixoJson = pSufixo .

END PROCEDURE.

PROCEDURE setDirJson:

    DEFINE INPUT  PARAMETER pDirJson AS CHARACTER   NO-UNDO.
    ASSIGN cDirJson = pDirJson.

END PROCEDURE.


PROCEDURE sincrFaixaCodSitResp:

    //implementaá∆o mais simples onde s¢ existir† uma faixa por tipo

    DEFINE INPUT  PARAMETER pCodIni AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pcodFim AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo   AS INTEGER     NO-UNDO.


    DEFINE BUFFER bf FOR ttCodSitResp .

    FIND bf WHERE bf.tipo = pTipo NO-ERROR.
    IF AVAIL bf THEN DO:
        DELETE bf.
    END.

    CREATE  ttCodSitResp.
    ASSIGN ttCodSitResp.codIni      = pCodIni
           ttCodSitResp.codFim      = pCodFim
           ttCodSitResp.tipo        = pTipo
           ttCodSitResp.logErro     = pTipo  > 3 . //1-informacao 2-sucesso 3-redirecionamento 4-erro cliente 5-erro servidor 

END PROCEDURE.


PROCEDURE _criarFaixasPadroesCodSitResp:

    RUN sincrFaixaCodSitResp(100,199,1).
    RUN sincrFaixaCodSitResp(200,299,2).
    RUN sincrFaixaCodSitResp(300,399,3).
    RUN sincrFaixaCodSitResp(400,499,4).
    RUN sincrFaixaCodSitResp(500,599,5).

END PROCEDURE.



PROCEDURE incluirErroRespBody:
    DEFINE INPUT  PARAMETER pChave      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pMsgErro  AS CHARACTER   NO-UNDO.

    FIND ttCondErroRespBody
        WHERE ttCondErroRespBody.chave = pChave NO-ERROR.
    IF NOT AVAIL ttCondErroRespBody THEN DO:
       CREATE ttCondErroRespBody.
       ASSIGN ttCondErroRespBody.chave = pChave .
    END.
    ASSIGN ttCondErroRespBody.valor     = pValor
           ttCondErroRespBody.MsgErro   = pMsgErro .
           

END PROCEDURE.

PROCEDURE setErroRespBody:
    
    DEFINE VARIABLE lErro   AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iId     AS INTEGER     NO-UNDO INIT 100.
    DEFINE VARIABLE cErro   AS CHARACTER   NO-UNDO.

    FOR EACH ttConderroRespBody:
        /*MESSAGE 'chave:' ttConderroRespBody.chave SKIP
                'valor:' ttConderroRespBody.valor
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        FIND FIRST ttJson
        WHERE ttJson.tag   = ttConderroRespBody.chave
        AND   ttJson.valor = ttConderroRespBody.valor NO-ERROR.
        IF AVAIL ttJson THEN DO:
           /*MESSAGE  'entrei no match'
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
           ASSIGN lErro = YES.
        END.
    END.

    IF lErro THEN DO:
       FOR EACH ttJson:
           ASSIGN iId = iId + 1 .
           ASSIGN cErro = ttJson.tag + "->" + ttJson.valor.
           FIND ttCondErroRespBody
               WHERE ttJson.tag   = ttConderroRespBody.chave
               AND   ttJson.valor = ttConderroRespBody.valor NO-ERROR.
           IF AVAIL ttCondErroRespBody AND ttConderroRespBody.msgErro <> '' THEN
               ASSIGN cErro = cErro + "(" +  ttConderroRespBody.msgErro + ")".
           RUN setMsg       IN hBoMsgCliApi(iId,
                                         cErro,
                                             'erro').  
       END.

    END.
END PROCEDURE.


PROCEDURE getErros:

    DEFINE OUTPUT PARAMETER cErro AS CHARACTER   NO-UNDO.

    RUN getErros IN hBoMsgCliApi(OUTPUT cErro).
    RUN expttMsg IN hBoMsgCliApi('c:\temp\erro_LISA_Detalhe_').

END PROCEDURE.



PROCEDURE _setVarsTestarAPI:

    DEFINE INPUT  PARAMETER cOpcao AS CHARACTER   NO-UNDO.
    
   CASE cOpcao:
       WHEN 'php' THEN DO:
           RUN setDominio('imaonline.imatextil.com.br').
           RUN setPorta(0).
           RUN setPathPrinc('/iol/bl_valida_cli_lisa/bl_valida_cli_lisa.php').
       END.
       WHEN 'webhook' THEN DO:
           RUN setDominio('webhook.site').
           RUN setPorta(0).
           RUN setPathPrinc('/2719da86-7249-4c7e-aa3e-df77def7e677').
           RUN setMetodo('POST').
       END.
       WHEN 'progress' THEN DO:
           RUN setDominio('192.168.0.38').
           RUN setPorta(8080).
           RUN setPathPrinc('/api/fnd/v1/tst').
           RUN sincrChaveCab('Authorization','Basic c3VwZXI6c3VwZXI='). 
       END.
   END CASE.


END PROCEDURE.


PROCEDURE setOpcaoTestarAPI:
    
    DEFINE INPUT  PARAMETER pOpcao AS CHARACTER   NO-UNDO.
    
    ASSIGN opcaoTesteAPI = pOpcao .

END PROCEDURE.
