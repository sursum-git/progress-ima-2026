/********************************************************************************
programa:boAPIsLisa
objetivo: Possibilitar a integra‡Æo com as APIs da Logistica LISA (porto de ITAJAI/SC)
otimizando o processo de atribui‡Æo de parametros de autentica‡Æo e identifica‡Æo
junto a LISA e utilizando a BO -> boCLienteApi como base 
ator: Tadeu Silva Parreiras
data:04/2023               
********************************************************************************/
USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

{esp/util.i}
{esapi/analisarJsonObject2.i}

DEFINE VARIABLE hBoClienteAPI   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoConsParam    AS HANDLE      NO-UNDO.
DEFINE VARIABLE cAutorizacao    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cChave          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCNPJ           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFilial         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSenha          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArmazem        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDominio        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVersaoApi      AS CHAR        NO-UNDO.
DEFINE VARIABLE iPorta          AS INT         NO-UNDO.
DEFINE VARIABLE lRetirarNoRoot  AS LOGICAL     NO-UNDO.
//DEFINE VARIABLE logTestarAPI AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cTenantid       AS CHARACTER   NO-UNDO.

PROCEDURE iniciar:

  RUN esbo/boClienteAPI.p PERSIST SET hBoClienteAPI.
  RUN iniciar IN hBoClienteApi.
  /*RUN sincrChaveCab IN hBoClienteAPI('Authorization',cAutorizacao).
  RUN sincrParamUrl IN hBoClienteAPI('CHAVE',cChave).
  RUN sincrParamUrl IN hBoClienteAPI('CGC',cCNPJ).
  RUN sincrParamUrl IN hBoClienteAPI('FILIALLISA',cFilial).
  RUN sincrParamUrl IN hBoClienteAPI('SENHA',cSenha).
  RUN sincrParamUrl IN hBoClienteAPI('CHAVE',cChave).
  RUN sincrParamUrl IN hBoClienteAPI('ARMAZEM',cArmazem).*/
  RUN setHttps      IN hBoClienteAPI(NO).
  RUN setTipoConteudo IN hBoClienteAPI('application/json').
   /*MESSAGE 'ponto 1:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

  RUN esbo/boConsParam.p PERSIST SET hBoConsParam.

END PROCEDURE.

PROCEDURE finalizar:

    IF VALID-HANDLE(hBoClienteAPI) THEN do:
       RUN finalizar IN hBoClienteApi.
    END.
    IF VALID-HANDLE(hBoClienteAPI) THEN
       DELETE PROCEDURE hBoClienteAPI.

    IF VALID-HANDLE(hBoConsParam) THEN
       DELETE PROCEDURE hboConsParam.

    DELETE PROCEDURE THIS-PROCEDURE.
   

END PROCEDURE.

PROCEDURE retirarNoRoot:

    DEFINE INPUT  PARAMETER lParam AS LOGICAL     NO-UNDO.
    ASSIGN lRetirarNoRoot = lParam.

END PROCEDURE.

PROCEDURE getDadosAutenticacao:

    DEFINE INPUT  PARAMETER pChave AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cValor AS CHARACTER   NO-UNDO.

    CASE pChave:
        WHEN 'token' THEN
            ASSIGN cValor = cAutorizacao .
        WHEN 'chave' THEN 
            ASSIGN cValor  = cChave.
        WHEN 'cnpj' THEN  
            ASSIGN cValor  = cCNPJ.
        WHEN 'filial' THEN
            ASSIGN cValor  = cFilial . 
        WHEN 'senha' THEN
            ASSIGN cValor = cSenha.
        WHEN 'armazem' THEN
            ASSIGN cValor = cArmazem.
    END CASE.

END PROCEDURE.


PROCEDURE setOpcaoTestarAPI:
    DEFINE INPUT  PARAMETER pOpcao AS CHARACTER   NO-UNDO.
    RUN setOpcaoTestarAPI IN hBoClienteAPi(pOpcao).

END PROCEDURE.



PROCEDURE enviarDadosNFImportacao:


    DEFINE INPUT  PARAMETER hObj    AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iNumCalc        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEndPoint       AS CHARACTER   NO-UNDO.
    RUN setApi('/rest/NOTAENTRADA','POST').
    ASSIGN cEndPoint = 'enviar_nota_import'.
    /*MESSAGE 'ponto 1:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    RUN sincrHandle     IN hBoClienteAPI(hObj,1,pTipo).
    //RUN retirarNoRoot   IN hBoClienteAPI(lRetirarNoRoot).
    //RUN _getNumCalcIntr(INPUT cEndPoint, OUTPUT iNumCalc).
    //RUN setSufixoJson   IN hBoClienteAPI(cEndPoint).
    //RUN setVersaoApi('').                           

    RUN exec            IN hBoCLienteAPI(INPUT 0,'13214').
    /*MESSAGE 'cheguei aqui sem erro'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    /**************************************************
       MESSAGE 'ponto 2:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     ***************************************************/

END PROCEDURE.


PROCEDURE enviarDadosRolosNFImportacao:

    DEFINE INPUT  PARAMETER hObj    AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iNumCalc        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEndPoint       AS CHARACTER   NO-UNDO.
    ASSIGN cEndPoint = 'enviar_etq_nota_import'.

    RUN setApi('rest/wms/v2/EntradaRolo','POST').
    /*MESSAGE 'ponto 1:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

    RUN sincrHandle     IN hBoClienteAPI(hObj,1,pTipo).
    RUN retirarNoRoot   IN hBoClienteAPI(lRetirarNoRoot).
    RUN _getNumCalcIntr(INPUT cEndPoint, OUTPUT iNumCalc).
    RUN setSufixoJson   IN hBoClienteAPI(cEndPoint).
    RUN setVersaoApi('v2').
    RUN exec            IN hBoCLienteAPI(iNumCalc,pChave).

    /*MESSAGE 'ponto 2:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

END PROCEDURE.


PROCEDURE enviarPedidoVenda:

    DEFINE INPUT  PARAMETER hObj    AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iNumCalc        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEndPoint       AS CHARACTER   NO-UNDO.
    ASSIGN cEndPoint = 'enviar_pedido_venda'.

    RUN setApi('rest/wms/v2/PrePedidoRolo','POST').
    /*MESSAGE 'ponto 1:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

    RUN sincrHandle     IN hBoClienteAPI(hObj,1,pTipo).
    RUN retirarNoRoot   IN hBoClienteAPI(lRetirarNoRoot).
    RUN _getNumCalcIntr(INPUT cEndPoint, OUTPUT iNumCalc).
    RUN setSufixoJson   IN hBoClienteAPI(cEndPoint).
    RUN setVersaoApi('v2').
    RUN exec            IN hBoCLienteAPI(iNumCalc,pChave).

    /*MESSAGE 'ponto 2:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

END PROCEDURE.

PROCEDURE avaliarSeparacaoPedido:

    DEFINE INPUT  PARAMETER hObj    AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iNumCalc        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEndPoint       AS CHARACTER   NO-UNDO.
    ASSIGN cEndPoint = 'avaliar_separacao_pedido_venda'.

    RUN setApi('rest/wms/v2/AprovaReprovaSeparacaoPrePedidoRolo','POST').

    /*MESSAGE 'ponto 1:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

    RUN sincrHandle     IN hBoClienteAPI(hObj,1,pTipo).
    RUN retirarNoRoot   IN hBoClienteAPI(lRetirarNoRoot).
    RUN _getNumCalcIntr(INPUT cEndPoint, OUTPUT iNumCalc).
    RUN setSufixoJson   IN hBoClienteAPI(cEndPoint).
    RUN setVersaoApi('v2').
    RUN exec            IN hBoCLienteAPI(iNumCalc,pChave).

    /*MESSAGE 'ponto 2:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

END PROCEDURE.

PROCEDURE alterarPedVenda:

    DEFINE INPUT  PARAMETER hObj    AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iNumCalc        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEndPoint       AS CHARACTER   NO-UNDO.
    ASSIGN cEndPoint = 'alterar_pedido_venda'.

    RUN setApi('rest/wms/v2/PrePedidoRolo','PUT').
    /*MESSAGE 'ponto 1:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

    RUN sincrHandle     IN hBoClienteAPI(hObj,1,pTipo).
    RUN retirarNoRoot   IN hBoClienteAPI(lRetirarNoRoot).
    RUN _getNumCalcIntr(INPUT cEndPoint, OUTPUT iNumCalc).
    RUN setSufixoJson   IN hBoClienteAPI(cEndPoint).
    RUN setVersaoApi('v2').
    RUN exec            IN hBoCLienteAPI(iNumCalc,pChave).


END PROCEDURE.

PROCEDURE excluirPedVenda:

    DEFINE INPUT  PARAMETER hObj    AS HANDLE      NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iNumCalc        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEndPoint       AS CHARACTER   NO-UNDO.
    ASSIGN cEndPoint = 'excluir_pedido_venda'.

    RUN setApi('rest/wms/v2/PrePedidoRolo','delete').
    /*MESSAGE 'ponto 1:' SKIP
          VALID-HANDLE(hboClienteAPI)
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

    RUN sincrHandle     IN hBoClienteAPI(hObj,1,pTipo).
    RUN retirarNoRoot   IN hBoClienteAPI(lRetirarNoRoot).
    RUN _getNumCalcIntr(INPUT cEndPoint, OUTPUT iNumCalc).
    RUN setSufixoJson   IN hBoClienteAPI(cEndPoint).
    RUN setVersaoApi('v2').
    RUN exec            IN hBoCLienteAPI(iNumCalc,pChave).


END PROCEDURE.


PROCEDURE setApi:

    DEFINE INPUT  PARAMETER pPathAPI  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pMetodo   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDir        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUrlJson    AS CHARACTER   NO-UNDO.
    RUN limparTTs       IN hBoClienteAPI.
    RUN _getParamsAutenticacao.
    RUN setDominio      IN hBoClienteAPI(cDominio).
    RUN setPorta        IN hBoClienteAPI(iPorta).
    RUN setPathPrinc    IN hBoClienteAPI(pPathAPI).
    RUN setMetodo       IN hBoClienteAPI(pMetodo).
    RUN sincrChaveCab   IN hBoClienteAPI('Authorization',cAutorizacao).
    RUN sincrChaveCab   IN hBoClienteAPI('tenantid',ctenantid).
    /*RUN getDirJsonLisa(OUTPUT cDir).
    RUN getUrlJson(OUTPUT cUrlJson).
    RUN setUrlJson      IN hBoClienteAPI(cUrlJson).
    RUN setDirJson      IN hBoClienteAPI(cDir).*/

END PROCEDURE.


PROCEDURE getTTRetorno:

    DEFINE OUTPUT PARAMETER TABLE FOR ttJson.
    RUN getTTRetorno IN hBoClienteAPI(OUTPUT TABLE ttJson).

END PROCEDURE.


PROCEDURE getStatusRetorno:

    DEFINE OUTPUT PARAMETER pStatus AS INTEGER     NO-UNDO.
    /*MESSAGE VALID-HANDLE(hBoClienteApi)
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    RUN getStatusRetorno IN hBoClienteAPI(OUTPUT pStatus ).

END PROCEDURE.   


PROCEDURE _getParamsAutenticacao:

  DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.

  RUN getVlParametro('autorizacao_lisa',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cAutorizacao = 'Basic YXBpd21zOmFwaXdtcyMyNDVA'.
  ELSE 
     ASSIGN cAutorizacao = cRetorno.


  RUN getVlParametro('chave_lisa',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cChave = '86158301'. //'86072302'.
  ELSE
    ASSIGN cChave = cRetorno.


  RUN getVlParametro('cnpj',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cCNPJ     = '06013812000581'.
  ELSE
     ASSIGN cCNPJ     = cRetorno .


  RUN getVlParametro('filial_lisa',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cFilial = '08'.
  ELSE
     ASSIGN cFilial = cRetorno.

  RUN getVlParametro('senha_lisa',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cSenha = '86072302'.
  ELSE
     ASSIGN cSenha = cRetorno.

  RUN getVlParametro('armazem_lisa',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cArmazem = '01'.
  ELSE
     ASSIGN cArmazem = cRetorno.

  /*RUN getVlParametro('produto_lisa',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cProduto = '698'.*/

  RUN getVlParametro('url_lisa',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cDominio = 'sobradocomercio128046.protheus.cloudtotvs.com.br'.
  ELSE
     ASSIGN cDominio = cRetorno.


  RUN getVlParametro('porta_lisa',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN iPorta = 4050.
  ELSE
     ASSIGN iPorta = int(cRetorno).

   RUN getVlParametro('tenantid',OUTPUT cRetorno) .
  IF cRetorno = '' THEN
     ASSIGN cTenantid = '03,08'.
  ELSE
     ASSIGN cTenantid = cRetorno.


     //tenantid: 03,08
  
  
END PROCEDURE.


PROCEDURE _getNumCalcIntr:

    DEFINE INPUT  PARAMETER pIntegracao AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iRetorno    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.

    CASE pIntegracao:

        WHEN 'enviar_nota_import' THEN DO:
             RUN getVlParametro(pIntegracao, OUTPUT cRetorno).
             IF cRetorno = '' THEN
                ASSIGN iRetorno =  50 .
             ELSE
                ASSIGN iRetorno = INT(cRetorno).


        END.
        WHEN 'enviar_etq_nota_import' THEN DO:
             RUN getVlParametro(pIntegracao, OUTPUT cRetorno).
             IF cRetorno = '' THEN
                ASSIGN iRetorno =  51 .
             ELSE
                ASSIGN iRetorno = INT(cRetorno).
        END.
        WHEN 'enviar_pedido_venda' THEN DO:
            RUN getVlParametro(pIntegracao, OUTPUT cRetorno).
             IF cRetorno = '' THEN
                ASSIGN iRetorno =  53 .
             ELSE
                ASSIGN iRetorno = INT(cRetorno).


        END.

        WHEN 'avaliar_separacao_pedido_venda' THEN DO:
            RUN getVlParametro(pIntegracao, OUTPUT cRetorno).
             IF cRetorno = '' THEN
                ASSIGN iRetorno =  57 .
             ELSE
                ASSIGN iRetorno = INT(cRetorno).


        END.

        WHEN 'alterar_pedido_venda' THEN DO:
            RUN getVlParametro(pIntegracao, OUTPUT cRetorno).
             IF cRetorno = '' THEN
                ASSIGN iRetorno =  58 .
             ELSE
                ASSIGN iRetorno = INT(cRetorno).

        END.

        WHEN 'excluir_pedido_venda' THEN DO:
            RUN getVlParametro(pIntegracao, OUTPUT cRetorno).
             IF cRetorno = '' THEN
                ASSIGN iRetorno =  59 .
             ELSE
                ASSIGN iRetorno = INT(cRetorno).

        END.
    END CASE.

END PROCEDURE.

PROCEDURE setVersaoApi:

    DEFINE INPUT  PARAMETER pVersao AS CHARACTER   NO-UNDO.
    ASSIGN cVersaoAPi = pVersao.
    RUN aplicarErrosPorVersao.

END PROCEDURE.

PROCEDURE aplicarErrosPorVersao:

    // se necess rio utilizar RUN sincrFaixaCodSitResp IN hBoClienteAPi(pCodIni,pCodFim,pTipo) aqui ou no procedimento da api para casos em que seja especifico da api.
    // se necess rio utilizar RUN incluirErroRespBody IN hBoClienteApi(pChave , pValor, pMsgErro).
    CASE cVersaoApi:
        WHEN '' THEN DO:
             RUN incluirErroRespBody IN hBoClienteApi('errorcode' , '400', '').
        END.
        WHEN 'v2' THEN DO:
             RUN incluirErroRespBody IN hBoClienteApi('code' , '400', '').   
        END.
    END CASE.


END PROCEDURE.




PROCEDURE getErro:

    DEFINE OUTPUT PARAMETER cErro AS CHARACTER   NO-UNDO.
    RUN getErros IN hBoClienteApi(OUTPUT cErro).

END PROCEDURE.


PROCEDURE getDirJsonLisa:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    RUN getDirJsonLisa IN hBoConsParam(OUTPUT cVlParam).
    

END PROCEDURE.


PROCEDURE getDirJsonRetornoLisa:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDir AS CHARACTER   NO-UNDO.
    RUN getDirJsonLisa IN hBoConsParam(OUTPUT cVlParam).
    RUN getDirRetornoLisa IN hBoConsParam(OUTPUT cDir).
    ASSIGN cVlParam = cVlParam + "\" + cDir.

    

END PROCEDURE.

PROCEDURE getUrlJson:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    RUN getUrlJsonLisa IN hBoConsParam(OUTPUT cVlParam).

END PROCEDURE.


PROCEDURE getUrlRetornoJson:

    DEFINE OUTPUT PARAMETER cVlParam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDir AS CHARACTER   NO-UNDO.
    RUN getUrlJsonLisa IN hBoConsParam(OUTPUT cVlParam).
    RUN getDirRetornoLisa IN hBoConsParam(OUTPUT cDir).
    ASSIGN cVlParam = cVlParam + "\" + cDir.

END PROCEDURE.


