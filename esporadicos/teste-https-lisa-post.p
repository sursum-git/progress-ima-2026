


BLOCK-LEVEL ON ERROR UNDO, THROW.

USING OpenEdge.Core.String.
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.IHttpClient.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.IHttpClientLibrary.
USING OpenEdge.Net.URI.
USING OpenEdge.Net.HTTP.RequestBuilder.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING Progress.Json.ObjectModel.*.
USING OpenEdge.Net.HTTP.Lib.ClientLibraryBuilder. 


/*LOG-MANAGER:LOGFILE-NAME = 'C:\temp\https_Tst_' + STRING(TIME) + '.txt'.
LOG-MANAGER:LOGGING-LEVEL = 5.*/

/*DEFINE INPUT  PARAMETER pIntegrador     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pIdIntegracao   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttJson.                      */





DEFINE VAR pTipo  AS CHARACTER   NO-UNDO INIT 'pedido_mp'.
DEFINE var pChave AS CHARACTER   NO-UNDO INIT '5381555'.
//DEFINE OUTPUT PARAMETER TABLE FOR ttJson.
/* ***************************  Definitions  ************************** */
DEFINE VARIABLE oClient   AS IHttpClient   NO-UNDO.
DEFINE VARIABLE oRequest  AS IHttpRequest  NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
DEFINE VARIABLE oProduto  AS JsonObject    NO-UNDO.
DEFINE VARIABLE oAux      AS JsonObject    NO-UNDO.
DEFINE VARIABLE oJsonResp AS JsonObject    NO-UNDO.
DEFINE VARIABLE oPedido   AS jsonObject     NO-UNDO.
DEFINE VARIABLE oAuxArray AS jsonArray     NO-UNDO.
DEFINE VARIABLE idIntegracao AS INTEGER    NO-UNDO.
DEFINE VARIABLE cJson     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE hDSet     AS HANDLE        NO-UNDO.
DEFINE VARIABLE lRetOk    AS LOGICAL       NO-UNDO.
DEFINE VARIABLE cPath     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUrl      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE oURI AS URI NO-UNDO.

DEFINE VARIABLE oLib AS IHttpClientLibrary NO-UNDO.
{esp/ssl.i} 
{esp/util.i}
{esp/utiljson.i}
OpenEdge.Net.HTTP.HttpHeaderBuilder:Registry:Remove('Authorization':u).
/* ***************************  Main Block  *************************** */
oLib = ClientLibraryBuilder:Build()
        :sslVerifyHost(NO)
        :serverNameIndicator('li195954.protheus.cloudtotvs.com.br')
        :SetSSLProtocols(protocolo)
        :SetSSLCiphers(cripto)
        :Library.
oClient = ClientBuilder:Build():UsingLibrary(oLib):Client.
//oClient = ClientBuilder:Build():Client.
oUri        = NEW URI('https','li195954.protheus.cloudtotvs.com.br',8401). 
//oUri         = NEW URI('http','sobradocomercio114725.protheus.cloudtotvs.com.br',4050).
oUri:addPathSegment('rest').
oUri:addPathSegment('wms').
oUri:addPathSegment('v2').
oUri:addPathSegment('PrePedidoRolo').







/*oUri:path   = 'v2/orders'.

CASE pTipo:
    WHEN 'pedido_any' THEN
        oUri:addPathSegment(pChave).
    WHEN 'pedido_mp' THEN DO:
        oURI:addQuery('marketPlaceId',pChave).
    END.
END CASE.
*/

RUN convFileJson2JsonObject('c:\temp\13913114.json', OUTPUT oPedido).

oRequest = RequestBuilder:POST(oUri,oPedido):addHeader('Authorization','Basic YXBpd21zOmFwaXdtcyMyNDVA'):addHeader('tenantid','03,08'):AcceptContentType('application/json;charset=uft-8'):ContentType('application/json;charset=utf-8'):Request.
oResponse = oClient:Execute(oRequest).        

MESSAGE oResponse:StatusCode
    VIEW-AS ALERT-BOX.


 /*
CATCH oError AS Progress.Lang.Error :    
    MESSAGE  "erro capturado" SKIP
             oError:GetMessage(1) SKIP(2)
            oError:CallStack
        VIEW-AS ALERT-BOX.
END CATCH.
*/
/*
curl --location 'https://li195954.protheus.cloudtotvs.com.br:8401/rest/wms/v2/Rolo/query?pageSize=100000&codigoCliente=86159501&status=P' \
--header 'tenantid: 03,08' \
--header 'Authorization: Basic YXBpd21zOmFwaXdtcyMyNDVA'

*/


