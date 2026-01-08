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
DEFINE VARIABLE hObj AS HANDLE      NO-UNDO.
DEFINE VARIABLE oJsonObjectBody AS jsonObject.
DEFINE  TEMP-TABLE ttPedido 
    FIELD codigoCliente AS CHAR
    FIELD pedidoCliente AS CHAR
    FIELD prePedido     AS CHAR
    .
CREATE ttPedido.
ASSIGN ttPedido.prePedido      = '321549'
       ttPedido.codigoCliente  =  '32154987'
       ttPedido.pedidoCliente  = '1321548'.


ASSIGN oJsonObjectBody = NEW jsonObject().
ASSIGN hObj = TEMP-TABLE ttPedido:HANDLE.
hObj:WRITE-JSON(  'jsonobject',
                oJsonObjectBody,
                FALSE,
                'utf-8',FALSE,FALSE).


ojsonObjectBody:writeFile('c:\temp\tadeu01.json').
