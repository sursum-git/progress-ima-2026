


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



DEF TEMP-TABLE infNotaEntrada NO-UNDO SERIALIZE-NAME ""
    FIELD nfid            AS INTEGER SERIALIZE-HIDDEN
    FIELD ccodfilial      AS CHAR
    FIELD cnota           AS CHAR
    FIELD cserie          AS CHAR
    FIELD cemissaonf      AS CHAR
    FIELD ccgcfornecedor  AS CHAR
    FIELD ccgc            AS CHAR
    FIELD cchavenfe       AS CHAR
    FIELD cnforiginal     AS CHAR
    FIELD cserieoriginal  AS CHAR
    FIELD citemoriginal   AS CHAR
    FIELD ccodigopostagem AS CHAR
    FIELD cnumdocsap      AS CHAR
    FIELD cxmldanfe       AS CHAR
    FIELD cpdfdanfe64     AS BLOB.   
    
DEF TEMP-TABLE infItens SERIALIZE-NAME "aitens" 
    FIELD itid                  AS INTEGER  SERIALIZE-HIDDEN
    FIELD nqtd                  AS DECIMAL  FORMAT ">>>,>>9.9999" 
	FIELD nvalorunitario        AS DECIMAL  FORMAT ">>>,>>9.9999999"
	FIELD cproduto              AS CHAR
	FIELD cprodutonofornecedor  AS CHAR
	FIELD clote                 AS CHAR
	FIELD clotevalidade         AS CHAR
	FIELD carmazem              AS CHAR
	FIELD cdescricao            AS CHAR
	FIELD ccodbarras            AS CHAR
	FIELD cncm                  AS CHAR
	FIELD nfatorconvercao       AS INTEGER
	FIELD lcontrolaserie        AS LOGICAL 
	FIELD npesokg               AS INTEGER
	FIELD altura                AS INTEGER
	FIELD largura               AS INTEGER
	FIELD comprimento           AS INTEGER
	FIELD cubagem               AS INTEGER
	FIELD alturaCaixa           AS INTEGER
	FIELD larguraCaixa          AS INTEGER
	FIELD comprimentoCaixa      AS INTEGER
	FIELD cCodClac              AS CHAR
	FIELD cDun14                AS CHAR
	FIELD cUpc                  AS CHAR
	FIELD cubagemCaixa          AS INTEGER
    FIELD gramatura             AS DECIMAL
    FIELD largtecido            AS CHAR
    FIELD regralavagem          AS CHAR
    FIELD composicao            AS CHAR
    FIELD paisOrigem            AS CHAR
    FIELD cRend                 AS CHAR.









LOG-MANAGER:LOGFILE-NAME = 'C:\temp\nota' + STRING(TIME) + '.txt'.
LOG-MANAGER:LOGGING-LEVEL = 5.


DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.
DEFINE VARIABLE oJsonObjectBody AS jsonobject   NO-UNDO.

DEFINE TEMP-TABLE tt
    FIELD a AS CHAR
    FIELD b AS CHAR .

CREATE tt.
ASSIGN tt.a = 'xxxxxx'
       tt.b = 'zzzzzzz'.
oJsonObjectBody = NEW jsonobject().

TEMP-TABLE tt:HANDLE:WRITE-JSON( 'jsonobject',
                oJsonObjectBody,TRUE,'utf-8',FALSE,FALSE).

RUN esbo/boClienteAPI.p PERSISTENT SET hBo .
RUN iniciar         IN hBo.  
RUN setDominio      IN hBO('imaonline.imatextil.com.br').
RUN setPathPrinc    IN hBO('/iol/bl_valida_cli_lisa/bl_valida_cli_lisa.php').
RUN setMetodo       IN hBO('POST').
RUN setDirJson      IN hBo('\\pv1\pv2\integracao_lisa').
RUN setUrlJson      IN hBo('http://imaonline.imatextil.com.br/iol/integracao_lisa/').
//RUN sincrHandle     IN hBo(TEMP-TABLE tt:HANDLE,1,'tt').
RUN setObjJsonBody  IN hBO(oJsonObjectBody).
RUN setSufixoJson   IN hBo('TESTETADEU').
RUN setHttps        IN hBo(NO).
//RUN sincrParamUrl   IN hBo('CHAVE','13456487548').
RUN setTipoConteudo IN hBo('json/application').
RUN sincrChaveCab   IN hBo('tenantid','03,08').
RUN retirarNoRoot   IN hBo(YES).
RUN setSufixoJson   IN hBo('a').
RUN exec            IN hBo(1234,'teste1234').

//RUN finalizar IN hBo.
//RUN setAPI('/iol/bl_valida_cli_lisa/bl_valida_cli_lisa.php','POST').




