{esapi/analisarJsonObject2.i}
 
 DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
 DEFINE INPUT  PARAMETER pPag    AS INTEGER     NO-UNDO.
 DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
 DEFINE OUTPUT PARAMETER TABLE FOR ttJson.
 
 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.

 
 {lisa/propsComunsComQueryParams.i}
 ASSIGN iCalc = 60.
 RUN incluirErroRespBody   IN hBo('code' , '400', '').
 RUN setSufixoJson         IN hBo('cons_pedido_venda').
 RUN setMetodo             IN hBO('GET').
 RUN setPathPrinc          IN hBO('rest/wms/v2/PrePedidoRolo/query').
 RUN sincrParamUrl         IN hBo('status','G').
 RUN setParamUrl           IN hBo('pagina',pPag).
 RUN exec                  IN hBo(iCalc,pChave).
 RUN getErros              IN hBo(OUTPUT cErro).
 RUN getTTRetorno          IN hBo( OUTPUT TABLE ttJson).
 RUN finalizar             IN hBo.

