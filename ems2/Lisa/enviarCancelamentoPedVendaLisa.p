//esse cancelamento ‚ do pedido gerado ap¢s o envio da nota fiscal
{esp/using_json.i}
{esapi/envia-cancelamento-pedido-lisa.i}
 DEFINE INPUT  PARAMETER TABLE FOR ttPedido    .
 DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
 DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
 
 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.

 DEFINE VARIABLE oPedido AS jsonObject .

 ASSIGN oPedido = NEW jsonobject().

 FIND FIRST ttPedido NO-ERROR.

 oPedido:ADD('ccodfilial',ttPedido.ccodfilial).
 oPedido:ADD('ccgc',ttPedido.ccgc).
 oPedido:ADD('cpedido',ttPedido.cpedido).
 oPedido:ADD('cstatus',ttPedido.cstatus).

 {lisa/propsComuns.i}
 ASSIGN iCalc = 67.
 RUN setObjJsonBody        IN hBo(oPedido).
 RUN incluirErroRespBody   IN hBo('code' , '400', '').
 RUN setSufixoJson         IN hBo('cancelar_pedido_venda_lisa').
 RUN setMetodo             IN hBO('PUT').
 RUN setPathPrinc          IN hBO('/rest/PEDIDOWMS').
 RUN exec                  IN hBo(iCalc,pChave).
 RUN getErros              IN hBo(OUTPUT cErro).
 RUN finalizar             IN hBo.

