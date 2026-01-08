 {esp/using_json.i}
 {esapi/envia-cancelamento-pedido.i}
 DEFINE INPUT  PARAMETER TABLE FOR ttPedido .
 DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
 DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
 
 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.
 DEFINE VARIABLE oPedido AS jsonObject .

 ASSIGN oPedido = NEW jsonobject().

 FIND FIRST ttPedido NO-ERROR.

 oPedido:ADD('codigoCliente',ttPedido.codigoCliente).
 oPedido:ADD('pedidoCliente',ttPedido.pedidoCliente).
 oPedido:ADD('prePedido',ttPedido.prePedido).


 {lisa/propsComuns.i}
 ASSIGN iCalc = 66.
 //RUN sincrHandle           IN hBo(pHDtSet,1,'dataset').


 RUN setObjJsonBody        IN hBo(oPedido).
 RUN setPosIniJson         IN hbo(1).  
 RUN setPosFimJson         IN hbo(0).  
 RUN incluirErroRespBody   IN hBo('code' , '400', '').
 RUN setSufixoJson         IN hBo('canc_pedido_venda').
 RUN setMetodo             IN hBO('POST').
 RUN setPathPrinc          IN hBO('rest/wms/v2/CancelarPrePedidoRolo').
 RUN exec                  IN hBo(iCalc,pChave).
 RUN getErros              IN hBo(OUTPUT cErro).
 RUN finalizar             IN hBo.

