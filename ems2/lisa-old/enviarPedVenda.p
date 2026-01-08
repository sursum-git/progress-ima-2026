 {esapi/analisarJsonObject2.i}
 DEFINE INPUT  PARAMETER pHDtSet AS HANDLE      NO-UNDO.
 DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
 DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
 DEFINE OUTPUT PARAMETER  TABLE FOR ttJson.
 
 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.
 {esp/util.i}
 

 {lisa/propsComuns.i}
 ASSIGN iCalc = 53.
 RUN sincrHandle           IN hBo(pHDtSet,1,'dataset').
 RUN incluirErroRespBody   IN hBo('code' , '400', '').
 RUN setSufixoJson         IN hBo('enviar_pedido_venda').
 RUN setMetodo             IN hBO('POST').
 RUN setPathPrinc          IN hBO('/rest/wms/v2/PrePedidoRolo').
 RUN exec                  IN hBo(iCalc,pChave).
 RUN getErros              IN hBo(OUTPUT cErro).
 RUN getTTRetorno          IN hBO(OUTPUT TABLE ttJson).
 RUN finalizar             IN hBo.

