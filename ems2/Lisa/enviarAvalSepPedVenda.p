 
 DEFINE INPUT  PARAMETER pHDtSet AS HANDLE      NO-UNDO.
 DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
 DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
 
 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.


 {lisa/propsComuns.i}
 ASSIGN iCalc = 57.
 RUN sincrHandle           IN hBo(pHDtSet,1,'dataset').
 RUN incluirErroRespBody   IN hBo('code' , '400', '').
 RUN setSufixoJson         IN hBo('avaliar_separacao_pedido_venda').
 RUN setMetodo             IN hBO('POST').
 RUN setPathPrinc          IN hBO('/rest/wms/v2/AprovaReprovaSeparacaoPrePedidoRolo').
 RUN exec                  IN hBo(iCalc,pChave).
 RUN getErros              IN hBo(OUTPUT cErro).
 RUN finalizar             IN hBo.

