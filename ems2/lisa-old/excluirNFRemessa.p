 
 DEFINE INPUT  PARAMETER pHDtSet AS HANDLE      NO-UNDO.
 DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
 DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
 
 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.


 {lisa/propsComuns.i}
 ASSIGN iCalc = 64.
 RUN sincrHandle           IN hBo(pHDtSet,1,'dataset').
 RUN incluirErroRespBody   IN hBo('errorcode' , '400', '').
 RUN setSufixoJson         IN hBo('excluir_nota_import').
 RUN setMetodo             IN hBO('DELETE').
 RUN setPathPrinc          IN hBO('/rest/NOTAENTRADA').
 RUN exec                  IN hBo(iCalc,pChave).
 RUN getErros              IN hBo(OUTPUT cErro).
 RUN finalizar             IN hBo.

