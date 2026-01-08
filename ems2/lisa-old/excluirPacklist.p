 
 DEFINE INPUT  PARAMETER pRowid  AS ROWID       NO-UNDO.
 DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
 
 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.
 FIND nota-fiscal NO-LOCK
      WHERE ROWID(nota-fiscal) = pRowid
      NO-ERROR.


 {lisa/propsComunsComQueryParams.i}
 ASSIGN iCalc = 65.
 RUN incluirErroRespBody   IN hBo('errorcode' , '400', '').
 RUN setSufixoJson         IN hBo('excluir_packlist').
 RUN setMetodo             IN hBO('DELETE').
 RUN setPathPrinc          IN hBO('/rest/wms/v2/entradaRolo/query').
 RUN sincrParamUrl         IN hBO('nota',nota-fiscal.nr-nota-fis).
 RUN sincrParamUrl         IN hBO('serie',nota-fiscal.serie).

 RUN exec                  IN hBo(iCalc,nota-fiscal.nr-nota-fis + "-" 
                                  + nota-fiscal.Serie).
 RUN getErros              IN hBo(OUTPUT cErro).
 RUN finalizar             IN hBo.

