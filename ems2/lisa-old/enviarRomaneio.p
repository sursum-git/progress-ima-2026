 //{esp/USING_json.i}
 DEFINE INPUT  PARAMETER pTT AS HANDLE          NO-UNDO.
 DEFINE INPUT  PARAMETER pChave  AS CHARACTER   NO-UNDO.
 DEFINE OUTPUT PARAMETER cErro   AS CHARACTER   NO-UNDO.
 //DEFINE VARIABLE oJsonObjectBody AS jsonObject  NO-UNDO.
 

 DEFINE VARIABLE iCalc AS INTEGER     NO-UNDO.

 {lisa/propsComuns.i}
 ASSIGN iCalc = 63.
 RUN setPosIniJson         IN hBo(25).
 RUN setPosFimJson         IN hBo(31).
 RUN sincrHandle           IN hBo(pTT,1,'tt').
 RUN incluirErroRespBody   IN hBo('errorcode' , '400', '').
 RUN setSufixoJson         IN hBo('enviar_romaneio_nf').
 RUN setMetodo             IN hBO('POST').
 RUN setPathPrinc          IN hBO('/rest/DOCUMENTOS').
 RUN exec                  IN hBo(iCalc,pChave).
 RUN getErros              IN hBo(OUTPUT cErro).
 RUN finalizar             IN hBo.





