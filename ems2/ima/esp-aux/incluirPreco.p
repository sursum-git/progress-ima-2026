DEFINE VARIABLE hBoControlePreco    AS HANDLE      NO-UNDO.

DEF VAR de-vlReal  AS DEC.
DEF VAR de-vlDolar AS DEC.

RUN esbo/boControlePreco.p PERSISTENT SET hBoControlePreco.

ASSIGN de-vlreal = 10
       de-vldolar = 2.


RUN setTbPreco          IN hBoControlePreco(1).   // 1-pe, 2-pi, 3-outlet
RUN setTpPreco          IN hBoControlePreco(1).   // 1-padrao 2-rubi                
RUN setNivel            IN hBoControlePreco(1).   // 1-item, 2-referencia
RUN setDtInicio         IN hBoControlePreco(TODAY).
RUN setDtFinal          IN hBoControlePreco(12.31.9999).
RUN setItem             IN hBoControlePreco('520078').
RUN inserirPreco        IN hBoControlePreco(de-vlReal, de-vlDolar). 
