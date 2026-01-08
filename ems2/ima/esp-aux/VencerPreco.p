DEFINE VARIABLE hBoControlePreco    AS HANDLE      NO-UNDO.

RUN esbo/boControlePreco.p PERSISTENT SET hBoControlePreco.

RUN setTbPreco          IN hBoControlePreco(1).   // 1-pe, 2-pi, 3-outlet
RUN setTpPreco          IN hBoControlePreco(1).   // 1-padrao 2-rubi                
RUN setNrContainer      IN hBoControlePreco(0).
RUN setNivel            IN hBoControlePreco(2).   // 1-item, 2-referencia
//RUN setDtInicio         IN hBoControlePreco  ()
//RUN setDtFinal          IN hBoControlePreco()

RUN setItem             IN hBoControlePreco('520078').
RUN setCodRefer         IN hBoControlePreco('028').

RUN vencerPreco         IN hBoControlePreco(TODAY + 1).



