    DEFINE VARIABLE h-bo    AS HANDLE       NO-UNDO.
    DEFINE VARIABLE vlReal  AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE vlDolar AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE idPreco AS INTEGER     NO-UNDO.
    DEFINE VARIABLE tipoRetorno AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE vlPreco AS DECIMAL.
    DEFINE VARIABLE vlPrecoOutlet AS DECIMAL.

    DEFINE VARIABLE i-nr-container AS INTEGER.
    DEFINE VARIABLE i-prazo-medio AS INTEGER.
    
    RUN esbo/boPrecosItemRef.p PERSISTENT SET h-bo.
    RUN iniciarBos      IN h-bo.
    RUN limparTTPreco   IN h-bo.
    RUN limparTTMsg     IN h-bo.
    RUN setTbPreco      IN h-bo(2). //1-padrao   2-rubi
    RUN setItem         IN h-bo('135071'). 
    RUN setRef          IN h-bo(''). 
    RUN setNrContainer  IN h-bo(i-nr-container).
    RUN setTipoBusca    IN h-bo(IF i-nr-Container <> 0 THEN 2 ELSE 1 ). // 0- todos, 1- pe, 2- pi
    RUN setPrazoMedio   IN h-bo(i-prazo-medio).
    RUN setDtRefer      IN h-bo(TODAY).
    RUN buscarPrecos    IN h-bo.

    IF i-nr-Container = 0 THEN DO:
       RUN getPrecoPrazo   IN h-bo (INPUT 'outlet',
                           OUTPUT vlReal,
                           OUTPUT vlDolar,
                           OUTPUT idPreco).
       ASSIGN vlPrecoOutlet = vlReal.

       RUN getPrecoPrazo IN h-bo (INPUT 'pe',
                                  OUTPUT vlReal,
                                  OUTPUT vlDolar,
                                  OUTPUT idPreco).
       ASSIGN vlPreco =  vlReal.
    END.
    ELSE DO:
       RUN getPrecoPrazo IN h-bo (INPUT 'pi',
                                  OUTPUT vlReal,
                                  OUTPUT vlDolar,
                                  OUTPUT idPreco ).  
       ASSIGN vlPreco = vlReal.
    END.
    
    RUN finalizarBos IN h-bo.
    IF VALID-HANDLE(h-bo) THEN
       DELETE PROCEDURE h-bo.


    MESSAGE vlPreco SKIP
            vlPrecoOutlet
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
