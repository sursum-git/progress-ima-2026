DEFINE VARIABLE h-bo AS HANDLE       NO-UNDO.
DEFINE VARIABLE vlReal  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vlDolar AS DECIMAL   NO-UNDO.

{esbo/boPrecosItemRef.i}
{esbo\boMsg.i}

RUN esbo/boPrecosItemRef.p PERSISTENT SET h-bo.
RUN iniciarBos      IN h-bo.
RUN limparTTPreco   IN h-bo.
RUN limparTTMsg     IN h-bo.
RUN setTbPreco      IN h-bo(2). //1-padrao   2-rubi
RUN setItem         IN h-bo('520078'). 
RUN setRef          IN h-bo('225'). 
RUN setNrContainer  IN h-bo(0).
RUN setTipoBusca    IN h-bo(1). // 0- todos, 1- pe, 2- pi
RUN setPrazoMedio   IN h-bo(90).
//RUN setUfCliente    IN h-bo(). como n∆o tem mais IMA n∆o precisa
RUN buscarPrecos    IN h-bo.

RUN getPrecoPrazo   IN h-bo (INPUT 'pi',
                             OUTPUT vlReal,
                             OUTPUT vlDolar).
MESSAGE 'pi' SKIP
        vlReal SKIP
        vlDolar SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

RUN getPrecoPrazo   IN h-bo (INPUT 'pe',
                             OUTPUT vlReal,
                             OUTPUT vlDolar).
MESSAGE 'pe' SKIP
         vlReal SKIP
         vlDolar SKIP
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

RUN getPrecoPrazo   IN h-bo (INPUT 'outlet',
                             OUTPUT vlReal,
                             OUTPUT vlDolar).
MESSAGE 'outlet' SKIP
         vlReal SKIP
         vlDolar SKIP
         VIEW-AS ALERT-BOX INFO BUTTONS OK.


RUN finalizarBos IN h-bo.
IF VALID-HANDLE(h-bo) THEN
   DELETE PROCEDURE h-bo.



