DEFINE VARIABLE hesbofin533a AS HANDLE      NO-UNDO.
{esbo/esbofin533a.i}
RUN esbo/esbofin533a.p PERSISTENT SET  hesbofin533a.

RUN definirDataCorte IN hesbofin533a(TODAY).
RUN buscarSaldos IN hesbofin533a.
RUN retornarRegistros IN hesbofin533a(OUTPUT TABLE ttSaldo).
OUTPUT TO c:\temp\esbofin533_teste.txt.
FOR EACH ttSaldo:
    DISP ttSaldo WITH WIDTH 550.
END.
OUTPUT CLOSE.


