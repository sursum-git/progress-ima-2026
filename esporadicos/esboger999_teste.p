DEFINE VARIABLE h AS HANDLE      NO-UNDO.
{esbo\esboger999.i}

RUN esbo\esboger999.p PERSISTENT SET h.
RUN definirDataIni IN h(TODAY).
RUN definirDataFim IN h(TODAY + 5).
RUN definirInicioSemana IN h(6).
RUN calcularSemanas IN h.
RUN retornarSemanas IN h(OUTPUT TABLE ttSemana).
OUTPUT TO c:\temp\ttSemana02.txt.
FOR EACH ttSemana.
    DISP ttSemana.
END.
OUTPUT CLOSE.
IF VALID-HANDLE(h) THEN
   DELETE PROCEDURE h.





