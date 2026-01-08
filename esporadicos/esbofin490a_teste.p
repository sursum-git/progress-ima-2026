DEFINE VARIABLE hesbofin490a AS HANDLE      NO-UNDO.
DEFINE VARIABLE inicio AS DATETIME    NO-UNDO.
DEFINE VARIABLE fim    AS DATETIME    NO-UNDO.

{esbo/esbofin490a.i}

RUN esbo\esbofin490a.p PERSISTENT SET hesbofin490a.
ASSIGN inicio = NOW.
RUN definirEstab IN hesbofin490a('501').
RUN buscarTitulos IN hesbofin490a.
ASSIGN fim = NOW.
RUN retornarRegistros IN hesbofin490a(OUTPUT TABLE ttTitulo).

OUTPUT TO c:\temp\titulos.txt.
PUT inicio fim SKIP.
FOR EACH ttTitulo.
    EXPORT DELIMITER "|" ttTitulo.
END.

OUTPUT CLOSE.

OS-COMMAND SILENT VALUE('start notepad c:\temp\titulos.txt').

