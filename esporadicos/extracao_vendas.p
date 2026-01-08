DEFINE VARIABLE h AS HANDLE      NO-UNDO.

RUN esbo/esbo_fatur.p PERSIST SET h.

RUN iniciarBos      IN h.
RUN setIntervalDtEmisNota IN h(TODAY - 720 ,TODAY).
RUN buscarFaturados IN h.
RUN buscarDevolucao IN h.
RUN exportarTtFatur IN h('c:\temp\ttfatur.txt').
RUN finalizarBos    IN h.




