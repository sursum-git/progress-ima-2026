DEFINE VARIABLE h AS HANDLE      NO-UNDO.

RUN esbo/boAnaliseEstoq.p PERSIST SET h.

RUN iniciar         IN h.
RUN setValsIni      IN h.
RUN setProp         IN h('itCodigo',1,'560183').
RUN setProp         IN h('itCodigo',2,'560183').
RUN gerarTTSaldo    IN h.
RUN exec            IN h.
RUN exportarTTParam IN h.
RUN ExportarTTMovto IN h.
RUN ExportarTTSaldo IN h.
RUN finalizar       IN h.
