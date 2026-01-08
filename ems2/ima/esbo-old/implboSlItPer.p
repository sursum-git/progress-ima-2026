DEFINE VARIABLE h AS HANDLE      NO-UNDO.

RUN esbo/boSlItPer.p PERSIST SET h.
RUN iniciar          IN h.
RUN setValsIni       IN h.
RUN setProp          IN h('geCodigo',1,60).
RUN setProp          IN h('geCodigo',2,60).
RUN exec             IN h.
RUN exportarTTResult IN h('saldo_produto_periodo.txt','|').
RUN finalizar IN h.
