DEFINE VARIABLE h AS HANDLE      NO-UNDO.
DEFINE VARIABLE cBanco AS CHARACTER   NO-UNDO.
RUN esbo/boMetaDados.p PERSISTENT SET h.
//RUN iniciarBos IN h.
RUN setTabela IN h('emitente').
RUN getBancoTb IN h(OUTPUT cBanco).
DELETE ALIAS bdori.
CREATE ALIAS bdori FOR DATABASE VALUE(cBanco).

RUN criarRelacsOf IN h(YES).
//RUN finalizarBos IN h.
