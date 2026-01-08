DEFINE VARIABLE h AS HANDLE      NO-UNDO.

RUN esbo/boAtivosInativos.p PERSISTENT SET h.

RUN buscarClientesRepres IN h.
