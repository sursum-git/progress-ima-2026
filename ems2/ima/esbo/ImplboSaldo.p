{esbo/boSaldo.i}
DEFINE VARIABLE hBoSaldo AS HANDLE      NO-UNDO.
RUN esbo/boSaldo.p PERSIST SET hBoSaldo.
RUN iniciarBos IN hBoSaldo.
//RUN getSaldoItemRef IN hBoSaldo(passar parametgros).
RUN getTTSaldo IN hBoSaldo(OUTPUT TABLE ttSaldo).
RUN finalizarBos IN hBoSaldo.


