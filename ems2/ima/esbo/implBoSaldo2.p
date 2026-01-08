DEFINE VARIABLE hBoSaldo2 AS HANDLE      NO-UNDO.
{esbo/boSaldo2.i}
{esp/util.i}

RUN esbo/boSaldo2.p PERSIST SET hBoSaldo2.
RUN iniciar   IN hBoSaldo2.
RUN limparTTs IN hBoSaldo2.
RUN setTTItensFaturaveis IN hBoSaldo2.
RUN setParamsLocaisEstoque IN hBoSaldo2.
RUN getSaldo  IN hBoSaldo2.
RUN getTTItens IN hBoSaldo2(OUTPUT TABLE ttItens).

RUN finalizar IN hBoSaldo2.

{esp/exportarTabelaCsv3.i ttItens " " " " "ttItens" }
