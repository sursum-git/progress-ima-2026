DEFINE TEMP-TABLE ttPreco
    FIELD itCodigo LIKE ITEM.it-codigo
    FIELD preco AS DECIMAL FORMAT '>>>>.9999999999'.
DEFINE VARIABLE h AS HANDLE      NO-UNDO.

RUN esbo/boControlePreco.p PERSISTENT SET h.
RUN iniciarBos IN h.

FOR EACH preco-item
    WHERE preco-item.nr-tabpre = 'tab e12'.
    FIND ttPreco
        WHERE ttPreco.itCodigo = preco-item.it-codigo
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttPreco THEN DO:
       CREATE ttPreco.
       ASSIGN ttPreco.itCodigo = preco-item.it-codigo
              ttPreco.preco    = preco-item.preco-venda .
    END.


END.


FOR EACH ttPreco:
    DISP ttPreco.
    PAUSE 0. 
    RUN setTbPreco IN h(1).
    RUN setTpPreco IN h(1).
    RUN setNrContainer IN h(0).
    RUN setNivel IN h(1).
    RUN setItem IN h(ttPreco.itCodigo).
    RUN setDtInicio IN h(TODAY).
    RUN setDtFinal IN h(12.31.9999).
    RUN inserirPreco IN h(ttPreco.preco,0).

END.


RUN finalizarBos IN h.

IF VALID-HANDLE(h) THEN
   DELETE PROCEDURE h .

/*FOR EACH controle_preco:
    ASSIGN controle_preco.tb_preco_id = 1.
END.*/
