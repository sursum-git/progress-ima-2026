DEFINE VARIABLE h AS HANDLE      NO-UNDO.
RUN esbo/boControlePreco.p PERSISTENT SET h.
RUN iniciarBos IN h.

OUTPUT TO c:\temp\migracao_liquida_ima_perc.txt.
FOR EACH liquida-ima
    WHERE liquida-ima.it-codigo <> '' AND liquida-ima.cod-refer <> '' AND liquida-ima.preco-item > 0  AND liquida-ima.perc-desc > 0 NO-LOCK.
    EXPORT DELIMITER  "|"   liquida-ima EXCEPT tipo-tabela.

    RUN setTbPreco      IN h(1).
    RUN setTpPreco      IN h(3).
    RUN setNrContainer  IN h(0).
    RUN setNivel        IN h(2).
    RUN setItem         IN h(liquida-ima.it-codigo).
    RUN setCodRefer     IN h(liquida-ima.cod-refer).
    RUN setDtInicio     IN h(liquida-ima.dt-inicio).
    RUN setDtFinal      IN h(liquida-ima.dt-final).
    RUN setVencido      IN h(YES).
    RUN inserirPreco IN h(liquida-ima.preco-item,0).


END.

OUTPUT CLOSE.

RUN finalizarBos IN h.

IF VALID-HANDLE(h) THEN
   DELETE PROCEDURE h .




/*FOR EACH controle_preco:
    ASSIGN controle_preco.tb_preco_id = 1.
END.*/
