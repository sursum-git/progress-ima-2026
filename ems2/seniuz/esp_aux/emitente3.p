OUTPUT TO c:\temp\emit_sem_email.csv.
PUT "Codigo;Nome;Ult-Pedido" SKIP.
FOR EACH emitente WHERE emitente.identific <> 2 NO-LOCK.
    IF emitente.e-mail <> "" THEN NEXT.
    FIND LAST ped-venda WHERE ped-venda.nome-abrev = emitente.nome-abrev
                        NO-LOCK NO-ERROR.
    PUT emitente.cod-emitente ";"
        emitente.nome-emit ";"
        IF AVAIL ped-venda THEN ped-venda.dt-emissao ELSE ?
        SKIP.
END.
OUTPUT CLOSE.
