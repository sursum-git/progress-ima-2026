DEF VAR c-esp-doc AS CHAR.

OUTPUT TO c:/temp/lixo.csv CONVERT SOURCE "ibm850".

PUT "Esp;Serie;Num-Doc;Emitente;Nome;Data;Valor;Nat-Op;Conta;Sub-Conta;Est;Ent/Sai;Item" SKIP.

FOR EACH docum-est WHERE docum-est.dt-emissao >= 04/01/2010 NO-LOCK,
    EACH item-doc-est OF docum-est NO-LOCK:
    
    {esinc/i-dsrb.i docum-est.esp-doc docum-est.esp-doc c-esp-doc} 

    FIND emitente WHERE emitente.cod-emitente = docum-est.cod-emitente NO-LOCK.
    FIND ITEM WHERE ITEM.it-codigo = item-doc-est.it-codigo NO-LOCK.
    
    PUT c-esp-doc ";"
        docum-est.serie-docto ";"
        docum-est.nro-docto ";"
        docum-est.cod-emitente ";"
        emitente.nome-emit ";"
        docum-est.dt-emissao ";"
        docum-est.tot-valor ";"
        docum-est.nat-operacao ";"
        docum-est.ct-transit ";"
        docum-est.sc-transit ";"
        docum-est.cod-estabel ";"
        IF docum-est.tipo-docto = 1 THEN "Ent" ELSE "Sai" ";"
        item-doc-est.it-codigo ";"
        ITEM.desc-item
        /*item-doc-est.narrativa*/
        SKIP.
END.
OUTPUT CLOSE.
