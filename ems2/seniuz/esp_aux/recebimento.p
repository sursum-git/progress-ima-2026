DEF VAR de-tot-nota  LIKE recebimento.valor-total.
DEF VAR de-tot-geral LIKE recebimento.valor-total.
DEF VAR de-icm-nota  LIKE recebimento.valor-icm.
DEF VAR de-icm-geral LIKE recebimento.valor-icm.

OUTPUT TO "c:\lixo\recebimento.txt" CONVERT SOURCE "ibm850".
PUT "Data;" "Fornec;" "Nome Fornecedor;" "Documento;" "Valor;" "NatOp;"
    "CFOP;" "Aliq.ICM;" "Valor ICM;" "Descricao do Item" 
    SKIP.

FOR EACH recebimento /*WHERE recebimento.data-movto >= 01/01/2006
                       AND recebimento.data-movto <= 01/31/2006*/
                     NO-LOCK,
    EACH ITEM WHERE ITEM.it-codigo = recebimento.it-codigo
                AND (ITEM.ge-codigo = 0 OR
                     ITEM.ge-codigo = 8)
              NO-LOCK,
    EACH natur-oper WHERE natur-oper.nat-operacao = recebimento.nat-operacao
                      AND (natur-oper.cd-trib-icm = 1 OR
                           natur-oper.cd-trib-icm = 4)
                    NO-LOCK
    BREAK BY recebimento.data-movto
          BY recebimento.serie-nota
          BY recebimento.numero-nota:

    FIND emitente WHERE emitente.cod-emitente = recebimento.cod-emitente NO-LOCK.
    PUT recebimento.data-movto ";"
        emitente.cod-emitente ";"
        emitente.nome-emit ";"
        recebimento.numero-nota ";"
        recebimento.valor-total ";"
        recebimento.nat-operacao ";"
        substr(natur-oper.char-1,45,10) format "9.999" ";"
        recebimento.aliquota-icm ";"
        recebimento.valor-icm ";"
        ITEM.desc-item
        SKIP.
    ASSIGN de-tot-nota  = de-tot-nota + recebimento.valor-total
           de-icm-nota  = de-icm-nota + recebimento.valor-icm
           de-tot-geral = de-tot-geral + recebimento.valor-total
           de-icm-geral = de-icm-geral + recebimento.valor-icm.  
    IF LAST-OF(recebimento.numero-nota) THEN DO:
       PUT ";;" 
           "Total do Documento"
           ";;"
           de-tot-nota
           ";;;;"
           de-icm-nota
           SKIP.
       ASSIGN de-tot-nota = 0
              de-icm-nota = 0.
    END.
END.
 
PUT ";;" 
    "Total Geral"
    ";;"
    de-tot-geral
    ";;;;"
    de-icm-geral
    SKIP.
OUTPUT CLOSE.

