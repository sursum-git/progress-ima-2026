DEFINE BUFFER bf FOR despesas.
FOR EACH despesas:
   DISP despesas.descricao.
    FOR EACH despesas_impostos
        WHERE despesas.despesa_id = despesas_impostos.imposto_id:
        FIND bf WHERE
            bf.despesa_id = despesas_impostos.despesa_id
            NO-LOCK NO-ERROR.
        DISP  bf.descricao  .
    END.

END.

