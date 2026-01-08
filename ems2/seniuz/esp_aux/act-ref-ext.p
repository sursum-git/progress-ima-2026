FOR EACH ref-item-ext NO-LOCK.
    FIND referencia-ext WHERE
         referencia-ext.cod-refer = ref-item-ext.cod-refer NO-ERROR.

    IF NOT AVAIL referencia-ext THEN DO.
       CREATE referencia-ext.
       ASSIGN referencia-ext.cod-refer = ref-item-ext.cod-refer
              referencia-ext.cod-obsoleto = ref-item-ext.cod-obsoleto
              referencia-ext.colecao = ref-item-ext.colecao
              referencia-ext.cod-fundo = ref-item-ext.cod-fundo 
              referencia-ext.cor = ref-item-ext.cor.
    END.
END.

