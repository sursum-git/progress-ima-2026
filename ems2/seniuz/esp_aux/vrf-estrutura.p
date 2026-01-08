FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 1 AND
         ob-etiqueta.tipo-ordem = 1 /*AND
         ob-etiqueta.it-codigo = '503751'*/ NO-LOCK 
         BY ob-etiqueta.dt-emissao
         BY ob-etiqueta.hr-emissao.

    FOR EACH ref-estrut WHERE
             ref-estrut.it-codigo = ob-etiqueta.it-codigo AND
             ref-estrut.cod-ref-it = ob-etiqueta.cod-refer
             NO-LOCK.

        FIND estrutura WHERE
             estrutura.it-codigo = ref-estrut.it-codigo AND
             estrutura.es-codigo = ref-estrut.es-codigo NO-ERROR.

        IF AMBIGUOUS estrutura THEN
           DISP ob-etiqueta.it-codigo
                ob-etiqueta.cod-refer
                ref-estrut.it-codigo
                ref-estrut.es-codigo.
    END.
END. 
