def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH mov-est-acbm WHERE mov-est-acbm.data-mov >= 10/25/2007
                        AND mov-est-acbm.data-mov <= 10/25/2007
                      NO-LOCK.

    FOR EACH ob-etiqueta WHERE ob-etiqueta.nr-ob      = mov-est-acbm.num-lote
                           AND ob-etiqueta.tipo-ordem = 1
                           AND ob-etiqueta.dt-emissao = mov-est-acbm.data-mov
                           AND ob-etiqueta.it-codigo  = mov-est-acbm.it-codigo
                           AND ob-etiqueta.cod-refer  = mov-est-acbm.cod-refer  
                         NO-LOCK.
        ACCUMULATE ob-etiqueta.quantidade(TOTAL).
    END.

    DISPLAY mov-est-acbm.num-lote
            /*mov-est-acbm.data-mov*/
            mov-est-acbm.qtd-tot-perf
            mov-est-acbm.qtd-tot-def
            mov-est-acbm.qtd-tot-sob
            mov-est-acbm.qtd-tot-perf + mov-est-acbm.qtd-tot-def + mov-est-acbm.qtd-tot-sob (TOTAL)
            (ACCUM TOTAL ob-etiqueta.quantidade) (TOTAL)
            WITH WIDTH 150.
END.
 
OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.

