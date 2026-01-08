/* Programa: ordem-benefic3.p
**           Encerra ordens de beneficiamento em situa‡Æo de nÆo encerrada h  mais de 15 dias,
**           com situacao "Em revisÆo" e cancelas as ob-etiquetas com quantidade = zero.
*/

DEF VAR c-situacao AS CHAR FORMAT "x(15)".

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH ordem-benefic WHERE ordem-benefic.situacao = 2 
                         AND (TODAY - ordem-benefic.dt-ob) > 15 
                       NO-LOCK.
    {esinc/i-dsrb.i ordem-benefic.situacao ordem-benefic.situacao c-situacao} 
                                                      
    FOR EACH ob-etiqueta WHERE ob-etiqueta.nr-ob      = ordem-benefic.nr-ob
                           AND ob-etiqueta.dt-ob      = ordem-benefic.dt-ob
                           AND ob-etiqueta.nr-carro   = ordem-benefic.nr-carro
                           AND ob-etiqueta.situacao   < 3
                           AND ob-etiqueta.quantidade = 0.
        ACCUMULATE ob-etiqueta.quantidade(COUNT).
        /*DELETE ob-etiqueta.*/
    END.
    DISP ordem-benefic.nr-ob
         ordem-benefic.dt-ob
         c-situacao
         (ACCUM COUNT ob-etiqueta.quantidade) LABEL "Ob-Etiqueta"
         TODAY - ordem-benefic.dt-ob LABEL "Atraso".
    /*ASSIGN ordem-benef.situacao = 4.*/
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.

