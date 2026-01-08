/* Programa: ordem-benefic2.p
**           Mostra ordens de beneficiamento em situaá∆o de n∆o encerrada h† mais de 5 dias
*/

DEF VAR c-situacao AS CHAR FORMAT "x(15)".

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH ordem-benefic WHERE ordem-benefic.situacao < 4 
                         AND (TODAY - ordem-benefic.dt-ob) > 5 
                       NO-LOCK.
    {esinc/i-dsrb.i ordem-benefic.situacao ordem-benefic.situacao c-situacao} 
    
    FIND FIRST ob-etiqueta WHERE ob-etiqueta.nr-ob    = ordem-benefic.nr-ob
                             AND ob-etiqueta.dt-ob    = ordem-benefic.dt-ob
                             AND ob-etiqueta.nr-carro = ordem-benefic.nr-carro
                             AND ob-etiqueta.situacao < 3 
                           NO-LOCK NO-ERROR.
    DISP ordem-benefic.nr-ob
         ordem-benefic.dt-ob
         c-situacao
         AVAIL ob-etiqueta FORMAT "Sim/N∆o" LABEL "Ob-Etiq"
         TODAY - ordem-benefic.dt-ob LABEL "Atraso".
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.

