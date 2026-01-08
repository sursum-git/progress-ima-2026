/* Programa: sub-conta.p
** Lista tabela sub-conta - Centros de custo
*/

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".
FOR EACH sub-conta NO-LOCK.
    DISP sub-conta.sc-codigo
         sub-conta.descricao.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.
