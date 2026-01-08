/* Programa: corte-comerc.p
** Lista tabela de Cortes Comerciais
*/

DEF VAR c-tipo AS CHAR FORMAT "x(10)".

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH corte-comerc NO-LOCK.
    {esinc/i-dsrb.i corte-comerc.tp-embalag corte-comerc.tp-embalag c-tipo} 
    DISP corte-comerc.codigo
         corte-comerc.descricao
         corte-comerc.compr-min
         corte-comerc.compr-max
         corte-comerc.compr-med
         c-tipo
         corte-comerc.corte-origem
         WITH WIDTH 130.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.

