/* Programa: qualid-tecido.p
** Lista tabela de Cortes Comerciais
*/

DEF VAR c-class-qualid AS CHAR FORMAT "x(10)" LABEL "Class.Qualid".

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".

FOR EACH qualid-tecido NO-LOCK.
    {esinc/i-dsrb.i qualid-tecido.class-qualid qualid-tecido.class-qualid c-class-qualid} 
    DISP qualid-tecido.codigo
         qualid-tecido.descricao
         c-class-qualid
         qualid-tecido.impr-tarja
         qualid-tecido.obriga-def
         qualid-tecido.situacao
         WITH WIDTH 130.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.

