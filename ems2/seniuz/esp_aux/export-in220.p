DEF VAR i-cont AS INT.
DEF VAR i-seq  AS INT.
DEF VAR c-arquivo AS CHAR.

def var h-acomp as handle no-undo.

ASSIGN c-arquivo = "c:\temp\in220.d".

OUTPUT TO VALUE(c-arquivo).

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando_Registros *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH movto-mat NO-LOCK:
    run pi-acompanhar in h-acomp (input "Sequˆncia: " + STRING(i-cont)). 
    EXPORT movto-mat.
    ASSIGN i-cont = i-cont + 1.
    IF i-cont = 4000000 THEN do:
       OUTPUT CLOSE.
       ASSIGN c-arquivo = "c:\temp\in220" + STRING(i-seq,"99") + ".d".
       OUTPUT TO VALUE(c-arquivo).
       ASSIGN i-seq  = i-seq + 1
              i-cont = 0.
    END.
END.

OUTPUT CLOSE.
