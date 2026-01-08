/* Programa: imp-maquina.p
** Objetivo: Importar o arquivo "csv" com dados parciais das maquinas da 
**           mecanica, para troca de c¢digo.
*/

DEF BUFFER b-mov-mec FOR mov-mec.
DEF BUFFER b-mov-man FOR mov-man.
DEF BUFFER b-maq-mec FOR maq-mec.

def temp-table tt-maquinas
    field cod-ant  LIKE maq-mec.codigo
    FIELD cod-atu  LIKE maq-mec.codigo.
    
input from "c:/lixo/maquinas.csv".
SET ^.

repeat:
   create tt-maquinas.
   import delimiter ";" tt-maquinas.
end.
input close.

FOR EACH tt-maquinas:
    IF tt-maquinas.cod-ant = "" THEN NEXT.
    FOR EACH mov-mec WHERE mov-mec.cod-maq = tt-maquinas.cod-ant.
        FIND b-mov-mec WHERE RECID(b-mov-mec) = RECID(mov-mec).
        /*ASSIGN b-mov-mec.cod-maq = tt-maquinas.cod-atu.*/
        ACCUMULATE mov-mec.cod-maq(COUNT).
    END.

    FOR EACH mov-man WHERE mov-man.cod-maq = tt-maquinas.cod-ant.
        FIND b-mov-man WHERE RECID(b-mov-man) = RECID(mov-man).
        /*ASSIGN b-mov-man.cod-maq = tt-maquinas.cod-atu.*/
        ACCUMULATE mov-man.cod-maq(COUNT).
    END.
    
    FOR EACH maq-mec WHERE maq-mec.codigo = tt-maquinas.cod-ant.
        FIND b-maq-mec WHERE RECID(b-maq-mec) = RECID(maq-mec).
        /*ASSIGN b-maq-mec.codigo = tt-maquinas.cod-atu.*/
        ACCUMULATE maq-mec.codigo(COUNT).
    END.
END.
DISPLAY (ACCUM COUNT mov-mec.cod-maq)
        (ACCUM COUNT mov-man.cod-maq)
        (ACCUM COUNT maq-mec.codigo).
