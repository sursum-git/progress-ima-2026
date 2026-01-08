/* Programa: cep_emitente_num.p
**           Verifica emitente sem n£mero no endere‡o
*/

DEF VAR h-acomp         AS HANDLE NO-UNDO.
DEF VAR c-comando       AS CHAR.

OUTPUT TO c:\temp\cep_inval_inscr.csv CONVERT SOURCE "ibm850".

PUT "Emitente;Nat-Jur;Inscricao;Nome;Endereco" SKIP.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando_CEPs_por_Emitente *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH emitente WHERE 
        /* emitente.cod-emitente > 8377 AND */
         emitente.identific <> 2 NO-LOCK:

    run pi-acompanhar in h-acomp (input "Emitente: " + STRING(emitente.cod-emitente)). 

       PUT emitente.cod-emitente ";"
           emitente.natureza ";"
           emitente.ins-estadual ";"
           emitente.nome-emit ";"
           emitente.endereco
           SKIP.
END.
OUTPUT CLOSE.
