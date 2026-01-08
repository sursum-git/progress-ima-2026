/* Lista clientes para reclassifica‡Æo.
*/

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

DEF VAR da-ult-nota LIKE nota-fiscal.dt-emis-nota.

OUTPUT TO "c:/lixo/lixo.csv" CONVERT SOURCE "ibm850".
PUT "Codigo;Nome-Abrev;Razao-Social;Grupo;Ult-NF;Acum-Ult.Ano" SKIP.

FOR EACH emitente WHERE emitente.identific <> 2 NO-LOCK.
    FIND gr-cli WHERE gr-cli.cod-gr-cli = emitente.cod-gr-cli NO-LOCK NO-ERROR.

    FOR EACH nota-fiscal WHERE nota-fiscal.nome-ab-cli   = emitente.nome-abrev
                           AND nota-fiscal.dt-emis-nota >= 05/23/2006
                         NO-LOCK:
        IF nota-fiscal.dt-cancela = ? THEN
           ACCUMULATE nota-fiscal.vl-tot-nota(TOTAL).
    END.
       
    FIND LAST nota-fiscal USE-INDEX ch-emi-nota 
         WHERE nota-fiscal.cod-emitente =  emitente.cod-emitente NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
       ASSIGN da-ult-nota = nota-fiscal.dt-emis-nota.
    ELSE
       ASSIGN da-ult-nota = ?.

    PUT emitente.cod-emitente ";"
        emitente.nome-abrev ";"
        emitente.nome-emit ";"
        string(emitente.cod-gr-cli,"99") + "-" + gr-cli.descricao ";"
        da-ult-nota ";"
        (ACCUM TOTAL nota-fiscal.vl-tot-nota) FORMAT ">>>,>>>,>>9.99"
        SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.
