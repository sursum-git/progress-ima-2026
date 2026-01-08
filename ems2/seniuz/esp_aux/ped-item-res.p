/* Verifica se h  mais de um item/referencia para a mesma sequencia 
** Gilvando - 08/02/2007 */

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO c:\temp\lixo.txt CONVERT SOURCE "ibm850".

DEF BUFFER b-ped-item-res FOR ped-item-res.

FOR EACH ped-item-res NO-LOCK.
    FIND b-ped-item-res WHERE 
         b-ped-item-res.nome-abrev   = ped-item-res.nome-abrev AND
         b-ped-item-res.nr-pedcli    = ped-item-res.nr-pedcli AND  
         b-ped-item-res.nr-sequencia = ped-item-res.nr-sequencia
    NO-LOCK NO-ERROR.

    IF AMBIGUOUS b-ped-item-res THEN
       DISP ped-item-res.nome-abrev  
            ped-item-res.nr-pedcli   
            ped-item-res.nr-sequencia
            ped-item-res.it-codigo
            ped-item-res.cod-refer
            ped-item-res.dt-trans
            ped-item-res.hr-trans
            WITH WIDTH 130.
END.
OUTPUT CLOSE.

run Execute in h-prog(input "notepad.exe",
 			          input "c:\temp\lixo.txt").
delete procedure h-prog.

