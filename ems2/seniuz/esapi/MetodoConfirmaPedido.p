DEF VAR bo-ped-venda-com AS HANDLE.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

FIND emitente 9493.

FIND ped-venda WHERE
     ped-venda.nome-abrev = emitente.nome-abrev AND
     ped-venda.nr-pedcli = '64009' NO-LOCK NO-ERROR.

run dibo/bodi159com.p persistent set bo-ped-venda-com.
run completeOrder in bo-ped-venda-com (input  rowid(ped-venda),
                                       output table Rowerrors).
 
for each Rowerrors:
    MESSAGE  rowerrors.errornumber
             rowerrors.errordescription VIEW-AS ALERT-BOX.
end.
 
delete procedure bo-ped-venda-com.

