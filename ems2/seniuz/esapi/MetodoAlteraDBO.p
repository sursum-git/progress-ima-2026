def temp-table tt-ped-item like ped-item 
    field r-rowid as rowid.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

DEF VAR bo-ped-item AS HANDLE.

FOR EACH tt-ped-item.
    DELETE tt-ped-item.
END.

FIND emitente 8760.

FIND ped-item WHERE
     ped-item.nome-abrev = emitente.nome-abrev AND
     ped-item.nr-pedcli = '64790' AND
     ped-item.nr-sequencia = 40.

DISP ped-item.qt-pedida.

BUFFER-COPY ped-item TO tt-ped-item
ASSIGN tt-ped-item.r-rowid = ROWID(ped-item)
       tt-ped-item.qt-pedida = 100
       tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori.

IF NOT VALID-HANDLE(bo-ped-item) THEN
   RUN dibo/bodi154.p PERSISTENT SET bo-ped-item.

run setConstraintKey IN bo-ped-item (INPUT tt-ped-item.nome-abrev,
                                     INPUT tt-ped-item.nr-pedcli,
                                     INPUT tt-ped-item.nr-sequencia,
                                     INPUT tt-ped-item.it-codigo,
                                     INPUT tt-ped-item.cod-refer).
    
run openQueryStatic in bo-ped-item (input "Key":U).        

run emptyRowErrors      in bo-ped-item.
run setRecord           in bo-ped-item(input table tt-ped-item).
run updateRecord        in bo-ped-item.

run getRowErrors        in bo-ped-item(output table RowErrors).

if  can-find(first RowErrors
            where RowErrors.ErrorSubType = "ERROR":U) then do:
    for each rowerrors where RowErrors.ErrorSubType = "ERROR":U:
        MESSAGE  rowerrors.errornumber
                 rowerrors.errordescription VIEW-AS ALERT-BOX.
    end.
end.



