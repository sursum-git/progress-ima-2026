DEF VAR h-bodi154 AS HANDLE.

/* Include com defini»’o da temp-table RowErrors */
DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

FIND FIRST ped-item NO-LOCK WHERE ped-item.nr-pedcli = "400341" .
DISP ped-item.nr-pedcli 
     ped-item.it-codigo 
     ped-item.nr-sequencia WITH 1 COL. PAUSE 0.

RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

run openquerystatic in h-bodi154 (input "DefaultPd4000":U).
run emptyRowErrors in h-bodi154.
RUN gotokey IN h-bodi154 (INPUT ped-item.nome-abrev,
                             INPUT ped-item.nr-pedcli,
                             INPUT ped-item.nr-sequencia,
                             INPUT ped-item.it-codigo,
                             INPUT ped-item.cod-refer).
run deleteRecord in h-bodi154.
run getRowErrors in h-bodi154(output table RowErrors).

if  can-find(first RowErrors
             where RowErrors.ErrorType <> "INTERNAL":U) then do:
    FOR EACH rowErrors WHERE RowErrors.ErrorType <> "INTERNAL":U :
        DISP rowerrors.errorNumber
             rowerrors.errorDescription FORMAT "x(60)" WITH 1 COL.
        PAUSE 0.
    END.
end.
DELETE PROCEDURE h-bodi154.

FIND FIRST ped-item NO-LOCK WHERE ped-item.nr-pedcli = "400341" NO-ERROR.
IF AVAIL ped-item THEN

    DISP ped-item.nr-pedcli
         ped-item.it-codigo
         ped-item.nr-sequencia WITH 1 COL.
ELSE
  DISP "eliminou" .   
    
