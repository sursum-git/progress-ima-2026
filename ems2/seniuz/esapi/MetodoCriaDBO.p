DEF TEMP-TABLE tt-ped-item LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi154 AS HANDLE.

FOR EACH tt-ped-item.
    DELETE tt-ped-item.
END.

FIND emitente 10708.

FIND ped-item WHERE
     ped-item.nome-abrev = emitente.nome-abrev AND
     ped-item.nr-pedcli = '62298' AND
     ped-item.nr-sequencia = 40 NO-LOCK.

RUN cria-item (INPUT "A").
RUN cria-item (INPUT "C").

PROCEDURE cria-item.
    DEF INPUT PARAMETER p-tipo AS CHAR.

    IF NOT VALID-HANDLE(h-bodi154) or
       h-bodi154:TYPE      <> "PROCEDURE":U OR
       h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
       RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

    IF p-tipo = "C" THEN DO.
        BUFFER-COPY ped-item TO tt-ped-item 
               ASSIGN tt-ped-item.nr-sequencia = 41
                      tt-ped-item.qt-pedida = 200
                      tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori.
        
        RUN openQueryStatic IN h-bodi154(INPUT "Main":U).
    END.
    ELSE DO.
        BUFFER-COPY ped-item TO tt-ped-item 
               ASSIGN tt-ped-item.qt-pedida = 100
                      tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori. 

        RUN setConstraintKey IN h-bodi154 (INPUT tt-ped-item.nome-abrev,
                                           INPUT tt-ped-item.nr-pedcli,
                                           INPUT tt-ped-item.nr-sequencia,
                                           INPUT tt-ped-item.it-codigo,
                                           INPUT tt-ped-item.cod-refer).

        RUN openQueryStatic in h-bodi154 (input "Key":U).        
    END.
    
    RUN emptyRowErrors IN h-bodi154.
    RUN setRecord IN h-bodi154(INPUT TABLE tt-ped-item).

    IF p-tipo = "C" THEN 
       RUN createRecord   IN h-bodi154.
    ELSE
       RUN updateRecord        in h-bodi154.
    
    RUN getRowErrors IN h-bodi154(OUTPUT TABLE RowErrors).
    
    IF CAN-FIND(FIRST RowErrors 
                WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
       FOR each rowerrors WHERE RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE  rowerrors.errornumber
                    rowerrors.errordescription VIEW-AS ALERT-BOX.
       END.
    END.
    
    IF VALID-HANDLE(h-bodi154) THEN
       DELETE PROCEDURE h-bodi154.

END PROCEDURE.


