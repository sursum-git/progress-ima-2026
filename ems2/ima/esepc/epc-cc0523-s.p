DEF NEW GLOBAL SHARED VAR wh-browse     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-coluna AS HANDLE.

DEF VAR cWhere AS CHAR.
DEF VAR hQuery AS HANDLE.
DEF VAR hBuffer AS HANDLE.

DEF VAR bh         AS HANDLE.
DEF VAR tth        AS HANDLE NO-UNDO.
DEF VAR c-coluna   AS CHAR. /* Guarda o nome real da c-coluna clicada no browse */
DEF VAR c-ordem    AS CHAR.
DEF VAR i-ct       AS INT.
DEF VAR i-row      AS int.

DEFINE VARIABLE qh AS HANDLE NO-UNDO.

ASSIGN hQuery = wh-browse:QUERY:HANDLE.
ASSIGN hbuffer = hQuery:GET-BUFFER-HANDLE(2).

ASSIGN c-coluna = wh-browse:CURRENT-COLUMN:NAME.
ASSIGN c-ordem =  IF wh-browse:CURRENT-COLUMN:SORT-ASCENDING = TRUE THEN 
                   " DESCENDING " ELSE "ASCENDING".


/* Criar a TEMP-tAABLE LIKE o BUFFER e o  index com a coluna clicada (DESCENDING OU ASCENDING) */
CREATE TEMP-TABLE tth.
tth:CREATE-LIKE(hbuffer).
tth:ADD-NEW-INDEX("abidx", FALSE, TRUE).
tth:ADD-INDEX-FIELD("abidx",c-coluna, c-ordem).
tth:TEMP-TABLE-PREPARE("ordx").

/* copiar os registros para a temp-table */
bh = tth:DEFAULT-BUFFER-HANDLE.

hquery:GET-FIRST().
DO WHILE hbuffer:AVAIL:
    bh:BUFFER-CREATE.  
    bh:BUFFER-COPY(hbuffer).
END.

/* limpar o browse */
wh-browse:DELETE-RESULT-LIST-ENTRY().


/*
IF wh-browse:CURRENT-COLUMN:SORT-ASCENDING = ? THEN DO:
   wh-browse:CLEAR-SORT-ARROWS().
   wh-browse:CURRENT-COLUMN:SORT-ASCENDING = FALSE. 
END.
ELSE 
   wh-browse:CURRENT-COLUMN:SORT-ASCENDING = NOT wh-browse:CURRENT-COLUMN:SORT-ASCENDING.
*/

/*
CREATE QUERY qh.
qh:SET-BUFFERS(bh).
qh:QUERY-PREPARE("FOR EACH ordx").
qh:QUERY-OPEN().


/* adicionar as registros da temp-table no browse */
REPEAT:  
    qh:GET-NEXT(). 
    IF qh:QUERY-OFF-END THEN LEAVE. 

    hbuffer:BUFFER-CREATE.  
    hbuffer:BUFFER-COPY(bh).
    wh-browse:CREATE-RESULT-LIST-ENTRY().
END. 
qh:QUERY-CLOSE().
bh:BUFFER-RELEASE().
DELETE OBJECT tth.
DELETE OBJECT qh.
*/



/*wh-browse:DELETE-RESULT-LIST-ENTRY().

/* Refresh no browse */
wh-browse:REFRESH().
*/

/*
MESSAGE hBuffer:NAME 
        hBuffer:NUM-FIELDS
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

/*
hbuffer:BUFFER-FIELD(i-ct):NAME 
hbuffer:BUFFER-FIELD(i-ct):DATA-TYPE
hbuffer:BUFFER-FIELD(i-ct):FORMAT
hbuffer:BUFFER-FIELD(i-ct):BUFFER-VALUE .
*/

/*
hquery:GET-FIRST().
DO WHILE hbuffer:AVAIL:

/* listar todos os campos do buffer */
REPEAT i-ct = 1 TO hbuffer:NUM-FIELDS:

EXPORT STREAM stCampos
hbuffer:BUFFER-FIELD(i-ct):NAME 
hbuffer:BUFFER-FIELD(i-ct):DATA-TYPE
hbuffer:BUFFER-FIELD(i-ct):FORMAT
hbuffer:BUFFER-FIELD(i-ct):BUFFER-VALUE .

END.
*/


   /*
ASSIGN cWhere = "FOR EACH tt_estab_ems2 NO-LOCK, EACH tt-req NO-LOCK BY " + c-coluna.

IF wh-browse:CURRENT-COLUMN:SORT-ASCENDING = FALSE THEN
   ASSIGN cWhere = "FOR EACH tt_estab_ems2 NO-LOCK, EACH tt-req NO-LOCK BY " + c-coluna.
ELSE IF wh-browse:CURRENT-COLUMN:SORT-ASCENDING = TRUE THEN 
   ASSIGN cWhere = "FOR EACH tt_estab_ems2 NO-LOCK, EACH tt-req NO-LOCK BY " + c-coluna + " DESCENDING ".
ELSE DO:
   wh-browse:CLEAR-SORT-ARROWS().
   RETURN.
END.

hQuery:QUERY-PREPARE(cWhere).
hQuery:QUERY-OPEN().
*/

