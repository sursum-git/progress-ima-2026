DEF NEW GLOBAL SHARED VAR wh-browse     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-last-col    AS HANDLE NO-UNDO.

DEF VAR coluna AS CHAR. /* Guarda o nome real da coluna clicada no browse */
DEF VAR cWhere AS CHAR.
DEF VAR hQuery AS HANDLE.

ASSIGN coluna = wh-browse:CURRENT-COLUMN:NAME.

ASSIGN cWhere = "FOR EACH tt-doc-pend-aprov NO-LOCK BY " + coluna.

IF wh-browse:CURRENT-COLUMN:SORT-ASCENDING = ? THEN DO:
   wh-browse:CLEAR-SORT-ARROWS().
   wh-browse:CURRENT-COLUMN:SORT-ASCENDING = FALSE. 
END.
ELSE 
   wh-browse:CURRENT-COLUMN:SORT-ASCENDING = NOT wh-browse:CURRENT-COLUMN:SORT-ASCENDING.

IF wh-browse:CURRENT-COLUMN:SORT-ASCENDING = FALSE THEN
    ASSIGN cWhere = "FOR EACH tt-doc-pend-aprov NO-LOCK BY " + coluna.
ELSE IF wh-browse:CURRENT-COLUMN:SORT-ASCENDING = TRUE THEN 
   ASSIGN cWhere = "FOR EACH tt-doc-pend-aprov NO-LOCK BY " + coluna + " DESC ".
ELSE DO:
   wh-browse:CLEAR-SORT-ARROWS().
   RETURN.
END.

ASSIGN h-last-col = wh-browse:CURRENT-COLUMN.

ASSIGN hQuery = wh-browse:QUERY:HANDLE.
hQuery:QUERY-PREPARE(cWhere).
hQuery:QUERY-OPEN().

