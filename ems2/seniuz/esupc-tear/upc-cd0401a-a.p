DEFINE NEW GLOBAL SHARED VAR gr-emitente AS ROWID NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-query AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cod-emitente AS HANDLE NO-UNDO.

/*
FIND emitente WHERE
     ROWID(emitente) = gr-emitente NO-LOCK NO-ERROR.
*/
FIND emitente WHERE
     emitente.cod-emit = INTEGER(h-cod-emitente:SCREEN-VALUE) NO-LOCK NO-ERROR.

FIND FIRST his-emit WHERE
           his-emit.cod-emitente = emitente.cod-emit NO-LOCK NO-ERROR.

IF AVAIL his-emit THEN
   RUN pi-reposiciona-query IN h-query (INPUT ROWID(his-emit) ).
