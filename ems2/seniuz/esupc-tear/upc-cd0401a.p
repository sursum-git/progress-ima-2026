/****************************************************************************
** Programa: upc-cd0401.p 
** Objetivo: Preencher Automaticamente os Campos:
**           (ENDERECO, ENDEREÄO2, BAIRRO, CIDADE, CEP, UF) do folder 
**           endereáo, apos o cursor estiver no campo CEP, e for prescionado
**           a tecla F10.
**
*****************************************************************************/
/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Variable Definitions *****************************************************/
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-query AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bt-firhis AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-bt-lashis AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-cod-emitente AS HANDLE NO-UNDO.

DEFINE VAR c-objects      AS CHARACTER NO-UNDO.
DEFINE VAR h-object       AS HANDLE    NO-UNDO.
DEFINE VAR i-objects      AS INTEGER   NO-UNDO.
DEFINE VAR l-record       AS LOGICAL   NO-UNDO INITIAL NO.

DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "cod-emitente" THEN
            ASSIGN h-cod-emitente = h-objeto.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.

   CREATE BUTTON h-bt-firhis
          ASSIGN FRAME = p-wgh-frame
                  WIDTH        = 5
                  HEIGHT       = 1.22
                  ROW          = 2.51
                  COL          = 2
                  VISIBLE      = YES
                  SENSITIVE    = YES
                  TOOLTIP      = "Posiciona no PRIMEIRO Historico do Cliente"
                  TRIGGERS:
                      ON "CHOOSE":U PERSISTENT RUN esupc/upc-cd0401a-a.p.
                 END TRIGGERS.

   CREATE BUTTON h-bt-lashis
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 5
                 HEIGHT       = 1.22
                 ROW          = 1.31
                 COL          = 2
                 VISIBLE      = YES
                 SENSITIVE    = YES
                 TOOLTIP      = "Posiciona no ÈLTIMO Historico do Cliente"
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esupc/upc-cd0401a-b.p.
                END TRIGGERS.

    h-bt-firhis:LOAD-IMAGE("image/im-fir.bmp").
    h-bt-lashis:LOAD-IMAGE("image/im-las.bmp").
END.

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO:

   RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                          INPUT "CONTAINER-TARGET":U,
                                          OUTPUT c-objects).

   DO i-objects = 1 TO NUM-ENTRIES(c-objects):
      ASSIGN h-object = WIDGET-HANDLE(entry(i-objects, c-objects)).

      IF INDEX(h-object:PRIVATE-DATA, "qry") <> 0 AND  /* Vocà deve verificar se e a query principal */
         NOT l-record THEN DO:
         ASSIGN l-record = YES.

         ASSIGN h-query = h-object.
      END.
   END.
END.


