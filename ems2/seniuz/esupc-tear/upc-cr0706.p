/* Parameter Definitions ****************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR wh-tx-susp  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-ed-obs   AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-bt-documentos  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto          AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO: 

   FIND FIRST usuar_grp_usuar WHERE 
              usuar_grp_usuar.cod_usuar = c-seg-usuario AND
             (usuar_grp_usuar.cod_grp_usuar = "FIN" OR
              usuar_grp_usuar.cod_grp_usuar = "ELD" OR
              usuar_grp_usuar.cod_grp_usuar = "BRP" OR
              usuar_grp_usuar.cod_grp_usuar = "SUP")
             NO-LOCK NO-ERROR.

   CREATE BUTTON h-bt-documentos
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 5
                 HEIGHT       = 1.22
                 ROW          = 1.27
                 COL          = 65.3
                 VISIBLE      = AVAIL usuar_grp_usuar
                 SENSITIVE    = YES
                 TOOLTIP      = "Documentos Digitalizados do Cliente"
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esapi/doctos-digitalizados.p. 
                END TRIGGERS.

   h-bt-documentos:LOAD-IMAGE("image/im-docs.bmp").
END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v31ad098.w" THEN DO:

   CREATE TEXT wh-tx-susp
        ASSIGN FRAME        = p-wgh-frame
               FORMAT       = "x(16)"
               WIDTH        = 42.50
               HEIGHT       = 1.00
               SCREEN-VALUE = "CRêDITO SUSPENSO"
               ROW          = 7.63
               COL          = 44.7
               FONT         = 22
               FGCOLOR      = 12
               VISIBLE      = NO.

  CREATE EDITOR wh-ed-obs
       ASSIGN FRAME        = p-wgh-frame
              ROW          = 3.70
              COL          = 52.7
              WIDTH        = 34.9
              HEIGHT       = 6.85
              FONT         = 22
              SCROLLBAR-VERTICAL = YES
              READ-ONLY    = YES
              FGCOLOR      = 12
              VISIBLE      = NO.
END.

IF p-ind-event  = "DISPLAY"  AND
   p-ind-object = "VIEWER" AND
   c-objeto     = "v31ad098.w" THEN DO:
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.

   ASSIGN wh-ed-obs:SCREEN-VALUE = "CREDITO SUSPENSO" + CHR(10) + CHR(10) + 
                                    emitente.observacoes.

   /*wh-tx-susp:VISIBLE = emitente.ind-cre-cli = 4.*/
   wh-ed-obs:VISIBLE = emitente.ind-cre-cli = 4.

   ASSIGN wh-ed-obs:SENSITIVE = YES.

END.

