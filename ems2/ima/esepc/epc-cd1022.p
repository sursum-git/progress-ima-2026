/*******************************************************************
** Programa: epc-cd0704.p                                         **
** Objetivo: Epc do programa de cadastro de emitente(cd0704)      **
** Autor...: Anderson Fagner Maio/2009                            **
** Observ..:                                                      **  
*******************************************************************/

DEF INPUT PARAM p-ind-event   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object  AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object  AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame   AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table   AS ROWID         NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-bt-complemento AS WIDGET-HANDLE NO-UNDO.

IF p-ind-event  = "INITIALIZE"    AND 
   p-ind-object = "CONTAINER"               THEN DO.

   CREATE BUTTON h-bt-complemento
          ASSIGN FRAME = p-wgh-frame
                 ROW          = 1.2
                 COL          = 60
                 WIDTH        = 7
                 HEIGHT       = 1.3
                 LABEL        = "" 
                 VISIBLE      = YES
                 SENSITIVE    = YES
                 TOOLTIP      = "Informa Dados Complementares da IMA"
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esp/escd0704.p.
                 END TRIGGERS.

   h-bt-complemento:LOAD-IMAGE("image\ico\ima3.ico").

END.
