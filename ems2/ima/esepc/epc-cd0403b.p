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

/* Global Variable Definitions **********************************************/
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl  AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-container     AS HANDLE. 
DEFINE NEW GLOBAL SHARED VAR h-folder        AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h_b01ad098      AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-bt-historico  AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bt-documentos AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-data-implant  AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario   AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR gr-emitente     AS ROWID NO-UNDO.

/* TON 29/01/2016
DEF NEW GLOBAL SHARED VAR h-fi-ramo      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fi-desc-ramo AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-ramo      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR vg-cod-ramo-ativ AS INT NO-UNDO.
FIM TON 29/01/2016 */

/* Variable Definitions *****************************************************/
DEFINE VAR h-objeto       AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR wh-obj         AS WIDGET-HANDLE EXTENT 20 NO-UNDO.
DEFINE VAR i-level        AS INTEGER INITIAL 1.
DEFINE VAR c-objeto       AS CHAR NO-UNDO.
DEFINE VAR i-ct           AS INT.
DEFINE VAR c-folder       AS CHARACTER NO-UNDO.
DEFINE VAR c-objects      AS CHARACTER NO-UNDO.
DEFINE VAR h-object       AS HANDLE    NO-UNDO.
DEFINE VAR i-objects      AS INTEGER   NO-UNDO.
DEFINE VAR l-record       AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VAR l-group-assign AS LOGICAL   NO-UNDO INITIAL NO.

DEF VAR i-pag AS INTEGER.
DEF VAR c-list AS CHAR.

/* Main Block ***************************************************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:private-data, "~/").

/*MESSAGE 
        p-ind-event  
        p-ind-object 
        p-wgh-object 
        p-wgh-frame  
        p-cod-table  
       STRING( p-row-table  )
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/


IF p-ind-event = "before-initialize" AND
   p-ind-object = "CONTAINER" THEN DO: 
   

   ASSIGN h-container = p-wgh-object.
   /* Encontra o campo  cbx-ambiente-sefaz */
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
        MESSAGE h-objeto:NAME
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      IF h-objeto:TYPE <> "xfield-group" THEN DO:
         IF h-objeto:NAME = "cbx-ambiente-sefaz" THEN
            ASSIGN h-objeto:sensitive = no.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
   
END.




