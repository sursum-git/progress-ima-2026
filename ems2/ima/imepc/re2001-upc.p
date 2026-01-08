/*------------------------------------------------------------------*/ 
/* EPC - External Procedure Call (Chamada Externa de Procedure)     */
/* Programa: RE2001-UPC --> Epc do re2001                           */
/* Autor   : Jo∆o Gabriel Costa Rocha                               */
/* Data    : 01/05/2003                                             */
/* Objetivo: Disponibilizar o ROWID da tabela doc-fisico           .*/
/*                                                                  */
/*------------------------------------------------------------------*/ 

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR gl-row-doc-fisico AS ROWID NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, 
                  "~/"), p-wgh-object:PRIVATE-DATA, "~/").
                  
/*
MESSAGE "RE2001" SKIP
        "EVENTO:" p-ind-event   SKIP
        "OBJETO:" p-ind-object SKIP
        "OBJ-HANDLE:" STRING(p-wgh-object) SKIP
        "FRAME:" STRING(p-wgh-frame)  SKIP
        "TABELA:" p-cod-table  SKIP
        "ROWID:" STRING(p-row-table) SKIP  
        "OBJ " c-objeto
        VIEW-AS ALERT-BOX.
*/

IF p-ind-event  = "BEFORE-DISPLAY" AND
   p-ind-object = "VIEWER"         AND
   p-cod-table  = "doc-fisico"     AND
   c-objeto     = "v09in163.w"     THEN DO:
    IF p-row-table <> ? THEN DO:
        ASSIGN gl-row-doc-fisico = p-row-table.
        /*
        MESSAGE "2001  pegou rowid" SKIP
                string(gl-row-doc-fisico) VIEW-AS ALERT-BOX.
        */        
    END.
END.

RETURN "Ok".










