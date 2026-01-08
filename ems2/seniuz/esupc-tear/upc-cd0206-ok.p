/****************************************************************************
** Programa: upc-cd0206.p 
** Objetivo: Criar um campo de digitacao combo-box, CLASSE onde ser† selecionada
**           a calsse da familia comercial "Reagentes,Equipamento,Aluguel,  
**           Peáas e Acess¢rios).
**
**           Este Novo campo sera gravado na tabela fam-comerc.char-2
**
** Autor   : TONINHO - SENIUZ - OUT/2009 
*****************************************************************************/
/* Parameter Definitions ****************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/

/* Variable Definitions *****************************************************/
DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto          AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR tx-classe AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-classe AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di050.w" THEN DO:
   
   CREATE TEXT tx-classe
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(7)"
               WIDTH         = 8
               SCREEN-VALUE  = "Classe:"
               ROW           = 4
               COL           = 64.1
               VISIBLE       = YES.

   CREATE COMBO-BOX wh-classe
          ASSIGN FRAME             = p-wgh-frame
                 SIDE-LABEL-HANDLE = tx-classe:HANDLE /* Inserir Label na Inclus∆o */
                 LABEL             = "Classe:"      /* Inserir Label na Inclus∆o */
                 FORMAT            = "x(20)" 
                 ROW               = 4.75
                 COL               = 70.0
                 LIST-ITEMS        = ",Reagentes,Equipamento,Aluguel,Peáas e Acess¢rios" 
                 VISIBLE           = YES
                 SENSITIVE         = NO.
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di050.w" THEN 
   ASSIGN wh-classe:SENSITIVE = YES.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di050.w" THEN 
   ASSIGN wh-classe:SENSITIVE = NO.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di050.w" THEN 
   ASSIGN wh-classe:SCREEN-VALUE = " "
          wh-classe:LABEL = "Classe:".

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di050.w" THEN DO:

   ASSIGN wh-classe:SCREEN-VALUE = " "
          wh-classe:LABEL = "Classe:".

   FIND fam-comerc WHERE
        ROWID(fam-comerc) = p-row-table NO-LOCK NO-ERROR.

   IF AVAIL fam-comerc THEN 
      ASSIGN wh-classe:SCREEN-VALUE = fam-comerc.char-2.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di050.w" THEN DO:

   FIND fam-comerc WHERE
        ROWID(fam-comerc) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL fam-comerc THEN
      ASSIGN fam-comerc.char-2 = wh-classe:SCREEN-VALUE.
END.



