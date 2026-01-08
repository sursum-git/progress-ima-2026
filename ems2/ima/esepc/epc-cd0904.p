/****************************************************************************
** Programa: upc-cd0904.p 
** Objetivo: Criar um campo de digitacao combo-box,REGI«O onde ser† selecionada
**           a regi∆o da unidade de Federaá∆o "Sul,Sudeste,Centro Oeste,  
**           Nordeste, Distrito Federal), Somente sera permitida a inclus∆o da
**           regi∆o se o pais for BRASIL.          
**
**           Este Novo campo sera gravado na tabela UNID-FEDER.char2
**
** Autor   : FµBIO COELHO LANZA - ABRIL/2007 
*****************************************************************************/
/* Parameter Definitions ****************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF BUFFER unid-feder FOR mgadm.unid-feder.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto          AS CHAR NO-UNDO.


/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").
DEF NEW GLOBAL SHARED VAR tx-regiao AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-regiao AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/

/*
MESSAGE "p-wgh-frame  " p-wgh-frame         SKIP
        "p-ind-event  " p-ind-event         SKIP
        "p-ind-object " p-ind-object        SKIP
        "p-cod-table  " STRING(p-cod-table) SKIP 
        "p-row-table  " STRING(P-row-table)    SKIP
        "c-objeto     " c-objeto               SKIP
        "p-whh-object " p-wgh-object:FILE-NAME SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/



IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03un007.w" THEN DO:
   
   CREATE TEXT tx-regiao
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(7)"
               WIDTH         = 8
               SCREEN-VALUE  = "Regi∆o:"
               ROW           = 3.8
               COL           = 64.1
               VISIBLE       = YES.

   CREATE COMBO-BOX wh-regiao
          ASSIGN FRAME             = p-wgh-frame
                 SIDE-LABEL-HANDLE = tx-regiao:HANDLE /* Inserir Label na Inclus∆o */
                 LABEL             = "Regi∆o:"      /* Inserir Label na Inclus∆o */
                 ROW        = 3.6
                 FORMAT     = "x(16)"
                 COL        = 70.0
                 LIST-ITEMS = "Sul,Sudeste,Centro Oeste,Nordeste,Norte,Distrito Federal," 
                 VISIBLE    = YES
                 SENSITIVE  = NO.
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03un007.w" THEN DO: 

   ASSIGN wh-regiao:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03un007.w" THEN DO: 
   ASSIGN wh-regiao:SENSITIVE = NO .
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03un007.w" THEN DO: 

   ASSIGN wh-regiao:SCREEN-VALUE = " ".
   ASSIGN wh-regiao:LABEL = "Regiao:".
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03un007.w" THEN DO:

   ASSIGN wh-regiao:SCREEN-VALUE = " ".
   ASSIGN wh-regiao:LABEL = "Regiao:".
   FIND unid-feder WHERE
        ROWID(unid-feder) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL unid-feder THEN DO.
      ASSIGN wh-regiao:SCREEN-VALUE = unid-feder.char-2.
   END.
END.

IF p-ind-event = "VALIDATE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03un007.w" THEN DO:

   FIND unid-feder WHERE
        ROWID(unid-feder) = p-row-table NO-LOCK NO-ERROR.
   IF wh-regiao:SCREEN-VALUE = " " AND unid-feder.pais = "BRASIL" THEN DO.
      MESSAGE "A Regiao deve ser informada !!!" VIEW-AS ALERT-BOX.
      APPLY 'entry' TO wh-regiao.
      RETURN 'NOK'.
   END.

END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03un007.w" THEN DO:

   FIND unid-feder WHERE
        ROWID(unid-feder) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL unid-feder THEN
      ASSIGN unid-feder.char-2 = wh-regiao:SCREEN-VALUE.
END.


