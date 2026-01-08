/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Variable Definitions *****************************************************/
DEF BUFFER cidade FOR mgind.cidade.
DEFINE NEW GLOBAL SHARED VARIABLE wh-tg-capital AS WIDGET-HANDLE.

/* Main Block ***************************************************************/
IF p-ind-event = "BEFORE-INITIALIZE" THEN DO.
   CREATE TOGGLE-BOX wh-tg-capital
          ASSIGN FRAME        = p-wgh-frame
                 LABEL        = 'Capital' 
                 WIDTH        = 15
                 ROW          = 5.15
                 COLUMN       = 50
                 HEIGHT       = 0.88
                 VISIBLE      = YES
                 TOOLTIP      = "Informa se a Cidade ‚ a Capital do Estado em Referˆncia...".
END.

IF p-ind-event = 'AFTER-DISPLAY' THEN DO.
   FIND cidade WHERE
        ROWID(cidade) = p-row-table NO-ERROR.
   IF AVAIL cidade THEN
      ASSIGN wh-tg-capital:SCREEN-VALUE = STRING(cidade.log-2).
END.

IF p-ind-event = 'AFTER-ENABLE' THEN 
   ASSIGN wh-tg-capital:SENSITIVE = YES.

IF p-ind-event = 'AFTER-DISABLE' THEN 
   ASSIGN wh-tg-capital:SENSITIVE = NO.

IF p-ind-event = 'AFTER-ASSIGN' THEN DO.
   FIND cidade WHERE
        ROWID(cidade) = p-row-table NO-ERROR.
   IF AVAIL cidade THEN
      ASSIGN cidade.log-2 = wh-tg-capital:INPUT-VALUE.
END.

