/******************************************************************************
*   Programa .....: IMCD0821                                                  *
*   Data .........: 05/01/2005                                                *
*   Cliente ......: IMA                                                       *
*   Programador ..: Aroldo                                                    *
*   Objetivo .....: Criar bot∆o para Amarrar Tipo de Pedido x Usuario         *
******************************************************************************/

/****** Parametros ******/
DEF INPUT PARAMETER p-ind-event  AS CHARACTER.              
DEF INPUT PARAMETER p-ind-object AS CHARACTER.              
DEF INPUT PARAMETER p-wgh-object AS HANDLE.                 
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.          
DEF INPUT PARAMETER p-cod-table  AS CHARACTER.              
DEF INPUT PARAMETER p-row-table  AS ROWID.                  

/****** Vari†veis ******/
DEF NEW GLOBAL SHARED VAR wh-bt1-cd0821    AS WIDGET-HANDLE NO-UNDO.

DEF VAR c-char  AS CHAR.
DEF BUFFER b-user-coml FOR user-coml.
{include/i-vrtab.i user-coml}.

IF VALID-HANDLE(p-wgh-object) THEN
   ASSIGN c-char = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"), p-wgh-object:FILE-NAME,"~/").

IF p-ind-event = 'INITIALIZE' AND p-ind-object = 'CONTAINER' THEN DO: 
   IF NOT VALID-HANDLE(wh-bt1-cd0821) THEN DO:
      CREATE BUTTON wh-bt1-cd0821
      ASSIGN LABEL           = '&Tipo Pedido'
             ROW             = 1.25
             COLUMN          = 62
             FRAME           = p-wgh-frame
             SENSITIVE       = YES
             VISIBLE         = YES
             HEIGHT          = 1.25
             WIDTH           = 4.0
             TOOLTIP         = "Usuario x Tipo Pedido"
             TRIGGERS:
                 ON CHOOSE PERSISTENT RUN imp/imes202.w.                           
             END TRIGGERS.
       IF wh-bt1-cd0821:LOAD-IMAGE-UP("image\im-descricao.bmp") THEN.
       ASSIGN wh-bt1-cd0821:TOOLTIP = "Usuario x Tipo Pedido".        
   END.
END.

IF p-ind-event = 'DISPLAY' AND p-ind-object = 'VIEWER' AND p-cod-table = 'user-coml' THEN DO:
   FIND FIRST b-user-coml WHERE ROWID(b-user-coml) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL b-user-coml THEN 
      ASSIGN gr-user-coml = rowid(b-user-coml).
END.

