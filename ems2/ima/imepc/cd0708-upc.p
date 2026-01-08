/******************************************************************************
*   Programa .....: cd0708-upc.p                                              *
*   Data .........: 31/10/2002                                                *
*   Cliente ......: Ima                                                       *
*   Objetivo .....: Inclus∆o do campo usu†rio                                 *
*                                                                             *
******************************************************************************/

/****** Parametros ******/
DEF INPUT PARAMETER p-ind-event            AS CHARACTER.
DEF INPUT PARAMETER p-ind-object           AS CHARACTER.
DEF INPUT PARAMETER p-wgh-object           AS HANDLE.
DEF INPUT PARAMETER p-wgh-frame            AS WIDGET-HANDLE.
DEF INPUT PARAMETER p-cod-table            AS CHARACTER.
DEF INPUT PARAMETER p-row-table            AS ROWID.

/****** Vari†veis ******/
DEF NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-folder          AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-objeto         AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-folder                            AS CHARACTER NO-UNDO.
DEF VAR c-char                              AS CHAR.

DEF NEW GLOBAL SHARED VAR wx-viewer         AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wx-container      AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wh-campo-conteudo AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wh-campo-label    AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wh-caixa-postal   AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR wh-complemento    AS WIDGET-HANDLE.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-objeto     AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-frame      AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo      AS WIDGET-HANDLE NO-UNDO.

DEF BUFFER b-representante FOR repres.

IF VALID-HANDLE(p-wgh-object) THEN
   ASSIGN c-char = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"), p-wgh-object:FILE-NAME,"~/").


RUN CepOnline/upc/upc-cd0708.p (INPUT p-ind-event,
                                INPUT p-ind-object,
                                INPUT p-wgh-object,
                                INPUT p-wgh-frame,
                                INPUT p-cod-table,
                                INPUT p-row-table).


IF c-char = "v01ad229.w"  THEN DO:
   ASSIGN wh-objeto  = p-wgh-frame:FIRST-CHILD.
   DO WHILE VALID-HANDLE(wh-objeto):      
      CASE wh-objeto:NAME:
           WHEN 'caixa-postal' THEN DO:                        
              ASSIGN wh-caixa-postal = wh-objeto.  
           END.
           WHEN 'complemento' THEN DO:                        
              ASSIGN wh-complemento = wh-objeto.  
           END.
      END CASE. 
      IF wh-objeto:TYPE = 'field-group'
      THEN ASSIGN wh-objeto = wh-objeto:FIRST-CHILD.
      ELSE ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.
   END.
END.


/**************** tratamento do campo novo *****************/

/*
IF VALID-HANDLE(wh-campo-conteudo) THEN DO:
   wh-campo-conteudo:MOVE-AFTER-TAB-ITEM(wh-caixa-postal).
   wh-complemento:MOVE-AFTER-TAB-ITEM(wh-campo-conteudo).
END.
*/

DO TRANSACTION :
CASE p-ind-event:
   WHEN 'BEFORE-INITIALIZE' THEN DO:
     CASE p-ind-object:
        WHEN 'CONTAINER' THEN DO:   
           RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object, 
                                                  INPUT "PAGE-SOURCE":U, 
                                                  OUTPUT c-folder).
           ASSIGN h-folder     = WIDGET-HANDLE(c-folder) NO-ERROR.
           ASSIGN wx-container = p-wgh-object.
        END.
     END CASE.
   END.
   WHEN 'DISPLAY'  THEN DO:
     CASE p-ind-object:
        WHEN 'VIEWER' THEN DO: 
          IF c-char = 'v01ad229.w' THEN DO:

             IF NOT VALID-HANDLE(wh-campo-label) THEN DO:
                CREATE TEXT wh-campo-label
                ASSIGN ROW               = 1.17
                       COLUMN            = 64
                       FRAME             = p-wgh-frame
                       SENSITIVE         = NO
                       VISIBLE           = YES
                       HEIGHT-CHARS      = 0.88
                       WIDTH-CHARS       = 7
                       FORMAT            = 'x(8)'
                       SCREEN-VALUE      = 'Usu†rio:'.

                 CREATE FILL-IN wh-campo-conteudo
                 ASSIGN ROW               = 1.17
                        COLUMN            = 70
                        FRAME             = p-wgh-frame
                        SENSITIVE         = NO
                        VISIBLE           = YES
                        HEIGHT-CHARS      = 0.88
                        WIDTH-CHARS       = 12
                        FORMAT            = 'x(12)'
                        SCREEN-VALUE      = ''
                        TRIGGERS:
                           ON "F5" PERSISTENT RUN imepc/cd0708-zoom.p.
                           ON "mouse-select-dblclick" PERSISTENT RUN imepc/cd0708-zoom.p.
                        END TRIGGERS.
             END.
             ELSE DO:             
                  FIND FIRST b-representante WHERE ROWID(b-representante) = p-row-table NO-LOCK NO-ERROR.
                  IF AVAIL b-representante THEN
                     ASSIGN wh-campo-conteudo:SCREEN-VALUE = SUBSTRING(b-representante.char-1,500,12).
                  ELSE 
                     ASSIGN wh-campo-conteudo:SCREEN-VALUE = "".
             END.

             IF wh-campo-conteudo:LOAD-MOUSE-POINTER("image\lupa.cur") THEN.

             /* posiciona na primeira pagina do folder */
             IF VALID-HANDLE(wx-container) THEN DO: 
                IF VALID-HANDLE(wx-viewer) THEN
                   RUN dispatch IN wx-viewer  ("initialize":U).
                RUN select-page IN wx-container (INPUT 1).
             END.
          END.
        END.
     END CASE.
   END.
   
   WHEN 'ENABLE' THEN DO:
      CASE p-ind-object:
        WHEN 'VIEWER' THEN DO: 
          IF c-char = 'v01ad229.w' AND 
             VALID-HANDLE(wh-campo-conteudo) THEN DO:
             ASSIGN wh-campo-conteudo:SENSITIVE = YES.
             
             FIND FIRST b-representante WHERE ROWID(b-representante) = p-row-table NO-LOCK NO-ERROR.
             IF AVAIL b-representante THEN
                ASSIGN wh-campo-conteudo:SCREEN-VALUE = SUBSTRING(b-representante.char-1,500,12).
             ELSE 
                ASSIGN wh-campo-conteudo:SCREEN-VALUE = "".
          END.
        END.
      END.       
   END.
   WHEN 'DISABLE' THEN DO:
      CASE p-ind-object:
        WHEN 'VIEWER' THEN DO: 
          IF c-char = 'v01ad229.w' THEN DO:
             IF VALID-HANDLE(wh-campo-conteudo) THEN 
                ASSIGN wh-campo-conteudo:SENSITIVE = NO.
          END.
        END.
      END.       
   END.
   
   WHEN 'VALIDATE' THEN DO:
     CASE p-ind-object:
        WHEN 'VIEWER' THEN DO:
          IF c-char = 'v01ad229.w' THEN DO:             
             IF VALID-HANDLE(wh-campo-conteudo) AND wh-campo-conteudo:SCREEN-VALUE <> "" THEN DO:
                FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = wh-campo-conteudo:SCREEN-VALUE NO-LOCK NO-ERROR.
                IF NOT AVAIL usuar_mestre THEN DO:
                   MESSAGE "Usu†rio n∆o cadastrado ..." VIEW-AS ALERT-BOX ERROR.
                   RETURN "nok".
                END.
             END.
          END.
        END.
     END CASE.
   END.           
   
   WHEN 'ASSIGN' THEN DO:  
     CASE p-ind-object:
        WHEN 'VIEWER' THEN DO:
          IF c-char = 'v01ad229.w' THEN DO:             
             IF VALID-HANDLE(wh-campo-conteudo) THEN DO:
                FIND FIRST b-representante WHERE ROWID(b-representante) = p-row-table NO-LOCK NO-ERROR.
                IF AVAIL b-representante THEN
                   OVERLAY(b-representante.char-1,500,12) = wh-campo-conteudo:SCREEN-VALUE.
             END.
          END.
        END. 
     END CASE.
   END.
   
END CASE.
END.
RETURN "Ok".
