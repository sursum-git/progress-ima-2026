&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-dt-trans fi-it-codigo fi-cod-refer fi-esp ~
fi-qtd Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-trans fi-it-codigo fi-cod-refer ~
fi-esp fi-qtd 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Referencia" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dt-trans AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-qtd AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-esp AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "TRA", 33,
"DIV", 6
     SIZE 19 BY 1.25 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi-dt-trans AT ROW 1.25 COL 18 COLON-ALIGNED WIDGET-ID 2
     fi-it-codigo AT ROW 2.5 COL 18 COLON-ALIGNED WIDGET-ID 4
     fi-cod-refer AT ROW 3.75 COL 18 COLON-ALIGNED WIDGET-ID 6
     fi-esp AT ROW 4.83 COL 20 NO-LABEL WIDGET-ID 12
     fi-qtd AT ROW 6.25 COL 18 COLON-ALIGNED WIDGET-ID 10
     Btn_OK AT ROW 10.5 COL 5
     Btn_Cancel AT ROW 10.5 COL 23
     "Especie:" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 5 COL 10 WIDGET-ID 16
     SPACE(46.13) SKIP(6.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Elimina Movimentos"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Elimina Movimentos */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   MESSAGE 'Confirma elimina‡Æo ?'
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-conf AS LOG.
   IF l-conf THEN
      RUN pi-elimina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-dt-trans fi-it-codigo fi-cod-refer fi-esp fi-qtd 
      WITH FRAME Dialog-Frame.
  ENABLE fi-dt-trans fi-it-codigo fi-cod-refer fi-esp fi-qtd Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-elimina Dialog-Frame 
PROCEDURE pi-elimina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        FIND FIRST param-global NO-LOCK NO-ERROR.
        FIND FIRST param-estoq NO-LOCK NO-ERROR.
        
        FOR EACH movto-estoq WHERE
                 movto-estoq.dt-trans = INPUT FRAME {&FRAME-NAME} fi-dt-trans AND
                 movto-estoq.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo AND
                 movto-estoq.cod-refer = INPUT FRAME {&FRAME-NAME} fi-cod-refer AND
                 movto-estoq.esp-docto = INPUT FRAME {&FRAME-NAME} fi-esp AND
                 movto-estoq.cod-prog-ori = 'essp0145' AND
                 movto-estoq.quantidade = INPUT FRAME {&FRAME-NAME} fi-qtd 
                 EXCLUSIVE-LOCK. 

            DO:
                FIND item WHERE
                     item.it-codigo = movto-estoq.it-codigo NO-ERROR.

                FIND item-estab WHERE
                     item-estab.it-codigo   = movto-estoq.it-codigo AND
                     item-estab.cod-estabel = movto-estoq.cod-estabel
                     NO-ERROR.
    
                IF NOT CAN-DO("4,1",STRING(item.tipo-contr)) THEN DO:
                    IF movto-estoq.esp-docto <> 1 AND
                       movto-estoq.esp-docto <> 8 AND
                       movto-estoq.esp-docto <> 27 THEN DO:

                        FIND conta-contab WHERE
                             conta-contab.ep-codigo = param-global.empresa-prin AND 
                             conta-contab.ct-codigo = movto-estoq.ct-codigo AND 
                             conta-contab.sc-codigo = movto-estoq.sc-codigo
                             NO-LOCK NO-ERROR.

                        IF AVAILABLE conta-contab AND
                           conta-contab.estoque = 1 THEN
                            IF movto-estoq.tipo-trans = 1 THEN
                                ASSIGN item.consumo-aad       = item.consumo-aad + movto-estoq.quantidade
                                       item-estab.consumo-aad = item-estab.consumo-aad + movto-estoq.quantidade.
                            ELSE 
                                ASSIGN item.consumo-aad       = item.consumo-aad - movto-estoq.quantidade
                                       item-estab.consumo-aad = item-estab.consumo-aad - movto-estoq.quantidade.
                    END.
                END.

                IF item.tipo-contr        <> 4 AND 
                   movto-estoq.quantidade <> 0 THEN DO:

                    FIND saldo-estoq WHERE
                         saldo-estoq.it-codigo = movto-estoq.it-codigo AND
                         saldo-estoq.cod-estabel = movto-estoq.cod-estabel AND
                         saldo-estoq.cod-depos = movto-estoq.cod-depos AND
                         saldo-estoq.cod-localiz = movto-estoq.cod-localiz AND
                         saldo-estoq.lote = movto-estoq.lote AND
                         saldo-estoq.cod-refer = movto-estoq.cod-refer 
                         NO-ERROR.
                    IF AVAILABLE saldo-estoq THEN DO:
                        IF movto-estoq.tipo-trans = 1 THEN 
                           ASSIGN saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu - movto-estoq.quantidade.
                        ELSE 
                           ASSIGN saldo-estoq.qtidade-atu = saldo-estoq.qtidade-atu + movto-estoq.quantidade.
                    END.
                END.
            END.

            FIND FIRST movto-mat USE-INDEX num-seq WHERE
                       movto-mat.nr-ord-prod = movto-estoq.nr-ord-produ AND
                       movto-mat.num-sequen = movto-estoq.num-sequen NO-ERROR.
            IF AVAILABLE movto-mat THEN DO:
                DELETE movto-mat VALIDATE(TRUE,"").
            END.
            DELETE movto-estoq VALIDATE(TRUE,"").

            MESSAGE 'Movimento Eliminado' 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
        END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

