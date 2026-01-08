&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME d-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS d-vapara 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
define output parameter     p-row-tabela    as rowid    no-undo.

/* Local Variable Definitions ---                                       */

DEF VAR c-etq AS CHAR FORMAT "x(15)".
DEF VAR i-digito AS INT.

{esinc/sz-pcl.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartVaPara
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME d-vapara

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ob-etiqueta.cod-estabel 
&Scoped-define ENABLED-TABLES ob-etiqueta
&Scoped-define FIRST-ENABLED-TABLE ob-etiqueta
&Scoped-Define ENABLED-OBJECTS tg-etq fi-num-etiqueta tg-rolo-imp tg-progr ~
bt-ok bt-cancela bt-ajuda rt-button RECT-1 RECT-2 RECT-13 
&Scoped-Define DISPLAYED-FIELDS ob-etiqueta.cod-estabel ~
ob-etiqueta.progressivo 
&Scoped-define DISPLAYED-TABLES ob-etiqueta
&Scoped-define FIRST-DISPLAYED-TABLE ob-etiqueta
&Scoped-Define DISPLAYED-OBJECTS tg-etq fi-num-etiqueta tg-rolo-imp ~
fi-nr-container fi-it-codigo fi-cod-refer fi-num-rolo tg-progr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-bt-ajuda 
       MENU-ITEM mi-sobre       LABEL "Sobre..."      .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 FONT 1.

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 FONT 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 FONT 1.

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ref" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-container AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Container" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88.

DEFINE VARIABLE fi-num-etiqueta AS CHARACTER FORMAT "x(10)" 
     LABEL "Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE fi-num-rolo AS INTEGER FORMAT ">>>>,>>9" INITIAL 0 
     LABEL "Num Rolo" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68.14 BY 5.5.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 1.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY 5.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 68 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tg-etq AS LOGICAL INITIAL yes 
     LABEL "Etiqueta" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-progr AS LOGICAL INITIAL no 
     LABEL "Progressivo" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.

DEFINE VARIABLE tg-rolo-imp AS LOGICAL INITIAL no 
     LABEL "Rolo Imp" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME d-vapara
     ob-etiqueta.cod-estabel AT ROW 1.42 COL 10 COLON-ALIGNED WIDGET-ID 2
          LABEL "Est"
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     tg-etq AT ROW 2.75 COL 6 WIDGET-ID 14
     fi-num-etiqueta AT ROW 3.75 COL 10 COLON-ALIGNED
     tg-rolo-imp AT ROW 2.75 COL 27 WIDGET-ID 16
     fi-nr-container AT ROW 3.75 COL 32 COLON-ALIGNED WIDGET-ID 8
     fi-it-codigo AT ROW 4.75 COL 32 COLON-ALIGNED WIDGET-ID 20
     fi-cod-refer AT ROW 5.75 COL 32 COLON-ALIGNED WIDGET-ID 22
     fi-num-rolo AT ROW 6.75 COL 32 COLON-ALIGNED WIDGET-ID 6
     tg-progr AT ROW 2.75 COL 49 WIDGET-ID 18
     ob-etiqueta.progressivo AT ROW 3.75 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 18 BY .88
     bt-ok AT ROW 8.25 COL 3.14
     bt-cancela AT ROW 8.25 COL 14
     bt-ajuda AT ROW 8.25 COL 59
     rt-button AT ROW 8 COL 2
     RECT-1 AT ROW 2.5 COL 2 WIDGET-ID 10
     RECT-2 AT ROW 2.5 COL 24 WIDGET-ID 12
     RECT-13 AT ROW 1.25 COL 2 WIDGET-ID 24
     SPACE(0.71) SKIP(7.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "V  Para Etiqueta de Tecido"
         DEFAULT-BUTTON bt-ok CANCEL-BUTTON bt-cancela.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartVaPara
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB d-vapara 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-vapara.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX d-vapara
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME d-vapara:SCROLLABLE       = FALSE
       FRAME d-vapara:HIDDEN           = TRUE.

ASSIGN 
       bt-ajuda:POPUP-MENU IN FRAME d-vapara       = MENU POPUP-MENU-bt-ajuda:HANDLE.

/* SETTINGS FOR FILL-IN ob-etiqueta.cod-estabel IN FRAME d-vapara
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME d-vapara
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME d-vapara
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-container IN FRAME d-vapara
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-num-rolo IN FRAME d-vapara
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.progressivo IN FRAME d-vapara
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX d-vapara
/* Query rebuild information for DIALOG-BOX d-vapara
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX d-vapara */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME d-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-vapara d-vapara
ON GO OF FRAME d-vapara /* V  Para Etiqueta de Tecido */
DO:

  IF INPUT FRAME {&FRAME-NAME} tg-etq THEN
     FIND ob-etiqueta WHERE
          ob-etiqueta.cod-estabel   = INPUT FRAME {&FRAME-NAME} ob-etiqueta.cod-estabel AND
          ob-etiqueta.num-etiqueta  = INTEGER(INPUT FRAME {&FRAME-NAME} fi-num-etiqueta) 
          USE-INDEX indice4 NO-LOCK NO-ERROR.
  ELSE IF INPUT FRAME {&FRAME-NAME} tg-rolo-imp THEN
     FIND ob-etiqueta WHERE
          ob-etiqueta.cod-estabel   = INPUT FRAME {&FRAME-NAME} ob-etiqueta.cod-estabel AND
          ob-etiqueta.nr-container = INPUT FRAME {&FRAME-NAME} fi-nr-container AND
          ob-etiqueta.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo AND
          ob-etiqueta.cod-refer = INPUT FRAME {&FRAME-NAME} fi-cod-refer AND
          ob-etiqueta.num-rolo-imp = INPUT FRAME {&FRAME-NAME} fi-num-rolo 
          NO-LOCK NO-ERROR.
  ELSE 
     FIND ob-etiqueta WHERE
          ob-etiqueta.progressivo  = INPUT FRAME {&FRAME-NAME} ob-etiqueta.progressivo 
          NO-LOCK NO-ERROR.

  IF NOT AVAIL ob-etiqueta THEN DO:
     {utp/ut-table.i espec ob-etiqueta 1}
     RUN utp/ut-msgs.p (INPUT "show":U,
                        INPUT 2,
                        INPUT RETURN-VALUE).
     RETURN NO-APPLY.
  END.

  ASSIGN p-row-tabela = ROWID(ob-etiqueta).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-vapara d-vapara
ON WINDOW-CLOSE OF FRAME d-vapara /* V  Para Etiqueta de Tecido */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda d-vapara
ON CHOOSE OF bt-ajuda IN FRAME d-vapara /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-container d-vapara
ON VALUE-CHANGED OF fi-nr-container IN FRAME d-vapara /* Container */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     (SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) < '0' OR
     SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) > '9') THEN DO.
     BELL.
     APPLY 'backspace' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN i-digito = 99.
  IF LENGTH(SELF:SCREEN-VALUE) = 10 THEN DO.
     ASSIGN i-digito = INT(SUBSTRING(SELF:SCREEN-VALUE,10,1)).
     ASSIGN SELF:SCREEN-VALUE = SUBSTR(SELF:SCREEN-VALUE,1,9).
     APPLY 'END' TO SELF.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta d-vapara
ON VALUE-CHANGED OF fi-num-etiqueta IN FRAME d-vapara /* Etiqueta */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     (SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) < '0' OR
     SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) > '9') THEN DO.
     BELL.
     APPLY 'backspace' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN i-digito = 99.
  IF LENGTH(SELF:SCREEN-VALUE) = 10 THEN DO.
     ASSIGN i-digito = INT(SUBSTRING(SELF:SCREEN-VALUE,10,1)).
     ASSIGN SELF:SCREEN-VALUE = SUBSTR(SELF:SCREEN-VALUE,1,9).
     APPLY 'END' TO SELF.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-rolo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-rolo d-vapara
ON VALUE-CHANGED OF fi-num-rolo IN FRAME d-vapara /* Num Rolo */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     (SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) < '0' OR
     SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) > '9') THEN DO.
     BELL.
     APPLY 'backspace' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN i-digito = 99.
  IF LENGTH(SELF:SCREEN-VALUE) = 10 THEN DO.
     ASSIGN i-digito = INT(SUBSTRING(SELF:SCREEN-VALUE,10,1)).
     ASSIGN SELF:SCREEN-VALUE = SUBSTR(SELF:SCREEN-VALUE,1,9).
     APPLY 'END' TO SELF.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre d-vapara
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-etq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-etq d-vapara
ON VALUE-CHANGED OF tg-etq IN FRAME d-vapara /* Etiqueta */
DO:
    ASSIGN fi-num-etiqueta:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nr-container:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-it-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-cod-refer:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-num-rolo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           ob-etiqueta.progressivo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    ASSIGN fi-num-etiqueta:SCREEN-VALUE = ''
           fi-nr-container:SCREEN-VALUE = ''
           fi-it-codigo:SCREEN-VALUE = ''
           fi-cod-refer:SCREEN-VALUE = ''
           fi-num-rolo:SCREEN-VALUE = ''
           ob-etiqueta.progressivo:SCREEN-VALUE = ''.

    ASSIGN tg-etq:SCREEN-VALUE = 'NO'
           tg-progr:SCREEN-VALUE = 'NO'.

   IF SELF:INPUT-VALUE = YES THEN DO.
      ASSIGN ob-etiqueta.cod-estabel:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
             fi-num-etiqueta:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
      APPLY 'entry' TO fi-num-etiqueta IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-progr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-progr d-vapara
ON VALUE-CHANGED OF tg-progr IN FRAME d-vapara /* Progressivo */
DO:
    ASSIGN fi-num-etiqueta:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nr-container:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-it-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-cod-refer:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-num-rolo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           ob-etiqueta.progressivo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    ASSIGN fi-num-etiqueta:SCREEN-VALUE = ''
           fi-nr-container:SCREEN-VALUE = ''
           fi-it-codigo:SCREEN-VALUE = ''
           fi-cod-refer:SCREEN-VALUE = ''
           fi-num-rolo:SCREEN-VALUE = ''
           ob-etiqueta.progressivo:SCREEN-VALUE = ''.

    ASSIGN tg-etq:SCREEN-VALUE = 'NO'
           tg-progr:SCREEN-VALUE = 'NO'.

    IF SELF:INPUT-VALUE = YES THEN DO.
       ASSIGN ob-etiqueta.progressivo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
       APPLY 'entry' TO ob-etiqueta.progressivo.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-rolo-imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-rolo-imp d-vapara
ON VALUE-CHANGED OF tg-rolo-imp IN FRAME d-vapara /* Rolo Imp */
DO:
    ASSIGN fi-num-etiqueta:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-nr-container:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-it-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-cod-refer:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-num-rolo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           ob-etiqueta.progressivo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

    ASSIGN fi-num-etiqueta:SCREEN-VALUE = ''
           fi-nr-container:SCREEN-VALUE = ''
           fi-it-codigo:SCREEN-VALUE = ''
           fi-cod-refer:SCREEN-VALUE = ''
           fi-num-rolo:SCREEN-VALUE = ''
           ob-etiqueta.progressivo:SCREEN-VALUE = ''.

    ASSIGN tg-etq:SCREEN-VALUE = 'NO'
           tg-progr:SCREEN-VALUE = 'NO'.
  
    IF SELF:INPUT-VALUE = YES THEN DO.
       ASSIGN fi-nr-container:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              fi-it-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              fi-cod-refer:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              fi-num-rolo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

       APPLY 'entry' TO fi-nr-container.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK d-vapara 


/* ***************************  Main Block  *************************** */

assign p-row-tabela = ?.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects d-vapara  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available d-vapara  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI d-vapara  _DEFAULT-DISABLE
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
  HIDE FRAME d-vapara.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI d-vapara  _DEFAULT-ENABLE
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
  DISPLAY tg-etq fi-num-etiqueta tg-rolo-imp fi-nr-container fi-it-codigo 
          fi-cod-refer fi-num-rolo tg-progr 
      WITH FRAME d-vapara.
  IF AVAILABLE ob-etiqueta THEN 
    DISPLAY ob-etiqueta.cod-estabel ob-etiqueta.progressivo 
      WITH FRAME d-vapara.
  ENABLE ob-etiqueta.cod-estabel tg-etq fi-num-etiqueta tg-rolo-imp tg-progr 
         bt-ok bt-cancela bt-ajuda rt-button RECT-1 RECT-2 RECT-13 
      WITH FRAME d-vapara.
  VIEW FRAME d-vapara.
  {&OPEN-BROWSERS-IN-QUERY-d-vapara}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy d-vapara 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize d-vapara 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "G01ES049" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ped-venda NO-LOCK NO-ERROR.
  ASSIGN ob-etiqueta.cod-estabel:SCREEN-VALUE = ped-venda.cod-estabel.

  APPLY 'entry' TO ob-etiqueta.cod-estabel.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records d-vapara  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartVaPara, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed d-vapara 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

