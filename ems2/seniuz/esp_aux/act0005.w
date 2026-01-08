&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgmov            PROGRESS
*/
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ACT00005 2.04.00.000}
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR c-arquivo  AS CHAR.
DEF VAR c-conteudo AS CHAR.

DEF STREAM saida-can.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS nota-fiscal.cod-estabel nota-fiscal.serie ~
nota-fiscal.nr-nota-fis nota-fiscal.dt-cancela 
&Scoped-define ENABLED-TABLES nota-fiscal
&Scoped-define FIRST-ENABLED-TABLE nota-fiscal
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-1 bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-FIELDS nota-fiscal.cod-estabel nota-fiscal.serie ~
nota-fiscal.nr-nota-fis nota-fiscal.dt-cancela 
&Scoped-define DISPLAYED-TABLES nota-fiscal
&Scoped-define FIRST-DISPLAYED-TABLE nota-fiscal
&Scoped-Define DISPLAYED-OBJECTS fi-end-exp-can fi-chave-sefaz 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY .88.

DEFINE VARIABLE fi-chave-sefaz AS CHARACTER FORMAT "X(60)":U 
     LABEL "Chave SEFAZ" 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE VARIABLE fi-end-exp-can AS CHARACTER FORMAT "X(70)":U 
     VIEW-AS FILL-IN 
     SIZE 48.43 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.38
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     nota-fiscal.cod-estabel AT ROW 1.46 COL 14 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .88
     fi-end-exp-can AT ROW 1.46 COL 17.57 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     nota-fiscal.serie AT ROW 2.46 COL 14 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 4.72 BY .88
     nota-fiscal.nr-nota-fis AT ROW 3.46 COL 14 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     BUTTON-1 AT ROW 3.46 COL 26.29 WIDGET-ID 14
     nota-fiscal.dt-cancela AT ROW 4.46 COL 14 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     fi-chave-sefaz AT ROW 5.46 COL 14 COLON-ALIGNED WIDGET-ID 10
     bt-ok AT ROW 7.13 COL 2.14
     bt-cancelar AT ROW 7.13 COL 13.14
     bt-ajuda AT ROW 7.13 COL 70.29
     RECT-1 AT ROW 6.92 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.58
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Digitacao
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-digita ASSIGN
         HIDDEN             = YES
         TITLE              = "Reenvio de XML de Nota Cancelada Ö Receita"
         HEIGHT             = 7.38
         WIDTH              = 80
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-digita 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-digit.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-digita
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fi-chave-sefaz IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-end-exp-can IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Reenvio de XML de Nota Cancelada Ö Receita */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Reenvio de XML de Nota Cancelada Ö Receita */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-digita
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  FIND CURRENT nota-fiscal NO-LOCK.
  ASSIGN c-arquivo = fi-end-exp-can:SCREEN-VALUE IN FRAME {&FRAME-NAME} +
                     STRING(nota-fiscal.nr-nota-fis,"9999999") + "_" + TRIM(nota-fiscal.serie) +
                     ".txt"
         c-conteudo = TRIM(SUBSTR(nota-fiscal.char-2,3,60)) + 
                      ";Nota cancelada por conter erros de processamento".

  OUTPUT STREAM saida-can TO VALUE(c-arquivo).
  PUT STREAM saida-can UNFORMAT c-conteudo SKIP.
  OUTPUT STREAM saida-can CLOSE.

  MESSAGE "Nota Fiscal: " nota-fiscal.nr-nota-fis 
          " SÇrie: " nota-fiscal.serie 
          " Estab: " nota-fiscal.cod-estabel SKIP
          "Cancelamento gerado com sucesso!" SKIP
          "Arquivo: " c-arquivo
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  ASSIGN nota-fiscal.dt-cancela:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
         fi-chave-sefaz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  APPLY 'entry' TO nota-fiscal.nr-nota-fis.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 w-digita
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  FIND nota-fiscal WHERE nota-fiscal.cod-estabel = INPUT FRAME {&FRAME-NAME} nota-fiscal.cod-estabel
                     AND nota-fiscal.serie       = INPUT FRAME {&FRAME-NAME} nota-fiscal.serie
                     AND nota-fiscal.nr-nota-fis = INPUT FRAME {&FRAME-NAME} nota-fiscal.nr-nota-fis
                   NO-LOCK NO-ERROR.
  
  IF NOT AVAIL nota-fiscal THEN DO:
     MESSAGE "Nota Fiscal n∆o encontrada."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO nota-fiscal.nr-nota-fis IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END. 
  ELSE DO: 
     IF nota-fiscal.dt-cancela = ? OR
        SUBSTR(nota-fiscal.char-2,3,60) = "" THEN DO:
        MESSAGE "Nota Fiscal n∆o pode ser reenviada Ö SEFAZ."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY 'entry' TO nota-fiscal.nr-nota-fis IN FRAME {&FRAME-NAME}.
           RETURN NO-APPLY.
     END.
     ELSE
        ASSIGN nota-fiscal.dt-cancela:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(nota-fiscal.dt-cancela)
               fi-chave-sefaz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(nota-fiscal.char-2,3,60).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nota-fiscal.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nota-fiscal.cod-estabel w-digita
ON LEAVE OF nota-fiscal.cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
   FIND gati-nfe-param WHERE gati-nfe-param.cod-estabel = INPUT FRAME {&FRAME-NAME} nota-fiscal.cod-estabel
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL gati-nfe-param THEN DO:
      ASSIGN fi-end-exp-can:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
      MESSAGE "ParÉmetro dos diret¢rios de sa°da n∆o encontrado."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO nota-fiscal.cod-estabel IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END. 
   ELSE
      ASSIGN fi-end-exp-can:SCREEN-VALUE IN FRAME {&FRAME-NAME} = gati-nfe-param.end-exp-can.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nota-fiscal.cod-estabel w-digita
ON RETURN OF nota-fiscal.cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  APPLY 'tab' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nota-fiscal.nr-nota-fis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nota-fiscal.nr-nota-fis w-digita
ON LEAVE OF nota-fiscal.nr-nota-fis IN FRAME F-Main /* Nr Nota Fiscal */
DO:
   APPLY 'choose' TO button-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nota-fiscal.nr-nota-fis w-digita
ON RETURN OF nota-fiscal.nr-nota-fis IN FRAME F-Main /* Nr Nota Fiscal */
DO:
    APPLY 'choose' TO button-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME nota-fiscal.serie
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL nota-fiscal.serie w-digita
ON RETURN OF nota-fiscal.serie IN FRAME F-Main /* SÇrie */
DO:
  APPLY 'tab' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-digita  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-digita  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-digita  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
  THEN DELETE WIDGET w-digita.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-digita  _DEFAULT-ENABLE
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
  DISPLAY fi-end-exp-can fi-chave-sefaz 
      WITH FRAME F-Main IN WINDOW w-digita.
  IF AVAILABLE nota-fiscal THEN 
    DISPLAY nota-fiscal.cod-estabel nota-fiscal.serie nota-fiscal.nr-nota-fis 
          nota-fiscal.dt-cancela 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 nota-fiscal.cod-estabel nota-fiscal.serie 
         nota-fiscal.nr-nota-fis BUTTON-1 nota-fiscal.dt-cancela bt-ok 
         bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-digita 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-digita 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ACT0005" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {include/i-inifld.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-digita  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this Digitacao, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-digita 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

