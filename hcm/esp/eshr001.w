&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEFINE VARIABLE v-dir-parcial AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v-dir-destino AS CHARACTER   NO-UNDO.
DEFINE VARIABLE v-nom-destino AS CHARACTER   NO-UNDO.

DEFINE VARIABLE err-status AS INTEGER.

/*Define Variable chTick          As Com-Handle               No-undo.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 fi-estabel bt-gerar ~
bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fi-estabel fi-empresa fi-arq-destino ~
fi-status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancelar 
     LABEL "Fechar" 
     SIZE 9.86 BY 1.13.

DEFINE BUTTON bt-dir 
     IMAGE-UP FILE "image/im-det3.bmp":U
     IMAGE-DOWN FILE "image/im-det2.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13.

DEFINE BUTTON bt-gerar 
     LABEL "Gerar" 
     SIZE 9.86 BY 1.13.

DEFINE VARIABLE fi-arq-destino AS CHARACTER FORMAT "X(256)":U INITIAL "Arquivo destino" 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 TOOLTIP "Diretorio de destino do arquivo" NO-UNDO.

DEFINE VARIABLE fi-arq-parcial AS CHARACTER FORMAT "X(256)":U INITIAL "P:~\ColetaParcial-rep-ima.txt" 
     LABEL "Arquivo" 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88 NO-UNDO.

DEFINE VARIABLE fi-empresa AS CHARACTER FORMAT "X(256)":U INITIAL "Ima Tecidos" 
     VIEW-AS FILL-IN 
     SIZE 44.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estabel AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Estabel" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Estabelecimento do arquivo" NO-UNDO.

DEFINE VARIABLE fi-status AS CHARACTER FORMAT "X(256)":U INITIAL "-----------------------------------------" 
     VIEW-AS FILL-IN 
     SIZE 17.14 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE IMAGE im-status
     FILENAME "image/im-consulta.bmp":U
     STRETCH-TO-FIT
     SIZE 3 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 3.5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 64 BY 1.71
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-arq-parcial AT ROW 1.33 COL 9 COLON-ALIGNED WIDGET-ID 4
     fi-estabel AT ROW 2.33 COL 9 COLON-ALIGNED WIDGET-ID 2
     fi-empresa AT ROW 2.33 COL 12.14 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     bt-dir AT ROW 3.17 COL 59.29 WIDGET-ID 6
     fi-arq-destino AT ROW 3.29 COL 9 COLON-ALIGNED WIDGET-ID 16
     bt-gerar AT ROW 5 COL 2.86 WIDGET-ID 8
     bt-cancelar AT ROW 5 COL 13.43 WIDGET-ID 18
     fi-status AT ROW 5.17 COL 41.29 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     RECT-1 AT ROW 1.13 COL 2 WIDGET-ID 12
     RECT-2 AT ROW 4.71 COL 2 WIDGET-ID 14
     im-status AT ROW 5.13 COL 60.86 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 67.29 BY 6.46
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Gerar Arquivo de Ponto"
         HEIGHT             = 5.58
         WIDTH              = 66
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-dir IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-arq-destino IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-arq-parcial IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi-arq-parcial:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN fi-empresa IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-status IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR IMAGE im-status IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Gerar Arquivo de Ponto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Gerar Arquivo de Ponto */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar W-Win
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Fechar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-gerar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-gerar W-Win
ON CHOOSE OF bt-gerar IN FRAME F-Main /* Gerar */
DO:
  RUN pi-executar.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-estabel W-Win
ON LEAVE OF fi-estabel IN FRAME F-Main /* Estabel */
DO:
  /*IF SELF:SCREEN-VALUE = "1" THEN DO:
     ASSIGN fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Ima Tecidos"
            fi-arq-parcial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "M:\HCM209\especificos\pgrep\Config\ColetaParcial-rep-ima.txt"
            fi-arq-destino:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "M:\HCM209\especificos\pgrep\ima\" + string(year(TODAY)) + "-" + string(MONTH(TODAY)) + "-" + string(DAY(TODAY)) + ".lst".
  END.
  ELSE DO:
      IF SELF:SCREEN-VALUE = "5" THEN DO:
         ASSIGN fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Medtextil"
                fi-arq-parcial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "M:\HCM209\especificos\pgrep\Config\ColetaParcial-rep-med.txt"
                fi-arq-destino:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "M:\HCM209\especificos\pgrep\med\" + string(year(TODAY)) + "-" + string(MONTH(TODAY)) + "-" + string(DAY(TODAY)) + ".lst".
      END.

  END.*/
  RUN pi-altera.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlFrame.PSTimer.Tick W-Win 
PROCEDURE CtrlFrame.PSTimer.Tick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SEARCH(v-dir-parcial) <> ? THEN DO:
   ASSIGN fi-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Coleta encontrada".
         /* fi-status:bgcolor In Frame {&Frame-Name}      = 2.*/
   im-status:LOAD-IMAGE("image\im-det2.bmp") IN FRAME {&FRAME-NAME}. 
           

END.
ELSE DO:
   ASSIGN fi-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Coleta nÆo encontrada".
          /*fi-status:bgcolor In Frame {&Frame-Name}      = 12.*/
   im-status:LOAD-IMAGE("image\im-det3.bmp") IN FRAME {&FRAME-NAME}. 
END.

/*CtrlFrame.PSTimer.Tick*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-estabel fi-empresa fi-arq-destino fi-status 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-1 RECT-2 fi-estabel bt-gerar bt-cancelar 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /*Assign chTick = chCtrlFrame:Controls:Item(1).*/
  RUN pi-altera.
  /*ASSIGN chTick:ENABLED  = YES
       chTick:Interval = 6000.*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-altera W-Win 
PROCEDURE pi-altera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN v-nom-destino = string(year(TODAY),"9999") + "-" + string(MONTH(TODAY),"99") + "-" + string(DAY(TODAY),"99") 
                         + "_" + SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + "h" 
                               + SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) + "m" 
                               + SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) + "s.lst"
         v-dir-parcial = "P:\ColetaParcial-rep-xxx"
         v-dir-destino = "P:\" + v-nom-destino.
  
  CASE fi-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
      WHEN "1" THEN DO:
          ASSIGN fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Ima Tecidos"
                 v-dir-parcial = replace(v-dir-parcial,"xxx","ima")
                 v-dir-destino = replace(v-dir-destino,"xxx","ima").
      END.
      WHEN "5" THEN DO:
          ASSIGN fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Medtextil"
                v-dir-parcial = replace(v-dir-parcial,"xxx","med")  
                v-dir-destino = replace(v-dir-destino,"xxx","med"). 
      END.
      WHEN "7" THEN DO:
          ASSIGN fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Ima Itabirito"
                v-dir-parcial = replace(v-dir-parcial,"xxx","ita")  
                v-dir-destino = replace(v-dir-destino,"xxx","ita"). 
      END.
      OTHERWISE DO:
          ASSIGN fi-empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NÆo Encontrado"
                v-dir-parcial = "NÆo Encontrado"
                v-dir-destino = "NÆo Encontrado"
                v-nom-destino = "--".
      END.

  END CASE.

  ASSIGN fi-arq-parcial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-dir-parcial
         fi-arq-destino:SCREEN-VALUE IN FRAME {&FRAME-NAME} = v-dir-destino.

  RUN esapi/seek-arquivo.p (INPUT v-dir-parcial, OUTPUT v-dir-parcial).

  RUN CtrlFrame.PSTimer.Tick.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar W-Win 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SEARCH(v-dir-parcial) <> ? THEN DO.
    
    OS-COPY VALUE(v-dir-parcial) VALUE(v-dir-destino).
    IF SEARCH(v-dir-destino) <> ? THEN DO.
       OS-DELETE value(v-dir-parcial).
       
       IF OS-ERROR = 0 THEN DO:
          MESSAGE "O arquivo '" v-nom-destino "' foi gerado com sucesso"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          /*apply "close":U to this-procedure.*/
       END.
       ELSE DO:
          MESSAGE "Erro inesperado" SKIP 
                  "Erro nø: " + STRING(OS-ERROR,"99") SKIP
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          OS-DELETE value(v-dir-destino).
       END.
    END.
    ELSE
        MESSAGE "Erro ao gerar arquivo." 
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
END.
ELSE
    MESSAGE "NÆo foi encontrada coleta parcial" SKIP
            "Efetue a coleta antes de gerar"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.

RUN pi-altera.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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

