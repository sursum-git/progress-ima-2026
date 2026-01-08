&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 btImpPedManual btPainel ~
btImpArqRet btAcoesLisa btImpArqRet-2 btAjustCorte btConcEtq ~
btConcEtqContainer btEnvRomaneio btSaldoTerc btConsWeb btAtuLocaliz ~
btExclRemContainer btExclRemAvulsa 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAcoesLisa 
     LABEL "Aá‰es Lisa" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btAjustCorte 
     LABEL "Ajuste Corte" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btAtuLocaliz 
     LABEL "Atualizar Localizaá∆o LISA" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btConcEtq 
     LABEL "Conciliaá∆o Etiquetas" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btConcEtqContainer 
     LABEL "Conciliaá∆o Etiquetas por Container" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btConsWeb 
     LABEL "Consultas Web" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btEnvRomaneio 
     LABEL "Envio Manual De Romaneio" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btExclRemAvulsa 
     LABEL "Exclus∆o Remessa Avulsa" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btExclRemContainer 
     LABEL "Exclus∆o Remessa Container" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btImpArqRet 
     LABEL "Imp. Arquivos Retorno" 
     SIZE 27 BY 1.13 TOOLTIP "Esse Programa Ç um contigente para quando as NFs n∆o forem importadas pela API".

DEFINE BUTTON btImpArqRet-2 
     LABEL "Integrar NF Retorno" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btImpPedManual 
     LABEL "Imp. Manual Pedidos LISA" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btPainel 
     LABEL "Painel LISA" 
     SIZE 27 BY 1.13.

DEFINE BUTTON btSaldoTerc 
     LABEL "Saldo Terceiro LISA" 
     SIZE 27 BY 1.13.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 6.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.72 BY 6.25.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 6.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btImpPedManual AT ROW 4.13 COL 9.14 WIDGET-ID 4
     btPainel AT ROW 4.17 COL 40.43 WIDGET-ID 2
     btImpArqRet AT ROW 5.29 COL 9.14 WIDGET-ID 10
     btAcoesLisa AT ROW 5.33 COL 40.43 WIDGET-ID 20
     btImpArqRet-2 AT ROW 6.46 COL 9.14 WIDGET-ID 12
     btAjustCorte AT ROW 6.54 COL 40.57 WIDGET-ID 22
     btConcEtq AT ROW 10.5 COL 9 WIDGET-ID 30
     btConcEtqContainer AT ROW 10.5 COL 38.43 WIDGET-ID 46
     btEnvRomaneio AT ROW 11.75 COL 9 WIDGET-ID 32
     btSaldoTerc AT ROW 11.79 COL 38.29 WIDGET-ID 42
     btConsWeb AT ROW 12.96 COL 9 WIDGET-ID 40
     btAtuLocaliz AT ROW 13.04 COL 38 WIDGET-ID 44
     btExclRemContainer AT ROW 14.25 COL 9 WIDGET-ID 34
     btExclRemAvulsa AT ROW 14.25 COL 37.86 WIDGET-ID 48
     "Outros" VIEW-AS TEXT
          SIZE 6.57 BY .67 AT ROW 9.58 COL 10.43 WIDGET-ID 28
     "Retornos" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 3 COL 11.29 WIDGET-ID 8
     "Integraá∆o Pedidos" VIEW-AS TEXT
          SIZE 17.72 BY .67 AT ROW 3 COL 41.43 WIDGET-ID 18
     RECT-1 AT ROW 3.29 COL 8.14 WIDGET-ID 6
     RECT-2 AT ROW 3.29 COL 39.43 WIDGET-ID 16
     RECT-3 AT ROW 9.75 COL 8 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.29 BY 17
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Menu - LISA"
         HEIGHT             = 17
         WIDTH              = 78.29
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Menu - LISA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Menu - LISA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAcoesLisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAcoesLisa W-Win
ON CHOOSE OF btAcoesLisa IN FRAME F-Main /* Aá‰es Lisa */
DO:
  RUN esporadicos/acoesPedidoLisa.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAjustCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAjustCorte W-Win
ON CHOOSE OF btAjustCorte IN FRAME F-Main /* Ajuste Corte */
DO:
  RUN esporadicos/ajusteEtq.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAtuLocaliz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtuLocaliz W-Win
ON CHOOSE OF btAtuLocaliz IN FRAME F-Main /* Atualizar Localizaá∆o LISA */
DO:
  RUN lisa/atu_localiz_etq_lisa.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConcEtq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConcEtq W-Win
ON CHOOSE OF btConcEtq IN FRAME F-Main /* Conciliaá∆o Etiquetas */
DO:
  RUN lisa/conciliacao_etq_lisa.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConcEtqContainer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConcEtqContainer W-Win
ON CHOOSE OF btConcEtqContainer IN FRAME F-Main /* Conciliaá∆o Etiquetas por Container */
DO:
  RUN lisa/conciliacao_etq_lisa_com_container.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConsWeb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConsWeb W-Win
ON CHOOSE OF btConsWeb IN FRAME F-Main /* Consultas Web */
DO:
  OS-COMMAND SILENT VALUE('start http://imaonline.imatextil.com.br/iol/menu_lisa').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btEnvRomaneio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btEnvRomaneio W-Win
ON CHOOSE OF btEnvRomaneio IN FRAME F-Main /* Envio Manual De Romaneio */
DO:
  RUN lisa/wEnviarRomaneio.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExclRemAvulsa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExclRemAvulsa W-Win
ON CHOOSE OF btExclRemAvulsa IN FRAME F-Main /* Exclus∆o Remessa Avulsa */
DO:
 RUN esporadicos/excluirNfRemessaAvulsaLisa.w .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExclRemContainer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExclRemContainer W-Win
ON CHOOSE OF btExclRemContainer IN FRAME F-Main /* Exclus∆o Remessa Container */
DO:
 RUN esporadicos/excluirNfRemessaLisa.w .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btImpArqRet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImpArqRet W-Win
ON CHOOSE OF btImpArqRet IN FRAME F-Main /* Imp. Arquivos Retorno */
DO:
  RUN esporadicos/importarRetornos.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btImpArqRet-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImpArqRet-2 W-Win
ON CHOOSE OF btImpArqRet-2 IN FRAME F-Main /* Integrar NF Retorno */
DO:
  RUN lisa/lisa0101.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btImpPedManual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btImpPedManual W-Win
ON CHOOSE OF btImpPedManual IN FRAME F-Main /* Imp. Manual Pedidos LISA */
DO:
  RUN lisa/importarpedidoslisa.w .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPainel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPainel W-Win
ON CHOOSE OF btPainel IN FRAME F-Main /* Painel LISA */
DO:
  RUN lisa/integra.w .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSaldoTerc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSaldoTerc W-Win
ON CHOOSE OF btSaldoTerc IN FRAME F-Main /* Saldo Terceiro LISA */
DO:
 RUN esp/saldo-terc-lisa.w .

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
  ENABLE RECT-1 RECT-2 RECT-3 btImpPedManual btPainel btImpArqRet btAcoesLisa 
         btImpArqRet-2 btAjustCorte btConcEtq btConcEtqContainer btEnvRomaneio 
         btSaldoTerc btConsWeb btAtuLocaliz btExclRemContainer btExclRemAvulsa 
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

