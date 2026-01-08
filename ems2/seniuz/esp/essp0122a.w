&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0122A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF INPUT-OUTPUT PARAMETER c-padrao-ini   AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-padrao-fin   AS CHAR.
DEF INPUT-OUTPUT PARAMETER i-cor-ini      AS INT.
DEF INPUT-OUTPUT PARAMETER i-cor-fin      AS INT.
DEF INPUT-OUTPUT PARAMETER i-tipo-ini     AS INT.
DEF INPUT-OUTPUT PARAMETER i-tipo-fin     AS INT.
DEF INPUT-OUTPUT PARAMETER i-tamanho-ini  AS INT.
DEF INPUT-OUTPUT PARAMETER i-tamanho-fin  AS INT.
DEF INPUT-OUTPUT PARAMETER c-dt-receb-ini AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-dt-receb-fin AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-deposito-ini AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-deposito-fin AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-local-ini    AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-local-fin    AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-letra-ini    AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-letra-fin    AS CHAR.
DEF INPUT-OUTPUT PARAMETER l-ok           AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-letra-ini fi-letra-fin fi-padrao-ini ~
fi-padrao-fin fi-cor-ini fi-cor-fin fi-tipo-ini fi-tipo-fin fi-tamanho-ini ~
fi-tamanho-fin fi-deposito-ini fi-deposito-fin fi-local-ini fi-local-fin ~
fi-dt-receb-ini fi-dt-receb-fin bt-ajuda bt-ok bt-cancelar IMAGE-1 IMAGE-13 ~
IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-2 IMAGE-23 IMAGE-24 IMAGE-25 ~
IMAGE-26 IMAGE-27 IMAGE-28 IMAGE-29 IMAGE-30 IMAGE-5 RECT-1 RECT-49 
&Scoped-Define DISPLAYED-OBJECTS fi-letra-ini fi-letra-fin fi-padrao-ini ~
fi-padrao-fin fi-cor-ini fi-cor-fin fi-tipo-ini fi-tipo-fin fi-tamanho-ini ~
fi-tamanho-fin fi-deposito-ini fi-deposito-fin fi-local-ini fi-local-fin ~
fi-dt-receb-ini fi-dt-receb-fin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VARIABLE fi-cor-fin AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cor-ini AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cor do Algod∆o" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-deposito-fin AS CHARACTER FORMAT "X(20)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Deposito Final" NO-UNDO.

DEFINE VARIABLE fi-deposito-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Codigo do Deposito" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Deposito Inicial" NO-UNDO.

DEFINE VARIABLE fi-dt-receb-fin AS DATE FORMAT "99/99/9999":U INITIAL 01/01/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data Recebimento Final" NO-UNDO.

DEFINE VARIABLE fi-dt-receb-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Recebimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data Recebimento Inicial" NO-UNDO.

DEFINE VARIABLE fi-letra-fin AS CHARACTER FORMAT "X(3)":U INITIAL "Z99" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Letral Final" NO-UNDO.

DEFINE VARIABLE fi-letra-ini AS CHARACTER FORMAT "X(3)":U INITIAL "A00" 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Classificaá∆o Inicial" NO-UNDO.

DEFINE VARIABLE fi-local-fin AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .79 TOOLTIP "Codigo da Localizaá∆o Final" NO-UNDO.

DEFINE VARIABLE fi-local-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Codigo Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 TOOLTIP "Codigo da Localizaá∆o Inicial" NO-UNDO.

DEFINE VARIABLE fi-padrao-fin AS CHARACTER FORMAT "X(20)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-padrao-ini AS CHARACTER FORMAT "X(20)" 
     LABEL "Padrao":R12 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 TOOLTIP "Padr∆o do Algodao" NO-UNDO.

DEFINE VARIABLE fi-tamanho-fin AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tamanho-ini AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Tamanho" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tipo-fin AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tipo-ini AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Tipo do Algodao" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-13
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-14
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-15
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-16
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-17
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-23
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-24
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-25
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-26
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-27
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-28
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 79 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 8.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-letra-ini AT ROW 1.54 COL 19.72 COLON-ALIGNED
     fi-letra-fin AT ROW 1.54 COL 53.29 COLON-ALIGNED NO-LABEL
     fi-padrao-ini AT ROW 2.54 COL 19.72 COLON-ALIGNED HELP
          "Padr∆o do Algod∆o"
     fi-padrao-fin AT ROW 2.54 COL 53.29 COLON-ALIGNED NO-LABEL
     fi-cor-ini AT ROW 3.54 COL 19.72 COLON-ALIGNED
     fi-cor-fin AT ROW 3.54 COL 53.29 COLON-ALIGNED NO-LABEL
     fi-tipo-ini AT ROW 4.54 COL 19.72 COLON-ALIGNED
     fi-tipo-fin AT ROW 4.54 COL 53.29 COLON-ALIGNED NO-LABEL
     fi-tamanho-ini AT ROW 5.5 COL 19.72 COLON-ALIGNED
     fi-tamanho-fin AT ROW 5.5 COL 53.29 COLON-ALIGNED NO-LABEL
     fi-deposito-ini AT ROW 6.54 COL 19.72 COLON-ALIGNED
     fi-deposito-fin AT ROW 6.54 COL 53.29 COLON-ALIGNED NO-LABEL
     fi-local-ini AT ROW 7.54 COL 19.72 COLON-ALIGNED
     fi-local-fin AT ROW 7.54 COL 53.29 COLON-ALIGNED NO-LABEL
     fi-dt-receb-ini AT ROW 8.54 COL 19.72 COLON-ALIGNED HELP
          "Data Recebimento Inicial"
     fi-dt-receb-fin AT ROW 8.54 COL 53.29 COLON-ALIGNED NO-LABEL
     bt-ajuda AT ROW 10.21 COL 70
     bt-ok AT ROW 10.25 COL 3
     bt-cancelar AT ROW 10.25 COL 14
     IMAGE-1 AT ROW 1.54 COL 43.14
     IMAGE-13 AT ROW 5.54 COL 43.14
     IMAGE-14 AT ROW 4.54 COL 51.72
     IMAGE-15 AT ROW 5.54 COL 51.72
     IMAGE-16 AT ROW 3.54 COL 43.14
     IMAGE-17 AT ROW 3.54 COL 51.72
     IMAGE-2 AT ROW 1.54 COL 51.72
     IMAGE-23 AT ROW 2.54 COL 43.14
     IMAGE-24 AT ROW 2.54 COL 51.72
     IMAGE-25 AT ROW 6.54 COL 51.72
     IMAGE-26 AT ROW 7.54 COL 51.72
     IMAGE-27 AT ROW 8.54 COL 51.72
     IMAGE-28 AT ROW 6.54 COL 43.14
     IMAGE-29 AT ROW 7.54 COL 43.14
     IMAGE-30 AT ROW 8.54 COL 43.14
     IMAGE-5 AT ROW 4.54 COL 43.14
     RECT-1 AT ROW 10.04 COL 2
     RECT-49 AT ROW 1.04 COL 2
     " Seleá∆o" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.86 BY 10.88
         FONT 1
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "ParÉmetro da Posiá∆o de Estoque dos Fardos Algod∆o"
         HEIGHT             = 10.46
         WIDTH              = 81
         MAX-HEIGHT         = 21.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.13
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* ParÉmetro da Posiá∆o de Estoque dos Fardos Algod∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* ParÉmetro da Posiá∆o de Estoque dos Fardos Algod∆o */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  ASSIGN l-ok = NO. 
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  ASSIGN c-letra-ini    = INPUT FRAME {&FRAME-NAME} fi-letra-ini   
         c-letra-fin    = INPUT FRAME {&FRAME-NAME} fi-letra-fin   
         c-padrao-ini   = INPUT FRAME {&FRAME-NAME} fi-padrao-ini   
         c-padrao-fin   = INPUT FRAME {&FRAME-NAME} fi-padrao-fin   
         i-cor-ini      = INPUT FRAME {&FRAME-NAME} fi-cor-ini   
         i-cor-fin      = INPUT FRAME {&FRAME-NAME} fi-cor-fin   
         i-tipo-ini     = INPUT FRAME {&FRAME-NAME} fi-tipo-ini   
         i-tipo-fin     = INPUT FRAME {&FRAME-NAME} fi-tipo-fin   
         i-tamanho-ini  = INPUT FRAME {&FRAME-NAME} fi-tamanho-ini   
         i-tamanho-fin  = INPUT FRAME {&FRAME-NAME} fi-tamanho-fin   
         c-deposito-ini = INPUT FRAME {&FRAME-NAME} fi-deposito-ini
         c-deposito-fin = INPUT FRAME {&FRAME-NAME} fi-deposito-fin
         c-local-ini    = INPUT FRAME {&FRAME-NAME} fi-local-ini
         c-local-fin    = INPUT FRAME {&FRAME-NAME} fi-local-fin
         c-dt-receb-ini = INPUT FRAME {&FRAME-NAME} fi-dt-receb-ini
         c-dt-receb-fin = INPUT FRAME {&FRAME-NAME} fi-dt-receb-fin
         l-ok           = YES.  
  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cor-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cor-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cor-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES054.w
                     &campo=fi-cor-fin
                     &campozoom=codigo}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cor-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cor-ini w-window
ON LEAVE OF fi-cor-ini IN FRAME F-Main /* Cor do Algod∆o */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cor-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cor-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cor-ini IN FRAME F-Main /* Cor do Algod∆o */
DO:
  {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES054.w
                     &campo=fi-cor-ini
                     &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-deposito-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-deposito-ini w-window
ON LEAVE OF fi-deposito-ini IN FRAME F-Main /* Codigo do Deposito */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-deposito-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-letra-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-letra-ini w-window
ON LEAVE OF fi-letra-ini IN FRAME F-Main /* Codigo */
DO:
  /*
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-letra-fin:SCREEN-VALUE = SELF:SCREEN-VALUE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-local-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-local-ini w-window
ON LEAVE OF fi-local-ini IN FRAME F-Main /* Codigo Localizaá∆o */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-local-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-padrao-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-padrao-ini w-window
ON LEAVE OF fi-padrao-ini IN FRAME F-Main /* Padrao */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-padrao-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tamanho-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tamanho-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-tamanho-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES056.w
                     &campo=fi-tamanho-fin
                     &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tamanho-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tamanho-ini w-window
ON LEAVE OF fi-tamanho-ini IN FRAME F-Main /* Tamanho */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-tamanho-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tamanho-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-tamanho-ini IN FRAME F-Main /* Tamanho */
DO:
  {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES056.w
                     &campo=fi-tamanho-ini
                     &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-tipo-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES055.w
                     &campo=fi-cor-fin
                     &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo-ini w-window
ON LEAVE OF fi-tipo-ini IN FRAME F-Main /* Tipo do Algodao */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-tipo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-tipo-ini IN FRAME F-Main /* Tipo do Algodao */
DO:
  {include/zoomvar.i &prog-zoom=especificos\eszoom\Z01ES055.w
                     &campo=fi-cor-ini
                     &campozoom=codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */
 fi-cor-ini:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
 fi-cor-fin:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
 fi-tipo-ini:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
 fi-tipo-fin:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
 fi-tamanho-ini:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
 fi-tamanho-fin:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY fi-letra-ini fi-letra-fin fi-padrao-ini fi-padrao-fin fi-cor-ini 
          fi-cor-fin fi-tipo-ini fi-tipo-fin fi-tamanho-ini fi-tamanho-fin 
          fi-deposito-ini fi-deposito-fin fi-local-ini fi-local-fin 
          fi-dt-receb-ini fi-dt-receb-fin 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE fi-letra-ini fi-letra-fin fi-padrao-ini fi-padrao-fin fi-cor-ini 
         fi-cor-fin fi-tipo-ini fi-tipo-fin fi-tamanho-ini fi-tamanho-fin 
         fi-deposito-ini fi-deposito-fin fi-local-ini fi-local-fin 
         fi-dt-receb-ini fi-dt-receb-fin bt-ajuda bt-ok bt-cancelar IMAGE-1 
         IMAGE-13 IMAGE-14 IMAGE-15 IMAGE-16 IMAGE-17 IMAGE-2 IMAGE-23 IMAGE-24 
         IMAGE-25 IMAGE-26 IMAGE-27 IMAGE-28 IMAGE-29 IMAGE-30 IMAGE-5 RECT-1 
         RECT-49 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}

      ASSIGN fi-letra-ini     = c-letra-ini   
             fi-letra-fin     = c-letra-fin   
             fi-padrao-ini    = c-padrao-ini    
             fi-padrao-fin    = c-padrao-fin 
             fi-cor-ini       = i-cor-ini    
             fi-cor-fin       = i-cor-fin 
             fi-tipo-ini      = i-tipo-ini    
             fi-tipo-fin      = i-tipo-fin 
             fi-tamanho-ini   = i-tamanho-ini    
             fi-tamanho-fin   = i-tamanho-fin 
             fi-deposito-ini  = c-deposito-ini
             fi-deposito-fin  = c-deposito-fin
             fi-local-ini     = c-local-ini
             fi-local-fin     = c-local-fin
             fi-dt-receb-ini  = DATE(c-dt-receb-ini)
             fi-dt-receb-fin  = DATE(c-dt-receb-fin).
  
/*  {utp/ut9000.i "XX9999" "9.99.99.999"} */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
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

