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
{include/i-prgvrs.i ESSP0181A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Variavies de Parƒmetros */
/* Local Variable Definitions ---                                       */
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel-ini   AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel-fin   AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER da-data-mov-ini     LIKE mov-est-acbm.data-mov.
DEFINE INPUT-OUTPUT PARAMETER da-data-mov-fin     LIKE mov-est-acbm.data-mov.
DEFINE INPUT-OUTPUT PARAMETER da-dt-ant-ini       LIKE mov-est-acbm.data-mov.
DEFINE INPUT-OUTPUT PARAMETER da-dt-ant-fin       LIKE mov-est-acbm.data-mov.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini     LIKE mov-est-acbm.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin     LIKE mov-est-acbm.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini     LIKE mov-est-acbm.cod-refer.                              
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin     LIKE mov-est-acbm.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER c-tipo-defeito-ini  LIKE mov-est-acbd.cod-tipo-def. 
DEFINE INPUT-OUTPUT PARAMETER c-tipo-defeito-fin  LIKE mov-est-acbd.cod-tipo-def.
DEFINE INPUT-OUTPUT PARAMETER c-tp-artigo         AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-tipo-tecelagem    AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER l-excluir-ob        AS LOG FORMAT "Sim/NÆo)".
DEFINE INPUT-OUTPUT PARAMETER c-cam-nom-lista     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER l-ok                AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-data-mov-ini fi-data-mov-fin IMAGE-100 ~
fi-it-codigo-ini IMAGE-101 fi-it-codigo-fin IMAGE-3 IMAGE-4 ~
fi-cod-refer-ini fi-cod-refer-fin IMAGE-5 IMAGE-6 fi-tipo-defeito-ini ~
fi-tipo-defeito-fin IMAGE-90 IMAGE-91 rs-opc-artigo IMAGE-98 tg-excluir-ob ~
IMAGE-99 sl-tp-tecelagem RECT-1 bt-ok RECT-45 bt-cancelar RECT-50 bt-ajuda ~
RECT-59 fi-cod-estabel-ini IMAGE-38 IMAGE-39 fi-cod-estabel-fin 
&Scoped-Define DISPLAYED-OBJECTS fi-data-mov-ini fi-data-mov-fin ~
fi-dt-ant-ini fi-dt-ant-fin fi-it-codigo-ini fi-it-codigo-fin ~
fi-cod-refer-ini fi-cod-refer-fin fi-tipo-defeito-ini fi-tipo-defeito-fin ~
rs-opc-artigo tg-excluir-ob fi-cam-nom-lista sl-tp-tecelagem ~
fi-cod-estabel-ini fi-cod-estabel-fin 

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

DEFINE VARIABLE fi-cam-nom-lista AS CHARACTER FORMAT "X(45)":U 
     LABEL "Lista de Ob's" 
     VIEW-AS FILL-IN 
     SIZE 39 BY .79 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-fin AS CHARACTER FORMAT "x(3)" INITIAL "2" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Estabelecimento final" NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "x(3)" INITIAL "1" 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Estabelecimento inicial" NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referˆncia final." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referˆncia inicial." NO-UNDO.

DEFINE VARIABLE fi-data-mov-fin AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data Final do movimento." NO-UNDO.

DEFINE VARIABLE fi-data-mov-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Movimento" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data inicial do movimento." NO-UNDO.

DEFINE VARIABLE fi-dt-ant-fin AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data Final do Per¡odo Anterior." NO-UNDO.

DEFINE VARIABLE fi-dt-ant-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Per¡odo Anterior" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 TOOLTIP "Data inicial do Periodo Anterior." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item inicial." NO-UNDO.

DEFINE VARIABLE fi-tipo-defeito-fin AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 2 BY .88 TOOLTIP "Corte comercial final." NO-UNDO.

DEFINE VARIABLE fi-tipo-defeito-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Tipo Defeito" 
     VIEW-AS FILL-IN 
     SIZE 2 BY .88 TOOLTIP "Codigo do Tipo de Defeito inicial." NO-UNDO.

DEFINE IMAGE IMAGE-100
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-101
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-39
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-90
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-91
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-98
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-99
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 36 BY .79 TOOLTIP "Tipos de artigos: Indigo, Outros ou Ambos." NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 76.14 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 3.75.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 14.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 1.5.

DEFINE VARIABLE sl-tp-tecelagem AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Nissan","Sulzer","Picanol","Tsudakoma","Toyota" 
     SIZE 15 BY 3.08 NO-UNDO.

DEFINE VARIABLE tg-excluir-ob AS LOGICAL INITIAL no 
     LABEL "Excluir Ob's" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-data-mov-ini AT ROW 2.75 COL 16.86 COLON-ALIGNED
     fi-data-mov-fin AT ROW 2.75 COL 44.86 COLON-ALIGNED NO-LABEL
     fi-dt-ant-ini AT ROW 3.75 COL 16.86 COLON-ALIGNED
     fi-dt-ant-fin AT ROW 3.75 COL 44.86 COLON-ALIGNED NO-LABEL
     fi-it-codigo-ini AT ROW 4.75 COL 16.86 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-it-codigo-fin AT ROW 4.75 COL 44.86 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-cod-refer-ini AT ROW 5.75 COL 16.86 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-cod-refer-fin AT ROW 5.75 COL 44.86 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-tipo-defeito-ini AT ROW 6.75 COL 16.86 COLON-ALIGNED
     fi-tipo-defeito-fin AT ROW 6.75 COL 44.86 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     rs-opc-artigo AT ROW 8.04 COL 18.86 NO-LABEL
     tg-excluir-ob AT ROW 9.42 COL 7.43
     fi-cam-nom-lista AT ROW 9.42 COL 30.86 COLON-ALIGNED
     sl-tp-tecelagem AT ROW 11.63 COL 31.29 NO-LABEL
     bt-ok AT ROW 15.63 COL 2.72
     bt-cancelar AT ROW 15.63 COL 13.72
     bt-ajuda AT ROW 15.63 COL 66
     fi-cod-estabel-ini AT ROW 1.75 COL 16.86 COLON-ALIGNED WIDGET-ID 8
     fi-cod-estabel-fin AT ROW 1.75 COL 44.86 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 9.86 BY .54 AT ROW 8.08 COL 8.72
     "Tipo de Tecelagem" VIEW-AS TEXT
          SIZE 13.57 BY .54 AT ROW 10.83 COL 32.14
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     IMAGE-100 AT ROW 3.75 COL 34.43
     IMAGE-101 AT ROW 3.75 COL 43.43
     IMAGE-3 AT ROW 4.75 COL 34.43
     IMAGE-4 AT ROW 4.75 COL 43.43
     IMAGE-5 AT ROW 5.75 COL 34.43
     IMAGE-6 AT ROW 5.75 COL 43.43
     IMAGE-90 AT ROW 6.75 COL 34.43
     IMAGE-91 AT ROW 6.75 COL 43.43
     IMAGE-98 AT ROW 2.75 COL 34.43
     IMAGE-99 AT ROW 2.75 COL 43.43
     RECT-1 AT ROW 15.42 COL 1.72
     RECT-45 AT ROW 11.25 COL 28.72
     RECT-50 AT ROW 1.25 COL 1.86
     RECT-59 AT ROW 9.13 COL 4
     IMAGE-38 AT ROW 1.75 COL 34.43 WIDGET-ID 10
     IMAGE-39 AT ROW 1.75 COL 43.43 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.92
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
         TITLE              = "Sele‡Æo da Analise de Defeitos na Produ‡Æo"
         HEIGHT             = 15.96
         WIDTH              = 75.43
         MAX-HEIGHT         = 34.5
         MAX-WIDTH          = 205.72
         VIRTUAL-HEIGHT     = 34.5
         VIRTUAL-WIDTH      = 205.72
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 1
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fi-cam-nom-lista IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ant-fin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ant-ini IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Sele‡Æo da Analise de Defeitos na Produ‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Sele‡Æo da Analise de Defeitos na Produ‡Æo */
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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   ASSIGN c-cod-estabel-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
          c-cod-estabel-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fin
          da-data-mov-ini      = INPUT FRAME {&FRAME-NAME} fi-data-mov-ini
          da-data-mov-fin      = INPUT FRAME {&FRAME-NAME} fi-data-mov-fin
          da-dt-ant-ini        = INPUT FRAME {&FRAME-NAME} fi-dt-ant-ini
          da-dt-ant-fin        = INPUT FRAME {&FRAME-NAME} fi-dt-ant-fin
          c-it-codigo-ini      = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
          c-it-codigo-fin      = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin   
          c-cod-refer-ini      = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini   
          c-cod-refer-fin      = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin   
          c-tipo-defeito-ini   = INPUT FRAME {&FRAME-NAME} fi-tipo-defeito-ini
          c-tipo-defeito-fin   = INPUT FRAME {&FRAME-NAME} fi-tipo-defeito-fin
          c-tp-artigo          = INPUT FRAME {&FRAME-NAME} rs-opc-artigo      
          c-tipo-tecelagem     = INPUT FRAME {&FRAME-NAME} sl-tp-tecelagem
          l-excluir-ob         = INPUT FRAME {&FRAME-NAME} tg-excluir-ob
          c-cam-nom-lista      = INPUT FRAME {&FRAME-NAME} fi-cam-nom-lista
          l-ok = YES.  

  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-fin
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-window
ON LEAVE OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-window
ON LEFT-MOUSE-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-data-mov-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-data-mov-ini w-window
ON LEAVE OF fi-data-mov-ini IN FRAME F-Main /* Data Movimento */
DO:
  /* Datas do Periodo Anterior */

  ASSIGN INPUT FRAME {&FRAME-NAME} fi-data-mov-ini.


/*  ASSIGN da-data-mov-ini = INPUT FRAME IN {&FRAME-NAME} da-data-mov-ini. */
  ASSIGN fi-dt-ant-fin = fi-data-mov-ini - 1
         fi-dt-ant-fin:SCREEN-VALUE = STRING(fi-dt-ant-fin)
         fi-dt-ant-ini:SCREEN-VALUE = "01" + STRING(MONTH(fi-dt-ant-fin),'99') +
                                             STRING(YEAR(fi-dt-ant-fin),'9999').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-fin
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-window
ON LEAVE OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     SELF:SCREEN-VALUE <> '5' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo-defeito-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo-defeito-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-tipo-defeito-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es037.w
                     &campo     = fi-tipo-defeito-fin
                     &campozoom = cod-tipo-def}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-tipo-defeito-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo-defeito-ini w-window
ON LEAVE OF fi-tipo-defeito-ini IN FRAME F-Main /* Tipo Defeito */
DO:
  
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-tipo-defeito-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-tipo-defeito-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-tipo-defeito-ini IN FRAME F-Main /* Tipo Defeito */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es037.w
                     &campo     = fi-tipo-defeito-ini
                     &campozoom = cod-tipo-def}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-excluir-ob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-excluir-ob w-window
ON VALUE-CHANGED OF tg-excluir-ob IN FRAME F-Main /* Excluir Ob's */
DO:
   IF INPUT FRAME {&FRAME-NAME} tg-excluir-ob = "yes" THEN
      ASSIGN fi-cam-nom-lista:SCREEN-VALUE = SESSION:TEMP-DIRECTORY + "ob-excluir.csv".
   ELSE
      ASSIGN fi-cam-nom-lista:SCREEN-VALUE = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */
fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-tipo-defeito-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-tipo-defeito-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  DISPLAY fi-data-mov-ini fi-data-mov-fin fi-dt-ant-ini fi-dt-ant-fin 
          fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin 
          fi-tipo-defeito-ini fi-tipo-defeito-fin rs-opc-artigo tg-excluir-ob 
          fi-cam-nom-lista sl-tp-tecelagem fi-cod-estabel-ini fi-cod-estabel-fin 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE fi-data-mov-ini fi-data-mov-fin IMAGE-100 fi-it-codigo-ini IMAGE-101 
         fi-it-codigo-fin IMAGE-3 IMAGE-4 fi-cod-refer-ini fi-cod-refer-fin 
         IMAGE-5 IMAGE-6 fi-tipo-defeito-ini fi-tipo-defeito-fin IMAGE-90 
         IMAGE-91 rs-opc-artigo IMAGE-98 tg-excluir-ob IMAGE-99 sl-tp-tecelagem 
         RECT-1 bt-ok RECT-45 bt-cancelar RECT-50 bt-ajuda RECT-59 
         fi-cod-estabel-ini IMAGE-38 IMAGE-39 fi-cod-estabel-fin 
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

  ASSIGN fi-cod-estabel-ini  = c-cod-estabel-ini
         fi-cod-estabel-fin  = c-cod-estabel-fin
         fi-data-mov-ini     = da-data-mov-ini
         fi-data-mov-fin     = da-data-mov-fin
         fi-dt-ant-ini       = da-dt-ant-ini
         fi-dt-ant-fin       = da-dt-ant-fin
         fi-it-codigo-ini    = c-it-codigo-ini   
         fi-it-codigo-fin    = c-it-codigo-fin    
         fi-cod-refer-ini    = c-cod-refer-ini    
         fi-cod-refer-fin    = c-cod-refer-fin   
         fi-tipo-defeito-ini = c-tipo-defeito-ini
         fi-tipo-defeito-fin = c-tipo-defeito-fin
         rs-opc-artigo       = c-tp-artigo 
         tg-excluir-ob       = l-excluir-ob
         fi-cam-nom-lista    = c-cam-nom-lista
         sl-tp-tecelagem     = c-tipo-tecelagem.
  
/*{utp/ut9000.i "XX9999" "9.99.99.999"} */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO fi-data-mov-ini.

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

