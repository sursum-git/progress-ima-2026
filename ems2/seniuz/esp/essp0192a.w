&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
** AUTOR: FµBIO COELHO LANZA - OUTUBRO 2009.
*******************************************************************************/
{include/i-prgvrs.i ESSP0192A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel-ini LIKE ordem-benefic.cod-estabel.
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel-fin LIKE ordem-benefic.cod-estabel.
DEFINE INPUT-OUTPUT PARAMETER da-dt-movto-ini    LIKE ordem-benefic.dt-ob.
DEFINE INPUT-OUTPUT PARAMETER da-dt-movto-fin    LIKE ordem-benefic.dt-ob.
DEFINE INPUT-OUTPUT PARAMETER i-nr-ob-ini        LIKE ordem-benefic.nr-ob.
DEFINE INPUT-OUTPUT PARAMETER i-nr-ob-fin        LIKE ordem-benefic.nr-ob.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini    LIKE ob-etiqueta.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin    LIKE ob-etiqueta.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini    LIKE ob-etiqueta.cod-refer. 
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin    LIKE ob-etiqueta.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER l-todas-obs    AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-producao     AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-retrabalho   AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-transf       AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-indust       AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-todos-status AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-disponivel   AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-em-revisao   AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-rev-parcial  AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-rev-total    AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-reportado    AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-pendente     AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-executada    AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-ok           AS LOG.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-88 ~
IMAGE-89 IMAGE-90 IMAGE-91 RECT-1 RECT-50 RECT-51 IMAGE-92 IMAGE-93 ~
fi-cod-estabel-ini fi-cod-estabel-fin fi-dt-movto-ini fi-dt-movto-fin ~
fi-nr-ob-ini fi-nr-ob-fin fi-it-codigo-ini fi-it-codigo-fin ~
fi-cod-refer-ini fi-cod-refer-fin tg-todas-obs tg-todos-status bt-ajuda ~
bt-ok bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel-ini fi-cod-estabel-fin ~
fi-dt-movto-ini fi-dt-movto-fin fi-nr-ob-ini fi-nr-ob-fin fi-it-codigo-ini ~
fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin tg-todas-obs tg-producao ~
tg-todos-status tg-Disponivel tg-retrabalho tg-em-revisao tg-indust ~
tg-rev-parcial tg-transf tg-rev-total tg-pendente tg-reportado tg-executada 

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

DEFINE VARIABLE fi-cod-estabel-fin AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Referància final." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Referància inicial." NO-UNDO.

DEFINE VARIABLE fi-dt-movto-fin AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de Abertura da OB [DD/MM/AAAA]" NO-UNDO.

DEFINE VARIABLE fi-dt-movto-ini AS DATE FORMAT "99/99/9999" 
     LABEL "Data Movimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de Abertura da OB [DD/MM/AAAA]" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 TOOLTIP "Item inicial." NO-UNDO.

DEFINE VARIABLE fi-nr-ob-fin AS INTEGER FORMAT ">>>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-ob-ini AS INTEGER FORMAT "->>>,>>>,>>9" INITIAL 0 
     LABEL "N£mero da OB" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
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

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-90
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-91
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-92
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-93
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 74 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 11.75.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 5.83.

DEFINE VARIABLE tg-Disponivel AS LOGICAL INITIAL no 
     LABEL "Disponivel" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .88 TOOLTIP "Apenas as OBs de Status (Disponivel)." NO-UNDO.

DEFINE VARIABLE tg-em-revisao AS LOGICAL INITIAL no 
     LABEL "Em Revisao" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .88 TOOLTIP "Apenas as OBs de Status (Em Revis∆o)" NO-UNDO.

DEFINE VARIABLE tg-executada AS LOGICAL INITIAL no 
     LABEL "Executada" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 TOOLTIP "Apenas as OBs com Transformaá∆o executada" NO-UNDO.

DEFINE VARIABLE tg-indust AS LOGICAL INITIAL no 
     LABEL "Industrializaá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 TOOLTIP "Apenas as OBs  (Industrializaá∆o)." NO-UNDO.

DEFINE VARIABLE tg-pendente AS LOGICAL INITIAL no 
     LABEL "Pendente" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 TOOLTIP "Apenas as OBs com Transformaá∆o pendentes" NO-UNDO.

DEFINE VARIABLE tg-producao AS LOGICAL INITIAL no 
     LABEL "Produá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .88 TOOLTIP "Apenas as OBs de (Produá∆o)." NO-UNDO.

DEFINE VARIABLE tg-reportado AS LOGICAL INITIAL no 
     LABEL "Reportado" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 TOOLTIP "Apenas as OBs  de Status (Reportado)." NO-UNDO.

DEFINE VARIABLE tg-retrabalho AS LOGICAL INITIAL no 
     LABEL "Retrabalho" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .88 TOOLTIP "Apenas as OBs (Retrabalho)" NO-UNDO.

DEFINE VARIABLE tg-rev-parcial AS LOGICAL INITIAL no 
     LABEL "Revis∆o Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.57 BY .88 TOOLTIP "Apenas as OBs  de Status (Revis∆o Parcial)." NO-UNDO.

DEFINE VARIABLE tg-rev-total AS LOGICAL INITIAL no 
     LABEL "Revis∆o Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 TOOLTIP "Apenas as OBs  de Status (Revis∆o Total)." NO-UNDO.

DEFINE VARIABLE tg-todas-obs AS LOGICAL INITIAL no 
     LABEL "TODAS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .88 TOOLTIP "Todas as OBs (Produá∆o, Retrabalho, Transformaá∆o)" NO-UNDO.

DEFINE VARIABLE tg-todos-status AS LOGICAL INITIAL no 
     LABEL "TODOS" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .88 TOOLTIP "Todos Status OBs (Disponivel, Em Revis∆o, Rev. Parcial, Rev.Total, Reportado]" NO-UNDO.

DEFINE VARIABLE tg-transf AS LOGICAL INITIAL no 
     LABEL "Transformaá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 TOOLTIP "Apenas as OBs  (Transformaá∆o)." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cod-estabel-ini AT ROW 1.75 COL 20 COLON-ALIGNED WIDGET-ID 2
     fi-cod-estabel-fin AT ROW 1.75 COL 49 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-dt-movto-ini AT ROW 2.75 COL 20 COLON-ALIGNED
     fi-dt-movto-fin AT ROW 2.75 COL 49 COLON-ALIGNED NO-LABEL
     fi-nr-ob-ini AT ROW 3.75 COL 20 COLON-ALIGNED
     fi-nr-ob-fin AT ROW 3.75 COL 49 COLON-ALIGNED NO-LABEL
     fi-it-codigo-ini AT ROW 4.75 COL 20 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS"
     fi-it-codigo-fin AT ROW 4.75 COL 49 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL
     fi-cod-refer-ini AT ROW 5.75 COL 20 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS"
     fi-cod-refer-fin AT ROW 5.75 COL 49 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS" NO-LABEL
     tg-todas-obs AT ROW 7.08 COL 13.72
     tg-producao AT ROW 7.08 COL 22.72
     tg-todos-status AT ROW 7.08 COL 48.29
     tg-Disponivel AT ROW 7.08 COL 58.72
     tg-retrabalho AT ROW 8.08 COL 22.71
     tg-em-revisao AT ROW 8.08 COL 58.72
     tg-indust AT ROW 9 COL 22.71 WIDGET-ID 16
     tg-rev-parcial AT ROW 9.08 COL 58.72
     tg-transf AT ROW 10 COL 22.72
     tg-rev-total AT ROW 10.08 COL 58.72
     tg-pendente AT ROW 10.75 COL 39.86 WIDGET-ID 10
     tg-reportado AT ROW 11.08 COL 58.72
     tg-executada AT ROW 11.58 COL 39.86 WIDGET-ID 12
     bt-ajuda AT ROW 13.29 COL 65.14
     bt-ok AT ROW 13.33 COL 3.14
     bt-cancelar AT ROW 13.33 COL 14.14
     "Tipo da OB:" VIEW-AS TEXT
          SIZE 8.57 BY .54 AT ROW 7.17 COL 5.43
     "Status da OB:" VIEW-AS TEXT
          SIZE 9.57 BY .54 AT ROW 7.17 COL 38.57
     "Tipo da Transformacao:" VIEW-AS TEXT
          SIZE 16.57 BY .54 AT ROW 11.21 COL 22.72 WIDGET-ID 14
     " Seleá∆o" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     IMAGE-3 AT ROW 4.75 COL 39
     IMAGE-4 AT ROW 4.75 COL 48
     IMAGE-5 AT ROW 5.75 COL 39
     IMAGE-6 AT ROW 5.75 COL 48
     IMAGE-88 AT ROW 2.75 COL 39
     IMAGE-89 AT ROW 2.75 COL 48
     IMAGE-90 AT ROW 3.75 COL 39
     IMAGE-91 AT ROW 3.75 COL 48
     RECT-1 AT ROW 13.13 COL 1.86
     RECT-50 AT ROW 1.25 COL 2
     RECT-51 AT ROW 6.75 COL 3.86
     IMAGE-92 AT ROW 1.75 COL 39 WIDGET-ID 6
     IMAGE-93 AT ROW 1.75 COL 48 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.14 ROW 1
         SIZE 79.86 BY 13.79
         FONT 1.


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
         TITLE              = "Seleá∆o da Analise Gerencial da PRODUÄ«O X ESTOQUE"
         COLUMN             = 15.14
         ROW                = 8.58
         HEIGHT             = 13.67
         WIDTH              = 75.43
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
/* SETTINGS FOR TOGGLE-BOX tg-Disponivel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-em-revisao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-executada IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-indust IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-pendente IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-producao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-reportado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-retrabalho IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-rev-parcial IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-rev-total IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-transf IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Seleá∆o da Analise Gerencial da PRODUÄ«O X ESTOQUE */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Seleá∆o da Analise Gerencial da PRODUÄ«O X ESTOQUE */
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
  ASSIGN l-ok = NO.
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  IF tg-transf:SCREEN-VALUE = "YES" AND
     tg-pendente:SCREEN-VALUE = "NO" AND
     tg-executada:SCREEN-VALUE = "NO" THEN DO:
     MESSAGE "Favor marcar uma opá∆o do TIPO DA TRANSFORMAÄ«O ! ! "
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'ENTRY' TO tg-pendente.
     RETURN NO-APPLY.
  END.

  IF tg-todas-obs:SCREEN-VALUE  = "NO" AND
     tg-producao:SCREEN-VALUE   = "NO" AND
     tg-retrabalho:SCREEN-VALUE = "NO" AND
     tg-indust:SCREEN-VALUE     = "NO" AND
     tg-transf:SCREEN-VALUE     = "NO" THEN DO:
     MESSAGE "Favor marcar uma opá∆o do TIPO OB ! ! "
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'ENTRY' TO tg-todas-obs.
     RETURN NO-APPLY.
  END.

  IF tg-todos-status:SCREEN-VALUE = "NO" AND
     tg-disponivel:SCREEN-VALUE   = "NO" AND
     tg-em-revisao:SCREEN-VALUE   = "NO" AND
     tg-rev-parcial:SCREEN-VALUE  = "NO" AND 
     tg-rev-total:SCREEN-VALUE    = "NO" AND
     tg-reportado:SCREEN-VALUE    = "NO"  THEN DO:
     MESSAGE "Favor marcar uma opá∆o do STATUS OB ! ! "
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'ENTRY' TO tg-todos-status.
     RETURN NO-APPLY.
  END.
  ASSIGN c-cod-estabel-ini = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
         c-cod-estabel-fin = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fin
         da-dt-movto-ini   = INPUT FRAME {&FRAME-NAME} fi-dt-movto-ini   
         da-dt-movto-fin   = INPUT FRAME {&FRAME-NAME} fi-dt-movto-fin   
         i-nr-ob-ini       = INPUT FRAME {&FRAME-NAME} fi-nr-ob-ini
         i-nr-ob-fin       = INPUT FRAME {&FRAME-NAME} fi-nr-ob-fin
         c-it-codigo-ini   = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
         c-it-codigo-fin   = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin   
         c-cod-refer-ini   = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini
         c-cod-refer-fin   = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin
         l-todas-obs       = INPUT FRAME {&FRAME-NAME} tg-todas-obs     
         l-producao        = INPUT FRAME {&FRAME-NAME} tg-producao     
         l-retrabalho      = INPUT FRAME {&FRAME-NAME} tg-retrabalho   
         l-transf          = INPUT FRAME {&FRAME-NAME} tg-transf  
         l-indust          = INPUT FRAME {&FRAME-NAME} tg-indust
         l-todos-status    = INPUT FRAME {&FRAME-NAME} tg-todos-status 
         l-disponivel      = INPUT FRAME {&FRAME-NAME} tg-disponivel   
         l-em-revisao      = INPUT FRAME {&FRAME-NAME} tg-em-revisao   
         l-rev-parcial     = INPUT FRAME {&FRAME-NAME} tg-rev-parcial  
         l-rev-total       = INPUT FRAME {&FRAME-NAME} tg-rev-total    
         l-reportado       = INPUT FRAME {&FRAME-NAME} tg-reportado  
         l-pendente        = INPUT FRAME {&FRAME-NAME} tg-pendente
         l-executada       = INPUT FRAME {&FRAME-NAME} tg-executada
         l-ok              = YES.  

  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-fin
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-digita
ON LEAVE OF fi-cod-refer-ini IN FRAME F-Main /* Referància */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-digita
ON LEFT-MOUSE-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referància */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-fin
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-digita
ON LEAVE OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     SELF:SCREEN-VALUE <> '5' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-todas-obs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-todas-obs w-digita
ON VALUE-CHANGED OF tg-todas-obs IN FRAME F-Main /* TODAS */
DO:
  ASSIGN tg-producao:SENSITIVE IN FRAME {&FRAME-NAME}   = YES  
         tg-retrabalho:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
         tg-transf:SENSITIVE IN FRAME {&FRAME-NAME}     = YES
         tg-indust:SENSITIVE IN FRAME {&FRAME-NAME}     = YES. 
  IF INPUT FRAME {&FRAME-NAME} tg-todas-obs = YES THEN
     ASSIGN tg-producao:SENSITIVE IN FRAME {&FRAME-NAME}   = NO  
            tg-retrabalho:SENSITIVE IN FRAME {&FRAME-NAME} = NO  
            tg-transf:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
            tg-indust:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
            tg-pendente:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
            tg-executada:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
            tg-pendente:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "YES"
            tg-executada:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "YES"
            tg-producao:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "NO"
            tg-retrabalho:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
            tg-transf:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = "NO"
            tg-indust:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = "NO".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-todos-status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-todos-status w-digita
ON VALUE-CHANGED OF tg-todos-status IN FRAME F-Main /* TODOS */
DO:
  ASSIGN tg-disponivel:SENSITIVE IN FRAME {&FRAME-NAME}  = YES  
         tg-em-revisao:SENSITIVE IN FRAME {&FRAME-NAME}  = YES 
         tg-rev-parcial:SENSITIVE IN FRAME {&FRAME-NAME} = YES  
         tg-rev-total:SENSITIVE IN FRAME {&FRAME-NAME}   = YES 
         tg-reportado:SENSITIVE IN FRAME {&FRAME-NAME}   = YES. 
  IF INPUT FRAME {&FRAME-NAME} tg-todos-status = YES THEN
      ASSIGN tg-disponivel:SENSITIVE IN FRAME {&FRAME-NAME}  = NO  
             tg-em-revisao:SENSITIVE IN FRAME {&FRAME-NAME}  = NO  
             tg-rev-parcial:SENSITIVE IN FRAME {&FRAME-NAME} = NO   
             tg-rev-total:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
             tg-reportado:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
             tg-disponivel:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "NO"
             tg-em-revisao:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "NO"
             tg-rev-parcial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
             tg-rev-total:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "NO"
             tg-reportado:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "NO".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-transf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-transf w-digita
ON VALUE-CHANGED OF tg-transf IN FRAME F-Main /* Transformaá∆o */
DO:
  ASSIGN tg-pendente:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "YES"
         tg-executada:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "YES".

  IF INPUT FRAME {&FRAME-NAME} tg-transf = YES THEN
     ASSIGN tg-pendente:SENSITIVE IN FRAME {&FRAME-NAME}   = YES
            tg-executada:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
  ELSE
     ASSIGN tg-pendente:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
            tg-executada:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */

fi-cod-estabel-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-cod-estabel-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

/* Substitui TAB por ENTER */
ON 'RETURN':U ANYWHERE DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
END.

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
  DISPLAY fi-cod-estabel-ini fi-cod-estabel-fin fi-dt-movto-ini fi-dt-movto-fin 
          fi-nr-ob-ini fi-nr-ob-fin fi-it-codigo-ini fi-it-codigo-fin 
          fi-cod-refer-ini fi-cod-refer-fin tg-todas-obs tg-producao 
          tg-todos-status tg-Disponivel tg-retrabalho tg-em-revisao tg-indust 
          tg-rev-parcial tg-transf tg-rev-total tg-pendente tg-reportado 
          tg-executada 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-88 IMAGE-89 IMAGE-90 IMAGE-91 
         RECT-1 RECT-50 RECT-51 IMAGE-92 IMAGE-93 fi-cod-estabel-ini 
         fi-cod-estabel-fin fi-dt-movto-ini fi-dt-movto-fin fi-nr-ob-ini 
         fi-nr-ob-fin fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini 
         fi-cod-refer-fin tg-todas-obs tg-todos-status bt-ajuda bt-ok 
         bt-cancelar 
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
  ASSIGN fi-cod-estabel-ini = c-cod-estabel-ini
         fi-cod-estabel-fin = c-cod-estabel-fin
         fi-dt-movto-ini    = da-dt-movto-ini   
         fi-dt-movto-fin    = da-dt-movto-fin
         fi-nr-ob-ini       = i-nr-ob-ini
         fi-nr-ob-fin       = i-nr-ob-fin
         fi-it-codigo-ini   = c-it-codigo-ini   
         fi-it-codigo-fin   = c-it-codigo-fin   
         fi-cod-refer-ini   = c-cod-refer-ini
         fi-cod-refer-fin   = c-cod-refer-fin
         tg-todas-obs       = l-todas-obs    
         tg-producao        = l-producao     
         tg-retrabalho      = l-retrabalho   
         tg-transf          = l-transf
         tg-indust          = l-indust
         tg-todos-status    = l-todos-status 
         tg-disponivel      = l-disponivel   
         tg-em-revisao      = l-em-revisao   
         tg-rev-parcial     = l-rev-parcial  
         tg-rev-total       = l-rev-total    
         tg-reportado       = l-reportado
         tg-pendente        = l-pendente
         tg-executada       = l-executada.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN tg-producao:SENSITIVE IN FRAME {&FRAME-NAME}   = YES  
         tg-retrabalho:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         tg-indust:SENSITIVE IN FRAME {&FRAME-NAME}     = YES
         tg-transf:SENSITIVE IN FRAME {&FRAME-NAME}     = YES.  
  IF l-todas-obs = YES THEN
     ASSIGN tg-producao:SENSITIVE IN FRAME {&FRAME-NAME}   = NO  
            tg-retrabalho:SENSITIVE IN FRAME {&FRAME-NAME} = NO  
            tg-transf:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
            tg-indust:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
            tg-pendente:SENSITIVE IN FRAME {&FRAME-NAME}   = NO
            tg-executada:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
            tg-pendente:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "YES"
            tg-executada:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "YES"
            tg-producao:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "NO"
            tg-retrabalho:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
            tg-transf:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = "NO"
            tg-indust:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = "NO".

  ASSIGN tg-disponivel:SENSITIVE IN FRAME {&FRAME-NAME}  = YES  
         tg-em-revisao:SENSITIVE IN FRAME {&FRAME-NAME}  = YES 
         tg-rev-parcial:SENSITIVE IN FRAME {&FRAME-NAME} = YES  
         tg-rev-total:SENSITIVE IN FRAME {&FRAME-NAME}   = YES 
         tg-reportado:SENSITIVE IN FRAME {&FRAME-NAME}   = YES. 
  IF l-todos-status = YES THEN
     ASSIGN tg-disponivel:SENSITIVE IN FRAME {&FRAME-NAME}  = NO  
            tg-em-revisao:SENSITIVE IN FRAME {&FRAME-NAME}  = NO  
            tg-rev-parcial:SENSITIVE IN FRAME {&FRAME-NAME} = NO   
            tg-rev-total:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
            tg-reportado:SENSITIVE IN FRAME {&FRAME-NAME}   = NO 
            tg-disponivel:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "NO"
            tg-em-revisao:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = "NO"
            tg-rev-parcial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO"
            tg-rev-total:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "NO"
            tg-reportado:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = "NO".

  IF l-transf = YES THEN
     ASSIGN tg-pendente:SENSITIVE IN FRAME {&FRAME-NAME}   = YES 
            tg-executada:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.


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

