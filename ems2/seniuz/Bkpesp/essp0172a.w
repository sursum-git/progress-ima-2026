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
{include/i-prgvrs.i ESSP0172A 2.04.00.000}

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
DEFINE INPUT-OUTPUT PARAMETER c-periodo           AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini     LIKE ob-etiqueta.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin     LIKE ob-etiqueta.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini     LIKE ob-etiqueta.cod-refer.                              
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin     LIKE ob-etiqueta.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER c-cod-qualid-ini    LIKE ob-etiqueta.cod-qualid. 
DEFINE INPUT-OUTPUT PARAMETER c-cod-qualid-fin    LIKE ob-etiqueta.cod-qualid.
DEFINE INPUT-OUTPUT PARAMETER c-cod-obsoleto-ini  LIKE ref-item-ext.cod-obsoleto. 
DEFINE INPUT-OUTPUT PARAMETER c-cod-obsoleto-fin  LIKE ref-item-ext.cod-obsoleto.
DEFINE INPUT-OUTPUT PARAMETER c-corte-comerc-ini  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT-OUTPUT PARAMETER c-corte-comerc-fin  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT-OUTPUT PARAMETER c-cod-depos         AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER i-tp-artigo         AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-lote-todos        AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-pp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-pd           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-rp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-rd           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-sc           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-ca           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-ok                AS LOG.

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
&Scoped-Define ENABLED-OBJECTS fi-periodo fi-it-codigo-ini fi-it-codigo-fin ~
fi-cod-refer-ini fi-cod-refer-fin fi-cod-qualid-ini fi-cod-qualid-fin ~
fi-cod-obsoleto-ini fi-cod-obsoleto-fin fi-corte-comerc-ini ~
fi-corte-comerc-fin fi-cod-depos tg-lote-todos rs-opc-artigo bt-ok ~
bt-cancelar bt-ajuda IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-90 IMAGE-91 ~
IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 RECT-1 RECT-50 
&Scoped-Define DISPLAYED-OBJECTS fi-periodo fi-it-codigo-ini ~
fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-cod-qualid-ini ~
fi-cod-qualid-fin fi-cod-obsoleto-ini fi-cod-obsoleto-fin ~
fi-corte-comerc-ini fi-corte-comerc-fin fi-cod-depos tg-lote-todos ~
tg-lote-pp tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca ~
rs-opc-artigo 

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

DEFINE VARIABLE fi-cod-depos AS CHARACTER FORMAT "x(3)" 
     LABEL "Dep¢sito":R10 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "C¢digo do dep¢sito." NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-fin AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo obsoleto final." NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Cod.Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo obsoleto inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-qualid-fin AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo de qualidade final." NO-UNDO.

DEFINE VARIABLE fi-cod-qualid-ini AS CHARACTER FORMAT "X(1)" 
     LABEL "Qualidade" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo de qualidade inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referˆncia final." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Referˆncia inicial." NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-fin AS CHARACTER FORMAT "!" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial final." NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-ini AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial inicial." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item final." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Item inicial." NO-UNDO.

DEFINE VARIABLE fi-periodo AS CHARACTER FORMAT "99/9999":U 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Periodo inicial (MM/AAAA)." NO-UNDO.

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

DEFINE IMAGE IMAGE-90
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-91
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-94
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-95
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-96
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-97
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-opc-artigo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", 1,
"Outros", 2,
"Ambos", 3
     SIZE 36 BY .79 TOOLTIP "Tipo de artigo." NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 76.14 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 11.

DEFINE VARIABLE tg-lote-ca AS LOGICAL INITIAL no 
     LABEL "CA" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote (Corte de Amostra)." NO-UNDO.

DEFINE VARIABLE tg-lote-pd AS LOGICAL INITIAL no 
     LABEL "PD" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote PD (Pe‡a Defeituosa)" NO-UNDO.

DEFINE VARIABLE tg-lote-pp AS LOGICAL INITIAL no 
     LABEL "PP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote PP (Pe‡a Perfeita)." NO-UNDO.

DEFINE VARIABLE tg-lote-rd AS LOGICAL INITIAL no 
     LABEL "RD" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RD (Rolo Defeituoso)." NO-UNDO.

DEFINE VARIABLE tg-lote-rp AS LOGICAL INITIAL no 
     LABEL "RP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RP (Rolo Perfeito)." NO-UNDO.

DEFINE VARIABLE tg-lote-sc AS LOGICAL INITIAL no 
     LABEL "SC" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote SC (Saco)." NO-UNDO.

DEFINE VARIABLE tg-lote-todos AS LOGICAL INITIAL yes 
     LABEL "TODOS" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 TOOLTIP "Todas as qualidades (PP, PD, RP e RD)." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-periodo AT ROW 1.75 COL 14 COLON-ALIGNED
     fi-it-codigo-ini AT ROW 2.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-it-codigo-fin AT ROW 2.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-cod-refer-ini AT ROW 3.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-cod-refer-fin AT ROW 3.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-cod-qualid-ini AT ROW 4.75 COL 14 COLON-ALIGNED
     fi-cod-qualid-fin AT ROW 4.75 COL 42 COLON-ALIGNED NO-LABEL
     fi-cod-obsoleto-ini AT ROW 5.75 COL 14 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 5.75 COL 42 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-corte-comerc-ini AT ROW 6.75 COL 14 COLON-ALIGNED
     fi-corte-comerc-fin AT ROW 6.75 COL 42 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     fi-cod-depos AT ROW 8.75 COL 14 COLON-ALIGNED HELP
          "C¢digo do Dep¢sito"
     tg-lote-todos AT ROW 9.83 COL 16.14
     tg-lote-pp AT ROW 9.83 COL 28
     tg-lote-pd AT ROW 9.83 COL 34
     tg-lote-rp AT ROW 9.83 COL 40
     tg-lote-rd AT ROW 9.83 COL 46
     tg-lote-sc AT ROW 9.83 COL 52
     tg-lote-ca AT ROW 9.83 COL 58
     rs-opc-artigo AT ROW 10.83 COL 16 NO-LABEL
     bt-ok AT ROW 12.71 COL 2
     bt-cancelar AT ROW 12.71 COL 13
     bt-ajuda AT ROW 12.71 COL 65.29
     IMAGE-3 AT ROW 2.75 COL 31.57
     IMAGE-4 AT ROW 2.75 COL 40.57
     IMAGE-5 AT ROW 3.75 COL 31.57
     IMAGE-6 AT ROW 3.75 COL 40.57
     IMAGE-90 AT ROW 4.75 COL 31.57
     IMAGE-91 AT ROW 4.75 COL 40.57
     IMAGE-94 AT ROW 5.75 COL 31.57
     IMAGE-95 AT ROW 5.75 COL 40.57
     IMAGE-96 AT ROW 6.75 COL 31.57
     IMAGE-97 AT ROW 6.75 COL 40.57
     RECT-1 AT ROW 12.5 COL 1
     RECT-50 AT ROW 1.25 COL 1.86
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 9.86 BY .54 AT ROW 10.88 COL 5.86
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 9.96 COL 12.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.21
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
         TITLE              = "Sele‡Æo da Evolu‡Æo do Estoque Acabado"
         HEIGHT             = 13
         WIDTH              = 75.57
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
                                                                        */
/* SETTINGS FOR TOGGLE-BOX tg-lote-ca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-pd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-pp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-rd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-rp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-sc IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Sele‡Æo da Evolu‡Æo do Estoque Acabado */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Sele‡Æo da Evolu‡Æo do Estoque Acabado */
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
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-periodo.
    IF INT(SUBSTR(fi-periodo,1,2)) <  1 OR
       INT(SUBSTR(fi-periodo,1,2)) > 12  THEN DO:
        MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
        APPLY 'entry' TO fi-periodo.
        RETURN NO-APPLY.
    END.
    IF INT(SUBSTR(fi-periodo,4,4)) <  1 THEN DO:
        MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
        APPLY 'entry' TO fi-periodo.
        RETURN NO-APPLY.
    END.
    IF INPUT FRAME {&FRAME-NAME} tg-lote-pp = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-pd = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-rp = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-rd = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-sc = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-ca = 'NO' THEN
       ASSIGN tg-lote-todos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.
    ASSIGN c-periodo          = INPUT FRAME {&FRAME-NAME} fi-periodo   
           c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
           c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin   
           c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini   
           c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin   
           c-cod-qualid-ini   = INPUT FRAME {&FRAME-NAME} fi-cod-qualid-ini
           c-cod-qualid-fin   = INPUT FRAME {&FRAME-NAME} fi-cod-qualid-fin
           c-cod-obsoleto-ini = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-ini
           c-cod-obsoleto-fin = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-fin
           c-corte-comerc-ini = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-ini
           c-corte-comerc-fin = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-fin
           c-cod-depos        = INPUT FRAME {&FRAME-NAME} fi-cod-depos
           i-tp-artigo        = INPUT FRAME {&FRAME-NAME} rs-opc-artigo      
           l-lote-todos       = INPUT FRAME {&FRAME-NAME} tg-lote-todos
           l-lote-pp          = INPUT FRAME {&FRAME-NAME} tg-lote-pp
           l-lote-pd          = INPUT FRAME {&FRAME-NAME} tg-lote-pd
           l-lote-rp          = INPUT FRAME {&FRAME-NAME} tg-lote-rp
           l-lote-rd          = INPUT FRAME {&FRAME-NAME} tg-lote-rd
           l-lote-sc          = INPUT FRAME {&FRAME-NAME} tg-lote-sc
           l-lote-ca          = INPUT FRAME {&FRAME-NAME} tg-lote-ca
           l-ok = YES.  

    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-obsoleto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-obsoleto-ini w-window
ON LEAVE OF fi-cod-obsoleto-ini IN FRAME F-Main /* Cod.Obsoleto */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-obsoleto-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-qualid-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-qualid-ini w-window
ON LEAVE OF fi-cod-qualid-ini IN FRAME F-Main /* Qualidade */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN
      ASSIGN fi-cod-qualid-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
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


&Scoped-define SELF-NAME fi-corte-comerc-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-fin w-window
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-fin IN FRAME F-Main
DO:
   {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                      &campo     = fi-corte-comerc-fin
                      &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini w-window
ON LEAVE OF fi-corte-comerc-ini IN FRAME F-Main /* Corte Comercial */
DO:
  IF SELF:SCREEN-VALUE = '' THEN DO:
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
  ELSE
     ASSIGN fi-corte-comerc-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini w-window
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-ini IN FRAME F-Main /* Corte Comercial */
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-ini
                       &campozoom = codigo}
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


&Scoped-define SELF-NAME tg-lote-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-lote-todos w-window
ON VALUE-CHANGED OF tg-lote-todos IN FRAME F-Main /* TODOS */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-lote-todos = NO THEN
     ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-pp)
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-rp).
  ELSE
      ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
             tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
             l-lote-pp = YES
             l-lote-rp = YES.
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
fi-corte-comerc-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-corte-comerc-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  DISPLAY fi-periodo fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini 
          fi-cod-refer-fin fi-cod-qualid-ini fi-cod-qualid-fin 
          fi-cod-obsoleto-ini fi-cod-obsoleto-fin fi-corte-comerc-ini 
          fi-corte-comerc-fin fi-cod-depos tg-lote-todos tg-lote-pp tg-lote-pd 
          tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca rs-opc-artigo 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE fi-periodo fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini 
         fi-cod-refer-fin fi-cod-qualid-ini fi-cod-qualid-fin 
         fi-cod-obsoleto-ini fi-cod-obsoleto-fin fi-corte-comerc-ini 
         fi-corte-comerc-fin fi-cod-depos tg-lote-todos rs-opc-artigo bt-ok 
         bt-cancelar bt-ajuda IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-90 IMAGE-91 
         IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 RECT-1 RECT-50 
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

  ASSIGN fi-periodo          = c-periodo   
         fi-it-codigo-ini    = c-it-codigo-ini   
         fi-it-codigo-fin    = c-it-codigo-fin    
         fi-cod-refer-ini    = c-cod-refer-ini    
         fi-cod-refer-fin    = c-cod-refer-fin   
         fi-cod-qualid-ini   = c-cod-qualid-ini
         fi-cod-qualid-fin   = c-cod-qualid-fin
         fi-cod-obsoleto-ini = c-cod-obsoleto-ini
         fi-cod-obsoleto-fin = c-cod-obsoleto-fin
         fi-corte-comerc-ini = c-corte-comerc-ini
         fi-corte-comerc-fin = c-corte-comerc-fin
         fi-cod-depos        = c-cod-depos       
         rs-opc-artigo       = i-tp-artigo 
         tg-lote-todos       = l-lote-todos
         tg-lote-pp          = l-lote-pp
         tg-lote-pd          = l-lote-pd
         tg-lote-rp          = l-lote-rp
         tg-lote-rd          = l-lote-rd
         tg-lote-sc          = l-lote-sc
         tg-lote-ca          = l-lote-ca.
  
/*{utp/ut9000.i "XX9999" "9.99.99.999"} */

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

