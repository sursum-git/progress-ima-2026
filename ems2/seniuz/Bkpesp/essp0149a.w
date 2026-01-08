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
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF INPUT-OUTPUT PARAMETER c-cod-estabel      AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-dt-limite        AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-it-codigo-ini    AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-it-codigo-fin    AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-refer-ini    AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-refer-fin    AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-cod-qualid-ini   AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-qualid-fin   AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-cod-obsoleto-ini AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-obsoleto-fin AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-cod-depos        AS CHAR.
DEF INPUT-OUTPUT PARAMETER c-tp-artigo        AS CHAR.
DEF INPUT-OUTPUT PARAMETER l-lote-todos       AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-pp          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-pd          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-rp          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-rd          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-sc          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-lote-ca          AS LOG.
DEF INPUT-OUTPUT PARAMETER l-ok               AS LOG.

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
&Scoped-Define ENABLED-OBJECTS IMAGE-2 IMAGE-51 IMAGE-52 IMAGE-73 IMAGE-75 ~
IMAGE-77 IMAGE-82 IMAGE-83 RECT-1 RECT-49 fi-cod-estabel fi-dt-limite ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
fi-cod-qualid-ini fi-cod-qualid-fin fi-cod-obsoleto-ini fi-cod-obsoleto-fin ~
fi-cod-depos tg-lote-todos rs-opc-artigo bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel fi-nome-estabel ~
fi-dt-limite fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-cod-qualid-ini fi-cod-qualid-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin fi-cod-depos tg-lote-todos tg-lote-pp tg-lote-pd ~
tg-lote-rp tg-lote-rd tg-lote-sc tg-lote-ca rs-opc-artigo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin 
&Scoped-define List-5 fi-cod-estabel 

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
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-fin AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Codigo Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-qualid-fin AS CHARACTER FORMAT "X(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo de qualidade final." NO-UNDO.

DEFINE VARIABLE fi-cod-qualid-ini AS CHARACTER FORMAT "X(1)" 
     LABEL "Qualidade" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo de qualidade inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-limite AS CHARACTER FORMAT "99/9999":U 
     LABEL "Data Limite" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-51
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-52
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-73
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-75
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-77
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-82
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-83
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 36.29 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 67 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66.86 BY 9.54.

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
     fi-cod-estabel AT ROW 2 COL 15.43 COLON-ALIGNED WIDGET-ID 6
     fi-nome-estabel AT ROW 2 COL 20.14 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-dt-limite AT ROW 3 COL 15.43 COLON-ALIGNED
     fi-it-codigo-ini AT ROW 4 COL 15.43 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 4 COL 45.43 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 5 COL 15.43 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 5 COL 45.43 COLON-ALIGNED NO-LABEL
     fi-cod-qualid-ini AT ROW 6 COL 15.43 COLON-ALIGNED
     fi-cod-qualid-fin AT ROW 6 COL 45.43 COLON-ALIGNED NO-LABEL
     fi-cod-obsoleto-ini AT ROW 7 COL 15.43 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 7 COL 45.43 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-cod-depos AT ROW 8 COL 15.43 COLON-ALIGNED HELP
          "C¢digo do Dep¢sito"
     tg-lote-todos AT ROW 9 COL 17.43
     tg-lote-pp AT ROW 9 COL 29.29
     tg-lote-pd AT ROW 9 COL 35.29
     tg-lote-rp AT ROW 9 COL 41.29
     tg-lote-rd AT ROW 9 COL 47.29
     tg-lote-sc AT ROW 9 COL 53.29
     tg-lote-ca AT ROW 9 COL 59.29
     rs-opc-artigo AT ROW 10 COL 17.29 NO-LABEL
     bt-ok AT ROW 11.42 COL 3
     bt-cancelar AT ROW 11.42 COL 14
     bt-ajuda AT ROW 11.42 COL 57.57
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 8.72 BY .54 AT ROW 1.17 COL 4.29
          FGCOLOR 9 FONT 6
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10.57 BY 1 AT ROW 9.88 COL 7
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 9.13 COL 13.57
     IMAGE-2 AT ROW 4 COL 43.43
     IMAGE-51 AT ROW 5 COL 34.43
     IMAGE-52 AT ROW 5 COL 43.43
     IMAGE-73 AT ROW 4 COL 34.43
     IMAGE-75 AT ROW 6 COL 34.43
     IMAGE-77 AT ROW 7 COL 34.43
     IMAGE-82 AT ROW 6 COL 43.43
     IMAGE-83 AT ROW 7 COL 43.43
     RECT-1 AT ROW 11.21 COL 2
     RECT-49 AT ROW 1.46 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 11.58
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
         TITLE              = "Parƒmetros e Sele‡Æo da Analise Transforma‡Æo - ESSP0149a"
         HEIGHT             = 11.71
         WIDTH              = 68.72
         MAX-HEIGHT         = 29.13
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.13
         VIRTUAL-WIDTH      = 146.29
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
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-fin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-ini IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME F-Main
   NO-ENABLE                                                            */
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
ON END-ERROR OF w-window /* Parƒmetros e Sele‡Æo da Analise Transforma‡Æo - ESSP0149a */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Parƒmetros e Sele‡Æo da Analise Transforma‡Æo - ESSP0149a */
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
    IF INPUT FRAME {&FRAME-NAME} tg-lote-pp = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-pd = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-rp = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-rd = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-sc = 'NO' AND
       INPUT FRAME {&FRAME-NAME} tg-lote-ca = 'NO' THEN
       ASSIGN tg-lote-todos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.

    ASSIGN c-cod-estabel      = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
           c-dt-limite        = INPUT FRAME {&FRAME-NAME} fi-dt-limite
           c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini
           c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin
           c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini
           c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin
           c-cod-qualid-ini   = INPUT FRAME {&FRAME-NAME} fi-cod-qualid-ini
           c-cod-qualid-fin   = INPUT FRAME {&FRAME-NAME} fi-cod-qualid-fin
           c-cod-obsoleto-ini = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-ini
           c-cod-obsoleto-fin = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-fin
           c-cod-depos        = INPUT FRAME {&FRAME-NAME} fi-cod-depos
           l-lote-todos       = INPUT FRAME {&FRAME-NAME} tg-lote-todos
           l-lote-pp          = INPUT FRAME {&FRAME-NAME} tg-lote-pp
           l-lote-pd          = INPUT FRAME {&FRAME-NAME} tg-lote-pd
           l-lote-rp          = INPUT FRAME {&FRAME-NAME} tg-lote-rp
           l-lote-rd          = INPUT FRAME {&FRAME-NAME} tg-lote-rd
           l-lote-sc          = INPUT FRAME {&FRAME-NAME} tg-lote-sc
           l-lote-ca          = INPUT FRAME {&FRAME-NAME} tg-lote-ca
           c-tp-artigo        = INPUT FRAME {&FRAME-NAME} rs-opc-artigo
           l-ok               = YES.

  APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-window
ON LEAVE OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE 'Estabelecimento nÆo Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
     ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-window
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad107.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-obsoleto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-obsoleto-ini w-window
ON LEAVE OF fi-cod-obsoleto-ini IN FRAME F-Main /* Codigo Obsoleto */
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
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-limite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite w-window
ON LEAVE OF fi-dt-limite IN FRAME F-Main /* Data Limite */
DO:
  IF INT(SUBSTR(SELF:SCREEN-VALUE,1,2)) <  1 OR
     INT(SUBSTR(SELF:SCREEN-VALUE,1,2)) > 12  THEN DO:
      MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
  END.
  IF INT(SUBSTR(SELF:SCREEN-VALUE,4,4)) <  1 THEN DO:
      MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
  END.

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
  DISPLAY fi-cod-estabel fi-nome-estabel fi-dt-limite fi-it-codigo-ini 
          fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-cod-qualid-ini 
          fi-cod-qualid-fin fi-cod-obsoleto-ini fi-cod-obsoleto-fin fi-cod-depos 
          tg-lote-todos tg-lote-pp tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc 
          tg-lote-ca rs-opc-artigo 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE IMAGE-2 IMAGE-51 IMAGE-52 IMAGE-73 IMAGE-75 IMAGE-77 IMAGE-82 IMAGE-83 
         RECT-1 RECT-49 fi-cod-estabel fi-dt-limite fi-it-codigo-ini 
         fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin fi-cod-qualid-ini 
         fi-cod-qualid-fin fi-cod-obsoleto-ini fi-cod-obsoleto-fin fi-cod-depos 
         tg-lote-todos rs-opc-artigo bt-ok bt-cancelar bt-ajuda 
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
  
  {utp/ut9000.i "ESSP0149A" "2.04.00.000"}

  ASSIGN fi-cod-estabel:SENSITIVE = NO.
  IF c-cod-estabel = "0" THEN DO:
     ASSIGN fi-cod-estabel:SENSITIVE = YES.
     ASSIGN c-cod-estabel = '1'.
  END.

  ASSIGN fi-cod-estabel      = c-cod-estabel
         fi-dt-limite        = c-dt-limite
         fi-it-codigo-ini    = c-it-codigo-ini 
         fi-it-codigo-fin    = c-it-codigo-fin 
         fi-cod-refer-ini    = c-cod-refer-ini 
         fi-cod-refer-fin    = c-cod-refer-fin
         fi-cod-qualid-ini   = c-cod-qualid-ini      
         fi-cod-qualid-fin   = c-cod-qualid-fin      
         fi-cod-obsoleto-ini = c-cod-obsoleto-ini 
         fi-cod-obsoleto-fin = c-cod-obsoleto-fin 
         fi-cod-depos        = c-cod-depos
         tg-lote-todos       = l-lote-todos
         rs-opc-artigo       = c-tp-artigo.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF NOT tg-lote-todos THEN DO.
     ASSIGN tg-lote-pp  = l-lote-pp
            tg-lote-pd  = l-lote-pd
            tg-lote-rp  = l-lote-rp
            tg-lote-rd  = l-lote-rd
            tg-lote-sc  = l-lote-sc
            tg-lote-ca  = l-lote-ca.

     APPLY 'value-changed' TO tg-lote-todos IN FRAME {&FRAME-NAME}.
  END.

  APPLY 'TAB' TO fi-cod-estabel.

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

