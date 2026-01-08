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
*******************************************************************************/
{include/i-prgvrs.i ESSP0169A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel       LIKE ob-etiqueta.cod-estabel.
DEFINE INPUT-OUTPUT PARAMETER da-dt-emissao-ini   LIKE ob-etiqueta.dt-emissao.
DEFINE INPUT-OUTPUT PARAMETER da-dt-emissao-fin   LIKE ob-etiqueta.dt-emissao.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini     LIKE ob-etiqueta.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin     LIKE ob-etiqueta.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini     LIKE ob-etiqueta.cod-refer. 
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin     LIKE ob-etiqueta.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER c-corte-comerc-ini  LIKE ob-etiqueta.corte-comerc. 
DEFINE INPUT-OUTPUT PARAMETER c-corte-comerc-fin  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT-OUTPUT PARAMETER i-tp-tecido         AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-erro-peso         AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-todos        AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-rp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-rd           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-sc           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-lote-ca           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER i-opc-acabado       AS INT.
DEFINE INPUT-OUTPUT PARAMETER i-tipo-ordem        AS INT.
DEFINE INPUT-OUTPUT PARAMETER l-ok                AS LOG.

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
IMAGE-89 RECT-1 RECT-50 fi-cod-estabel fi-dt-emissao-ini fi-dt-emissao-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
cb-tipo-ordem tg-lote-todos rs-tp-tecido rs-opc-acab bt-ok bt-cancelar ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-estabel fi-nome-estabel ~
fi-dt-emissao-ini fi-dt-emissao-fin fi-it-codigo-ini fi-it-codigo-fin ~
fi-cod-refer-ini fi-cod-refer-fin cb-tipo-ordem tg-lote-todos tg-lote-rp ~
tg-lote-rd tg-lote-sc tg-lote-ca rs-tp-tecido rs-opc-acab tg-erro-peso 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin 
&Scoped-define List-5 fi-cod-estabel 

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

DEFINE VARIABLE cb-tipo-ordem AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Tipo de Ordem" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "TODOS",9,
                     "Produá∆o",1,
                     "Retrabalho",2,
                     "Transformaá∆o",3,
                     "Industrializaá∆o",4
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao-fin AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de emissao Etiqueta [DD/MM/AAAA]" NO-UNDO.

DEFINE VARIABLE fi-dt-emissao-ini AS DATE FORMAT "99/99/9999" 
     LABEL "Dt Emiss∆o Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de emissao Etiqueta [DD/MM/AAAA]" NO-UNDO.

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

DEFINE VARIABLE rs-opc-acab AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Liso", 1,
"Estampado", 2,
"Ambos", 3
     SIZE 41.14 BY .79 TOOLTIP "Tipo de artigo." NO-UNDO.

DEFINE VARIABLE rs-tp-tecido AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Indigo", 1,
"Outros", 2,
"Ambos", 3
     SIZE 41.14 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 69 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 11.25.

DEFINE VARIABLE tg-erro-peso AS LOGICAL INITIAL no 
     LABEL "Imprime Somente Pesagens com Erros" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.86 BY .83 NO-UNDO.

DEFINE VARIABLE tg-lote-ca AS LOGICAL INITIAL no 
     LABEL "CA" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote (Corte de Amostra)." NO-UNDO.

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
     fi-cod-estabel AT ROW 1.75 COL 20 COLON-ALIGNED WIDGET-ID 6
     fi-nome-estabel AT ROW 1.75 COL 24.29 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-dt-emissao-ini AT ROW 2.75 COL 20 COLON-ALIGNED
     fi-dt-emissao-fin AT ROW 2.75 COL 49 COLON-ALIGNED NO-LABEL
     fi-it-codigo-ini AT ROW 3.75 COL 20 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 3.75 COL 49 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 4.75 COL 20 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 4.75 COL 49 COLON-ALIGNED NO-LABEL
     cb-tipo-ordem AT ROW 5.75 COL 20 COLON-ALIGNED WIDGET-ID 52
     tg-lote-todos AT ROW 8.08 COL 22.14 WIDGET-ID 44
     tg-lote-rp AT ROW 8.08 COL 35.86 WIDGET-ID 40
     tg-lote-rd AT ROW 8.08 COL 41.86 WIDGET-ID 38
     tg-lote-sc AT ROW 8.08 COL 47.86 WIDGET-ID 42
     tg-lote-ca AT ROW 8.08 COL 53.86 WIDGET-ID 32
     rs-tp-tecido AT ROW 9.25 COL 22 NO-LABEL WIDGET-ID 22
     rs-opc-acab AT ROW 10.21 COL 22 NO-LABEL WIDGET-ID 18
     tg-erro-peso AT ROW 11.29 COL 22.14
     bt-ok AT ROW 12.96 COL 2.86
     bt-cancelar AT ROW 12.96 COL 13.29
     bt-ajuda AT ROW 13 COL 60.43
     "Acabamento:" VIEW-AS TEXT
          SIZE 9.43 BY .54 AT ROW 10.29 COL 12.57 WIDGET-ID 28
     " Seleá∆o" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 8.21 COL 18 WIDGET-ID 26
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10.57 BY 1 AT ROW 9.08 COL 11.57 WIDGET-ID 30
     IMAGE-3 AT ROW 3.75 COL 39
     IMAGE-4 AT ROW 3.75 COL 48
     IMAGE-5 AT ROW 4.75 COL 39
     IMAGE-6 AT ROW 4.75 COL 48
     IMAGE-88 AT ROW 2.75 COL 39
     IMAGE-89 AT ROW 2.75 COL 48
     RECT-1 AT ROW 12.75 COL 2
     RECT-50 AT ROW 1.25 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.86 BY 18.58
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
         TITLE              = "Seleá∆o das Etiquetas"
         COLUMN             = 25.14
         ROW                = 12.25
         HEIGHT             = 13.13
         WIDTH              = 70.57
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR TOGGLE-BOX tg-erro-peso IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-ca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-rd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-rp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-lote-sc IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Seleá∆o das Etiquetas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Seleá∆o das Etiquetas */
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
   IF INPUT FRAME {&FRAME-NAME} tg-lote-rp = 'NO' AND
      INPUT FRAME {&FRAME-NAME} tg-lote-rd = 'NO' AND
      INPUT FRAME {&FRAME-NAME} tg-lote-sc = 'NO' AND
      INPUT FRAME {&FRAME-NAME} tg-lote-ca = 'NO' THEN
      ASSIGN tg-lote-todos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.

   ASSIGN c-cod-estabel      = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
          da-dt-emissao-ini  = INPUT FRAME {&FRAME-NAME} fi-dt-emissao-ini   
          da-dt-emissao-fin  = INPUT FRAME {&FRAME-NAME} fi-dt-emissao-fin   
          c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
          c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin   
          c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini
          c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin
          c-corte-comerc-ini = ''
          c-corte-comerc-fin = 'ZZ'
          i-tp-tecido        = INPUT FRAME {&FRAME-NAME} rs-tp-tecido
          i-tipo-ordem       = INPUT FRAME {&FRAME-NAME} cb-tipo-ordem
          l-erro-peso        = INPUT FRAME {&FRAME-NAME} tg-erro-peso
          l-lote-todos       = INPUT FRAME {&FRAME-NAME} tg-lote-todos
          l-lote-rp          = INPUT FRAME {&FRAME-NAME} tg-lote-rp
          l-lote-rd          = INPUT FRAME {&FRAME-NAME} tg-lote-rd
          l-lote-sc          = INPUT FRAME {&FRAME-NAME} tg-lote-sc
          l-lote-ca          = INPUT FRAME {&FRAME-NAME} tg-lote-ca
          i-opc-acabado      = INPUT FRAME {&FRAME-NAME} rs-opc-acab
          l-ok               = YES.


   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-digita
ON LEAVE OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE 'Estabelecimento n∆o Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-nome-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z02ad098.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel}
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-fin w-digita
ON RETURN OF fi-cod-refer-fin IN FRAME F-Main
DO:
  APPLY 'tab' TO SELF.
  RETURN NO-APPLY.
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
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referància */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-digita
ON RETURN OF fi-cod-refer-ini IN FRAME F-Main /* Referància */
DO:
  APPLY 'tab' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-emissao-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-emissao-fin w-digita
ON RETURN OF fi-dt-emissao-fin IN FRAME F-Main
DO:
  APPLY 'tab' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-emissao-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-emissao-ini w-digita
ON RETURN OF fi-dt-emissao-ini IN FRAME F-Main /* Dt Emiss∆o Etiqueta */
DO:
  APPLY 'tab' TO SELF.
  RETURN NO-APPLY.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-fin w-digita
ON RETURN OF fi-it-codigo-fin IN FRAME F-Main
DO:
  APPLY 'tab' TO SELF.
  RETURN NO-APPLY.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-digita
ON RETURN OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  APPLY 'tab' TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-lote-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-lote-todos w-digita
ON VALUE-CHANGED OF tg-lote-todos IN FRAME F-Main /* TODOS */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-lote-todos = NO THEN
     ASSIGN tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  ELSE DO.
      ASSIGN tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-lote-sc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
             tg-lote-ca:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'.

      ASSIGN tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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
  DISPLAY fi-cod-estabel fi-nome-estabel fi-dt-emissao-ini fi-dt-emissao-fin 
          fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin 
          cb-tipo-ordem tg-lote-todos tg-lote-rp tg-lote-rd tg-lote-sc 
          tg-lote-ca rs-tp-tecido rs-opc-acab tg-erro-peso 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-88 IMAGE-89 RECT-1 RECT-50 
         fi-cod-estabel fi-dt-emissao-ini fi-dt-emissao-fin fi-it-codigo-ini 
         fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin cb-tipo-ordem 
         tg-lote-todos rs-tp-tecido rs-opc-acab bt-ok bt-cancelar bt-ajuda 
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
  ASSIGN fi-cod-estabel = c-cod-estabel
         fi-dt-emissao-ini = da-dt-emissao-ini   
         fi-dt-emissao-fin = da-dt-emissao-fin
         fi-it-codigo-ini = c-it-codigo-ini   
         fi-it-codigo-fin = c-it-codigo-fin   
         fi-cod-refer-ini = c-cod-refer-ini
         fi-cod-refer-fin = c-cod-refer-fin
         cb-tipo-ordem = i-tipo-ordem
         tg-lote-todos = l-lote-todos
         rs-tp-tecido = i-tp-tecido
         rs-opc-acab = i-opc-acabado
         tg-erro-peso  = l-erro-peso.

   IF NOT tg-lote-todos THEN 
      ASSIGN tg-lote-rp = l-lote-rp
             tg-lote-rd = l-lote-rd
             tg-lote-sc = l-lote-sc
             tg-lote-ca = l-lote-ca.

   IF fi-cod-estabel = '0' THEN
      ASSIGN fi-cod-estabel = '1'.

   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

   /* Code placed here will execute AFTER standard behavior.    */
   
   APPLY 'value-changed' TO tg-lote-todos IN FRAME {&FRAME-NAME}.

   APPLY 'TAB' TO fi-cod-estabel IN FRAME {&FRAME-NAME}.



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

