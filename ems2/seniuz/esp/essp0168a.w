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
{include/i-prgvrs.i ESSP0167A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite-ini     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite-fin     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel-ini   AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-cod-estabel-fin   AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER i-num-reserva-ini   LIKE ped-reserva.num-reserva.
DEFINE INPUT-OUTPUT PARAMETER i-num-reserva-fin   LIKE ped-reserva.num-reserva.
DEFINE INPUT-OUTPUT PARAMETER i-cod-emit-ini      LIKE ped-reserva.cod-emitente. 
DEFINE INPUT-OUTPUT PARAMETER i-cod-emit-fin      LIKE ped-reserva.cod-emitente.
DEFINE INPUT-OUTPUT PARAMETER i-cod-rep-ini       LIKE ped-reserva.cod-rep. 
DEFINE INPUT-OUTPUT PARAMETER i-cod-rep-fin       LIKE ped-reserva.cod-rep.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini     LIKE ped-reserva-it.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin     LIKE ped-reserva-it.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini     LIKE ped-reserva-it.cod-refer.                              
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin     LIKE ped-reserva-it.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER i-reserva           AS INT.
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
IMAGE-89 IMAGE-90 IMAGE-91 RECT-1 RECT-50 RECT-52 IMAGE-92 IMAGE-93 ~
IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 fi-dt-limite-ini fi-dt-limite-fin ~
fi-cod-estabel-ini fi-cod-estabel-fin fi-num-reserva-ini fi-num-reserva-fin ~
fi-cod-emit-ini fi-cod-emit-fin fi-cod-rep-ini fi-cod-rep-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
rs-situacao bt-ajuda bt-ok bt-cancelar 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-limite-ini fi-dt-limite-fin ~
fi-cod-estabel-ini fi-cod-estabel-fin fi-num-reserva-ini fi-num-reserva-fin ~
fi-cod-emit-ini fi-cod-emit-fin fi-cod-rep-ini fi-cod-rep-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
rs-situacao 

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

DEFINE VARIABLE fi-cod-emit-fin AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente final." NO-UNDO.

DEFINE VARIABLE fi-cod-emit-ini AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     LABEL "Cliente":R9 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Cliente inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-fin AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "Referˆncia Final" NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Referˆncia Inicial" NO-UNDO.

DEFINE VARIABLE fi-cod-rep-fin AS INTEGER FORMAT ">>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Representante final." NO-UNDO.

DEFINE VARIABLE fi-cod-rep-ini AS INTEGER FORMAT ">>>>>>" INITIAL 0 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "Representante inicial." NO-UNDO.

DEFINE VARIABLE fi-dt-limite-fin AS CHARACTER FORMAT "99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Data limite final (MM/AAAA)." NO-UNDO.

DEFINE VARIABLE fi-dt-limite-ini AS CHARACTER FORMAT "99/9999":U 
     LABEL "Data Limite" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 TOOLTIP "Data limite inicial (MM/AAAA)." NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Item Final" NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Item Inicial" NO-UNDO.

DEFINE VARIABLE fi-num-reserva-fin AS INTEGER FORMAT ">>>>>>>>>" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Reserva final." NO-UNDO.

DEFINE VARIABLE fi-num-reserva-ini AS INTEGER FORMAT ">>>>>>>>" INITIAL 0 
     LABEL "Reserva":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 TOOLTIP "Reserva inicial." NO-UNDO.

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
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-94
     FILENAME "image\im-las":U
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

DEFINE VARIABLE rs-situacao AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Aberta", 1,
"Atendida", 2,
"Encerrada", 3,
"Todas", 4
     SIZE 28 BY 3.25 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 65 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 65 BY 12.75.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.29 BY 3.88.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-dt-limite-ini AT ROW 1.75 COL 14 COLON-ALIGNED
     fi-dt-limite-fin AT ROW 1.75 COL 42 COLON-ALIGNED NO-LABEL
     fi-cod-estabel-ini AT ROW 2.75 COL 14 COLON-ALIGNED WIDGET-ID 10
     fi-cod-estabel-fin AT ROW 2.75 COL 42 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fi-num-reserva-ini AT ROW 3.75 COL 14 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     fi-num-reserva-fin AT ROW 3.75 COL 42 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     fi-cod-emit-ini AT ROW 4.75 COL 14 COLON-ALIGNED HELP
          "Codigo do cliente"
     fi-cod-emit-fin AT ROW 4.75 COL 42 COLON-ALIGNED HELP
          "Codigo do cliente" NO-LABEL
     fi-cod-rep-ini AT ROW 5.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-cod-rep-fin AT ROW 5.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-it-codigo-ini AT ROW 6.75 COL 14 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 6.75 COL 42 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 7.75 COL 14 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 7.75 COL 42 COLON-ALIGNED NO-LABEL
     rs-situacao AT ROW 9.92 COL 23.86 NO-LABEL
     bt-ajuda AT ROW 14.13 COL 55.86
     bt-ok AT ROW 14.17 COL 3.29
     bt-cancelar AT ROW 14.17 COL 14.29
     "Situacao da Reserva" VIEW-AS TEXT
          SIZE 15 BY .58 AT ROW 9.17 COL 20.29
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     IMAGE-3 AT ROW 3.75 COL 31.57
     IMAGE-4 AT ROW 3.75 COL 40.57
     IMAGE-5 AT ROW 4.75 COL 31.57
     IMAGE-6 AT ROW 4.75 COL 40.57
     IMAGE-88 AT ROW 1.75 COL 31.57
     IMAGE-89 AT ROW 1.75 COL 40.57
     IMAGE-90 AT ROW 5.75 COL 31.57
     IMAGE-91 AT ROW 5.75 COL 40.57
     RECT-1 AT ROW 13.96 COL 2
     RECT-50 AT ROW 1.25 COL 2
     RECT-52 AT ROW 9.42 COL 18
     IMAGE-92 AT ROW 7.75 COL 31.57 WIDGET-ID 2
     IMAGE-93 AT ROW 6.75 COL 31.57 WIDGET-ID 4
     IMAGE-94 AT ROW 6.75 COL 40.57 WIDGET-ID 6
     IMAGE-95 AT ROW 7.75 COL 40.57 WIDGET-ID 8
     IMAGE-96 AT ROW 2.75 COL 31.57 WIDGET-ID 14
     IMAGE-97 AT ROW 2.75 COL 40.57 WIDGET-ID 16
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 21.29
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
         TITLE              = "Sele‡Æo de Reserva"
         COLUMN             = 29.57
         ROW                = 9.83
         HEIGHT             = 14.54
         WIDTH              = 66.43
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Sele‡Æo de Reserva */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Sele‡Æo de Reserva */
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
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-limite-ini.

  IF INT(SUBSTR(fi-dt-limite-ini,1,2)) <  1 OR
     INT(SUBSTR(fi-dt-limite-ini,1,2)) > 12  THEN DO:
      MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO fi-dt-limite-ini.
      RETURN NO-APPLY.
  END.
  IF INT(SUBSTR(fi-dt-limite-ini,4,4)) <  1 THEN DO:
      MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO fi-dt-limite-ini.
      RETURN NO-APPLY.
  END.

  ASSIGN c-dt-limite-ini    = INPUT FRAME {&FRAME-NAME} fi-dt-limite-ini   
         c-dt-limite-fin    = INPUT FRAME {&FRAME-NAME} fi-dt-limite-fin   
         c-cod-estabel-ini  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini   
         c-cod-estabel-fin  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fin   
         i-num-reserva-ini  = INPUT FRAME {&FRAME-NAME} fi-num-reserva-ini   
         i-num-reserva-fin  = INPUT FRAME {&FRAME-NAME} fi-num-reserva-fin   
         i-cod-emit-ini     = INPUT FRAME {&FRAME-NAME} fi-cod-emit-ini
         i-cod-emit-fin     = INPUT FRAME {&FRAME-NAME} fi-cod-emit-fin
         i-cod-rep-ini      = INPUT FRAME {&FRAME-NAME} fi-cod-rep-ini
         i-cod-rep-fin      = INPUT FRAME {&FRAME-NAME} fi-cod-rep-fin
         c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini
         c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin
         c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini
         c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin
         i-reserva          = INPUT FRAME {&FRAME-NAME} rs-situacao
         l-ok = YES.  
  APPLY "CLOSE":U TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-emit-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit-fin w-digita
ON LEAVE OF fi-cod-emit-fin IN FRAME F-Main
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND emitente USE-INDEX codigo WHERE 
           emitente.cod-emit = INPUT FRAME {&FRAME-NAME} fi-cod-emit-fin NO-LOCK NO-ERROR.

      IF AVAIL emitente THEN
         ASSIGN fi-cod-emit-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(emitente.cod-emitente).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-emit-fin IN FRAME F-Main
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-cod-emit-fin
                       &campozoom = cod-emitente}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-emit-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit-ini w-digita
ON LEAVE OF fi-cod-emit-ini IN FRAME F-Main /* Cliente */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     FIND emitente WHERE 
          emitente.cod-emit = INPUT FRAME {&FRAME-NAME} fi-cod-emit-ini 
          USE-INDEX codigo NO-LOCK NO-ERROR.

     IF AVAIL emitente THEN DO.
         ASSIGN fi-cod-emit-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(emitente.cod-emitente).
         ASSIGN fi-cod-emit-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit-ini w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-emit-ini IN FRAME F-Main /* Cliente */
DO:
    {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                       &campo     = fi-cod-emit-ini
                       &campozoom = cod-emitente}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                     &campo     = fi-cod-refer-ini
                     &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-digita
ON LEAVE OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-ini IN FRAME F-Main /* Referˆncia */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-rep-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-rep-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-cod-rep-fin
                     &campozoom = cod-rep}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-rep-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep-ini w-digita
ON LEAVE OF fi-cod-rep-ini IN FRAME F-Main /* Representante */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-rep-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-rep-ini w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-rep-ini IN FRAME F-Main /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-cod-rep-ini
                     &campozoom = cod-rep}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-limite-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite-fin w-digita
ON LEAVE OF fi-dt-limite-fin IN FRAME F-Main
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-fin w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-fin IN FRAME F-Main
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini w-digita
ON LEAVE OF fi-it-codigo-ini IN FRAME F-Main /* Item */
DO:
  IF SELF:SCREEN-VALUE <> '' AND SELF:SCREEN-VALUE <> '5' THEN
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


&Scoped-define SELF-NAME fi-num-reserva-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-reserva-ini w-digita
ON LEAVE OF fi-num-reserva-ini IN FRAME F-Main /* Reserva */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-num-reserva-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
fi-cod-emit-ini:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-cod-emit-fin:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
fi-cod-rep-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-cod-rep-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.


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
  DISPLAY fi-dt-limite-ini fi-dt-limite-fin fi-cod-estabel-ini 
          fi-cod-estabel-fin fi-num-reserva-ini fi-num-reserva-fin 
          fi-cod-emit-ini fi-cod-emit-fin fi-cod-rep-ini fi-cod-rep-fin 
          fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin 
          rs-situacao 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-88 IMAGE-89 IMAGE-90 IMAGE-91 
         RECT-1 RECT-50 RECT-52 IMAGE-92 IMAGE-93 IMAGE-94 IMAGE-95 IMAGE-96 
         IMAGE-97 fi-dt-limite-ini fi-dt-limite-fin fi-cod-estabel-ini 
         fi-cod-estabel-fin fi-num-reserva-ini fi-num-reserva-fin 
         fi-cod-emit-ini fi-cod-emit-fin fi-cod-rep-ini fi-cod-rep-fin 
         fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin 
         rs-situacao bt-ajuda bt-ok bt-cancelar 
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
  ASSIGN fi-dt-limite-ini    = c-dt-limite-ini   
         fi-dt-limite-fin    = c-dt-limite-fin
         fi-cod-estabel-ini  = c-cod-estabel-ini
         fi-cod-estabel-fin  = c-cod-estabel-fin
         fi-num-reserva-ini  = i-num-reserva-ini   
         fi-num-reserva-fin  = i-num-reserva-fin   
         fi-cod-emit-ini     = i-cod-emit-ini
         fi-cod-emit-fin     = i-cod-emit-fin
         fi-cod-rep-ini      = i-cod-rep-ini
         fi-cod-rep-fin      = i-cod-rep-fin
         fi-it-codigo-ini    = c-it-codigo-ini
         fi-it-codigo-fin    = c-it-codigo-fin
         fi-cod-refer-ini    = c-cod-refer-ini
         fi-cod-refer-fin    = c-cod-refer-fin
         rs-situacao         = i-reserva.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
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

