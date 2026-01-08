&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0123 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE tt-fardos LIKE mp-fardo 
       FIELD cod-emit       LIKE mp-entr-mat.cod-emit 
       FIELD nro-docto      LIKE mp-entr-mat.nro-docto
       FIELD dt-receb       LIKE mp-entr-mat.dt-recebimento
       FIELD cor            LIKE mp-coloracao.tonalidade
       FIELD tipo           LIKE mp-tipo.tipo
       FIELD tamanho        AS CHAR
       INDEX indice1 nr-fardo.

DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR opt-sort    AS INT.
DEF VAR c-nome-tipo AS CHAR FORMAT "x(15)".
DEF VAR c-desc-cor  AS CHAR FORMAT "x(20)".
DEF VAR c-desc-tam  AS CHAR FORMAT "x(15)".
DEF VAR i-sit       AS INT.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR c-empresa    AS CHAR.
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.
DEFINE VAR i-lin        AS INT.
DEFINE VAR i-pag        AS INT.
DEFINE VAR l-ok         AS LOG.

DEF BUFFER b-tt-fardos FOR tt-fardos.

DEF NEW GLOBAL SHARED VAR gr-mp-fardo AS ROWID NO-UNDO.

&GLOBAL-DEFINE SORTBY-PHRASE BY IF opt-sort = 1 ~
                                THEN tt-fardos.nr-fardo ~
                                ELSE tt-fardos.dt-receb

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-fardos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-fardos

/* Definitions for BROWSE br-fardos                                     */
&Scoped-define FIELDS-IN-QUERY-br-fardos tt-fardos.dt-receb tt-fardos.nr-fardo tt-fardos.nro-docto tt-fardos.cod-emit tt-fardos.padrao tt-fardos.letra tt-fardos.cor tt-fardos.tipo tt-fardos.peso tt-fardos.tamanho   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-fardos   
&Scoped-define SELF-NAME br-fardos
&Scoped-define QUERY-STRING-br-fardos FOR EACH tt-fardos NO-LOCK ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br-fardos OPEN QUERY {&SELF-NAME} FOR EACH tt-fardos NO-LOCK ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br-fardos tt-fardos
&Scoped-define FIRST-TABLE-IN-QUERY-br-fardos tt-fardos


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-fardos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 ~
IMAGE-33 RECT-28 rt-button bt-Entradas rs-sit-est fi-dt-receb-ini ~
fi-dt-receb-fin fi-nro-docto-ini fi-nro-docto-fin fi-padrao-ini ~
fi-padrao-fin bt-vapra br-fardos bt-detalhe bt-imprime 
&Scoped-Define DISPLAYED-OBJECTS rs-sit-est fi-dt-receb-ini fi-dt-receb-fin ~
fi-nro-docto-ini fi-nro-docto-fin fi-padrao-ini fi-padrao-fin fi-nr-fardo ~
fi-peso 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-Entradas 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-cor w-livre 
FUNCTION fn-desc-cor RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-tam w-livre 
FUNCTION fn-desc-tam RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-nome-tipo w-livre 
FUNCTION fn-nome-tipo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-detalhe 
     LABEL "Detalhe" 
     SIZE 12 BY 1.25.

DEFINE BUTTON bt-Entradas AUTO-GO 
     IMAGE-UP FILE "image/im-ran_a.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Consulta NFs de Entrada Fardos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Imprime o total das baixas".

DEFINE BUTTON bt-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY .88 TOOLTIP "Procura o Padr∆o".

DEFINE VARIABLE fi-dt-receb-fin AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-receb-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Recebimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-fardo AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Qtde Fardo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto-fin AS INTEGER FORMAT ">>>>>9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Nß Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-padrao-fin AS CHARACTER FORMAT "X(20)":U INITIAL "ZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 TOOLTIP "Padr∆o Final para Consulta" NO-UNDO.

DEFINE VARIABLE fi-padrao-ini AS CHARACTER FORMAT "X(20)":U 
     LABEL "Padr∆o Inicial" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 TOOLTIP "Padr∆o Inicial para Consulta"
     FONT 3 NO-UNDO.

DEFINE VARIABLE fi-peso AS DECIMAL FORMAT "->>>>,>>9.99":U INITIAL 0 
     LABEL "Peso" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-31
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-33
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-sit-est AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Em Descarga", 1,
"Aguardando Analise", 2,
"Estoque", 3,
"Requisitado (BAIXADO)", 4
     SIZE 65.57 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 4.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-fardos FOR 
      tt-fardos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-fardos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-fardos w-livre _FREEFORM
  QUERY br-fardos NO-LOCK DISPLAY
      tt-fardos.dt-receb  COLUMN-LABEL "Dt.Receb"
      tt-fardos.nr-fardo  COLUMN-LABEL "Fardo"      WIDTH 8
      tt-fardos.nro-docto COLUMN-LABEL "NF"
      tt-fardos.cod-emit  COLUMN-LABEL "Fornec"
      tt-fardos.padrao    COLUMN-LABEL "Padrao" WIDTH 20   
      tt-fardos.letra     COLUMN-LABEL "Letra"
      tt-fardos.cor       COLUMN-LABEL "Tonalidade"
      tt-fardos.tipo      COLUMN-LABEL "Tipo" WIDTH 4
      tt-fardos.peso      COLUMN-LABEL "Peso" FORMAT ">>>,>>9.99" WIDTH 9.8
      tt-fardos.tamanho   COLUMN-LABEL "Tamanho" FORMAT "x(18)"  WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 88 BY 9.75
         FONT 1
         TITLE "Fardos de  Algod∆o" ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-Entradas AT ROW 1.17 COL 61 WIDGET-ID 2
     rs-sit-est AT ROW 2.92 COL 16.43 NO-LABEL
     fi-dt-receb-ini AT ROW 3.92 COL 14.57 COLON-ALIGNED
     fi-dt-receb-fin AT ROW 3.92 COL 51.86 COLON-ALIGNED NO-LABEL
     fi-nro-docto-ini AT ROW 4.92 COL 14.57 COLON-ALIGNED
     fi-nro-docto-fin AT ROW 4.92 COL 51.86 COLON-ALIGNED NO-LABEL
     fi-padrao-ini AT ROW 5.92 COL 14.57 COLON-ALIGNED HELP
          "Padr∆o Inicial para Consulta"
     fi-padrao-fin AT ROW 5.92 COL 51.86 COLON-ALIGNED HELP
          "Padr∆o Fin al para Consulta" NO-LABEL
     bt-vapra AT ROW 5.92 COL 82.43
     br-fardos AT ROW 7.25 COL 2 WIDGET-ID 100
     bt-detalhe AT ROW 17.21 COL 1.86
     bt-imprime AT ROW 17.21 COL 24 WIDGET-ID 26
     fi-nr-fardo AT ROW 17.21 COL 61.43 COLON-ALIGNED
     fi-peso AT ROW 17.21 COL 76.57 COLON-ALIGNED
     "Situaá∆o do Fardo:" VIEW-AS TEXT
          SIZE 13.43 BY .88 AT ROW 2.92 COL 3
     IMAGE-1 AT ROW 3.92 COL 41
     IMAGE-29 AT ROW 3.92 COL 48.57
     IMAGE-30 AT ROW 4.92 COL 41
     IMAGE-31 AT ROW 5.92 COL 41
     IMAGE-32 AT ROW 4.92 COL 48.57
     IMAGE-33 AT ROW 5.92 COL 48.57
     RECT-28 AT ROW 2.5 COL 2
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 17.67
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta Estoque de Fardos"
         HEIGHT             = 17.79
         WIDTH              = 90
         MAX-HEIGHT         = 28.21
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.21
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-fardos bt-vapra f-cad */
/* SETTINGS FOR BUTTON bt-Entradas IN FRAME f-cad
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-fardo IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-peso IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-fardos
/* Query rebuild information for BROWSE br-fardos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-fardos NO-LOCK ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-fardos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Consulta Estoque de Fardos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Consulta Estoque de Fardos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-fardos
&Scoped-define SELF-NAME br-fardos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-fardos w-livre
ON ROW-DISPLAY OF br-fardos IN FRAME f-cad /* Fardos de  Algod∆o */
DO:
   /*
   IF tt-baixa.prog >  0 THEN
      RUN pi-cor (INPUT 12). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe w-livre
ON CHOOSE OF bt-detalhe IN FRAME f-cad /* Detalhe */
DO:
  FIND mp-fardo WHERE
       mp-fardo.nr-fardo = tt-fardos.nr-fardo NO-LOCK NO-ERROR.
  IF AVAIL mp-fardo THEN DO:
     ASSIGN gr-mp-fardo = ROWID(mp-fardo).
     RUN esp/essp0124.p.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Entradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Entradas w-livre
ON CHOOSE OF bt-Entradas IN FRAME f-cad
DO:
   RUN esp/essp0123a.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-livre
ON CHOOSE OF bt-imprime IN FRAME f-cad /* Impress∆o */
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapra w-livre
ON CHOOSE OF bt-vapra IN FRAME f-cad
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} rs-sit-est
         INPUT FRAME {&FRAME-NAME} fi-dt-receb-ini
         INPUT FRAME {&FRAME-NAME} fi-dt-receb-fin
         INPUT FRAME {&FRAME-NAME} fi-nro-docto-ini
         INPUT FRAME {&FRAME-NAME} fi-nro-docto-fin
         INPUT FRAME {&FRAME-NAME} fi-padrao-ini
         INPUT FRAME {&FRAME-NAME} fi-padrao-fin.
  RUN pi-popula-browse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nro-docto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nro-docto-ini w-livre
ON LEAVE OF fi-nro-docto-ini IN FRAME f-cad /* Nß Nota Fiscal */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-nro-docto-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-padrao-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-padrao-ini w-livre
ON LEAVE OF fi-padrao-ini IN FRAME f-cad /* Padr∆o Inicial */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-padrao-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-sit-est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-sit-est w-livre
ON VALUE-CHANGED OF rs-sit-est IN FRAME f-cad
DO:
   ASSIGN bt-detalhe:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
          bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   IF SELF:SCREEN-VALUE <> "4" THEN
      ASSIGN opt-sort = 1
             bt-detalhe:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
   ELSE
      ASSIGN opt-sort = 2
             bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   APPLY 'choose' TO bt-vapra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

ASSIGN fi-dt-receb-fin:SCREEN-VALUE = STRING(TODAY,"99/99/9999")
       bt-imprime:SENSITIVE IN FRAME {&FRAME-NAME} = NO.


RUN pi-popula-browse.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-Entradas:HANDLE IN FRAME f-cad , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY rs-sit-est fi-dt-receb-ini fi-dt-receb-fin fi-nro-docto-ini 
          fi-nro-docto-fin fi-padrao-ini fi-padrao-fin fi-nr-fardo fi-peso 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE IMAGE-1 IMAGE-29 IMAGE-30 IMAGE-31 IMAGE-32 IMAGE-33 RECT-28 rt-button 
         bt-Entradas rs-sit-est fi-dt-receb-ini fi-dt-receb-fin 
         fi-nro-docto-ini fi-nro-docto-fin fi-padrao-ini fi-padrao-fin bt-vapra 
         br-fardos bt-detalhe bt-imprime 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "ESSP0123" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec w-livre 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  PUT c-empresa  FORMAT "X(40)"                 AT   1
      "DATA: "                                  AT  58
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  64
      "HORA: "                                  AT  85
      STRING(TIME,"hh:mm:ss")                   AT  91
      "PAG:"                                    AT 125
      i-pag FORMAT ">>"                         AT 130
      SKIP(1).

  PUT "RELAT‡RIO DAS BAIXAS DE FARDOS DO ESTOQUE - PERIODO: " AT 19
      fi-dt-receb-ini AT 72
      "A"             AT 83
      fi-dt-receb-fin AT 85 SKIP(1).

  PUT "Dt.Baixa   Peso Baixado Estoque" AT 1. 
  PUT "---------- --------------------" AT 1.

  ASSIGN i-pag = i-pag + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime w-livre 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR h-prog    AS HANDLE NO-UNDO.
 DEF VAR i-ct      AS INT.
 DEF VAR de-totais AS DEC EXTENT 3.

 RUN utp/ut-utils.p PERSISTENT SET h-prog.


 RUN esapi/saida-imp.p (OUTPUT i-saida,
                        OUTPUT c-saida,
                        OUTPUT i-num-copias,
                        OUTPUT l-ok).

 CASE i-saida:
     WHEN 1 THEN DO.
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 64.
         PUT CONTROL "~033E~033(s16H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0123.tmp".
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     END.
 END CASE.

 ASSIGN i-lin     = 99
        i-pag     =  1
        de-totais =  0.

 DO i-ct = 1 TO i-num-copias.
    FOR EACH b-tt-fardos
             NO-LOCK
          BY b-tt-fardos.dt-receb.
    
        IF i-lin > 64 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        PUT b-tt-fardos.dt-receb                         AT  1
            b-tt-fardos.peso FORMAT ">>>,>>>,>>>,>>9.99" AT 14.
        ASSIGN i-lin = i-lin + 1.
        ACCUMULATE b-tt-fardos.peso (TOTAL).
    END.
    IF (ACCUM TOTAL b-tt-fardos.peso) <> 0 THEN DO:
       PUT "--------------------" AT 12 SKIP.
       PUT ACCUM TOTAL b-tt-fardos.peso  FORMAT ">>>,>>>,>>>,>>9.99" AT 14.
    END.
    IF i-saida = 1 THEN DO:
       PAGE.
       PUT "" AT 1.
    END.
 END.
 IF i-saida = 3 THEN DO.
    RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                          INPUT c-saida).
    DELETE PROCEDURE h-prog.
 END.
 OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse w-livre 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
 {utp/ut-liter.i Selecionando_Fardos *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 FOR EACH tt-fardos.
     DELETE tt-fardos.
 END.

 ASSIGN fi-nr-fardo = 0
        fi-peso     = 0.
 IF rs-sit-est <> 4 THEN DO:
    FOR EACH mp-fardo WHERE
             mp-fardo.padrao   >= fi-padrao-ini AND 
             mp-fardo.padrao   <= fi-padrao-fin AND  
             mp-fardo.situacao  = rs-sit-est NO-LOCK,
        EACH mp-entr-mat OF mp-fardo WHERE
             mp-entr-mat.nro-docto      >= fi-nro-docto-ini AND
             mp-entr-mat.nro-docto      <= fi-nro-docto-fin AND
             mp-entr-mat.dt-recebimento >= fi-dt-receb-ini AND
             mp-entr-mat.dt-recebimento <= fi-dt-receb-fin NO-LOCK.
     
        RUN pi-acompanhar IN h-acomp (INPUT "Fardo: " + TRIM(STRING(mp-fardo.nr-fardo,"9999,9999"))
                                             + " Situacao: " + STRING(mp-fardo.situacao)).
      
        FIND mp-coloracao  WHERE
             mp-coloracao.codigo = mp-fardo.cd-coloracao NO-LOCK NO-ERROR.
    
        FIND mp-tipo  WHERE
             mp-tipo.codigo = mp-fardo.cd-tipo NO-LOCK NO-ERROR.

        FIND mp-classificacao WHERE
             mp-classificacao.codigo = mp-fardo.cd-compr NO-LOCK NO-ERROR.

        CREATE tt-fardos.
        BUFFER-COPY mp-fardo TO tt-fardos.
        ASSIGN tt-fardos.cod-emit  = mp-entr-mat.cod-emit
               tt-fardos.nro-docto = mp-entr-mat.nro-docto
               tt-fardos.cor       = IF AVAIL mp-coloracao THEN mp-coloracao.tonalidade                
                                                           ELSE ""
               tt-fardos.tipo      = IF AVAIL mp-tipo      THEN mp-tipo.tipo 
                                                           ELSE ""
               tt-fardos.tamanho   = IF AVAIL mp-classificacao THEN STRING(mp-classificacao.compr-min,">>9.99") + "  A  " +
                                                                    STRING(mp-classificacao.compr-max,">>9.99")
                                                               ELSE ""
               tt-fardos.dt-receb  = mp-entr-mat.dt-recebimento.

        ASSIGN fi-nr-fardo = fi-nr-fardo + 1
               fi-peso     = fi-peso + mp-fardo.peso.

    END.
 END.
 ELSE DO:
    FOR EACH mp-fardo WHERE
             mp-fardo.situacao = 4 AND
             mp-fardo.dt-baixa >= fi-dt-receb-ini AND
             mp-fardo.dt-baixa <= fi-dt-receb-fin NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Fardo: " + TRIM(STRING(mp-fardo.nr-fardo,"9999,9999"))
                                             + " Situacao: " + STRING(mp-fardo.situacao)).

        FIND tt-fardos WHERE
             tt-fardos.dt-receb = mp-fardo.dt-baixa NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-fardos THEN DO:
           CREATE tt-fardos.
           ASSIGN tt-fardos.dt-receb = mp-fardo.dt-baixa.
        END.
        ASSIGN tt-fardos.peso = tt-fardos.peso + mp-fardo.peso.

        ASSIGN fi-nr-fardo = fi-nr-fardo + 1
               fi-peso     = fi-peso + mp-fardo.peso.
    END.
 END.

 RUN pi-finalizar in h-acomp.


 DISPLAY fi-nr-fardo
         fi-peso
        WITH FRAME {&FRAME-NAME}.

{&OPEN-QUERY-BR-FARDOS}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-fardos"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-cor w-livre 
FUNCTION fn-desc-cor RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND mp-coloracao                       
       WHERE mp-coloracao.codigo = mp-fardo.cd-coloracao         
       NO-LOCK NO-ERROR.                    
                                                                    
   ASSIGN c-desc-cor = "".          
   IF AVAIL mp-coloracao THEN                                        
      ASSIGN c-desc-cor = mp-coloracao.tonalidade.                
                                                                    
   RETURN c-desc-cor.   /* Function return value. */               
                                                                    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-tam w-livre 
FUNCTION fn-desc-tam RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND mp-classificacao                       
       WHERE mp-classificacao.codigo = mp-fardo.cd-compr         
       NO-LOCK NO-ERROR.                    
                                                                    
   ASSIGN c-desc-tam = "".          
   IF AVAIL mp-classificacao THEN                                        
      ASSIGN c-desc-tam = STRING(mp-classificacao.compr-min,">>9.99") + " A " + string(mp-classificacao.compr-max,">99.99").                
                                                                    
   RETURN c-desc-tam.   /* Function return value. */               
                                                                    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-nome-tipo w-livre 
FUNCTION fn-nome-tipo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   FIND mp-tipo                       
       WHERE mp-tipo.codigo = mp-fardo.cd-tipo         
       NO-LOCK NO-ERROR.                    
                                                                    
   ASSIGN c-nome-Tipo = "".          
   IF AVAIL mp-tipo THEN                                        
      ASSIGN c-nome-tipo = mp-tipo.tipo.                
                                                                    
   RETURN c-nome-tipo.   /* Function return value. */               
                                                                    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

