&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0111 2.04.00.000}
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

/* Local Variable Definitions ---  
*/

DEFINE TEMP-TABLE tt-ob-etiqueta LIKE ob-etiqueta 
       FIELD nome-abrev   LIKE ped-item-rom.nome-abrev
       FIELD nr-pedcli    LIKE ped-item-rom.nr-pedcli
       FIELD nr-nota-fis  LIKE ped-item-res.nr-nota-fis
       FIELD estado       AS   CHAR.

DEF VAR c-situacao AS CHAR FORMAT "x(12)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-ob-etiqueta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ob-etiqueta

/* Definitions for BROWSE br-ob-etiqueta                                */
&Scoped-define FIELDS-IN-QUERY-br-ob-etiqueta tt-ob-etiqueta.nr-sequencia tt-ob-etiqueta.nr-carro tt-ob-etiqueta.num-etiqueta tt-ob-etiqueta.dt-emissao tt-ob-etiqueta.hr-emissao tt-ob-etiqueta.nr-lote tt-ob-etiqueta.quantidade tt-ob-etiqueta.estado tt-ob-etiqueta.localizacao tt-ob-etiqueta.nr-reporte tt-ob-etiqueta.resp-revisao tt-ob-etiqueta.nome-abrev tt-ob-etiqueta.nr-pedcli tt-ob-etiqueta.nr-nota-fis   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-ob-etiqueta   
&Scoped-define SELF-NAME br-ob-etiqueta
&Scoped-define QUERY-STRING-br-ob-etiqueta FOR EACH tt-ob-etiqueta NO-LOCK                               BY tt-ob-etiqueta.nr-sequencia
&Scoped-define OPEN-QUERY-br-ob-etiqueta OPEN QUERY {&SELF-NAME} FOR EACH tt-ob-etiqueta NO-LOCK                               BY tt-ob-etiqueta.nr-sequencia.
&Scoped-define TABLES-IN-QUERY-br-ob-etiqueta tt-ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-ob-etiqueta tt-ob-etiqueta


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-ob-etiqueta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-ob-etiqueta bt-ajuda bt-ok bt-cancelar ~
fi-cod-estabel bt-ant bt-prox btn-vapra fi-nr-ob RECT-1 RECT-44 RECT-45 ~
RECT-46 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-carro fi-cod-estabel fi-nome-estabel ~
fi-nr-ob fi-it-codigo fi-desc-item fi-cod-refer fi-impressa fi-total ~
fi-producao fi-estoque fi-reserva fi-faturada fi-reprocesso fi-corte fi-rp ~
fi-rd fi-ca fi-sc fi-tot-lote fi-tipo-ordem 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-tipo-ordem 
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

DEFINE BUTTON bt-ant 
     IMAGE-UP FILE "image/im-ante.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Consulta Etiqueta Anterior".

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-prox 
     IMAGE-UP FILE "image/im-nex.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1 TOOLTIP "Consulta Proxima Etiqueta".

DEFINE BUTTON btn-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Btn 4" 
     SIZE 4 BY 1.

DEFINE VARIABLE fi-ca AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-estoque AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-faturada AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-impressa AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(8)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estabel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 39 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-carro AS CHARACTER FORMAT "X(3)":U 
     LABEL "Nß do Carro" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-ob AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "N£mero da OB" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-producao AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-rd AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-reprocesso AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-reserva AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-rp AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-sc AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tot-lote AS DECIMAL FORMAT ">>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-total AS DECIMAL FORMAT "->>>>,>>9.99":R16 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-tipo-ordem AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Produá∆o", 1,
"Retrabalho", 2,
"Transformaá∆o", 3,
"Industrializaá∆o", 4
     SIZE 13 BY 3.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 116 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 116 BY 10.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 9.58.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 6.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-ob-etiqueta FOR 
      tt-ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-ob-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-ob-etiqueta w-digita _FREEFORM
  QUERY br-ob-etiqueta DISPLAY
      tt-ob-etiqueta.nr-sequencia COLUMN-LABEL "Seq"   
      tt-ob-etiqueta.nr-carro     COLUMN-LABEL "Carro"    
      tt-ob-etiqueta.num-etiqueta COLUMN-LABEL "Num Etiqueta"
      tt-ob-etiqueta.dt-emissao   COLUMN-LABEL "Emiss∆o"
      tt-ob-etiqueta.hr-emissao   COLUMN-LABEL "Hora" WIDTH 5 FORMAT "999999"
      tt-ob-etiqueta.nr-lote      COLUMN-LABEL "Lote"
      tt-ob-etiqueta.quantidade   COLUMN-LABEL "Quantidade"
      tt-ob-etiqueta.estado       COLUMN-LABEL "Situacao" WIDTH 10 FORMAT "x(10)"
      tt-ob-etiqueta.localizacao  COLUMN-LABEL "Localiz" FORMAT "999/999" WIDTH 6
      tt-ob-etiqueta.nr-reporte   COLUMN-LABEL "Reporte"
      tt-ob-etiqueta.resp-revisao COLUMN-LABEL "Resp.Rev"
      tt-ob-etiqueta.nome-abrev   COLUMN-LABEL "Cliente" WIDTH 12
      tt-ob-etiqueta.nr-pedcli    COLUMN-LABEL "Pedido"
      tt-ob-etiqueta.nr-nota-fis  COLUMN-LABEL "Nota Fiscal" WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 115.86 BY 11.75
         FONT 1
         TITLE "Etiquetas da OB" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-ob-etiqueta AT ROW 11.13 COL 1.14
     fi-nr-carro AT ROW 2.5 COL 36.43 COLON-ALIGNED WIDGET-ID 106
     bt-ajuda AT ROW 23.21 COL 106.14
     bt-ok AT ROW 23.25 COL 2.14
     bt-cancelar AT ROW 23.25 COL 13.14
     fi-cod-estabel AT ROW 1.42 COL 14.29 COLON-ALIGNED WIDGET-ID 6
     fi-nome-estabel AT ROW 1.42 COL 18.72 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     bt-ant AT ROW 2.38 COL 62.86 WIDGET-ID 10
     bt-prox AT ROW 2.38 COL 66.86 WIDGET-ID 12
     btn-vapra AT ROW 2.42 COL 55 WIDGET-ID 14
     fi-nr-ob AT ROW 2.46 COL 14 COLON-ALIGNED WIDGET-ID 22
     fi-it-codigo AT ROW 3.67 COL 14 COLON-ALIGNED WIDGET-ID 20
     fi-desc-item AT ROW 3.67 COL 23.29 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fi-cod-refer AT ROW 4.58 COL 14 COLON-ALIGNED WIDGET-ID 16
     fi-impressa AT ROW 2.67 COL 97.86 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     fi-total AT ROW 9.67 COL 97.86 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     fi-producao AT ROW 3.67 COL 97.86 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     fi-estoque AT ROW 4.67 COL 97.86 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     fi-reserva AT ROW 5.67 COL 97.86 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fi-faturada AT ROW 6.67 COL 97.86 COLON-ALIGNED NO-LABEL WIDGET-ID 58
     fi-reprocesso AT ROW 7.67 COL 97.86 COLON-ALIGNED NO-LABEL WIDGET-ID 60
     fi-corte AT ROW 8.67 COL 97.86 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     fi-rp AT ROW 5.75 COL 55.43 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     fi-rd AT ROW 6.75 COL 55.43 COLON-ALIGNED NO-LABEL WIDGET-ID 80
     fi-ca AT ROW 7.75 COL 55.43 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     fi-sc AT ROW 8.75 COL 55.43 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     fi-tot-lote AT ROW 9.75 COL 55.43 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     fi-tipo-ordem AT ROW 5.83 COL 16.14 NO-LABEL WIDGET-ID 100
     "                     Total Geral" VIEW-AS TEXT
          SIZE 32.57 BY .58 AT ROW 1.33 COL 83.29 WIDGET-ID 34
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "                  Total Por  Lote" VIEW-AS TEXT
          SIZE 36.43 BY .58 AT ROW 4.83 COL 41.29 WIDGET-ID 72
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "Rolo Perfeito:" VIEW-AS TEXT
          SIZE 9.43 BY .54 AT ROW 5.92 COL 47.43 WIDGET-ID 78
          BGCOLOR 8 FGCOLOR 9 
     "Rolo Defeituoso:" VIEW-AS TEXT
          SIZE 11.57 BY .54 AT ROW 6.92 COL 45.29 WIDGET-ID 82
          BGCOLOR 8 FGCOLOR 9 
     "Corte de Amostra:" VIEW-AS TEXT
          SIZE 12.43 BY .54 AT ROW 7.92 COL 44.57 WIDGET-ID 86
          BGCOLOR 8 FGCOLOR 9 
     "Retalho:" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 8.92 COL 50.86 WIDGET-ID 92
          BGCOLOR 8 FGCOLOR 9 
     "T O T A L:" VIEW-AS TEXT
          SIZE 7.57 BY .54 AT ROW 9.92 COL 49.29 WIDGET-ID 94
          BGCOLOR 8 FGCOLOR 9 
     "Impressa:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 2.83 COL 98.43 RIGHT-ALIGNED WIDGET-ID 40
          BGCOLOR 8 FGCOLOR 9 
     "Tipo Ordem:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 5.92 COL 7.29 WIDGET-ID 98
     "Em Produá∆o:" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 3.83 COL 98.43 RIGHT-ALIGNED WIDGET-ID 44
          BGCOLOR 8 FGCOLOR 9 
     "Em Estoque:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 4.83 COL 98.29 RIGHT-ALIGNED WIDGET-ID 48
          BGCOLOR 8 FGCOLOR 9 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116 BY 23.46
         FONT 1
         DEFAULT-BUTTON bt-ok.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Reservada:" VIEW-AS TEXT
          SIZE 8.29 BY .54 AT ROW 5.83 COL 91 WIDGET-ID 52
          BGCOLOR 8 FGCOLOR 9 
     "Faturada:" VIEW-AS TEXT
          SIZE 6.86 BY .54 AT ROW 6.83 COL 92.43 WIDGET-ID 56
          BGCOLOR 8 FGCOLOR 9 
     "Em Reprocesso:" VIEW-AS TEXT
          SIZE 11.57 BY .54 AT ROW 7.83 COL 87.57 WIDGET-ID 62
          BGCOLOR 8 FGCOLOR 9 
     "Consumo Corte:" VIEW-AS TEXT
          SIZE 11.29 BY .54 AT ROW 8.83 COL 88 WIDGET-ID 66
          BGCOLOR 8 FGCOLOR 9 
     "Total da OB:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 9.83 COL 90.57 WIDGET-ID 68
          BGCOLOR 8 FGCOLOR 9 
     "              da OB Selecionada" VIEW-AS TEXT
          SIZE 32.57 BY .58 AT ROW 1.92 COL 83.29 WIDGET-ID 38
          BGCOLOR 1 FGCOLOR 15 FONT 6
     RECT-1 AT ROW 23 COL 1
     RECT-44 AT ROW 1.04 COL 1
     RECT-45 AT ROW 1.21 COL 83.14 WIDGET-ID 70
     RECT-46 AT ROW 4.75 COL 41.14 WIDGET-ID 96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116 BY 23.46
         FONT 1
         DEFAULT-BUTTON bt-ok.


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
         TITLE              = "Etiquetas Geradas Para Uma OB"
         HEIGHT             = 23.46
         WIDTH              = 116
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-ob-etiqueta 1 F-Main */
/* SETTINGS FOR FILL-IN fi-ca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME F-Main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-corte IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estoque IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-faturada IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-impressa IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-carro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-producao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-rd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-reprocesso IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-reserva IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-rp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET fi-tipo-ordem IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-tot-lote IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "Impressa:"
          SIZE 7 BY .54 AT ROW 2.83 COL 98.43 RIGHT-ALIGNED             */

/* SETTINGS FOR TEXT-LITERAL "Em Produá∆o:"
          SIZE 10 BY .54 AT ROW 3.83 COL 98.43 RIGHT-ALIGNED            */

/* SETTINGS FOR TEXT-LITERAL "Em Estoque:"
          SIZE 9 BY .54 AT ROW 4.83 COL 98.29 RIGHT-ALIGNED             */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-ob-etiqueta
/* Query rebuild information for BROWSE br-ob-etiqueta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ob-etiqueta NO-LOCK
                              BY tt-ob-etiqueta.nr-sequencia.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-ob-etiqueta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Etiquetas Geradas Para Uma OB */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Etiquetas Geradas Para Uma OB */
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


&Scoped-define SELF-NAME bt-ant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ant w-digita
ON CHOOSE OF bt-ant IN FRAME F-Main /* Button 2 */
DO:
  IF fi-nr-ob:SCREEN-VALUE <> "" THEN
     FIND PREV ordem-benefic WHERE
               ordem-benefic.cod-estabel  = fi-cod-estabel AND
               ordem-benefic.nr-ob       <= fi-nr-ob NO-LOCK NO-ERROR.
  ELSE
     FIND PREV ordem-benefic WHERE
               ordem-benefic.cod-estabel = fi-cod-estabel NO-LOCK NO-ERROR.
  IF AVAIL ordem-benefic THEN DO.
     ASSIGN fi-nr-ob:SCREEN-VALUE = STRING(ordem-benefic.nr-ob)
            fi-nr-carro:SCREEN-VALUE = ordem-benefic.nr-carro.
     APPLY 'choose' TO btn-vapra.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prox
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prox w-digita
ON CHOOSE OF bt-prox IN FRAME F-Main /* Button 3 */
DO:
   IF fi-nr-ob:SCREEN-VALUE <> "" THEN DO.
      FIND NEXT ordem-benefic WHERE
                ordem-benefic.cod-estabel  = fi-cod-estabel AND
                ordem-benefic.nr-ob       >= fi-nr-ob NO-LOCK NO-ERROR.
   END.
   ELSE
      FIND NEXT ordem-benefic WHERE
                ordem-benefic.cod-estabel = fi-cod-estabel NO-LOCK NO-ERROR.
   IF AVAIL ordem-benefic THEN DO.
      ASSIGN fi-nr-ob:SCREEN-VALUE = STRING(ordem-benefic.nr-ob)
             fi-nr-carro:SCREEN-VALUE = ordem-benefic.nr-carro.
      APPLY 'choose' TO btn-vapra.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-vapra w-digita
ON CHOOSE OF btn-vapra IN FRAME F-Main /* Btn 4 */
DO:
    /*
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-nr-ob
         INPUT FRAME {&FRAME-NAME} fi-cod-estabel.

  FIND FIRST ordem-benefic WHERE
             ordem-benefic.cod-estabel = fi-cod-estabel AND
             ordem-benefic.nr-ob = fi-nr-ob NO-LOCK NO-ERROR.
  */
  EMPTY TEMP-TABLE tt-ob-etiqueta.
  ASSIGN fi-impressa   = 0 fi-producao = 0 fi-estoque = 0 fi-reserva = 0
         fi-reprocesso = 0 fi-faturada = 0 fi-corte   = 0 fi-total   = 0.

  ASSIGN fi-rp = 0 fi-rd = 0 fi-sc = 0 fi-ca = 0 fi-tot-lote = 0.


  IF AVAIL ordem-benefic THEN DO.
     ASSIGN fi-desc-item:FGCOLOR = ?
            fi-desc-item:FONT    = ?.
     FIND item WHERE
          item.it-codigo = ordem-benefic.it-codigo NO-LOCK NO-ERROR.

     IF AVAIL ITEM THEN
        ASSIGN fi-desc-item:SCREEN-VALUE = ITEM.desc-item.

     ASSIGN fi-it-codigo:SCREEN-VALUE  = ordem-benefic.it-codigo
            fi-cod-refer:SCREEN-VALUE  = ordem-benefic.cod-refer
            fi-tipo-ordem:SCREEN-VALUE = STRING(ordem-benefic.tipo-ordem)
            fi-nr-carro:SCREEN-VALUE   = ordem-benefic.nr-carro.

     FOR EACH ob-etiqueta WHERE
              ob-etiqueta.cod-estabel = ordem-benefic.cod-estabel AND
              ob-etiqueta.nr-ob       = ordem-benefic.nr-ob       AND 
              ob-etiqueta.nr-carro    = ordem-benefic.nr-carro NO-LOCK.
         CREATE tt-ob-etiqueta.
         BUFFER-COPY ob-etiqueta TO tt-ob-etiqueta.

         {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 

         ASSIGN tt-ob-etiqueta.estado = c-situacao.

         FIND ped-item-rom USE-INDEX indice3 WHERE                  
              ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta AND
              ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel NO-LOCK NO-ERROR.                    
         IF AVAIL ped-item-rom THEN DO:                                       
            ASSIGN tt-ob-etiqueta.nr-pedcli = ped-item-rom.nr-pedcli
                   tt-ob-etiqueta.nome-abrev = ped-item-rom.nome-abrev.                
            FIND ped-item-res USE-INDEX INDICE1 WHERE 
                 ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
                 ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli  AND
                 ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
            IF AVAIL ped-item-res THEN                                        
               ASSIGN tt-ob-etiqueta.nr-nota-fis = ped-item-res.nr-nota-fis.                
         END.
         CASE ob-etiqueta.situacao:
              WHEN 1 THEN
                  ASSIGN fi-impressa = fi-impressa + ob-etiqueta.quantidade.
              WHEN 2 THEN
                  ASSIGN fi-producao = fi-producao + ob-etiqueta.quantidade.
              WHEN 3 THEN
                  ASSIGN fi-estoque = fi-estoque + ob-etiqueta.quantidade.
              WHEN 4 THEN
                  ASSIGN fi-reserva = fi-reserva + ob-etiqueta.quantidade.
              WHEN 5 THEN
                  ASSIGN fi-faturada = fi-faturada + ob-etiqueta.quantidade.
              WHEN 6 THEN
                  ASSIGN fi-reprocesso = fi-reprocesso + ob-etiqueta.quantidade.
              WHEN 7 THEN
                  ASSIGN fi-corte = fi-corte + ob-etiqueta.quantidade.
         END CASE.
         CASE ob-etiqueta.nr-lote:
              WHEN "RP" THEN
                  ASSIGN fi-rp = fi-rp + ob-etiqueta.quantidade.
              WHEN "RD" THEN
                  ASSIGN fi-rd = fi-rd + ob-etiqueta.quantidade.
              WHEN "CA" THEN
                  ASSIGN fi-ca = fi-ca + ob-etiqueta.quantidade.
              WHEN "SC" THEN
                  ASSIGN fi-sc = fi-sc + ob-etiqueta.quantidade.
         END CASE.
         ASSIGN fi-total    = fi-total    + ob-etiqueta.quantidade
                fi-tot-lote = fi-tot-lote + ob-etiqueta.quantidade.
     END.
  END.
  ELSE DO.
      ASSIGN fi-desc-item:FGCOLOR = 12
             fi-desc-item:FONT = 9.
      ASSIGN fi-desc-item:SCREEN-VALUE = "ERRO: OB n∆o encontrada no Sistema...".

      ASSIGN fi-it-codigo:SCREEN-VALUE = ''
             fi-cod-refer:SCREEN-VALUE = ''.
  END.

  {&OPEN-QUERY-br-ob-etiqueta}

  DISP fi-impressa
       fi-producao
       fi-estoque
       fi-reserva
       fi-faturada
       fi-reprocesso
       fi-corte
       fi-total
       fi-rp
       fi-rd
       fi-ca
       fi-sc
       fi-tot-lote
       WITH FRAME {&FRAME-NAME}.

  APPLY 'entry' TO fi-nr-ob.
  RETURN NO-APPLY.
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
  IF AVAIL estabelec THEN
     ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME F-Main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad107.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-ob
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-ob w-digita
ON RETURN OF fi-nr-ob IN FRAME F-Main /* N£mero da OB */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-nr-ob
         INPUT FRAME {&FRAME-NAME} fi-cod-estabel.

  FIND FIRST ordem-benefic WHERE
             ordem-benefic.cod-estabel = fi-cod-estabel AND
             ordem-benefic.nr-ob = fi-nr-ob NO-LOCK NO-ERROR.

  APPLY 'choose' TO btn-vapra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-ob-etiqueta
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
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
  DISPLAY fi-nr-carro fi-cod-estabel fi-nome-estabel fi-nr-ob fi-it-codigo 
          fi-desc-item fi-cod-refer fi-impressa fi-total fi-producao fi-estoque 
          fi-reserva fi-faturada fi-reprocesso fi-corte fi-rp fi-rd fi-ca fi-sc 
          fi-tot-lote fi-tipo-ordem 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE br-ob-etiqueta bt-ajuda bt-ok bt-cancelar fi-cod-estabel bt-ant 
         bt-prox btn-vapra fi-nr-ob RECT-1 RECT-44 RECT-45 RECT-46 
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

  {utp/ut9000.i "ESSP0111" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '1'.
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel 
       NO-LOCK NO-ERROR.
  IF AVAIL estabelec THEN
     ASSIGN fi-nome-estabel:SCREEN-VALUE = TRIM(estabelec.cidade) + " / " + TRIM(estabelec.bairro).

  APPLY 'entry' TO fi-nr-ob IN FRAME {&FRAME-NAME}.

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-ob-etiqueta"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

