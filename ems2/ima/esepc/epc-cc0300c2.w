&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
{include/i-prgvrs.i XX9999 9.99.99.999}
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
DEF NEW GLOBAL SHARED VAR gr-row-in295 AS ROWID.

/* Parameters Definitions ---                                           */
DEF BUFFER cotacao FOR mgcad.cotacao.
DEF BUFFER moeda FOR mgcad.moeda.

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-it-container LIKE pp-it-container.
DEF TEMP-TABLE tt-it-container-corrente LIKE pp-it-container.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-container

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES pp-container tt-it-container

/* Definitions for BROWSE br-container                                  */
&Scoped-define FIELDS-IN-QUERY-br-container pp-container.cod-estabel ~
pp-container.nr-container 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-container 
&Scoped-define QUERY-STRING-br-container FOR EACH pp-container ~
      WHERE pp-container.situacao = 1 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-container OPEN QUERY br-container FOR EACH pp-container ~
      WHERE pp-container.situacao = 1 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-container pp-container
&Scoped-define FIRST-TABLE-IN-QUERY-br-container pp-container


/* Definitions for BROWSE br-it-container                               */
&Scoped-define FIELDS-IN-QUERY-br-it-container tt-it-container.it-codigo tt-it-container.cod-refer tt-it-container.desc-item tt-it-container.qt-pedida tt-it-container.preco-compra   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-it-container tt-it-container.preco-compra   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-it-container tt-it-container
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-it-container tt-it-container
&Scoped-define SELF-NAME br-it-container
&Scoped-define OPEN-QUERY-br-it-container RUN pi-totais. OPEN QUERY br-it-container FOR EACH tt-it-container NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-it-container tt-it-container
&Scoped-define FIRST-TABLE-IN-QUERY-br-it-container tt-it-container


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-container}~
    ~{&OPEN-QUERY-br-it-container}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-31 RECT-32 RECT-33 br-container ~
fi-cod-emit-forn fi-comprador fi-dt-compra fi-dt-prev-chegada ~
br-it-container fi-moeda bt-preco fi-dt-cotacao bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-emit-forn fi-nome-forn fi-comprador ~
fi-nome-compr fi-dt-compra fi-dt-prev-chegada fi-moeda fi-desc-moeda ~
fi-tot-container fi-dt-cotacao fi-cotacao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-moeda 

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

DEFINE BUTTON bt-preco AUTO-GO 
     IMAGE-UP FILE "image/im-pcust.bmp":U
     LABEL "" 
     SIZE 9 BY 1.04 TOOLTIP "Informa Preáo de Compra"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-cod-emit-forn AS INTEGER FORMAT ">>>>>>>9" INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88.

DEFINE VARIABLE fi-comprador AS CHARACTER FORMAT "X(12)" 
     LABEL "Comprador" 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88.

DEFINE VARIABLE fi-cotacao AS DECIMAL FORMAT ">>,>>9.999999":U INITIAL 0 
     LABEL "Cotaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-moeda AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-compra AS DATE FORMAT "99/99/9999" 
     LABEL "Data da Compra" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE fi-dt-cotacao AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Cotaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-prev-chegada AS DATE FORMAT "99/99/9999" 
     LABEL "Data Prev Chegada" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88.

DEFINE VARIABLE fi-moeda AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Moeda" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-compr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-forn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-container AS DECIMAL FORMAT ">>>,>>9.99":R16 INITIAL 0 
     LABEL "Qtde Total Container" 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 3.5.

DEFINE RECTANGLE RECT-32
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 2.5.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 2.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-container FOR 
      pp-container SCROLLING.

DEFINE QUERY br-it-container FOR 
      tt-it-container SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-container w-digita _STRUCTURED
  QUERY br-container NO-LOCK DISPLAY
      pp-container.cod-estabel FORMAT "X(3)":U WIDTH 4.57
      pp-container.nr-container COLUMN-LABEL "Processo" FORMAT ">>>>,>9":U
            WIDTH 11.43
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 21.14 BY 11.5
         FONT 1
         TITLE "Containers Pendentes".

DEFINE BROWSE br-it-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-it-container w-digita _FREEFORM
  QUERY br-it-container NO-LOCK DISPLAY
      tt-it-container.it-codigo                                FORMAT "X(8)":U         WIDTH 8
      tt-it-container.cod-refer                                FORMAT "X(3)":U         WIDTH 5
      tt-it-container.desc-item                                FORMAT "X(36)":U        WIDTH 34
      tt-it-container.qt-pedida    COLUMN-LABEL "Qtd Comprada" FORMAT ">,>>>,>>9.99":U WIDTH 11.5
      tt-it-container.preco-compra COLUMN-LABEL "Preco Un"
ENABLE
      tt-it-container.preco-compra
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 7.75
         FONT 1
         TITLE "Itens do Container".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-container AT ROW 1.25 COL 1.86 WIDGET-ID 300
     fi-cod-emit-forn AT ROW 1.5 COL 35.86 COLON-ALIGNED WIDGET-ID 22
     fi-nome-forn AT ROW 1.5 COL 45.86 COLON-ALIGNED NO-LABEL WIDGET-ID 34
     fi-comprador AT ROW 2.5 COL 35.86 COLON-ALIGNED WIDGET-ID 24
     fi-nome-compr AT ROW 2.5 COL 45.86 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fi-dt-compra AT ROW 3.5 COL 35.86 COLON-ALIGNED WIDGET-ID 26
     fi-dt-prev-chegada AT ROW 3.5 COL 76 COLON-ALIGNED WIDGET-ID 28
     br-it-container AT ROW 5 COL 24 WIDGET-ID 200
     fi-moeda AT ROW 13.25 COL 11.86 COLON-ALIGNED WIDGET-ID 52
     fi-desc-moeda AT ROW 13.25 COL 16.86 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     bt-preco AT ROW 13.67 COL 81 WIDGET-ID 40
     fi-tot-container AT ROW 13.71 COL 79.01 RIGHT-ALIGNED WIDGET-ID 38
     fi-dt-cotacao AT ROW 14.25 COL 11.86 COLON-ALIGNED WIDGET-ID 46
     fi-cotacao AT ROW 14.25 COL 36.86 COLON-ALIGNED WIDGET-ID 48
     bt-ok AT ROW 15.83 COL 3.29
     bt-cancelar AT ROW 15.83 COL 13.86
     bt-ajuda AT ROW 15.83 COL 80.29
     RECT-1 AT ROW 15.58 COL 2
     RECT-31 AT ROW 1.25 COL 24 WIDGET-ID 36
     RECT-32 AT ROW 13 COL 2 WIDGET-ID 54
     RECT-33 AT ROW 13 COL 51 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.72 BY 16.54
         FONT 1 WIDGET-ID 100.


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
         TITLE              = "Gera Ordens de Compra com Itens do Container EPC-CC0300C2"
         HEIGHT             = 16.54
         WIDTH              = 90.72
         MAX-HEIGHT         = 29.42
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 29.42
         VIRTUAL-WIDTH      = 195.14
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
/* BROWSE-TAB br-container RECT-33 F-Main */
/* BROWSE-TAB br-it-container fi-dt-prev-chegada F-Main */
/* SETTINGS FOR FILL-IN fi-cotacao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-moeda IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-moeda IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-compr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-forn IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-container IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-container
/* Query rebuild information for BROWSE br-container
     _TblList          = "espec.pp-container"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.pp-container.situacao = 1"
     _FldNameList[1]   > espec.pp-container.cod-estabel
"pp-container.cod-estabel" ? ? "character" ? ? ? ? ? ? no ? no no "4.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > espec.pp-container.nr-container
"pp-container.nr-container" "Processo" ">>>>,>9" "integer" ? ? ? ? ? ? no ? no no "11.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br-container */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-it-container
/* Query rebuild information for BROWSE br-it-container
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY br-it-container FOR EACH tt-it-container NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "espec.pp-it-container.nr-container = pp-container.nr-container"
     _Query            is OPENED
*/  /* BROWSE br-it-container */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Gera Ordens de Compra com Itens do Container EPC-CC0300C2 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Gera Ordens de Compra com Itens do Container EPC-CC0300C2 */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-container
&Scoped-define SELF-NAME br-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-container w-digita
ON VALUE-CHANGED OF br-container IN FRAME F-Main /* Containers Pendentes */
DO:
   ASSIGN fi-cod-emit-forn:SCREEN-VALUE = STRING(pp-container.cod-emit-forn)
          fi-comprador:SCREEN-VALUE = STRING(pp-container.comprador)
          fi-dt-compra:SCREEN-VALUE = STRING(pp-container.dt-compra)
          fi-dt-prev-chegada:SCREEN-VALUE = STRING(pp-container.dt-prev-chegada).

   APPLY "LEAVE"TO fi-cod-emit-forn.
   APPLY "LEAVE"TO fi-comprador.

   EMPTY TEMP-TABLE tt-it-container.
   
   FOR EACH pp-it-container WHERE
            pp-it-container.nr-container = pp-container.nr-container NO-LOCK.
       FIND tt-it-container WHERE
            tt-it-container.it-codigo = pp-it-container.it-codigo AND
            tt-it-container.cod-refer = pp-it-container.cod-refer
            NO-LOCK NO-ERROR.

       IF NOT AVAIL tt-it-container THEN DO.
          CREATE tt-it-container.
          ASSIGN tt-it-container.it-codigo = pp-it-container.it-codigo
                 tt-it-container.desc-item = pp-it-container.desc-item
                 tt-it-container.cod-refer = pp-it-container.cod-refer .
       END.
       ASSIGN tt-it-container.qt-pedida = tt-it-container.qt-pedida + pp-it-container.qt-pedida.

   END.

   {&OPEN-QUERY-br-it-container}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-it-container
&Scoped-define SELF-NAME br-it-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-it-container w-digita
ON LEAVE OF br-it-container IN FRAME F-Main /* Itens do Container */
DO:
  ASSIGN tt-it-container.preco-compra:READ-ONLY IN BROWSE br-it-container = YES.
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
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-moeda
          INPUT FRAME {&FRAME-NAME} fi-dt-cotacao.

   FIND FIRST tt-it-container WHERE
              tt-it-container.preco-compra = 0 NO-LOCK NO-ERROR.

   IF AVAIL tt-it-container THEN DO.
      MESSAGE 'Existem Itens sem Preáos, Favor Verificar...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.
  
   IF fi-dt-cotacao = ? THEN DO.
      MESSAGE 'Favor informar a Data da Cotaá∆o...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO fi-dt-cotacao.
      RETURN NO-APPLY.
   END.

   MESSAGE "Confirma Gerar Oc's com os Itens do Container " pp-container.nr-container "?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-ok AS LOGICAL.

   IF l-ok THEN RUN pi-cria-oc.

   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-preco
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-preco w-digita
ON CHOOSE OF bt-preco IN FRAME F-Main
DO:
   ASSIGN tt-it-container.preco-compra:READ-ONLY IN BROWSE br-it-container = NO.
   APPLY "ENTRY" TO tt-it-container.preco-compra IN BROWSE br-it-container.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-emit-forn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-emit-forn w-digita
ON LEAVE OF fi-cod-emit-forn IN FRAME F-Main /* Fornecedor */
DO:
    FIND emitente WHERE
         emitente.identific <> 1 AND 
         emitente.cod-emitente =  pp-container.cod-emit-forn NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       ASSIGN fi-nome-forn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-comprador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-comprador w-digita
ON LEAVE OF fi-comprador IN FRAME F-Main /* Comprador */
DO:
  FIND comprador WHERE 
       comprador.cod-comprado = pp-container.comprador NO-LOCK NO-ERROR.
  IF AVAIL comprador THEN
      ASSIGN fi-nome-compr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = comprador.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-cotacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-cotacao w-digita
ON LEAVE OF fi-dt-cotacao IN FRAME F-Main /* Data Cotaá∆o */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-moeda
         INPUT FRAME {&FRAME-NAME} fi-dt-cotacao.
  MESSAGE "moeda:" fi-moeda SKIP
          "periodo:" STRING(YEAR(fi-dt-cotacao)) + STRING(MONTH(fi-dt-cotacao)) 

      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  FIND cotacao WHERE
       cotacao.mo-codigo = fi-moeda AND
       cotacao.ano-periodo = STRING(YEAR(fi-dt-cotacao)) + STRING(MONTH(fi-dt-cotacao),'99') 
       NO-LOCK NO-ERROR.
  IF NOT AVAIL cotacao THEN DO.
     MESSAGE 'Cotaá∆o n∆o Cadastrada para a Moeda/Data Informada, Verifique...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'ENTRY' TO fi-moeda.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-cotacao:SCREEN-VALUE = STRING(cotacao.cotacao[DAY(fi-dt-cotacao)]).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-moeda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-moeda w-digita
ON ENTRY OF fi-moeda IN FRAME F-Main /* Moeda */
DO:
   FIND moeda WHERE
        moeda.mo-codigo = INPUT FRAME {&FRAME-NAME} fi-moeda NO-LOCK NO-ERROR.
   IF AVAIL moeda THEN
      ASSIGN fi-desc-moeda:SCREEN-VALUE = moeda.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-moeda w-digita
ON LEAVE OF fi-moeda IN FRAME F-Main /* Moeda */
DO:
   FIND moeda WHERE
        moeda.mo-codigo = INPUT FRAME {&FRAME-NAME} fi-moeda NO-LOCK NO-ERROR.
   IF NOT AVAIL moeda THEN DO.
      MESSAGE 'Moeda n∆o Cadastrada...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-desc-moeda:SCREEN-VALUE = moeda.descricao.

   APPLY 'LEAVE' TO fi-dt-cotacao.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-container
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
ASSIGN tt-it-container.preco-compra:READ-ONLY IN BROWSE br-it-container = YES.
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
  DISPLAY fi-cod-emit-forn fi-nome-forn fi-comprador fi-nome-compr fi-dt-compra 
          fi-dt-prev-chegada fi-moeda fi-desc-moeda fi-tot-container 
          fi-dt-cotacao fi-cotacao 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 RECT-31 RECT-32 RECT-33 br-container fi-cod-emit-forn 
         fi-comprador fi-dt-compra fi-dt-prev-chegada br-it-container fi-moeda 
         bt-preco fi-dt-cotacao bt-ok bt-cancelar bt-ajuda 
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
  ASSIGN fi-moeda = 1
         fi-dt-cotacao = TODAY - 2.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'VALUE-CHANGED' TO br-container IN FRAME {&FRAME-NAME}.
  APPLY 'LEAVE' TO fi-moeda.
  APPLY 'LEAVE' TO fi-dt-cotacao.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-importacao w-digita 
PROCEDURE pi-cria-importacao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        /*
        def new global shared var gi-numero-ordem-i  like ordem-compra.numero-ordem no-undo.
        def new global shared var gi-cod-emitente-i  like emitente.cod-emitente     no-undo.
        def new global shared var gc-it-codigo-i     like item.it-codigo            no-undo.
        def new global shared var gi-seq-cotac-i     like cotacao-item.seq-cotac    no-undo.
        def new global shared var gi-num-pedido-i    like pedido-compr.num-pedido   no-undo.
        def new global shared var gc-opcao-im0060    as   char                      no-undo.
        def new global shared var gl-returnok        as   logical                   no-undo.
        def new global shared var gde-aliquota-ipi-i as  decimal                   no-undo.
        def new global shared var gl-regime-i        as   logical                   no-undo.

        FIND pedido-compr 1448.
        FIND ordem-compra OF pedido-compr WHERE 
             ordem-compra.numero-ordem = 208600.


            assign gi-numero-ordem-i  = ordem-compra.numero-ordem
                   gi-cod-emitente-i  = pedido-compr.cod-emitente
                   gc-it-codigo-i     = ordem-compra.it-codigo
                   gi-seq-cotac-i     = 20
                   gi-num-pedido-i    = ordem-compra.num-pedido
                   gc-opcao-im0060    = "create":U
                   gde-aliquota-ipi-i = 12.

            run imp/im0060.w.
        */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-oc w-digita 
PROCEDURE pi-cria-oc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND pedido-compr WHERE 
         ROWID(pedido-compr) = gr-row-in295 NO-LOCK NO-ERROR.

    FIND FIRST ordem-compra OF pedido-compr NO-LOCK NO-ERROR.

    IF AVAIL ordem-compra THEN DO.
       MESSAGE 'J† existe Ordem de Compra para esse Pedido...' SKIP(1)
               'Confirma Geraá∆o de Ordens ?'
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           UPDATE l-ok AS LOGICAL.

       IF NOT l-ok THEN NEXT.
    END.

    FOR EACH tt-it-container NO-LOCK.
        RUN esapi/cria-ordem-compra.p (INPUT pedido-compr.num-pedido,
                                       INPUT tt-it-container.it-codigo,
                                       INPUT tt-it-container.cod-refer,
                                       INPUT tt-it-container.qt-pedida,
                                       INPUT tt-it-container.preco-compra).

        RUN pi-cria-importacao.
    END.

    FOR EACH ordem-compra OF pedido-compr NO-LOCK.
        FIND cotacao-item OF ordem-compra NO-LOCK NO-ERROR.
        IF NOT AVAIL cotacao-item THEN
           RUN esapi/cria-cotacao-item.p (INPUT ordem-compra.numero-ordem,
                                          INPUT fi-moeda,
                                          INPUT fi-dt-cotacao).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais w-digita 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-container = 0.
    FOR EACH tt-it-container NO-LOCK.
        ASSIGN fi-tot-container = fi-tot-container + tt-it-container.qt-pedida.
    END.

    DISP fi-tot-container 
         WITH FRAME {&FRAME-NAME}.
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
  {src/adm/template/snd-list.i "tt-it-container"}
  {src/adm/template/snd-list.i "pp-container"}

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

