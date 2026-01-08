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
{esinc\esfin003.i}

DEF TEMP-TABLE tt-movto LIKE tt
    FIELD resultado AS CHAR
    INDEX indice1 data cod_modulo origem base cod_emitente. 

DEF TEMP-TABLE tt-tip-fluxo-financ LIKE tip_fluxo_financ.

/* Parameters Definitions ---                                           */
DEF BUFFER b-tt-movto FOR tt-movto.

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR v_rec_tit_ap AS RECID FORMAT ">>>>>>9":U INITIAL ? NO-UNDO.
DEF NEW GLOBAL SHARED VAR v_rec_tit_acr AS RECID FORMAT ">>>>>>9":U INITIAL ? NO-UNDO.

DEFINE VARIABLE cbaseOutra          AS CHARACTER   NO-UNDO FORMAT 'x(15)' .
DEFINE VARIABLE cbaseAtual          AS CHARACTER   NO-UNDO FORMAT 'x(15)' .
DEFINE VARIABLE arquivoSaida        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAmbiente           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cConexao            AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE digitoAmbiente      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE servidorAmbiente    AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE c-banco-ems5        AS CHARACTER   NO-UNDO FORMAT 'x(10)'.
DEFINE VARIABLE cBaseDesconectar    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPortaDesconectar   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hLog AS HANDLE      NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.
DEFINE VARIABLE cPlanilha AS CHARACTER   NO-UNDO FORMAT 'x(120)'.

{utp/ut-glob.i}
DEF VAR h-acomp     AS HANDLE  NO-UNDO.
DEF VAR h-objeto    AS HANDLE.
DEF VAR c-base      AS CHAR.
DEF VAR i-cor       AS INT.

/* Variaveis para o Excel */
DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR chworksheet AS COM-HANDLE NO-UNDO.
DEF VAR i-lin       AS INTEGER.
DEF VAR c-lin       AS CHAR FORMAT "x(500)".
DEF VAR arq-saida   AS CHAR FORMAT "x(50)".
DEF VAR i-col       AS INTEGER.
DEF VAR c-col       AS CHAR INIT "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z".

DEF VAR c-lst-origem AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-fluxo

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-tip-fluxo-financ tt-movto

/* Definitions for BROWSE br-fluxo                                      */
&Scoped-define FIELDS-IN-QUERY-br-fluxo tt-tip-fluxo-financ.des_tip_fluxo_financ   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-fluxo   
&Scoped-define SELF-NAME br-fluxo
&Scoped-define QUERY-STRING-br-fluxo FOR EACH tt-tip-fluxo-financ NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-fluxo OPEN QUERY {&SELF-NAME} FOR EACH tt-tip-fluxo-financ NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-fluxo tt-tip-fluxo-financ
&Scoped-define FIRST-TABLE-IN-QUERY-br-fluxo tt-tip-fluxo-financ


/* Definitions for BROWSE br-movto                                      */
&Scoped-define FIELDS-IN-QUERY-br-movto tt-movto.origem tt-movto.data tt-movto.base tt-movto.cod_emitente tt-movto.desc_emitente tt-movto.conta_contabil tt-movto.valor /* cod_empresa cod_estab conta_corrente DESC_conta tipo cc DESC_cc rowidNota id_movto_corren sequencia grupo_emitente classificacao grupo cCusto_Gerencial cod_modulo cod_param_desemb cod_param_desemb_cCusto */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-movto   
&Scoped-define SELF-NAME br-movto
&Scoped-define OPEN-QUERY-br-movto RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-movto WHERE                                  (tt-movto.origem = cb-origem OR cb-origem = 'TODAS') AND                                  (tt-movto.base = rs-base OR rs-base = 'A')                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-movto tt-movto
&Scoped-define FIRST-TABLE-IN-QUERY-br-movto tt-movto


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-fluxo}~
    ~{&OPEN-QUERY-br-movto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-movto br-fluxo ed-desc-conta fi-credito ~
rs-analise fi-dt-inicio fi-dt-final bt-processa bt-ajuda bt-ok fi-debito ~
RECT-1 RECT-12 RECT-17 IMAGE-1 IMAGE-2 RECT-18 
&Scoped-Define DISPLAYED-OBJECTS ed-desc-conta cb-origem fi-credito ~
fi-resultado rs-analise fi-dt-inicio fi-dt-final rs-base fi-debito 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-detalhe 
     IMAGE-UP FILE "image/img-det.bmp":U
     LABEL "Button 1" 
     SIZE 13.14 BY 1.25.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 13.14 BY 1.25 TOOLTIP "Gerar Planilha".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Sair" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-processa 
     IMAGE-UP FILE "image/im-sav.bmp":U
     LABEL "OK" 
     SIZE 6 BY 3.25 TOOLTIP "Salva Altera‡äes".

DEFINE VARIABLE cb-origem AS CHARACTER FORMAT "X(256)":U 
     LABEL "Origem" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 23.72 BY 1 NO-UNDO.

DEFINE VARIABLE ed-desc-conta AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 30 BY 6.5 NO-UNDO.

DEFINE VARIABLE fi-credito AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     BGCOLOR 2 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-debito AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88
     BGCOLOR 12 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-dt-final AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-inicio AS DATE FORMAT "99/99/9999":U 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-resultado AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .79
     FONT 6 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-analise AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Liquida‡Æo", 2,
"EmissÆo", 1
     SIZE 12 BY 1.75 NO-UNDO.

DEFINE VARIABLE rs-base AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "10", "10",
"12", "12",
"Ambas", "A"
     SIZE 8.86 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 136.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.57 BY 3.38.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 4.25.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 4.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-fluxo FOR 
      tt-tip-fluxo-financ SCROLLING.

DEFINE QUERY br-movto FOR 
      tt-movto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-fluxo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-fluxo w-digita _FREEFORM
  QUERY br-fluxo NO-LOCK DISPLAY
      tt-tip-fluxo-financ.des_tip_fluxo_financ COLUMN-LABEL "Tipo Fluxo"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 31 BY 6.75
         FONT 1
         TITLE "Fluxo Financeiro".

DEFINE BROWSE br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-movto w-digita _FREEFORM
  QUERY br-movto NO-LOCK DISPLAY
      tt-movto.origem                                     COLUMN-LABEL "Origem"       WIDTH 18  COLUMN-FONT 6
      tt-movto.data                                       COLUMN-LABEL "Data"         WIDTH 11  COLUMN-FONT 6
      tt-movto.base                                       COLUMN-LABEL "Base"         WIDTH 3.5 COLUMN-FONT 6
      tt-movto.cod_emitente                               COLUMN-LABEL "Emitente"     WIDTH 8   COLUMN-FONT 6
      tt-movto.desc_emitente                              COLUMN-LABEL "Nome Emit"    WIDTH 35  COLUMN-FONT 6
      tt-movto.conta_contabil                             COLUMN-LABEL "Conta Contab" WIDTH 10  COLUMN-FONT 6
      tt-movto.valor           FORMAT "->>>,>>>,>>9.99"   COLUMN-LABEL "Valor"        WIDTH 13  COLUMN-FONT 6
      
/*                          
    
    cod_empresa      
    cod_estab        
    conta_corrente   
    DESC_conta       
    tipo             
    cc               
    DESC_cc          
    
    rowidNota        
    id_movto_corren  
    
    sequencia        
    grupo_emitente   
    classificacao    
    grupo            
    cCusto_Gerencial 
    cod_modulo       
    cod_param_desemb 
    cod_param_desemb_cCusto
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 14.75
         FONT 1
         TITLE "Movimentos do Per¡odo" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-movto AT ROW 5.75 COL 2 WIDGET-ID 200
     br-fluxo AT ROW 5.75 COL 109 WIDGET-ID 300
     ed-desc-conta AT ROW 14 COL 109 NO-LABEL WIDGET-ID 118
     cb-origem AT ROW 2.75 COL 67.29 COLON-ALIGNED WIDGET-ID 108
     fi-credito AT ROW 20.83 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 98
     fi-resultado AT ROW 20.83 COL 94.57 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     bt-detalhe AT ROW 20.75 COL 2 WIDGET-ID 80
     rs-analise AT ROW 3.25 COL 17 NO-LABEL WIDGET-ID 76
     fi-dt-inicio AT ROW 1.75 COL 15 COLON-ALIGNED WIDGET-ID 2
     fi-dt-final AT ROW 1.75 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     rs-base AT ROW 2.25 COL 103 NO-LABEL WIDGET-ID 18
     bt-processa AT ROW 1.75 COL 51.86 WIDGET-ID 12
     bt-excel AT ROW 20.75 COL 16 WIDGET-ID 74
     bt-ajuda AT ROW 22.25 COL 125.86
     bt-ok AT ROW 22.29 COL 3
     fi-debito AT ROW 20.83 COL 65.72 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     " Base" VIEW-AS TEXT
          SIZE 4.57 BY .5 AT ROW 1.5 COL 100.29 WIDGET-ID 26
     " Descri‡Æo da Conta" VIEW-AS TEXT
          SIZE 30 BY .54 AT ROW 13.5 COL 109 WIDGET-ID 120
          BGCOLOR 3 FGCOLOR 15 FONT 6
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4 WIDGET-ID 110
     " Filtros Dinƒmicos" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 1 COL 63 WIDGET-ID 114
     "Resultado:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 21 COL 87.14 WIDGET-ID 106
          FONT 6
     " An lise:" VIEW-AS TEXT
          SIZE 6.14 BY .67 AT ROW 3.33 COL 10.29 WIDGET-ID 54
     "Cr‚dito:" VIEW-AS TEXT
          SIZE 6.57 BY .54 AT ROW 21 COL 35 WIDGET-ID 102
          FONT 6
     "D‚bito:" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 21 COL 61.29 WIDGET-ID 104
          FONT 6
     RECT-1 AT ROW 22.08 COL 2.14
     RECT-12 AT ROW 1.75 COL 99.43 WIDGET-ID 42
     RECT-17 AT ROW 1.25 COL 2 WIDGET-ID 90
     IMAGE-1 AT ROW 1.79 COL 29 WIDGET-ID 92
     IMAGE-2 AT ROW 1.79 COL 33.29 WIDGET-ID 94
     RECT-18 AT ROW 1.25 COL 61 WIDGET-ID 112
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 139 BY 22.79
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
         TITLE              = "Demonstrativo de Resultados - ESFIN003.w"
         HEIGHT             = 22.79
         WIDTH              = 139
         MAX-HEIGHT         = 22.79
         MAX-WIDTH          = 139
         VIRTUAL-HEIGHT     = 22.79
         VIRTUAL-WIDTH      = 139
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
/* BROWSE-TAB br-movto 1 F-Main */
/* BROWSE-TAB br-fluxo br-movto F-Main */
/* SETTINGS FOR BUTTON bt-detalhe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-excel IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR COMBO-BOX cb-origem IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       ed-desc-conta:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN fi-resultado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rs-base IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-fluxo
/* Query rebuild information for BROWSE br-fluxo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-tip-fluxo-financ NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-fluxo */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-movto
/* Query rebuild information for BROWSE br-movto
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-movto WHERE
                                 (tt-movto.origem = cb-origem OR cb-origem = 'TODAS') AND
                                 (tt-movto.base = rs-base OR rs-base = 'A')
                                 NO-LOCK INDEXED-REPOSITION
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-movto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Demonstrativo de Resultados - ESFIN003.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Demonstrativo de Resultados - ESFIN003.w */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-movto
&Scoped-define SELF-NAME br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-digita
ON MOUSE-SELECT-DBLCLICK OF br-movto IN FRAME F-Main /* Movimentos do Per¡odo */
DO:
   APPLY 'CHOOSE' TO bt-detalhe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-digita
ON ROW-DISPLAY OF br-movto IN FRAME F-Main /* Movimentos do Per¡odo */
DO:

   IF tt-movto.tipo = 'SAI' THEN
      ASSIGN i-cor = 12.
   ELSE
      ASSIGN i-cor = 2.

   ASSIGN tt-movto.origem:FGCOLOR IN BROWSE br-movto = i-cor           
          tt-movto.data:FGCOLOR = i-cor
          tt-movto.base:FGCOLOR = i-cor
          tt-movto.cod_emitente:FGCOLOR = i-cor
          tt-movto.desc_emitente:FGCOLOR = i-cor
          tt-movto.conta_contabil:FGCOLOR = i-cor
          tt-movto.valor:FGCOLOR = i-cor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-digita
ON VALUE-CHANGED OF br-movto IN FRAME F-Main /* Movimentos do Per¡odo */
DO:
  ASSIGN cb-origem:SENSITIVE = NO
         rs-base:SENSITIVE = NO
         bt-detalhe:SENSITIVE = NO
         bt-excel:SENSITIVE = NO.

  ASSIGN ed-desc-conta:SCREEN-VALUE = ''.

  EMPTY TEMP-TABLE tt-tip-fluxo-financ.

  IF AVAIL tt-movto THEN DO.
     ASSIGN cb-origem:SENSITIVE = YES
            rs-base:SENSITIVE = YES
            bt-excel:SENSITIVE = YES.

     ASSIGN ed-desc-conta:SCREEN-VALUE = tt-movto.desc_conta.

     CASE tt-movto.cod_modulo:
         WHEN 'APB'  THEN DO.
             FIND ems5.tit_ap WHERE
                  ems5.tit_ap.num_id_tit_ap = tt-movto.num_id_tit NO-LOCK NO-ERROR.
             IF AVAIL ems5.tit_ap THEN
                ASSIGN bt-detalhe:SENSITIVE = YES.

             FOR EACH val_tit_ap OF tit_ap NO-LOCK.
                 FIND tip_fluxo_financ OF val_tit_ap NO-LOCK NO-ERROR.

                 CREATE tt-tip-fluxo-financ.
                 BUFFER-COPY tip_fluxo_financ TO tt-tip-fluxo-financ.
             END.
         END.
         WHEN 'ACR'  THEN DO.
             FIND ems5.tit_acr WHERE
                  ems5.tit_acr.num_id_tit_acr = tt-movto.num_id_tit NO-LOCK NO-ERROR.
             IF AVAIL ems5.tit_acr THEN
                ASSIGN bt-detalhe:SENSITIVE = YES.

             EMPTY TEMP-TABLE tt-tip-fluxo-financ.
             FOR EACH val_tit_acr OF tit_acr NO-LOCK.
                 FIND tip_fluxo_financ OF val_tit_acr NO-LOCK NO-ERROR.

                 CREATE tt-tip-fluxo-financ.
                 BUFFER-COPY tip_fluxo_financ TO tt-tip-fluxo-financ.
             END.
         END.
     END CASE.
  END.

  {&OPEN-QUERY-br-fluxo}
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


&Scoped-define SELF-NAME bt-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe w-digita
ON CHOOSE OF bt-detalhe IN FRAME F-Main /* Button 1 */
DO:
  CASE tt-movto.cod_modulo:
      WHEN 'APB'  THEN DO.
          ASSIGN v_rec_tit_ap = ?.
          FIND ems5.tit_ap WHERE
               ems5.tit_ap.num_id_tit_ap = tt-movto.num_id_tit NO-LOCK NO-ERROR.
          IF AVAIL ems5.tit_ap THEN DO.
             ASSIGN v_rec_tit_ap = RECID(ems5.tit_ap).
             RUN prgfin/apb/apb222aa.p.
          END.
      END.
      WHEN 'ACR'  THEN DO.
          ASSIGN v_rec_tit_acr = ?.
          FIND ems5.tit_acr WHERE
               ems5.tit_acr.num_id_tit_acr = tt-movto.num_id_tit NO-LOCK NO-ERROR.
          IF AVAIL ems5.tit_acr THEN DO.
             ASSIGN v_rec_tit_acr = RECID(ems5.tit_acr).

             RUN prgfin/acr/acr212aa.p.
          END.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel w-digita
ON CHOOSE OF bt-excel IN FRAME F-Main /* Button 2 */
DO:
  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  RUN pi-inicializar IN h-acomp (INPUT "Processando..").

  CREATE "Excel.Application" chExcelApp NO-ERROR.
  IF chExcelApp <> ? THEN /* Cria a Planilha */
     ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar  Visivel */
            //chExcelApp:SheetsInNewWorkbook = 7 /* Nõ PLANILHAS A SEREM CRIADAS */
            chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
            chworksheet            = chExcelapp:sheets:ITEM(1).

  IF chExcelApp = ? THEN DO:
     MESSAGE "O Aplicativo EXCEL nÆo foi encontrado. NÆo ‚ possivel a execu‡Æo do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN arq-saida = "".
     RETURN.
  END.

  /* Nomear Aba da Planilha */
  /* Ativar a Planilha */
  chWorkSheet = chExcelapp:Sheets:ITEM(1).
  chWorkbook:Worksheets(1):activate.
  chExcelApp:ActiveWindow:Zoom = 100.

  ASSIGN chworksheet:range("A1"):VALUE = "DRE - Demonstrativo de Resultados".

  /* Configura Alinhamento Horizontal do Titulo da Planilha */
  ChWorkSheet:range("A1:K1"):SELECT().
  ChWorksheet:range("A1:K1"):Merge.
  Chworksheet:Range("A1:K1"):HorizontalAlignment =  3.

  /* Colorir Titulo da Planilha */
  chWorkSheet:Range("A1:L1"):FONT:ColorIndex     = 18. /* Avermelhado */
  chWorkSheet:Range("A1:L1"):Interior:ColorIndex = 2. /* Branco */

  /* Configura a Linha do Titulo da Planilha */
  ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 25
         chWorkSheet:Rows("2:2"):RowHeight =  4
         chWorkSheet:Rows("1:1"):FONT:SIZE = 12
         chWorkSheet:Rows("1:1"):FONT:bold = TRUE.

  ASSIGN chworksheet:range("A3"):VALUE = 'Data'
         chworksheet:range("B3"):VALUE = 'Base'
         chworksheet:range("C3"):VALUE = 'Emitente'
         chworksheet:range("D3"):VALUE = 'Nome Emitente'
         chworksheet:range("E3"):VALUE = 'Conta Contabil'
         chworksheet:range("F3"):VALUE = 'Valor'.

   /* Tamanho das Colunas */
   ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 20
          chWorkSheet:Columns("B"):ColumnWidth = 12
          chWorkSheet:Columns("C"):ColumnWidth = 5
          chWorkSheet:Columns("D"):ColumnWidth = 8
          chWorkSheet:Columns("E"):ColumnWidth = 40
          chWorkSheet:Columns("F"):ColumnWidth = 12
          chWorkSheet:Columns("G"):ColumnWidth = 12.

  chWorkSheet:Range("A3:L3"):SELECT().
  ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
         chExcelApp:SELECTION:FONT:SIZE               = 09
         chExcelApp:SELECTION:FONT:Bold               = TRUE 
         chExcelApp:SELECTION:FONT:ColorIndex         = 11.

  ASSIGN chworksheet:range("A:F"):NumberFormat        = "@".
  ASSIGN chworksheet:range("G:G"):NumberFormat        = "###.###.##0,00".

  ASSIGN i-lin = 4.
  FOR EACH b-tt-movto BREAK BY b-tt-movto.cod_modulo.
      RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(b-tt-movto.data,"99/99/9999") + "  Fornec: " + STRING(b-tt-movto.cod_emit) ).

      ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = b-tt-movto.origem
             chworksheet:range("B" + STRING(i-lin)):VALUE = b-tt-movto.data
             chworksheet:range("C" + STRING(i-lin)):VALUE = b-tt-movto.base
             chworksheet:range("D" + STRING(i-lin)):VALUE = b-tt-movto.cod_emitente
             chworksheet:range("E" + STRING(i-lin)):VALUE = b-tt-movto.desc_emitente
             chworksheet:range("F" + STRING(i-lin)):VALUE = b-tt-movto.conta_contabil
             chworksheet:range("G" + STRING(i-lin)):VALUE = b-tt-movto.valor.
      ASSIGN i-lin = i-lin + 1.
  END.

  RUN pi-finalizar in h-acomp.
  MESSAGE 'Planilha Gerada com Sucesso...'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* Sair */
DO:
  apply "close":U to this-procedure.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-processa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processa w-digita
ON CHOOSE OF bt-processa IN FRAME F-Main /* OK */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-inicio fi-dt-final rs-analise.

    SESSION:SET-WAIT-STATE("general":U).
    
    EMPTY TEMP-TABLE tt-movto.

    IF NOT CONNECTED("dbaux") THEN
       RUN esapi/connect-ima-med.p. 

    /*
    IF NOT CONNECTED("ems5bkp") THEN
       CONNECT VALUE(c-banco-ems5).
    */

    RUN pi-realizado.
    RUN pi-previsto.

    DISCONNECT dbaux.
    //DISCONNECT ems5bkp.
    
    ASSIGN c-lst-origem = 'TODAS'.
    FOR EACH tt-movto BREAK BY tt-movto.origem.
        IF FIRST-OF(tt-movto.origem) THEN
           ASSIGN c-lst-origem = c-lst-origem + ',' + tt-movto.origem.
    END.
    ASSIGN cb-origem:LIST-ITEMS IN FRAME {&FRAME-NAME} = c-lst-origem.

    ASSIGN cb-origem:SCREEN-VALUE = 'TODAS'.

    SESSION:SET-WAIT-STATE("":U).

    ASSIGN INPUT FRAME {&FRAME-NAME} cb-origem rs-analise.

    {&OPEN-QUERY-br-movto}
    APPLY 'VALUE-CHANGED' TO br-movto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-origem w-digita
ON VALUE-CHANGED OF cb-origem IN FRAME F-Main /* Origem */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} cb-origem rs-base.

    {&OPEN-QUERY-br-movto}
    APPLY 'VALUE-CHANGED' TO br-movto.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-inicio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-inicio w-digita
ON LEAVE OF fi-dt-inicio IN FRAME F-Main /* Per¡odo */
DO:
   IF fi-dt-inicio <> INPUT fi-dt-inicio THEN
      ASSIGN fi-dt-final:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-base
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-base w-digita
ON VALUE-CHANGED OF rs-base IN FRAME F-Main
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} cb-origem rs-base.
   {&OPEN-QUERY-br-movto}
   APPLY 'VALUE-CHANGED' TO br-movto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-fluxo
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
br-movto:LOAD-MOUSE-POINTER("image/detail.cur").

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
  DISPLAY ed-desc-conta cb-origem fi-credito fi-resultado rs-analise 
          fi-dt-inicio fi-dt-final rs-base fi-debito 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE br-movto br-fluxo ed-desc-conta fi-credito rs-analise fi-dt-inicio 
         fi-dt-final bt-processa bt-ajuda bt-ok fi-debito RECT-1 RECT-12 
         RECT-17 IMAGE-1 IMAGE-2 RECT-18 
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

  {utp/ut9000.i "ESFIN003" "9.99.99.999"}

  ASSIGN fi-dt-inicio = TODAY - DAY(TODAY) + 1
         fi-dt-final = TODAY.
                    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN h-objeto = FRAME f-main:FIRST-CHILD.
  ASSIGN h-objeto = h-objeto:FIRST-CHILD.
  DO WHILE VALID-HANDLE(h-objeto).
     IF h-objeto:TYPE = 'LITERAL' OR 
        h-objeto:TYPE = 'FILL-IN' THEN DO.
        IF h-objeto:ROW >= 20 AND
           h-objeto:ROW <= 21 THEN
           ASSIGN h-objeto:FONT = 6.

        IF h-objeto:ROW = 13.50 THEN
           ASSIGN h-objeto:FONT = 6
                  h-objeto:FGCOLOR = 15
                  h-objeto:BGCOLOR = 3.

     END.
     ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
  END.
  ASSIGN ed-desc-conta:FONT = 6.
         

  
  RUN esapi/busca-base.p (OUTPUT c-base).
  CASE c-base:
     WHEN 'base-pro' THEN
        ASSIGN digitoAmbiente     = '1'
               servidorAmbiente   = '192.168.0.44'
               c-banco-ems5       = '-db ems5 -ld ems5bkp -H 192.168.0.4 -S 30032'.
     WHEN 'base-bkp' THEN
        ASSIGN digitoAmbiente     = '3'
               servidorAmbiente   = '192.168.0.4'
               c-banco-ems5       = '-db ems5 -ld ems5bkp -H 192.168.0.44 -S 10032'.
     WHEN 'base-tst' THEN
        ASSIGN digitoAmbiente     = '2'
               servidorAmbiente   = '192.168.0.44'
               c-banco-ems5       = '-db ems5 -ld ems5bkp -H 192.168.0.4 -S 20032'.
     WHEN 'base-bkt' THEN
        ASSIGN digitoAmbiente     = '4'
               servidorAmbiente   = '192.168.0.4'
               c-banco-ems5       = '-db ems5 -ld ems5bkp -H 192.168.0.44 -S 40032'.
  END CASE.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-previsto w-digita 
PROCEDURE pi-previsto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    EMPTY TEMP-TABLE tt.
    RUN esp/esfin003a.p (INPUT fi-dt-inicio,
                         INPUT fi-dt-final,
                         INPUT rs-analise,
                         INPUT-OUTPUT TABLE tt). 
    FOR EACH tt.
        CREATE tt-movto.
        BUFFER-COPY tt TO tt-movto
             ASSIGN tt-movto.resultado = 'P'.

        ASSIGN tt-movto.tipo = 'ENT'.
        IF tt-movto.valor < 0 THEN
           ASSIGN tt-movto.tipo = 'SAI'.
    END.
    /*
    EMPTY TEMP-TABLE tt.
    RUN esp/esfin003aBKP.p (INPUT fi-dt-inicio,
                            INPUT fi-dt-final,
                            INPUT rs-analise,
                            INPUT-OUTPUT TABLE tt). 
    FOR EACH tt.
        CREATE tt-movto.
        BUFFER-COPY tt TO tt-movto
             ASSIGN tt-movto.resultado = 'P'.

        ASSIGN tt-movto.tipo = 'ENT'.
        IF tt-movto.valor < 0 THEN
           ASSIGN tt-movto.tipo = 'SAI'.
    END.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-realizado w-digita 
PROCEDURE pi-realizado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt.
    RUN esp/esfin003b.p (INPUT fi-dt-inicio,
                         INPUT fi-dt-final,
                         INPUT rs-analise,
                         INPUT-OUTPUT TABLE tt). 
    FOR EACH tt.
        CREATE tt-movto.
        BUFFER-COPY tt TO tt-movto
            ASSIGN tt-movto.resultado = 'R'.

        ASSIGN tt-movto.tipo = 'ENT'.
        IF tt-movto.valor < 0 THEN
           ASSIGN tt-movto.tipo = 'SAI'.
    END.
    /*
    EMPTY TEMP-TABLE tt.
    RUN esp/esfin003bBKP.p (INPUT fi-dt-inicio,
                            INPUT fi-dt-final,
                            INPUT rs-analise,
                            INPUT-OUTPUT TABLE tt). 
    FOR EACH tt.
        CREATE tt-movto.
        BUFFER-COPY tt TO tt-movto
            ASSIGN tt-movto.resultado = 'R'.

        ASSIGN tt-movto.tipo = 'ENT'.
        IF tt-movto.valor < 0 THEN
           ASSIGN tt-movto.tipo = 'SAI'.
    END.
    */
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
    ASSIGN fi-credito = 0
           fi-debito = 0
           fi-resultado = 0.

    ASSIGN c-lst-origem = 'Todos'.

    FOR EACH tt-movto WHERE
            (tt-movto.origem = cb-origem OR cb-origem = 'TODAS') AND
            (tt-movto.base = rs-base OR rs-base = 'A') NO-LOCK.

        IF tt-movto.tipo = 'SAI' THEN
           ASSIGN fi-debito = fi-debito + tt-movto.valor.
        ELSE
           ASSIGN fi-credito = fi-credito + tt-movto.valor.

        ASSIGN fi-resultado = fi-resultado + tt-movto.valor.
    END.

    DISP fi-credito
         fi-debito
         fi-resultado
         WITH FRAME {&FRAME-NAME}.

    ASSIGN fi-resultado:FGCOLOR  = 2.
    IF fi-resultado < 0 THEN
       ASSIGN fi-resultado:FGCOLOR = 12.


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
  {src/adm/template/snd-list.i "tt-movto"}
  {src/adm/template/snd-list.i "tt-tip-fluxo-financ"}

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

