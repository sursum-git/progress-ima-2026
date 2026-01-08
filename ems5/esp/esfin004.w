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
//{esinc\esfin003.i}

DEF TEMP-TABLE tt-movto 
    FIELD data              AS DATE FORMAT "99/99/9999"
    FIELD origem            AS CHAR
    FIELD base              AS CHAR
    FIELD cod_emitente      AS INT
    FIELD desc_emitente     AS CHAR FORMAT "x(15)"
    FIELD cod-tit-acr       LIKE tit_acr.cod_tit_acr
    FIELD parcela           LIKE tit_acr.cod_parcela
    FIELD valor-tit         LIKE tit_acr.val_origin_tit_acr
    FIELD valor-mov         LIKE tit_acr.val_origin_tit_acr
    FIELD dat-vencto-tit    LIKE tit_acr.dat_vencto_tit_acr
    FIELD dat-vencto-ori    LIKE tit_acr.dat_vencto_tit_acr
    FIELD dat-liquid        LIKE tit_acr.dat_liquidac_tit_acr
    FIELD resultado         AS CHAR
    FIELD num_id_tit        LIKE tit_acr.num_id_tit_acr 
    FIELD num_renegoc       LIKE renegoc_acr.num_renegoc_cobr_acr
    FIELD tipo-trans        LIKE movto_tit_acr.ind_trans_acr_ab
    INDEX indice1 data origem base cod_emitente. 

DEF TEMP-TABLE tt-tip-fluxo-financ LIKE tip_fluxo_financ.

/* Parameters Definitions ---                                           */
DEF BUFFER b-tt-movto FOR tt-movto.

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR v_rec_tit_ap AS RECID FORMAT ">>>>>>9":U INITIAL ? NO-UNDO.
DEF NEW GLOBAL SHARED VAR v_rec_tit_acr AS RECID FORMAT ">>>>>>9":U INITIAL ? NO-UNDO.
DEF NEW GLOBAL SHARED VAR v_rec_renegoc_acr AS RECID FORMAT ">>>>>>9":U INITIAL ? NO-UNDO.

DEFINE VARIABLE cbaseOutra          AS CHARACTER   NO-UNDO FORMAT 'x(15)' .
DEFINE VARIABLE cbaseAtual          AS CHARACTER   NO-UNDO FORMAT 'x(15)' .
DEFINE VARIABLE arquivoSaida        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAmbiente           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cConexao            AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cServidor           AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
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
DEF VAR i-ct        AS INT.

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
DEF VAR c-desc-trans AS CHAR FORMAT "x(50)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-movto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-movto

/* Definitions for BROWSE br-movto                                      */
&Scoped-define FIELDS-IN-QUERY-br-movto tt-movto.data tt-movto.base tt-movto.desc_emitente tt-movto.cod-tit-acr tt-movto.parcela tt-movto.valor-tit tt-movto.valor-mov tt-movto.dat-vencto-ori tt-movto.dat-vencto-tit tt-movto.dat-liquid fn-tipo-trans() @ c-desc-trans /* cod_empresa cod_estab conta_corrente DESC_conta tipo cc DESC_cc rowidNota id_movto_corren sequencia grupo_emitente classificacao grupo cCusto_Gerencial cod_modulo cod_param_desemb cod_param_desemb_cCusto */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-movto   
&Scoped-define SELF-NAME br-movto
&Scoped-define OPEN-QUERY-br-movto RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-movto WHERE                                  (tt-movto.tipo-trans = cb-analise OR cb-analise = 'TODAS') AND                                  (tt-movto.base = rs-base OR rs-base = 'A')                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-movto tt-movto
&Scoped-define FIRST-TABLE-IN-QUERY-br-movto tt-movto


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-movto}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-movto cb-analise fi-dt-inicio fi-dt-final ~
rs-base bt-processa bt-ajuda bt-ok bt-hist RECT-1 RECT-12 RECT-17 IMAGE-1 ~
IMAGE-2 RECT-18 
&Scoped-Define DISPLAYED-OBJECTS cb-analise fi-resultado fi-dt-inicio ~
fi-dt-final rs-base 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-tipo-trans w-digita 
FUNCTION fn-tipo-trans RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-det-renegoc 
     LABEL "Renegocia‡ao" 
     SIZE 13.14 BY 1.25.

DEFINE BUTTON bt-det-tit 
     LABEL "Det. T¡tulo" 
     SIZE 13.14 BY 1.25.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 7.14 BY 1.25 TOOLTIP "Gerar Planilha".

DEFINE BUTTON bt-hist 
     LABEL "Hist¢rico" 
     SIZE 13.14 BY 1.25.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Sair" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-processa 
     IMAGE-UP FILE "image/im-sav.bmp":U
     LABEL "OK" 
     SIZE 6 BY 1 TOOLTIP "Salva Altera‡äes".

DEFINE VARIABLE cb-analise AS CHARACTER FORMAT "X(256)":U INITIAL "TODAS" 
     LABEL "An lise" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "TODAS","Todas",
                     "Acerto Valor a Cr‚dito","AVCR",
                     "Acerto Valor a D‚bito","AVDB",
                     "Acerto Valor a Maior","AVMA",
                     "Acerto Valor a Menor","AVMN",
                     "Altera‡Æo Data Vencimento","ADVN",
                     "Corre‡Æo de Valor","CVAL",
                     "Corre‡Æo Valor na Liquida‡Æo","CVLL",
                     "Liquida‡Æo Enctro Ctas","LQEC",
                     "Liquida‡Æo Perda Dedut¡vel","LQPD",
                     "Liquida‡Æo Renegocia‡Æo","LQRN",
                     "Liquida‡Æo Transf Estabelecimento","LQTE",
                     "Renegocia‡Æo","REN",
                     "Desconto Banc rio","DCTO",
                     "Despesa Financeira","DESF"
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE fi-dt-final AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-inicio AS DATE FORMAT "99/99/9999":U 
     LABEL "Per¡odo" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-resultado AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .79
     FONT 6 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-base AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "10", "10",
"12", "12",
"Ambas", "A"
     SIZE 24.43 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 135.86 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.86 BY 1.38.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 3.75.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 3.75.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-movto FOR 
      tt-movto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-movto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-movto w-digita _FREEFORM
  QUERY br-movto NO-LOCK DISPLAY
      tt-movto.data                                       COLUMN-LABEL "Data"         WIDTH 11  
      tt-movto.base                                       COLUMN-LABEL "Base"         WIDTH 4   
      tt-movto.desc_emitente                              COLUMN-LABEL "Nome Emit"    WIDTH 15  
      tt-movto.cod-tit-acr                                COLUMN-LABEL "T¡tulo"       WIDTH 10  
      tt-movto.parcela                                    COLUMN-LABEL "Par"          WIDTH 3   
      tt-movto.valor-tit       FORMAT "->>>,>>>,>>9.99"   COLUMN-LABEL "Valor ORI"    WIDTH 10  
      tt-movto.valor-mov       FORMAT "->>>,>>>,>>9.99"   COLUMN-LABEL "Valor"        WIDTH 10  
      tt-movto.dat-vencto-ori                             COLUMN-LABEL "Venc ORI"     WIDTH 11  
      tt-movto.dat-vencto-tit                             COLUMN-LABEL "Dt Venc"      WIDTH 11  
      tt-movto.dat-liquid                                 COLUMN-LABEL "Dt Liquid"    WIDTH 11  
      fn-tipo-trans() @ c-desc-trans                      COLUMN-LABEL "Tipo Trans"   WIDTH 30  
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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 136 BY 15.25
         FONT 1
         TITLE "Movimentos do Per¡odo" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-movto AT ROW 5.25 COL 2 WIDGET-ID 200
     cb-analise AT ROW 1.83 COL 61 COLON-ALIGNED WIDGET-ID 122
     fi-resultado AT ROW 20.83 COL 94.57 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     bt-det-tit AT ROW 20.75 COL 2 WIDGET-ID 80
     fi-dt-inicio AT ROW 2.75 COL 9 COLON-ALIGNED WIDGET-ID 2
     fi-dt-final AT ROW 2.75 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     rs-base AT ROW 3.54 COL 68.57 NO-LABEL WIDGET-ID 18
     bt-processa AT ROW 2.67 COL 43 WIDGET-ID 12
     bt-excel AT ROW 20.75 COL 44 WIDGET-ID 74
     bt-ajuda AT ROW 22.29 COL 125.29
     bt-ok AT ROW 22.29 COL 3
     bt-det-renegoc AT ROW 20.75 COL 30 WIDGET-ID 126
     bt-hist AT ROW 20.75 COL 16 WIDGET-ID 128
     " Base" VIEW-AS TEXT
          SIZE 4.57 BY .5 AT ROW 3 COL 64 WIDGET-ID 26
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4 WIDGET-ID 110
     "Resultado:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 21 COL 87.14 WIDGET-ID 106
          FONT 6
     " Filtros Dinƒmicos" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 1 COL 55 WIDGET-ID 114
     RECT-1 AT ROW 22.08 COL 2.14
     RECT-12 AT ROW 3.25 COL 63.14 WIDGET-ID 42
     RECT-17 AT ROW 1.25 COL 2 WIDGET-ID 90
     IMAGE-1 AT ROW 2.79 COL 23 WIDGET-ID 92
     IMAGE-2 AT ROW 2.79 COL 27.29 WIDGET-ID 94
     RECT-18 AT ROW 1.25 COL 53 WIDGET-ID 124
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 137.57 BY 22.79
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
         TITLE              = "T¡tulos ACR Renegociados - ESFIN004.w"
         HEIGHT             = 22.79
         WIDTH              = 137.57
         MAX-HEIGHT         = 26.33
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 26.33
         VIRTUAL-WIDTH      = 182.86
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
/* SETTINGS FOR BUTTON bt-det-renegoc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-det-tit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-excel IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN fi-resultado IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-movto
/* Query rebuild information for BROWSE br-movto
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-movto WHERE
                                 (tt-movto.tipo-trans = cb-analise OR cb-analise = 'TODAS') AND
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
ON END-ERROR OF w-digita /* T¡tulos ACR Renegociados - ESFIN004.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* T¡tulos ACR Renegociados - ESFIN004.w */
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
   APPLY 'CHOOSE' TO bt-det-tit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-digita
ON ROW-DISPLAY OF br-movto IN FRAME F-Main /* Movimentos do Per¡odo */
DO:
   /*
   ASSIGN i-cor = 2.

   ASSIGN tt-movto.data:FGCOLOR IN BROWSE br-movto = i-cor
          tt-movto.base:FGCOLOR IN BROWSE br-movto = i-cor
          tt-movto.cod_emitente:FGCOLOR IN BROWSE br-movto = i-cor
          tt-movto.desc_emitente:FGCOLOR IN BROWSE br-movto = i-cor
          tt-movto.valor:FGCOLOR IN BROWSE br-movto = i-cor
          tt-movto.dat-vencto-ori:FGCOLOR IN BROWSE br-movto = i-cor     
          tt-movto.dat-vencto-tit:FGCOLOR IN BROWSE br-movto = i-cor     
          tt-movto.dat-liquid:FGCOLOR IN BROWSE br-movto = i-cor         
          c-desc-trans:FGCOLOR IN BROWSE br-movto = i-cor. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-movto w-digita
ON VALUE-CHANGED OF br-movto IN FRAME F-Main /* Movimentos do Per¡odo */
DO:
  ASSIGN bt-det-tit:SENSITIVE = NO
         bt-hist:SENSITIVE = NO
         bt-det-renegoc:SENSITIVE = NO
         bt-excel:SENSITIVE = NO.


  EMPTY TEMP-TABLE tt-tip-fluxo-financ.

  IF AVAIL tt-movto THEN DO.
     ASSIGN bt-excel:SENSITIVE = YES.

     FIND ems5.tit_acr WHERE
          ems5.tit_acr.num_id_tit_acr = tt-movto.num_id_tit NO-LOCK NO-ERROR.
     IF AVAIL ems5.tit_acr THEN
        ASSIGN bt-det-tit:SENSITIVE = YES.

     FIND FIRST histor_movto_tit_acr WHERE 
                histor_movto_tit_acr.cod_estab = tit_acr.cod_estab AND
                histor_movto_tit_acr.num_id_tit_acr = tit_acr.num_id_tit_acr AND
                histor_movto_tit_acr.ind_orig_histor_acr <> "Erro" NO-LOCK NO-ERROR.
     IF AVAIL histor_movto_tit_acr THEN
        ASSIGN bt-hist:SENSITIVE = YES.

     IF tt-movto.num_renegoc <> 0 THEN
        ASSIGN bt-det-renegoc:SENSITIVE = YES.

     EMPTY TEMP-TABLE tt-tip-fluxo-financ.
     FOR EACH val_tit_acr OF tit_acr NO-LOCK.
         FIND tip_fluxo_financ OF val_tit_acr NO-LOCK NO-ERROR.

         CREATE tt-tip-fluxo-financ.
         BUFFER-COPY tip_fluxo_financ TO tt-tip-fluxo-financ.
     END.
  END.

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


&Scoped-define SELF-NAME bt-det-renegoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-renegoc w-digita
ON CHOOSE OF bt-det-renegoc IN FRAME F-Main /* Renegocia‡ao */
DO:
   IF cbaseAtual <> tt-movto.base THEN DO.
      MESSAGE 'Para Detalhar esse Substitui‡Æo, favor acessar a Base ' + tt-movto.base
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN NO-APPLY.
   END.

   FIND renegoc_acr WHERE 
        renegoc_acr.num_renegoc_cobr_acr = tt-movto.num_renegoc NO-LOCK NO-ERROR.

   IF AVAIL renegoc_acr THEN DO.
      ASSIGN v_rec_renegoc_acr = RECID(renegoc_acr).
      
      RUN prgfin/acr/acr242aa.p.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tit w-digita
ON CHOOSE OF bt-det-tit IN FRAME F-Main /* Det. T¡tulo */
DO:
   IF cbaseAtual <> tt-movto.base THEN DO.
      MESSAGE 'Para Detalhar esse T¡tulo, favor acessar a Base ' + tt-movto.base
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN NO-APPLY.
   END.


   ASSIGN v_rec_tit_acr = ?.
   FIND ems5.tit_acr WHERE
        ems5.tit_acr.num_id_tit_acr = tt-movto.num_id_tit NO-LOCK NO-ERROR.
   IF AVAIL ems5.tit_acr THEN DO.
      ASSIGN v_rec_tit_acr = RECID(ems5.tit_acr).

      RUN prgfin/acr/acr212aa.p.
   END.
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
  FOR EACH b-tt-movto.
      RUN pi-acompanhar IN h-acomp (INPUT "Data: " + STRING(b-tt-movto.data,"99/99/9999") + "  Fornec: " + STRING(b-tt-movto.cod_emit) ).

      ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = b-tt-movto.origem
             chworksheet:range("B" + STRING(i-lin)):VALUE = b-tt-movto.data
             chworksheet:range("C" + STRING(i-lin)):VALUE = b-tt-movto.base
             chworksheet:range("D" + STRING(i-lin)):VALUE = b-tt-movto.cod_emitente
             chworksheet:range("E" + STRING(i-lin)):VALUE = b-tt-movto.desc_emitente
             chworksheet:range("G" + STRING(i-lin)):VALUE = b-tt-movto.valor-tit.
      ASSIGN i-lin = i-lin + 1.
  END.

  RUN pi-finalizar in h-acomp.
  MESSAGE 'Planilha Gerada com Sucesso...'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-hist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-hist w-digita
ON CHOOSE OF bt-hist IN FRAME F-Main /* Hist¢rico */
DO:
   IF cbaseAtual <> tt-movto.base THEN DO.
      MESSAGE 'Para Hist¢rico desse T¡tulo, favor acessar a Base ' + tt-movto.base
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN NO-APPLY.
   END.

   ASSIGN v_rec_tit_acr = ?.
   FIND ems5.tit_acr WHERE
        ems5.tit_acr.num_id_tit_acr = tt-movto.num_id_tit NO-LOCK NO-ERROR.
   IF AVAIL ems5.tit_acr THEN DO.
      ASSIGN v_rec_tit_acr = RECID(ems5.tit_acr).

      RUN esp/esfin004a.p.

   END.
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
    RUN pi-processa.

    {&OPEN-QUERY-br-movto}
    APPLY 'VALUE-CHANGED' TO br-movto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-analise
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-analise w-digita
ON VALUE-CHANGED OF cb-analise IN FRAME F-Main /* An lise */
DO:
    ASSIGN INPUT FRAME {&FRAME-NAME} cb-analise.
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
   ASSIGN INPUT FRAME {&FRAME-NAME} rs-base.

   {&OPEN-QUERY-br-movto}
   APPLY 'VALUE-CHANGED' TO br-movto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY cb-analise fi-resultado fi-dt-inicio fi-dt-final rs-base 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE br-movto cb-analise fi-dt-inicio fi-dt-final rs-base bt-processa 
         bt-ajuda bt-ok bt-hist RECT-1 RECT-12 RECT-17 IMAGE-1 IMAGE-2 RECT-18 
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

  {utp/ut9000.i "ESFIN004" "9.99.99.999"}

  ASSIGN fi-dt-inicio = TODAY - DAY(TODAY) + 1
         fi-dt-final = TODAY.
                    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN esapi/busca-base.p (OUTPUT c-base).
  CASE c-base:
     WHEN 'base-pro' THEN
        ASSIGN cAmbiente    = '1'
               cbaseAtual   = '10'
               cServidor    = '192.168.0.44'
               c-banco-ems5 = '-db ems5 -ld ems5bkp -H 192.168.0.4 -S 30032'.
     WHEN 'base-bkp' THEN
        ASSIGN cAmbiente    = '3'
               cbaseAtual   = '12'
               cServidor    = '192.168.0.4'
               c-banco-ems5 = '-db ems5 -ld ems5bkp -H 192.168.0.44 -S 10032'.
     WHEN 'base-tst' THEN
        ASSIGN cAmbiente    = '2'
               cbaseAtual   = '10'
               cServidor    = '192.168.0.44'
               c-banco-ems5 = '-db ems5 -ld ems5bkp -H 192.168.0.4 -S 20032'.
     WHEN 'base-bkt' THEN
        ASSIGN cAmbiente    = '4'
               cbaseAtual   = '12'
               cServidor    = '192.168.0.4'
               c-banco-ems5 = '-db ems5 -ld ems5bkp -H 192.168.0.44 -S 40032'.
  END CASE.
  
  RUN pi-processa.
  {&OPEN-QUERY-br-movto}
  APPLY 'VALUE-CHANGED' TO br-movto IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-movto-b10 w-digita 
PROCEDURE pi-busca-movto-b10 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
Transa‡Æo                                   Sigla
--------------------------------------  -----
Acerto Valor a Cr‚dito                  AVCR
Acerto Valor a D‚bito                   AVDB
Acerto Valor a Maior                    AVMA
Acerto Valor a Menor                    AVMN
Altera‡Æo Data Vencimento                   ADVN
Corre‡Æo de Valor                           CVAL
Corre‡Æo Valor na Liquida‡Æo            CVLL

Devolu‡Æo                                   DEV
Estorno Acerto Val Cr‚dito                  EVCR
Estorno Acerto Val D‚bito                   EVDB
Estorno Acerto Val Maior                    EVMA
Estorno Acerto Val Menor                    EVMN
Estorno Corre‡Æo Valor                  ECVL
Estorno Corre‡Æo Val Liquida‡Æo         ECLQ
Estorno de Liquida‡Æo                   ELIQ
Estorno de T¡tulo                           ESTT
Estorno Liquid Renegocia‡Æo                 ELQR
Estorno Liquid Transf estabelecimento   ELQT
Estorno Renegocia‡Æo                    EREN
Estorno Transf Estabelecimento          ETRE
Estorno Transf Unid Negocio                 ETUN
Implanta‡Æo                                 IMPL
Implanta‡Æo a Cr‚dito                   IMCR
Implanta‡Æo a D‚bito                    IMDB
Liquida‡Æo                                  LIQ
Liquida‡Æo Enctro Ctas                  LQEC
Liquida‡Æo Perda Dedut¡vel                  LQPD
Liquida‡Æo Renegocia‡Æo                 LQRN
Liquida‡Æo Transf Estabelecimento           LQTE
Renegocia‡Æo                            REN
Transf Estabelecimento                  TRES
Transf Unidade Neg¢cio                  TRUN
Desconto Banc rio                           DCTO
Estorno Desconto Banc rio                   EDCT
Despesa Financeira                          DESF
Estorno Desp Financeira                 EDES
*/

DEF INPUT PARAMETER p-trans AS CHAR.

FOR EACH movto_tit_acr WHERE
         movto_tit_acr.cod_estab = '501' AND
         movto_tit_acr.ind_trans_acr_ab = p-trans AND
         movto_tit_acr.dat_trans >= fi-dt-inicio AND
         movto_tit_acr.dat_trans <= fi-dt-final 
         USE-INDEX mvtttcr_estab_trans_dtf NO-LOCK .

    FIND tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.
    FIND FIRST ems5.cliente WHERE
               ems5.cliente.cdn_cliente = tit_acr.cdn_cliente NO-LOCK NO-ERROR.

    CREATE tt-movto.
    ASSIGN tt-movto.data           = movto_tit_acr.dat_trans
           tt-movto.base           = '10'
           tt-movto.cod_emitente   = ems5.tit_acr.cdn_cliente
           tt-movto.desc_emitente  = ems5.cliente.nom_abrev
           tt-movto.cod-tit-acr    = tit_acr.cod_tit_acr
           tt-movto.parcela        = tit_acr.cod_parcela
           tt-movto.valor-tit      = tit_acr.val_origin_tit_acr
           tt-movto.valor-mov      = movto_tit_acr.val_movto_tit_acr
           tt-movto.num_id_tit     = tit_acr.num_id_tit_acr
           tt-movto.num_renegoc    = tit_acr.num_renegoc_cobr_acr
           tt-movto.tipo-trans     = movto_tit_acr.ind_trans_acr_ab
           tt-movto.dat-vencto-tit = tit_acr.dat_vencto_tit_acr
           tt-movto.dat-vencto-ori = tit_acr.dat_vencto_origin_tit_acr
           tt-movto.dat-liquid     = tit_acr.dat_liquidac_tit_acr.

    IF tt-movto.dat-liquid = 12.31.9999 THEN
       ASSIGN tt-movto.dat-liquid = ?.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-digita 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-inicio fi-dt-final cb-analise rs-base.

    SESSION:SET-WAIT-STATE("general":U).
    
    EMPTY TEMP-TABLE tt-movto.

    IF rs-base = '10' OR rs-base = 'A' THEN DO.
       IF cb-analise = 'Todas' THEN DO.
          DO i-ct = 2 TO NUM-ENTRIES(cb-analise:LIST-ITEM-PAIRS) BY 2.
             RUN pi-busca-movto-b10 (INPUT ENTRY(i-ct,cb-analise:LIST-ITEM-PAIRS)).
          END.
       END.
       ELSE
          RUN pi-busca-movto-b10 (INPUT cb-analise).
    END.

    
    IF rs-base = '12' OR rs-base = 'A' THEN DO.
       CONNECT VALUE(c-banco-ems5) NO-ERROR.

       IF cb-analise = 'Todas' THEN DO.
          DO i-ct = 2 TO NUM-ENTRIES(cb-analise:LIST-ITEM-PAIRS) BY 2.
             RUN esapi/esfin004a.p (INPUT ENTRY(i-ct,cb-analise:LIST-ITEM-PAIRS),
                                    INPUT fi-dt-inicio,
                                    INPUT fi-dt-final,
                                    INPUT-OUTPUT TABLE tt-movto).
          END.
       END.
       ELSE
          RUN esapi/esfin004a.p (INPUT cb-analise,
                                 INPUT fi-dt-inicio,
                                 INPUT fi-dt-final,
                                 INPUT-OUTPUT TABLE tt-movto).

       DISCONNECT ems5bkp.
    END.
    
    SESSION:SET-WAIT-STATE("":U).

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
    ASSIGN fi-resultado = 0.

    ASSIGN c-lst-origem = 'Todos'.

    FOR EACH tt-movto WHERE
            (tt-movto.base = rs-base OR rs-base = 'A') NO-LOCK.

        ASSIGN fi-resultado = fi-resultado + tt-movto.valor-tit.
    END.

    DISP fi-resultado
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-tipo-trans w-digita 
FUNCTION fn-tipo-trans RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE tt-movto.tipo-trans.
        WHEN "AVCR" THEN ASSIGN c-desc-trans = "Acerto Valor a Cr‚dito".                  
        WHEN "AVDB" THEN ASSIGN c-desc-trans = "Acerto Valor a D‚bito".                   
        WHEN "AVMA" THEN ASSIGN c-desc-trans = "Acerto Valor a Maior".                    
        WHEN "AVMN" THEN ASSIGN c-desc-trans = "Acerto Valor a Menor".                    
        WHEN "ADVN" THEN ASSIGN c-desc-trans = "Altera‡Æo Data Vencimento".
        WHEN "CVAL" THEN ASSIGN c-desc-trans = "Corre‡Æo de Valor".
        WHEN "CVLL" THEN ASSIGN c-desc-trans = "Corre‡Æo Valor na Liquida‡Æo".
        WHEN "DEV"  THEN ASSIGN c-desc-trans = "Devolu‡Æo".
        WHEN "EVCR" THEN ASSIGN c-desc-trans = "Estorno Acerto Val Cr‚dito".
        WHEN "EVDB" THEN ASSIGN c-desc-trans = "Estorno Acerto Val D‚bito".
        WHEN "EVMA" THEN ASSIGN c-desc-trans = "Estorno Acerto Val Maior".
        WHEN "EVMN" THEN ASSIGN c-desc-trans = "Estorno Acerto Val Menor".
        WHEN "ECVL" THEN ASSIGN c-desc-trans = "Estorno Corre‡Æo Valor".
        WHEN "ECLQ" THEN ASSIGN c-desc-trans = "Estorno Corre‡Æo Val Liquida‡Æo".
        WHEN "ELIQ" THEN ASSIGN c-desc-trans = "Estorno de Liquida‡Æo".
        WHEN "ESTT" THEN ASSIGN c-desc-trans = "Estorno de T¡tulo".
        WHEN "ELQR" THEN ASSIGN c-desc-trans = "Estorno Liquid Renegocia‡Æo".
        WHEN "ELQT" THEN ASSIGN c-desc-trans = "Estorno Liquid Transf estabelecimento".
        WHEN "EREN" THEN ASSIGN c-desc-trans = "Estorno Renegocia‡Æo".
        WHEN "ETRE" THEN ASSIGN c-desc-trans = "Estorno Transf Estabelecimento".
        WHEN "ETUN" THEN ASSIGN c-desc-trans = "Estorno Transf Unid Negocio".
        WHEN "IMPL" THEN ASSIGN c-desc-trans = "Implanta‡Æo".
        WHEN "IMCR" THEN ASSIGN c-desc-trans = "Implanta‡Æo a Cr‚dito".
        WHEN "IMDB" THEN ASSIGN c-desc-trans = "Implanta‡Æo a D‚bito".
        WHEN "LIQ"  THEN ASSIGN c-desc-trans = "Liquida‡Æo".
        WHEN "LQEC" THEN ASSIGN c-desc-trans = "Liquida‡Æo Enctro Ctas".
        WHEN "LQPD" THEN ASSIGN c-desc-trans = "Liquida‡Æo Perda Dedut¡vel".              
        WHEN "LQRN" THEN ASSIGN c-desc-trans = "Liquida‡Æo Renegocia‡Æo".                 
        WHEN "LQTE" THEN ASSIGN c-desc-trans = "Liquida‡Æo Transf Estabelecimento".       
        WHEN "REN"  THEN ASSIGN c-desc-trans = "Renegocia‡Æo".                            
        WHEN "TRES" THEN ASSIGN c-desc-trans = "Transf Estabelecimento".                  
        WHEN "TRUN" THEN ASSIGN c-desc-trans = "Transf Unidade Neg¢cio".                  
        WHEN "DCTO" THEN ASSIGN c-desc-trans = "Desconto Banc rio".                       
        WHEN "EDCT" THEN ASSIGN c-desc-trans = "Estorno Desconto Banc rio".               
        WHEN "DESF" THEN ASSIGN c-desc-trans = "Despesa Financeira".                      
        WHEN "EDES" THEN ASSIGN c-desc-trans = "Estorno Desp Financeira".
    END CASE.

    RETURN c-desc-trans.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

