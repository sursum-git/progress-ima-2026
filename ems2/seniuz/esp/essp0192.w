&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
** AUTOR: FµBIO COELHO LANZA - OUTUBRO 2009
*******************************************************************************/
{include/i-prgvrs.i ESSP0192 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
/* Global Variable Definitions ---                                       */

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-ordem-benefic
    FIELD cod-estabel LIKE ordem-benefic.cod-estabel
    FIELD nr-ob       LIKE ordem-benefic.nr-ob
    FIELD dt-ob       LIKE ordem-benefic.dt-ob
    FIELD nr-carro    LIKE ordem-benefic.nr-carro
    FIELD num-trf     AS INT
    FIELD tipo-ob     AS CHAR FORMAT "x(16)"
    FIELD status-ob   AS CHAR FORMAT "x(15)"
    FIELD resp        AS CHAR  
    FIELD it-codigo   LIKE ob-etiqueta.it-codigo
    FIELD cod-refer   LIKE ob-etiqueta.cod-refer
    FIELD qtd-plan    AS DEC
    FIELD qtd-rp      AS DEC
    FIELD qtd-rd      AS DEC
    FIELD qtd-ca      AS DEC
    FIELD qtd-ret-m   AS DEC
    FIELD qtd-ret-kg  AS DEC
    FIELD c-corrente  AS DEC
    FIELD perc-rp     AS DEC FORMAT "->,>>>,>>9.99" 
    FIELD perc-rd     AS DEC FORMAT "->,>>>,>>9.99" 
    FIELD perc-ca     AS DEC FORMAT "->,>>>,>>9.99" 
    FIELD perc-m      AS DEC FORMAT "->,>>>,>>9.99".

DEF BUFFER b-tt-ordem-benefic FOR tt-ordem-benefic.                                                         

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp  AS HANDLE NO-UNDO.
DEF VAR h-query  AS HANDLE.
DEF VAR c-titulo AS CHAR.
DEF VAR c-empresa AS CHAR.

/* Variavies de ParÉmetros */
DEFINE VAR c-cod-estabel-ini AS CHAR INIT "1".
DEFINE VAR c-cod-estabel-fin AS CHAR INIT "1".
DEFINE VAR da-dt-movto-ini   LIKE ob-etiqueta.dt-emissao.
DEFINE VAR da-dt-movto-fin   LIKE ob-etiqueta.dt-emissao.
DEFINE VAR i-nr-ob-ini       LIKE ordem-benefic.nr-ob INITIAL 0.
DEFINE VAR i-nr-ob-fin       LIKE ordem-benefic.nr-ob INITIAL 999999999.
DEFINE VAR c-it-codigo-ini   LIKE ob-etiqueta.it-codigo INIT "".
DEFINE VAR c-it-codigo-fin   LIKE ob-etiqueta.it-codigo INIT "ZZZZZZ".
DEFINE VAR c-cod-refer-ini   LIKE ob-etiqueta.cod-refer INIT "". 
DEFINE VAR c-cod-refer-fin   LIKE ob-etiqueta.cod-refer INIT "ZZZZZZZ".
DEFINE VAR l-todas-obs     AS LOG INITIAL YES.
DEFINE VAR l-producao      AS LOG INITIAL NO.
DEFINE VAR l-retrabalho    AS LOG INITIAL NO.
DEFINE VAR l-transf        AS LOG INITIAL NO.
DEFINE VAR l-indust        AS LOG INITIAL NO.
DEFINE VAR l-todos-status  AS LOG INITIAL YES.
DEFINE VAR l-disponivel    AS LOG INITIAL NO.
DEFINE VAR l-em-revisao    AS LOG INITIAL NO.
DEFINE VAR l-rev-parcial   AS LOG INITIAL NO.
DEFINE VAR l-rev-total     AS LOG INITIAL NO.
DEFINE VAR l-reportado     AS LOG INITIAL NO.
DEFINE VAR l-pendente      AS LOG INITIAL NO.
DEFINE VAR l-executada     AS LOG INITIAL NO.
DEFINE VAR l-ok            AS LOG.

 /* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.
DEFINE VAR i-Lin        AS INT.
DEFINE VAR i-pag        AS INT.
DEFINE VAR i-ct         AS INT.

/* Variaveis para o Excel */
DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF VAR chworksheet AS COM-HANDLE NO-UNDO.
DEF VAR c-lin       AS CHAR FORMAT "x(500)".
DEF VAR arq-saida   AS CHAR FORMAT "x(50)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-tt-ordem-benefic

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ordem-benefic

/* Definitions for BROWSE br-tt-ordem-benefic                           */
&Scoped-define FIELDS-IN-QUERY-br-tt-ordem-benefic tt-ordem-benefic.nr-ob tt-ordem-benefic.dt-ob STRING(tt-ordem-benefic.num-trf, ">>>>>>") tt-ordem-benefic.it-codigo tt-ordem-benefic.cod-refer tt-ordem-benefic.qtd-plan tt-ordem-benefic.qtd-rp tt-ordem-benefic.perc-rp tt-ordem-benefic.qtd-rd tt-ordem-benefic.perc-rd tt-ordem-benefic.qtd-ca tt-ordem-benefic.perc-ca tt-ordem-benefic.qtd-ret-m tt-ordem-benefic.perc-m tt-ordem-benefic.qtd-ret-kg tt-ordem-benefic.c-corrente tt-ordem-benefic.tipo-ob tt-ordem-benefic.status-ob UPPER(tt-ordem-benefic.resp)   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tt-ordem-benefic   
&Scoped-define SELF-NAME br-tt-ordem-benefic
&Scoped-define OPEN-QUERY-br-tt-ordem-benefic RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-ordem-benefic WHERE tt-ordem-benefic.nr-ob > 0  NO-LOCK                               BY tt-ordem-benefic.dt-ob                               BY tt-ordem-benefic.it-codigo                               BY tt-ordem-benefic.cod-refer.
&Scoped-define TABLES-IN-QUERY-br-tt-ordem-benefic tt-ordem-benefic
&Scoped-define FIRST-TABLE-IN-QUERY-br-tt-ordem-benefic tt-ordem-benefic


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-tt-ordem-benefic}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-tt-ordem-benefic bt-param ~
bt-vapara bt-rp bt-imp-analitico bt-imprime bt-excel bt-exit bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-planejado fi-estoque-rp fi-estoque-rd ~
fi-estoque-ca fi-retalho-m fi-retalho-kg fi-estoque-total fi-perc-tot-rp ~
fi-perc-tot-rd fi-perc-tot-ca fi-perc-tot-m fi-conta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 4.86 BY 1.29 TOOLTIP "Help on Line"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.29 TOOLTIP "Gerar Planilha".

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imp-analitico 
     IMAGE-UP FILE "image/ii-pri.bmp":U
     LABEL "Button 1" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir as O.B. Relat¢rio Anal°tico".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprimir as Ordem de Beneficiamento".

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-rp 
     IMAGE-UP FILE "adeicon/rbuild%.ico":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Consultar as Etiquetas que comp‰em as OB."
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Posicionar na Etiqueta"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-conta AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "CTA CORRENTE" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-estoque-ca AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Estoque CA" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-estoque-rd AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Estoque RD" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1.

DEFINE VARIABLE fi-estoque-rp AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Estoque RP" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-estoque-total AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Estoque TOTAL" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FGCOLOR 9 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-tot-ca AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "% Etq. CA" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE fi-perc-tot-m AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "% Etq. M" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE fi-perc-tot-rd AS DECIMAL FORMAT ">>,>>9.99":U INITIAL 0 
     LABEL "% Etq. RD" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE fi-perc-tot-rp AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "% Etq. RP" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE fi-planejado AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Planejado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FGCOLOR 9 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-retalho-kg AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Retalho KG" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-retalho-m AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Retalho M" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88
     FONT 1 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 7 BY 20
     BGCOLOR 8 FGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-tt-ordem-benefic FOR 
      tt-ordem-benefic SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-tt-ordem-benefic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tt-ordem-benefic C-Win _FREEFORM
  QUERY br-tt-ordem-benefic NO-LOCK DISPLAY
      tt-ordem-benefic.nr-ob                       COLUMN-LABEL "OB"  
      tt-ordem-benefic.dt-ob                       COLUMN-LABEL "Data OB"
      STRING(tt-ordem-benefic.num-trf, ">>>>>>")   COLUMN-LABEL "Nß da Trf"
      tt-ordem-benefic.it-codigo                   COLUMN-LABEL "Item"         WIDTH 6
      tt-ordem-benefic.cod-refer                   COLUMN-LABEL "Referància"
      tt-ordem-benefic.qtd-plan                    COLUMN-LABEL "Planejado"    
      tt-ordem-benefic.qtd-rp                      COLUMN-LABEL "Estoque RP"   WIDTH 10
      tt-ordem-benefic.perc-rp                     COLUMN-LABEL "% RP"         WIDTH 8
      tt-ordem-benefic.qtd-rd                      COLUMN-LABEL "Estoque RD"   WIDTH 9.4
      tt-ordem-benefic.perc-rd                     COLUMN-LABEL "% RD"         WIDTH 8
      tt-ordem-benefic.qtd-ca                      COLUMN-LABEL "Estoque CA"   WIDTH 8
      tt-ordem-benefic.perc-ca                     COLUMN-LABEL "% CA"         WIDTH 8
      tt-ordem-benefic.qtd-ret-m                   COLUMN-LABEL "Retalho M "   WIDTH 8
      tt-ordem-benefic.perc-m                      COLUMN-LABEL "% M"          WIDTH 8
      tt-ordem-benefic.qtd-ret-kg                  COLUMN-LABEL "Retalho KG"   WIDTH 8
      tt-ordem-benefic.c-corrente                  COLUMN-LABEL "   C/C    "   WIDTH 9
      tt-ordem-benefic.tipo-ob                     COLUMN-LABEL "Tipo"         WIDTH 12
      tt-ordem-benefic.status-ob                   COLUMN-LABEL "Status"       WIDTH 11
      UPPER(tt-ordem-benefic.resp)                 COLUMN-LABEL "Responsavel"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 128.86 BY 17.25
         FONT 4
         TITLE "Ordens de Beneficiamento Selecionadas" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-tt-ordem-benefic AT ROW 1.25 COL 2.14
     bt-param AT ROW 1.54 COL 132.72
     bt-vapara AT ROW 2.83 COL 132.72
     bt-rp AT ROW 4.25 COL 132.72
     bt-imp-analitico AT ROW 8.08 COL 132.72 WIDGET-ID 12
     bt-imprime AT ROW 9.33 COL 132.72
     bt-excel AT ROW 10.63 COL 132.72
     bt-exit AT ROW 18.21 COL 132.72
     fi-planejado AT ROW 19 COL 13.14 COLON-ALIGNED
     fi-estoque-rp AT ROW 19 COL 34.57 COLON-ALIGNED
     fi-estoque-rd AT ROW 19 COL 57.43 COLON-ALIGNED
     fi-estoque-ca AT ROW 19 COL 77.72 COLON-ALIGNED
     fi-retalho-m AT ROW 19 COL 96.86 COLON-ALIGNED
     fi-retalho-kg AT ROW 19 COL 117.29 COLON-ALIGNED
     bt-ajuda AT ROW 19.54 COL 132.72
     fi-estoque-total AT ROW 20 COL 13.14 COLON-ALIGNED
     fi-perc-tot-rp AT ROW 20 COL 34.57 COLON-ALIGNED WIDGET-ID 2
     fi-perc-tot-rd AT ROW 20 COL 57.57 COLON-ALIGNED WIDGET-ID 4
     fi-perc-tot-ca AT ROW 20 COL 77.72 COLON-ALIGNED WIDGET-ID 6
     fi-perc-tot-m AT ROW 20 COL 97 COLON-ALIGNED WIDGET-ID 10
     fi-conta AT ROW 21 COL 13.14 COLON-ALIGNED
     rt-buttom AT ROW 1.25 COL 131.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 139 BY 21.58
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Analise Gerencial da PRODUÄ«O X ESTOQUE"
         COLUMN             = 13.43
         ROW                = 6.54
         HEIGHT             = 21.71
         WIDTH              = 138.43
         MAX-HEIGHT         = 29.79
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.79
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         FONT               = 1
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB br-tt-ordem-benefic rt-buttom DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-conta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estoque-ca IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estoque-rd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estoque-rp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-estoque-total IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-tot-ca IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-tot-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-tot-rd IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-tot-rp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-planejado IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-retalho-kg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-retalho-m IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tt-ordem-benefic
/* Query rebuild information for BROWSE br-tt-ordem-benefic
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-ordem-benefic WHERE tt-ordem-benefic.nr-ob > 0  NO-LOCK
                              BY tt-ordem-benefic.dt-ob
                              BY tt-ordem-benefic.it-codigo
                              BY tt-ordem-benefic.cod-refer.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgmov.ped-venda.cod-sit-ped = 1"
     _Query            is OPENED
*/  /* BROWSE br-tt-ordem-benefic */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Analise Gerencial da PRODUÄ«O X ESTOQUE */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Analise Gerencial da PRODUÄ«O X ESTOQUE */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-tt-ordem-benefic
&Scoped-define SELF-NAME br-tt-ordem-benefic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tt-ordem-benefic C-Win
ON ROW-DISPLAY OF br-tt-ordem-benefic IN FRAME DEFAULT-FRAME /* Ordens de Beneficiamento Selecionadas */
DO:
  IF tt-ordem-benefic.c-corrente < 0 THEN 
     tt-ordem-benefic.c-corrente:FGCOLOR IN BROWSE br-tt-ordem-benefic = 12. 

  
  IF tt-ordem-benefic.nr-ob <= 0 THEN DO:
     tt-ordem-benefic.dt-ob:FGCOLOR IN BROWSE br-tt-ordem-benefic = 12. 
    /* tt-ordem-benefic.num-trf:FGCOLOR IN BROWSE br-tt-ordem-benefic = 12.  */
     tt-ordem-benefic.it-codigo:FGCOLOR IN BROWSE br-tt-ordem-benefic = 12. 
     tt-ordem-benefic.cod-refer:FGCOLOR IN BROWSE br-tt-ordem-benefic = 12. 
     tt-ordem-benefic.qtd-plan:FGCOLOR IN BROWSE br-tt-ordem-benefic = 12.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME DEFAULT-FRAME /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME DEFAULT-FRAME /* Button 2 */
DO:

   ASSIGN arq-saida = SESSION:TEMP-DIRECTORY + "Analise Gerencial PRODUÄ«O X ESTOQUE.XLS".
   RUN esdlg/d02essp0192.w (INPUT-OUTPUT arq-saida).
   IF arq-saida <> "" AND arq-saida <> "NOK" THEN DO:
      RUN pi-gera-excel.
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess†-lo,  abra-o atravÇs do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK. 
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit C-Win
ON CHOOSE OF bt-exit IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imp-analitico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imp-analitico C-Win
ON CHOOSE OF bt-imp-analitico IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN pi-imprime.
   ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN pi-imprime.
   ASSIGN c-win:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   RUN esp/essp0192a.w (INPUT-OUTPUT c-cod-estabel-ini,
                        INPUT-OUTPUT c-cod-estabel-fin,
                        INPUT-OUTPUT da-dt-movto-ini,   
                        INPUT-OUTPUT da-dt-movto-fin, 
                        INPUT-OUTPUT i-nr-ob-ini,
                        INPUT-OUTPUT i-nr-ob-fin,
                        INPUT-OUTPUT c-it-codigo-ini,   
                        INPUT-OUTPUT c-it-codigo-fin,   
                        INPUT-OUTPUT c-cod-refer-ini,
                        INPUT-OUTPUT c-cod-refer-fin,
                        INPUT-OUTPUT l-todas-obs,
                        INPUT-OUTPUT l-producao,
                        INPUT-OUTPUT l-retrabalho,
                        INPUT-OUTPUT l-transf,
                        INPUT-OUTPUT l-indust,
                        INPUT-OUTPUT l-todos-status,
                        INPUT-OUTPUT l-disponivel,
                        INPUT-OUTPUT l-em-revisao,
                        INPUT-OUTPUT l-rev-parcial,
                        INPUT-OUTPUT l-rev-total,
                        INPUT-OUTPUT l-reportado,
                        INPUT-OUTPUT l-pendente,
                        INPUT-OUTPUT l-executada,
                        INPUT-OUTPUT l-ok). 

   IF l-ok THEN                                     
      RUN pi-popula-browse.
   ASSIGN c-win:SENSITIVE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-rp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-rp C-Win
ON CHOOSE OF bt-rp IN FRAME DEFAULT-FRAME
DO:
   IF AVAIL tt-ordem-benefic THEN
      RUN esp/essp0192b.p (INPUT tt-ordem-benefic.cod-estabel,
                           INPUT tt-ordem-benefic.nr-ob,
                           INPUT tt-ordem-benefic.dt-ob,
                           INPUT tt-ordem-benefic.nr-carro).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara C-Win
ON CHOOSE OF bt-vapara IN FRAME DEFAULT-FRAME
DO:
    DEF VAR c-codigo AS CHAR.

    RUN esdlg/d01essp0192.w (OUTPUT c-codigo).

    IF c-codigo <> "" THEN DO:

       FIND FIRST tt-ordem-benefic WHERE
                  tt-ordem-benefic.nr-ob = INT(c-codigo) NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-ordem-benefic THEN DO.
          MESSAGE "Ordem de Beneficiamento n∆o est† contido na seleá∆o!"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.

       h-query:REPOSITION-TO-ROWID(ROWID(tt-ordem-benefic)) NO-ERROR. 
       APPLY 'VALUE-CHANGED' TO br-tt-ordem-benefic.

    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

STATUS INPUT OFF. /* Desliga Mensagem no RodapÇ da Tela */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON 'F5':U OF br-tt-ordem-benefic DO:
   {&OPEN-QUERY-br-tt-ordem-benefic}
   APPLY 'value-changed' TO br-tt-ordem-benefic.
END.

ASSIGN h-query = br-tt-ordem-benefic:QUERY.
br-tt-ordem-benefic:NUM-LOCKED-COLUMNS = 2.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  
   RUN enable_UI.

   ASSIGN da-dt-movto-ini = DATE(MONTH(TODAY),1,YEAR(TODAY))
          da-dt-movto-fin = TODAY.

   APPLY 'choose' TO bt-param.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-planejado fi-estoque-rp fi-estoque-rd fi-estoque-ca fi-retalho-m 
          fi-retalho-kg fi-estoque-total fi-perc-tot-rp fi-perc-tot-rd 
          fi-perc-tot-ca fi-perc-tot-m fi-conta 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rt-buttom br-tt-ordem-benefic bt-param bt-vapara bt-rp 
         bt-imp-analitico bt-imprime bt-excel bt-exit bt-ajuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel C-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 CREATE "Excel.Application" chExcelApp NO-ERROR.
 IF chExcelApp <> ? THEN /* Cria a Planilha */
    ASSIGN chExcelApp:VISIBLE     = TRUE  /* A Planilha Ficar† Visivel */
           chExcelApp:SheetsInNewWorkbook = 1 /* Nı PLANILHAS A SEREM CRIADAS */
           chWorkbook             = chExcelApp:Workbooks:ADD() /* Cria Planilha */
           chworksheet            = chExcelapp:sheets:ITEM(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel C-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pi-abre-excel.
  IF chExcelApp = ? THEN DO:
     MESSAGE "O Aplicativo EXCEL n∆o foi encontrado. N∆o Ç possivel a execuá∆o do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN arq-saida = "".
     RETURN.
  END.

  RUN pi-monta-planilha.

  /* Posiciona o Foco no Inicio da Planilha */
  chExcelApp:Range("A1"):SELECT.
  chExcelApp:Range("A:A"):EntireColumn:AutoFit.

  /* Posiciona na Planilha 1, Salva e Fecha */
  chWorkSheet = chExcelapp:Sheets:ITEM(1).
  chWorkbook:Worksheets(1):activate.

  /* Salva e Fecha Planilha */
  OS-DELETE VALUE(arq-saida).

  IF chExcelApp:Version BEGINS "8":U THEN 
     chWorkBook:SaveAs(arq-saida,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
  ELSE 
     chWorkBook:SaveAs(arq-saida,,,,,,,).  /* Salva na Vers∆o da planilha da ESTAÄ«O */

  chWorkBook:CLOSE().
  chExcelApp:QUIT().
  RELEASE OBJECT chExcelApp. 
  RELEASE OBJECT chworkBook.
  RELEASE OBJECT chworksheet.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec C-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  PUT c-empresa  FORMAT "X(40)"                 AT   1
      "DATA: "                                  AT  71
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  77
      "HORA: "                                  AT 104
      STRING(TIME,"hh:mm:ss")                   AT 110
      "PAG:"                                    AT 144
      i-pag FORMAT ">>>"                        AT 149
      SKIP(1).

  PUT c-titulo FORMAT "X(75)" AT 50 SKIP(1).

  PUT "Nß DA OB DATA DA OB Nß TRF   ITEM C.REFER     PLANEJADO ESTOQUE RP ESTOQUE RD ESTOQUE CA  RETALHO M RETALHO KG    C/CORRENTE TIPO DA OB  STATUS DA OB REPONSAVEL" AT 1.
  PUT "-------- ---------- ------ ------ ------- ------------- ---------- ---------- ---------- ---------- ---------- ------------- ---------- ------------- ----------" AT 1.
  ASSIGN i-pag = i-pag + 1.                                                                              


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 39.
          PUT CONTROL "~033&l1O~033(s16H". /* ORIENTAÄ«O PAISAGEM & COMPACTA */ 
          /* PUT CONTROL "~033&l2S~033(s16H". /* DUPLEX BORDA CURTA */ */
          /* PUT CONTROL "~033E~033(s21H". */ /* ORIENTAÄ«O RETRATO & COMPACTA */
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0191-sint.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag      =  1
            i-lin      = 99.

     ASSIGN fi-planejado  = 0 fi-estoque-rp = 0 fi-estoque-rd = 0 fi-estoque-ca    = 0
            fi-retalho-m  = 0 fi-retalho-kg = 0 fi-conta      = 0 fi-estoque-total = 0.

     FOR EACH b-tt-ordem-benefic NO-LOCK  
           BY b-tt-ordem-benefic.cod-estabel
           BY b-tt-ordem-benefic.dt-ob      
           BY b-tt-ordem-benefic.it-codigo  
           BY b-tt-ordem-benefic.cod-refer.

         IF b-tt-ordem-benefic.nr-ob <= 0  THEN NEXT.

         IF i-lin > 39 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.
    
         PUT b-tt-ordem-benefic.nr-ob      FORMAT ">>>>,>>9"      AT   1     
             b-tt-ordem-benefic.dt-ob      FORMAT "99/99/9999"    AT  10.

         IF b-tt-ordem-benefic.num-trf > 0 THEN
            PUT b-tt-ordem-benefic.num-trf    FORMAT ">>,>>9"        AT  21.

         PUT b-tt-ordem-benefic.it-codigo  FORMAT "x(6)"          AT  28
             b-tt-ordem-benefic.cod-refer  FORMAT "x(7)"          AT  35
             b-tt-ordem-benefic.qtd-plan   FORMAT ">>,>>>,>>9.99" AT  43 
             b-tt-ordem-benefic.qtd-rp     FORMAT ">>>,>>9.99"    AT  57 
             b-tt-ordem-benefic.qtd-rd     FORMAT ">>>,>>9.99"    AT  68 
             b-tt-ordem-benefic.qtd-ca     FORMAT ">>>,>>9.99"    AT  79 
             b-tt-ordem-benefic.qtd-ret-m  FORMAT ">>>,>>9.99"    AT  90 
             b-tt-ordem-benefic.qtd-ret-kg FORMAT ">>>,>>9.99"    AT 101 
             b-tt-ordem-benefic.c-corrente FORMAT "->,>>>,>>9.99" AT 112 
             b-tt-ordem-benefic.tipo-ob    FORMAT "x(10)"         AT 126 
             b-tt-ordem-benefic.status-ob  FORMAT "x(13)"         AT 137 
             b-tt-ordem-benefic.resp       FORMAT "x(10)"         AT 151.
          
         ASSIGN i-lin = i-lin + 1.

         ASSIGN fi-planejado  = fi-planejado  + b-tt-ordem-benefic.qtd-plan
                fi-estoque-rp = fi-estoque-rp + b-tt-ordem-benefic.qtd-rp
                fi-estoque-rd = fi-estoque-rd + b-tt-ordem-benefic.qtd-rd
                fi-estoque-ca = fi-estoque-ca + b-tt-ordem-benefic.qtd-ca
                fi-retalho-m  = fi-retalho-m  + b-tt-ordem-benefic.qtd-ret-m
                fi-retalho-kg = fi-retalho-kg + b-tt-ordem-benefic.qtd-ret-kg.
     END.

     IF i-lin > 39 THEN DO:
        RUN pi-imp-cabec.
        ASSIGN i-lin = 7.
     END.
     PUT "------------- ---------- ---------- ---------- ---------- ----------" AT 43.
     PUT "TOTAL GERAL.......:" AT 10.
     PUT fi-planejado   FORMAT ">>,>>>,>>9.99" AT  43 
         fi-estoque-rp  FORMAT ">>>,>>9.99"    AT  57 
         fi-estoque-rd  FORMAT ">>>,>>9.99"    AT  68 
         fi-estoque-ca  FORMAT ">>>,>>9.99"    AT  79 
         fi-retalho-m   FORMAT ">>>,>>9.99"    AT  90 
         fi-retalho-kg  FORMAT ">>>,>>9.99"    AT 101. 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-analitico C-Win 
PROCEDURE pi-imprime-analitico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 39.
          PUT CONTROL "~033&l1O~033(s16H". /* ORIENTAÄ«O PAISAGEM & COMPACTA */ 
          /* PUT CONTROL "~033&l2S~033(s16H". /* DUPLEX BORDA CURTA */ */
          /* PUT CONTROL "~033E~033(s21H". */ /* ORIENTAÄ«O RETRATO & COMPACTA */
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0191-sint.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag      =  1
            i-lin      = 99.

     ASSIGN fi-planejado  = 0 fi-estoque-rp = 0 fi-estoque-rd = 0 fi-estoque-ca    = 0
            fi-retalho-m  = 0 fi-retalho-kg = 0 fi-conta      = 0 fi-estoque-total = 0.

     FOR EACH b-tt-ordem-benefic NO-LOCK  
     BREAK BY b-tt-ordem-benefic.cod-estabel
           BY b-tt-ordem-benefic.dt-ob      
           BY b-tt-ordem-benefic.it-codigo  
           BY b-tt-ordem-benefic.cod-refer.

         IF b-tt-ordem-benefic.nr-ob <= 0  THEN NEXT.

         IF i-lin > 39 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.
    
         PUT b-tt-ordem-benefic.nr-ob      FORMAT ">>>>,>>9"      AT   1     
             b-tt-ordem-benefic.dt-ob      FORMAT "99/99/9999"    AT  10.

         IF b-tt-ordem-benefic.num-trf > 0 THEN
            PUT b-tt-ordem-benefic.num-trf    FORMAT ">>,>>9"        AT  21.

         PUT b-tt-ordem-benefic.it-codigo  FORMAT "x(6)"          AT  28
             b-tt-ordem-benefic.cod-refer  FORMAT "x(7)"          AT  35
             b-tt-ordem-benefic.qtd-plan   FORMAT ">>,>>>,>>9.99" AT  43 
             b-tt-ordem-benefic.qtd-rp     FORMAT ">>>,>>9.99"    AT  57 
             b-tt-ordem-benefic.qtd-rd     FORMAT ">>>,>>9.99"    AT  68 
             b-tt-ordem-benefic.qtd-ca     FORMAT ">>>,>>9.99"    AT  79 
             b-tt-ordem-benefic.qtd-ret-m  FORMAT ">>>,>>9.99"    AT  90 
             b-tt-ordem-benefic.qtd-ret-kg FORMAT ">>>,>>9.99"    AT 101 
             b-tt-ordem-benefic.c-corrente FORMAT "->,>>>,>>9.99" AT 112 
             b-tt-ordem-benefic.tipo-ob    FORMAT "x(10)"         AT 126 
             b-tt-ordem-benefic.status-ob  FORMAT "x(13)"         AT 137 
             b-tt-ordem-benefic.resp       FORMAT "x(10)"         AT 151.
          
         ASSIGN i-lin = i-lin + 1.

         ASSIGN fi-planejado  = fi-planejado  + b-tt-ordem-benefic.qtd-plan
                fi-estoque-rp = fi-estoque-rp + b-tt-ordem-benefic.qtd-rp
                fi-estoque-rd = fi-estoque-rd + b-tt-ordem-benefic.qtd-rd
                fi-estoque-ca = fi-estoque-ca + b-tt-ordem-benefic.qtd-ca
                fi-retalho-m  = fi-retalho-m  + b-tt-ordem-benefic.qtd-ret-m
                fi-retalho-kg = fi-retalho-kg + b-tt-ordem-benefic.qtd-ret-kg.
     END.

     IF i-lin > 39 THEN DO:
        RUN pi-imp-cabec.
        ASSIGN i-lin = 7.
     END.
     PUT "------------- ---------- ---------- ---------- ---------- ----------" AT 43.
     PUT "TOTAL GERAL.......:" AT 10.
     PUT fi-planejado   FORMAT ">>,>>>,>>9.99" AT  43 
         fi-estoque-rp  FORMAT ">>>,>>9.99"    AT  57 
         fi-estoque-rd  FORMAT ">>>,>>9.99"    AT  68 
         fi-estoque-ca  FORMAT ">>>,>>9.99"    AT  79 
         fi-retalho-m   FORMAT ">>>,>>9.99"    AT  90 
         fi-retalho-kg  FORMAT ">>>,>>9.99"    AT 101. 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha C-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /* Nomear Aba da Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(1).
 chWorkSheet:NAME = "Produá∆o X Estoque".
 chWorkSheet:TAB:ColorIndex = 19.

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(1).
 chWorkbook:Worksheets(1):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("A1"):VALUE = c-titulo.

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("A1:O1"):SELECT().
 ChWorksheet:range("A1:O1"):Merge.
 Chworksheet:Range("A1:O1"):HorizontalAlignment =  3.
 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:O1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:O1"):Interior:ColorIndex = 2. /* Branco */

 /* Configura a Linha do Titulo da Planilha */
 ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 25
        chWorkSheet:Rows("2:2"):RowHeight =  4
        chWorkSheet:Rows("1:1"):FONT:SIZE = 13
        chWorkSheet:Rows("1:1"):FONT:bold = TRUE.

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A3"):VALUE = "Nß OB"
        chworksheet:range("B3"):VALUE = "DATA OB"    
        chworksheet:range("C3"):VALUE = "Nß TRF"  
        chworksheet:range("D3"):VALUE = "ITEM"
        chworksheet:range("E3"):VALUE = "REFER“NCIA"     
        chworksheet:range("F3"):VALUE = "PLANEJADO" 
        chworksheet:range("G3"):VALUE = "ESTOQUE RP" 
        chworksheet:range("H3"):VALUE = "ESTOQUE RD"
        chworksheet:range("I3"):VALUE = "ESTOQUE CA"
        chworksheet:range("J3"):VALUE = "RETALHO  M"     
        chworksheet:range("K3"):VALUE = "RETALHO KG"
        chworksheet:range("L3"):VALUE = "CONTA CORRENTE"
        chworksheet:range("M3"):VALUE = "TIPO OB" 
        chworksheet:range("N3"):VALUE = "STATUS OB"
        chworksheet:range("O3"):VALUE = "RESPONSAVEL".   

 /* Tamanho das Colunas */
 ASSIGN chWorkSheet:Columns("A"):ColumnWidth =  6
        chWorkSheet:Columns("B"):ColumnWidth = 10
        chWorkSheet:Columns("C"):ColumnWidth =  6
        chWorkSheet:Columns("D"):ColumnWidth =  6
        chWorkSheet:Columns("E"):ColumnWidth = 10
        chWorkSheet:Columns("F"):ColumnWidth = 11
        chWorkSheet:Columns("G"):ColumnWidth = 11
        chWorkSheet:Columns("H"):ColumnWidth = 11
        chWorkSheet:Columns("I"):ColumnWidth = 11
        chWorkSheet:Columns("J"):ColumnWidth = 11
        chWorkSheet:Columns("K"):ColumnWidth = 11 
        chWorkSheet:Columns("L"):ColumnWidth = 14
        chWorkSheet:Columns("M"):ColumnWidth =  9
        chWorkSheet:Columns("N"):ColumnWidth = 11
        chWorkSheet:Columns("O"):ColumnWidth = 12.

 /* Configura as Colunas da Planilha */
 ASSIGN chworksheet:range("A:A"):NumberFormat = "###.###.##0"
        chworksheet:range("B:B"):NumberFormat = "@"
        chworksheet:range("C:C"):NumberFormat = "###.###.##0"
        chworksheet:range("D:E"):NumberFormat = "@"
        chworksheet:range("F:L"):NumberFormat = "###.###.##0,00"
        chworksheet:range("M:O"):NumberFormat = "@"
        Chworksheet:range("A:A"):HorizontalAlignment = 4
        Chworksheet:range("C:C"):HorizontalAlignment = 4
        Chworksheet:range("F:L"):HorizontalAlignment = 4. /* Alinhamento a Direita */

 /* Configura Cabeáalho das Colunas */
 chWorkSheet:Range("A3:O3"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 09
        chExcelApp:SELECTION:FONT:Bold               = TRUE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 19
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 ASSIGN i-Lin    = 4.

ASSIGN fi-planejado  = 0 fi-estoque-rp = 0 fi-estoque-rd = 0 fi-estoque-ca = 0
       fi-retalho-m  = 0 fi-retalho-kg = 0 fi-conta      = 0 fi-estoque-total = 0.

FOR EACH b-tt-ordem-benefic NO-LOCK.
    ASSIGN chworksheet:range("A" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.nr-ob
           chworksheet:range("B" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.dt-ob
           chworksheet:range("D" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.it-codigo
           chworksheet:range("E" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.cod-refer
           chworksheet:range("F" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.qtd-plan
           chworksheet:range("G" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.qtd-rp
           chworksheet:range("H" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.qtd-rd
           chworksheet:range("I" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.qtd-ca
           chworksheet:range("J" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.qtd-ret-m
           chworksheet:range("K" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.qtd-ret-kg
           chworksheet:range("L" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.c-corrente
           chworksheet:range("M" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.tipo-ob
           chworksheet:range("N" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.status-ob
           chworksheet:range("O" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.resp.


    IF b-tt-ordem-benefic.num-trf <> 0 THEN
       ASSIGN chworksheet:range("C" + STRING(i-lin)):VALUE = b-tt-ordem-benefic.num-trf.


    /*  Configura Tamanho da Fonte */
    ASSIGN c-lin = STRING(i-lin) + ":" + STRING(i-lin)
           chworksheet:Rows(c-lin):FONT:SIZE = 9.

    ASSIGN i-lin = i-lin + 1.

    ASSIGN fi-planejado  = fi-planejado  + tt-ordem-benefic.qtd-plan
           fi-estoque-rp = fi-estoque-rp + tt-ordem-benefic.qtd-rp
           fi-estoque-rd = fi-estoque-rd + tt-ordem-benefic.qtd-rd
           fi-estoque-ca = fi-estoque-ca + tt-ordem-benefic.qtd-ca
           fi-retalho-m  = fi-retalho-m  + tt-ordem-benefic.qtd-ret-m
           fi-retalho-kg = fi-retalho-kg + tt-ordem-benefic.qtd-ret-kg.

END.
IF i-lin <> 4 THEN DO:
   ASSIGN chworksheet:range("E" + STRING(i-lin)):VALUE = "TOT.GERAL"
          chworksheet:range("F" + STRING(i-lin)):VALUE = fi-planejado     
          chworksheet:range("G" + STRING(i-lin)):VALUE = fi-estoque-rp    
          chworksheet:range("H" + STRING(i-lin)):VALUE = fi-estoque-rd    
          chworksheet:range("I" + STRING(i-lin)):VALUE = fi-estoque-ca    
          chworksheet:range("J" + STRING(i-lin)):VALUE = fi-retalho-m     
          chworksheet:range("K" + STRING(i-lin)):VALUE = fi-retalho-kg.   
    /* Colorir a Linha / Negrito */
   ASSIGN chWorkSheet:Range("A" + STRING(i-lin) + ":O" + STRING(i-lin)):Interior:ColorIndex = 14
          chWorkSheet:Range("A" + STRING(i-lin) + ":O" + STRING(i-lin)):FONT:ColorIndex     =  2
          chWorkSheet:Range("A" + STRING(i-lin) + ":O" + STRING(i-lin)):FONT:Bold           = TRUE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Ordens_de_Beneficiamento *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-ordem-benefic.

   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   ASSIGN c-titulo = "Analise Gerencial da PRODUÄ«O X ESTOQUE no Periodo: " + STRING(da-dt-movto-ini, "99/99/9999") + " A " +
                     STRING(da-dt-movto-fin, "99/99/9999").                                                         

   RUN pi-separa-obs.

   RUN pi-finalizar in h-acomp.

   {&OPEN-QUERY-br-tt-ordem-benefic}
   
   APPLY 'entry' TO br-tt-ordem-benefic IN FRAME {&FRAME-NAME}.
   APPLY 'value-changed' TO br-tt-ordem-benefic IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-obs C-Win 
PROCEDURE pi-separa-obs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR c-todas-obs     AS CHAR.
 DEF VAR c-todos-status  AS CHAR.
 DEF VAR c-tipo          AS CHAR.
 DEF VAR c-status        AS CHAR.
 DEF VAR c-trf           AS CHAR.
 DEF VAR de-total        AS DEC.
 DEF VAR de-tot-perfeito AS DEC.
 DEF VAR de-tot-defeito  AS DEC.
 DEF VAR de-tot-amostra  AS DEC.
 DEF VAR de-tot-ret-kg   AS DEC.
 DEF VAR de-tot-ret-m    AS DEC.

 IF l-todas-obs = YES THEN
   ASSIGN c-todas-obs = "1,2,3,4,".
 ELSE DO:
   ASSIGN c-todas-obs = c-todas-obs + IF l-producao   = YES THEN "1," ELSE ",".
   ASSIGN c-todas-obs = c-todas-obs + IF l-retrabalho = YES THEN "2," ELSE ",".
   ASSIGN c-todas-obs = c-todas-obs + IF l-transf     = YES THEN "3," ELSE ",".
   ASSIGN c-todas-obs = c-todas-obs + IF l-indust     = YES THEN "4," ELSE ",".
 END.

 IF l-todos-status = YES THEN
   ASSIGN c-todos-status = "1,2,3,4,5,".
 ELSE DO:
   ASSIGN c-todos-status = c-todos-status + IF l-disponivel  = YES THEN "1," ELSE ",".
   ASSIGN c-todos-status = c-todos-status + IF l-em-revisao  = YES THEN "2," ELSE ",".
   ASSIGN c-todos-status = c-todos-status + IF l-rev-parcial = YES THEN "3," ELSE ",".
   ASSIGN c-todos-status = c-todos-status + IF l-rev-total   = YES THEN "4," ELSE ",".
   ASSIGN c-todos-status = c-todos-status + IF l-reportado   = YES THEN "5," ELSE ",".
 END.
 ASSIGN c-trf = "".
 ASSIGN c-trf = c-trf + IF l-pendente  = YES THEN "1," ELSE ",".
 ASSIGN c-trf = c-trf + IF l-executada = YES THEN "2," ELSE ",".

 FOR EACH ordem-benefic WHERE
          ordem-benefic.cod-estabel >= c-cod-estabel-ini AND
          ordem-benefic.cod-estabel <= c-cod-estabel-fin AND 
          ordem-benefic.nr-ob       >= i-nr-ob-ini       AND 
          ordem-benefic.nr-ob       <= i-nr-ob-fin       AND
          ordem-benefic.dt-ob       >= da-dt-movto-ini   AND
          ordem-benefic.dt-ob       <= da-dt-movto-fin NO-LOCK.


     RUN pi-acompanhar IN h-acomp (INPUT "DATA: " + STRING(ordem-benefic.dt-ob) + 
                                         " OB " + STRING(ordem-benefic.nr-ob)).

     IF ordem-benefic.nr-ob <= 0 THEN NEXT.

     IF ordem-benefic.quantidade <= 0 THEN NEXT.

     IF ordem-benefic.it-codigo < c-it-codigo-ini OR
        ordem-benefic.it-codigo > c-it-codigo-fin THEN NEXT. 

     IF ordem-benefic.cod-refer < c-cod-refer-ini OR 
        ordem-benefic.cod-refer > c-cod-refer-fin THEN NEXT.

     IF LOOKUP(STRING(ordem-benefic.tipo-ordem, "9"),c-todas-obs) = 0 THEN NEXT.
     
     IF LOOKUP(STRING(ordem-benefic.situacao, "9"),c-todos-status) = 0 THEN NEXT.

     FIND ob-trf WHERE
          ob-trf.num-trf = ordem-benefic.num-trf NO-LOCK NO-ERROR.
     IF AVAIL ob-trf THEN DO:
        IF l-transf THEN
           IF LOOKUP(STRING(ob-trf.situacao, "9"),c-trf) = 0 THEN NEXT.
     END.

     CASE ordem-benefic.tipo-ordem:
         WHEN 1 THEN
            ASSIGN c-tipo = "Produá∆o".
         WHEN 2 THEN
            ASSIGN c-tipo = "Retrabalho".
         WHEN 3 THEN
            ASSIGN c-tipo = "Conserto".
         WHEN 4 THEN
            ASSIGN c-tipo = "Industrializaá∆o".
     END CASE.

     CASE ordem-benefic.situacao:
         WHEN 1 THEN
            ASSIGN c-status = "Disponivel".
         WHEN 2 THEN
            ASSIGN c-status = "Em Revis∆o".
         WHEN 3 THEN
            ASSIGN c-status = "Revis∆o Parcial".
         WHEN 4 THEN
            ASSIGN c-status = "Revis∆o Total".
         WHEN 5 THEN
            ASSIGN c-status = "Reportado".
     END CASE.

     /* E S T O Q U E */
     ASSIGN de-tot-perfeito = 0
            de-tot-defeito  = 0
            de-tot-amostra  = 0
            de-tot-ret-kg   = 0
            de-tot-ret-m    = 0.
     FOR EACH ob-etiqueta OF ordem-benefic WHERE 
              ob-etiqueta.cod-estabel = ordem-benefic.cod-estabel NO-LOCK:

         CASE SUBSTR(ob-etiqueta.nr-lote,1,2):
             WHEN "RP" THEN
                 ASSIGN de-tot-perfeito = de-tot-perfeito + ob-etiqueta.quantidade.
             WHEN "RD" THEN
                 ASSIGN de-tot-defeito = de-tot-defeito + ob-etiqueta.quantidade.
             WHEN "CA" THEN
                 ASSIGN de-tot-amostra = de-tot-amostra + ob-etiqueta.quantidade.
             WHEN "SC" THEN DO.
                 ASSIGN de-tot-ret-m = de-tot-ret-m + ob-etiqueta.quantidade.
                 FIND item WHERE
                      item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
                 IF AVAIL item THEN DO.
                    ASSIGN de-tot-ret-kg = de-tot-ret-kg + (item.peso-liquido * ob-etiqueta.quantidade).
                 END.
             END.
         END CASE.

         /*
         FOR EACH mov-est-acbd WHERE
                  mov-est-acbd.cod-estabel  = ob-etiqueta.cod-estabel  AND
                  mov-est-acbd.data-mov     = ob-etiqueta.dt-emissao   AND
                  mov-est-acbd.num-lote     = ob-etiqueta.nr-ob        AND
                  mov-est-acbd.nr-carro     = ob-etiqueta.nr-carro     AND
                  mov-est-acbd.acondic      = ob-etiqueta.acondic      AND
                  mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
                  mov-est-acbd.classif      = "RT" NO-LOCK.
             ASSIGN de-tot-ret-m = de-tot-ret-m + mov-est-acbd.qtd-defeit.
             FIND item WHERE
                  item.it-codigo = mov-est-acbd.it-codigo NO-LOCK NO-ERROR.
             IF AVAIL item THEN
                ASSIGN de-tot-ret-kg = de-tot-ret-kg + (item.peso-liquido * mov-est-acbd.qtd-defeit).
         END.
         */
     END.

     FIND tt-ordem-benefic WHERE
          tt-ordem-benefic.cod-estabel = ordem-benefic.cod-estabel AND
          tt-ordem-benefic.nr-ob       = ordem-benefic.nr-ob NO-ERROR.
     IF NOT AVAIL tt-ordem-benefic THEN DO:
        
        CREATE tt-ordem-benefic.
        ASSIGN tt-ordem-benefic.cod-estabel = ordem-benefic.cod-estabel 
               tt-ordem-benefic.nr-ob       = ordem-benefic.nr-ob
               tt-ordem-benefic.dt-ob       = ordem-benefic.dt-ob
               tt-ordem-benefic.nr-carro    = ordem-benefic.nr-carro
               tt-ordem-benefic.num-trf     = ordem-benefic.num-trf
               tt-ordem-benefic.it-codigo   = ordem-benefic.it-codigo
               tt-ordem-benefic.cod-refer   = ordem-benefic.cod-refer
               tt-ordem-benefic.resp        = ordem-benefic.responsavel
               tt-ordem-benefic.tipo-ob     = c-tipo
               tt-ordem-benefic.status-ob   = c-status
               tt-ordem-benefic.qtd-plan    = ordem-benefic.quantidade
               tt-ordem-benefic.qtd-rp      = de-tot-perfeito
               tt-ordem-benefic.perc-rp     = (de-tot-perfeito * 100) / ordem-benefic.quantidade 
               tt-ordem-benefic.qtd-rd      = de-tot-defeito
               tt-ordem-benefic.perc-rd     = (de-tot-defeito * 100) / ordem-benefic.quantidade
               tt-ordem-benefic.qtd-ca      = de-tot-amostra
               tt-ordem-benefic.perc-ca     = (de-tot-amostra * 100) / ordem-benefic.quantidade
               tt-ordem-benefic.qtd-ret-m   = de-tot-ret-m
               tt-ordem-benefic.perc-m     = (de-tot-ret-m * 100) / ordem-benefic.quantidade
               tt-ordem-benefic.qtd-ret-kg  = de-tot-ret-kg.
     END.
 END. /* KBO Ordem Benefic */

 FOR EACH ob-trf WHERE
         ob-trf.dt-solic >= da-dt-movto-ini AND
         ob-trf.dt-solic <= da-dt-movto-fin NO-LOCK.
      
    FIND ordem-benefic WHERE
         ordem-benefic.num-trf = ob-trf.num-trf NO-LOCK NO-ERROR.
    IF AVAIL ordem-benefic THEN NEXT.

    ASSIGN de-total = 0.
    FOR EACH ob-etq-trf WHERE
             ob-etq-trf.num-trf = ob-trf.num-trf NO-LOCK.
        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = ob-etq-trf.cod-estabel AND
             ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-LOCK NO-ERROR.
        ASSIGN de-total = de-total + ob-etiqueta.quantidade.
    END.
    FIND tt-ordem-benefic WHERE
         tt-ordem-benefic.num-trf = ob-trf.num-trf NO-ERROR.
    IF NOT AVAIL tt-ordem-benefic THEN DO:
       CREATE tt-ordem-benefic.
       ASSIGN tt-ordem-benefic.num-trf     = ob-trf.num-trf
              tt-ordem-benefic.dt-ob       = ob-trf.dt-solic
              tt-ordem-benefic.it-codigo   = ob-trf.it-codigo
              tt-ordem-benefic.cod-refer   = ob-trf.cod-refer
              tt-ordem-benefic.qtd-plan    = de-total.
    END.
 END.

 FOR EACH tt-ordem-benefic.
     ASSIGN tt-ordem-benefic.c-corrente = tt-ordem-benefic.qtd-plan - (tt-ordem-benefic.qtd-rp +
                                                                       tt-ordem-benefic.qtd-rd +    
                                                                       tt-ordem-benefic.qtd-ca +    
                                                                       tt-ordem-benefic.qtd-ret-m). 
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais C-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN fi-planejado   = 0
       fi-estoque-rp  = 0
       fi-perc-tot-rp = 0
       fi-estoque-rd  = 0
       fi-perc-tot-rd = 0
       fi-estoque-ca  = 0
       fi-perc-tot-ca = 0
       fi-retalho-m   = 0
       fi-perc-tot-m  = 0
       fi-retalho-kg  = 0.


FOR EACH tt-ordem-benefic NO-LOCK.
    ASSIGN fi-planejado  = fi-planejado  + tt-ordem-benefic.qtd-plan
           fi-estoque-rp = fi-estoque-rp + tt-ordem-benefic.qtd-rp
           fi-estoque-rd = fi-estoque-rd + tt-ordem-benefic.qtd-rd
           fi-estoque-ca = fi-estoque-ca + tt-ordem-benefic.qtd-ca
           fi-retalho-m  = fi-retalho-m  + tt-ordem-benefic.qtd-ret-m
           fi-retalho-kg = fi-retalho-kg + tt-ordem-benefic.qtd-ret-kg.
END.

IF fi-planejado > 0 THEN DO.

ASSIGN fi-perc-tot-rp = (fi-estoque-rp * 100) / fi-planejado
       fi-perc-tot-rd = (fi-estoque-rd * 100) / fi-planejado
       fi-perc-tot-ca = (fi-estoque-ca * 100) / fi-planejado
       fi-perc-tot-m = (fi-retalho-m * 100) / fi-planejado.

END.

ASSIGN fi-estoque-total = fi-estoque-rp + fi-estoque-rd + fi-estoque-ca +
       fi-retalho-m  + fi-retalho-kg.
ASSIGN fi-conta = fi-planejado - fi-estoque-total.
DISP fi-estoque-rp
     fi-perc-tot-rp
     fi-estoque-rd
     fi-perc-tot-rd
     fi-estoque-ca
     fi-perc-tot-ca
     fi-retalho-m
     fi-perc-tot-m
     fi-retalho-kg
     fi-planejado
     fi-estoque-total
     fi-conta
     WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

