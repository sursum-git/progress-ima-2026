&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B03di134 2.04.00.000}
DEF BUFFER empresa FOR mgadm.empresa.

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF BUFFER fornecedor FOR ems5.fornecedor.

{esinc\escm002.i}

/* Buscna NOTAS FISCAIS do Representante */
DEFINE TEMP-TABLE tt-work  NO-UNDO 
       FIELD cod-estabel   AS CHAR LABEL "Est"
       FIELD base          AS INT
       FIELD cod-rep       LIKE nota-fiscal.cod-rep
       FIELD vlr-fat       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD devolucao     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD liquidez      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD fat-liq       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comissao      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comis-dev     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD comis-liq     AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD desconto      AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD desc-base-ir  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD i-renda       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD adiantamento  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD emprestimo    AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD liquido       AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD vlr-nf        AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD marcar        AS CHAR.

DEFINE TEMP-TABLE tt-repres LIKE repres
       FIELD classe         AS CHAR
       FIELD marca          AS CHAR
       FIELD ap-integrado   AS LOG
       FIELD visualiza      AS LOG.

DEFINE TEMP-TABLE tt-calc-repres 
       FIELD cod-rep      LIKE repres.cod-rep
       FIELD nome-ab-rep  LIKE repres.nome-abrev
       FIELD cod-pai      LIKE repres.cod-rep
       FIELD nome-ab-pai  LIKE repres.nome-abrev
       FIELD marca        AS LOGICAL.

DEFINE TEMP-TABLE tt-nfs   LIKE nota-fiscal
       //FIELD nome-emit     LIKE emitente.nome-emit
       FIELD qt-faturada   AS   DECIMAL
       FIELD comissao      AS   DECIMAL
       FIELD base          AS   INT
       INDEX indice1 IS PRIMARY cod-rep cod-estabel serie nr-nota-fis.

DEFINE TEMP-TABLE tt-emprestimo LIKE cm-emprestimo.

DEFINE TEMP-TABLE tt-descontos 
       FIELD cod-estabel   LIKE nota-fiscal.cod-estabel
       FIELD estab-ori     LIKE nota-fiscal.cod-estabel
       FIELD cod-rep       LIKE nota-fiscal.cod-rep
       FIELD num-desconto  LIKE cm-desc-repres.num-desconto
       FIELD desc-desconto LIKE cm-desc-repres.desc-desconto
       FIELD vlr-original  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD vlr-desconto  AS DEC FORMAT "->>>,>>>,>>9.99"
       FIELD mes           LIKE cm-desc-repres.mes
       FIELD ano           LIKE cm-desc-repres.ano
       FIELD base          AS INT.

DEFINE TEMP-TABLE tt-digita
       FIELD opcao AS CHAR
       FIELD campo AS CHAR
       FIELD valor AS CHAR.

DEFINE BUFFER b-repres FOR repres.
DEFINE BUFFER b-tt-repres FOR tt-repres.
DEFINE BUFFER b-tt-work FOR tt-work.
DEFINE BUFFER b2-tt-work FOR tt-work.
DEFINE BUFFER b3-tt-work FOR tt-work.

DEFINE STREAM s1.

/* Variaveis do Parametro */
DEF VAR c-cod-estab-ini    AS CHAR.                              
DEF VAR c-cod-estab-fin    AS CHAR.
DEF VAR da-dt-periodo-ini  AS DATE.
DEF VAR da-dt-periodo-fin  AS DATE.
DEF VAR c-no-ab-reppri-ini AS CHAR.                              
DEF VAR c-no-ab-reppri-fin AS CHAR.
DEF VAR c-lst-classe       AS CHAR.
DEF VAR c-nr-nota-fis-ini  AS CHAR.                              
DEF VAR c-nr-nota-fis-fin  AS CHAR.
DEF VAR c-it-codigo-ini    AS CHAR.                              
DEF VAR c-it-codigo-fin    AS CHAR.                              
DEF VAR i-cond-pagto-ini   AS INT.                              
DEF VAR i-cond-pagto-fin   AS INT.                              

/* Variaveis normais do Programa */
DEF NEW GLOBAL SHARED VAR gr-repres     AS ROWID NO-UNDO.

DEF VAR c-meses          AS CHAR INIT "Janeiro,Fevereiro,Maráo,Abril,Maio,Junho,Julho,Agosto,Setembro,Outubro,Novemtro,Dezembro".
DEF VAR de-desc-12       AS DEC.
DEF VAR de-perc          AS DEC.
DEF VAR de-preco-tab     AS DEC.
DEF VAR de-comissao      AS DEC.
DEF VAR de-comissao-12   AS DEC.
DEF VAR da-data-lim-nf   AS DATE.
DEF VAR da-data-pagto    AS DATE.
DEF VAR c-observacao     AS CHAR.
DEF VAR l-erro-integra   AS LOGICAL.
DEF VAR l-nachou-nfs     AS LOGICAL.
DEF VAR c-arq-saida      AS CHAR.
DEF VAR da-dt-venc-tit   AS DATE.

DEF VAR de-emissao       AS DEC.



DEF VAR h-query          AS HANDLE.
DEF VAR h-acomp          AS HANDLE NO-UNDO.
DEF VAR c-empresa        AS CHAR.
DEF VAR c-cod-estab-ems5 AS CHAR.
DEF VAR i-cod-rep        AS INT.


 /* Variaveis da Rotina de Impress∆o */
DEFINE VAR l-ok         AS LOG.
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.
DEFINE VAR i-ct         AS INT.
DEFINE VAR i-lin        AS INT.
DEFINE VAR i-pag        AS INT.

/* Variaveis para o Excel */
DEFINE VAR chExcelApp     AS COM-HANDLE NO-UNDO.
DEFINE VAR chWorkbook     AS COM-HANDLE NO-UNDO.
DEFINE VAR chworksheet    AS COM-HANDLE NO-UNDO.
DEFINE VAR c-lin          AS CHAR FORMAT "x(500)".
DEFINE VAR arq-saida      AS CHAR FORMAT "x(51)".
DEFINE VAR arq-resumo     AS CHAR FORMAT "x(51)".
DEFINE VAR c-mensagem     AS CHAR.
DEFINE VAR c-destinatario AS CHAR.
DEFINE VAR i-lin1        AS INT.
DEFINE VAR i-lin2        AS INT.
DEFINE VAR i-lin3        AS INT.
DEFINE VAR i-lin4        AS INT.
DEFINE VAR i-lin5        AS INT.
DEFINE VAR i-lin6        AS INT.
DEFINE VAR i-lin7        AS INT.
DEFINE VAR i-lin8        AS INT.
DEFINE VAR l-visivel     AS LOG.
DEFINE VAR i-nr-plan     AS INT.
DEFINE VAR de-comis      AS DEC.
DEFINE VAR de-chq        AS DEC.
DEFINE VAR de-comis-chq  AS DEC.
DEFINE VAR de-comis-inf  AS DEC.
DEFINE VAR de-est-tit    AS DEC.
DEFINE VAR de-tot-chq    AS DEC.
DEFINE VAR de-ir         AS DEC.
DEFINE VAR de-desc       AS DEC.
DEFINE VAR de-emp        AS DEC.
DEFINE VAR de-rec        AS DEC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-repres

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-repres tt-work

/* Definitions for BROWSE br-repres                                     */
&Scoped-define FIELDS-IN-QUERY-br-repres tt-repres.nome-abrev tt-repres.cod-rep tt-repres.nome tt-repres.classe tt-repres.cidade tt-repres.estado tt-repres.comis-direta   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-repres   
&Scoped-define SELF-NAME br-repres
&Scoped-define QUERY-STRING-br-repres FOR EACH tt-repres WHERE                                  tt-repres.visualiza                                  NO-LOCK BY tt-repres.nome-abrev
&Scoped-define OPEN-QUERY-br-repres OPEN QUERY {&SELF-NAME} FOR EACH tt-repres WHERE                                  tt-repres.visualiza                                  NO-LOCK BY tt-repres.nome-abrev.
&Scoped-define TABLES-IN-QUERY-br-repres tt-repres
&Scoped-define FIRST-TABLE-IN-QUERY-br-repres tt-repres


/* Definitions for BROWSE br-work                                       */
&Scoped-define FIELDS-IN-QUERY-br-work tt-work.base tt-work.vlr-fat tt-work.devolucao tt-work.fat-liq tt-work.comissao tt-work.comis-dev tt-work.comis-liq tt-work.desconto tt-work.i-renda tt-work.emprestimo tt-work.adiantamento tt-work.liquido   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-work tt-work.liquido   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-work tt-work
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-work tt-work
&Scoped-define SELF-NAME br-work
&Scoped-define OPEN-QUERY-br-work RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-work WHERE                                  tt-work.cod-rep = tt-repres.cod-rep                                  NO-LOCK BY tt-work.cod-estabel.
&Scoped-define TABLES-IN-QUERY-br-work tt-work
&Scoped-define FIRST-TABLE-IN-QUERY-br-work tt-work


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-repres}~
    ~{&OPEN-QUERY-br-work}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-60 RECT-61 IMAGE-1 IMAGE-2 br-repres ~
bt-vapara bt-marca bt-desmarca bt-todos bt-nenhum bt-repres br-work ~
bt-modifica bt-pri bt-excel bt-consulta bt-devolucoes bt-emprestimo ~
bt-desconto bt-email bt-exporta 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-periodo-ini fi-dt-periodo-fin ~
fi-fat-geral fi-comis-ger fi-fat-repres fi-comis-repres 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-modifica bt-consulta bt-emprestimo 
&Scoped-define List-6 bt-excel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-ord-produ||y|mgmov.ped-item.nr-ord-produ
nr-programa||y|mgmov.ped-item.nr-programa
it-codigo||y|mgmov.ped-item.it-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nr-ord-produ,nr-programa,it-codigo"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-carg.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Consulta Notas Fiscais ou T°tulos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-desconto 
     IMAGE-UP FILE "image/im-aval.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Consulta Descontos"
     BGCOLOR 8 .

DEFINE BUTTON bt-desmarca AUTO-GO 
     IMAGE-UP FILE "image/im-cllps.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarcar um"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-devolucoes 
     IMAGE-UP FILE "image/im-desc.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Consulta Devoluá‰es"
     BGCOLOR 8 .

DEFINE BUTTON bt-email AUTO-GO 
     IMAGE-UP FILE "image/im-email.bmp":U
     LABEL "OK" 
     SIZE 5 BY 1.25 TOOLTIP "Confirmar para Envio de Emails.".

DEFINE BUTTON bt-emprestimo AUTO-GO 
     IMAGE-UP FILE "image/im-prep.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Consulta Emprestimos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 5 BY 1.25 TOOLTIP "Exportar Dados para Excel".

DEFINE BUTTON bt-exporta AUTO-GO 
     IMAGE-UP FILE "image/im-exp.bmp":U
     LABEL "OK" 
     SIZE 9 BY 1.25 TOOLTIP "Exporta Titulos Contas a Pagar".

DEFINE BUTTON bt-marca AUTO-GO 
     IMAGE-UP FILE "image/im-expan.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marcar um"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica AUTO-GO 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Altera Valor"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nenhum AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-pri 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprime Calculo das Comiss‰es".

DEFINE BUTTON bt-repres AUTO-GO 
     IMAGE-UP FILE "image/gr-usu.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Consulta Representante"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-todos AUTO-GO 
     IMAGE-UP FILE "image\im-ran_a.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "V† para"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-comis-ger AS DECIMAL FORMAT "->>>,>>>,>>9.99":R31 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-comis-repres AS DECIMAL FORMAT "->>>,>>>,>>9.99":R31 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-periodo-fin AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-periodo-ini AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fat-geral AS DECIMAL FORMAT ">>>,>>>,>>9.99":R25 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fat-repres AS DECIMAL FORMAT ">>>,>>>,>>9.99":R30 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 7.14 BY 7.75.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 117 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-repres FOR 
      tt-repres SCROLLING.

DEFINE QUERY br-work FOR 
      tt-work SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-repres B-table-Win _FREEFORM
  QUERY br-repres NO-LOCK DISPLAY
      tt-repres.nome-abrev   COLUMN-LABEL "Abrev"   WIDTH 15
      tt-repres.cod-rep      COLUMN-LABEL "Rep."    WIDTH 6
      tt-repres.nome         COLUMN-LABEL "Nome"    WIDTH 35
      tt-repres.classe       COLUMN-LABEL "Classe"  WIDTH 13
      tt-repres.cidade       COLUMN-LABEL "Cidade"  WIDTH 24
      tt-repres.estado       COLUMN-LABEL "UF"      WIDTH 3
      tt-repres.comis-direta COLUMN-LABEL "%Comis"  WIDTH 6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 109.29 BY 7.75
         FONT 1
         TITLE "REPRESENTANTES" ROW-HEIGHT-CHARS .67.

DEFINE BROWSE br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-work B-table-Win _FREEFORM
  QUERY br-work NO-LOCK DISPLAY
      tt-work.base          COLUMN-LABEL "P"              WIDTH 2
      tt-work.vlr-fat       COLUMN-LABEL "Faturamento"    WIDTH 13
      tt-work.devolucao     COLUMN-LABEL "Devoluá∆o"      WIDTH 11
      tt-work.fat-liq       COLUMN-LABEL "Fat Liquido"    WIDTH 13
      tt-work.comissao      COLUMN-LABEL "Comis.Bruta"    WIDTH 9
      tt-work.comis-dev     COLUMN-LABEL "Comis.Dev."     WIDTH 9
      tt-work.comis-liq     COLUMN-LABEL "Comis.Liq."     WIDTH 9
      tt-work.desconto      COLUMN-LABEL "Desconto"       WIDTH 8
      tt-work.i-renda       COLUMN-LABEL "I.R."           WIDTH 7
      tt-work.emprestimo    COLUMN-LABEL "Emprestimo"     WIDTH 8
      tt-work.adiantamento  COLUMN-LABEL "Adiant."        WIDTH 7
      tt-work.liquido       COLUMN-LABEL "A RECEBER"      WIDTH 10
ENABLE
      tt-work.liquido
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 117.29 BY 7.38
         FONT 1
         TITLE "CALCULO DE COMISSÂES" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-repres AT ROW 1.04 COL 1.72
     bt-vapara AT ROW 1.25 COL 113 WIDGET-ID 36
     bt-marca AT ROW 2.46 COL 113 WIDGET-ID 28
     bt-desmarca AT ROW 3.67 COL 113 WIDGET-ID 24
     bt-todos AT ROW 4.92 COL 113 WIDGET-ID 34
     bt-nenhum AT ROW 6.13 COL 113 WIDGET-ID 30
     bt-repres AT ROW 7.38 COL 113 WIDGET-ID 50
     br-work AT ROW 8.88 COL 1.72 WIDGET-ID 100
     bt-modifica AT ROW 16.5 COL 2 WIDGET-ID 48
     bt-pri AT ROW 16.5 COL 10 WIDGET-ID 44
     bt-excel AT ROW 16.5 COL 15 WIDGET-ID 58
     bt-consulta AT ROW 16.5 COL 27 WIDGET-ID 22
     bt-devolucoes AT ROW 16.5 COL 32 WIDGET-ID 38
     bt-emprestimo AT ROW 16.5 COL 37 WIDGET-ID 52
     bt-desconto AT ROW 16.5 COL 42 WIDGET-ID 54
     bt-email AT ROW 16.5 COL 105 WIDGET-ID 84
     bt-exporta AT ROW 16.5 COL 110 WIDGET-ID 72
     fi-dt-periodo-ini AT ROW 19.38 COL 7 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     fi-dt-periodo-fin AT ROW 19.38 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     fi-fat-geral AT ROW 19.38 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fi-comis-ger AT ROW 19.38 COL 58 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fi-fat-repres AT ROW 19.38 COL 80 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-comis-repres AT ROW 19.38 COL 98 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     "Per°odo" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 18.75 COL 9.29 WIDGET-ID 64
     "Totais" VIEW-AS TEXT
          SIZE 5 BY .54 AT ROW 18.25 COL 3.57 WIDGET-ID 68
     "Comiss∆o Geral" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 18.75 COL 60 WIDGET-ID 18
     "Comiss∆o Representante" VIEW-AS TEXT
          SIZE 17 BY .54 AT ROW 18.75 COL 100 WIDGET-ID 20
     "Faturam. Representante" VIEW-AS TEXT
          SIZE 17 BY .54 AT ROW 18.75 COL 82.14 WIDGET-ID 16
     "Faturamento Geral" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 18.75 COL 42.14 WIDGET-ID 14
     RECT-60 AT ROW 1 COL 112
     RECT-61 AT ROW 18.5 COL 2 WIDGET-ID 66
     IMAGE-1 AT ROW 19.42 COL 19.14 WIDGET-ID 60
     IMAGE-2 AT ROW 19.42 COL 22.57 WIDGET-ID 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.54
         WIDTH              = 118.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-repres IMAGE-2 F-Main */
/* BROWSE-TAB br-work bt-repres F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-consulta IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-emprestimo IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR BUTTON bt-excel IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-modifica IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-comis-ger IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-comis-repres IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-periodo-fin IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-periodo-ini IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-fat-geral IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-fat-repres IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-repres
/* Query rebuild information for BROWSE br-repres
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-repres WHERE
                                 tt-repres.visualiza
                                 NO-LOCK BY tt-repres.nome-abrev.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-repres */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-work
/* Query rebuild information for BROWSE br-work
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-work WHERE
                                 tt-work.cod-rep = tt-repres.cod-rep
                                 NO-LOCK BY tt-work.cod-estabel.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-work */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-repres
&Scoped-define SELF-NAME br-repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-repres B-table-Win
ON ROW-DISPLAY OF br-repres IN FRAME F-Main /* REPRESENTANTES */
DO:
   tt-repres.cod-rep:FONT IN BROWSE br-repres = ?.           
   tt-repres.nome-abrev:FONT IN BROWSE br-repres = ?.        
   tt-repres.nome:FONT IN BROWSE br-repres = ?.              
   tt-repres.classe:FONT IN BROWSE br-repres = ?.            
   tt-repres.cidade:FONT IN BROWSE br-repres = ?.            
   tt-repres.estado:FONT IN BROWSE br-repres = ?.            
   tt-repres.comis-direta:FONT IN BROWSE br-repres = ?.      

   tt-repres.cod-rep:FGCOLOR IN BROWSE br-repres = ?.           
   tt-repres.nome-abrev:FGCOLOR IN BROWSE br-repres = ?.        
   tt-repres.nome:FGCOLOR IN BROWSE br-repres = ?.              
   tt-repres.classe:FGCOLOR IN BROWSE br-repres = ?.            
   tt-repres.cidade:FGCOLOR IN BROWSE br-repres = ?.            
   tt-repres.estado:FGCOLOR IN BROWSE br-repres = ?.            
   tt-repres.comis-direta:FGCOLOR IN BROWSE br-repres = ?.      

   IF tt-repres.marca = "*" THEN DO.
      tt-repres.cod-rep:FONT IN BROWSE br-repres = 6.
      tt-repres.nome-abrev:FONT IN BROWSE br-repres = 6.  
      tt-repres.nome:FONT IN BROWSE br-repres = 6.        
      tt-repres.classe:FONT IN BROWSE br-repres = 6.      
      tt-repres.cidade:FONT IN BROWSE br-repres = 6.      
      tt-repres.estado:FONT IN BROWSE br-repres = 6.      
      tt-repres.comis-direta:FONT IN BROWSE br-repres = 6.

      tt-repres.cod-rep:FGCOLOR IN BROWSE br-repres = 12.           
      tt-repres.nome-abrev:FGCOLOR IN BROWSE br-repres = 12.        
      tt-repres.nome:FGCOLOR IN BROWSE br-repres = 12.              
      tt-repres.classe:FGCOLOR IN BROWSE br-repres = 12.            
      tt-repres.cidade:FGCOLOR IN BROWSE br-repres = 12.            
      tt-repres.estado:FGCOLOR IN BROWSE br-repres = 12.            
      tt-repres.comis-direta:FGCOLOR IN BROWSE br-repres = 12.      

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-repres B-table-Win
ON VALUE-CHANGED OF br-repres IN FRAME F-Main /* REPRESENTANTES */
DO:
   IF AVAIL tt-repres THEN
      ASSIGN bt-modifica:SENSITIVE = tt-repres.marca = "*".

   {&OPEN-QUERY-br-work}
   APPLY 'value-changed' TO br-work.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-work
&Scoped-define SELF-NAME br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-work B-table-Win
ON ROW-DISPLAY OF br-work IN FRAME F-Main /* CALCULO DE COMISSÂES */
DO:
    tt-work.base:FGCOLOR IN BROWSE br-work = ?.          
    tt-work.vlr-fat:FGCOLOR IN BROWSE br-work = ?.       
    tt-work.devolucao:FGCOLOR IN BROWSE br-work = ?.      
    tt-work.fat-liq:FGCOLOR IN BROWSE br-work = ?.      
    tt-work.comissao:FGCOLOR IN BROWSE br-work = ?.      
    tt-work.comis-liq:FGCOLOR IN BROWSE br-work = ?.      
    tt-work.desconto:FGCOLOR IN BROWSE br-work = ?.      
    tt-work.i-renda:FGCOLOR IN BROWSE br-work = ?.      
    tt-work.emprestimo:FGCOLOR IN BROWSE br-work = ?.
    tt-work.adiantamento:FGCOLOR IN BROWSE br-work = ?. 
    tt-work.liquido:FGCOLOR IN BROWSE br-work = ?.      
    

    IF tt-work.fat-liq < 0 THEN DO.
       tt-work.base:FGCOLOR IN BROWSE br-work = 12.          
       tt-work.vlr-fat:FGCOLOR IN BROWSE br-work = 12.       
       tt-work.devolucao:FGCOLOR IN BROWSE br-work = 12.      
       tt-work.fat-liq:FGCOLOR IN BROWSE br-work = 12.      
       tt-work.comissao:FGCOLOR IN BROWSE br-work = 12.      
       tt-work.comis-dev:FGCOLOR IN BROWSE br-work = 12.      
       tt-work.comis-liq:FGCOLOR IN BROWSE br-work = 12.      
       tt-work.desconto:FGCOLOR IN BROWSE br-work = 12.      
       tt-work.i-renda:FGCOLOR IN BROWSE br-work = 12.      
       tt-work.emprestimo:FGCOLOR IN BROWSE br-work = 12.      
       tt-work.adiantamento:FGCOLOR IN BROWSE br-work = 12. 
       tt-work.liquido:FGCOLOR IN BROWSE br-work = 12. 
    END.

    IF tt-work.cod-estabel = '98' THEN DO.
       tt-work.base:FGCOLOR IN BROWSE br-work = 15.
       tt-work.vlr-fat:FGCOLOR IN BROWSE br-work = 15.       
       tt-work.devolucao:FGCOLOR IN BROWSE br-work = 15.      
       tt-work.fat-liq:FGCOLOR IN BROWSE br-work = 15.      
       tt-work.comissao:FGCOLOR IN BROWSE br-work = 15.      
       tt-work.comis-dev:FGCOLOR IN BROWSE br-work = 15.      
       tt-work.comis-liq:FGCOLOR IN BROWSE br-work = 15.      
       tt-work.desconto:FGCOLOR IN BROWSE br-work = 15.      
       tt-work.i-renda:FGCOLOR IN BROWSE br-work = 15.      
       tt-work.emprestimo:FGCOLOR IN BROWSE br-work = 15.      
       tt-work.adiantamento:FGCOLOR IN BROWSE br-work = 15. 
       tt-work.liquido:FGCOLOR IN BROWSE br-work = 15.
    END.

    IF tt-work.cod-estabel = '99' THEN DO.
       tt-work.base:FGCOLOR IN BROWSE br-work = 15.

       tt-work.vlr-fat:FONT IN BROWSE br-work = 6.       
       tt-work.devolucao:FONT IN BROWSE br-work = 6.      
       tt-work.fat-liq:FONT IN BROWSE br-work = 6.      
       tt-work.comissao:FONT IN BROWSE br-work = 6.      
       tt-work.comis-dev:FONT IN BROWSE br-work = 6.      
       tt-work.comis-liq:FONT IN BROWSE br-work = 6.      
       tt-work.desconto:FONT IN BROWSE br-work = 6.      
       tt-work.i-renda:FONT IN BROWSE br-work = 6.      
       tt-work.emprestimo:FONT IN BROWSE br-work = 6.      
       tt-work.adiantamento:FONT IN BROWSE br-work = 6. 
       tt-work.liquido:FONT IN BROWSE br-work = 6.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-work B-table-Win
ON ROW-LEAVE OF br-work IN FRAME F-Main /* CALCULO DE COMISSÂES */
DO:
  tt-work.liquido:READ-ONLY IN BROWSE br-work = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-work B-table-Win
ON VALUE-CHANGED OF br-work IN FRAME F-Main /* CALCULO DE COMISSÂES */
DO:
   IF AVAIL tt-work THEN DO.
      IF tt-work.cod-estab = '98' OR
         tt-work.cod-estabel = '99' THEN
         APPLY 'CURSOR-UP' TO SELF.

      ASSIGN bt-emprestimo:SENSITIVE = tt-work.emprestimo > 0.
      ASSIGN bt-desconto:SENSITIVE = tt-work.desconto > 0.
      ASSIGN bt-devolucoes:SENSITIVE = tt-work.devolucao > 0.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta B-table-Win
ON CHOOSE OF bt-consulta IN FRAME F-Main
DO:
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = NO.
   RUN esp/escm002a.p (INPUT TABLE tt-nfs,
                       INPUT tt-repres.cod-rep,
                       INPUT tt-work.cod-estab,
                       INPUT tt-work.base,
                       INPUT 22).
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desconto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desconto B-table-Win
ON CHOOSE OF bt-desconto IN FRAME F-Main
DO:
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = NO.
   RUN esp/escm002b.w (INPUT TABLE tt-descontos,
                       INPUT tt-work.cod-rep,
                       INPUT tt-work.cod-estabel,
                       INPUT tt-work.base,
                       INPUT "D").
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca B-table-Win
ON CHOOSE OF bt-desmarca IN FRAME F-Main
DO:
  ASSIGN tt-repres.marca = "".
  br-repres:REFRESH().

  APPLY 'VALUE-CHANGED' TO br-repres.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-devolucoes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-devolucoes B-table-Win
ON CHOOSE OF bt-devolucoes IN FRAME F-Main
DO:
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = NO.
   RUN esp/escm002a.p (INPUT TABLE tt-nfs,
                       INPUT tt-repres.cod-rep,
                       INPUT tt-work.cod-estab,
                       INPUT tt-work.base,
                       INPUT 20).
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-email B-table-Win
ON CHOOSE OF bt-email IN FRAME F-Main /* OK */
DO:
    FIND FIRST b-tt-repres WHERE
               b-tt-repres.marca = "*"  NO-LOCK NO-ERROR.
    IF NOT AVAIL b-tt-repres THEN DO:
       MESSAGE "Favor marcar Representantes, que deseja Enviar E-mails . . . "
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Gerando_Planilhas *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    ASSIGN l-visivel = FALSE.
    RUN pi-email-repres.

    RUN pi-finalizar in h-acomp.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-emprestimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-emprestimo B-table-Win
ON CHOOSE OF bt-emprestimo IN FRAME F-Main
DO:
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = NO.
   RUN esp/escm002b.w (INPUT TABLE tt-descontos,
                       INPUT "E").
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel B-table-Win
ON CHOOSE OF bt-excel IN FRAME F-Main /* Button 2 */
DO:
    FIND FIRST b-tt-repres WHERE
               b-tt-repres.marca = "*"  NO-LOCK NO-ERROR.
    IF NOT AVAIL b-tt-repres THEN DO:
       MESSAGE "Favor marcar os Representantes, que deseja Gerar Planilha..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.
    ASSIGN arq-saida = SESSION:TEMP-DIRECTORY + "Resumo das Comiss‰es.xls".

    RUN esdlg/d03escm002.w (INPUT-OUTPUT arq-saida).
    IF arq-saida <> "" THEN DO:
       ASSIGN l-visivel = TRUE.

       RUN pi-gera-excel. 
       MESSAGE "O Resumo foi gerado em " TRIM(arq-saida) +  "." SKIP 
               "Para acess†-lo,  abra-o atravÇs do Excel."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exporta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exporta B-table-Win
ON CHOOSE OF bt-exporta IN FRAME F-Main /* OK */
DO:
    FIND FIRST b-tt-repres WHERE
               b-tt-repres.marca = "*"  NO-LOCK NO-ERROR.
    IF NOT AVAIL b-tt-repres THEN DO:
       MESSAGE "Favor marcar quais Representantes deseja Integrar os Titulos..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.

    ASSIGN c-arq-saida = SESSION:TEMP-DIRECTORY + "integra-comis-ap.txt".
    OS-DELETE VALUE(c-arq-saida).

    FOR EACH tt-work WHERE 
             tt-work.cod-estabel = "99" NO-LOCK.  // s¢ totais

        FIND cm-movto-calc WHERE
             cm-movto-calc.cod-estabel    = '1' AND
             cm-movto-calc.cod-rep        = tt-work.cod-rep AND
             cm-movto-calc.data-movto-ini = da-dt-periodo-ini 
             SHARE-LOCK NO-ERROR.

        IF NOT AVAIL cm-movto-calc THEN DO.
           CREATE cm-movto-calc.
           ASSIGN cm-movto-calc.cod-estabel    = '1'
                  cm-movto-calc.cod-rep        = tt-work.cod-rep          
                  cm-movto-calc.data-movto-ini = da-dt-periodo-ini
                  cm-movto-calc.data-movto-fin = da-dt-periodo-fin.
        END.
        ASSIGN cm-movto-calc.vlr-fat        = tt-work.vlr-fat    
               cm-movto-calc.vlr-emprestimo = tt-work.emprestimo    
               cm-movto-calc.vlr-desconto   = tt-work.desconto   
               cm-movto-calc.vlr-irrf       = tt-work.i-renda
               //cm-movto-calc.vlr-devolucao  = tt-work.comis-dev
               cm-movto-calc.vlr-fat-liq    = tt-work.vlr-fat            
               cm-movto-calc.vlr-comissao   = tt-work.comissao
               cm-movto-calc.vlr-comis-liq  = tt-work.comis-liq
               cm-movto-calc.vlr-liquido    = tt-work.liquido.
    END.

    RUN pi-calc-data-venc-tit.  // calcula data de Vencimento do Titulo
    
    RUN esp/escm002d.p (INPUT-OUTPUT da-dt-venc-tit,
                        OUTPUT l-ok).
    IF NOT l-ok THEN RETURN NO-APPLY.

    // Valida e Marca Titulos a serem Integrados
    FOR EACH b-tt-repres WHERE
             b-tt-repres.marca = '*' NO-LOCK.
        FOR EACH cm-movto-calc WHERE
                 cm-movto-calc.cod-estabel    = '1' AND
                 cm-movto-calc.cod-rep        = b-tt-repres.cod-rep AND
                 cm-movto-calc.data-movto-ini = da-dt-periodo-ini SHARE-LOCK.

            FIND representante WHERE
                 representante.cdn_repres = cm-movto-calc.cod-rep NO-LOCK NO-ERROR.

            FIND fornecedor WHERE
                 fornecedor.num_pessoa = representante.num_pessoa NO-LOCK NO-ERROR.

            FIND tit_ap WHERE
                 tit_ap.cod_estab = c-cod-estab-ems5 AND
                 tit_ap.cdn_fornecedor = fornecedor.cdn_fornec AND
                 tit_ap.cod_espec_docto = 'CO' AND
                 tit_ap.cod_ser_docto = "" AND
                 tit_ap.cod_tit_ap = STRING(cm-movto-calc.data-movto-fin,"99999999") AND
                 tit_ap.dat_trans = TODAY NO-LOCK NO-ERROR.

            IF AVAIL tit_ap THEN DO.
              MESSAGE "Titulos do Representante: " cm-movto-calc.cod-rep " desse Per°odo j† foi Integrado com o Contas a Pagar," SKIP(1)
                      "Deseja Integrar Novamente ? "
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-confirma AS LOGICAL.
              IF NOT l-confirma THEN RETURN NO-APPLY.
           END.
           //ASSIGN cm-movto-calc.ap-integrado = NO.
        END.
    END.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Gerando_Titulos_Contas_a_Pagar *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    ASSIGN l-erro-integra = NO.
    FOR EACH b-tt-repres WHERE
             b-tt-repres.marca = '*' NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Repres: " + b-tt-repres.nome-abrev).

        //ASSIGN b-tt-repres.ap-integrado = NO.

        FOR EACH cm-movto-calc WHERE
                 cm-movto-calc.cod-estabel    = '1' AND
                 cm-movto-calc.cod-rep        = b-tt-repres.cod-rep AND
                 cm-movto-calc.data-movto-ini = da-dt-periodo-ini /* AND 
                 cm-movto-calc.ap-integrado   = NO */ EXCLUSIVE-LOCK.

            RUN pi-integra-titulos.

            FIND FIRST tt_log_erros_atualiz WHERE 
                       tt_log_erros_atualiz.ttv_num_mensagem <> 0 NO-LOCK NO-ERROR.
            IF NOT AVAIL tt_log_erros_atualiz THEN DO. 
               ASSIGN b-tt-repres.ap-integrado = YES.
               NEXT.
            END.

            OUTPUT STREAM s1 TO VALUE(c-arq-saida) APPEND.
                PUT STREAM s1 'ERRO ao Integrar Titulo do Representante: ' cm-movto-calc.cod-rep
                     SKIP.

                FOR EACH tt_log_erros_atualiz NO-LOCK.
                    PUT STREAM s1  UNFORMATTED 
                        "  " 
                        TRIM(tt_log_erros_atualiz.ttv_des_msg_erro) " - "   
                        TRIM(tt_log_erros_atualiz.ttv_des_msg_ajuda)
                        SKIP.
                END.
                PUT STREAM s1 " " SKIP(2).
            OUTPUT STREAM s1 CLOSE.

            ASSIGN l-erro-integra = YES.
        END.
    END.
    RUN pi-finalizar in h-acomp.


    FOR EACH b-tt-repres WHERE
             b-tt-repres.marca = '*' NO-LOCK.

        FIND cm-movto-calc WHERE
             cm-movto-calc.cod-estabel    = '1' AND
             cm-movto-calc.cod-rep        = b-tt-repres.cod-rep AND
             cm-movto-calc.data-movto-ini = da-dt-periodo-ini 
             SHARE-LOCK NO-ERROR.

        FIND representante WHERE
             representante.cdn_repres = cm-movto-calc.cod-rep NO-LOCK NO-ERROR.

        FIND fornecedor WHERE
             fornecedor.num_pessoa = representante.num_pessoa NO-LOCK NO-ERROR.

        FIND tit_ap WHERE
             tit_ap.cod_estab = c-cod-estab-ems5 AND
             tit_ap.cdn_fornecedor = fornecedor.cdn_fornec AND
             tit_ap.cod_espec_docto = 'CO' AND
             tit_ap.cod_ser_docto = "" AND
             tit_ap.cod_tit_ap = STRING(cm-movto-calc.data-movto-fin,"99999999") AND
             tit_ap.dat_trans = TODAY SHARE-LOCK NO-ERROR.

        IF AVAIL tit_ap THEN DO.
           //ASSIGN cm-movto-calc.ap-integrado = YES.

           IF tit_ap.dat_vencto <> tit_ap.dat_prev_pagto THEN
              ASSIGN tit_ap.dat_vencto = tit_ap.dat_prev_pagto.
        END.
    END.
    FIND CURRENT tit_ap NO-LOCK NO-ERROR.
    FIND CURRENT cm-movto-calc NO-LOCK NO-ERROR.

    FOR EACH b-tt-repres.
        ASSIGN b-tt-repres.marca = "".
    END.
    br-repres:REFRESH().

    IF l-erro-integra = YES THEN DO.
       MESSAGE 'ERRO ao Integrar Titulos, verifique...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

       RUN utp/ut-utils.p PERSISTENT SET h-prog.
       RUN EXECUTE IN h-prog (INPUT "notepad.exe", INPUT c-arq-saida).
       DELETE PROCEDURE h-prog.
    END.
    ELSE
       MESSAGE "Titulos Gerados, Favor Verificar no Contas a Pagar"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca B-table-Win
ON CHOOSE OF bt-marca IN FRAME F-Main
DO:
   ASSIGN tt-repres.marca = "*".
   br-repres:REFRESH().

   APPLY 'VALUE-CHANGED' TO br-repres.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica B-table-Win
ON CHOOSE OF bt-modifica IN FRAME F-Main
DO:
  tt-work.liquido:READ-ONLY IN BROWSE br-work = NO.
  APPLY 'entry' TO tt-work.liquido IN BROWSE br-work.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum B-table-Win
ON CHOOSE OF bt-nenhum IN FRAME F-Main
DO:
  FOR EACH b-tt-repres.
      ASSIGN b-tt-repres.marca = "".
  END.
  br-repres:REFRESH().
  APPLY 'VALUE-CHANGED' TO br-repres.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pri B-table-Win
ON CHOOSE OF bt-pri IN FRAME F-Main
DO:
   RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-repres
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-repres B-table-Win
ON CHOOSE OF bt-repres IN FRAME F-Main
DO:
   FIND repres WHERE
        repres.cod-rep = tt-repres.cod-rep NO-LOCK NO-ERROR.

   ASSIGN gr-repres = ROWID(repres).

   ASSIGN FRAME f-main:WINDOW:SENSITIVE = NO.
   RUN pdp/pd0904.p.
   ASSIGN FRAME f-main:WINDOW:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos B-table-Win
ON CHOOSE OF bt-todos IN FRAME F-Main
DO:
   FOR EACH b-tt-repres.
       ASSIGN b-tt-repres.marca = "*".
   END.
   br-repres:REFRESH().
   APPLY 'VALUE-CHANGED' TO br-repres.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara B-table-Win
ON CHOOSE OF bt-vapara IN FRAME F-Main
DO:
    RUN esdlg/d01escm002.w (OUTPUT i-cod-rep).

    IF i-cod-rep <> 0 THEN DO:
       FIND FIRST tt-repres WHERE
                  tt-repres.cod-rep = i-cod-rep NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-repres THEN DO.
          MESSAGE "Representante n∆o est† contido na seleá∆o!"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.

       h-query:REPOSITION-TO-ROWID(ROWID(tt-repres)) NO-ERROR. 
       APPLY 'VALUE-CHANGED' TO br-repres.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-repres
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
ASSIGN h-query = br-repres:QUERY.

tt-work.liquido:READ-ONLY IN BROWSE br-work = YES.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel B-table-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-visivel  AS LOG.
    
    ASSIGN i-nr-plan = 9.
    
    CREATE "Excel.Application" chExcelApp NO-ERROR.
    IF chExcelApp <> ? THEN /* Cria a Planilha */
       ASSIGN chExcelApp:VISIBLE     = p-visivel         /* A Planilha Ficar† Visivel */
              chExcelApp:SheetsInNewWorkbook = i-nr-plan /* Nı PLANILHAS A SEREM CRIADAS */
              chWorkbook   = chExcelApp:Workbooks:ADD()  /* Cria Planilha */
              chworksheet  = chExcelapp:sheets:ITEM(1).  /* Ativa Planilha */
    
     /* NOMEAR AS ABAS DAS PLANILHAS */
    chWorkSheet = chExcelapp:Sheets:ITEM(2).
    chWorkSheet:NAME = "Comiss‰es".
    chWorkSheet:TAB:ColorIndex = 19.
    
    chWorkSheet = chExcelapp:Sheets:ITEM(3).
    chWorkSheet:NAME = "NFs Faturadas".
    chWorkSheet:TAB:ColorIndex = 19.
    
    chWorkSheet = chExcelapp:Sheets:ITEM(4).
    chWorkSheet:NAME = "Titulos REC".
    chWorkSheet:TAB:ColorIndex = 19.
    
    chWorkSheet = chExcelapp:Sheets:ITEM(5).
    chWorkSheet:NAME = "Titulos DEV".
    chWorkSheet:TAB:ColorIndex = 19.
    
    chWorkSheet = chExcelapp:Sheets:ITEM(6).
    chWorkSheet:NAME = "Tit.Liq.CHQ".
    chWorkSheet:TAB:ColorIndex = 19.
    
    chWorkSheet = chExcelapp:Sheets:ITEM(7).
    chWorkSheet:NAME = "Descontos".
    chWorkSheet:TAB:ColorIndex = 19.
    
    chWorkSheet = chExcelapp:Sheets:ITEM(8).
    chWorkSheet:NAME = "Emprestimos".
    chWorkSheet:TAB:ColorIndex = 19.
    
    chWorkSheet = chExcelapp:Sheets:ITEM(9).
    chWorkSheet:NAME = "Comis.Informada".
    chWorkSheet:TAB:ColorIndex = 19.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-config-planilha B-table-Win 
PROCEDURE pi-config-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-orientacao AS INT.
  DEFINE INPUT PARAMETER p-zoom       AS INT.

  ASSIGN chworksheet:PageSetup:PrintGridlines = TRUE              /* Imprimir Linhas de Grade */
         chworksheet:PageSetup:CenterHorizontally = TRUE          /* Centraliza Linhas Horizontais */
         chworksheet:PageSetup:CenterVertically   = FALSE         /* Centraliza Linhas Verticais */
         chworksheet:PageSetup:rightheader = "&d - &t" + "  Pagina: &9&P De &N" 
         chworksheet:pagesetup:ORIENTATION = p-orientacao         /* PAISAGEM */
         chworksheet:pagesetup:printTitleRows = "$1:$2"           /* Imprimir Sempre em cada Pagina as linhas 1 a 2*/
         chworksheet:pagesetup:zoom = p-zoom.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-descontos B-table-Win 
PROCEDURE pi-descontos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR de-desconto LIKE cm-desc-repres.vlr-desconto.

    FOR EACH cm-desc-repres WHERE 
             cm-desc-repres.mes = MONTH(da-dt-periodo-fin) AND
             cm-desc-repres.ano = YEAR(da-dt-periodo-fin) NO-LOCK
        BREAK BY cm-desc-repres.num-desconto.

        FIND FIRST tt-calc-repres WHERE
                   tt-calc-repres.cod-rep = cm-desc-repres.cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-calc-repres THEN NEXT.

        FIND repres WHERE
             repres.cod-rep = cm-desc-repres.cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL repres THEN NEXT.
        IF repres.comis-direta = 0 THEN NEXT.

        FIND cm-ext-repres WHERE
             cm-ext-repres.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL cm-ext-repres THEN NEXT.

        IF cm-ext-repres.bloqueado THEN NEXT.

        IF repres.nome-abrev < c-no-ab-reppri-ini OR
           repres.nome-abrev > c-no-ab-reppri-fin THEN NEXT.
        RUN pi-ver-digita (INPUT "Representante",
                           INPUT repres.nome-abrev).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND cm-descontos WHERE
             cm-descontos.codigo = cm-desc-repres.num-desconto NO-LOCK NO-ERROR.

        ASSIGN de-desconto = cm-desc-repres.vlr-desconto.

        IF de-desconto > 0 THEN DO.
           FIND FIRST tt-work WHERE   /* Mesma Base Mesmo Estab */ 
                      tt-work.cod-estab = cm-desc-repres.cod-estab AND
                      tt-work.cod-rep   = cm-desc-repres.cod-rep AND
                      tt-work.base      = cm-descontos.base NO-LOCK NO-ERROR.

           IF AVAIL tt-work AND tt-work.comis-liq - tt-work.desconto > 0 THEN DO.
              IF tt-work.comis-liq - tt-work.desconto <= de-desconto THEN DO.
                 CREATE tt-descontos.
                 BUFFER-COPY cm-desc-repres TO tt-descontos
                      ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                             tt-descontos.base = tt-work.base 
                             tt-descontos.estab-ori = cm-desc-repres.cod-estab
                             tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                             tt-descontos.vlr-desconto = tt-work.comis-liq - tt-work.desconto.

                 ASSIGN tt-work.desconto = tt-work.desconto + tt-descontos.vlr-desconto.
                 ASSIGN de-desconto = de-desconto - tt-descontos.vlr-desconto.
              END.
              ELSE DO.
                 CREATE tt-descontos.
                 BUFFER-COPY cm-desc-repres TO tt-descontos
                      ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                             tt-descontos.base = tt-work.base 
                             tt-descontos.estab-ori = cm-desc-repres.cod-estab
                             tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                             tt-descontos.vlr-desconto = de-desconto.

                 ASSIGN tt-work.desconto = tt-work.desconto + tt-descontos.vlr-desconto.
                 ASSIGN de-desconto = de-desconto - tt-descontos.vlr-desconto.
              END.
              IF cm-descontos.deduz-ir AND 
                 tt-work.base = 10 THEN
                 ASSIGN tt-work.desc-base-ir = tt-work.desc-base-ir + tt-descontos.vlr-desconto.
           END.
        END.

        IF de-desconto > 0 THEN DO.
           FIND FIRST tt-work WHERE     /* Mesma Base Outro Estab */ 
                      tt-work.cod-estab <> cm-desc-repres.cod-estab AND
                      tt-work.cod-rep   = cm-desc-repres.cod-rep AND
                      tt-work.base      = cm-descontos.base NO-LOCK NO-ERROR.
           IF AVAIL tt-work AND tt-work.comis-liq - tt-work.desconto > 0 THEN DO.
              IF tt-work.comis-liq - tt-work.desconto <= de-desconto THEN DO.
                 CREATE tt-descontos.
                 BUFFER-COPY cm-desc-repres TO tt-descontos
                      ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                             tt-descontos.base = tt-work.base 
                             tt-descontos.estab-ori = cm-desc-repres.cod-estab
                             tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                             tt-descontos.vlr-desconto = tt-work.comis-liq - tt-work.desconto.

                 ASSIGN tt-work.desconto = tt-work.desconto + tt-descontos.vlr-desconto.
                 ASSIGN de-desconto = de-desconto - tt-descontos.vlr-desconto.
              END.
              ELSE DO.
                 CREATE tt-descontos.
                 BUFFER-COPY cm-desc-repres TO tt-descontos
                      ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                             tt-descontos.base = tt-work.base 
                             tt-descontos.estab-ori = cm-desc-repres.cod-estab
                             tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                             tt-descontos.vlr-desconto = de-desconto.

                 ASSIGN tt-work.desconto = tt-work.desconto + tt-descontos.vlr-desconto.
                 ASSIGN de-desconto = de-desconto - tt-descontos.vlr-desconto.
              END.

              IF cm-descontos.deduz-ir AND 
                 tt-work.base = 10 THEN
                 ASSIGN tt-work.desc-base-ir = tt-work.desc-base-ir + tt-descontos.vlr-desconto.
           END.
        END.
        

        IF de-desconto > 0 THEN DO.
           FIND FIRST tt-work WHERE     /* Outra Base Mesmo Estab */
                      tt-work.cod-estab = cm-desc-repres.cod-estab AND
                      tt-work.cod-rep   = cm-desc-repres.cod-rep AND
                      tt-work.base      <> cm-descontos.base NO-LOCK NO-ERROR.
           IF AVAIL tt-work AND tt-work.comis-liq - tt-work.desconto > 0 THEN DO.
              IF tt-work.comis-liq - tt-work.desconto <= de-desconto THEN DO.
                 CREATE tt-descontos.
                 BUFFER-COPY cm-desc-repres TO tt-descontos
                      ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                             tt-descontos.base = tt-work.base 
                             tt-descontos.estab-ori = cm-desc-repres.cod-estab
                             tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                             tt-descontos.vlr-desconto = tt-work.comis-liq - tt-work.desconto.

                 ASSIGN tt-work.desconto = tt-work.desconto + tt-descontos.vlr-desconto.
                 ASSIGN de-desconto = de-desconto - tt-descontos.vlr-desconto.
              END.
              ELSE DO.
                 CREATE tt-descontos.
                 BUFFER-COPY cm-desc-repres TO tt-descontos
                      ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                             tt-descontos.base = tt-work.base 
                             tt-descontos.estab-ori = cm-desc-repres.cod-estab
                             tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                             tt-descontos.vlr-desconto = de-desconto.

                 ASSIGN tt-work.desconto = tt-work.desconto + tt-descontos.vlr-desconto.
                 ASSIGN de-desconto = de-desconto - tt-descontos.vlr-desconto.
              END.

              IF cm-descontos.deduz-ir AND 
                 tt-work.base = 10 THEN
                 ASSIGN tt-work.desc-base-ir = tt-work.desc-base-ir + tt-descontos.vlr-desconto.

           END.
        END.


        IF de-desconto > 0 THEN DO.
           FIND FIRST tt-work WHERE     /* Outra Base Outro Estab */
                      tt-work.cod-estab <> cm-desc-repres.cod-estab AND
                      tt-work.cod-rep   = cm-desc-repres.cod-rep AND
                      tt-work.base      <> cm-descontos.base NO-LOCK NO-ERROR.
           IF AVAIL tt-work AND tt-work.comis-liq - tt-work.desconto > 0 THEN DO.
              IF tt-work.comis-liq - tt-work.desconto <= de-desconto THEN DO.
                 CREATE tt-descontos.
                 BUFFER-COPY cm-desc-repres TO tt-descontos
                      ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                             tt-descontos.base = tt-work.base 
                             tt-descontos.estab-ori = cm-desc-repres.cod-estab
                             tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                             tt-descontos.vlr-desconto = tt-work.comis-liq - tt-work.desconto.

                 ASSIGN tt-work.desconto = tt-work.desconto + tt-descontos.vlr-desconto.
                 ASSIGN de-desconto = de-desconto - tt-descontos.vlr-desconto.
              END.
              ELSE DO.
                 CREATE tt-descontos.
                 BUFFER-COPY cm-desc-repres TO tt-descontos
                      ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                             tt-descontos.base = tt-work.base 
                             tt-descontos.estab-ori = cm-desc-repres.cod-estab
                             tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                             tt-descontos.vlr-desconto = de-desconto.

                 ASSIGN tt-work.desconto = tt-work.desconto + tt-descontos.vlr-desconto.
                 ASSIGN de-desconto = de-desconto - tt-descontos.vlr-desconto.
              END.

              IF cm-descontos.deduz-ir AND 
                 tt-work.base = 10 THEN
                 ASSIGN tt-work.desc-base-ir = tt-work.desc-base-ir + tt-descontos.vlr-desconto.

           END.
        END.


        IF de-desconto > 0 THEN DO.  /* Ainda Sobrou desconto */
           FIND FIRST tt-work WHERE
                      tt-work.cod-estab = cm-desc-repres.cod-estab AND
                      tt-work.cod-rep   = cm-desc-repres.cod-rep AND
                      tt-work.comis-liq >= de-desconto NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-work THEN DO.
              FIND FIRST tt-work WHERE
                         tt-work.cod-estab = cm-desc-repres.cod-estab AND
                         tt-work.cod-rep   = cm-desc-repres.cod-rep NO-LOCK NO-ERROR.
              IF NOT AVAIL tt-work THEN DO.
                 CREATE tt-work.
                 ASSIGN tt-work.cod-estab = cm-desc-repres.cod-estab
                        tt-work.cod-rep   = cm-desc-repres.cod-rep
                        tt-work.base      = cm-descontos.base.
              END.
           END.
           ASSIGN tt-work.desconto = tt-work.desconto + de-desconto.

           FIND tt-descontos WHERE
                tt-descontos.cod-rep = tt-work.cod-rep AND
                tt-descontos.cod-estab = tt-work.cod-estab AND 
                tt-descontos.base = tt-work.base AND 
                tt-descontos.num-desconto = cm-desc-repres.num-desconto NO-ERROR.
           IF NOT AVAIL tt-descontos THEN DO.
              CREATE tt-descontos.
              BUFFER-COPY cm-desc-repres TO tt-descontos
                   ASSIGN tt-descontos.cod-estabel = tt-work.cod-estab
                          tt-descontos.estab-ori = cm-desc-repres.cod-estab
                          tt-descontos.vlr-original = cm-desc-repres.vlr-desconto
                          tt-descontos.base = tt-work.base.
           END.
           ASSIGN tt-descontos.vlr-desconto = tt-descontos.vlr-desconto + de-desconto.

           IF cm-descontos.deduz-ir AND 
              tt-work.base = 10 THEN
              ASSIGN tt-work.desc-base-ir = tt-work.desc-base-ir + de-desconto.

        END.

        IF cm-descontos.compoe-adto THEN
           ASSIGN tt-work.adiantamento = tt-work.adiantamento + cm-desc-repres.vlr-desconto.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-devolucao B-table-Win 
PROCEDURE pi-devolucao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN esapi/connect-ima-med.p.
    RUN esapi/escm002b.p (INPUT-OUTPUT TABLE tt-work,
                          INPUT-OUTPUT TABLE tt-calc-repres,
                          INPUT-OUTPUT TABLE tt-nfs,
                          INPUT-OUTPUT TABLE tt-digita,
                          INPUT c-cod-estab-ini,
                          INPUT c-cod-estab-fin,
                          INPUT da-dt-periodo-ini,
                          INPUT da-dt-periodo-fin,
                          INPUT c-no-ab-reppri-ini, 
                          INPUT c-no-ab-reppri-fin, 
                          INPUT c-it-codigo-ini,
                          INPUT c-it-codigo-fin).
    DISCONNECT dbaux.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-emprestimos B-table-Win 
PROCEDURE pi-emprestimos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR da-data-base  AS DATE.
    DEF VAR de-emprestimo LIKE cm-emprestimo.vlr-parcelas.

    FOR EACH cm-emprestimo NO-LOCK.
        ASSIGN da-data-base = DATE(LOOKUP(cm-emprestimo.mes-base,c-meses),1,cm-emprestimo.ano-base).

        IF da-dt-periodo-fin > da-data-base + (cm-emprestimo.qtd-parcelas * 30) THEN NEXT.

        FIND FIRST tt-work WHERE
                   tt-work.cod-estab = cm-emprestimo.cod-estab AND
                   tt-work.cod-rep = cm-emprestimo.cod-rep AND
                   tt-work.base      = 10
                   NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-work THEN DO:
           CREATE tt-work.
           ASSIGN tt-work.cod-estab = cm-emprestimo.cod-estab
                  tt-work.cod-rep = cm-emprestimo.cod-rep
                  tt-work.base      = 10.
        END.

        ASSIGN tt-work.emprestimo = tt-work.emprestimo + cm-emprestimo.vlr-parcelas.

        IF LOOKUP(cm-emprestimo.mes-base,c-meses) < MONTH(da-dt-periodo-fin) AND
           cm-emprestimo.ano-base <= YEAR(da-dt-periodo-fin) THEN DO.

           ASSIGN de-emprestimo = cm-emprestimo.vlr-parcelas * (cm-emprestimo.perc-juros / 100). 

           IF de-emprestimo > 0 THEN DO.
              FIND FIRST tt-work WHERE   /* Base 12 Mesmo Estab */ 
                         tt-work.cod-estab = cm-emprestimo.cod-estab AND
                         tt-work.cod-rep   = cm-emprestimo.cod-rep AND
                         tt-work.base      = 12 NO-LOCK NO-ERROR.
              IF AVAIL tt-work THEN DO.
                 ASSIGN de-emprestimo = de-emprestimo + tt-work.emprestimo.
                 ASSIGN tt-work.emprestimo = IF tt-work.comis-liq <= de-emprestimo
                                           THEN tt-work.comis-liq
                                           ELSE de-emprestimo.
                 ASSIGN de-emprestimo = de-emprestimo - tt-work.comis-liq.
              END.
           END.

            IF de-emprestimo > 0 THEN DO.
               FIND FIRST tt-work WHERE     /* Base 12 Outro Estab */ 
                          tt-work.cod-estab <> cm-emprestimo.cod-estab AND
                          tt-work.cod-rep   = cm-emprestimo.cod-rep AND
                          tt-work.base      = 12 NO-LOCK NO-ERROR.
               IF AVAIL tt-work THEN DO.
                  ASSIGN de-emprestimo = de-emprestimo + tt-work.emprestimo.
                  ASSIGN tt-work.emprestimo = IF tt-work.comis-liq <= de-emprestimo
                                            THEN tt-work.comis-liq
                                            ELSE de-emprestimo.
                  ASSIGN de-emprestimo = de-emprestimo - tt-work.comis-liq.
               END.
            END.

            IF de-emprestimo > 0 THEN DO.
               FIND FIRST tt-work WHERE     /* Base 10 Mesmo Estab */
                          tt-work.cod-estab = cm-emprestimo.cod-estab AND
                          tt-work.cod-rep   = cm-emprestimo.cod-rep AND
                          tt-work.base      = 10 NO-LOCK NO-ERROR.
               IF AVAIL tt-work THEN DO.
                  ASSIGN de-emprestimo = de-emprestimo + tt-work.emprestimo.
                  ASSIGN tt-work.emprestimo = IF tt-work.comis-liq <= de-emprestimo
                                            THEN tt-work.comis-liq
                                            ELSE de-emprestimo.
                  ASSIGN de-emprestimo = de-emprestimo - tt-work.comis-liq.
               END.
            END.

            IF de-emprestimo > 0 THEN DO.
               FIND FIRST tt-work WHERE     /* Base 10 Outro Estab */
                          tt-work.cod-estab <> cm-emprestimo.cod-estab AND
                          tt-work.cod-rep   = cm-emprestimo.cod-rep AND
                          tt-work.base      = 10 NO-LOCK NO-ERROR.
               IF AVAIL tt-work THEN DO.
                  ASSIGN de-emprestimo = de-emprestimo + tt-work.emprestimo.
                  ASSIGN tt-work.emprestimo = IF tt-work.comis-liq <= de-emprestimo
                                            THEN tt-work.comis-liq
                                            ELSE de-emprestimo.
                  ASSIGN de-emprestimo = de-emprestimo - tt-work.comis-liq.
               END.
            END.

            IF de-emprestimo > 0 THEN DO.  /* Ainda Sobrou emprestimo */
               FIND FIRST tt-work WHERE
                          tt-work.cod-estab = cm-emprestimo.cod-estab AND
                          tt-work.cod-rep   = cm-emprestimo.cod-rep AND
                          tt-work.comis-liq >= de-emprestimo NO-LOCK NO-ERROR.
               IF NOT AVAIL tt-work THEN DO.
                  FIND FIRST tt-work WHERE
                             tt-work.cod-estab = cm-emprestimo.cod-estab AND
                             tt-work.cod-rep   = cm-emprestimo.cod-rep NO-LOCK NO-ERROR.
                  IF NOT AVAIL tt-work THEN DO.
                     CREATE tt-work.
                     ASSIGN tt-work.cod-estab = cm-emprestimo.cod-estab
                            tt-work.cod-rep   = cm-emprestimo.cod-rep
                            tt-work.base      = 12.
                  END.
                  ASSIGN tt-work.emprestimo = tt-work.emprestimo + de-emprestimo.
               END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-envia-email B-table-Win 
PROCEDURE pi-envia-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR de-tot-receber LIKE tt-work.liquido.
    DEF VAR c-arq-email AS CHAR.
    DEF VAR c-destinatario AS CHAR.
    DEF VAR c-mensagem AS CHAR.

    FIND FIRST b-tt-repres WHERE
               b-tt-repres.marca = "*"  NO-LOCK NO-ERROR.


    IF NOT AVAIL b-tt-repres THEN DO:
       MESSAGE "Favor marcar os Representantes, que deseja enviar o e-mail..."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN NO-APPLY.
    END.

    FOR EACH b-tt-repres WHERE
             b-tt-repres.visualiza = YES AND
             b-tt-repres.marca = "*" NO-LOCK.
        
        ASSIGN c-mensagem = "Belo Horizonte, " + STRING(TODAY,"99/99/9999") + CHR(13) +
                            "Sr(a) " + b-tt-repres.nome                     + CHR(13) +
                            "Segue abaixo valores para emiss∆o da Nota Fiscal de Comiss‰es, referente as vendas efetuadas no màs de " + ENTRY(MONTH(da-dt-periodo-fin),c-meses) + "/" + STRING(YEAR(da-dt-periodo-fin),"9999") + CHR(13) +
                            "Favor enviar Fax da Nota Fiscal atÇ " + STRING(da-data-lim-nf,"99/99/9999") + "as 18:00hs para pagamento dia " + STRING(da-data-pagto,"99/99/9999") + "." + CHR(13) + CHR(13) + 
                            "Atená∆o para o Novo Endereáo da IMA. NÉo aceitaremos Notas com o endereáo da Timbiras." + CHR(13) + CHR(13) +
                            "OBSERVAÄ«O" + CHR(13) +
                            c-observacao + CHR(13) + CHR(13) +
                            "DADOS:" + CHR(13) + CHR(13) + 
                            "Atená∆o Novo Endereáo" + CHR(13) + CHR(13) +
                            "Via Expressa de Contagem, 3350" + CHR(13) +
                            "Bairro Perobas" + CHR(13) +
                            "Contagem - MG  32.040.025" + CHR(13) +
                            "CNPJ: 21.126.271/0001-68" + CHR(13) +
                            "Insc: 062.290.127.0050" + CHR(13) + CHR(13) + CHR(13) +
                            "Medtextil Importaá∆o e Exportaá∆o Ltda" + CHR(13) +
                            "Rodovia Darly Santos, 4900" + CHR(13) +
                            "Bairro Darly Santos" + CHR(13) +
                            "Vila Velha - ES  29.103.091" + CHR(13) +
                            "CNPJ: 06.013.812/0001-58" + CHR(13) +
                            "Insc: 082.245.886".

        ASSIGN c-arq-email = SESSION:TEMP-DIRECTORY + b-tt-repres.nome-abrev + ".txt".
        OUTPUT TO VALUE(c-arq-email) CONVERT SOURCE "ibm850".    

        RUN pi-imp-cabec.
        ASSIGN i-pag = 1
               i-lin = 7.

        PUT "Representante: "      AT 01
            b-tt-repres.cod-rep    "-"
            b-tt-repres.nome       
            SKIP(1).

        ASSIGN i-lin = i-lin + 2.

        PUT "Est Base  Faturamento   Comiss∆o  Devoluá∆o   Liquidez    Fat. Liq. Comis.Liq.   Desconto       I.R. Emprestimo    Adiant.    A RECEBER    A FATURAR " SKIP
            "--- ---- ------------ ---------- ---------- ---------- ------------ ---------- ---------- ---------- ---------- ---------- ------------ ------------ " SKIP.

        ASSIGN de-tot-receber = 0.
        FOR EACH b-tt-work WHERE
                 b-tt-work.cod-rep = b-tt-repres.cod-rep
                 NO-LOCK BY b-tt-work.cod-estab.

            IF b-tt-work.base = 12 THEN NEXT.

            IF b-tt-work.cod-estab = '98' OR
               b-tt-work.cod-estabel = '99' THEN NEXT.

            PUT b-tt-work.cod-estabel   AT 01  FORMAT "x(3)"
                b-tt-work.base          AT 05  FORMAT ">>>9" 
                b-tt-work.vlr-fat       AT 10  FORMAT ">,>>>,>>9.99" 
                b-tt-work.comissao      AT 23  FORMAT ">>>,>>9.99" 
                b-tt-work.devolucao     AT 34  FORMAT ">>>,>>9.99" 
                b-tt-work.liquidez      AT 45  FORMAT ">>>,>>9.99" 
                b-tt-work.fat-liq       AT 56  FORMAT ">,>>>,>>9.99" 
                b-tt-work.comis-liq     AT 69  FORMAT ">>>,>>9.99" 
                b-tt-work.desconto      AT 80  FORMAT ">>>,>>9.99" 
                b-tt-work.i-renda       AT 91  FORMAT ">>>,>>9.99" 
                b-tt-work.emprestimo    AT 102 FORMAT ">>>,>>9.99" 
                b-tt-work.adiantamento  AT 113 FORMAT ">>>,>>9.99" 
                b-tt-work.liquido       AT 124 FORMAT ">,>>>,>>9.99" 
                b-tt-work.vlr-nf        AT 137 FORMAT ">,>>>,>>9.99" 
                SKIP.

            ASSIGN i-lin = i-lin + 1.
        END.
        OUTPUT CLOSE.

        FIND repres WHERE
             repres.nome-abrev = b-tt-repres.nome-abrev NO-LOCK.
        IF NOT AVAIL repres OR repres.e-mail = "" THEN DO:
           MESSAGE "E-mail do Representante " repres.nome-abrev " n∆o Cadastrado..."
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           NEXT.
        END.  

        RUN esapi/esapi002.p (INPUT "imatextil@imatextil.com.br", /* e-mail remetente     */
                              INPUT repres.e-mail,                /* e-mail destinat†rio  */ 
                              INPUT "Comiss∆o do representante",  /* Assunto              */
                              INPUT c-mensagem,                   /* Mensagem             */
                              INPUT c-arq-email,                  /* arquivo anexo        */           
                              INPUT YES).                         /* Mostra Erros         */
    END.

    FOR EACH b-tt-repres.
        ASSIGN b-tt-repres.marca = "".
    END.
    br-repres:REFRESH() IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder1 B-table-Win 
PROCEDURE pi-folder1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
      /* Posiciona na Planilha 1, Salva e Fecha */
     chWorkSheet = chExcelapp:Sheets:ITEM(1).
     chWorkbook:Worksheets(1):activate.
     chExcelApp:Range("A3"):SELECT.
     chWorkSheet:NAME = "Resumo Geral".
     chWorkSheet:TAB:ColorIndex = 19.
     chExcelApp:ActiveWindow:Zoom = 100.
    
     /* Posiciona o Foco no Inicio da Planilha */
     chExcelApp:Range("A:A"):EntireColumn:AutoFit.

     ASSIGN chworksheet:range("B1"):VALUE = "RESUMO COMISSÂES PER÷ODO: " + STRING(da-dt-periodo-ini, "99/99/9999") +
                                            " A " + STRING (da-dt-periodo-fin, "99/99/9999").
    
     /* Configura Alinhamento Horizontal do Titulo da Planilha */
     ChWorkSheet:range("B1:L1"):SELECT().
     ChWorksheet:range("B1:L1"):Merge.
     Chworksheet:Range("B1:L1"):HorizontalAlignment = 3. /* Centralizado */
     Chworksheet:Range("B1:L1"):VerticalAlignment   = 2. /* Centralizado */
    
     /* Colorir Titulo da Planilha */
     chWorkSheet:Range("B1:L1"):FONT:ColorIndex     = 11. /* Avermelhado */
     chWorkSheet:Range("B1:L1"):Interior:ColorIndex = 2. /* Branco */
    
     /* Configura a Linha do Titulo da Planilha */
     ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 45
            chWorkSheet:Rows("1:1"):FONT:NAME = 'ARIAL'
            chWorkSheet:Rows("1:1"):FONT:SIZE = 26
            chWorkSheet:Rows("1:1"):FONT:bold = TRUE.
    
     /* Inserir Logotipo da Tear e Alinhar Via Tamanho e Altura Logotipo */
     //ASSIGN FILE-INFO:FILE-NAME = SEARCH("image/ima-dup.jpg").
     //ChWorkSheet:range("A1"):SELECT().
     //ChWorkSheet:Pictures:INSERT(FILE-INFO:FULL-PATHNAME):SELECT. 
     //chExcelApp:SELECTION:ShapeRange:ScaleWidth(1.9,FALSE,FALSE).
     //chExcelApp:SELECTION:ShapeRange:ScaleHeight(0.61,FALSE,FALSE). 
     //chExcelApp:SELECTION:ShapeRange:IncrementLeft(-49.5).
     //chExcelApp:SELECTION:ShapeRange:IncrementTop( -48.75).
    
     /* Titulo das Colunas */
     chWorkSheet:range("A2:L2"):Borders(8):Weight = 3. // Linha Cima
     ASSIGN chworksheet:range("C2"):VALUE = "COMISS«O"    
            chworksheet:range("D2"):VALUE = "COMISS«O"    
            chworksheet:range("E2"):VALUE = "COMISSAO"    
            chworksheet:range("F2"):VALUE = "COMISSAO"    
            chworksheet:range("G2"):VALUE = "COMISS«O"
            chworksheet:range("H2"):VALUE = "COMISS«O".
    
     ASSIGN chworksheet:range("A3"):VALUE = "COD."
            chworksheet:range("B3"):VALUE = "REPRESENTANTE"
            chworksheet:range("C3"):VALUE = "  FAT."    
            chworksheet:range("D3"):VALUE = "TITULOS"
            chworksheet:range("E3"):VALUE = "CHEQUE"
            chworksheet:range("F3"):VALUE = "INF."
            chworksheet:range("G3"):VALUE = "ESTORNADA"
            chworksheet:range("H3"):VALUE = "BRUTA"    
            chworksheet:range("I3"):VALUE = "I.RENDA"
            chworksheet:range("J3"):VALUE = "DESCONTOS"
            chworksheet:range("K3"):VALUE = "EMPRESTIMOS"
            chworksheet:range("L3"):VALUE = "A RECEBER". 
    
     chWorkSheet:range("A4:L4"):Borders(8):Weight = 3. // Linha Baixo
     chWorkSheet:range("A2:L3"):Borders(2):Weight = 3. // Coluna das Laterais
     chWorkSheet:range("A2:L3"):Borders(1):Weight = 3. // Linha de Baixo
    
     /* Tamanho das Colunas */
     ASSIGN chWorkSheet:Columns("A"):ColumnWidth =  5.00
            chWorkSheet:Columns("B"):ColumnWidth = 54.00
            chWorkSheet:Columns("C"):ColumnWidth = 13.00
            chWorkSheet:Columns("D"):ColumnWidth = 13.00
            chWorkSheet:Columns("E"):ColumnWidth = 14.00
            chWorkSheet:Columns("F"):ColumnWidth = 14.00
            chWorkSheet:Columns("G"):ColumnWidth = 15.00
            chWorkSheet:Columns("H"):ColumnWidth = 14.00
            chWorkSheet:Columns("I"):ColumnWidth = 12.00
            chWorkSheet:Columns("J"):ColumnWidth = 15.00
            chWorkSheet:Columns("K"):ColumnWidth = 19.00
            chWorkSheet:Columns("L"):ColumnWidth = 14.00.
    
     /* Configura as Colunas da Planilha */
     ASSIGN chworksheet:range("A:B"):NumberFormat        = "@"
            chworksheet:range("C:L"):NumberFormat        = "###.###.##0,00"
            Chworksheet:range("C:L"):HorizontalAlignment = 4.
    
     /* Configura Cabeáalho das Colunas */
     chWorkSheet:Range("A2:L3"):SELECT().
     ASSIGN chExcelApp:SELECTION:FONT:NAME               = "ARIAL"
            chExcelApp:SELECTION:FONT:SIZE               = 12
            chExcelApp:SELECTION:FONT:Bold               = TRUE 
            chExcelApp:SELECTION:Interior:ColorIndex     = 37
            chExcelApp:SELECTION:FONT:ColorIndex         = 11.
    
     ASSIGN i-Lin1 = 4.
    
     ASSIGN de-comis = 0 de-comis-chq = 0 de-comis-inf = 0 de-est-tit = 0 de-ir    = 0     
            de-desc  = 0 de-emp       = 0 de-rec       = 0.
    
     FOR EACH b-tt-repres WHERE
              b-tt-repres.marca = "*" NO-LOCK 
           BY b-tt-repres.cod-rep.
    
         FIND repres WHERE
              repres.cod-rep = b-tt-repres.cod-rep NO-LOCK NO-ERROR.
    
    
         FOR EACH b-tt-work WHERE
                  b-tt-work.cod-rep      = b-tt-repres.cod-rep AND
                  b-tt-work.cod-estabel = "99" NO-LOCK 
         BREAK BY b-tt-work.cod-rep
               BY b-tt-work.cod-estab.
    
             IF FIRST-OF(b-tt-work.cod-rep) THEN DO.
                FIND emitente WHERE
                     emitente.nome-emit = b-tt-repres.nome NO-LOCK NO-ERROR.
                ASSIGN chworksheet:range("A" + STRING(i-lin1)):VALUE = STRING(b-tt-repres.cod-rep)
                       chworksheet:range("B" + STRING(i-lin1)):VALUE = IF AVAIL emitente THEN STRING(emitente.cod-emitente) + " - " + b-tt-repres.nome                 
                                                                                         ELSE b-tt-repres.nome.
             END.
    
             ASSIGN de-comis     = de-comis     + b-tt-work.comis-liq   
                    //de-comis-chq = de-comis-chq + b-tt-work.comis-chq   
                    //de-comis-inf = de-comis-inf + b-tt-work.comis-inf   
                    //de-est-tit   = de-est-tit   + b-tt-work.comis-dev // b-tt-work.vlr-tit-dev 
                    de-ir        = de-ir        + b-tt-work.i-renda      
                    de-desc      = de-desc      + b-tt-work.desconto     
                    de-emp       = de-emp       + b-tt-work.emprestimo   
                    de-rec       = de-rec       + b-tt-work.liquido. 
    
             ASSIGN l-nachou-nfs = NO.
             FOR EACH tt-nfs WHERE
                      tt-nfs.cod-rep = b-tt-repres.cod-rep NO-LOCK
                BREAK BY tt-nfs.cod-rep.
                      //BY tt-nfs.nome-emit.
    
    
                 ACCUMULATE tt-nfs.comissao    (TOTAL BY tt-nfs.cod-rep).
    
                 IF LAST-OF(tt-nfs.cod-rep) THEN 
                    ASSIGN chworksheet:range("C" + STRING(i-lin1)):VALUE = (ACCUM TOTAL BY tt-nfs.cod-rep tt-nfs.comissao)
                           l-nachou-nfs = YES
                           de-emissao   = de-emissao + (ACCUM TOTAL BY tt-nfs.cod-rep tt-nfs.comissao).
             END.
             IF NOT l-nachou-nfs THEN
                ASSIGN chworksheet:range("C" + STRING(i-lin1)):VALUE = 0.00.
    
             /*
             FOR EACH tt-tit WHERE
                      tt-tit.cod-rep = b-tt-repres.cod-rep AND
                      tt-tit.tipo-mov = 'CRE' NO-LOCK
                BREAK BY tt-tit.cod-rep
                      BY tt-tit.nome-emit
                      BY tt-tit.nr-docto.
    
                 ACCUMULATE tt-tit.comissao (TOTAL BY tt-tit.cod-rep).
    
                 IF LAST-OF(tt-tit.cod-rep) THEN 
                    ASSIGN chworksheet:range("D" + STRING(i-lin1)):VALUE = (ACCUM TOTAL BY tt-tit.cod-rep tt-tit.comissao)
                           de-baixa = de-baixa + (ACCUM TOTAL BY tt-tit.cod-rep tt-tit.comissao).
             END.
             */
             ASSIGN //chworksheet:range("E" + STRING(i-lin1)):VALUE =  b-tt-work.comis-chq    
                    //chworksheet:range("F" + STRING(i-lin1)):VALUE =  b-tt-work.comis-inf   
                    //chworksheet:range("G" + STRING(i-lin1)):VALUE =  b-tt-work.comis-dev // b-tt-work.vlr-tit-dev 
                    chworksheet:range("H" + STRING(i-lin1)):VALUE =  b-tt-work.comissao
                    chworksheet:range("I" + STRING(i-lin1)):VALUE =  b-tt-work.i-renda 
                    chworksheet:range("J" + STRING(i-lin1)):VALUE =  b-tt-work.desconto    
                    chworksheet:range("K" + STRING(i-lin1)):VALUE =  b-tt-work.emprestimo  
                    chworksheet:range("L" + STRING(i-lin1)):VALUE =  b-tt-work.liquido. 
    
             chWorkSheet:range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Borders(8):Weight = 3.
             chWorkSheet:range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Borders(2):Weight = 3.
             chWorkSheet:range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Borders(4):Weight = 3.
             chWorkSheet:range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Borders(1):Weight = 3.
    
             chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:NAME = "Arial".
             chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:Bold = TRUE.
             chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:SIZE = 10.
    
             chWorkSheet:Range("G" + STRING(i-Lin1) + ":G" + STRING(i-Lin1)):FONT:ColorIndex     =  9. /* Letra Vermelha */ 
             chWorkSheet:Range("G" + STRING(i-Lin1) + ":G" + STRING(i-Lin1)):Interior:ColorIndex =  2. /* Fundo Branco */
    
             chWorkSheet:Range("I" + STRING(i-Lin1) + ":K" + STRING(i-Lin1)):FONT:ColorIndex     =  9. /* Letra Vermelha */ 
             chWorkSheet:Range("I" + STRING(i-Lin1) + ":K" + STRING(i-Lin1)):Interior:ColorIndex =  2. /* Fundo Branco */
    
             IF b-tt-work.liquido > 0 THEN DO:
                chWorkSheet:Range("L" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:ColorIndex     = 11. /* Letra AZUL */ 
                chWorkSheet:Range("L" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Interior:ColorIndex =  2. /* Fundo Branco */
             END.
             ELSE DO:
               chWorkSheet:Range("L" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:ColorIndex     =  9. /* Letra Vermelha */ 
               chWorkSheet:Range("L" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Interior:ColorIndex =  2. /* Fundo Branco */
             END.
    
             ASSIGN i-lin1 = i-lin1 + 1.
         END.
     END.
     IF de-comis + de-ir + de-desc + de-emp + de-rec > 0 THEN DO:
        ASSIGN chworksheet:range("B" + STRING(i-lin1)):VALUE = "T O T A L  . . ."
               chworksheet:range("C" + STRING(i-lin1)):VALUE = de-emissao 
               //chworksheet:range("D" + STRING(i-lin1)):VALUE = de-baixa
               //chworksheet:range("E" + STRING(i-lin1)):VALUE = de-comis-chq 
               //chworksheet:range("F" + STRING(i-lin1)):VALUE = de-comis-inf 
               //chworksheet:range("G" + STRING(i-lin1)):VALUE = de-est-tit 
               chworksheet:range("H" + STRING(i-lin1)):VALUE = de-comis 
               chworksheet:range("I" + STRING(i-lin1)):VALUE = de-ir   
               chworksheet:range("J" + STRING(i-lin1)):VALUE = de-desc  
               chworksheet:range("K" + STRING(i-lin1)):VALUE = de-emp   
               chworksheet:range("L" + STRING(i-lin1)):VALUE = de-rec. 
    
        chWorkSheet:range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Borders(8):Weight = 3.
        chWorkSheet:range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Borders(2):Weight = 3.
        chWorkSheet:range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Borders(4):Weight = 3.
        chWorkSheet:range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Borders(1):Weight = 3.
    
        chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:NAME       = "Arial".
        chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:Bold       = TRUE.
        chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:SIZE       = 12.
    
        chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:ColorIndex     = 11. /* Letra Vermelha */ 
        chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):Interior:ColorIndex = 37. /* Fundo Branco */
    
        ASSIGN i-Lin1 = i-Lin1 + 4.
        ASSIGN chworksheet:range("B" + STRING(i-lin1)):VALUE = FILL("_",68).
        ASSIGN i-Lin1 = i-Lin1 + 1.
        ASSIGN chworksheet:range("B" + STRING(i-lin1)):VALUE = '              IMA TECIDOS DA MODA LTDA'.
        chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:NAME       = "Arial".
        chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:Bold       = TRUE.
        chWorkSheet:Range("A" + STRING(i-Lin1) + ":L" + STRING(i-Lin1)):FONT:SIZE       = 12.
     END.
    
     ASSIGN chworksheet:PageSetup:PrintGridlines     = FALSE         /* Imprimir Linhas de Grade */
            chworksheet:PageSetup:CenterHorizontally = TRUE          /* Centraliza Linhas Horizontais */
            chworksheet:PageSetup:CenterVertically   = FALSE         /* Centraliza Linhas Verticais */
            chworksheet:PageSetup:rightheader    = "&d - &t" + "  Pagina: &9&P De &N"
            chworksheet:pagesetup:ORIENTATION    = 2  /* PAISAGEM         */
            chWorkSheet:PageSetup:LeftMargin     =  0 /* Margem esquerda  */
            chWorkSheet:PageSetup:RightMargin    =  0 /* Margem direita   */
            chWorkSheet:PageSetup:TopMargin      = 40 /* Margem superior  */
            chWorkSheet:PageSetup:BottomMargin   = 40 /* Margem inferior  */
            chWorkSheet:PageSetup:HeaderMargin   =  0 /* Margem cabeáalho */
            chWorkSheet:PageSetup:FooterMargin   =  0 /* Margem rodapÇ    */
            chWorkSheet:PageSetup:Zoom           = FALSE   /*  ZOOM tem que ser desativado para funcionar */
            chWorkSheet:PageSetup:FitToPagesWide = 1       /*  Expandir a Planilha por Toda a Folha A4 apro */
            chWorkSheet:PageSetup:FitToPagesTall = 1000.   /*  veitando do o formulario. */
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$L$" + STRING(i-lin1).  /* Marcar Area de Impressao */
    
     RUN pi-config-planilha (INPUT 2, INPUT 75). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder2 B-table-Win 
PROCEDURE pi-folder2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     ASSIGN de-tot-chq = 0.
     chWorkSheet = chExcelapp:Sheets:ITEM(2). /* Ativar a Planilha */
     chWorkbook:Worksheets(2):activate.
    
     ASSIGN i-lin1 = 4.
     FOR EACH b-tt-work WHERE
              b-tt-work.cod-rep      = b-tt-repres.cod-rep AND
              b-tt-work.cod-estabel <> "98" NO-LOCK 
     BREAK BY b-tt-work.cod-rep
           BY b-tt-work.cod-estab.
    
         IF FIRST-OF(b-tt-work.cod-rep) THEN
            ASSIGN chworksheet:range("A" + STRING(i-lin1)):VALUE = b-tt-repres.nome.                  
         IF b-tt-work.cod-estabel <> "99" THEN
            ASSIGN chworksheet:range("B" + STRING(i-lin1)):VALUE = b-tt-work.base.                  
         ELSE DO:
            ASSIGN chworksheet:range("B" + STRING(i-lin1)):VALUE = "TOTAL".   
            ASSIGN chWorkSheet:Range("A" + STRING(i-lin1) + ":I" + STRING(i-lin1)):Interior:ColorIndex = 50
                   chWorkSheet:Range("L" + STRING(i-lin1) + ":M" + STRING(i-lin1)):Interior:ColorIndex = 50
                   chWorkSheet:Range("J" + STRING(i-lin1) + ":K" + STRING(i-lin1)):Interior:ColorIndex = 50

                   chWorkSheet:Range("A" + STRING(i-lin1) + ":I" + STRING(i-lin1)):FONT:ColorIndex     =  2
                   chWorkSheet:Range("L" + STRING(i-lin1) + ":M" + STRING(i-lin1)):FONT:ColorIndex     =  2
                   chWorkSheet:Range("J" + STRING(i-lin1) + ":K" + STRING(i-lin1)):FONT:ColorIndex     =  2

                   chWorkSheet:Range("A" + STRING(i-lin1) + ":I" + STRING(i-lin1)):FONT:Bold           = FALSE
                   chWorkSheet:Range("L" + STRING(i-lin1) + ":L" + STRING(i-lin1)):FONT:Bold           = FALSE
                   chWorkSheet:Range("J" + STRING(i-lin1) + ":K" + STRING(i-lin1)):FONT:Bold           = TRUE.
         END.
    
         ASSIGN chworksheet:range("C" + STRING(i-lin1)):VALUE = b-tt-work.vlr-fat                  
                chworksheet:range("D" + STRING(i-lin1)):VALUE = b-tt-work.devolucao 
                chworksheet:range("E" + STRING(i-lin1)):VALUE = b-tt-work.fat-liq   
                chworksheet:range("F" + STRING(i-lin1)):VALUE = b-tt-work.comissao
                chworksheet:range("G" + STRING(i-lin1)):VALUE = b-tt-work.comis-dev
                chworksheet:range("H" + STRING(i-lin1)):VALUE = b-tt-work.comis-liq 
                chworksheet:range("I" + STRING(i-lin1)):VALUE = b-tt-work.desconto  
                chworksheet:range("J" + STRING(i-lin1)):VALUE = b-tt-work.i-renda 
                chworksheet:range("K" + STRING(i-lin1)):VALUE = b-tt-work.emprestimo  
                chworksheet:range("L" + STRING(i-lin1)):VALUE = b-tt-work.adiantamento
                chworksheet:range("M" + STRING(i-lin1)):VALUE = b-tt-work.liquido. 
    
         ASSIGN i-lin1 = i-lin1 + 1.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder3 B-table-Win 
PROCEDURE pi-folder3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEF VAR c-tab-preco-ref AS CHAR.
     DEF VAR de-perc-reduc-comis AS DEC.
     DEF VAR de-perc-comis AS DEC FORMAT ">>9.9999".

     chWorkSheet = chExcelapp:Sheets:ITEM(3). /* Ativar a Planilha */
     chWorkbook:Worksheets(3):activate.
     FOR EACH tt-nfs WHERE
              tt-nfs.cod-rep = b-tt-repres.cod-rep NO-LOCK
         BREAK BY tt-nfs.cod-rep BY tt-nfs.base.

         IF FIRST-OF(tt-nfs.cod-rep) THEN
            ASSIGN chworksheet:range("A" + STRING(i-lin2)):VALUE = b-tt-repres.nome.   
    
         FIND nota-fiscal WHERE
              nota-fiscal.cod-estabel = tt-nfs.cod-estabel AND
              nota-fiscal.serie       = tt-nfs.serie       AND
              nota-fiscal.nr-nota-fis = tt-nfs.nr-nota-fis NO-LOCK NO-ERROR.
    
         FIND emitente WHERE
              emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
    
         FIND ped-venda WHERE
              ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
    
         FIND ped-venda-ext WHERE
              ped-venda-ext.cod-estab = ped-venda.cod-estab AND
              ped-venda-ext.nr-pedido = ped-venda.nr-pedido 
              NO-LOCK NO-ERROR.
    
         ASSIGN c-tab-preco-ref = 'Padrao'
                de-perc-reduc-comis = 0.

         FIND tbs_preco WHERE
              tbs_preco.tb_preco_id = ped-venda-ext.tb_preco_id NO-LOCK NO-ERROR.
         IF AVAIL tbs_preco THEN
            ASSIGN c-tab-preco-ref = tbs_preco.descricao 
                   de-perc-reduc-comis = tbs_preco.perc_reduc_comis.

         FIND ped-repre OF ped-venda WHERE
              ped-repre.nome-ab-rep = b-tt-repres.nome-abrev NO-LOCK NO-ERROR.
    
         // Procura o perentual de comiss∆o por estado (UF) 
         FIND cm-unid-feder OF b-tt-repres WHERE
              cm-unid-feder.uf = nota-fiscal.estado NO-LOCK NO-ERROR.
         IF AVAIL cm-unid-feder THEN DO.
            ASSIGN de-perc-comis = cm-unid-feder.perc-comis * (100 - de-perc-reduc-comis) / 100. 
         END.                               
         ELSE DO.   
            ASSIGN de-perc-comis = IF AVAIL ped-repre
                                   THEN ped-repre.perc-comis
                                   ELSE b-tt-repres.comis-direta * (100 - de-perc-reduc-comis) / 100.
         END.
         
         ASSIGN chworksheet:range("B" + STRING(i-lin2)):VALUE = ped-venda-ext.preposto
                chworksheet:range("C" + STRING(i-lin2)):VALUE = tt-nfs.base
                chworksheet:range("D" + STRING(i-lin2)):VALUE = emitente.nome-emit
                chworksheet:range("E" + STRING(i-lin2)):VALUE = nota-fiscal.estado
                chworksheet:range("F" + STRING(i-lin2)):VALUE = tt-nfs.nr-nota-fis
                chworksheet:range("G" + STRING(i-lin2)):VALUE = STRING(nota-fiscal.dt-emis-nota, "99/99/9999") 
                chworksheet:range("H" + STRING(i-lin2)):VALUE = tt-nfs.vl-tot-nota
                chworksheet:range("I" + STRING(i-lin2)):VALUE = ped-venda.nr-pedcli
                chworksheet:range("J" + STRING(i-lin2)):VALUE = c-tab-preco-ref
                chworksheet:range("K" + STRING(i-lin2)):VALUE = de-perc-comis
                chworksheet:range("L" + STRING(i-lin2)):VALUE = tt-nfs.comissao.    
    
         ASSIGN i-lin2 = i-lin2 + 1.
    
         ACCUMULATE tt-nfs.vl-tot-nota (TOTAL BY tt-nfs.cod-rep).
         ACCUMULATE tt-nfs.comissao    (TOTAL BY tt-nfs.cod-rep).
    
         IF LAST-OF(tt-nfs.cod-rep) THEN DO:
            ASSIGN chworksheet:range("H" + STRING(i-lin2)):VALUE = (ACCUM TOTAL BY tt-nfs.cod-rep tt-nfs.vl-tot-nota) 
                   chworksheet:range("L" + STRING(i-lin2)):VALUE = (ACCUM TOTAL BY tt-nfs.cod-rep tt-nfs.comissao).
    
            ASSIGN chWorkSheet:Range("A" + STRING(i-lin2) + ":L" + STRING(i-lin2)):Interior:ColorIndex = 31
                   chWorkSheet:Range("A" + STRING(i-lin2) + ":L" + STRING(i-lin2)):FONT:ColorIndex     =  2
                   chWorkSheet:Range("A" + STRING(i-lin2) + ":L" + STRING(i-lin2)):FONT:Bold           = FALSE.
            ASSIGN i-lin2= i-lin2 + 2. 
         END.
     END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder4 B-table-Win 
PROCEDURE pi-folder4 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

     chWorkSheet = chExcelapp:Sheets:ITEM(4). /* Ativar a Planilha */
     chWorkbook:Worksheets(4):activate.

     /*
     FOR EACH tt-tit WHERE
              tt-tit.cod-rep = b-tt-repres.cod-rep AND
              tt-tit.tipo-mov = 'CRE' NO-LOCK
        BREAK BY tt-tit.cod-rep
              BY tt-tit.nome-emit
              BY tt-tit.nr-docto.
    
         IF FIRST-OF(tt-tit.cod-rep) THEN
            ASSIGN chworksheet:range("A" + STRING(i-lin3)):VALUE = b-tt-repres.nome.                  
    
         ASSIGN chworksheet:range("B" + STRING(i-lin3)):VALUE =  tt-tit.cod-estabel            
                chworksheet:range("C" + STRING(i-lin3)):VALUE =  tt-tit.cod-emit  
                chworksheet:range("D" + STRING(i-lin3)):VALUE =  tt-tit.nome-emit 
                chworksheet:range("E" + STRING(i-lin3)):VALUE =  tt-tit.nr-docto    
                chworksheet:range("F" + STRING(i-lin3)):VALUE =  tt-tit.parcela     
                chworksheet:range("G" + STRING(i-lin3)):VALUE =  tt-tit.vl-original 
                chworksheet:range("H" + STRING(i-lin3)):VALUE =  tt-tit.comissao.    
    
         ASSIGN i-lin3 = i-lin3 + 1.
    
         ACCUMULATE tt-tit.vl-original (TOTAL BY tt-tit.cod-rep).
         ACCUMULATE tt-tit.comissao    (TOTAL BY tt-tit.cod-rep).
    
         IF LAST-OF(tt-tit.cod-rep) THEN DO:
            ASSIGN chworksheet:range("G" + STRING(i-lin3)):VALUE = (ACCUM TOTAL BY tt-tit.cod-rep tt-tit.vl-original) 
                   chworksheet:range("H" + STRING(i-lin3)):VALUE = (ACCUM TOTAL BY tt-tit.cod-rep tt-tit.comissao).
    
            ASSIGN chWorkSheet:Range("A" + STRING(i-lin3) + ":H" + STRING(i-lin3)):Interior:ColorIndex = 31
                   chWorkSheet:Range("A" + STRING(i-lin3) + ":H" + STRING(i-lin3)):FONT:ColorIndex     =  2
                   chWorkSheet:Range("A" + STRING(i-lin3) + ":H" + STRING(i-lin3)):FONT:Bold           = FALSE.
            ASSIGN i-lin3= i-lin3 + 1. 
         END.
     END.
    
     chWorkSheet = chExcelapp:Sheets:ITEM(5). /* Ativar a Planilha */
     chWorkbook:Worksheets(5):activate.
     FOR EACH tt-tit WHERE
              tt-tit.cod-rep = b-tt-repres.cod-rep AND
              tt-tit.tipo-mov = 'DEB' NO-LOCK
        BREAK BY tt-tit.cod-rep
              BY tt-tit.nome-emit
              BY tt-tit.nr-docto.
    
         IF FIRST-OF(tt-tit.cod-rep) THEN
            ASSIGN chworksheet:range("A" + STRING(i-lin4)):VALUE = b-tt-repres.nome.                  
    
         ASSIGN chworksheet:range("B" + STRING(i-lin4)):VALUE =  tt-tit.cod-estabel            
                chworksheet:range("C" + STRING(i-lin4)):VALUE =  tt-tit.cod-emit  
                chworksheet:range("D" + STRING(i-lin4)):VALUE =  tt-tit.nome-emit 
                chworksheet:range("E" + STRING(i-lin4)):VALUE =  tt-tit.nr-docto    
                chworksheet:range("F" + STRING(i-lin4)):VALUE =  tt-tit.parcela     
                chworksheet:range("G" + STRING(i-lin4)):VALUE =  tt-tit.vl-original 
                chworksheet:range("H" + STRING(i-lin4)):VALUE =  tt-tit.comissao.    
    
         ASSIGN i-lin4 = i-lin4 + 1.
    
         ACCUMULATE tt-tit.vl-original (TOTAL BY tt-tit.cod-rep).
         ACCUMULATE tt-tit.comissao    (TOTAL BY tt-tit.cod-rep).
    
         IF LAST-OF(tt-tit.cod-rep) THEN DO:
            ASSIGN chworksheet:range("G" + STRING(i-lin4)):VALUE = (ACCUM TOTAL BY tt-tit.cod-rep tt-tit.vl-original) 
                   chworksheet:range("H" + STRING(i-lin4)):VALUE = (ACCUM TOTAL BY tt-tit.cod-rep tt-tit.comissao).
    
            ASSIGN chWorkSheet:Range("A" + STRING(i-lin4) + ":H" + STRING(i-lin4)):Interior:ColorIndex = 31
                   chWorkSheet:Range("A" + STRING(i-lin4) + ":H" + STRING(i-lin4)):FONT:ColorIndex     =  2
                   chWorkSheet:Range("A" + STRING(i-lin4) + ":H" + STRING(i-lin4)):FONT:Bold           = FALSE.
            ASSIGN i-lin4= i-lin4 + 1. 
         END.
     END.
    
     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder5 B-table-Win 
PROCEDURE pi-folder5 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder6 B-table-Win 
PROCEDURE pi-folder6 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
     chWorkSheet = chExcelapp:Sheets:ITEM(6). /* Ativar a Planilha */
     chWorkbook:Worksheets(6):activate.
    
     /*
     FOR EACH tt-tit WHERE
              tt-tit.cod-rep = b-tt-repres.cod-rep AND
              tt-tit.tipo-mov = 'CHQ' NO-LOCK
        BREAK BY tt-tit.cod-rep
              BY tt-tit.nome-emit
              BY tt-tit.nr-docto.
    
         IF FIRST-OF(tt-tit.cod-rep) THEN
            ASSIGN chworksheet:range("A" + STRING(i-lin8)):VALUE = b-tt-repres.nome.                  
    
         ASSIGN chworksheet:range("B" + STRING(i-lin8)):VALUE =  tt-tit.cod-estabel            
                chworksheet:range("C" + STRING(i-lin8)):VALUE =  tt-tit.cod-emit  
                chworksheet:range("D" + STRING(i-lin8)):VALUE =  tt-tit.nome-emit 
                chworksheet:range("E" + STRING(i-lin8)):VALUE =  tt-tit.nr-docto    
                chworksheet:range("F" + STRING(i-lin8)):VALUE =  tt-tit.parcela     
                chworksheet:range("G" + STRING(i-lin8)):VALUE =  tt-tit.vl-original 
                chworksheet:range("H" + STRING(i-lin8)):VALUE =  tt-tit.comissao.    
    
         FIND tit_acr where
              tit_acr.cod_estab       = tt-tit.cod-estab AND
              tit_acr.cod_espec_docto = tt-tit.cod-esp   AND
              tit_acr.cod_ser_docto   = tt-tit.serie     AND
              tit_acr.cod_tit_acr     = tt-tit.nr-docto  AND
              tit_acr.cod_parcela     = tt-tit.parcela NO-LOCK NO-ERROR.
    
         ASSIGN de-chq = 0.
         FOR EACH relacto_cheq_acr OF tit_acr NO-LOCK.
             FOR EACH cheq_acr OF relacto_cheq_acr NO-LOCK.
                 ASSIGN chworksheet:range("I" + STRING(i-lin8)):VALUE =  cheq_acr.cod_banco            
                        chworksheet:range("J" + STRING(i-lin8)):VALUE =  cheq_acr.num_cheque  
                        chworksheet:range("K" + STRING(i-lin8)):VALUE =  STRING(cheq_acr.dat_apres_cheq_acr, "99/99/9999") 
                        chworksheet:range("L" + STRING(i-lin8)):VALUE =  cheq_acr.val_cheque.    
                 ASSIGN de-chq = de-chq + cheq_acr.val_cheque
                        i-lin8 = i-lin8 + 1.
             END.
         END.
         IF de-chq <> 0 THEN DO:
            ASSIGN i-lin8 = i-lin8 - 1.
            ASSIGN chworksheet:range("M" + STRING(i-lin8)):VALUE = de-chq.
            ASSIGN i-lin8 = i-lin8 + 1
                   de-tot-chq = de-tot-chq + de-chq.
         END.
    
         ACCUMULATE tt-tit.vl-original (TOTAL BY tt-tit.cod-rep).
         ACCUMULATE tt-tit.comissao    (TOTAL BY tt-tit.cod-rep).
    
         IF LAST-OF(tt-tit.cod-rep) THEN DO:
            ASSIGN chworksheet:range("G" + STRING(i-lin8)):VALUE = (ACCUM TOTAL BY tt-tit.cod-rep tt-tit.vl-original) 
                   chworksheet:range("H" + STRING(i-lin8)):VALUE = (ACCUM TOTAL BY tt-tit.cod-rep tt-tit.comissao)
                   chworksheet:range("M" + STRING(i-lin8)):VALUE = de-tot-chq.
    
            ASSIGN chWorkSheet:Range("A" + STRING(i-lin8) + ":M" + STRING(i-lin8)):Interior:ColorIndex = 31
                   chWorkSheet:Range("A" + STRING(i-lin8) + ":M" + STRING(i-lin8)):FONT:ColorIndex     =  2
                   chWorkSheet:Range("A" + STRING(i-lin8) + ":M" + STRING(i-lin8)):FONT:Bold           = FALSE.
            ASSIGN i-lin8= i-lin8 + 1. 
         END.
     END.
     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder7 B-table-Win 
PROCEDURE pi-folder7 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
     chWorkSheet = chExcelapp:Sheets:ITEM(7). /* Ativar a Planilha */
     chWorkbook:Worksheets(7):activate.
     FOR EACH tt-descontos WHERE
              tt-descontos.cod-rep = b-tt-repres.cod-rep AND
              tt-descontos.mes     = INT(MONTH(da-dt-periodo-ini)) AND
              tt-descontos.ano     = INT(YEAR(da-dt-periodo-ini)) NO-LOCK
        BREAK BY tt-descontos.cod-rep.
    
         IF FIRST-OF(tt-descontos.cod-rep) THEN
            ASSIGN chworksheet:range("A" + STRING(i-lin5)):VALUE = b-tt-repres.nome.                  
    
         ASSIGN chworksheet:range("B" + STRING(i-lin5)):VALUE = tt-descontos.mes                   
                chworksheet:range("C" + STRING(i-lin5)):VALUE = tt-descontos.ano           
                chworksheet:range("D" + STRING(i-lin5)):VALUE = tt-descontos.desc-desconto 
                chworksheet:range("E" + STRING(i-lin5)):VALUE = tt-descontos.vlr-desconto.  
    
         ASSIGN i-lin5 = i-lin5 + 1.
    
         ACCUMULATE tt-descontos.vlr-desconto (TOTAL BY tt-descontos.cod-rep).
    
         IF LAST-OF(tt-descontos.cod-rep) THEN DO:
            ASSIGN chworksheet:range("E" + STRING(i-lin5)):VALUE = (ACCUM TOTAL BY tt-descontos.cod-rep tt-descontos.vlr-desconto). 
    
            ASSIGN chWorkSheet:Range("A" + STRING(i-lin5) + ":E" + STRING(i-lin5)):Interior:ColorIndex = 31
                   chWorkSheet:Range("A" + STRING(i-lin5) + ":E" + STRING(i-lin5)):FONT:ColorIndex     =  2
                   chWorkSheet:Range("A" + STRING(i-lin5) + ":E" + STRING(i-lin5)):FONT:Bold           = FALSE.
            ASSIGN i-lin5= i-lin5 + 1. 
         END.
     END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder8 B-table-Win 
PROCEDURE pi-folder8 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
     chWorkSheet = chExcelapp:Sheets:ITEM(8). /* Ativar a Planilha */
     chWorkbook:Worksheets(8):activate.
     FOR EACH tt-emprestimo WHERE
              tt-emprestimo.cod-rep = b-tt-repres.cod-rep NO-LOCK
        BREAK BY tt-emprestimo.cod-rep.
    
         IF FIRST-OF(tt-emprestimo.cod-rep) THEN
            ASSIGN chworksheet:range("A" + STRING(i-lin6)):VALUE = b-tt-repres.nome.                  
    
         ASSIGN chworksheet:range("B" + STRING(i-lin6)):VALUE = STRING(LOOKUP(tt-emprestimo.mes-base, c-meses), "99")                   
                chworksheet:range("C" + STRING(i-lin6)):VALUE = tt-emprestimo.ano-base           
             /*   chworksheet:range("D" + STRING(i-lin6)):VALUE = tt-descontos.desc-desconto  */
                chworksheet:range("E" + STRING(i-lin6)):VALUE = tt-emprestimo.vlr-parcela.  
    
         ASSIGN i-lin6 = i-lin6 + 1.
    
         ACCUMULATE tt-emprestimo.vlr-parcela (TOTAL BY tt-emprestimo.cod-rep).
    
         IF LAST-OF(tt-emprestimo.cod-rep) THEN DO:
            ASSIGN chworksheet:range("E" + STRING(i-lin6)):VALUE = (ACCUM TOTAL BY tt-emprestimo.cod-rep tt-emprestimo.vlr-parcela). 
    
            ASSIGN chWorkSheet:Range("A" + STRING(i-lin6) + ":E" + STRING(i-lin5)):Interior:ColorIndex = 31
                   chWorkSheet:Range("A" + STRING(i-lin6) + ":E" + STRING(i-lin5)):FONT:ColorIndex     =  2
                   chWorkSheet:Range("A" + STRING(i-lin6) + ":E" + STRING(i-lin5)):FONT:Bold           = FALSE.
            ASSIGN i-lin6= i-lin6 + 1. 
         END.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-folder9 B-table-Win 
PROCEDURE pi-folder9 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 chWorkSheet = chExcelapp:Sheets:ITEM(9). /* Ativar a Planilha */
 chWorkbook:Worksheets(9):activate.
 /*
 FOR EACH tt-add-comis WHERE
          tt-add-comis.cod-rep = b-tt-repres.cod-rep AND 
          tt-add-comis.mes     = INT(MONTH(da-dt-periodo-ini)) NO-LOCK
    BREAK BY tt-add-comis.cod-rep.

     IF FIRST-OF(tt-add-comis.cod-rep) THEN
        ASSIGN chworksheet:range("A" + STRING(i-lin7)):VALUE = b-tt-repres.nome.                  

     ASSIGN chworksheet:range("B" + STRING(i-lin7)):VALUE = STRING(tt-add-comis.mes, "99")                   
            chworksheet:range("C" + STRING(i-lin7)):VALUE = STRING(tt-add-comis.ano, "9999")           
            chworksheet:range("D" + STRING(i-lin7)):VALUE = tt-add-comis.usuario
            chworksheet:range("E" + STRING(i-lin7)):VALUE = tt-add-comis.vlr-comis.

     RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(tt-add-comis.narrativa),CHR(13)," "),CHR(10)," "), INPUT 50). 
     FOR EACH tt-editor:
         ASSIGN chworksheet:range("F" + STRING(i-lin7)):VALUE = tt-editor.conteudo.  
         ASSIGN i-lin7 = i-lin7 + 1.
     END.

     ACCUMULATE tt-add-comis.vlr-comis (TOTAL BY tt-add-comis.cod-rep).

     IF LAST-OF(tt-add-comis.cod-rep) THEN DO:
        ASSIGN chworksheet:range("E" + STRING(i-lin7)):VALUE = (ACCUM TOTAL BY tt-add-comis.cod-rep tt-add-comis.vlr-comis). 

        ASSIGN chWorkSheet:Range("A" + STRING(i-lin7) + ":F" + STRING(i-lin7)):Interior:ColorIndex = 31
               chWorkSheet:Range("A" + STRING(i-lin7) + ":F" + STRING(i-lin7)):FONT:ColorIndex     =  2
               chWorkSheet:Range("A" + STRING(i-lin7) + ":F" + STRING(i-lin7)):FONT:Bold           = FALSE.
        ASSIGN i-lin7= i-lin7 + 1. 
     END.
 END.
 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel B-table-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pi-abre-excel (INPUT l-visivel).
  IF chExcelApp = ? THEN DO:
     MESSAGE "O Aplicativo EXCEL n∆o foi encontrado. N∆o Ç possivel a execuá∆o do programa."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN arq-saida = "".
     RETURN.
  END.

  RUN pi-monta-cabec.
  RUN pi-monta-planilha.
  
  /* Salva e Fecha Planilha */
  ASSIGN arq-saida = SUBSTR(arq-saida,1,LENGTH(arq-saida) - 3) + "xls".
  OS-DELETE VALUE(arq-saida).
  chWorkBook:SaveAs(arq-saida,39,,,,,,,TRUE). /* Salva em formato mais Antigo EXCEL97 */ 
  

  chWorkBook:CLOSE().
  chExcelApp:QUIT().
  RELEASE OBJECT chExcelApp. 
  RELEASE OBJECT chworkBook.
  RELEASE OBJECT chworksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-hierarquia B-table-Win 
PROCEDURE pi-hierarquia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    CASE cm-ext-repres.tp-aplic. 
        WHEN 1 THEN DO.  // Faturamento Total
           FOR EACH b-repres NO-LOCK.
               CREATE tt-calc-repres.
               ASSIGN tt-calc-repres.cod-rep  = b-repres.cod-rep
                      tt-calc-repres.cod-pai = repres.cod-rep.
           END.
        END.
        WHEN 3 THEN DO. // Hierarquia
           FOR EACH cm-hierarquia WHERE
                    cm-hierarquia.cod-rep = repres.cod-rep NO-LOCK.
               FIND b-repres WHERE
                    b-repres.cod-rep = cm-hierarquia.cod-depend
                    NO-LOCK NO-ERROR.
               IF AVAIL b-repres THEN DO.
                  CREATE tt-calc-repres.
                  ASSIGN tt-calc-repres.cod-rep  = b-repres.cod-rep
                         tt-calc-repres.cod-pai = repres.cod-rep.
               END.
           END.
           
           // verificar se existe representate por Estado
           FOR EACH b-repres NO-LOCK.
               FIND cm-unid-feder OF repres WHERE
                    cm-unid-feder.uf = b-repres.estado NO-LOCK NO-ERROR.
               IF AVAIL cm-unid-feder THEN DO.
                  CREATE tt-calc-repres.
                  ASSIGN tt-calc-repres.cod-rep  = b-repres.cod-rep
                         tt-calc-repres.cod-pai = repres.cod-rep.
               END.
           END.
        END.
        WHEN  4 THEN DO. // OutLet + Hierarquia
           FOR EACH b-repres NO-LOCK.
               CREATE tt-calc-repres.
               ASSIGN tt-calc-repres.cod-rep  = b-repres.cod-rep
                      tt-calc-repres.cod-pai = repres.cod-rep.
           END.

           FOR EACH cm-hierarquia WHERE
                    cm-hierarquia.cod-rep = repres.cod-rep NO-LOCK.
               FIND b-repres WHERE
                    b-repres.cod-rep = cm-hierarquia.cod-depend
                    NO-LOCK NO-ERROR.
               IF AVAIL b-repres THEN DO.
                  CREATE tt-calc-repres.
                  ASSIGN tt-calc-repres.cod-rep  = b-repres.cod-rep
                         tt-calc-repres.cod-pai = repres.cod-rep.
               END.
           END.
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec B-table-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa FORMAT "X(40)"                  AT   1
        "DATA: "                                  AT  58
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  64
        "HORA: "                                  AT  85
        STRING(TIME,"hh:mm:ss")                   AT  91
        "PAG:"                                    AT 125
        i-pag FORMAT ">>"                         AT 130
        SKIP(1).

    PUT "CALCULO DE COMISSOÂES " FORMAT "x(60)" AT 45 SKIP(1).

    ASSIGN i-pag = i-pag + 1.
    ASSIGN i-lin = 4.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime B-table-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEF VAR h-prog AS HANDLE NO-UNDO.
     DEF VAR i-ct   AS INT.
     DEF VAR de-tot-base12 LIKE tt-work.liquido.
    
     RUN utp/ut-utils.p PERSISTENT SET h-prog.
    
     RUN esapi/saida-imp.p (OUTPUT i-saida,
                            OUTPUT c-saida,
                            OUTPUT i-num-copias,
                            OUTPUT l-ok).
    
     CASE i-saida:
         WHEN 1 THEN DO.
             OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
             PUT CONTROL "~033E~033(s18H~033&l1o".    
         END.
         WHEN 2 THEN
             OUTPUT TO VALUE(c-saida).
         WHEN 3 THEN DO.
             ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "escm002.tmp".
             OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
         END.
     END CASE.
    
     DO i-ct = 1 TO i-num-copias.
        ASSIGN i-pag =  1
               i-lin = 99.

        FOR EACH b-tt-repres WHERE
                 b-tt-repres.visualiza NO-LOCK.
            
            IF i-lin > 44 THEN RUN pi-imp-cabec.

            PUT "Representante: "      AT 01
                b-tt-repres.cod-rep    "-"
                b-tt-repres.nome       
                SKIP.
            ASSIGN i-lin = i-lin + 1.
  
            PUT "Est Base  Faturamento   Comiss∆o  Devoluá∆o   Liquidez    Fat. Liq. Comis.Liq.   Desconto       I.R. Emprestimo    Adiant.    A RECEBER    A FATURAR " SKIP
                "--- ---- ------------ ---------- ---------- ---------- ------------ ---------- ---------- ---------- ---------- ---------- ------------ ------------ " SKIP.
            ASSIGN i-lin = i-lin + 2.

            ASSIGN de-tot-base12 = 0.
            FOR EACH b-tt-work WHERE
                     b-tt-work.cod-rep = b-tt-repres.cod-rep
                     NO-LOCK BY b-tt-work.cod-estab.

                IF b-tt-work.cod-estab = '98' OR
                   b-tt-work.cod-estabel = '99' THEN NEXT.

                PUT b-tt-work.cod-estabel   AT 01  FORMAT "x(3)"
                    b-tt-work.base          AT 05  FORMAT ">>>9" 
                    b-tt-work.vlr-fat       AT 10  FORMAT ">,>>>,>>9.99" 
                    b-tt-work.comissao      AT 23  FORMAT ">>>,>>9.99" 
                    b-tt-work.devolucao     AT 34  FORMAT ">>>,>>9.99" 
                    b-tt-work.liquidez      AT 45  FORMAT ">>>,>>9.99" 
                    b-tt-work.fat-liq       AT 56  FORMAT "->,>>>,>>9.99" 
                    b-tt-work.comis-liq     AT 69  FORMAT "->>>,>>9.99" 
                    b-tt-work.desconto      AT 80  FORMAT ">>>,>>9.99" 
                    b-tt-work.i-renda       AT 91  FORMAT ">>>,>>9.99" 
                    b-tt-work.emprestimo    AT 102 FORMAT ">>>,>>9.99" 
                    b-tt-work.adiantamento  AT 113 FORMAT ">>>,>>9.99" 
                    b-tt-work.liquido       AT 124 FORMAT "->,>>>,>>9.99" 
                    b-tt-work.vlr-nf        AT 137 FORMAT "->,>>>,>>9.99" 
                    SKIP.

                ASSIGN i-lin = i-lin + 1.

                IF b-tt-work.base = 12 THEN
                   ASSIGN de-tot-base12 = de-tot-base12 + b-tt-work.liquido.

                IF i-lin > 44 THEN DO.
                   RUN pi-imp-cabec.
                   PUT "Representante: "      AT 01
                       b-tt-repres.cod-rep    "-"
                       b-tt-repres.nome       
                       SKIP(1).
                   ASSIGN i-lin = i-lin + 2.
                   PUT "Est Base  Faturamento   Comiss∆o  Devoluá∆o   Liquidez    Fat. Liq. Comis.Liq.   Desconto       I.R. Emprestimo    Adiant.    A RECEBER    A FATURAR " SKIP
                       "--- ---- ------------ ---------- ---------- ---------- ------------ ---------- ---------- ---------- ---------- ---------- ------------ ------------ " SKIP.
                   ASSIGN i-lin = i-lin + 2.
                END.
            END.

            PUT " " SKIP.
            ASSIGN i-lin = i-lin + 1.

            IF de-tot-base12 > 0 THEN DO.
               IF i-lin > 44 THEN DO.
                  RUN pi-imp-cabec.

                  PUT "Representante: "      AT 01
                      b-tt-repres.cod-rep    "-"
                      b-tt-repres.nome       
                      SKIP(1).
                  ASSIGN i-lin = i-lin + 2.
               END.
  
               PUT "TOTAL BASE 12....:" AT 20
                   de-tot-base12 AT 40
                   SKIP.
               ASSIGN i-lin = i-lin + 1.
            END.

            PUT "" SKIP(1).
            ASSIGN i-lin = i-lin + 2.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-inserir-logo B-table-Win 
PROCEDURE pi-inserir-logo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* Configura a Linha do Titulo da Planilha */
 ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 40
        chWorkSheet:Rows("1:1"):FONT:SIZE = 24
        chWorkSheet:Rows("1:1"):FONT:bold = FALSE.

 /* Inserir Logotipo da Tear e Alinhar Via Tamanho e Altura Logotipo */
// ASSIGN FILE-INFO:FILE-NAME = SEARCH("image/logo-excel.bmp").
// ChWorkSheet:range("A1"):SELECT().
// ChWorkSheet:Pictures:INSERT(FILE-INFO:FULL-PATHNAME):SELECT. 
// chExcelApp:SELECTION:ShapeRange:ScaleWidth(1.9,FALSE,FALSE).
// chExcelApp:SELECTION:ShapeRange:ScaleHeight(0.61,FALSE,FALSE). 
// chExcelApp:SELECTION:ShapeRange:IncrementLeft(-49.5).
// chExcelApp:SELECTION:ShapeRange:IncrementTop( -48.75).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-integra-titulos B-table-Win 
PROCEDURE pi-integra-titulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tt-work NO-LOCK.

   END.
   MESSAGE "Titulos Gerados, Favor Verificar no Contas a Pagar"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-cabec B-table-Win 
PROCEDURE pi-monta-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------- PLANILHA 2 -------- */

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(2).
 chWorkbook:Worksheets(2):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("B1"):VALUE = "CALCULO DE COMISSÂES".

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("B1:M1"):SELECT().
 ChWorksheet:range("B1:M1"):Merge.
 Chworksheet:Range("B1:M1"):HorizontalAlignment = 3. /* Centralizado */
 Chworksheet:Range("B1:M1"):VerticalAlignment   = 2. /* Centralizado */

 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:M1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:M1"):Interior:ColorIndex = 2. /* Branco */

 //RUN pi-inserir-logo.
 ASSIGN chworksheet:range("A2"):VALUE = "REPRESENTANTE"
        chworksheet:range("B2"):VALUE = "BASE"
        chworksheet:range("C2"):VALUE = "FATURAMENTO"    
        chworksheet:range("D2"):VALUE = "DEVOLUÄ«O" 
        chworksheet:range("E2"):VALUE = "FAT.LIQUIDO" 
        chworksheet:range("F2"):VALUE = "COMIS.BRUTA"  
        chworksheet:range("G2"):VALUE = "COMIS.DEV"  
        chworksheet:range("H2"):VALUE = "COMIS.LIQ"   
        chworksheet:range("I2"):VALUE = "DESCONTOS"
        chworksheet:range("J2"):VALUE = "IR"
        chworksheet:range("K2"):VALUE = "EMPRESTIMOS"
        chworksheet:range("L2"):VALUE = "ADIANTAMENTO"
        chworksheet:range("M2"):VALUE = "A RECEBER". 

 ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
        chworksheet:range("B2"):ShrinkToFit = TRUE    
        chworksheet:range("C2"):ShrinkToFit = TRUE
        chworksheet:range("D2"):ShrinkToFit = TRUE
        chworksheet:range("E2"):ShrinkToFit = TRUE
        chworksheet:range("F2"):ShrinkToFit = TRUE
        chworksheet:range("G2"):ShrinkToFit = TRUE
        chworksheet:range("H2"):ShrinkToFit = TRUE
        chworksheet:range("I2"):ShrinkToFit = TRUE    
        chworksheet:range("J2"):ShrinkToFit = TRUE
        chworksheet:range("K2"):ShrinkToFit = TRUE
        chworksheet:range("L2"):ShrinkToFit = TRUE
        chworksheet:range("M2"):ShrinkToFit = TRUE.


 ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 37
        chWorkSheet:Columns("B"):ColumnWidth = 5
        chWorkSheet:Columns("C"):ColumnWidth = 16
        chWorkSheet:Columns("D"):ColumnWidth = 16
        chWorkSheet:Columns("E"):ColumnWidth = 16
        chWorkSheet:Columns("F"):ColumnWidth = 16
        chWorkSheet:Columns("G"):ColumnWidth = 16
        chWorkSheet:Columns("H"):ColumnWidth = 16
        chWorkSheet:Columns("I"):ColumnWidth = 16
        chWorkSheet:Columns("J"):ColumnWidth = 16
        chWorkSheet:Columns("K"):ColumnWidth = 16
        chWorkSheet:Columns("L"):ColumnWidth = 16
        chWorkSheet:Columns("M"):ColumnWidth = 16.

 ASSIGN chworksheet:range("A:B"):NumberFormat        = "@"
        chworksheet:range("C:M"):NumberFormat        = "###.###.##0,00"
        Chworksheet:range("C:M"):HorizontalAlignment = 4.

 chWorkSheet:Range("A2:M2"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 12
        chExcelApp:SELECTION:FONT:Bold               = FALSE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 37
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 RUN pi-config-planilha (INPUT 2, INPUT 40). 

 /*------- PLANILHA 3 -------- */

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(3).
 chWorkbook:Worksheets(3):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("B1"):VALUE = "NOTAS FISCAIS FATURADAS".

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("B1:L1"):SELECT().
 ChWorksheet:range("B1:L1"):Merge.
 Chworksheet:Range("B1:L1"):HorizontalAlignment = 3. /* Centralizado */
 Chworksheet:Range("B1:L1"):VerticalAlignment   = 2. /* Centralizado */

 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:L1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:L1"):Interior:ColorIndex = 2. /* Branco */

 RUN pi-inserir-logo.

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A2"):VALUE = "REPRESENTANTE"
        chworksheet:range("B2"):VALUE = "PREPOSTO"    
        chworksheet:range("C2"):VALUE = "BASE"
        chworksheet:range("D2"):VALUE = "CLIENTE"    
        chworksheet:range("E2"):VALUE = "UF"    
        chworksheet:range("F2"):VALUE = "NFs"
        chworksheet:range("G2"):VALUE = "VENCIMENTO" 
        chworksheet:range("H2"):VALUE = "VALOR "
        chworksheet:range("I2"):VALUE = "PEDIDO"
        chworksheet:range("J2"):VALUE = "TABELA"
        chworksheet:range("K2"):VALUE = "% COMIS"
        chworksheet:range("L2"):VALUE = "VL COMISS«O". 

 /* Ajustar o Tamanho Dentro da Celula */     
 ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
        chworksheet:range("B2"):ShrinkToFit = TRUE    
        chworksheet:range("C2"):ShrinkToFit = TRUE
        chworksheet:range("D2"):ShrinkToFit = TRUE
        chworksheet:range("E2"):ShrinkToFit = TRUE
        chworksheet:range("F2"):ShrinkToFit = TRUE
        chworksheet:range("G2"):ShrinkToFit = TRUE
        chworksheet:range("H2"):ShrinkToFit = TRUE
        chworksheet:range("I2"):ShrinkToFit = TRUE
        chworksheet:range("J2"):ShrinkToFit = TRUE
        chworksheet:range("K2"):ShrinkToFit = TRUE
        chworksheet:range("L2"):ShrinkToFit = TRUE.      
      

 /* Tamanho das Colunas */
 ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 40
        chWorkSheet:Columns("B"):ColumnWidth = 20
        chWorkSheet:Columns("C"):ColumnWidth = 5
        chWorkSheet:Columns("D"):ColumnWidth = 45
        chWorkSheet:Columns("E"):ColumnWidth = 5
        chWorkSheet:Columns("F"):ColumnWidth = 10
        chWorkSheet:Columns("G"):ColumnWidth = 12
        chWorkSheet:Columns("H"):ColumnWidth = 20
        chWorkSheet:Columns("I"):ColumnWidth = 12
        chWorkSheet:Columns("J"):ColumnWidth = 12
        chWorkSheet:Columns("K"):ColumnWidth = 8.
        chWorkSheet:Columns("L"):ColumnWidth = 15.


 /* Configura as Colunas da Planilha */
 ASSIGN chworksheet:range("A:G"):NumberFormat       = "@"
        chworksheet:Columns("H"):NumberFormat       = "###.###.##0,00"
        chworksheet:range("I:J"):NumberFormat       = "@"
        chworksheet:range("K:L"):NumberFormat       = "###.###.##0,00".
 ASSIGN Chworksheet:Columns("H"):HorizontalAlignment = 4
        Chworksheet:range("K:L"):HorizontalAlignment = 4.

 /* Configura Cabeáalho das Colunas */
 chWorkSheet:Range("A2:L2"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 12
        chExcelApp:SELECTION:FONT:Bold               = FALSE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 37
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 RUN pi-config-planilha (INPUT 1, INPUT 65). 

 /*-------- PLANILHA 6 -------- */
 
 
 /*-------- PLANILHA 7 -------- */

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(7).
 chWorkbook:Worksheets(7):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("B1"):VALUE = "DESCONTOS".

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("B1:E1"):SELECT().
 ChWorksheet:range("B1:E1"):Merge.
 Chworksheet:Range("B1:E1"):HorizontalAlignment = 3. /* Centralizado */
 Chworksheet:Range("B1:E1"):VerticalAlignment   = 2. /* Centralizado */

 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:E1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:E1"):Interior:ColorIndex = 2. /* Branco */

 RUN pi-inserir-logo.

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A2"):VALUE = "REPRESENTANTE"
        chworksheet:range("B2"):VALUE = "MES"
        chworksheet:range("C2"):VALUE = "ANO"    
        chworksheet:range("D2"):VALUE = "DESCONTOS"  
        chworksheet:range("E2"):VALUE = "VALOR".

 /* Ajustar o Tamanho Dentro da Celula */     
 ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
        chworksheet:range("B2"):ShrinkToFit = TRUE    
        chworksheet:range("C2"):ShrinkToFit = TRUE
        chworksheet:range("D2"):ShrinkToFit = TRUE
        chworksheet:range("E2"):ShrinkToFit = TRUE.

 /* Tamanho das Colunas */
 ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 39
        chWorkSheet:Columns("B"):ColumnWidth =  4
        chWorkSheet:Columns("C"):ColumnWidth =  4
        chWorkSheet:Columns("D"):ColumnWidth = 50
        chWorkSheet:Columns("E"):ColumnWidth = 12.

 /* Configura as Colunas da Planilha */
 ASSIGN chworksheet:range("A:D"):NumberFormat        = "@"
        chworksheet:range("E:E"):NumberFormat        = "-##.###.##0,00"
        Chworksheet:range("E:E"):HorizontalAlignment = 4.

 /* Configura Cabeáalho das Colunas */
 chWorkSheet:Range("A2:E2"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 12
        chExcelApp:SELECTION:FONT:Bold               = FALSE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 37
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 RUN pi-config-planilha (INPUT 1, INPUT 80). 


 /*------- PLANILHA 8 ------- */

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(8).
 chWorkbook:Worksheets(8):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("B1"):VALUE = "EMPRESTIMOS".

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("B1:E1"):SELECT().
 ChWorksheet:range("B1:E1"):Merge.
 Chworksheet:Range("B1:E1"):HorizontalAlignment = 3. /* Centralizado */
 Chworksheet:Range("B1:E1"):VerticalAlignment   = 2. /* Centralizado */

 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:E1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:E1"):Interior:ColorIndex = 2. /* Branco */

 RUN pi-inserir-logo.

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A2"):VALUE = "REPRESENTANTE"
        chworksheet:range("B2"):VALUE = "MES"
        chworksheet:range("C2"):VALUE = "ANO"    
        chworksheet:range("D2"):VALUE = "EMPRESTIMOS"  
        chworksheet:range("E2"):VALUE = "VALOR".

 /* Ajustar o Tamanho Dentro da Celula */     
 ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
        chworksheet:range("B2"):ShrinkToFit = TRUE    
        chworksheet:range("C2"):ShrinkToFit = TRUE
        chworksheet:range("D2"):ShrinkToFit = TRUE
        chworksheet:range("E2"):ShrinkToFit = TRUE.

 /* Tamanho das Colunas */
 ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 39
        chWorkSheet:Columns("B"):ColumnWidth =  4
        chWorkSheet:Columns("C"):ColumnWidth =  4
        chWorkSheet:Columns("D"):ColumnWidth = 21
        chWorkSheet:Columns("E"):ColumnWidth = 12.

 /* Configura as Colunas da Planilha */
 ASSIGN chworksheet:range("A:D"):NumberFormat        = "@"
        chworksheet:range("E:E"):NumberFormat        = "-##.###.##0,00"
        Chworksheet:range("E:E"):HorizontalAlignment = 4.

 /* Configura Cabeáalho das Colunas */
 chWorkSheet:Range("A2:E2"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 12
        chExcelApp:SELECTION:FONT:Bold               = FALSE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 37
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 RUN pi-config-planilha (INPUT 1, INPUT 100). 


 /*------- PLANILHA 9 ------- */

 /* Ativar a Planilha */
 chWorkSheet = chExcelapp:Sheets:ITEM(9).
 chWorkbook:Worksheets(9):activate.
 chExcelApp:ActiveWindow:Zoom = 100.

 ASSIGN chworksheet:range("B1"):VALUE = "COMISS«O INFORMADA".

 /* Configura Alinhamento Horizontal do Titulo da Planilha */
 ChWorkSheet:range("B1:F1"):SELECT().
 ChWorksheet:range("B1:F1"):Merge.
 Chworksheet:Range("B1:F1"):HorizontalAlignment = 3. /* Centralizado */
 Chworksheet:Range("B1:F1"):VerticalAlignment   = 2. /* Centralizado */

 /* Colorir Titulo da Planilha */
 chWorkSheet:Range("A1:F1"):FONT:ColorIndex     = 18. /* Avermelhado */
 chWorkSheet:Range("A1:F1"):Interior:ColorIndex = 2. /* Branco */

 RUN pi-inserir-logo.

 /* Titulo das Colunas */
 ASSIGN chworksheet:range("A2"):VALUE = "REPRESENTANTE"
        chworksheet:range("B2"):VALUE = "MES"
        chworksheet:range("C2"):VALUE = "ANO"    
        chworksheet:range("D2"):VALUE = "USUARIO"  
        chworksheet:range("E2"):VALUE = "VALOR"  
        chworksheet:range("F2"):VALUE = "NARRATIVA".

 /* Ajustar o Tamanho Dentro da Celula */     
 ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
        chworksheet:range("B2"):ShrinkToFit = TRUE    
        chworksheet:range("C2"):ShrinkToFit = TRUE
        chworksheet:range("D2"):ShrinkToFit = TRUE
        chworksheet:range("E2"):ShrinkToFit = TRUE
        chworksheet:range("F2"):ShrinkToFit = TRUE.

 /* Tamanho das Colunas */
 ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 39
        chWorkSheet:Columns("B"):ColumnWidth =  4
        chWorkSheet:Columns("C"):ColumnWidth =  4
        chWorkSheet:Columns("D"):ColumnWidth = 12
        chWorkSheet:Columns("E"):ColumnWidth = 12
        chWorkSheet:Columns("F"):ColumnWidth = 50.

 /* Configura as Colunas da Planilha */
 ASSIGN chworksheet:range("A:D"):NumberFormat        = "@"
        chworksheet:range("E:E"):NumberFormat        = "###.###.##0,00"
        Chworksheet:range("E:E"):HorizontalAlignment = 4
        chworksheet:range("F:F"):NumberFormat        = "@".

 /* Configura Cabeáalho das Colunas */
 chWorkSheet:Range("A2:F2"):SELECT().
 ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
        chExcelApp:SELECTION:FONT:SIZE               = 12
        chExcelApp:SELECTION:FONT:Bold               = FALSE 
        chExcelApp:SELECTION:Interior:ColorIndex     = 37
        chExcelApp:SELECTION:FONT:ColorIndex         = 11.

 RUN pi-config-planilha (INPUT 1, INPUT 65). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-planilha B-table-Win 
PROCEDURE pi-monta-planilha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     ASSIGN i-Lin1 = 3 i-lin2 = 3 i-lin3 = 3 i-lin4 = 3 i-lin5 = 3 i-lin6 = 3 i-lin7 = 3 i-lin8 = 3.
    
     RUN pi-folder1.

     FOR EACH b-tt-repres WHERE
              b-tt-repres.marca = "*" AND 
              b-tt-repres.visualiza NO-LOCK 
           BY b-tt-repres.cod-rep.
    
         RUN pi-folder2.
         RUN pi-folder3.
         RUN pi-folder4.
         RUN pi-folder5.
         RUN pi-folder6.
         RUN pi-folder7.
         RUN pi-folder8.
         RUN pi-folder9.
     END.
    
     /* Marcar Area de Impressao */
     chWorkSheet = chExcelapp:Sheets:ITEM(2). 
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$N$" + STRING(i-lin1).
     chWorkSheet = chExcelapp:Sheets:ITEM(3). 
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$I$" + STRING(i-lin2).  
     chWorkSheet = chExcelapp:Sheets:ITEM(4). 
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$H$" + STRING(i-lin3).  
     chWorkSheet = chExcelapp:Sheets:ITEM(5). 
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$H$" + STRING(i-lin4).  
     chWorkSheet = chExcelapp:Sheets:ITEM(6). 
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$M$" + STRING(i-lin8).  
     chWorkSheet = chExcelapp:Sheets:ITEM(7). 
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$E$" + STRING(i-lin5).  
     chWorkSheet = chExcelapp:Sheets:ITEM(8). 
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$E$" + STRING(i-lin6).  
     chWorkSheet = chExcelapp:Sheets:ITEM(9). 
     ASSIGN chWorkSheet:PageSetup:PrintArea = "$A$1:$F$" + STRING(i-lin7).  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-tt-calc B-table-Win 
PROCEDURE pi-monta-tt-calc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-classe         AS CHAR INIT "Gerente Geral,Gerente Loja,Pracista,Interno,Externo".

    /* Filtra Representantes Selecinados */ 
    FOR EACH repres WHERE
             repres.nome-abrev >= c-no-ab-reppri-ini AND
             repres.nome-abrev <= c-no-ab-reppri-fin NO-LOCK.
    
        IF repres.comis-direta = 0 THEN NEXT.

        RUN pi-ver-digita (INPUT "Representante",
                           INPUT repres.nome-abrev).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND cm-ext-repres WHERE
             cm-ext-repres.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.
    
        IF NOT AVAIL cm-ext-repres THEN NEXT.
        IF cm-ext-repres.bloqueado THEN NEXT.

        IF c-lst-classe <> "" AND
           LOOKUP(STRING(cm-ext-repres.classe,"9"),c-lst-classe) = 0 THEN NEXT.
        
        FIND tt-repres WHERE
             tt-repres.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.
        IF NOT AVAIL tt-repres THEN DO:
           CREATE tt-repres.
           BUFFER-COPY repres TO tt-repres.
    
           ASSIGN tt-repres.classe = ENTRY(cm-ext-repres.classe,c-classe).
        END.
    
        CREATE tt-calc-repres.
        ASSIGN tt-calc-repres.cod-rep  = repres.cod-rep
               tt-calc-repres.cod-pai = repres.cod-rep.

        RUN pi-hierarquia.
    END.
    
    /* Busca Digitaá∆o */
    FOR EACH tt-digita WHERE
             tt-digita.campo = 'Representante' NO-LOCK.
        IF tt-digita.opcao = 'D' THEN DO.
           FIND repres WHERE
                repres.nome-abrev = tt-digita.valor NO-LOCK NO-ERROR.
    
           IF NOT AVAIL repres THEN NEXT.
           IF repres.comis-direta = 0 THEN NEXT.
    
           FIND cm-ext-repres WHERE
                cm-ext-repres.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.
    
           IF NOT AVAIL cm-ext-repres THEN NEXT.
    
           IF cm-ext-repres.bloqueado THEN NEXT.
    
           FIND tt-repres WHERE
                tt-repres.cod-rep = repres.cod-rep NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-repres THEN DO:
              CREATE tt-repres.
              BUFFER-COPY repres TO tt-repres.
    
              ASSIGN tt-repres.classe = ENTRY(cm-ext-repres.classe,c-classe).
           END.
    
           CREATE tt-calc-repres.
           ASSIGN tt-calc-repres.cod-rep  = repres.cod-rep
                  tt-calc-repres.cod-pai = repres.cod-rep.
                     
           RUN pi-hierarquia.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-notas-fiscais B-table-Win 
PROCEDURE pi-notas-fiscais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN esapi/connect-ima-med.p.
    RUN esapi/escm002a.p (INPUT-OUTPUT TABLE tt-work,
                          INPUT-OUTPUT TABLE tt-calc-repres,
                          INPUT-OUTPUT TABLE tt-nfs,
                          INPUT-OUTPUT TABLE tt-digita,
                          INPUT c-cod-estab-ini,
                          INPUT c-cod-estab-fin,
                          INPUT da-dt-periodo-ini,
                          INPUT da-dt-periodo-fin,
                          INPUT c-nr-nota-fis-ini,
                          INPUT c-nr-nota-fis-fin,
                          INPUT c-no-ab-reppri-ini, 
                          INPUT c-no-ab-reppri-fin, 
                          INPUT i-cond-pagto-ini,
                          INPUT i-cond-pagto-fin,
                          INPUT c-it-codigo-ini,
                          INPUT c-it-codigo-fin).
    
    DISCONNECT dbaux.

    FOR EACH tt-work.
        ASSIGN fi-fat-geral = fi-fat-geral + tt-work.vlr-fat.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa B-table-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER TABLE FOR tt-digita. 
  DEFINE INPUT PARAMETER p-cod-estab-ini    AS CHAR.
  DEFINE INPUT PARAMETER p-cod-estab-fin    AS CHAR.
  DEFINE INPUT PARAMETER p-dt-periodo-ini   AS DATE.
  DEFINE INPUT PARAMETER p-dt-periodo-fin   AS DATE.
  DEFINE INPUT PARAMETER p-no-ab-reppri-ini AS CHAR.                              
  DEFINE INPUT PARAMETER p-no-ab-reppri-fin AS CHAR.
  DEFINE INPUT PARAMETER p-nr-nota-fis-ini  AS CHAR.
  DEFINE INPUT PARAMETER p-nr-nota-fis-fin  AS CHAR.
  DEFINE INPUT PARAMETER p-it-codigo-ini    AS CHAR.
  DEFINE INPUT PARAMETER p-it-codigo-fin    AS CHAR.
  DEFINE INPUT PARAMETER p-cond-pagto-ini   AS INT.
  DEFINE INPUT PARAMETER p-cond-pagto-fin   AS INT.
  DEFINE INPUT PARAMETER p-classe           AS CHAR.

  ASSIGN c-cod-estab-ini    = p-cod-estab-ini                             
         c-cod-estab-fin    = p-cod-estab-fin
         da-dt-periodo-ini  = p-dt-periodo-ini  
         da-dt-periodo-fin  = p-dt-periodo-fin  
         c-no-ab-reppri-ini = p-no-ab-reppri-ini                             
         c-no-ab-reppri-fin = p-no-ab-reppri-fin
         c-lst-classe       = p-classe
         c-nr-nota-fis-ini  = p-nr-nota-fis-ini                              
         c-nr-nota-fis-fin  = p-nr-nota-fis-fin 
         c-it-codigo-ini    = p-it-codigo-ini                                
         c-it-codigo-fin    = p-it-codigo-fin                                
         i-cond-pagto-ini   = p-cond-pagto-ini                              
         i-cond-pagto-fin   = p-cond-pagto-fin.

  ASSIGN fi-dt-periodo-ini = da-dt-periodo-ini
         fi-dt-periodo-fin = da-dt-periodo-fin.

  ASSIGN fi-fat-geral    = 0    fi-fat-repres   = 0
         fi-comis-ger    = 0    fi-comis-repres = 0.

  FOR EACH tt-work.
      DELETE tt-work.
  END.

  FOR EACH tt-repres.
      DELETE tt-repres.
  END.
  
  FOR EACH tt-calc-repres.
      DELETE tt-calc-repres.
  END.

  FOR EACH tt-nfs.
      DELETE tt-nfs.
  END.

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa  WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

  {utp/ut-liter.i Gerando_Relat¢rio *}
  RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

  RUN pi-monta-tt-calc.

  RUN pi-notas-fiscais.
  RUN pi-devolucao.

  FOR EACH tt-work.
      FIND tt-repres WHERE 
           tt-repres.cod-rep = tt-work.cod-rep NO-ERROR.

      FIND repres WHERE
           repres.cod-rep = tt-repres.cod-rep NO-LOCK NO-ERROR.

      ASSIGN tt-work.fat-liq = tt-work.vlr-fat - tt-work.devolucao - tt-work.liquidez.

      IF tt-work.fat-liq > 0 THEN 
         ASSIGN tt-work.comis-liq = tt-work.comissao - tt-work.comis-dev - 
                                   (tt-work.liquidez * repres.comis-direta / 100).
      /*
      IF tt-work.devolucao > 0 THEN 
         ASSIGN tt-work.comis-liq = tt-work.comissao - tt-worii.
                                   (tt-work.devolucao * repres.comis-direta / 100).
      */                             
  END.

  RUN pi-descontos.
  RUN pi-emprestimos.            

  FOR EACH tt-work.
      FIND tt-repres where
           tt-repres.cod-rep = tt-work.cod-rep NO-ERROR.

      FIND cm-ext-repres WHERE
           cm-ext-repres.cod-rep = tt-repres.cod-rep NO-LOCK NO-ERROR.

      IF tt-work.vlr-fat = 0     AND tt-work.devolucao = 0 AND
         tt-work.desconto = 0    AND tt-work.comis-liq = 0 AND
         tt-work.emprestimo = 0  AND tt-work.desconto = 0 THEN DO.
         DELETE tt-work.
         NEXT.
      END.

      IF AVAIL cm-ext-repres AND tt-work.base = 10 THEN
         ASSIGN tt-work.i-renda = (tt-work.comis-liq - tt-work.desc-base-ir) * cm-ext-repres.perc-ir / 100.

      FIND im-param WHERE
           im-param.cod-param = "MIN_IR_COMIS" NO-LOCK NO-ERROR.
      IF AVAIL im-param THEN DO.
         IF tt-work.i-renda < DEC(im-param.val-param) THEN
            ASSIGN tt-work.i-renda = 0.
      END.

      ASSIGN fi-comis-ger = fi-comis-ger + tt-work.comis-liq.

      IF tt-work.base = 10 THEN   /* S¢ Fatura Base 10 */
         ASSIGN tt-work.vlr-nf = tt-work.comis-liq - tt-work.desconto - tt-work.i-renda + tt-work.adiantamento .

      ASSIGN tt-work.liquido = tt-work.comis-liq - tt-work.desconto - tt-work.i-renda - tt-work.emprestimo.
  END.
  
  FOR EACH tt-repres.
      FIND FIRST tt-work WHERE
                 tt-work.cod-rep = tt-repres.cod-rep NO-ERROR.
      ASSIGN tt-repres.visualiza = AVAIL tt-work.
  END.

  /* Totaliza */
  FOR EACH tt-work WHERE
           tt-work.cod-estabel <> '99' 
           BREAK BY tt-work.cod-rep.

      ACCUMULATE tt-work.vlr-fat    (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.devolucao  (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.liquidez   (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.fat-liq    (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.comissao   (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.comis-liq  (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.desconto   (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.i-renda    (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.emprestimo (TOTAL BY tt-work.cod-rep).
      ACCUMULATE tt-work.liquido    (TOTAL BY tt-work.cod-rep).

      IF LAST-OF(tt-work.cod-rep) THEN DO.
         CREATE b-tt-work.
         ASSIGN b-tt-work.cod-estabel = '98'
                b-tt-work.cod-rep = tt-work.cod-rep.

         CREATE b-tt-work.
         ASSIGN b-tt-work.cod-estabel = '99'
                b-tt-work.cod-rep = tt-work.cod-rep
                b-tt-work.vlr-fat = (ACCUM TOTAL BY tt-work.cod-rep tt-work.vlr-fat)    
                b-tt-work.devolucao = (ACCUM TOTAL BY tt-work.cod-rep tt-work.devolucao)  
                b-tt-work.liquidez = (ACCUM TOTAL BY tt-work.cod-rep tt-work.liquidez)   
                b-tt-work.fat-liq = (ACCUM TOTAL BY tt-work.cod-rep tt-work.fat-liq)    
                b-tt-work.comissao = (ACCUM TOTAL BY tt-work.cod-rep tt-work.comissao)   
                b-tt-work.comis-liq = (ACCUM TOTAL BY tt-work.cod-rep tt-work.comis-liq)  
                b-tt-work.desconto = (ACCUM TOTAL BY tt-work.cod-rep tt-work.desconto)   
                b-tt-work.i-renda = (ACCUM TOTAL BY tt-work.cod-rep tt-work.i-renda)    
                b-tt-work.emprestimo = (ACCUM TOTAL BY tt-work.cod-rep tt-work.emprestimo) 
                b-tt-work.liquido = (ACCUM TOTAL BY tt-work.cod-rep tt-work.liquido).
      END.
  END.

  RUN pi-finalizar in h-acomp.
  RUN adm-open-query-cases.
 
  APPLY 'value-changed' TO br-repres IN FRAME {&FRAME-NAME}.
  APPLY 'entry' TO br-repres IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais B-table-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN fi-fat-repres = 0
         fi-comis-repres = 0.
  FOR EACH tt-work WHERE
           tt-work.cod-rep = tt-repres.cod-rep.
      IF tt-work.cod-estabel BEGINS "9" THEN NEXT.
      ASSIGN fi-fat-repres = fi-fat-repres + tt-work.vlr-fat
             fi-comis-repres = fi-comis-repres + tt-work.liquido.
  END.

  DISPLAY fi-dt-periodo-ini
          fi-dt-periodo-fin
          fi-fat-geral
          fi-fat-repres
          fi-comis-ger
          fi-comis-repres
          WITH FRAME {&FRAME-NAME}. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita B-table-Win 
PROCEDURE pi-ver-digita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER p-campo AS CHAR.
 DEF INPUT PARAMETER p-valor AS CHAR.

 IF CAN-FIND(FIRST tt-digita WHERE
                   tt-digita.opcao = 'D'      AND
                   tt-digita.campo = p-campo) AND
    NOT CAN-FIND(FIRST tt-digita WHERE
                       tt-digita.opcao = 'D'      AND
                       tt-digita.campo = p-campo  AND
                       tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
 ELSE
   IF CAN-FIND(FIRST tt-digita WHERE
                     tt-digita.opcao = 'E' AND
                     tt-digita.campo = p-campo AND
                     tt-digita.valor = p-valor) THEN RETURN 'ADM-ERROR'.
   ELSE
      RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "nr-ord-produ" "ped-item" "nr-ord-produ"}
  {src/adm/template/sndkycas.i "nr-programa" "ped-item" "nr-programa"}
  {src/adm/template/sndkycas.i "it-codigo" "ped-item" "it-codigo"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-work"}
  {src/adm/template/snd-list.i "tt-repres"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

