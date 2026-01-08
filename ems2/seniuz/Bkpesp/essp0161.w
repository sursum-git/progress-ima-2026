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
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF BUFFER empresa FOR mgcad.empresa.

/* INICIO DA DEFINIÄ«O DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */
/*                                                                                       */
{esinc/espd0002.i}

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEFINE TEMP-TABLE tt-digita NO-UNDO 
       FIELD nr-sequencia     AS   INT FORMAT '>>9'
       FIELD it-codigo        LIKE ped-item.it-codigo
       FIELD cod-refer        LIKE ped-item.cod-refer
       FIELD desc-item        AS   CHAR FORMAT "x(25)"
       FIELD qt-pedida        AS   DEC FORMAT ">>>,>>9.99" 
       FIELD qt-reserva       AS   DEC FORMAT ">>>,>>9.99" 
       FIELD sit-prog         AS   CHAR FORMAT "x(7)"
       INDEX seqped nr-sequencia.


DEFINE VAR raw-param   AS RAW NO-UNDO.
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.

/* FIM DAS DEFINIÄÂES DO RELATORIO ESPD0002RP.P */

/* Local Variable Definitions ---                                       */

/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR v_rec_cliente AS RECID FORMAT ">>>>>>9":U NO-UNDO.

DEF NEW GLOBAL SHARED VAR gr-emitente AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-nota-fiscal AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD tipo-pedido       LIKE ped-venda-ext.tp-pedido
    FIELD tp-frete          LIKE ped-venda-ext.tp-frete
    FIELD qt-pedida         LIKE ped-item.qt-pedida
    FIELD qt-reservada      LIKE ped-item.qt-pedida
    FIELD qt-romaneio       LIKE ped-item.qt-pedida
    FIELD qt-aberto         AS   DEC FORMAT "->,>>>,>>9.99"
    FIELD vl-pedido         LIKE ped-venda.vl-tot-ped
    FIELD vl-reservado      LIKE ped-venda.vl-tot-ped
    FIELD vl-aberto         LIKE ped-venda.vl-tot-ped
    FIELD des-cond-pagto    AS   CHAR FORMAT "x(15)"
    FIELD credito           AS   CHAR FORMAT "x(15)" 
    FIELD bloqueio          LIKE ped-venda-ext.l-bloqueio
    FIELD num-parcelas      AS   INT
    FIELD qt-volumes        LIKE ped-venda-ext.qt-fardos
    FIELD visualiza         AS   LOG INIT YES
    FIELD restricao         AS   INT INIT 0
    FIELD faturado          AS   LOG INIT NO
    FIELD nr-nota-fis       LIKE nota-fiscal.nr-nota-fis
    FIELD serie             LIKE nota-fiscal.serie
    FIELD vl-tot-nota       LIKE nota-fiscal.vl-tot-nota
    FIELD danfe-autorizado  AS   LOG
    INDEX indice1 IS PRIMARY dt-entrega.

DEFINE TEMP-TABLE tt-log-ped-venda LIKE his-ped-venda-ext.

DEFINE TEMP-TABLE wt-notas-geradas
       FIELD rw-nota-fiscal AS ROWID.

DEF BUFFER b-nota-fiscal FOR nota-fiscal.

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp           AS HANDLE NO-UNDO.

DEF VAR h-query           AS HANDLE.
DEF VAR c-busca           AS CHAR.
DEF VAR c-empresa         AS CHAR.

DEF VAR c-mensagem        AS CHAR.
DEF VAR de-peso-emb       AS DEC.
DEF VAR l-danfe-autoriz   AS LOG.      
DEF VAR l-ped-faturado    AS LOG.
DEF VAR l-notas-pend      AS LOG.
DEF VAR c-situacao        AS CHAR.
DEF VAR c-dia             AS CHAR.
DEF VAR da-dt-entrega     AS DATE.
DEF VAR c-lotes           AS CHAR FORMAT "x(18)".
DEF VAR c-nr-pedcli       LIKE ped-venda.nr-pedcli.

/* Faturamento Autom†tico */
DEF VAR i-row AS INT.

/* Def temp-table de erros. Ela tbÇm est† definida na include dbotterr.i */
def temp-table rowerrors no-undo
    field errorsequence    as int
    field errornumber      as int
    field errordescription as char
    field errorparameters  as char
    field errortype        as char
    field errorhelp        as char
    field errorsubtype     as char.

/* Definicao da tabela temporaria tt-notas-geradas, include {dibo/bodi317ef.i1} */
def temp-table tt-notas-geradas no-undo
    field rw-nota-fiscal as   rowid
    field nr-nota        like nota-fiscal.nr-nota-fis
    field seq-wt-docto   like wt-docto.seq-wt-docto.

/* Definiá∆o de um buffer para tt-notas-geradas */
def buffer b-tt-notas-geradas for tt-notas-geradas.

/* Restriá‰es dos Pedidos */
DEF VAR l-entrega-unica AS LOG.
DEF VAR l-data-marcada  AS LOG.
DEF VAR l-valor-max-nf  AS LOG.
DEF VAR l-valor-min-nf  AS LOG.
DEF VAR l-valor-min-dup AS LOG.
DEF VAR l-fatura        AS LOG.

/* Variavies de ParÉmetros */
DEFINE VAR c-cod-estabel       AS CHAR.
DEFINE VAR c-dt-limite         AS CHAR.
DEFINE VAR c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli INIT "".
DEFINE VAR c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli INIT "ZZZZZZZZ".  
DEFINE VAR c-it-codigo-ini     LIKE ped-item-ext.it-codigo INIT "".                 
DEFINE VAR c-it-codigo-fin     LIKE ped-item-ext.it-codigo INIT "ZZZZZZZZZZZZZZZ".  
DEFINE VAR c-cod-refer-ini     LIKE ped-item-ext.cod-refer.
DEFINE VAR c-cod-refer-fin     LIKE ped-item-ext.cod-refer INIT "ZZZZZZZZZZ".   
DEFINE VAR c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE VAR c-nome-abrev-fin    LIKE ped-venda.nome-abrev INIT "ZZZZZZZZZZZZ".
DEFINE VAR c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE VAR c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri INIT 'ZZZZZZZZZZZZ'.
DEFINE VAR c-opc-artigo        AS CHAR INIT 'A'.
DEFINE VAR l-lote-todos        AS LOG INIT YES.
DEFINE VAR l-lote-pp           AS LOG INIT NO.
DEFINE VAR l-lote-pd           AS LOG INIT NO.
DEFINE VAR l-lote-rp           AS LOG INIT NO.
DEFINE VAR l-lote-rd           AS LOG INIT NO.
DEFINE VAR l-lote-sc           AS LOG INIT NO.
DEFINE VAR l-lote-ca           AS LOG INIT NO.
DEFINE VAR l-ok                AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-pedidos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ped-venda

/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.nr-nota-fis tt-ped-venda.nr-pedcli tt-ped-venda.nome-abrev tt-ped-venda.dt-entrega tt-ped-venda.nome-transp tt-ped-venda.estado tt-ped-venda.cidade tt-ped-venda.tp-frete tt-ped-venda.qt-pedida tt-ped-venda.qt-reservada tt-ped-venda.qt-aberto tt-ped-venda.tipo-pedido fn-restricao() tt-ped-venda.vl-reservado tt-ped-venda.vl-desconto tt-ped-venda.vl-tot-ped (tt-ped-venda.vl-reservado / tt-ped-venda.num-parcelas) tt-ped-venda.qt-volumes tt-ped-venda.ind-fat-par tt-ped-venda.bloqueio tt-ped-venda.dt-entrega tt-ped-venda.credito tt-ped-venda.des-cond-pagto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos tt-ped-venda.qt-volumes   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define SELF-NAME br-pedidos
&Scoped-define QUERY-STRING-br-pedidos FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.faturado = l-ped-faturado AND                                  tt-ped-venda.visualiza = YES NO-LOCK                                  BY tt-ped-venda.nr-nota-fis BY tt-ped-venda.nome-transp BY tt-ped-venda.nome-abrev BY tt-ped-venda.nr-pedcli
&Scoped-define OPEN-QUERY-br-pedidos OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.faturado = l-ped-faturado AND                                  tt-ped-venda.visualiza = YES NO-LOCK                                  BY tt-ped-venda.nr-nota-fis BY tt-ped-venda.nome-transp BY tt-ped-venda.nome-abrev BY tt-ped-venda.nr-pedcli.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-venda


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 rt-button bt-param bt-imprime ~
tg-imp-danfe tg-faturaveis tg-restricao br-pedidos bt-vapara bt-todos ~
bt-nenhum bt-credito bt-sintegra bt-modifica bt-consulta bt-pre-nota bt-log ~
bt-fat-man bt-fat-aut ed-obs bt-monitor bt-imp-danfe fi-cgc 
&Scoped-Define DISPLAYED-OBJECTS tg-imp-danfe tg-faturaveis tg-restricao ~
ed-obs fi-tot-qtd-ped fi-tot-qtd-res fi-tot-qtd-abe fi-tot-vlr-ped ~
fi-tot-vlr-res fi-tot-vlr-abe fi-cgc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-restricao w-livre 
FUNCTION fn-restricao RETURNS CHARACTER
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
DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 7 BY 1.25 TOOLTIP "Detalhar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-credito AUTO-GO 
     IMAGE-UP FILE "image/im-fornec.bmp":U
     LABEL "" 
     SIZE 7 BY 1.25 TOOLTIP "Dados Gerais do Cliente"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-fat-aut AUTO-GO 
     IMAGE-UP FILE "image/imt-smabe.bmp":U
     IMAGE-INSENSITIVE FILE "image/imt-smades.bmp":U
     LABEL "" 
     SIZE 9 BY 1.92 TOOLTIP "Faturamento Autom†tico o Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-fat-man AUTO-GO 
     IMAGE-UP FILE "image/im-calc4.bmp":U
     LABEL "" 
     SIZE 7 BY 1.33 TOOLTIP "Faturamento Manual do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imp-danfe AUTO-GO 
     IMAGE-UP FILE "image/img-nfe.bmp":U
     LABEL "" 
     SIZE 9 BY 2.21 TOOLTIP "Imprime DANFE"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Imprime Resumo de Faturamento"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U
     LABEL "" 
     SIZE 7 BY 1.25 TOOLTIP "Alteraá‰es do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica AUTO-END-KEY 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "&Sair" 
     SIZE 7 BY 1.25 TOOLTIP "Modificar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-monitor AUTO-GO 
     IMAGE-UP FILE "image/img-gati.bmp":U
     LABEL "" 
     SIZE 9 BY 1.92 TOOLTIP "Monitor NFE"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nenhum AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 7 BY 1.25 TOOLTIP "Desmarca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 5 BY 1.25 TOOLTIP "ParÉmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-pre-nota AUTO-GO 
     IMAGE-UP FILE "image/im-prigr.bmp":U
     LABEL "" 
     SIZE 7 BY 1.25 TOOLTIP "Imprime PrÇ-Nota"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-sintegra AUTO-GO 
     IMAGE-UP FILE "image/im-globo-03.bmp":U
     LABEL "" 
     SIZE 7 BY 1.25 TOOLTIP "Copia CNPJ para Sintegra"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-todos AUTO-GO 
     IMAGE-UP FILE "image\im-ran_a.bmp":U
     LABEL "" 
     SIZE 7 BY 1.25 TOOLTIP "Marca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 7 BY 1.25 TOOLTIP "Posicionar no Pedido"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE cb-regras AS INTEGER FORMAT ">9":U INITIAL 99 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Todos",99,
                     "N∆o Aprovados",1,
                     "∑ Vista",2,
                     "Em Separaá∆o",3,
                     "Data Marcada",4,
                     "Valor Max NF",5,
                     "Valor Min NF",6,
                     "Valor Min DUP",7,
                     "Exportaá∆o",8,
                     "Bloq Faturamento",9,
                     "Fat. N∆o Liberado",10,
                     "Inconsistente",11
     DROP-DOWN-LIST
     SIZE 21 BY 1
     BGCOLOR 8 FONT 6 NO-UNDO.

DEFINE VARIABLE ed-obs AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 107 BY 3.5
     FGCOLOR 12 FONT 12 NO-UNDO.

DEFINE VARIABLE fi-cgc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 2 BY .67
     BGCOLOR 7 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-abe AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-ped AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-res AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-abe AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-ped AS DECIMAL FORMAT ">,>>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-res AS DECIMAL FORMAT ">,>>>,>>9.99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 107 BY 2
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 9 BY 14.25
     BGCOLOR 8 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 117 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tg-faturaveis AS LOGICAL INITIAL yes 
     LABEL " Pedidos Fatur†veis" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.14 BY 1.04
     BGCOLOR 8 FGCOLOR 2 FONT 6 NO-UNDO.

DEFINE VARIABLE tg-imp-danfe AS LOGICAL INITIAL no 
     LABEL " Impress∆o de Danfe" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY 1.04
     BGCOLOR 8 FGCOLOR 2 FONT 6 NO-UNDO.

DEFINE VARIABLE tg-restricao AS LOGICAL INITIAL no 
     LABEL "Pedidos com Restriá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 22 BY 1.04
     BGCOLOR 8 FGCOLOR 12 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pedidos FOR 
      tt-ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos w-livre _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.nr-nota-fis     COLUMN-LABEL "Nota Fiscal"     WIDTH 10
      tt-ped-venda.nr-pedcli       COLUMN-LABEL "Pedido"          WIDTH 7
      tt-ped-venda.nome-abrev      COLUMN-LABEL "Cliente"         WIDTH 13
      tt-ped-venda.dt-entrega      COLUMN-LABEL "Dt Entrega"      WIDTH 10
      tt-ped-venda.nome-transp     COLUMN-LABEL "Transportadora"  WIDTH 14
      tt-ped-venda.estado          COLUMN-LABEL "UF"              WIDTH 3
      tt-ped-venda.cidade          COLUMN-LABEL "Cidade"          WIDTH 20
      tt-ped-venda.tp-frete        COLUMN-LABEL "Tipo Frete"      WIDTH 12
      tt-ped-venda.qt-pedida       COLUMN-LABEL "Qt Pedida"       WIDTH 10
      tt-ped-venda.qt-reservada    COLUMN-LABEL "Qt Reservada"    WIDTH 10
      tt-ped-venda.qt-aberto       COLUMN-LABEL "Qt Aberto"       WIDTH 9
      tt-ped-venda.tipo-pedido     COLUMN-LABEL "Tipo de Pedido"  WIDTH 15 FORMAT "x(20)" 
      fn-restricao()               COLUMN-LABEL "Restriá∆o"       WIDTH 12 FORMAT "x(15)"
      tt-ped-venda.vl-reservado    COLUMN-LABEL "Vlr NF Faturar"  WIDTH 10
      tt-ped-venda.vl-desconto     COLUMN-LABEL "Vlr Desconto"    WIDTH 10
      tt-ped-venda.vl-tot-ped      COLUMN-LABEL "Vlr TOTAL"       WIDTH 10
      (tt-ped-venda.vl-reservado / tt-ped-venda.num-parcelas) 
                                   COLUMN-LABEL "Vlr DUP Faturar" WIDTH 10
      tt-ped-venda.qt-volumes      COLUMN-LABEL "Qt Vol"          WIDTH 5
      tt-ped-venda.ind-fat-par     COLUMN-LABEL "FatPar"
      tt-ped-venda.bloqueio        COLUMN-LABEL "Bloqueio"       
      tt-ped-venda.dt-entrega      COLUMN-LABEL "Dt Entrega"     
      tt-ped-venda.credito         COLUMN-LABEL "CrÇdito"         WIDTH 11
      tt-ped-venda.des-cond-pagto  COLUMN-LABEL "Cond Pagto"      WIDTH 12
ENABLE
      tt-ped-venda.qt-volumes
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 107 BY 14.25
         FONT 1
         TITLE "Pedidos Reservados" ROW-HEIGHT-CHARS .67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-param AT ROW 1.13 COL 91.29
     bt-imprime AT ROW 1.13 COL 96.57
     tg-imp-danfe AT ROW 1.21 COL 2.86 WIDGET-ID 2
     tg-faturaveis AT ROW 1.21 COL 26.86
     tg-restricao AT ROW 1.21 COL 48
     cb-regras AT ROW 1.38 COL 68 COLON-ALIGNED NO-LABEL
     br-pedidos AT ROW 2.75 COL 2
     bt-vapara AT ROW 3 COL 111
     bt-todos AT ROW 4.58 COL 111
     bt-nenhum AT ROW 5.88 COL 111
     bt-credito AT ROW 7.5 COL 111
     bt-sintegra AT ROW 8.79 COL 111
     bt-modifica AT ROW 10.29 COL 111
     bt-consulta AT ROW 11.58 COL 111
     bt-pre-nota AT ROW 12.88 COL 111
     bt-log AT ROW 14.13 COL 111
     bt-fat-man AT ROW 15.46 COL 111
     bt-fat-aut AT ROW 17.04 COL 110
     ed-obs AT ROW 17.25 COL 2 NO-LABEL
     bt-monitor AT ROW 18.96 COL 110 WIDGET-ID 6
     bt-imp-danfe AT ROW 20.88 COL 110 WIDGET-ID 4
     fi-tot-qtd-ped AT ROW 21.75 COL 2.86 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-qtd-res AT ROW 21.75 COL 21 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-qtd-abe AT ROW 21.75 COL 41 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-vlr-ped AT ROW 21.75 COL 59.29 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-vlr-res AT ROW 21.75 COL 77.43 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-tot-vlr-abe AT ROW 21.75 COL 94.14 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     fi-cgc AT ROW 1.5 COL 87.57 COLON-ALIGNED NO-LABEL
     "Vl Tot Aberto" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 21.17 COL 96.14
          BGCOLOR 8 
     "Qt Total Pedido" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 21.13 COL 4.86
          BGCOLOR 8 
     "Qt Tot Reservada" VIEW-AS TEXT
          SIZE 14 BY .54 AT ROW 21.13 COL 23
          BGCOLOR 8 
     "Vl Total Pedido" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 21.17 COL 61.14
          BGCOLOR 8 
     "Vl Tot Reservado" VIEW-AS TEXT
          SIZE 12.43 BY .54 AT ROW 21.17 COL 79.43
          BGCOLOR 8 
     "Qt Tot Aberto" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 21.13 COL 43
          BGCOLOR 8 
     RECT-1 AT ROW 21 COL 2
     RECT-2 AT ROW 2.75 COL 110
     rt-button AT ROW 1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121 BY 22.25
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
         TITLE              = "Faturamento Autom†tico de Pedidos"
         COLUMN             = 16.29
         ROW                = 5.67
         HEIGHT             = 22.17
         WIDTH              = 118.72
         MAX-HEIGHT         = 28.21
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 28.21
         VIRTUAL-WIDTH      = 182.86
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
/* BROWSE-TAB br-pedidos cb-regras f-cad */
/* SETTINGS FOR COMBO-BOX cb-regras IN FRAME f-cad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cb-regras:HIDDEN IN FRAME f-cad           = TRUE.

ASSIGN 
       ed-obs:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi-tot-qtd-abe IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-ped IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-res IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-abe IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-ped IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-res IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE
                                 tt-ped-venda.faturado = l-ped-faturado AND
                                 tt-ped-venda.visualiza = YES NO-LOCK
                                 BY tt-ped-venda.nr-nota-fis BY tt-ped-venda.nome-transp BY tt-ped-venda.nome-abrev BY tt-ped-venda.nr-pedcli.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "mgmov.ped-venda.cod-sit-ped = 1"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Faturamento Autom†tico de Pedidos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Faturamento Autom†tico de Pedidos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON ANY-KEY OF br-pedidos IN FRAME f-cad /* Pedidos Reservados */
DO:
  IF INDEX("1234567890",KEYFUNCTION(LASTKEY)) <> 0 THEN DO.
     ASSIGN c-busca = c-busca + KEYFUNCTION(LASTKEY).
    
     IF SUBSTR(STRING(tt-ped-venda.nr-pedcli),1,LENGTH(c-busca)) <> c-busca THEN DO.
        FIND FIRST tt-ped-venda WHERE
                   (tt-ped-venda.cod-sit-ped = 1 OR tt-ped-venda.cod-sit-ped = 3) AND
                    tt-ped-venda.nr-pedcli BEGINS c-busca NO-LOCK NO-ERROR.
        IF AVAIL tt-ped-venda THEN DO.
           h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR.   
           APPLY 'value-changed' TO SELF.
        END.
        ELSE DO.
           ASSIGN c-busca = "".
           FIND tt-ped-venda WHERE 
                ROWID(tt-ped-venda) = v-row-table NO-LOCK NO-ERROR.
        END.
     END.
     IF AVAIL tt-ped-venda THEN DO.
        IF tt-ped-venda.nr-pedcli = c-busca THEN
           ASSIGN c-busca = "".
     END.
  END.
  ELSE 
     ASSIGN c-busca = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON CURSOR-DOWN OF br-pedidos IN FRAME f-cad /* Pedidos Reservados */
DO:
    br-pedidos:SELECT-NEXT-ROW().
    APPLY "VALUE-CHANGED" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON CURSOR-UP OF br-pedidos IN FRAME f-cad /* Pedidos Reservados */
DO:
  br-pedidos:SELECT-PREV-ROW().
  APPLY "VALUE-CHANGED" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON MOUSE-SELECT-DBLCLICK OF br-pedidos IN FRAME f-cad /* Pedidos Reservados */
DO:
  APPLY 'choose' TO bt-consulta.
  /*
  tt-ped-venda.qt-volumes:READ-ONLY IN BROWSE br-pedidos = NO.
  APPLY 'ENTRY' TO tt-ped-venda.qt-volumes IN BROWSE br-pedidos.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON RETURN OF br-pedidos IN FRAME f-cad /* Pedidos Reservados */
DO:
  APPLY 'choose' TO bt-consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON ROW-DISPLAY OF br-pedidos IN FRAME f-cad /* Pedidos Reservados */
DO:
   ASSIGN tt-ped-venda.nr-nota-fis:FONT IN BROWSE br-pedidos = 6
          tt-ped-venda.nr-pedcli:FONT = 6.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON ROW-LEAVE OF br-pedidos IN FRAME f-cad /* Pedidos Reservados */
DO:
  tt-ped-venda.qt-volumes:READ-ONLY IN BROWSE br-pedidos = YES.

  FIND ped-venda-ext WHERE
       ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND
       ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido SHARE-LOCK NO-ERROR.
  IF AVAIL ped-venda-ext THEN
     ASSIGN ped-venda-ext.qt-fardos = INPUT BROWSE br-pedidos tt-ped-venda.qt-volumes.
  FIND CURRENT ped-venda-ext NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON VALUE-CHANGED OF br-pedidos IN FRAME f-cad /* Pedidos Reservados */
DO:
   IF tg-faturaveis:SCREEN-VALUE = 'yes' OR 
      tg-restricao:SCREEN-VALUE = 'yes' THEN DO.

      bt-fat-aut:LOAD-IMAGE("image/imt-smfec.bmp").

      ASSIGN bt-fat-man:SENSITIVE = NO
             bt-fat-aut:SENSITIVE = l-fatura
             bt-modifica:SENSITIVE = YES
             bt-consulta:SENSITIVE = YES
             bt-imprime:SENSITIVE = YES
             bt-log:SENSITIVE = YES
             bt-sintegra:SENSITIVE = YES
             bt-credito:SENSITIVE = YES.

    
      IF AVAIL tt-ped-venda THEN DO.
         ASSIGN ed-obs:SCREEN-VALUE = tt-ped-venda.observ.
    
         IF tt-ped-venda.restricao = 0 THEN DO.
            bt-fat-aut:LOAD-IMAGE("image/imt-smabe.bmp").
    
            ASSIGN bt-fat-man:SENSITIVE = l-fatura.
    
            IF br-pedidos:NUM-SELECTED-ROWS > 1 THEN DO.
               ASSIGN bt-fat-man:SENSITIVE = NO
                      bt-modifica:SENSITIVE = NO
                      bt-consulta:SENSITIVE = NO
                      bt-imprime:SENSITIVE = NO
                      bt-log:SENSITIVE = NO
                      bt-sintegra:SENSITIVE = NO
                      bt-credito:SENSITIVE = NO.
    
               ASSIGN ed-obs:SCREEN-VALUE = "".
            END.
         END.
    
          IF tt-ped-venda.tipo-pedido MATCHES '*Export*' THEN
             bt-sintegra:SENSITIVE = NO.

          ASSIGN bt-imp-danfe:SENSITIVE = NO.
       END.
   END.

   IF tg-imp-danfe:SCREEN-VALUE = 'yes' THEN DO.
      ASSIGN bt-imp-danfe:SENSITIVE = NO.

      IF AVAIL tt-ped-venda THEN DO.
          IF tt-ped-venda.danfe-autorizado = NO THEN DO.
             RUN esapi/danfe-autorizado.p (INPUT tt-ped-venda.cod-estabel,
                                           INPUT tt-ped-venda.serie,
                                           INPUT tt-ped-venda.nr-nota-fis,
                                           OUTPUT l-danfe-autoriz).
             ASSIGN tt-ped-venda.danfe-autorizado = l-danfe-autoriz.
          END.

          ASSIGN bt-imp-danfe:SENSITIVE = tt-ped-venda.danfe-autoriz.
      END.

      ASSIGN bt-sintegra:SENSITIVE = NO
             bt-credito:SENSITIVE = NO
             bt-fat-aut:SENSITIVE = NO
             bt-fat-man:SENSITIVE = NO
             bt-modifica:SENSITIVE = NO.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta w-livre
ON CHOOSE OF bt-consulta IN FRAME f-cad
DO:
   IF tg-imp-danfe:SCREEN-VALUE = 'YES' THEN DO.
      ASSIGN gr-nota-fiscal = ROWID(nota-fiscal).

      ASSIGN w-livre:SENSITIVE = NO.
      RUN ftp/ft0904.w.
      ASSIGN w-livre:SENSITIVE = YES.
   END.
   ELSE DO.
       FIND ped-venda WHERE
            ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
            ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
       ASSIGN gr-ped-venda = ROWID(ped-venda).
    
       ASSIGN w-livre:SENSITIVE = NO.
       RUN esp\espd4000.w (INPUT "Consultar").
       ASSIGN w-livre:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-credito w-livre
ON CHOOSE OF bt-credito IN FRAME f-cad
DO:
    FIND FIRST ems5.cliente WHERE
               ems5.cliente.cdn_cliente = tt-ped-venda.cod-emit NO-LOCK NO-ERROR.

    IF AVAIL ems5.cliente THEN
       ASSIGN v_rec_cliente = RECID(ems5.cliente).

    ASSIGN w-livre:SENSITIVE = NO.
    RUN prgfin/acr/acr205aa.p.
    ASSIGN w-livre:SENSITIVE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-fat-aut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-fat-aut w-livre
ON CHOOSE OF bt-fat-aut IN FRAME f-cad
DO:
    IF SELF:IMAGE = "image/imt-smabe.bmp" THEN DO.  /* Fatura o Pedido */
      IF l-fatura THEN DO.
         EMPTY TEMP-TABLE wt-notas-geradas.

         DO i-row = 1 TO br-pedidos:NUM-SELECTED-ROWS:
            IF br-pedidos:FETCH-SELECTED-ROW(i-row) THEN DO.

               FIND ped-venda WHERE
                    ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND 
                    ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli
                    SHARE-LOCK NO-ERROR.

                FOR EACH ped-item OF ped-venda NO-LOCK.                
                    RUN esapi/desaloca-peditem.p (INPUT ROWID(ped-item)).
                END.
                RUN pi-gera-nota.
            END.
         END.
      END.

      RUN pi-mostra (INPUT 0).
   END.
   ELSE DO. /* Libera Restriá∆o do Pedido */
      IF br-pedidos:NUM-SELECTED-ROWS <> 1 THEN NEXT.

      ASSIGN c-nr-pedcli = tt-ped-venda.nr-pedcli.

      CASE tt-ped-venda.restricao.
          WHEN 4 THEN DO.
              ASSIGN l-data-marcada = NO.
              RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT tt-ped-venda.nome-abrev,
                                             INPUT "Liberado Regra de Data Marcada para Faturamento",
                                             INPUT NO).
          END.
          WHEN 5 THEN DO.
              ASSIGN l-valor-max-nf = NO. 
              RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT tt-ped-venda.nome-abrev,
                                             INPUT "Liberado Regra de Valor M†ximo de NF",
                                             INPUT NO).
          END.
          WHEN 6 THEN DO.
              ASSIGN l-valor-min-nf = NO.
              RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT tt-ped-venda.nome-abrev,
                                             INPUT "Liberado Regra de Valor M°nimo de NF",
                                             INPUT NO).
          END.
          WHEN 7 THEN DO.
              ASSIGN l-valor-min-dup = NO.
              RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT tt-ped-venda.nome-abrev,
                                             INPUT "Liberado Regra de Valor M°nimo de Duplicata",
                                             INPUT NO).
          END.
          WHEN 10 THEN DO.
              RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT tt-ped-venda.nome-abrev,
                                             INPUT "Liberado Regra de Separaá∆o N«O Finalizada",
                                             INPUT NO).
          END.
          WHEN 11 THEN DO.
              RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                             INPUT tt-ped-venda.nome-abrev,
                                             INPUT "Liberado Regra de Inconsistància",
                                             INPUT NO).
          END.
      END.
      RUN pi-calc-restricao.

      tg-restricao:SCREEN-VALUE = 'NO'.
      IF tt-ped-venda.restricao <> 0 THEN
         ASSIGN cb-regras:SCREEN-VALUE = STRING(tt-ped-venda.restricao)
                tg-restricao:SCREEN-VALUE = 'YES'.

      APPLY 'VALUE-CHANGED' TO tg-restricao.

      FIND tt-ped-venda WHERE
           tt-ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
      br-pedidos:SELECT-FOCUSED-ROW().
      APPLY 'value-changed' TO br-pedidos.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-fat-man
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-fat-man w-livre
ON CHOOSE OF bt-fat-man IN FRAME f-cad
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN w-livre:SENSITIVE = NO.
   RUN ftp\ft4002.w.
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imp-danfe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imp-danfe w-livre
ON CHOOSE OF bt-imp-danfe IN FRAME f-cad
DO:
   DO i-row = 1 TO br-pedidos:NUM-SELECTED-ROWS:
      IF br-pedidos:FETCH-SELECTED-ROW(i-row) THEN DO.
         FIND nota-fiscal WHERE
              nota-fiscal.cod-estabel = tt-ped-venda.cod-estabel AND
              nota-fiscal.serie = tt-ped-venda.serie AND
              nota-fiscal.nr-nota-fis = tt-ped-venda.nr-nota-fis NO-LOCK NO-ERROR.

         ASSIGN l-danfe-autoriz = NO.
         IF AVAIL nota-fiscal THEN
            RUN esapi/danfe-autorizado.p (INPUT nota-fiscal.cod-estabel,
                                          INPUT nota-fiscal.serie,
                                          INPUT nota-fiscal.nr-nota-fis,
                                          OUTPUT l-danfe-autoriz).

         IF NOT l-danfe-autoriz THEN DO.
            MESSAGE "Existem Notas Selecionadas que N∆o foram Validadas..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.
   END.

   DO i-row = 1 TO br-pedidos:NUM-SELECTED-ROWS:
      IF br-pedidos:FETCH-SELECTED-ROW(i-row) THEN DO.
         RUN esrp/essp0161rp.p (INPUT tt-ped-venda.nr-nota-fis, 
                                INPUT tt-ped-venda.nr-nota-fis,
                                INPUT tt-ped-venda.cod-estabel,
                                INPUT tt-ped-venda.serie).

         /*
         FIND FIRST b-nota-fiscal WHERE
                    b-nota-fiscal.nr-nota-fis > tt-ped-venda.nr-nota-fis AND
                    b-nota-fiscal.cod-estabel = tt-ped-venda.cod-estabel AND
                    b-nota-fiscal.serie = tt-ped-venda.serie AND
                    b-nota-fiscal.nome-ab-cli = tt-ped-venda.nome-abrev-tri AND
                    b-nota-fiscal.vl-tot-nota = tt-ped-venda.vl-tot-nota
                    NO-LOCK NO-ERROR. 
         IF AVAIL b-nota-fiscal THEN
            RUN esrp/essp0161rp.p (INPUT b-nota-fiscal.nr-nota-fis, 
                                   INPUT b-nota-fiscal.nr-nota-fis,
                                   INPUT b-nota-fiscal.cod-estabel,
                                   INPUT b-nota-fiscal.serie).
         */

         ASSIGN tt-ped-venda.visualiza = NO.
      END.
   END.

   APPLY 'VALUE-CHANGED' TO tg-imp-danfe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-livre
ON CHOOSE OF bt-imprime IN FRAME f-cad
DO:
   RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log w-livre
ON CHOOSE OF bt-log IN FRAME f-cad
DO:
  RUN esp/essp0155b.p (INPUT tt-ped-venda.nr-pedcli).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica w-livre
ON CHOOSE OF bt-modifica IN FRAME f-cad /* Sair */
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).
  
   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Modificar").
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-monitor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-monitor w-livre
ON CHOOSE OF bt-monitor IN FRAME f-cad
DO:
   RUN pi-monitor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-cad
DO:
    br-pedidos:DESELECT-ROWS().
    APPLY 'value-changed' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-livre
ON CHOOSE OF bt-param IN FRAME f-cad /* Sair */
DO:
   ASSIGN l-entrega-unica = YES
          l-data-marcada  = YES 
          l-valor-max-nf  = YES 
          l-valor-min-nf  = YES 
          l-valor-min-dup = YES.

   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp/essp0161a.w (INPUT-OUTPUT c-cod-estabel,   
                        INPUT-OUTPUT c-dt-limite,   
                        INPUT-OUTPUT c-nr-pedcli-ini,
                        INPUT-OUTPUT c-nr-pedcli-fin,
                        INPUT-OUTPUT c-it-codigo-ini,
                        INPUT-OUTPUT c-it-codigo-fin,
                        INPUT-OUTPUT c-cod-refer-ini,
                        INPUT-OUTPUT c-cod-refer-fin,
                        INPUT-OUTPUT c-nome-abrev-ini,
                        INPUT-OUTPUT c-nome-abrev-fin,
                        INPUT-OUTPUT c-no-ab-reppri-ini,
                        INPUT-OUTPUT c-no-ab-reppri-fin,
                        INPUT-OUTPUT c-opc-artigo,
                        INPUT-OUTPUT l-lote-todos,
                        INPUT-OUTPUT l-lote-pp,
                        INPUT-OUTPUT l-lote-pd,
                        INPUT-OUTPUT l-lote-rp,
                        INPUT-OUTPUT l-lote-rd,
                        INPUT-OUTPUT l-lote-sc,
                        INPUT-OUTPUT l-lote-ca,
                        OUTPUT l-ok). 
   IF l-ok THEN                                     
      RUN pi-popula-browse.

   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pre-nota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pre-nota w-livre
ON CHOOSE OF bt-pre-nota IN FRAME f-cad
DO:
    CREATE tt-param.
    ASSIGN tt-param.usuario          = c-seg-usuario
           tt-param.data-exec        = TODAY
           tt-param.hora-exec        = TIME
           tt-param.destino          = 3
           tt-param.classifica       = 1
           tt-param.arquivo          = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp"
           tt-param.estabel-ini      = tt-ped-venda.cod-estab
           tt-param.estabel-fin      = tt-ped-venda.cod-estab
           tt-param.pedido-ini       = INT(tt-ped-venda.nr-pedcli)
           tt-param.pedido-fin       = INT(tt-ped-venda.nr-pedcli)
           tt-param.dt-emissao-ini   = 01.01.0001     
           tt-param.dt-emissao-fin   = 12.31.9999     
           tt-param.dt-entrega-ini   = 01.01.0001      
           tt-param.dt-entrega-fin   = 12.31.9999      
           tt-param.cod-emit-ini     = 0
           tt-param.cod-emit-fin     = 999999
           tt-param.no-ab-reppri-ini = ''   
           tt-param.no-ab-reppri-fin = 'ZZZZZZZZZZZZZZ'   
           tt-param.nome-transp-ini  = ''                  
           tt-param.nome-transp-fin  = 'ZZZZZZZZZZZZZZ'    
           tt-param.corte-comerc-ini = ''
           tt-param.corte-comerc-fin = 'ZZZ'
           tt-param.so-indigo        = NO    
           tt-param.exc-indigo       = NO
           tt-param.it-codigo        = ''
           tt-param.sit-total        = NO
           tt-param.sit-aberto       = YES   
           tt-param.sit-parcial      = YES    
           tt-param.sit-pendentes    = YES    
           tt-param.sit-suspensos    = YES    
           tt-param.sit-cancelados   = NO
           tt-param.cond-credito     = "T"   
           tt-param.cond-pagto       = "T"    
           tt-param.mercado          = "A"    
           tt-param.tp-pedido        = ""    
           tt-param.aceita-parc      = "T"    
           tt-param.qtd-minima       = 0    
           tt-param.perc-minres      = 0    
           tt-param.min-it-ares      = 0    
           tt-param.max-it-ares      = 9999    
           tt-param.it-reservados    = YES.

    SESSION:SET-WAIT-STATE("general":U).

    RUN pi-carrega-dados.
    {include/i-rprun.i esrp/espd0002rp.p}

    IF tt-param.destino = 3 THEN DO.
       RUN utp/ut-utils.p PERSISTENT SET h-prog.
       RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
                             INPUT tt-param.arquivo).
       DELETE PROCEDURE h-prog.
    END.
    SESSION:SET-WAIT-STATE("":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sintegra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sintegra w-livre
ON CHOOSE OF bt-sintegra IN FRAME f-cad
DO:
   FIND emitente WHERE
        emitente.nome-abrev = tt-ped-venda.nome-abrev
        NO-LOCK NO-ERROR.

   IF AVAIL emitente THEN DO.
      ASSIGN fi-cgc:SCREEN-VALUE = emitente.cgc.
      fi-cgc:SET-SELECTION(1,50). 
      fi-cgc:EDIT-COPY(). 

      RUN esapi/sintegra.p (INPUT emitente.estado).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad
DO:
   br-pedidos:SELECT-ALL.
   APPLY 'value-changed' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-livre
ON CHOOSE OF bt-vapara IN FRAME f-cad
DO:
  RUN esdlg/d01essp0155.w (OUTPUT c-nr-pedcli).

  IF c-nr-pedcli <> "" THEN DO:
     FIND FIRST tt-ped-venda WHERE
                tt-ped-venda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR.
     IF AVAIL tt-ped-venda AND 
        tt-ped-venda.visualiza = YES THEN DO:
        h-query:REPOSITION-TO-ROWID(ROWID(tt-ped-venda)) NO-ERROR. 
        br-pedidos:SELECT-FOCUSED-ROW().
        APPLY 'value-changed' TO br-pedidos.
     END.
     ELSE
        MESSAGE "Pedido n∆o est† contido na seleá∆o!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-regras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-regras w-livre
ON VALUE-CHANGED OF cb-regras IN FRAME f-cad
DO:
   RUN pi-mostra (INPUT SELF:INPUT-VALUE).
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


&Scoped-define SELF-NAME tg-faturaveis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-faturaveis w-livre
ON VALUE-CHANGED OF tg-faturaveis IN FRAME f-cad /*  Pedidos Fatur†veis */
DO:
   ASSIGN l-ped-faturado = NO.
   ASSIGN cb-regras:VISIBLE = NO.

   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      ASSIGN tg-restricao:SCREEN-VALUE = 'NO'
             tg-imp-danfe:SCREEN-VALUE = 'NO'.
      bt-fat-aut:TOOLTIP = 'Faturamento Autom†tico do Pedido'.
      bt-consulta:TOOLTIP = "Detalhar Pedido de Venda".
   END.

   RUN pi-mostra (INPUT 0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-imp-danfe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-imp-danfe w-livre
ON VALUE-CHANGED OF tg-imp-danfe IN FRAME f-cad /*  Impress∆o de Danfe */
DO:
   ASSIGN l-ped-faturado = NO.
   ASSIGN cb-regras:VISIBLE = NO.

   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      ASSIGN l-ped-faturado = YES.
      ASSIGN tg-faturaveis:SCREEN-VALUE = 'NO'
             tg-restricao:SCREEN-VALUE = 'NO'.

      bt-consulta:TOOLTIP = "Detalhar Nota Fiscal".
    END.

    RUN pi-mostra (INPUT 0).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-restricao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-restricao w-livre
ON VALUE-CHANGED OF tg-restricao IN FRAME f-cad /* Pedidos com Restriá∆o */
DO:
   ASSIGN cb-regras:VISIBLE = NO
          l-ped-faturado = NO.
   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      ASSIGN tg-faturaveis:SCREEN-VALUE = 'NO'
             tg-imp-danfe:SCREEN-VALUE = 'NO'.

      ASSIGN cb-regras:SENSITIVE = YES
             cb-regras:VISIBLE = YES.

      ASSIGN cb-regras:SCREEN-VALUE = '99'.

      bt-fat-aut:TOOLTIP = 'Libera Restriá∆o do Pedido'.
      bt-consulta:TOOLTIP = "Detalhar Pedido de Venda".

      APPLY 'VALUE-CHANGED' TO cb-regras.
   END.
   ELSE RUN pi-mostra (INPUT 0).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
FIND FIRST param-dis NO-LOCK NO-ERROR.
FIND FIRST para-fat NO-LOCK NO-ERROR.

br-pedidos:NUM-LOCKED-COLUMNS = 4.

tt-ped-venda.qt-volumes:READ-ONLY IN BROWSE br-pedidos = YES.


/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
       RUN set-position IN h_p-exihel ( 1.13 , 102.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-imprime:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY tg-imp-danfe tg-faturaveis tg-restricao ed-obs fi-tot-qtd-ped 
          fi-tot-qtd-res fi-tot-qtd-abe fi-tot-vlr-ped fi-tot-vlr-res 
          fi-tot-vlr-abe fi-cgc 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE RECT-1 RECT-2 rt-button bt-param bt-imprime tg-imp-danfe tg-faturaveis 
         tg-restricao br-pedidos bt-vapara bt-todos bt-nenhum bt-credito 
         bt-sintegra bt-modifica bt-consulta bt-pre-nota bt-log bt-fat-man 
         bt-fat-aut ed-obs bt-monitor bt-imp-danfe fi-cgc 
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

  {utp/ut9000.i "ESSP0161" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa
       WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  ASSIGN h-query = br-pedidos:QUERY.
  ASSIGN tt-ped-venda.nr-nota-fis:VISIBLE IN BROWSE br-pedidos = NO. 

  ASSIGN c-dt-limite = STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999')
         l-lote-todos = YES.

  /*
  FIND FIRST para-ped NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = para-ped.estab-padrao.
  */
  FIND FIRST ped-venda NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = ped-venda.cod-estabel.

  RUN enable-relatorio IN h_p-exihel (INPUT NO).

  FIND usuar_grp_usuar WHERE 
       usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
       usuar_grp_usuar.cod_grp_usuar = "FAT" NO-LOCK NO-ERROR.
  ASSIGN l-fatura = AVAIL usuar_grp_usuar. 

  ASSIGN l-fatura = YES.

  IF c-cod-estabel = "" THEN DO.
     ASSIGN bt-param:SENSITIVE = NO.

     MESSAGE 'Usuario ' c-seg-usuario ' n∆o relacionado Ö um Estabelecimento....'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

     APPLY 'VALUE-CHANGED' TO tg-faturaveis IN FRAME {&FRAME-NAME}.
  END.
  ELSE
     APPLY 'choose' TO bt-param.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-restricao w-livre 
PROCEDURE pi-calc-restricao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND
         ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido 
         NO-LOCK NO-ERROR.

    IF AVAIL ped-venda-ext THEN DO.
        /* Calcula Restriáoes */
    
        FIND natur-oper WHERE
             natur-oper.nat-operacao = tt-ped-venda.nat-operacao NO-LOCK NO-ERROR.

        /* N∆o Aprovados exceto "Ö vista e exportaá∆o" */
        /* Foi considerado a condiá∆o de Pagamento e n∆o o tipo de pedido, porque */
        /* podem haver pedidos do tipo Ö vista e com condiá∆o de pagamento diferente de 1-A vista */
        IF ped-venda-ext.tp-pedido <> 'Exportaá∆o' AND
           (tt-ped-venda.cod-sit-aval < 2 OR     
            tt-ped-venda.cod-sit-aval > 3) THEN DO.
           ASSIGN tt-ped-venda.restricao = 1.
           NEXT.
        END.

        IF tt-ped-venda.cod-sit-preco <> 2 OR 
           tt-ped-venda.cod-sit-com <> 2 THEN DO.
           ASSIGN tt-ped-venda.restricao = 1.
           NEXT.
        END.
    
        IF l-entrega-unica AND /* Entrega Ènica */
           tt-ped-venda.ind-fat-par = NO AND
           tt-ped-venda.qt-aberto <> 0 THEN DO.
           ASSIGN tt-ped-venda.restricao = 3.
           NEXT.
        END.
    
        IF tt-ped-venda.qt-aberto = 0 AND
           tt-ped-venda.qt-reservada <> tt-ped-venda.qt-romaneio THEN DO.
           ASSIGN tt-ped-venda.restricao = 11.
           NEXT.
        END.

        IF l-data-marcada AND   /* Data Marcada */
           (ped-venda-ext.tp-entrega = 'Na Data' OR
            ped-venda-ext.tp-entrega = "A Partir da Data") AND  
           tt-ped-venda.dt-entrega > TODAY THEN DO.           
           ASSIGN tt-ped-venda.restricao = 4.
           NEXT.
        END.
    
        IF ped-venda-ext.tp-pedido = '∑ Vista' AND  /* ∑ Vista */ 
           ped-venda-ext.l-bloqueio  THEN DO.
           ASSIGN tt-ped-venda.restricao = 2.
           NEXT.
        END.
    
        IF ped-venda-ext.tp-pedido = 'Exportaá∆o' AND  /* Exportaá∆o Bloqueados p/ Faturamento */
           ped-venda-ext.l-bloqueio THEN DO.
           ASSIGN tt-ped-venda.restricao = 8.
           NEXT.
        END.
    
        /* Restriá‰es para Pedidos exceto Övista e exportaá∆o */
        IF ped-venda-ext.tp-pedido <> '∑ Vista' AND
           ped-venda-ext.tp-pedido <> 'Exportaá∆o' THEN DO.
    
           IF natur-oper.emite-duplic THEN DO.
              IF l-valor-max-nf AND
                 ped-venda-ext.fat-max-nf > 0 AND  /* Valor M†ximo NF  */
                 tt-ped-venda.vl-reservado > ped-venda-ext.fat-max-nf THEN DO. 
                 ASSIGN tt-ped-venda.restricao = 5.
                 NEXT.
              END.
        
              IF l-valor-min-nf AND  /* Valor M°nimo NF */
                 (tt-ped-venda.vl-reservado < param-dis.vl-min-nf OR
                  (tt-ped-venda.vl-aberto > 0 AND
                   tt-ped-venda.vl-aberto < param-dis.vl-min-nf)) THEN DO. 
                 ASSIGN tt-ped-venda.restricao = 6.
                 NEXT.
              END.
        
              IF l-valor-min-dup AND /* Valor M°nimo DUPLICATA */
                 tt-ped-venda.cod-portador <> 990 AND
                 ((tt-ped-venda.vl-reservado / tt-ped-venda.num-parcelas) < param-dis.vl-min-dup OR
                  (tt-ped-venda.vl-aberto > 0 AND
                   (tt-ped-venda.vl-aberto / tt-ped-venda.num-parcelas) < param-dis.vl-min-dup)) THEN DO.
                 ASSIGN tt-ped-venda.restricao = 7.
                 NEXT.
              END.
           END.
    
           IF ped-venda-ext.l-bloqueio THEN DO. /* Bloqueio de Faturamento */
              ASSIGN tt-ped-venda.restricao = 9.
              NEXT.
           END.

           IF ped-venda-ext.qt-fardos = 0 THEN DO.
              ASSIGN tt-ped-venda.restricao = 10.
              NEXT.
           END.
        END.
        ASSIGN tt-ped-venda.restricao = 0.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados w-livre 
PROCEDURE pi-carrega-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {include/i-rprun.i esrp/espd0002arp.p}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-nota w-livre 
PROCEDURE pi-gera-nota :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Definiá∆o da vari†veis */
    DEF VAR h-bodi317pr          AS HANDLE NO-UNDO.
    DEF VAR h-bodi317sd          AS HANDLE NO-UNDO.
    DEF VAR h-bodi317im1bra      AS HANDLE NO-UNDO.
    DEF VAR h-bodi317va          AS HANDLE NO-UNDO.
    DEF VAR h-bodi317in          AS HANDLE NO-UNDO.
    DEF VAR h-bodi317ef          AS HANDLE NO-UNDO.
    DEF VAR l-proc-ok-aux        AS LOG    NO-UNDO.
    DEF VAR c-ultimo-metodo-exec AS CHAR   NO-UNDO.
    DEF VAR c-cod-estabel        AS CHAR   NO-UNDO.
    DEF VAR c-serie              AS CHAR   NO-UNDO.
    DEF VAR da-dt-emis-nota      AS DATE   NO-UNDO.
    DEF VAR da-dt-base-dup       AS DATE   NO-UNDO.
    DEF VAR da-dt-prvenc         AS DATE   NO-UNDO.
    DEF VAR c-seg-usuario        AS CHAR   NO-UNDO.
    DEF VAR c-nome-abrev         AS CHAR   NO-UNDO.   
    DEF VAR c-nr-pedcli          AS CHAR   NO-UNDO.
    DEF VAR c-nat-operacao       AS CHAR   NO-UNDO.
    DEF VAR c-cod-canal-venda    AS CHAR   NO-UNDO.
    DEF VAR i-seq-wt-docto       AS INT    NO-UNDO.

    DEF VAR c-clibnf      AS   CHAR FORMAT "x(200)".
    DEF VAR c-cliadq      AS   CHAR FORMAT "x(200)".
    
    /* Informaá‰es do embarque para c†lculo */
    ASSIGN c-seg-usuario     = c-seg-usuario            /* Usu†rio                    */
           c-cod-estabel     = tt-ped-venda.cod-estabel /* Estabelecimento do pedido  */
           c-serie           = para-fat.serie-pad       /* SÇrie das notas            */
           c-nome-abrev      = tt-ped-venda.nome-abrev  /* Nome abreviado do cliente  */
           c-nr-pedcli       = tt-ped-venda.nr-pedcli   /* Nr pedido do cliente       */
           da-dt-emis-nota   = TODAY                    /* Data de emiss∆o da nota    */
           c-nat-operacao    = ?                        /* Quando Ç ? busca do pedido */
           c-cod-canal-venda = ?.                       /* Quando Ç ? busca do pedido */
    
    FIND ped-venda WHERE
         ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND 
         ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli
         SHARE-LOCK NO-ERROR.

    /* Inicializaá∆o das BOS para C†lculo */
    RUN dibo/bodi317in.p PERSISTENT SET h-bodi317in.
    RUN inicializaBOS IN h-bodi317in (OUTPUT h-bodi317pr,
                                      OUTPUT h-bodi317sd,     
                                      OUTPUT h-bodi317im1bra,
                                      OUTPUT h-bodi317va).

    /* In°cio da transaá∆o */
    REPEAT TRANS:
        /* Limpar a tabela de erros em todas as BOS */
        RUN emptyRowErrors        IN h-bodi317in.
    
        /* Cria o registro WT-DOCTO para o pedido */
        RUN criaWtDocto IN h-bodi317sd (INPUT  c-seg-usuario,
                                        INPUT  c-cod-estabel,
                                        INPUT  c-serie,
                                        INPUT  "1", 
                                        INPUT  c-nome-abrev,
                                        INPUT  c-nr-pedcli,
                                        INPUT  1,    
                                        INPUT  9999, 
                                        INPUT  da-dt-emis-nota,
                                        INPUT  0,  
                                        INPUT  c-nat-operacao,
                                        INPUT  c-cod-canal-venda,
                                        OUTPUT i-seq-wt-docto,
                                        OUTPUT l-proc-ok-aux).

        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317sd IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                  OUTPUT TABLE RowErrors).
    
        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors NO-LOCK NO-ERROR.
        
        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN DO.
           FOR EACH RowErrors:
               MESSAGE rowerrors.errordescription
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK
                       TITLE "Erro - NF".
           END.
        END.
        
        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
           UNDO, LEAVE.
    
        FIND FIRST wt-docto WHERE
                   wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.

        IF tt-ped-venda.dt-base-ft <> ? THEN 
           IF AVAIL wt-docto THEN
              ASSIGN wt-docto.dt-base-dup = tt-ped-venda.dt-base-ft.

        IF tt-ped-venda.ind-fat-par = NO AND
           tt-ped-venda.qt-aberto <> 0 THEN
           ASSIGN ped-venda.ind-fat-par = YES.
        
        /* Limpar a tabela de erros em todas as BOS */
        RUN emptyRowErrors        IN h-bodi317in.

        /* Gera os itens para o pedido, com tela de acompanhamento */
        RUN inicializaAcompanhamento      IN h-bodi317sd.
        RUN geraWtItDoctoComItensDoPedido IN h-bodi317sd (OUTPUT l-proc-ok-aux).
        RUN finalizaAcompanhamento        IN h-bodi317sd.

        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317sd         IN h-bodi317sd (OUTPUT c-ultimo-metodo-exec,
                                                          OUTPUT TABLE RowErrors).

        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors NO-LOCK NO-ERROR.

        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN DO.
           FOR EACH RowErrors:
               MESSAGE rowerrors.errordescription 
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK
                   TITLE 'Erro - Itens NF'.
           END.
        END.

        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
           UNDO, LEAVE.

        ASSIGN de-peso-emb = 0.
        FOR EACH wt-it-docto WHERE
                 wt-it-docto.seq-wt-docto = i-seq-wt-docto NO-LOCK.

            FIND ped-item-res WHERE
                 ped-item-res.nr-pedcli  = c-nr-pedcli  AND
                 ped-item-res.nome-abrev = c-nome-abrev AND 
                 ped-item-res.nr-sequencia = wt-it-docto.nr-sequencia AND 
                 ped-item-res.faturado = NO 
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL ped-item-res THEN NEXT.

            FIND FIRST ped-item-ext WHERE
                       ped-item-ext.cod-estabel = ped-item-res.cod-estabel AND
                       ped-item-ext.nr-pedcli = ped-item-res.nr-pedcli AND 
                       ped-item-ext.nome-abrev = ped-item-res.nome-abrev AND
                       ped-item-ext.nr-sequencia = ped-item-res.nr-sequencia
                       NO-LOCK NO-ERROR.  

            IF ped-item-ext.bloqueio-fat THEN NEXT.

            /* Limpar a tabela de erros em todas as BOS */
            RUN emptyRowErrors IN h-bodi317in.

            /* Atende todos os itens do pedido, com tela de acompanhamento */
            RUN inicializaAcompanhamento IN h-bodi317pr.
            RUN atendeTotalSeq IN h-bodi317pr (INPUT  i-seq-wt-docto,
                                               INPUT  wt-it-docto.seq-wt-it-docto,
                                               OUTPUT l-proc-ok-aux).
            RUN finalizaAcompanhamento IN h-bodi317pr.
             
            /* Busca poss°veis erros que ocorreram nas validaá‰es */
            RUN devolveErrosbodi317pr IN h-bodi317pr (OUTPUT c-ultimo-metodo-exec,
                                                      OUTPUT TABLE RowErrors).

            /* Pesquisa algum erro ou advertància que tenha ocorrido */
            FIND FIRST RowErrors NO-LOCK NO-ERROR.

            /* Caso tenha achado algum erro ou advertància, mostra em tela */
            IF AVAIL RowErrors THEN DO.
               FOR EACH RowErrors:
                    MESSAGE "Seq: " ped-item-res.nr-sequencia 
                            rowerrors.errordescription 
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK
                        TITLE "ERRO - Atendendo Reservas".
               END.
            END.

            /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
            IF NOT l-proc-ok-aux THEN
               UNDO, LEAVE.

            /* Calcula peso Bruto da NF */ 
            FIND FIRST wt-docto WHERE
                       wt-docto.seq-wt-docto = i-seq-wt-docto NO-ERROR.
            
            FOR EACH ped-item-rom WHERE
                     ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                     ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                     ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                     NO-LOCK BREAK BY ped-item-rom.nr-volume.
                FIND ob-etiqueta WHERE
                     ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                     ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                     NO-LOCK NO-ERROR.

                FIND ITEM WHERE
                     ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

                IF ITEM.un = 'kg' THEN
                   ASSIGN wt-docto.peso-bru-tot-inf = wt-docto.peso-bru-tot-inf + ob-etiqueta.quantidade
                          wt-docto.peso-liq-tot-inf = wt-docto.peso-liq-tot-inf + ob-etiqueta.quantidade.
                ELSE
                   ASSIGN wt-docto.peso-bru-tot-inf = wt-docto.peso-bru-tot-inf + (ob-etiqueta.quantidade * ITEM.peso-bruto)
                          wt-docto.peso-liq-tot-inf = wt-docto.peso-liq-tot-inf + (ob-etiqueta.quantidade * ITEM.peso-liq).
            END.
        END.


        /* Limpar a tabela de erros em todas as BOS */
        RUN emptyRowErrors           IN h-bodi317in.
    
        /* Calcula o pedido, com acompanhamento */
        RUN inicializaAcompanhamento IN h-bodi317pr.
        RUN confirmaCalculo          IN h-bodi317pr (INPUT  i-seq-wt-docto,
                                                     OUTPUT l-proc-ok-aux).
        RUN finalizaAcompanhamento   IN h-bodi317pr.
    
        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317pr    IN h-bodi317pr (OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT TABLE RowErrors).
    
        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors NO-LOCK NO-ERROR.
        
        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN
           FOR EACH RowErrors:
               MESSAGE rowerrors.errordescription
                        VIEW-AS ALERT-BOX ERROR BUTTONS OK
                        TITLE "Erro - Calculo".
           END.
        
        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN
           UNDO, LEAVE.

        FIND wt-docto WHERE
             wt-docto.seq-wt-docto = i-seq-wt-docto.

        /* Efetiva os pedidos e cria a nota */
        RUN dibo/bodi317ef.p PERSISTENT SET h-bodi317ef.
        RUN emptyRowErrors           IN h-bodi317in.
        RUN inicializaAcompanhamento IN h-bodi317ef.
        RUN setaHandlesBOS           IN h-bodi317ef(h-bodi317pr,     
                                                    h-bodi317sd, 
                                                    h-bodi317im1bra, 
                                                    h-bodi317va).
        RUN efetivaNota              IN h-bodi317ef (INPUT i-seq-wt-docto,
                                                     INPUT YES,
                                                     OUTPUT l-proc-ok-aux).
        RUN finalizaAcompanhamento   IN h-bodi317ef.
    

        /* Busca poss°veis erros que ocorreram nas validaá‰es */
        RUN devolveErrosbodi317ef    IN h-bodi317ef (OUTPUT c-ultimo-metodo-exec,
                                                     OUTPUT TABLE RowErrors).
    
        /* Pesquisa algum erro ou advertància que tenha ocorrido */
        FIND FIRST RowErrors WHERE
                   RowErrors.ErrorSubType = "ERROR":U NO-ERROR.
    
        /* Caso tenha achado algum erro ou advertància, mostra em tela */
        IF AVAIL RowErrors THEN
           FOR EACH RowErrors:
               MESSAGE rowerrors.errordescription
                       VIEW-AS ALERT-BOX ERROR BUTTONS OK
                       TITLE "Erro - Efetivaá∆o".
           END.
        
        /* Caso ocorreu problema nas validaá‰es, n∆o continua o processo */
        IF NOT l-proc-ok-aux THEN DO:
           DELETE PROCEDURE h-bodi317ef.
           UNDO, LEAVE.
        END.
    
        /* Busca as notas fiscais geradas */
        RUN buscaTTNotasGeradas IN h-bodi317ef (OUTPUT l-proc-ok-aux,
                                                OUTPUT TABLE tt-notas-geradas).
    
        /* Elimina o handle do programa bodi317ef */
        DELETE PROCEDURE h-bodi317ef.

        IF tt-ped-venda.ind-fat-par = NO AND
           tt-ped-venda.qt-aberto <> 0 THEN
           ASSIGN ped-venda.ind-fat-par = NO.
        
        LEAVE.
    END.
            
    /* Finalizaá∆o das BOS utilizada no c†lculo */
    RUN finalizaBOS in h-bodi317in.
    
    FOR EACH tt-notas-geradas NO-LOCK:
        CREATE wt-notas-geradas.
        ASSIGN wt-notas-geradas.rw-nota-fiscal = tt-notas-geradas.rw-nota-fiscal.

        FIND FIRST nota-fiscal WHERE
             ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal
             NO-ERROR.

        IF nota-fiscal.nome-abrev-tri = "" THEN DO.
           FIND ped-venda WHERE
                ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
                ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.

           IF AVAIL ped-venda THEN DO.
              FIND tt-ped-venda WHERE
                   tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
                   tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
              IF NOT AVAIL tt-ped-venda THEN DO:
                 CREATE tt-ped-venda.
                 BUFFER-COPY ped-venda TO tt-ped-venda.
              END.
           END.
        END.
        ELSE DO.
            FIND LAST b-nota-fiscal WHERE
                      b-nota-fiscal.nr-nota-fis <> nota-fiscal.nr-nota-fis AND
                      b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel AND
                      b-nota-fiscal.serie = nota-fiscal.serie AND
                      b-nota-fiscal.nome-abrev-tri = nota-fiscal.nome-ab-cli AND
                      b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota
                      NO-ERROR. 
            IF NOT AVAIL b-nota-fiscal THEN
               FIND LAST b-nota-fiscal WHERE
                         b-nota-fiscal.nr-nota-fis <> nota-fiscal.nr-nota-fis AND
                         b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel AND
                         b-nota-fiscal.serie = nota-fiscal.serie AND
                         b-nota-fiscal.nome-ab-cli = nota-fiscal.nome-abrev-tri AND
                         b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota
                         NO-ERROR. 

            IF AVAIL b-nota-fiscal THEN DO.
               FIND ped-venda WHERE
                    ped-venda.nome-abrev = b-nota-fiscal.nome-ab-cli AND
                    ped-venda.nr-pedcli = b-nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.

               IF AVAIL ped-venda THEN DO.
                  FIND tt-ped-venda WHERE
                       tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
                       tt-ped-venda.nome-abrev = ped-venda.nome-abrev AND 
                       tt-ped-venda.nr-nota-fis  = nota-fiscal.nr-nota-fis NO-ERROR.
                  IF NOT AVAIL tt-ped-venda THEN DO:
                     CREATE tt-ped-venda.
                     BUFFER-COPY ped-venda TO tt-ped-venda.
                  END.
               END.
            END.
        END.

        IF AVAIL tt-ped-venda THEN
           ASSIGN tt-ped-venda.faturado = YES
                  tt-ped-venda.nr-nota-fis = nota-fiscal.nr-nota-fis
                  tt-ped-venda.serie = nota-fiscal.serie
                  tt-ped-venda.vl-tot-nota = nota-fiscal.vl-tot-nota.
    END.

    RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                   INPUT tt-ped-venda.nome-abrev,
                                   INPUT "Emitida Nota Fiscal " + tt-ped-venda.nr-nota-fis,
                                   INPUT NO).


    FIND emitente WHERE
         emitente.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.

    IF emitente.cod-suframa <> '' THEN DO.
       ASSIGN c-mensagem = "Emitido a Nota Fiscal " + tt-ped-venda.nr-nota-fis +
                           " para Zona Franca de Manaus, favor informar o Codigo PIN para o Faturamento".

       RUN esapi/esapi002.p (INPUT "imatextil@imatextil.com.br", /* e-mail remetente */
                             INPUT "contabilidade@imatextil.com.br,", /* e-mail destinat†rio */
                             INPUT "Solicitaá∆o de PIN" , /* Assunto */
                             INPUT c-mensagem, /* Mensagem */
                             INPUT "", /*arquivo anexo*/
                             INPUT NO). /* Mostra Erros */
    END.

    FOR EACH tt-log-ped-venda.
        CREATE his-ped-venda-ext.
        BUFFER-COPY tt-log-ped-venda TO his-ped-venda-ext.
    END.
    EMPTY TEMP-TABLE tt-log-ped-venda.

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
  PUT c-empresa  FORMAT "X(40)"                 AT  1
      "DATA: "                                  AT 77
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT 83
      "HORA: "                                  AT 97
      STRING(TIME,"hh:mm:ss")                   AT 103
      SKIP(1).
    
  PUT "RESUMO DE FATURAMENTO" AT 50 SKIP(1).

  PUT "Situacao                Qtde Pedida   Qtde Reserva    Qtde Aberto   Valor Pedido  Valor Reserva   Valor Aberto" AT 1.
  PUT "--------------------  -------------  -------------  -------------  -------------  -------------  -------------" AT 1.
  PUT SKIP (1).
 
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
    DEF VAR de-qtd-ped AS DEC FORMAT "->>>,>>9.99". 
    DEF VAR de-vlr-ped AS DEC FORMAT "->>>,>>9.99". 
    DEF VAR de-qtd-res AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-vlr-res AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-qtd-abe AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-vlr-abe AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-tot-qtd-ped AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-tot-vlr-ped AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-tot-qtd-res AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-tot-vlr-res AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-tot-qtd-abe AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-tot-vlr-abe AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-xtot-qtd-ped AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-xtot-vlr-ped AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-xtot-qtd-res AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-xtot-vlr-res AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-xtot-qtd-abe AS DEC FORMAT "->>>,>>9.99".
    DEF VAR de-xtot-vlr-abe AS DEC FORMAT "->>>,>>9.99".

    DEF VAR i-lin AS INT.
    DEF VAR c-arq-saida AS CHAR.

    ASSIGN c-arq-saida = SESSION:TEMP-DIRECTORY + c-seg-usuario + '.txt'.
    OUTPUT TO VALUE(c-arq-saida) CONVERT SOURCE "ibm850" PAGED PAGE-SIZE 74.

       ASSIGN i-lin = 99
              de-qtd-ped = 0       de-vlr-ped = 0      de-qtd-res = 0      de-vlr-res = 0
              de-tot-qtd-ped = 0   de-tot-vlr-ped = 0  de-tot-qtd-res = 0  de-tot-vlr-res = 0.

       FOR EACH tt-ped-venda WHERE
                tt-ped-venda.faturado = l-ped-faturado 
                NO-LOCK BREAK BY tt-ped-venda.restricao.

           IF i-lin > 74 THEN DO:
              RUN pi-imp-cabec.
              ASSIGN i-lin = 7.
           END.

           ASSIGN de-qtd-ped = de-qtd-ped + tt-ped-venda.qt-pedida
                  de-vlr-ped = de-vlr-ped + tt-ped-venda.vl-pedido
                  de-qtd-res = de-qtd-res + tt-ped-venda.qt-reservada
                  de-vlr-res = de-vlr-res + tt-ped-venda.vl-reservado.

           IF LAST-OF(tt-ped-venda.restricao) THEN DO.
              ASSIGN de-qtd-abe = de-qtd-ped - de-qtd-res
                     de-vlr-abe = de-vlr-ped - de-vlr-res.

              CASE tt-ped-venda.restricao.
                  WHEN 0 THEN PUT 'Fatur†veis' AT 1.
                  WHEN 1 THEN PUT 'N∆o Aprovados' AT 3.
                  WHEN 2 THEN PUT '∑ Vista' AT 3.
                  WHEN 3 THEN PUT 'Entrega Ènica' AT 3.
                  WHEN 4 THEN PUT 'Data Marcada' AT 3.
                  WHEN 5 THEN PUT 'Valor Max NF' AT 3.
                  WHEN 6 THEN PUT 'Valor Min NF' AT 3.
                  WHEN 7 THEN PUT 'Valor Min DUP' AT 3.
                  WHEN 8 THEN PUT 'Exportaá∆o' AT 3.
                  WHEN 9 THEN PUT 'Bloqueio Faturamento' AT 3.
              END CASE.

              PUT de-qtd-ped AT 25
                  de-qtd-res AT 40
                  de-qtd-abe AT 55
                  de-vlr-ped AT 70
                  de-vlr-res AT 85
                  de-vlr-abe AT 100
                  SKIP.

              IF tt-ped-venda.restricao = 0 THEN 
                 PUT SKIP(1)
                     "COM RESTRIÄ«O" AT 1
                     "-------------" AT 1 
                     SKIP.
              ELSE
                 ASSIGN de-xtot-qtd-ped = de-xtot-qtd-ped + de-qtd-ped
                        de-xtot-vlr-ped = de-xtot-vlr-ped + de-vlr-ped
                        de-xtot-qtd-res = de-xtot-qtd-res + de-qtd-res
                        de-xtot-vlr-res = de-xtot-vlr-res + de-vlr-res
                        de-xtot-qtd-abe = de-xtot-qtd-abe + de-qtd-abe
                        de-xtot-vlr-abe = de-xtot-vlr-abe + de-vlr-abe.
    
              ASSIGN de-tot-qtd-ped = de-tot-qtd-ped + de-qtd-ped
                     de-tot-vlr-ped = de-tot-vlr-ped + de-vlr-ped
                     de-tot-qtd-res = de-tot-qtd-res + de-qtd-res
                     de-tot-vlr-res = de-tot-vlr-res + de-vlr-res
                     de-tot-qtd-abe = de-tot-qtd-abe + de-qtd-abe
                     de-tot-vlr-abe = de-tot-vlr-abe + de-vlr-abe.

              ASSIGN i-lin = i-lin + 1
                     de-qtd-ped = 0   de-vlr-ped = 0   de-qtd-res = 0   de-vlr-res = 0
                     de-qtd-abe = 0   de-vlr-abe = 0.
           END.
       END.

       PUT SKIP(1)
           "-------------  -------------  -------------  -------------  -------------  -------------" AT 23
           "  TOTAL ..........."  AT 1
           de-xtot-qtd-ped   FORMAT "->>>,>>9.99" AT 25 
           de-xtot-qtd-res   FORMAT "->>>,>>9.99" AT 40
           de-xtot-qtd-abe   FORMAT "->>>,>>9.99" AT 55
           de-xtot-vlr-ped   FORMAT "->>>,>>9.99" AT 70 
           de-xtot-vlr-res   FORMAT "->>>,>>9.99" AT 85
           de-xtot-vlr-abe   FORMAT "->>>,>>9.99" AT 100.

       PUT SKIP(2)
           "TOTAL GERAL........"  AT 1
           de-tot-qtd-ped   FORMAT "->>>,>>9.99" AT 25 
           de-tot-qtd-res   FORMAT "->>>,>>9.99" AT 40
           de-tot-qtd-abe   FORMAT "->>>,>>9.99" AT 55
           de-tot-vlr-ped   FORMAT "->>>,>>9.99" AT 70 
           de-tot-vlr-res   FORMAT "->>>,>>9.99" AT 85
           de-tot-vlr-abe   FORMAT "->>>,>>9.99" AT 100.

    OUTPUT CLOSE.

    RUN utp/ut-utils.p PERSISTENT SET h-prog.
    RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
                          INPUT c-arq-saida).
    DELETE PROCEDURE h-prog.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime-notas w-livre 
PROCEDURE pi-imprime-notas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-nr-nota-ini LIKE nota-fiscal.nr-nota-fis.
    DEF VAR c-nr-nota-fin LIKE nota-fiscal.nr-nota-fis.

    /* Mostrar as notas geradas */
    FIND FIRST wt-notas-geradas NO-LOCK.
    FIND FIRST nota-fiscal WHERE
         ROWID(nota-fiscal) = wt-notas-geradas.rw-nota-fiscal
         NO-LOCK NO-ERROR.
    ASSIGN c-nr-nota-ini = nota-fiscal.nr-nota-fis.

    FIND LAST wt-notas-geradas NO-LOCK.
    FIND FIRST nota-fiscal WHERE
         ROWID(nota-fiscal) = wt-notas-geradas.rw-nota-fiscal
         NO-LOCK NO-ERROR.
    ASSIGN c-nr-nota-fin = nota-fiscal.nr-nota-fis.

    IF c-nr-nota-ini = c-nr-nota-fin THEN /* Gerou apenas uma Nota */
       RUN utp/ut-msgs.p (INPUT "show",
                          INPUT 15263,
                          INPUT STRING(c-nr-nota-ini) + "~~" +
                                STRING(nota-fiscal.cod-estabel)  + "~~" +
                                STRING(nota-fiscal.serie)).
    ELSE
       RUN utp/ut-msgs.p (INPUT "show",
                          INPUT 15264,
                          INPUT STRING(c-nr-nota-ini) + "~~" +
                                STRING(c-nr-nota-fin) + "~~" +
                                STRING(nota-fiscal.cod-estabel) + "~~" +
                                STRING(nota-fiscal.serie)).

    RUN esrp/essp0161rp.p (INPUT c-nr-nota-ini,
                           INPUT c-nr-nota-fin,
                           INPUT nota-fiscal.cod-estabel,
                           INPUT nota-fiscal.serie).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monitor w-livre 
PROCEDURE pi-monitor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-arq-java AS CHAR.
    DEF VAR c-comando AS CHAR.                                

    ASSIGN c-arq-java = SESSION:TEMP-DIRECTORY + "abrir.js".

    OUTPUT TO VALUE(c-arq-java).
       PUT 'var oIE = new ActiveXObject("InternetExplorer.Application");' SKIP
           'oIE.Navigate2("http://192.168.0.5:8080/nfe-monitor/");' FORMAT "x(150)" SKIP     
           'oIE.Visible = true;' SKIP.
    OUTPUT CLOSE.

    ASSIGN c-comando = 'wscript.exe ' + c-arq-java.

    OS-COMMAND SILENT VALUE(c-comando).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra w-livre 
PROCEDURE pi-mostra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-restricao AS INT.

  ASSIGN fi-tot-qtd-ped = 0
         fi-tot-qtd-res = 0
         fi-tot-vlr-ped = 0
         fi-tot-vlr-res = 0.

  tt-ped-venda.nr-nota-fis:VISIBLE IN BROWSE br-pedidos = tg-imp-danfe:INPUT-VALUE IN FRAME {&FRAME-NAME}. 

  tt-ped-venda.credito:VISIBLE IN BROWSE br-pedidos = NO.
  tt-ped-venda.ind-fat-par:VISIBLE IN BROWSE br-pedidos = NO.
  tt-ped-venda.bloqueio:VISIBLE IN BROWSE br-pedidos = NO.

  CASE p-restricao.
      WHEN 1 THEN
           tt-ped-venda.credito:VISIBLE IN BROWSE br-pedidos = YES.
      WHEN 2 OR WHEN 8 OR WHEN 9 THEN
           tt-ped-venda.bloqueio:VISIBLE IN BROWSE br-pedidos = YES.
      WHEN 3 THEN
           tt-ped-venda.ind-fat-par:VISIBLE IN BROWSE br-pedidos = YES. 
  END CASE.

  FOR EACH tt-ped-venda.
      ASSIGN tt-ped-venda.visualiza = NO.

      IF tg-imp-danfe:INPUT-VALUE IN FRAME {&FRAME-NAME} = YES AND 
         tt-ped-venda.nr-nota-fis <> "" THEN DO.
         FIND nota-fiscal WHERE
              nota-fiscal.cod-estabel = tt-ped-venda.cod-estabel AND
              nota-fiscal.serie = tt-ped-venda.serie AND
              nota-fiscal.nr-nota-fis = tt-ped-venda.nr-nota-fis NO-LOCK NO-ERROR.

         ASSIGN tt-ped-venda.visualiza = nota-fiscal.ind-sit-nota = 1.
      END.

      IF tg-faturaveis:INPUT-VALUE IN FRAME {&FRAME-NAME} = YES AND
         tt-ped-venda.nr-nota-fis = "" AND
         tt-ped-venda.restricao = p-restricao THEN 
         ASSIGN tt-ped-venda.visualiza = YES.

      IF tg-restricao:INPUT-VALUE IN FRAME {&FRAME-NAME} = YES AND
         tt-ped-venda.nr-nota-fis = "" AND
         tt-ped-venda.restricao <> 0  THEN DO.
         IF p-restricao = 99 THEN
            ASSIGN tt-ped-venda.visualiza = YES.
         ELSE IF p-restricao = tt-ped-venda.restricao THEN
              ASSIGN tt-ped-venda.visualiza = YES.
      END.

      IF tt-ped-venda.visualiza THEN
         ASSIGN fi-tot-qtd-ped = fi-tot-qtd-ped + tt-ped-venda.qt-pedida
                fi-tot-vlr-ped = fi-tot-vlr-ped + tt-ped-venda.vl-pedido
                fi-tot-qtd-res = fi-tot-qtd-res + tt-ped-venda.qt-reservada
                fi-tot-vlr-res = fi-tot-vlr-res + tt-ped-venda.vl-reservado.
  END.

  {&OPEN-QUERY-br-pedidos}
   

  ASSIGN fi-tot-qtd-abe = fi-tot-qtd-ped - fi-tot-qtd-res
         fi-tot-vlr-abe = fi-tot-vlr-ped - fi-tot-vlr-res.

  DISP fi-tot-qtd-ped
       fi-tot-qtd-res
       fi-tot-qtd-abe
       fi-tot-vlr-ped
       fi-tot-vlr-res
       fi-tot-vlr-abe
       WITH FRAME {&FRAME-NAME}.

  APPLY 'entry' TO br-pedidos IN FRAME {&FRAME-NAME}.

  IF br-pedidos:QUERY:NUM-RESULTS > 0 THEN
     br-pedidos:SELECT-ROW(1).

  APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.

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
   {utp/ut-liter.i Selecionando_Pedidos *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-ped-venda.

   ASSIGN c-lotes = "".
   IF l-lote-todos = YES THEN
      ASSIGN c-lotes = "pp pd rp rd sc ca ".
   ELSE DO:
      ASSIGN c-lotes = c-lotes + IF l-lote-pp = YES THEN "pp " ELSE "   ".
      ASSIGN c-lotes = c-lotes + IF l-lote-pd = YES THEN "pd " ELSE "   ".
      ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "rp " ELSE "   ".
      ASSIGN c-lotes = c-lotes + IF l-lote-rd = YES THEN "rd " ELSE "   ".
      ASSIGN c-lotes = c-lotes + IF l-lote-sc = YES THEN "sc " ELSE "   ".
      ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "ca " ELSE "   ".
   END.

   RUN esapi/ret-udm.p (INPUT c-dt-limite, OUTPUT c-dia).
   ASSIGN da-dt-entrega = DATE(c-dia + SUBSTR(c-dt-limite,1,2) + SUBSTR(c-dt-limite,3,4)).

   FOR EACH ped-item-res WHERE
            ped-item-res.faturado = NO AND
            ped-item-res.cod-estabel = c-cod-estabel    AND
            ped-item-res.nr-pedcli  >= c-nr-pedcli-ini  AND
            ped-item-res.nr-pedcli  <= c-nr-pedcli-fin  AND
            ped-item-res.nome-abrev >= c-nome-abrev-ini AND
            ped-item-res.nome-abrev <= c-nome-abrev-fin AND
            ped-item-res.it-codigo  >= c-it-codigo-ini  AND
            ped-item-res.it-codigo  <= c-it-codigo-fin  AND
            ped-item-res.cod-refer  >= c-cod-refer-ini  AND 
            ped-item-res.cod-refer  <= c-cod-refer-fin  AND 
            ped-item-res.qt-pedida  > 0 NO-LOCK,
      FIRST ped-item-ext WHERE
            ped-item-ext.cod-estabel = ped-item-res.cod-estabel AND
            ped-item-ext.nr-pedcli = ped-item-res.nr-pedcli AND 
            ped-item-ext.nome-abrev = ped-item-res.nome-abrev AND
            ped-item-ext.nr-sequencia = ped-item-res.nr-sequencia
            NO-LOCK. 

       RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item-res.nr-pedcli).

       FIND item-ext WHERE
            item-ext.it-codigo = ped-item-res.it-codigo NO-LOCK NO-ERROR.

       FIND ped-venda WHERE
            ped-venda.nr-pedcli = ped-item-res.nr-pedcli AND
            ped-venda.nome-abrev = ped-item-res.nome-abrev NO-LOCK NO-ERROR.

       IF ped-venda.cod-sit-ped = 3 OR 
          ped-venda.cod-sit-ped = 6 THEN NEXT.

       IF ped-venda.cod-estabel <> c-cod-estabel THEN NEXT.

       IF ped-venda.mo-codigo <> 0 THEN NEXT.

       IF ped-venda.dt-entrega > da-dt-entrega OR
          ped-venda.no-ab-reppri < c-no-ab-reppri-ini OR
          ped-venda.no-ab-reppri > c-no-ab-reppri-fin THEN NEXT.

       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
            ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

       IF ped-venda-ext.l-nao-aprovar THEN NEXT.

       FIND tt-ped-venda WHERE
            tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
            tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
       IF NOT AVAIL tt-ped-venda THEN DO:
          CREATE tt-ped-venda.
          BUFFER-COPY ped-venda TO tt-ped-venda.

          IF AVAIL ped-venda-ext THEN
             ASSIGN tt-ped-venda.tp-frete = ped-venda-ext.tp-frete
                    tt-ped-venda.qt-volumes = ped-venda-ext.qt-fardos.
       END.

       FIND ped-item OF ped-item-res NO-LOCK NO-ERROR.

       ASSIGN tt-ped-venda.qt-reservada = tt-ped-venda.qt-reservada + ped-item-res.qt-pedida
              tt-ped-venda.vl-reservado = tt-ped-venda.vl-reservado + IF AVAIL ped-item
                                                                      THEN (ped-item.qt-pedida * ped-item.vl-preuni)
                                                                      ELSE 0. 

       FOR EACH ped-item-rom WHERE
                ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                NO-LOCK NO-ERROR.
           IF ob-etiqueta.situacao = 4 THEN
              ASSIGN tt-ped-venda.qt-romaneio = tt-ped-venda.qt-romaneio + ob-etiqueta.quantidade.
       END.
   END.

   FOR EACH nota-fiscal WHERE
            nota-fiscal.cod-estabel = c-cod-estabel  AND
            nota-fiscal.serie = para-fat.serie-pad AND
            nota-fiscal.ind-sit-nota = 1 AND
            nota-fiscal.dt-cancela = ? NO-LOCK.

       RUN pi-acompanhar IN h-acomp (INPUT "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

       FIND natur-oper WHERE
            natur-oper.nat-oper = nota-fiscal.nat-oper NO-LOCK NO-ERROR.

       IF nota-fiscal.nome-abrev-tri = "" THEN DO.
          FIND ped-venda WHERE
               ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
               ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
    
          IF NOT AVAIL ped-venda THEN NEXT.

          FIND tt-ped-venda WHERE
               tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
               tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
          IF NOT AVAIL tt-ped-venda THEN DO:
             CREATE tt-ped-venda.
             BUFFER-COPY ped-venda TO tt-ped-venda.
          END.
       END.
       ELSE DO.
           FIND FIRST b-nota-fiscal WHERE
                      b-nota-fiscal.nr-nota-fis <> nota-fiscal.nr-nota-fis AND
                      b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel AND
                      b-nota-fiscal.serie = nota-fiscal.serie AND
                      b-nota-fiscal.nome-ab-cli = nota-fiscal.nome-abrev-tri AND
                      b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota
                      NO-ERROR. 
           IF NOT AVAIL b-nota-fiscal THEN
              FIND FIRST b-nota-fiscal WHERE
                         b-nota-fiscal.nr-nota-fis <> nota-fiscal.nr-nota-fis AND
                         b-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel AND
                         b-nota-fiscal.serie = nota-fiscal.serie AND
                         b-nota-fiscal.nome-abrev-tri = nota-fiscal.nome-ab-cli AND
                         b-nota-fiscal.vl-tot-nota = nota-fiscal.vl-tot-nota
                         NO-ERROR. 

           FIND ped-venda WHERE
                ped-venda.nome-abrev = b-nota-fiscal.nome-ab-cli AND
                ped-venda.nr-pedcli = b-nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.

           IF NOT AVAIL ped-venda THEN
              FIND ped-venda WHERE
                   ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
                   ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.

           IF NOT AVAIL ped-venda THEN NEXT.

           FIND tt-ped-venda WHERE
                tt-ped-venda.nr-pedcli = ped-venda.nr-pedcli AND
                tt-ped-venda.nome-abrev = ped-venda.nome-abrev AND 
                tt-ped-venda.nr-nota-fis  = nota-fiscal.nr-nota-fis NO-ERROR.
           IF NOT AVAIL tt-ped-venda THEN DO:
              CREATE tt-ped-venda.
              BUFFER-COPY ped-venda TO tt-ped-venda.
           END.
       END.
       
       RUN esapi/danfe-autorizado.p (INPUT nota-fiscal.cod-estabel,
                                     INPUT nota-fiscal.serie,
                                     INPUT nota-fiscal.nr-nota-fis,
                                     OUTPUT l-danfe-autoriz).
       
       ASSIGN tt-ped-venda.danfe-autorizado = l-danfe-autoriz
              tt-ped-venda.faturado = YES
              tt-ped-venda.nr-nota-fis = nota-fiscal.nr-nota-fis
              tt-ped-venda.serie = nota-fiscal.serie.

       FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
           ASSIGN tt-ped-venda.qt-reservada = tt-ped-venda.qt-reservada + it-nota-fisc.qt-faturada[1]
                  tt-ped-venda.vl-reservado = tt-ped-venda.vl-reservado + it-nota-fisc.vl-tot-item.
       END.
   END.

   FOR EACH tt-ped-venda.
       FIND cond-pagto WHERE
            cond-pagto.cod-cond-pag = tt-ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
       IF AVAIL cond-pagto THEN 
          ASSIGN tt-ped-venda.num-parcelas = cond-pagto.num-parcelas
                 tt-ped-venda.des-cond-pagto = cond-pagto.descricao.
       ELSE DO.
          ASSIGN tt-ped-venda.des-cond-pagto = 'E S P E C I A L'.
          FOR EACH cond-ped WHERE
                   cond-ped.nr-pedido = INT(tt-ped-venda.nr-pedcli) NO-LOCK.
              ASSIGN tt-ped-venda.num-parcelas = tt-ped-venda.num-parcelas + 1.
          END.
       END.

       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND
            ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido 
            NO-LOCK NO-ERROR.

       IF NOT AVAIL ped-venda-ext THEN NEXT.

       ASSIGN tt-ped-venda.tipo-pedido = ped-venda-ext.tp-pedido
              tt-ped-venda.bloqueio = ped-venda-ext.l-bloqueio.

       FOR EACH ped-item WHERE
                ped-item.nr-pedcli = tt-ped-venda.nr-pedcli AND
                ped-item.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK.

           IF ped-item.cod-sit-item = 6 THEN NEXT.

           ASSIGN tt-ped-venda.vl-desconto = tt-ped-venda.vl-desconto + ped-item.val-desconto-total.

           ASSIGN tt-ped-venda.qt-pedida = tt-ped-venda.qt-pedida + ped-item.qt-pedida
                  tt-ped-venda.vl-pedido = tt-ped-venda.vl-pedido + (ped-item.qt-pedida * ped-item.vl-preuni).
       END.
       

       ASSIGN tt-ped-venda.qt-aberto = ABS(tt-ped-venda.qt-pedida - tt-ped-venda.qt-reservada)
              tt-ped-venda.vl-aberto = ABS(tt-ped-venda.vl-pedido - tt-ped-venda.vl-reservado).
       
       ASSIGN tt-ped-venda.vl-tot-ped = tt-ped-venda.vl-tot-ped + tt-ped-venda.vl-desconto.

       {esinc/i-dsrb.i tt-ped-venda.cod-sit-aval tt-ped-venda.cod-sit-aval tt-ped-venda.credito}.  

       IF tt-ped-venda.nr-pedido = 171444 THEN NEXT.

       RUN pi-calc-restricao.
   END.

   RUN pi-finalizar IN h-acomp.

   ASSIGN tg-faturaveis:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'NO'
          tg-faturaveis:SENSITIVE = NO
          tg-imp-danfe:SENSITIVE = NO
          bt-todos:SENSITIVE = NO
          bt-nenhum:SENSITIVE = NO.

   IF l-fatura THEN DO.
      ASSIGN tg-faturaveis:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'
             tg-faturaveis:SENSITIVE = YES
             tg-imp-danfe:SENSITIVE = YES
             bt-todos:SENSITIVE = YES
             bt-nenhum:SENSITIVE = YES.
      APPLY 'value-changed' TO tg-faturaveis IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO.
      ASSIGN tg-restricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.
      APPLY 'value-changed' TO tg-restricao IN FRAME {&FRAME-NAME}.
   END.
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
  {src/adm/template/snd-list.i "tt-ped-venda"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-restricao w-livre 
FUNCTION fn-restricao RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
     DEF VAR c-restricao AS CHAR.

     CASE tt-ped-venda.restricao.
         WHEN 1  THEN ASSIGN c-restricao = "N∆o Aprovados".
         WHEN 2  THEN ASSIGN c-restricao = "∑ Vista".
         WHEN 3  THEN ASSIGN c-restricao = "Separaá∆o Incompleta".
         WHEN 4  THEN ASSIGN c-restricao = "Data Marcada".
         WHEN 5  THEN ASSIGN c-restricao = "Valor Max NF".
         WHEN 6  THEN ASSIGN c-restricao = "Valor Min NF".
         WHEN 7  THEN ASSIGN c-restricao = "Valor Min DUP".
         WHEN 8  THEN ASSIGN c-restricao = "Exportaá∆".
         WHEN 9  THEN ASSIGN c-restricao = "Bloqueio Faturamento".
         WHEN 10 THEN ASSIGN c-restricao = "Sem Qtde Volumes".
         WHEN 11 THEN ASSIGN c-restricao = "Inconsistente".

     END CASE.

  RETURN c-restricao.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

