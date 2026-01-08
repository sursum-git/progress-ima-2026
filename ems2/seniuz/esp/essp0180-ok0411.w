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

/* Parameters Definitions ---                                           */
DEF BUFFER cheque  FOR ems2ima.cheque.
DEF BUFFER empresa FOR mgadm.empresa.

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-clientes LIKE emitente
    FIELD repres           LIKE repres.nome-abrev
    FIELD tot-pend         AS DEC FORMAT ">>,>>>,>>9.99" 
    FIELD tot-aceitos      AS DEC FORMAT ">>,>>>,>>9.99" 
    FIELD tot-aprovados    AS DEC FORMAT ">>,>>>,>>9.99" 
    FIELD tit-vencidos     AS DEC FORMAT ">>,>>>,>>9.99" 
    FIELD tit-a-vencer     AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD cheques-compsar  AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD cheques-dev      AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD saldo            AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD tot-ped-ccred    AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD tot-ped-scred    AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD tot-ped-reav     AS DEC FORMAT ">>,>>>,>>9.99"
    FIELD restricao        AS LOG
    FIELD motivo           AS CHAR FORMAT "x(320)"
    FIELD desc-bloq-cr     LIKE ped-venda.desc-bloq-cr
    FIELD ordem            AS INT
    FIELD visualiza        AS LOG
    INDEX indice1 ordem repres nome-abrev.

DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD visualiza         AS LOG
    FIELD tipo-pedido       LIKE ped-venda-ext.tp-pedido
    FIELD vl-aberto         LIKE ped-venda.vl-tot-ped
    FIELD vl-total          LIKE ped-venda.vl-tot-ped
    FIELD usuario           AS CHAR
    FIELD marca             AS CHAR
    INDEX indice1 IS PRIMARY dt-entrega.

DEF TEMP-TABLE tt-titulos LIKE titulo
    FIELD visualiza AS LOG.

DEF TEMP-TABLE tt-cheques LIKE ems2ima.cheque
    FIELD visualiza AS LOG.

DEF BUFFER b-tt-ped-venda FOR tt-ped-venda.
DEF BUFFER b-tt-clientes  FOR tt-clientes.

DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR i-cod-emit AS INT NO-UNDO.
DEF NEW GLOBAL SHARED VAR v_rec_cliente AS RECID FORMAT ">>>>>>9":U NO-UNDO.
DEF NEW GLOBAL SHARED VAR v_cod_empres_usuar AS CHARACTER FORMAT "x(3)":U LABEL "Empresa" COLUMN-LABEL "Empresa" NO-UNDO.

DEF VAR rw-cliente            AS ROWID.
DEF VAR h-acomp               AS HANDLE.
DEF VAR h-query               AS HANDLE. 
DEF VAR c-dia                 AS CHAR.
DEF VAR da-dt-entrega-fin     AS DATE.
DEF VAR c-lotes               AS CHAR.
DEF VAR c-sit-ped             AS CHAR FORMAT "x(20)".
DEF VAR c-sit-cli             AS CHAR FORMAT "x(20)".
DEF VAR c-sit-aval            AS CHAR.
DEF VAR c-modalidade          AS CHAR FORMAT "x(20)".
DEF VAR c-historico           AS CHAR.
DEF VAR de-tot-ped            AS DEC.
DEF VAR de-tot-desc           AS DEC.
DEF VAR de-tot-res            AS DEC.
DEF VAR de-tot-aceitos        AS DEC.
DEF VAR de-tot-aprovados      AS DEC.
DEF VAR de-tot-pend           AS DEC.
DEF VAR de-tot-aprov          AS DEC.
DEF VAR de-tit-vencidos       AS DEC.
DEF VAR de-tit-a-vencer       AS DEC.
DEF VAR de-cheques-dev        AS DEC.
DEF VAR de-cheques-compsar    AS DEC.
DEF VAR c-nr-pedcli           LIKE ped-venda.nr-pedcli.
DEF VAR c-nome-abrev          LIKE emitente.nome-abrev.
DEF VAR c-texto-log           AS   CHAR FORMAT "x(100)".
DEF VAR i-cor-bg              AS   INT INIT 15.
DEF VAR i-cor-fg              AS   INT INIT 12.

DEF VAR i-lin                 AS INT INITIAL 99.
DEF VAR i-pag                 AS INT INITIAL  1.
DEF VAR c-mensagem            AS CHAR.
DEF VAR c-empresa             AS CHAR.
DEF VAR c-arq-email           AS CHAR FORMAT "x(45)".

DEF VAR i-sit-ped-ini   LIKE ped-venda.cod-sit-ped.
DEF VAR i-sit-ped-fin   LIKE ped-venda.cod-sit-ped.

/* Variaveis de Cancelamento de Pedidos */
DEF VAR c-motivo AS CHAR FORMAT "x(60)".
DEF VAR da-dt-trans AS DATE.
DEF VAR c-finalidade AS CHAR.
DEF VAR c-tipo-trans AS CHAR.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.

/* Aprovaá∆o/Reprovaá∆o de CrÇdito */
DEF VAR c-quem-aprovou LIKE ped-venda.quem-aprovou. 
DEF VAR da-data-aprov  LIKE ped-venda.dt-aprov.   
DEF VAR c-motivo-no    LIKE ped-venda.motivo.    
DEF VAR i-tp-aprov     AS   INT.

/* Global Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VAR gr-ped-venda  AS ROWID   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR gr-emitente   AS ROWID   NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR    NO-UNDO.

/* Variavies de ParÉmetros */
DEFINE VAR c-dt-limite-fin     AS CHAR.
DEFINE VAR c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli INIT "".
DEFINE VAR c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli INIT "ZZZZZZZZ".  
DEFINE VAR c-it-codigo-ini     LIKE ped-item-ext.it-codigo INIT "".                 
DEFINE VAR c-it-codigo-fin     LIKE ped-item-ext.it-codigo INIT "ZZZZZZZZZZZZZZZ".  
DEFINE VAR c-cod-refer-ini     LIKE ped-item-ext.cod-refer.
DEFINE VAR c-cod-refer-fin     LIKE ped-item-ext.cod-refer INIT "ZZZZZZZZZZ".   
DEFINE VAR c-matriz-ini        LIKE emitente.nome-matriz. 
DEFINE VAR c-matriz-fin        LIKE emitente.nome-matriz INIT "ZZZZZZZZZZZZ".
DEFINE VAR c-nome-abrev-ini    LIKE ped-venda.nome-abrev. 
DEFINE VAR c-nome-abrev-fin    LIKE ped-venda.nome-abrev INIT "ZZZZZZZZZZZZ".
DEFINE VAR c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE VAR c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri INIT 'ZZZZZZZZZZZZ'.
DEFINE VAR c-opc-artigo        AS CHAR INIT 'A'.
DEFINE VAR l-ok                AS LOG.

/* ***************************  Definitions  ************************** */
/* INICIO DA DEFINIÄ«O DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */

{esinc/espd0002.i}

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEFINE VAR raw-param   AS RAW NO-UNDO.
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
/* FIM DAS DEFINIÄÂES DO RELATORIO ESPD0002RP.P */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME br-clientes

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-clientes tt-ped-venda

/* Definitions for BROWSE br-clientes                                   */
&Scoped-define FIELDS-IN-QUERY-br-clientes tt-clientes.cod-emit tt-clientes.nome-emit tt-clientes.repres tt-clientes.lim-cred tt-clientes.tot-pend tt-clientes.tot-aceitos tt-clientes.tot-aprovados tt-clientes.tit-vencidos tt-clientes.tit-a-vencer tt-clientes.cheques-compsar tt-clientes.cheques-dev tt-clientes.saldo tt-clientes.portador fn-modalidade() @ c-modalidade fn-sit-cli() @ c-sit-cli tt-clientes.motivo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-clientes   
&Scoped-define SELF-NAME br-clientes
&Scoped-define QUERY-STRING-br-clientes FOR EACH tt-clientes WHERE                                  tt-clientes.visualiza = YES NO-LOCK                                  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-clientes OPEN QUERY {&SELF-NAME} FOR EACH tt-clientes WHERE                                  tt-clientes.visualiza = YES NO-LOCK                                  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-clientes tt-clientes
&Scoped-define FIRST-TABLE-IN-QUERY-br-clientes tt-clientes


/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.marca tt-ped-venda.nr-pedcli tt-ped-venda.dt-entrega tt-ped-venda.dt-implant fn-sit-ped() tt-ped-venda.tp-pedido fn-sit-cred() fn-cond-pagto() tt-ped-venda.vl-aberto tt-ped-venda.vl-desconto tt-ped-venda.vl-total   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define QUERY-STRING-br-pedidos FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND                                  tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND                                  tt-ped-venda.cod-sit-ped <= i-sit-ped-fin AND                                  LOOKUP(STRING(tt-ped-venda.cod-sit-aval), ~
      c-sit-aval) > 0                                  NO-LOCK
&Scoped-define OPEN-QUERY-br-pedidos OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND                                  tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND                                  tt-ped-venda.cod-sit-ped <= i-sit-ped-fin AND                                  LOOKUP(STRING(tt-ped-venda.cod-sit-aval), ~
      c-sit-aval) > 0                                  NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-venda


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-br-clientes}~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-desaprova RECT-10 RECT-54 rt-button ~
bt-param bt-param-2 bt-log bt-imprime bt-email rs-sit-credito br-clientes ~
bt-vapara bt-cons-cli bt-cons-global bt-historico bt-docs bt-sintegra ~
bt-receita-federal bt-serasa br-pedidos bt-marca bt-desmarca bt-todos ~
bt-nenhum bt-consulta bt-modifica bt-cancela bt-aprova fi-cgc 
&Scoped-Define DISPLAYED-OBJECTS rs-sit-credito fi-cgc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 bt-desaprova bt-marca bt-desmarca bt-todos bt-nenhum ~
bt-aprova 
&Scoped-define List-4 bt-email bt-serasa 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-cond-pagto w-livre 
FUNCTION fn-cond-pagto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-modalidade w-livre 
FUNCTION fn-modalidade RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-cli w-livre 
FUNCTION fn-sit-cli RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-cred w-livre 
FUNCTION fn-sit-cred RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-ped w-livre 
FUNCTION fn-sit-ped RETURNS CHARACTER
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
DEFINE BUTTON bt-aprova AUTO-GO 
     IMAGE-UP FILE "image/imt-aval.bmp":U
     LABEL "OK" 
     SIZE 10.57 BY 1.25 TOOLTIP "Aprova Pedidos".

DEFINE BUTTON bt-cancela AUTO-GO 
     IMAGE-UP FILE "image/im-cance.bmp":U
     LABEL "" 
     SIZE 4.86 BY 1.29 TOOLTIP "Cancelar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-cons-cli AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Detalhar Cliente"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-cons-global AUTO-GO 
     IMAGE-UP FILE "image/im-pcust.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "An†lise Global de CrÇdito"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-consulta AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Detalhar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-desaprova AUTO-GO 
     IMAGE-UP FILE "image/im-reprova-cred.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 10.57 BY 1.25 TOOLTIP "Desaprova CrÇdito dos Pedidos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-desmarca AUTO-GO 
     IMAGE-UP FILE "image/im-cllps.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-docs 
     IMAGE-UP FILE "image/im-docs.bmp":U
     LABEL "bt param 2" 
     SIZE 10.57 BY 1.71 TOOLTIP "Documentos Digitalizados do Cliente".

DEFINE BUTTON bt-email AUTO-GO 
     IMAGE-UP FILE "image/im-email.bmp":U
     LABEL "" 
     SIZE 4.14 BY 1.21 TOOLTIP "Envia Email para Representante"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-historico AUTO-GO 
     IMAGE-UP FILE "image/im-hist.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Hist¢rico do Cliente"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-pri.bmp":U
     LABEL "bt param 2" 
     SIZE 4.14 BY 1.21 TOOLTIP "Impress∆o".

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Alteraá‰es do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-marca AUTO-GO 
     IMAGE-UP FILE "image/im-expan.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-modifica AUTO-GO 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Modificar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nenhum AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca TODOS os Pedidos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-param 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "Button 3" 
     SIZE 4.14 BY 1.21 TOOLTIP "ParÉmetros".

DEFINE BUTTON bt-param-2 
     IMAGE-UP FILE "image/im-autom.bmp":U
     LABEL "bt param 2" 
     SIZE 4.14 BY 1.21 TOOLTIP "Atualizaá∆o de Dados na Tela".

DEFINE BUTTON bt-receita-federal AUTO-GO 
     IMAGE-UP FILE "image/img-receita-federal.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.71 TOOLTIP "Copia CNPJ para Receita Federal"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-serasa AUTO-GO 
     IMAGE-UP FILE "image/img-serasa.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.71 TOOLTIP "Consulta SERASA"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-sintegra AUTO-GO 
     IMAGE-UP FILE "image/img-sintegra.bmp":U
     LABEL "" 
     SIZE 10.57 BY 1.71 TOOLTIP "Copia CNPJ para Sintegra"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-todos AUTO-GO 
     IMAGE-UP FILE "image\im-ran_a.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca TODOS os Pedidos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY 1.21 TOOLTIP "Posicionar no Cliente"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-cgc AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 2 BY .67
     BGCOLOR 7 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE rs-sit-credito AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Com CrÇdito", 1,
"Sem CrÇdito", 2,
"Reavaliaá∆o", 3,
"Aprovados", 4,
"Todos", 5
     SIZE 70.86 BY 1
     FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 12.29 BY 8
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 12.43 BY 10.38
     BGCOLOR 8 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 107.43 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-clientes FOR 
      tt-clientes SCROLLING.

DEFINE QUERY br-pedidos FOR 
      tt-ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-clientes w-livre _FREEFORM
  QUERY br-clientes NO-LOCK DISPLAY
      tt-clientes.cod-emit            COLUMN-LABEL "CodCli"         WIDTH 7  COLUMN-FONT 6
      tt-clientes.nome-emit           COLUMN-LABEL "Nome"           WIDTH 30 COLUMN-FONT 6
      tt-clientes.repres              COLUMN-LABEL "Rep."           WIDTH 15
      tt-clientes.lim-cred            COLUMN-LABEL "Lim.Cred"       WIDTH 9  
      tt-clientes.tot-pend            COLUMN-LABEL "Pendentes"      WIDTH 12
      tt-clientes.tot-aceitos         COLUMN-LABEL "Reavaliar"      WIDTH 12
      tt-clientes.tot-aprovados       COLUMN-LABEL "Aprovados"      WIDTH 12
      tt-clientes.tit-vencidos        COLUMN-LABEL "Tit.Vencidos"   WIDTH 12
      tt-clientes.tit-a-vencer        COLUMN-LABEL "Tit.a Vencer"   WIDTH 12
      tt-clientes.cheques-compsar     COLUMN-LABEL "Ch.Compensar"   WIDTH 12
      tt-clientes.cheques-dev         COLUMN-LABEL "Ch.Devolvidos"  WIDTH 12
      tt-clientes.saldo               COLUMN-LABEL "Saldo"          WIDTH 9
      tt-clientes.portador            COLUMN-LABEL "Port"           WIDTH 5
      fn-modalidade() @ c-modalidade  COLUMN-LABEL "Modalidade"     WIDTH 15
      fn-sit-cli() @ c-sit-cli        COLUMN-LABEL "Sit Cliente"    WIDTH 15 
      tt-clientes.motivo              COLUMN-LABEL "Restriá∆o"      WIDTH 320
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 10.33
         FONT 1
         TITLE "Clientes".

DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos w-livre _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.marca           COLUMN-LABEL "M"              FORMAT "x(1)"       WIDTH 1.5 COLUMN-FONT 6
      tt-ped-venda.nr-pedcli       COLUMN-LABEL "Pedido"         FORMAT "x(6)"       WIDTH 06   
      tt-ped-venda.dt-entrega      COLUMN-LABEL "Dt.Entrega"     FORMAT "99/99/9999" WIDTH 09
      tt-ped-venda.dt-implant      COLUMN-LABEL "Dt.Implant"     FORMAT "99/99/9999" WIDTH 10
      fn-sit-ped()                 COLUMN-LABEL "Sit"                                WIDTH 03
      tt-ped-venda.tp-pedido       COLUMN-LABEL "Tipo Pedido"    FORMAT "x(15)"      WIDTH 12
      fn-sit-cred()                COLUMN-LABEL "Sit Cred"       FORMAT "x(15)"      WIDTH 10
      fn-cond-pagto()              COLUMN-LABEL "Cond Pagto"     FORMAT "x(20)"      WIDTH 15
      tt-ped-venda.vl-aberto       COLUMN-LABEL "Vlr Aberto"     FORMAT ">>>,>>9.99" WIDTH 09
      tt-ped-venda.vl-desconto     COLUMN-LABEL "Vlr Desconto"   FORMAT ">>>,>>9.99" WIDTH 10
      tt-ped-venda.vl-total        COLUMN-LABEL "Vlr Total"      FORMAT ">>>,>>9.99" WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 8
         FONT 1
         TITLE "Pedidos" ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-desaprova AT ROW 19.71 COL 98.14
     bt-param AT ROW 1.17 COL 74.72
     bt-param-2 AT ROW 1.17 COL 79.29 WIDGET-ID 4
     bt-log AT ROW 17.08 COL 103.57
     bt-imprime AT ROW 1.17 COL 84
     bt-email AT ROW 1.17 COL 88.43
     rs-sit-credito AT ROW 1.25 COL 3.14 NO-LABEL
     br-clientes AT ROW 2.67 COL 2
     bt-vapara AT ROW 2.83 COL 98
     bt-cons-cli AT ROW 2.83 COL 103.57
     bt-cons-global AT ROW 4.13 COL 98
     bt-historico AT ROW 4.13 COL 103.57
     bt-docs AT ROW 5.54 COL 98 WIDGET-ID 2
     bt-sintegra AT ROW 7.42 COL 98
     bt-receita-federal AT ROW 9.25 COL 98
     bt-serasa AT ROW 11.13 COL 98
     br-pedidos AT ROW 13.13 COL 2
     bt-marca AT ROW 13.33 COL 98.14
     bt-desmarca AT ROW 13.33 COL 103.57
     bt-todos AT ROW 14.58 COL 98.14
     bt-nenhum AT ROW 14.58 COL 103.57
     bt-consulta AT ROW 15.83 COL 98.14
     bt-modifica AT ROW 15.83 COL 103.57
     bt-cancela AT ROW 17.08 COL 98.14 WIDGET-ID 6
     bt-aprova AT ROW 18.38 COL 98.14
     fi-cgc AT ROW 13.21 COL 91 COLON-ALIGNED NO-LABEL
     RECT-10 AT ROW 13.13 COL 97.14
     RECT-54 AT ROW 2.63 COL 97
     rt-button AT ROW 1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.14 BY 20.38
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
         TITLE              = "Crivo Finaneiro e Comercial de Pedidos"
         COLUMN             = 18.14
         ROW                = 6.54
         HEIGHT             = 20.38
         WIDTH              = 109.14
         MAX-HEIGHT         = 28.13
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.13
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
/* BROWSE-TAB br-clientes rs-sit-credito f-cad */
/* BROWSE-TAB br-pedidos bt-serasa f-cad */
/* SETTINGS FOR BUTTON bt-aprova IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR BUTTON bt-desaprova IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR BUTTON bt-desmarca IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR BUTTON bt-email IN FRAME f-cad
   4                                                                    */
/* SETTINGS FOR BUTTON bt-marca IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR BUTTON bt-nenhum IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR BUTTON bt-serasa IN FRAME f-cad
   4                                                                    */
/* SETTINGS FOR BUTTON bt-todos IN FRAME f-cad
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-clientes
/* Query rebuild information for BROWSE br-clientes
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-clientes WHERE
                                 tt-clientes.visualiza = YES NO-LOCK
                                 INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-clientes */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE
                                 tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
                                 tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND
                                 tt-ped-venda.cod-sit-ped <= i-sit-ped-fin AND
                                 LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) > 0
                                 NO-LOCK
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Crivo Finaneiro e Comercial de Pedidos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Crivo Finaneiro e Comercial de Pedidos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-clientes
&Scoped-define SELF-NAME br-clientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-clientes w-livre
ON ROW-DISPLAY OF br-clientes IN FRAME f-cad /* Clientes */
DO:
   IF tt-clientes.ordem = 999 THEN
      ASSIGN tt-clientes.nome-emit:FGCOLOR IN BROWSE br-clientes = 15
             tt-clientes.repres:FONT IN BROWSE br-clientes = 6
             tt-clientes.repres:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.repres:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.lim-cred:FGCOLOR IN BROWSE br-clientes = 15
             tt-clientes.tot-pend:FONT IN BROWSE br-clientes = 6
             tt-clientes.tot-pend:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.tot-pend:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.tot-aceitos:FONT IN BROWSE br-clientes = 6
             tt-clientes.tot-aceitos:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.tot-aceitos:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.tot-aprovados:FONT IN BROWSE br-clientes = 6
             tt-clientes.tot-aprovados:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.tot-aprovados:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.tit-vencidos:FONT IN BROWSE br-clientes = 6
             tt-clientes.tit-vencidos:BGCOLOR IN BROWSE br-clientes = i-cor-bg   
             tt-clientes.tit-vencidos:FGCOLOR IN BROWSE br-clientes = i-cor-fg   
             tt-clientes.tit-a-vencer:FONT IN BROWSE br-clientes = 6
             tt-clientes.tit-a-vencer:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.tit-a-vencer:FGCOLOR IN BROWSE br-clientes = i-cor-fg   
             tt-clientes.cheques-compsar:FONT IN BROWSE br-clientes = 6
             tt-clientes.cheques-compsar:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.cheques-compsar:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.cheques-dev:FONT IN BROWSE br-clientes = 6
             tt-clientes.cheques-dev:BGCOLOR IN BROWSE br-clientes = i-cor-bg
             tt-clientes.cheques-dev:FGCOLOR IN BROWSE br-clientes = i-cor-fg
             tt-clientes.saldo:FGCOLOR IN BROWSE br-clientes = 15 
             tt-clientes.portador:FGCOLOR IN BROWSE br-clientes = 15
             c-modalidade:FGCOLOR IN BROWSE br-clientes = 15
             c-sit-cli:FGCOLOR IN BROWSE br-clientes = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-clientes w-livre
ON VALUE-CHANGED OF br-clientes IN FRAME f-cad /* Clientes */
DO:
   IF AVAIL tt-clientes THEN DO.
      IF tt-clientes.ordem = 999 THEN 
         APPLY 'CURSOR-UP' TO SELF.

      FIND emitente WHERE
           emitente.nome-abrev = tt-clientes.nome-abrev NO-LOCK NO-ERROR.
      IF AVAIL emitente THEN
         ASSIGN gr-emitente = ROWID(emitente)
                rw-cliente = ROWID(tt-clientes).

      {&OPEN-QUERY-br-pedidos} 

      APPLY 'VALUE-CHANGED' TO br-pedidos.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON ROW-DISPLAY OF br-pedidos IN FRAME f-cad /* Pedidos */
DO:
   IF tt-ped-venda.dt-implant < TODAY THEN
      ASSIGN tt-ped-venda.dt-implant:FONT IN BROWSE br-pedidos = 6
             tt-ped-venda.dt-implant:BGCOLOR IN BROWSE br-pedidos = i-cor-bg
             tt-ped-venda.dt-implant:FGCOLOR IN BROWSE br-pedidos = i-cor-fg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-livre
ON VALUE-CHANGED OF br-pedidos IN FRAME f-cad /* Pedidos */
DO:
   ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
   IF INPUT FRAME {&FRAME-NAME} rs-sit-credito <> 5 THEN DO.
      FIND FIRST b-tt-ped-venda WHERE
                 b-tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
                 LOOKUP(STRING(tt-ped-venda.cod-sit-aval),"1,4") > 0 AND
                 b-tt-ped-venda.marca <> "" NO-ERROR.

       ASSIGN bt-aprova:SENSITIVE = AVAIL b-tt-ped-venda AND
                                    AVAIL usu-gr-cli AND
                                    usu-gr-cli.ind-funcao = 1.
    
       FIND FIRST b-tt-ped-venda WHERE
                  b-tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
                  //LOOKUP(STRING(tt-ped-venda.cod-sit-aval),"2,3") > 0 AND
                  b-tt-ped-venda.marca <> "" NO-ERROR.
       ASSIGN bt-desaprova:SENSITIVE = AVAIL b-tt-ped-venda AND
                                       AVAIL usu-gr-cli AND
                                       usu-gr-cli.ind-funcao = 1.
   END.
   ELSE
      DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.

   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
   IF AVAIL usu-gr-cli THEN
      ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-aprova
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-aprova w-livre
ON CHOOSE OF bt-aprova IN FRAME f-cad /* OK */
DO:
   RUN esp/essp0180d.w (OUTPUT i-tp-aprov,
                        OUTPUT l-ok).
   
   IF l-ok = YES THEN DO.
      FOR EACH b-tt-ped-venda WHERE
               b-tt-ped-venda.marca <> "".
    
          FIND ped-venda WHERE
               ped-venda.nome-abrev = b-tt-ped-venda.nome-abrev AND
               ped-venda.nr-pedcli = b-tt-ped-venda.nr-pedcli
               SHARE-LOCK NO-ERROR.

          FIND ped-venda-ext WHERE
               ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
               ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
          IF ped-venda-ext.l-nao-aprovar THEN DO.
             MESSAGE 'Pedido: ' ped-venda.nr-pedcli 
                     ' est† Bloqueado para Aprovaá∆o, Verifique com o Departamento de Vendas...'
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             NEXT.
          END.
        
          FIND CURRENT ped-venda EXCLUSIVE-LOCK.
          IF i-tp-aprov = 1 THEN DO.  /* Libera pedido com Aprovaá∆o de CrÇdito */
             ASSIGN ped-venda.desc-forc-cr = ""
                    ped-venda.desc-bloq-cr = ""
                    ped-venda.dt-apr-cred  = TODAY
                    ped-venda.cod-sit-aval = 3
                    ped-venda.quem-aprovou = c-seg-usuario.

             ASSIGN ped-venda.cod-message-alert = 0
                    ped-venda.dt-mensagem = ?
                    ped-venda.dsp-pre-fat = YES.
       
             FIND tt-clientes WHERE
                  tt-clientes.nome-abrev = b-tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.

             RUN esapi/cria-log-pedvenda.p (INPUT b-tt-ped-venda.nr-pedcli,
                                            INPUT b-tt-ped-venda.nome-abrev,
                                            INPUT tt-clientes.motivo,
                                            INPUT NO).

             PAUSE 2 NO-MESSAGE. /* Pausa para sair do mesmo segundo */
             IF tt-ped-venda.marca = "* CC" THEN
                ASSIGN c-texto-log = "Liberaá∆o de CrÇdito pelo Sistema (Crivo)".
             ELSE
                ASSIGN c-texto-log = "Liberaá∆o de CrÇdito pelo Usu†rio".
    
             RUN esapi/cria-log-pedvenda.p (INPUT b-tt-ped-venda.nr-pedcli,
                                            INPUT b-tt-ped-venda.nome-abrev,
                                            INPUT c-texto-log,
                                            INPUT NO).

             ASSIGN b-tt-ped-venda.cod-sit-aval = 3.
          END.
          ELSE DO.
              ASSIGN c-texto-log = "Liberado o Pedido sem aprovar o CrÇdito".
              RUN esapi/cria-log-pedvenda.p (INPUT b-tt-ped-venda.nr-pedcli,
                                             INPUT b-tt-ped-venda.nome-abrev,
                                             INPUT c-texto-log,
                                             INPUT NO).
          END.
          ASSIGN ped-venda.cod-sit-ped = IF ped-venda.cod-sit-ped = 4 
                                         THEN 1 ELSE ped-venda.cod-sit-ped
                 b-tt-ped-venda.cod-sit-ped = ped-venda.cod-sit-ped
                 b-tt-ped-venda.marca = "".

          FIND CURRENT ped-venda NO-LOCK.
      END.
   END.
   
   APPLY 'VALUE-CHANGED' TO rs-sit-credito IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela w-livre
ON CHOOSE OF bt-cancela IN FRAME f-cad
DO:
   RUN pi-ver-permissao.
   IF NOT l-ok THEN DO.
      MESSAGE "Usu†rio: " + c-seg-usuario + ", n∆o tem permiss∆o para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   FOR EACH b-tt-ped-venda WHERE
            b-tt-ped-venda.marca <> "".

       FIND ped-venda WHERE
            ped-venda.nr-pedcli = b-tt-ped-venda.nr-pedcli AND
            ped-venda.nome-abrev = b-tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.

       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND  /* daf  */
            ped-venda-ext.nr-pedido = INT(ped-venda.nr-pedcli)
            NO-ERROR.
       IF AVAIL ped-venda-ext AND
          ped-venda-ext.l-etiqueta THEN DO.   /* Pedido em Separaá∆o */
          MESSAGE "Pedido est† em Processo de Separaá∆o," SKIP
                  "Solicite ao Setor Respons†vel para Liber†-lo"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK
              TITLE "Cancelamento n∆o Permitido".
          NEXT.
       END.

       FIND FIRST ped-item-res WHERE
                  ped-item-res.nr-pedcli = b-tt-ped-venda.nr-pedcli AND
                  ped-item-res.nome-abrev = b-tt-ped-venda.nome-abrev AND
                  ped-item-res.faturado = NO
                  NO-LOCK NO-ERROR.
       IF AVAIL ped-item-res THEN DO.
          MESSAGE "Existem Peáas Reservadas para esse Pedido," SKIP
                  "Cancelamento s¢ ser† permitido se Cancelar TODAS as Reservas." SKIP(1)
                  "Deseja Cancelar as Reservas ?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  TITLE "" UPDATE l-opcao AS LOGICAL.
          IF l-opcao = NO THEN
             RETURN NO-APPLY.
       END.
  
       FIND ped-venda WHERE
            ped-venda.nr-pedcli = b-tt-ped-venda.nr-pedcli AND
            ped-venda.nome-abrev = b-tt-ped-venda.nome-abrev EXCLUSIVE-LOCK NO-ERROR.
    
       ASSIGN c-finalidade = 'Cancelamento'.
       RUN pdp/pd4000a.p (INPUT c-finalidade,
                          OUTPUT c-tipo-trans,
                          OUTPUT c-motivo,
                          OUTPUT da-dt-trans,
                          OUTPUT l-ok).
  
       IF l-ok THEN DO.
          RUN esapi/cancela-pedvenda.p (INPUT b-tt-ped-venda.nr-pedcli,
                                        INPUT c-motivo).
    
          IF RETURN-VALUE = 'ADM-ERROR' THEN
             RETURN 'ADM-ERROR'.
    
          RUN pi-cancela-reserva.
      
          FIND ped-venda WHERE
               ped-venda.nr-pedcli = b-tt-ped-venda.nr-pedcli AND
               ped-venda.nome-abrev = b-tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
          ASSIGN b-tt-ped-venda.cod-sit-ped = ped-venda.cod-sit-ped.
      
          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT "Pedido Cancelado. " + c-motivo,
                                         INPUT YES).
    
          FOR EACH ped-item OF ped-venda WHERE 
                   ped-item.cod-sit-item = 6 NO-LOCK.
              RUN esapi/cria-log-pedvenda.p (INPUT ped-item.nr-pedcli,
                                             INPUT ped-item.nome-abrev,
                                             INPUT TRIM(STRING(ped-item.nr-sequencia,">>>9")) + 
                                                   " Item: " + TRIM(ped-item.it-codigo) + " Refer: " + TRIM(ped-item.cod-refer) + 
                                                   " Qtde: " + TRIM(STRING(ped-item.qt-pedida,">>>,>>9.99")) + ": Cancelada",
                                             INPUT YES).
          END.
       END.

       ASSIGN b-tt-ped-venda.marca = "".

       APPLY 'VALUE-CHANGED' TO rs-sit-credito.
   END.

   {&OPEN-QUERY-br-pedidos}
   APPLY 'value-changed' TO br-pedidos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cons-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cons-cli w-livre
ON CHOOSE OF bt-cons-cli IN FRAME f-cad
DO:
   RUN cdp/cd1022.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cons-global
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cons-global w-livre
ON CHOOSE OF bt-cons-global IN FRAME f-cad
DO:
   ASSIGN w-livre:SENSITIVE = NO.
   RUN imp/impd030.w. 
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta w-livre
ON CHOOSE OF bt-consulta IN FRAME f-cad
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK NO-ERROR.
   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp\espd4000.w (INPUT "Consultar").
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desaprova
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desaprova w-livre
ON CHOOSE OF bt-desaprova IN FRAME f-cad
DO:
    ASSIGN c-quem-aprovou = c-seg-usuario
           da-data-aprov = TODAY
           c-motivo = ""
           c-motivo-no = "".

    RUN cmp/cm0201b.p (INPUT-OUTPUT c-quem-aprovou,
                       INPUT-OUTPUT da-data-aprov,
                       INPUT-OUTPUT c-motivo,
                       INPUT-OUTPUT c-motivo-no,
                       OUTPUT l-ok).

    IF l-ok THEN DO.
       FOR EACH b-tt-ped-venda WHERE
                b-tt-ped-venda.marca <> "".
    
           FIND ped-venda WHERE
                ped-venda.nome-abrev = b-tt-ped-venda.nome-abrev AND
                ped-venda.nr-pedcli = b-tt-ped-venda.nr-pedcli
                NO-ERROR.
    
           ASSIGN ped-venda.desc-bloq-cr = c-motivo-no
                  ped-venda.dsp-pre-fat = NO
                  ped-venda.cod-sit-aval = 4 /* simula uma suspens∆o */
                  ped-venda.quem-aprovou = c-quem-aprovou.
    
           ASSIGN b-tt-ped-venda.cod-sit-aval = 4
                  b-tt-ped-venda.marca = "".
    
           FIND CURRENT ped-venda NO-LOCK.

           /* Grava LOG */
           ASSIGN c-texto-log = "CrÇdito Reprovado pelo Usu†rio // " + ped-venda.desc-bloq-cr.
           RUN esapi/cria-log-pedvenda.p (INPUT b-tt-ped-venda.nr-pedcli,
                                          INPUT b-tt-ped-venda.nome-abrev,
                                          INPUT c-texto-log,
                                          INPUT NO).

       END.
    END.
    APPLY 'VALUE-CHANGED' TO rs-sit-credito IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca w-livre
ON CHOOSE OF bt-desmarca IN FRAME f-cad
DO:
  ASSIGN tt-ped-venda.marca = "".
  br-pedidos:REFRESH().
  APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-docs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-docs w-livre
ON CHOOSE OF bt-docs IN FRAME f-cad /* bt param 2 */
DO:
  ASSIGN w-livre:SENSITIVE = NO.
  RUN esapi/doctos-digitalizados.p.
  ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-email w-livre
ON CHOOSE OF bt-email IN FRAME f-cad
DO:
   ASSIGN w-livre:SENSITIVE = NO.
   RUN esp/essp0180f.p (INPUT TABLE tt-clientes,
                        INPUT TABLE tt-ped-venda,
                        INPUT i-sit-ped-ini,
                        INPUT i-sit-ped-fin,
                        INPUT c-sit-aval,
                        INPUT INT(rs-sit-credito:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
   ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-historico w-livre
ON CHOOSE OF bt-historico IN FRAME f-cad
DO:
   FIND FIRST emscad.cliente WHERE
              emscad.cliente.cod_empresa = v_cod_empres_usuar AND
              emscad.cliente.cdn_cliente = tt-clientes.cod-emit NO-LOCK NO-ERROR.

   IF AVAIL emscad.cliente THEN DO.
      ASSIGN v_rec_cliente = RECID(emscad.cliente).
    
      RUN prgfin/acr/acr205aa.p.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime w-livre
ON CHOOSE OF bt-imprime IN FRAME f-cad /* bt param 2 */
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


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-livre
ON CHOOSE OF bt-marca IN FRAME f-cad
DO:
   IF rs-sit-credito:INPUT-VALUE = 1 THEN
      ASSIGN tt-ped-venda.marca = "* CC".
   ELSE
      ASSIGN tt-ped-venda.marca = "* SC".

   APPLY 'value-changed' TO br-pedidos.
   br-pedidos:REFRESH().
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica w-livre
ON CHOOSE OF bt-modifica IN FRAME f-cad
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


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-livre
ON CHOOSE OF bt-nenhum IN FRAME f-cad
DO:
    FOR EACH tt-ped-venda WHERE                                            
             tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
             tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND
             tt-ped-venda.cod-sit-ped <= i-sit-ped-fin. 
        ASSIGN tt-ped-venda.marca = "".
    END.
    br-pedidos:REFRESH().
    APPLY 'value-changed' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-livre
ON CHOOSE OF bt-param IN FRAME f-cad /* Button 3 */
DO:
    EMPTY TEMP-TABLE tt-digita.
    ASSIGN w-livre:SENSITIVE = NO.
    RUN esp/essp0180a.w (INPUT-OUTPUT TABLE tt-digita,
                         INPUT-OUTPUT c-dt-limite-fin,
                         INPUT-OUTPUT c-nr-pedcli-ini,   
                         INPUT-OUTPUT c-nr-pedcli-fin,   
                         INPUT-OUTPUT c-it-codigo-ini,
                         INPUT-OUTPUT c-it-codigo-fin,   
                         INPUT-OUTPUT c-cod-refer-ini,
                         INPUT-OUTPUT c-cod-refer-fin,
                         INPUT-OUTPUT c-matriz-ini,
                         INPUT-OUTPUT c-matriz-fin,
                         INPUT-OUTPUT c-nome-abrev-ini,
                         INPUT-OUTPUT c-nome-abrev-fin,
                         INPUT-OUTPUT c-no-ab-reppri-ini,
                         INPUT-OUTPUT c-no-ab-reppri-fin,
                         INPUT-OUTPUT c-opc-artigo, 
                         INPUT-OUTPUT l-ok). 
    IF l-ok THEN                                     
       RUN pi-processa.

  ASSIGN w-livre:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param-2 w-livre
ON CHOOSE OF bt-param-2 IN FRAME f-cad /* bt param 2 */
DO:
   RUN pi-processa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-receita-federal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-receita-federal w-livre
ON CHOOSE OF bt-receita-federal IN FRAME f-cad
DO:
    IF AVAIL emitente THEN DO.
       ASSIGN fi-cgc:SCREEN-VALUE = emitente.cgc.
       fi-cgc:SET-SELECTION(1,50). 
       fi-cgc:EDIT-COPY(). 

       RUN esapi/Open-Firefox.p (INPUT "http://www.receita.fazenda.gov.br/PessoaJuridica/CNPJ/cnpjreva/Cnpjreva_Solicitacao.asp").
/*       RUN pi-open-ie (INPUT "http://www.receita.fazenda.gov.br/PessoaJuridica/CNPJ/cnpjreva/Cnpjreva_Solicitacao.asp"). */

       /*        w-livre:SENSITIVE = NO.                                                */
       /*        ASSIGN c-historico = "COMPOSIÄ«O EMPRESARIAL" + CHR(13) +              */
       /*                             "=========================" + CHR(13) + CHR(13) + */
       /*                             "FUNDAÄ«O:" + CHR(13) + CHR(13) +                 */
       /*                             "DATA:" + CHR(13) + CHR(13) +                     */
       /*                             "CAPITAL SOCIAL:" + CHR(13) + CHR(13) +           */
       /*                             "S‡CIO:" + CHR(13) + CHR(13) +                    */
       /*                             "  CPF:" + CHR(13) + CHR(13) +                    */
       /*                             "FONTES COMERCIAIS:" + CHR(13) +                  */
       /*                             "INFORMAÄÂES COMERCIAIS:" + CHR(13) +             */
       /*                             "CLIENTE DESDE:" + CHR(13) + CHR(13) +            */
       /*                             "ULTIMA FATURA:" + CHR(13) + CHR(13) +            */
       /*                             "MAIOR FATURA:" + CHR(13) + CHR(13) +             */
       /*                             "MAIOR ACUMULO:" + CHR(13) + CHR(13) +            */
       /*                             "DEBITO ∑ VENCER:" + CHR(13) + CHR(13) +          */
       /*                             "DEBITO VENCIDO:" + CHR(13) + CHR(13) +           */
       /*                             "LIMITE DE CRêDITO:" + CHR(13) + CHR(13) +        */
       /*                             "PAGAMENTOS:" + CHR(13) + CHR(13) +               */
       /*                             "DUPLICATAS:" + CHR(13) + CHR(13) +               */
       /*                             "CHEQUES:" + CHR(13) + CHR(13) +                  */
       /*                             "OBSERVAÄÂES:".                                   */
       /*                                                                               */
       /*        RUN esp/essp0180e.p (INPUT emitente.cod-emit, INPUT c-historico).      */
       /*        w-livre:SENSITIVE = YES.                                               */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-serasa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-serasa w-livre
ON CHOOSE OF bt-serasa IN FRAME f-cad
DO:
   IF AVAIL emitente THEN DO.
      ASSIGN fi-cgc:SCREEN-VALUE = emitente.cgc.
      fi-cgc:SET-SELECTION(1,50). 
      fi-cgc:EDIT-COPY(). 
      
      RUN esapi/Open-Firefox.p (INPUT "http://www.serasa.com.br/").
/*      RUN pi-open-ie (INPUT "http://www.serasa.com.br/"). */

      /*       w-livre:SENSITIVE = NO.                                                   */
      /*       ASSIGN c-historico = "SERASA RELATO:" + CHR(13) + CHR(13) +               */
      /*                            "CREDIT RISKSCORING:" + CHR(13) + CHR(13) +          */
      /*                            "PROBABILIDADE INADIMPL“NCIA:" + CHR(13) + CHR(13) + */
      /*                            "FUNDAÄ«O:" + CHR(13) + CHR(13) +                    */
      /*                            "CAPITAL SOCIAL:" + CHR(13) + CHR(13) +              */
      /*                            "S‡CIO:" + CHR(13) + CHR(13) +                       */
      /*                            "  NADA CONSTA:" + CHR(13) + CHR(13) +               */
      /*                            "  CPF:" + CHR(13) + CHR(13) +                       */
      /*                            "  PERCENTUAL:" + CHR(13) + CHR(13) +                */
      /*                            "  CAPITAL:" + CHR(13) + CHR(13) +                   */
      /*                            "CONSULTAS" + CHR(13) +                              */
      /*                            "==========" + CHR(13) + CHR(13) +                   */
      /*                            "  ULTIMA COMPRA:" + CHR(13) + CHR(13) +             */
      /*                            "  MAIOR FATURA:" + CHR(13) + CHR(13) +              */
      /*                            "  MAIOR ACUMULO:" + CHR(13) + CHR(13) +             */
      /*                            "  COMPROMISSOS NO MERCADO:" + CHR(13) + CHR(13) +   */
      /*                            "PAGAMENTOS" + CHR(13) +                             */
      /*                            "============" + CHR(13) + CHR(13) +                 */
      /*                            "  DATA:" + CHR(13) + CHR(13) +                      */
      /*                            "  PONTUALIDADE:" + CHR(13) + CHR(13) +              */
      /*                            "OBSERVAÄÂES:".                                      */
      /*                                                                                 */
      /*       RUN esp/essp0180e.p (INPUT emitente.cod-emit, INPUT c-historico).         */
      /*       w-livre:SENSITIVE = YES.                                                  */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sintegra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sintegra w-livre
ON CHOOSE OF bt-sintegra IN FRAME f-cad
DO:
   IF AVAIL emitente THEN DO.
      ASSIGN fi-cgc:SCREEN-VALUE = emitente.cgc.
      fi-cgc:SET-SELECTION(1,50). 
      fi-cgc:EDIT-COPY(). 

      RUN esapi/sintegra.p (INPUT emitente.estado).

      /*       w-livre:SENSITIVE = NO.                                           */
      /*       ASSIGN c-historico = "SINTEGRA:".                                 */
      /*       RUN esp/essp0180e.p (INPUT emitente.cod-emit, INPUT c-historico). */
      /*       w-livre:SENSITIVE = YES.                                          */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-livre
ON CHOOSE OF bt-todos IN FRAME f-cad
DO:
   FOR EACH tt-ped-venda WHERE                                            
            tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
            tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND
            tt-ped-venda.cod-sit-ped <= i-sit-ped-fin. 

       IF rs-sit-credito:INPUT-VALUE = 1 THEN
          ASSIGN tt-ped-venda.marca = "* CC".
       ELSE
          ASSIGN tt-ped-venda.marca = "* SC".
   END.
   br-pedidos:REFRESH().
   APPLY 'value-changed' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara w-livre
ON CHOOSE OF bt-vapara IN FRAME f-cad
DO:
   RUN esdlg/d01essp0180.w (OUTPUT c-nome-abrev).

   IF c-nome-abrev <> "" THEN DO:
      FIND FIRST tt-clientes WHERE
                 SUBSTR(tt-clientes.nome-abrev,1,LENGTH(c-nome-abrev)) = c-nome-abrev AND
                 tt-clientes.visualiza = YES
                 NO-LOCK NO-ERROR. 

      IF NOT AVAIL tt-clientes THEN DO.
         FIND tt-clientes WHERE
              tt-clientes.cod-emitente = INTEGER(c-nome-abrev) AND
              tt-clientes.visualiza = YES
              NO-LOCK NO-ERROR.

         IF NOT AVAIL tt-clientes THEN DO:
            MESSAGE "Cliente n∆o est† contido na seleá∆o!"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
         END.
      END.

      h-query:REPOSITION-TO-ROWID(ROWID(tt-clientes)) NO-ERROR. 
      APPLY 'VALUE-CHANGED' TO br-clientes.
   END.
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


&Scoped-define SELF-NAME rs-sit-credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-sit-credito w-livre
ON VALUE-CHANGED OF rs-sit-credito IN FRAME f-cad
DO:
   ASSIGN de-tot-pend = 0            de-tot-aprovados = 0
          de-tot-aceitos = 0         de-tit-vencidos = 0
          de-tit-a-vencer = 0        de-cheques-dev = 0
          de-cheques-compsar = 0.             

   bt-desaprova:LOAD-IMAGE("image/im-reprova-cred.bmp").

   CASE SELF:SCREEN-VALUE:
       WHEN '1' THEN DO.

           ASSIGN i-sit-ped-ini = 1
                  i-sit-ped-fin = 4
                  c-sit-aval = "1".
           FOR EACH tt-clientes WHERE
                    tt-clientes.ordem <> 9999.
               ASSIGN tt-clientes.visualiza = NO.
               IF tt-clientes.saldo > 0 AND
                  tt-clientes.restricao = NO AND
                  CAN-FIND (FIRST tt-ped-venda WHERE
                                  tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
                                  tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND
                                  tt-ped-venda.cod-sit-ped <= i-sit-ped-fin AND
                                  LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) <> 0) THEN
                  ASSIGN tt-clientes.visualiza = YES
                         de-tot-pend = de-tot-pend + tt-clientes.tot-pend
                         de-tot-aceitos = de-tot-aceitos + tt-clientes.tot-aceitos
                         de-tot-aprovados = de-tot-aprovados + tt-clientes.tot-aprovados
                         de-tit-vencidos = de-tit-vencidos + tt-clientes.tit-vencidos
                         de-tit-a-vencer = de-tit-a-vencer + tt-clientes.tit-a-vencer
                         de-cheques-dev = de-cheques-dev + tt-clientes.cheques-dev
                         de-cheques-compsar = de-cheques-compsar + tt-clientes.cheques-compsar.
           END.
       END.
       WHEN '2' THEN DO.
           ASSIGN i-sit-ped-ini = 1
                  i-sit-ped-fin = 4
                  c-sit-aval = "1".
           FOR EACH tt-clientes WHERE
                    tt-clientes.ordem <> 999.
               ASSIGN tt-clientes.visualiza = NO.
               IF (tt-clientes.saldo < 0 OR
                  tt-clientes.restricao = YES) AND
                  CAN-FIND (FIRST tt-ped-venda WHERE
                                  tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
                                  tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND
                                  tt-ped-venda.cod-sit-ped <= i-sit-ped-fin AND
                                  LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) <> 0) THEN
                  ASSIGN tt-clientes.visualiza = YES
                         de-tot-pend = de-tot-pend + tt-clientes.tot-pend
                         de-tot-aceitos = de-tot-aceitos + tt-clientes.tot-aceitos
                         de-tot-aprovados = de-tot-aprovados + tt-clientes.tot-aprovados
                         de-tit-vencidos = de-tit-vencidos + tt-clientes.tit-vencidos
                         de-tit-a-vencer = de-tit-a-vencer + tt-clientes.tit-a-vencer
                         de-cheques-dev = de-cheques-dev + tt-clientes.cheques-dev
                         de-cheques-compsar = de-cheques-compsar + tt-clientes.cheques-compsar.
           END.
       END.
       WHEN '3' THEN DO.
           ASSIGN i-sit-ped-ini = 1
                  i-sit-ped-fin = 2
                  c-sit-aval = "4".
           FOR EACH tt-clientes WHERE 
                    tt-clientes.ordem <> 999.
               ASSIGN tt-clientes.visualiza = NO.
               IF CAN-FIND (FIRST tt-ped-venda WHERE
                                  tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
                                  tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND
                                  tt-ped-venda.cod-sit-ped <= i-sit-ped-fin AND
                                  LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) <> 0) THEN
                  ASSIGN tt-clientes.visualiza = YES
                         de-tot-pend = de-tot-pend + tt-clientes.tot-pend
                         de-tot-aceitos = de-tot-aceitos + tt-clientes.tot-aceitos
                         de-tot-aprovados = de-tot-aprovados + tt-clientes.tot-aprovados
                         de-tit-vencidos = de-tit-vencidos + tt-clientes.tit-vencidos
                         de-tit-a-vencer = de-tit-a-vencer + tt-clientes.tit-a-vencer
                         de-cheques-dev = de-cheques-dev + tt-clientes.cheques-dev
                         de-cheques-compsar = de-cheques-compsar + tt-clientes.cheques-compsar.
           END.
       END.
       WHEN '4' THEN DO.
           bt-desaprova:LOAD-IMAGE("image/im-can-cred.bmp").

           ASSIGN i-sit-ped-ini = 1
                  i-sit-ped-fin = 2
                  c-sit-aval = "2,3".
           FOR EACH tt-clientes WHERE 
                    tt-clientes.ordem <> 999.
               ASSIGN tt-clientes.visualiza = NO.
               IF CAN-FIND (FIRST tt-ped-venda WHERE
                                  tt-ped-venda.nome-abrev = tt-clientes.nome-abrev AND
                                  tt-ped-venda.cod-sit-ped >= i-sit-ped-ini AND
                                  tt-ped-venda.cod-sit-ped <= i-sit-ped-fin AND
                                  LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) <> 0) THEN
                  ASSIGN tt-clientes.visualiza = YES
                         de-tot-pend = de-tot-pend + tt-clientes.tot-pend
                         de-tot-aceitos = de-tot-aceitos + tt-clientes.tot-aceitos
                         de-tot-aprovados = de-tot-aprovados + tt-clientes.tot-aprovados
                         de-tit-vencidos = de-tit-vencidos + tt-clientes.tit-vencidos
                         de-tit-a-vencer = de-tit-a-vencer + tt-clientes.tit-a-vencer
                         de-cheques-dev = de-cheques-dev + tt-clientes.cheques-dev
                         de-cheques-compsar = de-cheques-compsar + tt-clientes.cheques-compsar.
           END.
       END.
       WHEN '5' THEN DO.
           ASSIGN i-sit-ped-ini = 1
                  i-sit-ped-fin = 4
                  c-sit-aval = "1,2,3,4".
           FOR EACH tt-clientes WHERE
                    tt-clientes.ordem <> 999.
               ASSIGN tt-clientes.visualiza = YES
                      de-tot-pend = de-tot-pend + tt-clientes.tot-pend
                      de-tot-aceitos = de-tot-aceitos + tt-clientes.tot-aceitos
                      de-tot-aprovados = de-tot-aprovados + tt-clientes.tot-aprovados
                      de-tit-vencidos = de-tit-vencidos + tt-clientes.tit-vencidos
                      de-tit-a-vencer = de-tit-a-vencer + tt-clientes.tit-a-vencer
                      de-cheques-dev = de-cheques-dev + tt-clientes.cheques-dev
                      de-cheques-compsar = de-cheques-compsar + tt-clientes.cheques-compsar.
           END.
       END.
   END CASE.

   FIND tt-clientes WHERE
        tt-clientes.ordem = 999 NO-ERROR.
   IF NOT AVAIL tt-clientes THEN DO.
      CREATE tt-clientes.
      ASSIGN tt-clientes.ordem = 999.
   END.
   ASSIGN tt-clientes.repres    = 'T O T A I S'
          tt-clientes.visualiza = YES
          tt-clientes.tot-pend = de-tot-pend
          tt-clientes.tot-aceitos = de-tot-aceitos
          tt-clientes.tot-aprovados = de-tot-aprovados
          tt-clientes.tit-vencidos = de-tit-vencidos
          tt-clientes.tit-a-vencer = de-tit-a-vencer
          tt-clientes.cheques-dev = de-cheques-dev
          tt-clientes.cheques-compsar = de-cheques-compsar.

   {&OPEN-QUERY-br-clientes} 

   /* Reposiciona no ultimo cliente ou no Cliente mais pr¢ximo */
   IF rw-cliente <> ? THEN DO.
      FIND tt-clientes WHERE
           ROWID(tt-clientes) = rw-cliente NO-LOCK NO-ERROR.
      IF tt-clientes.visualiza = NO THEN DO.
         REPEAT.
            FIND NEXT tt-clientes NO-ERROR.
            IF NOT AVAIL tt-clientes THEN DO.
               FIND FIRST tt-clientes NO-ERROR.
               IF NOT AVAIL tt-clientes THEN DO.
                  ASSIGN rw-cliente = ?.
                  LEAVE.
               END.
            END.
            IF tt-clientes.visualiza = YES THEN DO.
               ASSIGN rw-cliente = ROWID(tt-clientes).  
               LEAVE.
            END.
         END.
      END.
      IF rw-cliente <> ? THEN
         br-clientes:QUERY:REPOSITION-TO-ROWID(rw-cliente).
   END.
   APPLY 'ENTRY' TO br-clientes IN FRAME {&FRAME-NAME}.

   APPLY 'value-changed' TO br-clientes IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-clientes
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */
br-clientes:NUM-LOCKED-COLUMNS = 2.

ASSIGN h-query  = br-clientes:QUERY.

ASSIGN c-dt-limite-fin = STRING(MONTH(TODAY),"99") + STRING(YEAR(TODAY),"9999").

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

STATUS INPUT OFF. /* Desliga Mensagem no RodapÇ da Tela */

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
       RUN set-position IN h_p-exihel ( 1.13 , 92.86 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-param:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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
  DISPLAY rs-sit-credito fi-cgc 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE bt-desaprova RECT-10 RECT-54 rt-button bt-param bt-param-2 bt-log 
         bt-imprime bt-email rs-sit-credito br-clientes bt-vapara bt-cons-cli 
         bt-cons-global bt-historico bt-docs bt-sintegra bt-receita-federal 
         bt-serasa br-pedidos bt-marca bt-desmarca bt-todos bt-nenhum 
         bt-consulta bt-modifica bt-cancela bt-aprova fi-cgc 
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

  {utp/ut9000.i "ESSP0180" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  run pi-after-initialize.

  FIND usu-gr-cli WHERE
       usu-gr-cli.cod-usuario = c-seg-usuario NO-LOCK NO-ERROR.


  APPLY 'choose' TO bt-param.

  APPLY 'entry' TO br-clientes.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cancela-reserva w-livre 
PROCEDURE pi-cancela-reserva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ped-item-res WHERE
             ped-item-res.cod-estabel = tt-ped-venda.cod-estabel AND
             ped-item-res.nr-pedcli = tt-ped-venda.nr-pedcli AND
             ped-item-res.nome-abrev = tt-ped-venda.nome-abrev AND
             ped-item-res.faturado = NO
             SHARE-LOCK.
        DELETE ped-item-res.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cheques w-livre 
PROCEDURE pi-cheques :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i Selecionando_Cheques *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    FOR EACH tt-clientes EXCLUSIVE-LOCK.
        FOR EACH titulo WHERE 
                 titulo.cod-emit = tt-clientes.cod-emit AND
                 titulo.cod-esp = 'CQ' AND
                 titulo.vl-saldo <> 0 NO-LOCK USE-INDEX emitente.

            RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + titulo.nome-abrev).

            IF titulo.dt-vencimen < TODAY THEN 
               ASSIGN tt-clientes.cheques-dev = tt-clientes.cheques-dev + titulo.vl-saldo.
        END.
    END.

    FOR EACH cheque WHERE 
             cheque.devolvido = NO AND 
             cheque.situacao-cheque = 1 NO-LOCK.    /* Sit 1 = Pendente */

        FIND tt-clientes WHERE
             tt-clientes.cod-emit = cheque.cod-emit NO-ERROR.
        IF NOT AVAIL tt-clientes THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + STRING(cheque.cod-emit)).

        IF cheque.origem-cheque <> 6 /*CR*/ THEN NEXT. 

        ASSIGN tt-clientes.cheques-compsar = tt-clientes.cheques-compsar + cheque.vl-cheque.

        CREATE tt-cheques.
        BUFFER-COPY cheque TO tt-cheques.
    END.
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
    PAGE.
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  72
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  78
        "HORA: "                                  AT 104
        STRING(TIME,"hh:mm:ss")                   AT 110
        "PAG:"                                    AT 147
        i-pag FORMAT ">>>"                        AT 152
        SKIP(1).

    PUT "CRIVO FINANCEIRO E COMERCIAL DE PEDIDOS  -" AT 61. 
    CASE rs-sit-credito:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
        WHEN '1' THEN 
            PUT "COM CRêDITO" AT 104 SKIP(1). 
        WHEN '2' THEN 
            PUT "SEM CRêDITO" AT 104 SKIP(1).
        WHEN '3' THEN 
            PUT "REAVALIAÄ«O" AT 104 SKIP(1). 
        WHEN '4' THEN 
            PUT "APROVADOS"   AT 104 SKIP(1). 
        WHEN '5' THEN 
            PUT "TODOS"       AT 104 SKIP(1). 
    END CASE.

    PUT "Cliente             Representante    L.Credito   Pendentes   Reavaliar   Aprovados  T.Vencidos Tit  Vencer Ch Compensar    Ch Devol        Saldo Port  Modalid" AT 1.
    PUT "------------------- -------------  ----------- ----------- ----------- ----------- ----------- ----------- ------------ ----------- ------------ ---- --------" AT 1.

    /*
    PUT "Cliente      Representante    L.Credito   Pendentes   Reavaliar   Aprovados  T.Vencidos Tit  Vencer Ch Compensar    Ch Devol        Saldo Port  Modalid Sit" AT 1.
    PUT "------------ -------------  ----------- ----------- ----------- ----------- ----------- ----------- ------------ ----------- ------------ ---- -------- ---" AT 1.
    */
    ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec-ped w-livre 
PROCEDURE pi-imp-cabec-ped :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

PUT "Pedido Dt.Entrega Dt.Implant Sit Tipo Pedido Sit Credito Condiá∆o de Pagto  Valor Aberto Vlr.Reservado" AT 27 SKIP.
/* PUT "----- ---------- ---------- --- ----------- ----------- ----------------- ------------- ------------- " AT 27. */
 ASSIGN i-lin = i-lin + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cliente w-livre 
PROCEDURE pi-imp-cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF b-tt-clientes.modalidade <> 0 THEN DO.
    {esinc/i-dsrb.i b-tt-clientes.modalidade b-tt-clientes.modalidade c-modalidade}.
 END.
 ELSE
    ASSIGN c-modalidade = "".

 IF tt-clientes.ind-sit-emitente <> 0 THEN DO.
    {esinc/i-dsrb.i tt-clientes.ind-sit-emitente tt-clientes.ind-sit-emitente c-sit-cli}.
 END.
 ELSE
    ASSIGN c-sit-cli = "".

 IF i-lin >= 71 THEN DO:
    RUN pi-imp-cabec.
    ASSIGN i-lin = 7.
 END.

 PUT "" AT 1 SKIP.
 ASSIGN i-lin = i-lin + 1.

 PUT STRING(b-tt-clientes.cod-emitente, ">>>>>9") + "/" +
     b-tt-clientes.nome-abrev      FORMAT "x(18)"        AT   1  
     b-tt-clientes.repres                                AT  21
     b-tt-clientes.lim-cred        FORMAT ">>>>,>>9.99"  AT  36
     b-tt-clientes.tot-pend        FORMAT ">>>>,>>9.99"  AT  48
     b-tt-clientes.tot-aceitos     FORMAT ">>>>,>>9.99"  AT  60
     b-tt-clientes.tot-aprovados   FORMAT ">>>>,>>9.99"  AT  72
     b-tt-clientes.tit-vencidos    FORMAT ">>>>,>>9.99"  AT  84
     b-tt-clientes.tit-a-vencer    FORMAT ">>>>,>>9.99"  AT  96
     b-tt-clientes.cheques-compsar FORMAT ">>>>,>>9.99"  AT 109
     b-tt-clientes.cheques-dev     FORMAT ">>>>,>>9.99"  AT 121
     b-tt-clientes.saldo           FORMAT "->>>>,>>9.99" AT 133
     b-tt-clientes.portador        FORMAT ">>9"          AT 147 
     c-modalidade                  FORMAT "x(8)"         AT 151.
 ASSIGN i-lin = i-lin + 1.
 PUT SUBSTR(b-tt-clientes.motivo,1,155) FORMAT "x(155)"  AT   1.
 ASSIGN i-lin = i-lin + 1.
 PUT "" AT 1 SKIP.
 ASSIGN i-lin = i-lin + 1.


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
 DEF VAR h-prog          AS HANDLE NO-UNDO.
 DEF VAR i-ct            AS INT.
 DEF VAR l-imp-cabec-ped AS LOG.

 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 RUN esapi/saida-imp.p (OUTPUT i-saida,
                        OUTPUT c-saida,
                        OUTPUT i-num-copias,
                        OUTPUT l-ok).

 CASE i-saida:
     WHEN 1 THEN DO.
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 71.
         PUT CONTROL "~033E~033(s20H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0180.tmp".
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     END.
 END CASE.

 ASSIGN i-lin = 99
        i-pag =  1.

 DO i-ct = 1 TO i-num-copias.
    FOR EACH b-tt-clientes WHERE                              
             b-tt-clientes.visualiza = YES NO-LOCK
        BY b-tt-clientes.ordem                           
        BY b-tt-clientes.repres                          
        BY b-tt-clientes.nome-abrev.  

        /* Rotina CLIENTES */
        IF i-lin > 71 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        IF b-tt-clientes.repres = "T O T A I S" THEN DO:
           PUT b-tt-clientes.repres                                AT  14
               b-tt-clientes.tot-pend        FORMAT ">>>>,>>9.99"  AT  41
               b-tt-clientes.tot-aceitos     FORMAT ">>>>,>>9.99"  AT  53
               b-tt-clientes.tot-aprovados   FORMAT ">>>>,>>9.99"  AT  65
               b-tt-clientes.tit-vencidos    FORMAT ">>>>,>>9.99"  AT  77
               b-tt-clientes.tit-a-vencer    FORMAT ">>>>,>>9.99"  AT  89
               b-tt-clientes.cheques-compsar FORMAT ">>>>,>>9.99"  AT 102
               b-tt-clientes.cheques-dev     FORMAT ">>>>,>>9.99"  AT 114 SKIP.
           ASSIGN i-lin = i-lin + 1.

        END.
        ELSE DO:
           RUN pi-imp-cliente.
           ASSIGN i-lin = i-lin + 1.
        END.
        
        /* Rotina PEDIDOS */
        ASSIGN l-imp-cabec-ped = YES.
        FOR EACH tt-ped-venda WHERE                                        
                 tt-ped-venda.nome-abrev   = b-tt-clientes.nome-abrev     AND      
                 tt-ped-venda.cod-sit-ped >= i-sit-ped-ini                AND             
                 tt-ped-venda.cod-sit-ped <= i-sit-ped-fin                AND             
                 LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) > 0 NO-LOCK.

            IF i-lin > 68 THEN DO:
               RUN pi-imp-cabec.
               ASSIGN i-lin = 7.
               RUN pi-imp-cliente.
               ASSIGN i-lin = i-lin + 1.
               RUN pi-imp-cabec-ped.
            END.

            IF l-imp-cabec-ped THEN DO:
               IF i-lin <> 13 THEN
                  RUN pi-imp-cabec-ped.
               ASSIGN l-imp-cabec-ped = NO.
            END.

            PUT tt-ped-venda.nr-pedcli      FORMAT "x(6)"           AT  27
                tt-ped-venda.dt-entrega                             AT  34
                tt-ped-venda.dt-implant                             AT  45
                fn-sit-ped()                FORMAT "x(3)"           AT  56
                tt-ped-venda.tp-pedido      FORMAT "x(10)"          AT  60
                fn-sit-cred()               FORMAT "x(10)"          AT  72
                fn-cond-pagto()             FORMAT "x(16)"          AT  84
                tt-ped-venda.vl-aberto      FORMAT ">>,>>>,>>9.99"  AT 102
                tt-ped-venda.vl-desconto    FORMAT ">>,>>>,>>9.99"  AT 116 SKIP.
            ASSIGN i-lin = i-lin + 1.
        END.
        IF l-imp-cabec-ped = NO THEN DO:
           PUT FILL("-", 155) FORMAT "x(155)" AT 1 SKIP.
           ASSIGN i-lin = i-lin + 1.
        END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-open-ie w-livre 
PROCEDURE pi-open-ie :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-site AS CHAR FORMAT "x(100)".

    DEF VAR c-arq-java AS CHAR.
    DEF VAR c-comando AS CHAR.                                

    ASSIGN c-arq-java = SESSION:TEMP-DIRECTORY + "abrir.js".

    OUTPUT TO VALUE(c-arq-java).
       PUT 'var oIE = new ActiveXObject("InternetExplorer.Application");' SKIP
           'oIE.Navigate2("' + p-site + '");' FORMAT "x(150)" SKIP     
           'oIE.Visible = true;' SKIP.
    OUTPUT CLOSE.

    ASSIGN c-comando = 'wscript.exe ' + c-arq-java.
  
    OS-COMMAND SILENT VALUE(c-comando).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-pedidos w-livre 
PROCEDURE pi-pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i Selecionando_Pedidos *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    DEF INPUT PARAMETER p-situacao AS INT.

    FOR EACH ped-venda WHERE
             ped-venda.cod-sit-ped = p-situacao AND 
             ped-venda.completo NO-LOCK.

        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-venda.nr-pedcli).

        IF ped-venda.nome-abrev   < c-nome-abrev-ini   OR
           ped-venda.nome-abrev   > c-nome-abrev-fin   OR
           ped-venda.nr-pedcli    < c-nr-pedcli-ini    OR
           ped-venda.nr-pedcli    > c-nr-pedcli-fin    OR
           ped-venda.dt-entrega   > da-dt-entrega-fin  OR
           ped-venda.no-ab-reppri < c-no-ab-reppri-ini OR
           ped-venda.no-ab-reppri > c-no-ab-reppri-fin 
           THEN NEXT.

        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda-ext THEN DO.
           MESSAGE 'Inconsistància de Base no Pedido: ' ped-venda.nr-pedcli SKIP
                   'Solicite ao Departamento de Vendas para dar Manutená∆o no Pedido...'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           NEXT.
        END.
    
        IF ped-venda-ext.l-nao-aprovar THEN NEXT.

        IF ped-venda-ext.tp-pedido = "Amostra" OR
           ped-venda-ext.tp-pedido = "Amostra Exportaá∆o" THEN DO.
           FIND natur-oper WHERE
                natur-oper.nat-operacao = ped-venda.nat-oper NO-LOCK NO-ERROR.
           IF natur-oper.emite-duplic = NO THEN NEXT.
        END.

        FIND emitente WHERE
             emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
    
        IF NOT AVAIL emitente THEN NEXT.

        IF emitente.nome-matriz < c-matriz-ini OR
           emitente.nome-matriz > c-matriz-fin THEN NEXT.

        IF emitente.ind-cre-cli = 2 THEN NEXT.  /* CrÇdito Autom†tico */

        RUN pi-ver-digita (INPUT "Pedido_de_Venda",                                                             
                           INPUT ped-venda.nr-pedcli).                                                          
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.                                                                
                                                                                                                 
        RUN pi-ver-digita (INPUT "Cliente",
                           INPUT ped-venda.nome-abrev).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
        ASSIGN de-tot-aceitos = 0     de-tot-ped = 0     de-tot-desc = 0
               de-tot-aprov = 0       de-tot-res = 0     de-tot-pend = 0.

        FOR EACH ped-item OF ped-venda WHERE
                 ped-item.it-codigo >= c-it-codigo-ini AND
                 ped-item.it-codigo <= c-it-codigo-fin AND
                 ped-item.cod-refer >= c-cod-refer-ini AND 
                 ped-item.cod-refer <= c-cod-refer-fin NO-LOCK,
           FIRST ped-item-ext WHERE
                 ped-item-ext.cod-estabel = ped-venda.cod-estabel AND
                 ped-item-ext.nr-pedcli = ped-item.nr-pedcli AND 
                 ped-item-ext.nome-abrev = ped-item.nome-abrev AND
                 ped-item-ext.nr-sequencia = ped-item.nr-sequencia
                 NO-LOCK. 

            IF ped-item.cod-sit-item = 6 THEN NEXT.
            
            RUN pi-ver-digita (INPUT "Item",
                               INPUT ped-item.it-codigo).
            IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
            RUN pi-ver-digita (INPUT "Referància",
                               INPUT ped-item.cod-refer).
            IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
            FIND item WHERE
                 item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
            
            ASSIGN de-tot-ped = de-tot-ped + (ped-item.qt-pedida * ped-item.vl-preuni)
                   de-tot-desc = de-tot-desc + ped-item.val-desconto-total.

            IF (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval = 4) THEN DO. /* N∆o Avaliados */ 
               IF ped-venda.cod-sit-ped = 1 OR
                  ped-venda.cod-sit-ped = 2 THEN
                  ASSIGN de-tot-aceitos = de-tot-aceitos + (ped-item.qt-pedida * ped-item.vl-preuni) + ped-item.val-desconto-total.
    
               IF ped-venda.cod-sit-ped = 4 THEN
                  ASSIGN de-tot-pend = de-tot-pend + (ped-item.qt-pedida * ped-item.vl-preuni) + ped-item.val-desconto-total.
            END.
            ELSE
               ASSIGN de-tot-aprov = de-tot-aprov + (ped-item.qt-pedida * ped-item.vl-preuni) + ped-item.val-desconto-total.
            
            FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.
            IF AVAIL ped-item-res THEN
               ASSIGN de-tot-res = de-tot-res + (ped-item.qt-pedida * ped-item.vl-preuni) + ped-item.val-desconto-total.
        END.
        
        
        FIND tt-clientes WHERE
             tt-clientes.nome-abrev = ped-venda.nome-abrev NO-ERROR.
        IF NOT AVAIL tt-clientes THEN DO:
           CREATE tt-clientes.
           BUFFER-COPY emitente TO tt-clientes
                ASSIGN tt-clientes.repres = ped-venda.no-ab-reppri.
        END.
        ASSIGN tt-clientes.tot-pend = tt-clientes.tot-pend + de-tot-pend 
               tt-clientes.tot-aceitos = tt-clientes.tot-aceitos + de-tot-aceitos
               tt-clientes.tot-aprovados = tt-clientes.tot-aprovados + de-tot-aprov.

        ASSIGN tt-clientes.desc-bloq-cr = IF tt-clientes.desc-bloq-cr = "" AND ped-venda.desc-bloq-cr <> ""
                                          THEN ped-venda.desc-bloq-cr
                                          ELSE IF ped-venda.desc-bloq-cr <> "" 
                                               THEN tt-clientes.desc-bloq-cr + " // " + ped-venda.desc-bloq-cr
                                               ELSE tt-clientes.desc-bloq-cr.

        FIND tt-ped-venda WHERE
             tt-ped-venda.nr-pedcli  = ped-venda.nr-pedcli AND
             tt-ped-venda.nome-abrev = ped-venda.nome-abrev NO-ERROR.
        IF NOT AVAIL tt-ped-venda THEN DO:
           CREATE tt-ped-venda.
           BUFFER-COPY ped-venda TO tt-ped-venda.
           ASSIGN tt-ped-venda.cod-sit-ped =  ped-venda.cod-sit-ped
                  tt-ped-venda.tp-pedido = ped-venda-ext.tp-pedido
                  tt-ped-venda.vl-aberto = de-tot-ped
                  tt-ped-venda.vl-desconto = de-tot-desc
                  tt-ped-venda.vl-total = de-tot-ped + de-tot-desc
                  tt-ped-venda.visualiza = YES.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa w-livre 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

   EMPTY TEMP-TABLE tt-cheques.
   EMPTY TEMP-TABLE tt-titulos.
   EMPTY TEMP-TABLE tt-ped-venda.
   EMPTY TEMP-TABLE tt-clientes.

   RUN esapi/ret-udm.p (INPUT c-dt-limite-fin, OUTPUT c-dia).
   ASSIGN da-dt-entrega-fin = DATE(c-dia + SUBSTR(c-dt-limite-fin,1,2) + SUBSTR(c-dt-limite-fin,3,4)).

   ASSIGN c-sit-ped = '1,2,4'
          gr-emitente = ?
          rw-cliente = ?.

   RUN pi-pedidos (INPUT 1).
   RUN pi-pedidos (INPUT 2).
   RUN pi-pedidos (INPUT 4).

   RUN pi-titulos.
   RUN pi-cheques.

   {utp/ut-liter.i Calculando_Conta_Corrente *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
   FOR EACH tt-clientes BY tt-clientes.nome-abrev.

       RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + tt-clientes.nome-abrev).

       ASSIGN tt-clientes.lim-cred = tt-clientes.lim-cred + tt-clientes.lim-adicional.

       ASSIGN tt-clientes.saldo = tt-clientes.lim-cred - tt-clientes.tot-pend - tt-clientes.tot-aceitos -
                                  tt-clientes.tit-vencidos - tt-clientes.tot-aprov - tt-clientes.tit-a-vencer + tt-clientes.cheques-compsar -
                                  tt-clientes.cheques-dev.

       ASSIGN de-tot-pend = de-tot-pend + tt-clientes.tot-pend
              de-tot-aceitos = de-tot-aceitos + tt-clientes.tot-aceitos.
       
       RUN esapi/esapi0180.p (INPUT tt-clientes.cod-emit, 
                              OUTPUT tt-clientes.restricao,
                              OUTPUT tt-clientes.motivo).
       
       IF tt-clientes.saldo <= 0 THEN
          ASSIGN tt-clientes.motivo = IF tt-clientes.motivo = ""
                                      THEN "Excedeu Limite CrÇdito"
                                      ELSE "Excedeu Limite CrÇdito // " + tt-clientes.motivo.

       IF tt-clientes.saldo > 0 AND 
          NOT tt-clientes.restricao THEN
          ASSIGN tt-clientes.motivo = "Credito OK".

       IF tt-clientes.desc-bloq-cr <> "" THEN
          ASSIGN tt-clientes.motivo = tt-clientes.motivo + " // " + tt-clientes.desc-bloq-cr.
   END.

   RUN pi-finalizar IN h-acomp.

   APPLY 'VALUE-CHANGED' TO rs-sit-credito IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-serasa w-livre 
PROCEDURE pi-serasa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-titulos w-livre 
PROCEDURE pi-titulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {utp/ut-liter.i Selecionando_Titulos *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    FOR EACH tt-clientes EXCLUSIVE-LOCK.
        FOR EACH titulo WHERE 
                 titulo.cod-emit = tt-clientes.cod-emit AND
                 titulo.cod-esp = 'DP' AND
                 titulo.vl-saldo <> 0 NO-LOCK USE-INDEX emitente.
                                                                                            
            /* Ignora duplicatas substituidas */
            IF CAN-FIND(FIRST mov-tit OF titulo WHERE mov-tit.baixa-subs) THEN NEXT.
                                                                                           
            RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + titulo.nome-abrev).
    
            IF titulo.dt-vencimen < TODAY THEN  
               ASSIGN tt-clientes.tit-vencidos = tt-clientes.tit-vencidos + titulo.vl-saldo.
            ELSE /* A Vencer*/
               ASSIGN tt-clientes.tit-a-vencer = tt-clientes.tit-a-vencer + titulo.vl-saldo.
                                                                                                       CREATE tt-titulos.
            BUFFER-COPY titulo TO tt-titulos.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-permissao w-livre 
PROCEDURE pi-ver-permissao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-grupos AS CHAR FORMAT "x(20)".
    
    ASSIGN l-ok = NO.
    FIND FIRST espec.param-dis NO-LOCK NO-ERROR.

    CASE SELF:NAME.
        WHEN "bt-inclui" OR WHEN "bt-divide" THEN
           ASSIGN c-grupos = espec.param-dis.grp-inc-ped.
        WHEN "bt-modifica" OR WHEN "bt-divide" THEN
           ASSIGN c-grupos = espec.param-dis.grp-alt-ped.
        WHEN "bt-cancela" THEN
           ASSIGN c-grupos = espec.param-dis.grp-can-ped.
        WHEN "bt-suspende" THEN
           ASSIGN c-grupos = espec.param-dis.grp-sus-ped.
    END CASE.

    FOR EACH usuar_grp_usuar WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
        IF INDEX(c-grupos,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN DO:
           ASSIGN l-ok = YES.
           LEAVE.
        END.
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
  {src/adm/template/snd-list.i "tt-clientes"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-cond-pagto w-livre 
FUNCTION fn-cond-pagto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR c-cond-pagto AS CHAR FORMAT "x(30)".
   FIND cond-pagto WHERE
        cond-pagto.cod-cond-pag = tt-ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
   IF AVAIL cond-pagto THEN 
      ASSIGN c-cond-pagto = cond-pagto.descricao.
   ELSE 
      ASSIGN c-cond-pagto = 'E S P E C I A L'.

  RETURN c-cond-pagto.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-modalidade w-livre 
FUNCTION fn-modalidade RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF tt-clientes.modalidade <> 0 THEN DO.
     {esinc/i-dsrb.i tt-clientes.modalidade tt-clientes.modalidade c-modalidade}.
  END.
  ELSE
     ASSIGN c-modalidade = "".

  RETURN c-modalidade.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-cli w-livre 
FUNCTION fn-sit-cli RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   IF tt-clientes.ind-sit-emitente <> 0 THEN DO.
      {esinc/i-dsrb.i tt-clientes.ind-sit-emitente tt-clientes.ind-sit-emitente c-sit-cli}.
   END.
   ELSE
      ASSIGN c-sit-cli = "".

   RETURN c-sit-cli.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-cred w-livre 
FUNCTION fn-sit-cred RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  {esinc/i-dsrb.i tt-ped-venda.cod-sit-aval tt-ped-venda.cod-sit-aval c-sit-ped}.

  RETURN c-sit-ped.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-ped w-livre 
FUNCTION fn-sit-ped RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR c-sit-ped AS CHAR.

  CASE tt-ped-venda.cod-sit-ped.
       WHEN 1 THEN ASSIGN c-sit-ped = 'ABE'.
       WHEN 2 THEN ASSIGN c-sit-ped = 'ATP'.
       WHEN 3 THEN ASSIGN c-sit-ped = 'ATT'.
       WHEN 4 THEN ASSIGN c-sit-ped = 'PEN'.
       WHEN 5 THEN ASSIGN c-sit-ped = 'SUS'.
       WHEN 6 THEN ASSIGN c-sit-ped = 'CAN'.
  END CASE.

  RETURN c-sit-ped.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

