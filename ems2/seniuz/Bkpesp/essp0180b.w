&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{include/i-vrtab.i emitente}

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE NEW GLOBAL SHARED VAR gr-emitente AS ROWID NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-win-sel-estab AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-estabelec-dest LIKE estabelec.

DEF BUFFER b-titulo FOR titulo.

/* Local Variable Definitions ---                                       */
DEF TEMP-TABLE tt-dados NO-UNDO
    FIELD i-cod-emit       LIKE emitente.cod-emit
    FIELD fi-periodo       AS CHAR FORMAT "99/9999"
    FIELD da-data-conver   AS DATE
    FIELD tipo-consulta    AS INTEGER INITIAL 1
    FIELD cod-versao-integ AS INTEGER
    FIELD l-vid-rel        AS LOGICAL.

DEF TEMP-TABLE tt-estatistica NO-UNDO
    FIELD dt-maior-tit LIKE estatist.dt-maior-tit
    FIELD vl-maior-tit LIKE estatist.vl-maior-tit
    FIELD dt-ult-tit   LIKE estatist.dt-ult-tit
    FIELD vl-ult-tit   LIKE estatist.vl-ult-tit 
    FIELD fi-vendas            AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-acumuladas AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-saldo-aberto      AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-periodo-1         AS CHAR 
    FIELD fi-periodo-2         AS CHAR 
    FIELD fi-periodo-3         AS CHAR 
    FIELD fi-periodo-4         AS CHAR 
    FIELD fi-periodo-5         AS CHAR 
    FIELD fi-periodo-6         AS CHAR 
    FIELD fi-periodo-7         AS CHAR 
    FIELD fi-periodo-8         AS CHAR 
    FIELD fi-periodo-9         AS CHAR 
    FIELD fi-periodo-10        AS CHAR 
    FIELD fi-periodo-11        AS CHAR 
    FIELD fi-periodo-12        AS CHAR 
    FIELD fi-atm-1        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-2        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-3        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-4        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-5        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-6        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-7        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-8        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-9        AS INTEGER FORMAT "->>9"
    FIELD fi-atm-10       AS INTEGER FORMAT "->>9"
    FIELD fi-atm-11       AS INTEGER FORMAT "->>9"
    FIELD fi-atm-12       AS INTEGER FORMAT "->>9"
    FIELD fi-atm-media    AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-1        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-2        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-3        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-4        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-5        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-6        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-7        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-8        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-9        AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-10       AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-11       AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-12       AS INTEGER FORMAT "->>9"
    FIELD fi-pmr-media    AS INTEGER FORMAT "->>9"
    FIELD fi-vendas-1     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-2     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-3     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-4     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-5     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-6     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-7     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-8     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-9     AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-10    AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-11    AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-12    AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD fi-vendas-media AS DECIMAL FORMAT "->>,>>>,>>9.99".

DEF TEMP-TABLE tt-erro NO-UNDO
    FIELD i-cod-erro     AS INTEGER FORMAT "9999999"
    FIELD c-desc-erro    AS CHAR    FORMAT "x(70)"
    FIELD c-arquivo-erro AS CHAR    FORMAT "x(100)".

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

DEF TEMP-TABLE tt-titulos LIKE titulo
    FIELD visualiza AS LOG INIT NO.

DEF TEMP-TABLE tt-cheques LIKE cheque
    FIELD visualiza AS LOG.

/* Parƒmetros Recebidos */
DEF INPUT PARAMETER TABLE FOR tt-clientes.
DEF INPUT PARAMETER TABLE FOR tt-titulos.
DEF INPUT PARAMETER TABLE FOR tt-cheques.

/* Local Variable Definitions ---                                       */
DEF VAR i-tot-titulos          AS INT.
DEF VAR de-vlr-titulos         AS DEC.
DEF VAR i-tit-quitados         AS INT.
DEF VAR de-vlr-quitados        AS DEC.
DEF VAR i-tit-a-quitar         AS INT.
DEF VAR de-vlr-a-quitar        AS DEC.
DEF VAR i-tot-cheques          AS INT.
DEF VAR de-vlr-total-chq       AS DEC.
DEF VAR i-qtd-chq-compensado   AS INT.
DEF VAR de-vlr-chq-compensado  AS DEC.
DEF VAR i-qtd-chq-a-compensar  AS INT.
DEF VAR de-vlr-chq-a-compensar AS DEC.



DEF VAR d-tot-cheque-devolv-pendente AS DECIMAL NO-UNDO.
DEF VAR d-tot-cheque-pendente-semdevol AS DECIMAL NO-UNDO.
DEF VAR d-tot-cheque-quitado  AS DECIMAL NO-UNDO.
DEF VAR d-tot-cheque-aberto   AS DECIMAL NO-UNDO.
DEF VAR i-cheque-prorrogado   AS INTEGER NO-UNDO.
DEF VAR d-cheque-prorrogado   AS DECIMAL NO-UNDO.
DEF VAR i-cheque-compensado   AS INTEGER NO-UNDO.
DEF VAR d-cheque-compensado   AS DECIMAL NO-UNDO.
DEF VAR i-cheque-compensar    AS INTEGER NO-UNDO.
DEF VAR d-cheque-compensar    AS DECIMAL NO-UNDO.
DEF VAR i-qt-dup-prorrog      AS INTEGER NO-UNDO.
DEF VAR de-vlr-dup-prorrog    AS DECIMAL NO-UNDO.
DEF VAR i-duplic-quitada      AS INTEGER NO-UNDO.
DEF VAR d-duplic-quitada      AS DECIMAL NO-UNDO.
DEF VAR i-duplic-quitar       AS INTEGER NO-UNDO.
DEF VAR d-duplic-quitar       AS DECIMAL NO-UNDO.
DEF VAR d-maior-debito        AS DECIMAL NO-UNDO.
DEF VAR dt-maior-debito       AS DATE    NO-UNDO.
DEF VAR d-maior-compra        AS DECIMAL NO-UNDO.
DEF VAR dt-maior-compra       AS DATE    NO-UNDO.
DEF VAR d-notas               AS DECIMAL NO-UNDO.
DEF VAR d-tot-compensar       AS DECIMAL NO-UNDO.
DEF VAR d-tot-atraso          AS DECIMAL NO-UNDO.
DEF VAR d-tot-juros-pend      AS DECIMAL NO-UNDO.
DEF VAR d-tot-juros-quit      AS DECIMAL NO-UNDO.
DEF VAR d-tot-quitado         AS DECIMAL NO-UNDO.
DEF VAR d-tot-devolucao       AS DECIMAL NO-UNDO.
DEF VAR data-ultima-nota      AS DATE    NO-UNDO.
DEF VAR d-aux                     AS DEC NO-UNDO.
DEF VAR d-cheque-proprio          AS DEC NO-UNDO.
DEF VAR d-bordero                 AS DEC NO-UNDO.
DEF VAR d-titulo-quitado          AS DECIMAL NO-UNDO.
DEF VAR d-titulo-quitado-atraso   AS DEC NO-UNDO.
DEF VAR d-titulo-quitado-cartorio AS DEC NO-UNDO.
DEF VAR d-tot-titulo-compensar    AS DEC NO-UNDO.

                                                                                 
DEF VAR d-sld-tit-reab-dev        AS DEC NO-UNDO.
DEF VAR h-programa AS HANDLE.

DEF VAR de-saldo AS DEC.

/*
FIND emitente WHERE
     emitente.cod-emit = 16606.

ASSIGN gr-emitente = ROWID(emitente).
EMPTY TEMP-TABLE tt-estabelec-dest.
CREATE tt-estabelec-dest.
ASSIGN tt-estabelec-dest.ep-codigo = 1
       tt-estabelec-dest.cod-estabel = '2'.
*/

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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-12 RECT-13 RECT-14 RECT-15 ~
RECT-16 RECT-5 RECT-55 RECT-6 RECT-7 RECT-8 RECT-9 bt-analise fi-dt-inicial ~
bt-det-dupl-prorrog bt-det-chq-prorrog bt-det-tot-quitado bt-det-tot-atraso ~
bt-det-dupl-quit bt-det-chq-comp bt-det-tot-devol bt-det-tot-comp ~
bt-det-dupl-a-venc bt-det-chq-a-comp bt-det-juros-pend bt-det-chq-dev-pend ~
bt-det-dupl-quit-atraso bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-emit fi-nome-cli fi-sit-cre ~
fi-dt-inicial fi-tot-abe fi-total-titulos fi-qtd-total-chq d-total-aberto ~
fi-lim-cred fi-vlr-tot-titulos fi-vlr-total-chq d-total-quitado ~
fi-dt-cred-ini fi-qtd-tit-quitados fi-qtd-chq-compensado d-total-atraso ~
fi-lim-cred-ad fi-vlr-tit-quitados fi-vlr-chq-compensado d-total-devolucao ~
fi-dt-cred-fim fi-qtd-tit-a-quitar fi-qtd-chq-a-compensar d-total-compensar ~
fi-saldo-disp fi-vlr-tit-a-quitar fi-vlr-chq-a-compensar ~
fi-mes-maior-compra fi-ano-maior-compra fi-mes-maior-acum fi-ano-maior-acum ~
fi-dt-ult-nf d-juros-pendentes fi-vlr-maior-compra fi-vendas-acumuladas ~
fi-vlr-ult-nf d-juros-quitados d-pendentes d-tot-titulo-quitado ~
d-tot-cheque-proprio d-quitados d-tot-titulo-quitado-atraso d-tot-bordero ~
d-acumulado d-tot-titulo-quitado-cartorio 

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

DEFINE BUTTON bt-analise 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Button 1" 
     SIZE 5 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-det-chq-a-comp 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 6" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Cheques a Compensar".

DEFINE BUTTON bt-det-chq-comp 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 5" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Cheques Compensados".

DEFINE BUTTON bt-det-chq-dev-pend 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 8" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Cheques Pendentes".

DEFINE BUTTON bt-det-chq-prorrog 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 4" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Cheques Prorrogados".

DEFINE BUTTON bt-det-dupl-a-venc 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 3" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Duplicatas a Vencer".

DEFINE BUTTON bt-det-dupl-prorrog 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 1" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Duplicatas Prorrogadas".

DEFINE BUTTON bt-det-dupl-quit 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 2" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Duplicatas Quitadas".

DEFINE BUTTON bt-det-dupl-quit-atraso 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 15" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Duplicatas Quitadas em Atraso".

DEFINE BUTTON bt-det-juros-pend 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 14" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Juros Pendentes".

DEFINE BUTTON bt-det-tot-aberto 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 13" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Total em Aberto".

DEFINE BUTTON bt-det-tot-atraso 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 12" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Total em Atraso".

DEFINE BUTTON bt-det-tot-comp 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 11" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Total a Compensar".

DEFINE BUTTON bt-det-tot-devol 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 10" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Devolu‡Æo Total".

DEFINE BUTTON bt-det-tot-quitado 
     IMAGE-UP FILE "image/gr-zoo.bmp":U
     LABEL "Button 9" 
     SIZE 3 BY .88 TOOLTIP "Detalhar Total Quitado".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE d-acumulado AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Acumulados" 
     VIEW-AS FILL-IN 
     SIZE 15.29 BY .88 NO-UNDO.

DEFINE VARIABLE d-juros-pendentes AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Pendentes" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-juros-quitados AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Quitados" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-pendentes AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Pendentes" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .88 NO-UNDO.

DEFINE VARIABLE d-quitados AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Quitados" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-bordero AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cheque Terceiro" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-cheque-proprio AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cheque Pr¢prio" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-titulo-quitado AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Dupl Quitada" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-titulo-quitado-atraso AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Dupl Quitada Atraso" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-titulo-quitado-cartorio AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Dupl Quitada Cart¢rio" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-total-aberto AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Aberto" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-total-atraso AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Atraso" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-total-compensar AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Compensar" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88 NO-UNDO.

DEFINE VARIABLE d-total-devolucao AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Devolu‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-total-quitado AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Quitado" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ano-maior-acum AS CHARACTER FORMAT "X(4)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ano-maior-compra AS CHARACTER FORMAT "X(4)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-emit AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-cred-fim AS DATE FORMAT "99/99/9999":U 
     LABEL "Fim Limite Cr‚dito" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-cred-ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Limite Cr‚dito" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-inicial AS DATE FORMAT "99/99/9999":U 
     LABEL "Data In¡cio An lise" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-ult-nf AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-lim-cred AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Limite Cr‚dito" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-lim-cred-ad AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Lim. Cr‚d. Adicional" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-mes-maior-acum AS CHARACTER FORMAT "X(2)":U INITIAL "0" 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-mes-maior-compra AS CHARACTER FORMAT "X(2)":U INITIAL "0" 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-cli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-chq-a-compensar AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Compensar" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-chq-compensado AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Compensada" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-tit-a-quitar AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd a Quitar" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-tit-quitados AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Quitada" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-total-chq AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Total" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-saldo-disp AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Dispon¡vel" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-sit-cre AS CHARACTER FORMAT "x(40)":U 
     LABEL "Situa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-abe AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Em Aberto" 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-total-titulos AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Total" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vendas-acumuladas AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-chq-a-compensar AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Compensar" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-chq-compensado AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Compensado" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-maior-compra AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-tit-a-quitar AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor a Quitar" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-tit-quitados AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Quitado" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-tot-titulos AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-total-chq AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-vlr-ult-nf AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 111 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.57 BY 3.25.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 3.25.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63.43 BY 3.71.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 6.75.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 3.25.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 6.75.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 2.25.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 23.43 BY 3.25.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 3.71.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY 6.75.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 6.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cod-emit AT ROW 1.17 COL 15 COLON-ALIGNED
     fi-nome-cli AT ROW 1.17 COL 24.43 COLON-ALIGNED NO-LABEL
     bt-analise AT ROW 2.08 COL 81.57
     fi-sit-cre AT ROW 2.17 COL 15 COLON-ALIGNED
     fi-dt-inicial AT ROW 2.17 COL 68 COLON-ALIGNED
     fi-tot-abe AT ROW 3.88 COL 15 COLON-ALIGNED
     fi-total-titulos AT ROW 4 COL 38 COLON-ALIGNED
     fi-qtd-total-chq AT ROW 4 COL 69 COLON-ALIGNED
     d-total-aberto AT ROW 4.42 COL 93.72 COLON-ALIGNED
     bt-det-tot-aberto AT ROW 4.46 COL 107.86
     fi-lim-cred AT ROW 4.88 COL 15 COLON-ALIGNED
     fi-vlr-tot-titulos AT ROW 5 COL 38 COLON-ALIGNED
     bt-det-dupl-prorrog AT ROW 5 COL 53
     fi-vlr-total-chq AT ROW 5 COL 69 COLON-ALIGNED
     bt-det-chq-prorrog AT ROW 5 COL 82.57
     d-total-quitado AT ROW 5.42 COL 93.72 COLON-ALIGNED
     bt-det-tot-quitado AT ROW 5.42 COL 107.86
     fi-dt-cred-ini AT ROW 5.88 COL 15 COLON-ALIGNED
     fi-qtd-tit-quitados AT ROW 6 COL 38 COLON-ALIGNED
     fi-qtd-chq-compensado AT ROW 6 COL 69 COLON-ALIGNED
     d-total-atraso AT ROW 6.42 COL 93.72 COLON-ALIGNED
     bt-det-tot-atraso AT ROW 6.42 COL 107.86
     fi-lim-cred-ad AT ROW 6.88 COL 2.86
     fi-vlr-tit-quitados AT ROW 7 COL 38 COLON-ALIGNED
     bt-det-dupl-quit AT ROW 7 COL 53
     fi-vlr-chq-compensado AT ROW 7 COL 69 COLON-ALIGNED
     bt-det-chq-comp AT ROW 7 COL 82.57
     d-total-devolucao AT ROW 7.42 COL 93.72 COLON-ALIGNED
     bt-det-tot-devol AT ROW 7.42 COL 107.86
     fi-dt-cred-fim AT ROW 7.88 COL 15 COLON-ALIGNED
     fi-qtd-tit-a-quitar AT ROW 8 COL 38 COLON-ALIGNED
     fi-qtd-chq-a-compensar AT ROW 8 COL 69 COLON-ALIGNED
     d-total-compensar AT ROW 8.42 COL 93.86 COLON-ALIGNED
     bt-det-tot-comp AT ROW 8.42 COL 107.86
     fi-saldo-disp AT ROW 8.88 COL 15 COLON-ALIGNED
     fi-vlr-tit-a-quitar AT ROW 9 COL 38 COLON-ALIGNED
     bt-det-dupl-a-venc AT ROW 9 COL 53
     fi-vlr-chq-a-compensar AT ROW 9 COL 69 COLON-ALIGNED
     bt-det-chq-a-comp AT ROW 9 COL 82.57
     fi-mes-maior-compra AT ROW 11.5 COL 9.14 COLON-ALIGNED
     fi-ano-maior-compra AT ROW 11.5 COL 12.29 COLON-ALIGNED NO-LABEL
     fi-mes-maior-acum AT ROW 11.5 COL 36 COLON-ALIGNED
     fi-ano-maior-acum AT ROW 11.5 COL 39.29 COLON-ALIGNED NO-LABEL
     fi-dt-ult-nf AT ROW 11.5 COL 61 COLON-ALIGNED
     d-juros-pendentes AT ROW 11.5 COL 90.86 COLON-ALIGNED
     bt-det-juros-pend AT ROW 11.5 COL 106.29
     fi-vlr-maior-compra AT ROW 12.5 COL 9 COLON-ALIGNED
     fi-vendas-acumuladas AT ROW 12.5 COL 36 COLON-ALIGNED
     fi-vlr-ult-nf AT ROW 12.5 COL 61 COLON-ALIGNED
     d-juros-quitados AT ROW 12.54 COL 90.86 COLON-ALIGNED
     d-pendentes AT ROW 14.38 COL 81.57 COLON-ALIGNED
     bt-det-chq-dev-pend AT ROW 14.38 COL 99
     d-tot-titulo-quitado AT ROW 14.5 COL 19.72 COLON-ALIGNED
     d-tot-cheque-proprio AT ROW 15 COL 48.43 COLON-ALIGNED
     d-quitados AT ROW 15.33 COL 81.57 COLON-ALIGNED
     d-tot-titulo-quitado-atraso AT ROW 15.5 COL 19.72 COLON-ALIGNED
     bt-det-dupl-quit-atraso AT ROW 15.54 COL 35
     d-tot-bordero AT ROW 16 COL 48.43 COLON-ALIGNED
     d-acumulado AT ROW 16.38 COL 81.57 COLON-ALIGNED
     d-tot-titulo-quitado-cartorio AT ROW 16.5 COL 19.72 COLON-ALIGNED
     bt-ok AT ROW 18.21 COL 2
     bt-cancelar AT ROW 18.21 COL 12.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.72 BY 21.33
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     bt-ajuda AT ROW 18.21 COL 101.43
     " Maior Compra no Per¡odo" VIEW-AS TEXT
          SIZE 19.86 BY .67 AT ROW 10.25 COL 3.14
     " Titulos" VIEW-AS TEXT
          SIZE 5.72 BY .5 AT ROW 3.29 COL 30.29
     " éltima Compra" VIEW-AS TEXT
          SIZE 11.14 BY .67 AT ROW 10.25 COL 55.86
     " Cheques" VIEW-AS TEXT
          SIZE 7 BY .46 AT ROW 3.29 COL 58
     " Maior Ac£mulo 1 Ano" VIEW-AS TEXT
          SIZE 15 BY .67 AT ROW 10.25 COL 31.29
     " Total Juros" VIEW-AS TEXT
          SIZE 9 BY .67 AT ROW 10.25 COL 80.43
     " Cr‚dito" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 3.29 COL 3
     " Totais" VIEW-AS TEXT
          SIZE 6.14 BY .5 AT ROW 3.29 COL 87.86
     RECT-1 AT ROW 18 COL 1
     RECT-12 AT ROW 10.5 COL 1
     RECT-13 AT ROW 10.5 COL 28.14
     RECT-14 AT ROW 14.04 COL 1
     RECT-15 AT ROW 3.5 COL 87
     RECT-16 AT ROW 10.5 COL 78
     RECT-5 AT ROW 3.5 COL 1
     RECT-55 AT ROW 1 COL 1
     RECT-6 AT ROW 10.5 COL 53.86
     RECT-7 AT ROW 14.04 COL 65
     RECT-8 AT ROW 3.5 COL 57
     RECT-9 AT ROW 3.5 COL 29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.72 BY 21.33
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
         TITLE              = "Analise Global de Cr‚dito por Cliente - ESSP0180b"
         HEIGHT             = 18.5
         WIDTH              = 111.14
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.72
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.72
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
/* SETTINGS FOR BUTTON bt-det-tot-aberto IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-det-tot-aberto:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN d-acumulado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-juros-pendentes IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-juros-quitados IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-pendentes IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-quitados IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-bordero IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-cheque-proprio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-titulo-quitado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-titulo-quitado-atraso IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-titulo-quitado-cartorio IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-aberto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-atraso IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-compensar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-devolucao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-quitado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ano-maior-acum IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ano-maior-compra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-cred-fim IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-cred-ini IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ult-nf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lim-cred IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-lim-cred-ad IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN fi-mes-maior-acum IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-mes-maior-compra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-cli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-chq-a-compensar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-chq-compensado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-tit-a-quitar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-tit-quitados IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-total-chq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-saldo-disp IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sit-cre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-abe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-total-titulos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vendas-acumuladas IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-chq-a-compensar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-chq-compensado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-maior-compra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-tit-a-quitar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-tit-quitados IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-tot-titulos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-total-chq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-ult-nf IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Analise Global de Cr‚dito por Cliente - ESSP0180b */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Analise Global de Cr‚dito por Cliente - ESSP0180b */
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


&Scoped-define SELF-NAME bt-analise
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-analise w-digita
ON CHOOSE OF bt-analise IN FRAME F-Main /* Button 1 */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-inicial.

  FIND emitente WHERE
       ROWID(emitente) = gr-emitente NO-LOCK NO-ERROR.

  RUN pi-carrega-emitente.  
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


&Scoped-define SELF-NAME bt-det-chq-a-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-chq-a-comp w-digita
ON CHOOSE OF bt-det-chq-a-comp IN FRAME F-Main /* Button 6 */
DO:
/*     RUN imp\impd030a.w (INPUT "FOR EACH cheque NO-LOCK WHERE "                                              */
/*                               +        "cheque.cod-emitente = " + string(i-cod-emitente) + " AND "          */
/*                               +        "cheque.dt-vencimento > cheque.data-2 AND "                          */
/*                               +       "(cheque.situacao-cheque = 1 OR cheque.situacao-cheque = 5) AND "     */
/*                               +        "cheque.origem-cheque = 6 "                                          */
/*                               +  "FIRST tt-estabelec-dest NO-LOCK WHERE "                                   */
/*                               +        "tt-estabelec-dest.cod-estabel = cheque.cod-estabel-mvto:",          */
/*                         INPUT d-tot-cheque-compensar:SCREEN-VALUE IN FRAME {&FRAME-NAME}).                  */
/*                                                                                         /***DMJGD001.en***/ */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-chq-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-chq-comp w-digita
ON CHOOSE OF bt-det-chq-comp IN FRAME F-Main /* Button 5 */
DO:
/*     RUN imp\impd030a.w (INPUT "FOR EACH cheque NO-LOCK WHERE "                                                                         */
/*                               +        "cheque.cod-emitente = " + string(i-cod-emitente) + " AND "                                     */
/*                               +        "cheque.dt-vencimento > cheque.data-2 AND "                                                     */
/*                               +       "(cheque.situacao-cheque = 2  OR cheque.situacao-cheque = 7 OR cheque.situacao-cheque = 8) AND " */
/*                               +        "cheque.origem-cheque = 6 ",                                                                    */
/*                         INPUT d-tot-cheque-compensado:SCREEN-VALUE IN FRAME {&FRAME-NAME}).                                            */
                                                                                        /***DMJGD001.en***/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-chq-dev-pend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-chq-dev-pend w-digita
ON CHOOSE OF bt-det-chq-dev-pend IN FRAME F-Main /* Button 8 */
DO:
/*     DO WITH FRAME {&FRAME-NAME}:                                                                                                                                           */
/*         RUN imp\impd030j.w (INPUT "FOR EACH cheque NO-LOCK WHERE "                                                                                                         */
/*                                   +        "cheque.cod-emitente = " + STRING(i-cod-emitente) + " AND "                                                                     */
/*                                   +        "cheque.devolvido AND "                                                                                                         */
/*                                   +        "cheque.origem-cheque = 6  AND "                                                                                                */
/*                                   +       "(cheque.situacao-cheque = 1 OR 
                                               cheque.situacao-cheque = 5 OR
                                              (cheque.situacao-cheque = 2 AND cheque.dt-vencimento > TODAY )) ", */
/*                             INPUT d-sld-tit-reab-dev,                                                                                                                      */
/*                             INPUT d-tot-cheque-devolv-pendente).                                                                                                           */
/*     END.                                                                                                                                                                   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-chq-prorrog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-chq-prorrog w-digita
ON CHOOSE OF bt-det-chq-prorrog IN FRAME F-Main /* Button 4 */
DO:
    /*
    RUN imp\impd030a.w (INPUT "FOR EACH cheque NO-LOCK WHERE "
                              +        "cheque.cod-emitente = " + STRING(i-cod-emitente) + " AND "
                              +        "cheque.dt-vencimento > cheque.data-2 AND "
                              +        "cheque.origem-cheque = 6 ",
                        INPUT d-tot-cheque-prorrogado:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-dupl-a-venc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-dupl-a-venc w-digita
ON CHOOSE OF bt-det-dupl-a-venc IN FRAME F-Main /* Button 3 */
DO:
    FOR EACH tt-titulos.
        ASSIGN tt-titulos.visualiza = NO.

        IF tt-titulos.dt-vencimen > TODAY AND 
           tt-titulos.vl-saldo <> 0 THEN 
           ASSIGN tt-titulos.visualiza = YES.
    END.
    RUN esp/essp0180b1.w (INPUT TABLE tt-titulos,
                          "Titulos Vencidos"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-dupl-prorrog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-dupl-prorrog w-digita
ON CHOOSE OF bt-det-dupl-prorrog IN FRAME F-Main /* Button 1 */
DO:
   FOR EACH tt-titulos.
       ASSIGN tt-titulos.visualiza = NO.

       IF tt-titulos.dt-vencimen > TODAY THEN 
          ASSIGN tt-titulos.visualiza = YES.
   END.
   RUN esp/essp0180b1.w (INPUT TABLE tt-titulos,
                         INPUT "T¡tulos em Aberto").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-dupl-quit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-dupl-quit w-digita
ON CHOOSE OF bt-det-dupl-quit IN FRAME F-Main /* Button 2 */
DO:
    FOR EACH tt-titulos.
        ASSIGN tt-titulos.visualiza = NO.

        IF tt-titulos.dt-vencimen > TODAY AND 
           tt-titulos.vl-saldo = 0 THEN 
           ASSIGN tt-titulos.visualiza = YES.
    END.
    RUN esp/essp0180b1.w (INPUT TABLE tt-titulos,
                          INPUT "Titulos Pagos"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-dupl-quit-atraso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-dupl-quit-atraso w-digita
ON CHOOSE OF bt-det-dupl-quit-atraso IN FRAME F-Main /* Button 15 */
DO:
/*     RUN imp\impd030g.w (INPUT i-cod-emitente). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-juros-pend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-juros-pend w-digita
ON CHOOSE OF bt-det-juros-pend IN FRAME F-Main /* Button 14 */
DO:
/*     RUN imp\impd030f.w (INPUT i-cod-emitente). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-aberto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-aberto w-digita
ON CHOOSE OF bt-det-tot-aberto IN FRAME F-Main /* Button 13 */
DO:
/*     RUN esp\essp0180b2.w (INPUT 'FOR EACH titulo NO-LOCK WHERE '                                                                                                      */
/*                               +        'titulo.cod-emitente = ' + STRING(i-cod-emitente) + ' AND '                                                                    */
/*                               +        'titulo.tipo = 6 AND '                                                                                                         */
/*                               +        'titulo.vl-saldo <> 0  AND '                                                                                                   */
/*                               +       '(titulo.cod-esp = "DP" OR titulo.cod-esp = "DT" OR titulo.cod-esp = "DG" OR titulo.cod-esp = "NP" OR titulo.cod-esp = "CD") ', */
/*                         INPUT 'FOR EACH cheque NO-LOCK WHERE '                                                                                                        */
/*                               +        'cheque.cod-emitente = ' + STRING(i-cod-emitente) + ' AND '                                                                    */
/*                               +       '(cheque.situacao-cheque = 1 OR cheque.situacao-cheque = 5) AND '                                                               */
/*                               +        'cheque.origem-cheque = 6 ').                                                                                                  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-atraso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-atraso w-digita
ON CHOOSE OF bt-det-tot-atraso IN FRAME F-Main /* Button 12 */
DO:
/*     RUN imp\impd030b.w (INPUT i-cod-emitente). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-comp w-digita
ON CHOOSE OF bt-det-tot-comp IN FRAME F-Main /* Button 11 */
DO:
/*     RUN imp\impd030c.w (INPUT DATE(fi-dt-inicial:SCREEN-VALUE IN FRAME {&FRAME-NAME}),                                                                                                                                         */
/*                         INPUT "FOR EACH cheque NO-LOCK WHERE "                                                                                                                                                              */
/*                               +        "cheque.cod-emitente = " + STRING(i-cod-emitente) + " AND "                                                                                                                          */
/*                               +    "NOT cheque.devolvido AND "                                                                                                                                                              */
/*                               +      "( cheque.situacao-cheque = 1 OR ( ( cheque.situacao-cheque = 2 OR cheque.situacao-cheque = 3 OR cheque.situacao-cheque = 7) AND cheque.dt-vencimento > TODAY ) ) AND " /* FOMM.NEW */ */
/*                               +        "cheque.origem-cheque = 6 ",                                                                                                                                                         */
/*                         INPUT d-tot-titulo-compensar,                                                                                                                                                                       */
/*                         INPUT d-tot-cheque-pendente-semdevol).                                                                                                                                                              */
                                                                                        /***DMJGD001.en***/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-devol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-devol w-digita
ON CHOOSE OF bt-det-tot-devol IN FRAME F-Main /* Button 10 */
DO:
/*     RUN imp\impd030d.w. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-quitado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-quitado w-digita
ON CHOOSE OF bt-det-tot-quitado IN FRAME F-Main /* Button 9 */
DO:
/*     RUN imp\impd030e.w (INPUT i-cod-emitente,       */
/*                         INPUT d-titulo-quitado,     */
/*                         INPUT d-tot-cheque-aberto). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
  if return-value = "NOK":U then return no-apply.
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY fi-cod-emit fi-nome-cli fi-sit-cre fi-dt-inicial fi-tot-abe 
          fi-total-titulos fi-qtd-total-chq d-total-aberto fi-lim-cred 
          fi-vlr-tot-titulos fi-vlr-total-chq d-total-quitado fi-dt-cred-ini 
          fi-qtd-tit-quitados fi-qtd-chq-compensado d-total-atraso 
          fi-lim-cred-ad fi-vlr-tit-quitados fi-vlr-chq-compensado 
          d-total-devolucao fi-dt-cred-fim fi-qtd-tit-a-quitar 
          fi-qtd-chq-a-compensar d-total-compensar fi-saldo-disp 
          fi-vlr-tit-a-quitar fi-vlr-chq-a-compensar fi-mes-maior-compra 
          fi-ano-maior-compra fi-mes-maior-acum fi-ano-maior-acum fi-dt-ult-nf 
          d-juros-pendentes fi-vlr-maior-compra fi-vendas-acumuladas 
          fi-vlr-ult-nf d-juros-quitados d-pendentes d-tot-titulo-quitado 
          d-tot-cheque-proprio d-quitados d-tot-titulo-quitado-atraso 
          d-tot-bordero d-acumulado d-tot-titulo-quitado-cartorio 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 RECT-12 RECT-13 RECT-14 RECT-15 RECT-16 RECT-5 RECT-55 RECT-6 
         RECT-7 RECT-8 RECT-9 bt-analise fi-dt-inicial bt-det-dupl-prorrog 
         bt-det-chq-prorrog bt-det-tot-quitado bt-det-tot-atraso 
         bt-det-dupl-quit bt-det-chq-comp bt-det-tot-devol bt-det-tot-comp 
         bt-det-dupl-a-venc bt-det-chq-a-comp bt-det-juros-pend 
         bt-det-chq-dev-pend bt-det-dupl-quit-atraso bt-ok bt-cancelar bt-ajuda 
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
  
  ASSIGN fi-dt-inicial = TODAY.
                            
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY 'CHOOSE' TO bt-analise IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-emitente w-digita 
PROCEDURE pi-carrega-emitente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 FIND tt-clientes WHERE
      tt-clientes.cod-emit = emitente.cod-emit NO-LOCK NO-ERROR.

 ASSIGN fi-cod-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(emitente.cod-emit)
        fi-nome-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

 ASSIGN fi-sit-cre:FGCOLOR = 0.
 CASE emitente.ind-cre-cli:
      WHEN 1 THEN
          ASSIGN fi-sit-cre:SCREEN-VALUE = 'Normal'.
      WHEN 2 THEN
          ASSIGN fi-sit-cre:SCREEN-VALUE = 'Autom tico'.
      WHEN 3 THEN
          ASSIGN fi-sit-cre:SCREEN-VALUE = 'Somente Implanta Pedido'.
      WHEN 4 THEN
          ASSIGN fi-sit-cre:SCREEN-VALUE = 'Suspenso'
                 fi-sit-cre:FGCOLOR      = 12.
      WHEN 5 THEN
          ASSIGN fi-sit-cre:SCREEN-VALUE = 'Pagamento … Vista'.
      OTHERWISE 
          ASSIGN fi-sit-cre:SCREEN-VALUE = STRING(emitente.ind-cre-cli).
 END CASE.

 ASSIGN fi-lim-cred:SCREEN-VALUE    = STRING(emitente.lim-credito)
        fi-dt-cred-ini:SCREEN-VALUE = STRING(emitente.dt-lim-cred)
        fi-lim-cred-ad:SCREEN-VALUE = STRING(emitente.lim-adicional)
        fi-dt-cred-fim:SCREEN-VALUE = STRING(emitente.dt-fim-cred)
        fi-saldo-disp:SCREEN-VALUE  = STRING(tt-clientes.saldo)
        fi-tot-abe:SCREEN-VALUE     = STRING(tt-clientes.tot-pend     + tt-clientes.tot-aceitos     +
                                             tt-clientes.tit-vencidos + tt-clientes.tot-aprov       +
                                             tt-clientes.tit-a-vencer - tt-clientes.cheques-compsar +
                                             tt-clientes.cheques-dev).

 ASSIGN fi-saldo-disp:FGCOLOR = 0.
 IF tt-clientes.saldo < 0 THEN
    ASSIGN fi-saldo-disp:FGCOLOR = 12.

/* Dados Estatisticos */
 FOR EACH tt-dados EXCLUSIVE-LOCK:
     DELETE tt-dados.
 END.

 CREATE tt-dados.
 ASSIGN tt-dados.i-cod-emit       = emitente.cod-emit
        tt-dados.fi-periodo       = STRING(MONTH(fi-dt-inicial), "99") + STRING(YEAR(fi-dt-inicial), "9999") 
        tt-dados.da-data-conver   = fi-dt-inicial 
        tt-dados.tipo-consulta    = 1
        tt-dados.cod-versao-integ = 001
        tt-dados.l-vid-rel        = YES.

 RUN crp/crapi011.P (INPUT  TABLE tt-dados,
                     OUTPUT TABLE tt-estatistica,
                     OUTPUT TABLE tt-erro).

 FIND FIRST tt-estatistica EXCLUSIVE-LOCK NO-ERROR.

 /* Compras */
 FIND estatist WHERE
      estatist.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
 IF AVAIL estatist THEN DO:
    /* Maior Compra */
    ASSIGN fi-vlr-maior-compra:SCREEN-VALUE = STRING(estatist.vl-maior-tit)
           fi-mes-maior-compra:SCREEN-VALUE = STRING(MONTH(estatist.dt-maior-tit), "99")
           fi-ano-maior-compra:SCREEN-VALUE = STRING(YEAR(estatist.dt-maior-tit), "9999").
    /* Ultima Compra */
    ASSIGN fi-dt-ult-nf:SCREEN-VALUE  = STRING(estatist.dt-ult-tit)
           fi-vlr-ult-nf:SCREEN-VALUE = STRING(estatist.vl-ult-tit).
 END.
 ASSIGN fi-mes-maior-acum:SCREEN-VALUE = STRING(MONTH(fi-dt-inicial), "99")
        fi-ano-maior-acum:SCREEN-VALUE = STRING(YEAR(fi-dt-inicial), "9999")
        fi-vendas-acumuladas:SCREEN-VALUE = STRING(tt-estatistica.fi-vendas-acumuladas).


 ASSIGN i-tot-titulos   = 0
        de-vlr-titulos  = 0
        i-tit-quitados  = 0
        de-vlr-quitados = 0
        i-tit-a-quitar  = 0
        de-vlr-a-quitar = 0.
 /* Dados dos Titulos de Clientes */
 FOR EACH titulo WHERE
          titulo.cod-emitente = emitente.cod-emit AND
          titulo.cod-estabel  = '2' NO-LOCK.

     IF titulo.cod-esp  <> 'DP' THEN NEXT. 

     /* Todos os Titulos */
      ASSIGN i-tot-titulos  = i-tot-titulos  + 1
             de-vlr-titulos = de-vlr-titulos + titulo.vl-original.
     /* Titulos Quitados */
     IF titulo.dt-vencimen < TODAY AND titulo.vl-saldo = 0 THEN
        ASSIGN i-tit-quitados  = i-tit-quitados  + 1
               de-vlr-quitados = de-vlr-quitados + titulo.vl-original.
     /* Titulos a Quitar */
     IF titulo.vl-saldo <> 0 THEN 
         ASSIGN i-tit-a-quitar  = i-tit-a-quitar + 1
                de-vlr-a-quitar = de-vlr-a-quitar + titulo.vl-original.
 END.
 ASSIGN fi-total-titulos:SCREEN-VALUE    = STRING(i-tot-titulos)
        fi-vlr-tot-titulos:SCREEN-VALUE  = STRING(de-vlr-titulos)
        fi-qtd-tit-quitados:SCREEN-VALUE = STRING(i-tit-quitados)   
        fi-vlr-tit-quitados:SCREEN-VALUE = STRING(de-vlr-quitados)
        fi-qtd-tit-a-quitar:SCREEN-VALUE = STRING(i-tit-a-quitar)  
        fi-vlr-tit-a-quitar:SCREEN-VALUE = STRING(de-vlr-a-quitar).

 ASSIGN i-tot-cheques          = 0
        de-vlr-total-chq       = 0
        i-qtd-chq-compensado   = 0
        de-vlr-chq-compensado  = 0
        i-qtd-chq-a-compensar  = 0
        de-vlr-chq-a-compensar = 0.
 /* Dados dos Cheques de Clientes */
 FOR EACH cheque WHERE
          cheque.cod-emitente = emitente.cod-emit NO-LOCK.
      ASSIGN i-tot-cheques = i-tot-cheques + 1
             de-vlr-total-chq = de-vlr-total-chq + cheque.vl-cheque.
      IF cheque.situacao-cheque = 1  THEN /* Pendente */ 
         ASSIGN i-qtd-chq-a-compensar  = i-qtd-chq-a-compensar  + 1
                de-vlr-chq-a-compensar = de-vlr-chq-a-compensar + cheque.vl-cheque.
      ELSE
         ASSIGN i-qtd-chq-compensado  = i-qtd-chq-compensado  + 1
                de-vlr-chq-compensado = de-vlr-chq-compensado + cheque.vl-cheque.
 END.
 ASSIGN fi-qtd-total-chq:SCREEN-VALUE       = STRING(i-tot-cheques)
        fi-vlr-total-chq:SCREEN-VALUE       = STRING(de-vlr-total-chq)
        fi-qtd-chq-compensado:SCREEN-VALUE  = STRING(i-qtd-chq-compensado)   
        fi-vlr-chq-compensado:SCREEN-VALUE  = STRING(de-vlr-chq-compensado)
        fi-qtd-chq-a-compensar:SCREEN-VALUE = STRING(i-qtd-chq-a-compensar)   
        fi-vlr-chq-a-compensar:SCREEN-VALUE = STRING(de-vlr-chq-a-compensar).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-dados-old w-digita 
PROCEDURE pi-dados-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR l-tit-reab-dev AS LOG.

DO WITH FRAME {&FRAME-NAME}:

    /* O c¢digo existente para tratamento de cheques e duplicatas foi movido para AS duas procedures abaixo */
    RUN pi-trata-cheques.
    RUN pi-trata-duplicatas.

     /* Devolucao Total */
     FOR EACH tt-estabelec-dest NO-LOCK,
         EACH mov-tit FIELDS(cod-emitente 
                             dt-emissao 
                             transacao 
                             tipo 
                             ep-codigo 
                             cod-estabel
                             vl-baixa) WHERE 
              mov-tit.ep-codigo    = tt-estabelec-dest.ep-codigo AND
              mov-tit.cod-emitente = emitente.cod-emitente  AND 
              mov-tit.dt-trans    >= DATE(fi-dt-inicial:SCREEN-VALUE) NO-LOCK:

          IF mov-tit.cod-estabel <> tt-estabelec-dest.cod-estabel  THEN NEXT.

          IF mov-tit.transacao <> 3 THEN NEXT.

          IF mov-tit.tipo <> 1 THEN NEXT.

           ASSIGN  d-tot-devolucao = d-tot-devolucao + mov-tit.vl-baixa.

     END. /* each mov-tit */
                                                                                       
    /* Busca duplicatas substitutas */
    FOR EACH nf-dupl-cr WHERE 
             nf-dupl-cr.cod-emitente = emitente.cod-emitente NO-LOCK, 
        FIRST tt-estabelec-dest NO-LOCK WHERE
              tt-estabelec-dest.cod-estabel = nf-dupl-cr.cod-estabel
      BREAK BY nf-dupl-cr.cod-estab-d 
            BY nf-dupl-cr.cod-esp-d
            BY nf-dupl-cr.nr-docto-d
            BY nf-dupl-cr.serie-d 
            BY nf-dupl-cr.parcela-d: 

      IF FIRST-OF(nf-dupl-cr.cod-estab-d) OR
         FIRST-OF(nf-dupl-cr.cod-esp-d  ) OR
         FIRST-OF(nf-dupl-cr.nr-docto-d ) OR
         FIRST-OF(nf-dupl-cr.serie-d    ) OR
         FIRST-OF(nf-dupl-cr.parcela-d )  THEN DO:

         FOR FIRST b-titulo 
              FIELDS (cod-estabel 
                                                                                        /***DMJGD001.sn***/
                      ep-codigo
                                                                                        /***DMJGD001.en***/
                      cod-esp 
                      serie 
                      tipo
                      nr-docto 
                      parcela
                      dt-emissao
                      dt-ult-pagto
                      dt-vencimen
                      vl-original
                      vl-saldo) WHERE
                                                                                        /***DMJGD001.sn***/
                    b-titulo.ep-codigo   = nf-dupl-cr.ep-codigo   AND
                                                                                        /***DMJGD001.en***/
                    b-titulo.cod-estabel = nf-dupl-cr.cod-estab-d AND
                    b-titulo.cod-esp     = nf-dupl-cr.cod-esp-d   AND
                    b-titulo.serie       = nf-dupl-cr.serie-d     AND
                    b-titulo.nr-docto    = nf-dupl-cr.nr-docto-d  AND
                    b-titulo.parcela     = nf-dupl-cr.parcela-d   NO-LOCK:
    
              IF b-titulo.dt-emissao < DATE(fi-dt-inicial:SCREEN-VALUE) THEN NEXT.

              ASSIGN l-tit-reab-dev = NO.

              IF (b-titulo.cod-esp  = 'DP'  OR 
                  b-titulo.cod-esp  = 'DT'  OR
                  b-titulo.cod-esp  = 'DG'  OR
                                                                                        /***DMJGD002.sn***/
                  b-titulo.cod-esp  = 'CD'  OR
                                                                                        /***DMJGD002.en***/
                  b-titulo.cod-esp  = 'NP') AND
                  b-titulo.vl-saldo <> 0  THEN DO:

                  IF CAN-FIND(FIRST mov-tit OF b-titulo WHERE mov-tit.transacao = 13 /* AVA */ AND SUBSTRING(mov-tit.char-1, 1, 6) = "DEV_CH") THEN DO:
                                                                                        /***DMJG004.en***/
                      ASSIGN d-sld-tit-reab-dev = d-sld-tit-reab-dev + b-titulo.vl-saldo.
                      ASSIGN l-tit-reab-dev = YES.
                  END.
              END.
                                                                                        /***DMJG003.en***/
              /* Prorrogadas */
              IF b-titulo.dt-vencimen > TODAY THEN 
                  ASSIGN i-qt-dup-prorrog = i-qt-dup-prorrog + 1
                         de-vlr-dup-prorrog = de-vlr-dup-prorrog + b-titulo.vl-original.
    
              /* Quitadas */
              IF b-titulo.dt-vencimen > TODAY AND
                 b-titulo.vl-saldo = 0 THEN
                  ASSIGN i-duplic-quitada = i-duplic-quitada + 1
                         d-duplic-quitada = d-duplic-quitada + b-titulo.vl-original.
    
              /* A Quitar */
              IF b-titulo.dt-vencimen > TODAY AND 
                 b-titulo.vl-saldo    <> 0 THEN
                  ASSIGN i-duplic-quitar = i-duplic-quitar + 1
                         d-duplic-quitar = d-duplic-quitar + b-titulo.vl-original.
                                                                                        /***DMJG003.sn***/
              IF NOT l-tit-reab-dev THEN DO:
                                                                                        /***DMJG003.en***/
                  /* Totais (compensar e atraso) */
                  IF (b-titulo.cod-esp = 'DP'  OR 
                      b-titulo.cod-esp = 'DT'  OR
                      b-titulo.cod-esp = 'DG'  OR
                      b-titulo.cod-esp = 'PD'  OR                                       /***FOMM.NEW***/
                                                                                        /***DMJGD002.sn***/
                      b-titulo.cod-esp = 'CD'  OR
                                                                                        /***DMJGD002.en***/
                      b-titulo.cod-esp = 'NP') THEN DO:

                      IF b-titulo.dt-vencimen >=  TODAY AND
                         b-titulo.vl-saldo    <> 0      THEN
                         ASSIGN d-tot-compensar        = d-tot-compensar + b-titulo.vl-saldo
                                d-tot-titulo-compensar = d-tot-titulo-compensar + b-titulo.vl-saldo.
        
                      IF b-titulo.vl-saldo <> 0 AND 
                         b-titulo.dt-vencimen < TODAY THEN
                        d-tot-atraso = d-tot-atraso + b-titulo.vl-saldo.
                  END.
                                                                                        /***DMJG003.sn***/
              END.
                                                                                        /***DMJG003.en***/
             /* Devolucao Total */
             IF b-titulo.cod-esp = 'DM' THEN
                 d-tot-devolucao = d-tot-devolucao + b-titulo.vl-original.
    
             /* Juros Pendentes */
             IF b-titulo.tipo     = 6 AND
                titulo.cod-esp    <> "PD" AND                                           /***FOMM.NEW***/
                b-titulo.vl-saldo <> 0 THEN
                 d-tot-juros-pend = d-tot-juros-pend + b-titulo.vl-saldo.
    
             /* Juros Quitados */
             IF b-titulo.tipo      = 6 AND
                b-titulo.vl-saldo  = 0 THEN
                 d-tot-juros-quit = d-tot-juros-quit + b-titulo.vl-original.
    
             /* Total quitado */
             IF (b-titulo.cod-esp  = 'DP'  OR 
                 b-titulo.cod-esp  = 'DT'  OR
                 b-titulo.cod-esp  = 'DG'  OR
                 b-titulo.cod-esp  = "PD"  OR                                           /***FOMM.NEW***/
                 b-titulo.cod-esp  = "AD"  OR                                           /***FOMM.NEW***/

                                                                                        /***DMJGD002.sn***/
                 b-titulo.cod-esp  = 'CD'  OR
                                                                                        /***DMJGD002.en***/
                                                                                        /***DMJG999.so***
                 b-titulo.cod-esp  = 'NP'  AND
                 b-titulo.vl-saldo <> b-titulo.vl-original) THEN 
                                                                                         ***DMJG999.eo***/                 
                                                                                        /***DMJG999.sn***/
                 b-titulo.cod-esp  = 'NP' )  AND
                 b-titulo.vl-saldo <> b-titulo.vl-original THEN 
                                                                                        /***DMJG999.en***/                 
                   ASSIGN d-tot-quitado = d-tot-quitado + (b-titulo.vl-original - b-titulo.vl-saldo)
                          d-titulo-quitado = d-titulo-quitado + (b-titulo.vl-original - b-titulo.vl-saldo).
    
             /* Titulos Liquidados c/ Atraso - 1 */
             IF (b-titulo.cod-esp = 'DP'  OR 
                 b-titulo.cod-esp = 'DT'  OR
                 b-titulo.cod-esp = 'DG'  OR
                                                                                        /***DMJGD002.sn***/
                 b-titulo.cod-esp = 'CD'  OR
                                                                                        /***DMJGD002.en***/
                 b-titulo.cod-esp = 'NP') AND 
                 b-titulo.vl-saldo = 0    AND
                 b-titulo.dt-ult-pagto > b-titulo.dt-vencimen  THEN DO:
    
                 IF NOT CAN-FIND(FIRST mov-tit OF b-titulo WHERE
                                       mov-tit.baixa-subs AND
                                       mov-tit.transacao = 2 ) THEN
                     d-titulo-quitado-atraso = d-titulo-quitado-atraso + b-titulo.vl-original.
             END.
    
             /* Titulos Liquidados Cartorio */
             IF (b-titulo.cod-esp = 'DP'  OR 
                 b-titulo.cod-esp = 'DT'  OR
                 b-titulo.cod-esp = 'DG'  OR
                                                                                        /***DMJGD002.sn***/
                 b-titulo.cod-esp = 'CD'  OR
                                                                                        /***DMJGD002.en***/
                 b-titulo.cod-esp = 'NP') AND 
                 b-titulo.vl-saldo = 0    THEN DO:
    
                IF CAN-FIND(FIRST his-tit OF b-titulo WHERE
                                  his-tit.historico BEGINS 'Liquidacao em cartorio') THEN
                    d-titulo-quitado-cartorio = d-titulo-quitado-cartorio + b-titulo.vl-original.
             END. 
         END. /* for first b-titulo */
      END. /* first of nf-dupl-cr.nr-docto */
    END. /* each nf-dupl-cr */ 
END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-cheques w-digita 
PROCEDURE pi-trata-cheques :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tt-cheques WHERE
             tt-cheques.cod-emitente = emitente.cod-emit.

        ASSIGN d-tot-compensar = d-tot-compensar + tt-cheques.vl-cheque
               d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + tt-cheques.vl-cheque.

        IF tt-cheques.tipo-cheque = 1 AND
          (tt-cheques.situacao-cheque = 1  /* Pendente */   OR
           tt-cheques.situacao-cheque = 2  /* Depositado */ OR
           tt-cheques.situacao-cheque = 5  /* Devolvido */  OR
           tt-cheques.situacao-cheque = 7  /* Descontado */ OR
           tt-cheques.situacao-cheque = 8) /* Compensado */ THEN
            d-cheque-proprio = d-cheque-proprio + tt-cheques.vl-cheque.

        IF tt-cheques.tipo-cheque = 2 AND
          (tt-cheques.situacao-cheque = 1  /* Pendente */   OR
           tt-cheques.situacao-cheque = 2  /* Depositado */ OR
           tt-cheques.situacao-cheque = 5  /* Devolvido */  OR
           tt-cheques.situacao-cheque = 7  /* Descontado */ OR
           tt-cheques.situacao-cheque = 8) /* Compensado */ THEN
            d-bordero = d-bordero + tt-cheques.vl-cheque.


        /* Total Pendentes sem devolucao */
        IF NOT tt-cheques.devolvido             AND 
            tt-cheques.situacao-cheque = 1 /* Pendente */ THEN
            ASSIGN d-tot-compensar = d-tot-compensar + tt-cheques.vl-cheque
                   d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + tt-cheques.vl-cheque.

        IF NOT tt-cheques.devolvido   AND 
           tt-cheques.situacao-cheque = 2 /* Depositado */ AND 
           tt-cheques.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
            ASSIGN d-tot-compensar = d-tot-compensar + tt-cheques.vl-cheque
                   d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + tt-cheques.vl-cheque.
 
        IF NOT tt-cheques.devolvido   AND 
           tt-cheques.situacao-cheque = 3 /* Cau‡Æo */ AND 
           tt-cheques.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
            ASSIGN d-tot-compensar = d-tot-compensar + tt-cheques.vl-cheque
                   d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + tt-cheques.vl-cheque.
  
        /* Pendentes */
        IF (tt-cheques.situacao-cheque       = 1 /* Pendente */   OR 
            tt-cheques.situacao-cheque       = 5) /* Devolvido */ THEN
            ASSIGN d-tot-quitado         = d-tot-quitado - tt-cheques.vl-cheque 
                   d-tot-cheque-aberto   = d-tot-cheque-aberto + tt-cheques.vl-cheque.

        IF tt-cheques.devolvido AND
           tt-cheques.situacao-cheque = 2 /* Depositado */ AND 
           tt-cheques.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
            ASSIGN d-tot-cheque-aberto   = d-tot-cheque-aberto + tt-cheques.vl-cheque.

        IF tt-cheques.situacao-cheque = 2 /* Depositado */ AND 
           tt-cheques.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
            ASSIGN d-tot-quitado = d-tot-quitado - tt-cheques.vl-cheque.

        IF tt-cheques.situacao-cheque = 3 /* Cau‡Æo */ AND 
           tt-cheques.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
            ASSIGN d-tot-quitado = d-tot-quitado - tt-cheques.vl-cheque.

        /* Total Pendentes devolvidos */
        IF tt-cheques.devolvido             AND 
           (tt-cheques.situacao-cheque = 1 /* Pendente */   OR 
            tt-cheques.situacao-cheque = 5) /* Devolvido */ THEN
            ASSIGN d-tot-cheque-devolv-pendente = d-tot-cheque-devolv-pendente + tt-cheques.vl-cheque.

        IF tt-cheques.devolvido             AND 
           tt-cheques.situacao-cheque = 2   /* Depositado */   AND 
           tt-cheques.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
            ASSIGN d-tot-cheque-devolv-pendente = d-tot-cheque-devolv-pendente + tt-cheques.vl-cheque.

        /* Quitados */
        IF tt-cheques.devolvido AND
           ( (tt-cheques.situacao-cheque = 2 AND tt-cheques.dt-vencimento <= TODAY ) OR
            tt-cheques.situacao-cheque = 6 /* Cancelado  */ OR
            tt-cheques.situacao-cheque = 7 /* Descontado */ OR
            tt-cheques.situacao-cheque = 8)/* Compensado */ THEN
            d-tot-cheque-quitado = d-tot-cheque-quitado + tt-cheques.vl-cheque.
        
        /* Prorrogados */
        IF tt-cheques.dt-vencimento > TODAY THEN 
            ASSIGN i-cheque-prorrogado = i-cheque-prorrogado + 1
                   d-cheque-prorrogado = d-cheque-prorrogado + tt-cheques.vl-cheque.
        
        /* Compensados */
        IF tt-cheques.dt-vencimento > tt-cheques.data-2 AND
           (tt-cheques.situacao-cheque = 2  /* Depositado */ OR
            tt-cheques.situacao-cheque = 7  /* Descontado */ OR
            tt-cheques.situacao-cheque = 8) /* Compensado */ THEN
            ASSIGN i-cheque-compensado = i-cheque-compensado + 1
                   d-cheque-compensado = d-cheque-compensado + tt-cheques.vl-cheque.
        
        /* A Compensar */
        IF tt-cheques.dt-vencimento > tt-cheques.data-2 AND 
           (tt-cheques.situacao-cheque = 1  /* Pendente */   OR
            tt-cheques.situacao-cheque = 5)  /* Devolvido */ THEN          
            ASSIGN i-cheque-compensar  = i-cheque-compensar + 1
                   d-cheque-compensar = d-cheque-compensar + tt-cheques.vl-cheque.
    END. /* each cheque */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-duplicatas w-digita 
PROCEDURE pi-trata-duplicatas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                                                                                       
DEF VAR l-tit-reab-dev AS LOG.
                                                                                       
DO WITH FRAME {&FRAME-NAME}:
   FOR EACH tt-titulos WHERE 
            tt-titulos.cod-emitente = emitente.cod-emit NO-LOCK.

       ASSIGN l-tit-reab-dev = NO.

       /* IF tt-titulos.dt-emissao < DATE(fi-dt-inicial:SCREEN-VALUE) THEN NEXT. */

       /* Ignora duplicatas substituidas */
       IF CAN-FIND(FIRST mov-tit OF tt-titulos WHERE mov-tit.baixa-subs) THEN NEXT.

       IF (tt-titulos.cod-esp  = 'DP'  OR 
           tt-titulos.cod-esp  = 'DT'  OR
           tt-titulos.cod-esp  = 'DG'  OR
           tt-titulos.cod-esp  = 'CD'  OR
           tt-titulos.cod-esp  = 'NP') AND
           tt-titulos.vl-saldo <> 0  THEN DO:
           IF CAN-FIND(FIRST mov-tit OF tt-titulos WHERE mov-tit.transacao = 13 /* AVA */ 
                       AND SUBSTRING(mov-tit.char-1, 1, 6) = "DEV_CH") THEN DO:
                                                                                         
               ASSIGN d-sld-tit-reab-dev = d-sld-tit-reab-dev + tt-titulos.vl-saldo
                      l-tit-reab-dev     = YES.
           END.
       END.
                                                                                 
       /* Prorrogadas */
       IF tt-titulos.dt-vencimen > TODAY THEN 
           ASSIGN i-qt-dup-prorrog = i-qt-dup-prorrog + 1
                  de-vlr-dup-prorrog = de-vlr-dup-prorrog + tt-titulos.vl-original.
        
       /* Quitadas */
       IF tt-titulos.dt-vencimen > TODAY AND
          tt-titulos.vl-saldo = 0 THEN
           ASSIGN i-duplic-quitada = i-duplic-quitada + 1
                  d-duplic-quitada = d-duplic-quitada + tt-titulos.vl-original.
    
       /* A Quitar */
       IF tt-titulos.dt-vencimen > TODAY AND 
          tt-titulos.vl-saldo    <> 0 THEN
           ASSIGN i-duplic-quitar = i-duplic-quitar + 1
                  d-duplic-quitar = d-duplic-quitar + tt-titulos.vl-original.
                                                                    
       IF NOT l-tit-reab-dev THEN DO:
          /* Totais (compensar e atraso) */
          IF (tt-titulos.cod-esp = 'DP'  OR 
              tt-titulos.cod-esp = 'DT'  OR
              tt-titulos.cod-esp = 'DG'  OR
              tt-titulos.cod-esp = "PD"  OR                                               
                                                                                      
              tt-titulos.cod-esp = 'CD'  OR
                                                                                      
              tt-titulos.cod-esp = 'NP') THEN DO:
              IF tt-titulos.dt-vencimen >=  TODAY AND
                 tt-titulos.vl-saldo    <> 0      THEN
                 ASSIGN d-tot-compensar        = d-tot-compensar + tt-titulos.vl-saldo
                        d-tot-titulo-compensar = d-tot-titulo-compensar + tt-titulos.vl-saldo.
    
              IF tt-titulos.vl-saldo <> 0 AND 
                 tt-titulos.dt-vencimen < TODAY THEN
                d-tot-atraso = d-tot-atraso + tt-titulos.vl-saldo.
          END.
                                                                                      
       END.
                                                                                      
         /* Devolucao Total */
         IF tt-titulos.cod-esp = 'DM' THEN
             d-tot-devolucao = d-tot-devolucao + tt-titulos.vl-original.
    
         /* Juros Pendentes */
         IF tt-titulos.tipo     = 6 AND
            tt-titulos.cod-esp  <> "PD" AND                                                    
            tt-titulos.vl-saldo <> 0 THEN
             d-tot-juros-pend = d-tot-juros-pend + tt-titulos.vl-saldo.
        
         /* Juros Quitados */
         IF tt-titulos.tipo      = 6 AND
            tt-titulos.vl-saldo  = 0 THEN
             d-tot-juros-quit = d-tot-juros-quit + tt-titulos.vl-original.
    
         /*
         /* Total quitado */
         IF (tt-titulos.cod-esp  = 'DP'  OR 
             tt-titulos.cod-esp  = 'DT'  OR
             tt-titulos.cod-esp  = 'DG'  OR
             tt-titulos.cod-esp  = 'PD'  OR                                  
             tt-titulos.cod-esp  = 'AD'  OR                                  
             tt-titulos.cod-esp  = 'CD'  OR
             tt-titulos.cod-esp  = 'NP'  AND
             tt-titulos.vl-saldo <> tt-titulos.vl-original) THEN 
    
             tt-titulos.cod-esp  = 'NP')  AND
             tt-titulos.vl-saldo <> tt-titulos.vl-original THEN 
                                                                                                  
               ASSIGN d-tot-quitado = d-tot-quitado + (tt-titulos.vl-original - tt-titulos.vl-saldo)
                      d-tt-titulos-quitado = d-tt-titulos-quitado + (tt-titulos.vl-original - tt-titulos.vl-saldo).
         */
         
         /* tt-tituloss Liquidados c/ Atraso - 2 */
         IF (tt-titulos.cod-esp = 'DP'  OR 
             tt-titulos.cod-esp = 'DT'  OR
             tt-titulos.cod-esp = 'DG'  OR
                                                                                        
             tt-titulos.cod-esp = 'CD'  OR
                                                                                        
             tt-titulos.cod-esp = 'NP') AND 
             tt-titulos.vl-saldo = 0    AND
             tt-titulos.dt-ult-pagto > tt-titulos.dt-vencimen  THEN DO:
    
             IF NOT CAN-FIND(FIRST mov-tit OF tt-titulos WHERE
                                   mov-tit.baixa-subs AND
                                   mov-tit.transacao = 2 ) THEN
                 d-titulo-quitado-atraso = d-titulo-quitado-atraso + tt-titulos.vl-original.
         END.
    
         /* tt-tituloss Liquidados Cartorio */
         IF (tt-titulos.cod-esp = 'DP'  OR 
             tt-titulos.cod-esp = 'DT'  OR
             tt-titulos.cod-esp = 'DG'  OR
                                                                                          
             tt-titulos.cod-esp = 'CD'  OR
                                                                                          
             tt-titulos.cod-esp = 'NP') AND 
             tt-titulos.vl-saldo = 0    THEN DO:
    
            IF CAN-FIND(FIRST his-tit OF tt-titulos WHERE
                              his-tit.historico BEGINS 'Liquidacao em cartorio') THEN
                d-titulo-quitado-cartorio = d-titulo-quitado-cartorio + tt-titulos.vl-original.
         END.
     END. /* each tt-titulos */
END. /* do with frame */

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

