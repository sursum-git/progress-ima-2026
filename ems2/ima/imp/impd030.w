&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-consim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-consim 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.

** Altera‡äes: DMJG001 - Dataminas - JoÆo Gabriel Costa Rocha - 02/10/2003
**             Os cheques descontados, ou seja, aqueles que sÆo depositados
**             com data de vencimento (data prevista dep¢sito) futura, 
**             atualmente figuram no total de cheques depositados 
**            (tela IMPD030E - Detalhe Total Quitado), pois possuem o status 
**             de "Depositado". Por‚m estes cheques devem figurar como 
**             a compensar (campo Compensar da µrea Totais), passando a ser 
**             considerados como depositados somente quando sua data de 
**             vencimento for inferior ou igual … data corrente.              
**
**             DMJG002 - Dataminas - JoÆo Gabriel Costa Rocha - 02/10/2003
**             Os cheques caucionados, ou seja, aqueles que sÆo depositados
**             com data de vencimento (data prevista dep¢sito) futura e com  
**             situa‡Æo "Cau‡Æo", atualmente nÆo sÆo considerados pela an lise
**             de cr‚dito. Por‚m estes cheques devem figurar como  
**             a compensar (campo Compensar da µrea Totais), quando sua data de 
**             vencimento for maior que a data corrente, ou no total de cheques 
**             depositados da tela IMPD030E - Detalhe Total Quitado, quando sua 
**             data de vencimento for inferior ou igual … data corrente.              
**
**             DMJG003 - Dataminas - JoÆo Gabriel Costa Rocha - 02/10/2003
**             Os t¡tulos que sÆo reabertos em decorrˆncia da devolu‡Æo de cheques
**             sem condi‡Æo de representa‡Æo, irÆo figurar no total de cheques
**             devolvidos pendentes (µrea Devolu‡Æo Cheques). 
**             Todos os cheques serÆo baixados no portador 100, sendo que caso o
**             t¡tulo seja reaberto por devolu‡Æo de cheques, o sistema gera uma
**             transa‡Æo (mov-tit) do tipo AVA no portador 100.
**
**             DMJG004 - Dataminas - JoÆo Gabriel Costa Rocha - 24/11/2003
**             Altera‡Æo de conceito no item DMJG003, pois o AVA referente … 
**             reabertura do t¡tulo por devolu‡Æo de cheque podereria ser gera-
**             do em qualquer portador, e nÆo somente no 100. Desta forma, para
**             identificar o AVA deve ser considerado o campo mov-tit.char-1, que
**             tem suas primeiras 6 posi‡äes preenchidas com "DEV_CH" nestes casos.
**
**             DMJGD001 - Dataminas - JoÆo Gabriel Costa Rocha - 09/01/2004
**             Implanta‡Æo tratamento das informa‡äes por estabelecimento, sendo
**             permitida a consolida‡Æo dos dados de estabelecimentos distintos. 
**
**             DMJGD002 - Dataminas - JoÆo Gabriel Costa Rocha - 22/01/2004
**             Considerar tamb‚m os t¡tulos de esp‚cie "CD" referentes a cheques
**             devolvidos.                                                       
*******************************************************************************/
{include/i-prgvrs.i IMPD030 1.00.00.000}
{include/i-vrtab.i emitente}

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE tt_tit_prorrog LIKE tit_acr LABEL "TitProrrog".
DEF TEMP-TABLE tt_tit_prorrog_quitado LIKE tit_acr LABEL "TitProrrogQuitado".
DEF TEMP-TABLE tt_tit_prorrog_aberto LIKE tit_acr LABEL "TitProrrogAberto".

DEF TEMP-TABLE tt_tit_aberto LIKE tit_acr LABEL "TitAberto".
DEF TEMP-TABLE tt_tit_quitado LIKE tit_acr LABEL "TitQuitado".
DEF TEMP-TABLE tt_tit_atraso LIKE tit_acr LABEL "TitAtraso".
DEF TEMP-TABLE tt_tit_devol LIKE tit_acr LABEL "TitDevol".
DEF TEMP-TABLE tt_tit_compsar LIKE tit_acr LABEL "TitCompsar".
DEF TEMP-TABLE tt_tit_liq_atraso LIKE tit_acr LABEL "TitLiqAtraso".
DEF TEMP-TABLE tt_juros_pend LIKE tit_acr LABEL "JurosPend".

DEF TEMP-TABLE tt_ch_prorrog LIKE cheq_acr LABEL "ChqProrrog".
DEF TEMP-TABLE tt_ch_prorrog_compsado LIKE cheq_acr LABEL "ChqProrrogCompsado".
DEF TEMP-TABLE tt_ch_prorrog_compsar LIKE cheq_acr LABEL "ChqProrrogCompsar".

DEF TEMP-TABLE tt_cheque_compsar LIKE cheq_acr LABEL "ChqCompsar".
DEF TEMP-TABLE tt_cheque_compsado LIKE cheq_acr LABEL "ChqCompsdo".
DEF TEMP-TABLE tt_cheque_aberto LIKE cheq_acr LABEL "ChqAberto".
DEF TEMP-TABLE tt_cheque_pend LIKE cheq_acr LABEL "ChqPend".

DEF TEMP-TABLE tt-notas LIKE nota-fiscal LABEL "NotasSaida".

DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-estabelec-dest LIKE estabelec.

DEFINE NEW GLOBAL SHARED VAR i-cod-emitente LIKE emitente.cod-emitente NO-UNDO. 
DEFINE NEW GLOBAL SHARED VAR h-impd030 AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-win-sel-estab AS WIDGET-HANDLE NO-UNDO.
                                                                                        /***DMJGD001.en***/
DEF BUFFER b-titulo FOR titulo.
DEF BUFFER empresa FOR mgadm.empresa.                                                                                       
DEF BUFFER cheque FOR movadm.cheque.

    /***DMJGD001.so***
DEF VAR i-empresa-ima   AS INTEGER NO-UNDO.
DEF VAR i-empresa-inter AS INTEGER NO-UNDO.
                                                                                         ***DMJGD001.eo***/
/* Local Variable Definitions ---                                       */
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
DEF VAR i-duplic-prorrogada   AS INTEGER NO-UNDO.
DEF VAR d-duplic-prorrogada   AS DECIMAL NO-UNDO.
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
DEF VAR i-num-tit-acr-ems2        AS INTEGER INIT 100000.   
DEF VAR i-num_id_cheq_acr         AS INTEGER INIT 100000.
DEF VAR l-tit-reab-dev            AS LOG.
DEF VAR d-sld-tit-reab-dev        AS DEC NO-UNDO.
DEF VAR h-programa AS HANDLE.
                                                                                        /***DMJG003.en***/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-consim
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-12 RECT-13 RECT-14 RECT-15 RECT-16 ~
RECT-17 RECT-18 RECT-5 RECT-6 RECT-7 RECT-8 RECT-9 rt-button bt-param ~
dt-inicial bt-det-dupl-prorrog bt-det-chq-prorrog bt-det-tot-aberto ~
bt-det-tot-quitado bt-det-dupl-quit bt-det-chq-comp bt-det-tot-atraso ~
bt-det-tot-devol bt-det-dupl-a-venc bt-det-chq-a-comp bt-det-tot-comp ~
bt-det-juros-pend bt-det-chq-dev-pend bt-det-dupl-quit-atraso 
&Scoped-Define DISPLAYED-OBJECTS dt-inicial c-estabs-sel ind-cre-cli ~
d-total-notas i-num-duplic-prorrogado i-num-cheque-prorrogado ~
d-total-aberto d-limite d-tot-duplic-prorrogado d-tot-cheque-prorrogado ~
d-total-quitado dt-limite-ini i-num-duplic-compensado ~
i-num-cheque-compensado d-total-atraso d-limite-ad d-tot-duplic-compensado ~
d-tot-cheque-compensado d-total-devolucao dt-limite-fim i-num-duplic-quitar ~
i-num-cheque-compensar d-total-compensar d-saldo d-tot-duplic-quitar ~
d-tot-cheque-compensar d-juros-pendentes i-mes-compra i-ano-compra ~
i-mes-debito i-ano-debito dt-ultima-nota Categ d-juros-quitados ~
d-tot-compra d-tot-debito d-ultima-nota d-pendentes d-tot-titulo-quitado ~
d-tot-cheque-proprio d-quitados d-tot-titulo-quitado-atraso d-tot-bordero ~
d-acumulado d-tot-titulo-quitado-cartorio 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-consim AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-arquivo 
       MENU-ITEM mi-primeiro    LABEL "&Primeiro"      ACCELERATOR "CTRL-HOME"
       MENU-ITEM mi-anterior    LABEL "An&terior"      ACCELERATOR "CTRL-CURSOR-LEFT"
       MENU-ITEM mi-proximo     LABEL "Pr&¢ximo"       ACCELERATOR "CTRL-CURSOR-RIGHT"
       MENU-ITEM mi-ultimo      LABEL "&éltimo"        ACCELERATOR "CTRL-END"
       MENU-ITEM mi-va-para     LABEL "&V  para"       ACCELERATOR "CTRL-T"
       MENU-ITEM mi-pesquisa    LABEL "Pes&quisa"      ACCELERATOR "CTRL-F5"
       RULE
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-cadastro MENUBAR
       SUB-MENU  mi-arquivo     LABEL "&Arquivo"      
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navega AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q01im030 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v03impd030 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/ii-chck1.bmp":U NO-FOCUS
     LABEL "Button 7" 
     SIZE 3 BY .88.

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

DEFINE BUTTON bt-param 
     IMAGE-UP FILE "image/ii-param.bmp":U
     LABEL "Seleciona Estabelecimentos An lise Cr‚dito" 
     SIZE 4.29 BY 1.25 TOOLTIP "Seleciona estabelecimentos para an lise de cr‚dito".

DEFINE VARIABLE c-estabs-sel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estab's An lise" 
     VIEW-AS FILL-IN 
     SIZE 27.57 BY .88 NO-UNDO.

DEFINE VARIABLE Categ AS CHARACTER FORMAT "X(2)":U 
     LABEL "Categ." 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

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

DEFINE VARIABLE d-limite AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-limite-ad AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-pendentes AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Pendentes" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .88 NO-UNDO.

DEFINE VARIABLE d-quitados AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Quitados" 
     VIEW-AS FILL-IN 
     SIZE 15.14 BY .88 NO-UNDO.

DEFINE VARIABLE d-saldo AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-bordero AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cheque Terceiro" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-cheque-compensado AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Compensado" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-cheque-compensar AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Compensar" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-cheque-proprio AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Cheque Pr¢prio" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-cheque-prorrogado AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-compra AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-debito AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-duplic-compensado AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Quitado" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-duplic-prorrogado AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE d-tot-duplic-quitar AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor a Quitar" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

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
     LABEL "Ch Compsar" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .88 NO-UNDO.

DEFINE VARIABLE d-total-devolucao AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Devolu‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-total-notas AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-total-quitado AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Quitado" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .88 NO-UNDO.

DEFINE VARIABLE d-ultima-nota AS DECIMAL FORMAT "->>,>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Valor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE dt-inicial AS DATE FORMAT "99/99/9999":U INITIAL 10/01/02 
     LABEL "Data In¡cio An lise" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE dt-limite-fim AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE dt-limite-ini AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE dt-ultima-nota AS DATE FORMAT "99/99/9999":U 
     LABEL "Data" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE i-ano-compra AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .88 NO-UNDO.

DEFINE VARIABLE i-ano-debito AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .88 NO-UNDO.

DEFINE VARIABLE i-mes-compra AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Ref." 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE VARIABLE i-mes-debito AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Ref." 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-num-cheque-compensado AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Compensada" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-num-cheque-compensar AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Compensar" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-num-cheque-prorrogado AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Total" 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .88 NO-UNDO.

DEFINE VARIABLE i-num-duplic-compensado AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Quitada" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE i-num-duplic-prorrogado AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd Total" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE i-num-duplic-quitar AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Qtd a Quitar" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE ind-cre-cli AS CHARACTER FORMAT "x(40)":U 
     LABEL "Situa‡Æo Cliente" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18.57 BY 3.25.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY 3.25.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63.43 BY 3.71.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.43 BY 6.75.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 3.25.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 110.43 BY 1.5.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17 BY 3.25.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.57 BY 6.75.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 20 BY 3.25.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.43 BY 3.71.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29.29 BY 6.75.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27.43 BY 6.75.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 110.43 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     bt-confirma AT ROW 4.46 COL 29.86 NO-TAB-STOP 
     bt-param AT ROW 1.33 COL 28.14
     dt-inicial AT ROW 4.5 COL 17.43 COLON-ALIGNED
     c-estabs-sel AT ROW 4.5 COL 45.86 COLON-ALIGNED
     ind-cre-cli AT ROW 4.5 COL 89 COLON-ALIGNED
     d-total-notas AT ROW 6.5 COL 14.14 COLON-ALIGNED NO-LABEL
     i-num-duplic-prorrogado AT ROW 6.5 COL 37.43 COLON-ALIGNED
     bt-det-dupl-prorrog AT ROW 6.5 COL 52.29
     i-num-cheque-prorrogado AT ROW 6.5 COL 68.86 COLON-ALIGNED
     bt-det-chq-prorrog AT ROW 6.5 COL 82.29
     d-total-aberto AT ROW 6.92 COL 94.14 COLON-ALIGNED
     bt-det-tot-aberto AT ROW 6.96 COL 108.29
     d-limite AT ROW 7.46 COL 14.14 COLON-ALIGNED NO-LABEL
     d-tot-duplic-prorrogado AT ROW 7.5 COL 37.43 COLON-ALIGNED
     d-tot-cheque-prorrogado AT ROW 7.5 COL 68.86 COLON-ALIGNED
     d-total-quitado AT ROW 7.92 COL 94.14 COLON-ALIGNED
     bt-det-tot-quitado AT ROW 7.92 COL 108.29
     dt-limite-ini AT ROW 8.42 COL 14.14 COLON-ALIGNED NO-LABEL
     i-num-duplic-compensado AT ROW 8.5 COL 37.43 COLON-ALIGNED
     bt-det-dupl-quit AT ROW 8.5 COL 52.29
     i-num-cheque-compensado AT ROW 8.5 COL 68.86 COLON-ALIGNED
     bt-det-chq-comp AT ROW 8.5 COL 82.29
     d-total-atraso AT ROW 8.92 COL 94.14 COLON-ALIGNED
     bt-det-tot-atraso AT ROW 8.92 COL 108.29
     d-limite-ad AT ROW 9.38 COL 16.14 NO-LABEL
     d-tot-duplic-compensado AT ROW 9.5 COL 37.43 COLON-ALIGNED
     d-tot-cheque-compensado AT ROW 9.5 COL 68.86 COLON-ALIGNED
     d-total-devolucao AT ROW 9.92 COL 94.14 COLON-ALIGNED
     bt-det-tot-devol AT ROW 9.92 COL 108.29
     dt-limite-fim AT ROW 10.33 COL 14.14 COLON-ALIGNED NO-LABEL
     i-num-duplic-quitar AT ROW 10.5 COL 37.43 COLON-ALIGNED
     bt-det-dupl-a-venc AT ROW 10.5 COL 52.29
     i-num-cheque-compensar AT ROW 10.5 COL 68.86 COLON-ALIGNED
     bt-det-chq-a-comp AT ROW 10.5 COL 82.29
     d-total-compensar AT ROW 10.92 COL 94.29 COLON-ALIGNED
     bt-det-tot-comp AT ROW 10.92 COL 108.29
     d-saldo AT ROW 11.25 COL 14.14 COLON-ALIGNED NO-LABEL
     d-tot-duplic-quitar AT ROW 11.5 COL 37.43 COLON-ALIGNED
     d-tot-cheque-compensar AT ROW 11.5 COL 68.86 COLON-ALIGNED
     d-juros-pendentes AT ROW 13.75 COL 72.86 COLON-ALIGNED
     bt-det-juros-pend AT ROW 13.75 COL 88.29
     i-mes-compra AT ROW 14 COL 5.43 COLON-ALIGNED
     i-ano-compra AT ROW 14 COL 9.43 COLON-ALIGNED NO-LABEL
     i-mes-debito AT ROW 14 COL 24.86 COLON-ALIGNED
     i-ano-debito AT ROW 14 COL 28.86 COLON-ALIGNED NO-LABEL
     dt-ultima-nota AT ROW 14 COL 45.14 COLON-ALIGNED
     Categ AT ROW 14.25 COL 101 COLON-ALIGNED
     d-juros-quitados AT ROW 14.79 COL 72.86 COLON-ALIGNED
     d-tot-compra AT ROW 15 COL 5.29 COLON-ALIGNED
     d-tot-debito AT ROW 15 COL 24.86 COLON-ALIGNED
     d-ultima-nota AT ROW 15 COL 45.14 COLON-ALIGNED
     d-pendentes AT ROW 16.88 COL 82 COLON-ALIGNED
     bt-det-chq-dev-pend AT ROW 16.88 COL 99.43
     d-tot-titulo-quitado AT ROW 17 COL 20.14 COLON-ALIGNED
     d-tot-cheque-proprio AT ROW 17.5 COL 48.86 COLON-ALIGNED
     d-quitados AT ROW 17.83 COL 82 COLON-ALIGNED
     d-tot-titulo-quitado-atraso AT ROW 18 COL 20.14 COLON-ALIGNED
     bt-det-dupl-quit-atraso AT ROW 18.04 COL 35.43
     d-tot-bordero AT ROW 18.5 COL 48.86 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.29 BY 21.13
         FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME f-cad
     d-acumulado AT ROW 18.88 COL 82 COLON-ALIGNED
     d-tot-titulo-quitado-cartorio AT ROW 19 COL 20.14 COLON-ALIGNED
     "Limite Cr‚dito:" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 7.54 COL 6.29
     "Total Juros" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 12.75 COL 63
     "Compra:" VIEW-AS TEXT
          SIZE 6 BY .67 AT ROW 6.54 COL 10
     "Data Limite Cr‚dito:" VIEW-AS TEXT
          SIZE 13 BY .67 AT ROW 8.54 COL 2.57
     "Duplicatas Prorrogadas" VIEW-AS TEXT
          SIZE 16 BY .67 AT ROW 5.75 COL 30.29
     "Valor Acumulado" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 5.75 COL 3.14
     "Equifax" VIEW-AS TEXT
          SIZE 8 BY .75 AT ROW 12.75 COL 96
     "Maior Ac£mulo" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 12.75 COL 22
     "Maior Compra" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 12.75 COL 2.86
     "Totais" VIEW-AS TEXT
          SIZE 5 BY .67 AT ROW 5.75 COL 87.72
     "Cheques Prorrogados" VIEW-AS TEXT
          SIZE 16 BY .67 AT ROW 5.75 COL 58.43
     "Devolu‡Æo Cheques" VIEW-AS TEXT
          SIZE 14 BY .67 AT ROW 16.25 COL 68.29
     "Saldo Dispon¡vel:" VIEW-AS TEXT
          SIZE 11.72 BY .67 AT ROW 11.38 COL 3.57
     "Fim Limite Cr‚dito:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 10.42 COL 3.57
     "éltima Compra" VIEW-AS TEXT
          SIZE 10 BY .67 AT ROW 12.75 COL 41.29
     "Lim. Cr‚d. Adicional:" VIEW-AS TEXT
          SIZE 14 BY .67 AT ROW 9.46 COL 2
     "Liquidez" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 16.29 COL 2.72
     RECT-12 AT ROW 13 COL 1.43
     RECT-13 AT ROW 13 COL 20.57
     RECT-14 AT ROW 16.54 COL 1.43
     RECT-15 AT ROW 6 COL 86.57
     RECT-16 AT ROW 13 COL 61
     RECT-17 AT ROW 4.17 COL 1.57
     RECT-18 AT ROW 13 COL 95
     RECT-5 AT ROW 6 COL 1.43
     RECT-6 AT ROW 13 COL 40.14
     RECT-7 AT ROW 16.54 COL 65.43
     RECT-8 AT ROW 6 COL 56.72
     RECT-9 AT ROW 6 COL 28.57
     rt-button AT ROW 1.21 COL 1.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.29 BY 21.13
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-consim
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-consim ASSIGN
         HIDDEN             = YES
         TITLE              = "An lise de Cr‚dito"
         HEIGHT             = 19.29
         WIDTH              = 111.29
         MAX-HEIGHT         = 23.13
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 23.13
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-cadastro:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-consim 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-consim.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-consim
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
/* SETTINGS FOR BUTTON bt-confirma IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       bt-confirma:HIDDEN IN FRAME f-cad           = TRUE.

/* SETTINGS FOR FILL-IN c-estabs-sel IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Categ IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-acumulado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-juros-pendentes IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-juros-quitados IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-limite IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-limite-ad IN FRAME f-cad
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN d-pendentes IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-quitados IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-saldo IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-bordero IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-cheque-compensado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-cheque-compensar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-cheque-proprio IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-cheque-prorrogado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-compra IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-debito IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-duplic-compensado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-duplic-prorrogado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-duplic-quitar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-titulo-quitado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-titulo-quitado-atraso IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-tot-titulo-quitado-cartorio IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-aberto IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-atraso IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-compensar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-devolucao IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-notas IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-total-quitado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN d-ultima-nota IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dt-limite-fim IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dt-limite-ini IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dt-ultima-nota IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-ano-compra IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-ano-debito IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-mes-compra IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-mes-debito IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-num-cheque-compensado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-num-cheque-compensar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-num-cheque-prorrogado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-num-duplic-compensado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-num-duplic-prorrogado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN i-num-duplic-quitar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ind-cre-cli IN FRAME f-cad
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-consim)
THEN w-consim:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-consim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-consim w-consim
ON END-ERROR OF w-consim /* An lise de Cr‚dito */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-consim w-consim
ON WINDOW-CLOSE OF w-consim /* An lise de Cr‚dito */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma w-consim
ON CHOOSE OF bt-confirma IN FRAME f-cad /* Button 7 */
DO:

  STATUS INPUT  'Carregando Dados'.
  SESSION:SET-WAIT-STATE("General").      

  ASSIGN INPUT FRAME {&FRAME-NAME} dt-inicial.

  
  DO WITH FRAME {&FRAME-NAME}:
      /* Zera contadores e limpa tela */
      ASSIGN i-num-cheque-prorrogado:SCREEN-VALUE = ''
             d-tot-cheque-prorrogado:SCREEN-VALUE = ''
             i-num-cheque-compensado:SCREEN-VALUE = '' 
             d-tot-cheque-compensado:SCREEN-VALUE = ''
             i-num-cheque-compensar:SCREEN-VALUE  = ''
             d-tot-cheque-compensar:SCREEN-VALUE  = ''
             i-num-duplic-prorrogado:SCREEN-VALUE = ''
             d-tot-duplic-prorrogado:SCREEN-VALUE = ''
             i-num-duplic-compensado:SCREEN-VALUE = ''
             d-tot-duplic-compensado:SCREEN-VALUE = ''
             i-num-duplic-quitar:SCREEN-VALUE     = ''
             d-tot-duplic-quitar:SCREEN-VALUE     = ''
             d-pendentes:SCREEN-VALUE             = ''
             d-quitados:SCREEN-VALUE              = ''
             d-acumulado:SCREEN-VALUE             = ''
             i-mes-debito:SCREEN-VALUE            = ''
             i-ano-debito:SCREEN-VALUE            = ''
             d-tot-debito:SCREEN-VALUE            = ''
             i-mes-compra:SCREEN-VALUE            = ''
             i-ano-compra:SCREEN-VALUE            = ''
             d-tot-compra:SCREEN-VALUE            = ''
             d-total-notas:SCREEN-VALUE           = ''
             d-total-compensar:SCREEN-VALUE       = ''
             d-total-atraso:SCREEN-VALUE          = ''
             d-juros-pendentes:SCREEN-VALUE       = ''
             d-tot-cheque-proprio:SCREEN-VALUE    = ''       
             d-tot-bordero:SCREEN-VALUE           = ''              
             d-tot-titulo-quitado-cartorio:SCREEN-VALUE =  ''
             d-tot-titulo-quitado-atraso:SCREEN-VALUE = ''
             d-tot-titulo-quitado:SCREEN-VALUE    = ''       
             d-total-devolucao:SCREEN-VALUE       = ''
             d-total-quitado:SCREEN-VALUE         = ''
             d-total-aberto:SCREEN-VALUE          = ''
             d-ultima-nota:SCREEN-VALUE           = ''
             dt-ultima-nota:SCREEN-VALUE          = ''
             d-tot-cheque-pendente-semdevol       = 0
             d-tot-titulo-compensar               = 0
             d-titulo-quitado                     = 0
             d-tot-cheque-devolv-pendente         = 0
             d-tot-cheque-aberto                  = 0
             d-tot-cheque-quitado                 = 0
             i-cheque-prorrogado                  = 0
             d-cheque-prorrogado                  = 0
             i-cheque-compensado                  = 0
             d-cheque-compensado                  = 0
             i-cheque-compensar                   = 0
             d-cheque-compensar                   = 0
             i-duplic-prorrogada                  = 0
             d-duplic-prorrogada                  = 0
             i-duplic-quitada                     = 0
             d-duplic-quitada                     = 0
             i-duplic-quitar                      = 0
             d-duplic-quitar                      = 0
             d-maior-debito                       = 0
             dt-maior-debito                      = ?
             d-maior-compra                       = 0
             dt-maior-compra                      = ?
             d-notas                              = 0
             d-tot-compensar                      = 0
             d-tot-atraso                         = 0
             d-tot-juros-pend                     = 0
             d-tot-juros-quit                     = 0
             d-tot-quitado                        = 0
             d-tot-devolucao                      = 0
             d-cheque-proprio                     = 0
             d-bordero                            = 0
             d-titulo-quitado-cartorio            = 0
             d-titulo-quitado-atraso              = 0
             d-titulo-quitado                     = 0
             data-ultima-nota                     = 01/01/0001
             d-aux                                = 0.
                                                                                        /***DMJG003.sn***/
      ASSIGN d-sld-tit-reab-dev                   = 0
             i-num-tit-acr-ems2                   = 0.

                                                                                        /***DMJG003.en***/
                                                                                        /***DMJGD001.so***
      ASSIGN i-empresa-ima = IF (i-visao-empresa:SCREEN-VALUE = '3'  OR
                                 i-visao-empresa:SCREEN-VALUE = '1') THEN 1 ELSE 2
             i-empresa-inter = IF (i-visao-empresa:SCREEN-VALUE = '3'  OR
                                   i-visao-empresa:SCREEN-VALUE = '2') THEN 2 ELSE 1.
                                                                                         ***DMJGD001.eo***/                             
      run pi-dados.

      ASSIGN i-num-cheque-prorrogado:SCREEN-VALUE       = STRING(i-cheque-prorrogado)
             d-tot-cheque-prorrogado:SCREEN-VALUE       = STRING(d-cheque-prorrogado)
             i-num-cheque-compensado:SCREEN-VALUE       = STRING(i-cheque-compensado)
             d-tot-cheque-compensado:SCREEN-VALUE       = STRING(d-cheque-compensado)
             i-num-cheque-compensar:SCREEN-VALUE        = STRING(i-cheque-compensar)
             d-tot-cheque-compensar:SCREEN-VALUE        = STRING(d-cheque-compensar)
             i-num-duplic-prorrogado:SCREEN-VALUE       = STRING(i-duplic-prorrogada)
             d-tot-duplic-prorrogado:SCREEN-VALUE       = STRING(d-duplic-prorrogada)
             i-num-duplic-compensado:SCREEN-VALUE       = STRING(i-duplic-quitada)
             d-tot-duplic-compensado:SCREEN-VALUE       = STRING(d-duplic-quitada)
             i-num-duplic-quitar:SCREEN-VALUE           = STRING(i-duplic-quitar)
             d-tot-duplic-quitar:SCREEN-VALUE           = STRING(d-duplic-quitar)
             /* d-pendentes:SCREEN-VALUE                = STRING(d-tot-cheque-devolv-pendente + d-sld-tit-reab) */
             d-pendentes:SCREEN-VALUE                   = STRING(d-tot-cheque-devolv-pendente)
             /*d-quitados:SCREEN-VALUE                    = STRING(d-tot-cheque-quitado - d-sld-tit-reab-dev)*/
             d-quitados:SCREEN-VALUE                    = STRING(d-tot-cheque-quitado)
             d-acumulado:SCREEN-VALUE                   = STRING(d-tot-cheque-devolv-pendente + d-tot-cheque-quitado)
             i-mes-debito:SCREEN-VALUE                  = STRING(MONTH(dt-maior-debito))
             i-ano-debito:SCREEN-VALUE                  = STRING(YEAR(dt-maior-debito))
             d-tot-debito:SCREEN-VALUE                  = STRING(d-maior-debito)
             i-mes-compra:SCREEN-VALUE                  = STRING(MONTH(dt-maior-compra))
             i-ano-compra:SCREEN-VALUE                  = STRING(YEAR(dt-maior-compra))
             d-tot-compra:SCREEN-VALUE                  = STRING(d-maior-compra)
             d-total-notas:SCREEN-VALUE                 = STRING(d-notas)
             d-total-compensar:SCREEN-VALUE             = STRING(d-tot-compensar)
             d-total-atraso:SCREEN-VALUE                = STRING(d-tot-atraso)
             d-total-devolucao:SCREEN-VALUE             = STRING(d-tot-devolucao)
             d-total-quitado:SCREEN-VALUE               = STRING(d-tot-quitado)
             d-tot-cheque-proprio:SCREEN-VALUE          = STRING(d-cheque-proprio)
             d-tot-bordero:SCREEN-VALUE                 = STRING(d-bordero)
             d-total-compensar:SCREEN-VALUE             = STRING(d-tot-compensar)
             d-total-aberto:SCREEN-VALUE                = STRING(d-tot-cheque-aberto + d-tot-compensar + d-tot-atraso + d-tot-juros-pend + INT(d-pendentes:SCREEN-VALUE)) 
             d-juros-pendentes:SCREEN-VALUE             = STRING(d-tot-juros-pend)
             d-juros-quitados:SCREEN-VALUE              = STRING(d-tot-juros-quit)
             d-tot-titulo-quitado:SCREEN-VALUE          = STRING(d-titulo-quitado)
             d-tot-titulo-quitado-atraso:SCREEN-VALUE   = STRING(d-titulo-quitado-atraso)
             d-tot-titulo-quitado-cartorio:SCREEN-VALUE = STRING(d-titulo-quitado-cartorio)
                                                                                        /***DMJGD001.sn***/
             c-estabs-sel:SCREEN-VALUE                  = "".                           
                                                                                        /***DMJGD001.en***/
      IF DEC(d-pendentes:SCREEN-VALUE) > 0 THEN
          d-pendentes:FGCOLOR   = 12.
      ELSE d-pendentes:FGCOLOR  = 0.
                                                                                        /***DMJGD001.sn***/
      FOR EACH tt-estabelec-dest NO-LOCK:
          ASSIGN c-estabs-sel:SCREEN-VALUE = c-estabs-sel:SCREEN-VALUE + tt-estabelec-dest.cod-estabel + ", ".
      END.
      ASSIGN c-estabs-sel:SCREEN-VALUE = SUBSTR(c-estabs-sel:SCREEN-VALUE, 1, LENGTH(c-estabs-sel:SCREEN-VALUE) - 1). 
                                                                                        /***DMJGD001.en***/
  END. /* do with frame */

  STATUS INPUT 'An lise de Cr‚dito'.
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-chq-a-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-chq-a-comp w-consim
ON CHOOSE OF bt-det-chq-a-comp IN FRAME f-cad /* Button 6 */
DO:
    RUN imp\impd030a.w (INPUT TABLE tt_ch_prorrog_compsar,
                        INPUT d-cheque-compensar).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-chq-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-chq-comp w-consim
ON CHOOSE OF bt-det-chq-comp IN FRAME f-cad /* Button 5 */
DO:
   RUN imp\impd030a.w (INPUT TABLE tt_ch_prorrog_compsado,
                       INPUT d-cheque-compensado).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-chq-dev-pend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-chq-dev-pend w-consim
ON CHOOSE OF bt-det-chq-dev-pend IN FRAME f-cad /* Button 8 */
DO:

    RUN imp\impd030j.w (INPUT TABLE tt_tit_atraso,
                        INPUT TABLE tt_cheque_pend,
                        INPUT d-sld-tit-reab-dev,
                        INPUT d-tot-cheque-devolv-pendente).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-chq-prorrog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-chq-prorrog w-consim
ON CHOOSE OF bt-det-chq-prorrog IN FRAME f-cad /* Button 4 */
DO:
    RUN imp\impd030a.w (INPUT TABLE tt_ch_prorrog,
                        INPUT d-cheque-prorrogado).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-dupl-a-venc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-dupl-a-venc w-consim
ON CHOOSE OF bt-det-dupl-a-venc IN FRAME f-cad /* Button 3 */
DO:
    RUN imp\impd030i.w (INPUT TABLE tt_tit_prorrog_aberto).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-dupl-prorrog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-dupl-prorrog w-consim
ON CHOOSE OF bt-det-dupl-prorrog IN FRAME f-cad /* Button 1 */
DO:
    RUN imp\impd030i.w (INPUT TABLE tt_tit_prorrog).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-dupl-quit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-dupl-quit w-consim
ON CHOOSE OF bt-det-dupl-quit IN FRAME f-cad /* Button 2 */
DO:
    RUN imp\impd030i.w (INPUT TABLE tt_tit_prorrog_quitado).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-dupl-quit-atraso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-dupl-quit-atraso w-consim
ON CHOOSE OF bt-det-dupl-quit-atraso IN FRAME f-cad /* Button 15 */
DO:
    RUN imp\impd030g.w (INPUT TABLE tt_tit_liq_atraso).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-juros-pend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-juros-pend w-consim
ON CHOOSE OF bt-det-juros-pend IN FRAME f-cad /* Button 14 */
DO:
    RUN imp\impd030f.w (INPUT TABLE tt_juros_pend).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-aberto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-aberto w-consim
ON CHOOSE OF bt-det-tot-aberto IN FRAME f-cad /* Button 13 */
DO:
    EMPTY TEMP-TABLE tt_tit_aberto.

    FOR EACH tt_juros_pend.
        CREATE tt_tit_aberto.
        BUFFER-COPY tt_juros_pend TO tt_tit_aberto.
    END.
    
    FOR EACH tt_tit_atraso.
        CREATE tt_tit_aberto.
        BUFFER-COPY tt_tit_atraso TO tt_tit_aberto.
    END.

    FOR EACH tt_tit_compsar.
        CREATE tt_tit_aberto.
        BUFFER-COPY tt_tit_compsar TO tt_tit_aberto.
    END.

    RUN imp\impd030c.w (INPUT TABLE tt_tit_aberto,
                        INPUT TABLE tt_cheque_aberto,
                        INPUT d-tot-compensar + d-tot-atraso,
                        INPUT d-tot-cheque-aberto + d-tot-cheque-devolv-pendente).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-atraso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-atraso w-consim
ON CHOOSE OF bt-det-tot-atraso IN FRAME f-cad /* Button 12 */
DO:
    RUN imp\impd030b.w (INPUT TABLE tt_tit_atraso).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-comp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-comp w-consim
ON CHOOSE OF bt-det-tot-comp IN FRAME f-cad /* Button 11 */
DO:
  RUN imp\impd030c.w (INPUT TABLE tt_tit_compsar,
                      INPUT TABLE tt_cheque_compsar,
                      INPUT d-tot-compensar,
                      INPUT d-tot-cheque-compensar).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-devol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-devol w-consim
ON CHOOSE OF bt-det-tot-devol IN FRAME f-cad /* Button 10 */
DO:
    RUN imp\impd030d.w (INPUT TABLE tt_tit_devol).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-tot-quitado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-tot-quitado w-consim
ON CHOOSE OF bt-det-tot-quitado IN FRAME f-cad /* Button 9 */
DO:
    RUN imp\impd030e.w (INPUT TABLE tt_tit_quitado,
                        INPUT TABLE tt_cheque_compsado,
                        INPUT d-titulo-quitado,
                        INPUT d-tot-cheque-aberto).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param w-consim
ON CHOOSE OF bt-param IN FRAME f-cad /* Seleciona Estabelecimentos An lise Cr‚dito */
DO:
  RUN imp/impd030x.w.
  APPLY 'choose' TO bt-confirma.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dt-inicial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dt-inicial w-consim
ON LEAVE OF dt-inicial IN FRAME f-cad /* Data In¡cio An lise */
DO:
  IF INPUT FRAME {&FRAME-NAME} dt-inicial <> dt-inicial THEN 
     APPLY 'choose' TO bt-confirma.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dt-inicial w-consim
ON RETURN OF dt-inicial IN FRAME f-cad /* Data In¡cio An lise */
DO:
  APPLY 'choose' TO bt-confirma.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-anterior w-consim
ON CHOOSE OF MENU-ITEM mi-anterior /* Anterior */
DO:
  RUN pi-anterior IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-arquivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-arquivo w-consim
ON MENU-DROP OF MENU mi-arquivo /* Arquivo */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-consim
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-consim
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-consim
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-pesquisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-pesquisa w-consim
ON CHOOSE OF MENU-ITEM mi-pesquisa /* Pesquisa */
DO:
  RUN pi-pesquisa IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-primeiro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-primeiro w-consim
ON CHOOSE OF MENU-ITEM mi-primeiro /* Primeiro */
DO:
  RUN pi-primeiro IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-proximo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-proximo w-consim
ON CHOOSE OF MENU-ITEM mi-proximo /* Pr¢ximo */
DO:
  RUN pi-proximo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-consim
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-consim
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-ultimo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-ultimo w-consim
ON CHOOSE OF MENU-ITEM mi-ultimo /* éltimo */
DO:
  RUN pi-ultimo IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-va-para
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-va-para w-consim
ON CHOOSE OF MENU-ITEM mi-va-para /* V  para */
DO:
  RUN pi-vapara IN h_p-navega.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-consim 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-consim  _ADM-CREATE-OBJECTS
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
             INPUT  'panel/p-navega.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navega ).
       RUN set-position IN h_p-navega ( 1.33 , 2.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 24.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.33 , 95.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'imvwr/v03impd030.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v03impd030 ).
       RUN set-position IN h_v03impd030 ( 2.75 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.29 , 111.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'imqry/q01im030.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'ProgPesquisa = adzoom\z02ad098.w,
                     ProgVaPara = adgo\g07ad098.w,
                     ProgIncMod = ,
                     Implantar = no':U ,
             OUTPUT h_q01im030 ).
       RUN set-position IN h_q01im030 ( 1.25 , 42.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.63 , 7.72 ) */

       /* Links to SmartViewer h_v03impd030. */
       RUN add-link IN adm-broker-hdl ( h_q01im030 , 'Record':U , h_v03impd030 ).

       /* Links to SmartQuery h_q01im030. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , h_q01im030 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'Navigation':U , h_q01im030 ).
       RUN add-link IN adm-broker-hdl ( h_p-navega , 'State':U , h_q01im030 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             bt-param:HANDLE IN FRAME f-cad , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v03impd030 ,
             h_p-exihel , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-consim  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-consim  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-consim)
  THEN DELETE WIDGET w-consim.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-consim  _DEFAULT-ENABLE
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
  DISPLAY dt-inicial c-estabs-sel ind-cre-cli d-total-notas 
          i-num-duplic-prorrogado i-num-cheque-prorrogado d-total-aberto 
          d-limite d-tot-duplic-prorrogado d-tot-cheque-prorrogado 
          d-total-quitado dt-limite-ini i-num-duplic-compensado 
          i-num-cheque-compensado d-total-atraso d-limite-ad 
          d-tot-duplic-compensado d-tot-cheque-compensado d-total-devolucao 
          dt-limite-fim i-num-duplic-quitar i-num-cheque-compensar 
          d-total-compensar d-saldo d-tot-duplic-quitar d-tot-cheque-compensar 
          d-juros-pendentes i-mes-compra i-ano-compra i-mes-debito i-ano-debito 
          dt-ultima-nota Categ d-juros-quitados d-tot-compra d-tot-debito 
          d-ultima-nota d-pendentes d-tot-titulo-quitado d-tot-cheque-proprio 
          d-quitados d-tot-titulo-quitado-atraso d-tot-bordero d-acumulado 
          d-tot-titulo-quitado-cartorio 
      WITH FRAME f-cad IN WINDOW w-consim.
  ENABLE RECT-12 RECT-13 RECT-14 RECT-15 RECT-16 RECT-17 RECT-18 RECT-5 RECT-6 
         RECT-7 RECT-8 RECT-9 rt-button bt-param dt-inicial bt-det-dupl-prorrog 
         bt-det-chq-prorrog bt-det-tot-aberto bt-det-tot-quitado 
         bt-det-dupl-quit bt-det-chq-comp bt-det-tot-atraso bt-det-tot-devol 
         bt-det-dupl-a-venc bt-det-chq-a-comp bt-det-tot-comp bt-det-juros-pend 
         bt-det-chq-dev-pend bt-det-dupl-quit-atraso 
      WITH FRAME f-cad IN WINDOW w-consim.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-consim.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects w-consim 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  h-impd030 = THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-consim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-consim 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-consim 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "IMPD030" "1.00.00.000"}

  RUN set-attribute-list IN h_q01im030 (INPUT "Reposition-Pending=YES":U).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
                                                                                        /***DMJGD001.sn***/
  ASSIGN dt-inicial = 10.01.2002.
  DISP dt-inicial WITH FRAME {&FRAME-NAME}.

  RUN imp/impd030x.w.
                                                                                        /***DMJGD001.en***/
  IF gr-emitente <> ? THEN 
    RUN pi-reposiciona-query IN h_q01im030 (INPUT gr-emitente).
  ELSE
    RUN dispatch IN h_q01im030 (INPUT "get-first":U).
                                                                                        /***DMJGD001.so***
  ASSIGN dt-inicial:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '01/10/2002'.

  RUN pi-carrega-emitente.  
                                                                                         ***DMJGD001.eo***/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-notas w-consim 
PROCEDURE pi-busca-notas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        RUN esapi/connect-ima-med.p.
    
        
        RUN imp/impd030nf.p (OUTPUT TABLE tt-notas,
                             INPUT i-cod-emitente,
                             INPUT DATE(dt-inicial:SCREEN-VALUE)).
        
        ASSIGN data-ultima-nota = ?.
        FOR EACH tt-notas BY tt-notas.dt-emis-nota DESCENDING.
            ASSIGN d-notas = d-notas + tt-notas.vl-tot-nota.

            /* Ultima Compra */
            IF data-ultima-nota = ? THEN
               ASSIGN dt-ultima-nota:SCREEN-VALUE  = STRING(tt-notas.dt-emis-nota)
                      d-ultima-nota:SCREEN-VALUE   = STRING(tt-notas.vl-tot-nota)
                      data-ultima-nota = tt-notas.dt-emis-nota.
        END.
    
        FOR EACH tt-notas NO-LOCK,
            FIRST tt-estabelec-dest NO-LOCK WHERE
                  tt-estabelec-dest.cod-estabel = tt-notas.cod-estabel            /***DMJGD001.en***/
            BREAK BY YEAR(tt-notas.dt-emis-nota)                      
                  BY MONTH(tt-notas.dt-emis-nota):                                /***DMJGD001.so***
                  
            IF tt-notas.ep-codigo <> i-empresa-ima   AND
               tt-notas.ep-codigo <> i-empresa-inter  THEN
                NEXT.                              ***DMJGD001.eo***/
    
            /* Maior Compra */
            IF tt-notas.vl-tot-nota > d-maior-compra THEN
                ASSIGN d-maior-compra    = tt-notas.vl-tot-nota
                       dt-maior-compra   = tt-notas.dt-emis-nota.
    
            d-aux = d-aux + tt-notas.vl-tot-nota.
    
            /* Maior Debito */
            IF  d-maior-debito < d-aux THEN
                ASSIGN d-maior-debito = d-aux
                       dt-maior-debito = tt-notas.dt-emis-nota.
    
            IF LAST-OF(MONTH(tt-notas.dt-emis-nota)) THEN
               d-aux = 0.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-emitente w-consim 
PROCEDURE pi-carrega-emitente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/               
DO WITH FRAME {&FRAME-NAME}:
                                                                                        /***DMJGD001.so***
  DISP " " @ Categ WITH FRAME {&FRAME-NAME}.
                                                                                         ***DMJGD001.eo***/
                                                                                        /***DMJGD001.sn***/
  ASSIGN categ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
                                                                                        /***DMJGD001.en***/
  FIND FIRST emitente WHERE
             emitente.cod-emitente = i-cod-emitente NO-LOCK NO-ERROR.

  IF AVAIL emitente THEN DO:
                                                                                        /***DMJGD001.so***
      FIND LAST equifax WHERE int(cod-interno) = emitente.cod-emitente NO-LOCK NO-ERROR.
                                                                                         ***DMJGD001.eo***/
                                                                                        /***DMJGD001.sn***/
      FIND LAST equifax WHERE 
                equifax.cod-interno = STRING(emitente.cod-emitente)
                NO-LOCK NO-ERROR.
                                                                                        /***DMJGD001.en***/
      IF AVAIL equifax THEN
                                                                                        /***DMJGD001.so***
         DISP equifax.ranking-atual @ Categ WITH FRAME {&FRAME-NAME}.
                                                                                         ***DMJGD001.eo***/
                                                                                        /***DMJGD001.sn***/
         ASSIGN categ:SCREEN-VALUE IN FRAME {&FRAME-NAME} = equifax.ranking-atual.
                                                                                        /***DMJGD001.en***/
      ASSIGN ind-cre-cli:SCREEN-VALUE     = STRING(emitente.ind-cre-cli)
             d-limite:SCREEN-VALUE        = STRING(emitente.lim-credito)
             dt-limite-ini:SCREEN-VALUE   = STRING(emitente.dt-lim-cred)
             d-limite-ad:SCREEN-VALUE     = STRING(emitente.lim-adicional)
             dt-limite-fim:SCREEN-VALUE   = STRING(emitente.dt-fim-cred).             
        
      CASE emitente.ind-cre-cli:
        WHEN 1 THEN
            ind-cre-cli:SCREEN-VALUE = 'Normal'.
        WHEN 2 THEN
            ind-cre-cli:SCREEN-VALUE =  'Autom tico'.
        WHEN 3 THEN
            ind-cre-cli:SCREEN-VALUE = 'Somente Implanta Pedido'.
        WHEN 4 THEN
            ind-cre-cli:SCREEN-VALUE = 'Suspenso'.
        WHEN 5 THEN
            ind-cre-cli:SCREEN-VALUE = 'Pagamento … Vista'.
        OTHERWISE ind-cre-cli:SCREEN-VALUE = STRING(emitente.ind-cre-cli).
      END CASE.
  END.

  IF ind-cre-cli:SCREEN-VALUE = 'Suspenso' THEN
      ASSIGN ind-cre-cli:FGCOLOR     = 12.
  ELSE ASSIGN ind-cre-cli:FGCOLOR     = 0.
END.

APPLY 'choose' TO bt-confirma.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-tc w-consim 
PROCEDURE pi-cria-tc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEF INPUT PARAMETER p-tp-dados AS CHAR.

     ASSIGN i-num_id_cheq_acr = i-num_id_cheq_acr + 1.
     CASE p-tp-dados.
         WHEN 'aberto' THEN DO.
             FIND tt_cheque_aberto WHERE
                  tt_cheque_aberto.cod_banco = STRING(cheque.cod-banco) AND
                  tt_cheque_aberto.cod_agenc_bcia = cheque.agencia AND
                  tt_cheque_aberto.cod_cta_corren_bco = cheque.conta-corren AND
                  tt_cheque_aberto.num_cheque = INTEGER(cheque.nr-cheque)
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_cheque_aberto THEN DO. 
                CREATE tt_cheque_aberto.
                ASSIGN tt_cheque_aberto.cod_estab  = cheque.cod-estabel 
                       tt_cheque_aberto.num_cheque = INTEGER(cheque.nr-cheque)
                       tt_cheque_aberto.cod_banco  = STRING(cheque.cod-banco)
                       tt_cheque_aberto.cod_agenc_bcia = cheque.agencia
                       tt_cheque_aberto.cod_cta_corren_bco = cheque.conta-corren
                       tt_cheque_aberto.val_cheque = cheque.vl-cheque     
                       tt_cheque_aberto.dat_emis_cheq = cheque.dt-emissao 
                       tt_cheque_aberto.dat_prev_apres_cheq_acr = cheque.dt-vencimento 
                       tt_cheque_aberto.num_id_cheq_acr = i-num_id_cheq_acr.
             END.
         END.
         WHEN 'compsado' THEN DO.
             FIND tt_cheque_compsado WHERE
                  tt_cheque_compsado.cod_banco = STRING(cheque.cod-banco) AND
                  tt_cheque_compsado.cod_agenc_bcia = cheque.agencia AND
                  tt_cheque_compsado.cod_cta_corren_bco = cheque.conta-corren AND
                  tt_cheque_compsado.num_cheque = INTEGER(cheque.nr-cheque)
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_cheque_compsado THEN DO. 
                CREATE tt_cheque_compsado.
                ASSIGN tt_cheque_compsado.cod_estab  = cheque.cod-estabel 
                       tt_cheque_compsado.num_cheque = INTEGER(cheque.nr-cheque)
                       tt_cheque_compsado.cod_banco  = STRING(cheque.cod-banco)
                       tt_cheque_compsado.cod_agenc_bcia = cheque.agencia
                       tt_cheque_compsado.cod_cta_corren_bco = cheque.conta-corren
                       tt_cheque_compsado.val_cheque = cheque.vl-cheque     
                       tt_cheque_compsado.dat_emis_cheq = cheque.dt-emissao 
                       tt_cheque_compsado.dat_prev_apres_cheq_acr = cheque.dt-vencimento 
                       tt_cheque_compsado.num_id_cheq_acr = i-num_id_cheq_acr.
             END.
         END.
         WHEN 'compsar' THEN DO.
             FIND tt_cheque_compsar WHERE
                  tt_cheque_compsar.cod_banco = STRING(cheque.cod-banco) AND
                  tt_cheque_compsar.cod_agenc_bcia = cheque.agencia AND
                  tt_cheque_compsar.cod_cta_corren_bco = cheque.conta-corren AND
                  tt_cheque_compsar.num_cheque = INTEGER(cheque.nr-cheque)
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_cheque_compsar THEN DO. 
                CREATE tt_cheque_compsar.
                ASSIGN tt_cheque_compsar.cod_estab  = cheque.cod-estabel 
                       tt_cheque_compsar.num_cheque = INTEGER(cheque.nr-cheque)
                       tt_cheque_compsar.cod_banco  = STRING(cheque.cod-banco)
                       tt_cheque_compsar.cod_agenc_bcia = cheque.agencia
                       tt_cheque_compsar.cod_cta_corren_bco = cheque.conta-corren
                       tt_cheque_compsar.val_cheque = cheque.vl-cheque     
                       tt_cheque_compsar.dat_emis_cheq = cheque.dt-emissao 
                       tt_cheque_compsar.dat_prev_apres_cheq_acr = cheque.dt-vencimento 
                       tt_cheque_compsar.num_id_cheq_acr = i-num_id_cheq_acr.
             END.
         END.
         WHEN 'prorrog' THEN DO.
             FIND tt_ch_prorrog WHERE
                  tt_ch_prorrog.cod_banco = STRING(cheque.cod-banco) AND
                  tt_ch_prorrog.cod_agenc_bcia = cheque.agencia AND
                  tt_ch_prorrog.cod_cta_corren_bco = cheque.conta-corren AND
                  tt_ch_prorrog.num_cheque = INTEGER(cheque.nr-cheque)
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_ch_prorrog THEN DO. 
                CREATE tt_ch_prorrog.
                ASSIGN tt_ch_prorrog.cod_estab  = cheque.cod-estabel 
                       tt_ch_prorrog.num_cheque = INTEGER(cheque.nr-cheque)
                       tt_ch_prorrog.cod_banco  = STRING(cheque.cod-banco)
                       tt_ch_prorrog.cod_agenc_bcia = cheque.agencia
                       tt_ch_prorrog.cod_cta_corren_bco = cheque.conta-corren
                       tt_ch_prorrog.val_cheque = cheque.vl-cheque     
                       tt_ch_prorrog.dat_emis_cheq = cheque.dt-emissao 
                       tt_ch_prorrog.dat_prev_apres_cheq_acr = cheque.dt-vencimento 
                       tt_ch_prorrog.num_id_cheq_acr = i-num_id_cheq_acr.
             END.
         END.
         WHEN 'prorrog_compsar' THEN DO.
             FIND tt_ch_prorrog_compsar WHERE
                  tt_ch_prorrog_compsar.cod_banco = STRING(cheque.cod-banco) AND
                  tt_ch_prorrog_compsar.cod_agenc_bcia = cheque.agencia AND
                  tt_ch_prorrog_compsar.cod_cta_corren_bco = cheque.conta-corren AND
                  tt_ch_prorrog_compsar.num_cheque = INTEGER(cheque.nr-cheque)
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_ch_prorrog_compsar THEN DO. 
                CREATE tt_ch_prorrog_compsar.
                ASSIGN tt_ch_prorrog_compsar.cod_estab  = cheque.cod-estabel 
                       tt_ch_prorrog_compsar.num_cheque = INTEGER(cheque.nr-cheque)
                       tt_ch_prorrog_compsar.cod_banco  = STRING(cheque.cod-banco)
                       tt_ch_prorrog_compsar.cod_agenc_bcia = cheque.agencia
                       tt_ch_prorrog_compsar.cod_cta_corren_bco = cheque.conta-corren
                       tt_ch_prorrog_compsar.val_cheque = cheque.vl-cheque     
                       tt_ch_prorrog_compsar.dat_emis_cheq = cheque.dt-emissao 
                       tt_ch_prorrog_compsar.dat_prev_apres_cheq_acr = cheque.dt-vencimento 
                       tt_ch_prorrog_compsar.num_id_cheq_acr = i-num_id_cheq_acr.
             END.
         END.
         WHEN 'prorrog_compsado' THEN DO.
             FIND tt_ch_prorrog_compsado WHERE
                  tt_ch_prorrog_compsado.cod_banco = STRING(cheque.cod-banco) AND
                  tt_ch_prorrog_compsado.cod_agenc_bcia = cheque.agencia AND
                  tt_ch_prorrog_compsado.cod_cta_corren_bco = cheque.conta-corren AND
                  tt_ch_prorrog_compsado.num_cheque = INTEGER(cheque.nr-cheque)
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_ch_prorrog_compsado THEN DO. 
                CREATE tt_ch_prorrog_compsado.
                ASSIGN tt_ch_prorrog_compsado.cod_estab  = cheque.cod-estabel 
                       tt_ch_prorrog_compsado.num_cheque = INTEGER(cheque.nr-cheque)
                       tt_ch_prorrog_compsado.cod_banco  = STRING(cheque.cod-banco)
                       tt_ch_prorrog_compsado.cod_agenc_bcia = cheque.agencia
                       tt_ch_prorrog_compsado.cod_cta_corren_bco = cheque.conta-corren
                       tt_ch_prorrog_compsado.val_cheque = cheque.vl-cheque     
                       tt_ch_prorrog_compsado.dat_emis_cheq = cheque.dt-emissao 
                       tt_ch_prorrog_compsado.dat_prev_apres_cheq_acr = cheque.dt-vencimento 
                       tt_ch_prorrog_compsado.num_id_cheq_acr = i-num_id_cheq_acr.
             END.
         END.
     END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-tt w-consim 
PROCEDURE pi-cria-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEF INPUT PARAMETER p-tp-dados AS CHAR.
     ASSIGN i-num-tit-acr-ems2 = i-num-tit-acr-ems2 + 1.

     CASE p-tp-dados.
         WHEN 'quitado' THEN DO.
             FIND tt_tit_quitado WHERE
                  tt_tit_quitado.cod_estab = b-titulo.cod-estabel AND
                  tt_tit_quitado.cod_espec_docto = b-titulo.cod-esp AND
                  tt_tit_quitado.cod_ser_docto = b-titulo.serie AND
                  tt_tit_quitado.cod_tit_acr = b-titulo.nr-docto AND
                  tt_tit_quitado.cod_parcela = b-titulo.parcela
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_tit_quitado THEN DO. 
                CREATE tt_tit_quitado.
                ASSIGN tt_tit_quitado.num_id_tit_acr = i-num-tit-acr-ems2
                       tt_tit_quitado.cod_estab = b-titulo.cod-estabel 
                       tt_tit_quitado.cod_espec_docto = b-titulo.cod-esp
                       tt_tit_quitado.cod_ser_docto = b-titulo.serie 
                       tt_tit_quitado.cod_tit_acr = b-titulo.nr-docto 
                       tt_tit_quitado.cod_parcela = b-titulo.parcela
                       tt_tit_quitado.num_id_tit_acr = RECID(b-titulo).

                 ASSIGN tt_tit_quitado.cod_portador = STRING(b-titulo.port)
                        tt_tit_quitado.val_sdo_tit_acr = b-titulo.vl-original
                        tt_tit_quitado.val_origin_tit_acr = b-titulo.vl-original
                        tt_tit_quitado.dat_emis_docto = b-titulo.dt-emissao
                        tt_tit_quitado.dat_vencto_tit_acr = b-titulo.dt-venc.
             END.
         END.
         WHEN 'compsar' THEN DO.
             FIND tt_tit_compsar WHERE
                  tt_tit_compsar.cod_estab = b-titulo.cod-estabel AND
                  tt_tit_compsar.cod_espec_docto = b-titulo.cod-esp AND
                  tt_tit_compsar.cod_ser_docto = b-titulo.serie AND
                  tt_tit_compsar.cod_tit_acr = b-titulo.nr-docto AND
                  tt_tit_compsar.cod_parcela = b-titulo.parcela
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_tit_compsar THEN DO.
                CREATE tt_tit_compsar.
                ASSIGN tt_tit_compsar.num_id_tit_acr = i-num-tit-acr-ems2
                       tt_tit_compsar.cod_estab = b-titulo.cod-estabel 
                       tt_tit_compsar.cod_espec_docto = b-titulo.cod-esp
                       tt_tit_compsar.cod_ser_docto = b-titulo.serie 
                       tt_tit_compsar.cod_tit_acr = b-titulo.nr-docto 
                       tt_tit_compsar.cod_parcela = b-titulo.parcela
                       tt_tit_quitado.num_id_tit_acr = RECID(b-titulo).

                 ASSIGN tt_tit_compsar.cod_portador = STRING(b-titulo.port)
                        tt_tit_compsar.val_sdo_tit_acr = b-titulo.vl-saldo
                        tt_tit_compsar.val_origin_tit_acr = b-titulo.vl-saldo
                        tt_tit_compsar.dat_emis_docto = b-titulo.dt-emissao
                        tt_tit_compsar.dat_vencto_tit_acr = b-titulo.dt-venc.
             END.
         END.
         WHEN 'liq_atraso' THEN DO.
             FIND tt_tit_liq_atraso WHERE
                  tt_tit_liq_atraso.cod_estab = b-titulo.cod-estabel AND
                  tt_tit_liq_atraso.cod_espec_docto = b-titulo.cod-esp AND
                  tt_tit_liq_atraso.cod_ser_docto = b-titulo.serie AND
                  tt_tit_liq_atraso.cod_tit_acr = b-titulo.nr-docto AND
                  tt_tit_liq_atraso.cod_parcela = b-titulo.parcela
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_tit_liq_atraso THEN DO.
                CREATE tt_tit_liq_atraso.
                ASSIGN tt_tit_liq_atraso.num_id_tit_acr = i-num-tit-acr-ems2
                       tt_tit_liq_atraso.cod_estab = b-titulo.cod-estabel 
                       tt_tit_liq_atraso.cod_espec_docto = b-titulo.cod-esp
                       tt_tit_liq_atraso.cod_ser_docto = b-titulo.serie 
                       tt_tit_liq_atraso.cod_tit_acr = b-titulo.nr-docto 
                       tt_tit_liq_atraso.cod_parcela = b-titulo.parcela
                       tt_tit_quitado.num_id_tit_acr = RECID(b-titulo).

                 ASSIGN tt_tit_liq_atraso.cod_portador = STRING(b-titulo.port)
                        tt_tit_liq_atraso.val_sdo_tit_acr = b-titulo.vl-saldo
                        tt_tit_liq_atraso.val_origin_tit_acr = b-titulo.vl-saldo
                        tt_tit_liq_atraso.dat_emis_docto = b-titulo.dt-emissao
                        tt_tit_liq_atraso.dat_vencto_tit_acr = b-titulo.dt-venc.
             END.
         END.
         WHEN 'atraso' THEN DO.
             FIND tt_tit_atraso WHERE
                  tt_tit_atraso.cod_estab = b-titulo.cod-estabel AND
                  tt_tit_atraso.cod_espec_docto = b-titulo.cod-esp AND
                  tt_tit_atraso.cod_ser_docto = b-titulo.serie AND
                  tt_tit_atraso.cod_tit_acr = b-titulo.nr-docto AND
                  tt_tit_atraso.cod_parcela = b-titulo.parcela
                  NO-LOCK NO-ERROR.
             IF NOT AVAIL tt_tit_atraso THEN DO.
                CREATE tt_tit_atraso.
                ASSIGN tt_tit_atraso.num_id_tit_acr = i-num-tit-acr-ems2
                       tt_tit_atraso.cod_estab = b-titulo.cod-estabel 
                       tt_tit_atraso.cod_espec_docto = b-titulo.cod-esp
                       tt_tit_atraso.cod_ser_docto = b-titulo.serie 
                       tt_tit_atraso.cod_tit_acr = b-titulo.nr-docto 
                       tt_tit_atraso.cod_parcela = b-titulo.parcela
                       tt_tit_quitado.num_id_tit_acr = RECID(b-titulo).

                 ASSIGN tt_tit_atraso.cod_portador = STRING(b-titulo.port)
                        tt_tit_atraso.val_sdo_tit_acr = b-titulo.vl-saldo
                        tt_tit_atraso.val_origin_tit_acr = b-titulo.vl-saldo
                        tt_tit_atraso.dat_emis_docto = b-titulo.dt-emissao
                        tt_tit_atraso.dat_vencto_tit_acr = b-titulo.dt-venc.
             END.
         END.
     END CASE.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-dados w-consim 
PROCEDURE pi-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-base AS CHAR.

    EMPTY TEMP-TABLE tt-notas.

    EMPTY TEMP-TABLE tt_tit_prorrog.
    EMPTY TEMP-TABLE tt_tit_prorrog_quitado.
    EMPTY TEMP-TABLE tt_tit_prorrog_aberto.

    EMPTY TEMP-TABLE tt_tit_aberto.
    EMPTY TEMP-TABLE tt_tit_quitado. 
    EMPTY TEMP-TABLE tt_tit_atraso.
    EMPTY TEMP-TABLE tt_tit_devol.
    EMPTY TEMP-TABLE tt_tit_compsar. 
    EMPTY TEMP-TABLE tt_tit_liq_atraso.
    EMPTY TEMP-TABLE tt_juros_pend.
                      
    EMPTY TEMP-TABLE tt_ch_prorrog.
    EMPTY TEMP-TABLE tt_cheque_compsar.
    EMPTY TEMP-TABLE tt_cheque_compsado.
    EMPTY TEMP-TABLE tt_cheque_aberto.
    EMPTY TEMP-TABLE tt_cheque_pend.

    RUN esapi/busca-base.p (OUTPUT c-base).
    IF NOT c-base MATCHES "*bkp*" THEN
       RUN pi-busca-notas.

    RUN pi-dados-ems5.
    RUN pi-dados-ems2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-dados-ems2 w-consim 
PROCEDURE pi-dados-ems2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR l-tit-reab-dev AS LOG.
                                                                                        

DO WITH FRAME {&FRAME-NAME}:

    DEF VAR c-base AS CHAR    NO-UNDO.
    DEF VAR i-base AS INTEGER NO-UNDO.

    /* Busca no arquivo ini da sessao se a base e teste ou producao */
    GET-KEY-VALUE SECTION 'Startup':U key 'BASE':U VALUE c-base.

    IF c-base = 'OFICIAL' THEN
       i-base = 1.
    ELSE IF c-base = 'BACKUP' THEN
       i-base = 2.

    /***DMJG001.sn***/
    /* O c¢digo existente para tratamento de cheques e duplicatas foi movido para AS duas procedures abaixo */
  
    RUN pi-trata-cheques-ems2.
    RUN pi-trata-duplicatas-ems2.
    

    /* Devolucao Total */
    FOR EACH tt-estabelec-dest NO-LOCK,
        EACH mov-tit WHERE 
              mov-tit.ep-codigo    = tt-estabelec-dest.ep-codigo AND
              mov-tit.cod-emitente = i-cod-emitente  AND 
              mov-tit.dt-trans    >= DATE(dt-inicial:SCREEN-VALUE) NO-LOCK:

        IF mov-tit.cod-estabel <> tt-estabelec-dest.cod-estabel THEN NEXT.
        IF mov-tit.transacao <> 3 THEN NEXT.
        IF mov-tit.tipo <> 1 THEN NEXT.

        ASSIGN d-tot-devolucao = d-tot-devolucao + mov-tit.vl-baixa.

        ASSIGN i-num-tit-acr-ems2 = i-num-tit-acr-ems2 + 1.

        FIND tt_tit_devol WHERE
             tt_tit_devol.cod_estab = mov-tit.cod-estabel AND
             tt_tit_devol.cod_espec_docto = mov-tit.cod-esp AND
             tt_tit_devol.cod_ser_docto = mov-tit.serie AND
             tt_tit_devol.cod_tit_acr = mov-tit.nr-docto AND
             tt_tit_devol.cod_parcela = mov-tit.parcela
             NO-LOCK NO-ERROR.
        IF NOT AVAIL tt_tit_devol THEN DO.
           CREATE tt_tit_devol.
           ASSIGN tt_tit_devol.num_id_tit_acr = i-num-tit-acr-ems2
                  tt_tit_devol.cod_estab = mov-tit.cod-estabel 
                  tt_tit_devol.cod_espec_docto = mov-tit.cod-esp
                  tt_tit_devol.cod_ser_docto = mov-tit.serie 
                  tt_tit_devol.cod_tit_acr = mov-tit.nr-docto 
                  tt_tit_devol.cod_parcela = mov-tit.parcela
                  tt_tit_quitado.num_id_tit_acr = RECID(mov-tit).
        
            ASSIGN tt_tit_devol.cod_portador = STRING(mov-tit.cod-portador)
                   tt_tit_devol.val_sdo_tit_acr = mov-tit.vl-baixa
                   tt_tit_devol.val_origin_tit_acr = mov-tit.vl-baixa 
                   tt_tit_devol.dat_emis_docto = mov-tit.dt-emissao
                   tt_tit_devol.dat_vencto_tit_acr = mov-tit.dt-venc.
        END.
    END. /* each mov-tit */


    /* Busca duplicatas substitutas */
    FOR EACH nf-dupl-cr WHERE 
             nf-dupl-cr.cod-emitente = i-cod-emitente NO-LOCK, 
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
    
          FOR FIRST b-titulo WHERE
                    b-titulo.ep-codigo   = nf-dupl-cr.ep-codigo   AND
                    b-titulo.cod-estabel = nf-dupl-cr.cod-estab-d AND
                    b-titulo.cod-esp     = nf-dupl-cr.cod-esp-d   AND
                    b-titulo.serie       = nf-dupl-cr.serie-d     AND
                    b-titulo.nr-docto    = nf-dupl-cr.nr-docto-d  AND
                    b-titulo.parcela     = nf-dupl-cr.parcela-d   NO-LOCK:
    
              IF b-titulo.vl-original = 0 THEN NEXT.

              IF b-titulo.dt-emissao < DATE(dt-inicial:SCREEN-VALUE) THEN NEXT.

              /*** Verifica se o t¡tulo foi reaberto devido … devolu‡Æo de cheques, buscando uma transa‡Æo do tipo 
                   AVA no portador 100 para o mesmo. Caso encontre este t¡tulo ser  tratado na devolu‡aä de cheques ***/
              ASSIGN l-tit-reab-dev = NO.


              
              IF (b-titulo.cod-esp  = 'DP'  OR 
                  b-titulo.cod-esp  = 'DT'  OR
                  b-titulo.cod-esp  = 'DG'  OR
                                                                                        /***DMJGD002.sn***/
                  b-titulo.cod-esp  = 'CD'  OR
                                                                                        /***DMJGD002.en***/
                  b-titulo.cod-esp  = 'NP') AND
                  b-titulo.vl-saldo <> 0  THEN DO:
                  IF CAN-FIND(FIRST mov-tit OF b-titulo WHERE 
                                    mov-tit.transacao = 13 /* AVA */ AND 
                                    SUBSTRING(mov-tit.char-1, 1, 6) = "DEV_CH") THEN DO:
                      ASSIGN d-sld-tit-reab-dev = d-sld-tit-reab-dev + b-titulo.vl-saldo.
                      ASSIGN l-tit-reab-dev = YES.
                  END.
              END.

              /* Prorrogadas */
              IF b-titulo.dt-vencimen > b-titulo.data-1 THEN DO.
                  ASSIGN i-duplic-prorrogada = i-duplic-prorrogada + 1
                         d-duplic-prorrogada = d-duplic-prorrogada + b-titulo.vl-original.

                  FIND tt_tit_prorrog WHERE
                       tt_tit_prorrog.cod_estab = b-titulo.cod-estabel AND
                       tt_tit_prorrog.cod_espec_docto = b-titulo.cod-esp AND
                       tt_tit_prorrog.cod_ser_docto = b-titulo.serie AND
                       tt_tit_prorrog.cod_tit_acr = b-titulo.nr-docto AND
                       tt_tit_prorrog.cod_parcela = b-titulo.parcela
                       NO-LOCK NO-ERROR.
                  IF NOT AVAIL tt_tit_prorrog THEN DO. 
                     CREATE tt_tit_prorrog.
                     ASSIGN tt_tit_prorrog.cod_estab = b-titulo.cod-estabel 
                            tt_tit_prorrog.cod_espec_docto = b-titulo.cod-esp
                            tt_tit_prorrog.cod_ser_docto = b-titulo.serie 
                            tt_tit_prorrog.cod_tit_acr = b-titulo.nr-docto 
                            tt_tit_prorrog.cod_parcela = b-titulo.parcela
                            tt_tit_prorrog.num_id_tit_acr = RECID(b-titulo).

                      ASSIGN tt_tit_prorrog.cod_portador = STRING(b-titulo.port)
                             tt_tit_prorrog.val_sdo_tit_acr = b-titulo.vl-original
                             tt_tit_prorrog.val_origin_tit_acr = b-titulo.vl-original
                             tt_tit_prorrog.dat_emis_docto = b-titulo.dt-emissao
                             tt_tit_prorrog.dat_vencto_tit_acr = b-titulo.dt-venc.
                  END.
              END.
    
              /* Quitadas */
              IF b-titulo.dt-vencimen > b-titulo.data-1 AND
                 b-titulo.vl-saldo = 0 THEN DO.
                 ASSIGN i-duplic-quitada = i-duplic-quitada + 1
                        d-duplic-quitada = d-duplic-quitada + b-titulo.vl-original.
                 FIND tt_tit_prorrog_quitado WHERE
                      tt_tit_prorrog_quitado.cod_estab = b-titulo.cod-estabel AND
                      tt_tit_prorrog_quitado.cod_espec_docto = b-titulo.cod-esp AND
                      tt_tit_prorrog_quitado.cod_ser_docto = b-titulo.serie AND
                      tt_tit_prorrog_quitado.cod_tit_acr = b-titulo.nr-docto AND
                      tt_tit_prorrog_quitado.cod_parcela = b-titulo.parcela
                      NO-LOCK NO-ERROR.
                 IF NOT AVAIL tt_tit_prorrog_quitado THEN DO. 
                    CREATE tt_tit_prorrog_quitado.
                    ASSIGN tt_tit_prorrog_quitado.cod_estab = b-titulo.cod-estabel 
                           tt_tit_prorrog_quitado.cod_espec_docto = b-titulo.cod-esp
                           tt_tit_prorrog_quitado.cod_ser_docto = b-titulo.serie 
                           tt_tit_prorrog_quitado.cod_tit_acr = b-titulo.nr-docto 
                           tt_tit_prorrog_quitado.cod_parcela = b-titulo.parcela
                           tt_tit_prorrog_quitado.num_id_tit_acr = RECID(b-titulo).

                     ASSIGN tt_tit_prorrog_quitado.cod_portador = STRING(b-titulo.port)
                            tt_tit_prorrog_quitado.val_sdo_tit_acr = b-titulo.vl-original
                            tt_tit_prorrog_quitado.val_origin_tit_acr = b-titulo.vl-original
                            tt_tit_prorrog_quitado.dat_emis_docto = b-titulo.dt-emissao
                            tt_tit_prorrog_quitado.dat_vencto_tit_acr = b-titulo.dt-venc.
                 END.
              END.


              
              /* A Quitar */
              IF b-titulo.dt-vencimen > b-titulo.data-1 AND 
                 b-titulo.vl-saldo    <> 0 THEN
                  ASSIGN i-duplic-quitar = i-duplic-quitar + 1
                         d-duplic-quitar = d-duplic-quitar + b-titulo.vl-original.
              
                                                                                       
              /*
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
                         b-titulo.vl-saldo    <> 0      THEN DO.
                         ASSIGN d-tot-compensar        = d-tot-compensar + b-titulo.vl-saldo
                                d-tot-titulo-compensar = d-tot-titulo-compensar + b-titulo.vl-saldo.

                         RUN pi-cria-tt (INPUT "compsar").
                      END.
        
                      IF b-titulo.vl-saldo <> 0 AND 
                         b-titulo.dt-vencimen < TODAY THEN DO.
                         ASSIGN d-tot-atraso = d-tot-atraso + b-titulo.vl-saldo.
                         RUN pi-cria-tt (INPUT "atraso").
                      END.
                  END.
             END.
             */
                                                                                        /***DMJG003.en***/
             /*
             /* Devolucao Total */
             IF b-titulo.cod-esp = 'DM' THEN
                ASSIGN d-tot-devolucao = d-tot-devolucao + b-titulo.vl-original.
             */

             
             /* Juros Pendentes */
             IF b-titulo.tipo     = 6 AND
                titulo.cod-esp    <> "PD" AND                                           /***FOMM.NEW***/
                b-titulo.vl-saldo <> 0 THEN DO.
                ASSIGN d-tot-juros-pend = d-tot-juros-pend + b-titulo.vl-saldo.

                FIND tt_juros_pend WHERE
                     tt_juros_pend.cod_estab = b-titulo.cod-estabel AND
                     tt_juros_pend.cod_espec_docto = b-titulo.cod-esp AND
                     tt_juros_pend.cod_ser_docto = b-titulo.serie AND
                     tt_juros_pend.cod_tit_acr = b-titulo.nr-docto AND
                     tt_juros_pend.cod_parcela = b-titulo.parcela
                     NO-LOCK NO-ERROR.
                IF NOT AVAIL tt_juros_pend THEN DO. 
                   CREATE tt_juros_pend.
                   ASSIGN tt_juros_pend.cod_estab = b-titulo.cod-estabel 
                          tt_juros_pend.cod_espec_docto = b-titulo.cod-esp
                          tt_juros_pend.cod_ser_docto = b-titulo.serie 
                          tt_juros_pend.cod_tit_acr = b-titulo.nr-docto 
                          tt_juros_pend.cod_parcela = b-titulo.parcela
                          tt_juros_pend.num_id_tit_acr = RECID(b-titulo).

                    ASSIGN tt_juros_pend.cod_portador = STRING(b-titulo.port)
                           tt_juros_pend.val_sdo_tit_acr = b-titulo.vl-original
                           tt_juros_pend.val_origin_tit_acr = b-titulo.vl-original
                           tt_juros_pend.dat_emis_docto = b-titulo.dt-emissao
                           tt_juros_pend.dat_vencto_tit_acr = b-titulo.dt-venc.
                END.
             END.
    
             /* Juros Quitados */
             IF b-titulo.tipo      = 6 AND
                b-titulo.vl-saldo  = 0 THEN
                ASSIGN d-tot-juros-quit = d-tot-juros-quit + b-titulo.vl-original.
             
                   
             /* Total quitado */
             IF (b-titulo.cod-esp  = 'DP'  OR 
                 b-titulo.cod-esp  = 'DT'  OR
                 b-titulo.cod-esp  = 'DG'  OR
                 b-titulo.cod-esp  = "PD"  OR 
                 b-titulo.cod-esp  = "AD"  OR 
                 b-titulo.cod-esp  = 'CD'  OR
                 b-titulo.cod-esp  = 'NP' )  AND
                 b-titulo.vl-saldo <> b-titulo.vl-original THEN  DO.
                 ASSIGN d-tot-quitado = d-tot-quitado + (b-titulo.vl-original - b-titulo.vl-saldo)
                        d-titulo-quitado = d-titulo-quitado + (b-titulo.vl-original - b-titulo.vl-saldo).
                 
                 RUN pi-cria-tt (INPUT "quitado").
             END.
    
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
                                       mov-tit.transacao = 2 ) THEN DO.
                     d-titulo-quitado-atraso = d-titulo-quitado-atraso + b-titulo.vl-original.
                     RUN pi-cria-tt (INPUT "liq_atraso").
                 END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-dados-ems5 w-consim 
PROCEDURE pi-dados-ems5 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR l-tit-reab-dev AS LOG.
                                                                                   
    DO WITH FRAME {&FRAME-NAME}:
    
       /* O c¢digo existente para tratamento de cheques e duplicatas foi movido para AS duas procedures abaixo */
       RUN pi-trata-cheques-ems5.
       /*RUN pi-trata-duplicatas-ems5.*/

       /* Devolucao Total */
       FOR EACH movto_tit_acr WHERE 
                movto_tit_acr.cdn_cliente   = i-cod-emitente  AND 
                movto_tit_acr.dat_transacao >= DATE(dt-inicial:SCREEN-VALUE) NO-LOCK,
           FIRST tt-estabelec-dest NO-LOCK WHERE
                 tt-estabelec-dest.cod-estabel = SUBSTR(movto_tit_acr.cod_estab,1,1):

            IF SUBSTR(movto_tit_acr.cod_estab,1,1) <> tt-estabelec-dest.cod-estabel THEN NEXT.

            IF movto_tit_acr.ind_trans_acr <> "Devolu‡Æo" THEN NEXT.

            /*IF movto_tit_acr.tipo <> 1 THEN  NEXT.*/   /* Normal */

            ASSIGN d-tot-devolucao = d-tot-devolucao + movto_tit_acr.val_movto_tit_acr.

            FIND tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.

            FIND tt_tit_devol WHERE
                 tt_tit_devol.cod_estab = tit_acr.cod_estab AND
                 tt_tit_devol.cod_espec_docto = tit_acr.cod_espec_docto AND
                 tt_tit_devol.cod_ser_docto = tit_acr.cod_ser_docto AND
                 tt_tit_devol.cod_tit_acr = tit_acr.cod_tit_acr AND
                 tt_tit_devol.cod_parcela = tit_acr.cod_parcela
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL tt_tit_devol THEN DO. 
               CREATE tt_tit_devol.
               BUFFER-COPY tit_acr TO tt_tit_devol.
            END.
       END. 
                                                                                            
       /* Busca duplicatas substitutas */
       FOR EACH tit_acr WHERE 
                tit_acr.cdn_cliente = i-cod-emitente NO-LOCK,
           FIRST tt-estabelec-dest NO-LOCK WHERE
                 tt-estabelec-dest.cod-estabel = SUBSTR(tit_acr.cod_estab,1,1):

           IF tit_acr.log_tit_acr_estordo THEN NEXT.

           IF tit_acr.dat_emis_docto < DATE(dt-inicial:SCREEN-VALUE) THEN NEXT.
              ASSIGN l-tit-reab-dev = NO.

           IF (tit_acr.cod_espec_docto  = 'DP'  OR 
               tit_acr.cod_espec_docto  = 'DT'  OR
               tit_acr.cod_espec_docto  = 'DG'  OR
               tit_acr.cod_espec_docto  = 'CD'  OR
               tit_acr.cod_espec_docto  = 'NP') AND
               tit_acr.val_sdo_tit_acr <> 0  THEN DO:
               IF CAN-FIND(FIRST movto_tit_acr OF tit_acr WHERE 
                                 movto_tit_acr.ind_trans_acr BEGINS "Acerto") THEN DO:  
                  ASSIGN d-sld-tit-reab-dev = d-sld-tit-reab-dev + tit_acr.val_sdo_tit_acr
                         l-tit-reab-dev     = YES.
               END.
           END.
        
           /* Juros Pendentes */
           IF tit_acr.ind_tip_espec_docto = "Aviso D‚bito" AND
              tit_acr.val_sdo_tit_acr <> 0 THEN DO.
              ASSIGN d-tot-juros-pend = d-tot-juros-pend + tit_acr.val_sdo_tit_acr.
              CREATE tt_juros_pend.
              BUFFER-COPY tit_acr TO tt_juros_pend.
           END.

           /* Juros Quitados */
           IF tit_acr.ind_tip_espec_docto = "Aviso D‚bito" AND
              tit_acr.val_sdo_tit_acr = 0 THEN 
              ASSIGN d-tot-juros-quit = d-tot-juros-quit + tit_acr.val_origin_tit_acr.

           /* Prorrogadas */
           IF tit_acr.dat_vencto_tit_acr > tit_acr.dat_vencto_origin_tit_acr OR
              CAN-FIND (FIRST movto_tit_acr OF tit_acr WHERE
                              movto_tit_acr.ind_trans_acr = "Liquida‡Æo Renegociac") THEN DO.
              ASSIGN i-duplic-prorrogada = i-duplic-prorrogada + 1
                     d-duplic-prorrogada = d-duplic-prorrogada + tit_acr.val_origin_tit_acr.

              CREATE tt_tit_prorrog.
              BUFFER-COPY tit_acr TO tt_tit_prorrog.
           END.
            
           /* Quitadas */
           IF tit_acr.dat_vencto_tit_acr > tit_acr.dat_vencto_origin_tit_acr AND
              tit_acr.val_sdo_tit_acr = 0 THEN DO.
              ASSIGN i-duplic-quitada = i-duplic-quitada + 1
                     d-duplic-quitada = d-duplic-quitada + tit_acr.val_origin_tit_acr.

              CREATE tt_tit_prorrog_quitado.
              BUFFER-COPY tit_acr TO tt_tit_prorrog_quitado.
           END.

           /* A Quitar */
           IF tit_acr.dat_vencto_tit_acr > tit_acr.dat_vencto_origin_tit_acr AND 
              tit_acr.val_sdo_tit_acr    <> 0 THEN DO.
              ASSIGN i-duplic-quitar = i-duplic-quitar + 1
                     d-duplic-quitar = d-duplic-quitar + tit_acr.val_origin_tit_acr.

              CREATE tt_tit_prorrog_aberto.
              BUFFER-COPY tit_acr TO tt_tit_prorrog_aberto.
           END.

           IF NOT l-tit-reab-dev THEN DO:
              /* Totais (compensar e atraso) */
              IF (tit_acr.cod_espec_docto = 'DP'  OR 
                  tit_acr.cod_espec_docto = 'DT'  OR
                  tit_acr.cod_espec_docto = 'DG'  OR
                  tit_acr.cod_espec_docto = "PD"  OR 
                  tit_acr.cod_espec_docto = 'CD'  OR
                  tit_acr.cod_espec_docto = 'NP') THEN DO:
                  IF tit_acr.dat_vencto_tit_acr >= TODAY AND
                     tit_acr.val_sdo_tit_acr <> 0 THEN DO.
                     ASSIGN d-tot-compensar = d-tot-compensar + tit_acr.val_sdo_tit_acr
                            d-tot-titulo-compensar = d-tot-titulo-compensar + tit_acr.val_sdo_tit_acr.

                     CREATE tt_tit_compsar.
                     BUFFER-COPY tit_acr TO tt_tit_compsar.
                  END.
        
                  IF tit_acr.val_sdo_tit_acr <> 0 AND 
                     tit_acr.dat_vencto_tit_acr < TODAY THEN DO.
                     d-tot-atraso = d-tot-atraso + tit_acr.val_sdo_tit_acr.

                     CREATE tt_tit_atraso.
                     BUFFER-COPY tit_acr TO tt_tit_atraso.
                  END.
              END.
           END.

           /* Devolucao Total */
           IF tit_acr.cod_espec_docto = 'DM' THEN DO.
              d-tot-devolucao = d-tot-devolucao + tit_acr.val_origin_tit_acr.

              FIND tt_tit_devol WHERE
                   tt_tit_devol.cod_estab = tit_acr.cod_estab AND
                   tt_tit_devol.cod_espec_docto = tit_acr.cod_espec_docto AND
                   tt_tit_devol.cod_ser_docto = tit_acr.cod_ser_docto AND
                   tt_tit_devol.cod_tit_acr = tit_acr.cod_tit_acr AND
                   tt_tit_devol.cod_parcela = tit_acr.cod_parcela
                   NO-LOCK NO-ERROR.
              IF NOT AVAIL tt_tit_devol THEN DO. 
                 CREATE tt_tit_devol.
                 BUFFER-COPY tit_acr TO tt_tit_devol.
              END.
           END.
                                                                                   
    
           /* Total quitado */
           IF (tit_acr.cod_espec_docto  = 'DP'  OR  tit_acr.cod_espec_docto  = 'DT'  OR
               tit_acr.cod_espec_docto  = 'DG'  OR  tit_acr.cod_espec_docto  = 'PD'  OR         
               tit_acr.cod_espec_docto  = 'AD'  OR  tit_acr.cod_espec_docto  = 'CD'  OR
               tit_acr.cod_espec_docto  = 'NP') AND
               tit_acr.val_sdo_tit_acr <> tit_acr.val_origin_tit_acr THEN DO.

               
               FIND FIRST movto_tit_acr OF tit_acr WHERE
                          movto_tit_acr.ind_trans_acr = "Liquida‡Æo Subst" OR
                          movto_tit_acr.ind_trans_acr = "Liquida‡Æo Renegociac" 
                          NO-LOCK NO-ERROR.

               IF NOT AVAIL movto_tit_acr THEN DO.  
                  ASSIGN d-tot-quitado = d-tot-quitado + (tit_acr.val_origin_tit_acr - tit_acr.val_sdo_tit_acr)
                         d-titulo-quitado = d-titulo-quitado + (tit_acr.val_origin_tit_acr - tit_acr.val_sdo_tit_acr).
    
                  CREATE tt_tit_quitado.
                  BUFFER-COPY tit_acr TO tt_tit_quitado.
               END.
           END.
    
           /* tit_acrs Liquidados c/ Atraso - 2 */
           IF (tit_acr.cod_espec_docto = 'DP'  OR 
               tit_acr.cod_espec_docto = 'DT'  OR
               tit_acr.cod_espec_docto = 'DG'  OR
               tit_acr.cod_espec_docto = 'CD'  OR
               tit_acr.cod_espec_docto = 'NP') AND 
               tit_acr.val_sdo_tit_acr = 0    AND
               tit_acr.dat_liquidac_tit_acr > tit_acr.dat_vencto_tit_acr  THEN DO:
               IF NOT CAN-FIND(FIRST movto_tit_acr OF tit_acr WHERE
                                     movto_tit_acr.ind_trans_acr = "Liquida‡Æo Subst" AND
                                     movto_tit_acr.ind_trans_acr = "Liquida‡Æo Renegociac" AND
                                     movto_tit_acr.ind_trans_acr = "Baixa" ) THEN DO.
                   ASSIGN d-titulo-quitado-atraso = d-titulo-quitado-atraso + tit_acr.val_origin_tit_acr.

                   CREATE tt_tit_liq_atraso.
                   BUFFER-COPY tit_acr TO tt_tit_liq_atraso.
               END.
           END.

           /*
           /* pago com cheque */
           FOR EACH relacto_cheq_acr OF tit_acr NO-LOCK.
               FIND cheq_acr OF relacto_cheq_acr NO-LOCK NO-ERROR.

               DISP cheq_acr.num_cheq
                    relacto_cheq_acr.val_vincul_cheq_acr (TOTAL).

               DISP relacto_cheq_acr.val_vincul_cheq_acr (TOTAL).
           END.
           */
        

           /* TON
           /* tit_acrs Liquidados Cartorio */
           IF (tit_acr.cod_espec_docto = 'DP'  OR 
               tit_acr.cod_espec_docto = 'DT'  OR
               tit_acr.cod_espec_docto = 'DG'  OR
               tit_acr.cod_espec_docto = 'CD'  OR
               tit_acr.cod_espec_docto = 'NP') AND 
               tit_acr.val_sdo_tit_acr = 0    THEN DO:
               IF CAN-FIND(FIRST his-tit OF tit_acr WHERE
                                 his-tit.historico BEGINS 'Liquidacao em cartorio') THEN
                  d-tit_acr-quitado-cartorio = d-tit_acr-quitado-cartorio + tit_acr.val_origin_tit_acr.
           END.
           */
       END. 
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-cheques-ems2 w-consim 
PROCEDURE pi-trata-cheques-ems2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    /* Informa‡äes de Cheques */
    FOR EACH cheque WHERE
             cheque.cod-emitente  = i-cod-emitente NO-LOCK,
        
        FIRST tt-estabelec-dest NO-LOCK WHERE
              tt-estabelec-dest.cod-estabel = cheque.cod-estabel-mvto:

        IF cheque.dt-emissao < DATE(dt-inicial:SCREEN-VALUE) THEN NEXT.
        
        IF cheque.origem-cheque <> 6 /*CR*/ THEN NEXT.

        IF cheque.tipo-cheque = 1 AND
          (cheque.situacao-cheque = 1  /* Pendente */   OR
           cheque.situacao-cheque = 2  /* Depositado */ OR
           cheque.situacao-cheque = 5  /* Devolvido */  OR
           cheque.situacao-cheque = 7  /* Descontado */ OR
           cheque.situacao-cheque = 8) /* Compensado */ THEN
            d-cheque-proprio = d-cheque-proprio + cheque.vl-cheque.

        IF cheque.tipo-cheque = 2 AND
          (cheque.situacao-cheque = 1  /* Pendente */   OR
           cheque.situacao-cheque = 2  /* Depositado */ OR
           cheque.situacao-cheque = 5  /* Devolvido */  OR
           cheque.situacao-cheque = 7  /* Descontado */ OR
           cheque.situacao-cheque = 8) /* Compensado */ THEN
            d-bordero = d-bordero + cheque.vl-cheque.


        /* Total Pendentes sem devolucao */
        IF NOT cheque.devolvido             AND 
            cheque.situacao-cheque = 1 /* Pendente */ THEN DO.
            ASSIGN d-tot-compensar = d-tot-compensar + cheque.vl-cheque
                   d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + cheque.vl-cheque.
            RUN pi-cria-tc (INPUT "compsar").
        END.

        /*** Cheques nÆo devolvidos e depositados com data de vencimento futura devem ser considerados como a compensar ***/
        IF NOT cheque.devolvido   AND 
           cheque.situacao-cheque = 2 /* Depositado */ AND 
           cheque.dt-vencimento > TODAY /* Vencimento Futuro */ THEN DO.
            ASSIGN d-tot-compensar = d-tot-compensar + cheque.vl-cheque
                   d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + cheque.vl-cheque.
            RUN pi-cria-tc (INPUT "compsar").
        END.

        /*** Cheques nÆo devolvidos e caucionados com data de vencimento futura devem ser considerados como a compensar ***/
        IF NOT cheque.devolvido   AND 
           cheque.situacao-cheque = 3 /* Cau‡Æo */ AND 
           cheque.dt-vencimento > TODAY /* Vencimento Futuro */ THEN DO.
           ASSIGN d-tot-compensar = d-tot-compensar + cheque.vl-cheque
                  d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + cheque.vl-cheque.
           RUN pi-cria-tc (INPUT "compsar").
        END.

        /* Pendentes */
        IF (cheque.situacao-cheque       = 1 /* Pendente */   OR 
            cheque.situacao-cheque       = 5) /* Devolvido */ THEN
            ASSIGN d-tot-quitado         = d-tot-quitado - cheque.vl-cheque 
                   d-tot-cheque-aberto   = d-tot-cheque-aberto + cheque.vl-cheque.

        /* Os cheques devolvidos e depositados com data futura serÆo considerados como pendentes. */
        IF cheque.devolvido AND
           cheque.situacao-cheque = 2 /* Depositado */ AND 
           cheque.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
            ASSIGN d-tot-cheque-aberto   = d-tot-cheque-aberto + cheque.vl-cheque.

        /*** As baixas de titulos com cheques depositados com data futura nÆo devem ser consideradas como quita‡Æo,
             pois o valor s¢ estar  realmente quitado quando a data de vencimento for menor ou igual a data atual ***/
        IF cheque.situacao-cheque = 2 /* Depositado */ AND 
           cheque.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
            ASSIGN d-tot-quitado = d-tot-quitado - cheque.vl-cheque.

        /*** As baixas de titulos com cheques caucionados com data futura nÆo devem ser consideradas como quita‡Æo,
             pois o valor s¢ estar  realmente quitado quando a data de vencimento for menor ou igual a data atual ***/
        IF cheque.situacao-cheque = 3 /* Cau‡Æo */ AND 
           cheque.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
           ASSIGN d-tot-quitado = d-tot-quitado - cheque.vl-cheque.

        /* Total Pendentes devolvidos */
        IF cheque.devolvido             AND 
           (cheque.situacao-cheque = 1 /* Pendente */   OR 
            cheque.situacao-cheque = 5) /* Devolvido */ THEN
            ASSIGN d-tot-cheque-devolv-pendente = d-tot-cheque-devolv-pendente + cheque.vl-cheque.

        /* Caso o cheque tenha sido devolvido e depositado novamente com data de vencimento futura deve
           ser considerado como devolvido pendente */ 
        IF cheque.devolvido             AND 
           cheque.situacao-cheque = 2   /* Depositado */   AND 
           cheque.dt-vencimento > TODAY /* Vencimento Futuro */ THEN
           ASSIGN d-tot-cheque-devolv-pendente = d-tot-cheque-devolv-pendente + cheque.vl-cheque.

        /* Quitados */
        IF cheque.devolvido AND
           /* Os cheques devolvidos e depositados com data futura serÆo considerados como quitados
              somente qdo a data de vencimento for menor ou igual … data atual. */
           ( (cheque.situacao-cheque = 2 /* Depositado */ AND cheque.dt-vencimento <= TODAY ) OR
            cheque.situacao-cheque = 6 /* Cancelado  */ OR
            cheque.situacao-cheque = 7 /* Descontado */ OR
            cheque.situacao-cheque = 8)/* Compensado */ THEN
            ASSIGN d-tot-cheque-quitado = d-tot-cheque-quitado + cheque.vl-cheque.
        
        /* Prorrogados */
        IF cheque.dt-vencimento > cheque.data-2 THEN DO.
            ASSIGN i-cheque-prorrogado = i-cheque-prorrogado + 1
                   d-cheque-prorrogado = d-cheque-prorrogado + cheque.vl-cheque.
            RUN pi-cria-tc (INPUT "prorrog").
        END.
        
        /* Compensados */
        IF cheque.dt-vencimento > cheque.data-2 AND
           (cheque.situacao-cheque = 2  /* Depositado */ OR
            cheque.situacao-cheque = 7  /* Descontado */ OR
            cheque.situacao-cheque = 8) /* Compensado */ THEN DO.
            ASSIGN i-cheque-compensado = i-cheque-compensado + 1
                   d-cheque-compensado = d-cheque-compensado + cheque.vl-cheque.
        
            RUN pi-cria-tc (INPUT "prorrog_compsado").
        END.

        /* A Compensar */
        IF cheque.dt-vencimento > cheque.data-2 AND 
           (cheque.situacao-cheque = 1  /* Pendente */   OR
            cheque.situacao-cheque = 5)  /* Devolvido */ THEN DO.
            ASSIGN i-cheque-compensar  = i-cheque-compensar + 1
                   d-cheque-compensar = d-cheque-compensar + cheque.vl-cheque.
            RUN pi-cria-tc (INPUT "prorrog_compsar").
        END.
    END. /* each cheque */
END. /* do with frame */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-cheques-ems5 w-consim 
PROCEDURE pi-trata-cheques-ems5 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH cheq_acr WHERE 
                 cheq_acr.cdn_cliente    = i-cod-emitente AND
                 cheq_acr.dat_emis_cheq >= DATE(dt-inicial:SCREEN-VALUE) NO-LOCK,
           FIRST tt-estabelec-dest NO-LOCK WHERE
                 tt-estabelec-dest.cod-estabel = SUBSTR(cheq_acr.cod_estab,1,1):

            IF cheq_acr.ind_orig_cheq_acr <> "Liquidacao" THEN NEXT.

            IF cheq_acr.log_cheq_terc = NO THEN  /* Cheque Proprio */
               ASSIGN d-cheque-proprio = d-cheque-proprio + cheq_acr.val_cheque.
            ELSE  /* Cheque de Terceiro */
               ASSIGN d-bordero = d-bordero + cheq_acr.val_cheque.

            /* Total Pendentes sem devolucao */
            IF NOT cheq_acr.log_cheq_acr_devolv  AND 
               cheq_acr.dat_apres_cheq_acr = ? /* Pendente */ THEN DO.

               FIND tt_cheque_aberto WHERE
                    tt_cheque_aberto.num_cheq = cheq_acr.num_cheq NO-ERROR.
               IF NOT AVAIL tt_cheque_aberto THEN DO.

                   ASSIGN d-tot-cheque-aberto = d-tot-cheque-aberto + cheq_acr.val_cheque
                          d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + cheq_acr.val_cheque.
                   /*
                   ASSIGN d-cheque-compensar = d-cheque-compensar + cheq_acr.val_cheque
                          i-cheque-compensar  = i-cheque-compensar + 1.
                   */
                  CREATE tt_cheque_aberto.
                  BUFFER-COPY cheq_acr TO tt_cheque_aberto.
               END.
            END.

            IF NOT cheq_acr.log_cheq_acr_devolv   AND 
               cheq_acr.dat_apres_cheq_acr <> ? /* Depositado */ AND 
               cheq_acr.dat_prev_apres_cheq_acr > TODAY /* Vencimento Futuro */ THEN DO.
               ASSIGN d-cheque-compensar = d-cheque-compensar + cheq_acr.val_cheque
                      i-cheque-compensar  = i-cheque-compensar + 1
                      d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + cheq_acr.val_cheque.

               FIND tt_cheque_compsar WHERE
                    tt_cheque_compsar.num_cheq = cheq_acr.num_cheq NO-ERROR.
               IF NOT AVAIL tt_cheque_compsar THEN DO.
                   ASSIGN d-tot-cheque-aberto = d-tot-cheque-aberto + cheq_acr.val_cheque
                          d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + cheq_acr.val_cheque.

                  CREATE tt_cheque_compsar.
                  BUFFER-COPY cheq_acr TO tt_cheque_compsar.
               END.
            END.

            IF NOT cheq_acr.log_cheq_acr_devolv   AND 
               cheq_acr.dat_prev_apres_cheq_acr = ? /* Cau‡Æo */ AND 
               cheq_acr.dat_prev_cr_cheq_acr > TODAY /* Vencimento Futuro */ THEN DO.
               FIND tt_cheque_aberto WHERE
                    tt_cheque_aberto.num_cheq = cheq_acr.num_cheq NO-ERROR.
               IF NOT AVAIL tt_cheque_aberto THEN DO.
                   /*
                   ASSIGN d-cheque-compensar = d-cheque-compensar + cheq_acr.val_cheque
                          i-cheque-compensar  = i-cheque-compensar + 1.
                   */
                   ASSIGN d-tot-cheque-aberto = d-tot-cheque-aberto + cheq_acr.val_cheque
                          d-tot-cheque-pendente-semdevol = d-tot-cheque-pendente-semdevol + cheq_acr.val_cheque.

                  CREATE tt_cheque_aberto.
                  BUFFER-COPY cheq_acr TO tt_cheque_aberto.
               END.
            END.

            IF cheq_acr.log_cheq_acr_devolv OR /* Devolvido */
               cheq_acr.dat_apres_cheq_acr = ? /* Pendente */ THEN DO.
               ASSIGN d-tot-quitado         = d-tot-quitado - cheq_acr.val_cheque .

               CREATE tt_cheque_pend.
               BUFFER-COPY cheq_acr TO tt_cheque_pend.
            END.


            IF cheq_acr.log_cheq_acr_devolv AND
               cheq_acr.dat_apres_cheq_acr <> ? /* Depositado */ AND 
               cheq_acr.dat_prev_cr_cheq_acr > TODAY /* Vencimento Futuro */ THEN DO.
               ASSIGN d-tot-cheque-compensar = d-tot-cheque-compensar + cheq_acr.val_cheque
                      i-cheque-compensar  = i-cheque-compensar + 1.

               FIND tt_cheque_compsar WHERE
                    tt_cheque_compsar.num_cheq = cheq_acr.num_cheq NO-ERROR.
               IF NOT AVAIL tt_cheque_compsar THEN DO.
                  CREATE tt_cheque_compsar.
                  BUFFER-COPY cheq_acr TO tt_cheque_compsar.
               END.
            END.
    
            IF cheq_acr.dat_apres_cheq_acr <> ? /* Depositado */ AND 
               cheq_acr.dat_prev_cr_cheq_acr > TODAY /* Vencimento Futuro */ THEN
               ASSIGN d-tot-quitado = d-tot-quitado - cheq_acr.val_cheque.

            IF cheq_acr.dat_prev_apres_cheq_acr = ? /* Cau‡Æo */ AND 
               cheq_acr.dat_prev_cr_cheq_acr > TODAY /* Vencimento Futuro */ THEN
               ASSIGN d-tot-quitado = d-tot-quitado - cheq_acr.val_cheque.

            IF cheq_acr.log_cheq_acr_devolv    AND 
               cheq_acr.dat_apres_cheq_acr = ? THEN
               ASSIGN d-tot-cheque-devolv-pendente = d-tot-cheque-devolv-pendente + cheq_acr.val_cheque.

            IF cheq_acr.log_cheq_acr_devolv  AND 
               cheq_acr.dat_apres_cheq_acr <> ?  /* Depositado */   AND 
               cheq_acr.dat_prev_cr_cheq_acr > TODAY /* Vencimento Futuro */ THEN
                ASSIGN d-tot-cheque-devolv-pendente = d-tot-cheque-devolv-pendente + cheq_acr.val_cheque.

            IF cheq_acr.log_cheq_acr_devolv AND
               ( (cheq_acr.dat_apres_cheq_acr <> ?  /* Depositado */ AND 
                  cheq_acr.dat_prev_cr_cheq_acr <= TODAY ) OR
                cheq_acr.log_cheq_acr_renegoc)/* Compensado */ THEN 
                ASSIGN d-tot-cheque-quitado = d-tot-cheque-quitado + cheq_acr.val_cheque.

            IF cheq_acr.dat_prev_cr_cheq_acr < TODAY AND
               NOT cheq_acr.log_cheq_acr_devolv AND 
               cheq_acr.dat_renegoc_cheq_acr = ? THEN DO.
               FIND tt_cheque_compsado WHERE
                    tt_cheque_compsado.num_cheq = cheq_acr.num_cheq NO-ERROR.
               IF NOT AVAIL tt_cheque_compsado THEN DO.
                  CREATE tt_cheque_compsado.
                  BUFFER-COPY cheq_acr TO tt_cheque_compsado.
               END.
            END.

            
            /* Prorrogados ou Renegociados */
            IF cheq_acr.dat_renegoc_cheq_acr <> ?  THEN DO.
               IF cheq_acr.dat_renegoc_cheq_acr > TODAY THEN DO.
                   ASSIGN i-cheque-prorrogado = i-cheque-prorrogado + 1
                          d-cheque-prorrogado = d-cheque-prorrogado + cheq_acr.val_cheque.
                   FIND tt_ch_prorrog WHERE
                        tt_ch_prorrog.num_cheq = cheq_acr.num_cheq NO-ERROR.
                   IF NOT AVAIL tt_ch_prorrog THEN DO.
                      CREATE tt_ch_prorrog.
                      BUFFER-COPY cheq_acr TO tt_ch_prorrog.
                   END.
               END.

               /* Compensados */
               IF cheq_acr.dat_prev_cr_cheq_acr < TODAY AND
                  NOT cheq_acr.log_cheq_acr_devolv THEN DO.
                  ASSIGN i-cheque-compensado = i-cheque-compensado + 1
                         d-cheque-compensado = d-cheque-compensado + cheq_acr.val_cheque.
                  FIND tt_ch_prorrog_compsado WHERE
                       tt_ch_prorrog_compsado.num_cheq = cheq_acr.num_cheq NO-ERROR.
                  IF NOT AVAIL tt_ch_prorrog_compsado THEN DO.
                     CREATE tt_ch_prorrog_compsado.
                     BUFFER-COPY cheq_acr TO tt_ch_prorrog_compsado.
                  END.
               END.
                
               /* A Compensar */
               IF cheq_acr.dat_prev_cr_cheq_acr > TODAY OR
                  cheq_acr.log_cheq_acr_devolv THEN DO.
                  ASSIGN i-cheque-compensar  = i-cheque-compensar + 1
                         d-cheque-compensar = d-cheque-compensar + cheq_acr.val_cheque.
                  FIND tt_ch_prorrog_compsar WHERE
                       tt_ch_prorrog_compsar.num_cheq = cheq_acr.num_cheq NO-ERROR.
                  IF NOT AVAIL tt_ch_prorrog_compsar THEN DO.
                     CREATE tt_ch_prorrog_compsar.
                     BUFFER-COPY cheq_acr TO tt_ch_prorrog_compsar.
                  END.
               END.
            END.
        END. /* each cheq_acr */
    END. /* do with frame */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-duplicatas-ems2 w-consim 
PROCEDURE pi-trata-duplicatas-ems2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    /* Informa‡äes sobre Duplicatas */
   FOR EACH titulo FIELDS(cod-emitente
                           cod-estabel
                           serie
                           parcela
                           nr-docto
                           dt-emissao
                           ep-codigo
                           dt-vencimen
                           data-1
                           vl-saldo
                           cod-esp
                           tipo
                           dt-ult-pagto
                           vl-original)
             WHERE titulo.cod-emitente = i-cod-emitente NO-LOCK,
                                                                                        /***DMJGD001.sn***/
        FIRST tt-estabelec-dest NO-LOCK WHERE
              tt-estabelec-dest.cod-estabel = titulo.cod-estabel:
                                                                                        /***DMJGD001.en***/
        ASSIGN l-tit-reab-dev = NO.

        IF titulo.dt-emissao < DATE(dt-inicial:SCREEN-VALUE) THEN
            NEXT.
                                                                                        /***DMJGD001.so***
        IF titulo.ep-codigo <> i-empresa-ima  AND
           titulo.ep-codigo <> i-empresa-inter THEN
            NEXT.
                                                                                         ***DMJGD001.eo***/    
        /* Ignora duplicatas substituidas */
        IF CAN-FIND(FIRST mov-tit OF titulo WHERE mov-tit.baixa-subs) THEN NEXT.
                                                                                        
        
        /*
        /***DMJG003.sn***/
        /*** Verifica se o t¡tulo foi reaberto devido … devolu‡Æo de cheques, buscando uma transa‡Æo do tipo 
             AVA no portador 100 para o mesmo. Caso encontre este t¡tulo ser  tratado na devolu‡aä de cheques ***/
        IF (titulo.cod-esp  = 'DP'  OR 
            titulo.cod-esp  = 'DT'  OR
            titulo.cod-esp  = 'DG'  OR
                                                                                        /***DMJGD002.sn***/
            titulo.cod-esp  = 'CD'  OR
                                                                                        /***DMJGD002.en***/
            titulo.cod-esp  = 'NP') AND
            titulo.vl-saldo <> 0  THEN DO:
                                                                                        /***DMJG004.so***
          IF CAN-FIND(FIRST mov-tit OF titulo WHERE mov-tit.transacao = 13 /* AVA */ AND mov-tit.cod-portador = 100) THEN DO:
                                                                                         ***DMJG004.eo***/
                                                                                        /***DMJG004.sn***/
            IF CAN-FIND(FIRST mov-tit OF titulo WHERE mov-tit.transacao = 13 /* AVA */ AND SUBSTRING(mov-tit.char-1, 1, 6) = "DEV_CH") THEN DO:
                                                                                        /***DMJG004.en***/
                ASSIGN d-sld-tit-reab-dev = d-sld-tit-reab-dev + titulo.vl-saldo
                       l-tit-reab-dev     = YES.
            END.
        END.
                                                                                        /***DMJG003.en***/
        /* Prorrogadas */
        IF titulo.dt-vencimen > titulo.data-1 THEN 
            ASSIGN i-duplic-prorrogada = i-duplic-prorrogada + 1
                   d-duplic-prorrogada = d-duplic-prorrogada + titulo.vl-original.
        
        /* Quitadas */
        IF titulo.dt-vencimen > titulo.data-1 AND
           titulo.vl-saldo = 0 THEN
            ASSIGN i-duplic-quitada = i-duplic-quitada + 1
                   d-duplic-quitada = d-duplic-quitada + titulo.vl-original.
    
        /* A Quitar */
        IF titulo.dt-vencimen > titulo.data-1 AND 
           titulo.vl-saldo    <> 0 THEN
            ASSIGN i-duplic-quitar = i-duplic-quitar + 1
                   d-duplic-quitar = d-duplic-quitar + titulo.vl-original.
        */                                                                                

        /*
        /***DMJG003.sn***/
        IF NOT l-tit-reab-dev THEN DO:
                                                                                        /***DMJG003.en***/
            /* Totais (compensar e atraso) */
            IF (titulo.cod-esp = 'DP'  OR 
                titulo.cod-esp = 'DT'  OR
                titulo.cod-esp = 'DG'  OR
                titulo.cod-esp = "PD"  OR                                                 /***FOMM.NEW***/
                                                                                        /***DMJGD002.sn***/
                titulo.cod-esp = 'CD'  OR
                                                                                        /***DMJGD002.en***/
                titulo.cod-esp = 'NP') THEN DO:
                IF titulo.dt-vencimen >=  TODAY AND
                   titulo.vl-saldo    <> 0      THEN
                   ASSIGN d-tot-compensar        = d-tot-compensar + titulo.vl-saldo
                          d-tot-titulo-compensar = d-tot-titulo-compensar + titulo.vl-saldo.
    
                IF titulo.vl-saldo <> 0 AND 
                   titulo.dt-vencimen < TODAY THEN
                   d-tot-atraso = d-tot-atraso + titulo.vl-saldo.
            END.
                                                                                        /***DMJG003.sn***/
        END.
        
                                                                                        
        /***DMJG003.en***/
        /* Devolucao Total */
        IF titulo.cod-esp = 'DM' THEN
           d-tot-devolucao = d-tot-devolucao + titulo.vl-original.

        /* Juros Pendentes */
        IF titulo.tipo     = 6 AND
           titulo.cod-esp  <> "PD" AND                                                     /***FOMM.NEW***/
           titulo.vl-saldo <> 0 THEN
           d-tot-juros-pend = d-tot-juros-pend + titulo.vl-saldo.
    
        /* Juros Quitados */
        IF titulo.tipo      = 6 AND
           titulo.vl-saldo  = 0 THEN
           d-tot-juros-quit = d-tot-juros-quit + titulo.vl-original.
        */
        
        /* Total quitado */
        IF (titulo.cod-esp  = 'DP'  OR 
            titulo.cod-esp  = 'DT'  OR
            titulo.cod-esp  = 'DG'  OR
            titulo.cod-esp  = 'PD'  OR 
            titulo.cod-esp  = 'AD'  OR 
            titulo.cod-esp  = 'CD'  OR
            titulo.cod-esp  = 'NP')  AND
            titulo.vl-saldo <> titulo.vl-original THEN DO.

            ASSIGN d-tot-quitado = d-tot-quitado + (titulo.vl-original - titulo.vl-saldo)
                   d-titulo-quitado = d-titulo-quitado + (titulo.vl-original - titulo.vl-saldo).

            FIND tt_tit_quitado WHERE
                 tt_tit_quitado.cod_estab = titulo.cod-estabel AND
                 tt_tit_quitado.cod_espec_docto = titulo.cod-esp AND
                 tt_tit_quitado.cod_ser_docto = titulo.serie AND
                 tt_tit_quitado.cod_tit_acr = titulo.nr-docto AND
                 tt_tit_quitado.cod_parcela = titulo.parcela
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL tt_tit_quitado THEN DO. 
               CREATE tt_tit_quitado.
               ASSIGN tt_tit_quitado.cod_estab = titulo.cod-estabel 
                      tt_tit_quitado.cod_espec_docto = titulo.cod-esp
                      tt_tit_quitado.cod_ser_docto = titulo.serie 
                      tt_tit_quitado.cod_tit_acr = titulo.nr-docto 
                      tt_tit_quitado.cod_parcela = titulo.parcela
                      tt_tit_quitado.num_id_tit_acr = RECID(titulo).

                ASSIGN tt_tit_quitado.cod_portador = '9'
                       tt_tit_quitado.val_sdo_tit_acr = titulo.vl-original
                       tt_tit_quitado.val_origin_tit_acr = titulo.vl-original
                       tt_tit_quitado.dat_emis_docto = titulo.dt-emissao
                       tt_tit_quitado.dat_vencto_tit_acr = titulo.dt-venc.
            END.
        END.

        /*
       /* Titulos Liquidados c/ Atraso - 2 */
       IF (titulo.cod-esp = 'DP'  OR 
           titulo.cod-esp = 'DT'  OR
           titulo.cod-esp = 'DG'  OR
                                                                                        /***DMJGD002.sn***/
           titulo.cod-esp = 'CD'  OR
                                                                                        /***DMJGD002.en***/
           titulo.cod-esp = 'NP') AND 
           titulo.vl-saldo = 0    AND
           titulo.dt-ult-pagto > titulo.dt-vencimen  THEN DO:

           IF NOT CAN-FIND(FIRST mov-tit OF titulo WHERE
                                 mov-tit.baixa-subs AND
                                 mov-tit.transacao = 2 ) THEN
               d-titulo-quitado-atraso = d-titulo-quitado-atraso + titulo.vl-original.
       END.

       /* Titulos Liquidados Cartorio */
       IF (titulo.cod-esp = 'DP'  OR 
           titulo.cod-esp = 'DT'  OR
           titulo.cod-esp = 'DG'  OR
                                                                                        /***DMJGD002.sn***/
           titulo.cod-esp = 'CD'  OR
                                                                                        /***DMJGD002.en***/
           titulo.cod-esp = 'NP') AND 
           titulo.vl-saldo = 0    THEN DO:

          IF CAN-FIND(FIRST his-tit OF titulo WHERE
                            his-tit.historico BEGINS 'Liquidacao em cartorio') THEN
              d-titulo-quitado-cartorio = d-titulo-quitado-cartorio + titulo.vl-original.
       END.
       */
   END. /* each titulo */
END. /* do with frame */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trata-duplicatas-ems5 w-consim 
PROCEDURE pi-trata-duplicatas-ems5 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /********************* E M S  5 *********************** */
   /* Informa‡äes sobre Duplicatas */
   FOR EACH tit_acr WHERE 
            tit_acr.cdn_cliente = i-cod-emitente NO-LOCK,
        FIRST tt-estabelec-dest NO-LOCK WHERE
              tt-estabelec-dest.cod-estabel = SUBSTR(tit_acr.cod_estab,1,1):
                                                                                   
        ASSIGN l-tit-reab-dev = NO.

        IF tit_acr.dat_emis_docto < DATE(dt-inicial:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN NEXT.

        /* Ignora duplicatas substituidas */
        IF CAN-FIND(FIRST movto_tit_acr OF tit_acr WHERE
                          movto_tit_acr.ind_trans_acr = "Liquida‡Æo Subst") THEN NEXT.

        IF (tit_acr.cod_espec_docto  = 'DP'  OR 
            tit_acr.cod_espec_docto  = 'DT'  OR
            tit_acr.cod_espec_docto  = 'DG'  OR
            tit_acr.cod_espec_docto  = 'CD'  OR
            tit_acr.cod_espec_docto  = 'NP') AND
            tit_acr.val_sdo_tit_acr <> 0  THEN DO:
            IF CAN-FIND(FIRST movto_tit_acr OF tit_acr WHERE 
                              movto_tit_acr.ind_trans_acr BEGINS "Acerto") THEN DO:  
               ASSIGN d-sld-tit-reab-dev = d-sld-tit-reab-dev + tit_acr.val_sdo_tit_acr
                      l-tit-reab-dev     = YES.
            END.
        END.

        /* Prorrogadas */
        IF tit_acr.dat_vencto_tit_acr > tit_acr.dat_vencto_origin_tit_acr THEN 
            ASSIGN i-duplic-prorrogada = i-duplic-prorrogada + 1
                   d-duplic-prorrogada = d-duplic-prorrogada + tit_acr.val_origin_tit_acr.
        
        /* Quitadas */
        IF tit_acr.dat_vencto_tit_acr > tit_acr.dat_vencto_origin_tit_acr AND
           tit_acr.val_sdo_tit_acr = 0 THEN
            ASSIGN i-duplic-quitada = i-duplic-quitada + 1
                   d-duplic-quitada = d-duplic-quitada + tit_acr.val_origin_tit_acr.
    
        /* A Quitar */
        IF tit_acr.dat_vencto_tit_acr > tit_acr.dat_vencto_origin_tit_acr AND 
           tit_acr.val_sdo_tit_acr    <> 0 THEN
            ASSIGN i-duplic-quitar = i-duplic-quitar + 1
                   d-duplic-quitar = d-duplic-quitar + tit_acr.val_origin_tit_acr.

        IF NOT l-tit-reab-dev THEN DO:
            /* Totais (compensar e atraso) */
            IF (tit_acr.cod_espec_docto = 'DP'  OR 
                tit_acr.cod_espec_docto = 'DT'  OR
                tit_acr.cod_espec_docto = 'DG'  OR
                tit_acr.cod_espec_docto = "PD"  OR 
                tit_acr.cod_espec_docto = 'CD'  OR
                tit_acr.cod_espec_docto = 'NP') THEN DO:
                IF tit_acr.dat_vencto_tit_acr >=  TODAY AND
                   tit_acr.val_sdo_tit_acr    <> 0      THEN
                   ASSIGN d-tot-compensar        = d-tot-compensar + tit_acr.val_sdo_tit_acr
                          d-tot-titulo-compensar = d-tot-titulo-compensar + tit_acr.val_sdo_tit_acr.
    
                IF tit_acr.val_sdo_tit_acr <> 0 AND 
                   tit_acr.dat_vencto_tit_acr < TODAY THEN DO.
                   d-tot-atraso = d-tot-atraso + tit_acr.val_sdo_tit_acr.

                END.
            END.
        END.

        /* Devolucao Total */
        IF tit_acr.cod_espec_docto = 'DM' THEN
           d-tot-devolucao = d-tot-devolucao + tit_acr.val_origin_tit_acr.

        /* TON
        /* Juros Pendentes */
        IF tit_acr.tipo     = 6 AND
           tit_acr.cod_espec_docto  <> "PD" AND         
           tit_acr.val_sdo_tit_acr <> 0 THEN
           d-tot-juros-pend = d-tot-juros-pend + tit_acr.val_sdo_tit_acr.
    
        /* Juros Quitados */
        IF tit_acr.tipo      = 6 AND
           tit_acr.val_sdo_tit_acr  = 0 THEN
           d-tot-juros-quit = d-tot-juros-quit + tit_acr.val_origin_tit_acr.
         */
          
        /* Total quitado */
        IF (tit_acr.cod_espec_docto  = 'DP'  OR 
            tit_acr.cod_espec_docto  = 'DT'  OR
            tit_acr.cod_espec_docto  = 'DG'  OR
            tit_acr.cod_espec_docto  = 'PD'  OR         
            tit_acr.cod_espec_docto  = 'AD'  OR         
            tit_acr.cod_espec_docto  = 'CD'  OR
            tit_acr.cod_espec_docto  = 'NP')  AND
            tit_acr.val_sdo_tit_acr <> tit_acr.val_origin_tit_acr THEN DO.
            ASSIGN d-tot-quitado = d-tot-quitado + (tit_acr.val_origin_tit_acr - tit_acr.val_sdo_tit_acr)
                   d-titulo-quitado = d-titulo-quitado + (tit_acr.val_origin_tit_acr - tit_acr.val_sdo_tit_acr).

        END.

       /* tit_acrs Liquidados c/ Atraso - 2 */
       IF (tit_acr.cod_espec_docto = 'DP'  OR 
           tit_acr.cod_espec_docto = 'DT'  OR
           tit_acr.cod_espec_docto = 'DG'  OR
           tit_acr.cod_espec_docto = 'CD'  OR
           tit_acr.cod_espec_docto = 'NP') AND 
           tit_acr.val_sdo_tit_acr = 0    AND
           tit_acr.dat_ult_liquidac_tit_acr > tit_acr.dat_vencto_tit_acr  THEN DO:
           IF NOT CAN-FIND(FIRST movto_tit_acr OF tit_acr WHERE
                                 movto_tit_acr.ind_trans_acr = "Liquida‡Æo Subst" AND
                                 movto_tit_acr.ind_trans_acr = "Baixa" ) THEN
               d-titulo-quitado-atraso = d-titulo-quitado-atraso + tit_acr.val_origin_tit_acr.
       END.

       /* TON
       /* tit_acrs Liquidados Cartorio */
       IF (tit_acr.cod_espec_docto = 'DP'  OR 
           tit_acr.cod_espec_docto = 'DT'  OR
           tit_acr.cod_espec_docto = 'DG'  OR
           tit_acr.cod_espec_docto = 'CD'  OR
           tit_acr.cod_espec_docto = 'NP') AND 
           tit_acr.val_sdo_tit_acr = 0    THEN DO:
           IF CAN-FIND(FIRST his-tit OF tit_acr WHERE
                             his-tit.historico BEGINS 'Liquidacao em cartorio') THEN
              d-tit_acr-quitado-cartorio = d-tit_acr-quitado-cartorio + tit_acr.val_origin_tit_acr.
       END.
       */
   END. /* each tit_acr */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-consim  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-consim, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-consim 
PROCEDURE state-changed :
/* -----------------------------------------------------------
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

