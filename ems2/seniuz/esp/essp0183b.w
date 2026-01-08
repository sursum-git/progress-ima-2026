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
{include/i-prgvrs.i ESSP0183B 2.04.00.000}
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
DEF BUFFER empresa FOR mgcad.empresa. 
DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD visualiza         AS LOG
    FIELD tipo-pedido       LIKE ped-venda-ext.tp-pedido
    FIELD qt-pedida         LIKE ped-item.qt-pedida
    FIELD qt-reservada      LIKE ped-item.qt-pedida
    FIELD qt-aberto         AS   DEC FORMAT "->,>>>,>>9.99"
    FIELD vl-reservado      LIKE ped-venda.vl-tot-ped
    FIELD vl-aberto         LIKE ped-venda.vl-tot-ped
    FIELD vl-solic          LIKE ped-venda.vl-tot-ped
    FIELD dt-solic          AS DATE
    FIELD qt-solic          LIKE ped-venda.vl-tot-ped
    FIELD usuario           AS CHAR
    FIELD marca             AS CHAR
    FIELD imprime           AS CHAR
    FIELD dt-atraso         AS DATE
    FIELD nr-embarque       LIKE pre-fatur.nr-embarque
    FIELD verifica-envio    AS LOG
    INDEX indice1 IS PRIMARY dt-entrega.


DEF TEMP-TABLE tt-ped-parcela LIKE ped-parcela
    FIELD faturado            AS LOG
    FIELD nova-sequencia      AS LOG
    FIELD sequencias          AS CHAR
    FIELD dias-atraso         AS INT
    FIELD nome-abrev          LIKE ped-venda.nome-abrev.

DEF TEMP-TABLE tt-digita
    FIELD opcao  AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEF TEMP-TABLE tt-work LIKE ped-parcela
    FIELD faturado            AS LOG
    FIELD nova-sequencia      AS LOG
    FIELD verifica-envio      AS LOG
    FIELD nome-abrev          LIKE ped-venda.nome-abrev.


    
/* Parametros                                                           */
DEFINE INPUT PARAMETER TABLE FOR tt-ped-venda.
    /*
DEFINE INPUT PARAMETER TABLE FOR tt-digita.
DEFINE INPUT PARAMETER TABLE FOR tt-work.
DEFINE INPUT PARAMETER c-it-codigo-ini     LIKE ped-item-ext.it-codigo.
DEFINE INPUT PARAMETER c-it-codigo-fin     LIKE ped-item-ext.it-codigo.
DEFINE INPUT PARAMETER c-cod-refer-ini     LIKE ped-item-ext.cod-refer.                              
DEFINE INPUT PARAMETER c-cod-refer-fin     LIKE ped-item-ext.cod-refer.
DEFINE INPUT PARAMETER c-situacao          AS CHAR.
DEFINE INPUT PARAMETER c-lotes             AS CHAR.
DEFINE INPUT PARAMETER c-corte-comerc-ini  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT PARAMETER c-corte-comerc-fin  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT PARAMETER c-opc-artigo        AS CHAR.
DEFINE INPUT PARAMETER c-cod-obsoleto-ini  AS CHAR.
DEFINE INPUT PARAMETER c-cod-obsoleto-fin  AS CHAR.
      */
      
DEF BUFFER b-tt-ped-venda FOR tt-ped-venda.
DEF BUFFER b2-tt-ped-venda FOR tt-ped-venda.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR i-lin          AS INT.
DEF VAR i-pag          AS INT.
DEF VAR i-ct           AS INT.
DEF VAR c-mensagem     AS CHAR.
DEF VAR c-sequencias   AS CHAR.
DEF VAR c-destinatario AS CHAR.
DEF VAR c-empresa      AS CHAR.
DEF VAR c-arq-email    AS CHAR FORMAT "x(45)".
DEF VAR de-tot-qtd     AS DEC.
DEF VAR de-tot-vlr     AS DEC.
DEF VAR de-vlr-ipi     AS DEC.
DEF VAR de-tot-ipi     AS DEC.
DEF VAR c-artigo-ipi   AS CHAR.

DEF VAR i-hr-solic     AS INT.
DEF VAR l-opc          AS LOG INITIAL NO.
DEF VAR de-vl-aberto   AS DEC FORMAT ">,>>>,>>>,>>9.99".  /*   daf    */
DEF VAR de-vl-desconto AS DEC FORMAT ">,>>>,>>>,>>9.99".  /*   daf    */
DEF VAR d-date-email   AS DATE.


/* Variaveis da Rotina de Impress∆o */
DEFINE VAR l-ok                AS LOG.
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-pedidos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ped-venda ped-venda-ext

/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.nr-pedcli tt-ped-venda.nome-abrev tt-ped-venda.dt-entrega tt-ped-venda.dt-implant tt-ped-venda.qt-pedida tt-ped-venda.qt-reservada tt-ped-venda.vl-aberto tt-ped-venda.vl-desconto tt-ped-venda.vl-tot-ped /* tt-ped-venda.imprime */   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define QUERY-STRING-br-pedidos FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.marca = "*"  NO-LOCK, ~
                                   EACH ped-venda-ext WHERE                                  ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND                                  ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK                               BY tt-ped-venda.no-ab-reppri                               BY tt-ped-venda.nr-pedcli
&Scoped-define OPEN-QUERY-br-pedidos OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE                                  tt-ped-venda.marca = "*"  NO-LOCK, ~
                                   EACH ped-venda-ext WHERE                                  ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND                                  ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK                               BY tt-ped-venda.no-ab-reppri                               BY tt-ped-venda.nr-pedcli.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-venda ped-venda-ext
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-venda
&Scoped-define SECOND-TABLE-IN-QUERY-br-pedidos ped-venda-ext


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-pedidos RECT-10 bt-desmarca bt-marca ~
bt-todos bt-nenhum bt-email bt-ok-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-email 

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

DEFINE BUTTON bt-desmarca AUTO-GO 
     IMAGE-UP FILE "image/im-cllps.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-email 
     IMAGE-UP FILE "image/im-email.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Envia e-mail das Programaá‰es Selecionadas".

DEFINE BUTTON bt-marca AUTO-GO 
     IMAGE-UP FILE "image/im-expan.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-nenhum AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Desmarca TODOS os Pedidos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok-2 AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "&Cancelar" 
     SIZE 5 BY 1.67 TOOLTIP "Finalizar Programa"
     BGCOLOR 8 .

DEFINE BUTTON bt-todos AUTO-GO 
     IMAGE-UP FILE "image\im-ran_a.bmp":U
     LABEL "" 
     SIZE 5 BY 1.25 TOOLTIP "Marca TODOS os Pedidos"
     BGCOLOR 8 FONT 10.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 5.86 BY 15.5
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pedidos FOR 
      tt-ped-venda, 
      ped-venda-ext SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos w-digita _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.nr-pedcli       COLUMN-LABEL "Pedido"         FORMAT "x(6)"       WIDTH 5.5   
          tt-ped-venda.nome-abrev      COLUMN-LABEL "Cliente"        FORMAT "x(12)"      WIDTH 12
          tt-ped-venda.dt-entrega      COLUMN-LABEL "Dt.Entrega"     FORMAT "99/99/9999" WIDTH 9
          tt-ped-venda.dt-implant      COLUMN-LABEL "Dt.Implant"     FORMAT "99/99/9999" WIDTH 9
          tt-ped-venda.qt-pedida       COLUMN-LABEL "Qt.Pedida"      FORMAT ">>>,>>9.99" WIDTH 9
          tt-ped-venda.qt-reservada    COLUMN-LABEL "Qt.Reservada"   FORMAT ">>>,>>9.99" WIDTH 10      
          tt-ped-venda.vl-aberto       COLUMN-LABEL "Valor Faturar"  FORMAT ">>>,>>9.99" WIDTH 10
          tt-ped-venda.vl-desconto     COLUMN-LABEL "Valor Desconto" FORMAT ">>>,>>9.99" WIDTH 11
          tt-ped-venda.vl-tot-ped      COLUMN-LABEL "Valor Total"    FORMAT ">>>,>>9.99" WIDTH 10
/*        tt-ped-venda.imprime         COLUMN-LABEL "*"              FORMAT "x(2)"       WIDTH 2.5  */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 15.5
         FONT 1
         TITLE "Consulta/Impress∆o Gerenciamento Pedidos a Vista" ROW-HEIGHT-CHARS .6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-pedidos AT ROW 1 COL 1
     bt-desmarca AT ROW 1.42 COL 98.14
     bt-marca AT ROW 2.71 COL 98.29
     bt-todos AT ROW 3.96 COL 98.29
     bt-nenhum AT ROW 5.25 COL 98.29
     bt-email AT ROW 9.08 COL 98.29
     bt-ok-2 AT ROW 14.58 COL 98.29
     bt-ajuda AT ROW 16.75 COL 72.14
     bt-ok AT ROW 16.79 COL 4.14
     bt-cancelar AT ROW 16.79 COL 15.14
     RECT-1 AT ROW 16.58 COL 3
     RECT-10 AT ROW 1 COL 97.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103 BY 17.21
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
         TITLE              = "Consulta/Imprime Gerenciamento Pedido a Vista"
         HEIGHT             = 15.63
         WIDTH              = 103.86
         MAX-HEIGHT         = 34.5
         MAX-WIDTH          = 205.72
         VIRTUAL-HEIGHT     = 34.5
         VIRTUAL-WIDTH      = 205.72
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
/* BROWSE-TAB br-pedidos 1 F-Main */
/* SETTINGS FOR BUTTON bt-ajuda IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-ajuda:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON bt-cancelar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-cancelar:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON bt-email IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON bt-ok IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-ok:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RECT-1:HIDDEN IN FRAME F-Main           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-venda WHERE
                                 tt-ped-venda.marca = "*"  NO-LOCK,
                            EACH ped-venda-ext WHERE
                                 ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND
                                 ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK
                              BY tt-ped-venda.no-ab-reppri
                              BY tt-ped-venda.nr-pedcli.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Consulta/Imprime Gerenciamento Pedido a Vista */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Consulta/Imprime Gerenciamento Pedido a Vista */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos w-digita
ON ROW-DISPLAY OF br-pedidos IN FRAME F-Main /* Consulta/Impress∆o Gerenciamento Pedidos a Vista */
DO:
  
    IF tt-ped-venda.imprime = "*" THEN
       ASSIGN  tt-ped-venda.nr-pedcli:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.nome-abrev:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.dt-entrega:FONT IN BROWSE br-pedidos = 6 
               tt-ped-venda.dt-implant:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.qt-pedida:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.qt-reservada:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.vl-aberto:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.vl-desconto:FONT IN BROWSE br-pedidos = 6
               tt-ped-venda.vl-tot-ped:FONT IN BROWSE br-pedidos = 6.

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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca w-digita
ON CHOOSE OF bt-desmarca IN FRAME F-Main
DO:
  IF AVAIL tt-ped-venda AND tt-ped-venda.marca = '*' AND tt-ped-venda.imprime = '*' THEN DO:
     ASSIGN tt-ped-venda.imprime = ''.
     br-pedidos:REFRESH().
     APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-email w-digita
ON CHOOSE OF bt-email IN FRAME F-Main
DO:
    
   FIND FIRST tt-ped-venda WHERE
              tt-ped-venda.imprime = "*"  NO-LOCK NO-ERROR.
       
   IF NOT AVAIL tt-ped-venda THEN DO:
      MESSAGE "Favor marcar os pedidos, que deseja enviar ! ! !"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   SESSION:SET-WAIT-STATE("general":U).     /*    cursor vira ampulheta   */   

   FOR EACH tt-ped-venda WHERE
            tt-ped-venda.imprime = "*"  NO-LOCK.    
   
       FIND emitente WHERE
            emitente.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK.

       ASSIGN c-destinatario = "".
       FIND cont-emit OF emitente WHERE
            cont-emit.area = 'FINANCEIRO' NO-LOCK NO-ERROR.
       IF AVAIL cont-emit THEN
          ASSIGN c-destinatario = TRIM(cont-emit.e-mail).

       IF c-destinatario = "" THEN DO:
          MESSAGE "E-mail do Cliente n∆o Cadastrado ! ! !"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.

       FIND repres WHERE
            repres.nome-abrev = tt-ped-venda.no-ab-reppri NO-LOCK.
       IF AVAIL repres AND repres.e-mail <> "" THEN
          ASSIGN c-destinatario = c-destinatario + "," + TRIM(repres.e-mail).
       ELSE DO:
          MESSAGE "E-mail do Representante n∆o Cadastrado ! ! !"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.  

       ASSIGN c-destinatario = c-destinatario + "," + "cobranca@imatextil.com.br" + "," + "admvendas@imatextil.com.br".

       FIND FIRST param-dis NO-LOCK NO-ERROR.                                                          

       RUN esapi/soma-dias-uteis.p (INPUT 3,                /* qtd dias a somar */
                                    INPUT TODAY,            /* data de inicio   */
                                    OUTPUT d-date-email).   /* data calculada   */ 

       IF d-date-email = ? THEN
          ASSIGN d-date-email = TODAY.

       ASSIGN de-vl-aberto = tt-ped-venda.vl-aberto
              de-vl-desconto = tt-ped-venda.vl-desconto.   

       // Monta arquivo com os dados do Pedido
       RUN pi-monta-arquivo.

       // Busca dados das Contas Banc†rias e Monta o Corpo do e-mail para o Cliente
       RUN pi-email-cli.

       RUN esapi/esapi002.p (INPUT "imatextil@imatextil.com.br", /* e-mail remetente */
                             INPUT c-destinatario,  /* e-mail destinat†rio */ 
                             INPUT "Solicitaá∆o Pagamento Pedido A VISTA" , /* Assunto */
                             INPUT c-mensagem, /* Mensagem */
                             INPUT "", /*arquivo anexo*/           
                             INPUT YES). /* Mostra Erros */

       // Busca dados das Contas Banc†rias e Monta o Corpo do e-mail para o Faturamento
       IF tt-ped-venda.vl-desconto > 0  THEN DO.
          ASSIGN c-destinatario = "cobranca@imatextil.com.br".

          RUN pi-email-cob.

          RUN esapi/esapi002.p (INPUT "imatextil@imatextil.com.br", /* e-mail remetente */
                                INPUT c-destinatario,  /* e-mail destinat†rio */ 
                                INPUT "Solicitaá∆o Pagamento Pedido A VISTA" , /* Assunto */
                                INPUT c-mensagem, /* Mensagem */
                                INPUT "", /*arquivo anexo*/           
                                INPUT YES). /* Mostra Erros */
       END.
   END.   
   

   FOR EACH b-tt-ped-venda WHERE 
            b-tt-ped-venda.imprime = "*"
            NO-LOCK
       BREAK BY b-tt-ped-venda.no-ab-reppri                                     
             BY b-tt-ped-venda.nr-pedcli.                                       


       /* Grava Novas Sequencias 'Ped-Parcela' */
       RUN esapi/cv-hora.p (INPUT STRING(TIME,"hh:mm:ss"), OUTPUT i-hr-solic).    

       FIND ped-parcela WHERE
            ped-parcela.cod-estabel = b-tt-ped-venda.cod-estabel AND
            ped-parcela.nr-pedido = b-tt-ped-venda.nr-pedido NO-LOCK NO-ERROR.

       IF NOT AVAIL ped-parcela THEN DO.
          CREATE ped-parcela.
          ASSIGN ped-parcela.cod-estabel  = b-tt-ped-venda.cod-estabel  /*  daf  */
                 ped-parcela.nr-pedido    = b-tt-ped-venda.nr-pedido
                 ped-parcela.dt-solic     = TODAY
                 ped-parcela.dt-pagto     = d-date-email
                 ped-parcela.hr-solic     = i-hr-solic   
                 ped-parcela.usuario      = c-seg-usuario
                 ped-parcela.vlr-solic    = b-tt-ped-venda.vl-tot-ped
                 ped-parcela.vl-desconto  = b-tt-ped-venda.vl-desconto
                 ped-parcela.qtd-solic    = b-tt-ped-venda.qt-pedida.
                 ped-parcela.destinatario = c-destinatario.
                 
          RUN esapi/cria-log-pedvenda.p (INPUT b-tt-ped-venda.nr-pedcli,
                                         INPUT b-tt-ped-venda.nome-abrev,
                                         INPUT "O Valor R$ " + TRIM(STRING(b-tt-ped-venda.vl-tot-ped,">>>,>>9.99")) +
                                               " Foi  Solicitado em: " + TRIM(STRING(b-tt-ped-venda.dt-solic,"99/99/9999")),
                                         INPUT NO).
       END.
   END.

   FOR EACH b-tt-ped-venda WHERE       
            b-tt-ped-venda.marca = "*". 
       ASSIGN b-tt-ped-venda.marca = "".

       FIND FIRST ped-parcela WHERE
                  ped-parcela.cod-estabel = b-tt-ped-venda.cod-estabel AND   /*  daf  */
                  ped-parcela.nr-pedido = b-tt-ped-venda.nr-pedido NO-LOCK NO-ERROR.
       IF AVAIL ped-parcela THEN
          ASSIGN b-tt-ped-venda.verifica-envio = YES.
   END.  


   FOR EACH tt-ped-venda WHERE
            tt-ped-venda.imprime = "*" NO-LOCK.
       DELETE tt-ped-venda.
   END.

   SESSION:SET-WAIT-STATE("":U).    /*   cursor volta ao normal   */

   MESSAGE 'E-mail Enviado com Sucesso....'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

   br-pedidos:REFRESH().

  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-digita
ON CHOOSE OF bt-marca IN FRAME F-Main
DO:
  IF AVAIL tt-ped-venda AND tt-ped-venda.marca = '*' AND tt-ped-venda.imprime = '' THEN DO: 
     ASSIGN tt-ped-venda.imprime = '*'.
     br-pedidos:REFRESH().
     APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum w-digita
ON CHOOSE OF bt-nenhum IN FRAME F-Main
DO:
   FOR EACH tt-ped-venda WHERE                                            
            tt-ped-venda.marca = "*" NO-LOCK.
        ASSIGN  tt-ped-venda.imprime = "".
   END.
    br-pedidos:REFRESH().
    APPLY 'value-changed' TO br-pedidos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
 /*
  RUN salva-rel IN <handle-browse>.
  if return-value = "NOK":U then return no-apply.
  run pi-cria-registro in <handle-browse>.
  if return-value = "NOK":U then return no-apply.
  apply "close":U to this-procedure.
*/  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok-2 w-digita
ON CHOOSE OF bt-ok-2 IN FRAME F-Main /* Cancelar */
DO:
  FIND FIRST tt-ped-venda WHERE 
             tt-ped-venda.marca = "*" NO-LOCK NO-ERROR.
  IF AVAIL tt-ped-venda THEN DO:
      MESSAGE "                    ATENÄ«O                   " SKIP  
              "Existe(m) pedido(s), que n∆o  foram enviado(s)" SKIP  
              "e-mail(s). Abandonando  esta  rotina  este(s)"  SKIP   
              "email(s) n∆o mais poder∆o ser enviado(s)."      SKIP(1)
              "DESEJA MESMO ASSIM ABANDONAR ROTINA ?"
               VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE l-opc.
      IF l-opc = YES THEN 
         APPLY "CLOSE":U TO THIS-PROCEDURE.
      ELSE  DO:
         APPLY 'value-changed' TO br-pedidos.
         RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-digita
ON CHOOSE OF bt-todos IN FRAME F-Main
DO:
   FOR EACH tt-ped-venda WHERE                                            
            tt-ped-venda.marca = '*' NO-LOCK.
        ASSIGN tt-ped-venda.imprime = '*'.
   END.
   
   br-pedidos:REFRESH().
   APPLY 'value-changed' TO br-pedidos.
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
  ENABLE br-pedidos RECT-10 bt-desmarca bt-marca bt-todos bt-nenhum bt-email 
         bt-ok-2 
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

  {utp/ut9000.i "ESSP0183B" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").



  {include/i-inifld.i}
  STATUS INPUT OFF. /* Desliga Mensagem no RodapÇ da Tela */
  APPLY 'entry' TO br-pedidos IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cabec-item w-digita 
PROCEDURE pi-cabec-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 PUT "Item   Descriá∆o                           Und Referància Seq Metros a Faturar Preáo Unitario Valores a Faturar" AT 7.
 PUT "------ ----------------------------------- --- ---------- --- ---------------- -------------- -----------------" AT 7.
 ASSIGN i-lin = i-lin + 2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-email-cli w-digita 
PROCEDURE pi-email-cli :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-mensagem = "PEDIDO ∑ VISTA Nß " + tt-ped-venda.nr-pedcli + CHR(13) +
                        "MEDTEXTIL IMPORT. E EXPORT. LTDA." + CHR(13) +
                        "REPRESENTANTE:" + tt-ped-venda.no-ab-reppri + CHR(13) + CHR(13) +
                        "Prezado Cliente:" + tt-ped-venda.nome-abrev + CHR(13) + 
                        "Agradecemos pelo seu Pedido de Compra Ö vista e informamos que o mesmo j† se encontra DISPON÷VEL PARA EMBARQUE IMEDIATO." + CHR(13) + CHR(13).

    FOR EACH ped-item WHERE 
             ped-item.nome-abrev = tt-ped-venda.nome-abrev AND
             ped-item.nr-pedcli = tt-ped-venda.nr-pedcli AND 
             ped-item.cod-sit-item = 1 NO-LOCK
        BREAK BY ped-item.it-codigo.

        IF FIRST-OF (ped-item.it-codigo) THEN DO:
           FIND ITEM WHERE 
                ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
           ASSIGN c-mensagem = c-mensagem + "ARTIGO: " + ped-item.it-codigo + " - " + item.desc-item + CHR(13).
        END.
    END.

    IF de-tot-ipi > 0 THEN
       ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                           "Ser† acrescentado o Valor de: " + STRING(de-tot-ipi,">>>,>>9.99") + " referente Ö IPI nos Artigos:" + CHR(13) + 
                           c-artigo-ipi + CHR(13).

    ASSIGN c-mensagem = c-mensagem + CHR(13) +
                        "Solicitamos a gentileza de efetuar o DEP‡SITO de acordo com as condiá‰es negociadas nas seguintes contas-correntes relacionadas:" + CHR(13) + CHR(13) +
                        tt-ped-venda.nr-pedcli + " - VALOR: R$" + STRING(de-vl-aberto,">,>>>,>>9.99") + CHR(13) + CHR(13) +   
                        "     MEDTEXTIL IMPORT. E EXPORT. LTDA - CNPJ: 06.013.812/0001-58."  + CHR(13) +
                        "     BANCO SICOOB: AG.4027 C/C:90611883-2"  + CHR(13) +
                        "     BANCO DO BRASIL: AG.3394-4 C/C:5178-0"  + CHR(13) +
                        "     BANCO BRADESCO: AG.1430 C/C:44333-6."    + CHR(13) +
                        "     BANCO ITAÈ: AG.1403 C/C:26917-6."    + CHR(13) + CHR(13) +
                        "     CHAVE PIX: 06013812000158" + CHR(13).

    ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                        "Obs. DEP‡SITO EM CHEQUE SOMENTE SERµ LIBERADO AP‡S A COMPENSAÄ«O." + CHR(13) + CHR(13) +
                        "Para agilizarmos o envio do seu pedido, solicitamos nos enviar o(s) comprovantes(s) de dep¢sito(s) por:" + CHR(13).

    ASSIGN c-mensagem = c-mensagem + "Email: cobranca@imatextil.com.br" + CHR(13) +
                        "Telefone  (31) 98428-8685 (WhatsApp)" + CHR(13) + CHR(13) + 
                        "Informamos que em funá∆o da demanda pelo artigo de seu pedido O PRAZO MµXIMO PARA DEP‡SITO SERµ DE 03 DIAS ÈTEIS, vencendo no dia:" + STRING(d-date-email,"99/99/9999") + ". Ap¢s vencimento o pedido ser† cancelado sem aviso prÇvio."+ CHR(13) + CHR(13) +
                        "Contamos com sua compreens∆o e apoio." + CHR(13) + CHR(13) +
                        "Grupo IMA T“XTIL".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-email-cob w-digita 
PROCEDURE pi-email-cob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-mensagem = "ORÄAMENTO Nß " + tt-ped-venda.nr-pedcli + CHR(13) +
                        "MEDTEXTIL IMPORT. E EXPORT. LTDA." + CHR(13) +
                        "REPRESENTANTE:" + tt-ped-venda.no-ab-reppri + CHR(13) + CHR(13) +
                        "Prezado Cliente:" + tt-ped-venda.nome-abrev + CHR(13) + 
                        "Agradecemos pelo seu Pedido de Compra Ö vista e informamos que o mesmo j† se encontra DISPON÷VEL PARA EMBARQUE IMEDIATO." + CHR(13) + CHR(13).

    FOR EACH ped-item WHERE 
             ped-item.nome-abrev = tt-ped-venda.nome-abrev AND
             ped-item.nr-pedcli = tt-ped-venda.nr-pedcli AND 
             ped-item.cod-sit-item = 1 NO-LOCK
        BREAK BY ped-item.it-codigo.

        IF FIRST-OF (ped-item.it-codigo) THEN DO:
           FIND ITEM WHERE 
                ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
           ASSIGN c-mensagem = c-mensagem + "ARTIGO: " + ped-item.it-codigo + " - " + item.desc-item + CHR(13).
        END.
    END.

    IF de-tot-ipi > 0 THEN
       ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                           "Ser† acrescentado o Valor de: " + STRING(de-tot-ipi,">>>,>>9.99") + " referente Ö IPI nos Artigos:" + CHR(13) + 
                           c-artigo-ipi + CHR(13).
    
    ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                        tt-ped-venda.nr-pedcli + " - ORÄAMENTO  - VALOR: R$ " + STRING(de-vl-desconto,">,>>>,>>9.99") + CHR(13) + CHR(13) +   
                        "     MEDTEXTIL IMPORT. E EXPORT. LTDA "  + CHR(13) +
                        "     BANCO SICOOB: AG.4027 C/C:90612054-3"    + CHR(13) +
                        "     CHAVE PIX: medpix@imatextil.com.br" + CHR(13).

    ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                        "Obs. DEP‡SITO EM CHEQUE SOMENTE SERµ LIBERADO AP‡S A COMPENSAÄ«O." + CHR(13) + CHR(13) +
                        "Para agilizarmos o envio do seu pedido, solicitamos nos enviar o(s) comprovantes(s) de dep¢sito(s) por:" + CHR(13). 

    ASSIGN c-mensagem = c-mensagem + "Email: cobranca@imatextil.com.br" + CHR(13) +
                        "Telefone (31) 98428-8685 (WhatsApp)" + CHR(13) + CHR(13) + 
                        "Informamos que em funá∆o da demanda pelo artigo de seu pedido O PRAZO MµXIMO PARA DEP‡SITO SERµ DE 03 DIAS ÈTEIS, vencendo no dia:" + STRING(d-date-email,"99/99/9999") + ". Ap¢s vencimento o pedido ser† cancelado sem aviso prÇvio."+ CHR(13) + CHR(13) +
                        "Contamos com sua compreens∆o e apoio." + CHR(13) + CHR(13) +
                        "Grupo IMA T“XTIL".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-email-fat w-digita 
PROCEDURE pi-email-fat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-mensagem = "PEDIDO ∑ VISTA Nß " + tt-ped-venda.nr-pedcli + CHR(13) +
                        "MEDTEXTIL IMPORT. E EXPORT. LTDA." + CHR(13) +
                        "REPRESENTANTE:" + tt-ped-venda.no-ab-reppri + CHR(13) + CHR(13) +
                        "Prezado Cliente:" + tt-ped-venda.nome-abrev + CHR(13) + 
                        "Agradecemos pelo seu Pedido de Compra Ö vista e informamos que o mesmo j† se encontra DISPON÷VEL PARA EMBARQUE IMEDIATO." + CHR(13) + CHR(13).

    FOR EACH ped-item WHERE 
             ped-item.nome-abrev = tt-ped-venda.nome-abrev AND
             ped-item.nr-pedcli = tt-ped-venda.nr-pedcli AND 
             ped-item.cod-sit-item = 1 NO-LOCK
        BREAK BY ped-item.it-codigo.

        IF FIRST-OF (ped-item.it-codigo) THEN DO:
           FIND ITEM WHERE 
                ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
           ASSIGN c-mensagem = c-mensagem + "ARTIGO: " + ped-item.it-codigo + " - " + item.desc-item + CHR(13).
        END.
    END.

    IF de-tot-ipi > 0 THEN
       ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                           "Ser† acrescentado o Valor de: " + STRING(de-tot-ipi,">>>,>>9.99") + " referente Ö IPI nos Artigos:" + CHR(13) + 
                           c-artigo-ipi + CHR(13).

    ASSIGN c-mensagem = c-mensagem + CHR(13) +
                        "Solicitamos a gentileza de efetuar o DEP‡SITO de acordo com as condiá‰es negociadas nas seguintes contas-correntes relacionadas:" + CHR(13) + CHR(13) +
                        tt-ped-venda.nr-pedcli + " - VALOR: R$" + STRING(de-vl-aberto,">,>>>,>>9.99") + CHR(13) + CHR(13) +   
                        "     MEDTEXTIL IMPORT. E EXPORT. LTDA - CNPJ: 06.013.812/0001-58."  + CHR(13) +
                        "     BANCO SICOOB: AG.4027 C/C:90611883-2"  + CHR(13) +
                        "     BANCO DO BRASIL: AG.3394-4 C/C:5178-0"  + CHR(13) +
                        "     BANCO BRADESCO: AG.1430 C/C:44333-6."    + CHR(13) +
                        "     BANCO ITAÈ: AG.1403 C/C:26917-6."    + CHR(13) + CHR(13) +
                        "     CHAVE PIX: 06013812000158" + CHR(13).

    IF tt-ped-venda.vl-desconto > 0  THEN
       ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                           tt-ped-venda.nr-pedcli + " - ORÄAMENTO  - VALOR: R$ " + STRING(de-vl-desconto,">,>>>,>>9.99") + CHR(13) + CHR(13) +   
                           "     MEDTEXTIL IMPORT. E EXPORT. LTDA - CNPJ: 06.013.812/0001-58."  + CHR(13) +
                           "     BANCO SICOOB: AG.4027 C/C:90612054-3"  + CHR(13) +
                           "     BANCO DO BRASIL: AG.3394-4 C/C:7301-6"  + CHR(13) +
                           "     BANCO BRADESCO: AG.1430 C/C:13926-2."    + CHR(13) +
                           "     BANCO ITAÈ: AG.3054 C/C:09274-1."    + CHR(13) + CHR(13) +
                           "     CHAVE PIX: medpix@imatextil.com.br" + CHR(13).

    ASSIGN c-mensagem = c-mensagem + CHR(13) + 
                        "Grupo IMA T“XTIL". 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-email-ima w-digita 
PROCEDURE pi-email-ima :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN c-mensagem = "PEDIDO ∑ VISTA Nß " + tt-ped-venda.nr-pedcli + CHR(13) +
                        "IMA TECIDOS DA MODA LTDA." + CHR(13) +
                        "REPRESENTANTE:" + tt-ped-venda.no-ab-reppri + CHR(13) + CHR(13) +
                        "Prezado Cliente:" + tt-ped-venda.nome-abrev + CHR(13) + 
                        "Agradecemos pelo seu Pedido de Compra Ö vista e informamos que o mesmo j† se encontra DISPON÷VEL PARA EMBARQUE IMEDIATO." + CHR(13) + CHR(13).
    
       FOR EACH ped-item WHERE 
                ped-item.nome-abrev = tt-ped-venda.nome-abrev AND
                ped-item.nr-pedcli = tt-ped-venda.nr-pedcli  AND 
                ped-item.cod-sit-item = 1 NO-LOCK
           BREAK BY ped-item.it-codigo.
    
           IF FIRST-OF (ped-item.it-codigo) THEN DO:
              FIND ITEM WHERE 
                   ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
              ASSIGN c-mensagem = c-mensagem + "ARTIGO: " + ped-item.it-codigo + " - " + item.desc-item + CHR(13).
           END.
       END.
    
       IF de-tot-ipi > 0 THEN
          ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                              "Ser† acrescentado o Valor de: " + STRING(de-tot-ipi,">>>,>>9.99") + " referente Ö IPI nos Artigos:" + CHR(13) + 
                              c-artigo-ipi + CHR(13).

       ASSIGN c-mensagem = c-mensagem + CHR(13) +
                           "Solicitamos a gentileza de efetuar o DEP‡SITO de acordo com as condiá‰es negociadas nas seguintes contas-correntes relacionadas:" + CHR(13) + CHR(13) +
                           tt-ped-venda.nr-pedcli + " - VALOR: R$" + STRING(de-vl-aberto,">,>>>,>>9.99") + CHR(13) + CHR(13) +   
                           "     IMA TECIDOS DA MODA - CNPJ: 21.126.271/0001-68."  + CHR(13) +
                           "     BANCO DO BRASIL: AG.3394-4 C/C:602170-0"  + CHR(13) +
                           "     BANCO BRADESCO: AG.1430 C/C:44333-6."    + CHR(13) +
                           "     BANCO ITAÈ: AG.1403 C/C:42342-7."    + CHR(13).
   
       IF tt-ped-venda.vl-desconto > 0  THEN
          ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                              tt-ped-venda.nr-pedcli + " - COMPLEMENTO - VALOR: R$ " + STRING(de-vl-desconto,">,>>>,>>9.99") + CHR(13) + CHR(13) +   
                              "     IMA TECIDOS DA MODA - CNPJ: 21.126.271/0001-68."  + CHR(13) +
                              "     BANCO DO BRASIL: AG.3394-4 C/C:6001-1"  + CHR(13) +
                              "     BANCO BRADESCO: AG.1430 C/C:13926-2."    + CHR(13) +
                              "     BANCO ITAÈ: AG.3054 C/C:24860-8."    + CHR(13).

       ASSIGN c-mensagem = c-mensagem + CHR(13) + CHR(13) +
                           "Obs. DEP‡SITO EM CHEQUE SOMENTE SERµ LIBERADO AP‡S A COMPENSAÄ«O." + CHR(13) + CHR(13) +
                           "Para agilizarmos o envio do seu pedido, solicitamos nos enviar o(s) comprovantes(s) de dep¢sito(s) por:" + CHR(13) +
                           "Fax: (31) 3238-3162" + CHR(13).
    
       ASSIGN c-mensagem = c-mensagem + // "Email: " + usuar_mestre.cod_e_mail_local + 
                           " ou faturamento.med@imatextil.com.br" + CHR(13) +
                           "ou pelos telefones (31) 3238-3262" + CHR(13) +
                           "                           (31) 98428-1139 (Claro)" + CHR(13) +
                           "                           (27) 3339-0023" + CHR(13) + CHR(13) +
                           "Informamos que em funá∆o da demanda pelo artigo de seu pedido O PRAZO MµXIMO PARA DEP‡SITO SERµ DE 03 DIAS ÈTEIS, vencendo no dia:" + STRING (d-date-email) + ". Ap¢s vencimento o pedido ser† cancelado sem aviso prÇvio."+ CHR(13) + CHR(13) +
                           "Contamos com sua compreens∆o e apoio." + CHR(13) + CHR(13) +
                           "Grupo IMA T“XTIL".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec w-digita 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  47
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  53
        "HORA: "                                  AT  79
        STRING(TIME,"hh:mm:ss")                   AT  85
        "PAG:"                                    AT 110
        i-pag FORMAT ">>>"                        AT 115
        SKIP(1).

    PUT "RELATORIO DE SOLICITAÄ«O DE VALORES A VISTA " AT 38 SKIP(1). 
    FIND repres WHERE
         repres.nome-abrev = tt-ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
    IF AVAIL repres THEN DO:
       PUT "Representante: "    AT 1.
       PUT  repres.nome FORMAT "X(40)" AT 16 SKIP(1).
    END.


    PUT "Pedido Cliente      Ped.Repres.  Dt.Implant Data Solic Solicitante " AT 1.   
    PUT "------ ------------ ------------ ---------- ---------- ------------" AT 1.
    ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime w-digita 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.

  FIND FIRST tt-ped-venda WHERE
             tt-ped-venda.imprime = "*" NO-LOCK NO-ERROR.
  IF NOT AVAIL tt-ped-venda THEN DO:
     MESSAGE "Favor marcar os pedidos que deseja imprimir ! ! !"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN NO-APPLY.
  END.

  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 61.
          PUT CONTROL "~033E~033(s18H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida).
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0183.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-lin = 99
            i-pag =  1.

     FOR EACH tt-ped-venda WHERE                                            
              tt-ped-venda.imprime = "*" NO-LOCK,                             
         EACH ped-venda-ext WHERE                                           
              ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND      
              ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK      
         BREAK  BY tt-ped-venda.no-ab-reppri                                     
                BY tt-ped-venda.nr-pedcli.                                       

         IF i-lin > 61 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.

         PUT tt-ped-venda.nr-pedcli    FORMAT "x(6)"           AT  1
             tt-ped-venda.nome-abrev   FORMAT "x(12)"          AT  8
             tt-ped-venda.nr-pedrep    FORMAT "x(12)"          AT 21
             tt-ped-venda.dt-implant   FORMAT "99/99/9999"     AT 34
             ped-venda-ext.dt-solic    FORMAT "99/99/9999"     AT 45
             ped-venda-ext.usuario     FORMAT "x(12)"          AT 56.
         ASSIGN i-lin = i-lin + 1.


         PUT SKIP.
         ASSIGN i-lin = i-lin + 1.
         PUT SKIP.                                /*  daf   */
         ASSIGN i-lin = i-lin + 1.                /*  daf   */


         ASSIGN de-tot-qtd = 0
                de-tot-vlr = 0.
         FOR EACH ped-item WHERE
                  ped-item.nr-pedcli  = tt-ped-venda.nr-pedcli AND
                  ped-item.nome-abrev = tt-ped-venda.nome-abrev AND 
                  ped-item.cod-sit-item = 1 NO-LOCK
               BY ped-item.it-codigo
               BY ped-item.nr-sequencia
               BY ped-item.cod-refer.

             RUN pi-ver-digita (INPUT "Item",
                                INPUT ped-item.it-codigo).
             IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

             RUN pi-ver-digita (INPUT "Referància",
                                INPUT ped-item.cod-refer).
             IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.


    /*       IF ped-item-ext.corte-comerc < c-corte-comerc-ini OR  
                ped-item-ext.corte-comerc > c-corte-comerc-fin THEN NEXT.       

             RUN pi-ver-digita (INPUT "Corte_Comercial",
                                INPUT ped-item-ext.corte-comerc).
             IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.                              */
   
             FIND item WHERE
                  item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

             FIND item-ext WHERE
                  item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.


 /*            FIND ped-item-res OF ped-item WHERE
                  ped-item-res.faturado = NO NO-LOCK NO-ERROR.
             IF NOT AVAIL ped-item-res THEN  NEXT.                     */

             /* Despreza Sequencias (nr-sequencia) que ja foram solicitadas Anteriormente 
                e que est∆o em Aberto ou Atendido Parcial */
/*              FIND tt-work WHERE                                       */
/*                   tt-work.nr-pedido      = tt-ped-venda.nr-pedido AND */
/*                   tt-work.nr-sequencia   = ped-item.nr-sequencia  AND */
/*                   tt-work.nova-sequencia = NO NO-LOCK NO-ERROR.       */
/*              IF AVAIL tt-work THEN NEXT.                              */

             IF de-tot-qtd + de-tot-vlr = 0 THEN
                RUN pi-cabec-item.


             PUT ped-item.it-codigo     FORMAT "x(6)"                              AT  07
                 item.desc-item         FORMAT "x(34)"                             AT  14
                 ITEM.un                FORMAT "x(3)"                              AT  50
                 ped-item.cod-refer                                                AT  54
                 ped-item.nr-sequencia  FORMAT ">>9"                               AT  65
                 ped-item.qt-pedida     FORMAT ">>>,>>>,>>9.99"                    AT  71
                 ped-item.vl-preuni     FORMAT ">>>,>>>,>>9.99"                    AT  86
                 ped-item.qt-pedida * ped-item.vl-preuni FORMAT ">,>>>,>>>,>>9.99" AT 102.
          
             ASSIGN i-lin = i-lin + 1.

             ASSIGN de-tot-qtd = de-tot-qtd + ped-item.qt-pedida
                    de-tot-vlr = de-tot-vlr + (ped-item.qt-pedida * ped-item.vl-preuni).
         END.
         PUT SKIP(2).
         ASSIGN i-lin = i-lin + 2.
           

         PUT "Total do Pedido......................: " AT  30.
         PUT de-tot-qtd  FORMAT ">>>,>>>,>>9.99"       AT  71
             de-tot-vlr  FORMAT ">,>>>,>>>,>>9.99"     AT 102.
         PUT SKIP(3).
         ASSIGN i-lin = i-lin + 3.


         /*
         ACCUMULATE tt-ped-venda.qt-reservada (TOTAL BY tt-ped-venda.no-ab-reppri).
         ACCUMULATE tt-ped-venda.vl-reservado (TOTAL BY tt-ped-venda.no-ab-reppri).     */

         IF LAST-OF(tt-ped-venda.no-ab-reppri) THEN DO:
          /*  PUT SKIP.
            PUT "TOTAL REPRESENTANTE .................: " AT 30.
            PUT ACCUM TOTAL BY tt-ped-venda.no-ab-reppri tt-ped-venda.qt-reservada FORMAT ">>>,>>>,>>9.99"   AT  71
                ACCUM TOTAL BY tt-ped-venda.no-ab-reppri tt-ped-venda.vl-reservado FORMAT ">,>>>,>>>,>>9.99" AT 102.
                */
            PAGE.
            ASSIGN i-lin = 99.
         END.
     END.
     IF i-saida = 1 THEN DO:
        PAGE.
        PUT "" AT 1.
     END.
  END.
  OUTPUT CLOSE.
  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                           INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-arquivo w-digita 
PROCEDURE pi-monta-arquivo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    // Monta o Arquivo, caso deseje enviar anexo...
    ASSIGN i-lin = 99
           i-pag =  1.
 
    FIND FIRST ped-venda-ext WHERE                                           
               ped-venda-ext.cod-estabel = tt-ped-venda.cod-estabel AND       
               ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido NO-LOCK NO-ERROR.       
 
    ASSIGN c-arq-email = SESSION:TEMP-DIRECTORY + tt-ped-venda.no-ab-reppri + ".txt".
    OUTPUT TO VALUE(c-arq-email) CONVERT SOURCE "ibm850".    
 
       IF i-lin > 61 THEN DO:
          RUN pi-imp-cabec.
          ASSIGN i-lin = 7.
       END.
 
       PUT tt-ped-venda.nr-pedcli    FORMAT "x(6)"           AT  1
           tt-ped-venda.nome-abrev   FORMAT "x(12)"          AT  8
           tt-ped-venda.nr-pedrep    FORMAT "x(12)"          AT 21
           tt-ped-venda.dt-implant   FORMAT "99/99/9999"     AT 34
           ped-venda-ext.dt-solic    FORMAT "99/99/9999"     AT 45
           ped-venda-ext.usuario     FORMAT "x(12)"          AT 56.

       ASSIGN i-lin = i-lin + 1.

       PUT SKIP.
       ASSIGN i-lin = i-lin + 1.
 
       ASSIGN de-tot-qtd = 0
              de-tot-vlr = 0
              de-tot-ipi = 0
              c-artigo-ipi = ''.
       FOR EACH ped-item WHERE
                ped-item.nr-pedcli  = tt-ped-venda.nr-pedcli AND
                ped-item.nome-abrev = tt-ped-venda.nome-abrev AND
                ped-item.cod-sit-item = 1 NO-LOCK
                BY ped-item.it-codigo
                BY ped-item.nr-sequencia
                BY ped-item.cod-refer.
 
           FIND item WHERE
                item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
 
           FIND item-ext WHERE
                item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
 
           IF de-tot-qtd + de-tot-vlr = 0 THEN
              RUN pi-cabec-item.                                                

           ASSIGN de-tot-vlr = de-tot-vlr + (ped-item.qt-pedida * ped-item.vl-preuni)
                  de-tot-qtd = de-tot-qtd + ped-item.qt-pedida.  

           FIND ITEM WHERE
                ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

           ASSIGN de-vlr-ipi = 0.
           IF ITEM.aliquota-ipi <> 0 THEN DO.
              ASSIGN de-vlr-ipi = ((ped-item.qt-pedida * ped-item.vl-preuni) * ITEM.aliquota-ipi) / 100.
              ASSIGN de-tot-ipi = de-tot-ipi + de-vlr-ipi.

              ASSIGN c-artigo-ipi = IF c-artigo-ipi = '' 
                                    THEN ped-item.it-codigo
                                    ELSE c-artigo-ipi + ";" + ped-item.it-codigo.
           END.

           PUT ped-item.it-codigo     FORMAT "x(6)"                              AT  07
               item.desc-item         FORMAT "x(34)"                             AT  14
               ITEM.un                FORMAT "x(3)"                              AT  50
               ped-item.cod-refer     FORMAT "99.9999-9"                         AT  54
               ped-item.nr-sequencia  FORMAT ">>9"                               AT  65
               ped-item.qt-pedida     FORMAT ">>>,>>>,>>9.99"                    AT  71
               ped-item.vl-preuni     FORMAT ">>>,>>>,>>9.99"                    AT  86
               ped-item.qt-pedida * ped-item.vl-preuni FORMAT ">,>>>,>>>,>>9.99" AT 102
               de-vlr-ipi             FORMAT ">>>,>>9.99"                        AT 120.

           ASSIGN i-lin = i-lin + 1.
       END.
       PUT SKIP(2).
       ASSIGN i-lin = i-lin + 2.
 
 
       PUT "Total do Pedido......................: " AT 30.
       PUT de-tot-qtd  FORMAT ">>>,>>>,>>9.99"   AT  71
           de-tot-vlr  FORMAT ">,>>>,>>>,>>9.99" AT 102
           de-tot-ipi  FORMAT ">,>>>,>>>,>>9.99" AT 120.

       PUT SKIP(3).
       ASSIGN i-lin = i-lin + 3.
 
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita w-digita 
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
  {src/adm/template/snd-list.i "tt-ped-venda"}
  {src/adm/template/snd-list.i "ped-venda-ext"}

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

