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
{include/i-prgvrs.i ESSP0180F 2.04.00.000}
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
DEF BUFFER empresa FOR mgadm.empresa.

DEF TEMP-TABLE tt-clientes LIKE emitente
    FIELD repres           LIKE repres.nome-abrev
    FIELD tot-pend         AS DEC
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
    FIELD motivo           AS CHAR FORMAT "x(200)"
    FIELD ordem            AS INT
    FIELD visualiza        AS LOG.

DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD visualiza         AS LOG
    FIELD tipo-pedido       LIKE ped-venda-ext.tp-pedido
    FIELD vl-aberto         LIKE ped-venda.vl-tot-ped
    FIELD vl-reservado      LIKE ped-venda.vl-tot-ped
    FIELD usuario           AS CHAR
    FIELD marca             AS CHAR
    INDEX indice1 IS PRIMARY dt-entrega.


/* Parametros                                                           */
DEFINE INPUT PARAMETER TABLE FOR tt-clientes.
DEFINE INPUT PARAMETER TABLE FOR tt-ped-venda.
DEFINE INPUT PARAMETER i-sit-ped-ini LIKE ped-venda.cod-sit-ped.
DEFINE INPUT PARAMETER i-sit-ped-fin LIKE ped-venda.cod-sit-ped.
DEFINE INPUT PARAMETER c-sit-aval    AS CHAR. 
DEFINE INPUT PARAMETER i-crivo       AS INT.

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR i-lin       AS INT.
DEF VAR i-pag       AS INT.
DEF VAR c-mensagem  AS CHAR.
DEF VAR c-arq-email AS CHAR FORMAT "x(45)".
DEF VAR c-empresa   AS CHAR.
DEF VAR de-tot-qtd  AS DEC.
DEF VAR de-tot-vlr  AS DEC.

/* Variaveis da Rotina de ImpressÆo */
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
&Scoped-define INTERNAL-TABLES tt-clientes tt-ped-venda

/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-ped-venda.marca tt-clientes.nome-abrev tt-clientes.repres tt-ped-venda.nr-pedcli tt-ped-venda.dt-entrega tt-ped-venda.dt-implant fn-sit-ped() tt-ped-venda.tp-pedido fn-sit-cred() fn-cond-pagto() tt-ped-venda.vl-aberto tt-ped-venda.vl-reservado tt-clientes.motivo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define QUERY-STRING-br-pedidos FOR EACH tt-clientes WHERE                                  tt-clientes.visualiza = YES NO-LOCK, ~
                                   EACH tt-ped-venda WHERE                                  tt-ped-venda.nome-abrev   = tt-clientes.nome-abrev AND                                  tt-ped-venda.cod-sit-ped >= i-sit-ped-ini          AND                                  tt-ped-venda.cod-sit-ped <= i-sit-ped-fin          AND                                  LOOKUP(STRING(tt-ped-venda.cod-sit-aval), ~
      c-sit-aval) > 0                                  NO-LOCK                               BY tt-clientes.ordem                               BY tt-clientes.repres                               BY tt-clientes.nome-abrev
&Scoped-define OPEN-QUERY-br-pedidos OPEN QUERY {&SELF-NAME} FOR EACH tt-clientes WHERE                                  tt-clientes.visualiza = YES NO-LOCK, ~
                                   EACH tt-ped-venda WHERE                                  tt-ped-venda.nome-abrev   = tt-clientes.nome-abrev AND                                  tt-ped-venda.cod-sit-ped >= i-sit-ped-ini          AND                                  tt-ped-venda.cod-sit-ped <= i-sit-ped-fin          AND                                  LOOKUP(STRING(tt-ped-venda.cod-sit-aval), ~
      c-sit-aval) > 0                                  NO-LOCK                               BY tt-clientes.ordem                               BY tt-clientes.repres                               BY tt-clientes.nome-abrev.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-clientes tt-ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-clientes
&Scoped-define SECOND-TABLE-IN-QUERY-br-pedidos tt-ped-venda


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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-cond-pagto w-digita 
FUNCTION fn-cond-pagto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-cred w-digita 
FUNCTION fn-sit-cred RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-ped w-digita 
FUNCTION fn-sit-ped RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
     SIZE 5 BY 1.25 TOOLTIP "Envia e-mail das Programa‡äes Selecionadas".

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
     SIZE 5 BY 1.25 TOOLTIP "Finalizar Programa"
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
     SIZE 5.86 BY 14.92
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pedidos FOR 
      tt-clientes, 
      tt-ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos w-digita _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-ped-venda.marca           COLUMN-LABEL "M"              FORMAT "x(1)"       WIDTH 1.5 COLUMN-FONT 6
      tt-clientes.nome-abrev       COLUMN-LABEL "Cliente"                            WIDTH 15 COLUMN-FONT 6
      tt-clientes.repres           COLUMN-LABEL "Rep."                               WIDTH 15
      tt-ped-venda.nr-pedcli       COLUMN-LABEL "Pedido"         FORMAT "x(6)"       WIDTH 06   
      tt-ped-venda.dt-entrega      COLUMN-LABEL "Dt.Entrega"     FORMAT "99/99/9999" WIDTH 09
      tt-ped-venda.dt-implant      COLUMN-LABEL "Dt.Implant"     FORMAT "99/99/9999" WIDTH 10
      fn-sit-ped()                 COLUMN-LABEL "Sit"                                WIDTH 03
      tt-ped-venda.tp-pedido       COLUMN-LABEL "Tipo Pedido"    FORMAT "x(15)"      WIDTH 12
      fn-sit-cred()                COLUMN-LABEL "Sit Cred"       FORMAT "x(15)"      WIDTH 10
      fn-cond-pagto()              COLUMN-LABEL "Cond Pagto"     FORMAT "x(20)"      WIDTH 15
      tt-ped-venda.vl-aberto       COLUMN-LABEL "Vlr Aberto"     FORMAT ">>>,>>9.99" WIDTH 09
      tt-ped-venda.vl-reservado    COLUMN-LABEL "Vlr Reservado"  FORMAT ">>>,>>9.99" WIDTH 10
      tt-clientes.motivo           COLUMN-LABEL "Motivo"         FORMAT "x(70)"      WIDTH 60
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93 BY 15.5
         FONT 1
         TITLE "Gerenciamento de E-mails" ROW-HEIGHT-CHARS .6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-pedidos AT ROW 1 COL 1
     bt-desmarca AT ROW 1.38 COL 94.86
     bt-marca AT ROW 2.67 COL 95
     bt-todos AT ROW 3.92 COL 95
     bt-nenhum AT ROW 5.21 COL 95
     bt-email AT ROW 9.04 COL 95
     bt-ok-2 AT ROW 14.54 COL 95
     bt-ajuda AT ROW 16.75 COL 72.14
     bt-ok AT ROW 16.79 COL 4.14
     bt-cancelar AT ROW 16.79 COL 15.14
     RECT-1 AT ROW 16.58 COL 3
     RECT-10 AT ROW 1.08 COL 94.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101 BY 17.21
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
         TITLE              = "Gerenciamento de E-mails do Crivo Financeiro"
         HEIGHT             = 15.54
         WIDTH              = 100
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
OPEN QUERY {&SELF-NAME} FOR EACH tt-clientes WHERE
                                 tt-clientes.visualiza = YES NO-LOCK,
                            EACH tt-ped-venda WHERE
                                 tt-ped-venda.nome-abrev   = tt-clientes.nome-abrev AND
                                 tt-ped-venda.cod-sit-ped >= i-sit-ped-ini          AND
                                 tt-ped-venda.cod-sit-ped <= i-sit-ped-fin          AND
                                 LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) > 0
                                 NO-LOCK
                              BY tt-clientes.ordem
                              BY tt-clientes.repres
                              BY tt-clientes.nome-abrev.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Gerenciamento de E-mails do Crivo Financeiro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Gerenciamento de E-mails do Crivo Financeiro */
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
   
  IF AVAIL tt-clientes AND AVAIL tt-ped-venda AND tt-ped-venda.marca = '*' THEN DO:
     ASSIGN tt-ped-venda.marca = ''.

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

   DEF VAR c-crivo AS CHAR EXTENT 5.
   ASSIGN c-crivo[1] = "Com Cr‚dito"
          c-crivo[2] = "Sem Cr‚dito"
          c-crivo[3] = "Cr‚dito a Ser Reavaliado"
          c-crivo[4] = "Aprovados"
          c-crivo[5] = "Com Todas as Anal¡ses Financeiras".

   FIND FIRST tt-ped-venda WHERE
              tt-ped-venda.marca = "*" NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-ped-venda THEN DO:
      MESSAGE "Favor marcar os pedidos, que deseja enviar E-MAILS ! ! !"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Enviando_Emails *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   ASSIGN c-mensagem = "Segue anexo rela‡Æo de pedidos " +
          c-crivo[i-crivo] + CHR(10) +  CHR(10) +
          "Grato, " + CHR(13) + CHR(13) +
          "Departamento Financeiro" + CHR(13) +
          "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA".

   ASSIGN  i-lin = 99
           i-pag =  1.

   FOR EACH tt-ped-venda WHERE                                            
            tt-ped-venda.marca = "*" NO-LOCK,                             
       EACH tt-clientes WHERE  
            tt-clientes.visualiza  = YES AND
            tt-clientes.nome-abrev = tt-ped-venda.nome-abrev NO-LOCK      
      BREAK  BY tt-ped-venda.no-ab-reppri                                     
             BY tt-ped-venda.nr-pedcli. 


       RUN pi-acompanhar IN h-acomp (INPUT "Cliente: " + tt-clientes.nome-abrev + "   " + 
                                           "Pedido: " + tt-ped-venda.nr-pedcli).

       IF FIRST-OF(tt-ped-venda.no-ab-reppri) THEN DO:
          ASSIGN c-arq-email = SESSION:TEMP-DIRECTORY + tt-ped-venda.no-ab-reppri + ".txt".
          OUTPUT TO VALUE(c-arq-email) CONVERT SOURCE "ibm850".
       END.

       IF i-lin > 61 THEN DO:
          RUN pi-imp-cabec.
          ASSIGN i-lin = 7.
       END.

       PUT tt-clientes.nome-abrev    FORMAT "x(12)"        AT   1 
           tt-ped-venda.nr-pedcli    FORMAT "x(6)"         AT  14
           tt-ped-venda.dt-entrega   FORMAT "99/99/9999"   AT  21 
           tt-ped-venda.dt-implant   FORMAT "99/99/9999"   AT  32
           fn-sit-ped()              FORMAT "x(3)"         AT  43
           tt-ped-venda.tp-pedido    FORMAT "x(15)"        AT  47
           fn-sit-cred()             FORMAT "x(15)"        AT  63
           fn-cond-pagto()           FORMAT "x(20)"        AT  84
           tt-ped-venda.vl-aberto    FORMAT ">,>>>,>>9.99" AT 105 
           tt-ped-venda.vl-reservado FORMAT ">,>>>,>>9.99" AT 119
           tt-clientes.motivo        FORMAT "x(70)"        AT 132.

       PUT SKIP.
       ASSIGN i-lin = i-lin + 1.

       ACCUMULATE tt-ped-venda.vl-aberto    (TOTAL BY tt-ped-venda.no-ab-reppri).
       ACCUMULATE tt-ped-venda.vl-reservado (TOTAL BY tt-ped-venda.no-ab-reppri).

       IF LAST-OF(tt-ped-venda.no-ab-reppri) THEN DO:
          PUT SKIP.
          PUT "TOTAL REPRESENTANTE .................: " AT 49.
          PUT ACCUM TOTAL BY tt-ped-venda.no-ab-reppri tt-ped-venda.vl-aberto    FORMAT ">,>>>,>>9.99" AT 105.
          PUT ACCUM TOTAL BY tt-ped-venda.no-ab-reppri tt-ped-venda.vl-reservado FORMAT ">,>>>,>>9.99" AT 119.

          OUTPUT CLOSE.
          
          RUN esapi/esapi002.p (INPUT "financeiro@teartextil.com.br", /* e-mail remetente */
                                INPUT "albino.junior@teartextil.com.br", /* e-mail destinat rio */ 
                          /*      INPUT repres.e-mail                    /* E-mail Destinatario */  */
                                INPUT "Crivo Financeiro de Pedidos - TEAR TEXTIL" , /* Assunto */
                                INPUT c-mensagem, /* Mensagem */
                                INPUT c-arq-email, /*arquivo anexo*/
                                INPUT YES). /* Mostra Erros */
                              
          ASSIGN  i-lin = 99
                  i-pag =  1.
       END.
   END.
   RUN pi-finalizar in h-acomp.
   MESSAGE 'Email(s) Enviado(s) com Sucesso....'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca w-digita
ON CHOOSE OF bt-marca IN FRAME F-Main
DO:
    IF AVAIL tt-clientes AND AVAIL tt-ped-venda AND tt-ped-venda.marca = '' THEN DO:
       ASSIGN tt-ped-venda.marca = '*'.

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
    FOR EACH tt-clientes WHERE                                          
             tt-clientes.visualiza = YES NO-LOCK,                       
        EACH tt-ped-venda WHERE                                         
             tt-ped-venda.nome-abrev   = tt-clientes.nome-abrev AND     
             tt-ped-venda.cod-sit-ped >= i-sit-ped-ini          AND     
             tt-ped-venda.cod-sit-ped <= i-sit-ped-fin          AND     
             LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) > 0 NO-LOCK.

        ASSIGN  tt-ped-venda.marca = ''.

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
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos w-digita
ON CHOOSE OF bt-todos IN FRAME F-Main
DO:
  FOR EACH tt-clientes WHERE                                          
           tt-clientes.visualiza = YES NO-LOCK,                       
      EACH tt-ped-venda WHERE                                         
           tt-ped-venda.nome-abrev   = tt-clientes.nome-abrev AND     
           tt-ped-venda.cod-sit-ped >= i-sit-ped-ini          AND     
           tt-ped-venda.cod-sit-ped <= i-sit-ped-fin          AND     
           LOOKUP(STRING(tt-ped-venda.cod-sit-aval),c-sit-aval) > 0 NO-LOCK.

      ASSIGN  tt-ped-venda.marca = '*'.

   END.
   
   br-pedidos:REFRESH().
   APPLY 'value-changed' TO br-pedidos.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
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

  {utp/ut9000.i "ESSP0180F" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {include/i-inifld.i}
  STATUS INPUT OFF. /* Desliga Mensagem no Rodap‚ da Tela */

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").


  APPLY 'entry' TO br-pedidos IN FRAME {&FRAME-NAME}.

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
        "DATA: "                                  AT  62
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  68
        "HORA: "                                  AT  88
        STRING(TIME,"hh:mm:ss")                   AT  94
        SKIP(1).

    PUT "RELATORIO DA AVALIA€ÇO FINANCEIRA DE PEDIDOS " AT 38 SKIP(1). 
    FIND repres WHERE
         repres.nome-abrev = tt-ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
    IF AVAIL repres THEN DO:
       PUT "Representante: "    AT 1.
       PUT  repres.nome FORMAT "X(40)" AT 16 SKIP(1).
    END.
                                                                                            
    PUT "Cliente      Pedido Dt.Entrega Dt.Implant Sit Tipo de Pedido  Situa‡Æo de Cr‚dito  Condi‡Æo Pagamento   Valor Aberto Vlr Reservado Motivo" AT 1.
    PUT "------------ ------ ---------- ---------- --- --------------- -------------------- -------------------- ------------ ------------- ----------------------------------------------------------------------" AT 1.

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
  {src/adm/template/snd-list.i "tt-clientes"}
  {src/adm/template/snd-list.i "tt-ped-venda"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-cond-pagto w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-cred w-digita 
FUNCTION fn-sit-cred RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR c-sit-ped AS CHAR FORMAT "x(20)".

  {esinc/i-dsrb.i tt-ped-venda.cod-sit-aval tt-ped-venda.cod-sit-aval c-sit-ped}.

  RETURN c-sit-ped.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-ped w-digita 
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

