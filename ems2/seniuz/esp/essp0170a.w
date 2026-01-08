&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgmov            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0150E 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* INICIO DA DEFINI€ÇO DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */

{esinc/espd0002.i}
/*
DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD pedido-ini       LIKE ped-venda.nr-pedido 
       FIELD pedido-fin       LIKE ped-venda.nr-pedido
       FIELD dt-emissao-ini   LIKE ped-venda.dt-emissao
       FIELD dt-emissao-fin   LIKE ped-venda.dt-emissao
       FIELD dt-entrega-ini   LIKE ped-venda.dt-entrega   
       FIELD dt-entrega-fin   LIKE ped-venda.dt-entrega   
       FIELD cod-emit-ini     LIKE ped-venda.cod-emitente 
       FIELD cod-emit-fin     LIKE ped-venda.cod-emitente 
       FIELD no-ab-reppri-ini LIKE ped-venda.no-ab-reppri 
       FIELD no-ab-reppri-fin LIKE ped-venda.no-ab-reppri
       FIELD nome-transp-ini  LIKE ped-venda.nome-transp
       FIELD nome-transp-fin  LIKE ped-venda.nome-transp
       FIELD corte-comerc-ini LIKE ped-item-ext.corte-comerc
       FIELD corte-comerc-fin LIKE ped-item-ext.corte-comerc
       FIELD so-indigo        AS   LOG
       FIELD exc-indigo       AS   LOG
       FIELD it-codigo        LIKE item.it-codigo EXTENT 10
       FIELD sit-total        AS   LOG
       FIELD sit-aberto       AS   LOG 
       FIELD sit-parcial      AS   LOG
       FIELD sit-pendentes    AS   LOG 
       FIELD sit-suspensos    AS   LOG 
       FIELD sit-cancelados   AS   LOG
       FIELD cond-credito     AS   CHAR FORMAT "x"
       FIELD cond-pagto       AS   CHAR FORMAT "x"
       FIELD mercado          AS   CHAR FORMAT "x"
       FIELD tp-pedido        LIKE ped-venda.tp-pedido
       FIELD aceita-parc      AS   CHAR FORMAT "x"
       FIELD qtd-minima       AS   DEC FORMAT ">>>,>>>,>>9.99" 
       FIELD perc-minres      AS   DEC FORMAT ">>9.99"
       FIELD min-it-ares      AS   INT FORMAT ">>>9" 
       FIELD max-it-ares      AS   INT FORMAT ">>>9"
       FIELD it-reservados    AS   LOG
       FIELD nr-coletor       AS   INT.
*/
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

/* FIM DAS DEFINI€åES DO RELATORIO ESPD0002RP.P */

DEF TEMP-TABLE tt-pedidos
    FIELD row-temp-table  AS ROWID
    FIELD nr-pedcli       LIKE ped-item.nr-pedcli
    FIELD nome-abrev      LIKE ped-item.nome-abrev
    FIELD qt-pedida       LIKE ped-item.qt-pedida
    FIELD dt-entrega      LIKE ped-venda.dt-entrega
    FIELD mercado         AS   LOG FORMAT "Interno/Externo"
    FIELD visualiza       AS   LOG.

DEFINE INPUT PARAMETER p-mercado AS CHAR.
DEFINE INPUT PARAMETER p-it-codigo AS CHAR.
DEFINE INPUT PARAMETER p-cod-refer AS CHAR.
DEFINE INPUT PARAMETER TABLE FOR tt-pedidos.  

DEF BUFFER b-tt-pedidos FOR tt-pedidos.

/* DEFINI€ÇO DE VARIAVEIS PARA A CHAMADA DO RELATORIO ESPD0002RP.P */
/*                                                      */

 DEFINE VAR raw-param   AS RAW NO-UNDO.
 DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
 DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
/* FIM DAS DEFINI€åES DE VARIAVEIS PARA CHAMADA DO RELATORIO ESPD0002RP.P */
/*                                                                        */

/* Local Variable Definitions ---                                       */
DEF VAR c-nat-oper-ini LIKE ped-venda.nat-operacao.
DEF VAR c-nat-oper-fin LIKE ped-venda.nat-operacao.
DEF VAR de-qt-pedida    AS DEC.
DEF VAR de-qt-reservada AS DEC.
DEF VAR l-ok  AS LOG.
DEF VAR i-lin AS INT.
DEF VAR de-tot-pedida    AS DEC.
DEF VAR de-tot-reservada AS DEC.
DEF VAR de-tot-saldo     AS DEC.
DEF VAR c-empresa        AS CHAR.
DEF NEW SHARED VAR c-nr-ped AS CHAR.

DEF NEW GLOBAL SHARED VAR gr-ped-venda AS ROWID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-pedidos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-pedidos ped-venda

/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-pedidos.nr-pedcli tt-pedidos.nome-abrev tt-pedidos.dt-entrega tt-pedidos.qt-pedida p-it-codigo p-cod-refer   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define OPEN-QUERY-br-pedidos RUN pi-totais. RUN pi-agrupa. OPEN QUERY {&SELF-NAME} FOR EACH tt-pedidos WHERE                                  tt-pedidos.visualiza = YES NO-LOCK, ~
                                   FIRST ped-venda WHERE                                   ped-venda.nr-pedcli = tt-pedidos.nr-pedcli AND                                   ped-venda.nome-abrev = tt-pedidos.nome-abrev AND                                   ped-venda.nat-operacao >= c-nat-oper-ini AND                                   ped-venda.nat-operacao <= c-nat-oper-fin                                   NO-LOCK BY tt-pedidos.dt-entrega                                           BY tt-pedidos.nr-pedcli.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-pedidos ped-venda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-pedidos
&Scoped-define SECOND-TABLE-IN-QUERY-br-pedidos ped-venda


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 rt-buttom rs-situacao br-pedidos ~
bt-detalhe bt-imprime bt-modifica bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS rs-situacao fi-tot-pedida 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-detalhe 
     IMAGE-UP FILE "image/img-det.bmp":U
     LABEL "Detalhe" 
     SIZE 10.57 BY 1.13.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "ImpressÆo" 
     SIZE 11 BY 1.13.

DEFINE BUTTON bt-modifica 
     IMAGE-UP FILE "image/img-mod.bmp":U
     LABEL "Modifica" 
     SIZE 11 BY 1.13.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-tot-pedida AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE rs-situacao AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Mercado Interno", "I",
"Mercado Externo", "E",
"Ambos", "A"
     SIZE 56.86 BY .88
     BGCOLOR 8 FGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 1.25
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pedidos FOR 
      tt-pedidos, 
      ped-venda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos D-Dialog _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-pedidos.nr-pedcli  FORMAT "x(12)":U
tt-pedidos.nome-abrev       FORMAT "x(12)":U  WIDTH 15
tt-pedidos.dt-entrega
tt-pedidos.qt-pedida        COLUMN-LABEL "Qt Pedida" WIDTH 12
p-it-codigo                 COLUMN-LABEL "Item" 
p-cod-refer                 COLUMN-LABEL "Referˆncia"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 75 BY 10.5
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rs-situacao AT ROW 1.42 COL 3.14 NO-LABEL
     br-pedidos AT ROW 2.75 COL 2
     bt-detalhe AT ROW 13.5 COL 2
     bt-imprime AT ROW 13.5 COL 12.72
     bt-modifica AT ROW 13.5 COL 23.72
     fi-tot-pedida AT ROW 13.63 COL 33.14 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 15.25 COL 3
     bt-cancela AT ROW 15.25 COL 13.43
     bt-ajuda AT ROW 15.25 COL 66.14
     RECT-51 AT ROW 1.25 COL 2
     rt-buttom AT ROW 15 COL 2
     SPACE(0.85) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Pedidos Selecionados - ESSP0170A"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
/* BROWSE-TAB br-pedidos rs-situacao D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-tot-pedida IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
RUN pi-totais.
RUN pi-agrupa.
OPEN QUERY {&SELF-NAME} FOR EACH tt-pedidos WHERE
                                 tt-pedidos.visualiza = YES NO-LOCK,
                            FIRST ped-venda WHERE
                                  ped-venda.nr-pedcli = tt-pedidos.nr-pedcli AND
                                  ped-venda.nome-abrev = tt-pedidos.nome-abrev AND
                                  ped-venda.nat-operacao >= c-nat-oper-ini AND
                                  ped-venda.nat-operacao <= c-nat-oper-fin
                                  NO-LOCK BY tt-pedidos.dt-entrega
                                          BY tt-pedidos.nr-pedcli.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Pedidos Selecionados - ESSP0170A */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe D-Dialog
ON CHOOSE OF bt-detalhe IN FRAME D-Dialog /* Detalhe */
DO:
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   CREATE tt-param.
   ASSIGN tt-param.usuario          = c-seg-usuario
          tt-param.data-exec        = TODAY
          tt-param.hora-exec        = TIME
          tt-param.destino          = 3
          tt-param.classifica       = 1
          tt-param.arquivo          = SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp"
          tt-param.estabel-ini      = ped-venda.cod-estabel 
          tt-param.estabel-fin      = ped-venda.cod-estabel
          tt-param.pedido-ini       = INT(tt-pedidos.nr-pedcli)
          tt-param.pedido-fin       = INT(tt-pedidos.nr-pedcli)
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

   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* ImpressÆo */
DO:
   CLOSE QUERY br-pedidos. 
   RUN pi-imprime.
   {&OPEN-QUERY-br-pedidos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica D-Dialog
ON CHOOSE OF bt-modifica IN FRAME D-Dialog /* Modifica */
DO:
   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-pedidos.nr-pedcli AND
        ped-venda.nome-abrev = tt-pedidos.nome-abrev
        NO-LOCK NO-ERROR.

   ASSIGN gr-ped-venda = ROWID(ped-venda).

   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   RUN esp\espd4000.w (INPUT "Modificar").
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-situacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-situacao D-Dialog
ON VALUE-CHANGED OF rs-situacao IN FRAME D-Dialog
DO:
   CASE SELF:SCREEN-VALUE.
       WHEN 'I' THEN
           ASSIGN c-nat-oper-ini = '1' 
                  c-nat-oper-fin = '6ZZZZZ'.
       WHEN 'E' THEN
           ASSIGN c-nat-oper-ini = '7' 
                  c-nat-oper-fin = '7ZZZZZ'.
       WHEN 'A' THEN
           ASSIGN c-nat-oper-ini = '1' 
                  c-nat-oper-fin = '7ZZZZZ'.
   END CASE.

   CLOSE QUERY br-pedidos.
   {&OPEN-QUERY-br-pedidos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY rs-situacao fi-tot-pedida 
      WITH FRAME D-Dialog.
  ENABLE RECT-51 rt-buttom rs-situacao br-pedidos bt-detalhe bt-imprime 
         bt-modifica bt-ok bt-cancela bt-ajuda 
      WITH FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

 /* {utp/ut9000.i "ESSP0150E" "2.04.00.000"} */

  ASSIGN rs-situacao = p-mercado.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  APPLY 'value-changed' TO rs-situacao IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-agrupa D-Dialog 
PROCEDURE pi-agrupa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN de-qt-pedida    = 0.
    FOR EACH b-tt-pedidos WHERE 
             b-tt-pedidos.visualiza = YES NO-LOCK
             BREAK BY b-tt-pedidos.nr-pedcli:
        ASSIGN de-qt-pedida    = de-qt-pedida    + b-tt-pedidos.qt-pedida.
         
        ASSIGN b-tt-pedidos.visualiza = NO.
        IF LAST-OF(b-tt-pedidos.nr-pedcli) THEN DO:
           ASSIGN b-tt-pedidos.visualiza = YES
                  b-tt-pedidos.qt-pedida    = de-qt-pedida.
           ASSIGN de-qt-pedida = 0.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carrega-dados D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec D-Dialog 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT c-empresa  FORMAT "X(40)"                 AT  1
     "DATA: "                                  AT 47
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT 53
     "HORA: "                                  AT 67
     STRING(TIME,"hh:mm:ss")                   AT 73
     SKIP(1).
    
 PUT "RELATORIO DE DETALHAMENTO DOS PEDIDOS" AT 22 SKIP(1).

 
 PUT "Pedido Nome Abrevidado  Dt.Entrega  Qtde Pedida Qt Reservada        Saldo" AT 1.
 PUT "------ ---------------  ----------  ----------- ------------ ------------" AT 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime D-Dialog 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 SYSTEM-DIALOG PRINTER-SETUP UPDATE l-ok.
 IF l-ok THEN DO:
    OUTPUT TO PRINTER PAGED PAGE-SIZE 74.
    ASSIGN i-lin            = 99
           de-tot-pedida    =  0
           de-tot-saldo     =  0.

    FOR EACH tt-pedidos WHERE 
             tt-pedidos.visualiza = YES NO-LOCK
             BY tt-pedidos.dt-entrega.

        IF i-lin > 74 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        PUT tt-pedidos.nr-pedcli     FORMAT "x(6)"  AT   1
            tt-pedidos.nome-abrev    FORMAT "x(12)"  AT  8
            ped-venda.dt-entrega                     AT 25
            tt-pedidos.qt-pedida     FORMAT "->>>,>>9.99" AT 37.

        ASSIGN i-lin            = i-lin + 1
               de-tot-pedida    = de-tot-pedida  + tt-pedidos.qt-pedida.
    END.
    IF de-tot-pedida    <> 0 THEN DO.
       PUT "----------- ------------ ------------"   AT 37.
       PUT "T O T A I S  . . . ."  AT 13
           de-tot-pedida     FORMAT "->>>,>>9.99" AT 37.
    END.
    OUTPUT CLOSE.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais D-Dialog 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN fi-tot-pedida  = 0.
  FOR EACH tt-pedidos WHERE
           tt-pedidos.visualiza = YES NO-LOCK,
      FIRST ped-venda WHERE
            ped-venda.nr-pedcli = tt-pedidos.nr-pedcli AND
            ped-venda.nome-abrev = tt-pedidos.nome-abrev AND
            ped-venda.nat-operacao >= c-nat-oper-ini AND
            ped-venda.nat-operacao <= c-nat-oper-fin NO-LOCK.

      ASSIGN fi-tot-pedida = fi-tot-pedida + tt-pedidos.qt-pedida.
  END.
  DISP fi-tot-pedida
       WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-pedidos"}
  {src/adm/template/snd-list.i "ped-venda"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

