&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
{include/i-prgvrs.i ESSP0150F 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* INICIO DA DEFINI€ÇO DAS TABELAS TEMPORARIAS; PARA A CHAMADA DO RELATORIO ESPD0002RP.P */
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

/* FIM DAS DEFINI€åES DO RELATORIO ESPD0002RP.P */

/* Parameters Definitions ---                                           */

DEF TEMP-TABLE tt-pedidos
    FIELD row-tt-movto    AS ROWID
    FIELD nr-pedcli       LIKE ped-venda.nr-pedcli
    FIELD nome-abrev      LIKE ped-venda.nome-abrev
    FIELD dt-entrega      LIKE ped-venda.dt-entrega
    FIELD qt-pedida       LIKE ped-item.qt-pedida
    FIELD qt-reservada    LIKE ped-item.qt-pedida
    FIELD visualiza       AS LOG 
    INDEX indice1 IS PRIMARY row-tt-movto nr-pedcli.

DEF TEMP-TABLE tt-ped-item
    FIELD row-tt-movto AS ROWID
    FIELD nr-pedcli    LIKE ped-venda.nr-pedcli
    FIELD nome-abrev   LIKE ped-venda.nome-abrev
    FIELD nr-sequencia LIKE ped-item.nr-sequencia
    FIELD qt-pedida    LIKE ob-etiqueta.quantidade  LABEL "Qt Estoq"
    FIELD qt-reservada LIKE ped-item.qt-pedida
    FIELD visualiza    AS   LOG
    INDEX indice1 IS PRIMARY nr-pedcli nome-abrev nr-sequencia.

DEFINE INPUT  PARAMETER TABLE FOR tt-pedidos.  
DEFINE INPUT  PARAMETER TABLE FOR tt-ped-item.  

/* DEFINI€ÇO DE VARIAVEIS PARA A CHAMADA DO RELATORIO ESPD0002RP.P */
/*                                                      */

 DEFINE VAR raw-param   AS RAW NO-UNDO.
 DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
 DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
/* FIM DAS DEFINI€åES DE VARIAVEIS PARA CHAMADA DO RELATORIO ESPD0002RP.P */
/*                                                                        */


/* Local Variable Definitions ---                                       */
DEF VAR de-qt-pedida     AS DEC.
DEF VAR de-qt-reservada  AS DEC.
DEF VAR l-ok             AS LOG.
DEF VAR i-lin            AS INT.
DEF VAR i-sit-cart       AS INT.
DEF VAR de-tot-pedida    AS DEC.
DEF VAR de-tot-reservada AS DEC.
DEF VAR de-tot-saldo     AS DEC.
DEF VAR c-empresa        AS CHAR.

DEF NEW SHARED VAR c-nr-ped AS CHAR.
DEF NEW GLOBAL SHARED VAR gr-ped-venda AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

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
&Scoped-define INTERNAL-TABLES tt-ped-item tt-pedidos

/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-pedidos.nr-pedcli tt-pedidos.nome-abrev tt-pedidos.dt-entrega tt-ped-item.nr-sequencia tt-ped-item.qt-pedida tt-ped-item.qt-reservada tt-ped-item.qt-pedida - tt-ped-item.qt-reservada fn-fat-parcial() fn-credito() fn-cond-pagto()   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define OPEN-QUERY-br-pedidos RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-item WHERE                                  tt-ped-item.visualiza = YES AND                                 (IF i-sit-cart = 1                                     THEN tt-ped-item.qt-reservada = 0                                     ELSE IF i-sit-cart = 2                                          THEN tt-ped-item.qt-reservada <> 0                                          ELSE tt-ped-item.qt-reservada >= 0) NO-LOCK, ~
                                   FIRST tt-pedidos WHERE                                   tt-pedidos.nr-pedcli = tt-ped-item.nr-pedcli AND                                   tt-pedidos.nome-abrev = tt-ped-item.nome-abrev NO-LOCK                              BY tt-pedidos.dt-entrega.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-ped-item tt-pedidos
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-ped-item
&Scoped-define SECOND-TABLE-IN-QUERY-br-pedidos tt-pedidos


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 rt-buttom rs-situacao br-pedidos ~
bt-detalhe bt-imprime bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS rs-situacao fi-tot-pedida fi-tot-reserva ~
fi-tot-saldo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-cond-pagto D-Dialog 
FUNCTION fn-cond-pagto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-credito D-Dialog 
FUNCTION fn-credito RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-fat-parcial D-Dialog 
FUNCTION fn-fat-parcial RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE fi-tot-pedida AS DECIMAL FORMAT "-ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-reserva AS DECIMAL FORMAT "-ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-saldo AS DECIMAL FORMAT "-Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE rs-situacao AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Carteira", 1,
"Carteira Reservada", 2,
"Ambas", 3
     SIZE 53.86 BY .88
     BGCOLOR 8 FGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 88 BY 1.25
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 88 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pedidos FOR 
      tt-ped-item, 
      tt-pedidos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos D-Dialog _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-pedidos.nr-pedcli  FORMAT "x(12)":U
      tt-pedidos.nome-abrev FORMAT "x(12)":U  WIDTH 15
      tt-pedidos.dt-entrega                                                          WIDTH 10
      tt-ped-item.nr-sequencia                          COLUMN-LABEL "Seq"           WIDTH 4
      tt-ped-item.qt-pedida                             COLUMN-LABEL "Qt Pedida"     WIDTH 11
      tt-ped-item.qt-reservada                          COLUMN-LABEL "Qt Reservada"  WIDTH 10
      tt-ped-item.qt-pedida - tt-ped-item.qt-reservada  COLUMN-LABEL "Saldo"  FORMAT "->>,>>>,>>9.99" WIDTH 11
      fn-fat-parcial()      FORMAT "Sim/NÆo"            COLUMN-LABEL "FP"            WIDTH 4
      fn-credito()          FORMAT "x(15)"              COLUMN-LABEL "Credito"       WIDTH 10
      fn-cond-pagto()       FORMAT "x(15)"              COLUMN-LABEL "CondPagto"     WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 88 BY 9.96
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rs-situacao AT ROW 1.42 COL 3.14 NO-LABEL
     br-pedidos AT ROW 2.75 COL 2
     bt-detalhe AT ROW 13.25 COL 2
     bt-imprime AT ROW 13.25 COL 12.72
     bt-modifica AT ROW 13.25 COL 23.72
     fi-tot-pedida AT ROW 13.38 COL 40.57 COLON-ALIGNED NO-LABEL
     fi-tot-reserva AT ROW 13.38 COL 51.86 COLON-ALIGNED NO-LABEL
     fi-tot-saldo AT ROW 13.38 COL 63.14 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 15.08 COL 3
     bt-cancela AT ROW 15.08 COL 13.43
     bt-ajuda AT ROW 15.08 COL 79.14
     RECT-51 AT ROW 1.21 COL 2
     rt-buttom AT ROW 14.83 COL 2
     SPACE(0.71) SKIP(0.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Pedidos Selecionados - ESSP0150F"
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

/* SETTINGS FOR BUTTON bt-modifica IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-pedida IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-reserva IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-saldo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-item WHERE
                                 tt-ped-item.visualiza = YES AND
                                (IF i-sit-cart = 1
                                    THEN tt-ped-item.qt-reservada = 0
                                    ELSE IF i-sit-cart = 2
                                         THEN tt-ped-item.qt-reservada <> 0
                                         ELSE tt-ped-item.qt-reservada >= 0) NO-LOCK,
                            FIRST tt-pedidos WHERE
                                  tt-pedidos.nr-pedcli = tt-ped-item.nr-pedcli AND
                                  tt-pedidos.nome-abrev = tt-ped-item.nome-abrev NO-LOCK
                             BY tt-pedidos.dt-entrega.
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Pedidos Selecionados - ESSP0150F */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos D-Dialog
ON VALUE-CHANGED OF br-pedidos IN FRAME D-Dialog
DO:
   FIND ped-venda WHERE
        ped-venda.nome-abrev = tt-pedidos.nome-abrev AND 
        ped-venda.nr-pedcli = tt-pedidos.nr-pedcli
        NO-LOCK NO-ERROR.

   ASSIGN bt-modifica:SENSITIVE = AVAIL ped-venda AND ped-venda.cod-sit-ped <= 2.
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
    FIND ped-venda WHERE
         ped-venda.nr-pedcli = tt-pedidos.nr-pedcli AND
         ped-venda.nome-abrev = tt-pedidos.nome-abrev NO-LOCK.

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
   RUN pi-ver-permissao.
   IF NOT l-ok THEN
      MESSAGE "Usu rio: " + c-seg-usuario + ", nÆo tem permissÆo para essa rotina."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   ELSE DO:
      FIND ped-venda WHERE
           ped-venda.nr-pedcli = tt-pedidos.nr-pedcli AND
           ped-venda.nome-abrev = tt-pedidos.nome-abrev NO-LOCK.

      ASSIGN gr-ped-venda = ROWID(ped-venda).
  
      ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
      RUN esp\espd4000.w (INPUT "Modificar").
      ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-situacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-situacao D-Dialog
ON VALUE-CHANGED OF rs-situacao IN FRAME D-Dialog
DO:
   i-sit-cart = SELF:INPUT-VALUE.
   {&OPEN-QUERY-br-pedidos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
ASSIGN i-sit-cart = 3.
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
  DISPLAY rs-situacao fi-tot-pedida fi-tot-reserva fi-tot-saldo 
      WITH FRAME D-Dialog.
  ENABLE RECT-51 rt-buttom rs-situacao br-pedidos bt-detalhe bt-imprime bt-ok 
         bt-cancela bt-ajuda 
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa
       WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").


  /* Code placed here will execute AFTER standard behavior.    */

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
 PUT c-empresa  FORMAT "X(40)"   AT  1
     "DATA: "                                  AT 64
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT 70
     "HORA: "                                  AT 103
     STRING(TIME,"hh:mm:ss")                   AT 109
     SKIP(1).
    
 PUT "RELATORIO DE DETALHAMENTO DOS PEDIDOS" AT 45 SKIP(1).

                                                                                                                
 PUT "PEDIDO NOME             SEQ DT.ENTRAGA  QTDE PEDIDA QT RESERVADA        SALDO FP  CREDITO      CONDICAO DE PAGAMENTO" AT 1.
 PUT "------ ---------------  --- ----------  ----------- ------------ ------------ --- ------------ ---------------------" AT 1.

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

    OUTPUT TO PRINTER CONVERT TARGET "ISO8859-1" PAGED PAGE-SIZE 61.
    PUT CONTROL "~033(s16H". 

    ASSIGN i-lin            = 99
           de-tot-pedida    =  0
           de-tot-reservada =  0
           de-tot-saldo     =  0.

    FOR EACH tt-ped-item WHERE
             tt-ped-item.visualiza = YES AND
             (if i-sit-cart = 1 THEN tt-ped-item.qt-reservada = 0
                                ELSE 
                                   IF  i-sit-cart = 2 THEN tt-ped-item.qt-reservada <> 0
                                                      ELSE tt-ped-item.qt-reservada >= 0) NO-LOCK,
        FIRST tt-pedidos WHERE
              tt-pedidos.nr-pedcli = tt-ped-item.nr-pedcli AND
              tt-pedidos.nome-abrev = tt-ped-item.nome-abrev NO-LOCK
              BY tt-pedidos.dt-entrega.

        IF i-lin > 61 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.

        PUT tt-pedidos.nr-pedcli      FORMAT "x(6)"   AT  1
            tt-pedidos.nome-abrev     FORMAT "x(12)"  AT  8
            tt-ped-item.nr-sequencia  FORMAT ">>9"    AT 25
            tt-pedidos.dt-entrega                    AT 29
            tt-ped-item.qt-pedida     FORMAT "->>>,>>9.99" AT 41
            tt-ped-item.qt-reservada  FORMAT "->>>,>>9.99" AT 54
            tt-ped-item.qt-pedida - tt-ped-item.qt-reservada FORMAT "->>>,>>9.99"  AT 67
            fn-fat-parcial() FORMAT "Sim/Nao" AT 79
            fn-credito()     FORMAT "x(12)"   AT 83
            fn-cond-pagto()  FORMAT "x(20)"   AT 96.

        ASSIGN i-lin            = i-lin + 1
               de-tot-pedida    = de-tot-pedida  + tt-ped-item.qt-pedida                   
               de-tot-reservada = de-tot-reservada + tt-ped-item.qt-reservada                       
               de-tot-saldo     = de-tot-saldo + (tt-ped-item.qt-pedida - tt-ped-item.qt-reservada).
    END.
    IF de-tot-pedida    <> 0 OR 
       de-tot-reservada <> 0 OR
       de-tot-saldo     <> 0 THEN DO:
       PUT "----------- ------------ ------------"   AT 41.
       PUT "T O T A I S  . . . ."  AT 19
           de-tot-pedida     FORMAT "->>>,>>9.99" AT 41
           de-tot-reservada  FORMAT "->>>,>>9.99" AT 54
           de-tot-saldo      FORMAT "->>>,>>9.99" AT 67.
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
  ASSIGN fi-tot-pedida  = 0
         fi-tot-reserva = 0
         fi-tot-saldo   = 0.

  FOR EACH tt-ped-item WHERE
           tt-ped-item.visualiza = YES AND
           (if i-sit-cart = 1 THEN tt-ped-item.qt-reservada = 0
            ELSE IF  i-sit-cart = 2 THEN tt-ped-item.qt-reservada <> 0
            ELSE tt-ped-item.qt-reservada >= 0) NO-LOCK.

      ASSIGN fi-tot-pedida = fi-tot-pedida + tt-ped-item.qt-pedida                   
             fi-tot-reserva = fi-tot-reserva + tt-ped-item.qt-reservada                       
             fi-tot-saldo = fi-tot-saldo + (tt-ped-item.qt-pedida - tt-ped-item.qt-reservada).
  END.
  DISP fi-tot-pedida
       fi-tot-reserva
       fi-tot-saldo
       WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-permissao D-Dialog 
PROCEDURE pi-ver-permissao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-grupos AS CHAR FORMAT "x(20)".
    
    ASSIGN l-ok = NO.
    FIND FIRST espec.param-dis NO-LOCK NO-ERROR.

    IF SELF:NAME = "bt-inclui" THEN
       ASSIGN c-grupos = espec.param-dis.grp-inc-ped.
    IF SELF:NAME = "bt-modifica" THEN
       ASSIGN c-grupos = espec.param-dis.grp-alt-ped.
    IF SELF:NAME = "bt-cancela" THEN
       ASSIGN c-grupos = espec.param-dis.grp-can-ped.
    IF SELF:NAME = "bt-suspende" THEN
       ASSIGN c-grupos = espec.param-dis.grp-sus-ped.

    FOR EACH usuar_grp_usuar WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
        IF INDEX(c-grupos,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN DO:
           ASSIGN l-ok = YES.
           LEAVE.
        END.
    END.

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
  {src/adm/template/snd-list.i "tt-ped-item"}
  {src/adm/template/snd-list.i "tt-pedidos"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-cond-pagto D-Dialog 
FUNCTION fn-cond-pagto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ped-venda WHERE
       ped-venda.nr-pedcli = tt-pedidos.nr-pedcli AND
       ped-venda.nome-abrev = tt-pedidos.nome-abrev NO-LOCK NO-ERROR.

  IF ped-venda.cod-cond-pag <> 0 THEN DO:
     FIND cond-pagto WHERE 
         cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK.

     RETURN cond-pagto.descricao.
  END.
  ELSE RETURN 'ESPECIAL'. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-credito D-Dialog 
FUNCTION fn-credito RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR c-sit-aval AS CHAR.

  {esinc/i-dsrb.i ped-venda.cod-sit-aval ped-venda.cod-sit-aval c-sit-aval}

  RETURN c-sit-aval.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-fat-parcial D-Dialog 
FUNCTION fn-fat-parcial RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ped-venda WHERE
       ped-venda.nr-pedcli = tt-pedidos.nr-pedcli AND
       ped-venda.nome-abrev = tt-pedidos.nome-abrev NO-LOCK NO-ERROR.

  RETURN ped-venda.ind-fat-par.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

