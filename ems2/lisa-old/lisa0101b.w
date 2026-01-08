&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i lisa0101b 9.99.99.999}

/* Chamada a include do gerenciador de licen‡as. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i lisa0101b rep}
&ENDIF

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{esapi/integrarRetornoERP.i}
{lisa/lisa0101a.i}
/*DEFINE INPUT  PARAMETER pNotaRetorno    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNotaRemessa    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer       AS CHARACTER   NO-UNDO.*/
DEFINE INPUT  PARAMETER pId AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER TABLE FOR ttItemNf.
DEFINE INPUT  PARAMETER TABLE FOR ttFiltro.
DEFINE OUTPUT PARAMETER rowidSaldoTerc  AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER qtRem           AS DECIMAL     NO-UNDO.

DEFINE VARIABLE nfIni AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE nfFim AS CHARACTER   NO-UNDO INIT 'zzzzzzzzzzzzz'.
DEFINE VARIABLE dtFim AS DATE        NO-UNDO.

DEFINE TEMP-TABLE ttSaldoTerc LIKE saldo-terc
    FIELD r-rowid AS ROWID.

DEFINE VARIABLE cSerie AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNat   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iEmit  AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSaldoTerc

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 ttSaldoTerc.nro-docto ttSaldoTerc.nat-operacao ttSaldoTerc.sequencia ttSaldoTerc.cod-refer ttSaldoTerc.quantidade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH ttSaldoTerc NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH ttSaldoTerc NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 ttSaldoTerc
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 ttSaldoTerc


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-108 fiProduto fiCodRefer fiNota ~
tgSomenteRemessa BROWSE-4 bt-ok bt-cancelar bt-ajuda fiqt fiqtLimite 
&Scoped-Define DISPLAYED-OBJECTS fiProduto fiCodRefer fiNota ~
tgSomenteRemessa fiqt fiqtLimite 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE VARIABLE fiCodRefer AS CHARACTER FORMAT "X(5)":U 
     LABEL "Refer.Orig." 
     VIEW-AS FILL-IN 
     SIZE 12 BY .79 NO-UNDO.

DEFINE VARIABLE fiNota AS CHARACTER FORMAT "X(15)":U 
     LABEL "NF.Remessa" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .79 NO-UNDO.

DEFINE VARIABLE fiProduto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Produto" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .79 NO-UNDO.

DEFINE VARIABLE fiqt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Qt.Remessa" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .79 NO-UNDO.

DEFINE VARIABLE fiqtLimite AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Qt.Limite" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 97.72 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-108
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 2.

DEFINE VARIABLE tgSomenteRemessa AS LOGICAL INITIAL yes 
     LABEL "S¢ NF.Remessa Corrente?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.57 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      ttSaldoTerc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 w-window _FREEFORM
  QUERY BROWSE-4 NO-LOCK DISPLAY
      ttSaldoTerc.nro-docto FORMAT "x(16)":U
ttSaldoTerc.nat-operacao FORMAT "x(06)":U
ttSaldoTerc.sequencia FORMAT ">>>>9":U
ttSaldoTerc.cod-refer FORMAT "x(8)":U
ttSaldoTerc.quantidade FORMAT ">>>>>,>>9.9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 17
         FONT 1
         TITLE "NFs Remessa com Saldo".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiProduto AT ROW 2.08 COL 7.72 COLON-ALIGNED WIDGET-ID 2
     fiCodRefer AT ROW 2.08 COL 31.29 COLON-ALIGNED WIDGET-ID 4
     fiNota AT ROW 2.08 COL 57.29 COLON-ALIGNED WIDGET-ID 6
     tgSomenteRemessa AT ROW 2.08 COL 76 WIDGET-ID 8
     BROWSE-4 AT ROW 4 COL 2 WIDGET-ID 200
     bt-ok AT ROW 21.46 COL 2.29
     bt-cancelar AT ROW 21.46 COL 13.29
     bt-ajuda AT ROW 21.46 COL 88.29
     fiqt AT ROW 21.5 COL 31.86 COLON-ALIGNED WIDGET-ID 12
     fiqtLimite AT ROW 21.5 COL 51.43 COLON-ALIGNED WIDGET-ID 14
     RECT-1 AT ROW 21.25 COL 1.29
     RECT-108 AT ROW 1.5 COL 2 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         PAGE-TOP SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.43 BY 21.71
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert Custom SmartWindow title>"
         HEIGHT             = 21.71
         WIDTH              = 99.43
         MAX-HEIGHT         = 41.33
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 41.33
         VIRTUAL-WIDTH      = 274.29
         ALWAYS-ON-TOP      = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-4 tgSomenteRemessa F-Main */
ASSIGN 
       fiCodRefer:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fiNota:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fiProduto:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       fiqtLimite:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSaldoTerc NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "ems2med.saldo-terc.cod-estabel = '505'
and saldo-terc.serie = '2'
and saldo-terc.cod-emitente = 38284
and saldo-terc.nro-docto >= nfIni
and saldo-terc.nro-docto <= nfFim
and saldo-terc.it-codigo = pItcodigo
and quantidade > 0
and saldo-terc.dt-retorno <= dtFim"
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* <insert Custom SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* <insert Custom SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
&Scoped-define SELF-NAME BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-4 w-window
ON VALUE-CHANGED OF BROWSE-4 IN FRAME F-Main /* NFs Remessa com Saldo */
DO:
  IF AVAIL ttSaldoTerc THEN DO:
     ASSIGN fiQT:SCREEN-VALUE = STRING(ttSaldoTerc.quantidade).
  END.
  ELSE
    ASSIGN fiQt:SCREEN-VALUE = '0'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-window
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
   ASSIGN rowidSaldoTerc = ?
          qtRem = 0.
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   IF AVAIL ttSaldoTerc THEN DO:
      IF dec(fiQtLimite:SCREEN-VALUE) < DEC(fiQt:SCREEN-VALUE) THEN DO:
         MESSAGE 'A Quantidade de Remessa nÆo pode ser maior que a quantidade Limite'
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
         RETURN NO-APPLY.
      END.
      ELSE DO:
        ASSIGN rowidSaldoTerc = ttSaldoTerc.r-rowid
             qtRem          = INPUT FRAME {&frame-name} fiQt. 
        apply "close":U to this-procedure.
      END.
   END.
   ELSE DO:
      MESSAGE 'NÆo existe Registro Selecionado. Clique em CANCELAR para sair '
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                         
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiqtLimite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiqtLimite w-window
ON LEAVE OF fiqtLimite IN FRAME F-Main /* Qt.Limite */
DO:
  IF AVAIL ttSaldoTerc THEN
     ASSIGN fiQt:SCREEN-VALUE = string(ttSaldoTerc.quantidade).
  ELSE
     ASSIGN fiQt:SCREEN-VALUE = '0'.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSomenteRemessa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSomenteRemessa w-window
ON VALUE-CHANGED OF tgSomenteRemessa IN FRAME F-Main /* S¢ NF.Remessa Corrente? */
DO:
  RUN setIntervalNFRemessa(logical(tgSomenteRemessa:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
  RUN preencherTT.
  {&open-query-browse-4}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY fiProduto fiCodRefer fiNota tgSomenteRemessa fiqt fiqtLimite 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 RECT-108 fiProduto fiCodRefer fiNota tgSomenteRemessa BROWSE-4 
         bt-ok bt-cancelar bt-ajuda fiqt fiqtLimite 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "lisa0101b" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  


  FIND ttItemNF 
      WHERE ttITemNF.id = pid NO-ERROR.

  

  //busca a data da nota fiscal de retorno para limitar as notas fiscais de remessa por essa data
  FIND retornos_lisa NO-LOCK 
      WHERE int(retornos_lisa.nr_nota_fis)   =int(ttItemNf.nfRetorno)
      NO-ERROR.

  IF AVAIL retornos_lisa THEN
     ASSIGN dtFim = retornos_lisa.dt_nf.
  
  
  //atribui valores dos campos passados por parametro para tela
  ASSIGN fiProduto:SCREEN-VALUE   = ttITemNf.itCodigo
         fiCodRefer:SCREEN-VALUE  = ttItemNf.codRefer
         fiNota:SCREEN-VALUE      = string(int(ttItemNf.nfOriginal),'9999999')
         fiQtLimite:SCREEN-VALUE  = string(ttITemNf.QtExcedente) .

  RUN setIntervalNFRemessa(YES).
  RUN preencherTT.

  {&open-query-browse-4}
  APPLY 'value-changed' TO BROWSE browse-4.

  RUN esapi/getVarsSaldoTercLisa.p(OUTPUT cSerie,OUTPUT cNat,OUTPUT iEmit).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preencherTT w-window 
PROCEDURE preencherTT :
DEFINE VARIABLE dSaldo AS DECIMAL     NO-UNDO.



EMPTY TEMP-TABLE ttSaldoTerc.
saldoTerc:
FOR EACH  saldo-terc NO-LOCK
        WHERE saldo-terc.cod-estabel = '505'
        and saldo-terc.serie = cSerie
        and saldo-terc.cod-emitente = iEmit
        AND saldo-terc.nat-operacao = cNat
        and saldo-terc.nro-docto >= nfIni
        and saldo-terc.nro-docto <= nfFim
        and saldo-terc.it-codigo = ttItemNf.itCodigo
        and saldo-terc.quantidade > 0
        and saldo-terc.dt-retorno <= dtFim .
        FOR EACH ttITemNf
            WHERE int(ttitemNf.nfOriginal) = INT(saldo-terc.nro-docto)
            AND   ttItemNf.itCodigo   = saldo-terc.it-codigo
            AND   ttItemNf.codRefer   = saldo-terc.cod-refer
            AND   ttItemNf.sequencia  = saldo-terc.sequencia
            ,
            EACH ttFiltro WHERE ttFiltro.agrup_id = ttItemNf.pedidoLisaId
            and ttFiltro.id = ttItemNf.id 
            .
            /*IF saldo-terc.quantidade < IF ttITemNf.qtNfSubst > 0 THEN ttItemNf.qtNfSubst
                                       ELSE ttItemNf.qtFatura THEN NEXT saldoterc.*/
            NEXT saldoTerc.
        END.
        CREATE ttsaldoTerc.
        BUFFER-COPY saldo-terc TO ttSaldoTerc.
        ASSIGN ttSaldoTerc.r-rowid = ROWID(saldo-terc).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ttSaldoTerc"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setIntervalNFRemessa w-window 
PROCEDURE setIntervalNFRemessa :
DEFINE INPUT  PARAMETER pSomenteRemessa AS LOGICAL     NO-UNDO.
IF pSomenteRemessa  THEN
     ASSIGN nfIni = fiNota:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            nfFim = fiNota:SCREEN-VALUE IN FRAME {&FRAME-NAME} .
  ELSE 
     ASSIGN nfIni = ''
            nfFim = '999999999'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
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

