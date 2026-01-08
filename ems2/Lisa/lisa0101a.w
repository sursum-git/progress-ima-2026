&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i lisa0101a 9.99.99.999}

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i lisa0101a rep}
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

{lisa/lisa0101a.i}
{esapi/integrarRetornoERP.i}
DEFINE TEMP-TABLE ttItemNfOri LIKE ttItemNf.
DEFINE TEMP-TABLE ttFiltroOri LIKE ttFiltro.
//DEFINE INPUT  PARAMETER pAcao      AS CHARACTER   NO-UNDO.

DEFINE INPUT  PARAMETER pNotaRet   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pQuant     AS DECIMAL     NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER TABLE FOR ttItemNf.
DEFINE INPUT  PARAMETER pId        AS INT        NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFiltro.
DEFINE INPUT  PARAMETER pRetornoLisa  AS INTEGER     NO-UNDO.

DEFINE VARIABLE RowidSaldoTerc AS ROWID       NO-UNDO.
DEFINE VARIABLE qtRem          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE qtRecompor     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iCor           AS INTEGER     NO-UNDO.

DEFINE BUFFER bf FOR ttItemNf.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttItemNF ttFiltro

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttItemNF.nfOriginal ttItemNF.sequencia ttItemNF.codRefer ttItemNF.qtFaturada ttItemNf.qtNfSubst ttITemNf.qtSaldoDisp ttItemNf.qtExcedente ttItemNF.nfSubstituta ttItemNF.codReferSubst   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2   
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttItemNF     WHERE ttItemNf.retornoLisaId = pRetornoLisa, ~
           FIRST ttFiltro WHERE     ttFiltro.agrup_id = ttItemNf.pedidoLisaId     AND ttFiltro.id   = ttItemNf.id
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttItemNF     WHERE ttItemNf.retornoLisaId = pRetornoLisa, ~
           FIRST ttFiltro WHERE     ttFiltro.agrup_id = ttItemNf.pedidoLisaId     AND ttFiltro.id   = ttItemNf.id.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttItemNF ttFiltro
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttItemNF
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 ttFiltro


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button RECT-106 RECT-109 RECT-107 ~
RECT-110 fiNrRetorno fiCodReferRetorno fiQtRetorno fiSaldo fiItCodigo ~
fiDescItem BROWSE-2 btAlterar fiNrRemessa fiNrSeqRemessa fiCodReferRemessa ~
btIncluir btExcluir 
&Scoped-Define DISPLAYED-OBJECTS fiNrRetorno fiCodReferRetorno fiQtRetorno ~
fiSaldo fiItCodigo fiDescItem fiNrRemessa fiNrSeqRemessa fiCodReferRemessa ~
fiQtRetRemessa 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
DEFINE BUTTON btAlterar 
     LABEL "Alterar" 
     SIZE 8 BY 1.13.

DEFINE BUTTON btCanc 
     LABEL "Cancelar" 
     SIZE 8 BY 1.13.

DEFINE BUTTON btConfirmar 
     LABEL "Confirmar" 
     SIZE 8 BY 1.13.

DEFINE BUTTON btExcluir 
     LABEL "Excluir" 
     SIZE 8 BY 1.13.

DEFINE BUTTON btIncluir 
     LABEL "Incluir" 
     SIZE 8 BY 1.13.

DEFINE VARIABLE fiCodReferRemessa AS CHARACTER FORMAT "X(5)":U 
     LABEL "Refer." 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .88 NO-UNDO.

DEFINE VARIABLE fiCodReferRetorno AS CHARACTER FORMAT "X(5)":U 
     LABEL "Refer." 
     VIEW-AS FILL-IN 
     SIZE 7.43 BY .88 NO-UNDO.

DEFINE VARIABLE fiDescItem AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 62.57 BY .88 NO-UNDO.

DEFINE VARIABLE fiItCodigo AS CHARACTER FORMAT "X(15)":U 
     LABEL "Produto" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fiNrRemessa AS CHARACTER FORMAT "X(15)":U 
     LABEL "Nr.Remessa" 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .79 NO-UNDO.

DEFINE VARIABLE fiNrRetorno AS CHARACTER FORMAT "X(15)":U 
     LABEL "NF.Retorno" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE fiNrSeqRemessa AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Nr.Seq." 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY .88 NO-UNDO.

DEFINE VARIABLE fiQtRetorno AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88 NO-UNDO.

DEFINE VARIABLE fiQtRetRemessa AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Qte." 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88 NO-UNDO.

DEFINE VARIABLE fiSaldo AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Saldo a Retornar" 
     VIEW-AS FILL-IN 
     SIZE 9.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-106
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 112.14 BY 2.92.

DEFINE RECTANGLE RECT-107
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 10.25.

DEFINE RECTANGLE RECT-109
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 37.72 BY 1.5
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-110
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 37.72 BY 1.5
     BGCOLOR 7 .

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 113 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      ttItemNF, 
      ttFiltro SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 w-livre _FREEFORM
  QUERY BROWSE-2 DISPLAY
      ttItemNF.nfOriginal
ttItemNF.sequencia
ttItemNF.codRefer
ttItemNF.qtFaturada     
ttItemNf.qtNfSubst
ttITemNf.qtSaldoDisp
ttItemNf.qtExcedente
ttItemNF.nfSubstituta
ttItemNF.codReferSubst   COLUMN-LABEL "Refer.Subst."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 73.29 BY 10.5
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiNrRetorno AT ROW 2.92 COL 18.29 COLON-ALIGNED WIDGET-ID 2
     fiCodReferRetorno AT ROW 2.92 COL 37.14 COLON-ALIGNED WIDGET-ID 14
     fiQtRetorno AT ROW 2.92 COL 54.29 COLON-ALIGNED WIDGET-ID 6
     fiSaldo AT ROW 2.92 COL 83.86 COLON-ALIGNED WIDGET-ID 22
     fiItCodigo AT ROW 4.08 COL 18.57 COLON-ALIGNED WIDGET-ID 30
     fiDescItem AT ROW 4.08 COL 31.57 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     BROWSE-2 AT ROW 5.42 COL 1.72 WIDGET-ID 200
     btAlterar AT ROW 6 COL 77 WIDGET-ID 40
     fiNrRemessa AT ROW 8.08 COL 85 COLON-ALIGNED WIDGET-ID 12
     fiNrSeqRemessa AT ROW 9.08 COL 85 COLON-ALIGNED WIDGET-ID 18
     fiCodReferRemessa AT ROW 10.08 COL 85 COLON-ALIGNED WIDGET-ID 16
     fiQtRetRemessa AT ROW 11.08 COL 85 COLON-ALIGNED WIDGET-ID 20
     btConfirmar AT ROW 14.67 COL 77 WIDGET-ID 44
     btCanc AT ROW 14.67 COL 85.14 WIDGET-ID 46
     btIncluir AT ROW 16.04 COL 2 WIDGET-ID 38
     btExcluir AT ROW 16.04 COL 10 WIDGET-ID 42
     rt-button AT ROW 1 COL 1
     RECT-106 AT ROW 2.5 COL 1.86 WIDGET-ID 8
     RECT-109 AT ROW 14.5 COL 76.29 WIDGET-ID 48
     RECT-107 AT ROW 5.75 COL 76 WIDGET-ID 24
     RECT-110 AT ROW 5.83 COL 76.29 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 16.71
         FONT 1 WIDGET-ID 100.


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
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 16.71
         WIDTH              = 113.86
         MAX-HEIGHT         = 40.5
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 40.5
         VIRTUAL-WIDTH      = 274.29
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
/* BROWSE-TAB BROWSE-2 fiDescItem f-cad */
/* SETTINGS FOR BUTTON btCanc IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btConfirmar IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiCodReferRemessa:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fiCodReferRetorno:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fiDescItem:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fiItCodigo:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fiNrRemessa:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fiNrRetorno:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fiNrSeqRemessa:READ-ONLY IN FRAME f-cad        = TRUE.

ASSIGN 
       fiQtRetorno:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fiQtRetRemessa IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       fiSaldo:READ-ONLY IN FRAME f-cad        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttItemNF
    WHERE ttItemNf.retornoLisaId = pRetornoLisa,
    FIRST ttFiltro WHERE
    ttFiltro.agrup_id = ttItemNf.pedidoLisaId
    AND ttFiltro.id   = ttItemNf.id
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  //APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 w-livre
ON ROW-DISPLAY OF BROWSE-2 IN FRAME f-cad
DO:
    IF ttItemNf.qtExcedente > 0 THEN DO:
      ASSIGN iCor = 12.
   END.
   ELSE DO:
      ASSIGN iCor = 0.

   END.
   ASSIGN ttItemNF.sequencia:FGCOLOR    IN BROWSE {&browse-name}   = iCor
          ttItemNf.codRefer:FGCOLOR     IN BROWSE {&browse-name}   = iCor
          ttItemNf.nfOriginal:FGCOLOR   IN BROWSE {&browse-name}   = iCor
          ttItemNf.qtfaturada:FGCOLOR   IN BROWSE {&browse-name}   = iCor
          ttItemNf.qtSaldoDisp:FGCOLOR  IN BROWSE {&browse-name}   = iCor
          ttItemNF.qtExcedente:FGCOLOR  IN BROWSE {&browse-name}   = iCor
          ttItemNf.qtnfSubst:FGCOLOR    IN BROWSE {&browse-name}   = iCor
          ttItemNf.nfSubstituta:FGCOLOR IN BROWSE {&browse-name}   = iCor
          ttitemNf.codReferSub:FGCOLOR  IN BROWSE {&browse-name}   = iCor .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 w-livre
ON VALUE-CHANGED OF BROWSE-2 IN FRAME f-cad
DO: 
  IF AVAIL ttItemNf THEN DO:
     //preenche os campos dos formul†rios
     ASSIGN fiNrRemessa:SCREEN-VALUE            = ttItemNf.nfOriginal
            fiNrSeqRemessa:SCREEN-VALUE         = string(ttItemNf.sequencia)
            fiCodReferRemessa:SCREEN-VALUE      = ttItemNf.codRefer
            fiQtRetRemessa:SCREEN-VALUE         = IF ttItemNf.qtNfSubst <> 0 THEN string(ttItemNf.qtNfSubst )
                                                     ELSE string(ttItemNf.qtFaturada) .
     //evita a alteraá∆o de registros calculados pelo sistema, isto Ç, que n∆o foram incluidos manualmente 
     IF ttItemNf.logCalcSist  THEN
        ASSIGN btAlterar:SENSITIVE  =  NO
               btExcluir:SENSITIVE  =  NO.
     ELSE
        ASSIGN btAlterar:SENSITIVE  = YES
               btExcluir:SENSITIVE  = YES.

     //s¢ deixa incluir caso a linha tenha uma diferenáa a ser compensada
     ASSIGN btIncluir:SENSITIVE = ttItemNf.qtExcedente > 0 .
     
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAlterar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAlterar w-livre
ON CHOOSE OF btAlterar IN FRAME f-cad /* Alterar */
DO:
  ASSIGN btConfirmar:SENSITIVE   = YES
         btCanc:SENSITIVE        = YES
         btAlterar:SENSITIVE     = NO
         btExcluir:SENSITIVE     = NO
         btIncluir:SENSITIVE     = NO
         fiQtRetRemessa:SENSITIVE = YES.
         .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCanc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCanc w-livre
ON CHOOSE OF btCanc IN FRAME f-cad /* Cancelar */
DO:
  ASSIGN btConfirmar:SENSITIVE   = NO
         btCanc:SENSITIVE        = NO
         btAlterar:SENSITIVE     = YES
         btExcluir:SENSITIVE     = YES
         btIncluir:SENSITIVE     = YES
         fiQtRetRemessa:SENSITIVE = NO
         .
  APPLY 'value-changed' TO BROWSE browse-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btConfirmar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btConfirmar w-livre
ON CHOOSE OF btConfirmar IN FRAME f-cad /* Confirmar */
DO:

  IF AVAIL ttItemNf THEN DO:
     ASSIGN qtReCompor = ttItemNf.qtFaturada - decimal(fiQtRetRemessa:SCREEN-VALUE).
     FIND FIRST bf USE-INDEX ind-id NO-ERROR.
     IF AVAIL bf THEN DO:
        ASSIGN bf.qtExcedente = bf.qtExcedente + qtReCompor
               bf.qtFaturada = bf.qtFaturada   + qtReCompor.
     END.
     ASSIGN ttItemNf.qtFaturada = decimal(fiQtRetRemessa:SCREEN-VALUE).
     RUN calcSaldoRet.
     {&open-query-browse-2}
  END.
  ASSIGN 
         btConfirmar:SENSITIVE  = NO
         btCanc:SENSITIVE       = NO
         btAlterar:SENSITIVE    = YES
         btExcluir:SENSITIVE    = YES
         fiQtRetRemessa:SENSITIVE = NO 
         .
  APPLY 'value-changed' TO BROWSE browse-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExcluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExcluir w-livre
ON CHOOSE OF btExcluir IN FRAME f-cad /* Excluir */
DO:
  IF AVAIL ttItemNF THEN DO:
     ASSIGN qtRecompor = ttItemNf.qtFaturada.
     FIND FIRST bf USE-INDEX ind-id NO-ERROR.
     IF AVAIL bf THEN DO:
        ASSIGN bf.qtFaturada  = bf.qtFaturada   + qtRecompor
               bf.qtExcedente = bf.qtExcedente  + qtRecompor
               .
     END.
     RUN sincrTtFiltro('excluir',ttItemNf.pedidoLisaId,ttItemNf.id).
     DELETE ttITemNf.                         
  END.
  RUN calcSaldoRet.
  {&open-query-browse-2}
  apply 'value-changed' TO BROWSE BROWSE-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btIncluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btIncluir w-livre
ON CHOOSE OF btIncluir IN FRAME f-cad /* Incluir */
DO:
  IF AVAIL ttItemNF THEN DO:
     RUN lisa/lisa0101b.w(ttitemNf.id,
                          TABLE ttItemNf,
                          TABLE ttFiltro,
                       OUTPUT ROWIDSaldoTerc ,
                       OUTPUT qtRem
                       ).
    
     IF rowidSaldoTerc <> ? THEN DO:
        RUN criarRegistro(
                       ttItemNf.pedidoLisaId,
                       ttItemNf.itemPedidoLisaId,
                       ttItemnf.itCodigo,
                       ttItemNf.retornoLisaId,
                       ttitemNf.nfRetorno,
                       rowidSaldoTerc,
                       qtRem
                       ). 

     
        IF RETURN-VALUE = 'nok' THEN
           MESSAGE 'Erro ao Criar o Registro' SKIP
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
         ELSE DO:
            ASSIGN ttItemNf.qtFaturada  = ttItemNf.qtFaturada  - qtRem
                   ttItemNf.qtExcedente = ttItemNf.qtExcedente - qtRem .
            RUN calcSaldoRet.
        END.
     END.   
  END.
  {&open-query-browse-2}
  APPLY 'value-changed' TO BROWSE BROWSE-2 .

END.

/*ttITemNf.id              
  ttItemNf.pedidoLisaId    
  ttItemNf.itemPedidoLisaId
  ttItemNf.nfOriginal      
  ttItemNf.itCodigo        
  ttItemNf.codRefer        
  ttItemNf.sequencia       
  ttItemNf.retornoLisaId   
  ttItemNf.nfRetorno       
  ttItemNf.codReferRet     
  ttItemNf.logCalcSist     */

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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

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
       RUN set-position IN h_p-exihel ( 1.13 , 97.72 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiNrRetorno:HANDLE IN FRAME f-cad , 'BEFORE':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcSaldoRet w-livre 
PROCEDURE calcSaldoRet :
DEFINE VARIABLE dTotal AS DECIMAL     NO-UNDO.

    FOR EACH ttItemNF
        WHERE ttItemNF.nfRetorno = pNotaRet
        AND   ttItemNf.itCodigo  = pItCodigo
        AND   ttItemNF.codRefer  = pCodRefer,
        FIRST ttFiltro WHERE ttFiltro.agrup_id = ttItemNf.pedidoLisaId
        AND ttFiltro.id = ttItemNf.id .
        

        ASSIGN dTotal = dTotal + ttItemNf.qtExcedente.
             

    END.
    ASSIGN fiSaldo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dTotal).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE criarRegistro w-livre 
PROCEDURE criarRegistro :
DEFINE INPUT  PARAMETER pPedidoLisaId     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pItemPedidoLisaId AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRetornoId        AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNFRetorno        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRowidSaldoTerc   AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER pQtRem            AS DECIMAL     NO-UNDO.
DEFINE BUFFER bf FOR ttItemNF.
DEFINE VARIABLE iProximo AS INTEGER     NO-UNDO.

FIND LAST bf
    USE-INDEX ind-id NO-ERROR.

IF AVAIL bf THEN
   ASSIGN iProximo = bf.id + 1.

FIND saldo-terc NO-LOCK
    WHERE ROWID(saldo-terc) = pRowidSaldoTerc
    NO-ERROR.
IF NOT AVAIL saldo-terc THEN DO:
   MESSAGE 'saldo terc n∆o encontrado'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN 'nok'.
END.

CREATE bf.
ASSIGN 
bf.id               = iProximo
bf.pedidoLisaId     = pPedidoLisaId
bf.itemPedidoLisaId = pItemPedidoLisaId
bf.nfOriginal       = saldo-terc.nro-docto
bf.itCodigo         = pItCodigo
bf.codRefer         = saldo-terc.cod-refer
bf.sequencia        = saldo-terc.sequencia
bf.retornoLisaId    = pRetornoId
bf.nfRetorno        = pNfRetorno
bf.codReferRet      = saldo-terc.cod-refer
bf.qtFaturada       = pQtRem
bf.logCalcSist      = NO
bf.codReferOri      = fiCodReferRetorno:SCREEN-VALUE IN FRAME {&FRAME-NAME}
.

/*
MESSAGE 
    bf.id               SKIP
    bf.pedidoLisaId     SKIP
    bf.itemPedidoLisaId SKIP
    bf.nfOriginal       SKIP
    bf.itCodigo         SKIP
    bf.codRefer         SKIP
    bf.sequencia        SKIP
    bf.retornoLisaId    SKIP
    bf.nfRetorno        SKIP
    bf.codReferRet      SKIP
    bf.qtFaturada       SKIP
    bf.logCalcSist      SKIP

    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/





RUN sincrTTFiltro('incluir',pPedidoLisaId,iProximo).
RETURN 'ok'.

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
  DISPLAY fiNrRetorno fiCodReferRetorno fiQtRetorno fiSaldo fiItCodigo 
          fiDescItem fiNrRemessa fiNrSeqRemessa fiCodReferRemessa fiQtRetRemessa 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button RECT-106 RECT-109 RECT-107 RECT-110 fiNrRetorno 
         fiCodReferRetorno fiQtRetorno fiSaldo fiItCodigo fiDescItem BROWSE-2 
         btAlterar fiNrRemessa fiNrSeqRemessa fiCodReferRemessa btIncluir 
         btExcluir 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportarttItemNF w-livre 
PROCEDURE exportarttItemNF :
DEFINE INPUT  PARAMETER pNomeArquivo AS CHARACTER   NO-UNDO.
DEFINE BUFFER bf FOR ttItemNF.
OUTPUT TO VALUE(pNomeArquivo).
  FOR EACH bf
   // WHERE lookup(string(ttItemNF.id),cListaIds  ) > 0
   .
   EXPORT DELIMITER "|" bf.
  END.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE guardarDados w-livre 
PROCEDURE guardarDados :
/*------------------------------------------------------------------------------
  guarda os dados da tabela ttItemNF para no caso de desistencia voltar os dados
------------------------------------------------------------------------------*/
    FOR EACH ttItemNF:
    
        CREATE ttItemNfOri.
        BUFFER-COPY ttItemNf TO ttItemNfOri.
    
    END.


    FOR EACH ttFiltro.
        CREATE ttFiltroOri.
        BUFFER-COPY ttFiltro TO ttFiltroOri.

    END.

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
  RETURN NO-APPLY.

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
     IF dec(fiSaldo:SCREEN-VALUE IN FRAME {&FRAME-NAME}) > 0 THEN DO:
        MESSAGE "N∆o Ç poss°vel Salvar as alteraá‰es sem que o saldo a retornar esteja zerado." SKIP
            "Deseja sair sem salvar?" UPDATE l AS LOGICAL
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO.
        IF l THEN DO:
           //Limpa a tabela e coloca os valores originais tando para dos dados quanto para os filtros
           EMPTY TEMP-TABLE  ttItemNF.
           FOR EACH ttItemNFOri:
               CREATE ttITemNf.
               BUFFER-COPY ttItemNfOri TO ttItemNf.
           END.                       
           EMPTY TEMP-TABLE ttFiltro.
           FOR EACH ttFiltroOri:
               CREATE ttFiltro.
               BUFFER-COPY ttFiltroOri TO ttFiltro.
           END.                       


           APPLY "CLOSE":U TO THIS-PROCEDURE.
           RETURN. 
        END.      
     END.  
     ELSE DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN. 

     END.


  
  
  
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

  {utp/ut9000.i "lisa0101a" "9.99.99.999"}
  FIND FIRST ttItemNf 
      WHERE int(ttItemNf.nfRetorno) = int(pNotaRet)
      AND   ttItemNf.itCodigo   = pItCodigo
      AND   ttItemNf.codRefer   = pCodRefer NO-ERROR.
  IF AVAIL ttITemNf THEN
     RUN sincrTtFiltro('incluir',ttItemNf.pedidoLisaId,pId).

  /*MESSAGE 'nota rem.'   pNotaRet          SKIP
          'prod:'       pItCodigo         SKIP
          'Refer:'      pCodRefer         SKIP
          'ttITemNF:'   AVAIL ttItemNF    SKIP
          'pID:'        pId
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  RUN guardarDados.
  RUN preencherCampos.
  
  /* Code placed here will execute AFTER standard behavior.    */
  run pi-after-initialize.
  RUN calcSaldoRet.
  //{&open-query-browse-2}
  RUN exportarTtItemNf('c:\temp\tta.txt').
  APPLY 'value-changed' TO BROWSE browse-2 .
  ASSIGN btAlterar:SENSITIVE IN FRAME {&FRAME-NAME} = browse-2:query:NUM-RESULTS > 0 .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preencherCampos w-livre 
PROCEDURE preencherCampos :
ASSIGN fiNrRetorno:SCREEN-VALUE       IN FRAME {&FRAME-NAME}   = pNotaRet
       fiCodReferRetorno:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = pCodRefer
       fiItCodigo:SCREEN-VALUE        IN FRAME {&FRAME-NAME}   = pItCodigo
       fiQtRetorno:SCREEN-VALUE       IN FRAME {&FRAME-NAME}   = STRING(pQuant)
       .
FIND ITEM NO-LOCK
    WHERE ITEM.it-codigo = pItCodigo
    NO-ERROR.
IF AVAIL ITEM THEN
   ASSIGN fiDescitem:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  ITEM.DESC-item.


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
  {src/adm/template/snd-list.i "ttItemNF"}
  {src/adm/template/snd-list.i "ttFiltro"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sincrTtFiltro w-livre 
PROCEDURE sincrTtFiltro :
DEFINE INPUT  PARAMETER pAcao         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pPedidoLisaId AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pId           AS INTEGER     NO-UNDO.

CASE pAcao:
    WHEN 'incluir' THEN DO:
        FIND ttFiltro
            WHERE ttFiltro.id = pId NO-ERROR.
        IF NOT AVAIL ttFiltro THEN DO:
           CREATE ttFiltro.
           ASSIGN  ttFiltro.agrup_id = pPedidoLisaId
                   ttFiltro.id       = pId. 
        END.
        

    END.
    WHEN 'excluir' THEN DO:
        FIND ttFiltro
            WHERE  ttFiltro.agrup_id = pPedidoLisaId
            AND    ttFiltro.id = pId NO-ERROR.
        IF AVAIL ttFiltro THEN
           DELETE ttFiltro.
    END.
END CASE.

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

