&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-relat 
/*:T*******************************************************************************
** Copyright TOTVS S.A. (2009)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da TOTVS, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/*{include/i-prgvrs.i EFGL302AC 5.06.00.001}*/

/* Chamada a include do gerenciador de licenáas. Necessario alterar os parametros */
/*                                                                                */
/* <programa>:  Informar qual o nome do programa.                                 */
/* <m¢dulo>:  Informar qual o m¢dulo a qual o programa pertence.                  */

/* &IF "{&EMSFND_VERSION}" >= "1.00" &THEN               */
/*     {include/i-license-manager.i <programa> <m¢dulo>} */
/* &ENDIF                                                */

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/*:T Preprocessadores do Template de Relat¢rio                            */
/*:T Obs: Retirar o valor do preprocessador para as p†ginas que n∆o existirem  */

/*&GLOBAL-DEFINE PGSEL */
  
/* Parameters Definitions ---                                           */

/* Temporary Table Definitions ---                                      */
/* Local Variable Definitions ---                                       */
define new global shared var v_cod_empres_usuar as char format 'x(3)' no-undo.

DEFINE VARIABLE h_acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE cSessao         AS CHARACTER   NO-UNDO FORMAT 'x(5000)'.
DEFINE VARIABLE cArquivoTexto   AS CHARACTER   NO-UNDO FORMAT 'x(200)' .
DEFINE VARIABLE cDatas          AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE TEMP-TABLE ttLancamentos
     /*FIELD id               AS INT*/
     FIELD data             AS DATE
     FIELD contaContabil    AS CHAR FORMAT 'x(50)'
     FIELD centroCusto      AS CHAR FORMAT 'x(20)'
     FIELD lote             AS INT
     FIELD descLote         AS CHAR FORMAT 'x(20)'
     FIELD lanc             AS INT
     FIELD seq              AS INT
     FIELD origem           AS CHAR
     FIELD tipoMov          AS CHAR
     FIELD historico        AS CHAR FORMAT 'x(400)'
     FIELD valor            AS DEC
     FIELD codEstabel       AS CHAR
     FIELD unidNegocio      AS CHAR
     FIELD PCContabil       AS CHAR
     FIELD PCCentroCusto    AS CHAR
     FIELD contaContabilExt AS CHAR
     FIELD centroCustoExt   AS CHAR
     FIELD rRowid           AS CHAR FORMAT 'x(50)'
     FIELD cenario          AS CHAR FORMAT 'x(20)'
     FIELD finalidade       AS CHAR FORMAT 'x(20)'
     FIELD observacao       AS CHAR FORMAT 'x(300)'
     FIELD sequencias       AS CHAR FORMAT 'x(300)'
     FIELD grupoEstoque     AS CHAR
     INDEX pri AS PRIMARY
     centrocusto
     contacontabil
     data
     INDEX Lot 
     lote
     lanc
     contaContabil
     centroCusto
     grupoEstoque.

DEFINE TEMP-TABLE ttEstoque
    /*FIELD id AS INT*/
    FIELD codEstabel AS CHAR
    FIELD grupoEst   AS CHAR
    FIELD narrativa  AS CHAR FORMAT 'x(500)'
    FIELD valor      AS DEC
    FIELD codUsuario AS CHAR
    FIELD codFornec  AS INT
    FIELD nomeFornec AS CHAR FORMAT 'x(100)'
    FIELD nrDocto    AS CHAR 
    FIELD naturOper  AS CHAR
    FIELD serie      AS CHAR
    FIELD itCodigo   AS CHAR FORMAT 'x(20)'
    FIELD rRowidLanc AS CHAR FORMAT 'x(50)'
    FIELD ctCodigo   AS CHAR FORMAT 'x(20)'
    FIELD scCodigo   AS CHAR FORMAT 'x(20)'
    FIELD tipoMov    AS INT
    INDEX indrowidLanc AS PRIMARY 
    rRowidLanc .

DEFINE TEMP-TABLE ttSaldoConta
    FIELD PCContabil        LIKE ttLancamentos.PCContabil
    FIELD contaContabil     LIKE ttLancamentos.contaContabil
    FIELD finalidade        LIKE ttLancamentos.finalidade
    FIELD cenario           LIKE ttLancamentos.cenario
    FIELD codEstabel        LIKE ttLancamentos.codEstabel
    FIELD UnidNegocio       LIKE ttLancamentos.UnidNegocio
    FIELD data              LIKE ttLancamentos.data
    FIELD valor             LIKE ttLancamentos.valor.

DEFINE TEMP-TABLE ttSaldoContaCC
    FIELD PCContabil        LIKE ttLancamentos.PCContabil
    FIELD contaContabil     LIKE ttLancamentos.contaContabil
    FIELD PCCentroCusto     LIKE ttLancamentos.PCCentroCusto
    FIELD centroCusto       LIKE ttLancamentos.centroCusto
    FIELD finalidade        LIKE ttLancamentos.finalidade
    FIELD cenario           LIKE ttLancamentos.cenario
    FIELD codEstabel        LIKE ttLancamentos.codEstabel
    FIELD UnidNegocio       LIKE ttLancamentos.UnidNegocio
    FIELD data              LIKE ttLancamentos.data
    FIELD valor             LIKE ttLancamentos.valor.


DEFINE TEMP-TABLE ttMatrizExterna
    FIELD contaContabilExterna  LIKE ttLancamentos.contaContabil
    FIELD centroCustoExterno    LIKE ttLancamentos.centroCusto .


DEFINE VARIABLE cListaEmpresas AS CHARACTER   NO-UNDO FORMAT 'x(500)'.

DEFINE VARIABLE lContinua           AS LOGICAL     NO-UNDO .

DEFINE TEMP-TABLE ttItens
    FIELD itCodigo  AS CHAR FORMAT 'x(50)' LABEL "Item".

DEFINE TEMP-TABLE ttConta
    FIELD conta   AS CHAR FORMAT 'x(50)' LABEL "Conta Contabil"
    FIELD cCusto  AS CHAR FORMAT 'x(50)' LABEL "Centro de Custo"
    FIELD codEstabel AS CHAR LABEL "Estab.".

DEFINE TEMP-TABLE ttArquivo
       FIELD tipo           AS CHAR FORMAT 'x(20)'
       FIELD planilha       AS CHAR FORMAT 'x(200)'.


/* variaveis excel */
DEFINE VARIABLE chExcelApplication      AS COMPONENT-HANDLE.
DEFINE VARIABLE chWorkBook              AS COM-HANDLE.
DEFINE VARIABLE chWorkSheet             AS COM-HANDLE.

/*tabelas temporarias para calculo do saldo da api  prgfin/fgl/fgl905za.py */
def temp-table tt_erro_relatorio_razao no-undo
    field tta_num_lote_ctbl                as integer format ">>>,>>>,>>9" initial 1 label "Lote Contabil" column-label "Lote Contòbil"
    field tta_num_lancto_ctbl              as integer format ">>,>>>,>>9" initial 10 label "Lanáamento Contòbil" column-label "Lan?amento Contòbil"
    field tta_num_seq_lancto_ctbl          as integer format ">>>>9" initial 0 label "Sequencia Lancto" column-label "Sequencia Lancto"
    field ttv_des_ajuda                    as character format "x(50)" label "Ajuda" column-label "Ajuda"
    index tt_                              is primary
          tta_num_lote_ctbl                ascending
          tta_num_lancto_ctbl              ascending
          tta_num_seq_lancto_ctbl          ascending.

def temp-table tt_erro_relatorio_razao_acum no-undo
    field tta_num_lote_ctbl                as integer format ">>>,>>>,>>9" initial 1 label "Lote Contabil" column-label "Lote Contabil"
    field tta_num_lancto_ctbl              as integer format ">>,>>>,>>9" initial 10 label "Lanáamento Contabil" column-label "Lanáamento Contabil"
    field tta_num_seq_lancto_ctbl          as integer format ">>>>9" initial 0 label "Sequencia Lancto" column-label "Sequencia Lancto"
    field ttv_des_ajuda                    as character format "x(50)" label "Ajuda" column-label "Ajuda"
    FIELD contaContabil                    AS CHAR FORMAT "x(20)"
    FIELD centroCusto                      AS CHAR FORMAT "x(10)"
    index tt_                              is primary
          tta_num_lote_ctbl                ascending
          tta_num_lancto_ctbl              ascending
          tta_num_seq_lancto_ctbl          ascending.



def new shared temp-table tt_sdo_ctbl        
    field tta_dat_sdo_ctbl                 as date format "99/99/9999" initial ? label "Data Saldo Contabil" column-label "Data Saldo Contabil"
    field tta_val_sdo_ctbl_db              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto Debito" column-label "Movto Debito"
    field tta_val_sdo_ctbl_cr              as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Movto Credito" column-label "Movto Credito"
    field tta_val_sdo_ctbl_fim             as decimal format "->>,>>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo Contabil Final" column-label "Saldo Contabil Final"
    index tt_id                            is primary unique
          tta_dat_sdo_ctbl                 ascending.
          
def new shared temp-table tt_estab_unid_negoc_select         like estab_unid_negoc
    index tt_estab_unid_negoc_select_id    is primary unique
          cod_estab                        ascending
          cod_unid_negoc                   ascending.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-relat
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-pg-sel

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 IMAGE-2 RECT-13 IMAGE-3 IMAGE-4 ~
IMAGE-5 IMAGE-6 RECT-15 cb-empresa cb-excel tg-sdo-apuracao-resultado ~
data-ini data-fim ccusto-ini ccusto-fim contacontabil-ini contacontabil-fim 
&Scoped-Define DISPLAYED-OBJECTS cb-empresa cb-excel ~
tg-sdo-apuracao-resultado data-ini data-fim ccusto-ini ccusto-fim ~
contacontabil-ini contacontabil-fim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-relat AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cb-empresa AS CHARACTER FORMAT "X(3)":U INITIAL "0" 
     LABEL "Empresa" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 52.86 BY 1 NO-UNDO.

DEFINE VARIABLE cb-excel AS CHARACTER FORMAT "X(3)":U INITIAL "1" 
     LABEL "Geraá∆o Excel" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Contabilidade","1"
     DROP-DOWN-LIST
     SIZE 52.86 BY 1 NO-UNDO.

DEFINE VARIABLE ccusto-fim AS CHARACTER FORMAT "X(11)":U INITIAL "ZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE ccusto-ini AS CHARACTER FORMAT "X(11)":U 
     LABEL "Centro Custo" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE contacontabil-fim AS CHARACTER FORMAT "x(20)" INITIAL "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .88.

DEFINE VARIABLE contacontabil-ini AS CHARACTER FORMAT "x(20)" 
     LABEL "Conta Cont†bil" 
     VIEW-AS FILL-IN 
     SIZE 21.14 BY .88.

DEFINE VARIABLE data-fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE data-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/00 
     LABEL "Data inicial" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-3
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-4
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-5
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-6
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  
     SIZE 72 BY 4.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE   GROUP-BOX  
     SIZE 72 BY 4.79 TOOLTIP "ParÉmetros".

DEFINE VARIABLE tg-sdo-apuracao-resultado AS LOGICAL INITIAL yes 
     LABEL "Considera Saldo Apuraá∆o de Resultados ?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .79 NO-UNDO.

DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Fechar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-executar 
     LABEL "Executar" 
     SIZE 10 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 0    
     SIZE 78.86 BY .13
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 11.38 TOOLTIP "Seleá∆o"
     FGCOLOR 0 .

DEFINE RECTANGLE rt-folder-left
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 15 .

DEFINE RECTANGLE rt-folder-right
     EDGE-PIXELS 0    
     SIZE .43 BY 11.21
     BGCOLOR 7 .

DEFINE RECTANGLE rt-folder-top
     EDGE-PIXELS 0    
     SIZE 78.86 BY .13
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-relat
     bt-executar AT ROW 12.71 COL 2 HELP
          "Dispara a execuá∆o do relat¢rio"
     bt-cancelar AT ROW 12.71 COL 13 HELP
          "Fechar"
     bt-ajuda AT ROW 12.71 COL 69 HELP
          "Ajuda"
     RECT-1 AT ROW 12.5 COL 1
     RECT-6 AT ROW 12.38 COL 1.14
     rt-folder-top AT ROW 1.21 COL 1.14
     rt-folder-right AT ROW 1.29 COL 79.43
     rt-folder-left AT ROW 1.21 COL 1.14
     rt-folder AT ROW 1.13 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.43 BY 15
         DEFAULT-BUTTON bt-executar WIDGET-ID 100.

DEFINE FRAME f-pg-sel
     cb-empresa AT ROW 1.5 COL 17 COLON-ALIGNED WIDGET-ID 36
     cb-excel AT ROW 2.67 COL 17 COLON-ALIGNED WIDGET-ID 44
     tg-sdo-apuracao-resultado AT ROW 3.88 COL 19 WIDGET-ID 40
     data-ini AT ROW 6.58 COL 17 COLON-ALIGNED
     data-fim AT ROW 6.58 COL 49.14 COLON-ALIGNED NO-LABEL
     ccusto-ini AT ROW 7.63 COL 17.14 COLON-ALIGNED WIDGET-ID 6
     ccusto-fim AT ROW 7.63 COL 49.14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     contacontabil-ini AT ROW 8.67 COL 17 COLON-ALIGNED HELP
          "C¢digo Conta Cont†bil" WIDGET-ID 20
     contacontabil-fim AT ROW 8.67 COL 49.14 COLON-ALIGNED HELP
          "C¢digo Conta Cont†bil" NO-LABEL WIDGET-ID 24
     IMAGE-1 AT ROW 6.58 COL 42
     IMAGE-2 AT ROW 6.58 COL 47.14
     RECT-13 AT ROW 6.25 COL 4 WIDGET-ID 2
     IMAGE-3 AT ROW 7.63 COL 42.14 WIDGET-ID 8
     IMAGE-4 AT ROW 7.63 COL 47.14 WIDGET-ID 10
     IMAGE-5 AT ROW 8.71 COL 42 WIDGET-ID 16
     IMAGE-6 AT ROW 8.71 COL 47.14 WIDGET-ID 18
     RECT-15 AT ROW 1.21 COL 4 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.86 ROW 1.38
         SIZE 76.86 BY 10.88
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-relat
   Allow: Basic,Browse,DB-Fields,Window,Query
   Add Fields to: Neither
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-relat ASSIGN
         HIDDEN             = YES
         TITLE              = "Raz∆o por Centro de Custo Excel - EFGL302AC"
         HEIGHT             = 12.75
         WIDTH              = 79
         MAX-HEIGHT         = 33.29
         MAX-WIDTH          = 273.14
         VIRTUAL-HEIGHT     = 33.29
         VIRTUAL-WIDTH      = 273.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-relat 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-relat.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-relat
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-pg-sel
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME f-relat
                                                                        */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-left IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-right IN FRAME f-relat
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE rt-folder-top IN FRAME f-relat
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
THEN w-relat:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-pg-sel
/* Query rebuild information for FRAME f-pg-sel
     _Query            is NOT OPENED
*/  /* FRAME f-pg-sel */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON END-ERROR OF w-relat /* Raz∆o por Centro de Custo Excel - EFGL302AC */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-relat w-relat
ON WINDOW-CLOSE OF w-relat /* Raz∆o por Centro de Custo Excel - EFGL302AC */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-relat
&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-relat
ON CHOOSE OF bt-ajuda IN FRAME f-relat /* Ajuda */
DO:
   {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-relat
ON CHOOSE OF bt-cancelar IN FRAME f-relat /* Fechar */
DO:
   apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-executar w-relat
ON CHOOSE OF bt-executar IN FRAME f-relat /* Executar */
DO:
   do  on error undo, return no-apply:
       run utp/ut-acomp.p persistent set h_acomp.
       RUN piValidacoes.
       run pi-executar.
   end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME f-pg-sel
&Scoped-define SELF-NAME data-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL data-ini w-relat
ON ENTRY OF data-ini IN FRAME f-pg-sel /* Data inicial */
DO:
/*   ASSIGN DATA-INI:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY - 30) */
/*          DATA-FIM:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-relat 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/*{utp/ut9000.i "CI0001" "1.00.00.000"}*/

/*:T inicializaá‰es do template de relat¢rio */
/*{include/i-rpini.i}*/

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/*{include/i-rplbl.i}*/

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO  ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
  RUN piBuscaEmpresas(OUTPUT cListaEmpresas).
  IF cListaEmpresas <> "" THEN
     ASSIGN cb-empresa:LIST-ITEM-PAIRS IN FRAME f-pg-sel = cListaEmpresas
            cb-empresa   = v_cod_empres_usuar .
  ASSIGN data-ini = TODAY - 30
         data-fim = TODAY .
  RUN enable_UI.
  IF  NOT THIS-PROCEDURE:PERSISTENT THEN
       WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-relat  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-relat  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-relat  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-relat)
  THEN DELETE WIDGET w-relat.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-relat  _DEFAULT-ENABLE
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
  ENABLE bt-executar bt-cancelar bt-ajuda 
      WITH FRAME f-relat IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-relat}
  DISPLAY cb-empresa cb-excel tg-sdo-apuracao-resultado data-ini data-fim 
          ccusto-ini ccusto-fim contacontabil-ini contacontabil-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  ENABLE IMAGE-1 IMAGE-2 RECT-13 IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 RECT-15 
         cb-empresa cb-excel tg-sdo-apuracao-resultado data-ini data-fim 
         ccusto-ini ccusto-fim contacontabil-ini contacontabil-fim 
      WITH FRAME f-pg-sel IN WINDOW w-relat.
  {&OPEN-BROWSERS-IN-QUERY-f-pg-sel}
  VIEW w-relat.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-relat 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-relat 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  


  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar w-relat 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cArquivoConf  AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cArquivoExcel AS CHARACTER   NO-UNDO FORMAT 'x(200)'.

/*RUN piRetornaUltDiaMes(DATE(data-ini:SCREEN-VALUE IN FRAME f-pg-sel), DATE(data-fim:SCREEN-VALUE IN FRAME f-pg-sel), OUTPUT cDatas ).*/


do on error undo, return error on stop  undo, return error:
   IF c-seg-usuario = 'super' THEN
       ASSIGN lContinua = YES.
    ELSE
       ASSIGN lContinua = NO.
   /* ASSIGN lContinua = YES.*/
   
   
    IF cb-excel:SCREEN-VALUE IN FRAME f-pg-sel = '1' THEN
       ASSIGN cArquivoTexto = SESSION:TEMP-DIRECTORY + 'razaoCentroCusto.txt'.
    ELSE
       ASSIGN cArquivoTexto = SESSION:TEMP-DIRECTORY + 'razaoCentroCustoFilial.txt'. 
    SESSION:SET-WAIT-STATE("general":U).
    RUN piLimpaVariaveis.
    run pi-inicializar in h_acomp(input "ACOMPANHAMENTO").
    run pi-habilita-cancela in h_acomp.
    RUN piBuscaDados.
    run pi-acompanhar in h_acomp(input "Buscando Saldos de Conta e Centro de Custo").
    RUN piCalculaSaldos.
    run pi-acompanhar in h_acomp(input "Buscando Conta,Centro Custo e Fornecedor").
    RUN piExportaDados.
    RUN pi-finalizar IN h_acomp.

    FIND FIRST ttItens NO-ERROR.
    IF AVAIL ttItens THEN
       MESSAGE 'Existem Itens sem cadastro.' SKIP
               'Verifique o arquivo:itensSemCadastro.txt no diret¢rio ' SESSION:TEMP-DIRECTORY
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

    OUTPUT TO value(session:TEMP-DIRECTORY + 'itensSemCadastro.txt').
    FOR EACH ttItens:
        DISP ttItens.
    END.
    OUTPUT CLOSE.

    FIND FIRST ttConta NO-ERROR.
    IF AVAIL ttConta THEN
       MESSAGE 'Existem contas sem Matriz de Traduá∆o Cadastrada para contas externas.' SKIP
           'Verifique o arquivo: contasSemMatriz.txt no diret¢rio ' SESSION:TEMP-DIRECTORY
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     OUTPUT TO value(session:TEMP-DIRECTORY + 'contasSemMatriz.txt').
     FOR EACH ttConta:
        DISP ttConta WITH WIDTH 550.
     END.
     OUTPUT CLOSE.  

     FIND FIRST tt_erro_relatorio_razao_acum NO-LOCK NO-ERROR.
     IF AVAIL tt_erro_relatorio_razao_acum THEN
        MESSAGE 'Existem inconscistàncias de saldos.' SKIP
           'Verifique o arquivo: tt_erro_relatorio_razao_acum.txt no diret¢rio:' SESSION:TEMP-DIRECTORY
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

     OUTPUT TO value(session:TEMP-DIRECTORY + 'tt_erro_relatorio_razao_acum.txt').
     FOR EACH tt_erro_relatorio_razao_acum:
        DISP tt_erro_relatorio_razao_acum.
     END.
    OUTPUT CLOSE.
    ASSIGN cArquivoConf = IF SEARCH('fgl/efgl302ac.ini') <> ? THEN  search('fgl/efgl302ac.ini') ELSE ''.
    IF cArquivoConf <> '' THEN DO:
       INPUT FROM VALUE(cArquivoConf).
       REPEAT:
         CREATE ttArquivo.
         IMPORT DELIMITER "=" ttArquivo.
       END.
       INPUT CLOSE.                    
       IF cb-excel:SCREEN-VALUE IN FRAME f-pg-sel = '1' THEN DO:
          FIND FIRST ttArquivo
              WHERE ttArquivo.tipo="contabilidade" NO-LOCK NO-ERROR.
          IF AVAIL ttArquivo THEN
             ASSIGN cArquivoExcel = ttArquivo.planilha.
       END.
       ELSE DO:
          FIND FIRST ttArquivo
              WHERE ttArquivo.tipo="filial" NO-LOCK NO-ERROR.
          IF AVAIL ttArquivo THEN
             ASSIGN cArquivoExcel = ttArquivo.planilha.

       END.
       
    END. 
    ELSE
       ASSIGN cArquivoExcel = 'T:/ESPECIFICOS/excel/' + IF cb-excel:SCREEN-VALUE IN FRAME f-pg-sel = '1' THEN 'razao.xlsx' ELSE 'razaofilial.xlsx'.

    /*tratamento para quando o arquivo de configuraáao estiver em branco */
    IF cArquivoExcel = '' THEN
       ASSIGN cArquivoExcel = 'T:/ESPECIFICOS/excel/' + IF cb-excel:SCREEN-VALUE IN FRAME f-pg-sel = '1' THEN 'razao.xlsx' ELSE 'razaofilial.xlsx'.

     /*CREATE "Excel.Application" chExcelApplication.
     /*chExcelApplication:RUN('atualizarDados ,' + cArquivoTexto).*/
    chExcelApplication:RUN('atualizarDados').
     ASSIGN chExcelApplication:VISIBLE = yes.
            chWorkbook  = chExcelApplication:Workbooks:OPEN(cArquivoExcel).
     RELEASE OBJECT chWorkBook.
     RELEASE OBJECT chExcelApplication.*/

    DOS SILENT value( "START excel /t " + cArquivoExcel).
    SESSION:SET-WAIT-STATE("":U).
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-troca-pagina w-relat 
PROCEDURE pi-troca-pagina :
/*:T------------------------------------------------------------------------------
  Purpose: Gerencia a Troca de P†gina (folder)   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*{include/i-rptrp.i}*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaContaContabilExt w-relat 
PROCEDURE piBuscaContaContabilExt :
DEFINE INPUT  PARAMETER pEmpresa            AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pPlanoContasConta   AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER pContaContabil      AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER pCentroCusto        AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER pCodEstabel         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pUnidNegocio        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pPlanoCC            AS CHARACTER   NO-UNDO.

EMPTY TEMP-TABLE ttMatrizExterna.
/*busca a matriz de traduá∆o da empresa*/
FIND FIRST trad_org_ext
     WHERE trad_org_ext.cod_unid_organ  = pEmpresa
     AND   trad_org_ext.cod_tip_unid_organ = '998'
     USE-INDEX trdrgxt_ndrgn
     NO-LOCK NO-ERROR.
IF AVAIL trad_org_ext THEN DO:
   IF lContinua THEN
      MESSAGE 'encontrei a matriz externa:' trad_org_ext.cod_matriz_trad_cta_ext
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
   FIND FIRST matriz_trad_cta_ctbl_ext
       WHERE matriz_trad_cta_ctbl_ext.cod_unid_organ = pEmpresa
       AND   matriz_trad_cta_ctbl_ext.cod_matriz_trad_cta_ext = trad_org_ext.cod_matriz_trad_cta_ext
       AND matriz_trad_cta_ctbl_ext.ind_orig_cta_ctbl_ext = 'outros'  NO-LOCK NO-ERROR.
   IF AVAIL matriz_trad_cta_ctbl_ext THEN DO:
      IF lContinua THEN
         MESSAGE 'posicionei na matriz da conta contabil externa' 
                 'PLANO DE CONTAS:' pPlanoContasConta SKIP 
                 'CONTA CONTABIL:'  pContaContabil    SKIP
                 'CENTRO CUSTO:'    pCentroCusto      SKIP
                 'ESTABEL:'         pCodEstabel       SKIP
                 'UNID.NEG:'        pUnidNegocio 
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
      /*busca a matriz de traduá∆o para o estabelecimento , conta , centro de custo(opcional) do lanáamento*/


      FOR EACH trad_cta_ctbl_ext OF matriz_trad_cta_ctbl_ext
          WHERE trad_cta_ctbl_ext.cod_plano_cta_ctbl        = pPlanoContasConta
          AND   trad_cta_ctbl_ext.cod_cta_ctbl              = pContaContabil
          AND   trad_cta_ctbl_ext.cod_ccusto                = pCentroCusto
          AND   trad_cta_ctbl_ext.cod_estab                 = pCodEstabel
          AND   trad_cta_ctbl_ext.cod_unid_negoc            = pUnidNegocio
          USE-INDEX trdctctb_ctaext
          NO-LOCK.
      
          
         IF lContinua THEN
            MESSAGE 'encontrei a conta e o centro de custo ext' SKIP
                     'conta:' trad_cta_ctbl_ext.cod_cta_ctbl SKIP
                     'conta ext:' trad_cta_ctbl_ext.cod_cta_ctbl_ext SKIP
                     'centro custo:' trad_cta_ctbl_ext.cod_ccusto SKIP
                     'centro custo ext:' trad_cta_ctbl_ext.cod_sub_cta_ctbl_ext
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         CREATE ttMatrizExterna.
         ASSIGN ttMatrizExterna.contaContabilExterna    = trad_cta_ctbl_ext.cod_cta_ctbl_ext
                ttMatrizExterna.CentroCustoExterno      = trad_cta_ctbl_ext.cod_sub_cta_ctbl_ext.
      END.
   END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaDados w-relat 
PROCEDURE piBuscaDados :
/*------------------------------------------------------------------------------
  Buscar os Dados conforme os Parametros    
------------------------------------------------------------------------------*/
DEFINE VARIABLE cOrigem             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i                   AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE cGrupoMov           AS CHARACTER   NO-UNDO INIT "".
DEFINE VARIABLE cEstabMov           AS CHARACTER   NO-UNDO INIT "".
DEFINE VARIABLE cListaGruposLabel   AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cListaGrupos        AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cListaEstabsLabel   AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cListaEstabs        AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE lEstoque            AS LOGICAL     NO-UNDO INIT NO.


/* MESSAGE 'entrei na busca de dados' SKIP                         */
/*          cb-empresa:SCREEN-VALUE IN FRAME f-pg-sel        SKIP  */
/*          DATE(data-ini:SCREEN-VALUE IN FRAME f-pg-sel)    SKIP  */
/*          DATE(data-fim:SCREEN-VALUE IN FRAME f-pg-sel)    SKIP  */
/*          contacontabil-ini:SCREEN-VALUE IN FRAME f-pg-sel SKIP  */
/*          contacontabil-fim:SCREEN-VALUE IN FRAME f-pg-sel SKIP  */
/*          ccusto-ini:SCREEN-VALUE IN FRAME f-pg-sel        SKIP  */
/*          ccusto-fim:SCREEN-VALUE IN FRAME f-pg-sel        SKIP  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                          */


RUN piGeraGruposEstoque(OUTPUT cListaGruposLabel, OUTPUT cListaGrupos). 
RUN piGeraEstabsEstoque(OUTPUT cListaEstabsLabel, OUTPUT cListaEstabs). 
OUTPUT TO value(session:TEMP-DIRECTORY + "profile.txt").
FOR EACH ITEM_lancto_ctbl NO-LOCK
    WHERE ITEM_lancto_ctbl.cod_empresa          = cb-empresa:SCREEN-VALUE IN FRAME f-pg-sel
    AND   ITEM_lancto_ctbl.dat_lancto_ctbl     >= DATE(data-ini:SCREEN-VALUE IN FRAME f-pg-sel)
    AND   ITEM_lancto_ctbl.dat_lancto_ctbl     <= DATE(data-fim:SCREEN-VALUE IN FRAME f-pg-sel)
    AND   ITEM_lancto_ctbl.cod_cta_ctbl        >= contacontabil-ini:SCREEN-VALUE IN FRAME f-pg-sel
    AND   ITEM_lancto_ctbl.cod_cta_ctbl        <= contacontabil-fim:SCREEN-VALUE IN FRAME f-pg-sel
    AND   ITEM_lancto_ctbl.cod_ccusto          >= ccusto-ini:SCREEN-VALUE IN FRAME f-pg-sel
    AND   ITEM_lancto_ctbl.cod_ccusto          <= ccusto-fim:SCREEN-VALUE IN FRAME f-pg-sel
    AND   item_lancto_ctbl.ind_sit_lancto_ctbl  = 'ctbz'
    /*AND   ITEM_lancto_ctbl.cod_ccusto <> ''*/
    USE-INDEX tmlnctcb_data_lancto :
    
   
   /*  verificar se as contas transitorias far„o parte da apuraÁ„o, pois, est„o marcadas como apuraÁ„o.
   FIND FIRST cta_ctbl
        WHERE cta_ctbl.cod_plano_cta_ctbl = ITEM_lancto_ctbl.cod_plano_cta_ctbl
        AND   cta_ctbl.cod_cta_ctbl       = ITEM_lancto_ctbl.cod_cta_ctbl use-index  ctactbl_id NO-LOCK NO-ERROR.
    IF AVAIL cta_ctbl THEN DO:


    END.*/
    EXPORT DELIMITER "|"
           "ITEM_LANCTO_CTBL"
           ITEM_lancto_ctbl.num_lote_ctbl
           ITEM_lancto_ctbl.num_lancto_ctbl        
           ITEM_lancto_ctbl.num_seq_lancto_ctbl
           STRING(TIME,"hh:mm:ss").
    run pi-acompanhar in h_acomp(input STRING(ITEM_lancto_ctbl.dat_lancto_ctbl)).
    IF lContinua THEN
       MESSAGE "Deseja continuar o log?"
       VIEW-AS ALERT-BOX INFO BUTTONS YES-NO-CANCEL UPDATE lContinua.
/*     MESSAGE 'entrei no item_lancto'        */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*                                            */
    FIND FIRST lote_ctbl OF ITEM_lancto_ctbl NO-LOCK NO-ERROR. /*busca o lote do lancamento para saber qual a referencia contabil deve ser buscada no estoque*/
    IF AVAIL lote_ctbl THEN DO: 
       IF SUBSTR(lote_ctbl.des_lote_ctbl,1,2) = "CE" THEN
          ASSIGN lEstoque = YES.
       ELSE 
          ASSIGN lEstoque = NO.
       /*ASSIGN i  = i + 1.*/
       FIND FIRST ttLancamentos
           WHERE ttLancamentos.lote = ITEM_lancto_ctbl.num_lote_ctbl
           AND   ttLancamentos.lanc = ITEM_lancto_ctbl.num_lancto_ctbl
           AND   ttLancamentos.contaContabil = ITEM_Lancto_ctbl.cod_cta_ctbl
           AND   ttLancamentos.centroCusto   = ITEM_lancto_ctbl.cod_ccusto
           AND   ttLancamentos.grupoEstoque  = ENTRY(8,ITEM_lancto_ctbl.des_histor_lancto_ctbl," ")
           USE-INDEX lot NO-ERROR .
       IF NOT AVAIL ttLancamentos THEN DO:
          CREATE ttLancamentos.
          ASSIGN 
              ttLancamentos.data            = ITEM_lancto_ctbl.dat_lancto_ctbl
              ttLancamentos.contaContabil   = ITEM_Lancto_ctbl.cod_cta_ctbl
              ttLancamentos.centroCusto     = ITEM_lancto_ctbl.cod_ccusto
              ttLancamentos.lote            = ITEM_lancto_ctbl.num_lote_ctbl
              ttLancamentos.descLote        = substr(lote_ctbl.des_lote_ctbl,1,10) /*refere-se apenas a referencia contabil, retirando-se o estabelecimento */  
              ttLancamentos.lanc            = ITEM_lancto_ctbl.num_lancto_ctbl
              ttLancamentos.seq             = ITEM_lancto_ctbl.num_seq_lancto_ctbl
              ttLancamentos.origem          = lote_ctbl.cod_modul_dtsul 
              ttLancamentos.tipoMov         = ITEM_lancto_ctbl.ind_natur_lancto_ctbl
              ttLancamentos.historico       = ITEM_lancto_ctbl.des_histor_lancto_ctbl
              ttLancamentos.valor           = ITEM_lancto_ctbl.val_lancto_ctbl
              ttLancamentos.codEstabel      = ITEM_lancto_ctbl.cod_estab
              ttLancamentos.unidNegocio     = ITEM_lancto_ctbl.cod_unid_negoc
              ttLancamentos.PCContabil      = ITEM_lancto_ctbl.cod_plano_cta_ctbl
              ttLancamentos.PCCentroCusto   = ITEM_lancto_ctbl.cod_plano_ccusto
              ttLancamentos.rrowid          = string(ROWID(ITEM_lancto_ctbl))
              ttLancamentos.cenario         = item_lancto_ctbl.cod_cenar_ctbl
              ttLancamentos.observacao      = IF lEstoque = YES THEN 'sequencia:' + string(ttLancamentos.seq) + ' valor:' + string(ITEM_lancto_ctbl.val_lancto_ctbl) ELSE ""
              ttLancamentos.sequencias      = string(ttLancamentos.seq)
              ttLancamentos.grupoEstoque    = ENTRY(8,ttLancamentos.historico," ").
                

          IF lcontinua THEN
             MESSAGE  
                'ITEM_lancto_ctbl.dat_lancto_ctbl'              ITEM_lancto_ctbl.dat_lancto_ctbl       SKIP
                'ITEM_Lancto_ctbl.cod_cta_ctbl'                 ITEM_Lancto_ctbl.cod_cta_ctbl          SKIP
                'ITEM_lancto_ctbl.cod_ccusto'                   ITEM_lancto_ctbl.cod_ccusto            SKIP
                'ITEM_lancto_ctbl.num_lote_ctbl'                ITEM_lancto_ctbl.num_lote_ctbl         SKIP
                'substr(lote_ctbl.des_lote_ctbl,1,10)'          substr(lote_ctbl.des_lote_ctbl,1,10)   SKIP
                'ITEM_lancto_ctbl.num_lancto_ctbl'              ITEM_lancto_ctbl.num_lancto_ctbl       SKIP
                'ITEM_lancto_ctbl.num_seq_lancto_ctbl'          ITEM_lancto_ctbl.num_seq_lancto_ctbl   SKIP
                'lote_ctbl.cod_modul_dtsul'                     lote_ctbl.cod_modul_dtsul              SKIP
                'ITEM_lancto_ctbl.ind_natur_lancto_ctbl'        ITEM_lancto_ctbl.ind_natur_lancto_ctbl SKIP
                'ITEM_lancto_ctbl.des_histor_lancto_ctbl'       ITEM_lancto_ctbl.des_histor_lancto_ctbl SKIP
                'ITEM_lancto_ctbl.val_lancto_ctbl'              ITEM_lancto_ctbl.val_lancto_ctbl       SKIP
                'ITEM_lancto_ctbl.cod_estab'                    ITEM_lancto_ctbl.cod_estab             SKIP
                'ITEM_lancto_ctbl.cod_unid_negoc'               ITEM_lancto_ctbl.cod_unid_negoc        SKIP
                'ITEM_lancto_ctbl.cod_plano_cta_ctbl'           ITEM_lancto_ctbl.cod_plano_cta_ctbl    SKIP
                'ITEM_lancto_ctbl.cod_plano_ccusto'             ITEM_lancto_ctbl.cod_plano_ccusto
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
    
          IF lEstoque = YES THEN DO:
             
             encontraGrupo:                                                                                                        
             REPEAT i = 1 TO NUM-ENTRIES(cListaGruposLabel,"|"):                                                           
               IF LOOKUP(ENTRY(i,cListaGruposLabel,"|"),ITEM_lancto_ctbl.des_histor_lancto_ctbl," ") > 0 THEN DO:        
                 ASSIGN cGrupoMov = ENTRY(i,cListaGrupos,"|").                                                          
                 LEAVE encontraGrupo.                                                                                   
               END.                                                                                                      
             END.                                                                                                          
                                                                                                                        
            encontraEstab:                                                                                                
            REPEAT i = 1 TO NUM-ENTRIES(cListaEstabsLabel,"|"):                                                           
              IF LOOKUP(ENTRY(i,cListaEstabsLabel,"|"),ITEM_lancto_ctbl.des_histor_lancto_ctbl," ") > 0 THEN DO:        
                 ASSIGN cEstabMov = ENTRY(i,cListaEstabs,"|").                                                          
                 LEAVE encontraEstab.                                                                                   
              END.                                                                                                      
            END.                                                                                                          
            IF cGrupoMov <> "" AND cEstabMov <> "" THEN DO:
                /*
               RUN piBuscaContaContabilExt(
                                       INPUT cb-empresa:SCREEN-VALUE IN FRAME f-pg-sel,
                                       INPUT ttLancamentos.PCContabil,
                                       INPUT ttLancamentos.contaContabil,
                                       INPUT ttLancamentos.centroCusto,
                                       INPUT ttLancamentos.codEstabel,
                                       INPUT ttLancamentos.unidNegocio,
                                       INPUT ttLancamentos.PCCentroCusto).
               FOR EACH ttMatrizExterna:                                                                                   
                 RUN piGeraDadosEstoque (INPUT ttLancamentos.rRowid,                                                    
                                         INPUT ttLancamentos.descLote,                                                  
                                         INPUT ttMatrizExterna.contaContabilExterna,                                          
                                         INPUT ttMatrizExterna.centroCustoExterno,                                            
                                         INPUT ttLancamentos.data,                                                      
                                         INPUT ttLancamentos.tipoMov,                                                   
                                         INPUT cGrupoMov,                                                               
                                         INPUT cEstabMov).                                                              
                                                                                                                        
              END.    */

              RUN piGeraDadosEstoque (INPUT ttLancamentos.rRowid,                                                    
                                      INPUT ttLancamentos.descLote,                                                  
                                      INPUT ttLancamentos.contaContabil,                                          
                                      INPUT ttLancamentos.centroCusto,                                            
                                      INPUT ttLancamentos.data,                                                      
                                      INPUT ttLancamentos.tipoMov,                                                   
                                      INPUT cGrupoMov,                                                               
                                      INPUT cEstabMov).      
            END.
          END.
       END.
       ELSE DO: /*caso exista um lancamento para o mesmo lote, conta contabil, centro de custo e codigo do lancamento, soma o valor do lancamento e preenche campo histÛrico e sequencias*/
          IF SUBSTR(lote_ctbl.des_lote_ctbl,1,2) = "CE" /*AND lote_ctbl.cod_modul_dtsul = "CEP"*/ THEN DO:
             ASSIGN  ttLancamentos.valor           = ttLancamentos.valor + ITEM_lancto_ctbl.val_lancto_ctbl
                     ttLancamentos.observacao      = ttLancamentos.observacao + ' / sequencia:' + string(ITEM_lancto_ctbl.num_seq_lancto_ctbl) + ' valor:' + string(ITEM_lancto_ctbl.val_lancto_ctbl)
                     ttLancamentos.sequencias      = ttLancamentos.sequencias + '/' + string(ITEM_lancto_ctbl.num_seq_lancto_ctbl).
          END.
          ELSE DO:
             CREATE ttLancamentos.
             ASSIGN 
             ttLancamentos.data            = ITEM_lancto_ctbl.dat_lancto_ctbl
             ttLancamentos.contaContabil   = ITEM_Lancto_ctbl.cod_cta_ctbl
             ttLancamentos.centroCusto     = ITEM_lancto_ctbl.cod_ccusto
             ttLancamentos.lote            = ITEM_lancto_ctbl.num_lote_ctbl
             ttLancamentos.descLote        = substr(lote_ctbl.des_lote_ctbl,1,10) /*refere-se apenas a referencia contabil, retirando-se o estabelecimento */  
             ttLancamentos.lanc            = ITEM_lancto_ctbl.num_lancto_ctbl
             ttLancamentos.seq             = ITEM_lancto_ctbl.num_seq_lancto_ctbl
             ttLancamentos.origem          = lote_ctbl.cod_modul_dtsul 
             ttLancamentos.tipoMov         = ITEM_lancto_ctbl.ind_natur_lancto_ctbl
             ttLancamentos.historico       = ITEM_lancto_ctbl.des_histor_lancto_ctbl
             ttLancamentos.valor           = ITEM_lancto_ctbl.val_lancto_ctbl
             ttLancamentos.codEstabel      = ITEM_lancto_ctbl.cod_estab
             ttLancamentos.unidNegocio     = ITEM_lancto_ctbl.cod_unid_negoc
             ttLancamentos.PCContabil      = ITEM_lancto_ctbl.cod_plano_cta_ctbl
             ttLancamentos.PCCentroCusto   = ITEM_lancto_ctbl.cod_plano_ccusto
             ttLancamentos.rrowid          = string(ROWID(ITEM_lancto_ctbl))
             ttLancamentos.cenario         = item_lancto_ctbl.cod_cenar_ctbl
             ttLancamentos.observacao      = IF lEstoque = YES THEN 'sequencia:' + string(ttLancamentos.seq) + ' valor:' + string(ITEM_lancto_ctbl.val_lancto_ctbl) ELSE ""
             ttLancamentos.sequencias      = string(ttLancamentos.seq)
             ttLancamentos.grupoEstoque    = ENTRY(8,ttLancamentos.historico," ").

          END.
       END.
    END.
END.    
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscaEmpresas w-relat 
PROCEDURE piBuscaEmpresas :
/*------------------------------------------------------------------------------
Busca as empresas existentes
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER pListaEmpresas AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cItem AS CHARACTER   NO-UNDO FORMAT 'x(50)' .
ASSIGN pListaEmpresas = 'Selecione,0'.

FOR EACH ems5.empresa NO-LOCK:
    ASSIGN cItem = empresa.cod_empresa + '-' + empresa.nom_abrev + "-" + empresa.nom_razao_social + "," + empresa.cod_empresa  .
           pListaEmpresas = IF pListaEmpresas = "" THEN  cItem
                            ELSE  pListaEmpresas + ',' + cItem.
END.




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalculaSaldos w-relat 
PROCEDURE piCalculaSaldos :
FOR EACH ttLancamentos BREAK BY  ttLancamentos.codEstabel  BY ttLancamentos.centroCusto BY ttLancamentos.contaContabil   BY ttLancamentos.data:
    /*FIND FIRST ttSaldoConta
        WHERE ttSaldoConta.contaContabil = ttLancamentos.contaContabil
        AND   ttSaldoConta.PCContabil    = ttLancamentos.PCContabil
        AND   ttSaldoConta.unidNegocio   = ttLancamentos.unidNegocio
        AND   ttSaldoConta.cenario       = ttLancamentos.cenario
        AND   ttSaldoConta.codEstabel    = ttLancamentos.codEstabel
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttSaldoConta THEN DO:
       CREATE ttSaldoConta.
       ASSIGN ttSaldoConta.contaContabil = ttLancamentos.contaContabil
              ttSaldoConta.PCContabil    = ttLancamentos.PCContabil     
              ttSaldoConta.unidNegocio   = ttLancamentos.unidNegocio    
              ttSaldoConta.cenario       = ttLancamentos.cenario
              ttSaldoConta.data          = ttLancamentos.data
              ttSaldoConta.codEstabel    = ttLancamentos.codEstabel.
    END.*/
    IF /*FIRST-OF(ttLancamentos.contaContabil) AND*/ FIRST-OF(ttLancamentos.contaContabil) AND FIRST-OF(ttLancamentos.codEstabel) THEN DO:
       /* MESSAGE 'passei na primeira vez pelo lancamento' SKIP
                ttLancamentos.data
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
       FIND FIRST ttSaldoContaCC                                                              
           WHERE ttSaldoContaCC.contaContabil   = ttLancamentos.contaContabil                 
           AND   ttSaldoContaCC.centroCusto     = ttLancamentos.centroCusto                   
           AND   ttSaldoContaCC.PCContabil      = ttLancamentos.PCContabil                    
           AND   ttSaldoContaCC.unidNegocio     = ttLancamentos.unidNegocio                   
           AND   ttSaldoContaCC.cenario         = ttLancamentos.cenario                       
           AND   ttSaldoContaCC.codEstabel      = ttLancamentos.codEstabel                    
           AND   ttSaldoContaCC.PCCentroCusto   = ttLancamentos.PCCentroCusto                 
           NO-LOCK NO-ERROR.                                                                  
       IF NOT AVAIL ttSaldoContaCC THEN DO:  
           /*MESSAGE 'n„o achei o registro da ttsaldocontacc'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
          CREATE ttSaldoContaCC.                                                              
          ASSIGN ttSaldoContaCC.contaContabil   = ttLancamentos.contaContabil                 
                 ttSaldoContaCC.centroCusto     = ttLancamentos.centroCusto                   
                 ttSaldoContaCC.PCContabil      = ttLancamentos.PCContabil                    
                 ttSaldoContaCC.unidNegocio     = ttLancamentos.unidNegocio                   
                 ttSaldoContaCC.cenario         = ttLancamentos.cenario                       
                 ttSaldoContaCC.codEstabel      = ttLancamentos.codEstabel                    
                 ttSaldoContaCC.PCCentroCusto   = ttLancamentos.PCCentroCusto                 
                 ttSaldoContaCC.data            = ttLancamentos.data.                         
       END.    
    END.       
END.
/*
FOR EACH ttLancamentos   BY ttLancamentos.centroCusto BY ttLancamentos.contaContabil BY ttLancamentos.data:
    FIND FIRST ttSaldoContaCC                                                              
           WHERE ttSaldoContaCC.contaContabil   = ttLancamentos.contaContabil                 
           AND   ttSaldoContaCC.centroCusto     = ttLancamentos.centroCusto                   
           AND   ttSaldoContaCC.PCContabil      = ttLancamentos.PCContabil                    
           AND   ttSaldoContaCC.unidNegocio     = ttLancamentos.unidNegocio                   
           AND   ttSaldoContaCC.cenario         = ttLancamentos.cenario                       
           AND   ttSaldoContaCC.codEstabel      = ttLancamentos.codEstabel                    
           AND   ttSaldoContaCC.PCCentroCusto   = ttLancamentos.PCCentroCusto                 
           NO-LOCK NO-ERROR.   
    IF NOT AVAIL ttSaldoContaCC THEN DO:  
           /*MESSAGE 'n„o achei o registro da ttsaldocontacc'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
          CREATE ttSaldoContaCC.                                                              
          ASSIGN ttSaldoContaCC.contaContabil   = ttLancamentos.contaContabil                 
                 ttSaldoContaCC.centroCusto     = ttLancamentos.centroCusto                   
                 ttSaldoContaCC.PCContabil      = ttLancamentos.PCContabil                    
                 ttSaldoContaCC.unidNegocio     = ttLancamentos.unidNegocio                   
                 ttSaldoContaCC.cenario         = ttLancamentos.cenario                       
                 ttSaldoContaCC.codEstabel      = ttLancamentos.codEstabel                    
                 ttSaldoContaCC.PCCentroCusto   = ttLancamentos.PCCentroCusto                 
                 ttSaldoContaCC.data            = ttLancamentos.data.                         
     END.
END.

*/





/* esse bloco foi comentado, pois, Ç necessario apenas se for utilizado ordenaá∆o por conta,porÇm, est† fora do escopo atual
FOR EACH ttSaldoConta:
    EMPTY TEMP-TABLE tt_erro_relatorio_razao .
    run prgfin/fgl/fgl905za.py(1,
                                INPUT FRAME f-pg-sel cb-empresa ,

                                ttSaldoConta.cenario ,
                                'CORRENTE' , /*valor fixado por estar fora do escopo inicial, caso seja necessario a convers∆o deve ser implementada a finalidade de forma variavel*/
                                ttSaldoConta.PCContabil ,
                                ttSaldoConta.contaContabil ,
                                ttSaldoConta.codEstabel ,
                                ttSaldoConta.unidNegoc , 
                                '' ,
                                '' ,
                                ttSaldoConta.data - 1 ,
                                ttSaldoConta.data - 1 ,
                                no,
                                Input FRAME f-pg-sel tg-sdo-apuracao-resultado , /*considera apuracao resultado*/
                                YES, /* considera unid.negocio */
                                input-output table tt_erro_relatorio_razao).

    FIND FIRST tt_erro_relatorio_razao NO-ERROR.
    IF AVAIL tt_erro_relatorio_razao THEN DO:
       FOR EACH tt_erro_relatorio_razao:
           CREATE tt_erro_relatorio_razao_acum.
           BUFFER-COPY tt_erro_relatorio_razao TO tt_erro_relatorio_razao_acum.
           ASSIGN tt_erro_relatorio_razao_acum.contaContabil = ttSaldoConta.contaContabil
                  tt_erro_relatorio_razao_acum.centroCusto   = ''.
       END.
    END.
    ELSE DO:
       FIND FIRST tt_sdo_ctbl
           WHERE tt_sdo_ctbl.tta_dat_sdo_ctbl = ttSaldoConta.data NO-ERROR.
       IF AVAIL tt_sdo_ctbl THEN DO:
          ASSIGN ttSaldoConta.valor = tt_sdo_ctbl.tta_val_sdo_ctbl_fim.
       END.
    END.
END.
*/
OUTPUT TO value(session:TEMP-DIRECTORY + "profile.txt") APPEND.
FOR EACH ttSaldoContaCC:
    EXPORT DELIMITER "|" "ttSaldoContaCC" ttSaldoContaCC.cenario ttSaldoContaCC.PCContabil ttSaldoContaCC.contaContabil  ttSaldoContaCC.codEstabel
     ttSaldoContaCC.unidNegoc  ttSaldoContaCC.PCCentroCusto ttSaldoContaCC.centroCusto STRING(TIME,"hh:mm:ss").
    EMPTY TEMP-TABLE tt_erro_relatorio_razao .
    EMPTY TEMP-TABLE tt_sdo_ctbl.
    EMPTY TEMP-TABLE tt_estab_unid_negoc_select.
    /*EMPTY TEMP-TABLE tt_erro_relatorio_razao_acum.*/
/*     MESSAGE                                */
/*          ttSaldoContaCC.PCContabil         */
/*          ttSaldoContaCC.contaContabil      */
/*          ttSaldoContaCC.codEstabel         */
/*          ttSaldoContaCC.unidNegoc          */
/*          ttSaldoContaCC.PCCentroCusto      */
/*          ttSaldoContaCC.centroCusto        */
/*          ttSaldoContaCC.data - 1           */
/*          ttSaldoContaCC.data - 1           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */

    /*OUTPUT TO c:\temp\params.txt APPEND.

    PUT 1 SKIP INPUT FRAME f-pg-sel cb-empresa SKIP ttSaldoContaCC.cenario SKIP
                                'CORRENTE' SKIP
                                ttSaldoContaCC.PCContabil SKIP
                                ttSaldoContaCC.contaContabil SKIP
                                ttSaldoContaCC.codEstabel SKIP
                                ttSaldoContaCC.unidNegoc SKIP 
                                ttSaldoContaCC.PCCentroCusto SKIP
                                ttSaldoContaCC.centroCusto SKIP
                                ttSaldoContaCC.data - 1 SKIP
                                ttSaldoContaCC.data - 1 SKIP
                                NO SKIP
                                Input FRAME f-pg-sel tg-sdo-apuracao-resultado SKIP
                                YES SKIP(3).

    OUTPUT CLOSE.*/


    run prgfin/fgl/fgl905za.py(1,
                                INPUT FRAME f-pg-sel cb-empresa ,
                                ttSaldoContaCC.cenario ,
                                'CORRENTE' , /*valor fixado por estar fora do escopo inicial, caso seja necessario a convers∆o deve ser implementada a finalidade de forma variavel*/
                                ttSaldoContaCC.PCContabil ,
                                ttSaldoContaCC.contaContabil ,
                                ttSaldoContaCC.codEstabel ,
                                ttSaldoContaCC.unidNegoc , 
                                ttSaldoContaCC.PCCentroCusto ,
                                ttSaldoContaCC.centroCusto ,
                                ttSaldoContaCC.data - 1 ,
                                ttSaldoContaCC.data - 1 ,
                                no,
                                Input FRAME f-pg-sel tg-sdo-apuracao-resultado , /*considera apuracao resultado*/
                                YES, /* considera unid.negocio */
                                input-output table tt_erro_relatorio_razao).
        FIND FIRST tt_erro_relatorio_razao NO-ERROR.
    IF AVAIL tt_erro_relatorio_razao THEN DO:
       FOR EACH tt_erro_relatorio_razao:
           CREATE tt_erro_relatorio_razao_acum.
           BUFFER-COPY tt_erro_relatorio_razao TO tt_erro_relatorio_razao_acum.
           ASSIGN tt_erro_relatorio_razao_acum.contaContabil = ttSaldoContaCC.contaContabil
                  tt_erro_relatorio_razao_acum.centroCusto   = ttSaldoContaCC.centroCusto.
       END.
    END.
    ELSE DO:
       FIND FIRST tt_sdo_ctbl
           WHERE tt_sdo_ctbl.tta_dat_sdo_ctbl = ttSaldoContaCC.data NO-ERROR.
       IF AVAIL tt_sdo_ctbl THEN DO:
          ASSIGN ttSaldoContaCC.valor = tt_sdo_ctbl.tta_val_sdo_ctbl_fim.
       END.
    END.
END.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piExportaDados w-relat 
PROCEDURE piExportaDados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lExportado              AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cNarrativa              AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cHistorico              AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cDescConta              AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cDescCentroCusto        AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cDescEstabel            AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cDescGrupoEst           AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cDescFornec             AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cDescOper               AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cDescItem               AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cNaturezaConta          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNaturezaSaldo          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSaldoConta             AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSaldoCC                AS DECIMAL     NO-UNDO INIT 0.
DEFINE VARIABLE dSaldoCCEstoque         AS DECIMAL     NO-UNDO INIT 0.
DEFINE VARIABLE dValorAbsoluto          AS DECIMAL     NO-UNDO INIT 0.
DEFINE VARIABLE dValorAbsolutoEstoque   AS DECIMAL     NO-UNDO INIT 0.
/*DEFINE VARIABLE iSinalEstoque           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSinal                  AS INTEGER     NO-UNDO.*/
OUTPUT TO value(session:TEMP-DIRECTORY + "profile.txt") APPEND.
EXPORT "|" "Inicio Exportacao" STRING(TIME,"hh:mm:ss").
OUTPUT CLOSE.
OUTPUT TO value(cArquivoTexto) NO-CONVERT.
FOR EACH ttLancamentos
    BREAK BY ttLancamentos.centroCusto BY ttLancamentos.contaContabil  BY ttLancamentos.data:
    
    /*busca natureza da conta contabil*/
    FIND FIRST cta_ctbl
        WHERE cta_ctbl.cod_plano_cta_ctbl = ttLancamentos.PCContabil
        AND   cta_ctbl.cod_cta_ctbl       = ttLancamentos.contaContabil use-index  ctactbl_id NO-LOCK NO-ERROR.
    IF AVAIL cta_ctbl THEN DO:
       ASSIGN cNaturezaConta = cta_ctbl.ind_natur_cta_ctbl.
    END.
   
    ASSIGN lExportado = NO
           /* retira quebra de linhas*/
           cHistorico = replace(ttLancamentos.historico,CHR(10),'')
           cHistorico = replace(cHistorico,CHR(13),'').    
    ASSIGN cDescConta = IF AVAIL cta_ctbl THEN cta_ctbl.des_tit_ctbl ELSE ''.

    /*Busca Descriá∆o do Centro de Custo*/
    FIND FIRST ems5.cCusto
        WHERE cCusto.cod_empresa        = cb-empresa:SCREEN-VALUE IN FRAME f-pg-sel
        AND   cCusto.cod_plano_ccusto   = ttLancamentos.PCCentroCusto
        AND   cCusto.cod_ccusto         = ttLancamentos.centroCusto
        NO-LOCK NO-ERROR.                                                     
    ASSIGN cDescCentroCusto = IF AVAIL cCusto THEN cCusto.des_tit_ctbl ELSE ''.
   IF FIRST-OF(ttLancamentos.centroCusto) THEN DO:
      ASSIGN dSaldoCC           = 0
             dSaldoCCEStoque    = 0.
   END.
     
   IF /*FIRST-OF(ttLancamentos.centroCusto) AND*/ FIRST-OF(ttLancamentos.contaContabil) THEN DO:
     
      FIND FIRST ttSaldoContaCC
          WHERE  ttSaldoContaCC.contaContabil = ttLancamentos.contacontabil
          AND    ttSaldoContaCC.centroCusto   = ttLancamentos.centroCusto
          AND    ttSaldoContaCC.cenario       = ttLancamentos.cenario
          AND    ttSaldoContacc.codEstabel    = ttLancamentos.codestabel
          AND    ttSaldoContacc.unidNegocio   = ttLancamentos.unidNegocio
          AND    ttSaldoContaCC.PCContabil    = ttLancamentos.PCContabil
          AND    ttSaldoContacc.PCCentroCusto = ttLancamentos.PCCentroCusto
          NO-LOCK NO-ERROR.
      ASSIGN dSaldoConta = IF AVAIL ttSaldoContaCC THEN ttSaldoContaCC.valor ELSE 0
             dSaldoCC        =  dSaldoCC       + IF AVAIL ttSaldoContaCC THEN ttSaldoContaCC.valor ELSE 0
             dSaldoCCEstoque = dSaldoCCEstoque + IF AVAIL ttSaldoContaCC THEN ttSaldoContaCC.valor ELSE 0.
      IF AVAIL ttSaldoContaCC THEN DO:
         IF (ttSaldoContaCC.valor >= 0 AND cNaturezaConta = 'DB' ) OR (ttSaldoContaCC.valor <= 0 AND cNaturezaConta = 'CR') THEN
             ASSIGN cNaturezaSaldo = "DB".
         ELSE
            ASSIGN cNaturezaSaldo = "CR".
        IF cb-excel:SCREEN-VALUE IN FRAME f-pg-sel = '1' THEN DO:
           EXPORT DELIMITER "|" 
           ttLancamentos.contaContabil
           cDescConta
           ttLancamentos.centroCusto
           cDescCentroCusto
           ttLancamentos.data FORMAT "99/99/9999"
           '0'
           'SALDO ANTERIOR'
           '0'
           '0'
           'SALDO ANTERIOR'
           cNaturezaSaldo
           ttSaldoContaCC.valor    FORMAT ">>>,>>>,>>9.99"
           dSaldoConta             FORMAT ">>>,>>>,>>9.99" 
           dSaldoCC                FORMAT ">>>,>>>,>>9.99" 
           'SALDO ANTERIOR'
           ttLancamentos.codEstabel
           /*cDescEstabel*/
           ttLancamentos.unidNegocio
           ttLancamentos.contaContabilExt
           ttLancamentos.centroCustoExt
           " "
           'SALDO ANTERIOR' 
           ttSaldoContaCC.valor
           " "
           " "
           " "
           " "
           " "
           " "
           " "
           dSaldoCCEstoque.
        END.
        ELSE DO:
           EXPORT DELIMITER "|"                                 
           ttLancamentos.contaContabil                          
           cDescConta                                           
           ttLancamentos.centroCusto                            
           cDescCentroCusto                                     
           ttLancamentos.data FORMAT "99/99/9999"               
           cNaturezaSaldo                                       
           'SALDO ANTERIOR'                                     
           ttSaldoContaCC.valor                                 
           " "                                                  
           " "                                                  
           " "                                                  
           " "                                                  
           " "                                                  
           " "                                                  
           " "                                                  
           dSaldoCCEstoque.  
        END.
      END.
   END.
   FIND FIRST ttEstoque
        WHERE ttEstoque.rRowidLanc = ttLancamentos.rRowid NO-ERROR.
   /*IF cNaturezaConta = 'CR' THEN 
      ASSIGN iSinal = 1. 
   ELSE 
     ASSIGN iSinal = -1.*/
   
   IF cNaturezaConta = 'CR' THEN DO: 
       IF ttLancamentos.tipoMov <> cNaturezaConta THEN
           ASSIGN dValorAbsoluto = ttLancamentos.valor * -1 .
       ELSE
          ASSIGN dValorAbsoluto = ttLancamentos.valor.
   END.
   ELSE DO: 
       IF ttLancamentos.tipoMov <> cNaturezaConta THEN
           ASSIGN dValorAbsoluto = ttLancamentos.valor.
       ELSE
          ASSIGN dValorAbsoluto = ttLancamentos.valor  * -1.
   END.

   IF SUBSTR(ttLancamentos.contaContabil,1,1)= '1' OR SUBSTR(ttLancamentos.contaContabil,1,1)= '2'  THEN  DO:
      IF ttLancamentos.tipoMov <> cNaturezaConta THEN
           ASSIGN dValorAbsoluto = ttLancamentos.valor * -1 .
       ELSE
          ASSIGN dValorAbsoluto = ttLancamentos.valor.
   END.
   
   ASSIGN 
          dSaldoConta        = dSaldoConta + dValorAbsoluto
          dSaldoCC           = dSaldoCC    + dValorAbsoluto
          dSaldoCCEstoque   = IF NOT AVAIL ttEstoque THEN dSaldoCCEstoque + dValorAbsoluto ELSE dSaldoCCEstoque. /*verifica se existe pelo menos um  registro de estoque. caso 
                                exista, n∆o  soma o valor total do lanáamento na variavel dContaccEstoq, deixando a soma para o for da tabela ttEstoque */
   /*IF cNaturezaConta = 'CR' THEN 
      ASSIGN iSinalEstoque = -1. 
   ELSE 
     ASSIGN iSinalEstoque = 1.*/

   FOR EACH ttEstoque
       WHERE ttEstoque.rRowidLanc = ttLancamentos.rRowid:
       FIND FIRST ITEM 
           WHERE ITEM.it-codigo = ttEstoque.itCodigo NO-LOCK NO-ERROR.
      
       
       /* IF LOOKUP(string(ITEM.tipo-contr),'1,4') > 0 THEN
          ASSIGN dValorAbsolutoEstoque = IF ttEstoque.tipoMov = 1 THEN ttEstoque.valor * 1 ELSE ttEstoque.valor * -1 .
       ELSE
          ASSIGN dValorAbsolutoEstoque = IF ttEstoque.tipoMov = 1 THEN ttEstoque.valor  ELSE ttEstoque.valor * -1 .*/

       IF  ttEstoque.tipoMov = 2 THEN
           ASSIGN dValorAbsolutoEstoque = ttEstoque.valor * -1.
       ELSE
           ASSIGN dValorAbsolutoEstoque = ttEstoque.valor.


       /*ASSIGN dValorAbsolutoEstoque = dValorAbsolutoEstoque * iSinalEstoque.*/
          

       ASSIGN dSaldoCCEstoque = dSaldoCCEstoque + dValorAbsolutoEstoque.

       /* busca descricao do item*/
       /*FIND FIRST ITEM
       WHERE ITEM.it-codigo = ttEstoque.itCodigo
       NO-LOCK NO-ERROR.
       ASSIGN cDescItem = IF AVAIL ITEM THEN ITEM.desc-item ELSE ''.*/
       
       FIND FIRST emitente
           WHERE emitente.cod-emitente = ttestoque.codFornec
           NO-LOCK NO-ERROR.
       ASSIGN cDescFornec = IF AVAIL emitente THEN emitente.nome-abrev ELSE "".

       ASSIGN lExportado = YES
              cNarrativa = replace(ttestoque.narrativa,CHR(10),'')
              cNarrativa = replace(cNarrativa,CHR(13),'').
       IF cb-excel:SCREEN-VALUE IN FRAME f-pg-sel = '1' THEN DO:
          EXPORT DELIMITER "|" 
          ttLancamentos.contaContabil
          cDescConta
          ttLancamentos.centroCusto
          cDescCentroCusto
          ttLancamentos.data FORMAT "99/99/9999"
          ttLancamentos.lote
          ttLancamentos.descLote
          ttLancamentos.lanc
          ttLancamentos.sequencias
          ttLancamentos.origem
          ttLancamentos.tipoMov
          dValorAbsoluto          FORMAT "->>>,>>>,>>9.99"
          dSaldoConta             FORMAT "->>>,>>>,>>9.99" 
          dSaldoCC                FORMAT "->>>,>>>,>>9.99"       
          cHistorico
          ttLancamentos.codEstabel
          /*cDescEstabel*/
          ttLancamentos.unidNegocio
          ttLancamentos.contaContabilExt
          ttLancamentos.centroCustoExt
          ttEstoque.grupoEst
          /*cDescGrupoEst*/
          if cNarrativa = "" then cHistorico else cNarrativa
          dValorAbsolutoEstoque FORMAT "->>>,>>>,>>9.99"
          ttEstoque.codUsuario
          ttEstoque.codFornec
          cDescFornec
          ttEstoque.nrDocto
          ttEstoque.serie
          ttEstoque.naturOper
          /*cDescOper*/
          ttEstoque.itcodigo
          /*cDescItem*/
          dSaldoCCEstoque
          IF num-entries(ttLancamentos.observacao,"/") > 1 THEN ttLancamentos.observacao ELSE ''.
          
       END.
       ELSE DO:
          EXPORT DELIMITER "|"                                                
          ttLancamentos.contaContabil                                         
          cDescConta                                                          
          ttLancamentos.centroCusto                                           
          cDescCentroCusto                                                    
          ttLancamentos.data FORMAT "99/99/9999"
          ttLancamentos.tipoMov  
          if cNarrativa = "" then cHistorico else cNarrativa
          dValorAbsolutoEstoque FORMAT "->>>,>>>,>>9.99"                       
          ttEstoque.codUsuario                                                
          ttEstoque.codFornec                                                 
          cDescFornec                                                         
          ttEstoque.nrDocto                                                   
          ttEstoque.serie                                                     
          ttEstoque.naturOper                                                 
          /*cDescOper*/                                                       
          ttEstoque.itcodigo                                                  
          /*cDescItem*/                                                       
          dSaldoCCEstoque.
       END.  
   END.
   IF lExportado = NO THEN DO:
      IF cb-excel:SCREEN-VALUE IN FRAME f-pg-sel = '1' THEN DO:
          EXPORT DELIMITER "|"                                                             
          ttLancamentos.contaContabil                                                      
          cDescConta                                                                       
          ttLancamentos.centroCusto                                                        
          cDescCentroCusto                                                                 
          ttLancamentos.data FORMAT "99/99/9999"                                           
          ttLancamentos.lote                                                               
          ttLancamentos.descLote                                                           
          ttLancamentos.lanc                                                               
          ttLancamentos.sequencias                                                                
          ttLancamentos.origem                                                             
          ttLancamentos.tipoMov                                                            
          dValorAbsoluto          FORMAT "->>>,>>>,>>9.99"                                  
          dSaldoConta             FORMAT "->>>,>>>,>>9.99"                                  
          dSaldoCC                FORMAT "->>>,>>>,>>9.99"                                  
          cHistorico                                                                       
          ttLancamentos.codEstabel                                                         
          /*cDescEstabel*/                                                                 
          ttLancamentos.unidNegocio                                                        
          ttLancamentos.contaContabilExt                                                   
          ttLancamentos.centroCustoExt                                                     
          " "                                                                              
          cHistorico                                                                       
          dValorAbsoluto          FORMAT "->>>,>>>,>>9.99"                                  
          " "                                                                              
          " "                                                                              
          " "                                                                              
          " "                                                                              
          " "                                                                              
          " "                                                                              
          " "                                                                              
          dSaldoCCEstoque
         IF num-entries(ttLancamentos.observacao,"/") > 1 THEN ttLancamentos.observacao ELSE ''.
      END.
      ELSE DO:
          EXPORT DELIMITER "|"                                
          ttLancamentos.contaContabil                         
          cDescConta                                          
          ttLancamentos.centroCusto                           
          cDescCentroCusto                                    
          ttLancamentos.data FORMAT "99/99/9999" 
          ttLancamentos.tipoMov                               
          cHistorico                                          
          dValorAbsoluto          FORMAT ">>>,>>>,>>9.99"     
          " "                                                 
          " "                                                 
          " "                                                 
          " "                                                 
          " "                                                 
          " "                                                 
          " "                                                 
          dSaldoCCEstoque. 
      END.
   END.
END.
FIND FIRST ttLancamentos NO-ERROR.
IF NOT AVAIL ttLancamentos THEN
   PUT "N∆o h† dados..." SKIP.
OUTPUT CLOSE.
/*
OUTPUT TO value(session:TEMP-DIRECTORY + 'ttlancamentos.txt').
FOR EACH ttLancamentos:
    EXPORT DELIMITER "|" ttLancamentos.
END.
OUTPUT CLOSE.
OUTPUT TO value(session:TEMP-DIRECTORY + 'ttEstoque.txt').
FOR EACH ttEstoque:
    EXPORT DELIMITER "|" ttEstoque.
END.
OUTPUT CLOSE.
ASSIGN cSessao = SESSION:STARTUP-PARAMETERS.
OUTPUT TO value(session:TEMP-DIRECTORY + 'parametrosSessao.txt').
PUT cSessao.
OUTPUT CLOSE.
*/
OUTPUT TO value(session:TEMP-DIRECTORY + "profile.txt") APPEND.
EXPORT "|" "Fim Exportaá∆o" STRING(TIME,"hh:mm:ss").
OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGeraDadosEstoque w-relat 
PROCEDURE piGeraDadosEstoque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pID             AS CHAR FORMAT 'x(50)' NO-UNDO.
DEFINE INPUT  PARAMETER pReferencia     AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER pContaContabil  AS CHARACTER   NO-UNDO FORMAT "x(50)".
DEFINE INPUT  PARAMETER pCentroCusto    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pData           AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pTipoMov        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pGrupoMov       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEstabel     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iGrupoItem AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNarrativa AS CHARACTER   NO-UNDO FORMAT 'x(1000)'.
DEFINE VARIABLE cNarrAux   AS CHARACTER   NO-UNDO FORMAT 'x(1000)'.
DEFINE VARIABLE lEstoque   AS LOGICAL     NO-UNDO INIT NO.
IF lContinua THEN
   MESSAGE  'estoque' SKIP
    pCodEstabel       SKIP
    pData             SKIP
    pReferencia       SKIP
    pContaContabil    SKIP
    pCentroCusto      SKIP  
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
OUTPUT TO c:\temp\LOG_movto_estoq.txt APPEND .
PUT 'busquei a referentecia:' pReferencia ' conta:' pContaContabil ' centro de custo: '  pCentroCusto ' data:' pData .


FOR EACH movto-estoq NO-LOCK
    WHERE 
    /*movto-estoq.contabilizado = YES
    AND */
    movto-estoq.cod-estabel         = pCodEstabel
    AND   movto-estoq.dt-trans      = pData
    AND   
    /*(movto-estoq.refer-contab  = pReferencia  OR 
     (
         movto-estoq.refer-contab = '' AND  LOOKUP(string(movto-estoq.dt-trans,'99/99/9999'), cDatas ,"|"  ) > 0
     )
    )*/
    pReferencia = "CE" + string(DAY(movto-estoq.dt-trans),'99') + string(MONTH(movto-estoq.dt-trans),'99') + string(YEAR(movto-estoq.dt-trans),'9999') 
    AND   (
             (movto-estoq.ct-codigo     = pContaContabil AND   movto-estoq.sc-codigo     = pCentroCusto) OR
             (movto-estoq.ct-saldo      = pContaContabil AND   movto-estoq.sc-saldo      = pCentroCusto)
           )
    USE-INDEX data-conta:
    ASSIGN lEstoque = YES.
    FIND FIRST ITEM 
         WHERE ITEM.it-codigo = movto-estoq.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
       ASSIGN iGrupoItem = ITEM.ge-codigo.
    IF NOT AVAIL ITEM THEN DO:
       FIND FIRST ttItens
           WHERE itCodigo = movto-estoq.it-codigo NO-ERROR.
       IF NOT AVAIL ttItens THEN DO:
          CREATE  ttItens.
          ASSIGN itCodigo = movto-estoq.it-codigo.
       END.
    END.
    FIND LAST ge-troca
        WHERE ge-troca.it-codigo = movto-estoq.it-codigo
        AND   ge-troca.dt-troca <= pData NO-LOCK NO-ERROR.
    IF AVAIL ge-troca THEN
       ASSIGN iGrupoItem = ge-troca.ge-destino.
    IF lContinua THEN
       MESSAGE 'entrei no estoque' SKIP
               'igrupoitem:' iGrupoItem SKIP
               'pgrupomov:'  INT(pGrupoMov)  
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
    IF iGrupoItem = INT(pGrupoMov) THEN DO:
        IF lcontinua THEN
          MESSAGE 'entrei no item doc-est'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        FIND FIRST  item-doc-est
            WHERE  item-doc-est.serie-docto   =   movto-estoq.serie-docto 
            AND    item-doc-est.nro-docto     =   movto-estoq.nro-docto   
            AND    item-doc-est.nat-operacao  =   movto-estoq.nat-operacao
            AND    item-doc-est.cod-emitente  =   movto-estoq.cod-emitente
            AND    item-doc-est.sequencia     =   movto-estoq.sequen-nf   
            AND    item-doc-est.it-codigo     =   movto-estoq.it-codigo   
           NO-LOCK NO-ERROR.
        ASSIGN cNarrativa = "".
        IF movto-estoq.descricao-db <> "" THEN DO:
           ASSIGN  cNarrAux   = REPLACE(movto-estoq.descricao-db,CHR(13),'')
                   cNarrAux   = REPLACE(cNarrAux,CHR(10),'')
                   cNarrativa = "Estoque:" + cNarrAux.
        END.
           
        IF AVAIL item-doc-est THEN DO:
           ASSIGN cNarrAux   = replace(item-doc-est.narrativa,CHR(13),'')
                  cNarrAux   = REPLACE(cNarrAux,CHR(10),'').
           if item-doc-est.narrativa <> movto-estoq.descricao-db then do:
              assign cNarrativa = IF cNarrativa = "" THEN cNarrAux
                                  ELSE cNarrativa + " Recebimento:" + cNarrAux.
           end.
        END.
          
        CREATE ttEstoque.
        ASSIGN ttEstoque.rRowidLanc     = pID
               ttEStoque.codEstabel     = pCodEstabel
               ttEstoque.GrupoEst       = pGrupoMov
               ttEstoque.narrativa      = cNarrativa
               ttEstoque.codUsuario     = movto-estoq.usuario
               ttEstoque.codFornec      = movto-estoq.cod-Emitente
               ttEstoque.nrDocto        = movto-estoq.nro-docto
               ttEstoque.naturOper      = movto-estoq.nat-operacao
               ttEstoque.serie          = movto-estoq.serie-docto
               ttEstoque.valor          = IF movto-estoq.valor-nota = 0 THEN
                                         movto-estoq.valor-mat-m[1] + movto-estoq.valor-ggf-m[1] + movto-estoq.valor-mob-m[1] ELSE movto-estoq.valor-nota
               ttEstoque.itCodigo       = movto-estoq.it-codigo
               ttEstoque.ctCodigo       = movto-estoq.ct-codigo
               ttEstoque.scCodigo       = movto-estoq.sc-codigo
               ttEstoque.tipoMov        = movto-estoq.tipo-trans.
       IF lContinua THEN
          MESSAGE 
         /* 'id' ttEstoque.id                         SKIP            */
          'estab' ttEStoque.codEstabel              SKIP
          'grupo' ttEstoque.GrupoEst                SKIP
          'narrativa' ttEstoque.narrativa           SKIP
          'codusuario' ttEstoque.codUsuario         SKIP
          'fornec' ttEstoque.codFornec             SKIP
          'nrdocto' ttEstoque.nrDocto              SKIP
          'natureza' ttEstoque.naturOper           SKIP
          'serie' ttEstoque.serie                  SKIP
          'valor' ttEstoque.valor                  SKIP
          'item' ttEstoque.itCodigo       
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END. 
END.
PUT lEstoque SKIP.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGeraEstabsEstoque w-relat 
PROCEDURE piGeraEstabsEstoque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER pEstabsLabel  AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE OUTPUT PARAMETER pEstabs       AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cLabel                AS CHARACTER   NO-UNDO FORMAT 'x(50)'.




FOR EACH estabelec NO-LOCK:
    ASSIGN cLabel = "Est: ".
    IF pEstabs = "" THEN DO:
       ASSIGN pEstabs = string(estabelec.cod-estab )
              pEstabsLabel = cLabel + string(estabelec.cod-estab ) .
    END.
       
    ELSE DO:                                                          
       ASSIGN pEstabs =  pEstabs + "|" + string(estabelec.cod-estab )
              pEstabsLabel = pEstabsLabel + "|" + cLabel + string(estabelec.cod-estab ).
    END.
       
END.

/* MESSAGE pestabslabel                   */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piGeraGruposEstoque w-relat 
PROCEDURE piGeraGruposEstoque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER pGruposLabel  AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE OUTPUT PARAMETER pGrupos       AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cLabel                AS CHARACTER   NO-UNDO FORMAT 'x(50)'.




FOR EACH grup-estoq NO-LOCK:
    ASSIGN cLabel = "Grupo: ".
    IF grup-estoq.ge-codigo < 10 THEN
       ASSIGN cLabel = cLabel + " ".

    IF pGrupos = "" THEN DO:
       ASSIGN pGrupos = string(grup-estoq.ge-codigo)
              pGruposLabel = cLabel + string(grup-estoq.ge-codigo) .
    END.
       
    ELSE DO:                                                          
       ASSIGN pGrupos =  pGrupos + "|" + string(grup-estoq.ge-codigo)
              pGruposLabel = pGruposLabel + "|" + cLabel + string(grup-estoq.ge-codigo).
    END.
       
END.
/* MESSAGE pGruposLabel                    */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piLimpaVariaveis w-relat 
PROCEDURE piLimpaVariaveis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE ttLancamentos.
EMPTY TEMP-TABLE ttEstoque.
EMPTY TEMP-TABLE ttItens.
EMPTY TEMP-TABLE ttArquivo.
EMPTY TEMP-TABLE ttConta.
EMPTY TEMP-TABLE ttSaldoConta.
EMPTY TEMP-TABLE ttSaldoContaCC.
EMPTY TEMP-TABLE tt_erro_relatorio_razao.
EMPTY TEMP-TABLE tt_erro_relatorio_razao_acum. 
EMPTY TEMP-TABLE tt_sdo_ctbl.
EMPTY TEMP-TABLE tt_estab_unid_negoc_select.




















































END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piRetornaUltDiaMes w-relat 
PROCEDURE piRetornaUltDiaMes :
DEFINE INPUT  PARAMETER d1 AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER d2 AS DATE        NO-UNDO.
DEFINE OUTPUT PARAMETER cDatas AS CHAR FORMAT 'x(500)'.
DEFINE VARIABLE  mesd1      AS int         NO-UNDO.
DEFINE VARIABLE  mesd2      AS INT         NO-UNDO.
DEFINE VARIABLE  anod1      AS int         NO-UNDO.
DEFINE VARIABLE  anod2      AS INT         NO-UNDO.
DEFINE VARIABLE cont        AS INTEGER     NO-UNDO.
DEFINE VARIABLE contMes     AS INTEGER     NO-UNDO.
DEFINE VARIABLE dtUltDia    AS DATE        NO-UNDO.
DEFINE VARIABLE iMes        AS INT         NO-UNDO.
DEFINE VARIABLE i           AS INT         NO-UNDO.

 
ASSIGN mesd1 = MONTH(d1)
        mesd2 = MONTH(d2)
        anod1 = YEAR(d1)
        anod2 = YEAR(d2).
ASSIGN cont = d2 - d1.
       contmes = mesd2 + ( 12 * (anod2 - anod1)) - mesd1.
REPEAT i = 0 TO contMes + 1:
    ASSIGN iMes = (mesd1 + i) MOD 12.
    IF iMes = 0  THEN
       ASSIGN iMes  = 12.
    ASSIGN  dtUltDia = DATE(iMes,1, anod1) - 1 .
    IF dtUltDia >= d1 AND dtUltDia <= d2 THEN DO:
       IF cDatas = '' THEN
          ASSIGN cDatas = string(dtUltDia,"99/99/9999").
       ELSE
          ASSIGN cDatas = cDatas + "|" + string(dtUltDia,"99/99/9999").
    END.
        DISP dtUltDia.
    IF  iMes = 12 THEN
        ASSIGN anod1 = anod1 + 1.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piValidacoes w-relat 
PROCEDURE piValidacoes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cMensagem AS CHARACTER   NO-UNDO FORMAT 'x(4000)' INIT "".
IF INPUT FRAME f-pg-sel cb-empresa  = '0' THEN
   ASSIGN cMensagem = '- Favor Selecionar a empresa.' + CHR(13) .
IF INPUT FRAME f-pg-sel data-ini > INPUT FRAME f-pg-sel data-fim THEN
   ASSIGN cMensagem = cMensagem + '- Data Inicial n∆o pode ser Maior que a Data Final.' + CHR(13) .
IF INPUT FRAME f-pg-sel ccusto-ini > INPUT FRAME f-pg-sel ccusto-fim THEN
   ASSIGN cMensagem =  cMensagem + '- Centro de Custo Inicial maior que o Centro de Custo Final.' + CHR(13) .
IF INPUT FRAME f-pg-sel contacontabil-ini > INPUT FRAME f-pg-sel contacontabil-fim THEN
   ASSIGN cMensagem =  cMensagem + '- Conta Contabil Inicial maior que a Conta Contabil Final.' + CHR(13) .

IF cMensagem <> "" THEN DO:
   MESSAGE cMensagem
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   RETURN NO-APPLY.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-relat  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-relat, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-relat 
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

