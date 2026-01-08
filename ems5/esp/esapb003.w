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
{include/i-prgvrs.i ESAPB002 5.06.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/*lote de pagamento*/

{esp\esapb002.i}


/*DEFINE  TEMP-TABLE tt
    FIELD acaoAtual             AS CHAR
    FIELD situacao              AS CHAR FORMAT 'x(15)'
    FIELD cod_estab             AS CHAR
    FIELD cod_tit_ap            LIKE tit_ap.cod_tit_ap
    FIELD cod_ser_docto         LIKE tit_ap.cod_ser_docto
    FIELD cdn_fornecedor        LIKE tit_ap.cdn_fornecedor
    FIELD nome_abrev            AS CHAR FORMAT 'x(20)'
    FIELD cod_parcela           LIKE tit_ap.cod_parcela
    FIELD dat_emis_docto        LIKE tit_ap.dat_emis_docto
    FIELD dat_vencto_tit_ap     LIKE tit_ap.dat_vencto_tit_ap
    FIELD val_origin_tit_ap     LIKE tit_ap.val_origin_tit_ap
    FIELD val_sdo_tit_ap        LIKE tit_ap.val_sdo_tit_ap
    FIELD val_calc_adiant       AS DEC FORMAT '>>>,>>>,>>9.99'
    FIELD val_real              AS DEC FORMAT '>>>,>>>,>>9.99'
    FIELD val_dolar             AS DEC FORMAT '>>>,>>>,>>9.99'
    FIELD val_desconto          AS DEC FORMAT '>>>,>>>,>>9.99'
    FIELD val_juros_dia_atraso  LIKE tit_ap.val_juros_dia_atraso 
    FIELD val_multa             AS DEC FORMAT '>>>,>>>,>>9.99'
    FIELD aprovador             AS CHAR FORMAT 'x(20)'
    FIELD dt_Liberacao          AS DATE
    FIELD hr_liberacao          AS CHAR
    FIELD usuario_liberacao     AS CHAR
    FIELD num_id_tit_ap         AS INT
    FIELD rRowid                AS ROWID
    FIELD val_cotacao           AS DEC FORMAT '>9.999999999'
    FIELD cod_refer_ant_pef     AS CHAR FORMAT 'x(20)' 
    FIELD rRowidProcesPagto     AS ROWID
    FIELD alteraData            AS LOGICAL INIT NO.*/

DEFINE TEMP-TABLE ttAntec
    FIELD estab         AS CHAR 
    FIELD especie       AS CHAR
    FIELD serie         AS CHAR
    FIELD titulo        AS CHAR FORMAT 'x(20)'
    FIELD parcela       AS CHAR
    FIELD fornecedor    AS INT
    FIELD nome_abrev    AS CHAR FORMAT 'x(12)'
    FIELD vl_saldo      AS DECIMAL
    FIELD moeda         AS CHAR
    FIELD dt_trans      AS DATE
    FIELD num_linha     AS INT
    FIELD vl_utiliz     AS DEC
    FIELD usuarAprov    AS CHAR FORMAT 'x(12)'.

DEFINE TEMP-TABLE ttTitulosPago
    FIELD estab         AS CHAR 
    FIELD especie       AS CHAR
    FIELD serie         AS CHAR
    FIELD titulo        AS CHAR FORMAT 'x(20)'
    FIELD parcela       AS CHAR
    FIELD fornecedor    AS INT
    FIELD nome_abrev    AS CHAR FORMAT 'x(12)'
    FIELD moeda         AS CHAR
    FIELD dt_emissao    AS DATE
    FIELD dt_vencto     AS DATE
    FIELD num_linha_ant AS INT
    FIELD vl_pago       AS DEC
    FIELD rRowid        AS ROWID.
                                
DEFINE TEMP-TABLE ttTitulos
    FIELD estab         AS CHAR 
    FIELD especie       AS CHAR
    FIELD serie         AS CHAR
    FIELD titulo        AS CHAR FORMAT 'x(20)'
    FIELD parcela       AS CHAR
    FIELD fornecedor    AS INT
    FIELD nome_abrev    AS CHAR FORMAT 'x(12)'
    FIELD vl_saldo      AS DECIMAL
    FIELD moeda         AS CHAR
    FIELD dt_emissao    AS DATE
    FIELD dt_vencto     AS DATE
    FIELD num_linha_ant AS INT
    FIELD vl_utiliz     AS DEC
    FIELD vl_utiliz_out AS DEC
    FIELD rRowid        AS ROWID
    FIELD usuarAprov    AS CHAR FORMAT 'x(12)'.


/*
DEFINE TEMP-TABLE ttTitulosSaldo
    FIELD estab         AS CHAR 
    FIELD especie       AS CHAR
    FIELD serie         AS CHAR
    FIELD titulo        AS CHAR FORMAT 'x(20)'
    FIELD parcela       AS CHAR
    FIELD fornecedor    AS INT
    FIELD nome_abrev    AS CHAR FORMAT 'x(12)'
    FIELD vl_saldo      AS DECIMAL
    FIELD moeda         AS CHAR
    FIELD dt_emissao    AS DATE
    FIELD dt_vencto     AS DATE
    FIELD vl_utiliz  AS DEC.*/




/*DEFINE TEMP-TABLE ttPortador
    FIELD cod_portador AS CHAR FORMAT 'x(20)'
    FIELD desc_portador AS CHAR FORMAT 'x(100)'.

DEFINE TEMP-TABLE ttAntecipacao
    FIELD cod_estab AS CHAR
    FIELD cdn_fornecedor AS INT
    FIELD valor AS DECIMAL.*/

DEFINE VARIABLE cod_estab_ini       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cod_estab_fim       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaPortadores    AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cod_fornec_ini      AS INT         NO-UNDO.
DEFINE VARIABLE cod_fornec_fim      AS INT         NO-UNDO.
DEFINE VARIABLE cod_espec_ini       AS CHAR        NO-UNDO.
DEFINE VARIABLE cod_espec_fim       AS CHAR        NO-UNDO.
DEFINE VARIABLE log_saldo_ini       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE log_saldo_fim       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cod_titulo_ini      AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE VARIABLE cod_titulo_fim      AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE VARIABLE cod_parcela_ini     AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE VARIABLE cod_parcela_fim     AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
/*DEFINE VARIABLE serie_ini         AS CHARACTER   NO-UNDO .
DEFINE VARIABLE serie_fim           AS CHARACTER   NO-UNDO .*/
DEFINE VARIABLE i AS INTEGER     NO-UNDO.
DEFINE VARIABLE cListaEspecDocto    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-Acomp             AS HANDLE      NO-UNDO.
DEFINE VARIABLE lUsuarioAprovador   AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lCamposEmBranco     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cErro               AS CHARACTER   NO-UNDO FORMAT 'x(150)'.

DEFINE BUFFER bf_tit_ap FOR tit_ap.
DEFINE BUFFER bf_ttTitulos FOR ttTitulos.
DEFINE VARIABLE v_hdl_aux AS HANDLE      NO-UNDO.
DEFINE VARIABLE cArquivo  AS CHARACTER   NO-UNDO FORMAT 'x(200)'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME brAntecipacao

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAntec ttTitulos

/* Definitions for BROWSE brAntecipacao                                 */
&Scoped-define FIELDS-IN-QUERY-brAntecipacao ttAntec.num_linha ttAntec.estab ttAntec.especie ttAntec.serie ttAntec.titulo ttAntec.parcela ttAntec.fornecedor ttAntec.nome_abrev ttAntec.vl_saldo ttAntec.moeda ttAntec.dt_trans   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brAntecipacao   
&Scoped-define SELF-NAME brAntecipacao
&Scoped-define QUERY-STRING-brAntecipacao FOR EACH ttAntec
&Scoped-define OPEN-QUERY-brAntecipacao OPEN QUERY {&SELF-NAME} FOR EACH ttAntec.
&Scoped-define TABLES-IN-QUERY-brAntecipacao ttAntec
&Scoped-define FIRST-TABLE-IN-QUERY-brAntecipacao ttAntec


/* Definitions for BROWSE brtitulos                                     */
&Scoped-define FIELDS-IN-QUERY-brtitulos ttTitulos.estab ttTitulos.especie ttTitulos.serie ttTitulos.titulo ttTitulos.parcela ttTitulos.fornecedor ttTitulos.nome_abrev ttTitulos.vl_saldo ttTitulos.moeda ttTitulos.dt_emissao ttTitulos.dt_vencto ttTitulos.vl_utiliz ttTitulos.vl_utiliz_out   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brtitulos   
&Scoped-define SELF-NAME brtitulos
&Scoped-define QUERY-STRING-brtitulos FOR EACH ttTitulos EXCLUSIVE-LOCK                              WHERE ttTitulos.fornecedor      = INPUT BROWSE brAntecipacao ttAntec.fornecedor                               AND  ttTitulos.num_linha_ant  = INPUT BROWSE brAntecipacao ttAntec.num_linha. RUN piTotalizarTitulos(INPUT BROWSE brAntecipacao ttAntec.num_linha)
&Scoped-define OPEN-QUERY-brtitulos OPEN QUERY {&SELF-NAME} FOR EACH ttTitulos EXCLUSIVE-LOCK                              WHERE ttTitulos.fornecedor      = INPUT BROWSE brAntecipacao ttAntec.fornecedor                               AND  ttTitulos.num_linha_ant  = INPUT BROWSE brAntecipacao ttAntec.num_linha. RUN piTotalizarTitulos(INPUT BROWSE brAntecipacao ttAntec.num_linha).
&Scoped-define TABLES-IN-QUERY-brtitulos ttTitulos
&Scoped-define FIRST-TABLE-IN-QUERY-brtitulos ttTitulos


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brAntecipacao}~
    ~{&OPEN-QUERY-brtitulos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_cod_estab cb_moeda fi_cod_refer ~
fi_dt_trans fi_data_emis_ini fi_data_emis_fim btFiltrar btAtu fiVlVinc ~
fi_cod_fornecedor fi_especie fi_titulo fi_parcela brtitulos rt-button ~
RECT-1 RECT-2 RECT-3 
&Scoped-Define DISPLAYED-OBJECTS fi_cod_estab cb_moeda fi_cod_refer ~
fi_dt_trans fi_data_emis_ini fi_data_emis_fim fiAntCorr fiVlVinc ~
fi_cod_fornecedor fi_especie fi_titulo fi_parcela fi_desc_fornec fiTotTit ~
fiTotVinc fiTotVincOut fiTotVincGeral 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi_cod_estab cb_moeda fi_cod_refer fi_dt_trans ~
fi_data_emis_ini fi_data_emis_fim btLimpar fi_cod_fornecedor fi_especie ~
fi_titulo fi_parcela fi_desc_fornec 
&Scoped-define List-3 fi_cod_refer fi_dt_trans fi_titulo fi_parcela 

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
DEFINE BUTTON btAtu 
     LABEL "Atualizar" 
     SIZE 8.14 BY 1.

DEFINE BUTTON btCancFiltrar 
     IMAGE-UP FILE "image/toolbar/im-reini.bmp":U
     IMAGE-DOWN FILE "image/toolbar/im-reini.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-reini.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancelar Filtro" 
     SIZE 4.86 BY 1.25 TOOLTIP "Cancelar Filtro".

DEFINE BUTTON btFiltrar 
     IMAGE-UP FILE "image/toolbar/im-fil.bmp":U
     IMAGE-DOWN FILE "image/toolbar/im-fil.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-fil.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Filtrar" 
     SIZE 4.86 BY 1.25.

DEFINE BUTTON btLimpar 
     IMAGE-UP FILE "image/toolbar/im-clr.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-clr.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Limpar Filtro" 
     SIZE 4.72 BY 1.25.

DEFINE BUTTON btProcessar 
     IMAGE-UP FILE "image/toolbar/im-chck3.bmp":U
     IMAGE-DOWN FILE "image/toolbar/im-chck3.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-chck3.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Btn 1" 
     SIZE 4.86 BY 1.25.

DEFINE VARIABLE cb_moeda AS CHARACTER FORMAT "X(8)":U 
     LABEL "Moeda" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Dolar","DOLAR",
                     "Real","REAL"
     DROP-DOWN-LIST
     SIZE 11.43 BY 1 NO-UNDO.

DEFINE VARIABLE fiAntCorr AS DECIMAL FORMAT ">,>>>,>>9.99":U INITIAL 0 
     LABEL "Antec.Corrente" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE fiTotTit AS DECIMAL FORMAT ">>>,>>>,>>9.99":R INITIAL 0 
     LABEL "Titulos Disp." 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fiTotVinc AS DECIMAL FORMAT ">>>,>>>,>>9.99":R INITIAL 0 
     LABEL "Vinculado" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fiTotVincGeral AS DECIMAL FORMAT ">>>,>>>,>>9.99":R INITIAL 0 
     LABEL "Vinculado Geral" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fiTotVincOut AS DECIMAL FORMAT ">>>,>>>,>>9.99":R INITIAL 0 
     LABEL "Vinculado Out." 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fiVlVinc AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Vl.Utilizar" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .79 NO-UNDO.

DEFINE VARIABLE fi_cod_estab AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estab." 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_fornecedor AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornecedor" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_cod_refer AS CHARACTER FORMAT "X(10)":U 
     LABEL "Referencia" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_data_emis_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_data_emis_ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Data Transaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_desc_fornec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi_dt_trans AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt.Transaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_especie AS CHARACTER FORMAT "X(3)":U 
     LABEL "EspÇcie" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi_parcela AS CHARACTER FORMAT "X(2)":U 
     LABEL "Parcela" 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_titulo AS CHARACTER FORMAT "X(20)":U 
     LABEL "Titulo" 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81.57 BY 4.08.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 4.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 6 BY 4.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 129 BY 1.46
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brAntecipacao FOR 
      ttAntec SCROLLING.

DEFINE QUERY brtitulos FOR 
      ttTitulos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brAntecipacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brAntecipacao w-livre _FREEFORM
  QUERY brAntecipacao DISPLAY
      ttAntec.num_linha    COLUMN-LABEL "ID"
   ttAntec.estab        COLUMN-LABEL "Estab."
   ttAntec.especie      COLUMN-LABEL "Esp."    
   ttAntec.serie        COLUMN-LABEL "SÇrie"    
   ttAntec.titulo       COLUMN-LABEL "T°tulo"    
   ttAntec.parcela      COLUMN-LABEL "Parc."     
   ttAntec.fornecedor   COLUMN-LABEL "Fornec." 
   ttAntec.nome_abrev   COLUMN-LABEL "Nome Abrev."  FORMAT "X(20)" 
   ttAntec.vl_saldo     COLUMN-LABEL "Saldo" 
   ttAntec.moeda        COLUMN-LABEL "Moeda" 
   ttAntec.dt_trans     COLUMN-LABEL "Dt.Transaá∆o"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 126.29 BY 5.5
         FONT 1
         TITLE "Antecipaá‰es Disponiveis".

DEFINE BROWSE brtitulos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brtitulos w-livre _FREEFORM
  QUERY brtitulos DISPLAY
      ttTitulos.estab         COLUMN-LABEL "Estab." 
ttTitulos.especie       COLUMN-LABEL "Espec."   
ttTitulos.serie         COLUMN-LABEL "Serie"   
ttTitulos.titulo        COLUMN-LABEL "T°tulo"   
ttTitulos.parcela       COLUMN-LABEL "Parc."   
ttTitulos.fornecedor    COLUMN-LABEL "Fornec."
ttTitulos.nome_abrev    COLUMN-LABEL "Nome Abrev." FORMAT 'x(20)'
ttTitulos.vl_saldo      COLUMN-LABEL "Vl.Saldo"
ttTitulos.moeda         COLUMN-LABEL "Moeda"
ttTitulos.dt_emissao    COLUMN-LABEL "Dt.Emiss∆o"
ttTitulos.dt_vencto     COLUMN-LABEL "Dt.Vencto"
ttTitulos.vl_utiliz  COLUMN-LABEL "Vl.Utilizado"
ttTitulos.vl_utiliz_out  COLUMN-LABEL "Vl.Util.Outro"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 126.29 BY 5.71
         FONT 1
         TITLE "Titulos Disponiveis - Antecipaá∆o Corrente".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fi_cod_estab AT ROW 3.75 COL 13 COLON-ALIGNED WIDGET-ID 86
     cb_moeda AT ROW 3.67 COL 96.57 COLON-ALIGNED WIDGET-ID 90
     fi_cod_refer AT ROW 4.67 COL 96.57 COLON-ALIGNED WIDGET-ID 92
     fi_dt_trans AT ROW 5.67 COL 96.57 COLON-ALIGNED WIDGET-ID 94
     fi_data_emis_ini AT ROW 5.75 COL 13.14 COLON-ALIGNED WIDGET-ID 2
     btCancFiltrar AT ROW 4 COL 123.14 WIDGET-ID 40 NO-TAB-STOP 
     fi_data_emis_fim AT ROW 5.75 COL 39.14 COLON-ALIGNED WIDGET-ID 4
     btFiltrar AT ROW 2.83 COL 123.29 WIDGET-ID 36 NO-TAB-STOP 
     btLimpar AT ROW 5.29 COL 123.29 WIDGET-ID 82 NO-TAB-STOP 
     brAntecipacao AT ROW 7.04 COL 1.72 WIDGET-ID 200
     btAtu AT ROW 18.96 COL 119 WIDGET-ID 112
     fiAntCorr AT ROW 19 COL 10.86 COLON-ALIGNED WIDGET-ID 108
     btProcessar AT ROW 1.08 COL 1.14 WIDGET-ID 44
     fiVlVinc AT ROW 19 COL 105.57 COLON-ALIGNED WIDGET-ID 110
     fi_cod_fornecedor AT ROW 3.75 COL 39 COLON-ALIGNED WIDGET-ID 12
     fi_especie AT ROW 4.75 COL 13.14 COLON-ALIGNED WIDGET-ID 18
     fi_titulo AT ROW 4.75 COL 39.14 COLON-ALIGNED WIDGET-ID 52
     fi_parcela AT ROW 4.71 COL 57.14 COLON-ALIGNED WIDGET-ID 54
     fi_desc_fornec AT ROW 3.75 COL 50.72 COLON-ALIGNED NO-LABEL WIDGET-ID 14 NO-TAB-STOP 
     fiTotTit AT ROW 19 COL 29.86 COLON-ALIGNED WIDGET-ID 104
     fiTotVinc AT ROW 19 COL 47 COLON-ALIGNED WIDGET-ID 106
     brtitulos AT ROW 13 COL 2 WIDGET-ID 300
     fiTotVincOut AT ROW 19 COL 67.14 COLON-ALIGNED WIDGET-ID 114
     fiTotVincGeral AT ROW 19 COL 88.43 COLON-ALIGNED WIDGET-ID 116
     "Filtros Antecipaá‰es" VIEW-AS TEXT
          SIZE 29.14 BY .54 AT ROW 2.79 COL 1.86 WIDGET-ID 20
          FONT 0
     "Dados Efetivaá∆o" VIEW-AS TEXT
          SIZE 19 BY .54 AT ROW 2.96 COL 83.43 WIDGET-ID 96
          FONT 0
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.67 COL 1.43 WIDGET-ID 10
     RECT-2 AT ROW 2.71 COL 83 WIDGET-ID 98
     RECT-3 AT ROW 2.75 COL 123 WIDGET-ID 100
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.14 BY 19.67
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
         TITLE              = "Baixa de Antecipaá‰es - Customizado"
         HEIGHT             = 19.58
         WIDTH              = 129.14
         MAX-HEIGHT         = 27.5
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 27.5
         VIRTUAL-WIDTH      = 195.14
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brAntecipacao btLimpar f-cad */
/* BROWSE-TAB brtitulos fiTotVinc f-cad */
/* SETTINGS FOR BROWSE brAntecipacao IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btCancFiltrar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btLimpar IN FRAME f-cad
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON btProcessar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb_moeda IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fiAntCorr IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotTit IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotVinc IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotVincGeral IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotVincOut IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_cod_estab IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_cod_fornecedor IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_cod_refer IN FRAME f-cad
   1 3                                                                  */
/* SETTINGS FOR FILL-IN fi_data_emis_fim IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_data_emis_ini IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_desc_fornec IN FRAME f-cad
   NO-ENABLE 1                                                          */
ASSIGN 
       fi_desc_fornec:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi_dt_trans IN FRAME f-cad
   1 3                                                                  */
/* SETTINGS FOR FILL-IN fi_especie IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_parcela IN FRAME f-cad
   1 3                                                                  */
/* SETTINGS FOR FILL-IN fi_titulo IN FRAME f-cad
   1 3                                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brAntecipacao
/* Query rebuild information for BROWSE brAntecipacao
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAntec.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brAntecipacao */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brtitulos
/* Query rebuild information for BROWSE brtitulos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTitulos EXCLUSIVE-LOCK
                             WHERE ttTitulos.fornecedor      = INPUT BROWSE brAntecipacao ttAntec.fornecedor
                              AND  ttTitulos.num_linha_ant  = INPUT BROWSE brAntecipacao ttAntec.num_linha.
RUN piTotalizarTitulos(INPUT BROWSE brAntecipacao ttAntec.num_linha).
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brtitulos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Baixa de Antecipaá‰es - Customizado */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Baixa de Antecipaá‰es - Customizado */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brAntecipacao
&Scoped-define SELF-NAME brAntecipacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brAntecipacao w-livre
ON VALUE-CHANGED OF brAntecipacao IN FRAME f-cad /* Antecipaá‰es Disponiveis */
DO:
  {&OPEN-QUERY-brTitulos}
  APPLY 'value-changed' TO BROWSE brTitulos.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brtitulos
&Scoped-define SELF-NAME brtitulos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brtitulos w-livre
ON ROW-LEAVE OF brtitulos IN FRAME f-cad /* Titulos Disponiveis - Antecipaá∆o Corrente */
DO:
  RUN piTotalizarTitulos(ttAntec.num_linha:SCREEN-VALUE IN BROWSE brAntecipacao).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brtitulos w-livre
ON VALUE-CHANGED OF brtitulos IN FRAME f-cad /* Titulos Disponiveis - Antecipaá∆o Corrente */
DO:
   IF DEC(ttTitulos.vl_utiliz:SCREEN-VALUE IN BROWSE brTitulos)  = 0 THEN
       ASSIGN fiVlVinc:SCREEN-VALUE = ttTitulos.vl_saldo:SCREEN-VALUE IN BROWSE brTitulos.
   ELSE
      ASSIGN fiVlVinc:SCREEN-VALUE = ttTitulos.vl_utiliz:SCREEN-VALUE IN BROWSE brTitulos.
   IF DEC(ttAntec.vl_saldo:SCREEN-VALUE IN BROWSE brAntecipacao) < 
       DEC(ttTitulos.vl_saldo:SCREEN-VALUE IN BROWSE brTitulos) - 
      DEC(ttTitulos.vl_utiliz:SCREEN-VALUE IN BROWSE brTitulos) THEN DO:
      ASSIGN fiVlVinc:SCREEN-VALUE = ttAntec.vl_saldo:SCREEN-VALUE IN BROWSE brAntecipacao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAtu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAtu w-livre
ON CHOOSE OF btAtu IN FRAME f-cad /* Atualizar */
DO:
  ASSIGN cErro = ''.
  FIND CURRENT ttTitulos NO-ERROR.

  IF AVAIL ttTitulos THEN DO:
     /******************************************************************************************************************************
        VALIDAÄ«O  VALOR DO TITULO
        validar se o valor digitado no campo 'vl.Utilizar' n∆o Ç maior que o saldo do titulo descontando o valor utilizado em outros
        adiantamentos
      ****************************************************************************************************************************/
      IF  dec(fiVlVinc:SCREEN-VALUE IN FRAME {&FRAME-NAME})> ttTitulos.vl_saldo - ttTitulos.vl_utiliz_out THEN DO:
         ASSIGN cErro = '1- O valor digitado n∆o pode ser maior que o valor de saldo do titulo corrente!'.
      END.

      /******************************************************************************************************************************
        VALIDAÄ«O  VALOR DA ANTECIPAÄ«O
        validar se o valor digitado no campo 'vl.Utilizar' n∆o Ç maior que os valor do saldo do adiantamento descontado os valores 
        j† utilizados para a antepicaá∆o
      ****************************************************************************************************************************/
      IF dec(fiAntCorr:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - dec(fiTotVinc:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + ttTitulos.vl_utiliz - 
         DEC(fiVlVinc:SCREEN-VALUE IN FRAME {&FRAME-NAME})  < 0 THEN DO:
        ASSIGN cErro = cErro + CHR(10) + CHR(13) +  '2- O valor digitado, somado aos valores j† vinculados ultrapassa do valor da Antecipaá∆o'.
      END.
  END.
  IF cErro <> '' THEN DO:
     MESSAGE 'Foram encontrados os seguintes Erros:' skip
              cErro
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fiVlVinc IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
     
  END.
  ELSE DO:
      FOR EACH bf_ttTitulos
      WHERE bf_ttTitulos.estab          =   ttTitulos.estab       
      AND   bf_ttTitulos.especie        =   ttTitulos.especie     
      AND   bf_ttTitulos.serie          =   ttTitulos.serie       
      AND   bf_ttTitulos.titulo         =   ttTitulos.titulo      
      AND   bf_ttTitulos.parcela        =   ttTitulos.parcela     
      AND   bf_ttTitulos.fornecedor     =   ttTitulos.fornecedor  
      AND   bf_ttTitulos.num_linha      <>  ttTitulos.num_linha :
        ASSIGN bf_ttTitulos.vl_utiliz_out = bf_ttTitulos.vl_utiliz_out + DEC(fiVlVinc:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
      END.
  END.
  ASSIGN ttTitulos.vl_utiliz = DEC(fiVlVinc:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  {&OPEN-QUERY-brTitulos}
  APPLY 'value-changed' TO BROWSE brTitulos.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancFiltrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancFiltrar w-livre
ON CHOOSE OF btCancFiltrar IN FRAME f-cad /* Cancelar Filtro */
DO:
  ASSIGN btCancFiltrar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         btFiltrar:SENSITIVE IN FRAME {&FRAME-NAME}     = YES
         btProcessar:SENSITIVE IN FRAME {&FRAME-NAME}   = NO.
 /* ASSIGN cb_portador:LIST-ITEM-PAIRS = "'',''".*/
  /*ASSIGN fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}            = '0'
         fiTotalAprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '0'
         fiTotalReprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = '0'.*/
  ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  DISABLE {&list-2} WITH FRAME {&FRAME-NAME}.
  EMPTY TEMP-TABLE ttAntec.
  EMPTY TEMP-TABLE ttTitulos.
  /*EMPTY TEMP-TABLE ttAntecipacao.*/
  {&OPEN-QUERY-brTitulos}
  {&OPEN-QUERY-brAntecipacao}


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFiltrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFiltrar w-livre
ON CHOOSE OF btFiltrar IN FRAME f-cad /* Filtrar */
DO:
  RUN piVerificarCamposEmBranco(OUTPUT lCamposEmBranco).
  IF lCamposemBranco THEN
     RETURN NO-APPLY.
  RUN piSetarVariaveis.
  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  RUN pi-inicializar IN h-acomp (INPUT "Processando..").
  
  ASSIGN btCancFiltrar:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         btFiltrar:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
         btProcessar:SENSITIVE IN FRAME {&FRAME-NAME}   = lUsuarioAprovador.
  DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  RUN pi-acompanhar IN h-acomp("Buscando Antecipaá‰es/Titulos...").
  RUN piBuscarTitAnt.
  /*RUN pi-acompanhar IN h-acomp("Buscando Antecipaá‰es...").
  RUN piBuscarAntPef.*/
  {&OPEN-QUERY-brAntecipacao}
  {&OPEN-QUERY-brTitulos}

 /* MESSAGE NUM-RESULTS("brTitulos") 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  IF NUM-RESULTS("brAntecipacao") > 0 THEN DO:
     ENABLE  {&list-2} WITH FRAME {&FRAME-NAME}.
     ASSIGN brAntecipacao:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY 'value-changed':u TO BROWSE brAntecipacao.
     
  END.
  IF NUM-RESULTS("brTitulos") > 0 THEN DO:
    /* ENABLE  {&list-2} WITH FRAME {&FRAME-NAME}.*/
     ASSIGN brTitulos:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     
  END.
  RUN pi-finalizar IN h-acomp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLimpar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLimpar w-livre
ON CHOOSE OF btLimpar IN FRAME f-cad /* Limpar Filtro */
DO:
  ASSIGN /*cb_empresa:SCREEN-VALUE = '100'*/
         fi_cod_fornecedor:SCREEN-VALUE = '0'
         fi_desc_fornec:SCREEN-VALUE = ''
         fi_data_emis_ini:SCREEN-VALUE = '01/01/2001'
         fi_data_emis_fim:SCREEN-VALUE = '31/12/9999'
         fi_especie:SCREEN-VALUE = ''
         /*fi_data_lib:SCREEN-VALUE = ''*/
         /*cb_portador:SCREEN-VALUE = '1'*/ .


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btProcessar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btProcessar w-livre
ON CHOOSE OF btProcessar IN FRAME f-cad /* Btn 1 */
DO:
  RUN piEfetivarLiquidacao.
  
  ASSIGN btFiltrar:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  APPLY 'choose' TO btCancFiltrar IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cod_fornecedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cod_fornecedor w-livre
ON LEAVE OF fi_cod_fornecedor IN FRAME f-cad /* Fornecedor */
DO:
  IF fi_cod_fornecedor:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> '0' THEN DO:
     FIND FIRST emitente 
         WHERE emitente.cod-emitente = int(fi_cod_fornecedor:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         NO-LOCK NO-ERROR.
     IF AVAIL emitente THEN
        ASSIGN fi_desc_fornec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
     ELSE
        ASSIGN fi_desc_fornec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Fornecedor N∆o Encontrado".

  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cod_fornecedor w-livre
ON MOUSE-SELECT-DBLCLICK OF fi_cod_fornecedor IN FRAME f-cad /* Fornecedor */
DO:
  {include/zoomvar.i &prog-zoom  = adzoom\z01ad098.w
                     &campo      = fi_cod_fornecedor
                     &campozoom  = cod-emitente
                     &campo2     = fi_desc_fornec
                     &campozoom2 = nome-abrev
                     &FRAME = {&FRAME-NAME}}
                     
  
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


&Scoped-define BROWSE-NAME brAntecipacao
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/*ON 'leave':U OF  tt.dt_liberacao IN  BROWSE brTitulos DO:
    IF tt.situacao:SCREEN-VALUE IN BROWSE brTitulos = 'confirmado' AND tt.dt_liberacao <> INPUT BROWSE brTitulos tt.dt_liberacao THEN DO:
       MESSAGE 'Essa data n∆o pode ser alterada, pois, o titulo j† foi Pago'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       ASSIGN tt.dt_liberacao:SCREEN-VALUE IN BROWSE brTitulos = string(tt.dt_liberacao).
       APPLY 'entry' TO tt.dt_liberacao IN BROWSE brTitulos.
       RETURN NO-APPLY.
    END.
    ELSE DO:
       ASSIGN tt.alteraData = YES
              tt.dt_liberacao =  INPUT BROWSE brTitulos tt.dt_liberacao .
    END.
   
END.*/


/*
ON 'value-changed':U OF ttTitulos.vl_utiliz IN BROWSE brTitulos DO:
    
   FIND FIRST ttAntec
       WHERE  ttAntec.num_linha = INT(ttAntec.num_linha:SCREEN-VALUE IN BROWSE brAntecipacao) NO-ERROR.
   IF AVAIL ttAntec THEN DO:
      ASSIGN ttAntec.vl_utiliz = ttAntec.vl_utiliz + dec(ttTitulos.vl_utiliz:SCREEN-VALUE IN BROWSE brTitulos) - ttTitulos.vl_utiliz.
             /*fitotVinc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ttAntec.vl_utiliz).*/
      RUN piTotalizarTitulos(INT(ttAntec.num_linha:SCREEN-VALUE IN BROWSE brAntecipacao)).
   END.
   
       
    /*RETURN.*/
END.*/

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
       RUN set-position IN h_p-exihel ( 1.13 , 113.86 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiTotVinc:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY fi_cod_estab cb_moeda fi_cod_refer fi_dt_trans fi_data_emis_ini 
          fi_data_emis_fim fiAntCorr fiVlVinc fi_cod_fornecedor fi_especie 
          fi_titulo fi_parcela fi_desc_fornec fiTotTit fiTotVinc fiTotVincOut 
          fiTotVincGeral 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE fi_cod_estab cb_moeda fi_cod_refer fi_dt_trans fi_data_emis_ini 
         fi_data_emis_fim btFiltrar btAtu fiVlVinc fi_cod_fornecedor fi_especie 
         fi_titulo fi_parcela brtitulos rt-button RECT-1 RECT-2 RECT-3 
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

  {utp/ut9000.i "esapb002" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  /*RUN piSetarVariaveis.*/
  
  /*ASSIGN fi_data_vencto_ini:SCREEN-VALUE = STRING(TODAY + 1)
         fi_data_vencto_fim:SCREEN-VALUE = STRING(TODAY + 1)
         fi_data_lib:SCREEN-VALUE        = STRING(TODAY).*/
  /* Code placed here will execute AFTER standard behavior.    */
  RUN piSetarVariaveis.
  RUN piBuscarPortadores.
  RUN piVerificarPermAprovacao.
  

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAlterarData w-livre 
PROCEDURE piAlterarData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*DISABLE TRIGGERS FOR LOAD OF proces_pagto.
FOR EACH tt
    WHERE tt.alteraData = YES AND tt.rRowidProcesPagto <> ?:
    FIND proces_pagto 
        WHERE ROWID(proces_pagto) = tt.rRowidProcesPagto EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL proces_pagto THEN DO:
       ASSIGN proces_pagto.dat_liber_pagto = tt.dt_liberacao.
    END.
    RELEASE proces_pagto.
    
END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAtualizarAcao w-livre 
PROCEDURE piAtualizarAcao :
/*------------------------------------------------------------------------------
    atualiza a aá∆o do bot∆o nas linhas selecionadas
------------------------------------------------------------------------------*/
/*DEFINE INPUT  PARAMETER pAcao AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE VARIABLE l             AS LOGICAL     NO-UNDO INIT NO.
 DO i = 1 TO brTitulos:NUM-SELECTED-ROWS IN FRAME  {&FRAME-NAME}:
    
    
    brTitulos:FETCH-SELECTED-ROW(i).
    IF AVAILABLE tt THEN DO:
       IF pAcao <> "semacao" THEN DO:
          CASE tt.situacao:                                
              WHEN 'PENDENTE' OR WHEN  'estornado' THEN  DO:                   
                  IF pAcao = "aprovar" THEN              
                     ASSIGN tt.acaoAtual  = pAcao.       
                  ELSE                                   
                     ASSIGN l = YES.                     
              END.                                       
              WHEN "LIBERADO"  THEN DO :                 
                  IF pAcao = "reprovar" THEN             
                     ASSIGN tt.acaoAtual = pAcao.        
                  ELSE                                   
                     ASSIGN l = YES.                     
              END.                                       
              OTHERWISE                                  
                 ASSIGN l = YES.
          END CASE.
       END.
       ELSE
         ASSIGN tt.acaoAtual = ''.
    END.
  END.
  /*retirada a trava */
  ASSIGN l = NO.

  IF l = YES THEN
     MESSAGE 
      'ATENÄ«O' SKIP 
      'Foram atribuidas aá‰es a titulos que tem a situaá∆o incompativel ....' SKIP
      'Exemplo: tentar aprovar um titulo j† aprovado, ou reprovar um titulo que ainda nem foi aprovado' SKIP
      'A atribuiá∆o da aá∆o incompativel para esses titulos foi desconsiderada' 
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  {&OPEN-QUERY-brTitulos}*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAvaliarTitulo w-livre 
PROCEDURE piAvaliarTitulo :
/*------------------------------------------------------------------------------
Avaliar os titulos selecionados
------------------------------------------------------------------------------*/
/*titap_token*/



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarAntecipacoes w-livre 
PROCEDURE piBuscarAntecipacoes :
/*------------------------------------------------------------------------------
buscar antecipaá‰es do fornecedor do titulo colocando-as como 
disponiveis atÇ o limite do titulo corrente
------------------------------------------------------------------------------*/
/*DEFINE INPUT  PARAMETER pFornecedor AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pCodEstab   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pValorTit   AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER deValor     AS DECIMAL     NO-UNDO.
DEFINE BUFFER bf_tit_ap FOR tit_ap.
FIND FIRST ttAntecipacao
    WHERE  ttAntecipacao.cdn_fornecedor = pFornecedor
    AND    ttAntecipacao.cod_estab      = pCodEstab NO-ERROR.
IF NOT AVAIL ttAntecipacao THEN DO:
   
   CREATE ttAntecipacao.                                                                                             
   ASSIGN ttAntecipacao.cdn_fornecedor = pFornecedor
          ttAntecipacao.cod_Estab      = pCodEstab
          ttAntecipacao.valor          = 0.                                                                                   
   FOR EACH bf_tit_ap NO-LOCK                                                                                           
       WHERE bf_tit_ap.cod_estab = pCodEstab                                                                            
       AND   LOOKUP(bf_tit_ap.cod_espec_docto,cListaEspecDocto) > 0                                                     
       AND   bf_tit_ap.log_sdo_tit_ap = YES                                                                             
       AND   bf_tit_ap.cdn_fornecedor = pFornecedor USE-INDEX titap_sdo_tit_ap :   
       /*MESSAGE bf_tit_ap.cdn_fornecedor   SKIP
               bf_tit_ap.cod_espec_docto  SKIP
               bf_tit_ap.cod_tit_ap SKIP
               bf_tit_ap.val_sdo_tit_ap
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
       ASSIGN ttAntecipacao.valor = ttAntecipacao.valor + bf_tit_ap.val_sdo_tit_ap.                                     
   END.                                                                                                              
END.
IF AVAIL ttAntecipacao THEN DO:
   IF ttAntecipacao.valor > pValorTit  THEN DO:
       ASSIGN deValor = pValorTit
              ttAntecipacao.valor = ttAntecipacao.valor - pValorTit. 
   END.
   ELSE DO:
       ASSIGN deValor = ttAntecipacao.valor
              ttAntecipacao.valor = 0.
   END.
END.          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarAntPEF w-livre 
PROCEDURE piBuscarAntPEF :
/*------------------------------------------------------------------------------
  buscar todas as antecipaá‰es ainda n∆o aprovadas e Pagamentos extra fornecedores 
------------------------------------------------------------------------------*/
/*
DEFINE VARIABLE decTotAdiant        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE decVlReal           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE decVlDolar          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE decVlJuros          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cSituacao           AS CHAR        NO-UNDO INIT "PENDENTE".
DEFINE VARIABLE cHrLiberacao        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dataLiberacao       AS DATE        NO-UNDO.
DEFINE VARIABLE cUsuarioLiberacao   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE decVlCotacao        AS DECIMAL     NO-UNDO.


FOR EACH antecip_pef_pend NO-LOCK
    WHERE antecip_pef_pend.cod_estab          >= cod_estab_ini
    AND   antecip_pef_pend.cod_estab          <= cod_estab_fim
    AND   antecip_pef_pend.dat_vencto_tit_ap  >= INPUT FRAME {&frame-name} fi_data_vencto_ini
    AND   antecip_pef_pend.dat_vencto_tit_ap  <= INPUT FRAME {&frame-name} fi_data_vencto_fim
    AND   antecip_pef_pend.dat_emis_docto     >= INPUT FRAME {&frame-name} fi_data_emis_ini
    AND   antecip_pef_pend.dat_emis_docto     <= INPUT FRAME {&frame-name} fi_data_emis_fim
    AND   antecip_pef_pend.cod_espec_docto    >= cod_espec_ini
    AND   antecip_pef_pend.cod_espec_docto    <= cod_espec_fim 
    AND   antecip_pef_pend.cod_tit_ap         >= cod_titulo_ini
    AND   antecip_pef_pend.cod_tit_ap         <= cod_titulo_fim
    AND   antecip_pef_pend.cod_parcela        >= cod_parcela_ini 
    AND   antecip_pef_pend.cod_parcela        <= cod_parcela_fim:
    
    FIND FIRST emitente WHERE
        emitente.cod-emitente = antecip_pef_pend.cdn_fornecedor
        NO-LOCK NO-ERROR.


    IF (antecip_pef_pend.ind_sit_pef_antecip = 'pago' AND LOG_saldo_fim = YES)
        OR (antecip_pef_pend.ind_sit_pef_antecip <> 'pago' AND LOG_saldo_fim = NO)  THEN NEXT.
   
    IF antecip_pef_pend.cod_indic_econ = 'dolar' THEN DO:
       ASSIGN   decVlReal    = antecip_pef_pend.val_tit_ap / antecip_pef_pend.val_cotac_indic_econ
                decVlDolar   = antecip_pef_pend.val_tit_ap
                decVlCotacao =  antecip_pef_pend.val_tit_ap / antecip_pef_pend.val_cotac_indic_econ / antecip_pef_pend.val_tit_ap .
    END.
    ELSE DO:
      ASSIGN    decVlReal  = antecip_pef_pend.val_tit_ap
                decVlDolar = 0.
    END.

    FIND LAST proces_pagto 
        WHERE PROCES_PAGTO.COD_ESTAB = ANTECIP_PEF_PEND.COD_ESTAB
        AND   PROCES_PAGTO.COD_REFER_ANTECIP_PEF = ANTECIP_PEF_PEND.COD_REFER
        USE-INDEX prcspgt_antec_pef NO-LOCK NO-ERROR.
    

    /*
     caso o usuario n∆o selecione os j† aprovados ser∆o desconsiderados os registros
     j† liberados e ou confirmados
    */

    IF AVAIL proces_pagto THEN DO:
       ASSIGN cSituacao     =  proces_pagto.ind_sit_proces_pagto
              dataLiberacao =  proces_pagto.dat_liber_pagto
              cHrLiberacao  = substr(proces_pagto.hra_liber_proces_pagto,1,2) + ":" + substr(proces_pagto.hra_liber_proces_pagto,4,2) + ":" + substr(proces_pagto.hra_liber_proces_pagto,6,2)
              cUsuarioLiberacao = proces_pagto.cod_usuar_liber_pagto  .
    
       IF  (proces_pagto.ind_sit_proces_pagto    = 'liberado'
           OR proces_pagto.ind_sit_proces_pagto  =  'confirmado' 
           OR proces_pagto.ind_sit_proces_pagto  =  'Em Pagamento')
           AND INPUT FRAME {&frame-name} tg_aprovados = NO THEN
           NEXT.

       /*
        caso o usuario n∆o selecione os registros pendentes de aprovaá∆o ser∆o desconsiderados 
        os registros que n∆o est∆o liberados ou confirmados
       */
       IF  (proces_pagto.ind_sit_proces_pagto   <> 'liberado'
           OR proces_pagto.ind_sit_proces_pagto <>  'confirmado'
           OR  proces_pagto.ind_sit_proces_pagto <>  'Em Pagamento' )
           AND INPUT FRAME {&frame-name} tg_aprovar = NO THEN
           NEXT.
       
       /*
       caso o usuario que aprovou n∆o selecione os titulos aprovados por outros ser∆o desconsiderados
       os registros que n∆o foram aprovados pelo usuario corrente
       */
       IF proces_pagto.cod_usuar_liber_pagto <> c-seg-usuario AND  INPUT FRAME {&frame-name} tg_outros = NO 
           AND  (proces_pagto.ind_sit_proces_pagto   = 'liberado'
           OR proces_pagto.ind_sit_proces_pagto =  'confirmado'
           OR proces_pagto.ind_sit_proces_pagto =  'em pagamento' ) THEN
           NEXT.

    END.
    ELSE DO:
        ASSIGN cSituacao         =  'PENDENTE'
                dataLiberacao    =   INPUT FRAME {&FRAME-NAME} fi_data_lib
               cHrLiberacao      =  ''
               cUsuarioLiberacao =  '' .
       IF INPUT FRAME {&frame-name} tg_aprovar = NO THEN
           NEXT.
    END. 
    CREATE tt. 
    ASSIGN tt.situacao               = cSituacao
           tt.cod_tit_ap             = antecip_pef_pend.cod_tit_ap
           tt.cod_ser_docto          = antecip_pef_pend.cod_ser_docto       
           tt.cdn_fornecedor         = antecip_pef_pend.cdn_fornecedor      
           tt.cod_parcela            = antecip_pef_pend.cod_parcela         
           tt.dat_emis_docto         = antecip_pef_pend.dat_emis_docto      
           tt.dat_vencto_tit_ap      = antecip_pef_pend.dat_vencto_tit_ap   
           tt.val_origin_tit_ap      = antecip_pef_pend.val_tit_ap   
           tt.val_sdo_tit_ap         = decVlReal     
           tt.val_calc_adiant        = 0
           tt.val_dolar              = decVlDolar
           tt.val_desconto           = 0
           tt.val_juros_dia_atraso   = 0
           tt.val_multa              = 0
           tt.num_id_tit_ap          = 0
           tt.cod_estab              = antecip_pef_pend.cod_estab
           tt.dt_Liberacao           = dataLiberacao
           tt.hr_liberacao           = cHrLiberacao
           tt.usuario_liberacao      = cUsuarioLiberacao
           tt.RrOWID                 = ROWID(antecip_pef_pend)
           tt.val_cotacao            = decVlCotacao
           tt.cod_refer_ant_pef      = antecip_pef_pend.cod_refer
           tt.rRowidProcesPagto      = IF AVAIL proces_pagto THEN ROWID(proces_pagto) ELSE ?
           tt.nome_abrev             = IF AVAIL emitente THEN emitente.nome-abrev ELSE ''.

    
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarAprovAnt w-livre 
PROCEDURE piBuscarAprovAnt :
/*------------------------------------------------------------------------------
  ------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pRowid      AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cAprovador  AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
FIND FIRST tit_AP
    WHERE ROWID(tit_ap) = pRowid NO-LOCK NO-ERROR.
FIND LAST proces_pagto OF tit_ap NO-LOCK NO-ERROR.
IF AVAIL proces_pagto THEN
   ASSIGN cAprovador = proces_pagto.cod_usuar_liber_pagto.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarEspAnt w-livre 
PROCEDURE piBuscarEspAnt :
/*------------------------------------------------------------------------------
busca as especies que s∆o do tipo antecipaá∆o  
------------------------------------------------------------------------------*/
ASSIGN cListaEspecDocto  = ''.
FOR EACH ems5.espec_docto NO-LOCK                                                                                 
       WHERE ems5.espec_docto.ind_tip_espec_docto = "antecipaá∆o":                                                   
       ASSIGN cListaEspecDocto = IF cListaESpecDocto = '' THEN                                                       
                                 cod_espec_docto ELSE cListaEspecDocto + "," + cod_Espec_docto.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarPortadores w-livre 
PROCEDURE piBuscarPortadores :
/*------------------------------------------------------------------------------
Buscar os portadores conforme o estabelecimento informado na tela
------------------------------------------------------------------------------*/
/*DEFINE VARIABLE cComum AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
EMPTY TEMP-TABLE ttPortador.

FOR EACH portad_finalid_econ
    WHERE portad_finalid_econ.cod_estab >= cod_estab_ini
    AND   portad_finalid_econ.cod_estab <= cod_estab_fim  NO-LOCK:

    FIND FIRST ems5.portador OF portad_finalid_econ NO-LOCK NO-ERROR.
    IF AVAIL ems5.portador THEN DO:
       FIND FIRST ttPortador
           WHERE ttPortador.cod_portador = ems5.portador.cod_portador NO-LOCK NO-ERROR.
       IF NOT AVAIL ttPortador THEN DO:
          CREATE ttPortador.
          ASSIGN ttPortador.cod_portador  = portador.cod_portador.
                 ttPortador.desc_portador = portador.cod_portador + '-' +  portador.nom_abrev + "," + portador.cod_portador.
       END.
    END.
END.

FOR EACH ttPortador:
    ASSIGN  cListaPortadores = IF cListaPortadores = '' THEN ttPortador.desc_portador ELSE cListaPortadores + "," +  ttPortador.desc_portador.
END.


/**/
IF cListaPortadores <> '' THEN
   ASSIGN cb_Portador:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListaPortadores
          cb_portador:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '1'. 



/* 
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_estab                        char        im
   20 cod_portador                     char        im
   30 cod_cart_bcia                    char        im
   40 cod_finalid_econ                 char        im
   50 cod_cta_corren                   char        im
   60 val_sdo_cobr                     deci-2      m
   70 cod_livre_1                      char
   80 log_livre_1                      logi
   90 num_livre_1                      inte
  100 val_livre_1                      deci-4
  110 dat_livre_1                      date
  120 cod_livre_2                      char
  130 dat_livre_2                      date
  140 log_livre_2                      logi
  150 num_livre_2                      inte
  160 val_livre_2                      deci-4
  170 cdd_version                      deci-0





portador
 Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_portador                     char        im
   20 ind_tip_portad                   char        m
   30 num_pessoa_jurid                 inte        im
   40 nom_pessoa                       char        m
   50 cod_pais                         char        im
   60 cod_id_feder                     char        m
   70 nom_abrev                        char        m
   80 cod_banco                        char        im
   90 cod_agenc_bcia                   char        im
  100 log_agrup_tit_cobr_especial      logi
  110 log_calc_dat_vencto_cobr         logi
  120 cod_livre_1                      char
  130 log_livre_1                      logi
  140 num_livre_1                      inte
  150 val_livre_1                      deci-4
  160 dat_livre_1                      date
  170 cod_livre_2                      char
  180 dat_livre_2                      date
  190 log_livre_2                      logi
  200 num_livre_2                      inte
  210 val_livre_2                      deci-4
  220 cdd_version                      deci-0
  230 log_atualiz_previs_fluxo_cx      logi
  240 log_atualiz_realiz_fluxo_cx      logi

*/
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarTitAnt w-livre 
PROCEDURE piBuscarTitAnt :
DEFINE VARIABLE decVlReal           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE decVlDolar          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cAprovador          AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
FOR EACH tit_ap
      WHERE tit_ap.log_sdo_tit_ap = YES 
      AND   tit_ap.ind_tip_espec_docto      = 'antecipaá∆o'
      AND   tit_ap.cod_indic_econ           = cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND   tit_ap.dat_transacao            <= INPUT FRAME {&frame-name} fi_dt_trans
      AND   tit_ap.dat_transacao            >= INPUT FRAME {&frame-name} fi_data_emis_ini
      AND   tit_ap.dat_transacao            <= INPUT FRAME {&frame-name} fi_data_emis_fim
      AND   tit_ap.cod_espec_docto          >= cod_espec_ini
      AND   tit_ap.cod_espec_docto          <= cod_espec_fim 
      AND   tit_ap.cod_tit_ap               >= cod_titulo_ini
      AND   tit_ap.cod_tit_ap               <= cod_titulo_fim
      AND   tit_ap.cod_parcela              >= cod_parcela_ini 
      AND   tit_ap.cod_parcela              <= cod_parcela_fim 
      AND   tit_ap.cdn_fornec               >= cod_fornec_ini
      AND   tit_ap.cdn_fornec               <= cod_fornec_fim
      AND   tit_ap.log_tit_ap_estordo       = NO
      NO-LOCK.
    /*MESSAGE 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    FIND FIRST emitente 
        WHERE emitente.cod-emitente = tit_ap.cdn_fornecedor 
        NO-LOCK NO-ERROR.
/*
    RUN piBuscarValorTitDolar(rowid(tit_ap), OUTPUT decVlReal).
    IF tit_ap.cod_indic_econ = 'dolar' THEN DO:

       RUN piBuscarValorTitDolar(rowid(tit_ap), OUTPUT decVlReal).
       ASSIGN decVlDolar = tit_ap.val_sdo_tit_ap.
    END.
    ELSE DO:
      ASSIGN decVlReal  = tit_ap.val_sdo_tit_ap
             decVlDolar = 0.
    END.   */
    RUN piBuscarAprovAnt(ROWID(tit_ap), OUTPUT cAprovador).
    ASSIGN i = i + 1.
    CREATE ttAntec.
    ASSIGN ttAntec.estab      = tit_ap.cod_estab
           ttAntec.especie    = tit_ap.cod_espec_docto  
           ttAntec.serie      = tit_ap.cod_ser_docto   
           ttAntec.titulo     = tit_ap.cod_tit_ap  
           ttAntec.parcela    = tit_ap.cod_parcela 
           ttAntec.fornecedor = tit_ap.cdn_fornecedor
           ttAntec.nome_abrev = emitente.nome-abrev
           ttAntec.vl_saldo   = tit_ap.val_sdo_tit_ap
           ttAntec.moeda      = tit_ap.cod_indic_econ   
           ttAntec.dt_trans   = tit_ap.dat_transacao   
           ttAntec.num_linh   = i
           ttAntec.usuarAprov = cAprovador. 

    FOR EACH bf_tit_ap
         WHERE bf_tit_ap.cdn_fornec          = tit_ap.cdn_fornec
         /*AND   bf_tit_ap.dat_emis           >= tit_ap.dat_transacao*/
         AND   bf_tit_ap.cod_estab           = tit_ap.cod_estab
         AND   bf_tit_ap.cod_indic_econ      = tit_ap.cod_indic_econ
         AND   bf_tit_ap.LOG_sdo_tit_ap      = YES
         AND   bf_tit_ap.log_tit_ap_estordo  = NO 
         AND   bf_tit_ap.ind_tip_espec_docto = 'normal' NO-LOCK:
         FIND FIRST emitente 
        WHERE emitente.cod-emitente = bf_tit_ap.cdn_fornecedor 
        NO-LOCK NO-ERROR.
        /*FIND FIRST ttTitulosSaldo
             WHERE ttTitulosSaldo.estab      = bf_tit_ap.cod_estab
             AND   ttTitulosSaldo.especie    = bf_tit_ap.cod_espec_docto
             AND   ttTitulosSaldo.serie      = bf_tit_ap.cod_ser_docto
             AND   ttTitulosSaldo.titulo     = bf_tit_ap.cod_tit_ap
             AND   ttTitulosSaldo.parcela    = bf_tit_ap.cod_parcela
             AND   ttTitulosSaldo.fornecedor = bf_tit_ap.cdn_fornecedor   NO-LOCK NO-ERROR.
         IF NOT AVAIL ttTitulosSaldo THEN DO:
            CREATE  ttTitulosSaldo.
                ASSIGN 
                    ttTitulosSaldo.estab         = bf_tit_ap.cod_estab       
                    ttTitulosSaldo.especie       = bf_tit_ap.cod_espec_docto 
                    ttTitulosSaldo.serie         = bf_tit_ap.cod_ser_docto   
                    ttTitulosSaldo.titulo        = bf_tit_ap.cod_tit_ap      
                    ttTitulosSaldo.parcela       = bf_tit_ap.cod_parcela     
                    ttTitulosSaldo.fornecedor    = bf_tit_ap.cdn_fornecedor  
                    ttTitulosSaldo.nome_abrev    = emitente.nome-abrev
                    ttTitulosSaldo.vl_saldo      = bf_tit_ap.val_sdo_tit_ap
                    ttTitulosSaldo.moeda         = bf_tit_ap.cod_indic_econ
                    ttTitulosSaldo.dt_emissao    = bf_tit_ap.dat_emis
                    ttTitulosSaldo.dt_vencto     = bf_tit_ap.dat_vencto.


         END.*/
         CREATE  ttTitulos.
         ASSIGN 
             ttTitulos.estab         = bf_tit_ap.cod_estab       
             ttTitulos.especie       = bf_tit_ap.cod_espec_docto 
             ttTitulos.serie         = bf_tit_ap.cod_ser_docto   
             ttTitulos.titulo        = bf_tit_ap.cod_tit_ap      
             ttTitulos.parcela       = bf_tit_ap.cod_parcela     
             ttTitulos.fornecedor    = bf_tit_ap.cdn_fornecedor  
             ttTitulos.nome_abrev    = emitente.nome-abrev
             ttTitulos.vl_saldo      = bf_tit_ap.val_sdo_tit_ap
             ttTitulos.moeda         = bf_tit_ap.cod_indic_econ
             ttTitulos.dt_emissao    = bf_tit_ap.dat_emis
             ttTitulos.dt_vencto     = bf_tit_ap.dat_vencto
             ttTitulos.num_linha_ant = i
             ttTitulos.rRowid        = ROWID(bf_tit_ap)
             ttTitulos.usuarAProv    = cAprovador.
         
     END.
    /*CREATE tt. 
    ASSIGN /*tt.situacao               = cSituacao*/
           tt.cod_tit_ap             = tit_ap.cod_tit_ap
           tt.cod_ser_docto          = tit_ap.cod_ser_docto       
           tt.cdn_fornecedor         = tit_ap.cdn_fornecedor      
           tt.cod_parcela            = tit_ap.cod_parcela         
           tt.dat_emis_docto         = tit_ap.dat_emis_docto      
           tt.dat_vencto_tit_ap      = tit_ap.dat_vencto_tit_ap   
           tt.val_origin_tit_ap      = tit_ap.val_origin_tit_ap   
           tt.val_sdo_tit_ap         = decVlReal
           tt.val_dolar              = decVlDolar 
           tt.val_desconto           = tit_ap.val_desconto
           /*tt.val_juros_dia_atraso   = DecVlJuros*/
           tt.val_multa              = tit_ap.val_perc_multa_atraso * tit_ap.val_sdo_tit_ap
           tt.num_id_tit_ap          = tit_ap.num_id_tit_ap
           tt.cod_estab              = tit_ap.cod_estab
           tt.RRowid                 = ROWID(tit_ap)
           tt.rRowidProcesPagto      = IF AVAIL proces_pagto THEN ROWID(proces_pagto) ELSE ? 
           tt.nome_abrev             = IF AVAIL emitente THEN emitente.nome-abrev ELSE ''.*/
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarValorTitDolar w-livre 
PROCEDURE piBuscarValorTitDolar :
/*------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER rRowid AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER valor  AS DECIMAL     NO-UNDO INIT 0.

FIND FIRST tit_ap 
    WHERE ROWID(tit_ap) = rRowid NO-LOCK NO-ERROR.
IF AVAIL tit_ap THEN 
   FIND FIRST val_tit_ap OF tit_ap
    WHERE val_tit_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
   IF AVAIL val_tit_ap THEN 
      ASSIGN valor = val_tit_ap.val_sdo_tit_ap.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalcularHora w-livre 
PROCEDURE piCalcularHora :
DEFINE OUTPUT PARAMETER cHora AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hour     AS INTEGER NO-UNDO.
DEFINE VARIABLE minute   AS INTEGER NO-UNDO.
DEFINE VARIABLE sec      AS INTEGER NO-UNDO.
DEFINE VARIABLE timeleft AS INTEGER NO-UNDO. 

timeleft = (24 * 60 * 60) - TIME.   
/* seconds till next midnight */
sec = timeleft MOD 60.
timeleft = (timeleft - sec) / 60.   
/* minutes till next midnight */
minute = timeleft MOD 60. 
/* hours till next midnight */
hour = (timeleft - minute) / 60.   


ASSIGN cHora = string(hour,'99')  +  string(minute,'99') + string(sec,'99').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalcularTotais w-livre 
PROCEDURE piCalcularTotais :
/*------------------------------------------------------------------------------
  calcula os totais dos titulos que aparecem no browse conforme a coluna situaá∆o
------------------------------------------------------------------------------*/
/*DEFINE VARIABLE dTotal          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAprovadoTotal  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dReprovadoTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dQtTotal            AS INT     NO-UNDO.
DEFINE VARIABLE dQtAprovadoTotal    AS INT     NO-UNDO.
DEFINE VARIABLE dqtReprovadoTotal   AS INT     NO-UNDO.

FOR EACH tt:
    ASSIGN dTotal   = dTotal   + tt.val_sdo_tit_ap
           dQtTotal = dQtTotal + 1 . 
    CASE tt.acaoAtual:
        WHEN 'aprovar' THEN
            ASSIGN dAprovadoTotal = dAprovadoTotal + tt.val_sdo_tit_ap
                   dQtAprovadoTotal = dQtAprovadoTotal + 1.
        WHEN 'reprovar' THEN
            ASSIGN dAprovadoTotal = dAprovadoTotal + tt.val_sdo_tit_ap
                   dQtAprovadoTotal = dQtAprovadoTotal + 1.

    END CASE.
END.
ASSIGN fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dTotal)
       fiTotalAprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dAprovadoTotal)
       fiTotalReprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dReprovadoTotal)
       fiQtTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(dQtTotal)
       fiQtTotalAprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dQtAprovadoTotal)
       fiQTTotalReprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dQtReprovadoTotal).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCriarAprovacao w-livre 
PROCEDURE piCriarAprovacao :
/*------------------------------------------------------------------------------
 
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pRowid          AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER pAprovadorAnt   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE novaSequencia           AS INTEGER     NO-UNDO.
DEFINE VARIABLE cHora                   AS CHARACTER   NO-UNDO.



DISABLE TRIGGERS FOR LOAD OF proces_pagto.
RUN piCalcularHora(OUTPUT cHora).

 /*busca o ultimo processo de pagamento existente para a antecipaá∆o/PEF para numerar corretamente a sequencia*/
    FIND tit_ap
        WHERE ROWID(tit_ap) = pRowid NO-LOCK NO-ERROR.
    IF AVAIL tit_ap THEN DO:
       FIND FIRST val_tit_ap OF tit_ap
           WHERE cod_indic_econ = 'dolar' NO-LOCK NO-ERROR.
       /*busca o ultimo processo de pagamento existente para o titulo para numerar corretamente a sequencia*/
       FIND LAST proces_pagto OF tit_ap EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL proces_pagto THEN NEXT.
       CREATE proces_pagto.
       ASSIGN 
         proces_pagto.num_id_proces_pagto          =  NEXT-VALUE(seq_proces_pagto) 
         proces_pagto.cod_empresa                  =  tit_ap.cod_empresa  
         proces_pagto.cod_estab                    =  tit_ap.cod_estab  
         proces_pagto.cod_espec_docto              =  tit_ap.cod_espec_docto             
         proces_pagto.cod_ser_docto                =  tit_ap.cod_ser_docto                
         proces_pagto.cdn_fornecedor               =  tit_ap.cdn_fornecedor               
         proces_pagto.cod_tit_ap                   =  tit_ap.cod_tit_ap                   
         proces_pagto.cod_parcela                  =  tit_ap.cod_parcela 
         proces_pagto.cod_portador                 =  IF cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  'dolar' THEN 'dolar' ELSE '1'
         proces_pagto.dat_vencto_tit_ap            =  tit_ap.dat_vencto_tit_ap            
         proces_pagto.dat_prev_pagto               =  tit_ap.dat_prev_pagto 
         proces_pagto.dat_desconto                 =  tit_ap.dat_desconto 
         proces_pagto.ind_sit_proces_pagto         =  "LIBERADO"  
         proces_pagto.cod_usuar_prepar_pagto       =  c-seg-usuario
         proces_pagto.dat_prepar_pagto             =  INPUT FRAME {&frame-name} fi_dt_trans 
         proces_pagto.cod_usuar_liber_pagto        =  pAprovadorAnt
         proces_pagto.dat_liber_pagto              =  INPUT FRAME {&frame-name} fi_dt_trans
         proces_pagto.val_liberd_pagto             =  tit_ap.val_sdo_tit_ap
         proces_pagto.val_liber_pagto_orig         =  tit_ap.val_sdo_tit_ap
         proces_pagto.cod_indic_econ               =  tit_ap.cod_indic_econ    
         /*proces_pagto.cod_refer_antecip_pef        =  tit_ap.cod_refer_antecip_pef */
         proces_pagto.hra_liber_proces_pagto       =  cHora
         proces_pagto.val_cotac_indic_econ         = IF AVAIL val_tit_ap THEN  val_tit_ap.val_sdo_tit_ap / tit_ap.val_sdo_tit_ap ELSE tit_ap.val_sdo_tit_ap
         proces_pagto.num_seq_pagto_tit_ap         = 1
         proces_pagto.dat_livre_1                  = TODAY
         proces_pagto.ind_modo_pagto               = ''.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piEfetivarAcao w-livre 
PROCEDURE piEfetivarAcao :
/*------------------------------------------------------------------------------
 efetivar as aá‰es conforme a aá∆o selecionada para o registro da tabela(titulo, antecipaá∆o, P‘F)   
------------------------------------------------------------------------------*/
/*DEFINE VARIABLE cHora AS CHARACTER  NO-UNDO .
DEFINE VARIABLE novaSequencia AS INTEGER     NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF proces_pagto.
RUN piCalcularHora(OUTPUT cHora).
/*ASSIGN cHora = STRING(TIME,"hh:mm:ss").*/
FOR EACH tt:
    
    CASE acaoAtual:
        WHEN 'aprovar' THEN DO:
           IF tt.num_id_tit_ap = 0 THEN DO:
              FIND  antecip_pef_pend
                   WHERE ROWID(antecip_pef_pend) = tt.rRowid
                   NO-LOCK NO-ERROR.
              IF AVAIL antecip_pef_pend THEN DO:

                 /*busca o ultimo processo de pagamento existente para a antecipaá∆o/PEF para numerar corretamente a sequencia*/
                 FIND LAST proces_pagto 
                     WHERE antecip_pef_pend.cod_estab = proces_pagto.cod_estab
                     AND   antecip_pef_pend.cod_refer = proces_pagto.cod_refer_antecip_pef
                     USE-INDEX prcspgt_antec_pef NO-LOCK NO-ERROR.
                 IF AVAIL proces_pagto THEN
                    ASSIGN novaSequencia = proces_pagto.num_seq_pagto_tit_ap + 1.
                 ELSE
                    ASSIGN novaSequencia = 1.
                 /************************************************************************************************************/

                 CREATE proces_pagto.
                 ASSIGN 
                        proces_pagto.num_id_proces_pagto          =  NEXT-VALUE(seq_proces_pagto) 
                        proces_pagto.cod_empresa                  =  antecip_pef_pend.cod_empresa  
                        proces_pagto.cod_estab                    =  antecip_pef_pend.cod_estab  
                        proces_pagto.cod_espec_docto              =  antecip_pef_pend.cod_espec_docto             
                        proces_pagto.cod_ser_docto                =  antecip_pef_pend.cod_ser_docto                
                        proces_pagto.cdn_fornecedor               =  antecip_pef_pend.cdn_fornecedor               
                        proces_pagto.cod_tit_ap                   =  antecip_pef_pend.cod_tit_ap                   
                        proces_pagto.cod_parcela                  =  antecip_pef_pend.cod_parcela 
                        proces_pagto.cod_portador                 =  INPUT FRAME {&frame-name} cb_portador /*antecip_pef_pend.cod_portador*/                
                        proces_pagto.dat_vencto_tit_ap            =  antecip_pef_pend.dat_vencto_tit_ap            
                        proces_pagto.dat_prev_pagto               =  antecip_pef_pend.dat_prev_pagto 
                        proces_pagto.dat_desconto                 =  antecip_pef_pend.dat_desconto 
                        proces_pagto.ind_sit_proces_pagto         =  "LIBERADO"  
                        proces_pagto.cod_usuar_prepar_pagto       =  c-seg-usuario
                        proces_pagto.dat_prepar_pagto             =  IF proces_pagto.dat_prepar_pagto <> ? THEN INPUT FRAME {&frame-name} fi_data_lib ELSE proces_pagto.dat_prepar_pagto
                        proces_pagto.cod_usuar_liber_pagto        =  c-seg-usuario
                        proces_pagto.dat_liber_pagto              =  INPUT FRAME {&frame-name} fi_data_lib
                        proces_pagto.val_liberd_pagto             =  antecip_pef_pend.val_tit_ap
                        proces_pagto.val_liber_pagto_orig         =  antecip_pef_pend.val_tit_ap
                        proces_pagto.cod_indic_econ               =  antecip_pef_pend.cod_indic_econ    
                        proces_pagto.cod_refer_antecip_pef        =  antecip_pef_pend.cod_refer 
                        proces_pagto.hra_liber_proces_pagto       =  cHora
                        proces_pagto.val_cotac_indic_econ         =  antecip_pef_pend.val_cotac_indic_econ
                        proces_pagto.num_seq_pagto_tit_ap         = novaSequencia
                        proces_pagto.dat_livre_1                  = TODAY.
                       
                                                                  
              END.
           END.
           ELSE DO:
                FIND tit_ap
                    WHERE ROWID(tit_ap) = tt.rRowid NO-LOCK NO-ERROR.
                IF AVAIL tit_ap THEN DO:
                   /*busca o ultimo processo de pagamento existente para o titulo para numerar corretamente a sequencia*/
                   FIND LAST proces_pagto OF tit_ap EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAIL proces_pagto THEN 
                      ASSIGN novaSequencia = proces_pagto.num_seq_pagto_tit_ap + 1.
                   ELSE
                      ASSIGN novaSequencia = 1.
                   /************************************************************************************************************/

                   CREATE proces_pagto.
                   ASSIGN 
                        proces_pagto.num_id_proces_pagto          =  NEXT-VALUE(seq_proces_pagto) 
                        proces_pagto.cod_empresa                  =  tit_ap.cod_empresa  
                        proces_pagto.cod_estab                    =  tit_ap.cod_estab  
                        proces_pagto.cod_espec_docto              =  tit_ap.cod_espec_docto             
                        proces_pagto.cod_ser_docto                =  tit_ap.cod_ser_docto                
                        proces_pagto.cdn_fornecedor               =  tit_ap.cdn_fornecedor               
                        proces_pagto.cod_tit_ap                   =  tit_ap.cod_tit_ap                   
                        proces_pagto.cod_parcela                  =  tit_ap.cod_parcela 
                        proces_pagto.cod_portador                 = INPUT FRAME {&frame-name} cb_portador              
                        proces_pagto.dat_vencto_tit_ap            =  tit_ap.dat_vencto_tit_ap            
                        proces_pagto.dat_prev_pagto               =  tit_ap.dat_prev_pagto 
                        proces_pagto.dat_desconto                 =  tit_ap.dat_desconto 
                        proces_pagto.ind_sit_proces_pagto         =  "LIBERADO"  
                        proces_pagto.cod_usuar_prepar_pagto       =  c-seg-usuario
                        proces_pagto.dat_prepar_pagto             =  IF proces_pagto.dat_prepar_pagto <> ? THEN INPUT FRAME {&frame-name} fi_data_lib ELSE proces_pagto.dat_prepar_pagto
                        proces_pagto.cod_usuar_liber_pagto        =  c-seg-usuario
                        proces_pagto.dat_liber_pagto              =  INPUT FRAME {&frame-name} fi_data_lib
                        proces_pagto.val_liberd_pagto             =  tit_ap.val_sdo_tit_ap
                        proces_pagto.val_liber_pagto_orig         =  tit_ap.val_sdo_tit_ap
                        proces_pagto.cod_indic_econ               =  tit_ap.cod_indic_econ    
                        /*proces_pagto.cod_refer_antecip_pef        =  tit_ap.cod_refer_antecip_pef */
                        proces_pagto.hra_liber_proces_pagto       =  cHora
                        proces_pagto.val_cotac_indic_econ         =  tt.val_dolar / tt.val_sdo_tit_ap
                        proces_pagto.num_seq_pagto_tit_ap         = novaSequencia
                        proces_pagto.dat_livre_1                 = TODAY.
                END.
           END.
        END.
        WHEN 'reprovar' THEN DO:
             IF tt.num_id_tit_ap = 0 THEN DO:
                FIND  antecip_pef_pend
                   WHERE ROWID(antecip_pef_pend) = tt.rRowid
                   NO-LOCK NO-ERROR.
                IF AVAIL antecip_pef_pend THEN DO:
                   FIND LAST proces_pagto 
                      WHERE proces_pagto.cod_estab = antecip_pef_pend.cod_estab
                     AND   proces_pagto.cod_refer_antecip_pef = antecip_pef_pend.cod_refer
                     NO-LOCK NO-ERROR.
                   IF AVAIL proces_pagto THEN
                      DELETE proces_pagto.
                END.
             END.
             ELSE DO:
                FIND tit_ap
                    WHERE ROWID(tit_ap) = tt.rRowid NO-LOCK NO-ERROR.
                IF AVAIL tit_ap THEN DO:
                   FIND LAST proces_pagto OF tit_ap EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAIL proces_pagto  THEN
                      DELETE proces_pagto.
                END.
             END.
        END.
    END CASE.
END.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piEfetivarLiquidacao w-livre 
PROCEDURE piEfetivarLiquidacao :
/*------------------------------------------------------------------------------

------------------------------------------------------------------------------*/
DEFINE VARIABLE cEmpresa AS CHARACTER   NO-UNDO.
FIND FIRST estabelecimento 
    WHERE cod_estab = fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    NO-LOCK NO-ERROR.
IF AVAIL estabelecimento THEN DO:
   ASSIGN cEmpresa = estabelecimento.cod_empresa.
END.
/*MESSAGE cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/


CREATE tt_integr_apb_pagto.
ASSIGN tt_integr_apb_pagto.tta_cod_empresa                    = cEmpresa
       tt_integr_apb_pagto.tta_cod_estab_refer                = fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
       tt_integr_apb_pagto.tta_cod_refer                      = fi_cod_refer:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       /*tt_integr_apb_pagto.tta_cod_estab_bord                 =*/
       tt_integr_apb_pagto.tta_dat_transacao                  = INPUT FRAME {&frame-name} fi_dt_trans
       tt_integr_apb_pagto.tta_cod_indic_econ                 = cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME}
       /*tt_integr_apb_pagto.tta_val_tot_lote_pagto_efetd     =*/
       /*tt_integr_apb_pagto.tta_val_tot_lote_pagto_infor     = */
       /*tt_integr_apb_pagto.tta_cdn_fornecedor               =*/
       /*tt_integr_apb_pagto.tta_cdn_cliente                  = */
       tt_integr_apb_pagto.tta_cod_usuar_pagto                = c-seg-usuario
       tt_integr_apb_pagto.tta_log_enctro_cta                 = NO
       /*tt_integr_apb_pagto.tta_val_tot_liquidac_tit_acr     =*/
       /*tt_integr_apb_pagto.tta_num_bord_ap                  =*/
       /*tt_integr_apb_pagto.tta_cod_msg_inic                 =*/
       /*tt_integr_apb_pagto.tta_cod_msg_fim                  =*/
       /*tt_integr_apb_pagto.tta_log_bord_ap_escrit           = */
       /*tt_integr_apb_pagto.tta_log_bord_ap_escrit_envdo     =*/
       /*tt_integr_apb_pagto.tta_ind_tip_bord_ap              = */
       tt_integr_apb_pagto.tta_cod_finalid_econ               = IF cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  'dolar' THEN 'dolar' ELSE 'corrente'
       /*tt_integr_apb_pagto.tta_cod_cart_bcia                =*/
       /*tt_integr_apb_pagto.tta_cod_livre_1                  =
       tt_integr_apb_pagto.tta_cod_livre_2                  =
       tt_integr_apb_pagto.tta_dat_livre_1                  =
       tt_integr_apb_pagto.tta_dat_livre_2                  =
       tt_integr_apb_pagto.tta_log_livre_1                  =
       tt_integr_apb_pagto.tta_log_livre_2                  =
       tt_integr_apb_pagto.tta_num_livre_1                  =
       tt_integr_apb_pagto.tta_num_livre_2                  =
       tt_integr_apb_pagto.tta_val_livre_1                  =
       tt_integr_apb_pagto.tta_val_livre_2                  =*/
       tt_integr_apb_pagto.ttv_log_atualiz_refer              =  YES
       tt_integr_apb_pagto.ttv_log_gera_lote_parcial          =  NO
       tt_integr_apb_pagto.ttv_ind_tip_atualiz                = 'lote'
       tt_integr_apb_pagto.tta_cod_portador                   = IF cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  'dolar' THEN 'dolar' ELSE '1'
       tt_integr_apb_pagto.ttv_rec_table_parent               =  RECID(tt_integr_apb_pagto)
       /*tt_integr_apb_pagto.tta_cod_estab_ext                =
       tt_integr_apb_pagto.tta_cod_portad_ext               =
       tt_integr_apb_pagto.tta_cod_modalid_ext              =*/
       /*tt_integr_apb_pagto.tta_cod_finalid_econ_ext         =*/
       /*tt_integr_apb_pagto.ttv_log_vinc_impto_auto          = */
       .




FOR EACH ttTitulos
    WHERE ttTitulos.vl_utiliz > 0:
    IF NOT AVAIL ttAntec THEN NEXT.
    RUN piCriarAprovacao(ttTitulos.rRowid, ttTitulos.usuarAprov).
    FIND FIRST ttTitulosPago 
         WHERE ttTitulos.estab      = ttTitulosPago.estab
         AND   ttTitulos.especie    = ttTitulosPago.especie      
         AND   ttTitulos.serie      = ttTitulosPago.serie        
         AND   ttTitulos.titulo     = ttTitulosPago.titulo       
         AND   ttTitulos.parcela    = ttTitulosPago.parcela      
         AND   ttTitulos.fornecedor = ttTitulosPago.fornecedor         
         AND   ttTitulos.nome_abrev = ttTitulosPago.nome_abrev  
         AND   ttTitulos.moeda      = ttTitulosPago.moeda        
         AND   ttTitulos.dt_emissao = ttTitulosPago.dt_emissao  
         AND   ttTitulos.dt_vencto  = ttTitulosPago.dt_vencto   
        NO-LOCK NO-ERROR.    
    IF NOT AVAIL ttTitulosPago THEN DO:
       CREATE ttTitulosPago.
       ASSIGN ttTitulosPago.estab           = ttTitulos.estab      
              ttTitulosPago.especie         = ttTitulos.especie    
              ttTitulosPago.serie           = ttTitulos.serie      
              ttTitulosPago.titulo          = ttTitulos.titulo     
              ttTitulosPago.parcela         = ttTitulos.parcela    
              ttTitulosPago.fornecedor      = ttTitulos.fornecedor 
              ttTitulosPago.nome_abrev      = ttTitulos.nome_abrev 
              ttTitulosPago.moeda           = ttTitulos.moeda      
              ttTitulosPago.dt_emissao      = ttTitulos.dt_emissao 
              ttTitulosPago.dt_vencto       = ttTitulos.dt_vencto
              ttTitulosPago.rRowid          = ttTitulos.rRowid
              ttTitulosPago.num_linha_ant   = ttTitulos.num_linha_ant.
    END.
    ASSIGN ttTitulosPago.vl_pago = ttTitulosPago.vl_pago +  ttTitulos.vl_utiliz.
END.


FOR EACH ttTitulosPago:
    CREATE tt_integr_bord_lote_pagto_1.
    FIND  tit_ap 
        WHERE ROWID(tit_ap) = ttTitulosPago.rRowid
         NO-LOCK NO-ERROR.
    FIND FIRST  val_tit_ap OF tit_ap NO-LOCK NO-ERROR.
    FIND FIRST estabelecimento 
    WHERE estabelecimento.cod_estab = fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    NO-LOCK NO-ERROR.
    IF AVAIL estabelecimento THEN DO:
        ASSIGN cEmpresa = estabelecimento.cod_empresa.
    END.
   
    ASSIGN
            tt_integr_bord_lote_pagto_1.tta_cod_empresa                         = cEmpresa
            tt_integr_bord_lote_pagto_1.ttv_cod_estab_bord_refer                = fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
            tt_integr_bord_lote_pagto_1.tta_cod_refer                           = fi_cod_refer:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            tt_integr_bord_lote_pagto_1.tta_cod_portador                        = IF cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} =  'dolar' THEN 'dolar' ELSE tit_ap.cod_portador
            /*tt_integr_bord_lote_pagto_1.tta_cod_refer_antecip_pef               =*/
            tt_integr_bord_lote_pagto_1.tta_cod_estab                           = fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
            tt_integr_bord_lote_pagto_1.tta_cod_espec_docto                     = ttTitulosPago.especie
            tt_integr_bord_lote_pagto_1.tta_cod_ser_docto                       = ttTitulosPago.serie
            tt_integr_bord_lote_pagto_1.tta_cdn_fornecedor                      = ttTitulosPago.fornecedor
            tt_integr_bord_lote_pagto_1.tta_cod_tit_ap                          = ttTitulosPago.titulo
            tt_integr_bord_lote_pagto_1.tta_cod_parcela                         = ttTitulosPago.parcela
            tt_integr_bord_lote_pagto_1.tta_dat_cotac_indic_econ                = val_tit_ap.dat_cotac_indic_econ 
            tt_integr_bord_lote_pagto_1.tta_val_cotac_indic_econ                = val_tit_ap.val_cotac_indic_econ
            tt_integr_bord_lote_pagto_1.tta_val_pagto                           = ttTitulosPago.vl_pago
            tt_integr_bord_lote_pagto_1.tta_val_multa_tit_ap                    = tit_ap.val_multa_tit_ap
            tt_integr_bord_lote_pagto_1.tta_val_juros                           = tit_ap.val_juros
            tt_integr_bord_lote_pagto_1.tta_val_cm_tit_ap                       = 0
            /*tt_integr_bord_lote_pagto_1.tta_val_desc_tit_ap                     = tit_ap.desc_tit_ap*/
            tt_integr_bord_lote_pagto_1.tta_val_abat_tit_ap                     = tit_ap.val_abat_tit_ap
            tt_integr_bord_lote_pagto_1.tta_des_text_histor                     = ''
            tt_integr_bord_lote_pagto_1.ttv_rec_table_child                     = RECID(tt_integr_bord_lote_pagto_1)
            tt_integr_bord_lote_pagto_1.ttv_rec_table_parent                    = tt_integr_apb_pagto.ttv_rec_table_parent
            tt_integr_bord_lote_pagto_1.tta_cod_indic_econ                      = cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            tt_integr_bord_lote_pagto_1.tta_cod_finalid_econ                    = cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            /*tt_integr_bord_lote_pagto_1.tta_num_cheque                          = 0*/ .       
    FOR EACH ttAntec
        WHERE ttAntec.num_linha = ttTitulosPago.num_linha.
        CREATE tt_integr_apb_abat_antecip.
        ASSIGN 
        tt_integr_apb_abat_antecip.tta_cod_estab                 = ttAntec.estab
        tt_integr_apb_abat_antecip.tta_cod_espec_docto           = ttAntec.especie     
        tt_integr_apb_abat_antecip.tta_cod_ser_docto             = ttAntec.serie
        tt_integr_apb_abat_antecip.tta_cdn_fornecedor            = ttAntec.fornecedor  
        tt_integr_apb_abat_antecip.tta_cod_tit_ap                = ttAntec.titulo
        tt_integr_apb_abat_antecip.tta_cod_parcela               = ttAntec.parcela
        tt_integr_apb_abat_antecip.tta_val_abat_tit_ap           = ttAntec.vl_utiliz
        tt_integr_apb_abat_antecip.ttv_rec_integr_apb_item_lote  = tt_integr_bord_lote_pagto_1.ttv_rec_table_child .
    
    END.
END.


run prgfin/apb/apb902ze.py persistent set v_hdl_aux .
run pi_main_code_api_integr_apb_pagto_4_evo_4 in v_hdl_aux (Input 1,
                                             Input table tt_integr_apb_pagto,
                                             output table tt_log_erros_atualiz,
                                             Input table tt_integr_bord_lote_pagto_1,
                                             Input table tt_integr_apb_abat_prev,
                                             Input table tt_integr_apb_abat_antecip,
                                             Input table tt_integr_apb_impto_impl_pend,
                                             Input 'ems2',
                                             Input table tt_integr_cambio_ems5,
                                             Input table tt_1099,
                                             Input table tt_integr_apb_pagto_aux_1,
                                             Input table tt_integr_apb_bord_lote_pg_a,
                                             input-output table tt_params_generic_api).

/*************************
exportaá∆o de tabelas temporarias
****************************/

OUTPUT TO c:\temp\tt_integr_apb_pagto.txt.
FOR EACH tt_integr_apb_pagto:
    EXPORT DELIMITER "|" tt_integr_apb_pagto.
END.
OUTPUT CLOSE.

OUTPUT TO c:\temp\tt_integr_bord_lote_pagto_1.txt.
FOR EACH tt_integr_bord_lote_pagto_1:
    EXPORT DELIMITER "|" tt_integr_bord_lote_pagto_1.
END.
OUTPUT CLOSE.

OUTPUT TO c:\temp\tt_integr_apb_abat_antecip.txt.
FOR EACH tt_integr_apb_abat_antecip:
    EXPORT DELIMITER "|" tt_integr_apb_abat_antecip.
END.
OUTPUT CLOSE.






ASSIGN cArquivo = 'c:\temp\tt_log_erros_atualiz.txt'.
OUTPUT TO VALUE(cArquivo) .
FOR EACH tt_log_erros_atualiz:
    EXPORT tt_Log_erros_atualiz.
END.
OUTPUT CLOSE.
OS-COMMAND SILENT VALUE(" start notepad " + cArquivo).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piSetarVariaveis w-livre 
PROCEDURE piSetarVariaveis :
/*------------------------------------------------------------------------------
colocar valores abertos para as variaveis que se n∆o informadas ficar∆o com filtros
abertos
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE ttAntec.
EMPTY TEMP-TABLE ttTitulos.
EMPTY TEMP-TABLE tt_integr_apb_pagto.
EMPTY TEMP-TABLE ttTitulosPago.
EMPTY TEMP-TABLE tt_integr_bord_lote_pagto_1.
EMPTY TEMP-TABLE tt_integr_apb_abat_antecip.

/*IF cb_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '100' THEN*/
   ASSIGN cod_estab_ini    = fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
          cod_estab_fim    = fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
/*ELSE
  ASSIGN cod_estab_ini = '501'
         cod_estab_fim = '599'.*/

IF fi_cod_fornecedor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0' THEN
   ASSIGN cod_fornec_ini   = 0
          cod_fornec_fim   = 999999999.
ELSE
   ASSIGN cod_fornec_ini   = int(fi_cod_fornecedor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) 
          cod_fornec_fim   = int(fi_cod_fornecedor:SCREEN-VALUE IN FRAME {&FRAME-NAME}) .

IF fi_especie:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cod_espec_ini    = ''
          cod_espec_fim    = 'zzz' .
ELSE
  ASSIGN cod_espec_ini = fi_especie:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         cod_espec_fim = fi_especie:SCREEN-VALUE IN FRAME {&FRAME-NAME}.


IF fi_titulo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cod_titulo_ini = ''
          cod_titulo_fim = 'zzzzzzzzzzzzzzzz'.
ELSE
   ASSIGN cod_titulo_ini = fi_titulo:SCREEN-VALUE IN FRAME {&FRAME-NAME}
          cod_titulo_fim = fi_titulo:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

IF fi_parcela:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cod_parcela_ini = ''
          cod_parcela_fim = 'zzz'.
ELSE
   ASSIGN cod_parcela_ini = fi_parcela:SCREEN-VALUE IN FRAME {&FRAME-NAME}
          cod_parcela_fim = fi_parcela:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
/*
MESSAGE  "estab:" cod_estab_ini                                
         cod_estab_fim SKIP
         "fornec:" cod_fornec_ini cod_fornec_fim SKIP
         /*"data vencto:" INPUT FRAME {&frame-name} fi_data_vencto_ini 
         INPUT FRAME {&frame-name} fi_data_vencto_fim SKIP*/
         "data emissao:" INPUT FRAME {&frame-name} fi_data_emis_ini   
         INPUT FRAME {&frame-name} fi_data_emis_fim   SKIP
         /*"log saldo:" log_saldo_ini                                
         log_saldo_fim                                SKIP*******/
         "titulo:" cod_titulo_ini                               
         cod_titulo_fim                               SKIP
         "parcela:" cod_parcela_ini                       
         cod_parcela_fim                              SKIP
         "especie:" cod_espec_ini                     SKIP
         cod_espec_fim                                SKIP

        VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piTotalizarTitulos w-livre 
PROCEDURE piTotalizarTitulos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pLinha  AS INTEGER           NO-UNDO.
DEFINE VARIABLE dTotalTit       AS DECIMAL           NO-UNDO.
DEFINE VARIABLE dTotalVinc      AS DECIMAL           NO-UNDO.
DEFINE VARIABLE dTotalVincOut   AS DECIMAL           NO-UNDO.
DEFINE VARIABLE dTotalVincGeral AS DECIMAL           NO-UNDO.

FOR EACH ttTitulos
    WHERE ttTitulos.num_linha_ant = pLinha.
    ASSIGN dTotalTit        = dTotalTit         + ttTitulos.vl_saldo
           dTotalVinc       = dTotalVinc        + ttTitulos.vl_utiliz
           dTotalVIncOut    = dTotalVincOut     + ttTitulos.vl_utiliz_out
           dTotalVincGeral  = dTotalVincGeral   + ttTitulos.vl_utiliz + ttTitulos.vl_utiliz_out.
END.

ASSIGN fiTotTit:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = STRING(dTotalTit)
       fiTotVinc:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(dTotalVinc)
       fiTotVincOut:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = STRING(dTotalVincOut)
       fiTotVincGeral:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = STRING(dTotalVincGeral).
ASSIGN fiAntCorr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ttAntec.vl_saldo:SCREEN-VALUE IN BROWSE brAntecipacao.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piVerificarCamposEmBranco w-livre 
PROCEDURE piVerificarCamposEmBranco :
DEFINE OUTPUT PARAMETER l AS LOGICAL     NO-UNDO INIT NO.

DEFINE VARIABLE cCampos AS CHARACTER   NO-UNDO INIT ''.

IF fi_cod_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cCampos = 'Estab.'.
IF fi_data_emis_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cCampos = cCampos + ',Data Emiss∆o Inicial'.
IF fi_data_emis_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cCampos = cCampos + ',Data Emiss∆o Final'.
IF fi_cod_refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cCampos = cCampos + ',Referància'.
IF fi_dt_trans:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cCampos = cCampos + ',Data Transaá∆o'.
IF cb_moeda:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '' THEN
   ASSIGN cCampos = cCampos + ',Moeda'.
IF cCampos <> '' THEN DO:
   MESSAGE " Os campos Abaixo n∆o foram preenchidos:" SKIP
        cCampos
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
   ASSIGN l = YES.
   
END.
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piVerificarPermAprovacao w-livre 
PROCEDURE piVerificarPermAprovacao :
/*------------------------------------------------------------------------------
  verifica se o usuario corrrente tem a autorizaá∆o para fazer aprovaá‰es
------------------------------------------------------------------------------*/

FIND FIRST usuar_financ_estab_apb
    WHERE cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
IF AVAIL usuar_financ_estab_apb THEN
   ASSIGN lUsuarioAprovador = usuar_financ_estab_apb.LOG_habilit_liber_tit_ap.


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
  {src/adm/template/snd-list.i "ttTitulos"}
  {src/adm/template/snd-list.i "ttAntec"}

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

