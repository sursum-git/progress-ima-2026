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
{include/i-prgvrs.i ESAPB001 5.06.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE  TEMP-TABLE tt
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
    FIELD alteraData            AS LOGICAL INIT NO
    FIELD codtpFluxo            AS CHAR FORMAT 'x(15)'
    FIELD descTpFluxo           AS CHAR FORMAT 'x(60)'.

DEFINE TEMP-TABLE ttPortador
    FIELD cod_portador AS CHAR FORMAT 'x(20)'
    FIELD desc_portador AS CHAR FORMAT 'x(100)'.

DEFINE TEMP-TABLE ttAntecipacao
    FIELD cod_estab AS CHAR
    FIELD cdn_fornecedor AS INT
    FIELD valor AS DECIMAL.

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
DEFINE VARIABLE i                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cListaEspecDocto    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-Acomp             AS HANDLE      NO-UNDO.
DEFINE VARIABLE lUsuarioAprovador   AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE h_esbofin328a       AS HANDLE      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad
&Scoped-define BROWSE-NAME brTitulos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt

/* Definitions for BROWSE brTitulos                                     */
&Scoped-define FIELDS-IN-QUERY-brTitulos tt.acaoAtual tt.cod_estab tt.situacao /*VIEW-AS COMBO-BOX LIST-ITEM-PAIRS "Nenhuma",0,"Aprovar",1,"Reprovar",2 */ tt.cod_tit_ap tt.cod_ser_docto tt.cdn_fornecedor tt.nome_abrev tt.cod_parcela tt.cod_refer_ant_pef tt.dat_emis_docto tt.dat_vencto_tit_ap tt.val_origin_tit_ap tt.val_sdo_tit_ap tt.dt_Liberacao tt.val_calc_adiant tt.val_dolar tt.val_desconto tt.val_juros_dia_atraso tt.hr_Liberacao tt.usuario_liberacao tt.codTpFluxo tt.desctpFluxo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brTitulos tt.dt_liberacao   
&Scoped-define ENABLED-TABLES-IN-QUERY-brTitulos tt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brTitulos tt
&Scoped-define SELF-NAME brTitulos
&Scoped-define QUERY-STRING-brTitulos FOR EACH tt. /*ASSIGN fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(decimal(fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + dec(tt.val_sdo_tit_ap:SCREEN-VALUE IN BROWSE brTitulos).*/  RUN piCalcularTotais
&Scoped-define OPEN-QUERY-brTitulos OPEN QUERY {&SELF-NAME} FOR EACH tt. /*ASSIGN fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(decimal(fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + dec(tt.val_sdo_tit_ap:SCREEN-VALUE IN BROWSE brTitulos).*/  RUN piCalcularTotais.
&Scoped-define TABLES-IN-QUERY-brTitulos tt
&Scoped-define FIRST-TABLE-IN-QUERY-brTitulos tt


/* Definitions for FRAME f-cad                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-f-cad ~
    ~{&OPEN-QUERY-brTitulos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb_empresa btLimpar btSemAcao btNenhum ~
btTodos btAprovar btReprovar btFiltrar cb_data fi_data_emis_ini ~
fi_data_emis_fim fi_cod_fornecedor fi_especie tg_aprovar tg_aprovados ~
cb_portador fi_titulo fi_parcela fi_data_Lib rt-button RECT-1 RECT-3 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS cb_empresa fiTotalReprovado cb_data ~
fi_data_vencto_ini fi_data_vencto_fim fi_data_emis_ini fi_data_emis_fim ~
fi_cod_fornecedor fi_especie tg_aprovar tg_aprovados tg_pagos tg_outros ~
fi_desc_fornec cb_portador fiTotal fi_titulo fi_parcela fi_data_Lib ~
fiTotalAprovado fiQtTotalReprovado fiQtTotal fiQtTotalAprovado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cb_empresa btLimpar cb_data fi_data_emis_ini ~
fi_data_emis_fim fi_cod_fornecedor fi_especie tg_aprovar tg_aprovados ~
tg_pagos tg_outros fi_desc_fornec cb_portador fi_titulo fi_parcela ~
fi_data_Lib 
&Scoped-define List-2 btexcel 
&Scoped-define List-3 fi_data_vencto_ini fi_data_vencto_fim fi_titulo ~
fi_parcela 

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
DEFINE BUTTON btAprovar  NO-FOCUS FLAT-BUTTON
     LABEL "Aprovar" 
     SIZE 9 BY 1.13.

DEFINE BUTTON btCancFiltrar 
     IMAGE-UP FILE "image/toolbar/im-reini.bmp":U
     IMAGE-DOWN FILE "image/toolbar/im-reini.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-reini.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancelar Filtro" 
     SIZE 4.86 BY 1.25 TOOLTIP "Cancelar Filtro".

DEFINE BUTTON btexcel 
     IMAGE-UP FILE "image/toolbar/im-prigr.bmp":U
     IMAGE-DOWN FILE "image/toolbar/im-prigr.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-prigr.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Btn 1" 
     SIZE 4.86 BY 1.25.

DEFINE BUTTON btFiltrar 
     IMAGE-UP FILE "image/toolbar/im-fil.bmp":U
     IMAGE-DOWN FILE "image/toolbar/im-fil.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-fil.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Filtrar" 
     SIZE 4.86 BY 1.25.

DEFINE BUTTON btLimpar  NO-FOCUS FLAT-BUTTON
     LABEL "Limpar Filtro" 
     SIZE 17.43 BY 1.25.

DEFINE BUTTON btNenhum  NO-FOCUS FLAT-BUTTON
     LABEL "Nenhum" 
     SIZE 9 BY 1.13.

DEFINE BUTTON btProcessar 
     IMAGE-UP FILE "image/toolbar/im-chck3.bmp":U
     IMAGE-DOWN FILE "image/toolbar/im-chck3.bmp":U
     IMAGE-INSENSITIVE FILE "image/toolbar/ii-chck1.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Btn 1" 
     SIZE 4.86 BY 1.25.

DEFINE BUTTON btReprovar  NO-FOCUS FLAT-BUTTON
     LABEL "Reprovar" 
     SIZE 9 BY 1.13.

DEFINE BUTTON btSemAcao  NO-FOCUS FLAT-BUTTON
     LABEL "Sem Aá∆o" 
     SIZE 9 BY 1.13.

DEFINE BUTTON btTodos  NO-FOCUS FLAT-BUTTON
     LABEL "Todos" 
     SIZE 9 BY 1.13.

DEFINE VARIABLE cb_data AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Vencto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Amanh∆",1,
                     "Hoje",2,
                     "A partir de Hoje",3,
                     "A partir de Amanh∆",4,
                     "Informado",5
     DROP-DOWN-LIST
     SIZE 16.57 BY 1 NO-UNDO.

DEFINE VARIABLE cb_empresa AS CHARACTER FORMAT "X(4)":U INITIAL "500" 
     LABEL "Emp." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "IMA","100",
                     "MED","500"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cb_portador AS CHARACTER FORMAT "X(256)":U 
     LABEL "Portador" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23.86 BY 1 DROP-TARGET NO-UNDO.

DEFINE VARIABLE fiQtTotal AS INTEGER FORMAT ">,>>>,>>9":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fiQtTotalAprovado AS INTEGER FORMAT ">>>>>>9":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fiQtTotalReprovado AS INTEGER FORMAT ">,>>>,>>9":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fiTotal AS DECIMAL FORMAT ">>>,>>>,>>9.99":R INITIAL 0 
     LABEL "Total T°tulos" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fiTotalAprovado AS DECIMAL FORMAT ">>>,>>>,>>9.99":R INITIAL 0 
     LABEL "Aprovar" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fiTotalReprovado AS DECIMAL FORMAT ">>>,>>>,>>9.99":R INITIAL 0 
     LABEL "Reprovar" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE fi_cod_fornecedor AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Fornec." 
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi_data_emis_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_data_emis_ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Data Emiss∆o" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_data_Lib AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Liberaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_data_vencto_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "AtÇ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_data_vencto_ini AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Vencto" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi_desc_fornec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28.72 BY .88 NO-UNDO.

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
     SIZE 96.57 BY 5.58.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.57 BY 5.58.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 135.43 BY 1.5.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 136 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tg_aprovados AS LOGICAL INITIAL no 
     LABEL "J† Aprovados" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE tg_aprovar AS LOGICAL INITIAL yes 
     LABEL "Pendentes Aprovaá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE tg_outros AS LOGICAL INITIAL no 
     LABEL "Aprovados Por Outros" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE tg_pagos AS LOGICAL INITIAL no 
     LABEL "Considerar Titulos Sem Saldo" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.43 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brTitulos FOR 
      tt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brTitulos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brTitulos w-livre _FREEFORM
  QUERY brTitulos DISPLAY
      tt.acaoAtual              COLUMN-LABEL "Aá∆o"
 tt.cod_estab              COLUMN-LABEL "Estab."
 tt.situacao               COLUMN-LABEL "Situaá∆o" /*VIEW-AS COMBO-BOX LIST-ITEM-PAIRS "Nenhuma",0,"Aprovar",1,"Reprovar",2              */
 tt.cod_tit_ap             COLUMN-LABEL "Titulo"  FORMAT 'x(15)'      
 tt.cod_ser_docto          COLUMN-LABEL "SÇrie"   
 tt.cdn_fornecedor         COLUMN-LABEL "Fornecedor" 
 tt.nome_abrev             COLUMN-LABEL "Nome Abrev."  
 tt.cod_parcela            COLUMN-LABEL "Parcela"   
 tt.cod_refer_ant_pef      COLUMN-LABEL "Referància"
 tt.dat_emis_docto         COLUMN-LABEL "Dt.Emiss∆o"   
 tt.dat_vencto_tit_ap      COLUMN-LABEL "Dt.Vencto"   
 tt.val_origin_tit_ap      COLUMN-LABEL "Vl.Original"   
 tt.val_sdo_tit_ap         COLUMN-LABEL "Vl.Saldo"   
 tt.dt_Liberacao           COLUMN-LABEL "Dt.Liber."
 tt.val_calc_adiant        COLUMN-LABEL "Adiant." 
 tt.val_dolar              COLUMN-LABEL "Vl.Dolar"   
 tt.val_desconto           COLUMN-LABEL "Vl.Desconto"   
 tt.val_juros_dia_atraso   COLUMN-LABEL "Vl.Juros"
 tt.hr_Liberacao           COLUMN-LABEL "Hr.Liber."
 tt.usuario_liberacao      COLUMN-LABEL "Usu†rio" FORMAT "x(15)"
 tt.codTpFluxo             COLUMN-LABEL "Tp.Fluxo" FORMAT "x(15)"
 tt.desctpFluxo            COLUMN-LABEL "Descr.Tp.Fluxo" 
ENABLE tt.dt_liberacao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 134.43 BY 11.5
         FONT 1
         TITLE "Titulos do Contas a Pagar" ROW-HEIGHT-CHARS .58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     btCancFiltrar AT ROW 1.13 COL 6.29 WIDGET-ID 40 NO-TAB-STOP 
     btProcessar AT ROW 1.13 COL 11.43 WIDGET-ID 44
     cb_empresa AT ROW 4.5 COL 5.72 COLON-ALIGNED WIDGET-ID 84
     btexcel AT ROW 1.13 COL 34 WIDGET-ID 80
     btLimpar AT ROW 1.13 COL 16.43 WIDGET-ID 82
     btSemAcao AT ROW 20.54 COL 20.29 WIDGET-ID 72
     btNenhum AT ROW 20.54 COL 29.29 WIDGET-ID 60
     btTodos AT ROW 20.54 COL 38.14 WIDGET-ID 64
     btAprovar AT ROW 20.54 COL 2.43 WIDGET-ID 56
     btReprovar AT ROW 20.54 COL 11.43 WIDGET-ID 58
     btFiltrar AT ROW 1.13 COL 1.14 WIDGET-ID 36 NO-TAB-STOP 
     fiTotalReprovado AT ROW 20.67 COL 92.86 COLON-ALIGNED WIDGET-ID 46
     cb_data AT ROW 3.5 COL 5.43 COLON-ALIGNED WIDGET-ID 30
     fi_data_vencto_ini AT ROW 3.5 COL 32.86 COLON-ALIGNED WIDGET-ID 8
     fi_data_vencto_fim AT ROW 3.5 COL 53.86 COLON-ALIGNED WIDGET-ID 6
     fi_data_emis_ini AT ROW 4.54 COL 32.86 COLON-ALIGNED WIDGET-ID 2
     fi_data_emis_fim AT ROW 4.54 COL 53.86 COLON-ALIGNED WIDGET-ID 4
     fi_cod_fornecedor AT ROW 5.67 COL 6 COLON-ALIGNED WIDGET-ID 12
     fi_especie AT ROW 5.63 COL 54 COLON-ALIGNED WIDGET-ID 18
     tg_aprovar AT ROW 3.38 COL 72.72 WIDGET-ID 32
     tg_aprovados AT ROW 4.25 COL 72.86 WIDGET-ID 34
     tg_pagos AT ROW 6 COL 72.86 WIDGET-ID 42
     tg_outros AT ROW 5.13 COL 72.72 WIDGET-ID 38
     fi_desc_fornec AT ROW 5.67 COL 18.43 COLON-ALIGNED NO-LABEL WIDGET-ID 14 NO-TAB-STOP 
     brTitulos AT ROW 8.5 COL 2 WIDGET-ID 200
     cb_portador AT ROW 5.25 COL 109.57 COLON-ALIGNED WIDGET-ID 26
     fiTotal AT ROW 20.67 COL 118 COLON-ALIGNED WIDGET-ID 50
     fi_titulo AT ROW 6.75 COL 6 COLON-ALIGNED WIDGET-ID 52
     fi_parcela AT ROW 6.75 COL 24.72 COLON-ALIGNED WIDGET-ID 54
     fi_data_Lib AT ROW 4.17 COL 109.57 COLON-ALIGNED WIDGET-ID 62
     fiTotalAprovado AT ROW 20.71 COL 69.72 COLON-ALIGNED WIDGET-ID 66
     fiQtTotalReprovado AT ROW 20.67 COL 102.14 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     fiQtTotal AT ROW 20.67 COL 127.14 COLON-ALIGNED NO-LABEL WIDGET-ID 74
     fiQtTotalAprovado AT ROW 20.71 COL 79 COLON-ALIGNED NO-LABEL WIDGET-ID 76
     "Filtros" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 2.79 COL 1.86 WIDGET-ID 20
          FONT 0
     "Liberaá∆o" VIEW-AS TEXT
          SIZE 13 BY .54 AT ROW 2.79 COL 99 WIDGET-ID 70
          FONT 0
     rt-button AT ROW 1 COL 1
     RECT-1 AT ROW 2.67 COL 1.43 WIDGET-ID 10
     RECT-3 AT ROW 20.29 COL 1.72 WIDGET-ID 86
     RECT-2 AT ROW 2.67 COL 98.43 WIDGET-ID 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136.43 BY 20.88
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
         TITLE              = "Aprovaá∆o de Titulos Para Pagamento"
         HEIGHT             = 20.88
         WIDTH              = 136.43
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brTitulos fi_desc_fornec f-cad */
/* SETTINGS FOR BROWSE brTitulos IN FRAME f-cad
   NO-ENABLE                                                            */
ASSIGN 
       brTitulos:NUM-LOCKED-COLUMNS IN FRAME f-cad     = 8
       brTitulos:COLUMN-RESIZABLE IN FRAME f-cad       = TRUE.

/* SETTINGS FOR BUTTON btCancFiltrar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btexcel IN FRAME f-cad
   NO-ENABLE 2                                                          */
ASSIGN 
       btexcel:HIDDEN IN FRAME f-cad           = TRUE.

/* SETTINGS FOR BUTTON btLimpar IN FRAME f-cad
   1                                                                    */
ASSIGN 
       btLimpar:HIDDEN IN FRAME f-cad           = TRUE.

/* SETTINGS FOR BUTTON btProcessar IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb_data IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cb_empresa IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR COMBO-BOX cb_portador IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fiQtTotal IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQtTotalAprovado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiQtTotalReprovado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotal IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotalAprovado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotalReprovado IN FRAME f-cad
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_cod_fornecedor IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_data_emis_fim IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_data_emis_ini IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_data_Lib IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_data_vencto_fim IN FRAME f-cad
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_data_vencto_ini IN FRAME f-cad
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi_desc_fornec IN FRAME f-cad
   NO-ENABLE 1                                                          */
ASSIGN 
       fi_desc_fornec:READ-ONLY IN FRAME f-cad        = TRUE.

/* SETTINGS FOR FILL-IN fi_especie IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_parcela IN FRAME f-cad
   1 3                                                                  */
/* SETTINGS FOR FILL-IN fi_titulo IN FRAME f-cad
   1 3                                                                  */
/* SETTINGS FOR TOGGLE-BOX tg_aprovados IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg_aprovar IN FRAME f-cad
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg_outros IN FRAME f-cad
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TOGGLE-BOX tg_pagos IN FRAME f-cad
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brTitulos
/* Query rebuild information for BROWSE brTitulos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt.
/*ASSIGN fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(decimal(fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + dec(tt.val_sdo_tit_ap:SCREEN-VALUE IN BROWSE brTitulos).*/
 RUN piCalcularTotais.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brTitulos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Aprovaá∆o de Titulos Para Pagamento */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Aprovaá∆o de Titulos Para Pagamento */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAprovar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAprovar w-livre
ON CHOOSE OF btAprovar IN FRAME f-cad /* Aprovar */
DO:
  RUN piAtuaLizarAcao('Aprovar').
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
  ASSIGN fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME}            = '0'
         fiTotalAprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '0'
         fiTotalReprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = '0'
         .
         .
  ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  DISABLE {&list-2} WITH FRAME {&FRAME-NAME}.
  EMPTY TEMP-TABLE tt.
  EMPTY TEMP-TABLE ttAntecipacao.
  {&OPEN-QUERY-brTitulos}


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFiltrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFiltrar w-livre
ON CHOOSE OF btFiltrar IN FRAME f-cad /* Filtrar */
DO:
  IF INPUT FRAME {&FRAME-NAME} fi_data_lib = ? AND lUsuarioAprovador = YES AND INPUT FRAME {&frame-name} tg_aprovar = YES THEN DO:
      MESSAGE 'Informe a data de liberaá∆o'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO fi_data_lib IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
  END.
  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  RUN pi-inicializar IN h-acomp (INPUT "Processando..").
  RUN piSetarVariaveis.
  ASSIGN btCancFiltrar:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         btFiltrar:SENSITIVE IN FRAME {&FRAME-NAME}     = NO
         btProcessar:SENSITIVE IN FRAME {&FRAME-NAME}   = lUsuarioAprovador.
  DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  RUN pi-acompanhar IN h-acomp("Buscando Titulos...").
  RUN piBuscarTitulos.
  RUN pi-acompanhar IN h-acomp("Buscando Antecipaá‰es...").
  RUN piBuscarAntPef.
  {&OPEN-QUERY-brTitulos}
 /* MESSAGE NUM-RESULTS("brTitulos") 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  IF NUM-RESULTS("brTitulos") > 0 THEN DO:
     ENABLE  {&list-2} WITH FRAME {&FRAME-NAME}.
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
  ASSIGN cb_empresa:SCREEN-VALUE = '100'
         fi_cod_fornecedor:SCREEN-VALUE = '0'
         fi_desc_fornec:SCREEN-VALUE = ''
         fi_data_emis_ini:SCREEN-VALUE = '01/01/2001'
         fi_data_emis_fim:SCREEN-VALUE = '31/12/9999'
         fi_especie:SCREEN-VALUE = ''
         fi_data_lib:SCREEN-VALUE = ''
         cb_portador:SCREEN-VALUE = '1'.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btNenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btNenhum w-livre
ON CHOOSE OF btNenhum IN FRAME f-cad /* Nenhum */
DO:
  {&BROWSE-NAME}:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btProcessar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btProcessar w-livre
ON CHOOSE OF btProcessar IN FRAME f-cad /* Btn 1 */
DO:
  RUN piEfetivarAcao.
  RUN piAlterarData.
  ASSIGN btFiltrar:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
  APPLY 'choose' TO btFiltrar IN FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btReprovar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btReprovar w-livre
ON CHOOSE OF btReprovar IN FRAME f-cad /* Reprovar */
DO:
  RUN piAtuaLizarAcao('Reprovar').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSemAcao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSemAcao w-livre
ON CHOOSE OF btSemAcao IN FRAME f-cad /* Sem Aá∆o */
DO:
  RUN piAtuaLizarAcao('semacao').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTodos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTodos w-livre
ON CHOOSE OF btTodos IN FRAME f-cad /* Todos */
DO:
  DO i = 1 TO NUM-RESULTS("brTitulos"):
     reposition brTitulos to ROW(i).
     brTitulos:SELECT-FOCUSED-ROW().
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb_data
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb_data w-livre
ON VALUE-CHANGED OF cb_data IN FRAME f-cad /* Vencto */
DO:
  CASE cb_data:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
      WHEN '1' THEN DO:
          ASSIGN fi_data_vencto_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY + 1)
                 fi_data_vencto_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY + 1).
          DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.

      END.
      WHEN '2' THEN DO:
          ASSIGN fi_data_vencto_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY )
                 fi_data_vencto_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
           DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.
      END.
      WHEN '3' THEN DO:
          ASSIGN fi_data_vencto_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
                 fi_data_vencto_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING('31/12/9999').
           DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.
      END.
      WHEN '4' THEN DO:
          ASSIGN fi_data_vencto_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY + 1)
                 fi_data_vencto_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING('31/12/9999').
           DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.
      END.
      WHEN '5' THEN DO:
          ASSIGN fi_data_vencto_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
                 fi_data_vencto_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING('31/12/9999').
           ENABLE {&list-3} WITH FRAME {&FRAME-NAME}.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cod_fornecedor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cod_fornecedor w-livre
ON LEAVE OF fi_cod_fornecedor IN FRAME f-cad /* Fornec. */
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
ON MOUSE-SELECT-DBLCLICK OF fi_cod_fornecedor IN FRAME f-cad /* Fornec. */
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


&Scoped-define SELF-NAME tg_aprovados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg_aprovados w-livre
ON VALUE-CHANGED OF tg_aprovados IN FRAME f-cad /* J† Aprovados */
DO:
 
  ASSIGN tg_outros:SENSITIVE IN FRAME {&FRAME-NAME}     = LOGICAL(SELF:SCREEN-VALUE)
         tg_outros:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = SELF:SCREEN-VALUE
         tg_pagos:SENSITIVE IN FRAME {&FRAME-NAME}     = LOGICAL(SELF:SCREEN-VALUE)
         tg_pagos:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = SELF:SCREEN-VALUE.
 

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brTitulos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

ON 'leave':U OF  tt.dt_liberacao IN  BROWSE brTitulos DO:
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
   
END.

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
       RUN set-position IN h_p-exihel ( 1.13 , 120.57 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiTotalReprovado:HANDLE IN FRAME f-cad , 'AFTER':U ).
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
  DISPLAY cb_empresa fiTotalReprovado cb_data fi_data_vencto_ini 
          fi_data_vencto_fim fi_data_emis_ini fi_data_emis_fim fi_cod_fornecedor 
          fi_especie tg_aprovar tg_aprovados tg_pagos tg_outros fi_desc_fornec 
          cb_portador fiTotal fi_titulo fi_parcela fi_data_Lib fiTotalAprovado 
          fiQtTotalReprovado fiQtTotal fiQtTotalAprovado 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE cb_empresa btLimpar btSemAcao btNenhum btTodos btAprovar btReprovar 
         btFiltrar cb_data fi_data_emis_ini fi_data_emis_fim fi_cod_fornecedor 
         fi_especie tg_aprovar tg_aprovados cb_portador fi_titulo fi_parcela 
         fi_data_Lib rt-button RECT-1 RECT-3 RECT-2 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTpFluxo w-livre 
PROCEDURE getTpFluxo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pOrigem     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRowid      AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER codFluxo    AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER descrFluxo  AS CHARACTER   NO-UNDO.

     CASE pOrigem:
         WHEN 'titulo' THEN DO:
            FIND tit_ap NO-LOCK
            WHERE ROWID(tit_ap) = tt.rRowid NO-ERROR.
            IF AVAIL tit_ap THEN DO:
               FIND FIRST val_tit_ap OF tit_ap NO-LOCK NO-ERROR.
               IF  AVAIL val_tit_ap THEN DO:
                  FIND tip_fluxo_financ OF val_tit_ap NO-LOCK NO-ERROR.
                  ASSIGN codFluxo       =  val_tit_ap.cod_tip_fluxo_finan 
                         descrFluxo     =  IF AVAIL tip_fluxo_financ THEN tip_fluxo_financ.des_tip_fluxo_financ
                                           ELSE ''   . 
               END.
            END.
         END.
         WHEN 'pef_antecipacao' THEN DO:
             FIND antecip_pef_pend NO-LOCK
                WHERE ROWID(antecip_pef_pend) = tt.rRowid NO-ERROR.
             IF AVAIL antecip_pef_pend THEN DO:
                FIND FIRST aprop_ctbl_pend_ap OF antecip_pef_pend NO-LOCK NO-ERROR.
                IF  AVAIL aprop_ctbl_pend_ap THEN DO:
                  FIND tip_fluxo_financ OF aprop_ctbl_pend_ap NO-LOCK NO-ERROR.
                  ASSIGN codFluxo       =  aprop_ctbl_pend_ap.cod_tip_fluxo_finan 
                         descrFluxo     =  IF AVAIL tip_fluxo_financ THEN tip_fluxo_financ.des_tip_fluxo_financ
                                           ELSE ''   . 
                END.
              END.
         END.
     END CASE.
     
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

  {utp/ut9000.i "esapb001" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  /*RUN piSetarVariaveis.*/
  
  ASSIGN fi_data_vencto_ini:SCREEN-VALUE = STRING(TODAY + 1)
         fi_data_vencto_fim:SCREEN-VALUE = STRING(TODAY + 1)
         fi_data_lib:SCREEN-VALUE        = STRING(TODAY).
  ASSIGN btCancFiltrar:SENSITIVE         = NO
         btProcessar:SENSITIVE           = NO.
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
DISABLE TRIGGERS FOR LOAD OF proces_pagto.
FOR EACH tt
    WHERE tt.alteraData = YES AND tt.rRowidProcesPagto <> ?:
    FIND proces_pagto 
        WHERE ROWID(proces_pagto) = tt.rRowidProcesPagto EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL proces_pagto THEN DO:
       ASSIGN proces_pagto.dat_liber_pagto = tt.dt_liberacao.
    END.
    RELEASE proces_pagto.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAtualizarAcao w-livre 
PROCEDURE piAtualizarAcao :
/*------------------------------------------------------------------------------
    atualiza a aá∆o do bot∆o nas linhas selecionadas
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pAcao AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE VARIABLE l             AS LOGICAL     NO-UNDO INIT NO.
 DO i = 1 TO brTitulos:NUM-SELECTED-ROWS IN FRAME  {&FRAME-NAME}:
    
    
    brTitulos:FETCH-SELECTED-ROW(i).
    /*IF AVAILABLE tt THEN DO:
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
    END.*/
    ASSIGN tt.acaoAtual = pAcao.
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
  {&OPEN-QUERY-brTitulos}
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
DEFINE INPUT  PARAMETER pFornecedor AS INTEGER     NO-UNDO.
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
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarAntPEF w-livre 
PROCEDURE piBuscarAntPEF :
/*------------------------------------------------------------------------------
  buscar todas as antecipaá‰es ainda n∆o aprovadas e Pagamentos extra fornecedores 
------------------------------------------------------------------------------*/

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
           OR proces_pagto.ind_sit_proces_pagto =  'em pagamento' ) THEN NEXT.

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
           RUN  getTpFluxo('pef_antecipacao',tt.rRowid,OUTPUT tt.codTpFluxo, OUTPUT tt.DescTpFluxo).
    
END.

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
DEFINE VARIABLE cComum AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarTitulos w-livre 
PROCEDURE piBuscarTitulos :
/*------------------------------------------------------------------------------
  buscar os titulos conforme parametros da tela
------------------------------------------------------------------------------*/
DEFINE VARIABLE decTotAdiant        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE decVlReal           AS DECIMAL     NO-UNDO.
DEFINE VARIABLE decVlDolar          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE decVlJuros          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cSituacao           AS CHAR        NO-UNDO INIT "PENDENTE".
DEFINE VARIABLE dataLiberacao       AS DATE        NO-UNDO.
DEFINE VARIABLE cHrLiberacao        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUsuarioLiberacao   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lAprovador          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE ll AS LOGICAL     NO-UNDO INIT NO.
EMPTY TEMP-TABLE tt.

RUN esbo/esbofin328a.p PERSISTENT SET h_esbofin328a.




FOR EACH tit_ap NO-LOCK
    WHERE tit_ap.cod_estab          >= cod_estab_ini
    AND   tit_ap.cod_estab          <= cod_estab_fim
    AND   tit_ap.dat_vencto_tit_ap  >= INPUT FRAME {&frame-name} fi_data_vencto_ini
    AND   tit_ap.dat_vencto_tit_ap  <= INPUT FRAME {&frame-name} fi_data_vencto_fim
    AND   tit_ap.dat_emis_docto     >= INPUT FRAME {&frame-name} fi_data_emis_ini
    AND   tit_ap.dat_emis_docto     <= INPUT FRAME {&frame-name} fi_data_emis_fim
    AND   tit_ap.log_sdo_tit_ap     >= log_saldo_ini
    AND   tit_ap.log_sdo_tit_ap     <= log_saldo_fim
    AND   tit_ap.cod_espec_docto    >= cod_espec_ini
    AND   tit_ap.cod_espec_docto    <= cod_espec_fim 
    AND   tit_ap.cod_tit_ap         >= cod_titulo_ini
    AND   tit_ap.cod_tit_ap         <= cod_titulo_fim
    AND   tit_ap.cod_parcela        >= cod_parcela_ini 
    AND   tit_ap.cod_parcela        <= cod_parcela_fim 
    AND   tit_ap.cdn_fornec         >= cod_fornec_ini
    AND   tit_ap.cdn_fornec         <= cod_fornec_fim
    AND   tit_ap.log_tit_ap_estordo = NO:
    
    FIND FIRST ems5.espec_docto OF tit_ap 
        WHERE ems5.espec_docto.ind_tip_espec_docto = "previs∆o" NO-LOCK NO-ERROR.
    IF AVAIL ems5.espec_docto THEN NEXT.
    FIND FIRST ems5.fornecedor 
        WHERE fornecedor.cdn_fornecedor = tit_ap.cdn_fornecedor NO-LOCK NO-ERROR.
   /* IF  tit_ap.cod_tit_ap = '3010156' THEN
       ASSIGN ll = YES.
    ELSE 
       ASSIGN ll = NO.*/

    IF tit_ap.cod_indic_econ = 'dolar' THEN DO:

       RUN piBuscarValorTitDolar(rowid(tit_ap), OUTPUT decVlReal).
       ASSIGN decVlDolar = tit_ap.val_sdo_tit_ap.
    END.
    ELSE DO:
      ASSIGN decVlReal  = tit_ap.val_sdo_tit_ap - tit_ap.val_desconto + tit_Ap.val_juros
             decVlDolar = 0.
    END.
    RUN piCalcularJuros(INPUT tit_ap.val_juros_dia_atraso, tit_ap.dat_vencto_tit_ap, 
                        INPUT FRAME {&FRAME-NAME} fi_data_lib, OUTPUT decVlJuros).

    RUN piBuscarAntecipacoes(INPUT tit_ap.cdn_fornec , INPUT tit_ap.cod_Estab, INPUT tit_ap.val_sdo_tit_ap, OUTPUT decTotAdiant ).
    
    FIND LAST proces_pagto OF tit_ap NO-LOCK NO-ERROR.
    
    /*
     caso o usuario n∆o selecione os j† aprovados ser∆o desconsiderados os registros
     j† liberados e ou confirmados
    */

    IF AVAIL proces_pagto THEN DO:
       RUN verificarPermissaoAPB02 IN h_esbofin328a(INPUT  proces_pagto.cod_usuar_liber_pagto ,OUTPUT lAprovador).

       ASSIGN  cSituacao        = proces_pagto.ind_sit_proces_pagto
               dataLiberacao    = proces_pagto.dat_liber_pagto
               cHrLiberacao     = substr(proces_pagto.hra_liber_proces_pagto,1,2) + ":" + substr(proces_pagto.hra_liber_proces_pagto,4,2) + ":" + substr(proces_pagto.hra_liber_proces_pagto,6,2)
              cUsuarioLiberacao = proces_pagto.cod_usuar_liber_pagto .


       IF  (proces_pagto.ind_sit_proces_pagto    = 'liberado'
           OR proces_pagto.ind_sit_proces_pagto  =  'confirmado' 
           OR proces_pagto.ind_sit_proces_pagto  =  'em pagamento' )
           AND INPUT FRAME {&frame-name} tg_aprovados = NO
           THEN DO:
           IF proces_pagto.val_liberd_pagto >= tit_ap.val_sdo_tit_ap  THEN DO:
              IF proces_pagto.ind_sit_proces_pagto = 'confirmado' AND lAprovador = YES  THEN NEXT.
              IF proces_pagto.ind_sit_proces_pagto <> 'confirmado' THEN NEXT.
           END.
              
       END.
       IF ll THEN
           MESSAGE 'regra 01' SKIP
                proces_pagto.ind_sit_proces_pagto
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       /*
        caso o usuario n∆o selecione os registros pendentes de aprovaá∆o ser∆o desconsiderados 
        os registros que n∆o est∆o liberados ou confirmados OU EST«O EM PAGAMENTO
       */
       IF  proces_pagto.ind_sit_proces_pagto       <> 'liberado'
           AND  proces_pagto.ind_sit_proces_pagto  <>  'confirmado'
           AND  proces_pagto.ind_sit_proces_pagto  <>  'em pagamento'
           AND INPUT FRAME {&frame-name} tg_aprovar = NO THEN DO:

           IF proces_pagto.val_liberd_pagto >= tit_ap.val_sdo_tit_ap THEN DO:
              IF proces_pagto.ind_sit_proces_pagto = 'confirmado' AND lAprovador = YES  THEN NEXT.
              IF proces_pagto.ind_sit_proces_pagto <> 'confirmado' THEN NEXT.
           END.
              
       END.
           
       IF ll THEN
           MESSAGE 'regra 02'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       /*
       caso o usuario que aprovou n∆o selecione os titulos aprovados por outros ser∆o desconsiderados
       os registros que n∆o foram aprovados pelo usuario corrente
       */
       IF cod_usuar_liber_pagto <> c-seg-usuario AND  INPUT FRAME {&frame-name} tg_outros = NO 
           AND proces_pagto.ind_sit_proces_pagto   <> 'liberado'
           AND proces_pagto.ind_sit_proces_pagto <>  'confirmado'
           AND proces_pagto.ind_sit_proces_pagto <>  'em pagamento' THEN DO:
           IF proces_pagto.ind_sit_proces_pagto = 'confirmado' AND lAprovador = YES  THEN NEXT.
           IF proces_pagto.ind_sit_proces_pagto <> 'confirmado' THEN NEXT.
       END.
           
       IF ll THEN
           MESSAGE 'regra 03'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
        ASSIGN cSituacao        =  'PENDENTE'
               dataLiberacao     =  INPUT FRAME {&FRAME-NAME} fi_data_lib
               cHrLiberacao     =  ''
               cUsuarioLiberacao =  '' .
       IF INPUT FRAME {&frame-name} tg_aprovar = NO THEN
           NEXT.
       IF ll THEN
       MESSAGE ' n∆o achou processo de pagamento' 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END. 
    CREATE tt. 
    ASSIGN tt.situacao               = cSituacao
           tt.cod_tit_ap             = tit_ap.cod_tit_ap
           tt.cod_ser_docto          = tit_ap.cod_ser_docto       
           tt.cdn_fornecedor         = tit_ap.cdn_fornecedor      
           tt.cod_parcela            = tit_ap.cod_parcela         
           tt.dat_emis_docto         = tit_ap.dat_emis_docto      
           tt.dat_vencto_tit_ap      = tit_ap.dat_vencto_tit_ap   
           tt.val_origin_tit_ap      = tit_ap.val_origin_tit_ap   
           tt.val_sdo_tit_ap         = decVlReal     
           tt.val_calc_adiant        = decTotAdiant
           tt.val_dolar              = decVlDolar 
           tt.val_desconto           = tit_ap.val_desconto
           tt.val_juros_dia_atraso   = DecVlJuros
           tt.val_multa              = tit_ap.val_perc_multa_atraso * tit_ap.val_sdo_tit_ap
           tt.num_id_tit_ap          = tit_ap.num_id_tit_ap
           tt.cod_estab              = tit_ap.cod_estab
           tt.dt_Liberacao           = dataLiberacao
           tt.hr_liberacao           = cHrLiberacao
           tt.usuario_liberacao      = cUsuarioLiberacao
           tt.RRowid                 = ROWID(tit_ap)
           tt.rRowidProcesPagto      = IF AVAIL proces_pagto THEN ROWID(proces_pagto) ELSE ? 
           tt.nome_abrev             = IF AVAIL fornecedor THEN fornecedor.nom_abrev ELSE ''.
           RUN  getTpFluxo(INPUT 'titulo',
                           INPUT tt.rRowid,
                           OUTPUT tt.codTpFluxo, 
                           OUTPUT tt.DescTpFluxo).
    
    
END.

IF  valid-handle(h_esbofin328a) THEN
    DELETE PROCEDURE h_esbofin328a.

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
/*------------------------------------------------------------------------------

------------------------------------------------------------------------------*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalcularJuros w-livre 
PROCEDURE piCalcularJuros :
/*------------------------------------------------------------------------------
 calcula os juros conforme a data de liberaá∆o informada e o percentual de juros 
 diario informado no titulo
 ------------------------------------------------------------------------------*/
 DEFINE INPUT  PARAMETER valorDiario    AS DECIMAL     NO-UNDO.
 DEFINE INPUT  PARAMETER dataVencto     AS DATE        NO-UNDO.
 DEFINE INPUT  PARAMETER dataLiberacao  AS DATE        NO-UNDO.
 DEFINE OUTPUT PARAMETER dValor         AS DECIMAL     NO-UNDO.
 DEFINE VARIABLE qtDias                 AS INTEGER     NO-UNDO.

 ASSIGN qtDias = dataLiberacao - dataVencto .
 IF qtDias > 0 THEN
    ASSIGN dValor = valorDiario * qtDias.
 ELSE
    ASSIGN dValor = 0.

 
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piCalcularTotais w-livre 
PROCEDURE piCalcularTotais :
/*------------------------------------------------------------------------------
  calcula os totais dos titulos que aparecem no browse conforme a coluna situaá∆o
------------------------------------------------------------------------------*/
DEFINE VARIABLE dTotal          AS DECIMAL     NO-UNDO.
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
       fiQTTotalReprovado:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dQtReprovadoTotal).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piEfetivarAcao w-livre 
PROCEDURE piEfetivarAcao :
/*------------------------------------------------------------------------------
 efetivar as aá‰es conforme a aá∆o selecionada para o registro da tabela(titulo, antecipaá∆o, P‘F)   
------------------------------------------------------------------------------*/
DEFINE VARIABLE cHora AS CHARACTER  NO-UNDO FORMAT 'x(10)'.
DEFINE VARIABLE novaSequencia AS INTEGER     NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF proces_pagto.
/*RUN piCalcularHora(OUTPUT cHora).*/
ASSIGN cHora = string(TIME,"hh:mm:ss")
       cHora = REPLACE(cHora,":","").

/*REPLACE(greeting, "user", USERID("DICTDB")).*/

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
                     AND   (proces_pagto.ind_sit_proces_pagto  = 'Preparado' OR proces_pagto.ind_sit_proces_pagto  = 'Estornado')
                     USE-INDEX prcspgt_antec_pef EXCLUSIVE-LOCK NO-ERROR.
                 IF AVAIL proces_pagto THEN DO:
                    ASSIGN  proces_pagto.ind_sit_proces_pagto         = "LIBERADO"
                            proces_pagto.cod_usuar_liber_pagto        =  c-seg-usuario
                            proces_pagto.dat_liber_pagto              =  INPUT FRAME {&frame-name} fi_data_lib
                            proces_pagto.hra_liber_proces_pagto       =  cHora
                            proces_pagto.val_liberd_pagto             =  antecip_pef_pend.val_tit_ap
                            proces_pagto.val_liber_pagto_orig         =  antecip_pef_pend.val_tit_ap.
                 END.
                 ELSE DO:
                    FIND LAST proces_pagto 
                     WHERE antecip_pef_pend.cod_estab = proces_pagto.cod_estab
                     AND   antecip_pef_pend.cod_refer = proces_pagto.cod_refer_antecip_pef NO-LOCK NO-ERROR.
                    IF AVAIL proces_pagto THEN DO:
                       ASSIGN novaSequencia = proces_pagto.num_seq_pagto_tit_ap + 1.
                    END.
                    ELSE
                       ASSIGN novaSequencia = 1.



                    CREATE proces_pagto.
                    ASSIGN 
                        /*proces_pagto.num_id_proces_pagto          =  NEXT-VALUE(seq_proces_pagto) */
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
                        proces_pagto.cod_usuar_prepar_pagto       =  antecip_pef_pend.cod_usuar_gerac_movto
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
                        /*proces_pagto.dat_livre_1                  = TODAY*/ .

                 END.
              END.
           END.
           ELSE DO:
                FIND tit_ap
                    WHERE ROWID(tit_ap) = tt.rRowid NO-LOCK NO-ERROR.
                IF AVAIL tit_ap THEN DO:
                   /*busca o ultimo processo de pagamento existente para o titulo para numerar corretamente a sequencia*/
                   FIND LAST proces_pagto OF tit_ap 
                       WHERE proces_pagto.ind_sit_proces_pagto  = 'Preparado' OR proces_pagto.ind_sit_proces_pagto  = 'Estornado' EXCLUSIVE-LOCK NO-ERROR.
                   IF AVAIL proces_pagto THEN DO:
                      ASSIGN
                            proces_pagto.ind_sit_proces_pagto         = "LIBERADO"
                            proces_pagto.cod_usuar_liber_pagto        =  c-seg-usuario
                            proces_pagto.dat_liber_pagto              =  INPUT FRAME {&frame-name} fi_data_lib
                            proces_pagto.hra_liber_proces_pagto       =  cHora
                            proces_pagto.val_liberd_pagto             =  tit_ap.val_sdo_tit_ap
                            proces_pagto.val_liber_pagto_orig         =  tit_ap.val_sdo_tit_ap.
                   END.
                   ELSE DO:
                      FIND LAST proces_pagto OF tit_ap  NO-LOCK NO-ERROR.
                      IF AVAIL proces_pagto THEN DO:
                        ASSIGN novaSequencia = proces_pagto.num_seq_pagto_tit_ap + 1.
                      END.
                      ELSE
                        ASSIGN novaSequencia = 1.

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
                        proces_pagto.dat_livre_1                  = TODAY.

                   END. 
                END.
           END.
        END.
        WHEN 'reprovar' THEN DO:
             IF tt.num_id_tit_ap = 0 THEN DO:
                FIND  antecip_pef_pend
                   WHERE ROWID(antecip_pef_pend) = tt.rRowid NO-ERROR.
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piSetarVariaveis w-livre 
PROCEDURE piSetarVariaveis :
/*------------------------------------------------------------------------------
colocar valores abertos para as variaveis que se n∆o informadas ficar∆o com filtros
abertos
------------------------------------------------------------------------------*/

CASE cb_empresa:SCREEN-VALUE IN FRAME {&FRAME-NAME} :
    WHEN '100' THEN
        ASSIGN cod_estab_ini    = '101'
               cod_estab_fim    = '199'.
    WHEN '500' THEN
        ASSIGN cod_estab_ini = '501'
               cod_estab_fim = '599'.
    OTHERWISE 
        ASSIGN cod_estab_ini = '101'
               cod_estab_fim = '599'.
END CASE.


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

IF tg_pagos:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'yes' THEN
   ASSIGN LOG_saldo_ini = NO
          LOG_saldo_fim = YES.
ELSE
   ASSIGN LOG_saldo_ini = YES
          LOG_saldo_fim = YES.

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

/*MESSAGE  "estab:" cod_estab_ini                                
         cod_estab_fim SKIP                               
         "data vencto:" INPUT FRAME {&frame-name} fi_data_vencto_ini 
         INPUT FRAME {&frame-name} fi_data_vencto_fim SKIP
         "data emissao:" INPUT FRAME {&frame-name} fi_data_emis_ini   
         INPUT FRAME {&frame-name} fi_data_emis_fim   SKIP
         "log saldo:" log_saldo_ini                                
         log_saldo_fim                                SKIP
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
  {src/adm/template/snd-list.i "tt"}

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

