&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0181 2.04.00.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

{utp/utapi011.i} /* Gera‡Æo de Graficos */


/* ***************************  Definitions  ************************** */
/* Global Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-corte-comerc AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-item AS ROWID NO-UNDO.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-tipo-def 
    FIELD cod-tipo-def  LIKE tipo-def.cod-tipo-def
    FIELD qtd-regular   LIKE mov-est-acbd.qtd-defeit   
    FIELD qtd-leve-def  LIKE mov-est-acbd.qtd-defeit      
    FIELD qtd-retalho   LIKE mov-est-acbd.qtd-defeit    
    FIELD qtd-prod-perf LIKE mov-est-acbd.qtd-defeit   
    FIELD qtd-prod-def  LIKE mov-est-acbd.qtd-defeit      
    FIELD qtd-prod-sob  LIKE mov-est-acbd.qtd-defeit 
    FIELD qtd-rg-ant    LIKE mov-est-acbd.qtd-defeit
    FIELD qtd-ld-ant    LIKE mov-est-acbd.qtd-defeit
    FIELD qtd-rt-ant    LIKE mov-est-acbd.qtd-defeit
    INDEX indice1 cod-tipo-def.

DEF TEMP-TABLE tt-defeitos
    FIELD cod-tipo-def  LIKE defeito.cod-tipo-def
    FIELD cod-defeito   LIKE defeito.cod-defeito
    FIELD qtd-regular   LIKE mov-est-acbd.qtd-defeit   
    FIELD qtd-leve-def  LIKE mov-est-acbd.qtd-defeit      
    FIELD qtd-retalho   LIKE mov-est-acbd.qtd-defeit    
    INDEX indice1 cod-tipo-def cod-defeito.

DEF TEMP-TABLE tt-itens
    FIELD it-codigo     LIKE mov-est-acbd.it-codigo
    FIELD cod-tipo-def  LIKE defeito.cod-tipo-def
    FIELD cod-defeito   LIKE defeito.cod-defeito
    FIELD qtd-prod-per  LIKE mov-est-acbm.qtd-tot-perf
    FIELD qtd-prod-def  LIKE mov-est-acbm.qtd-tot-def
    FIELD qtd-prod-sob  LIKE mov-est-acbm.qtd-tot-sob
    FIELD qtd-regular   LIKE mov-est-acbd.qtd-defeit    
    FIELD qtd-leve-def  LIKE mov-est-acbd.qtd-defeit      
    FIELD qtd-retalho   LIKE mov-est-acbd.qtd-defeit   
    INDEX indice1 cod-tipo-def cod-defeito it-codigo.

DEF TEMP-TABLE tt-nuance
    FIELD it-codigo     LIKE mov-est-acbd.it-codigo
    FIELD cod-tipo-def  LIKE defeito.cod-tipo-def
    FIELD cod-defeito   LIKE defeito.cod-defeito
    FIELD nuance        AS CHAR
    FIELD qtd-nuance    AS DEC
    INDEX indice1 cod-tipo-def cod-defeito it-codigo.

DEF TEMP-TABLE tt-qualid
    FIELD it-codigo     LIKE mov-est-acbd.it-codigo
    FIELD cod-tipo-def  LIKE defeito.cod-tipo-def
    FIELD cod-defeito   LIKE defeito.cod-defeito
    FIELD cod-qualid LIKE ob-etiqueta.cod-qualid
    FIELD qtd-qualid    AS DEC
    INDEX indice1 cod-tipo-def cod-defeito it-codigo.

DEF TEMP-TABLE tt-excluir-ob
    FIELD num-lote LIKE mov-est-acbm.num-lote
    INDEX ch-excluir-ob num-lote.

/* --- Local Variable Definitions --- */
DEF VAR h-acomp          AS HANDLE NO-UNDO.
DEF VAR h-query          AS HANDLE.
DEF VAR c-empresa        AS CHAR.
DEF BUFFER b-tt-itens    FOR tt-itens.
DEF BUFFER b-tt-defeitos FOR tt-defeitos.
DEF BUFFER b-tt-tipo-def FOR tt-tipo-def.

DEF VAR c-desc-tipo-def  LIKE tipo-def.descricao.
DEF VAR c-desc-defeito   LIKE defeito.descricao.
DEF VAR c-desc-qualid    LIKE qualid-tecido.descricao.
DEF VAR c-desc-item      LIKE item.desc-item.
DEF VAR c-it-codigo      LIKE ob-sl-estoq-per.it-codigo.
DEF VAR de-tot-qualid    AS DEC.
DEF VAR c-lotes          AS CHAR FORMAT "x(18)".
DEF VAR arq-saida        AS CHAR FORMAT "x(45)".

DEF VAR c-dia             AS CHAR.
DEF VAR da-dt-emissao-ini AS DATE.
DEF VAR da-dt-emissao-fin AS DATE.

DEF VAR i-lin      AS INT.
DEF VAR i-cont     AS INT.
DEF VAR i-pag      AS INT.
DEF VAR i-tipo-rel AS INT INITIAL 1. /* 1=Sintetico 2=Analitico */
DEF VAR c-item-ini AS CHAR FORMAT "x(6)" INITIAL "".
DEF VAR c-item-fin AS CHAR FORMAT "x(6)" INITIAL "ZZZZZZ".
DEF VAR l-item     AS LOG INITIAL YES.

/* Variaveis usadas na Rotina de ImpressÆo */
DEFINE VAR l-ok                AS LOG.
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* Variaveis de Parƒmetros */
DEFINE VAR c-cod-estabel-ini  AS CHAR INIT "1".
DEFINE VAR c-cod-estabel-fin  AS CHAR INIT "2".
DEFINE VAR da-data-mov-ini    LIKE mov-est-acbm.data-mov.
DEFINE VAR da-data-mov-fin    LIKE mov-est-acbm.data-mov.
DEFINE VAR da-dt-ant-ini      LIKE mov-est-acbm.data-mov.
DEFINE VAR da-dt-ant-fin      LIKE mov-est-acbm.data-mov.
DEFINE VAR c-it-codigo-ini    LIKE mov-est-acbm.it-codigo    INIT "".
DEFINE VAR c-it-codigo-fin    LIKE mov-est-acbm.it-codigo    INIT "ZZZZZZ".
DEFINE VAR c-cod-refer-ini    LIKE mov-est-acbm.cod-refer    INIT "". 
DEFINE VAR c-cod-refer-fin    LIKE mov-est-acbm.cod-refer    INIT "ZZZZZZZ".
DEFINE VAR c-tipo-defeito-ini LIKE mov-est-acbd.cod-tipo-def INIT "".
DEFINE VAR c-tipo-defeito-fin LIKE mov-est-acbd.cod-tipo-def INIT "Z".
DEFINE VAR c-tp-tecido        AS CHAR INITIAL "A".
DEFINE VAR l-excluir-ob       AS LOG FORMAT "Sim/NÆo" INITIAL "NÆo".
DEFINE VAR c-cam-nom-lista    AS CHAR.
DEFINE VAR c-tipo-tecelagem   AS CHAR INITIAL "Nissan,Sulzer,Picanol,Tsudakoma,Toyota". 

/* Variaveis para o Excel */
DEFINE VAR i-canal     AS INT.
DEFINE VAR sys         AS INT.
DEFINE VAR sis         AS INT.
DEFINE VAR c-lin       AS CHAR FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR FORMAT "x(100)".

DEF STREAM aux.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-defeitos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-defeitos tt-itens tt-nuance tt-qualid ~
tt-tipo-def

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-defeitos                                   */
&Scoped-define FIELDS-IN-QUERY-br-defeitos tt-defeitos.cod-defeito fn-desc-defeito() @ c-desc-defeito tt-defeitos.qtd-regular tt-defeitos.qtd-regular / fi-tot-tp-def-rg * 100 tt-defeitos.qtd-leve-def tt-defeitos.qtd-leve-def / fi-tot-tp-def-ld * 100 tt-defeitos.qtd-retalho tt-defeitos.qtd-retalho / fi-tot-tp-def-rt * 100 tt-defeitos.qtd-regular + tt-defeitos.qtd-leve-def + tt-defeitos.qtd-retalho (tt-defeitos.qtd-regular + tt-defeitos.qtd-leve-def + tt-defeitos.qtd-retalho) / fi-tot-tp-def-ger * 100 (tt-defeitos.qtd-regular + tt-defeitos.qtd-leve-def + tt-defeitos.qtd-retalho) / fi-ger-tp-def-ger * 100   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-defeitos   
&Scoped-define SELF-NAME br-defeitos
&Scoped-define QUERY-STRING-br-defeitos FOR EACH tt-defeitos WHERE                                  tt-defeitos.cod-tipo-def = tt-tipo-def.cod-tipo-def                                  NO-LOCK BY (tt-defeitos.qtd-regular  +                                              tt-defeitos.qtd-leve-def +                                              tt-defeitos.qtd-retalho) DESCEND
&Scoped-define OPEN-QUERY-br-defeitos OPEN QUERY {&SELF-NAME} FOR EACH tt-defeitos WHERE                                  tt-defeitos.cod-tipo-def = tt-tipo-def.cod-tipo-def                                  NO-LOCK BY (tt-defeitos.qtd-regular  +                                              tt-defeitos.qtd-leve-def +                                              tt-defeitos.qtd-retalho) DESCEND.
&Scoped-define TABLES-IN-QUERY-br-defeitos tt-defeitos
&Scoped-define FIRST-TABLE-IN-QUERY-br-defeitos tt-defeitos


/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo fn-desc-item() @ c-desc-item tt-itens.qtd-prod-per + tt-itens.qtd-prod-def + tt-itens.qtd-prod-sob tt-itens.qtd-regular tt-itens.qtd-regular / fi-tot-tp-def-rg * 100 tt-itens.qtd-leve-def tt-itens.qtd-leve-def / fi-tot-tp-def-ld * 100 tt-itens.qtd-retalho tt-itens.qtd-retalho / fi-tot-tp-def-rt * 100 tt-itens.qtd-regular + tt-itens.qtd-leve-def + tt-itens.qtd-retalho (tt-itens.qtd-regular + tt-itens.qtd-leve-def + tt-itens.qtd-retalho) / fi-tot-tp-def-ger * 100 (tt-itens.qtd-regular + tt-itens.qtd-leve-def + tt-itens.qtd-retalho) / fi-ger-tp-def-ger * 100   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define QUERY-STRING-br-itens FOR EACH tt-itens WHERE                                  tt-itens.cod-tipo-def = tt-tipo-def.cod-tipo-def AND                                  tt-itens.cod-defeito  = tt-defeitos.cod-defeito                                  NO-LOCK BY (tt-itens.qtd-regular  +                                              tt-itens.qtd-leve-def +                                              tt-itens.qtd-retalho) DESCEND
&Scoped-define OPEN-QUERY-br-itens OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE                                  tt-itens.cod-tipo-def = tt-tipo-def.cod-tipo-def AND                                  tt-itens.cod-defeito  = tt-defeitos.cod-defeito                                  NO-LOCK BY (tt-itens.qtd-regular  +                                              tt-itens.qtd-leve-def +                                              tt-itens.qtd-retalho) DESCEND.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for BROWSE br-nuance                                     */
&Scoped-define FIELDS-IN-QUERY-br-nuance tt-nuance.nuance tt-nuance.qtd-nuance tt-nuance.qtd-nuance / (tt-itens.qtd-prod-per + tt-itens.qtd-prod-def + tt-itens.qtd-prod-sob) * 100   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-nuance   
&Scoped-define SELF-NAME br-nuance
&Scoped-define QUERY-STRING-br-nuance FOR EACH tt-nuance WHERE                                  tt-nuance.cod-tipo-def = tt-itens.cod-tipo-def AND                                  tt-nuance.cod-defeito  = tt-itens.cod-defeito  AND                                  tt-nuance.it-codigo    = tt-itens.it-codigo                                  NO-LOCK BY (tt-nuance.qtd-nuance) DESCEND
&Scoped-define OPEN-QUERY-br-nuance OPEN QUERY {&SELF-NAME} FOR EACH tt-nuance WHERE                                  tt-nuance.cod-tipo-def = tt-itens.cod-tipo-def AND                                  tt-nuance.cod-defeito  = tt-itens.cod-defeito  AND                                  tt-nuance.it-codigo    = tt-itens.it-codigo                                  NO-LOCK BY (tt-nuance.qtd-nuance) DESCEND.
&Scoped-define TABLES-IN-QUERY-br-nuance tt-nuance
&Scoped-define FIRST-TABLE-IN-QUERY-br-nuance tt-nuance


/* Definitions for BROWSE br-qualid                                     */
&Scoped-define FIELDS-IN-QUERY-br-qualid fn-desc-qualid() @ c-desc-qualid tt-qualid.qtd-qualid / de-tot-qualid * 100   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-qualid   
&Scoped-define SELF-NAME br-qualid
&Scoped-define OPEN-QUERY-br-qualid RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-qualid WHERE                                  tt-qualid.cod-tipo-def = tt-itens.cod-tipo-def AND                                  tt-qualid.cod-defeito  = tt-itens.cod-defeito  AND                                  tt-qualid.it-codigo    = tt-itens.it-codigo    AND                                  tt-qualid.qtd-qualid   > 0                                  NO-LOCK BY (tt-qualid.qtd-qualid) DESCEND.
&Scoped-define TABLES-IN-QUERY-br-qualid tt-qualid
&Scoped-define FIRST-TABLE-IN-QUERY-br-qualid tt-qualid


/* Definitions for BROWSE br-tipo-def                                   */
&Scoped-define FIELDS-IN-QUERY-br-tipo-def tt-tipo-def.cod-tipo-def fn-desc-tipo-def() @ c-desc-tipo-def   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-tipo-def   
&Scoped-define SELF-NAME br-tipo-def
&Scoped-define QUERY-STRING-br-tipo-def FOR EACH tt-tipo-def NO-LOCK
&Scoped-define OPEN-QUERY-br-tipo-def OPEN QUERY {&SELF-NAME} FOR EACH tt-tipo-def NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-tipo-def tt-tipo-def
&Scoped-define FIRST-TABLE-IN-QUERY-br-tipo-def tt-tipo-def


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-defeitos}~
    ~{&OPEN-QUERY-br-itens}~
    ~{&OPEN-QUERY-br-nuance}~
    ~{&OPEN-QUERY-br-qualid}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 RECT-55 RECT-56 RECT-57 br-tipo-def ~
br-defeitos bt-param bt-vapara bt-excel bt-imprime br-itens bt-Grafico-qtd ~
fi-perc-rg fi-perc-ld fi-perc-rt fi-perc-tt br-qualid br-nuance fi-perc-per ~
fi-perc-rg-ant fi-perc-ld-ant fi-perc-sob bt-exit fi-perc-rt-ant ~
fi-perc-def fi-perc-tt-ant bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-tp-def-rg fi-perc-rg ~
fi-ger-tp-def-rg fi-tot-tp-def-ld fi-perc-ld fi-ger-tp-def-ld ~
fi-tot-tp-def-rt fi-perc-rt fi-ger-tp-def-rt fi-tot-tp-def-ger fi-perc-tt ~
fi-ger-tp-def-ger fi-tot-prod-per fi-perc-per fi-def-rg-ant fi-perc-rg-ant ~
fi-ger-rg-ant fi-def-ld-ant fi-perc-ld-ant fi-ger-ld-ant fi-tot-prod-sob ~
fi-perc-sob fi-def-rt-ant fi-perc-rt-ant fi-ger-rt-ant fi-tot-prod-def ~
fi-perc-def fi-tot-ant fi-perc-tt-ant fi-ger-ant fi-tot-prod-ger 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-excel bt-Grafico-qtd 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-defeito C-Win 
FUNCTION fn-desc-defeito RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item C-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-qualid C-Win 
FUNCTION fn-desc-qualid RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-tipo-def C-Win 
FUNCTION fn-desc-tipo-def RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     IMAGE-UP FILE "image/im-hel.bmp":U
     LABEL "&Ajuda" 
     SIZE 6 BY 1.5 TOOLTIP "Help on Line"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-excel 
     IMAGE-UP FILE "image/excel.bmp":U
     LABEL "Button 2" 
     SIZE 6 BY 1.5.

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Fechar Programa"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-Grafico-qtd 
     IMAGE-UP FILE "image/im-grf.bmp":U
     LABEL "Grafico" 
     SIZE 6 BY 1.5 TOOLTIP "Visualizar o Grafico dos Tipos de Defeitos".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/im-printer.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Imprimir a Evolu‡Æo do Estoque Acabado".

DEFINE BUTTON bt-param AUTO-GO 
     IMAGE-UP FILE "image/im-fil.bmp":U
     LABEL "&Sair" 
     SIZE 6 BY 1.5 TOOLTIP "Parƒmetros"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapara AUTO-GO 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Posicionar no Item"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-def-ld-ant AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-def-rg-ant AS DECIMAL FORMAT "ZZ,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-def-rt-ant AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-ger-ant AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-ger-ld-ant AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-ger-rg-ant AS DECIMAL FORMAT "ZZ,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-ger-rt-ant AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-ger-tp-def-ger AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-ger-tp-def-ld AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-ger-tp-def-rg AS DECIMAL FORMAT "ZZ,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-ger-tp-def-rt AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-perc-def AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88
     BGCOLOR 8 FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-ld AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-ld-ant AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-per AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88
     BGCOLOR 8 FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-rg AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-rg-ant AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-rt AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-rt-ant AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-sob AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88
     BGCOLOR 8 FGCOLOR 12 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-tt AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-perc-tt-ant AS DECIMAL FORMAT "ZZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 8 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-ant AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-prod-def AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-prod-ger AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .88
     BGCOLOR 15 FGCOLOR 12 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-prod-per AS DECIMAL FORMAT "ZZ,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-prod-sob AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .88
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-tp-def-ger AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-tp-def-ld AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-tp-def-rg AS DECIMAL FORMAT "ZZ,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-tp-def-rt AS DECIMAL FORMAT "zz,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .88
     FGCOLOR 4 FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 48 BY 5.75
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 8 BY 20.5.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 5.25.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 5.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-defeitos FOR 
      tt-defeitos SCROLLING.

DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.

DEFINE QUERY br-nuance FOR 
      tt-nuance SCROLLING.

DEFINE QUERY br-qualid FOR 
      tt-qualid SCROLLING.

DEFINE QUERY br-tipo-def FOR 
      tt-tipo-def SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-defeitos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-defeitos C-Win _FREEFORM
  QUERY br-defeitos NO-LOCK DISPLAY
      tt-defeitos.cod-defeito            COLUMN-LABEL "Cod"   WIDTH 03
      fn-desc-defeito() @ c-desc-defeito COLUMN-LABEL "Descri‡Æo" WIDTH 26.60
      tt-defeitos.qtd-regular   FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Regular"
      tt-defeitos.qtd-regular  / fi-tot-tp-def-rg * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "
      tt-defeitos.qtd-leve-def  FORMAT "->,>>>,>>9.99" COLUMN-LABEL "L.Defeito"
      tt-defeitos.qtd-leve-def / fi-tot-tp-def-ld * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "
      tt-defeitos.qtd-retalho   FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Retalho"
      tt-defeitos.qtd-retalho  / fi-tot-tp-def-rt * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "


      tt-defeitos.qtd-regular +
      tt-defeitos.qtd-leve-def +
      tt-defeitos.qtd-retalho  FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "T.Defeito"

     (tt-defeitos.qtd-regular +
      tt-defeitos.qtd-leve-def +
      tt-defeitos.qtd-retalho)  / fi-tot-tp-def-ger * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "

     (tt-defeitos.qtd-regular +
      tt-defeitos.qtd-leve-def +
      tt-defeitos.qtd-retalho) / fi-ger-tp-def-ger * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 7.5
         FONT 1
         TITLE "Defeitos" ROW-HEIGHT-CHARS .58.

DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens C-Win _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "x(6)" WIDTH 5  COLUMN-LABEL "Codigo"
      fn-desc-item() @ c-desc-item     WIDTH 20 COLUMN-LABEL "Descricao"
      tt-itens.qtd-prod-per +
      tt-itens.qtd-prod-def +
      tt-itens.qtd-prod-sob  FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "Producao"
      tt-itens.qtd-regular   FORMAT ">>>,>>9.99"    COLUMN-LABEL "Regular"
      tt-itens.qtd-regular  / fi-tot-tp-def-rg * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "
      tt-itens.qtd-leve-def  FORMAT ">>>,>>9.99"    COLUMN-LABEL "L.Defeito"
      tt-itens.qtd-leve-def / fi-tot-tp-def-ld * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "
      tt-itens.qtd-retalho   FORMAT ">>>,>>9.99"    COLUMN-LABEL "Retalho"
      tt-itens.qtd-retalho  / fi-tot-tp-def-rt * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "
      tt-itens.qtd-regular +
      tt-itens.qtd-leve-def +
      tt-itens.qtd-retalho  FORMAT ">>,>>>,>>9.99" COLUMN-LABEL "T.Defeito"
      (tt-itens.qtd-regular +
       tt-itens.qtd-leve-def +
       tt-itens.qtd-retalho)  / fi-tot-tp-def-ger * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "
      (tt-itens.qtd-regular +
       tt-itens.qtd-leve-def +
       tt-itens.qtd-retalho) / fi-ger-tp-def-ger * 100 FORMAT ">>9.99" COLUMN-LABEL "  %  "
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79.43 BY 7.25
         FONT 1
         TITLE "Itens" ROW-HEIGHT-CHARS .5.

DEFINE BROWSE br-nuance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-nuance C-Win _FREEFORM
  QUERY br-nuance NO-LOCK DISPLAY
      tt-nuance.nuance                 WIDTH 5.5  COLUMN-LABEL "Nuance"
      tt-nuance.qtd-nuance FORMAT ">>,>>>,>>9.99" WIDTH 10 COLUMN-LABEL "Quantidade"
      tt-nuance.qtd-nuance /
      (tt-itens.qtd-prod-per +
       tt-itens.qtd-prod-def +
       tt-itens.qtd-prod-sob) * 100 FORMAT ">>9.99" WIDTH 5 COLUMN-LABEL "  %  "
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 25 BY 5.42
         FONT 1
         TITLE "Nuances" ROW-HEIGHT-CHARS .45.

DEFINE BROWSE br-qualid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-qualid C-Win _FREEFORM
  QUERY br-qualid NO-LOCK DISPLAY
      fn-desc-qualid() @ c-desc-qualid COLUMN-LABEL "Descri‡Æo" WIDTH 12
      tt-qualid.qtd-qualid / de-tot-qualid * 100 FORMAT ">>9.99" WIDTH 6 COLUMN-LABEL "  %  "
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 22 BY 5.42
         FONT 1
         TITLE "Qualidade" ROW-HEIGHT-CHARS .45.

DEFINE BROWSE br-tipo-def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-tipo-def C-Win _FREEFORM
  QUERY br-tipo-def NO-LOCK DISPLAY
      tt-tipo-def.cod-tipo-def             COLUMN-LABEL "Codigo"     WIDTH 5
      fn-desc-tipo-def() @ c-desc-tipo-def COLUMN-LABEL "Descri‡Æo" WIDTH 38
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 48.29 BY 8.17
         FONT 1
         TITLE "Tipos de Defeito" ROW-HEIGHT-CHARS .58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     br-tipo-def AT ROW 1.08 COL 1.72
     br-defeitos AT ROW 1.08 COL 51.14
     bt-param AT ROW 1.38 COL 132.43
     bt-vapara AT ROW 3.67 COL 132.43
     bt-excel AT ROW 5.33 COL 132.43
     bt-imprime AT ROW 7 COL 132.43
     br-itens AT ROW 8.67 COL 51.72
     bt-Grafico-qtd AT ROW 8.67 COL 132.43
     fi-tot-tp-def-rg AT ROW 10.71 COL 11.86 COLON-ALIGNED NO-LABEL
     fi-perc-rg AT ROW 10.71 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-ger-tp-def-rg AT ROW 10.71 COL 33 COLON-ALIGNED NO-LABEL
     fi-tot-tp-def-ld AT ROW 11.71 COL 11.86 COLON-ALIGNED NO-LABEL
     fi-perc-ld AT ROW 11.71 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-ger-tp-def-ld AT ROW 11.71 COL 33 COLON-ALIGNED NO-LABEL
     fi-tot-tp-def-rt AT ROW 12.71 COL 11.86 COLON-ALIGNED NO-LABEL
     fi-perc-rt AT ROW 12.71 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-ger-tp-def-rt AT ROW 12.71 COL 33 COLON-ALIGNED NO-LABEL
     fi-tot-tp-def-ger AT ROW 13.71 COL 11.86 COLON-ALIGNED NO-LABEL
     fi-perc-tt AT ROW 13.71 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-ger-tp-def-ger AT ROW 13.71 COL 33 COLON-ALIGNED NO-LABEL
     br-qualid AT ROW 16.04 COL 83.57
     br-nuance AT ROW 16.04 COL 106
     fi-tot-prod-per AT ROW 16.79 COL 57.86 COLON-ALIGNED NO-LABEL
     fi-perc-per AT ROW 16.79 COL 74.86 COLON-ALIGNED NO-LABEL
     fi-def-rg-ant AT ROW 16.83 COL 11.86 COLON-ALIGNED NO-LABEL
     fi-perc-rg-ant AT ROW 16.83 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-ger-rg-ant AT ROW 16.83 COL 33 COLON-ALIGNED NO-LABEL
     fi-def-ld-ant AT ROW 17.83 COL 11.86 COLON-ALIGNED NO-LABEL
     fi-perc-ld-ant AT ROW 17.83 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-ger-ld-ant AT ROW 17.83 COL 33 COLON-ALIGNED NO-LABEL
     fi-tot-prod-sob AT ROW 18 COL 57.86 COLON-ALIGNED NO-LABEL
     fi-perc-sob AT ROW 18 COL 74.86 COLON-ALIGNED NO-LABEL
     bt-exit AT ROW 18.13 COL 132.43
     fi-def-rt-ant AT ROW 18.83 COL 11.86 COLON-ALIGNED NO-LABEL
     fi-perc-rt-ant AT ROW 18.83 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-ger-rt-ant AT ROW 18.83 COL 33 COLON-ALIGNED NO-LABEL
     fi-tot-prod-def AT ROW 19.04 COL 57.86 COLON-ALIGNED NO-LABEL
     fi-perc-def AT ROW 19.04 COL 74.86 COLON-ALIGNED NO-LABEL
     fi-tot-ant AT ROW 19.83 COL 11.86 COLON-ALIGNED NO-LABEL
     fi-perc-tt-ant AT ROW 19.83 COL 26.72 COLON-ALIGNED NO-LABEL
     fi-ger-ant AT ROW 19.83 COL 33 COLON-ALIGNED NO-LABEL
     bt-ajuda AT ROW 19.83 COL 132.43
     fi-tot-prod-ger AT ROW 20.25 COL 65.72 COLON-ALIGNED NO-LABEL
     "Retalho:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 12.83 COL 6.57
          BGCOLOR 8 FGCOLOR 9 FONT 6
     " PERÖODO CORRENTE" VIEW-AS TEXT
          SIZE 19.86 BY .75 AT ROW 9.29 COL 3.29
          BGCOLOR 8 FGCOLOR 12 FONT 0
     "TOTAL GERAL" VIEW-AS TEXT
          SIZE 12.57 BY .54 AT ROW 10.08 COL 37.14
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "%" VIEW-AS TEXT
          SIZE 2 BY .54 AT ROW 10 COL 30.86
          BGCOLOR 8 
     "TIPOS DEFEITOS" VIEW-AS TEXT
          SIZE 15 BY .54 AT ROW 10.08 COL 13.29
          BGCOLOR 8 FGCOLOR 4 FONT 6
     "Total Produ‡Æo:" VIEW-AS TEXT
          SIZE 15 BY .67 AT ROW 20.38 COL 52.57
          BGCOLOR 8 FGCOLOR 12 
     "PERÖODO ANTERIOR" VIEW-AS TEXT
          SIZE 18.57 BY .71 AT ROW 15.38 COL 3.43
          BGCOLOR 8 FGCOLOR 12 FONT 0
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 138.57 BY 20.75.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     "%" VIEW-AS TEXT
          SIZE 2 BY .54 AT ROW 16.04 COL 30.86
          BGCOLOR 8 
     "Leve Defeito:" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 17.92 COL 2.57
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Defeito:" VIEW-AS TEXT
          SIZE 7.43 BY .67 AT ROW 19.17 COL 52.29
          BGCOLOR 8 
     "Total Geral:" VIEW-AS TEXT
          SIZE 9.29 BY .54 AT ROW 13.88 COL 4.14
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "%" VIEW-AS TEXT
          SIZE 2 BY .54 AT ROW 16.25 COL 79
          BGCOLOR 8 
     "Regular:" VIEW-AS TEXT
          SIZE 6.57 BY .54 AT ROW 10.92 COL 7
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Perfeita:" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 16.92 COL 51.72
          BGCOLOR 8 
     "Leve Defeito:" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 11.79 COL 2.57
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "TIPOS DEFEITOS" VIEW-AS TEXT
          SIZE 15 BY .54 AT ROW 16.13 COL 13.29
          BGCOLOR 8 FGCOLOR 4 FONT 6
     "Sobras:" VIEW-AS TEXT
          SIZE 7.43 BY .67 AT ROW 18.13 COL 52.29
          BGCOLOR 8 
     "TOTAL GERAL" VIEW-AS TEXT
          SIZE 12.57 BY .54 AT ROW 16.13 COL 37.14
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Retalho:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 18.96 COL 6.57
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Total Geral:" VIEW-AS TEXT
          SIZE 9.29 BY .54 AT ROW 20 COL 4.14
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Regular:" VIEW-AS TEXT
          SIZE 6.57 BY .54 AT ROW 17.04 COL 7
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "PRODU€ÇO" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 15.96 COL 53.14
          BGCOLOR 8 FGCOLOR 9 FONT 6
     RECT-5 AT ROW 9.54 COL 2
     RECT-55 AT ROW 1 COL 131.43
     RECT-56 AT ROW 16.25 COL 51.29
     RECT-57 AT ROW 15.75 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 138.57 BY 20.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Analise de Defeitos na Produ‡Æo - ESSP0181"
         COLUMN             = 7.29
         ROW                = 6.42
         HEIGHT             = 20.58
         WIDTH              = 139.43
         MAX-HEIGHT         = 29.79
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29.79
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB br-tipo-def RECT-57 DEFAULT-FRAME */
/* BROWSE-TAB br-defeitos br-tipo-def DEFAULT-FRAME */
/* BROWSE-TAB br-itens bt-imprime DEFAULT-FRAME */
/* BROWSE-TAB br-qualid fi-ger-tp-def-ger DEFAULT-FRAME */
/* BROWSE-TAB br-nuance br-qualid DEFAULT-FRAME */
/* SETTINGS FOR BUTTON bt-excel IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR BUTTON bt-Grafico-qtd IN FRAME DEFAULT-FRAME
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-def-ld-ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-def-rg-ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-def-rt-ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ger-ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ger-ld-ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ger-rg-ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ger-rt-ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ger-tp-def-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ger-tp-def-ld IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ger-tp-def-rg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-ger-tp-def-rt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-ant IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-prod-def IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-prod-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-prod-per IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-prod-sob IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-tp-def-ger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-tp-def-ld IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-tp-def-rg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-tp-def-rt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-defeitos
/* Query rebuild information for BROWSE br-defeitos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-defeitos WHERE
                                 tt-defeitos.cod-tipo-def = tt-tipo-def.cod-tipo-def
                                 NO-LOCK BY (tt-defeitos.qtd-regular  +
                                             tt-defeitos.qtd-leve-def +
                                             tt-defeitos.qtd-retalho) DESCEND.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-defeitos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE
                                 tt-itens.cod-tipo-def = tt-tipo-def.cod-tipo-def AND
                                 tt-itens.cod-defeito  = tt-defeitos.cod-defeito
                                 NO-LOCK BY (tt-itens.qtd-regular  +
                                             tt-itens.qtd-leve-def +
                                             tt-itens.qtd-retalho) DESCEND.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-nuance
/* Query rebuild information for BROWSE br-nuance
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-nuance WHERE
                                 tt-nuance.cod-tipo-def = tt-itens.cod-tipo-def AND
                                 tt-nuance.cod-defeito  = tt-itens.cod-defeito  AND
                                 tt-nuance.it-codigo    = tt-itens.it-codigo
                                 NO-LOCK BY (tt-nuance.qtd-nuance) DESCEND.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-nuance */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-qualid
/* Query rebuild information for BROWSE br-qualid
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-qualid WHERE
                                 tt-qualid.cod-tipo-def = tt-itens.cod-tipo-def AND
                                 tt-qualid.cod-defeito  = tt-itens.cod-defeito  AND
                                 tt-qualid.it-codigo    = tt-itens.it-codigo    AND
                                 tt-qualid.qtd-qualid   > 0
                                 NO-LOCK BY (tt-qualid.qtd-qualid) DESCEND.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-qualid */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-tipo-def
/* Query rebuild information for BROWSE br-tipo-def
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-tipo-def NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-tipo-def */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Analise de Defeitos na Produ‡Æo - ESSP0181 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Analise de Defeitos na Produ‡Æo - ESSP0181 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-defeitos
&Scoped-define SELF-NAME br-defeitos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-defeitos C-Win
ON VALUE-CHANGED OF br-defeitos IN FRAME DEFAULT-FRAME /* Defeitos */
DO:
  {&OPEN-QUERY-br-itens}
  APPLY 'value-changed' TO br-itens.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens C-Win
ON VALUE-CHANGED OF br-itens IN FRAME DEFAULT-FRAME /* Itens */
DO:
    {&OPEN-QUERY-br-nuance}
    APPLY 'value-changed' TO br-nuance.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-nuance
&Scoped-define SELF-NAME br-nuance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nuance C-Win
ON VALUE-CHANGED OF br-nuance IN FRAME DEFAULT-FRAME /* Nuances */
DO:
    {&OPEN-QUERY-br-qualid}
    APPLY 'value-changed' TO br-qualid.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-qualid
&Scoped-define SELF-NAME br-qualid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-qualid C-Win
ON VALUE-CHANGED OF br-qualid IN FRAME DEFAULT-FRAME /* Qualidade */
DO:
 /* APPLY 'entry' TO br-itens. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-tipo-def
&Scoped-define SELF-NAME br-tipo-def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-tipo-def C-Win
ON VALUE-CHANGED OF br-tipo-def IN FRAME DEFAULT-FRAME /* Tipos de Defeito */
DO:
 IF AVAIL tt-tipo-def THEN DO:
    ASSIGN fi-tot-tp-def-rg  = tt-tipo-def.qtd-regular
           fi-tot-tp-def-ld  = tt-tipo-def.qtd-leve-def
           fi-tot-tp-def-rt  = tt-tipo-def.qtd-retalho
           fi-tot-tp-def-ger = tt-tipo-def.qtd-regular + tt-tipo-def.qtd-leve-def + tt-tipo-def.qtd-retalho
           fi-def-rg-ant     = tt-tipo-def.qtd-rg-ant
           fi-def-ld-ant     = tt-tipo-def.qtd-ld-ant
           fi-def-rt-ant     = tt-tipo-def.qtd-rt-ant
           fi-tot-ant        = tt-tipo-def.qtd-rg-ant + tt-tipo-def.qtd-ld-ant + tt-tipo-def.qtd-rt-ant.


    ASSIGN fi-perc-rg     = fi-tot-tp-def-rg  / fi-ger-tp-def-ger * 100
           fi-perc-ld     = fi-tot-tp-def-ld  / fi-ger-tp-def-ger * 100
           fi-perc-rt     = fi-tot-tp-def-rt  / fi-ger-tp-def-ger * 100
           fi-perc-tt     = fi-tot-tp-def-ger / fi-ger-tp-def-ger * 100
           fi-perc-rg-ant = fi-def-rg-ant     / fi-ger-rg-ant * 100 
           fi-perc-ld-ant = fi-def-ld-ant     / fi-ger-ld-ant * 100 
           fi-perc-rt-ant = fi-def-rt-ant     / fi-ger-rt-ant * 100 
           fi-perc-tt-ant = fi-tot-ant        / fi-ger-ant    * 100.



    IF fi-tot-tp-def-ger > fi-tot-ant THEN
       ASSIGN fi-perc-tt:FGCOLOR IN FRAME {&FRAME-NAME} = 12.
    ELSE
       ASSIGN fi-perc-tt:FGCOLOR IN FRAME {&FRAME-NAME} = 0.

    IF fi-tot-tp-def-rt > fi-def-rt-ant THEN
       ASSIGN fi-perc-rt:FGCOLOR IN FRAME {&FRAME-NAME} = 12.
    ELSE
       ASSIGN fi-perc-rt:FGCOLOR IN FRAME {&FRAME-NAME} = 0.

    IF fi-tot-tp-def-ld > fi-def-ld-ant THEN
       ASSIGN fi-perc-ld:FGCOLOR IN FRAME {&FRAME-NAME} = 12.
    ELSE
       ASSIGN fi-perc-ld:FGCOLOR IN FRAME {&FRAME-NAME} = 0.

    IF fi-tot-tp-def-rg > fi-def-rg-ant THEN
       ASSIGN fi-perc-rg:FGCOLOR IN FRAME {&FRAME-NAME} = 12.
    ELSE
       ASSIGN fi-perc-rg:FGCOLOR IN FRAME {&FRAME-NAME} = 0.

    {&OPEN-QUERY-br-defeitos}
    APPLY 'value-changed' TO br-defeitos.

    DISPLAY fi-tot-tp-def-rg
            fi-perc-rg
            fi-tot-tp-def-ld
            fi-perc-ld
            fi-tot-tp-def-rt
            fi-perc-rt
            fi-tot-tp-def-ger
            fi-perc-tt
            fi-def-rg-ant
            fi-perc-rg-ant
            fi-def-ld-ant
            fi-perc-ld-ant
            fi-def-rt-ant
            fi-perc-rt-ant
            fi-tot-ant
            fi-perc-tt-ant
            WITH FRAME {&FRAME-NAME}. 
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda C-Win
ON CHOOSE OF bt-ajuda IN FRAME DEFAULT-FRAME /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-excel C-Win
ON CHOOSE OF bt-excel IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
   ASSIGN i-tipo-rel = 1.
   RUN esdlg/d01essp0181.w (INPUT-OUTPUT c-it-codigo-ini,   
                            INPUT-OUTPUT c-it-codigo-fin,   
                            INPUT-OUTPUT c-tipo-defeito-ini,
                            INPUT-OUTPUT c-tipo-defeito-fin,
                            INPUT-OUTPUT i-tipo-rel,   
                            INPUT-OUTPUT l-ok,
                            OUTPUT arq-saida). 
   IF arq-saida <> "" THEN DO:
      RUN pi-gera-excel (INPUT arq-saida). 
      MESSAGE "O arquivo foi gerado em " TRIM(arq-saida) + "." SKIP 
              "Para acess -lo,  abra-o atrav‚s do Excel."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit C-Win
ON CHOOSE OF bt-exit IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Grafico-qtd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Grafico-qtd C-Win
ON CHOOSE OF bt-Grafico-qtd IN FRAME DEFAULT-FRAME /* Grafico */
DO:

 RUN pi-grafico-tp-def.


 /*
    RUN esdlg/d04essp0174.w (OUTPUT l-barra-vendidos,
                             OUTPUT l-barra-cancelados,
                             OUTPUT l-barra-faturados).
    IF l-barra-vendidos   = NO AND 
       l-barra-cancelados = NO AND 
       l-barra-faturados  = NO THEN DO:
          MESSAGE "NÆo Existe Barra(s) para a Montagem do Grafico. Favor selecionar ! ! !"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    ELSE DO:
       RUN pi-grafico-qtd.
       FIND FIRST tt-work NO-LOCK NO-ERROR.
    END.
 */   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME
DO:
   
   ASSIGN c-win:SENSITIVE = NO.
   ASSIGN i-tipo-rel = 1.
   RUN esp/essp0181b.w (INPUT-OUTPUT c-it-codigo-ini,   
                        INPUT-OUTPUT c-it-codigo-fin,   
                        INPUT-OUTPUT c-tipo-defeito-ini,
                        INPUT-OUTPUT c-tipo-defeito-fin,
                        INPUT-OUTPUT i-tipo-rel,   
                        INPUT-OUTPUT l-ok). 
   ASSIGN c-win:SENSITIVE = YES.
   IF l-ok THEN
      RUN pi-imprime.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-param
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-param C-Win
ON CHOOSE OF bt-param IN FRAME DEFAULT-FRAME /* Sair */
DO:
   ASSIGN c-win:SENSITIVE = NO.
   ASSIGN l-ok = NO.
   RUN esp/essp0181a.w (INPUT-OUTPUT c-cod-estabel-ini,
                        INPUT-OUTPUT c-cod-estabel-fin,
                        INPUT-OUTPUT da-data-mov-ini,
                        INPUT-OUTPUT da-data-mov-fin,
                        INPUT-OUTPUT da-dt-ant-ini,
                        INPUT-OUTPUT da-dt-ant-fin,
                        INPUT-OUTPUT c-it-codigo-ini,   
                        INPUT-OUTPUT c-it-codigo-fin,   
                        INPUT-OUTPUT c-cod-refer-ini,
                        INPUT-OUTPUT c-cod-refer-fin,
                        INPUT-OUTPUT c-tipo-defeito-ini,
                        INPUT-OUTPUT c-tipo-defeito-fin,
                        INPUT-OUTPUT c-tp-tecido,
                        INPUT-OUTPUT c-tipo-tecelagem, 
                        INPUT-OUTPUT l-excluir-ob,
                        INPUT-OUTPUT c-cam-nom-lista,
                        INPUT-OUTPUT l-ok).
   IF l-ok THEN                                     
      RUN pi-popula-browse.
   ASSIGN c-win:SENSITIVE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara C-Win
ON CHOOSE OF bt-vapara IN FRAME DEFAULT-FRAME
DO:
  
   RUN esdlg/d01essp0172.w (OUTPUT c-it-codigo).

   IF c-it-codigo <> ? THEN DO:
      FIND tt-itens WHERE
           tt-itens.it-codigo = c-it-codigo NO-LOCK NO-ERROR. 

      IF NOT AVAIL tt-itens THEN DO.
         MESSAGE "Item nÆo est  contido na sele‡Æo!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
      END.
       /* br-itens:QUERY:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR. */
      h-query:REPOSITION-TO-ROWID(ROWID(tt-itens)) NO-ERROR.
      APPLY 'VALUE-CHANGED' TO br-itens.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-defeitos
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/*
ON 'F5':U OF br-itens DO:
   {&OPEN-QUERY-br-itens}
   APPLY 'value-changed' TO br-itens.
END.

br-itens:NUM-LOCKED-COLUMNS = 3.
*/

ASSIGN h-query = br-itens:QUERY.


ASSIGN da-data-mov-ini = DATE("01" + STRING(MONTH(TODAY),'99') + STRING(YEAR(TODAY),'9999'))
       da-data-mov-fin = TODAY
       da-dt-ant-fin   = da-data-mov-ini - 1
       da-dt-ant-ini   = date("01" + STRING(MONTH(da-dt-ant-fin),'99') +
                                             STRING(YEAR(da-dt-ant-fin),'9999')).


STATUS INPUT OFF. /* Desliga Mensagem no Rodap‚ da Tela */

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

   RUN enable_UI.

   APPLY 'choose' TO bt-param.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-tot-tp-def-rg fi-perc-rg fi-ger-tp-def-rg fi-tot-tp-def-ld 
          fi-perc-ld fi-ger-tp-def-ld fi-tot-tp-def-rt fi-perc-rt 
          fi-ger-tp-def-rt fi-tot-tp-def-ger fi-perc-tt fi-ger-tp-def-ger 
          fi-tot-prod-per fi-perc-per fi-def-rg-ant fi-perc-rg-ant fi-ger-rg-ant 
          fi-def-ld-ant fi-perc-ld-ant fi-ger-ld-ant fi-tot-prod-sob fi-perc-sob 
          fi-def-rt-ant fi-perc-rt-ant fi-ger-rt-ant fi-tot-prod-def fi-perc-def 
          fi-tot-ant fi-perc-tt-ant fi-ger-ant fi-tot-prod-ger 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-5 RECT-55 RECT-56 RECT-57 br-tipo-def br-defeitos bt-param 
         bt-vapara bt-excel bt-imprime br-itens bt-Grafico-qtd fi-perc-rg 
         fi-perc-ld fi-perc-rt fi-perc-tt br-qualid br-nuance fi-perc-per 
         fi-perc-rg-ant fi-perc-ld-ant fi-perc-sob bt-exit fi-perc-rt-ant 
         fi-perc-def fi-perc-tt-ant bt-ajuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-abre-excel C-Win 
PROCEDURE pi-abre-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER p-arquivo AS CHAR.

   def var h-prog as handle no-undo.
   run utp/ut-utils.p persistent set h-prog.

   run Execute in h-prog(input "EXCEL.EXE", input p-arquivo).

   delete procedure h-prog.
   PAUSE 5 NO-MESSAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-gera-excel C-Win 
PROCEDURE pi-gera-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE excelAppl   AS COM-HANDLE. 
DEFINE VARIABLE excelSheet  AS COM-HANDLE.

    DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
    DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1. 
    ENABLE ALL WITH FRAME frm_excel.
    
    RUN pi-abre-excel (INPUT "").
    PAUSE 3 NO-MESSAGE.
    DDE INITIATE sys     FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
    DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "plan1".


    IF i-tipo-rel = 1 THEN
       RUN pi-planilha-sintetica. 
    ELSE 
       RUN pi-planilha-analitica. 

    OS-DELETE VALUE(p-arq-saida).
    DDE EXECUTE   sys COMMAND '[save.as("' + p-arq-saida + '")]'.
    DDE EXECUTE   sys COMMAND "[close(0)]". 
    DDE EXECUTE   sys COMMAND "[quit()]". 
    DDE TERMINATE sys. 
    
    HIDE FRAME frm_excel.
    CLEAR FRAME frm_excel.
    DISABLE ALL WITH FRAME frm_excel.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grafico-tp-def C-Win 
PROCEDURE pi-grafico-tp-def :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 DEF VAR i-point   AS INT INITIAL 1.
 DEF VAR i-numsets AS INT INITIAL 0.

 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configura‡Æo Geral do Grafico */
 /*                               */

 CREATE tt-atributos.
 ASSIGN tt-atributos.cod-versao-integracao = 3
        tt-atributos.graphtype             = 3
        tt-atributos.graphtitle            = 'ANALISE DE DEFEITOS NA PRODU€ÇO NO ' + 'PERIODO DE ' + STRING(da-data-mov-ini, '99/99/9999') + ' ' + 
                                                                                  STRING(da-data-mov-fin, '99/99/9999')
        tt-atributos.lefttitle             = 'Quantidade de Defeitos em METROS.'
        tt-atributos.lefttitlestyle        = 1
        tt-atributos.bottomtitle           = fn-desc-tipo-def()
        tt-atributos.numgraph              = 1.

 /* Configura‡Æo das Variantes do Grafico (Linhas ou  Barras */
 /*                                                          */
 ASSIGN i-numsets = 1.
 CREATE tt-sets.
 ASSIGN tt-sets.NumSet   = i-numsets 
        tt-sets.NumGraph = 1
        tt-sets.ColorSet = 1
        tt-sets.legendText = "Regular".
 ASSIGN i-numsets = i-numsets + 1.

 CREATE tt-sets.
 ASSIGN tt-sets.NumSet   = i-numsets
        tt-sets.NumGraph = 1
        tt-sets.ColorSet = 4
        tt-sets.legendText = "Leve Defeito".
 ASSIGN i-numsets = i-numsets + 1.

 CREATE tt-sets.
 ASSIGN tt-sets.NumSet   = i-numsets
        tt-sets.NumGraph = 1
        tt-sets.ColorSet = 2
        tt-sets.legendText = "Retalho".
 ASSIGN i-numsets = i-numsets + 1.

 CREATE tt-sets.
 ASSIGN tt-sets.NumSet   = i-numsets
        tt-sets.NumGraph = 1
        tt-sets.ColorSet = 5
        tt-sets.legendText = "Total Geral Defeitos".
 ASSIGN i-numsets = i-numsets + 1.


 /* Valores do EIXO X (DIAS) */
  CREATE tt-points-2.
  ASSIGN tt-points-2.NumPoint  = i-point
         tt-points-2.NumGraph  = 1
         tt-points-2.labeltext = ''.

 ASSIGN i-numsets = 1.
 /* Valores do EIXO Y (Regular) */ 
 CREATE tt-dados.
 ASSIGN tt-dados.NumPoint   = i-point
        tt-dados.NumSet     = i-numsets
        tt-dados.NumGraph   = 1
        tt-dados.graphdata  = tt-tipo-def.qtd-regular.
 ASSIGN i-numsets = i-numsets + 1.

 /* Valores do EIXO Y (Leve defeito) */
 CREATE tt-dados.
 ASSIGN tt-dados.NumPoint   = i-point
        tt-dados.NumSet     = i-numsets
        tt-dados.NumGraph   = 1
        tt-dados.graphdata  = tt-tipo-def.qtd-leve-def.
 ASSIGN i-numsets = i-numsets + 1.

 /* Valores do EIXI Y (Retalho) */
 CREATE tt-dados.
 ASSIGN tt-dados.NumPoint   = i-point
        tt-dados.NumSet     = i-numsets
        tt-dados.NumGraph   = 1
        tt-dados.graphdata  = tt-tipo-def.qtd-retalho.
 ASSIGN i-numsets = i-numsets + 1.

 /* Valores do EIXI Y (Total Geral Defeitos) */
 CREATE tt-dados.
 ASSIGN tt-dados.NumPoint   = i-point
        tt-dados.NumSet     = i-numsets
        tt-dados.NumGraph   = 1
        tt-dados.graphdata  = DEC(fi-tot-prod-def:SCREEN-VALUE IN  FRAME {&FRAME-NAME}).
 ASSIGN i-point = i-point + 1.


 DEF VAR h-utapi011 AS HANDLE NO-UNDO.

 RUN utp/utapi011.p PERSISTENT SET h-utapi011.

 RUN pi-execute IN h-utapi011 (INPUT  TABLE tt-atributos,
                               INPUT  TABLE tt-points-2,
                               INPUT  TABLE tt-sets,
                               INPUT  TABLE tt-dados,
                               INPUT  TABLE tt-ylabels,
                               OUTPUT TABLE tt-erros).

 IF RETURN-VALUE = "NOK" THEN DO: 
    FOR EACH tt-erros: 
        DISP cod-erro desc-erro FORMAT "x(100)" WITH 1 COL WIDTH 500. 
    END.
 END.                  

 DELETE PROCEDURE h-utapi011.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-movto C-Win 
PROCEDURE pi-grava-movto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
 DEFINE INPUT PARAMETER p-it-codigo AS CHAR.

 FOR EACH ob-sl-estoq-per WHERE
          ob-sl-estoq-per.periodo       = c-periodo          AND 
          ob-sl-estoq-per.it-codigo     = p-it-codigo        AND
          ob-sl-estoq-per.cod-refer    >= c-cod-refer-ini    AND
          ob-sl-estoq-per.cod-refer    <= c-cod-refer-fin    AND
          ob-sl-estoq-per.corte-comerc >= c-corte-comerc-ini AND
          ob-sl-estoq-per.corte-comerc <= c-corte-comerc-fin AND
          ob-sl-estoq-per.cod-qualid   >= c-cod-qualid-ini   AND
          ob-sl-estoq-per.cod-qualid   <= c-cod-qualid-fin   AND
          LOOKUP(ob-sl-estoq-per.nr-lote,c-lotes) <> 0  NO-LOCK.

     FIND tt-work WHERE
          tt-work.it-codigo = p-it-codigo               AND
          tt-work.cod-refer = ob-sl-estoq-per.cod-refer AND
          tt-work.nr-lote   = ob-sl-estoq-per.nr-lote   NO-ERROR.
     IF NOT AVAIL tt-work THEN DO:
        CREATE tt-work.
        ASSIGN tt-work.it-codigo = p-it-codigo
               tt-work.cod-refer = ob-sl-estoq-per.cod-refer
               tt-work.nr-lote   = ob-sl-estoq-per.nr-lote.
     END.
     ASSIGN tt-work.qtd-inicial   = tt-work.qtd-inicial   + ob-sl-estoq-per.qtd-inicial
            tt-work.qtd-entr-est  = tt-work.qtd-entr-est  + ob-sl-estoq-per.qtd-entr-est
            tt-work.qtd-transf    = tt-work.qtd-transf    + ob-sl-estoq-per.qtd-transf
            tt-work.qtd-devolvida = tt-work.qtd-devolvida + ob-sl-estoq-per.qtd-devolvida
            tt-work.qtd-faturada  = tt-work.qtd-faturada  + ob-sl-estoq-per.qtd-faturada
            tt-work.qtd-final     = tt-work.qtd-final     + ob-sl-estoq-per.qtd-final
            tt-work.qtd-real      = tt-work.qtd-real      + ob-sl-estoq-per.qtd-real.
 END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-analitico C-Win 
PROCEDURE pi-imp-analitico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
          PUT CONTROL "~033E~033(s18H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0181-ana.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
      ASSIGN i-pag      =  1
             i-lin      = 99.
      FOR EACH b-tt-tipo-def WHERE
               b-tt-tipo-def.cod-tipo-def >= c-tipo-defeito-ini AND
               b-tt-tipo-def.cod-tipo-def <= c-tipo-defeito-fin NO-LOCK.
    
          FOR EACH b-tt-defeitos WHERE
                   b-tt-defeitos.cod-tipo-def = b-tt-tipo-def.cod-tipo-def
                    NO-LOCK
                 BY (b-tt-defeitos.qtd-regular  +
                     b-tt-defeitos.qtd-leve-def +
                     b-tt-defeitos.qtd-retalho) DESCEND.
    
              FOR EACH b-tt-itens WHERE
                       b-tt-itens.cod-tipo-def = b-tt-tipo-def.cod-tipo-def AND
                       b-tt-itens.cod-defeito  = b-tt-defeitos.cod-defeito  AND
                       b-tt-itens.it-codigo   >= c-it-codigo-ini            AND
                       b-tt-itens.it-codigo   <= c-it-codigo-fin
                       NO-LOCK
                    BY (b-tt-itens.qtd-regular  +        
                        b-tt-itens.qtd-leve-def +        
                        b-tt-itens.qtd-retalho) DESCEND. 
    
                  IF i-lin > 62 THEN DO:
                     RUN pi-imp-cabec.
                     ASSIGN i-lin = 8.
                  END.
    
                  FIND ITEM WHERE
                       ITEM.it-codigo = b-tt-itens.it-codigo NO-LOCK NO-ERROR.
    
                  PUT b-tt-itens.it-codigo        FORMAT "X(6)"           AT   1
                      item.desc-item              FORMAT "x(29)"          AT   8 
                      b-tt-itens.qtd-regular      FORMAT ">>>,>>>,>>9.99" AT  39.
                  IF b-tt-itens.qtd-regular > 0 THEN
                     PUT b-tt-itens.qtd-regular /
                         fi-tot-tp-def-rg  * 100 FORMAT ">>9.99" AT  54.
                  PUT b-tt-itens.qtd-leve-def FORMAT ">>>,>>>,>>9.99" AT  62.
                  IF b-tt-itens.qtd-leve-def > 0 THEN
                     PUT b-tt-itens.qtd-leve-def /
                         fi-tot-tp-def-ld * 100 FORMAT ">>9.99"   AT 77.
                  PUT b-tt-itens.qtd-retalho FORMAT ">>>,>>>,>>9.99" AT 84.
                  IF b-tt-itens.qtd-retalho > 0 THEN
                     PUT b-tt-itens.qtd-retalho /
                         fi-tot-tp-def-rt  * 100  FORMAT ">>9.99" AT 99.
                  PUT (b-tt-itens.qtd-regular  + 
                       b-tt-itens.qtd-leve-def +
                       b-tt-itens.qtd-retalho)    FORMAT ">>>,>>>,>>9.99" AT 106.
                  IF b-tt-itens.qtd-regular + b-tt-itens.qtd-regular + b-tt-itens.qtd-retalho > 0 THEN
                     PUT (b-tt-itens.qtd-regular  + 
                          b-tt-itens.qtd-leve-def +
                          b-tt-itens.qtd-retalho) /
                          fi-tot-tp-def-ger * 100 FORMAT ">>9.99" AT 121.
                  ASSIGN i-lin = i-lin + 1.
              END.
              IF i-lin > 62 THEN DO:
                 RUN pi-imp-cabec.
                 ASSIGN i-lin = 8.
              END.
    
              FIND defeito WHERE
                   defeito.cod-tipo-def = b-tt-defeitos.cod-tipo-def AND
                   defeito.cod-defeito  = b-tt-defeitos.cod-defeito NO-LOCK NO-ERROR.
    
              PUT b-tt-defeitos.cod-defeito                           AT   1
                  defeito.descricao           FORMAT "x(30)"          AT   8 
                  b-tt-defeitos.qtd-regular   FORMAT ">>>,>>>,>>9.99" AT  39.
              IF b-tt-defeitos.qtd-regular > 0 THEN
                 PUT  b-tt-defeitos.qtd-regular /
                      b-tt-tipo-def.qtd-regular  * 100  FORMAT ">>9.99" AT 54.
              PUT b-tt-defeitos.qtd-leve-def  FORMAT ">>>,>>>,>>9.99" AT  62.
              IF b-tt-defeitos.qtd-leve-def > 0 THEN
                 PUT b-tt-defeitos.qtd-leve-def /
                     b-tt-tipo-def.qtd-leve-def * 100 FORMAT ">>9.99" AT 77.
              PUT b-tt-defeitos.qtd-retalho FORMAT ">>>,>>>,>>9.99" AT 84.
              IF b-tt-defeitos.qtd-retalho > 0 THEN
                 PUT  b-tt-defeitos.qtd-retalho /
                      b-tt-tipo-def.qtd-retalho  * 100 FORMAT ">>9.99" AT 99.
              PUT (b-tt-defeitos.qtd-regular  + 
                   b-tt-defeitos.qtd-leve-def +
                   b-tt-defeitos.qtd-retalho) FORMAT ">>>,>>>,>>9.99" AT 106.
              IF b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho > 0 THEN
                 PUT (b-tt-defeitos.qtd-regular  + 
                      b-tt-defeitos.qtd-leve-def +
                      b-tt-defeitos.qtd-retalho) /
                     (b-tt-tipo-def.qtd-regular  +
                      b-tt-tipo-def.qtd-leve-def +
                      b-tt-tipo-def.qtd-retalho)  * 100  FORMAT ">>9.99" AT 121.
              PUT (b-tt-defeitos.qtd-regular  + 
                   b-tt-defeitos.qtd-leve-def +
                   b-tt-defeitos.qtd-retalho) /
                   fi-tot-prod-ger  * 100   FORMAT ">>9.999"         AT 128 SKIP(1).
              ASSIGN i-lin = i-lin + 2.
          END.
          IF i-lin > 62 THEN DO:
             RUN pi-imp-cabec.
             ASSIGN i-lin = 8.
          END.
    
          FIND tipo-def WHERE
               tipo-def.cod-tipo-def = b-tt-tipo-def.cod-tipo-def NO-LOCK NO-ERROR.
          
          PUT b-tt-tipo-def.cod-tipo-def                        AT  1
              tipo-def.descricao        FORMAT "x(24)"          AT  8
              b-tt-tipo-def.qtd-regular FORMAT ">>>,>>>,>>9.99" AT 39.
          IF b-tt-tipo-def.qtd-regular > 0 THEN
             PUT b-tt-tipo-def.qtd-regular /
                 fi-ger-tp-def-rg * 100      FORMAT ">>9.99" AT  54.
          PUT b-tt-tipo-def.qtd-leve-def  FORMAT ">>>,>>>,>>9.99" AT  62.
          IF b-tt-tipo-def.qtd-leve-def > 0 THEN
             PUT b-tt-tipo-def.qtd-leve-def /
                 fi-ger-tp-def-ld * 100 FORMAT ">>9.99" AT 77.
          PUT b-tt-tipo-def.qtd-retalho FORMAT ">>>,>>>,>>9.99" AT 84.
          IF b-tt-tipo-def.qtd-retalho > 0 THEN
             PUT b-tt-tipo-def.qtd-retalho /
                fi-ger-tp-def-rt * 100 FORMAT ">>9.99" AT 99.
          PUT (b-tt-tipo-def.qtd-regular  + 
               b-tt-tipo-def.qtd-leve-def +
               b-tt-tipo-def.qtd-retalho) FORMAT ">>>,>>>,>>9.99" AT 106.
          IF b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho > 0 THEN
             PUT (b-tt-tipo-def.qtd-regular  + 
                  b-tt-tipo-def.qtd-leve-def +
                  b-tt-tipo-def.qtd-retalho) /
                  fi-ger-tp-def-ger * 100 FORMAT ">>9.99" AT 121.
          IF b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho > 0 THEN
             PUT (b-tt-tipo-def.qtd-regular  + 
                  b-tt-tipo-def.qtd-leve-def +
                  b-tt-tipo-def.qtd-retalho) /
                  fi-tot-prod-ger  * 100 FORMAT ">>9.999" AT 128 SKIP(1).

          ASSIGN i-lin = i-lin + 2.
      END.
      IF i-lin > 62 THEN DO:
         RUN pi-imp-cabec.
         ASSIGN i-lin = 8.
      END.
      PUT "TOTAL GERAL..............." AT 1.
      PUT fi-ger-tp-def-rg  FORMAT ">>>,>>>,>>9.99"        AT  39
          fi-ger-tp-def-ld  FORMAT ">>>,>>>,>>9.99"        AT  62
          fi-ger-tp-def-rt  FORMAT ">>>,>>>,>>9.99"        AT  84
          fi-ger-tp-def-ger FORMAT ">>>,>>>,>>9.99"        AT 107
          fi-tot-prod-ger   FORMAT ">>>,>>>,>>9.99"        AT 121.
      IF i-saida = 1 THEN DO:
         PAGE.
         PUT "" AT 1.
      END.
  END.
  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                               INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.
  OUTPUT CLOSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec C-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT c-empresa  FORMAT "X(40)"                 AT   1
        "DATA: "                                  AT  65
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  71
        "HORA: "                                  AT  86
        STRING(TIME,"hh:mm:ss")                   AT  92
        "PAG:"                                    AT 127
        i-pag FORMAT ">>>"                        AT 132
        SKIP(1).

    IF i-tipo-rel = 1 THEN DO:
       PUT "RELATORIO DA ANALISE DOS DEFEITOS NA PRODU€ÇO"  AT 52 SKIP.
       PUT "PERIODO: "                                      AT 57 
       STRING(da-data-mov-ini, "99/99/9999") FORMAT "x(10)" AT 66.
       PUT "A"                                              AT 78
       STRING(da-data-mov-fin, "99/99/9999") FORMAT "x(10)" AT 81 SKIP(1).

       PUT "COD DEFEITOS                              REGULAR    %      LEVE DEFEITO    %          RETALHO    %   TOTAL DEFEITOS    %    %S/PRD" AT 1.
       PUT "--- ------------------------------ -------------- ------ --------------- ------ -------------- ------ -------------- ------ -------" AT 1.
    END.
     ELSE DO:
       PUT "RELATORIO DA ANALISE DOS DEFEITOS POR ITENS NA PRODU€ÇO"  AT 40 SKIP.
       PUT "PERIODO: "                                      AT 50 
       STRING(da-data-mov-ini, "99/99/9999") FORMAT "x(10)" AT 59.
       PUT "A"                                              AT 71
       STRING(da-data-mov-fin, "99/99/9999") FORMAT "x(10)" AT 74 SKIP(1).

       PUT "CODIGO DESCRI€ÇO                             REGULAR    %      LEVE DEFEITO    %          RETALHO    %   TOTAL DEFEITOS    %    %S/PRD" AT 1.
       PUT "------ ------------------------------ -------------- ------ --------------- ------ -------------- ------ -------------- ------ -------" AT 1.
     END.
    ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-sintetico C-Win 
PROCEDURE pi-imp-sintetico :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR h-prog AS HANDLE NO-UNDO.
  DEF VAR i-ct   AS INT.

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
          PUT CONTROL "~033E~033(s18H".    
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0181-sint.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag      =  1
            i-lin      = 99.
     FOR EACH b-tt-tipo-def WHERE
              b-tt-tipo-def.cod-tipo-def >= c-tipo-defeito-ini AND
              b-tt-tipo-def.cod-tipo-def <= c-tipo-defeito-fin NO-LOCK.
    
         FOR EACH b-tt-defeitos WHERE
                  b-tt-defeitos.cod-tipo-def = b-tt-tipo-def.cod-tipo-def
                  NO-LOCK
               BY (b-tt-defeitos.qtd-regular  +
                   b-tt-defeitos.qtd-leve-def +
                   b-tt-defeitos.qtd-retalho) DESCEND.
    
             IF i-lin > 62 THEN DO:
                RUN pi-imp-cabec.
                ASSIGN i-lin = 8.
             END.
    
             FIND defeito WHERE
                  defeito.cod-tipo-def = b-tt-defeitos.cod-tipo-def AND
                  defeito.cod-defeito  = b-tt-defeitos.cod-defeito NO-LOCK NO-ERROR.
    
             PUT b-tt-defeitos.cod-defeito   FORMAT "X(2)"           AT   1
                 defeito.descricao           FORMAT "x(30)"          AT   5 
                 b-tt-defeitos.qtd-regular   FORMAT ">>>,>>>,>>9.99" AT  36.
                 IF b-tt-defeitos.qtd-regular > 0 THEN
                    PUT b-tt-defeitos.qtd-regular /
                        b-tt-tipo-def.qtd-regular  * 100  FORMAT ">>9.99"   AT  51.
                 PUT b-tt-defeitos.qtd-leve-def  FORMAT ">>>,>>>,>>9.99" AT  59.
                 IF b-tt-defeitos.qtd-leve-def > 0 THEN
                    PUT b-tt-defeitos.qtd-leve-def /
                        b-tt-tipo-def.qtd-leve-def * 100  FORMAT ">>9.99"   AT  74.
                 PUT b-tt-defeitos.qtd-retalho   FORMAT ">>>,>>>,>>9.99" AT  81.
                 IF b-tt-defeitos.qtd-retalho > 0 THEN
                    PUT b-tt-defeitos.qtd-retalho /
                        b-tt-tipo-def.qtd-retalho  * 100  FORMAT ">>9.99"   AT  96.
                 PUT (b-tt-defeitos.qtd-regular  + 
                      b-tt-defeitos.qtd-leve-def +
                      b-tt-defeitos.qtd-retalho) FORMAT ">>>,>>>,>>9.99" AT 103.
                 IF b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho > 0 THEN
                    PUT (b-tt-defeitos.qtd-regular  + 
                         b-tt-defeitos.qtd-leve-def +
                         b-tt-defeitos.qtd-retalho) /
                        (b-tt-tipo-def.qtd-regular  +
                         b-tt-tipo-def.qtd-leve-def +
                         b-tt-tipo-def.qtd-retalho)  * 100  FORMAT ">>9.99" AT 118.
                 IF b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho > 0 THEN
                    PUT (b-tt-defeitos.qtd-regular  + 
                         b-tt-defeitos.qtd-leve-def +
                         b-tt-defeitos.qtd-retalho) /
                         fi-tot-prod-ger * 100  FORMAT ">>9.999"           AT 125.
             ASSIGN i-lin = i-lin + 1.
         END.
         IF i-lin > 62 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 8.
         END.
    
         FIND tipo-def WHERE
              tipo-def.cod-tipo-def = b-tt-tipo-def.cod-tipo-def NO-LOCK NO-ERROR.
    
         PUT "TOTAL " + tipo-def.descricao       FORMAT "x(34)" AT 1.
         PUT b-tt-tipo-def.qtd-regular  FORMAT ">>>,>>>,>>9.99" AT  36.
         IF b-tt-tipo-def.qtd-regular > 0 THEN
            PUT  b-tt-tipo-def.qtd-regular /
                 fi-ger-tp-def-rg * 100   FORMAT ">>9.99" AT  51.
         PUT b-tt-tipo-def.qtd-leve-def FORMAT ">>>,>>>,>>9.99" AT  59.
         IF b-tt-tipo-def.qtd-leve-def > 0 THEN
            PUT  b-tt-tipo-def.qtd-leve-def /
                 fi-ger-tp-def-ld * 100   FORMAT ">>9.99" AT  74.
         PUT b-tt-tipo-def.qtd-retalho  FORMAT ">>>,>>>,>>9.99" AT  81.
         IF b-tt-tipo-def.qtd-retalho > 0 THEN
            PUT b-tt-tipo-def.qtd-retalho /
                fi-ger-tp-def-rt * 100 FORMAT ">>9.99" AT  96.
         PUT (b-tt-tipo-def.qtd-regular  + 
              b-tt-tipo-def.qtd-leve-def +
              b-tt-tipo-def.qtd-retalho)FORMAT ">>>,>>>,>>9.99" AT 103.
         IF b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho > 0 THEN
            PUT (b-tt-tipo-def.qtd-regular  + 
                 b-tt-tipo-def.qtd-leve-def +
                 b-tt-tipo-def.qtd-retalho) /
                 fi-ger-tp-def-ger * 100  FORMAT ">>9.99" AT 118.
         IF b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho > 0 THEN
            PUT (b-tt-tipo-def.qtd-regular  + 
                 b-tt-tipo-def.qtd-leve-def +
                 b-tt-tipo-def.qtd-retalho) /
                 fi-tot-prod-ger  * 100   FORMAT ">>9.999" AT 125 SKIP(1).
         ASSIGN i-lin = i-lin + 2.
     END.
     IF i-lin > 62 THEN DO:
        RUN pi-imp-cabec.
        ASSIGN i-lin = 8.
     END.
     PUT "TOTAL GERAL..............." AT 1.
     PUT fi-ger-tp-def-rg  FORMAT ">>>,>>>,>>9.99"        AT  36
         fi-ger-tp-def-ld  FORMAT ">>>,>>>,>>9.99"        AT  59
         fi-ger-tp-def-rt  FORMAT ">>>,>>>,>>9.99"        AT  81
         fi-ger-tp-def-ger FORMAT ">>>,>>>,>>9.99"        AT 103
         fi-tot-prod-ger   FORMAT ">>>,>>>,>>9.99"        AT 118.
     IF i-saida = 1 THEN DO:
        PAGE.
        PUT "" AT 1.
     END.
  END.
  IF i-saida = 3 THEN DO.
     RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                           INPUT c-saida).
     DELETE PROCEDURE h-prog.
  END.
  OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime C-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 IF i-tipo-rel = 1 THEN
    RUN pi-imp-sintetico.
 ELSE
    RUN pi-imp-analitico.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-planilha-analitica C-Win 
PROCEDURE pi-planilha-analitica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Cabe‡alho  da Planilha */
    ASSIGN c-Lin = c-empresa + "                    " + " ANALISE ANALITICA DE DEFEITOS NA PRODU€ÇO NO PERIODO: "  + STRING(da-data-mov-ini, "99/99/9999") + " A " + STRING(da-data-mov-fin, "99/99/9999"). 
    DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
    DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C11")]'.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".

    /* Cabe‡alho dos Dados */
    DDE SEND i-canal SOURCE "ITEM"            ITEM "L3C1".
    DDE SEND i-canal SOURCE "DESCRICAO"       ITEM "L3C2".
    DDE SEND i-canal SOURCE "REGULAR"         ITEM "L3C3".
    DDE SEND i-canal SOURCE "  %  "           ITEM "L3C4".
    DDE SEND i-canal SOURCE "LEVE DEFEITO"    ITEM "L3C5".
    DDE SEND i-canal SOURCE "  %  "           ITEM "L3C6".
    DDE SEND i-canal SOURCE "RETALHO"         ITEM "L3C7".
    DDE SEND i-canal SOURCE "  %  "           ITEM "L3C8".
    DDE SEND i-canal SOURCE "TOTAL DEFEITOS"  ITEM "L3C9".
    DDE SEND i-canal SOURCE "  %  "           ITEM "L3C10".
    DDE SEND i-canal SOURCE "%s/PRD"          ITEM "L3C11".
    

    /* Formata‡Æo das Celulas do Cabe‡alho de Dados */
    DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C11")]'.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(6.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(38.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]". 
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,000~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".
    
    /* Montagem das Celulas de Dados */
    ASSIGN i-Lin = 4.
    FOR EACH b-tt-tipo-def WHERE
             b-tt-tipo-def.cod-tipo-def >= c-tipo-defeito-ini AND
             b-tt-tipo-def.cod-tipo-def <= c-tipo-defeito-fin NO-LOCK.

        FOR EACH b-tt-defeitos WHERE
                 b-tt-defeitos.cod-tipo-def = b-tt-tipo-def.cod-tipo-def
                 NO-LOCK
              BY (b-tt-defeitos.qtd-regular  +
                  b-tt-defeitos.qtd-leve-def +
                  b-tt-defeitos.qtd-retalho) DESCEND.

            FOR EACH b-tt-itens WHERE
                     b-tt-itens.cod-tipo-def = b-tt-tipo-def.cod-tipo-def AND
                     b-tt-itens.cod-defeito  = b-tt-defeitos.cod-defeito
                     NO-LOCK
                  BY (b-tt-itens.qtd-regular  +        
                      b-tt-itens.qtd-leve-def +        
                      b-tt-itens.qtd-retalho) DESCEND. 

                FIND ITEM WHERE
                     ITEM.it-codigo = b-tt-itens.it-codigo NO-LOCK NO-ERROR.

                DDE SEND i-canal SOURCE STRING(b-tt-itens.it-codigo)                                                                                     ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
                DDE SEND i-canal SOURCE STRING(TRIM(item.desc-item))                                                                                     ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
                DDE SEND i-canal SOURCE STRING(b-tt-itens.qtd-regular)                                                                                   ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
                IF b-tt-itens.qtd-regular > 0 THEN
                   DDE SEND i-canal SOURCE STRING((b-tt-itens.qtd-regular / fi-tot-tp-def-rg * 100))                                                     ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
                DDE SEND i-canal SOURCE STRING(b-tt-itens.qtd-leve-def)                                                                                  ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
                IF b-tt-itens.qtd-leve-def > 0 THEN
                   DDE SEND i-canal SOURCE STRING((b-tt-itens.qtd-leve-def / fi-tot-tp-def-ld * 100))                                                    ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
                DDE SEND i-canal SOURCE STRING(b-tt-itens.qtd-retalho)                                                                                   ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
                IF b-tt-itens.qtd-retalho > 0 THEN
                   DDE SEND i-canal SOURCE STRING((b-tt-itens.qtd-retalho / fi-tot-tp-def-rt * 100))                                                     ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
                DDE SEND i-canal SOURCE STRING((b-tt-itens.qtd-regular + b-tt-itens.qtd-leve-def + b-tt-itens.qtd-retalho))                              ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
                IF b-tt-itens.qtd-regular + b-tt-itens.qtd-leve-def + b-tt-itens.qtd-retalho > 0 THEN
                   DDE SEND i-canal SOURCE STRING(((b-tt-itens.qtd-regular + b-tt-itens.qtd-leve-def + b-tt-itens.qtd-retalho) /  fi-tot-tp-def-ger * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
                DDE SEND i-canal SOURCE STRING(((b-tt-itens.qtd-regular  +  b-tt-itens.qtd-leve-def +  b-tt-itens.qtd-retalho) / 
                                                 fi-tot-prod-ger * 100))                                                                                 ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
                ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
                DDE EXECUTE i-canal COMMAND aux-command.
                /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
                DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
                ASSIGN i-lin = i-lin + 1.
            END.

            FIND defeito WHERE
                 defeito.cod-tipo-def = b-tt-defeitos.cod-tipo-def AND
                 defeito.cod-defeito  = b-tt-defeitos.cod-defeito NO-LOCK NO-ERROR.


            DDE SEND i-canal SOURCE STRING(b-tt-defeitos.cod-defeito) ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
            DDE SEND i-canal SOURCE STRING(TRIM(defeito.descricao))   ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
            DDE SEND i-canal SOURCE STRING(b-tt-defeitos.qtd-regular) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".

            IF b-tt-defeitos.qtd-regular > 0 THEN
               DDE SEND i-canal SOURCE STRING((b-tt-defeitos.qtd-regular / b-tt-tipo-def.qtd-regular  * 100))   ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
            DDE SEND i-canal SOURCE STRING(b-tt-defeitos.qtd-leve-def)                                          ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
            IF b-tt-defeitos.qtd-leve-def > 0 THEN
               DDE SEND i-canal SOURCE STRING((b-tt-defeitos.qtd-leve-def / b-tt-tipo-def.qtd-leve-def * 100))  ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
            DDE SEND i-canal SOURCE STRING(b-tt-defeitos.qtd-retalho)  ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
            IF b-tt-defeitos.qtd-retalho > 0 THEN 
               DDE SEND i-canal SOURCE STRING((b-tt-defeitos.qtd-retalho / b-tt-tipo-def.qtd-retalho  * 100))  ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
            DDE SEND i-canal SOURCE STRING((b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho)) ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
            IF b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho > 0 THEN
               DDE SEND i-canal SOURCE STRING(((b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho) /
                                               (b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho) * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
            IF b-tt-defeitos.qtd-regular  +  b-tt-defeitos.qtd-leve-def +  b-tt-defeitos.qtd-retalho > 0 THEN
               DDE SEND i-canal SOURCE STRING(((b-tt-defeitos.qtd-regular  +  b-tt-defeitos.qtd-leve-def +  b-tt-defeitos.qtd-retalho) / 
                                                fi-tot-prod-ger * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C11".

            ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
            DDE EXECUTE i-canal COMMAND aux-command.
            /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
/*            DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]". */
            DDE EXECUTE sys COMMAND "[format.font(~"Lucida Sans~",8,True,False,False,False,10)]".

            ASSIGN i-Lin = i-Lin + 1.
        END.

        FIND tipo-def WHERE
             tipo-def.cod-tipo-def = b-tt-tipo-def.cod-tipo-def NO-LOCK NO-ERROR.
        DDE SEND i-canal SOURCE STRING(TRIM(tipo-def.descricao))  ITEM "L" + TRIM(STRING(i-Lin)) + "C2".  

        DDE SEND i-canal SOURCE STRING(b-tt-tipo-def.qtd-regular) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
        IF b-tt-tipo-def.qtd-regular > 0 THEN
           DDE SEND i-canal SOURCE STRING((b-tt-tipo-def.qtd-regular / fi-ger-tp-def-rg * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
        DDE SEND i-canal SOURCE STRING(b-tt-tipo-def.qtd-leve-def) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
        IF b-tt-tipo-def.qtd-leve-def > 0 THEN
           DDE SEND i-canal SOURCE STRING((b-tt-tipo-def.qtd-leve-def / fi-ger-tp-def-ld * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
        DDE SEND i-canal SOURCE STRING(b-tt-tipo-def.qtd-retalho) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
        IF b-tt-tipo-def.qtd-retalho > 0 THEN
           DDE SEND i-canal SOURCE STRING((b-tt-tipo-def.qtd-retalho / fi-ger-tp-def-rt  * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
        DDE SEND i-canal SOURCE STRING((b-tt-tipo-def.qtd-regular  +  b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho)) ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
        IF b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho > 0 THEN
           DDE SEND i-canal SOURCE STRING(((b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho) /
                                            fi-ger-tp-def-ger  * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C10". 
        IF b-tt-tipo-def.qtd-regular  +  b-tt-tipo-def.qtd-leve-def +  b-tt-tipo-def.qtd-retalho > 0 THEN
           DDE SEND i-canal SOURCE STRING(((b-tt-tipo-def.qtd-regular  +  b-tt-tipo-def.qtd-leve-def +  b-tt-tipo-def.qtd-retalho) / 
                                            fi-tot-prod-ger * 100))  ITEM "L" + TRIM(STRING(i-Lin)) + "C11".

        /* Formata‡Æo das Celulas do Cabe‡alho de Dados */
        ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
        DDE EXECUTE i-canal COMMAND aux-command.
        /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",9,True,False,False,False,3)]".
        ASSIGN i-Lin = i-Lin + 2.
   END.

   DDE SEND i-canal SOURCE "TOTAL DA PRODU€ÇO: " + STRING(fi-tot-prod-ger,">>,>>>,>>9.99")   ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
   DDE SEND i-canal SOURCE STRING(fi-ger-tp-def-rg)                 ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
   DDE SEND i-canal SOURCE STRING(fi-ger-tp-def-ld)                 ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
   DDE SEND i-canal SOURCE STRING(fi-ger-tp-def-rt)                 ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
   DDE SEND i-canal SOURCE STRING(fi-ger-tp-def-ger)                ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
   ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
   DDE EXECUTE i-canal COMMAND aux-command.
   /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
   DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",9,True,False,False,False,5)]".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-planilha-sintetica C-Win 
PROCEDURE pi-planilha-sintetica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Cabe‡alho  da Planilha */
    ASSIGN c-Lin = c-empresa + "                    " + " ANALISE SINTETICA DE DEFEITOS NA PRODU€ÇO NO PERIODO: "  + STRING(da-data-mov-ini, "99/99/9999") + " A " + STRING(da-data-mov-fin, "99/99/9999"). 
    DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
    DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C11")]'.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".

    /* Cabe‡alho dos Dados */
    DDE SEND i-canal SOURCE "COD"             ITEM "L3C1".
    DDE SEND i-canal SOURCE "DEFEITOS"        ITEM "L3C2".
    DDE SEND i-canal SOURCE "REGULAR"         ITEM "L3C3".
    DDE SEND i-canal SOURCE "  %  "           ITEM "L3C4".
    DDE SEND i-canal SOURCE "LEVE DEFEITO"    ITEM "L3C5".
    DDE SEND i-canal SOURCE "  %  "           ITEM "L3C6".
    DDE SEND i-canal SOURCE "RETALHO"         ITEM "L3C7".
    DDE SEND i-canal SOURCE "  %  "           ITEM "L3C8".
    DDE SEND i-canal SOURCE "TOTAL DEFEITOS"  ITEM "L3C9".
    DDE SEND i-canal SOURCE "  %  "           ITEM "L3C10".
    DDE SEND i-canal SOURCE "%s/PRD"          ITEM "L3C11".
    

    /* Formata‡Æo das Celulas do Cabe‡alho de Dados */
    DDE EXECUTE i-canal COMMAND '[select("L3C1:L3C11")]'.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,True,False,False,False,5)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(4.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(38.00)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]". 
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(8.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.00)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"##0,000~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".
    
    /* Montagem das Celulas de Dados */
    ASSIGN i-Lin = 4.
    FOR EACH b-tt-tipo-def WHERE
             b-tt-tipo-def.cod-tipo-def >= c-tipo-defeito-ini AND
             b-tt-tipo-def.cod-tipo-def <= c-tipo-defeito-fin NO-LOCK.

        FOR EACH b-tt-defeitos WHERE
                 b-tt-defeitos.cod-tipo-def = b-tt-tipo-def.cod-tipo-def
                 NO-LOCK
              BY (b-tt-defeitos.qtd-regular  +
                  b-tt-defeitos.qtd-leve-def +
                  b-tt-defeitos.qtd-retalho) DESCEND.


            FIND defeito WHERE
                 defeito.cod-tipo-def = b-tt-defeitos.cod-tipo-def AND
                 defeito.cod-defeito  = b-tt-defeitos.cod-defeito NO-LOCK NO-ERROR.

            DDE SEND i-canal SOURCE STRING(b-tt-defeitos.cod-defeito) ITEM "L" + TRIM(STRING(i-Lin)) + "C1". 
            DDE SEND i-canal SOURCE STRING(TRIM(defeito.descricao))  ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
            DDE SEND i-canal SOURCE STRING(b-tt-defeitos.qtd-regular) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
            IF b-tt-defeitos.qtd-regular > 0 THEN
               DDE SEND i-canal SOURCE STRING((b-tt-defeitos.qtd-regular / b-tt-tipo-def.qtd-regular  * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
            DDE SEND i-canal SOURCE STRING(b-tt-defeitos.qtd-leve-def) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
            IF b-tt-defeitos.qtd-leve-def > 0 THEN
               DDE SEND i-canal SOURCE STRING((b-tt-defeitos.qtd-leve-def / b-tt-tipo-def.qtd-leve-def * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
            DDE SEND i-canal SOURCE STRING(b-tt-defeitos.qtd-retalho) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
            IF b-tt-defeitos.qtd-retalho > 0 THEN
               DDE SEND i-canal SOURCE STRING((b-tt-defeitos.qtd-retalho / b-tt-tipo-def.qtd-retalho  * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
            DDE SEND i-canal SOURCE STRING((b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho)) ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
            IF b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho > 0 THEN
               DDE SEND i-canal SOURCE STRING(((b-tt-defeitos.qtd-regular + b-tt-defeitos.qtd-leve-def + b-tt-defeitos.qtd-retalho) /
                                               (b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho) * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
            IF b-tt-defeitos.qtd-regular  +  b-tt-defeitos.qtd-leve-def +  b-tt-defeitos.qtd-retalho > 0 THEN
               DDE SEND i-canal SOURCE STRING(((b-tt-defeitos.qtd-regular  +  b-tt-defeitos.qtd-leve-def +  b-tt-defeitos.qtd-retalho) / 
                                                fi-tot-prod-ger * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C11".

            ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
            DDE EXECUTE i-canal COMMAND aux-command.
            /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
            DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".

            ASSIGN i-Lin = i-Lin + 1.
        END.

        FIND tipo-def WHERE
             tipo-def.cod-tipo-def = b-tt-tipo-def.cod-tipo-def NO-LOCK NO-ERROR.
        DDE SEND i-canal SOURCE STRING(TRIM(tipo-def.descricao)) ITEM "L" + TRIM(STRING(i-Lin)) + "C2".  
        DDE SEND i-canal SOURCE STRING(b-tt-tipo-def.qtd-regular) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
        IF b-tt-tipo-def.qtd-regular > 0 THEN
           DDE SEND i-canal SOURCE STRING((b-tt-tipo-def.qtd-regular / fi-ger-tp-def-rg * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
        DDE SEND i-canal SOURCE STRING(b-tt-tipo-def.qtd-leve-def) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
        IF b-tt-tipo-def.qtd-leve-def > 0 THEN
           DDE SEND i-canal SOURCE STRING((b-tt-tipo-def.qtd-leve-def / fi-ger-tp-def-ld * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
        DDE SEND i-canal SOURCE STRING(b-tt-tipo-def.qtd-retalho) ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
        IF b-tt-tipo-def.qtd-retalho > 0 THEN
           DDE SEND i-canal SOURCE STRING((b-tt-tipo-def.qtd-retalho / fi-ger-tp-def-rt  * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
        DDE SEND i-canal SOURCE STRING((b-tt-tipo-def.qtd-regular  +  b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho)) ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
        IF b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho > 0 THEN
           DDE SEND i-canal SOURCE STRING(((b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho) /
                                            fi-ger-tp-def-ger  * 100))  ITEM "L" + TRIM(STRING(i-Lin)) + "C10". 
        IF b-tt-tipo-def.qtd-regular + b-tt-tipo-def.qtd-leve-def + b-tt-tipo-def.qtd-retalho > 0 THEN
           DDE SEND i-canal SOURCE STRING(((b-tt-tipo-def.qtd-regular  +  b-tt-tipo-def.qtd-leve-def +  b-tt-tipo-def.qtd-retalho) / 
                                            fi-tot-prod-ger * 100)) ITEM "L" + TRIM(STRING(i-Lin)) + "C11".

        /* Formata‡Æo das Celulas do Cabe‡alho de Dados */
        ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
        DDE EXECUTE i-canal COMMAND aux-command.
        /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
        DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",9,True,False,False,False,3)]".
        ASSIGN i-Lin = i-Lin + 2.
   END.

   DDE SEND i-canal SOURCE "TOTAL DA PRODU€ÇO: " + STRING(fi-tot-prod-ger,">>,>>>,>>9.99")   ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
   DDE SEND i-canal SOURCE STRING(fi-ger-tp-def-rg)                 ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
   DDE SEND i-canal SOURCE STRING(fi-ger-tp-def-ld)                 ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
   DDE SEND i-canal SOURCE STRING(fi-ger-tp-def-rt)                 ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
   DDE SEND i-canal SOURCE STRING(fi-ger-tp-def-ger)                ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
   ASSIGN aux-command = '[select("L' + TRIM(STRING(i-Lin)) + 'C1:L' + TRIM(STRING(i-Lin)) + 'C11")]'.
   DDE EXECUTE i-canal COMMAND aux-command.
   /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
   DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",9,True,False,False,False,5)]".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Limpa Tabelas Temporarias */
   EMPTY TEMP-TABLE tt-tipo-def.
   EMPTY TEMP-TABLE tt-defeitos.
   EMPTY TEMP-TABLE tt-itens.
   EMPTY TEMP-TABLE tt-nuance.

   /* Busca Nome da Empresa */
   FIND FIRST param-global NO-LOCK NO-ERROR.
   FIND FIRST empresa WHERE
              empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
   ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

   /* Carrega a Tabela Temporaria das OB's a serem EXCLUIDDAS */
   IF l-excluir-ob = YES THEN DO:
      INPUT FROM VALUE(c-cam-nom-lista) CONVERT SOURCE "ibm850". 
      SET ^.
      REPEAT:
         CREATE tt-excluir-ob.
         IMPORT DELIMITER ";" tt-excluir-ob.
      END.
      INPUT CLOSE.
      FOR EACH tt-excluir-ob WHERE tt-excluir-ob.num-lote = 0:
         DELETE tt-excluir-ob.  
      END.
   END.

   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Selecionando_Defeitos *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
     
   /* Datas do Periodo Anterior */
   ASSIGN da-dt-ant-fin = da-data-mov-ini - 1
          da-dt-ant-ini = DATE("01" + STRING(MONTH(da-dt-ant-fin),'99') +
                                      STRING(YEAR(da-dt-ant-fin),'9999')).

   /* Zera Acumuladores */
   ASSIGN fi-tot-prod-per   = 0
          fi-tot-prod-def   = 0
          fi-tot-prod-sob   = 0
          fi-tot-prod-ger   = 0
          fi-tot-tp-def-rg  = 0
          fi-tot-tp-def-ld  = 0
          fi-tot-tp-def-rt  = 0
          fi-tot-tp-def-ger = 0
          fi-ger-tp-def-rg  = 0
          fi-ger-tp-def-ld  = 0
          fi-ger-tp-def-rt  = 0
          fi-ger-tp-def-ger = 0
          fi-def-rg-ant     = 0
          fi-def-ld-ant     = 0
          fi-def-rt-ant     = 0
          fi-tot-ant        = 0
          fi-ger-rg-ant     = 0
          fi-ger-ld-ant     = 0
          fi-ger-rt-ant     = 0
          fi-ger-ant        = 0
          fi-perc-rg        = 0
          fi-perc-ld        = 0
          fi-perc-rt        = 0
          fi-perc-tt        = 0
          fi-perc-per       = 0
          fi-perc-def       = 0
          fi-perc-sob       = 0.

   /* Limpa Acumuladores na Tela */
   DISPLAY fi-tot-prod-per
           fi-tot-prod-def
           fi-tot-prod-sob
           fi-tot-prod-ger
           fi-tot-tp-def-rg 
           fi-tot-tp-def-ld 
           fi-tot-tp-def-rt 
           fi-tot-tp-def-ger
           fi-ger-tp-def-rg
           fi-ger-tp-def-ld
           fi-ger-tp-def-rt
           fi-ger-tp-def-ger
           fi-perc-rg  
           fi-perc-ld  
           fi-perc-rt  
           fi-perc-tt  
           fi-perc-per 
           fi-perc-def 
           fi-perc-sob 
           WITH FRAME {&FRAME-NAME}. 


   RUN pi-separa-defeitos. 

   RUN pi-finalizar in h-acomp. /* Limpa Mensagens no Video */

   {&OPEN-QUERY-br-tipo-def}
   {&OPEN-QUERY-br-defeitos}
   {&OPEN-QUERY-br-itens}

   APPLY 'value-changed' TO br-tipo-def IN FRAME {&FRAME-NAME}.
   APPLY 'entry'         TO br-tipo-def IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-separa-defeitos C-Win 
PROCEDURE pi-separa-defeitos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 /* Calcula o Total Geral de Defeitos */
 {utp/ut-liter.i Calculando_Total_Geral *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 /* Periodo Atual */
 FOR EACH mov-est-acbd WHERE
          mov-est-acbd.cod-estabel   >= c-cod-estabel-ini AND
          mov-est-acbd.cod-estabel   <= c-cod-estabel-fin AND
          mov-est-acbd.data-mov      >= da-data-mov-ini   AND
          mov-est-acbd.data-mov      <= da-data-mov-fin NO-LOCK.

    RUN pi-acompanhar in h-acomp (INPUT "Data: " +  STRING(mov-est-acbd.data-mov) + "  Item: " + mov-est-acbd.it-codigo).

    IF l-excluir-ob THEN DO:
        FIND tt-excluir-ob WHERE
             tt-excluir-ob.num-lote = mov-est-acbd.num-lote NO-LOCK NO-ERROR.
        IF AVAIL tt-excluir-ob THEN NEXT.
    END.
    
    CASE mov-est-acbd.classific:
        WHEN "RG" THEN
            ASSIGN fi-ger-tp-def-rg         = fi-ger-tp-def-rg + mov-est-acbd.qtd-defeit.

        WHEN "LD" THEN
            ASSIGN fi-ger-tp-def-ld         = fi-ger-tp-def-ld + mov-est-acbd.qtd-defeit.
        WHEN "RT" THEN
            ASSIGN fi-ger-tp-def-rt         = fi-ger-tp-def-rt + mov-est-acbd.qtd-defeit.
    END CASE.
 END.
 ASSIGN fi-ger-tp-def-ger = fi-ger-tp-def-rg +  fi-ger-tp-def-ld  + fi-ger-tp-def-rt. 


 /* Periodo Anterior */
 FOR EACH mov-est-acbd WHERE
          mov-est-acbd.cod-estabel   >= c-cod-estabel-ini AND
          mov-est-acbd.cod-estabel   <= c-cod-estabel-fin AND
          mov-est-acbd.data-mov      >= da-dt-ant-ini AND
          mov-est-acbd.data-mov      <= da-dt-ant-fin 
          NO-LOCK.

    RUN pi-acompanhar in h-acomp (INPUT "Data: " +  STRING(mov-est-acbd.data-mov) + "  Item: " + mov-est-acbd.it-codigo).

    IF l-excluir-ob THEN DO:
        FIND tt-excluir-ob WHERE
             tt-excluir-ob.num-lote = mov-est-acbd.num-lote NO-LOCK NO-ERROR.
        IF AVAIL tt-excluir-ob THEN NEXT.
    END.

    CASE mov-est-acbd.classific:
        WHEN "RG" THEN
            ASSIGN fi-ger-rg-ant = fi-ger-rg-ant + mov-est-acbd.qtd-defeit.
        WHEN "LD" THEN
            ASSIGN fi-ger-ld-ant = fi-ger-ld-ant + mov-est-acbd.qtd-defeit.
        WHEN "RT" THEN
            ASSIGN fi-ger-rt-ant = fi-ger-rt-ant + mov-est-acbd.qtd-defeit.
    END CASE.
 END.
 ASSIGN fi-ger-ant = fi-ger-rg-ant +  fi-ger-ld-ant  + fi-ger-rt-ant. 


 {utp/ut-liter.i Calculando_Defeitosl *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
 /* Periodo CORRENTE */
 FOR EACH mov-est-acbd WHERE
          mov-est-acbd.cod-estabel   >= c-cod-estabel-ini  AND
          mov-est-acbd.cod-estabel   <= c-cod-estabel-fin  AND
          mov-est-acbd.data-mov      >= da-data-mov-ini    AND
          mov-est-acbd.data-mov      <= da-data-mov-fin    AND  
          mov-est-acbd.cod-tipo-def  >= c-tipo-defeito-ini AND
          mov-est-acbd.cod-tipo-def  <= c-tipo-defeito-fin AND 
          mov-est-acbd.it-codigo     >= c-it-codigo-ini    AND
          mov-est-acbd.it-codigo     <= c-it-codigo-fin    AND
          mov-est-acbd.cod-refer     >= c-cod-refer-ini    AND
          mov-est-acbd.cod-refer     <= c-cod-refer-fin   
    NO-LOCK,
    EACH mov-est-acbm WHERE 
         mov-est-acbm.cod-estabel = mov-est-acbd.cod-estabel AND
         mov-est-acbm.data-mov    = mov-est-acbd.data-mov    AND
         mov-est-acbm.num-lote    = mov-est-acbd.num-lote    AND
         mov-est-acbm.it-codigo   = mov-est-acbd.it-codigo   AND
         mov-est-acbm.cod-refer   = mov-est-acbd.cod-refer
    NO-LOCK,     
    EACH item-ext WHERE
         item-ext.it-codigo = mov-est-acbd.it-codigo /*AND
         (item-ext.indigo   = YES AND c-tp-tecido = "I" OR 
          item-ext.indigo   = NO  AND c-tp-tecido = "O" OR
          c-tp-tecido = "A") */ NO-LOCK:  

    RUN pi-acompanhar in h-acomp (INPUT "Data: " +  STRING(mov-est-acbd.data-mov) + "  Item: " + mov-est-acbd.it-codigo).
    
    IF INDEX(c-tipo-tecelagem,mov-est-acbm.tipo-tear) = 0 THEN NEXT.

    IF l-excluir-ob THEN DO:
        FIND tt-excluir-ob WHERE
             tt-excluir-ob.num-lote = mov-est-acbd.num-lote NO-LOCK NO-ERROR.
        IF AVAIL tt-excluir-ob THEN NEXT.
    END.

    /* GRAVA TIPOS DE DEFEITOS */
    FIND tt-tipo-def WHERE 
         tt-tipo-def.cod-tipo-def = mov-est-acbd.cod-tipo-def NO-ERROR.
    IF NOT AVAIL tt-tipo-def THEN DO:
       CREATE tt-tipo-def.
       ASSIGN tt-tipo-def.cod-tipo-def = mov-est-acbd.cod-tipo-def.  
    END.

    /* GRAVA OS DEFEITOS */
    FIND tt-defeitos WHERE
         tt-defeitos.cod-tipo-def = mov-est-acbd.cod-tipo-def AND
         tt-defeitos.cod-defeito  = mov-est-acbd.cod-defeito NO-ERROR.
    IF NOT AVAIL tt-defeitos THEN DO:
       CREATE tt-defeitos.
       ASSIGN tt-defeitos.cod-tipo-def = mov-est-acbd.cod-tipo-def
              tt-defeitos.cod-defeito  = mov-est-acbd.cod-defeito.
    END.

    /* GRAVA OS ITENS POR DEFEITOS */
    FIND tt-itens WHERE
         tt-itens.cod-tipo-def = mov-est-acbd.cod-tipo-def AND 
         tt-itens.cod-defeito  = mov-est-acbd.cod-defeito  AND 
         tt-itens.it-codigo    = mov-est-acbd.it-codigo    NO-ERROR.
    IF NOT AVAIL tt-itens THEN DO:
       CREATE tt-itens.
       ASSIGN tt-itens.cod-tipo-def = mov-est-acbd.cod-tipo-def  
              tt-itens.cod-defeito  = mov-est-acbd.cod-defeito   
              tt-itens.it-codigo    = mov-est-acbd.it-codigo.    
    END.

    /* GRAVA QUANTIDADES */
    CASE mov-est-acbd.classific:
        WHEN "RG" THEN
            ASSIGN tt-tipo-def.qtd-regular  = tt-tipo-def.qtd-regular  + mov-est-acbd.qtd-defeit
                   tt-defeitos.qtd-regular  = tt-defeitos.qtd-regular  + mov-est-acbd.qtd-defeit
                   tt-itens.qtd-regular     = tt-itens.qtd-regular     + mov-est-acbd.qtd-defeit.
        WHEN "LD" THEN
            ASSIGN tt-tipo-def.qtd-leve-def = tt-tipo-def.qtd-leve-def + mov-est-acbd.qtd-defeit
                   tt-defeitos.qtd-leve-def = tt-defeitos.qtd-leve-def + mov-est-acbd.qtd-defeit
                   tt-itens.qtd-leve-def    = tt-itens.qtd-leve-def    + mov-est-acbd.qtd-defeit.
        WHEN "RT" THEN
            ASSIGN tt-tipo-def.qtd-retalho  = tt-tipo-def.qtd-retalho  + mov-est-acbd.qtd-defeit
                   tt-defeitos.qtd-retalho  = tt-defeitos.qtd-retalho  + mov-est-acbd.qtd-defeit
                   tt-itens.qtd-retalho     = tt-itens.qtd-retalho     + mov-est-acbd.qtd-defeit.
    END CASE.

 END.

 /* Per¡odo ANTERIOR */
 FOR EACH mov-est-acbd WHERE
          mov-est-acbd.cod-estabel   >= c-cod-estabel-ini  AND
          mov-est-acbd.cod-estabel   <= c-cod-estabel-fin  AND
          mov-est-acbd.data-mov      >= da-dt-ant-ini      AND
          mov-est-acbd.data-mov      <= da-dt-ant-fin      AND  
          mov-est-acbd.cod-tipo-def  >= c-tipo-defeito-ini AND
          mov-est-acbd.cod-tipo-def  <= c-tipo-defeito-fin AND 
          mov-est-acbd.it-codigo     >= c-it-codigo-ini    AND
          mov-est-acbd.it-codigo     <= c-it-codigo-fin    AND
          mov-est-acbd.cod-refer     >= c-cod-refer-ini    AND
          mov-est-acbd.cod-refer     <= c-cod-refer-fin  
    NO-LOCK,
     EACH mov-est-acbm WHERE 
          mov-est-acbm.cod-estabel = mov-est-acbd.cod-estabel AND
          mov-est-acbm.data-mov    = mov-est-acbd.data-mov    AND
          mov-est-acbm.num-lote    = mov-est-acbd.num-lote    AND
          mov-est-acbm.it-codigo   = mov-est-acbd.it-codigo   AND
          mov-est-acbm.cod-refer   = mov-est-acbd.cod-refer
    NO-LOCK,     

    EACH item-ext WHERE
         item-ext.it-codigo = mov-est-acbd.it-codigo /* AND
         (item-ext.indigo   = YES AND c-tp-tecido = "I" OR 
          item-ext.indigo   = NO  AND c-tp-tecido = "O" OR
          c-tp-tecido = "A") */ NO-LOCK:  

    RUN pi-acompanhar in h-acomp (INPUT "Data: " +  STRING(mov-est-acbd.data-mov) + "  Item: " + mov-est-acbd.it-codigo).
    
    IF INDEX(c-tipo-tecelagem,mov-est-acbm.tipo-tear) = 0 THEN NEXT.

    IF l-excluir-ob THEN DO:
        FIND tt-excluir-ob WHERE
             tt-excluir-ob.num-lote = mov-est-acbd.num-lote NO-LOCK NO-ERROR.
        IF AVAIL tt-excluir-ob THEN NEXT.
    END.

    /* GRAVA TIPOS DE DEFEITOS */
    FIND tt-tipo-def WHERE 
         tt-tipo-def.cod-tipo-def = mov-est-acbd.cod-tipo-def NO-ERROR.
    IF NOT AVAIL tt-tipo-def THEN DO:
       CREATE tt-tipo-def.
       ASSIGN tt-tipo-def.cod-tipo-def = mov-est-acbd.cod-tipo-def.  
    END.

    /* GRAVA QUANTIDADES */
    CASE mov-est-acbd.classific:
        WHEN "RG" THEN
            ASSIGN tt-tipo-def.qtd-rg-ant = tt-tipo-def.qtd-rg-ant + mov-est-acbd.qtd-defeit.
        WHEN "LD" THEN
            ASSIGN tt-tipo-def.qtd-ld-ant = tt-tipo-def.qtd-ld-ant + mov-est-acbd.qtd-defeit.
        WHEN "RT" THEN
            ASSIGN tt-tipo-def.qtd-rt-ant = tt-tipo-def.qtd-rt-ant + mov-est-acbd.qtd-defeit.
    END CASE.

 END.

 {utp/ut-liter.i Calculando_a_Produ‡Æo_Nuance *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 FIND FIRST tt-tipo-def NO-LOCK NO-ERROR.
 IF AVAIL tt-tipo-def THEN DO:
    FOR EACH mov-est-acbm WHERE
             mov-est-acbm.cod-estabel >= c-cod-estabel-ini AND
             mov-est-acbm.cod-estabel <= c-cod-estabel-fin AND
             mov-est-acbm.data-mov    >= da-data-mov-ini   AND 
             mov-est-acbm.data-mov    <= da-data-mov-fin   AND
             mov-est-acbm.it-codigo   >= c-it-codigo-ini   AND
             mov-est-acbm.it-codigo   <= c-it-codigo-fin   AND
             mov-est-acbm.cod-refer   >= c-cod-refer-ini   AND
             mov-est-acbm.cod-refer   <= c-cod-refer-fin    
        NO-LOCK,
        EACH item-ext WHERE 
             item-ext.it-codigo = mov-est-acbm.it-codigo /* AND 
             (item-ext.indigo   = YES AND c-tp-tecido = "I" OR
              item-ext.indigo   = NO  AND c-tp-tecido = "O" OR
              c-tp-tecido = "A") */ NO-LOCK:
   
        RUN pi-acompanhar in h-acomp (INPUT "Data: " +  STRING(mov-est-acbm.data-mov) + "  Item: " + mov-est-acbm.it-codigo).
   
        IF INDEX(c-tipo-tecelagem,mov-est-acbm.tipo-tear) = 0 THEN NEXT.

        FOR EACH tt-itens WHERE
                 tt-itens.it-codigo = mov-est-acbm.it-codigo SHARE-LOCK.
            ASSIGN tt-itens.qtd-prod-per = tt-itens.qtd-prod-per + mov-est-acbm.qtd-tot-perf
                   tt-itens.qtd-prod-def = tt-itens.qtd-prod-def + mov-est-acbm.qtd-tot-def
                   tt-itens.qtd-prod-sob = tt-itens.qtd-prod-sob + mov-est-acbm.qtd-tot-sob.

            DO i-cont = 1 TO 10:
               IF mov-est-acbm.nuance-cla[i-cont] <> "" THEN DO:
                  /* GRAVA AS NUANCE POR ITENS */
                  FIND tt-nuance WHERE
                       tt-nuance.cod-tipo-def = tt-itens.cod-tipo-def AND 
                       tt-nuance.cod-defeito  = tt-itens.cod-defeito  AND 
                       tt-nuance.it-codigo    = tt-itens.it-codigo    AND
                       tt-nuance.nuance       = mov-est-acbm.nuance-cla[i-cont] NO-ERROR.
                  IF NOT AVAIL tt-nuance THEN DO:
                     CREATE tt-nuance.
                     ASSIGN tt-nuance.cod-tipo-def = tt-itens.cod-tipo-def  
                            tt-nuance.cod-defeito  = tt-itens.cod-defeito   
                            tt-nuance.it-codigo    = tt-itens.it-codigo
                            tt-nuance.nuance       = mov-est-acbm.nuance-cla[i-cont].    
                  END.
                  ASSIGN tt-nuance.qtd-nuance = tt-nuance.qtd-nuance +  mov-est-acbm.nuance-qtd[i-cont].
               END.
            END.

            /* Qualidade Tecido */
            FOR EACH ob-etiqueta WHERE
                     ob-etiqueta.cod-estabel = mov-est-acbm.cod-estabel AND
                     ob-etiqueta.nr-ob       = mov-est-acbm.num-lote    AND 
                     ob-etiqueta.tipo-ordem  = 1                        AND
                     ob-etiqueta.dt-emissao  = mov-est-acbm.data-mov    AND 
                     ob-etiqueta.it-codigo   = mov-est-acbm.it-codigo NO-LOCK.

                FIND tt-qualid WHERE
                     tt-qualid.cod-tipo-def = tt-itens.cod-tipo-def AND 
                     tt-qualid.cod-defeito  = tt-itens.cod-defeito  AND 
                     tt-qualid.it-codigo    = tt-itens.it-codigo AND
                     tt-qualid.cod-qualid   = ob-etiqueta.cod-qualid NO-ERROR.
                IF NOT AVAIL tt-qualid THEN DO:
                   CREATE tt-qualid.
                   ASSIGN tt-qualid.cod-tipo-def = tt-itens.cod-tipo-def  
                          tt-qualid.cod-defeito  = tt-itens.cod-defeito   
                          tt-qualid.it-codigo    = tt-itens.it-codigo
                          tt-qualid.cod-qualid   = ob-etiqueta.cod-qualid.     
                END.
                ASSIGN tt-qualid.qtd-qualid = tt-qualid.qtd-qualid + ob-etiqueta.quantidade.
            END. 
        END.
        ASSIGN fi-tot-prod-per = fi-tot-prod-per + mov-est-acbm.qtd-tot-perf
               fi-tot-prod-def = fi-tot-prod-def + mov-est-acbm.qtd-tot-def
               fi-tot-prod-sob = fi-tot-prod-sob + mov-est-acbm.qtd-tot-sob
               fi-tot-prod-ger = fi-tot-prod-ger + mov-est-acbm.qtd-tot-sob + 
                                                   mov-est-acbm.qtd-tot-def +
                                                   mov-est-acbm.qtd-tot-per.
    END.
 END.
 ASSIGN fi-perc-def = fi-tot-prod-def / fi-tot-prod-ger * 100
        fi-perc-per = fi-tot-prod-per / fi-tot-prod-ger * 100
        fi-perc-sob = fi-tot-prod-sob / fi-tot-prod-ger * 100.

 DISPLAY fi-tot-prod-per
         fi-perc-per
         fi-tot-prod-def
         fi-perc-def
         fi-tot-prod-sob
         fi-perc-sob
         fi-tot-prod-ger
         fi-ger-tp-def-rg
         fi-ger-tp-def-ld
         fi-ger-tp-def-rt
         fi-ger-tp-def-ger
         fi-def-rg-ant
         fi-def-ld-ant
         fi-def-rt-ant
         fi-tot-ant
         fi-ger-rg-ant
         fi-ger-ld-ant
         fi-ger-rt-ant
         fi-ger-ant
         WITH FRAME {&FRAME-NAME}. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-total C-Win 
PROCEDURE pi-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 ASSIGN de-tot-qualid= 0.
 FOR EACH tt-qualid WHERE                                     
          tt-qualid.cod-tipo-def = tt-itens.cod-tipo-def AND  
          tt-qualid.cod-defeito  = tt-itens.cod-defeito  AND  
          tt-qualid.it-codigo    = tt-itens.it-codigo    AND  
          tt-qualid.qtd-qualid   > 0 NO-LOCK.                         
      ASSIGN de-tot-qualid = de-tot-qualid + tt-qualid.qtd-qualid.          
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-defeito C-Win 
FUNCTION fn-desc-defeito RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND defeito WHERE
       defeito.cod-tipo-def = tt-defeitos.cod-tipo-def AND
       defeito.cod-defeito  = tt-defeitos.cod-defeito NO-LOCK NO-ERROR.

  RETURN TRIM(defeito.descricao).  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item C-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND ITEM WHERE
       ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

  RETURN item.desc-item.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-qualid C-Win 
FUNCTION fn-desc-qualid RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND qualid-tecido WHERE
       qualid-tecido.codigo = tt-qualid.cod-qualid NO-LOCK NO-ERROR.

  RETURN qualid-tecido.descricao.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-tipo-def C-Win 
FUNCTION fn-desc-tipo-def RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND tipo-def WHERE
       tipo-def.cod-tipo-def = tt-tipo-def.cod-tipo-def NO-LOCK NO-ERROR.

  RETURN TRIM(tipo-def.descricao).  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

