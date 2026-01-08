&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* Tabela para calculo por corte comercial */
DEF TEMP-TABLE tt-work
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD cod-refer    LIKE ped-item.cod-refer
    FIELD corte-comerc LIKE ob-etiqueta.corte-comerc
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade
    FIELD qt-interno   LIKE ped-item.qt-pedida
    FIELD qt-externo   LIKE ped-item.qt-pedida
    FIELD qt-p-interno LIKE ped-item.qt-pedida
    FIELD qt-p-externo LIKE ped-item.qt-pedida
    FIELD qt-negativo  LIKE ped-item.qt-pedida
    FIELD qt-trf       LIKE ped-item.qt-pedida
    INDEX indice2 IS PRIMARY it-codigo cod-refer.

DEF TEMP-TABLE tt-itens 
    FIELD it-codigo    LIKE ped-item.it-codigo
    INDEX indice1 IS PRIMARY it-codigo.

/* Tabela de itens e referencias acumuladas */
DEF TEMP-TABLE tt-refer
    FIELD it-codigo    LIKE ped-item.it-codigo
    FIELD cod-refer    LIKE ped-item.cod-refer
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade
    FIELD qt-interno   LIKE ped-item.qt-pedida
    FIELD qt-externo   LIKE ped-item.qt-pedida
    FIELD qt-p-interno LIKE ped-item.qt-pedida
    FIELD qt-p-externo LIKE ped-item.qt-pedida
    FIELD qt-margem    LIKE ped-item.qt-pedida
    FIELD qt-negativo  LIKE ped-item.qt-pedida
    FIELD qt-new-prog  LIKE ped-item.qt-pedida
    FIELD qt-trf       LIKE ped-item.qt-pedida
    FIELD qt-sld-prog  LIKE ped-item.qt-pedida
    FIELD dt-ult-prog  LIKE ob-pcp-ref.dt-ult-prog
    FIELD qt-proc      LIKE ped-item.qt-pedida
    FIELD dt-ult-proc  LIKE ob-pcp-ref.dt-ult-proc
    FIELD qt-pron      LIKE ped-item.qt-pedida
    FIELD dt-ult-pron  LIKE ob-pcp-ref.dt-ult-pron
    FIELD marca        AS   CHAR FORMAT "x(2)"
    INDEX indice2 IS PRIMARY it-codigo cod-refer.

DEF TEMP-TABLE tt-estoque
    FIELD it-codigo      LIKE ob-etiqueta.it-codigo
    FIELD cod-refer      LIKE ob-etiqueta.cod-refer
    FIELD corte-comerc   LIKE ob-etiqueta.corte-comerc LABEL "Corte Comercial"
    FIELD qt-estoque     LIKE ob-etiqueta.quantidade   LABEL "Estoque"
    FIELD qt-res-antc    LIKE ob-etiqueta.quantidade   LABEL "Res.Antec."
    FIELD qt-ped-reserva LIKE ob-etiqueta.quantidade   LABEL "Ped.Reserva"
    FIELD qt-trf         AS   DEC FORMAT "->>>,>>9.99" LABEL "Transform"
    FIELD qt-benefic     LIKE ob-etiqueta.quantidade   LABEL "Benefic"
    FIELD qt-carteira    LIKE ped-item.qt-pedida       LABEL "Carteira"
    FIELD qt-res-cart    LIKE ob-etiqueta.quantidade   LABEL "Cart.Forn."
    FIELD qt-saldo       LIKE saldo-estoq.qtidade-atu  LABEL "SALDO"
    FIELD visualiza      AS   LOG INIT NO
    INDEX indice1 IS PRIMARY it-codigo cod-refer corte-comerc.

DEF TEMP-TABLE tt-pedidos
    FIELD row-temp-table  AS ROWID
    FIELD nr-pedcli       LIKE ped-item.nr-pedcli
    FIELD nome-abrev      LIKE ped-item.nome-abrev
    FIELD qt-pedida       LIKE ped-item.qt-pedida
    FIELD dt-entrega      LIKE ped-venda.dt-entrega
    FIELD mercado         AS   LOG FORMAT "Interno/Externo"
    FIELD visualiza       AS   LOG.

DEF TEMP-TABLE tt-pcp LIKE ob-pcp-ref
    FIELD it-codigo   LIKE ob-pcp.it-codigo
    FIELD modifica    AS   LOG  
    FIELD elimina     AS   LOG  FORMAT "Sim/N∆o" 
    FIELD visualiza   AS   LOG.

DEF TEMP-TABLE tt-digita
    FIELD opcao AS CHAR
    FIELD campo AS CHAR
    FIELD valor AS CHAR.

DEFINE TEMP-TABLE tt-fotos NO-UNDO
       FIELD arq-image AS CHAR.

DEF NEW GLOBAL SHARED VAR gr-item AS ROWID NO-UNDO.

DEF NEW SHARED VAR p-cod-estabel-182 AS CHAR.
DEF NEW SHARED VAR p-it-codigo-182 AS CHAR.
DEF NEW SHARED VAR p-cod-refer-182 AS CHAR.
DEF NEW SHARED VAR p-manut-182     AS LOG.
DEF NEW SHARED VAR p-it-codigo-150 AS CHAR.
DEF NEW SHARED VAR p-cod-refer-150 AS CHAR.
DEF NEW SHARED VAR p-lote-rp-150   AS LOG.
DEF NEW SHARED VAR p-progr-ini-166 LIKE ob-pcp.num-progr.
DEF NEW SHARED VAR p-progr-fin-166 LIKE ob-pcp.num-progr.

DEF VAR de-qt-margem   AS   DEC.
DEF VAR h-acomp        AS   HANDLE NO-UNDO.
DEF VAR h-essp0150     AS   HANDLE.
DEF VAR h-query        AS   HANDLE.
DEF VAR c-cod-refer    AS   CHAR.
DEF VAR de-tot-res     LIKE ped-item.qt-pedida.
DEF VAR c-desc-item    LIKE ITEM.desc-item.
DEF VAR c-obsoleto     AS   CHAR.
DEF VAR c-lotes        AS   CHAR.
DEF VAR c-dia          AS   CHAR.
DEF VAR da-dt-entrega  AS   DATE.
DEF VAR i-pag          AS   INT.
DEF VAR c-mensagem     AS   CHAR.
DEF VAR c-progr        AS   CHAR.
DEF VAR i-new-progr    LIKE ob-pcp.num-progr.
DEF VAR l-confirma     AS   LOG.
DEF VAR de-qt-ppp      AS   DEC.
DEF VAR c-ped-ex       AS   CHAR FORMAT "x(100)".
DEF VAR de-qt-ex       AS   DEC.
DEF VAR c-ped-en       AS   CHAR FORMAT "x(100)".
DEF VAR de-qt-en       AS   DEC.

DEF VAR c-cod-estabel      AS CHAR.
DEF VAR c-dt-limite        AS CHAR.      
DEF VAR c-it-codigo-ini    AS CHAR.      
DEF VAR c-it-codigo-fin    AS CHAR.      
DEF VAR c-cod-refer-ini    AS CHAR.      
DEF VAR c-cod-refer-fin    AS CHAR.      
DEF VAR c-corte-comerc-ini LIKE ob-etiqueta.corte-comerc.
DEF VAR c-corte-comerc-fin LIKE ob-etiqueta.corte-comerc.
DEF VAR c-cod-obsoleto-ini AS CHAR.      
DEF VAR c-cod-obsoleto-fin AS CHAR.      
DEF VAR l-lote-rp          AS LOG.
DEF VAR l-lote-ca          AS LOG.
DEF VAR c-tp-artigo        AS CHAR.
DEF VAR i-credito          AS INT.
DEF VAR i-tp-acab          AS INT.
DEF VAR l-apenas-neg       AS LOG.
DEF VAR l-apenas-estoq     AS LOG.
DEF VAR l-apenas-ppp       AS LOG.
DEF VAR de-min-prod        AS DEC.

DEF VAR c-arq-image AS CHAR.
DEF VAR c-comando AS CHAR.
DEF VAR c-arq-texto AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-estoque

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-estoque corte-comerc tt-itens tt-refer

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-estoque                                    */
&Scoped-define FIELDS-IN-QUERY-br-estoque corte-comerc.descricao tt-estoque.qt-estoque tt-estoque.qt-res-antc tt-estoque.qt-ped-reserva tt-estoque.qt-trf fn-benefic() @ tt-estoque.qt-benefic fn-carteira() @ tt-estoque.qt-carteira tt-estoque.qt-res-cart tt-estoque.qt-saldo   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-estoque   
&Scoped-define SELF-NAME br-estoque
&Scoped-define OPEN-QUERY-br-estoque RUN pi-total-refer. OPEN QUERY {&SELF-NAME} FOR EACH tt-estoque WHERE                                  tt-estoque.it-codigo = tt-refer.it-codigo AND                                  tt-estoque.cod-refer = tt-refer.cod-refer NO-LOCK, ~
                                  FIRST corte-comerc WHERE                                  corte-comerc.codigo = tt-estoque.corte-comerc NO-LOCK                                  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-estoque tt-estoque corte-comerc
&Scoped-define FIRST-TABLE-IN-QUERY-br-estoque tt-estoque
&Scoped-define SECOND-TABLE-IN-QUERY-br-estoque corte-comerc


/* Definitions for BROWSE br-itens                                      */
&Scoped-define FIELDS-IN-QUERY-br-itens tt-itens.it-codigo fn-desc-item() @ c-desc-item   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-itens   
&Scoped-define SELF-NAME br-itens
&Scoped-define OPEN-QUERY-br-itens RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-itens tt-itens
&Scoped-define FIRST-TABLE-IN-QUERY-br-itens tt-itens


/* Definitions for BROWSE br-refer                                      */
&Scoped-define FIELDS-IN-QUERY-br-refer tt-refer.cod-refer fn-obsoleto() @ c-obsoleto tt-refer.qt-interno tt-refer.qt-externo tt-refer.qt-negativo tt-refer.qt-new-prog tt-refer.marca   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-refer   
&Scoped-define SELF-NAME br-refer
&Scoped-define QUERY-STRING-br-refer FOR EACH tt-refer WHERE                                  tt-refer.it-codigo = tt-itens.it-codigo NO-LOCK
&Scoped-define OPEN-QUERY-br-refer OPEN QUERY {&SELF-NAME} FOR EACH tt-refer WHERE                                  tt-refer.it-codigo = tt-itens.it-codigo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-refer tt-refer
&Scoped-define FIRST-TABLE-IN-QUERY-br-refer tt-refer


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-estoque}~
    ~{&OPEN-QUERY-br-refer}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-itens RECT-10 RECT-6 RECT-7 RECT-9 ~
br-refer bt-marca bt-desmarca bt-todos bt-nenhum bt-pedidos bt-desenho ~
br-estoque bt-vapra bt-det-progr bt-margem bt-imprime bt-anl-gerencial ~
bt-ok fi-qt-new-prog 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-estoque fi-tot-res-antc ~
fi-tot-reserva fi-tot-trf-est fi-tot-benefic fi-tot-carteira ~
fi-tot-res-cart fi-tot-saldo-est fi-sld-prog fi-dt-prog fi-saldo ~
fi-tot-interno-it fi-tot-interno fi-proc fi-dt-proc fi-margem ~
fi-tot-externo-it fi-tot-externo fi-pronto fi-dt-pronto fi-qt-prog-s ~
fi-tot-saldo-it fi-tot-saldo fi-tot-prog-it fi-tot-prog fi-p-interno ~
fi-negativo-p fi-tot-trf-it fi-tot-trf fi-p-externo fi-qt-prog-p ~
fi-tot-negativo-it fi-tot-negativo fi-qt-new-prog fi-tot-aprog-it ~
fi-tot-aprog 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-ord-produ||y|mgmov.ped-item.nr-ord-produ
nr-programa||y|mgmov.ped-item.nr-programa
it-codigo||y|mgmov.ped-item.it-codigo
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nr-ord-produ,nr-programa,it-codigo"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-benefic B-table-Win 
FUNCTION fn-benefic RETURNS DECIMAL
( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-carteira B-table-Win 
FUNCTION fn-carteira RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-desc-item B-table-Win 
FUNCTION fn-desc-item RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-obsoleto B-table-Win 
FUNCTION fn-obsoleto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-anl-gerencial 
     IMAGE-UP FILE "image/im-150.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Consulta Gerencial do Estoque"
     BGCOLOR 8 .

DEFINE BUTTON bt-can-progr 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "bt ped mi p 2" 
     SIZE 4 BY 1 TOOLTIP "Modifica e Cancela Programaá‰es".

DEFINE BUTTON bt-desenho 
     IMAGE-UP FILE "image/im-show.bmp":U
     LABEL "" 
     SIZE 4 BY 1.83 TOOLTIP "Visualiza Imagem do Desenho"
     BGCOLOR 8 .

DEFINE BUTTON bt-desmarca AUTO-GO 
     IMAGE-UP FILE "image/im-cllps.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Desmarca"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-det-proc 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "bt ped mi p 2" 
     SIZE 4 BY 1 TOOLTIP "Pedidos Mercado Externo".

DEFINE BUTTON bt-det-progr 
     IMAGE-UP FILE "image/im-bt-grupo-maq.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Manutená∆o e Consulta PCP"
     BGCOLOR 8 .

DEFINE BUTTON bt-det-pron 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Pedidos Mercado Interno".

DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-email.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Impress∆o e Mensagens da Programaá∆o"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-marca AUTO-GO 
     IMAGE-UP FILE "image/im-expan.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Marca"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-margem 
     IMAGE-UP FILE "image/im-ajust.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Ajusta % Margem do Item"
     BGCOLOR 8 .

DEFINE BUTTON bt-nenhum AUTO-GO 
     IMAGE-UP FILE "image/im-nenhum.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Desmarca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "OK" 
     SIZE 4 BY 1.25 TOOLTIP "Confirma Programaá∆o".

DEFINE BUTTON bt-ped-me-p 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "" 
     SIZE 4 BY 1 TOOLTIP "Pedidos Mercado Interno".

DEFINE BUTTON bt-ped-mi-p 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Pedidos Mercado Externo".

DEFINE BUTTON bt-pedidos 
     IMAGE-UP FILE "image/im-local.bmp":U
     LABEL "" 
     SIZE 4 BY 1.83 TOOLTIP "Detalha Pedidos"
     BGCOLOR 8 .

DEFINE BUTTON bt-todos AUTO-GO 
     IMAGE-UP FILE "image/im-ran_a.bmp":U
     LABEL "" 
     SIZE 4 BY 1.29 TOOLTIP "Marca TODOS"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 4 BY 1.71 TOOLTIP "V† para Referància"
     BGCOLOR 8 .

DEFINE VARIABLE fi-dt-proc AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-prog AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-pronto AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-margem AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "% Margem" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-negativo-p AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Negativo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-p-externo AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Merc. Externo" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-p-interno AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Merc. Interno" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-proc AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Processo" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 16 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-pronto AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pronto" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 2 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-qt-new-prog AS DECIMAL FORMAT ">>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.25
     FONT 11 NO-UNDO.

DEFINE VARIABLE fi-qt-prog-p AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Total Ö Programar" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-prog-s AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Qtde Ö Progr." 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-saldo AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Saldo" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 9 FONT 1 NO-UNDO.

DEFINE VARIABLE fi-sld-prog AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Programado" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .88
     FGCOLOR 12 FONT 0 NO-UNDO.

DEFINE VARIABLE fi-tot-aprog AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-aprog-it AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-benefic AS DECIMAL FORMAT "ZZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-carteira AS DECIMAL FORMAT "ZZ,ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-estoque AS DECIMAL FORMAT ">>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.29 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-externo AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-externo-it AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-interno AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-interno-it AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":R INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-negativo AS DECIMAL FORMAT "-zzzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 12 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-negativo-it AS DECIMAL FORMAT "-zzzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FGCOLOR 12 FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-prog AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-prog-it AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-res-antc AS DECIMAL FORMAT ">>>,>>9.99":R18 INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-res-cart AS DECIMAL FORMAT "ZZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-reserva AS DECIMAL FORMAT "Z,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-saldo AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-saldo-est AS DECIMAL FORMAT "ZZZ,ZZZ,ZZZ,ZZ9.99-":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.72 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-saldo-it AS DECIMAL FORMAT "z,zzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-trf AS DECIMAL FORMAT "-zzzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE VARIABLE fi-tot-trf-est AS DECIMAL FORMAT "-ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-tot-trf-it AS DECIMAL FORMAT "-zzzz,zz9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .88
     FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 40 BY 8
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 8.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 6 BY 22.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 2.75.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-estoque FOR 
      tt-estoque, 
      corte-comerc SCROLLING.

DEFINE QUERY br-itens FOR 
      tt-itens SCROLLING.

DEFINE QUERY br-refer FOR 
      tt-refer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-estoque B-table-Win _FREEFORM
  QUERY br-estoque DISPLAY
      corte-comerc.descricao COLUMN-LABEL "Corte Comercial" FORMAT "x(30)" 
      tt-estoque.qt-estoque 
      tt-estoque.qt-res-antc
      tt-estoque.qt-ped-reserva 
      tt-estoque.qt-trf     
      fn-benefic() @ tt-estoque.qt-benefic  COLUMN-LABEL "Benefic" WIDTH 10
      fn-carteira() @ tt-estoque.qt-carteira COLUMN-LABEL "Carteira" 
      tt-estoque.qt-res-cart 
      tt-estoque.qt-saldo WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 101.72 BY 4.29
         FONT 1
         TITLE "Estoque" ROW-HEIGHT-CHARS .48.

DEFINE BROWSE br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-itens B-table-Win _FREEFORM
  QUERY br-itens NO-LOCK DISPLAY
      tt-itens.it-codigo FORMAT "x(8)":U                    WIDTH 6
      fn-desc-item() @ c-desc-item COLUMN-LABEL "Descriá∆o" WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 40 BY 8.75
         FONT 1
         TITLE "Itens Processados" ROW-HEIGHT-CHARS .63.

DEFINE BROWSE br-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-refer B-table-Win _FREEFORM
  QUERY br-refer DISPLAY
      tt-refer.cod-refer         FORMAT "x(8)"        COLUMN-LABEL "Referància"      WIDTH 7.5
      fn-obsoleto() @ c-obsoleto FORMAT "x(18)":U     COLUMN-LABEL "Cod. Obsoleto"   WIDTH 13.5
      tt-refer.qt-interno        FORMAT ">>>,>>9.99"  COLUMN-LABEL "Merc. Int."      WIDTH 8
      tt-refer.qt-externo        FORMAT ">>>,>>9.99"  COLUMN-LABEL "Merc. Ext."      WIDTH 8
      tt-refer.qt-negativo       FORMAT "->>>,>>9.99" COLUMN-LABEL "Negativo"        WIDTH 8
      tt-refer.qt-new-prog       FORMAT "->>>,>>9.99" COLUMN-LABEL "∑ Progr."        WIDTH 8
      tt-refer.marca             COLUMN-LABEL "M" COLUMN-FGCOLOR 12 COLUMN-FONT 0    WIDTH 2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 62 BY 8.75
         FONT 1
         TITLE "Referàncias do Item" ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-itens AT ROW 1 COL 1.14
     br-refer AT ROW 1 COL 41.43
     bt-marca AT ROW 1.25 COL 105.14
     bt-desmarca AT ROW 2.5 COL 105.14
     bt-todos AT ROW 3.75 COL 105.14
     bt-nenhum AT ROW 5.04 COL 105.14
     bt-pedidos AT ROW 7.21 COL 105.14
     bt-desenho AT ROW 9.08 COL 105.14
     br-estoque AT ROW 9.83 COL 1.29 WIDGET-ID 100
     bt-vapra AT ROW 10.96 COL 105.14
     fi-tot-estoque AT ROW 14.25 COL 30.72 RIGHT-ALIGNED NO-LABEL WIDGET-ID 6
     fi-tot-res-antc AT ROW 14.25 COL 38.86 RIGHT-ALIGNED NO-LABEL WIDGET-ID 8
     fi-tot-reserva AT ROW 14.25 COL 38.14 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fi-tot-trf-est AT ROW 14.25 COL 47.57 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fi-tot-benefic AT ROW 14.25 COL 56.43 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fi-tot-carteira AT ROW 14.25 COL 66.86 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi-tot-res-cart AT ROW 14.25 COL 77.57 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fi-tot-saldo-est AT ROW 14.25 COL 85.86 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     bt-det-progr AT ROW 15.42 COL 105.14
     bt-can-progr AT ROW 15.54 COL 74.43
     fi-sld-prog AT ROW 15.58 COL 49 COLON-ALIGNED
     fi-dt-prog AT ROW 15.58 COL 62 COLON-ALIGNED NO-LABEL
     fi-saldo AT ROW 15.58 COL 87.57 COLON-ALIGNED
     fi-tot-interno-it AT ROW 16.21 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-interno AT ROW 16.21 COL 24.72 COLON-ALIGNED NO-LABEL
     bt-det-proc AT ROW 16.54 COL 74.57
     bt-margem AT ROW 16.54 COL 96
     fi-proc AT ROW 16.58 COL 49 COLON-ALIGNED
     fi-dt-proc AT ROW 16.58 COL 62 COLON-ALIGNED NO-LABEL
     fi-margem AT ROW 16.58 COL 87.57 COLON-ALIGNED
     fi-tot-externo-it AT ROW 17.21 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-externo AT ROW 17.21 COL 24.72 COLON-ALIGNED NO-LABEL
     bt-det-pron AT ROW 17.5 COL 74.57
     fi-pronto AT ROW 17.58 COL 49 COLON-ALIGNED
     fi-dt-pronto AT ROW 17.58 COL 62 COLON-ALIGNED NO-LABEL
     fi-qt-prog-s AT ROW 17.58 COL 87.57 COLON-ALIGNED
     fi-tot-saldo-it AT ROW 18.21 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-saldo AT ROW 18.21 COL 24.72 COLON-ALIGNED NO-LABEL
     bt-imprime AT ROW 18.33 COL 105.14
     fi-tot-prog-it AT ROW 19.21 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-prog AT ROW 19.21 COL 24.72 COLON-ALIGNED NO-LABEL
     bt-ped-mi-p AT ROW 19.29 COL 67.14
     fi-p-interno AT ROW 19.33 COL 52.72 COLON-ALIGNED
     fi-negativo-p AT ROW 19.33 COL 86 COLON-ALIGNED
     bt-anl-gerencial AT ROW 19.67 COL 105.14
     fi-tot-trf-it AT ROW 20.21 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-trf AT ROW 20.21 COL 24.72 COLON-ALIGNED NO-LABEL
     bt-ped-me-p AT ROW 20.29 COL 67.14
     fi-p-externo AT ROW 20.33 COL 52.72 COLON-ALIGNED
     fi-qt-prog-p AT ROW 20.33 COL 86 COLON-ALIGNED
     fi-tot-negativo-it AT ROW 21.21 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-negativo AT ROW 21.21 COL 24.72 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 21.79 COL 105.14
     fi-qt-new-prog AT ROW 21.83 COL 78 COLON-ALIGNED NO-LABEL
     fi-tot-aprog-it AT ROW 22.17 COL 11 COLON-ALIGNED NO-LABEL
     fi-tot-aprog AT ROW 22.17 COL 24.72 COLON-ALIGNED NO-LABEL
     "Transform.:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 20.33 COL 11.43 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "∑ Programar:" VIEW-AS TEXT
          SIZE 10.29 BY .54 AT ROW 22.29 COL 2.14
          BGCOLOR 8 FGCOLOR 9 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Estoque:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 18.33 COL 11.57 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "GERAL" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 15.58 COL 26.72
          BGCOLOR 8 FONT 6
     "Negativo:" VIEW-AS TEXT
          SIZE 8.14 BY .54 AT ROW 21.29 COL 11.86 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     " Meses Posteriores" VIEW-AS TEXT
          SIZE 16 BY .67 AT ROW 18.54 COL 43.43
          FONT 6
     "PPP:" VIEW-AS TEXT
          SIZE 4.43 BY .54 AT ROW 19.33 COL 12 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Merc.Interno:" VIEW-AS TEXT
          SIZE 10.72 BY .54 AT ROW 16.33 COL 11.58 RIGHT-ALIGNED
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "Quantidade Ö Programar:" VIEW-AS TEXT
          SIZE 34 BY 1.25 AT ROW 21.83 COL 45.43
          FONT 11
     "Merc Externo:" VIEW-AS TEXT
          SIZE 11.43 BY .54 AT ROW 17.38 COL 1.57
          BGCOLOR 8 FGCOLOR 9 FONT 6
     "ITEM" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 15.58 COL 13
          BGCOLOR 8 FONT 6
     "Totais do Estoque:" VIEW-AS TEXT
          SIZE 20.29 BY .54 AT ROW 14.42 COL 2.72 WIDGET-ID 18
          FONT 0
     " Totais" VIEW-AS TEXT
          SIZE 9 BY .63 AT ROW 15.13 COL 2.57
          BGCOLOR 8 FGCOLOR 12 FONT 0
     RECT-10 AT ROW 15.29 COL 1
     RECT-6 AT ROW 15.29 COL 41
     RECT-7 AT ROW 1 COL 104
     RECT-9 AT ROW 18.96 COL 42.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 22.38
         WIDTH              = 109.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-itens TEXT-2 F-Main */
/* BROWSE-TAB br-refer RECT-9 F-Main */
/* BROWSE-TAB br-estoque bt-desenho F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-can-progr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-det-proc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-det-pron IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ped-me-p IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-ped-mi-p IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-proc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-prog IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-pronto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-margem IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-negativo-p IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-p-externo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-p-interno IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-proc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-pronto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-prog-p IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-prog-s IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-saldo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-sld-prog IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-aprog IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-aprog-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-benefic IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-carteira IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-estoque IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-tot-externo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-externo-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-interno IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-interno-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-negativo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-negativo-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-prog IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-prog-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-res-antc IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN fi-tot-res-cart IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-reserva IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-saldo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-saldo-est IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-saldo-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-trf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-trf-est IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-trf-it IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "Merc.Interno:"
          SIZE 10.72 BY .54 AT ROW 16.33 COL 11.58 RIGHT-ALIGNED        */

/* SETTINGS FOR TEXT-LITERAL "Estoque:"
          SIZE 7 BY .54 AT ROW 18.33 COL 11.57 RIGHT-ALIGNED            */

/* SETTINGS FOR TEXT-LITERAL "PPP:"
          SIZE 4.43 BY .54 AT ROW 19.33 COL 12 RIGHT-ALIGNED            */

/* SETTINGS FOR TEXT-LITERAL "Transform.:"
          SIZE 9 BY .54 AT ROW 20.33 COL 11.43 RIGHT-ALIGNED            */

/* SETTINGS FOR TEXT-LITERAL "Negativo:"
          SIZE 8.14 BY .54 AT ROW 21.29 COL 11.86 RIGHT-ALIGNED         */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-estoque
/* Query rebuild information for BROWSE br-estoque
     _START_FREEFORM
RUN pi-total-refer.
OPEN QUERY {&SELF-NAME} FOR EACH tt-estoque WHERE
                                 tt-estoque.it-codigo = tt-refer.it-codigo AND
                                 tt-estoque.cod-refer = tt-refer.cod-refer NO-LOCK,
                           FIRST corte-comerc WHERE
                                 corte-comerc.codigo = tt-estoque.corte-comerc NO-LOCK
                                 INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-estoque */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-itens
/* Query rebuild information for BROWSE br-itens
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens WHERE NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-itens */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-refer
/* Query rebuild information for BROWSE br-refer
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-refer WHERE
                                 tt-refer.it-codigo = tt-itens.it-codigo NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-refer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-estoque
&Scoped-define SELF-NAME br-estoque
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-estoque B-table-Win
ON ROW-DISPLAY OF br-estoque IN FRAME F-Main /* Estoque */
DO:
  ASSIGN tt-estoque.qt-saldo:FGCOLOR IN BROWSE br-estoque = ?.
  IF tt-estoque.qt-saldo < 0 THEN
      ASSIGN tt-estoque.qt-saldo:FGCOLOR IN BROWSE br-estoque = 12.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-itens
&Scoped-define SELF-NAME br-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-itens B-table-Win
ON VALUE-CHANGED OF br-itens IN FRAME F-Main /* Itens Processados */
DO:
    ASSIGN fi-tot-interno-it = 0     fi-tot-externo-it = 0
             fi-tot-saldo-it = 0        fi-tot-prog-it = 0
               fi-tot-trf-it = 0    fi-tot-negativo-it = 0
             fi-tot-aprog-it = 0.

    FOR EACH tt-refer WHERE
             tt-refer.it-codigo = tt-itens.it-codigo NO-LOCK.

        ASSIGN fi-tot-interno-it  = fi-tot-interno-it  + tt-refer.qt-interno
               fi-tot-externo-it  = fi-tot-externo-it  + tt-refer.qt-externo
               fi-tot-saldo-it    = fi-tot-saldo-it    + tt-refer.qt-estoque
               fi-tot-prog-it     = fi-tot-prog-it     + tt-refer.qt-sld-prog + tt-refer.qt-proc + tt-refer.qt-pron
               fi-tot-trf-it      = fi-tot-trf-it      + tt-refer.qt-trf
               fi-tot-negativo-it = fi-tot-negativo-it + ABS(tt-refer.qt-negativo)
               fi-tot-aprog-it    = fi-tot-aprog-it    + tt-refer.qt-new-prog.
    END.

    DISPLAY fi-tot-interno-it 
            fi-tot-externo-it 
            fi-tot-saldo-it
            fi-tot-prog-it
            fi-tot-trf-it
            fi-tot-negativo-it
            fi-tot-aprog-it    
            WITH FRAME {&FRAME-NAME}.

    {&OPEN-QUERY-br-refer}
    APPLY 'value-changed' TO br-refer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-refer
&Scoped-define SELF-NAME br-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-refer B-table-Win
ON VALUE-CHANGED OF br-refer IN FRAME F-Main /* Referàncias do Item */
DO:
   ASSIGN fi-sld-prog = 0        fi-proc = 0        fi-pronto = 0 
          fi-dt-prog = ?     fi-dt-proc = ?     fi-dt-pronto = ?
          fi-margem = 0      fi-saldo = 0       fi-qt-prog-s = 0
          fi-qt-new-prog = 0 fi-negativo-p = 0  fi-qt-prog-p = 0
          fi-p-interno = 0   fi-p-externo = 0.  

   IF AVAIL tt-refer THEN DO.
      ASSIGN c-arq-image = SESSION:TEMP-DIRECTORY + SUBSTR(tt-refer.cod-refer,3,4) + '.txt'.
             c-comando = 'DIR /b ' + param-dis.dir-img-item + '\*' + SUBSTR(tt-refer.cod-refer,3,4) + '* >' +
                         c-arq-image.
      OS-COMMAND SILENT VALUE(c-comando).
       
      EMPTY TEMP-TABLE tt-fotos.

      INPUT FROM VALUE(c-arq-image).
      REPEAT.
          CREATE tt-fotos.
          IMPORT tt-fotos.
      END.
      INPUT CLOSE.
        
      FOR EACH tt-fotos.
          IF tt-fotos.arq-image MATCHES '*' + SUBSTR(tt-refer.cod-refer,3,4) + '*' THEN DO.
             ASSIGN tt-fotos.arq-image = param-dis.dir-img-item + "\" + tt-fotos.arq-image.
             NEXT.
          END.
          ELSE
             DELETE tt-fotos.
      END.
        
      FIND FIRST tt-fotos NO-ERROR.
      ASSIGN bt-desenho:SENSITIVE = NO.
      IF AVAIL tt-fotos THEN
         ASSIGN bt-desenho:SENSITIVE = YES.
    
      FIND item-ext WHERE
           item-ext.it-codigo = tt-refer.it-codigo
           NO-LOCK NO-ERROR.
    
      RUN pi-recalc-ppp.

      ASSIGN fi-sld-prog = tt-refer.qt-sld-prog
             fi-dt-prog = IF fi-sld-prog > 0 
                          THEN tt-refer.dt-ult-prog ELSE fi-dt-prog
             fi-proc = tt-refer.qt-proc 
             fi-dt-proc = IF fi-proc > 0
                          THEN tt-refer.dt-ult-proc ELSE fi-dt-proc
             fi-pronto = tt-refer.qt-pron
             fi-dt-pronto = IF fi-pronto > 0
                            THEN tt-refer.dt-ult-pron ELSE fi-dt-pronto
             /*fi-margem = item-ext.margem-pcp*/
             fi-saldo = tt-refer.qt-negativo
             fi-qt-prog-s = ABS(fi-saldo + (fi-saldo * (fi-margem / 100)))
             fi-p-interno = tt-refer.qt-p-interno
             fi-p-externo = tt-refer.qt-p-externo
             fi-negativo-p = fi-p-interno + fi-p-externo + ((fi-p-interno + fi-p-externo) * (fi-margem / 100))
             fi-qt-prog-p = fi-qt-prog-s + fi-negativo-p.

      IF tt-refer.marca <> '' THEN
         ASSIGN fi-qt-new-prog = tt-refer.qt-new-prog.
      ELSE
         ASSIGN fi-qt-new-prog = IF fi-qt-prog-s >= 50
                                 THEN ROUND(fi-qt-prog-s / 50,0) * 50
                                 ELSE 50. 
   END.

   DISP fi-sld-prog    fi-proc      fi-pronto  
        fi-dt-prog     fi-dt-proc   fi-dt-pronto
        fi-margem      fi-saldo     fi-qt-prog-s
        fi-qt-new-prog fi-p-interno fi-p-externo
        fi-negativo-p  fi-qt-prog-p
        WITH FRAME {&FRAME-NAME}.

   ASSIGN bt-ped-mi-p:SENSITIVE = fi-p-interno > 0
          bt-ped-me-p:SENSITIVE = fi-p-externo > 0
          bt-can-progr:SENSITIVE = fi-sld-prog > 0
          bt-det-proc:SENSITIVE = fi-proc > 0 
          bt-det-pron:SENSITIVE = fi-pronto > 0 
          bt-margem:SENSITIVE = AVAIL tt-refer.

   RUN pi-estoque.

   {&OPEN-QUERY-br-estoque}
   APPLY 'value-changed' TO br-estoque IN FRAME {&FRAME-NAME}.

   APPLY 'entry' TO fi-qt-new-prog.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-anl-gerencial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-anl-gerencial B-table-Win
ON CHOOSE OF bt-anl-gerencial IN FRAME F-Main
DO:
   IF AVAIL tt-refer THEN DO.
      ASSIGN FRAME F-Main:WINDOW:SENSITIVE = NO.
      ASSIGN p-it-codigo-150 = tt-refer.it-codigo
             p-cod-refer-150 = tt-refer.cod-refer
             p-lote-rp-150 = YES.

      RUN esp/essp0150.w "SHARED".
      ASSIGN FRAME F-Main:WINDOW:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-can-progr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-can-progr B-table-Win
ON CHOOSE OF bt-can-progr IN FRAME F-Main /* bt ped mi p 2 */
DO:
    ASSIGN FRAME F-Main:WINDOW:SENSITIVE = NO.
    ASSIGN p-cod-estabel-182 = c-cod-estabel
           p-it-codigo-182 = tt-refer.it-codigo
           p-cod-refer-182 = tt-refer.cod-refer
           p-manut-182 = YES.

    RUN esp/essp0182.w "SHARED".
    ASSIGN FRAME F-Main:WINDOW:SENSITIVE = YES.
    
    APPLY 'VALUE-CHANGED' TO br-refer.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desenho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desenho B-table-Win
ON CHOOSE OF bt-desenho IN FRAME F-Main
DO:
   RUN esdlg/d01-desenho.w (INPUT tt-refer.cod-refer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-desmarca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desmarca B-table-Win
ON CHOOSE OF bt-desmarca IN FRAME F-Main
DO:
   ASSIGN tt-refer.marca = "".
   br-refer:REFRESH().
   APPLY 'value-changed' TO br-refer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-proc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-proc B-table-Win
ON CHOOSE OF bt-det-proc IN FRAME F-Main /* bt ped mi p 2 */
DO:
  FOR EACH tt-pcp.
      ASSIGN tt-pcp.visualiza = NO.
      IF tt-pcp.it-codigo = tt-refer.it-codigo AND
         tt-pcp.cod-refer = tt-refer.cod-refer AND
         tt-pcp.qtd-proc > 0 THEN
         ASSIGN tt-pcp.visualiza = YES.
  END.

  RUN esp/essp0170c.p (INPUT-OUTPUT TABLE tt-pcp).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-progr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-progr B-table-Win
ON CHOOSE OF bt-det-progr IN FRAME F-Main
DO:
    IF AVAIL tt-refer THEN DO.
       ASSIGN FRAME F-Main:WINDOW:SENSITIVE = NO.
       ASSIGN p-it-codigo-182 = tt-refer.it-codigo
              p-cod-refer-182 = tt-refer.cod-refer.

       RUN esp/essp0182.w "SHARED".
       ASSIGN FRAME F-Main:WINDOW:SENSITIVE = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det-pron
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det-pron B-table-Win
ON CHOOSE OF bt-det-pron IN FRAME F-Main
DO:
  FOR EACH tt-pcp.
      ASSIGN tt-pcp.visualiza = NO.
      IF tt-pcp.it-codigo = tt-refer.it-codigo AND
         tt-pcp.cod-refer = tt-refer.cod-refer AND
         tt-pcp.qtd-pron > 0 THEN
         ASSIGN tt-pcp.visualiza = YES.
  END.

  RUN esp/essp0170c.p (INPUT-OUTPUT TABLE tt-pcp).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime B-table-Win
ON CHOOSE OF bt-imprime IN FRAME F-Main
DO:
    IF c-progr <> '' THEN 
       ASSIGN p-progr-ini-166 = INT(ENTRY(1,c-progr,";"))
              p-progr-fin-166 = INT(ENTRY(NUM-ENTRIES(c-progr,";"),c-progr,";")).
    
    RUN esp/essp0166.w "SHARED".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-marca B-table-Win
ON CHOOSE OF bt-marca IN FRAME F-Main
DO:
   ASSIGN tt-refer.marca = "*".
          tt-refer.qt-new-prog = fi-qt-new-prog:INPUT-VALUE.

   br-refer:REFRESH().
   APPLY 'value-changed' TO br-refer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-margem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-margem B-table-Win
ON CHOOSE OF bt-margem IN FRAME F-Main
DO:
   FIND item WHERE
        item.it-codigo = tt-refer.it-codigo NO-LOCK.

   IF AVAIL item THEN DO.
      ASSIGN FRAME F-Main:WINDOW:SENSITIVE = NO.
      ASSIGN gr-item = ROWID(item).
      /*RUN esp/essp0026.p.*/
      RUN cdp/cd0204.p.
      ASSIGN FRAME F-Main:WINDOW:SENSITIVE = YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-nenhum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-nenhum B-table-Win
ON CHOOSE OF bt-nenhum IN FRAME F-Main
DO:
    FOR EACH tt-refer WHERE
             tt-refer.it-codigo = tt-itens.it-codigo.
        ASSIGN tt-refer.marca = "".
    END.
    br-refer:REFRESH().
    APPLY 'value-changed' TO br-refer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok B-table-Win
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
    ASSIGN l-confirma = NO
           i-new-progr = 0.

    RUN pi-ver-inclusao.
    RUN pi-ver-alteracao.

    IF l-confirma THEN
       MESSAGE 'Programaá∆o ' i-new-progr ' Efetuada com Sucesso....'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    RUN pi-open-query.
    APPLY 'value-changed' TO br-refer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ped-me-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ped-me-p B-table-Win
ON CHOOSE OF bt-ped-me-p IN FRAME F-Main
DO:
   FOR EACH tt-pedidos.
       ASSIGN tt-pedidos.visualiza = NO.

       IF tt-pedidos.row-temp-table = ROWID(tt-refer) AND
          tt-pedidos.dt-entrega > da-dt-entrega THEN
       ASSIGN tt-pedidos.visualiza = YES.
   END.
   RUN esp/essp0170a.p (INPUT 'E',
                        INPUT tt-refer.it-codigo,
                        INPUT tt-refer.cod-refer,
                        INPUT TABLE tt-pedidos).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ped-mi-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ped-mi-p B-table-Win
ON CHOOSE OF bt-ped-mi-p IN FRAME F-Main /* Button 2 */
DO:
    FOR EACH tt-pedidos.
        ASSIGN tt-pedidos.visualiza = NO.

        IF tt-pedidos.row-temp-table = ROWID(tt-refer) AND
           tt-pedidos.dt-entrega > da-dt-entrega THEN
        ASSIGN tt-pedidos.visualiza = YES.
    END.
    RUN esp/essp0170a.p (INPUT 'I',
                         INPUT tt-refer.it-codigo,
                         INPUT tt-refer.cod-refer,
                         INPUT TABLE tt-pedidos).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-pedidos B-table-Win
ON CHOOSE OF bt-pedidos IN FRAME F-Main
DO:
   FOR EACH tt-pedidos.
       ASSIGN tt-pedidos.visualiza = NO.

       IF tt-pedidos.row-temp-table = ROWID(tt-refer) AND
          tt-pedidos.dt-entrega <= da-dt-entrega THEN
       ASSIGN tt-pedidos.visualiza = YES.
   END.
   RUN esp/essp0170a.p (INPUT 'A',
                        INPUT tt-refer.it-codigo,
                        INPUT tt-refer.cod-refer,
                        INPUT TABLE tt-pedidos).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-todos B-table-Win
ON CHOOSE OF bt-todos IN FRAME F-Main
DO:
   FOR EACH tt-refer WHERE
            tt-refer.it-codigo = tt-itens.it-codigo.

       FIND item-ext WHERE
            item-ext.it-codigo = tt-refer.it-codigo
            NO-LOCK NO-ERROR.

       ASSIGN tt-refer.marca = "*"
              tt-refer.qt-sld-prog = IF ABS(tt-refer.qt-negativo + (tt-refer.qt-negativo /* * (item-ext.margem-pcp / 100)*/)) >= 50
                                     THEN ROUND(ABS(tt-refer.qt-negativo + (tt-refer.qt-negativo /* * (item-ext.margem-pcp / 100)*/)) / 50,0) * 50
                                     ELSE 50
              tt-refer.qt-new-prog = IF tt-refer.qt-new-prog >= 50
                                     THEN ROUND(tt-refer.qt-new-prog / 50,0) * 50
                                     ELSE 50. 
   END.

   br-refer:REFRESH().
   APPLY 'value-changed' TO br-refer IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapra B-table-Win
ON CHOOSE OF bt-vapra IN FRAME F-Main
DO:
  RUN esdlg/d01essp0160.w (OUTPUT c-cod-refer).
  IF c-cod-refer <> "" THEN DO:
     FIND FIRST tt-refer WHERE
                tt-refer.it-codigo = tt-itens.it-codigo AND
                tt-refer.cod-refer = c-cod-refer NO-LOCK NO-ERROR.
     IF AVAIL tt-refer THEN
        h-query:REPOSITION-TO-ROWID(ROWID(tt-refer)) NO-ERROR. 
     ELSE
        MESSAGE "Referància n∆o encontrada na seleá∆o..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

     APPLY 'value-changed' TO br-refer IN FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qt-new-prog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qt-new-prog B-table-Win
ON CURSOR-DOWN OF fi-qt-new-prog IN FRAME F-Main
DO:
   APPLY 'leave' TO SELF.
   APPLY 'cursor-down' TO br-refer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qt-new-prog B-table-Win
ON CURSOR-UP OF fi-qt-new-prog IN FRAME F-Main
DO:
   APPLY 'leave' TO SELF.
   APPLY 'cursor-up' TO br-refer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qt-new-prog B-table-Win
ON RETURN OF fi-qt-new-prog IN FRAME F-Main
DO:
  APPLY 'TAB' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qt-new-prog B-table-Win
ON TAB OF fi-qt-new-prog IN FRAME F-Main
DO:
  IF SELF:INPUT-VALUE < 0 AND
     fi-sld-prog:INPUT-VALUE - ABS(SELF:INPUT-VALUE) < 0 THEN DO.
     MESSAGE "Quantidade Programada n∆o pode ficar Negativa..."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.

  IF AVAIL tt-refer THEN DO.
     IF fi-qt-new-prog <> SELF:INPUT-VALUE THEN
        ASSIGN tt-refer.qt-new-prog = SELF:INPUT-VALUE.
               
     ELSE
        ASSIGN tt-refer.qt-new-prog = SELF:INPUT-VALUE.  

     ASSIGN tt-refer.marca = "*".
  END.

  br-refer:REFRESH().
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-estoque
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
ASSIGN c-arq-texto = SESSION:TEMP-DIRECTORY + "pcp.txt".

br-itens:NUM-LOCKED-COLUMNS = 2.
ASSIGN h-query = br-refer:QUERY.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-carteira B-table-Win 
PROCEDURE pi-carteira :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-situacao AS INT.

    FOR EACH ped-item WHERE        
             ped-item.cod-sit-item = p-situacao AND
             ped-item.it-codigo >= c-it-codigo-ini AND
             ped-item.it-codigo <= c-it-codigo-fin AND
             ped-item.cod-refer >= c-cod-refer-ini AND 
             ped-item.cod-refer <= c-cod-refer-fin NO-LOCK,
       FIRST ped-item-ext OF ped-item 
             WHERE LOOKUP(SUBSTR(ped-item-ext.lote,1,2),c-lotes) <> 0 NO-LOCK.
                        
        FIND ped-venda OF ped-item NO-LOCK NO-ERROR.

        IF ped-venda.cod-estab <> c-cod-estabel THEN NEXT.

        IF ped-item-ext.corte-comerc < c-corte-comerc-ini OR  
           ped-item-ext.corte-comerc > c-corte-comerc-fin THEN NEXT.

        FIND item WHERE
             item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

        FIND item-ext WHERE
             item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

        /*IF c-tp-artigo <> 'A' AND AVAIL item-ext THEN
           IF (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.*/

        IF i-tp-acab = 1 AND SUBSTR(ped-item.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-tp-acab = 2 AND SUBSTR(ped-item.cod-refer,7,1) = '0' THEN NEXT.

        IF ITEM.tipo-con-est = 4 THEN DO.
           FIND ref-item-ext WHERE
                ref-item-ext.it-codigo = ped-item.it-codigo AND
                ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
    
           IF NOT AVAIL ref-item-ext AND
              c-cod-obsoleto-ini <> '' THEN NEXT.
    
           IF AVAIL ref-item-ext THEN
              IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
                 ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.
        END.

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ped-item.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ped-item.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Corte_Comercial",
                           INPUT ped-item-ext.corte-comerc).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Codigo_Obsoleto",
                           INPUT ref-item-ext.cod-obsoleto).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + ped-item.nr-pedcli +
                                        "  Item: " + ped-item.it-codigo +
                                        "   Ref: " + ped-item.cod-refer).


        ASSIGN de-tot-res = 0.
        FOR EACH ped-item-rom WHERE
                 ped-item-rom.nome-abrev = ped-item.nome-abrev AND
                 ped-item-rom.nr-pedcli = ped-item.nr-pedcli AND
                 ped-item-rom.nr-sequencia = ped-item.nr-sequencia
                 NO-LOCK.

            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estab = ped-item-rom.cod-estab AND
                 ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL ob-etiqueta THEN NEXT.

            ASSIGN de-tot-res = de-tot-res + ob-etiqueta.quantidade.
        END.

        FIND tt-itens WHERE
             tt-itens.it-codigo = ped-item.it-codigo
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ped-item.it-codigo.
        END.

        FIND tt-work WHERE
             tt-work.it-codigo = ped-item.it-codigo AND
             tt-work.cod-refer = ped-item.cod-refer AND
             tt-work.corte-comerc = ped-item-ext.corte-comerc
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-work THEN DO.
           CREATE tt-work.
           ASSIGN tt-work.it-codigo = ped-item.it-codigo 
                  tt-work.cod-refer = ped-item.cod-refer
                  tt-work.corte-comerc = ped-item-ext.corte-comerc.
        END.

        IF ped-venda.dt-entrega <= da-dt-entrega THEN DO.
           IF ped-venda.nat-oper BEGINS '7' THEN
              ASSIGN tt-work.qt-externo = tt-work.qt-externo + ped-item.qt-pedida - de-tot-res.
           ELSE
              ASSIGN tt-work.qt-interno = tt-work.qt-interno + ped-item.qt-pedida - de-tot-res.
        END.
        ELSE DO. /* Meses Posteriores */
           IF ped-venda.nat-oper BEGINS '7' THEN
              ASSIGN tt-work.qt-p-externo = tt-work.qt-p-externo + ped-item.qt-pedida - de-tot-res.
           ELSE
              ASSIGN tt-work.qt-p-interno = tt-work.qt-p-interno + ped-item.qt-pedida - de-tot-res.
        END.

        FIND tt-pedidos WHERE
             tt-pedidos.row-temp-table = ROWID(tt-work) AND
             tt-pedidos.nr-pedcli = ped-item.nr-pedcli NO-ERROR.
        IF NOT AVAIL tt-pedidos THEN DO.
           CREATE tt-pedidos.
           ASSIGN tt-pedidos.row-temp-table = ROWID(tt-work)
                  tt-pedidos.nr-pedcli = ped-item.nr-pedcli
                  tt-pedidos.nome-abrev = ped-item.nome-abrev
                  tt-pedidos.dt-entrega = ped-venda.dt-entrega
                  tt-pedidos.mercado = NOT ped-venda.nat-oper BEGINS '7'.
        END.
        ASSIGN tt-pedidos.qt-pedida = tt-pedidos.qt-pedida + ped-item.qt-pedida - de-tot-res.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-estoque B-table-Win 
PROCEDURE pi-estoque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN esp/essp0150.p PERSISTENT SET h-essp0150.

    IF AVAIL tt-refer THEN DO:
       IF tt-refer.it-codigo <> "" THEN DO:
          FIND FIRST tt-estoque WHERE
                     tt-estoque.it-codigo = tt-refer.it-codigo AND
                     tt-estoque.cod-refer = tt-refer.cod-refer NO-LOCK NO-ERROR.
          
          IF NOT AVAIL tt-estoque THEN
             RUN pi-retorna-temp-table IN h-essp0150 (INPUT-OUTPUT TABLE tt-estoque,
                                                      INPUT c-cod-estabel,
                                                      INPUT tt-refer.it-codigo,
                                                      INPUT tt-refer.cod-refer,
                                                      INPUT c-dt-limite). 
       END.

       br-estoque:TITLE IN FRAME {&FRAME-NAME} = "ESTOQUE DA REFER“NCIA:   " + 
                                                  SUBSTR(tt-refer.cod-refer,1,2) + "." +
                                                  SUBSTR(tt-refer.cod-refer,3,4) + "." +
                                                  SUBSTR(tt-refer.cod-refer,7,1) .
    END.
    DELETE OBJECT h-essp0150.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-limpa B-table-Win 
PROCEDURE pi-limpa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt-itens.
    EMPTY TEMP-TABLE tt-work.
    EMPTY TEMP-TABLE tt-refer.
    EMPTY TEMP-TABLE tt-pcp.
    
    RUN adm-open-query-cases.
    APPLY 'VALUE-CHANGED' TO br-itens IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-open-query B-table-Win 
PROCEDURE pi-open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     {&OPEN-QUERY-br-refer}
     APPLY 'value-changed' TO br-refer IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-pcp B-table-Win 
PROCEDURE pi-pcp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ob-pcp WHERE
             ob-pcp.it-codigo >= c-it-codigo-ini AND
             ob-pcp.it-codigo <= c-it-codigo-fin NO-LOCK,
        EACH ob-pcp-ref OF ob-pcp WHERE
             ob-pcp-ref.situacao = 1 AND
             ob-pcp-ref.cod-refer >= c-cod-refer-ini AND
             ob-pcp-ref.cod-refer <= c-cod-refer-fin NO-LOCK.

        IF ob-pcp.cod-estab <> c-cod-estabel THEN NEXT.

        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ob-pcp.it-codigo AND
             ref-item-ext.cod-refer = ob-pcp-ref.cod-refer 
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ref-item-ext AND
           c-cod-obsoleto-ini <> '' THEN NEXT.

        IF AVAIL ref-item-ext THEN
           IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
              ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.

        IF i-tp-acab = 1 AND SUBSTR(ob-pcp-ref.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-tp-acab = 2 AND SUBSTR(ob-pcp-ref.cod-refer,7,1) = '0' THEN NEXT.

        FIND tt-refer WHERE
             tt-refer.it-codigo = ob-pcp.it-codigo AND
             tt-refer.cod-refer = ob-pcp-ref.cod-refer NO-ERROR.

        IF NOT AVAIL tt-refer THEN DO.
           CREATE tt-refer.
           ASSIGN tt-refer.it-codigo = ob-pcp.it-codigo 
                  tt-refer.cod-refer = ob-pcp-ref.cod-refer.
        END.

        CREATE tt-pcp.
        BUFFER-COPY ob-pcp-ref TO tt-pcp
              ASSIGN tt-pcp.it-codigo = ob-pcp.it-codigo.

        ASSIGN tt-refer.qt-sld-prog = tt-refer.qt-sld-prog + ob-pcp-ref.qtd-sld-prog 
               tt-refer.qt-margem = tt-refer.qt-margem - ob-pcp-ref.margem-pcp
               tt-refer.dt-ult-prog = ob-pcp-ref.dt-ult-prog
               tt-refer.qt-proc = tt-refer.qt-proc + ob-pcp-ref.qtd-proc 
               tt-refer.dt-ult-proc = ob-pcp-ref.dt-ult-proc
               tt-refer.qt-pron = tt-refer.qt-pron + ob-pcp-ref.qtd-pron
               tt-refer.dt-ult-pron = ob-pcp-ref.dt-ult-pron.

        IF tt-refer.qt-sld-prog < 0 THEN 
           ASSIGN tt-refer.qt-sld-prog = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa B-table-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-cod-estabel      AS CHAR.
    DEF INPUT PARAMETER p-dt-limite        AS CHAR.
    DEF INPUT PARAMETER p-it-codigo-ini    AS CHAR.
    DEF INPUT PARAMETER p-it-codigo-fin    AS CHAR.
    DEF INPUT PARAMETER p-cod-refer-ini    AS CHAR.
    DEF INPUT PARAMETER p-cod-refer-fin    AS CHAR.
    DEF INPUT PARAMETER p-corte-comerc-ini LIKE ob-etiqueta.corte-comerc.
    DEF INPUT PARAMETER p-corte-comerc-fin LIKE ob-etiqueta.corte-comerc.
    DEF INPUT PARAMETER p-cod-obsoleto-ini AS CHAR.                              
    DEF INPUT PARAMETER p-cod-obsoleto-fin AS CHAR.
    DEF INPUT PARAMETER p-lote-rp          AS LOG.
    DEF INPUT PARAMETER p-lote-ca          AS LOG.
    DEF INPUT PARAMETER p-tp-artigo        AS CHAR.
    DEF INPUT PARAMETER p-credito          AS INT.
    DEF INPUT PARAMETER p-tp-acab          AS INT.
    DEF INPUT PARAMETER p-negativo         AS LOG.
    DEF INPUT PARAMETER p-ppp              AS LOG.
    DEF INPUT PARAMETER p-estoque          AS LOG.
    DEF INPUT PARAMETER p-min-prod         AS DEC.
    DEF INPUT PARAMETER TABLE FOR tt-digita.  

    &SCOPED-DEFINE BREAK-BY tt-itens-ped.it-codigo + tt-itens-ped.cod-refer + tt-itens-ped.lote + tt-itens-ped.corte-comerc

    ASSIGN c-cod-estabel      = p-cod-estabel
           c-dt-limite        = p-dt-limite
           c-it-codigo-ini    = p-it-codigo-ini
           c-it-codigo-fin    = p-it-codigo-fin
           c-cod-refer-ini    = p-cod-refer-ini
           c-cod-refer-fin    = p-cod-refer-fin
           c-corte-comerc-ini = p-corte-comerc-ini
           c-corte-comerc-fin = p-corte-comerc-fin
           c-cod-obsoleto-ini = p-cod-obsoleto-ini 
           c-cod-obsoleto-fin = p-cod-obsoleto-fin 
           l-lote-rp          = p-lote-rp  
           l-lote-ca          = p-lote-ca  
           c-tp-artigo        = p-tp-artigo
           i-credito          = p-credito
           i-tp-acab          = p-tp-acab
           l-apenas-neg       = p-negativo
           l-apenas-ppp       = p-ppp
           l-apenas-estoq     = p-estoque
           de-min-prod        = p-min-prod
           c-progr            = "".
           
    FIND FIRST param-dis NO-LOCK NO-ERROR.

    EMPTY TEMP-TABLE tt-itens.
    EMPTY TEMP-TABLE tt-work.
    EMPTY TEMP-TABLE tt-refer.
    EMPTY TEMP-TABLE tt-pcp.

    FOR EACH tt-estoque.
        DELETE tt-estoque.
    END.

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Calculando_Estoque *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN esapi/ret-udm.p (INPUT c-dt-limite, OUTPUT c-dia).
    ASSIGN da-dt-entrega = DATE(c-dia + SUBSTR(c-dt-limite,1,2) + SUBSTR(c-dt-limite,3,4)).

    ASSIGN c-lotes = "".
    ASSIGN c-lotes = c-lotes + IF l-lote-rp = YES THEN "rp," ELSE ",".
    ASSIGN c-lotes = c-lotes + IF l-lote-ca = YES THEN "ca," ELSE ",".

    FOR EACH ob-etiqueta WHERE
             ob-etiqueta.situacao   = 3 AND
             ob-etiqueta.it-codigo >= c-it-codigo-ini AND
             ob-etiqueta.it-codigo <= c-it-codigo-fin AND
             ob-etiqueta.cod-refer >= c-cod-refer-ini AND
             ob-etiqueta.cod-refer <= c-cod-refer-fin AND
             LOOKUP(ob-etiqueta.nr-lote,c-lotes) <> 0 NO-LOCK:

        IF ob-etiqueta.cod-estab <> c-cod-estabel THEN NEXT.

        IF ob-etiqueta.corte-comerc < c-corte-comerc-ini OR  
           ob-etiqueta.corte-comerc > c-corte-comerc-fin THEN NEXT.

        IF ob-etiqueta.localizacao BEGINS '7' THEN NEXT.

        FIND item WHERE
             item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

        FIND item-ext WHERE
             item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

       /* IF c-tp-artigo <> 'A' AND AVAIL item-ext THEN
           IF (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.*/

        IF i-tp-acab = 1 AND SUBSTR(ob-etiqueta.cod-refer,7,1) <> '0' THEN NEXT.
        IF i-tp-acab = 2 AND SUBSTR(ob-etiqueta.cod-refer,7,1) = '0' THEN NEXT.

        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ob-etiqueta.it-codigo AND
             ref-item-ext.cod-refer = ob-etiqueta.cod-refer 
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ref-item-ext AND
           c-cod-obsoleto-ini <> '' THEN NEXT.

        IF AVAIL ref-item-ext THEN
           IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
              ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ob-etiqueta.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ob-etiqueta.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Corte_Comercial",
                           INPUT ob-etiqueta.corte-comerc).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Codigo_Obsoleto",
                           INPUT ref-item-ext.cod-obsoleto).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta) + 
                                            "    Item: " + ob-etiqueta.it-codigo +
                                            "     Ref: " + ob-etiqueta.cod-refer).

        FIND tt-itens WHERE
             tt-itens.it-codigo = ob-etiqueta.it-codigo
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ob-etiqueta.it-codigo.
        END.

        FIND tt-work WHERE 
             tt-work.it-codigo = tt-itens.it-codigo AND
             tt-work.cod-refer = ob-etiqueta.cod-refer AND
             tt-work.corte-comerc = ob-etiqueta.corte-comerc
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-work THEN DO.
           CREATE tt-work.
           ASSIGN tt-work.it-codigo = tt-itens.it-codigo
                  tt-work.cod-refer = ob-etiqueta.cod-refer
                  tt-work.corte-comerc = ob-etiqueta.corte-comerc.
        END.
        ASSIGN tt-work.qt-estoque = tt-work.qt-estoque + ob-etiqueta.quantidade.
    END.
                 
    {utp/ut-liter.i Calculando_Carteira *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-carteira (INPUT 1).
    RUN pi-carteira (INPUT 2).
    RUN pi-carteira (INPUT 5).

    {utp/ut-liter.i Calculando_Transformaá‰es *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).
    RUN pi-trf.

    IF NOT l-apenas-estoq THEN DO.

       {utp/ut-liter.i Calculando_Itens_Sem_Estoque *}
       RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

       FOR EACH item-uni-estab WHERE
                item-uni-estab.cod-estabel = c-cod-estabel AND
                item-uni-estab.it-codigo >= c-it-codigo-ini AND
                item-uni-estab.it-codigo <= c-it-codigo-fin NO-LOCK,
           FIRST item WHERE
                 item.it-codigo = item-uni-estab.it-codigo NO-LOCK,
           EACH ref-item WHERE
                ref-item.it-codigo = item.it-codigo AND
                ref-item.cod-refer >= c-cod-refer-ini AND
                ref-item.cod-refer <= c-cod-refer-fin NO-LOCK.

           FIND item-ext WHERE
                item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

           /*IF c-tp-artigo <> 'A' AND AVAIL item-ext THEN
              IF (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
                 (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.*/

           FIND ref-item-ext WHERE
                ref-item-ext.it-codigo = ref-item.it-codigo AND
                ref-item-ext.cod-refer = ref-item.cod-refer 
                NO-LOCK NO-ERROR.
           IF NOT AVAIL ref-item-ext AND
              c-cod-obsoleto-ini <> '' THEN NEXT.

           IF AVAIL ref-item-ext THEN
              IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
                 ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.


           RUN pi-ver-digita (INPUT "Item",
                              INPUT ITEM.it-codigo).
           IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
           RUN pi-ver-digita (INPUT "Referància",
                              INPUT ref-item.cod-refer).
           IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
           RUN pi-ver-digita (INPUT "Codigo_Obsoleto",
                              INPUT ref-item-ext.cod-obsoleto).
           IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

           RUN pi-acompanhar IN h-acomp (INPUT "Item: " + item.it-codigo +
                                               "  Ref: " + ref-item.cod-refer).

           FIND tt-itens WHERE
                tt-itens.it-codigo = item.it-codigo
                NO-LOCK NO-ERROR.

           IF NOT AVAIL tt-itens THEN DO.
              CREATE tt-itens.
              ASSIGN tt-itens.it-codigo = item.it-codigo.
           END.

           FIND tt-refer WHERE
                tt-refer.it-codigo = item.it-codigo AND
                tt-refer.cod-refer = ref-item.cod-refer NO-ERROR.
    
           IF NOT AVAIL tt-refer THEN DO.
              CREATE tt-refer.
              ASSIGN tt-refer.it-codigo = item.it-codigo 
                     tt-refer.cod-refer = ref-item.cod-refer.
           END.
       END.
    END.

    /* Calcula o Negatio de cada Corte Comercial sem as Programaá‰es */ 
    FOR EACH tt-work.
        ASSIGN tt-work.qt-negativo = tt-work.qt-estoque + tt-work.qt-trf - 
                                     tt-work.qt-interno - tt-work.qt-externo.
    END.

    FOR EACH tt-work WHERE
             tt-work.qt-negativo < 0 NO-LOCK 
        BREAK BY tt-work.it-codigo 
              BY tt-work.cod-refer.

        FIND item-ext WHERE
             item-ext.it-codigo = tt-work.it-codigo
             NO-LOCK NO-ERROR.

        FIND tt-refer WHERE
             tt-refer.it-codigo = tt-work.it-codigo AND
             tt-refer.cod-refer = tt-work.cod-refer NO-ERROR.

        IF NOT AVAIL tt-refer THEN DO.
           CREATE tt-refer.
           ASSIGN tt-refer.it-codigo = tt-work.it-codigo 
                  tt-refer.cod-refer = tt-work.cod-refer.
        END.

        ASSIGN tt-refer.qt-estoque   = tt-refer.qt-estoque + tt-work.qt-estoque   
               tt-refer.qt-interno   = tt-refer.qt-interno + tt-work.qt-interno   
               tt-refer.qt-externo   = tt-refer.qt-externo  + tt-work.qt-externo
               tt-refer.qt-p-interno = tt-refer.qt-p-interno + tt-work.qt-p-interno 
               tt-refer.qt-p-externo = tt-refer.qt-p-externo + tt-work.qt-p-externo
               tt-refer.qt-negativo  = tt-refer.qt-negativo + tt-work.qt-negativo  
               tt-refer.qt-trf       = tt-refer.qt-trf + tt-work.qt-trf.

        /* Relaciona os pedidos Ös referàncias */
        FOR EACH tt-pedidos WHERE
                 tt-pedidos.row-temp-table = ROWID(tt-work).
            ASSIGN tt-pedidos.row-temp-table = ROWID(tt-refer).
        END.
    END.

    IF NOT l-apenas-neg THEN DO.
       FOR EACH tt-work WHERE
                tt-work.qt-negativo >= 0 NO-LOCK.
    
           IF CAN-FIND(FIRST tt-refer WHERE
                             tt-refer.it-codigo = tt-work.it-codigo AND
                             tt-refer.cod-refer = tt-work.cod-refer AND
                             tt-refer.qt-negativo < 0) THEN NEXT.
    
           FIND tt-refer WHERE
                tt-refer.it-codigo = tt-work.it-codigo AND
                tt-refer.cod-refer = tt-work.cod-refer NO-ERROR.
    
           IF NOT AVAIL tt-refer THEN DO.
              CREATE tt-refer.
              ASSIGN tt-refer.it-codigo = tt-work.it-codigo 
                     tt-refer.cod-refer = tt-work.cod-refer.
           END.
    
           ASSIGN tt-refer.qt-estoque   = tt-refer.qt-estoque + tt-work.qt-estoque   
                  tt-refer.qt-interno   = tt-refer.qt-interno + tt-work.qt-interno   
                  tt-refer.qt-externo   = tt-refer.qt-externo  + tt-work.qt-externo
                  tt-refer.qt-p-interno = tt-refer.qt-p-interno + tt-work.qt-p-interno 
                  tt-refer.qt-p-externo = tt-refer.qt-p-externo + tt-work.qt-p-externo
                  tt-refer.qt-negativo  = 0 
                  tt-refer.qt-new-prog  = 0
                  tt-refer.qt-trf       = tt-refer.qt-trf + tt-work.qt-trf.
    
           /* Relaciona os pedidos Ös referàncias */
           FOR EACH tt-pedidos WHERE
                    tt-pedidos.row-temp-table = ROWID(tt-work).
               ASSIGN tt-pedidos.row-temp-table = ROWID(tt-refer).
           END.
       END.
    END.

    /* Verifica Programaá‰es j† efetuadas.... */
    RUN pi-pcp.

    FOR EACH tt-refer NO-LOCK.
        ASSIGN de-qt-ppp = tt-refer.qt-sld-prog + tt-refer.qt-proc + tt-refer.qt-pron.

        FIND item-ext WHERE
             item-ext.it-codigo = tt-refer.it-codigo
             NO-LOCK NO-ERROR.

        IF tt-refer.qt-negativo < 0 THEN
           ASSIGN tt-refer.qt-negativo = de-qt-ppp - ABS(tt-refer.qt-margem) - ABS(tt-refer.qt-negativo)  
                  tt-refer.qt-new-prog = ABS(tt-refer.qt-negativo) + ABS(tt-refer.qt-negativo /** (item-ext.margem-pcp / 100)*/).

        IF tt-refer.qt-negativo > 0 THEN
           ASSIGN tt-refer.qt-negativo = 0
                  tt-refer.qt-new-prog = 0.

        IF ABS(tt-refer.qt-negativo) < p-min-prod THEN DO.
           FOR EACH tt-pedidos WHERE
                    tt-pedidos.row-temp-table = ROWID(tt-refer).
               DELETE tt-pedidos.
           END.
           DELETE tt-refer.
           NEXT.
        END.

        IF (l-apenas-neg AND tt-refer.qt-negativo >= 0) OR
           (l-apenas-ppp AND de-qt-ppp = 0) OR
           (l-apenas-estoq AND tt-refer.qt-estoq = 0) THEN DO.
           FOR EACH tt-pedidos WHERE
                    tt-pedidos.row-temp-table = ROWID(tt-refer).
               DELETE tt-pedidos.
           END.
           DELETE tt-refer.
           NEXT.
        END.
    END.
    
    FOR EACH tt-pedidos WHERE
             tt-pedidos.qt-pedida = 0.
        DELETE tt-pedidos.
    END.
    
    FOR EACH tt-itens.
        IF NOT CAN-FIND(FIRST tt-refer WHERE
                              tt-refer.it-codigo = tt-itens.it-codigo) THEN
           DELETE tt-itens.
    END.

    RUN pi-finalizar in h-acomp.

    {&OPEN-QUERY-br-itens}
    APPLY 'value-changed' TO br-itens IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-recalc-ppp B-table-Win 
PROCEDURE pi-recalc-ppp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN tt-refer.qt-sld-prog = 0
           tt-refer.qt-proc = 0
           tt-refer.qt-pron = 0.
    FOR EACH ob-pcp WHERE
             ob-pcp.it-codigo = tt-refer.it-codigo NO-LOCK,
        EACH ob-pcp-ref OF ob-pcp WHERE
             ob-pcp-ref.situacao = 1 AND
             ob-pcp-ref.cod-refer = tt-refer.cod-refer NO-LOCK.

        ASSIGN tt-refer.qt-sld-prog = tt-refer.qt-sld-prog + ob-pcp-ref.qtd-sld-prog 
               tt-refer.dt-ult-prog = ob-pcp-ref.dt-ult-prog
               tt-refer.qt-proc = tt-refer.qt-proc + ob-pcp-ref.qtd-proc 
               tt-refer.dt-ult-proc = ob-pcp-ref.dt-ult-proc
               tt-refer.qt-pron = tt-refer.qt-pron + ob-pcp-ref.qtd-pron
               tt-refer.dt-ult-pron = ob-pcp-ref.dt-ult-pron.

        IF tt-refer.qt-sld-prog < 0 THEN 
           ASSIGN tt-refer.qt-sld-prog = 0.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-tot-estoq B-table-Win 
PROCEDURE pi-tot-estoq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-interno  = 0       fi-tot-externo = 0  
           fi-tot-saldo    = 0       fi-tot-prog    = 0
           fi-tot-trf      = 0       fi-tot-negativo = 0
           fi-tot-aprog    = 0.

    FOR EACH tt-refer NO-LOCK.
        ASSIGN fi-tot-interno  = fi-tot-interno  + tt-refer.qt-interno   
               fi-tot-externo  = fi-tot-externo  + tt-refer.qt-externo   
               fi-tot-saldo    = fi-tot-saldo    + tt-refer.qt-estoque
               fi-tot-prog     = fi-tot-prog     + tt-refer.qt-sld-prog + tt-refer.qt-proc + tt-refer.qt-pron
               fi-tot-trf      = fi-tot-trf      + tt-refer.qt-trf
               fi-tot-negativo = fi-tot-negativo + ABS(tt-refer.qt-negativo)
               fi-tot-aprog    = fi-tot-aprog    + tt-refer.qt-sld-prog.
    END.
    DISPLAY fi-tot-interno   
            fi-tot-externo   
            fi-tot-saldo
            fi-tot-prog
            fi-tot-trf
            fi-tot-negativo  
            fi-tot-aprog
            WITH FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais B-table-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-interno  = 0       fi-tot-externo = 0  
           fi-tot-saldo    = 0       fi-tot-prog    = 0
           fi-tot-trf      = 0       fi-tot-negativo = 0
           fi-tot-aprog    = 0.

    FOR EACH tt-refer NO-LOCK.
        ASSIGN fi-tot-interno  = fi-tot-interno  + tt-refer.qt-interno   
               fi-tot-externo  = fi-tot-externo  + tt-refer.qt-externo   
               fi-tot-saldo    = fi-tot-saldo    + tt-refer.qt-estoque
               fi-tot-prog     = fi-tot-prog     + tt-refer.qt-sld-prog + tt-refer.qt-proc + tt-refer.qt-pron
               fi-tot-trf      = fi-tot-trf      + tt-refer.qt-trf
               fi-tot-negativo = fi-tot-negativo + ABS(tt-refer.qt-negativo)
               fi-tot-aprog    = fi-tot-aprog    + tt-refer.qt-sld-prog.
    END.
    DISPLAY fi-tot-interno   
            fi-tot-externo   
            fi-tot-saldo
            fi-tot-prog
            fi-tot-trf
            fi-tot-negativo  
            fi-tot-aprog
            WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-total-refer B-table-Win 
PROCEDURE pi-total-refer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-estoque   = 0
           fi-tot-res-antc  = 0
           fi-tot-reserva   = 0
           fi-tot-trf-est   = 0
           fi-tot-benefic   = 0
           fi-tot-carteira  = 0
           fi-tot-res-cart  = 0
           fi-tot-saldo-est = 0.

    FOR EACH tt-estoque WHERE
             tt-estoque.it-codigo = tt-refer.it-codigo AND
             tt-estoque.cod-refer = tt-refer.cod-refer NO-LOCK,
        FIRST corte-comerc WHERE
              corte-comerc.codigo = tt-estoque.corte-comerc NO-LOCK.

        ASSIGN fi-tot-estoque   = fi-tot-estoque  + tt-estoque.qt-estoque
               fi-tot-res-antc  = fi-tot-res-antc + tt-estoque.qt-res-antc
               fi-tot-reserva   = fi-tot-reserva  + tt-estoque.qt-ped-reserva
               fi-tot-trf-est   = fi-tot-trf-est  + tt-estoque.qt-trf
               fi-tot-carteira  = fi-tot-carteira + 
                                  IF tt-estoque.qt-carteira - tt-estoque.qt-res-cart >= 0
                                  THEN tt-estoque.qt-carteira - tt-estoque.qt-res-cart
                                  ELSE 0
               fi-tot-res-cart  = fi-tot-res-cart + tt-estoque.qt-res-cart
               fi-tot-saldo-est = fi-tot-saldo-est + tt-estoque.qt-saldo.
        IF fi-tot-benefic = 0 AND tt-estoque.qt-benefic <> 0 THEN
           ASSIGN  fi-tot-benefic = tt-estoque.qt-benefic.

    END.
    IF fi-tot-carteira < 0 THEN
       ASSIGN fi-tot-carteira = 0.

    ASSIGN fi-tot-saldo-est:FGCOLOR IN FRAME {&FRAME-NAME} = ?.
    IF fi-tot-saldo-est < 0 THEN
       ASSIGN fi-tot-saldo-est:FGCOLOR IN FRAME {&FRAME-NAME} = 12.

    DISP fi-tot-estoque 
         fi-tot-res-antc
         fi-tot-reserva 
         fi-tot-trf-est     
         fi-tot-benefic 
         fi-tot-carteira
         fi-tot-res-cart
         fi-tot-saldo-est   
         WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-trf B-table-Win 
PROCEDURE pi-trf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ob-trf WHERE 
             ob-trf.situacao = 1 AND
             ob-trf.it-codigo >= c-it-codigo-ini AND
             ob-trf.it-codigo <= c-it-codigo-fin AND
             ob-trf.cod-refer >= c-cod-refer-ini AND 
             ob-trf.cod-refer <= c-cod-refer-fin AND
             LOOKUP(ob-trf.nr-lote,c-lotes) <> 0 NO-LOCK.

        IF SUBSTR(ob-trf.char-1,1,1) <> c-cod-estabel THEN NEXT.

        IF ob-trf.corte-comerc < c-corte-comerc-ini OR  
           ob-trf.corte-comerc > c-corte-comerc-fin THEN NEXT.
    
        FIND item WHERE
             item.it-codigo = ob-trf.it-codigo NO-LOCK NO-ERROR.
        /*IF item.deposito-pad <> c-cod-depos THEN NEXT.*/
    
        FIND item-ext WHERE
             item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
       /* IF c-tp-artigo <> 'A' THEN 
           IF AVAIL item-ext AND
              (item-ext.indigo = YES AND c-tp-artigo <> "I") OR
              (item-ext.indigo = NO  AND c-tp-artigo <> "O") THEN NEXT.*/
    
        FIND ref-item-ext WHERE
             ref-item-ext.it-codigo = ob-trf.it-codigo AND
             ref-item-ext.cod-refer = ob-trf.cod-refer 
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ref-item-ext AND
           c-cod-obsoleto-ini <> '' THEN NEXT.

        IF AVAIL ref-item-ext THEN
           IF ref-item-ext.cod-obsoleto < c-cod-obsoleto-ini OR
              ref-item-ext.cod-obsoleto > c-cod-obsoleto-fin THEN NEXT.

        RUN pi-ver-digita (INPUT "Item",
                           INPUT ob-trf.it-codigo).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Referància",
                           INPUT ob-trf.cod-refer).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        RUN pi-ver-digita (INPUT "Corte_Comercial",
                           INPUT ob-trf.corte-comerc).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    
        RUN pi-ver-digita (INPUT "Codigo_Obsoleto",
                           INPUT ref-item-ext.cod-obsoleto).
        IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.

        FIND tt-itens WHERE
             tt-itens.it-codigo = ob-trf.it-codigo
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-itens THEN DO.
           CREATE tt-itens.
           ASSIGN tt-itens.it-codigo = ob-trf.it-codigo.
        END.

        FOR EACH ob-etq-trf OF ob-trf NO-LOCK.
            FIND ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel = '1' AND
                 ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta
                 NO-LOCK NO-ERROR.

            /* Acrescenta transformaá‰es no saldo */
            FIND tt-work WHERE
                 tt-work.it-codigo = ob-trf.it-codigo AND
                 tt-work.cod-refer = ob-trf.cod-refer AND
                 tt-work.corte-comerc = ob-trf.corte-comerc
                 NO-LOCK NO-ERROR.

            IF NOT AVAIL tt-work THEN DO.
               CREATE tt-work.
               ASSIGN tt-work.it-codigo = ob-trf.it-codigo 
                      tt-work.cod-refer = ob-trf.cod-refer
                      tt-work.corte-comerc = ob-trf.corte-comerc.
            END.
            ASSIGN tt-work.qt-trf = tt-work.qt-trf + ob-etiqueta.quantidade.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-alteracao B-table-Win 
PROCEDURE pi-ver-alteracao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF NOT CAN-FIND(FIRST tt-pcp WHERE
                         tt-pcp.elimina OR
                         tt-pcp.modifica) THEN RETURN.

   ASSIGN c-mensagem = "Segue Anexo Arquivo com ALTERAÄÂES na Programaá∆o de Produá∆o" + CHR(10) + CHR(10) + CHR(10) +
                       "Atenciosamente," + CHR(10) +
                       "PCP".

   OUTPUT TO VALUE(c-arq-texto).
   PUT "Nro Progr  Data        Item      Descriá∆o                       Referencia  Fundo     Qtde DE  Qtde PARA  Observ" SKIP
       "---------  ----------  --------  ------------------------------  ----------  -----  ----------  ---------- --------------------"
       SKIP.

   FOR EACH tt-pcp WHERE
            tt-pcp.modifica OR 
            tt-pcp.elimina NO-LOCK,
       FIRST ob-pcp-ref WHERE
             ob-pcp-ref.num-progr = tt-pcp.num-progr AND
             ob-pcp-ref.cod-refer = tt-pcp.cod-refer SHARE-LOCK
             BREAK BY tt-pcp.it-codigo.

       IF tt-pcp.modifica AND
          ob-pcp-ref.qtd-prog = 0 THEN DO.
          MESSAGE 'Quantidade Programa est† Zerada...' SKIP
                  'Programaá∆o ' ob-pcp-ref.num-progr ' n∆o pode ser Modificada...'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN l-confirma = NO.
          NEXT.
       END.

       IF tt-pcp.elimina THEN DO.
          IF ob-pcp-ref.qtd-prog = 0 THEN DO.
             IF (ob-pcp-ref.qtd-proc > 0 OR 
                 ob-pcp-ref.qtd-pron > 0) THEN DO.
                 MESSAGE 'Existem quantidades em Processo ou Pronto' SKIP
                         'Programaá∆o ' ob-pcp-ref.num-progr ' n∆o pode ser Eliminada...'
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.

                 ASSIGN tt-pcp.elimina = NO
                        l-confirma = NO.

                 NEXT.
             END.
          END.
          ELSE DO.
             IF (ob-pcp-ref.qtd-proc > 0 OR 
                 ob-pcp-ref.qtd-pron > 0) THEN 
                 ASSIGN tt-pcp.qtd-sld-prog = tt-pcp.qtd-sld-prog - tt-pcp.qtd-progr
                        tt-pcp.qtd-progr = 0
                        tt-pcp.elimina = NO
                        tt-pcp.modifica = YES.
          END.
       END.

       FIND referencia-ext WHERE
            referencia-ext.cod-refer = tt-pcp.cod-refer
            NO-LOCK NO-ERROR.

       ASSIGN l-confirma = YES
              i-new-progr = ob-pcp-ref.num-progr.

       IF FIRST-OF(tt-pcp.it-codigo) THEN DO.
          FIND ITEM WHERE
               ITEM.it-codigo = tt-pcp.it-codig NO-LOCK NO-ERROR.

          PUT ob-pcp-ref.dt-ult-prog     AT 12
              tt-pcp.it-codigo           AT 24 FORMAT "x(8)" 
              ITEM.desc-item             AT 34 FORMAT "x(30)".
       END.

       PUT ob-pcp-ref.cod-refer      AT 66
           referencia-ext.cod-fundo  AT 78
           ob-pcp-ref.qtd-progr      AT 85
           IF tt-pcp.modifica 
           THEN tt-pcp.qtd-progr
           ELSE 0                    AT 97.

       IF tt-pcp.elimina THEN DO.
          PUT 'ELIMINADO' AT 108
              SKIP.

          DELETE ob-pcp-ref.
          DELETE tt-pcp.
          NEXT.
       END.

       PUT " " SKIP.

       ACCUMULATE tt-pcp.qtd-prog (TOTAL BY tt-pcp.it-codigo).
       ACCUMULATE tt-pcp.qtd-prog (TOTAL).

       ASSIGN ob-pcp-ref.qtd-prog = tt-pcp.qtd-progr
              ob-pcp-ref.qtd-sld-prog = tt-pcp.qtd-sld-prog
              ob-pcp-ref.dt-ult-prog = TODAY.

       ASSIGN tt-pcp.modifica = NO.
              
       IF LAST-OF(tt-pcp.it-codigo) THEN DO.
          PUT "TOTAL DO ITEM................."  AT 34
              ACCUM TOTAL BY tt-pcp.it-codigo tt-pcp.qtd-prog AT 97
              SKIP(1).
       END.
   END.

   PUT SKIP(2)
       "TOTAL GERAL..................."  AT 34
       ACCUM TOTAL tt-pcp.qtd-prog       AT 97
       SKIP.

   OUTPUT CLOSE.
   
   RUN esapi/esapi002.p (INPUT "controle.acabado@teartextil.com.br", /* e-mail remetente */
                         INPUT param-dis.grp-recipiente, /* e-mail destinat†rio */
                         INPUT "Programaá∆o de Produá∆o" , /* Assunto */
                         INPUT c-mensagem, /* Mensagem */
                         INPUT c-arq-texto, /*arquivo anexo*/
                         INPUT YES). /* Mostra Erros */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-digita B-table-Win 
PROCEDURE pi-ver-digita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-campo AS CHAR.
    DEF INPUT PARAMETER p-valor AS CHAR.

    IF CAN-FIND(FIRST tt-digita WHERE
                      tt-digita.opcao = 'D' AND
                      tt-digita.campo = p-campo) AND
       NOT CAN-FIND(FIRST tt-digita WHERE
                          tt-digita.opcao = 'D' AND
                          tt-digita.campo = p-campo AND
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ver-inclusao B-table-Win 
PROCEDURE pi-ver-inclusao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT CAN-FIND(FIRST tt-refer WHERE
                          tt-refer.marca <> "" AND
                          tt-refer.qt-new-prog > 0) THEN RETURN.

    DO TRANSACTION:
       FOR EACH tt-refer WHERE
                tt-refer.marca <> "" AND 
                tt-refer.qt-new-prog > 0 EXCLUSIVE-LOCK
                BREAK BY tt-refer.it-codigo.

           ASSIGN l-confirma = YES.

           IF FIRST-OF(tt-refer.it-codigo) THEN DO.
              CREATE ob-pcp.
              ASSIGN ob-pcp.num-progr = NEXT-VALUE(seq-pcp)
                     ob-pcp.cod-estab = c-cod-estabel
                     ob-pcp.dt-progr  = TODAY
                     ob-pcp.hr-progr  = TIME
                     ob-pcp.it-codigo = tt-refer.it-codigo
                     ob-pcp.usuario   = c-seg-usuario.

              ASSIGN i-new-progr = ob-pcp.num-progr
                     c-progr = IF c-progr = ""
                               THEN STRING(ob-pcp.num-progr)
                               ELSE c-progr + ";" + STRING(ob-pcp.num-progr).
           END.

           CREATE ob-pcp-ref.
           ASSIGN ob-pcp-ref.num-progr = ob-pcp.num-progr
                  ob-pcp-ref.cod-refer = tt-refer.cod-refer
                  ob-pcp-ref.qtd-progr = tt-refer.qt-new-prog
                  ob-pcp-ref.qtd-sld-prog = tt-refer.qt-new-prog
                  ob-pcp-ref.situacao = 1
                  ob-pcp-ref.usr-ult-prog = c-seg-usuario
                  ob-pcp-ref.dt-ult-prog = TODAY
                  ob-pcp-ref.observ = 'Programaá∆o: ' + STRING(ob-pcp.num-progr) + CHR(13).

           IF tt-refer.qt-negativo = 0 THEN
              ASSIGN ob-pcp-ref.margem-pcp = tt-refer.qt-new-prog /* * item-ext.margem-pcp / 100*/.
           ELSE 
              ASSIGN ob-pcp-ref.margem-pcp = tt-refer.qt-negativo /* * item-ext.margem-pcp / 100*/.

           ASSIGN c-ped-ex = ""    c-ped-en = ""
                  de-qt-en = 0     de-qt-ex = 0.

           FOR EACH tt-pedidos WHERE
                    tt-pedidos.row-temp-table = ROWID(tt-refer) AND
                    tt-pedidos.dt-entrega <= da-dt-entrega NO-LOCK.

               FIND ped-venda-ext WHERE
                    ped-venda-ext.nr-pedido = INT(tt-pedidos.nr-pedcli)  NO-LOCK NO-ERROR.
               IF AVAIL ped-venda-ext THEN DO.
                  IF ped-venda-ext.tp-pedido = 'Exportaá∆o' THEN
                     ASSIGN c-ped-ex = IF c-ped-ex = ""
                                       THEN tt-pedidos.nr-pedcli
                                       ELSE c-ped-ex + ", " + tt-pedidos.nr-pedcli
                            de-qt-ex = de-qt-ex + tt-pedidos.qt-pedida.

                  IF ped-venda-ext.l-emb-neutra THEN
                     ASSIGN c-ped-en = IF c-ped-en = ""
                                       THEN tt-pedidos.nr-pedcli
                                       ELSE c-ped-en + ", " + tt-pedidos.nr-pedcli
                            de-qt-en = de-qt-en + tt-pedidos.qt-pedida.
               END.
           END.

           IF c-ped-ex <> '' OR c-ped-en <> '' THEN DO.
              IF c-ped-ex = c-ped-en THEN 
                 ASSIGN ob-pcp-ref.observ = ob-pcp-ref.observ + 
                                            'Exportaá∆o / Emb Neutra / Ourela' + STRING(de-qt-ex,">>>,>>9.99") + 'm  (Pedidos: ' + c-ped-ex + ")" + CHR(13).  
              ELSE DO.
                  IF c-ped-ex <> '' THEN 
                     ASSIGN ob-pcp-ref.observ = ob-pcp-ref.observ + 
                                                'Exportaá∆o          ' + STRING(de-qt-ex,">>>,>>9.99") + 'm  (Pedidos: ' + c-ped-ex + ")" + CHR(13).  

                  IF c-ped-en <> '' THEN
                     ASSIGN ob-pcp-ref.observ = ob-pcp-ref.observ + 
                                                'Emb Neutra / Ourela ' + STRING(de-qt-en,">>>,>>9.99") + 'm  (Pedidos: ' + c-ped-en + ")" + CHR(13). 
              END.
           END.
           ASSIGN tt-refer.qt-sld-prog = tt-refer.qt-sld-prog + tt-refer.qt-new-prog
                  tt-refer.marca = "".

           IF tt-refer.qt-new-prog - tt-refer.qt-negativo > 0 THEN
              DELETE tt-refer.
       END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-vrf-dados B-table-Win 
PROCEDURE pi-vrf-dados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CAN-FIND(FIRST tt-refer WHERE
                      tt-refer.marca <> "") OR 
       CAN-FIND(FIRST tt-pcp WHERE
                      tt-pcp.elimina OR 
                      tt-pcp.modifica) THEN DO.

       MESSAGE 'A T E N Ä « O' SKIP(1)
               'Existem Manutená‰es efetuadas nas Programaá‰es' SKIP
               'que n∆o foram efetivadas....' SKIP(1)
               'Deseja SALVAR as Manutená‰es ?' 
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-choice AS LOGICAL.

       IF l-choice THEN.
          APPLY 'CHOOSE' TO bt-ok IN FRAME {&FRAME-NAME}.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "nr-ord-produ" "mgmov.ped-item" "nr-ord-produ"}
  {src/adm/template/sndkycas.i "nr-programa" "mgmov.ped-item" "nr-programa"}
  {src/adm/template/sndkycas.i "it-codigo" "mgmov.ped-item" "it-codigo"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-refer"}
  {src/adm/template/snd-list.i "tt-itens"}
  {src/adm/template/snd-list.i "tt-estoque"}
  {src/adm/template/snd-list.i "corte-comerc"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-benefic B-table-Win 
FUNCTION fn-benefic RETURNS DECIMAL
( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR de-benefic AS DEC.
   
   ASSIGN de-benefic = 0.
   IF CURRENT-RESULT-ROW("br-estoque") = 1 THEN
      ASSIGN de-benefic = tt-estoque.qt-benefic.
 
   RETURN de-benefic.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-carteira B-table-Win 
FUNCTION fn-carteira RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

 DEF VAR de-carteira AS DEC.
   
 ASSIGN de-carteira = tt-estoque.qt-carteira - tt-estoque.qt-res-cart.
 IF de-carteira < 0 THEN
    ASSIGN de-carteira = 0.

 RETURN de-carteira.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-desc-item B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-obsoleto B-table-Win 
FUNCTION fn-obsoleto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND ref-item-ext WHERE
         ref-item-ext.it-codigo = tt-refer.it-codigo AND
         ref-item-ext.cod-refer = tt-refer.cod-refer
         NO-LOCK NO-ERROR.

    IF AVAIL ref-item-ext THEN DO.
       CASE ref-item-ext.cod-obsoleto.
           WHEN '0' THEN RETURN 'Lanáamento'.
           WHEN '1' THEN RETURN 'Fora de Produá∆o'.
           WHEN '2' THEN RETURN 'Em Produá∆o'.
           WHEN '3' THEN RETURN 'Retalho'.
           WHEN '4' THEN RETURN 'Exclusividade'.
           WHEN '5' THEN RETURN 'Exportaá∆o'.
       END CASE.
    END.
    ELSE
       RETURN "N∆o Definido".  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

