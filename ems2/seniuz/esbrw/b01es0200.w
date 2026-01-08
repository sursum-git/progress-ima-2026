&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

DEF NEW GLOBAL SHARED VAR h-essp0200 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b01es0200 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b02es0200 AS HANDLE NO-UNDO.

DEF TEMP-TABLE  tt-itens-nf
    FIELD nr-nota-fis LIKE it-nota-fisc.nr-nota-fis
    FIELD nome-abrev  LIKE nota-fiscal.nome-ab-cli
    FIELD dt-emis     LIKE it-nota-fisc.dt-emis-nota
    FIELD it-codigo   LIKE it-nota-fisc.it-codigo
    FIELD cod-refer   LIKE ITEM.cod-refer
    FIELD nr-lote     LIKE ob-etiqueta.nr-lote
    FIELD qtde-item   AS DECIMAL FORMAT ">>,>>9.99"
    FIELD vlr-unit    AS DECIMAL FORMAT ">>>,>>9.99"
    FIELD vlr-total   AS DECIMAL FORMAT ">,>>>,>>9.99"
    FIELD nr-pedcli   LIKE it-nota-fisc.nr-pedcli
    FIELD descricao   AS CHAR FORMAT "x(30)"
    FIELD acao        AS CHAR
    FIELD nr-seq-ped  AS INT .

DEF TEMP-TABLE tt-itens-sel
    FIELD marca       AS   CHAR
    FIELD nr-nota-fis LIKE it-nota-fisc.nr-nota-fis
    FIELD nome-abrev  LIKE nota-fiscal.nome-ab-cli
    FIELD dt-emis     LIKE it-nota-fisc.dt-emis-nota
    FIELD it-codigo   LIKE it-nota-fisc.it-codigo
    FIELD cod-refer   LIKE ITEM.cod-refer
    FIELD nr-lote     LIKE ob-etiqueta.nr-lote
    FIELD qtde-item   AS DECIMAL FORMAT ">>,>>9.99"
    FIELD vlr-unit    AS DECIMAL FORMAT ">>>,>>9.99"
    FIELD vlr-total   AS DECIMAL FORMAT ">,>>>,>>9.99"
    FIELD nr-pedcli   LIKE it-nota-fisc.nr-pedcli
    FIELD descricao   AS CHAR FORMAT "x(30)"
    FIELD nr-seq-ped  AS INT
    FIELD manual      AS CHAR.


DEF TEMP-TABLE tt-etq-dev NO-UNDO LIKE ob-etiqueta
    FIELD nr-nota-fis         LIKE it-nota-fisc.nr-nota-fis
    INDEX indice1 localizacao ASCENDING num-etiqueta DESCENDING.

DEF TEMP-TABLE tt-aux 
    FIELD c-linha AS CHAR.

DEF BUFFER b-tt-itens-sel    FOR tt-itens-sel.    


/* Local Variable Definitions ---                                       */

DEF VAR i-row AS INTEGER.
DEF VAR l-ok AS LOGICAL INITIAL YES.

DEF VAR de-qtd-tot-nf AS DECIMAL NO-UNDO.
DEF VAR de-vlr-tot-nf AS DECIMAL NO-UNDO.

DEF VAR de-qtd-tot-sel AS DECIMAL NO-UNDO.
DEF VAR de-vlr-tot-sel AS DECIMAL NO-UNDO.

DEF VAR wh-pesquisa  AS HANDLE NO-UNDO.
DEF VAR l-implanta   AS LOGICAL NO-UNDO.
DEF VAR OKpressed  AS LOG.

DEF NEW GLOBAL SHARED VAR gr-nota-fiscal AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-emitente AS ROWID NO-UNDO.
    
DEF VAR i-ult-seq    AS INT.
DEF VAR i-resto      AS INT.
DEF VAR i-nr-seq-ped AS INT.
    
DEF VAR i-cont       AS INT.
DEF VAR i-soma       AS INT.
DEF VAR i-digito     AS INT.
DEF VAR c-aux        AS CHAR.
DEF VAR i-list-fat   AS INT EXTENT 43 INIT [4,3,2,9,8,7,6,5,4,3,2,9,8,7,6,5,4,3,2,9,8,7,6,5,4,3,2,9,8,7,6,5,4,3,2,9,8,7,6,5,4,3,2].

DEF VAR d-tot-item   AS DECIMAL.
DEF VAR i-cor        AS INT.

DEF VAR h-acomp      AS HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-item-nf

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens-nf tt-itens-sel

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br-item-nf                                    */
&Scoped-define FIELDS-IN-QUERY-br-item-nf tt-itens-nf.it-codigo tt-itens-nf.cod-refer tt-itens-nf.nr-lote tt-itens-nf.qtde-item tt-itens-nf.vlr-unit tt-itens-nf.vlr-tot tt-itens-nf.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-item-nf   
&Scoped-define SELF-NAME br-item-nf
&Scoped-define OPEN-QUERY-br-item-nf RUN pi-totais-nf. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-nf NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-item-nf tt-itens-nf
&Scoped-define FIRST-TABLE-IN-QUERY-br-item-nf tt-itens-nf


/* Definitions for BROWSE br-item-sel                                   */
&Scoped-define FIELDS-IN-QUERY-br-item-sel tt-itens-sel.it-codigo tt-itens-sel.cod-refer tt-itens-sel.nr-lote tt-itens-sel.qtde-item tt-itens-sel.vlr-unit tt-itens-sel.vlr-tot tt-itens-sel.descricao   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-item-sel tt-itens-sel.it-codigo   tt-itens-sel.cod-refer   tt-itens-sel.nr-lote ~
tt-itens-sel.qtde-item   tt-itens-sel.vlr-unit   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-item-sel tt-itens-sel
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-item-sel tt-itens-sel
&Scoped-define SELF-NAME br-item-sel
&Scoped-define OPEN-QUERY-br-item-sel RUN pi-totais-sel. OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-sel NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-item-sel tt-itens-sel
&Scoped-define FIRST-TABLE-IN-QUERY-br-item-sel tt-itens-sel


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-cod-cliente fi-nf-cli fi-serie-cliente ~
fi-nat-oper-cli tg-dev-cli fi-cod-chave-aces-nf-eletro fi-nf-origem tg-snfd ~
fi-arquivo-entrada bt-conf tg-dev-total br-item-nf br-item-sel ~
tg-nf-propria bt-right bt-left bt-modelo-rtf bt-add-item bt-mod-item ~
bt-exc-item fi-cod-estab fi-serie-origem RECT-11 RECT-62 RECT-66 RECT-67 ~
RECT-68 RECT-69 RECT-70 
&Scoped-Define DISPLAYED-OBJECTS fi-cod-cliente fi-nome-emit fi-nf-cli ~
fi-serie-cliente fi-nat-oper-cli tg-dev-cli fi-cod-chave-aces-nf-eletro ~
fi-serie-propria fi-nat-oper-propria fi-nome-estab fi-nat-oper fi-nf-origem ~
tg-snfd fi-arquivo-entrada tg-dev-total tg-nf-propria fi-qtd-tot-nf ~
fi-qtd-tot-sel fi-vlr-tot-nf fi-vlr-tot-sel fi-cod-estab fi-serie-origem 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 fi-nf-cli fi-serie-cliente fi-nat-oper-cli ~
fi-cod-chave-aces-nf-eletro 
&Scoped-define List-3 fi-serie-propria fi-nat-oper-propria 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-pedido||y|mgmov.it-nota-fisc.nr-pedido
it-codigo||y|mgmov.it-nota-fisc.it-codigo
nr-ord-produ||y|mgmov.it-nota-fisc.nr-ord-produ
cod-refer||y|mgmov.it-nota-fisc.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "nr-pedido,it-codigo,nr-ord-produ,cod-refer"':U).

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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add-item 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "" 
     SIZE 4.14 BY 1.75 TOOLTIP "Inclui Novo Item"
     BGCOLOR 8 .

DEFINE BUTTON bt-conf 
     IMAGE-UP FILE "image/im-cq.bmp":U
     LABEL "Button 1" 
     SIZE 5 BY 3.38 TOOLTIP "Detalha Nota Fiscal".

DEFINE BUTTON bt-consulta-nf 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "Button 3" 
     SIZE 4 BY 1.13 TOOLTIP "Detalha Nota Fiscal".

DEFINE BUTTON bt-exc-item 
     IMAGE-UP FILE "image/gr-eli.bmp":U
     LABEL "" 
     SIZE 4.14 BY 1.75 TOOLTIP "Elimina Item"
     BGCOLOR 8 .

DEFINE BUTTON bt-left 
     IMAGE-UP FILE "image/im-pre.bmp":U
     LABEL "Button 2" 
     SIZE 4.86 BY 1.33.

DEFINE BUTTON bt-mod-item 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "" 
     SIZE 4.14 BY 1.75 TOOLTIP "Modificar Valores"
     BGCOLOR 8 .

DEFINE BUTTON bt-modelo-rtf 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-right 
     IMAGE-UP FILE "image\im-nex.bmp":U
     LABEL "Button 1" 
     SIZE 4.86 BY 1.38.

DEFINE VARIABLE fi-arquivo-entrada AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arquivo de Dados" 
     VIEW-AS FILL-IN 
     SIZE 45 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-chave-aces-nf-eletro AS CHARACTER FORMAT "9999.9999.9999.9999.9999.9999.9999.9999.9999.9999.9999":U 
     LABEL "Chave Acesso" 
     VIEW-AS FILL-IN 
     SIZE 44 BY .88 TOOLTIP "Chave de Acesso" NO-UNDO.

DEFINE VARIABLE fi-cod-cliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estab AS CHARACTER FORMAT "X":U 
     LABEL "Estab." 
     VIEW-AS FILL-IN 
     SIZE 3.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nat-oper AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nat. Operaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nat-oper-cli AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nat. Oper." 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nat-oper-propria AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nat. Oper." 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nf-cli AS CHARACTER FORMAT "X(7)":U 
     LABEL "N. Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nf-origem AS CHARACTER FORMAT "X(7)":U 
     LABEL "NF de Origem" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nome-emit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 101.57 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.29 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-qtd-tot-nf AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Qtde Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FONT 1.

DEFINE VARIABLE fi-qtd-tot-sel AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Qtde Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-serie-cliente AS CHARACTER FORMAT "X(2)":U 
     LABEL "SÇrie" 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-serie-origem AS CHARACTER FORMAT "X(2)":U 
     LABEL "SÇrie Origem" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-serie-propria AS CHARACTER FORMAT "X(256)":U 
     LABEL "SÇrie" 
     VIEW-AS FILL-IN 
     SIZE 6.43 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-vlr-tot-nf AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-vlr-tot-sel AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Valor Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130 BY 13.75.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 5.75.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 5.75.

DEFINE RECTANGLE RECT-67
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 1.75.

DEFINE RECTANGLE RECT-68
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 6 BY 9.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-69
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130 BY 2.25.

DEFINE RECTANGLE RECT-70
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY 5.75.

DEFINE VARIABLE tg-dev-cli AS LOGICAL INITIAL yes 
     LABEL "Devoluá∆o NF do Cliente" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .83 NO-UNDO.

DEFINE VARIABLE tg-dev-total AS LOGICAL INITIAL no 
     LABEL "Devoluá∆o TOTAL da Nota  (Todas Etiquetas)" 
     VIEW-AS TOGGLE-BOX
     SIZE 42 BY .58 NO-UNDO.

DEFINE VARIABLE tg-nf-propria AS LOGICAL INITIAL no 
     LABEL "Devoluá∆o NF Pr¢pria" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE tg-snfd AS LOGICAL INITIAL no 
     LABEL "Nota de Faturamento N«O Espec°ficada" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-item-nf FOR 
      tt-itens-nf SCROLLING.

DEFINE QUERY br-item-sel FOR 
      tt-itens-sel SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-item-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-item-nf B-table-Win _FREEFORM
  QUERY br-item-nf NO-LOCK DISPLAY
      tt-itens-nf.it-codigo LABEL "Item"        FORMAT "x(8)":U   WIDTH 7
      tt-itens-nf.cod-refer LABEL "Ref."        FORMAT "x(8)":U   WIDTH 8.5
      tt-itens-nf.nr-lote   LABEL "Lote"        FORMAT "x(2)":U   WIDTH 3
      tt-itens-nf.qtde-item LABEL "Quant"       
      tt-itens-nf.vlr-unit  LABEL "Valor Un" 
      tt-itens-nf.vlr-tot   LABEL "Valor Total" 
      tt-itens-nf.descricao LABEL "Desc."       FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 56 BY 10.04
         FONT 1
         TITLE "Itens Faturados".

DEFINE BROWSE br-item-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-item-sel B-table-Win _FREEFORM
  QUERY br-item-sel NO-LOCK DISPLAY
      tt-itens-sel.it-codigo LABEL "Item"        FORMAT "x(10)":U  WIDTH 7   
      tt-itens-sel.cod-refer LABEL "Ref."        FORMAT "x(8)":U   WIDTH 8.5 
      tt-itens-sel.nr-lote   LABEL "Lote"        FORMAT "!!":U   WIDTH 3   
      tt-itens-sel.qtde-item LABEL "Quant."      
      tt-itens-sel.vlr-unit  LABEL "Valor Unit." 
      tt-itens-sel.vlr-tot   LABEL "Valor Total" 
      tt-itens-sel.descricao LABEL "Desc."       FORMAT "x(30)":U
ENABLE 
     tt-itens-sel.it-codigo
     tt-itens-sel.cod-refer
     tt-itens-sel.nr-lote  
     tt-itens-sel.qtde-item
     tt-itens-sel.vlr-unit
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 59 BY 10
         FONT 1
         TITLE "Itens Devolvidos" ROW-HEIGHT-CHARS .5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-cod-cliente AT ROW 1.75 COL 12 COLON-ALIGNED WIDGET-ID 8
     fi-nome-emit AT ROW 1.75 COL 26.43 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fi-nf-cli AT ROW 5.25 COL 12 COLON-ALIGNED WIDGET-ID 34
     fi-serie-cliente AT ROW 6.25 COL 12 COLON-ALIGNED WIDGET-ID 122
     fi-nat-oper-cli AT ROW 7.25 COL 12 COLON-ALIGNED WIDGET-ID 130
     tg-dev-cli AT ROW 4.17 COL 14 WIDGET-ID 166 NO-TAB-STOP 
     fi-cod-chave-aces-nf-eletro AT ROW 8.25 COL 12 COLON-ALIGNED WIDGET-ID 162
     fi-serie-propria AT ROW 5.25 COL 68 COLON-ALIGNED WIDGET-ID 30
     fi-nat-oper-propria AT ROW 6.25 COL 68 COLON-ALIGNED WIDGET-ID 134
     fi-nome-estab AT ROW 4.42 COL 95.72 COLON-ALIGNED NO-LABEL WIDGET-ID 140
     fi-nat-oper AT ROW 7.42 COL 92 COLON-ALIGNED WIDGET-ID 16 NO-TAB-STOP 
     fi-nf-origem AT ROW 6.42 COL 92 COLON-ALIGNED WIDGET-ID 14
     tg-snfd AT ROW 8.5 COL 94 WIDGET-ID 146
     fi-arquivo-entrada AT ROW 10.75 COL 78 COLON-ALIGNED WIDGET-ID 156
     bt-conf AT ROW 18.5 COL 125.72 WIDGET-ID 176
     tg-dev-total AT ROW 10.88 COL 14 WIDGET-ID 178
     br-item-nf AT ROW 12 COL 3
     br-item-sel AT ROW 12 COL 66 WIDGET-ID 200
     tg-nf-propria AT ROW 4.17 COL 60 WIDGET-ID 170 NO-TAB-STOP 
     bt-consulta-nf AT ROW 6.29 COL 108.43 WIDGET-ID 116
     bt-right AT ROW 16 COL 60 WIDGET-ID 112
     bt-left AT ROW 17.46 COL 60 WIDGET-ID 114
     fi-qtd-tot-nf AT ROW 22.21 COL 13.72 COLON-ALIGNED WIDGET-ID 66
     fi-qtd-tot-sel AT ROW 22.25 COL 80 COLON-ALIGNED WIDGET-ID 72
     bt-modelo-rtf AT ROW 10.67 COL 125.14 HELP
          "Escolha do nome do arquivo" WIDGET-ID 76
     fi-vlr-tot-nf AT ROW 22.21 COL 40.72 COLON-ALIGNED WIDGET-ID 68
     fi-vlr-tot-sel AT ROW 22.25 COL 106.57 COLON-ALIGNED WIDGET-ID 74
     bt-add-item AT ROW 13 COL 126 WIDGET-ID 144
     bt-mod-item AT ROW 14.75 COL 126 WIDGET-ID 46
     bt-exc-item AT ROW 16.5 COL 126 WIDGET-ID 58
     fi-cod-estab AT ROW 4.42 COL 92 COLON-ALIGNED WIDGET-ID 138 NO-TAB-STOP 
     fi-serie-origem AT ROW 5.42 COL 92 COLON-ALIGNED WIDGET-ID 12 NO-TAB-STOP 
     " Etiquetas Devolvidas" VIEW-AS TEXT
          SIZE 16.14 BY .71 AT ROW 9.88 COL 5 WIDGET-ID 158
          FGCOLOR 9 FONT 1
     " Nota Fiscal de Faturamento" VIEW-AS TEXT
          SIZE 21 BY .54 AT ROW 3.5 COL 83.14 WIDGET-ID 172
     " Devoluá∆o" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 3.46 COL 4 WIDGET-ID 174
     RECT-11 AT ROW 9.75 COL 2 WIDGET-ID 4
     RECT-62 AT ROW 3.75 COL 2 WIDGET-ID 108
     RECT-66 AT ROW 3.75 COL 81 WIDGET-ID 128
     RECT-67 AT ROW 10.25 COL 3 WIDGET-ID 154
     RECT-68 AT ROW 12.75 COL 125.14 WIDGET-ID 160
     RECT-69 AT ROW 1 COL 2 WIDGET-ID 164
     RECT-70 AT ROW 3.75 COL 58.72 WIDGET-ID 168
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


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
         HEIGHT             = 22.67
         WIDTH              = 132.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-item-nf tg-dev-total F-Main */
/* BROWSE-TAB br-item-sel br-item-nf F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-consulta-nf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-chave-aces-nf-eletro IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN fi-nat-oper IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nat-oper-cli IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN fi-nat-oper-propria IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi-nf-cli IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-emit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-tot-nf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-tot-sel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-serie-cliente IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR FILL-IN fi-serie-propria IN FRAME F-Main
   NO-ENABLE 3                                                          */
/* SETTINGS FOR FILL-IN fi-vlr-tot-nf IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-vlr-tot-sel IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-item-nf
/* Query rebuild information for BROWSE br-item-nf
     _START_FREEFORM
RUN pi-totais-nf.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-nf NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br-item-nf */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-item-sel
/* Query rebuild information for BROWSE br-item-sel
     _START_FREEFORM
RUN pi-totais-sel.
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-sel NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* BROWSE br-item-sel */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-item-nf
&Scoped-define SELF-NAME br-item-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-nf B-table-Win
ON ROW-DISPLAY OF br-item-nf IN FRAME F-Main /* Itens Faturados */
DO:
    FIND tt-itens-sel WHERE
         tt-itens-sel.nr-nota-fis = tt-itens-nf.nr-nota-fis AND
         tt-itens-sel.it-codigo = tt-itens-nf.it-codigo AND
         tt-itens-sel.cod-refer = tt-itens-nf.cod-refer AND
         tt-itens-sel.nr-seq-ped = tt-itens-nf.nr-seq-ped NO-ERROR.

    ASSIGN i-cor = ?.
    IF AVAIL tt-itens-sel THEN 
       ASSIGN i-cor = 12.

    tt-itens-nf.it-codigo:FGCOLOR IN BROWSE br-item-nf = i-cor.
    tt-itens-nf.cod-refer:FGCOLOR IN BROWSE br-item-nf = i-cor.
    tt-itens-nf.nr-lote:FGCOLOR IN BROWSE br-item-nf   = i-cor.
    tt-itens-nf.qtde-item:FGCOLOR IN BROWSE br-item-nf = i-cor.
    tt-itens-nf.vlr-unit:FGCOLOR IN BROWSE br-item-nf  = i-cor.
    tt-itens-nf.vlr-tot:FGCOLOR IN BROWSE br-item-nf   = i-cor.
    tt-itens-nf.descricao:FGCOLOR IN BROWSE br-item-nf = i-cor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-item-sel
&Scoped-define SELF-NAME br-item-sel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-sel B-table-Win
ON END-ERROR OF br-item-sel IN FRAME F-Main /* Itens Devolvidos */
ANYWHERE 
DO:
  IF tg-snfd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'yes'  THEN
     ASSIGN bt-add-item:SENSITIVE = YES.
  
  IF br-item-sel:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO:
     IF AVAIL tt-itens-sel THEN
        DELETE tt-itens-sel.
     IF br-item-sel:DELETE-CURRENT-ROW() IN FRAME {&FRAME-NAME} THEN. 
   END.

   ASSIGN tt-itens-sel.it-codigo:READ-ONLY IN BROWSE br-item-sel = YES  
          tt-itens-sel.cod-refer:READ-ONLY IN BROWSE br-item-sel = YES 
          tt-itens-sel.nr-lote:READ-ONLY IN BROWSE br-item-sel = YES    
          tt-itens-sel.qtde-item:READ-ONLY IN BROWSE br-item-sel = YES  
          tt-itens-sel.vlr-unit:READ-ONLY IN BROWSE br-item-sel = YES.

   {&OPEN-QUERY-br-item-sel}
        
   ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   IF tt-itens-sel.manual = 'S' THEN DO.
      APPLY 'VALUE-CHANGED' TO br-item-sel.
      APPLY 'entry' TO bt-add-item.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-sel B-table-Win
ON RETURN OF br-item-sel IN FRAME F-Main /* Itens Devolvidos */
ANYWHERE
DO:
   APPLY 'tab':U TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-sel B-table-Win
ON ROW-ENTRY OF br-item-sel IN FRAME F-Main /* Itens Devolvidos */
DO:
  IF br-item-sel:NEW-ROW IN FRAME {&FRAME-NAME} THEN DO.
     FIND LAST b-tt-itens-sel NO-LOCK NO-ERROR.
     IF AVAIL b-tt-itens-sel THEN DO.
        ASSIGN tt-itens-sel.it-codigo:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-tt-itens-sel.it-codigo)
               tt-itens-sel.cod-refer:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-tt-itens-sel.cod-refer)
               tt-itens-sel.nr-lote:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-tt-itens-sel.nr-lote)
               tt-itens-sel.qtde-item:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-tt-itens-sel.qtde-item)
               tt-itens-sel.vlr-unit:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(b-tt-itens-sel.vlr-unit).

        FIND ITEM WHERE
             ITEM.it-codigo = b-tt-itens-sel.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
           ASSIGN tt-itens-sel.descricao:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ITEM.desc-item).
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-sel B-table-Win
ON ROW-LEAVE OF br-item-sel IN FRAME F-Main /* Itens Devolvidos */
DO:
  ASSIGN d-tot-item = 0.
  
  IF LOOKUP(KEYFUNCTION(LAST-KEY),"TAB,RETURN") > 0 THEN DO.
     ASSIGN tt-itens-sel.it-codigo:READ-ONLY IN BROWSE br-item-sel = YES  
            tt-itens-sel.cod-refer:READ-ONLY IN BROWSE br-item-sel = YES 
            tt-itens-sel.nr-lote:READ-ONLY IN BROWSE br-item-sel = YES    
            tt-itens-sel.qtde-item:READ-ONLY IN BROWSE br-item-sel = YES  
            tt-itens-sel.vlr-unit:READ-ONLY IN BROWSE br-item-sel = YES.

     ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
            bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
            bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
   
     IF br-item-sel:NEW-ROW IN FRAME {&FRAME-NAME} THEN
        DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
   
        IF LOOKUP(KEYFUNCTION(LAST-KEY),"TAB,RETURN") > 0 THEN DO.
           CREATE tt-itens-sel.
           ASSIGN tt-itens-sel.nr-seq-ped = i-ult-seq.

           ASSIGN INPUT BROWSE br-item-sel tt-itens-sel.it-codigo
                  INPUT BROWSE br-item-sel tt-itens-sel.cod-refer
                  INPUT BROWSE br-item-sel tt-itens-sel.nr-lote
                  INPUT BROWSE br-item-sel tt-itens-sel.qtde-item
                  INPUT BROWSE br-item-sel tt-itens-sel.vlr-unit
                  INPUT BROWSE br-item-sel tt-itens-sel.vlr-tot.
          
           ASSIGN tt-itens-sel.manual = 'S'
                  tt-itens-sel.nome-abrev = fi-cod-cliente:SCREEN-VALUE.
       
           br-item-sel:CREATE-RESULT-LIST-ENTRY() IN FRAME {&FRAME-NAME}.
           RELEASE tt-itens-sel.
   
           APPLY 'CHOOSE' TO bt-add-item.
        END.
        ELSE
           APPLY 'END-ERROR' TO br-item-sel.
     END.
     ELSE DO TRANSACTION ON ERROR UNDO, RETURN NO-APPLY:
        ASSIGN INPUT BROWSE br-item-sel tt-itens-sel.it-codigo
               INPUT BROWSE br-item-sel tt-itens-sel.cod-refer
               INPUT BROWSE br-item-sel tt-itens-sel.nr-lote
               INPUT BROWSE br-item-sel tt-itens-sel.qtde-item
               INPUT BROWSE br-item-sel tt-itens-sel.vlr-unit
               INPUT BROWSE br-item-sel tt-itens-sel.vlr-tot.
          
        IF CURRENT-RESULT-ROW("br-item-sel") = NUM-RESULTS("br-item-sel") THEN 
           APPLY 'ENTRY' TO bt-mod-item.
     END.
  END.

  RUN pi-totais-sel.

  APPLY 'ENTRY' TO br-item-sel.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-item-sel B-table-Win
ON VALUE-CHANGED OF br-item-sel IN FRAME F-Main /* Itens Devolvidos */
DO:
   ASSIGN bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-exc-item:SENSITIVE = NO.

   IF AVAIL tt-itens-sel THEN
      ASSIGN bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME} = tt-itens-sel.manual = 'S' 
             bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add-item B-table-Win
ON CHOOSE OF bt-add-item IN FRAME F-Main
DO:
  ASSIGN tt-itens-sel.it-codigo:READ-ONLY IN BROWSE br-item-sel = NO.         
         tt-itens-sel.cod-refer:READ-ONLY IN BROWSE br-item-sel = NO.         
         tt-itens-sel.nr-lote:READ-ONLY IN BROWSE br-item-sel = NO.           
         tt-itens-sel.qtde-item:READ-ONLY IN BROWSE br-item-sel = NO.         
         tt-itens-sel.vlr-unit:READ-ONLY IN BROWSE br-item-sel = NO.           
         
  RUN pi-add-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-conf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-conf B-table-Win
ON CHOOSE OF bt-conf IN FRAME F-Main /* Button 1 */
DO:
   ASSIGN fi-cod-chave-aces-nf-eletro:FORMAT = "x(50)".
 
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-cliente
          INPUT FRAME {&FRAME-NAME} tg-dev-cli
          INPUT FRAME {&FRAME-NAME} fi-nf-cli 
          INPUT FRAME {&FRAME-NAME} fi-serie-cliente 
          INPUT FRAME {&FRAME-NAME} fi-nat-oper-cli
          INPUT FRAME {&FRAME-NAME} fi-cod-chave-aces-nf-eletro
          INPUT FRAME {&FRAME-NAME} tg-nf-propria
          INPUT FRAME {&FRAME-NAME} fi-serie-propria
          INPUT FRAME {&FRAME-NAME} fi-nat-oper-propria
          INPUT FRAME {&FRAME-NAME} fi-cod-estab
          INPUT FRAME {&FRAME-NAME} fi-serie-origem
          INPUT FRAME {&FRAME-NAME} fi-nf-origem
          INPUT FRAME {&FRAME-NAME} fi-nat-oper
          INPUT FRAME {&FRAME-NAME} tg-snfd
          INPUT FRAME {&FRAME-NAME} fi-arquivo-entrada.
 
   RUN pi-validate. 
   IF RETURN-VALUE = 'ADM-ERROR' THEN
      RETURN NO-APPLY.
 
   RUN pi-select-page IN h-essp0200 (INPUT 2).  
 
   RUN pi-processa IN h-b02es0200 (INPUT TABLE tt-itens-sel,
                                   INPUT TABLE tt-etq-dev,
                                   INPUT fi-cod-cliente,
                                   INPUT fi-cod-estab,
                                   INPUT fi-nf-origem,
                                   INPUT fi-serie-origem,
                                   INPUT fi-nat-oper,
                                   INPUT tg-snfd,
                                   INPUT fi-serie-propria,
                                   INPUT fi-nat-oper-propria,
                                   INPUT fi-nf-cli,
                                   INPUT fi-cod-chave-aces-nf-eletro,
                                   INPUT fi-serie-cliente,
                                   INPUT fi-nat-oper-cli). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-consulta-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-consulta-nf B-table-Win
ON CHOOSE OF bt-consulta-nf IN FRAME F-Main /* Button 3 */
DO:  
   ASSIGN CURRENT-WINDOW:SENSITIVE = NO.
   RUN ftp/ft0904.w.
   ASSIGN CURRENT-WINDOW:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exc-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exc-item B-table-Win
ON CHOOSE OF bt-exc-item IN FRAME F-Main
DO:
   GET CURRENT br-item-sel.
   DELETE tt-itens-sel.
   {&open-query-br-item-sel}
   APPLY 'VALUE-CHANGED' TO br-item-sel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-left
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-left B-table-Win
ON CHOOSE OF bt-left IN FRAME F-Main /* Button 2 */
DO:
  DO i-row = 1 TO br-item-sel:NUM-SELECTED-ROWS:
     IF br-item-sel:FETCH-SELECTED-ROW(i-row) THEN DO.
        FIND FIRST tt-itens-nf WHERE
                   tt-itens-nf.it-codigo = tt-itens-sel.it-codigo AND
                   tt-itens-nf.cod-refer = tt-itens-sel.cod-refer NO-ERROR.
        IF AVAIL tt-itens-nf THEN DO.
           ASSIGN tt-itens-nf.acao = "".
           DELETE tt-itens-sel.       
        END.
     END.
  END.
  {&OPEN-QUERY-br-item-sel}
  {&OPEN-QUERY-br-item-nf}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mod-item
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mod-item B-table-Win
ON CHOOSE OF bt-mod-item IN FRAME F-Main
DO:
   ASSIGN tt-itens-sel.qtde-item:READ-ONLY IN BROWSE br-item-sel = NO.         
          tt-itens-sel.vlr-unit:READ-ONLY IN BROWSE br-item-sel = NO.                   
          
   GET CURRENT br-item-sel.
   ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   APPLY 'entry':U TO tt-itens-sel.qtde-item IN BROWSE br-item-sel. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modelo-rtf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modelo-rtf B-table-Win
ON CHOOSE OF bt-modelo-rtf IN FRAME F-Main
DO:
   SYSTEM-DIALOG GET-FILE fi-arquivo-entrada
        TITLE      "Escolha o Arquivo"
        FILTERS    "Textos  (*.lst,*.txt)" "*.txt,*.lst",
                   "Todos Arquivos  (*.*)" "*.*"
        INITIAL-DIR SESSION:TEMP-DIRECTORY
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
      
   IF OKpressed = TRUE THEN DO.
      ASSIGN fi-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-arquivo-entrada.

      RUN pi-itens-devol.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-right
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-right B-table-Win
ON CHOOSE OF bt-right IN FRAME F-Main /* Button 1 */
DO:
  DO i-row = 1 TO  br-item-nf:NUM-SELECTED-ROWS.
     IF br-item-nf:FETCH-SELECTED-ROW(i-row) THEN DO.
        FIND tt-itens-sel WHERE
             tt-itens-sel.nr-nota-fis = tt-itens-nf.nr-nota-fis AND
             tt-itens-sel.it-codigo = tt-itens-nf.it-codigo AND
             tt-itens-sel.cod-refer = tt-itens-nf.cod-refer AND
             tt-itens-sel.nr-seq-ped = tt-itens-nf.nr-seq-ped NO-ERROR.

        IF AVAIL tt-itens-sel THEN DO.
            MESSAGE "Esse item j† foi selecionado!"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
            NEXT.
        END.

        CREATE tt-itens-sel.
        ASSIGN tt-itens-sel.it-codigo   = tt-itens-nf.it-codigo
               tt-itens-sel.cod-refer   = tt-itens-nf.cod-refer
               tt-itens-sel.nr-lote     = tt-itens-nf.nr-lote
               tt-itens-sel.qtde-item   = tt-itens-nf.qtde-item
               tt-itens-sel.vlr-unit    = tt-itens-nf.vlr-unit 
               tt-itens-sel.vlr-total   = tt-itens-nf.vlr-total
               tt-itens-sel.descricao   = tt-itens-nf.descricao
               tt-itens-sel.nr-nota-fis = tt-itens-nf.nr-nota-fis 
               tt-itens-sel.nome-abrev  = tt-itens-nf.nome-abrev 
               tt-itens-sel.dt-emis     = tt-itens-nf.dt-emis
               tt-itens-sel.nr-pedcli   = tt-itens-nf.nr-pedcli
               tt-itens-sel.nr-seq-ped  = tt-itens-nf.nr-seq-ped.
                   
        ASSIGN tt-itens-nf.acao = "sel".
     END.
  END.

  {&OPEN-QUERY-br-item-sel}
  {&OPEN-QUERY-br-item-nf}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-chave-aces-nf-eletro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-chave-aces-nf-eletro B-table-Win
ON ENTRY OF fi-cod-chave-aces-nf-eletro IN FRAME F-Main /* Chave Acesso */
DO:
   ASSIGN SELF:FORMAT = '9999.9999.9999.9999.9999.9999.9999.9999.9999.9999.9999'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-chave-aces-nf-eletro B-table-Win
ON LEAVE OF fi-cod-chave-aces-nf-eletro IN FRAME F-Main /* Chave Acesso */
DO:
   ASSIGN SELF:FORMAT = 'x(50)'.

   IF INPUT FRAME {&FRAME-NAME} fi-cod-chave-aces-nf-eletro <> "" THEN DO:
      ASSIGN i-soma = 0
             c-aux  = SUBSTR(INPUT FRAME {&FRAME-NAME} fi-cod-chave-aces-nf-eletro,1,43).
   
      DO i-cont = 1 TO 43:
         ASSIGN i-soma = i-soma + INT(SUBSTR(c-aux,i-cont,1)) * i-list-fat[i-cont].
      END.
      ASSIGN i-digito = 11 - (i-soma MODULO 11).
      IF i-digito > 9 THEN
         ASSIGN i-digito = 0.
   
      IF INT(SUBSTR(INPUT FRAME {&FRAME-NAME} fi-cod-chave-aces-nf-eletro,44,1)) <> i-digito THEN DO:
         MESSAGE "D°gito verificador da chave n∆o confere!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-cod-chave-aces-nf-eletro IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cliente B-table-Win
ON F5 OF fi-cod-cliente IN FRAME F-Main /* Cliente */
DO:
   {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                      &campo     = fi-cod-cliente
                      &campozoom = nome-abrev}
                       
   APPLY "LEAVE" TO fi-cod-cliente.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cliente B-table-Win
ON LEAVE OF fi-cod-cliente IN FRAME F-Main /* Cliente */
DO:
   IF LAST-EVENT:FUNCTION = 'MOUSE-SELECT-DBLCLICK' THEN NEXT.
                              
   IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
      APPLY 'BACK-TAB' TO SELF.
      RETURN NO-APPLY.
   END.

   IF SELF:SCREEN-VALUE  = "" THEN DO.
      MESSAGE "Favor Informar O Cliente"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "entry" TO fi-cod-cliente.
      RETURN NO-APPLY.
   END.
   
   FIND emitente WHERE 
        emitente.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-cod-cliente
        NO-LOCK NO-ERROR.

   IF NOT AVAIL emitente THEN
      FIND emitente WHERE 
           STRING(emitente.cod-emit) = INPUT FRAME {&FRAME-NAME} fi-cod-cliente 
           USE-INDEX codigo NO-LOCK NO-ERROR.

   IF NOT AVAIL emitente THEN DO.
      MESSAGE 'Cliente n∆o Cadastrado...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   ASSIGN fi-nome-emit:SCREEN-VALUE  = emitente.nome-emit
          fi-cod-cliente:SCREEN-VALUE = emitente.nome-abrev
          fi-nf-origem:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          fi-nat-oper:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          gr-emitente = ROWID(emitente).

   RUN pi-deleta-processo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cliente B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-cliente IN FRAME F-Main /* Cliente */
DO:
   APPLY "F5" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estab B-table-Win
ON LEAVE OF fi-cod-estab IN FRAME F-Main /* Estab. */
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND estabelec WHERE 
           estabelec.cod-estabel =  SELF:SCREEN-VALUE NO-LOCK NO-ERROR.

      IF NOT AVAIL estabelec THEN DO.
         MESSAGE "Estabelecimento N∆o Cadastrado !!!"
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO fi-cod-estab.
      END.

      ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nf-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nf-cli B-table-Win
ON LEAVE OF fi-nf-cli IN FRAME F-Main /* N. Fiscal */
DO:
  ASSIGN SELF:SCREEN-VALUE = STRING(INT(SELF:SCREEN-VALUE),"9999999").

  FIND FIRST devol-cli WHERE
             devol-cli.cod-estabel = INPUT fi-cod-estab AND 
             devol-cli.nr-nota-fis = nota-fiscal.nr-nota-fis AND
             devol-cli.serie = INPUT fi-serie-cliente AND                
             devol-cli.nro-docto = INPUT fi-nf-cli  NO-LOCK NO-ERROR.
  IF AVAIL devol-cli THEN DO.
     FIND natur-oper WHERE
          natur-oper.nat-operacao = devol-cli.nat-operacao NO-LOCK NO-ERROR.
     ASSIGN fi-nat-oper-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.nat-operacao.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nf-origem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nf-origem B-table-Win
ON F5 OF fi-nf-origem IN FRAME F-Main /* NF de Origem */
DO:
  {include/zoomvar.i &prog-zoom = dizoom/z03di135.w
                     &campo     = fi-nf-origem
                     &campozoom = nr-nota-fis}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nf-origem B-table-Win
ON LEAVE OF fi-nf-origem IN FRAME F-Main /* NF de Origem */
DO:
    ASSIGN gr-nota-fiscal = ?.
    ASSIGN tg-snfd:SCREEN-VALUE = 'NO'.
    ASSIGN bt-consulta-nf:SENSITIVE = NO.

    IF fi-nf-origem:SCREEN-VALUE <> "" THEN DO.
       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = fi-cod-estab:SCREEN-VALUE AND
            nota-fiscal.serie = fi-serie-origem:SCREEN-VALUE AND
            nota-fiscal.nr-nota-fis = fi-nf-origem:SCREEN-VALUE
            NO-LOCK NO-ERROR.
    
       IF NOT AVAIL nota-fiscal THEN DO.
          MESSAGE 'Nota Fiscal N«O Encontrada no Sistema....'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO SELF.
          RETURN NO-APPLY.
       END.
    
       IF nota-fiscal.nome-ab-cli <> fi-cod-cliente:SCREEN-VALUE THEN DO.
          MESSAGE 'Nota Fiscal n∆o Pertente ao Cliente Informado....'
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO SELF.
          RETURN NO-APPLY.
       END.
    
       ASSIGN fi-nat-oper:SCREEN-VALUE = nota-fiscal.nat-operacao.
       ASSIGN gr-nota-fiscal =  ROWID(nota-fiscal).
       ASSIGN bt-consulta-nf:SENSITIVE = YES.
    
       RUN pi-itens-nf.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nf-origem B-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nf-origem IN FRAME F-Main /* NF de Origem */
DO:
   APPLY 'F5' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-dev-cli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-dev-cli B-table-Win
ON VALUE-CHANGED OF tg-dev-cli IN FRAME F-Main /* Devoluá∆o NF do Cliente */
DO:
    ASSIGN tg-nf-propria:SCREEN-VALUE = 'NO'
           SELF:SCREEN-VALUE = 'YES'.

    ASSIGN fi-serie-propria:SCREEN-VALUE = ''.

    DISABLE {&list-3} WITH FRAME {&FRAME-NAME}.
    ENABLE  {&list-2} WITH FRAME {&FRAME-NAME}.

    IF INPUT FRAME {&FRAME-NAME} fi-cod-cliente <> '' THEN
       APPLY 'entry' TO fi-nf-cli.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-dev-total
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-dev-total B-table-Win
ON VALUE-CHANGED OF tg-dev-total IN FRAME F-Main /* Devoluá∆o TOTAL da Nota  (Todas Etiquetas) */
DO:
   EMPTY TEMP-TABLE tt-itens-sel.
   EMPTY TEMP-TABLE tt-etq-dev.

   {&OPEN-QUERY-br-item-sel}

   IF tg-dev-total:SCREEN-VALUE = 'NO' THEN
      RETURN NO-APPLY.

   FIND nota-fiscal WHERE
        nota-fiscal.cod-estabel = fi-cod-estab:SCREEN-VALUE AND
        nota-fiscal.serie = fi-serie-origem:SCREEN-VALUE AND
        nota-fiscal.nr-nota-fis = fi-nf-origem:SCREEN-VALUE
        NO-LOCK NO-ERROR.
 
   FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
       FOR EACH ped-item-res WHERE
                ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
                ped-item-res.serie       = nota-fiscal.serie AND
                ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
                ped-item-res.faturado    = YES NO-LOCK.
 
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
 
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    NO-LOCK NO-ERROR.
 
               IF ob-etiqueta.situacao <> 5 THEN DO.
                  MESSAGE 'Etiqueta ' ob-etiqueta.num-etiqueta ' Ser† desconsiderada pois n∆o foi Faturada' 
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  NEXT.
               END.
 
               FIND ITEM WHERE
                    ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
 
               FIND FIRST tt-itens-sel WHERE
                          tt-itens-sel.nr-nota-fis = nota-fiscal.nr-nota-fis AND
                          tt-itens-sel.it-codigo   = ob-etiqueta.it-codigo AND
                          tt-itens-sel.cod-refer   = ob-etiqueta.cod-refer AND
                          tt-itens-sel.nr-lote     = ob-etiqueta.nr-lote   AND
                          tt-itens-sel.vlr-unit    = it-nota-fisc.vl-preuni NO-LOCK NO-ERROR.
 
               IF NOT AVAIL tt-itens-sel THEN DO.
                  CREATE tt-itens-sel.
                  ASSIGN tt-itens-sel.dt-emis      = nota-fiscal.dt-emis
                         tt-itens-sel.nr-nota-fis  = nota-fiscal.nr-nota-fis
                         tt-itens-sel.nr-pedcli    = nota-fiscal.nr-pedcli
                         tt-itens-sel.nome-abrev   = nota-fiscal.nome-ab-cli
                         tt-itens-sel.it-codigo    = ob-etiqueta.it-codigo
                         tt-itens-sel.cod-refer    = ob-etiqueta.cod-refer
                         tt-itens-sel.nr-lote      = ob-etiqueta.nr-lote
                         tt-itens-sel.vlr-unit     = it-nota-fisc.vl-preuni
                         tt-itens-sel.descricao    = ITEM.desc-item.
               END.
               ASSIGN tt-itens-sel.vlr-total = tt-itens-sel.vlr-total + (it-nota-fisc.vl-preuni * ob-etiqueta.quantidade) 
                      tt-itens-sel.qtde-item = tt-itens-sel.qtde-item + ob-etiqueta.quantidade.
 
               CREATE tt-etq-dev.
               BUFFER-COPY ob-etiqueta TO tt-etq-dev
                    ASSIGN tt-etq-dev.nr-nota-fis = nota-fiscal.nr-nota-fis
                           tt-etq-dev.it-codigo = ob-etiqueta.it-codigo
                           tt-etq-dev.cod-refer = ob-etiqueta.cod-refer
                           tt-etq-dev.nr-lote = ob-etiqueta.nr-lote.
           END.
       END.
   END.
     
   {&OPEN-QUERY-br-item-sel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-nf-propria
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-nf-propria B-table-Win
ON VALUE-CHANGED OF tg-nf-propria IN FRAME F-Main /* Devoluá∆o NF Pr¢pria */
DO:
  ASSIGN tg-nf-propria:SCREEN-VALUE = 'YES'
         tg-dev-cli:SCREEN-VALUE = 'NO'.

  ENABLE  {&list-3} WITH FRAME {&FRAME-NAME}.
  DISABLE {&list-2} WITH FRAME {&FRAME-NAME}.

  FIND FIRST para-fat NO-LOCK NO-ERROR.

  ASSIGN fi-serie-propria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = para-fat.serie-pad.

  APPLY 'entry' TO fi-serie-propria.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-snfd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-snfd B-table-Win
ON VALUE-CHANGED OF tg-snfd IN FRAME F-Main /* Nota de Faturamento N«O Espec°ficada */
DO:
   EMPTY TEMP-TABLE tt-itens-sel.
   EMPTY TEMP-TABLE tt-itens-sel.
   EMPTY TEMP-TABLE tt-etq-dev.

   ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

   ASSIGN bt-right:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-left:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   ASSIGN fi-nf-origem:SENSITIVE = YES.

   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      ASSIGN fi-nf-origem:SCREEN-VALUE = ''
             fi-nat-oper:SCREEN-VALUE = ''.

      ASSIGN fi-nf-origem:SENSITIVE = NO.

      ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME} = YES
             bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME} =  NO
             bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME} =  NO.

      ASSIGN bt-right:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             bt-left:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

      FOR EACH tt-itens-nf.
          DELETE tt-itens-nf.
      END.

   END.

   {&OPEN-QUERY-br-item-nf}
   {&OPEN-QUERY-br-item-sel}

   APPLY 'ENTRY' TO fi-nf-origem.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-item-nf
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
/*
/* Substitui TAB por ENTER */
ON 'return':U ANYWHERE DO:
    APPLY 'tab' TO SELF.
    RETURN NO-APPLY.
END.
*/   

 ASSIGN tt-itens-sel.it-codigo:READ-ONLY IN BROWSE br-item-sel = YES         
        tt-itens-sel.cod-refer:READ-ONLY IN BROWSE br-item-sel = YES         
        tt-itens-sel.nr-lote:READ-ONLY IN BROWSE br-item-sel = YES           
        tt-itens-sel.qtde-item:READ-ONLY IN BROWSE br-item-sel = YES         
        tt-itens-sel.vlr-unit:READ-ONLY IN BROWSE br-item-sel = YES.          

ON 'ENTRY':U ANYWHERE DO:
    ASSIGN fi-cod-chave-aces-nf-eletro:FORMAT IN FRAME {&FRAME-NAME}= '9999.9999.9999.9999.9999.9999.9999.9999.9999.9999.9999'.
END.

ON 'leave':U OF tt-itens-sel.it-codigo IN BROWSE br-item-sel DO:
    IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
       APPLY 'END-ERROR' TO br-item-sel IN FRAME {&FRAME-NAME}.
       RETURN NO-APPLY.
    END.

    IF INPUT BROWSE br-item-sel tt-itens-sel.it-codigo = '' THEN DO.
       MESSAGE 'Item deve ser Informado...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN NO-APPLY.
    END.

    FIND ITEM WHERE
         ITEM.it-codigo = INPUT BROWSE br-item-sel tt-itens-sel.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL item THEN DO.
       MESSAGE 'Item n∆o Cadastrado...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN NO-APPLY.
    END.
    ASSIGN tt-itens-sel.descricao:SCREEN-VALUE IN BROWSE br-item-sel = STRING(ITEM.desc-item).
    /*DISP ITEM.desc-item WITH BROWSE br-item-sel.*/

END.

ON 'F5':U, 'mouse-select-dblclick':U OF tt-itens-sel.it-codigo IN BROWSE br-item-sel DO:
    {include/zoomvar.i &prog-zoom=inzoom/z01in172.w
                       &campo=tt-itens-sel.it-codigo
                       &campozoom=it-codigo
                       &BROWSE=br-item-sel}
END.

ON 'leave':U OF tt-itens-sel.cod-refer IN BROWSE br-item-sel DO:
    IF KEYFUNCTION(LASTKEY) = 'back-tab' THEN DO.
       APPLY 'BACK-TAB' TO SELF.
       RETURN NO-APPLY.
    END.

    IF item.tipo-con-est <> 4 AND INPUT BROWSE br-item-sel tt-itens-sel.cod-refer <> "" THEN DO:
       MESSAGE "Item n∆o Ç controlado por Referància. Referància deve ser BRANCO." 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN NO-APPLY.
    END.

    IF item.tipo-con-est = 4 THEN DO:
       FIND referencia WHERE 
            referencia.cod-refer = INPUT BROWSE br-item-sel tt-itens-sel.cod-refer
            NO-LOCK NO-ERROR.

       IF NOT AVAIL referencia THEN DO:
           MESSAGE 'Referància n∆o Cadastrada...'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY 'entry' TO SELF.
           RETURN NO-APPLY.
       END.

       FIND ref-item WHERE
            ref-item.cod-refer = INPUT BROWSE br-item-sel tt-itens-sel.cod-refer AND
            ref-item.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.

       IF NOT AVAIL ref-item THEN DO.
          MESSAGE "Referància n∆o Vinculada ao Item..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'entry' TO SELF.
          RETURN NO-APPLY.
       END.
    END.
END.

ON 'F5':U, 'mouse-select-dblclick':U OF tt-itens-sel.cod-refer IN BROWSE br-item-sel DO:
    {include/zoomvar.i &prog-zoom  = inzoom/z01in375.w
                       &campo      = tt-itens-sel.cod-refer
                       &campozoom  = cod-refer
                       &BROWSE     = br-item-sel
                       &parametros = "run pi-seta-inicial in wh-pesquisa(input input BROWSE br-item-sel tt-itens-sel.it-codigo)."}
END.
        
ON 'leave':U OF tt-itens-sel.qtde-item IN BROWSE br-item-sel DO:
    IF AVAIL tt-itens-sel AND
       INPUT BROWSE br-item-sel tt-itens-sel.qtde-item > tt-itens-sel.qtde-item THEN DO.
       MESSAGE 'Quantidade Informada N∆o pode ser maior que Quantidade da Nota...'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO SELF.
       RETURN NO-APPLY.
    END.

    ASSIGN tt-itens-sel.vlr-tot:SCREEN-VALUE IN BROWSE br-item-sel = STRING(INPUT BROWSE br-item-sel tt-itens-sel.qtde-item * INPUT BROWSE br-item-sel tt-itens-sel.vlr-unit).
END.

ON 'leave':U OF tt-itens-sel.vlr-unit IN BROWSE br-item-sel DO:
    ASSIGN tt-itens-sel.vlr-tot:SCREEN-VALUE IN BROWSE br-item-sel = STRING(INPUT BROWSE br-item-sel tt-itens-sel.qtde-item * INPUT BROWSE br-item-sel tt-itens-sel.vlr-unit).
    APPLY "VALUE-CHANGED" TO br-item-sel IN FRAME {&FRAME-NAME}.
END.

fi-cod-cliente:LOAD-MOUSE-POINTER("image/lupa.cur").
fi-nf-origem:LOAD-MOUSE-POINTER("image/lupa.cur").
    
tt-itens-sel.it-codigo:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE br-item-sel.
tt-itens-sel.cod-refer:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE br-item-sel.



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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN local-initialize-field.
  
  ENABLE {&list-2} WITH FRAME {&FRAME-NAME}.

  APPLY "entry " TO fi-cod-cliente IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize-field B-table-Win 
PROCEDURE local-initialize-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <NOne>
  NOtes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-cod-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nome-emit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    ASSIGN fi-serie-propria:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        
    ASSIGN fi-serie-propria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nat-oper-propria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nf-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""       
           fi-serie-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nat-oper-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME} =  NO
           bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME} =  NO.
        
    FIND FIRST para-fat NO-LOCK NO-ERROR.
    FIND FIRST nota-fiscal NO-LOCK NO-ERROR.

    ASSIGN fi-cod-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = nota-fiscal.cod-estabel
           fi-serie-origem:SCREEN-VALUE IN FRAME {&FRAME-NAME} = para-fat.serie-pad
           fi-nf-origem:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-nat-oper:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    ASSIGN tg-snfd:SCREEN-VALUE = 'NO'.

    APPLY 'LEAVE' TO fi-cod-estab.

    ASSIGN fi-qtd-tot-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-vlr-tot-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-qtd-tot-sel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-vlr-tot-sel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-arquivo-entrada:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           gr-nota-fiscal = ?.

    FOR EACH tt-itens-nf NO-LOCK.
        DELETE tt-itens-nf.
    END.
    FOR EACH tt-itens-sel NO-LOCK.
        DELETE tt-itens-sel.
    END.

    {&OPEN-QUERY-br-item-nf}
    {&OPEN-QUERY-br-item-sel}

    APPLY "entry " TO fi-cod-cliente.
    RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-add-item B-table-Win 
PROCEDURE pi-add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    ASSIGN i-ult-seq = 10.
    IF NUM-RESULTS("br-item-sel") > 0 THEN DO.
       br-item-sel:QUERY:GET-LAST() IN FRAME {&FRAME-NAME}.
       br-item-sel:QUERY:REPOSITION-TO-ROWID(ROWID(tt-itens-sel)).
       br-item-sel:SELECT-FOCUSED-ROW().

       ASSIGN i-resto = 10 - (tt-itens-sel.nr-seq-ped MODULO 10).

       IF i-resto <> 10 THEN
          ASSIGN i-ult-seq = tt-itens-sel.nr-seq-ped + i-resto.
       ELSE
          ASSIGN i-ult-seq = tt-itens-sel.nr-seq-ped + 10.
    END.
    br-item-sel:INSERT-ROW("after":U) IN FRAME {&FRAME-NAME}.

    ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-deleta-processo B-table-Win 
PROCEDURE pi-deleta-processo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH tt-itens-nf NO-LOCK.
        DELETE tt-itens-nf.
    END.
    
    FOR EACH tt-itens-sel NO-LOCK.
        DELETE tt-itens-sel.
    END.

    {&OPEN-QUERY-br-item-nf}
    {&OPEN-QUERY-br-item-sel}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-itens-devol B-table-Win 
PROCEDURE pi-itens-devol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR c-docto AS CHAR NO-UNDO.
   DEF VAR c-nr-ped AS CHARACTER   NO-UNDO.
   DEF VAR c-result        AS CHAR.
   DEF VAR i-cont          AS INT.

   RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
   {utp/ut-liter.i Calculando_Estoque *}
   RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

   EMPTY TEMP-TABLE tt-itens-sel.
   EMPTY TEMP-TABLE tt-aux.
   EMPTY TEMP-TABLE tt-etq-dev.

   ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-estab 
                                    fi-arquivo-entrada.

   INPUT FROM VALUE(fi-arquivo-entrada).
   REPEAT.
      CREATE tt-aux.
      IMPORT DELIMITER ":&*@" tt-aux.
   END.
   
   FOR EACH tt-aux WHERE
            tt-aux.c-linha <> "" NO-LOCK.

       RUN pi-acompanhar IN h-acomp (INPUT tt-aux.c-linha).
   
       FIND ob-etiqueta WHERE
            ob-etiqueta.cod-estabel  = fi-cod-estab AND
            ob-etiqueta.num-etiqueta = INT(SUBSTR(tt-aux.c-linha,7,9)) 
            USE-INDEX indice4 NO-LOCK NO-ERROR.
       
       IF NOT AVAIL ob-etiqueta THEN DO.
          MESSAGE 'Etiqueta ' SUBSTR(tt-aux.c-linha,7,9) ' Ser† desconsiderada pois n∆o est† Cadastrada...' 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT.
       END.

       IF ob-etiqueta.situacao <> 5 THEN DO.
          MESSAGE 'Etiqueta ' ob-etiqueta.num-etiqueta ' Ser† desconsiderada pois n∆o foi Faturada' 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT.
       END.

       FIND ped-item-rom WHERE
            ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
            ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
        
       IF NOT AVAIL ped-item-rom THEN DO:
          MESSAGE 'Etiqueta ' ob-etiqueta.num-etiqueta ' Ser† desconsiderada pois n∆o foi Encontrado nos Romaneios' 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT.
       END.

       FIND ped-item WHERE
            ped-item.nr-pedcli = ped-item-rom.nr-pedcli   AND
            ped-item.nome-abrev = ped-item-rom.nome-abrev AND
            ped-item.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.

       ASSIGN c-nr-ped = ped-item-rom.nr-pedcli.
       FIND ped-item-res OF ped-item NO-LOCK NO-ERROR.

       IF NOT AVAIL ped-item-res THEN DO.
          MESSAGE 'Etiqueta ' ob-etiqueta.num-etiqueta ' Ser† desconsiderada pois n∆o foi Encontrado Reserva do Pedido' 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT.
       END.

       ASSIGN c-docto = STRING(ped-item-res.nr-nota-fis, "9999999")
              i-nr-seq-ped = ped-item-rom.nr-sequencia.
       
       IF fi-nf-origem <> "" AND
          c-docto <> fi-nf-origem THEN DO.
          MESSAGE 'Etiqueta ' ob-etiqueta.num-etiqueta ' Ser† desconsiderada pois foi Faturada para Outra NF' 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          NEXT.
       END.


       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = ped-item-res.cod-estabel AND
            nota-fiscal.serie       = ped-item-res.serie       AND
            nota-fiscal.nr-nota-fis = c-docto NO-LOCK NO-ERROR.

       FIND it-nota-fisc OF nota-fiscal WHERE
            it-nota-fisc.nr-seq-ped = ped-item.nr-sequencia NO-LOCK NO-ERROR.
           
       FIND ITEM WHERE
            ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

       FIND FIRST tt-itens-sel WHERE
                  tt-itens-sel.nr-nota-fis = c-docto               AND
                  tt-itens-sel.it-codigo   = ob-etiqueta.it-codigo AND
                  tt-itens-sel.cod-refer   = ob-etiqueta.cod-refer AND
                  tt-itens-sel.nr-lote     = ob-etiqueta.nr-lote   AND
                  tt-itens-sel.vlr-unit    = it-nota-fisc.vl-preuni NO-LOCK NO-ERROR.

       IF NOT AVAIL tt-itens-sel THEN DO.
          CREATE tt-itens-sel.
          ASSIGN tt-itens-sel.dt-emis      = nota-fiscal.dt-emis
                 tt-itens-sel.nr-nota-fis  = c-docto
                 tt-itens-sel.nr-pedcli    = c-nr-ped
                 tt-itens-sel.nome-abrev   = nota-fiscal.nome-ab-cli
                 tt-itens-sel.it-codigo    = ob-etiqueta.it-codigo
                 tt-itens-sel.cod-refer    = ob-etiqueta.cod-refer
                 tt-itens-sel.nr-lote      = ob-etiqueta.nr-lote
                 tt-itens-sel.vlr-unit     = it-nota-fisc.vl-preuni
                 tt-itens-sel.descricao    = ITEM.desc-item.
       END.
       ASSIGN tt-itens-sel.vlr-total = tt-itens-sel.vlr-total + (it-nota-fisc.vl-preuni * ob-etiqueta.quantidade) 
              tt-itens-sel.qtde-item = tt-itens-sel.qtde-item + ob-etiqueta.quantidade.

       CREATE tt-etq-dev.
       BUFFER-COPY ob-etiqueta TO tt-etq-dev
            ASSIGN tt-etq-dev.nr-nota-fis = nota-fiscal.nr-nota-fis
                   tt-etq-dev.it-codigo = ob-etiqueta.it-codigo
                   tt-etq-dev.cod-refer = ob-etiqueta.cod-refer
                   tt-etq-dev.nr-lote = ob-etiqueta.nr-lote.
    END.
    
    RUN pi-finalizar in h-acomp.

    {&OPEN-QUERY-br-item-sel}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-itens-nf B-table-Win 
PROCEDURE pi-itens-nf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE tt-itens-nf.
    
    ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-right:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           bt-left:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        CREATE tt-itens-nf.
        ASSIGN tt-itens-nf.it-codigo    = it-nota-fisc.it-codigo    
               tt-itens-nf.cod-refer    = it-nota-fisc.cod-refer
               tt-itens-nf.nr-nota-fis  = it-nota-fisc.nr-nota-fis
               tt-itens-nf.nome-abrev   = nota-fiscal.nome-ab-cli
               tt-itens-nf.dt-emis      = it-nota-fisc.dt-emis-nota
               tt-itens-nf.nr-pedcli    = it-nota-fisc.nr-pedcli
               tt-itens-nf.nr-seq-ped   = it-nota-fisc.nr-seq-ped.   
    
        FIND ITEM WHERE
             ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
           ASSIGN tt-itens-nf.descricao = item.desc-item.
    
        FIND fat-ser-lote OF it-nota-fisc NO-LOCK NO-ERROR.
    
        IF AVAIL fat-ser-lote THEN
           ASSIGN tt-itens-nf.nr-lote = substring(fat-ser-lote.nr-serlote,1,2).
    
        ASSIGN tt-itens-nf.qtde-item = it-nota-fisc.qt-faturada[2]    
               tt-itens-nf.vlr-unit  = it-nota-fisc.vl-preuni    
               tt-itens-nf.vlr-total = it-nota-fisc.vl-tot-item.


        IF tg-nf-propria:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "yes" THEN DO.
           CREATE tt-itens-sel.
           ASSIGN tt-itens-sel.it-codigo   = tt-itens-nf.it-codigo
                  tt-itens-sel.cod-refer   = tt-itens-nf.cod-refer
                  tt-itens-sel.nr-lote     = tt-itens-nf.nr-lote
                  tt-itens-sel.qtde-item   = tt-itens-nf.qtde-item
                  tt-itens-sel.vlr-unit    = tt-itens-nf.vlr-unit 
                  tt-itens-sel.vlr-total   = tt-itens-nf.vlr-total
                  tt-itens-sel.descricao   = tt-itens-nf.descricao
                  tt-itens-sel.nr-nota-fis = tt-itens-nf.nr-nota-fis 
                  tt-itens-sel.nome-abrev  = tt-itens-nf.nome-abrev 
                  tt-itens-sel.dt-emis     = tt-itens-nf.dt-emis
                  tt-itens-sel.nr-pedcli   = tt-itens-nf.nr-pedcli
                  tt-itens-sel.nr-seq-ped  = tt-itens-nf.nr-seq-ped.
                   
           ASSIGN tt-itens-nf.acao = "sel".

           ASSIGN bt-add-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                  bt-mod-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                  bt-exc-item:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                  bt-right:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                  bt-left:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
        END.
    END.

    {&OPEN-QUERY-br-item-nf}
    {&OPEN-QUERY-br-item-sel}
    
    APPLY 'value-changed' TO br-item-nf IN FRAME {&FRAME-NAME}.
    APPLY 'entry' TO br-item-nf IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais-nf B-table-Win 
PROCEDURE pi-totais-nf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN de-qtd-tot-nf = 0
           de-vlr-tot-nf = 0.
    
    FOR EACH tt-itens-nf NO-LOCK.
        ASSIGN de-qtd-tot-nf = de-qtd-tot-nf + tt-itens-nf.qtde-item
               de-vlr-tot-nf = de-vlr-tot-nf + tt-itens-nf.vlr-total.
    END.
    
    ASSIGN fi-qtd-tot-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-qtd-tot-nf) 
           fi-vlr-tot-nf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-vlr-tot-nf).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais-sel B-table-Win 
PROCEDURE pi-totais-sel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN de-qtd-tot-sel = 0
           de-vlr-tot-sel = 0.
    
    FOR EACH tt-itens-sel NO-LOCK.
        ASSIGN de-qtd-tot-sel = de-qtd-tot-sel + tt-itens-sel.qtde-item
               de-vlr-tot-sel = de-vlr-tot-sel + tt-itens-sel.vlr-total.
    END.

    ASSIGN fi-qtd-tot-sel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING (de-qtd-tot-sel) 
           fi-vlr-tot-sel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING (de-vlr-tot-sel).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate B-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF tg-dev-cli THEN DO.
      IF fi-nf-cli = "" THEN DO.
         MESSAGE "Favor Informar a Nota Fiscal do Cliente"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO fi-nf-cli IN FRAME {&FRAME-NAME}.
         RETURN 'ADM-ERROR'.
      END.

      FIND natur-oper WHERE 
           natur-oper.nat-operacao = fi-nat-oper-cli NO-LOCK NO-ERROR.
      IF NOT AVAIL natur-oper THEN DO.
         MESSAGE "Natureza de Operaá∆o Inv†lida..."
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO fi-nat-oper-cli.
         RETURN 'ADM-ERROR'.
      END.
   END.

   IF tg-nf-propria THEN DO.
      IF fi-serie-propria = "" THEN DO.
         MESSAGE "Favor Informar a Serie Propria"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO fi-serie-propria IN FRAME {&FRAME-NAME}.
         RETURN 'ADM-ERROR'.
      END.

      FIND natur-oper WHERE 
           natur-oper.nat-operacao = fi-nat-oper-propria NO-LOCK NO-ERROR.
      IF NOT AVAIL natur-oper THEN DO.
         MESSAGE "Natureza de Operaá∆o Inv†lida..."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO fi-nat-oper-propria.
         RETURN 'ADM-ERROR'.
      END.
   END.

   IF tg-snfd = NO THEN DO.
      IF fi-nf-origem = "" THEN DO.
         MESSAGE "Favor Informar a Nota Fiscal de Origem"
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "entry" TO fi-nf-origem IN FRAME {&FRAME-NAME}.
         RETURN 'ADM-ERROR'.
      END.

      FIND nota-fiscal WHERE
           nota-fiscal.cod-estabel = fi-cod-estab AND
           nota-fiscal.serie = fi-serie-origem AND
           nota-fiscal.nr-nota-fis = fi-nf-origem
           NO-LOCK NO-ERROR.

      IF NOT AVAIL nota-fiscal THEN DO.
         MESSAGE 'Nota Fiscal N«O Encontrada no Sistema....'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO fi-nf-origem.
         RETURN 'ADM-ERROR'.
      END.

      IF nota-fiscal.nome-ab-cli <> fi-cod-cliente THEN DO.
         MESSAGE 'Nota Fiscal n∆o Pertente ao Cliente Informado....'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO fi-nf-origem.
         RETURN 'ADM-ERROR'.
      END.
   END.

   FIND FIRST tt-itens-sel NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-itens-sel THEN DO.
      MESSAGE "Selecione os itens da NF de Faturamento..."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN 'ADM-ERROR'.
   END.

   IF tg-snfd = YES AND
      fi-arquivo-entrada = "" THEN DO.
      MESSAGE "Arquivo de Dados com Etiquetas Devolvidas n∆o Selecionado..." 
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO fi-arquivo-entrada.
      RETURN 'ADM-ERROR'.
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
  {src/adm/template/sndkycas.i "nr-pedido" "it-nota-fisc" "nr-pedido"}
  {src/adm/template/sndkycas.i "it-codigo" "it-nota-fisc" "it-codigo"}
  {src/adm/template/sndkycas.i "nr-ord-produ" "it-nota-fisc" "nr-ord-produ"}
  {src/adm/template/sndkycas.i "cod-refer" "it-nota-fisc" "cod-refer"}

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
  {src/adm/template/snd-list.i "tt-itens-sel"}
  {src/adm/template/snd-list.i "tt-itens-nf"}

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

