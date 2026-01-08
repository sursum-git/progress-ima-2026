&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          ems2ima          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEF BUFFER moeda FOR mgadm.moeda.

DEFINE VARIABLE c-container AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ped-venda
&Scoped-define FIRST-EXTERNAL-TABLE ped-venda


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ped-venda.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ped-venda.nome-abrev-tri ~
ped-venda.no-ab-reppri ped-venda.nr-pedrep ped-venda.nat-operacao ~
ped-venda.nr-tabpre ped-venda.nr-tab-finan ped-venda.nr-ind-finan ~
ped-venda.mo-codigo ped-venda.nome-transp ped-venda.observacoes 
&Scoped-define ENABLED-TABLES ped-venda
&Scoped-define FIRST-ENABLED-TABLE ped-venda
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold bt-log 
&Scoped-Define DISPLAYED-FIELDS ped-venda.dt-implant ped-venda.nr-pedcli ~
ped-venda.nome-abrev ped-venda.nome-abrev-tri ped-venda.no-ab-reppri ~
ped-venda.nr-pedrep ped-venda.nat-operacao ped-venda.nr-tabpre ~
ped-venda.nr-tab-finan ped-venda.nr-ind-finan ped-venda.mo-codigo ~
ped-venda.nome-transp ped-venda.observacoes 
&Scoped-define DISPLAYED-TABLES ped-venda
&Scoped-define FIRST-DISPLAYED-TABLE ped-venda
&Scoped-Define DISPLAYED-OBJECTS cb-tp-pedido fi-reserva fi-nome-cli ~
FILL-IN-12 fi-nom-cli-tri fi-nome-rep fi-perc-comis fi-denominacao ~
fi-cod-cond-pag fi-desc-cond-pag COMBO-BOX-1 cb-tp-entrega fi-dt-entrega ~
fi-data-base fi-desc-moeda fi-nome-tr-red fi-tp-frete tg-etiqueta ~
tg-emb-neutra tg-fat-parcial 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS ped-venda.nome-abrev fi-cod-cond-pag 
&Scoped-define ADM-ASSIGN-FIELDS fi-cod-cond-pag 
&Scoped-define ADM-MODIFY-FIELDS fi-cod-cond-pag 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cond-esp 
     IMAGE-UP FILE "image/im-open.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-open.bmp":U
     LABEL "Button 2" 
     SIZE 4 BY 1 TOOLTIP "Condi‡äes Especiais de Pagto".

DEFINE BUTTON bt-log AUTO-GO 
     IMAGE-UP FILE "image/im-log.bmp":U
     LABEL "" 
     SIZE 4.29 BY 1.21 TOOLTIP "Altera‡äes do Pedido"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE cb-tp-entrega AS CHARACTER FORMAT "X(256)":U INITIAL "No Mˆs" 
     LABEL "Tipo Entrega" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "1¦ Quinzena","2¦ Quinzena","No Mˆs","Na Data","A Partir da Data","At‚ a Data","Imediata" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE cb-tp-pedido AS CHARACTER FORMAT "X(256)":U INITIAL "Normal" 
     LABEL "Tipo Pedido" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Normal","Exporta‡Æo","Reserva","Amostra","· Vista","Opera‡Æo Triangular","Bonifica‡Æo","Doa‡Æo","Bancado","Refaturamento","Amostra Exporta‡Æo" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Normal" 
     LABEL "Tipo Pagto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Normal ","Vendor" 
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fi-tp-frete AS CHARACTER FORMAT "X(256)":U INITIAL "Cif Total" 
     LABEL "Tipo de Frete" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Cif Total","Cif at‚ Redesp","Fob" 
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-cond-pag AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Cond. Pagto" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-data-base AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Base Fat." 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-denominacao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-cond-pag AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-moeda AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-entrega AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nom-cli-tri AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-cli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-rep AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-tr-red AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transp. Redesp." 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc-comis AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "%Comis" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-reserva AS CHARACTER FORMAT "X(256)":U 
     LABEL "Num. Reserva" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Ped. Cliente" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 3.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 7.42.

DEFINE VARIABLE tg-emb-neutra AS LOGICAL INITIAL no 
     LABEL "Embalag. Neutra" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .58 NO-UNDO.

DEFINE VARIABLE tg-etiqueta AS LOGICAL INITIAL no 
     LABEL "Etiqueta" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .58 NO-UNDO.

DEFINE VARIABLE tg-fat-parcial AS LOGICAL INITIAL yes 
     LABEL "Fat. Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .54 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     cb-tp-pedido AT ROW 1.13 COL 57.57 COLON-ALIGNED
     ped-venda.dt-implant AT ROW 1.17 COL 28.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-reserva AT ROW 1.17 COL 95 COLON-ALIGNED
     ped-venda.nr-pedcli AT ROW 1.96 COL 2 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1.5
          BGCOLOR 8 FONT 10
     ped-venda.nome-abrev AT ROW 2.17 COL 28.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-nome-cli AT ROW 2.17 COL 40.86 COLON-ALIGNED NO-LABEL
     FILL-IN-12 AT ROW 2.17 COL 95 COLON-ALIGNED
     bt-log AT ROW 2.29 COL 17.43
     ped-venda.nome-abrev-tri AT ROW 3.17 COL 28.57 COLON-ALIGNED
          LABEL "Cli Rem Tri."
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-nom-cli-tri AT ROW 3.17 COL 40.86 COLON-ALIGNED NO-LABEL
     ped-venda.no-ab-reppri AT ROW 4.5 COL 10.86 COLON-ALIGNED
          LABEL "Representante":R15
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     fi-nome-rep AT ROW 4.54 COL 23.29 COLON-ALIGNED NO-LABEL
     fi-perc-comis AT ROW 4.54 COL 65.72 COLON-ALIGNED
     ped-venda.nr-pedrep AT ROW 4.54 COL 90 COLON-ALIGNED
          LABEL "Ped. Repres.":R12
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ped-venda.nat-operacao AT ROW 5.5 COL 10.72 COLON-ALIGNED
          LABEL "Natur Oper":R11
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     fi-denominacao AT ROW 5.54 COL 20.29 COLON-ALIGNED NO-LABEL
     ped-venda.nr-tabpre AT ROW 5.54 COL 90 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     bt-cond-esp AT ROW 6.42 COL 17.86
     fi-cod-cond-pag AT ROW 6.5 COL 10.72 COLON-ALIGNED HELP
          "C¢digo da condi‡Æo de pagamento"
     fi-desc-cond-pag AT ROW 6.54 COL 20.29 COLON-ALIGNED NO-LABEL
     COMBO-BOX-1 AT ROW 6.54 COL 58.72 COLON-ALIGNED
     ped-venda.nr-tab-finan AT ROW 6.54 COL 90 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     ped-venda.nr-ind-finan AT ROW 6.54 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     cb-tp-entrega AT ROW 7.54 COL 10.72 COLON-ALIGNED
     fi-dt-entrega AT ROW 7.54 COL 25.72 COLON-ALIGNED NO-LABEL
     fi-data-base AT ROW 7.54 COL 58.72 COLON-ALIGNED
     ped-venda.mo-codigo AT ROW 7.54 COL 90 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-desc-moeda AT ROW 7.54 COL 94.43 COLON-ALIGNED NO-LABEL
     ped-venda.nome-transp AT ROW 8.54 COL 10.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY .88
     fi-nome-tr-red AT ROW 8.54 COL 58.72 COLON-ALIGNED
     fi-tp-frete AT ROW 8.54 COL 90 COLON-ALIGNED
     tg-etiqueta AT ROW 9.5 COL 92
     ped-venda.observacoes AT ROW 9.75 COL 12.72 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 61.29 BY 1.75
     tg-emb-neutra AT ROW 10.25 COL 92
     tg-fat-parcial AT ROW 11 COL 92
     "Observa‡Æo:" VIEW-AS TEXT
          SIZE 9 BY .54 AT ROW 9.54 COL 3.72
     "N§ Pedido" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1.33 COL 2.14
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 4.33 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgmov.ped-venda
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 10.88
         WIDTH              = 107.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{include/c-viewer.i}
{utp/ut-glob.i}
{include/i_dbtype.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-cond-esp IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-tp-entrega IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX cb-tp-pedido IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-venda.dt-implant IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-cond-pag IN FRAME f-main
   NO-ENABLE 1 2 3                                                      */
/* SETTINGS FOR FILL-IN fi-data-base IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-denominacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-cond-pag IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-moeda IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-entrega IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nom-cli-tri IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-cli IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-rep IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-tr-red IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-perc-comis IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-reserva IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX fi-tp-frete IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-12 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ped-venda.nat-operacao IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ped-venda.no-ab-reppri IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ped-venda.nome-abrev IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ped-venda.nome-abrev-tri IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ped-venda.nr-pedcli IN FRAME f-main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN ped-venda.nr-pedrep IN FRAME f-main
   EXP-LABEL                                                            */
ASSIGN 
       ped-venda.observacoes:READ-ONLY IN FRAME f-main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX tg-emb-neutra IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-etiqueta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-fat-parcial IN FRAME f-main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME f-main
/* Query rebuild information for FRAME f-main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME f-main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME bt-cond-esp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cond-esp V-table-Win
ON CHOOSE OF bt-cond-esp IN FRAME f-main /* Button 2 */
DO:
    /*
  RUN esp/espd4000a.p (INPUT-OUTPUT TABLE tt-cond-ped, INPUT fi-cod-cond-pag:INPUT-VALUE).

  FIND FIRST tt-cond-ped NO-LOCK NO-ERROR.
  IF AVAIL tt-cond-ped THEN DO.
     ASSIGN fi-cod-cond-pag:SCREEN-VALUE = '0' 
            fi-desc-cond-pag:SCREEN-VALUE = "E S P E C I A L".
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-log
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-log V-table-Win
ON CHOOSE OF bt-log IN FRAME f-main
DO:
   RUN esp/essp0155b.p (INPUT ped-venda.nr-pedcli:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tp-entrega
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-entrega V-table-Win
ON VALUE-CHANGED OF cb-tp-entrega IN FRAME f-main /* Tipo Entrega */
DO:
  ASSIGN fi-dt-entrega:SCREEN-VALUE = ?.
  IF SELF:SCREEN-VALUE = "Na Data" OR
     SELF:SCREEN-VALUE = "A Partir da Data" OR
     SELF:SCREEN-VALUE = "At‚ a Data" THEN
     ASSIGN fi-dt-entrega:SENSITIVE = YES.
  ELSE
     ASSIGN fi-dt-entrega:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tp-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-pedido V-table-Win
ON VALUE-CHANGED OF cb-tp-pedido IN FRAME f-main /* Tipo Pedido */
DO:
    /*
  ASSIGN fi-reserva:SENSITIVE = NO
         fi-cliente-tri:SENSITIVE = NO
         bt-imp-res:SENSITIVE = NO
         bt-cond-esp:SENSITIVE = YES
         fi-cod-cond-pag:SENSITIVE = YES
         fi-cod-cond-pag:SCREEN-VALUE  = ''
         fi-desc-cond-pag:SCREEN-VALUE = ''.

  CASE SELF:SCREEN-VALUE.
      WHEN "Reserva" THEN 
            ASSIGN fi-reserva:SENSITIVE = YES
                   bt-imp-res:SENSITIVE = YES.
      WHEN "Opera‡Æo Triangular" THEN 
            ASSIGN fi-cliente-tri:SENSITIVE = YES.
      WHEN "· Vista" THEN DO.
            ASSIGN bt-cond-esp:SENSITIVE = NO
                   fi-cod-cond-pag:SENSITIVE = NO.
            FIND cond-pagto WHERE
                 cond-pagto.cod-cond-pag = 1 NO-LOCK NO-ERROR.
            IF AVAIL cond-pagto THEN 
               ASSIGN fi-cod-cond-pag:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(cond-pagto.cod-cond-pag)
                      fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.
      END.
  END CASE.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-cond-pag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cond-pag V-table-Win
ON LEAVE OF fi-cod-cond-pag IN FRAME f-main /* Cond. Pagto */
DO:
    /*
   IF fi-cod-cond-pag <> fi-cod-cond-pag:INPUT-VALUE THEN DO. /* Alterou Condi‡Æo de Pagamento */
      FOR EACH tt-cond-ped.
          DELETE tt-cond-ped.
      END.
   END.
   */
   ASSIGN fi-desc-cond-pag:SCREEN-VALUE = ''
          ped-venda.nr-tab-finan:SCREEN-VALUE = ''
          ped-venda.nr-ind-finan:SCREEN-VALUE = ''.

   FIND cond-pagto WHERE
        cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag NO-LOCK NO-ERROR.
   IF AVAIL cond-pagto THEN
      ASSIGN bt-cond-esp:SENSITIVE = IF cond-pagto.cod-cond-pag <> 1 THEN YES ELSE NO
             fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao
             ped-venda.nr-tab-finan:SCREEN-VALUE = STRING(cond-pagto.nr-tab-finan)
             ped-venda.nr-ind-finan:SCREEN-VALUE = STRING(cond-pagto.nr-ind-finan).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cond-pag V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-cond-pag IN FRAME f-main /* Cond. Pagto */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad039.w
                     &campo     = fi-cod-cond-pag
                     &campozoom = cod-cond-pag}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-cond-pag V-table-Win
ON VALUE-CHANGED OF fi-cod-cond-pag IN FRAME f-main /* Cond. Pagto */
DO:
   ASSIGN bt-cond-esp:SENSITIVE = NO
          fi-desc-cond-pag:SCREEN-VALUE = ''.
   IF SELF:INPUT-VALUE <> 0 AND SELF:INPUT-VALUE <> 1 THEN DO.
      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = INPUT FRAME {&FRAME-NAME} fi-cod-cond-pag
           NO-LOCK NO-ERROR.
      IF AVAIL cond-pagto THEN 
         ASSIGN bt-cond-esp:SENSITIVE = YES
                fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-tr-red
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-tr-red V-table-Win
ON LEAVE OF fi-nome-tr-red IN FRAME f-main /* Transp. Redesp. */
DO:
  FIND transporte WHERE 
       transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-nome-tr-red NO-LOCK NO-ERROR.
  IF NOT AVAIL transporte THEN
     FIND transporte WHERE 
          STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} fi-nome-tr-red NO-LOCK NO-ERROR.

  IF AVAIL transporte THEN
     ASSIGN fi-nome-tr-red:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-tr-red V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nome-tr-red IN FRAME f-main /* Transp. Redesp. */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad268.w
                     &campo     = fi-nome-tr-red
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ped-venda.mo-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ped-venda.mo-codigo V-table-Win
ON ENTRY OF ped-venda.mo-codigo IN FRAME f-main /* Moeda */
DO:
   FIND moeda WHERE
        moeda.mo-codigo = INPUT FRAME {&FRAME-NAME} ped-venda.mo-codigo NO-LOCK NO-ERROR.
   IF AVAIL moeda THEN
      ASSIGN fi-desc-moeda:SCREEN-VALUE = moeda.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ped-venda.nat-operacao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ped-venda.nat-operacao V-table-Win
ON LEAVE OF ped-venda.nat-operacao IN FRAME f-main /* Natur Oper */
DO:
  ASSIGN fi-denominacao:SCREEN-VALUE = ''.

  FIND natur-oper WHERE
       natur-oper.nat-operacao = INPUT FRAME {&FRAME-NAME} ped-venda.nat-operacao NO-LOCK NO-ERROR.
  IF AVAIL natur-oper THEN
     ASSIGN fi-denominacao:SCREEN-VALUE = natur-oper.denominacao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ped-venda.nat-operacao V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ped-venda.nat-operacao IN FRAME f-main /* Natur Oper */
DO:
  {include/zoomvar.i &prog-zoom = inzoom\z01in245.w
                     &campo     = ped-venda.nat-operacao
                     &campozoom = nat-operacao}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ped-venda.no-ab-reppri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ped-venda.no-ab-reppri V-table-Win
ON LEAVE OF ped-venda.no-ab-reppri IN FRAME f-main /* Representante */
DO:
  ASSIGN fi-nome-rep:SCREEN-VALUE = ''.

  FIND repres WHERE
       repres.cod-rep = INTEGER(INPUT FRAME {&FRAME-NAME} ped-venda.no-ab-reppri) NO-LOCK NO-ERROR.
  IF NOT AVAIL repres THEN
     FIND repres WHERE
          repres.nome-abrev = INPUT FRAME {&FRAME-NAME} ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

  IF AVAIL repres THEN
     ASSIGN ped-venda.no-ab-reppri:SCREEN-VALUE = repres.nome-abrev
            fi-nome-rep:SCREEN-VALUE = repres.nome.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ped-venda.no-ab-reppri V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ped-venda.no-ab-reppri IN FRAME f-main /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad229.w
                     &campo     = ped-venda.no-ab-reppri
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ped-venda.nome-transp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ped-venda.nome-transp V-table-Win
ON LEAVE OF ped-venda.nome-transp IN FRAME f-main /* Transportador */
DO:
  FIND transporte WHERE 
       transporte.nome-abrev = INPUT FRAME {&FRAME-NAME} ped-venda.nome-transp NO-LOCK NO-ERROR.
  IF NOT AVAIL transporte THEN
     FIND transporte WHERE 
          STRING(transporte.cod-transp) = INPUT FRAME {&FRAME-NAME} ped-venda.nome-transp NO-LOCK NO-ERROR.

  IF AVAIL transporte THEN
     ASSIGN ped-venda.nome-transp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = transporte.nome-abrev.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ped-venda.nome-transp V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ped-venda.nome-transp IN FRAME f-main /* Transportador */
DO:
  {include/zoomvar.i &prog-zoom = adzoom\z01ad268.w
                     &campo     = ped-venda.nome-transp
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ped-venda.nr-tabpre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ped-venda.nr-tabpre V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ped-venda.nr-tabpre IN FRAME f-main /* Tab Pre‡os */
DO:
  {include/zoomvar.i &prog-zoom = dizoom\z01di189.w
                     &campo     = ped-venda.nr-tabpre
                     &campozoom = nr-tabpre}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "ped-venda"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ped-venda"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME f-main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {include/i-valid.i}
    
    /*:T Ponha na pi-validate todas as valida‡äes */
    /*:T NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Code placed here will execute PRIOR to standard behavior. */
    
   /* Dispatch standard ADM method.                             */
   RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
   IF AVAIL ped-venda THEN DO.
      FIND emitente WHERE 
           emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

      IF AVAIL emitente THEN 
         ASSIGN fi-nome-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.

      IF ped-venda.nome-abrev-tri <> '' THEN DO.
         FIND emitente WHERE 
              emitente.nome-abrev = ped-venda.nome-abrev-tri NO-LOCK NO-ERROR.

         IF AVAIL emitente THEN 
            ASSIGN fi-nom-cli-tri:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-emit.
      END.

      FIND repres WHERE
           repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

      IF AVAIL repres THEN
         ASSIGN fi-nome-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome.
      /*
      FIND FIRST ped-rep OF ped-venda NO-LOCK NO-ERROR.
      ASSIGN fi-perc-comis:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-rep.perc-comis).
      */

      FIND natur-oper WHERE
           natur-oper.nat-operacao = ped-venda.nat-operacao NO-LOCK NO-ERROR.
      IF AVAIL natur-oper THEN
         ASSIGN fi-denominacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = natur-oper.denominacao.
      /*
      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
      IF AVAIL cond-pagto THEN
         ASSIGN fi-desc-cond-pag:SCREEN-VALUE = cond-pagto.descricao.
      */

      ASSIGN cb-tp-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Na Data"
             fi-dt-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.dt-entrega).

      FIND moeda WHERE
           moeda.mo-codigo = ped-venda.mo-codigo NO-LOCK NO-ERROR.
      IF AVAIL moeda THEN
         ASSIGN fi-desc-moeda:SCREEN-VALUE = moeda.descricao.

      ASSIGN tg-fat-parcial:SCREEN-VALUE = STRING(ped-venda.ind-fat-par).

      RUN pi-popula-browse IN WIDGET-HANDLE(c-container) (INPUT(ROWID(ped-venda))).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    &if  defined(ADM-MODIFY-FIELDS) &then
    if adm-new-record = yes then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  RUN get-link-handle IN adm-broker-hdl ( INPUT THIS-PROCEDURE,
                                          INPUT "CONTAINER",
                                          OUTPUT c-container).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-parent V-table-Win 
PROCEDURE pi-atualiza-parent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define input parameter v-row-parent-externo as rowid no-undo.
    
    assign v-row-parent = v-row-parent-externo.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*:T------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Valida‡Æo de dicion rio */
    
/*:T    Segue um exemplo de valida‡Æo de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ped-venda"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

