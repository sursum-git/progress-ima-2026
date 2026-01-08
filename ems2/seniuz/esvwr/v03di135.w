&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i V03di135 9.99.99.999}

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
DEF VAR c-dia AS CHAR.

DEF NEW GLOBAL SHARED VAR h-essp0174 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-b03di135 AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-109 IMAGE-110 IMAGE-111 IMAGE-113 ~
IMAGE-114 IMAGE-115 IMAGE-120 IMAGE-121 IMAGE-122 IMAGE-123 rt-key ~
IMAGE-124 IMAGE-125 fi-dt-limite fi-cod-estabel-ini fi-cod-estabel-fin ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
fi-nome-abrev-ini fi-nome-abrev-fin fi-no-ab-reppri-ini fi-no-ab-reppri-fin ~
fi-corte-comerc-ini fi-corte-comerc-fin rs-tp-relat cb-tp-pedido ~
tg-lote-todos tg-lote-pp tg-lote-pd tg-lote-rp tg-lote-rd tg-lote-sc ~
tg-lote-ca rs-opc-artigo rs-tp-merc bt-confirma 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-limite fi-cod-estabel-ini ~
fi-cod-estabel-fin fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin fi-nome-abrev-ini fi-nome-abrev-fin fi-no-ab-reppri-ini ~
fi-no-ab-reppri-fin fi-corte-comerc-ini fi-corte-comerc-fin rs-tp-relat ~
cb-tp-pedido tg-lote-todos tg-lote-pp tg-lote-pd tg-lote-rp tg-lote-rd ~
tg-lote-sc tg-lote-ca rs-opc-artigo rs-tp-merc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-5 fi-nome-abrev-ini fi-nome-abrev-fin ~
fi-no-ab-reppri-ini fi-no-ab-reppri-fin 

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
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/imt-gerar.bmp":U
     LABEL "Gerar o Relat¢rio" 
     SIZE 44 BY 1.75.

DEFINE VARIABLE cb-tp-pedido AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Tipo de Pedido" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Todos","Normal","Exportaá∆o","Reserva","Amostra","∑ Vista","Operaá∆o Triangular","Bonificaá∆o","Doaá∆o","Bancado","Refaturamento","Amostra Exportaá∆o","Rem.Industrializaá∆o","Venda Confec." 
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-fin AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ini AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-fin AS CHARACTER FORMAT "!" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-ini AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-limite AS CHARACTER FORMAT "99/9999":U 
     LABEL "Data Limite" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-fin AS CHARACTER FORMAT "x(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Representante Final" NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Representante":R28 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "Representante Inicial" NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-fin AS CHARACTER FORMAT "X(12)" INITIAL "ZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "C¢digo do Cliente Final" NO-UNDO.

DEFINE VARIABLE fi-nome-abrev-ini AS CHARACTER FORMAT "X(12)" 
     LABEL "Cliente":R8 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 TOOLTIP "C¢digo do Cliente Inicial" NO-UNDO.

DEFINE IMAGE IMAGE-109
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-110
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-111
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-113
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-114
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-115
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-120
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-121
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-122
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-123
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-124
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-125
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 41.86 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tp-merc AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Interno", "I",
"Externo", "E",
"Ambos", "A"
     SIZE 41.29 BY .88 NO-UNDO.

DEFINE VARIABLE rs-tp-relat AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Mensal", 1,
"Anual", 2
     SIZE 41.29 BY .88
     FGCOLOR 4 FONT 11 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97.43 BY 15.5.

DEFINE VARIABLE tg-lote-ca AS LOGICAL INITIAL no 
     LABEL "CA" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote (Corte de Amostra)." NO-UNDO.

DEFINE VARIABLE tg-lote-pd AS LOGICAL INITIAL no 
     LABEL "PD" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote PD (Peáa Defeituosa)" NO-UNDO.

DEFINE VARIABLE tg-lote-pp AS LOGICAL INITIAL no 
     LABEL "PP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote PP (Peáa Perfeita)." NO-UNDO.

DEFINE VARIABLE tg-lote-rd AS LOGICAL INITIAL no 
     LABEL "RD" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RD (Rolo Defeituoso)." NO-UNDO.

DEFINE VARIABLE tg-lote-rp AS LOGICAL INITIAL no 
     LABEL "RP" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote RP (Rolo Perfeito)." NO-UNDO.

DEFINE VARIABLE tg-lote-sc AS LOGICAL INITIAL no 
     LABEL "SC" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.57 BY .88 TOOLTIP "Apenas o lote SC (Saco)." NO-UNDO.

DEFINE VARIABLE tg-lote-todos AS LOGICAL INITIAL yes 
     LABEL "TODOS" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .88 TOOLTIP "Todas as qualidades (PP, PD, RP e RD)." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-dt-limite AT ROW 1.42 COL 17 COLON-ALIGNED
     fi-cod-estabel-ini AT ROW 2.42 COL 17 COLON-ALIGNED WIDGET-ID 2
     fi-cod-estabel-fin AT ROW 2.42 COL 59.86 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi-it-codigo-ini AT ROW 3.42 COL 17 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 3.42 COL 59.86 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 4.42 COL 17 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 4.42 COL 59.86 COLON-ALIGNED NO-LABEL
     fi-nome-abrev-ini AT ROW 5.42 COL 17 COLON-ALIGNED
     fi-nome-abrev-fin AT ROW 5.42 COL 59.86 COLON-ALIGNED NO-LABEL
     fi-no-ab-reppri-ini AT ROW 6.42 COL 17 COLON-ALIGNED HELP
          "Nome abreviado do representante principal"
     fi-no-ab-reppri-fin AT ROW 6.42 COL 59.86 COLON-ALIGNED HELP
          "Nome abreviado do representante principal" NO-LABEL
     fi-corte-comerc-ini AT ROW 7.42 COL 17 COLON-ALIGNED
     fi-corte-comerc-fin AT ROW 7.42 COL 59.86 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     rs-tp-relat AT ROW 8.63 COL 59.15 RIGHT-ALIGNED NO-LABEL
     cb-tp-pedido AT ROW 9.83 COL 17 COLON-ALIGNED
     tg-lote-todos AT ROW 10.88 COL 18.86
     tg-lote-pp AT ROW 10.88 COL 32.43
     tg-lote-pd AT ROW 10.88 COL 39.57
     tg-lote-rp AT ROW 10.88 COL 46.57
     tg-lote-rd AT ROW 10.88 COL 53.72
     tg-lote-sc AT ROW 10.88 COL 60.72
     tg-lote-ca AT ROW 10.88 COL 68.29
     rs-opc-artigo AT ROW 12 COL 18.72 NO-LABEL
     rs-tp-merc AT ROW 13 COL 59.01 RIGHT-ALIGNED NO-LABEL
     bt-confirma AT ROW 14.21 COL 31.43
     "Lote:" VIEW-AS TEXT
          SIZE 3.57 BY .54 AT ROW 11.04 COL 15.14
     "Tipo Relat¢rio:" VIEW-AS TEXT
          SIZE 10.57 BY .75 AT ROW 8.71 COL 8.86
     "Mercado:" VIEW-AS TEXT
          SIZE 6.86 BY .75 AT ROW 13.04 COL 11.86
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10 BY .75 AT ROW 12.08 COL 8.57
     IMAGE-109 AT ROW 5.42 COL 35.57
     IMAGE-110 AT ROW 4.42 COL 35.57
     IMAGE-111 AT ROW 3.42 COL 35.57
     IMAGE-113 AT ROW 5.42 COL 58.43
     IMAGE-114 AT ROW 4.42 COL 58.43
     IMAGE-115 AT ROW 3.42 COL 58.43
     IMAGE-120 AT ROW 6.42 COL 35.57
     IMAGE-121 AT ROW 6.42 COL 58.43
     IMAGE-122 AT ROW 7.42 COL 58.43
     IMAGE-123 AT ROW 7.42 COL 35.57
     rt-key AT ROW 1 COL 1.57
     IMAGE-124 AT ROW 2.42 COL 35.57 WIDGET-ID 10
     IMAGE-125 AT ROW 2.42 COL 58.43 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
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
         HEIGHT             = 15.63
         WIDTH              = 98.29.
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

/* SETTINGS FOR FILL-IN fi-no-ab-reppri-fin IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-no-ab-reppri-ini IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-abrev-fin IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-abrev-ini IN FRAME f-main
   5                                                                    */
/* SETTINGS FOR RADIO-SET rs-tp-merc IN FRAME f-main
   ALIGN-R                                                              */
/* SETTINGS FOR RADIO-SET rs-tp-relat IN FRAME f-main
   ALIGN-R                                                              */
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

&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma V-table-Win
ON CHOOSE OF bt-confirma IN FRAME f-main /* Gerar o Relat¢rio */
DO:
  
  CASE rs-tp-merc:SCREEN-VALUE:
      WHEN "I" THEN DO:
          IF cb-tp-pedido:SCREEN-VALUE = "Exportaá∆o" THEN DO:
             MESSAGE "Tipo de Pedido Ç incompativel com o Mercado ! ! ! "
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO rs-tp-merc.
             RETURN 'ADM-ERROR':U.
          END.
      END.
      WHEN "E" THEN DO:
          IF cb-tp-pedido:SCREEN-VALUE <> "Todos" AND  cb-tp-pedido:SCREEN-VALUE <> "Exportaá∆o" THEN DO:
             MESSAGE "Tipo de Pedido Ç incompativel com o Mercado ! ! ! "
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'entry' TO rs-tp-merc.
             RETURN 'ADM-ERROR':U.
          END.
      END.
  END CASE.
  
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-limite  
         INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
         INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fin
         INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini
         INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin    
         INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini    
         INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin    
         INPUT FRAME {&FRAME-NAME} fi-nome-abrev-ini 
         INPUT FRAME {&FRAME-NAME} fi-nome-abrev-fin
         INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
         INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin
         INPUT FRAME {&FRAME-NAME} fi-corte-comerc-ini
         INPUT FRAME {&FRAME-NAME} fi-corte-comerc-fin
         INPUT FRAME {&FRAME-NAME} rs-tp-relat
         INPUT FRAME {&FRAME-NAME} cb-tp-pedido
         INPUT FRAME {&FRAME-NAME} tg-lote-todos
         INPUT FRAME {&FRAME-NAME} tg-lote-pp
         INPUT FRAME {&FRAME-NAME} tg-lote-pd
         INPUT FRAME {&FRAME-NAME} tg-lote-rp
         INPUT FRAME {&FRAME-NAME} tg-lote-rd
         INPUT FRAME {&FRAME-NAME} tg-lote-sc
         INPUT FRAME {&FRAME-NAME} tg-lote-ca
         INPUT FRAME {&FRAME-NAME} rs-tp-merc
         INPUT FRAME {&FRAME-NAME} rs-opc-artigo.
    
  RUN pi-select-page IN h-essp0174 (INPUT 2).

  RUN pi-processa IN h-b03di135 (INPUT fi-dt-limite, 
                                 INPUT fi-cod-estabel-ini,
                                 INPUT fi-cod-estabel-fin,
                                 INPUT fi-it-codigo-ini,    
                                 INPUT fi-it-codigo-fin,    
                                 INPUT fi-cod-refer-ini,    
                                 INPUT fi-cod-refer-fin,    
                                 INPUT fi-nome-abrev-ini,   
                                 INPUT fi-nome-abrev-fin,   
                                 INPUT fi-no-ab-reppri-ini, 
                                 INPUT fi-no-ab-reppri-fin,
                                 INPUT fi-corte-comerc-ini,    
                                 INPUT fi-corte-comerc-fin,
                                 INPUT rs-tp-relat,
                                 INPUT cb-tp-pedido,           
                                 INPUT tg-lote-todos,          
                                 INPUT tg-lote-pp,             
                                 INPUT tg-lote-pd,             
                                 INPUT tg-lote-rp,             
                                 INPUT tg-lote-rd,              
                                 INPUT tg-lote-sc,             
                                 INPUT tg-lote-ca,             
                                 INPUT rs-tp-merc,             
                                 INPUT rs-opc-artigo).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-fin V-table-Win
ON LEAVE OF fi-cod-estabel-fin IN FRAME f-main
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-fin
       NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE 'Estabelecimento n∆o Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel-fin IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad107.w
                     &campo=fi-cod-estabel-ini
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-ini V-table-Win
ON LEAVE OF fi-cod-estabel-ini IN FRAME f-main /* Estabelecimento */
DO:
  FIND estabelec WHERE
       estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ini
       NO-LOCK NO-ERROR.
  IF NOT AVAIL estabelec THEN DO.
     MESSAGE 'Estabelecimento n∆o Cadastrado....'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel-ini IN FRAME f-main /* Estabelecimento */
DO:
  {include/zoomvar.i &prog-zoom=adzoom\z01ad107.w
                     &campo=fi-cod-estabel-ini
                     &campozoom=cod-estabel}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-fin
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-refer-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini V-table-Win
ON LEAVE OF fi-cod-refer-ini IN FRAME f-main /* Referància */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-refer-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-refer-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-refer-ini IN FRAME f-main /* Referància */
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in377.w
                       &campo     = fi-cod-refer-ini
                       &campozoom = cod-refer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-fin
                       &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini V-table-Win
ON LEAVE OF fi-corte-comerc-ini IN FRAME f-main /* Corte Comercial */
DO:
  IF SELF:SCREEN-VALUE = '' THEN DO.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-corte-comerc-ini IN FRAME f-main /* Corte Comercial */
DO:
    {include/zoomvar.i &prog-zoom = eszoom/z01es065.w
                       &campo     = fi-corte-comerc-ini
                       &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-limite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite V-table-Win
ON LEAVE OF fi-dt-limite IN FRAME f-main /* Data Limite */
DO:
  IF INT(SUBSTR(SELF:SCREEN-VALUE,1,2)) <  1 OR
     INT(SUBSTR(SELF:SCREEN-VALUE,1,2)) > 12  THEN DO:
      MESSAGE "MES invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
  END.
  IF INT(SUBSTR(SELF:SCREEN-VALUE,4,4)) <  1 THEN DO:
      MESSAGE "ANO invalido ! ! !"  VIEW-AS ALERT-BOX. 
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-fin IN FRAME f-main
DO:
    {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                       &campo     = fi-it-codigo-fin
                       &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini V-table-Win
ON LEAVE OF fi-it-codigo-ini IN FRAME f-main /* Item */
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     SELF:SCREEN-VALUE <> '5' THEN
     ASSIGN fi-it-codigo-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-it-codigo-ini IN FRAME f-main /* Item */
DO:
  {include/zoomvar.i &prog-zoom = inzoom/z02in172.w
                     &campo     = fi-it-codigo-ini
                     &campozoom = it-codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-no-ab-reppri-fin IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-no-ab-reppri-fin
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-no-ab-reppri-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini V-table-Win
ON LEAVE OF fi-no-ab-reppri-ini IN FRAME f-main /* Representante */
DO:
   IF SELF:SCREEN-VALUE <> "" THEN DO:
       FIND repres WHERE 
            repres.nome-abrev = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini NO-LOCK NO-ERROR.
       IF NOT AVAIL repres THEN
          FIND repres WHERE 
               STRING(repres.cod-rep) = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini NO-LOCK NO-ERROR.

       IF AVAIL repres THEN
           ASSIGN fi-no-ab-reppri-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome-abrev
                  fi-no-ab-reppri-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome-abrev.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-no-ab-reppri-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-no-ab-reppri-ini IN FRAME f-main /* Representante */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z01ad229.w
                     &campo     = fi-no-ab-reppri-ini
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin V-table-Win
ON LEAVE OF fi-nome-abrev-fin IN FRAME f-main
DO:
  IF SELF:SCREEN-VALUE <> '' THEN DO:
     FIND emitente WHERE 
          emitente.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL emitente THEN
        FIND emitente WHERE 
             STRING(emitente.cod-emit) = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
     ELSE
         ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "ZZZZZZZZZZZZ".
  END.                                                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-fin V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nome-abrev-fin IN FRAME f-main
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                     &campo     = fi-nome-abrev-fin
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nome-abrev-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini V-table-Win
ON LEAVE OF fi-nome-abrev-ini IN FRAME f-main /* Cliente */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN DO:
     FIND emitente WHERE 
          emitente.nome-abrev = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAIL emitente THEN
        FIND emitente WHERE 
             STRING(emitente.cod-emit) = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAIL emitente THEN
         ASSIGN fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev
                fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev.
     ELSE
         ASSIGN fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nome-abrev-ini V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-nome-abrev-ini IN FRAME f-main /* Cliente */
DO:
  {include/zoomvar.i &prog-zoom = adzoom/z02ad098.w
                     &campo     = fi-nome-abrev-ini
                     &campozoom = nome-abrev}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tp-merc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-merc V-table-Win
ON VALUE-CHANGED OF rs-tp-merc IN FRAME f-main
DO:
  
  CASE SELF:SCREEN-VALUE:
      WHEN "I" THEN DO:
          IF cb-tp-pedido:SCREEN-VALUE = "Exportaá∆o" THEN DO:
             MESSAGE "Tipo de Pedido Ç incompativel com o Mercado ! ! ! "
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             ASSIGN SELF:SCREEN-VALUE = "A".
          END.
      END.
      WHEN "E" THEN DO:
          IF cb-tp-pedido:SCREEN-VALUE <> "Todos" AND  cb-tp-pedido:SCREEN-VALUE <> "Exportaá∆o" THEN DO:
             MESSAGE "Tipo de Pedido Ç incompativel com o Mercado ! ! ! "
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             ASSIGN SELF:SCREEN-VALUE = "A".
          END.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tp-relat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-relat V-table-Win
ON VALUE-CHANGED OF rs-tp-relat IN FRAME f-main
DO:
  
  CASE SELF:SCREEN-VALUE:
      WHEN "I" THEN DO:
          IF cb-tp-pedido:SCREEN-VALUE = "Exportaá∆o" THEN DO:
             MESSAGE "Tipo de Pedido Ç incompativel com o Mercado ! ! ! "
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             ASSIGN SELF:SCREEN-VALUE = "A".
          END.
      END.
      WHEN "E" THEN DO:
          IF cb-tp-pedido:SCREEN-VALUE <> "Todos" AND  cb-tp-pedido:SCREEN-VALUE <> "Exportaá∆o" THEN DO:
             MESSAGE "Tipo de Pedido Ç incompativel com o Mercado ! ! ! "
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             ASSIGN SELF:SCREEN-VALUE = "A".
          END.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-lote-todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-lote-todos V-table-Win
ON VALUE-CHANGED OF tg-lote-todos IN FRAME f-main /* TODOS */
DO:
  IF INPUT FRAME {&FRAME-NAME} tg-lote-todos = NO THEN
     ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'YES'.
  ELSE
     ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
            tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  fi-cod-estabel-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
  fi-cod-estabel-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
  fi-it-codigo-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
  fi-it-codigo-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
  fi-cod-refer-ini:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
  fi-cod-refer-fin:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.
  fi-nome-abrev-ini:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
  fi-nome-abrev-fin:LOAD-MOUSE-POINTER("image/lupa.cur")   IN FRAME {&FRAME-NAME}.
  fi-no-ab-reppri-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-no-ab-reppri-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-corte-comerc-ini:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-corte-comerc-fin:LOAD-MOUSE-POINTER("image/lupa.cur") IN FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY 'entry' TO fi-dt-limite IN FRAME {&FRAME-NAME}.

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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
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

  /* Code placed here will execute AFTER standard behavior.    */
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize-fields V-table-Win 
PROCEDURE local-initialize-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN fi-dt-limite:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = STRING(MONTH(TODAY),'99') +
                                                                   STRING(YEAR(TODAY),'9999')
         fi-cod-estabel-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = '1'
         fi-cod-estabel-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = '2'
         fi-it-codigo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-it-codigo-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZ'
         fi-cod-refer-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = ''
         fi-cod-refer-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = 'ZZZZZZZZ'
         fi-nome-abrev-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ''
         fi-nome-abrev-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = 'ZZZZZZZZZZZZ'
         fi-no-ab-reppri-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
         fi-no-ab-reppri-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'ZZZZZZZZZZZZ'
         fi-corte-comerc-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'A'
         fi-corte-comerc-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 'Z'
         rs-tp-relat:SCREEN-VALUE IN FRAME {&FRAME-NAME}         = '1'
         cb-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME}        = 'Todos'
         tg-lote-todos:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = 'YES'
         tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         tg-lote-pd:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         tg-lote-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         tg-lote-sc:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         tg-lote-ca:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'YES'
         rs-opc-artigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = 'A'.    
         rs-tp-merc:SCREEN-VALUE IN FRAME {&FRAME-NAME}          = 'A'.    

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
  Notes: N∆o fazer assign aqui. Nesta procedure
  devem ser colocadas apenas validaá‰es, pois neste ponto do programa o registro 
  ainda n∆o foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /*:T Validaá∆o de dicion†rio */
    
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

