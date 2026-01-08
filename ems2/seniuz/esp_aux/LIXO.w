&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0155A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite-ini     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-dt-limite-fin     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-ini     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-nr-pedcli-fin     LIKE ped-item-ext.nr-pedcli.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-ini     LIKE ped-item-ext.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-it-codigo-fin     LIKE ped-item-ext.it-codigo.
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-ini     LIKE ped-item-ext.cod-refer.                              
DEFINE INPUT-OUTPUT PARAMETER c-cod-refer-fin     LIKE ped-item-ext.cod-refer.
DEFINE INPUT-OUTPUT PARAMETER c-tp-pedido-ini     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-tp-pedido-fin     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER i-cod-emitente-ini  LIKE ped-venda.cod-emitente. 
DEFINE INPUT-OUTPUT PARAMETER i-cod-emitente-fin  LIKE ped-venda.cod-emitente.
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-ini  LIKE ped-venda.no-ab-reppri. 
DEFINE INPUT-OUTPUT PARAMETER c-no-ab-reppri-fin  LIKE ped-venda.no-ab-reppri.
DEFINE INPUT-OUTPUT PARAMETER c-cod-obsoleto-ini  LIKE ref-item-ext.cod-obsoleto. 
DEFINE INPUT-OUTPUT PARAMETER c-cod-obsoleto-fin  LIKE ref-item-ext.cod-obsoleto.
DEFINE INPUT-OUTPUT PARAMETER c-corte-comerc-ini  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT-OUTPUT PARAMETER c-corte-comerc-fin  LIKE ob-etiqueta.corte-comerc.
DEFINE INPUT-OUTPUT PARAMETER c-cod-restr-ini     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-cod-restr-fin     AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-cod-depos         AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER c-opc-artigo        AS CHAR.
DEFINE INPUT-OUTPUT PARAMETER l-sit-todas         AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-abe           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-atp           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-att           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-pen           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-sus           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-sit-can           AS LOG.
DEFINE INPUT-OUTPUT PARAMETER l-ok                AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-nr-pedcli-ini fi-nr-pedcli-fin ~
fi-it-codigo-ini fi-it-codigo-fin tg-sit-abe tg-sit-sus tg-sit-atp ~
tg-sit-can tg-sit-att tg-sit-pen bt-ok bt-cancela bt-ajuda tg-sit-todas ~
fi-dt-limite-ini fi-dt-limite-fin fi-cod-refer-ini fi-cod-refer-fin ~
fi-tp-pedido-ini fi-tp-pedido-fin fi-cod-emitente-ini fi-cod-emitente-fin ~
rs-opc-artigo fi-no-ab-reppri-ini fi-no-ab-reppri-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin fi-corte-comerc-ini fi-corte-comerc-fin fi-cod-depos ~
fi-cod-restr-ini fi-cod-restr-fin IMAGE-3 IMAGE-4 IMAGE-5 IMAGE-6 IMAGE-77 ~
IMAGE-78 IMAGE-83 IMAGE-87 IMAGE-88 IMAGE-89 IMAGE-90 IMAGE-91 IMAGE-92 ~
IMAGE-93 IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 IMAGE-98 IMAGE-99 RECT-50 ~
rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-pedcli-ini fi-nr-pedcli-fin ~
fi-it-codigo-ini fi-it-codigo-fin tg-sit-abe tg-sit-sus tg-sit-atp ~
tg-sit-can tg-sit-att tg-sit-pen tg-sit-todas fi-dt-limite-ini ~
fi-dt-limite-fin fi-cod-refer-ini fi-cod-refer-fin fi-tp-pedido-ini ~
fi-tp-pedido-fin fi-cod-emitente-ini fi-cod-emitente-fin rs-opc-artigo ~
fi-no-ab-reppri-ini fi-no-ab-reppri-fin fi-cod-obsoleto-ini ~
fi-cod-obsoleto-fin fi-corte-comerc-ini fi-corte-comerc-fin fi-cod-depos ~
fi-cod-restr-ini fi-cod-restr-fin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-cod-depos AS CHARACTER FORMAT "x(3)" 
     LABEL "Dep¢sito":R10 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-emitente-fin AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "C¢digo do emitente final." NO-UNDO.

DEFINE VARIABLE fi-cod-emitente-ini AS INTEGER FORMAT ">>>>>>>>9" INITIAL 0 
     LABEL "Cliente":R9 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 TOOLTIP "C¢digo do emitente inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-fin AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo obsoleto final." NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto-ini AS CHARACTER FORMAT "x(1)" 
     LABEL "Cod.Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "C¢digo obsoleto inicial." NO-UNDO.

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Referˆncia" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-restr-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-restr-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Restri‡äes" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-fin AS CHARACTER FORMAT "!" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial final." NO-UNDO.

DEFINE VARIABLE fi-corte-comerc-ini AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 TOOLTIP "Corte comercial inicial." NO-UNDO.

DEFINE VARIABLE fi-dt-limite-fin AS CHARACTER FORMAT "99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-limite-ini AS CHARACTER FORMAT "99/9999":U 
     LABEL "Data Limite" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-no-ab-reppri-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-fin AS CHARACTER FORMAT "x(12)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli-ini AS CHARACTER FORMAT "x(12)" 
     LABEL "Pedido":R17 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido-fin AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido-ini AS CHARACTER FORMAT "x(16)" 
     LABEL "Tipo de Pedido" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

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

DEFINE IMAGE IMAGE-77
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-78
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-83
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-87
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE IMAGE IMAGE-88
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-89
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-90
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-91
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-92
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-93
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-94
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-95
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-96
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-97
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-98
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-99
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE VARIABLE rs-opc-artigo AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Indigo", "I",
"Outros", "O",
"Ambos", "A"
     SIZE 36.29 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 74 BY 17.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 74 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tg-sit-abe AS LOGICAL INITIAL no 
     LABEL "Aberto" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-atp AS LOGICAL INITIAL no 
     LABEL "Atendido Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-att AS LOGICAL INITIAL no 
     LABEL "Atendido Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-can AS LOGICAL INITIAL no 
     LABEL "Cancelado" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-pen AS LOGICAL INITIAL no 
     LABEL "Pendente" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-sus AS LOGICAL INITIAL no 
     LABEL "Suspenso" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE tg-sit-todas AS LOGICAL INITIAL yes 
     LABEL "TODAS SITUA€åES" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 TOOLTIP "Todas as qualidades (PP, PD, RP e RD)." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-nr-pedcli-ini AT ROW 2.75 COL 14 COLON-ALIGNED HELP
          "N£mero do pedido do cliente"
     fi-nr-pedcli-fin AT ROW 2.75 COL 42 COLON-ALIGNED HELP
          "N£mero do pedido do cliente" NO-LABEL
     fi-it-codigo-ini AT ROW 3.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-it-codigo-fin AT ROW 3.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     tg-sit-abe AT ROW 16.17 COL 4
     tg-sit-sus AT ROW 17.17 COL 4
     tg-sit-atp AT ROW 16.17 COL 27
     tg-sit-can AT ROW 17.17 COL 52
     tg-sit-att AT ROW 16.17 COL 52
     tg-sit-pen AT ROW 17.17 COL 27
     bt-ok AT ROW 18.75 COL 3
     bt-cancela AT ROW 18.75 COL 14
     bt-ajuda AT ROW 18.75 COL 64.86
     tg-sit-todas AT ROW 15.17 COL 4
     fi-dt-limite-ini AT ROW 1.75 COL 14 COLON-ALIGNED
     fi-dt-limite-fin AT ROW 1.75 COL 42 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 4.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-cod-refer-fin AT ROW 4.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-tp-pedido-ini AT ROW 5.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-tp-pedido-fin AT ROW 5.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-cod-emitente-ini AT ROW 6.75 COL 14 COLON-ALIGNED HELP
          "C¢digo do cliente"
     fi-cod-emitente-fin AT ROW 6.75 COL 42 COLON-ALIGNED HELP
          "C¢digo do cliente" NO-LABEL
     rs-opc-artigo AT ROW 12.88 COL 16.29 NO-LABEL
     fi-no-ab-reppri-ini AT ROW 7.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-no-ab-reppri-fin AT ROW 7.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     fi-cod-obsoleto-ini AT ROW 8.75 COL 14 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-cod-obsoleto-fin AT ROW 8.75 COL 42 COLON-ALIGNED HELP
          "Codigo obsoleto" NO-LABEL
     fi-corte-comerc-ini AT ROW 9.75 COL 14 COLON-ALIGNED
     fi-corte-comerc-fin AT ROW 9.75 COL 42 COLON-ALIGNED HELP
          "Corte Comercial" NO-LABEL
     fi-cod-depos AT ROW 11.75 COL 14 COLON-ALIGNED HELP
          "C¢digo do Dep¢sito"
     fi-cod-restr-ini AT ROW 10.75 COL 14 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS"
     fi-cod-restr-fin AT ROW 10.75 COL 42 COLON-ALIGNED HELP
          "Codigo do Ötem no EMS" NO-LABEL
     IMAGE-3 AT ROW 2.75 COL 32
     IMAGE-4 AT ROW 2.75 COL 40
     IMAGE-5 AT ROW 3.75 COL 32
     IMAGE-6 AT ROW 3.75 COL 40
     IMAGE-77 AT ROW 8.75 COL 32.14
     IMAGE-78 AT ROW 9.75 COL 32
     IMAGE-83 AT ROW 8.75 COL 40
     IMAGE-87 AT ROW 9.75 COL 40
     IMAGE-88 AT ROW 1.75 COL 32
     IMAGE-89 AT ROW 1.75 COL 40
     IMAGE-90 AT ROW 4.75 COL 32
     IMAGE-91 AT ROW 4.75 COL 40
     IMAGE-92 AT ROW 5.75 COL 32
     IMAGE-93 AT ROW 5.75 COL 40
     IMAGE-94 AT ROW 6.75 COL 32
     IMAGE-95 AT ROW 6.75 COL 40
     IMAGE-96 AT ROW 7.75 COL 32
     IMAGE-97 AT ROW 7.75 COL 40
     IMAGE-98 AT ROW 10.75 COL 32
     IMAGE-99 AT ROW 10.75 COL 40
     RECT-50 AT ROW 1.25 COL 2
     rt-buttom AT ROW 18.5 COL 2
     "Situa‡Æo dos Pedidos" VIEW-AS TEXT
          SIZE 70 BY .75 AT ROW 14.25 COL 4
          BGCOLOR 9 FGCOLOR 15 FONT 6
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         DEFAULT-BUTTON bt-ok.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME D-Dialog
     "Tipo de Artigo:" VIEW-AS TEXT
          SIZE 10.57 BY .75 AT ROW 12.88 COL 5.86
     " Sele‡Æo" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 1 COL 4.29
     SPACE(65.56) SKIP(18.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Sele‡Æo de Ötens de Pedidos"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   Custom                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Sele‡Æo de Ötens de Pedidos */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
  ASSIGN fi-dt-limite-ini    = INPUT FRAME {&FRAME-NAME} fi-dt-limite-ini   
         fi-dt-limite-fin    = INPUT FRAME {&FRAME-NAME} fi-dt-limite-fin   
         fi-nr-pedcli-ini    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-ini   
         fi-nr-pedcli-fin    = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli-fin   
         fi-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini   
         fi-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin   
         fi-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini   
         fi-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin   
         fi-tp-pedido-ini    = INPUT FRAME {&FRAME-NAME} fi-tp-pedido-ini   
         fi-tp-pedido-fin    = INPUT FRAME {&FRAME-NAME} fi-tp-pedido-fin   
         fi-cod-emitente-ini = INPUT FRAME {&FRAME-NAME} fi-cod-emitente-ini
         fi-cod-emitente-fin = INPUT FRAME {&FRAME-NAME} fi-cod-emitente-fin
         fi-no-ab-reppri-ini = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-ini
         fi-no-ab-reppri-fin = INPUT FRAME {&FRAME-NAME} fi-no-ab-reppri-fin
         fi-cod-obsoleto-ini = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-ini
         fi-cod-obsoleto-fin = INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto-fin
         fi-corte-comerc-ini = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-ini
         fi-corte-comerc-fin = INPUT FRAME {&FRAME-NAME} fi-corte-comerc-fin
         fi-cod-restr-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-restr-ini   
         fi-cod-restr-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-restr-fin   
         fi-cod-depos        = INPUT FRAME {&FRAME-NAME} fi-cod-depos       
         rs-opc-artigo       = INPUT FRAME {&FRAME-NAME} rs-opc-artigo      
         tg-sit-todas        = INPUT FRAME {&FRAME-NAME} tg-sit-todas       
         tg-sit-abe          = INPUT FRAME {&FRAME-NAME} tg-sit-abe         
         tg-sit-atp          = INPUT FRAME {&FRAME-NAME} tg-sit-atp         
         tg-sit-att          = INPUT FRAME {&FRAME-NAME} tg-sit-att         
         tg-sit-sus          = INPUT FRAME {&FRAME-NAME} tg-sit-sus         
         tg-sit-pen          = INPUT FRAME {&FRAME-NAME} tg-sit-pen         
         tg-sit-can          = INPUT FRAME {&FRAME-NAME} tg-sit-can.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-obsoleto-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-obsoleto-ini D-Dialog
ON LEAVE OF fi-cod-obsoleto-ini IN FRAME D-Dialog /* Cod.Obsoleto */
DO:
  IF SELF:SCREEN-VALUE <> '' THEN
     ASSIGN fi-cod-obsoleto-fin:SCREEN-VALUE = SELF:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc-ini D-Dialog
ON LEAVE OF fi-corte-comerc-ini IN FRAME D-Dialog /* Corte Comercial */
DO:
  IF SELF:SCREEN-VALUE = '' THEN DO.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-limite-fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite-fin D-Dialog
ON LEAVE OF fi-dt-limite-fin IN FRAME D-Dialog
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


&Scoped-define SELF-NAME fi-dt-limite-ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-limite-ini D-Dialog
ON LEAVE OF fi-dt-limite-ini IN FRAME D-Dialog /* Data Limite */
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


&Scoped-define SELF-NAME tg-sit-todas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sit-todas D-Dialog
ON VALUE-CHANGED OF tg-sit-todas IN FRAME D-Dialog /* TODAS SITUA€åES */
DO:
    /*
  IF INPUT FRAME {&FRAME-NAME} tg-lote-todos = NO THEN
     ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = YES
            tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-pp)
            tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(l-lote-rp).
  ELSE
      ASSIGN tg-lote-pp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rp:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-rd:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-sc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-ca:SENSITIVE IN FRAME {&FRAME-NAME} = NO
             tg-lote-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
             tg-lote-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "no"
             l-lote-pp = YES
             l-lote-rp = YES. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fi-nr-pedcli-ini fi-nr-pedcli-fin fi-it-codigo-ini fi-it-codigo-fin 
          tg-sit-abe tg-sit-sus tg-sit-atp tg-sit-can tg-sit-att tg-sit-pen 
          tg-sit-todas fi-dt-limite-ini fi-dt-limite-fin fi-cod-refer-ini 
          fi-cod-refer-fin fi-tp-pedido-ini fi-tp-pedido-fin fi-cod-emitente-ini 
          fi-cod-emitente-fin rs-opc-artigo fi-no-ab-reppri-ini 
          fi-no-ab-reppri-fin fi-cod-obsoleto-ini fi-cod-obsoleto-fin 
          fi-corte-comerc-ini fi-corte-comerc-fin fi-cod-depos fi-cod-restr-ini 
          fi-cod-restr-fin 
      WITH FRAME D-Dialog.
  ENABLE fi-nr-pedcli-ini fi-nr-pedcli-fin fi-it-codigo-ini fi-it-codigo-fin 
         tg-sit-abe tg-sit-sus tg-sit-atp tg-sit-can tg-sit-att tg-sit-pen 
         bt-ok bt-cancela bt-ajuda tg-sit-todas fi-dt-limite-ini 
         fi-dt-limite-fin fi-cod-refer-ini fi-cod-refer-fin fi-tp-pedido-ini 
         fi-tp-pedido-fin fi-cod-emitente-ini fi-cod-emitente-fin rs-opc-artigo 
         fi-no-ab-reppri-ini fi-no-ab-reppri-fin fi-cod-obsoleto-ini 
         fi-cod-obsoleto-fin fi-corte-comerc-ini fi-corte-comerc-fin 
         fi-cod-depos fi-cod-restr-ini fi-cod-restr-fin IMAGE-3 IMAGE-4 IMAGE-5 
         IMAGE-6 IMAGE-77 IMAGE-78 IMAGE-83 IMAGE-87 IMAGE-88 IMAGE-89 IMAGE-90 
         IMAGE-91 IMAGE-92 IMAGE-93 IMAGE-94 IMAGE-95 IMAGE-96 IMAGE-97 
         IMAGE-98 IMAGE-99 RECT-50 rt-buttom 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  ASSIGN fi-dt-limite-ini    = c-dt-limite-ini   
         fi-dt-limite-fin    = c-dt-limite-fin
         fi-nr-pedcli-ini    = c-nr-pedcli-ini   
         fi-nr-pedcli-fin    = c-nr-pedcli-fin   
         fi-it-codigo-ini    = c-it-codigo-ini   
         fi-it-codigo-fin    = c-it-codigo-fin    
         fi-cod-refer-ini    = c-cod-refer-ini    
         fi-cod-refer-fin    = c-cod-refer-fin   
         fi-tp-pedido-ini    = c-tp-pedido-ini   
         fi-tp-pedido-fin    = c-tp-pedido-fin   
         fi-cod-emitente-ini = i-cod-emitente-ini
         fi-cod-emitente-fin = i-cod-emitente-fin
         fi-no-ab-reppri-ini = c-no-ab-reppri-ini
         fi-no-ab-reppri-fin = c-no-ab-reppri-fin
         fi-cod-obsoleto-ini = c-cod-obsoleto-ini
         fi-cod-obsoleto-fin = c-cod-obsoleto-fin
         fi-corte-comerc-ini = c-corte-comerc-ini
         fi-corte-comerc-fin = c-corte-comerc-fin
         fi-cod-restr-ini    = c-cod-restr-ini   
         fi-cod-restr-fin    = c-cod-restr-fin   
         fi-cod-depos        = c-cod-depos       
         rs-opc-artigo       = c-opc-artigo
         tg-sit-todas        = l-sit-todas.      

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF NOT tg-sit-todas THEN DO:
     ASSIGN tg-sit-abe = l-sit-abe
            tg-sit-atp = l-sit-atp
            tg-sit-att = l-sit-att
            tg-sit-pen = l-sit-pen
            tg-sit-sus = l-sit-sus
            tg-sit-can = l-sit-can.
     
     APPLY 'value-changed' TO tg-sit-todas IN FRAME {&FRAME-NAME}.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

