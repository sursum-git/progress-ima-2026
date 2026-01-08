&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i V01IN375 2.04.00.000}

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

DEFINE NEW GLOBAL SHARED VAR l-programacao AS LOG.
DEFINE NEW GLOBAL SHARED VAR l-codigo-ean  AS LOG.
DEFINE NEW GLOBAL SHARED VAR h-w-cadsi2 AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE VAR de-qtd-prog  LIKE ref-item-ext.qtd-prog.
DEFINE VAR de-qtd-proc  LIKE ref-item-ext.qtd-proc.
DEFINE VAR de-qtd-pron  LIKE ref-item-ext.qtd-pron.
DEFINE VAR de-reserva   AS DEC.  
DEFINE VAR de-carteira  AS DEC.
DEFINE VAR de-estoque   AS DEC.
DEFINE VAR de-programar AS DEC.

DEF VAR c-item-ref    AS CHAR FORMAT "x(24)".
def var i-dv-fatores  as int extent 13 init[6,5,4,3,2,9,8,7,6,5,4,3,2].
def var i-dv-dig-calc as int.
def var i-dv-cont     as int.
def var i-dv-soma     as int.

DEF BUFFER b-ref-item-ext FOR ref-item-ext.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME f-main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ref-item
&Scoped-define FIRST-EXTERNAL-TABLE ref-item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ref-item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ref-item.it-codigo 
&Scoped-define ENABLED-TABLES ref-item
&Scoped-define FIRST-ENABLED-TABLE ref-item
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS ref-item.it-codigo ref-item.cod-refer 
&Scoped-define DISPLAYED-TABLES ref-item
&Scoped-define FIRST-DISPLAYED-TABLE ref-item
&Scoped-Define DISPLAYED-OBJECTS fi-desc-item fi-desc-ref fi-cod-ean ~
fi-aux-ean fi-dv fi-cod-obsoleto fi-colecao fi-cor fi-cod-fundo ~
tg-bloqueio-pp fi-qtd-prog0 fi-dt-ult-prog fi-qtd-prog fi-qtd-proc ~
fi-qtd-pron fi-qtd-prog1 fi-qtd-proc1 fi-qtd-pron1 fi-dt-ult-proc ~
fi-dt-ult-pron fi-qtd-bxa-pron tg-bloqueio-pd tg-bloqueio-rp tg-bloqueio-rd ~
fi-usu-ult-prog fi-usu-ult-proc fi-usu-ult-pron 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-colecao fi-cor fi-cod-fundo tg-bloqueio-pp ~
tg-bloqueio-pd tg-bloqueio-rp tg-bloqueio-rd 

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
DEFINE VARIABLE fi-aux-ean AS INTEGER FORMAT "99999" INITIAL 0 
     LABEL "Codigo Ean" 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-ean AS CHARACTER FORMAT "999999999999":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-fundo AS CHARACTER FORMAT "x(4)" 
     LABEL "Codigo Fundo" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-obsoleto AS CHARACTER FORMAT "x(1)" 
     LABEL "Cod.Obsoleto" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-colecao AS CHARACTER FORMAT "x(20)" 
     LABEL "Colecao" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cor AS CHARACTER FORMAT "X(20)" 
     LABEL "Cor" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 57.86 BY .88.

DEFINE VARIABLE fi-desc-ref AS CHARACTER FORMAT "x(32)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY .88.

DEFINE VARIABLE fi-dt-ult-proc AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "Data da £ltima altera‡Æo em Processo." NO-UNDO.

DEFINE VARIABLE fi-dt-ult-prog AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "Data da £ltima altera‡Æo em Programado." NO-UNDO.

DEFINE VARIABLE fi-dt-ult-pron AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "Data da £ltima altera‡Æo em Pronto." NO-UNDO.

DEFINE VARIABLE fi-dv AS CHARACTER FORMAT "x(1)" 
     LABEL "DV" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-bxa-pron AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Quantidade a baixar de Pronto" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-proc AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-proc1 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-prog AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-prog0 AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Necessidade de Programa‡Æo" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-prog1 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-pron AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qtd-pron1 AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-usu-ult-proc AS CHARACTER FORMAT "X(12)" 
     LABEL "Processo" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "Usu rio que fez a £ltima altera‡Æo em Processo." NO-UNDO.

DEFINE VARIABLE fi-usu-ult-prog AS CHARACTER FORMAT "X(12)" 
     LABEL "Programado" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "Usu rio que fez a £ltima altera‡Æo em Programado." NO-UNDO.

DEFINE VARIABLE fi-usu-ult-pron AS CHARACTER FORMAT "X(12)" 
     LABEL "Pronto" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .88 TOOLTIP "Usu rio que fez a £ltima altera‡Æo em Pronto." NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 2.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 7.5.

DEFINE VARIABLE tg-bloqueio-pd AS LOGICAL INITIAL no 
     LABEL "Pd" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .88 TOOLTIP "Bloquear Item/Ref com lote iniciado por Pd para faturamento." NO-UNDO.

DEFINE VARIABLE tg-bloqueio-pp AS LOGICAL INITIAL no 
     LABEL "Pp" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .88 TOOLTIP "Bloquear Item/Ref com lote iniciado por Pp para faturamento." NO-UNDO.

DEFINE VARIABLE tg-bloqueio-rd AS LOGICAL INITIAL no 
     LABEL "Rd" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .88 TOOLTIP "Bloquear Item/Ref com lote iniciado por Rd para faturamento." NO-UNDO.

DEFINE VARIABLE tg-bloqueio-rp AS LOGICAL INITIAL no 
     LABEL "Rp" 
     VIEW-AS TOGGLE-BOX
     SIZE 5 BY .88 TOOLTIP "Bloquear Item/Ref com lote iniciado por Rp para faturamento." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ref-item.it-codigo AT ROW 1.17 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY .88
     fi-desc-item AT ROW 1.17 COL 28.14 COLON-ALIGNED NO-LABEL
     ref-item.cod-refer AT ROW 2.17 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.86 BY .88
     fi-desc-ref AT ROW 2.17 COL 21.14 COLON-ALIGNED NO-LABEL
     fi-cod-ean AT ROW 3.54 COL 17.57 COLON-ALIGNED NO-LABEL
     fi-aux-ean AT ROW 3.54 COL 12 COLON-ALIGNED HELP
          "Codigo Ean/Brasil"
     fi-dv AT ROW 4.54 COL 12 COLON-ALIGNED HELP
          "Digito verifcador do item"
     fi-cod-obsoleto AT ROW 5.54 COL 12 COLON-ALIGNED HELP
          "Codigo obsoleto"
     fi-colecao AT ROW 6.54 COL 12 COLON-ALIGNED HELP
          "Colecao do item"
     fi-cor AT ROW 7.54 COL 12 COLON-ALIGNED
     fi-cod-fundo AT ROW 8.54 COL 12 COLON-ALIGNED HELP
          "Codigo de fundo para programacao de producao"
     tg-bloqueio-pp AT ROW 9.54 COL 14
     fi-qtd-prog0 AT ROW 9.58 COL 77.57 COLON-ALIGNED
     fi-dt-ult-prog AT ROW 4.25 COL 58 COLON-ALIGNED NO-LABEL
     fi-qtd-prog AT ROW 4.25 COL 68.57 COLON-ALIGNED NO-LABEL
     fi-qtd-proc AT ROW 5.25 COL 68.57 COLON-ALIGNED NO-LABEL
     fi-qtd-pron AT ROW 6.25 COL 68.57 COLON-ALIGNED NO-LABEL
     fi-qtd-prog1 AT ROW 4.25 COL 77.57 COLON-ALIGNED NO-LABEL
     fi-qtd-proc1 AT ROW 5.25 COL 77.57 COLON-ALIGNED NO-LABEL
     fi-qtd-pron1 AT ROW 6.25 COL 77.57 COLON-ALIGNED NO-LABEL
     fi-dt-ult-proc AT ROW 5.25 COL 58 COLON-ALIGNED NO-LABEL
     fi-dt-ult-pron AT ROW 6.25 COL 58 COLON-ALIGNED NO-LABEL
     fi-qtd-bxa-pron AT ROW 7.25 COL 77.57 COLON-ALIGNED
     tg-bloqueio-pd AT ROW 9.54 COL 19.43
     tg-bloqueio-rp AT ROW 9.54 COL 25.14
     tg-bloqueio-rd AT ROW 9.54 COL 30.72
     fi-usu-ult-prog AT ROW 4.25 COL 47.43 COLON-ALIGNED
     fi-usu-ult-proc AT ROW 5.25 COL 47.43 COLON-ALIGNED
     fi-usu-ult-pron AT ROW 6.25 COL 47.43 COLON-ALIGNED
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 3.25 COL 1
     "éltima Data" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3.58 COL 61
     "Acumulado" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3.58 COL 71.29
     "Lan‡amento" VIEW-AS TEXT
          SIZE 8.86 BY .54 AT ROW 3.58 COL 79.86
     "Bloqueio:" VIEW-AS TEXT
          SIZE 6.86 BY .54 AT ROW 9.63 COL 7.14
     "élt.Usu rio" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 3.58 COL 50.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ems206cad.ref-item
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
         HEIGHT             = 9.83
         WIDTH              = 88.14.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ref-item.cod-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-aux-ean IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-ean IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-fundo IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-obsoleto IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-colecao IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cor IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-ref IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ult-proc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ult-prog IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ult-pron IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dv IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-bxa-pron IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-proc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-proc1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-prog IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-prog0 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-prog1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-pron IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-pron1 IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usu-ult-proc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usu-ult-prog IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usu-ult-pron IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-bloqueio-pd IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-bloqueio-pp IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-bloqueio-rd IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR TOGGLE-BOX tg-bloqueio-rp IN FRAME f-main
   NO-ENABLE 4                                                          */
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

&Scoped-define SELF-NAME ref-item.cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ref-item.cod-refer V-table-Win
ON LEAVE OF ref-item.cod-refer IN FRAME f-main /* Referˆncia */
DO:
  FIND referencia WHERE referencia.cod-refer = ref-item.cod-refer NO-LOCK NO-ERROR.
  IF AVAIL referencia THEN
     ASSIGN fi-desc-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
  ELSE
     ASSIGN fi-desc-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-aux-ean
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-aux-ean V-table-Win
ON LEAVE OF fi-aux-ean IN FRAME f-main /* Codigo Ean */
DO:
  ASSIGN fi-cod-ean:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "7890413" + 
         INPUT FRAME {&FRAME-NAME} fi-aux-ean.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-bxa-pron
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-bxa-pron V-table-Win
ON LEAVE OF fi-qtd-bxa-pron IN FRAME f-main /* Quantidade a baixar de Pronto */
DO:
  IF INPUT FRAME {&FRAME-NAME} fi-qtd-bxa-pron > ref-item-ext.qtd-pron THEN DO:
     MESSAGE "Quantidade excede a quantidade em Pronto." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-qtd-bxa-pron IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-proc1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-proc1 V-table-Win
ON LEAVE OF fi-qtd-proc1 IN FRAME f-main
DO:
  IF INPUT FRAME {&FRAME-NAME} fi-qtd-proc1 > ref-item-ext.qtd-prog THEN DO:
     MESSAGE "Quantidade excede a quantidade Programada." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-qtd-proc1 IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qtd-pron1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qtd-pron1 V-table-Win
ON LEAVE OF fi-qtd-pron1 IN FRAME f-main
DO:
  IF INPUT FRAME {&FRAME-NAME} fi-qtd-pron1 > ref-item-ext.qtd-proc THEN DO:
     MESSAGE "Quantidade excede a quantidade em Processo." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-qtd-pron1 IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ref-item.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ref-item.it-codigo V-table-Win
ON LEAVE OF ref-item.it-codigo IN FRAME f-main /* Item */
DO:
  FIND ITEM WHERE ITEM.it-codigo = ref-item.it-codigo NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.
  ELSE
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
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
  {src/adm/template/row-list.i "ref-item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ref-item"}

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
  /*  {include/i-valid.i} */
    
  /* Ponha na pi-validate todas as valida‡äes */
  /* NÆo gravar nada no registro antes do dispatch do assign-record e 
     nem na PI-validate. */
    
  /* Dispatch standard ADM method.                             */
  /* --- Comentado porque viewer s¢ tem campos fill-in habilitados ---
  * RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  * if RETURN-VALUE = 'ADM-ERROR':U then 
  *    return 'ADM-ERROR':U. 
  -----------------------------------Gilvando Nov/2003---------------*/
  RUN pi-validate.
  if RETURN-VALUE = 'ADM-ERROR':U then 
     return 'ADM-ERROR':U.
  
  FIND ref-item-ext WHERE ref-item-ext.it-codigo = ref-item.it-codigo 
                      AND ref-item-ext.cod-refer = ref-item.cod-refer
                    NO-ERROR.
  IF NOT AVAIL ref-item-ext THEN DO:
     RUN pi-calc-dv.

     CREATE ref-item-ext.
     ASSIGN ref-item-ext.it-codigo = ref-item.it-codigo
            ref-item-ext.cod-refer = ref-item.cod-refer
            ref-item-ext.dv        = STRING(i-dv-dig-calc).
  END.

  /* Salva valores, para verificar se foi alterado na tela */
  ASSIGN /*de-qtd-prog = ref-item-ext.qtd-prog*/
         de-qtd-proc = ref-item-ext.qtd-proc            
         de-qtd-pron = ref-item-ext.qtd-pron.

  ASSIGN ref-item-ext.cod-ean      = "7890413" + INPUT FRAME {&FRAME-NAME} fi-aux-ean
         ref-item-ext.cod-obsoleto = INPUT FRAME {&frame-name} fi-cod-obsoleto
         ref-item-ext.colecao      = INPUT FRAME {&FRAME-NAME} fi-colecao
         ref-item-ext.cor          = INPUT FRAME {&FRAME-NAME} fi-cor
         ref-item-ext.cod-fundo    = INPUT FRAME {&FRAME-NAME} fi-cod-fundo
         ref-item-ext.bloqueio-pp  = INPUT FRAME {&FRAME-NAME} tg-bloqueio-pp
         ref-item-ext.bloqueio-pd  = INPUT FRAME {&FRAME-NAME} tg-bloqueio-pd
         ref-item-ext.bloqueio-rp  = INPUT FRAME {&FRAME-NAME} tg-bloqueio-rp
         ref-item-ext.bloqueio-rd  = INPUT FRAME {&FRAME-NAME} tg-bloqueio-rd
         ref-item-ext.qtd-prog     = ref-item-ext.qtd-prog - INT(INPUT FRAME {&FRAME-NAME} fi-qtd-proc1)       
         ref-item-ext.qtd-proc     = ref-item-ext.qtd-proc + INT(INPUT FRAME {&FRAME-NAME} fi-qtd-proc1)
                                                     - INT(INPUT FRAME {&FRAME-NAME} fi-qtd-pron1)
         ref-item-ext.qtd-pron     = ref-item-ext.qtd-pron - INT(INPUT FRAME {&FRAME-NAME} fi-qtd-bxa-pron)
                                                     + INT(INPUT FRAME {&FRAME-NAME} fi-qtd-pron1)
         ref-item-ext.usu-ult-prog = c-seg-usuario WHEN l-programacao AND INPUT FRAME {&FRAME-NAME} fi-qtd-prog1 <> 0
         ref-item-ext.usu-ult-proc = c-seg-usuario WHEN l-programacao AND INPUT FRAME {&FRAME-NAME} fi-qtd-proc1 <> 0
         ref-item-ext.usu-ult-pron = c-seg-usuario WHEN l-programacao AND (INPUT FRAME {&FRAME-NAME} fi-qtd-pron1 <> 0 OR
                                                                           INPUT FRAME {&FRAME-NAME} fi-qtd-bxa-pron <> 0)
         ref-item-ext.dt-ult-prog  = TODAY WHEN l-programacao AND INPUT FRAME {&FRAME-NAME} fi-qtd-prog1 <> 0
         ref-item-ext.dt-ult-proc  = TODAY WHEN l-programacao AND INPUT FRAME {&FRAME-NAME} fi-qtd-proc1 <> 0
         ref-item-ext.dt-ult-pron  = TODAY WHEN l-programacao AND (INPUT FRAME {&FRAME-NAME} fi-qtd-pron1 <> 0 OR
                                                                   INPUT FRAME {&FRAME-NAME} fi-qtd-bxa-pron <> 0).                               
                                                                                                                                         
  RUN pi-habilita IN h-w-cadsi2.

  /* Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pi-habilita IN h-w-cadsi2.

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
    &if defined(ADM-MODIFY-FIELDS) &then
        disable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    
    ASSIGN fi-cod-obsoleto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-colecao:SENSITIVE IN FRAME {&FRAME-NAME}      = NO
           fi-cor:SENSITIVE IN FRAME {&FRAME-NAME}          = NO
           fi-cod-fundo:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
           tg-bloqueio-pp:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
           tg-bloqueio-pd:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
           tg-bloqueio-rp:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
           tg-bloqueio-rd:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
           fi-qtd-prog1:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
           fi-qtd-proc1:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
           fi-qtd-pron1:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
           fi-qtd-bxa-pron:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-aux-ean:SENSITIVE IN FRAME {&FRAME-NAME}      = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
     
    FIND ITEM WHERE ITEM.it-codigo = ref-item.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL ITEM THEN
       ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.
    
    FIND referencia WHERE referencia.cod-ref = ref-item.cod-refer NO-LOCK NO-ERROR.
    IF AVAIL referencia THEN
       ASSIGN fi-desc-ref:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.

    FIND ref-item-ext WHERE ref-item-ext.cod-refer = ref-item.cod-refer 
                        AND ref-item-ext.it-codigo = ref-item.it-codigo
                      NO-LOCK NO-ERROR.
    IF AVAIL ref-item-ext THEN DO:
       ASSIGN fi-cod-ean:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ref-item-ext.cod-ean
              fi-aux-ean:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTR(ref-item-ext.cod-ean,8,5)
              fi-dv:SCREEN-VALUE IN FRAME {&FRAME-NAME}= ref-item-ext.dv
              fi-cod-obsoleto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ref-item-ext.cod-obsoleto)
              fi-colecao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ref-item-ext.colecao
              fi-cor:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ref-item-ext.cor
              fi-cod-fundo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ref-item-ext.cod-fundo
              tg-bloqueio-pp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ref-item-ext.bloqueio-pp)
              tg-bloqueio-pd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ref-item-ext.bloqueio-pd)
              tg-bloqueio-rp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ref-item-ext.bloqueio-rp)
              tg-bloqueio-rd:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ref-item-ext.bloqueio-rd)
              fi-qtd-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ref-item-ext.qtd-prog)
              fi-qtd-proc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ref-item-ext.qtd-proc)
              fi-qtd-pron:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(ref-item-ext.qtd-pron)
              fi-usu-ult-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ref-item-ext.usu-ult-prog)
              fi-usu-ult-proc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ref-item-ext.usu-ult-proc)
              fi-usu-ult-pron:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ref-item-ext.usu-ult-pron)
              fi-dt-ult-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ref-item-ext.dt-ult-prog)
              fi-dt-ult-proc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ref-item-ext.dt-ult-proc)
              fi-dt-ult-pron:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ref-item-ext.dt-ult-pron).
    END.
    ELSE DO:
        ASSIGN fi-cod-ean:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" 
               fi-dv:SCREEN-VALUE IN FRAME {&FRAME-NAME}= ""
               fi-cod-obsoleto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-colecao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-cod-fundo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-qtd-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-qtd-proc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-qtd-pron:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-dt-ult-prog:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-dt-ult-proc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
               fi-dt-ult-pron:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    END.

    /*--- C lculo da necessidade de programa‡Æo ---*/ 
    IF l-programacao AND AVAIL ped-item-res THEN DO:
       assign de-reserva   = 0
              de-carteira  = 0
              de-estoque   = 0
              de-programar = 0.
       
       for each ped-item-res where ped-item-res.it-codigo = ref-item.it-codigo
                               AND ped-item-res.cod-refer = ref-item.cod-refer
                               and ped-item-res.faturado  = no
                             no-lock:
           assign de-reserva = de-reserva + ped-item-res.qt-pedida.
       end.
       
       for each ped-item where ped-item.it-codigo     =  ref-item.it-codigo
                           AND ped-item.cod-refer     =  ref-item.cod-refer
                           and ped-item.dt-entrega    >= TODAY
                           and ped-item.dt-entrega    <= today + (int(entry(MONTH(TODAY),"31,28,31,30,31,30,31,31,30,31,30,31")) - day(today))
                           and (ped-item.cod-sit-item <  3 or
                                ped-item.cod-sit-item =  5)
                         no-lock:
           assign de-carteira = de-carteira + ped-item.qt-pedida -
                                              ped-item.qt-atendida -
                                              ped-item.qt-pendente.
       end.
       
       if de-carteira > de-reserva then
          assign de-carteira = de-carteira - de-reserva.
       
       for each saldo-estoq where saldo-estoq.it-codigo = ref-item.it-codigo 
                              AND saldo-estoq.cod-refer = ref-item.cod-refer
                            no-lock.
           assign de-estoque = de-estoque + saldo-estoq.qtidade-atu.
       end.
       
       assign de-estoque = de-estoque - de-reserva.
       
       assign de-programar  = (de-estoque - de-carteira) +
                              (item-prog.programado + item-prog.processo).

       ASSIGN fi-qtd-prog0:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(de-programar).
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
    if adm-new-record = NO then
        enable {&ADM-MODIFY-FIELDS} with frame {&frame-name}.
    &endif
    IF l-programacao = NO AND l-codigo-ean = NO THEN
       ASSIGN fi-cod-obsoleto:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
              fi-colecao:SENSITIVE IN FRAME {&FRAME-NAME}      = NO 
              fi-cor:SENSITIVE IN FRAME {&FRAME-NAME}          = NO 
              fi-cod-fundo:SENSITIVE IN FRAME {&FRAME-NAME}    = NO 
              tg-bloqueio-pp:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
              tg-bloqueio-pd:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
              tg-bloqueio-rp:SENSITIVE IN FRAME {&FRAME-NAME}  = YES
              tg-bloqueio-rd:SENSITIVE IN FRAME {&FRAME-NAME}  = YES.
                                                              
    IF l-programacao THEN DO:
        /* Toninho 23/07/2008 */

       /*ASSIGN fi-qtd-prog1:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 

       IF ref-item-ext.qtd-prog <> 0 OR ref-item-ext.qtd-proc <> 0 THEN
          ASSIGN fi-qtd-proc1:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
       ELSE
          ASSIGN fi-qtd-proc1:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

       IF ref-item-ext.qtd-proc <> 0 OR ref-item-ext.qtd-pron <> 0 THEN
          ASSIGN fi-qtd-pron1:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
       ELSE
          ASSIGN fi-qtd-pron1:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

       IF ref-item-ext.qtd-pron <> 0 THEN
          ASSIGN fi-qtd-bxa-pron:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
       ELSE
          ASSIGN fi-qtd-bxa-pron:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
          
        */  
    END.

    IF l-codigo-ean THEN
       ASSIGN fi-aux-ean:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    FIND rel-grup-user WHERE
         rel-grup-user.cd-grupo = "QL0" AND
         rel-grup-user.usuario = c-seg-usuario NO-LOCK NO-ERROR.

    IF AVAIL rel-grup-user THEN
       ASSIGN fi-cod-obsoleto:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              fi-colecao:SENSITIVE IN FRAME {&FRAME-NAME}      = NO
              fi-cor:SENSITIVE IN FRAME {&FRAME-NAME}          = NO
              fi-cod-fundo:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
              fi-qtd-prog1:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
              fi-qtd-proc1:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
              fi-qtd-pron1:SENSITIVE IN FRAME {&FRAME-NAME}    = NO.

    ASSIGN ref-item.it-codigo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-dv V-table-Win 
PROCEDURE pi-calc-dv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO:
  /* Calculo do Digito verificador do item */
  assign i-dv-soma  = 0
         c-item-ref = INPUT FRAME {&FRAME-NAME} ref-item.it-codigo + 
                      INPUT FRAME {&FRAME-NAME} ref-item.cod-refer.

  DO i-dv-cont = 1 to 13:
     if  substr(c-item-ref,i-dv-cont,1) >= "0" 
     and substr(c-item-ref,i-dv-cont,1) <= "9" then
         assign i-dv-soma = i-dv-soma + 
                int(substr(c-item-ref,i-dv-cont,1)) * i-dv-fatores[i-dv-cont].
  end.
  assign i-dv-dig-calc = 11 - (i-dv-soma MODULO 11).
  if i-dv-dig-calc > 9 then
     assign i-dv-dig-calc = 0.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pi-validate V-table-Win 
PROCEDURE Pi-validate :
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
    {include/i-vldfrm.i} /* Valida‡Æo de dicion rio */
    
    IF l-programacao = NO AND l-codigo-ean = NO THEN DO:
       IF not(INPUT FRAME {&frame-name} fi-cod-obsoleto >= "0" AND
              INPUT FRAME {&FRAME-NAME} fi-cod-obsoleto <= "5") THEN DO:
          MESSAGE "C¢digo Obsoleto deve estar entre 0 e 5." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO fi-cod-obsoleto.                                                          
          return 'ADM-ERROR':U.                                                                    
       END.
    END.

    IF l-programacao THEN DO:
       FIND ref-item-ext WHERE ref-item-ext.it-codigo = ref-item.it-codigo 
                           AND ref-item-ext.cod-refer = ref-item.cod-refer
                         NO-ERROR.
       IF NOT AVAIL ref-item-ext THEN DO:
           MESSAGE "Ötem/Referˆncia sem informa‡äes b sicas (C¢digo Obsoleto e Fator)." SKIP
                   "NÆo pode ser Programado." VIEW-AS ALERT-BOX.
           return 'ADM-ERROR':U.                                                                    
       END.

       IF ref-item-ext.qtd-prog - INT(INPUT FRAME {&FRAME-NAME} fi-qtd-proc1) < 0 OR 
          fi-qtd-proc1 > fi-qtd-proc THEN DO:
          MESSAGE "Quantidade excede a quantidade Programada." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO fi-qtd-proc1 IN FRAME {&FRAME-NAME}.
          return 'ADM-ERROR':U.                                                                    
       END.

       IF ref-item-ext.qtd-proc + INT(INPUT FRAME {&FRAME-NAME} fi-qtd-proc1)
                                - INT(INPUT FRAME {&FRAME-NAME} fi-qtd-pron1) < 0 OR 
          fi-qtd-pron1 > fi-qtd-pron THEN DO:
          MESSAGE "Quantidade excede a quantidade em Processo." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO fi-qtd-pron1 IN FRAME {&FRAME-NAME}.
          return 'ADM-ERROR':U.                                                                    
       END.

       IF ref-item-ext.qtd-pron - INT(INPUT FRAME {&FRAME-NAME} fi-qtd-bxa-pron)
                                + INT(INPUT FRAME {&FRAME-NAME} fi-qtd-pron1) < 0 THEN DO:
          MESSAGE "Quantidade excede a quantidade em Pronto." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO fi-qtd-bxa-pron IN FRAME {&FRAME-NAME}.
          return 'ADM-ERROR':U.                                                                    
       END.
    END.
    IF l-codigo-ean THEN DO:
       IF INPUT FRAME {&FRAME-NAME} fi-aux-ean = 0 THEN DO:
           MESSAGE "C¢digo EAN Deve estar entre 1 e 99999" VIEW-AS ALERT-BOX.
           APPLY 'entry' TO fi-aux-ean IN FRAME {&FRAME-NAME}.
           return 'ADM-ERROR':U.                                                                    
       END.
       fi-cod-ean:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "7890413" + 
                     string(INPUT FRAME {&FRAME-NAME} fi-aux-ean,"99999").
       FIND b-ref-item-ext WHERE b-ref-item-ext.cod-ean = INPUT FRAME {&FRAME-NAME} fi-cod-ean NO-LOCK NO-ERROR.
       IF AVAIL b-ref-item-ext THEN DO:
          MESSAGE "C¢digo EAN j  est  sendo utilizado pelo Item: " + b-ref-item-ext.it-codigo VIEW-AS ALERT-BOX.
          APPLY 'entry' TO fi-aux-ean IN FRAME {&FRAME-NAME}.
          return 'ADM-ERROR':U.                                                                    
       END.
    END.

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
  {src/adm/template/snd-list.i "ref-item"}

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

  IF p-state = "update-begin" THEN
     RUN pi-desabilita IN h-w-cadsi2.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  run pi-trata-state (p-issuer-hdl, p-state).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

