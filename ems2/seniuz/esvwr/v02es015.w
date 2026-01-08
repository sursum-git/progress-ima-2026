&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
{include/i-prgvrs.i V02ES015 2.04.00.000}

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

DEF VAR i-ct AS INT.
DEF VAR c-lst-cortes AS CHAR.
DEF VAR c-lst-refer AS CHAR.
DEF VAR c-gr-refer AS CHAR.
DEF VAR c-des-refer AS CHAR.
DEF VAR de-perc AS DEC.

DEF BUFFER b-item FOR ITEM.

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
&Scoped-define EXTERNAL-TABLES item
&Scoped-define FIRST-EXTERNAL-TABLE item


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR item.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS item.peso-liquido 
&Scoped-define ENABLED-TABLES item
&Scoped-define FIRST-ENABLED-TABLE item
&Scoped-Define ENABLED-OBJECTS RECT-1 rt-key 
&Scoped-Define DISPLAYED-FIELDS item.peso-liquido 
&Scoped-define DISPLAYED-TABLES item
&Scoped-define FIRST-DISPLAYED-TABLE item
&Scoped-Define DISPLAYED-OBJECTS fi-cod-composi fi-desc-composi ~
sl-des-refer fi-cod-rlgp fi-largura fi-fator-conv to-indigo ~
cb-corte-comerc1 fi-perc1 cb-corte-comerc2 fi-perc2 cb-corte-comerc3 ~
fi-perc3 fi-gr-refer fi-descricao fi-margem-pcp 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-cod-composi sl-des-refer fi-cod-rlgp fi-largura ~
to-indigo cb-corte-comerc1 fi-perc1 cb-corte-comerc2 fi-perc2 ~
cb-corte-comerc3 fi-perc3 fi-gr-refer fi-descricao bt-confirma ~
fi-margem-pcp 

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
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13.

DEFINE VARIABLE cb-corte-comerc1 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE cb-corte-comerc2 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE cb-corte-comerc3 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION UNIQUE-MATCH
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cod-composi AS CHARACTER FORMAT "x(2)" 
     LABEL "Composiá∆o" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-rlgp AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Rec.Lavagem" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-composi AS CHARACTER FORMAT "x(45)" 
     VIEW-AS FILL-IN 
     SIZE 39.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-descricao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fator-conv AS DECIMAL FORMAT ">>9.9999" INITIAL 0 
     LABEL "Fator M" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-gr-refer AS CHARACTER FORMAT "99":U 
     LABEL "C¢digo" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-largura AS DECIMAL FORMAT "9.99" INITIAL 0 
     LABEL "Largura" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-margem-pcp AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Margem PCP" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc1 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc2 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE VARIABLE fi-perc3 AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 4.38.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 7.67.

DEFINE VARIABLE sl-des-refer AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 27 BY 7.25
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE to-indigo AS LOGICAL INITIAL no 
     LABEL "÷ndigo" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .88 TOOLTIP "A fam°lia Ç um ÷ndigo?" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-cod-composi AT ROW 1.25 COL 11 COLON-ALIGNED
     fi-desc-composi AT ROW 1.25 COL 16.29 COLON-ALIGNED NO-LABEL
     sl-des-refer AT ROW 1.25 COL 59 NO-LABEL
     fi-cod-rlgp AT ROW 2.25 COL 11 COLON-ALIGNED HELP
          "Codigo de Recomendacao de Lavagem por Grupo de Produto"
     fi-largura AT ROW 3.25 COL 11 COLON-ALIGNED HELP
          "Largura do tecido"
     item.peso-liquido AT ROW 4.25 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.43 BY .79
     fi-fator-conv AT ROW 5.25 COL 11 COLON-ALIGNED HELP
          "Fator de conversao para Metros"
     to-indigo AT ROW 2.25 COL 23
     cb-corte-comerc1 AT ROW 3.79 COL 34.86 NO-LABEL
     fi-perc1 AT ROW 3.79 COL 46.86 COLON-ALIGNED NO-LABEL
     cb-corte-comerc2 AT ROW 4.79 COL 34.86 NO-LABEL
     fi-perc2 AT ROW 4.79 COL 46.86 COLON-ALIGNED NO-LABEL
     cb-corte-comerc3 AT ROW 5.79 COL 34.86 NO-LABEL
     fi-perc3 AT ROW 5.79 COL 46.86 COLON-ALIGNED NO-LABEL
     fi-gr-refer AT ROW 7.54 COL 11 COLON-ALIGNED
     fi-descricao AT ROW 7.54 COL 14.57 COLON-ALIGNED NO-LABEL
     bt-confirma AT ROW 7.42 COL 54
     fi-margem-pcp AT ROW 6.25 COL 11 COLON-ALIGNED HELP
          "Margem PCP"
     RECT-1 AT ROW 2.67 COL 32
     rt-key AT ROW 1.08 COL 1
     "%" VIEW-AS TEXT
          SIZE 3 BY .54 AT ROW 3.17 COL 49
          FGCOLOR 9 FONT 0
     "Corte" VIEW-AS TEXT
          SIZE 11 BY .54 AT ROW 3.17 COL 34.86
          FGCOLOR 9 FONT 0
     " Sobras" VIEW-AS TEXT
          SIZE 6 BY .75 AT ROW 2.29 COL 33.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcad.item
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
         HEIGHT             = 7.92
         WIDTH              = 86.43.
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

/* SETTINGS FOR BUTTON bt-confirma IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR COMBO-BOX cb-corte-comerc1 IN FRAME f-main
   NO-ENABLE ALIGN-L 4                                                  */
/* SETTINGS FOR COMBO-BOX cb-corte-comerc2 IN FRAME f-main
   NO-ENABLE ALIGN-L 4                                                  */
/* SETTINGS FOR COMBO-BOX cb-corte-comerc3 IN FRAME f-main
   NO-ENABLE ALIGN-L 4                                                  */
/* SETTINGS FOR FILL-IN fi-cod-composi IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cod-rlgp IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-desc-composi IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-descricao IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-fator-conv IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-gr-refer IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-largura IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-margem-pcp IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-perc1 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-perc2 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-perc3 IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR SELECTION-LIST sl-des-refer IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR TOGGLE-BOX to-indigo IN FRAME f-main
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

&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma V-table-Win
ON CHOOSE OF bt-confirma IN FRAME f-main /* Button 1 */
DO:
   ASSIGN c-gr-refer = fi-gr-refer:SCREEN-VALUE + ' - ' 
          c-des-refer = c-gr-refer + fi-descricao:SCREEN-VALUE 
          c-lst-refer = sl-des-refer:LIST-ITEMS.

  IF bt-confirma:IMAGE-UP = "image\im-era.bmp" THEN DO.
      ASSIGN c-lst-refer = c-lst-refer + ','.
      ASSIGN c-lst-refer = REPLACE(c-lst-refer,sl-des-refer:SCREEN-VALUE + ',','').
      IF LENGTH(c-lst-refer) <> 0 THEN DO:
         OVERLAY(c-lst-refer,LENGTH(c-lst-refer),1) = ''.
      END.
   END.
   ELSE DO.
       IF INDEX(c-lst-refer,c-gr-refer) > 0 AND sl-des-refer:SCREEN-VALUE = ? THEN DO.
           MESSAGE 'Grupo de Referàncias j† Cadastrado....'
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY 'entry' TO fi-gr-refer.
           RETURN NO-APPLY.
       END.

       IF INDEX(c-lst-refer,c-gr-refer) > 0 THEN 
          ASSIGN c-lst-refer = REPLACE(c-lst-refer,sl-des-refer:SCREEN-VALUE,c-des-refer).
       ELSE
          ASSIGN c-lst-refer = IF sl-des-refer:LIST-ITEMS = ?
                               THEN c-des-refer 
                               ELSE c-lst-refer + ',' + c-des-refer.
   END.

   ASSIGN sl-des-refer:LIST-ITEMS = IF c-lst-refer = '' THEN ? ELSE c-lst-refer.

   IF c-lst-refer = '' THEN
      ASSIGN SELF:SENSITIVE = NO.

   ASSIGN fi-gr-refer:SCREEN-VALUE = ''
          fi-descricao:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-composi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-composi V-table-Win
ON ENTRY OF fi-cod-composi IN FRAME f-main /* Composiá∆o */
DO:
  FIND composi WHERE composi.cod-composi = INPUT FRAME {&FRAME-NAME} fi-cod-composi 
               NO-LOCK NO-ERROR.
  IF AVAIL composi THEN
     ASSIGN fi-desc-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = composi.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-composi V-table-Win
ON LEAVE OF fi-cod-composi IN FRAME f-main /* Composiá∆o */
DO:
   FIND composi WHERE 
        composi.cod-composi = INPUT FRAME {&FRAME-NAME} fi-cod-composi
        NO-LOCK NO-ERROR.

   IF AVAIL composi THEN 
      ASSIGN fi-desc-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = composi.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-composi V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-composi IN FRAME f-main /* Composiá∆o */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es003.w
                     &campo     = fi-cod-composi
                     &campozoom = cod-composi}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-descricao
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-descricao V-table-Win
ON VALUE-CHANGED OF fi-descricao IN FRAME f-main
DO:
   ASSIGN bt-confirma:SENSITIVE = YES.
   bt-confirma:LOAD-IMAGE("image\im-chck1.bmp").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-gr-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-gr-refer V-table-Win
ON VALUE-CHANGED OF fi-gr-refer IN FRAME f-main /* C¢digo */
DO:
   ASSIGN fi-gr-refer:SCREEN-VALUE = STRING(INT(SELF),"99").
   ASSIGN bt-confirma:SENSITIVE = YES.
   bt-confirma:LOAD-IMAGE("image\im-chck1.bmp").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-perc1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc1 V-table-Win
ON LEAVE OF fi-perc1 IN FRAME f-main
DO:
   IF SELF:INPUT-VALUE = 100 THEN DO.
       ASSIGN cb-corte-comerc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
              fi-perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
              cb-corte-comerc3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
              fi-perc3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.

       ASSIGN cb-corte-comerc2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              fi-perc2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              cb-corte-comerc3:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              fi-perc3:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   END.
   ELSE DO.
       RUN pi-verifica.
       CASE RETURN-VALUE.
           WHEN 'ADM-ERROR' THEN DO.
               APPLY 'entry' TO SELF.
               RETURN NO-APPLY.
           END.
           WHEN 'NOK' THEN DO.
               ASSIGN cb-corte-comerc2:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                      fi-perc2:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
               APPLY 'entry' TO cb-corte-comerc2.
               RETURN NO-APPLY.
           END.
           WHEN 'OK' THEN DO.
               ASSIGN cb-corte-comerc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
                      fi-perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
               ASSIGN cb-corte-comerc2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                      fi-perc2:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
           END.
       END CASE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-perc2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc2 V-table-Win
ON LEAVE OF fi-perc2 IN FRAME f-main
DO:
   RUN pi-verifica.
   CASE RETURN-VALUE.
       WHEN 'ADM-ERROR' THEN DO.
           APPLY 'entry' TO SELF.
           RETURN NO-APPLY.
       END.
       WHEN 'NOK' THEN DO.
           ASSIGN cb-corte-comerc3:SENSITIVE IN FRAME {&FRAME-NAME} = YES
                  fi-perc3:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
           APPLY 'entry' TO cb-corte-comerc3.
           RETURN NO-APPLY.
       END.
       WHEN 'OK' THEN DO.
           ASSIGN cb-corte-comerc3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
                  fi-perc3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
           ASSIGN cb-corte-comerc3:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                  fi-perc3:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
       END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-perc3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-perc3 V-table-Win
ON LEAVE OF fi-perc3 IN FRAME f-main
DO:
  RUN pi-verifica.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl-des-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl-des-refer V-table-Win
ON VALUE-CHANGED OF sl-des-refer IN FRAME f-main
DO:
   ASSIGN fi-gr-refer:SCREEN-VALUE = ENTRY(1,SELF:SCREEN-VALUE,"-")
          fi-descricao:SCREEN-VALUE = TRIM(ENTRY(2,SELF:SCREEN-VALUE,"-")).

   bt-confirma:LOAD-IMAGE("image\im-era.bmp").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  FOR EACH corte-comerc NO-LOCK.
      ASSIGN c-lst-cortes = IF c-lst-cortes = ''
                            THEN corte-comerc.descricao 
                            ELSE c-lst-cortes + ',' + corte-comerc.descricao.
  END.
  ASSIGN cb-corte-comerc1:LIST-ITEMS = c-lst-cortes.
  ASSIGN cb-corte-comerc2:LIST-ITEMS = c-lst-cortes.
  ASSIGN cb-corte-comerc3:LIST-ITEMS = c-lst-cortes.              

  fi-cod-composi:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  fi-gr-refer:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
                 
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
  {src/adm/template/row-list.i "item"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "item"}

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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-composi
           INPUT FRAME {&FRAME-NAME} fi-cod-rlgp   
           INPUT FRAME {&FRAME-NAME} fi-largura    
           INPUT FRAME {&FRAME-NAME} fi-margem-pcp    
           INPUT FRAME {&FRAME-NAME} fi-fator-conv 
           INPUT FRAME {&FRAME-NAME} to-indigo  
           INPUT FRAME {&FRAME-NAME} cb-corte-comerc1   
           INPUT FRAME {&FRAME-NAME} fi-perc1      
           INPUT FRAME {&FRAME-NAME} cb-corte-comerc2   
           INPUT FRAME {&FRAME-NAME} fi-perc2      
           INPUT FRAME {&FRAME-NAME} cb-corte-comerc3   
           INPUT FRAME {&FRAME-NAME} fi-perc3.     

    ASSIGN c-lst-refer = sl-des-refer:LIST-ITEMS IN FRAME {&FRAME-NAME}.

   /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U then 
       RETURN 'ADM-ERROR':U.

   /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
   /* Code placed here will execute AFTER standard behavior.    */
    
    IF (ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59) OR
        ITEM.ge-codigo = 12 OR
        ITEM.ge-codigo = 13 OR
        ITEM.ge-codigo = 70 THEN DO:
       FIND item-ext WHERE
            item-ext.it-codigo = item.it-codigo NO-ERROR.
    
       IF NOT AVAIL item-ext THEN DO:
          CREATE item-ext.
          ASSIGN item-ext.it-codigo = item.it-codigo
                 item-ext.descricao = ITEM.descricao-1 + ITEM.descricao-2.
       END.
       IF ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59 THEN DO:
          ASSIGN item-ext.cod-composi   = fi-cod-composi
                 item-ext.cod-rlgp      = fi-cod-rlgp
                 item-ext.largura       = fi-largura
                 item-ext.margem-pcp    = fi-margem-pcp
                 item-ext.fator-conv    = fi-fator-conv
                 item-ext.indigo        = to-indigo
                 item-ext.corte[1]      = cb-corte-comerc1
                 item-ext.perc-corte[1] = fi-perc1
                 item-ext.corte[2]      = cb-corte-comerc2
                 item-ext.perc-corte[2] = fi-perc2
                 item-ext.corte[3]      = cb-corte-comerc3
                 item-ext.perc-corte[3] = fi-perc3.
        
          ASSIGN item-ext.cod-gr-ref = ''
                 item-ext.des-gr-ref = ''.
          DO i-ct = 1 TO NUM-ENTRIES(c-lst-refer).
             ASSIGN c-des-refer = ENTRY(i-ct,c-lst-refer).
          
             IF c-des-refer = '' THEN LEAVE.
             ASSIGN item-ext.cod-gr-ref[i-ct] = TRIM(ENTRY(1,c-des-refer,"-"))
                    item-ext.des-gr-ref[i-ct] = TRIM(ENTRY(2,c-des-refer,"-")). 
          END.
       END.
       ELSE 
           ASSIGN item-ext.fator-conv = fi-fator-conv.
    END.

    IF ITEM.peso-liquido <> 0 THEN
       ASSIGN item.peso-bruto = item.peso-liquido.
    
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
    disable {&list-4} with frame {&frame-name}.

    ASSIGN item.peso-liquido:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-fator-conv:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    /*
    ASSIGN cb-corte-comerc2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-perc2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           cb-corte-comerc3:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-perc3:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
    */
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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN fi-cod-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
           fi-cod-rlgp:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = '0'
           fi-largura:SCREEN-VALUE IN FRAME {&FRAME-NAME}     = '0'
           fi-margem-pcp:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = '0'
           fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = '0'
           cb-corte-comerc1:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''
           fi-perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = '0'
           cb-corte-comerc2:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''
           fi-perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = '0'
           cb-corte-comerc3:LIST-ITEMS IN FRAME {&FRAME-NAME} = ''
           fi-perc3:SCREEN-VALUE IN FRAME {&FRAME-NAME}       = '0'
           to-indigo:SCREEN-VALUE IN FRAME {&FRAME-NAME}      = 'NO'
           sl-des-refer:LIST-ITEMS = ''.

    IF AVAIL ITEM THEN DO:
       IF (ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59) /* OR
           ITEM.ge-codigo = 70 OR
           ITEM.ge-codigo = 13 */ THEN DO:
          IF ITEM.un = "m" THEN DO:
             ASSIGN fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '1'.
          END.
          ELSE DO:
             FIND b-item WHERE b-item.it-codigo = SUBSTR(ITEM.it-codigo,1,5) + "0" NO-LOCK NO-ERROR.
             IF AVAIL b-item THEN DO:
                IF b-item.peso-liquido <> 0 THEN
                   ASSIGN fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(DEC(1 / b-item.peso-liquido)).
                ELSE
                   fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '1'.
             END.
             ELSE DO:
                MESSAGE "N∆o foi poss°vel encontrar o Item cru, correspondente ao Item " + ITEM.it-codigo + 
                        ", para obter o peso" SKIP
                        "Foi usado o valor 1 para o fator de convers∆o."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                ASSIGN fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '1'.
             END.
          END.
       END.
       FIND item-ext WHERE
            item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
       IF AVAIL item-ext THEN DO.
          FIND composi WHERE
               composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.

          ASSIGN fi-cod-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item-ext.cod-composi
                 fi-desc-composi:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL composi
                                                                       THEN composi.descricao
                                                                       ELSE ?
                 fi-cod-rlgp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-ext.cod-rlgp)
                 fi-largura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-ext.largura)
                 fi-margem-pcp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-ext.margem-pcp)
                 fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-ext.fator-conv)
                 to-indigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-ext.indigo)
                 cb-corte-comerc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item-ext.corte[1] 
                 fi-perc1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-ext.perc-corte[1]) 
                 cb-corte-comerc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item-ext.corte[2] 
                 fi-perc2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-ext.perc-corte[2]) 
                 cb-corte-comerc3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item-ext.corte[3] 
                 fi-perc3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(item-ext.perc-corte[3]).

          ASSIGN c-lst-refer = ''.
          DO i-ct = 1 TO EXTENT(item-ext.cod-gr-ref).
              IF item-ext.cod-gr-ref[i-ct] <> '' THEN
                 ASSIGN c-lst-refer = IF c-lst-refer = ''
                                      THEN item-ext.cod-gr-ref[i-ct] + " - " + item-ext.des-gr-ref[i-ct]
                                      ELSE c-lst-refer + ',' + 
                                           item-ext.cod-gr-ref[i-ct] + " - " + item-ext.des-gr-ref[i-ct].
          END.
          ASSIGN sl-des-refer:LIST-ITEMS = c-lst-refer.
       END.
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
    
    /*
    IF (ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59) THEN
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    */

    ASSIGN item.peso-liquido:SENSITIVE IN FRAME {&FRAME-NAME} = NO. /* Apenas para atender o DDK */

    IF (ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59) AND ITEM.un = "m" THEN DO: 
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
       ASSIGN item.peso-liquido:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
    END.
    IF (ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59) AND ITEM.un = "kg" THEN DO: 
       ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
       ASSIGN fi-fator-conv:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              item.peso-liquido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
    END.
    IF (ITEM.ge-codigo = 12 OR ITEM.ge-codigo = 13 OR ITEM.ge-codigo = 70) AND ITEM.un = "m" THEN DO: 
        ASSIGN item.peso-liquido:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               fi-fator-conv:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
    END.
    IF (ITEM.ge-codigo = 12 OR ITEM.ge-codigo = 13 OR ITEM.ge-codigo = 70) AND ITEM.un = "kg" THEN DO: 
        ASSIGN fi-fator-conv:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               item.peso-liquido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "1".
    END.
    IF (ITEM.ge-codigo = 12 OR ITEM.ge-codigo = 13 OR ITEM.ge-codigo = 70) AND ITEM.un <> "m" AND item.un <> "kg" THEN DO: 
        ASSIGN fi-fator-conv:SENSITIVE IN FRAME {&FRAME-NAME} = YES
               item.peso-liquido:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    END.

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
    
    
    IF (ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59) THEN DO:
       IF INPUT FRAME {&FRAME-NAME} fi-cod-composi <> '' THEN DO.
          FIND composi WHERE
               composi.cod-composi = INPUT FRAME {&FRAME-NAME} fi-cod-composi NO-LOCK NO-ERROR.
       
          IF NOT AVAIL composi THEN DO.
             MESSAGE 'Composiá∆o n∆o Cadastrada...' VIEW-AS ALERT-BOX.
             APPLY 'ENTRY' TO fi-cod-composi.
             RETURN 'ADM-ERROR':U. 
          END.
       END.
       
       IF INPUT FRAME {&FRAME-NAME} fi-cod-rlgp <> 0 AND
          (INPUT FRAME {&FRAME-NAME} fi-cod-rlgp < 1 or
           INPUT FRAME {&FRAME-NAME} fi-cod-rlgp > 4) THEN DO:
           MESSAGE "C¢digo de Recomendaá∆o de Lavagem deve estar entre 1 e 4." VIEW-AS ALERT-BOX.
           APPLY 'ENTRY' TO fi-cod-rlgp.
           RETURN 'ADM-ERROR':U. 
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-verifica V-table-Win 
PROCEDURE pi-verifica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN de-perc = fi-perc1:INPUT-VALUE IN FRAME {&FRAME-NAME} +
                    fi-perc2:INPUT-VALUE IN FRAME {&FRAME-NAME} + 
                    fi-perc3:INPUT-VALUE IN FRAME {&FRAME-NAME}.
   IF de-perc > 100 THEN DO.
      MESSAGE 'Percentual n∆o pode ultrapassar 100%'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN 'ADM-ERROR'.
   END.

   IF de-perc < 100 THEN
      RETURN 'NOK'.

   RETURN 'OK'.

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
  {src/adm/template/snd-list.i "item"}

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

