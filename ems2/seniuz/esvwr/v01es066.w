&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
{include/i-prgvrs.i V02ES066 2.04.00.000}

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
DEF VAR v-row-parent     AS ROWID NO-UNDO.
DEF VAR c-container      AS CHAR.
DEF VAR i-ct             AS INT.
DEF VAR c-lst-mistura    AS CHAR.
DEF VAR i-fd-bancada     AS INT.

/* Variaveis da rotina do WORD */
DEF VAR c-arq-modelo     AS CHAR.  
DEF VAR c-arq-gerado-doc AS CHAR.
DEF VAR ChWord           AS COM-HANDLE NO-UNDO.         
DEF VAR ChDoc            AS COM-HANDLE NO-UNDO.


/* Vari†veis do Report Builder */
DEF VAR RB-REPORT-LIBRARY    AS CHARACTER INITIAL "".
DEF VAR RB-REPORT-NAME       AS CHARACTER INITIAL "".
DEF VAR RB-DB-CONNECTION     AS CHARACTER INITIAL "".
DEF VAR RB-INCLUDE-RECORDS   AS CHARACTER INITIAL "".
DEF VAR RB-FILTER            AS CHARACTER INITIAL "".
DEF VAR RB-MEMO-FILE         AS CHARACTER INITIAL "".
DEF VAR RB-PRINT-DESTINATION AS CHARACTER INITIAL "".
DEF VAR RB-PRINTER-NAME      AS CHARACTER INITIAL "".
DEF VAR RB-PRINTER-PORT      AS CHARACTER INITIAL "".
DEF VAR RB-OUTPUT-FILE       AS CHARACTER INITIAL "".
DEF VAR RB-NUMBER-COPIES     AS INTEGER   INITIAL 1.
DEF VAR RB-BEGIN-PAGE        AS INTEGER   INITIAL 0.
DEF VAR RB-END-PAGE          AS INTEGER   INITIAL 0.
DEF VAR RB-TEST-PATTERN      AS LOGICAL   INITIAL NO.
DEF VAR RB-WINDOW-TITLE      AS CHARACTER INITIAL "".
DEF VAR RB-DISPLAY-ERRORS    AS LOGICAL   INITIAL YES.
DEF VAR RB-DISPLAY-STATUS    AS LOGICAL   INITIAL YES.
DEF VAR RB-NO-WAIT           AS LOGICAL   INITIAL NO.
DEF VAR RB-OTHER-PARAMETERS  AS CHARACTER INITIAL "".
DEF VAR RB-STATUS-FILE       AS CHARACTER INITIAL "".

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
&Scoped-define EXTERNAL-TABLES mp-mistura
&Scoped-define FIRST-EXTERNAL-TABLE mp-mistura


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mp-mistura.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mp-mistura.dt-mistura mp-mistura.qtd-bancadas ~
mp-mistura.tp-mistura mp-mistura.qtd-fardos 
&Scoped-define ENABLED-TABLES mp-mistura
&Scoped-define FIRST-ENABLED-TABLE mp-mistura
&Scoped-Define ENABLED-OBJECTS rt-key 
&Scoped-Define DISPLAYED-FIELDS mp-mistura.nr-mistura mp-mistura.dt-mistura ~
mp-mistura.qtd-bancadas mp-mistura.tp-mistura mp-mistura.qtd-fardos 
&Scoped-define DISPLAYED-TABLES mp-mistura
&Scoped-define FIRST-DISPLAYED-TABLE mp-mistura


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS mp-mistura.nr-mistura 
&Scoped-define List-6 mp-mistura.dt-mistura mp-mistura.qtd-bancadas ~
mp-mistura.tp-mistura mp-mistura.qtd-fardos bt-processa 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
nr-mistura|y|y|espec.mp-mistura.nr-mistura
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "nr-mistura",
     Keys-Supplied = "nr-mistura"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-processa 
     IMAGE-UP FILE "image/im-plin.bmp":U
     LABEL "Button 7" 
     SIZE 4.29 BY 1.13 TOOLTIP "Carrega Fardos por Padr∆o".

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 2.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     mp-mistura.nr-mistura AT ROW 1.25 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     mp-mistura.dt-mistura AT ROW 2.25 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     mp-mistura.qtd-bancadas AT ROW 1.25 COL 42.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     mp-mistura.tp-mistura AT ROW 2.25 COL 42 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          DROP-DOWN-LIST
          SIZE 23.43 BY 1
     mp-mistura.qtd-fardos AT ROW 2.25 COL 76 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .88
     bt-processa AT ROW 2.08 COL 84
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.mp-mistura
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
         HEIGHT             = 2.63
         WIDTH              = 88.57.
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

/* SETTINGS FOR BUTTON bt-processa IN FRAME f-main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR FILL-IN mp-mistura.dt-mistura IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN mp-mistura.nr-mistura IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN mp-mistura.qtd-bancadas IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN mp-mistura.qtd-fardos IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR COMBO-BOX mp-mistura.tp-mistura IN FRAME f-main
   6                                                                    */
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

&Scoped-define SELF-NAME bt-processa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processa V-table-Win
ON CHOOSE OF bt-processa IN FRAME f-main /* Button 7 */
DO:
   DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
   RUN pi-processa IN WIDGET-HANDLE(c-container) (INPUT mp-mistura.qtd-bancadas:INPUT-VALUE,
                                                  INPUT mp-mistura.qtd-fardos:INPUT-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mp-mistura.tp-mistura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mp-mistura.tp-mistura V-table-Win
ON VALUE-CHANGED OF mp-mistura.tp-mistura IN FRAME f-main /* Tipos Mistura */
DO:
   ASSIGN i-ct = LOOKUP(mp-mistura.tp-mistura:SCREEN-VALUE,mp-mistura.tp-mistura:LIST-ITEMS).
   ASSIGN mp-mistura.qtd-fardos:SCREEN-VALUE = STRING(mp-param.fd-tp-mistura[i-ct]).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'nr-mistura':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = mp-mistura
           &WHERE = "WHERE mp-mistura.nr-mistura eq INTEGER(key-value)"
       }
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  {src/adm/template/row-list.i "mp-mistura"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mp-mistura"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
       
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
    FIND FIRST mp-param NO-LOCK NO-ERROR.
    IF AVAIL mp-param THEN DO.
       ASSIGN mp-mistura.qtd-bancadas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF AVAIL mp-param
                                                                            THEN STRING(mp-param.qt-bancadas)
                                                                            ELSE ''.
    
       ASSIGN c-lst-mistura = ''.
       DO i-ct = 1 TO EXTENT(mp-param.tp-mistura).
          IF mp-param.tp-mistura[i-ct] <> '' THEN
             ASSIGN c-lst-mistura = IF c-lst-mistura = ''
                                    THEN mp-param.tp-mistura[i-ct] 
                                    ELSE c-lst-mistura + ',' + mp-param.tp-mistura[i-ct].
       END.
       ASSIGN mp-mistura.tp-mistura:LIST-ITEMS = c-lst-mistura
              mp-mistura.tp-mistura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mp-param.tp-mistura[2]. 

       APPLY 'value-changed' TO mp-mistura.tp-mistura.
    END.

    ASSIGN mp-mistura.nr-mistura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(NEXT-VALUE(seq-mistura))
           mp-mistura.dt-mistura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).
  

    RUN pi-mostra IN WIDGET-HANDLE(c-container) (INPUT mp-mistura.nr-mistura:INPUT-VALUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry V-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN bt-processa:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
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
    RUN pi-validate.
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    
    RUN pi-grava IN WIDGET-HANDLE(c-container) (INPUT mp-mistura.nr-mistura).

    ASSIGN adm-new-record = NO.

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
  ASSIGN bt-processa:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.

  ASSIGN mp-mistura.nr-mistura:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(NEXT-VALUE(seq-mistura))
         mp-mistura.dt-mistura:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  APPLY 'entry' TO mp-mistura.dt-mistura.
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
    
    IF AVAIL mp-mistura THEN DO.
       FIND FIRST mp-param NO-LOCK NO-ERROR.
       ASSIGN c-lst-mistura = ''.
       IF AVAIL mp-param THEN DO.
         DO i-ct = 1 TO EXTENT(mp-param.tp-mistura).
            IF mp-param.tp-mistura[i-ct] <> '' THEN
               ASSIGN c-lst-mistura = IF c-lst-mistura = ''
                                      THEN mp-param.tp-mistura[i-ct] 
                                      ELSE c-lst-mistura + ',' + mp-param.tp-mistura[i-ct].
         END.
         ASSIGN mp-mistura.tp-mistura:LIST-ITEMS IN FRAME {&FRAME-NAME} = c-lst-mistura.
       END.
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    IF AVAIL mp-mistura THEN DO.
       RUN pi-habilita IN WIDGET-HANDLE(c-container) (INPUT IF mp-mistura.situacao = 1 THEN YES ELSE NO).

       IF NOT adm-new-record THEN
          RUN pi-mostra IN WIDGET-HANDLE(c-container) (INPUT mp-mistura.nr-mistura).
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

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-distribuicao V-table-Win 
PROCEDURE pi-imp-distribuicao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /* Abertura e Criaá∆o de Arquivo do Word */
 ASSIGN c-arq-modelo     = SEARCH("modelos/distribuiá∆o.doc").
 ASSIGN c-arq-gerado-doc = SESSION:TEMP-DIRECTORY + "distribuiá∆o.doc".

 OS-COPY VALUE(c-arq-modelo) VALUE(c-arq-gerado-doc).

 CREATE "Word.Application":U chWord NO-ERROR.
 IF ChWord = ? THEN DO:
    MESSAGE "O Aplicativo WORD n∆o foi encontrado." SKIP
            "N∆o Ç possivel a execuá∆o do programa."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
 END.

 ASSIGN ChWord:VISIBLE = FALSE  
        ChWord:DisplayAlerts = FALSE
        ChDoc = ChWord:Documents:OPEN(c-arq-gerado-doc) NO-ERROR.

 /* Rotina para Gravar Campos de Controle Criados no Documento Word */
 chDoc:FormFields:ITEM("data"):RESULT      = STRING(TODAY, "99/99/9999") NO-ERROR.
 chDoc:FormFields:ITEM("m_numero"):RESULT  = STRING(mp-mistura.nr-mistura,">>,>>9,999") NO-ERROR.
 chDoc:FormFields:ITEM("m_tipo"):RESULT    = mp-mistura.tp-mistura NO-ERROR.
 chDoc:FormFields:ITEM("m_data"):RESULT    = STRING(mp-mistura.dt-mistura, "99/99/9999") NO-ERROR.
 chDoc:FormFields:ITEM("f_bancada"):RESULT = STRING(mp-mistura.qtd-fardos, "9999") NO-ERROR.

 ChWord:SELECTION:MoveDown(5,11).   /* Move 10 Linhas para baixo  */

 /* Rotina para Imprimir Detalhes do Documento */
 FOR EACH mp-distribuicao WHERE
          mp-distribuicao.nr-mistura = mp-mistura.nr-mistura NO-LOCK.
     ChWord:SELECTION:TypeText (mp-distribuicao.padrao).
     ChWord:SELECTION:MoveRight.
     ChWord:SELECTION:TypeText (mp-distribuicao.codificacao).
     ChWord:SELECTION:MoveDown(5,1).   /* Move 1 Linhas para baixo  */
     ChWord:SELECTION:MoveLeft.
 END.
 
 ChDoc:PrintOut(). /* Impress∆o do Documento */ 

 /* Fecha Documento */
 chDoc:CLOSE().
 RELEASE OBJECT chDoc NO-ERROR.
 chWord:QUIT().
 RELEASE OBJECT chWord NO-ERROR.

 OS-DELETE VALUE(c-arq-gerado-doc).                      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-requisicao V-table-Win 
PROCEDURE pi-imp-requisicao :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR de-total AS INT.

 /* Abertura e Criaá∆o de Arquivo do Word */
 ASSIGN c-arq-modelo     = SEARCH("modelos/requisiá∆o.doc").
 ASSIGN c-arq-gerado-doc = SESSION:TEMP-DIRECTORY + "requisiá∆o.doc".

 OS-COPY VALUE(c-arq-modelo) VALUE(c-arq-gerado-doc).

 CREATE "Word.Application":U chWord NO-ERROR.
 IF ChWord = ? THEN DO:
    MESSAGE "O Aplicativo WORD n∆o foi encontrado." SKIP
            "N∆o Ç possivel a execuá∆o do programa."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
 END.

 ASSIGN ChWord:VISIBLE = FALSE  
        ChWord:DisplayAlerts = FALSE
        ChDoc = ChWord:Documents:OPEN(c-arq-gerado-doc) NO-ERROR.

 ChWord:SELECTION:MoveDown(5,8).   /* Move 8 Linhas para baixo  */

 /* Rotina para Imprimir Detalhes do Documento */
 ASSIGN de-total = 0.
 FOR EACH mp-comp-mistura WHERE
          mp-comp-mistura.nr-mistura = mp-mistura.nr-mistura NO-LOCK.
     ChWord:SELECTION:TypeText (mp-comp-mistura.padrao).
     ChWord:SELECTION:MoveRight.
     ChWord:SELECTION:TypeText (mp-comp-mistura.codificacao).
     ChWord:SELECTION:MoveRight.
     ChWord:SELECTION:TypeText (STRING(mp-comp-mistura.qtd-fd-req, ">>>9")).
     ChWord:SELECTION:MoveDown(5,1).   /* Move 1 Linhas para baixo  */
     ChWord:SELECTION:MoveLeft.
     ChWord:SELECTION:MoveLeft.
     ASSIGN de-total = de-total + mp-comp-mistura.qtd-fd-req.
 END.

 /* Rotina para Gravar Campos de Controle Criados no Documento Word */
 chDoc:FormFields:ITEM("data"):RESULT     = STRING(TODAY, "99/99/9999") NO-ERROR.
 chDoc:FormFields:ITEM("m_numero"):RESULT = STRING(mp-mistura.nr-mistura,">>9,999") NO-ERROR.
 chDoc:FormFields:ITEM("m_data"):RESULT   = STRING(mp-mistura.dt-mistura, "99/99/9999") NO-ERROR.
 chDoc:FormFields:ITEM("m_tipo"):RESULT   = mp-mistura.tp-mistura NO-ERROR.
 chDoc:FormFields:ITEM("total"):RESULT    = STRING(de-total, ">>>>9") NO-ERROR.


 ChDoc:PrintOut(). /* Impress∆o do Documento */

  /* Fecha Documento */
 chDoc:CLOSE().
 RELEASE OBJECT chDoc NO-ERROR.
 chWord:QUIT().
 RELEASE OBJECT chWord NO-ERROR.

 OS-DELETE VALUE(c-arq-gerado-doc).                      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime V-table-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN pi-imp-requisicao.
   IF ChWord <> ? THEN
      RUN pi-imp-distribuicao.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-lixo V-table-Win 
PROCEDURE pi-lixo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DEF VAR c-arq-modelo      AS CHAR FORMAT "x(30)".         
DEF VAR c-arq-gerado-doc  AS CHAR FORMAT "x(30)".         
DEF VAR ChWord            AS COM-HANDLE.         
DEF VAR ChDoc             AS COM-HANDLE.

DEF VAR Tot-reg           AS INTEGER.

/******************************************************************************/


ASSIGN c-arq-email = SESSION:TEMP-DIRECTORY + tt-ped-venda.no-ab-reppri + ".txt".


FOR EACH gr-cli
   WHERE gr-cli.cod-gr-cli >= 20
     AND gr-cli.cod-gr-cli <= 20
     NO-LOCK
     BREAK BY gr-cli.cod-gr-cli.

    IF FIRST-OF (gr-cli.cod-gr-cli) THEN DO:
        ASSIGN Tot-reg = 0. 

        RUN PI-GERAR-PEDIDO.
        RUN PI-GRAVAR-CABECALHO.
    END.

   FOR EACH emitente
     WHERE emitente.cod-gr-cli = gr-cli.cod-gr-cli
       AND emitente.cod-emitente <= 500
       NO-LOCK BY emitente.cod-gr-cli.

       RUN PI-GRAVAR-DETALHE.

       ASSIGN Tot-reg = Tot-reg + 1. 
   END.

   IF LAST-OF (gr-cli.cod-gr-cli) THEN DO:

        RUN PI-GRAVAR-RODAPE.

        RUN PI-ENCERRAR-PEDIDO.

   END.


END.

MESSAGE "Processo conclu°do"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/******************************************************************************/
PROCEDURE PI-GERAR-PEDIDO:

ASSIGN c-arq-modelo     = SEARCH("modelos/modelo-world-02.doc").
ASSIGN c-arq-gerado-doc = "c:\temp\ped" + STRING(gr-cli.cod-gr-cli) + ".DOC".

OS-COPY VALUE(c-arq-modelo) VALUE(c-arq-gerado-doc).

CREATE "word.application" ChWord.

ASSIGN ChWord:VISIBLE = TRUE
       ChWord:DisplayAlerts = FALSE
       ChDoc = ChWord:Documents:OPEN(c-arq-gerado-doc) NO-ERROR.

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-GRAVAR-CABECALHO:

ChWord:SELECTION:FONT:NAME = "CURRIER NEW". /*Tipo de letra*/
ChWord:SELECTION:FONT:SIZE = 13.            /*Tamanho da letra*/
ChWord:SELECTION:FONT:bold = YES.           /*Negrito */
ChWord:SELECTION:FONT:Italic = YES.         /*It†lico*/       
ChWord:SELECTION:TypeText ("XXX Formul†rio modelo para teste de impress∆o XXX"). /*Testo*/

chDoc:FormFields:ITEM("cod_gr_cli"):RESULT  = string(gr-cli.cod-gr-cli,"9999") NO-ERROR.
chDoc:FormFields:ITEM("cod_gr_cli2"):RESULT  = string(gr-cli.cod-gr-cli,"9999") NO-ERROR.
chDoc:FormFields:ITEM("cod_gr_cli3"):RESULT  = string(gr-cli.cod-gr-cli,"9999") NO-ERROR.
chDoc:FormFields:ITEM("descricao"):RESULT   = gr-cli.descricao NO-ERROR.
chDoc:FormFields:ITEM("lim_credito"):RESULT = gr-cli.lim-credito NO-ERROR.

ChWord:SELECTION:MoveDown(5,18).

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-GRAVAR-DETALHE:

ChWord:SELECTION:InsertRows.
ChWord:SELECTION:Collapse.

ChWord:SELECTION:TypeText (STRING(emitente.cod-emitente,"999999")).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (emitente.nome-emi).
ChWord:SELECTION:MoveRight.
ChWord:SELECTION:TypeText (STRING (emitente.data-implant,"99/99/9999")).

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-GRAVAR-RODAPE:

chDoc:FormFields:ITEM("tot_reg"):RESULT = STRING (tot-reg,"9999") NO-ERROR.

END PROCEDURE.

/******************************************************************************/
PROCEDURE PI-ENCERRAR-PEDIDO:

ChDoc:SAVE().

RELEASE OBJECT ChDoc NO-ERROR.
ChWord:VISIBLE = true NO-ERROR.

/* ChWord:QUIT().                  */
/* RELEASE OBJECT ChWord NO-ERROR. */
/*                                                         */
/* RUN esapi/gera-pdf.p (INPUT c-arq-gerado-doc) NO-ERROR. */
/*                                                         */
/* OS-DELETE VALUE(c-arq-gerado-doc).                      */

END PROCEDURE.




*/
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
    
    RUN pi-busca IN WIDGET-HANDLE(c-container) (OUTPUT i-fd-bancada).
    IF i-fd-bancada <> INPUT FRAME {&FRAME-NAME} mp-mistura.qtd-fardos THEN DO.
       MESSAGE 'Quantidade de Fardos da Distribuiá∆o Diferente da Quantidade da Mistura'
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN 'ADM-ERROR':U.                                            
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "nr-mistura" "mp-mistura" "nr-mistura"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "mp-mistura"}

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

