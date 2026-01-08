&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
{include/i-prgvrs.i V01ES022 2.04.00.000}

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
DEF VAR c-num-os     LIKE mov-man.num-os.
DEF VAR i-seq        AS INT.
DEF VAR l-ok         AS LOG.

DEF BUFFER b-mov-man FOR mov-man.

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
&Scoped-define EXTERNAL-TABLES mov-man
&Scoped-define FIRST-EXTERNAL-TABLE mov-man


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR mov-man.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mov-man.cod-setor mov-man.data-abe ~
mov-man.hora-abe mov-man.os-visada mov-man.cod-maq 
&Scoped-define ENABLED-TABLES mov-man
&Scoped-define FIRST-ENABLED-TABLE mov-man
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS mov-man.cod-area mov-man.num-os ~
mov-man.cod-setor mov-man.data-abe mov-man.hora-abe mov-man.os-visada ~
mov-man.cod-maq 
&Scoped-define DISPLAYED-TABLES mov-man
&Scoped-define FIRST-DISPLAYED-TABLE mov-man
&Scoped-Define DISPLAYED-OBJECTS fi-desc-area-mec fi-desc-set-mec ~
fi-desc-maq-mec 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS mov-man.cod-area 
&Scoped-define ADM-ASSIGN-FIELDS mov-man.num-os 

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
DEFINE VARIABLE fi-desc-area-mec AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 27.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-maq-mec AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 33.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-set-mec AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 27.72 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88.57 BY 3.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     mov-man.cod-area AT ROW 1.17 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .88
     fi-desc-area-mec AT ROW 1.17 COL 20.43 COLON-ALIGNED NO-LABEL
     mov-man.num-os AT ROW 1.17 COL 65.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
          FONT 1
     mov-man.cod-setor AT ROW 2.54 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.14 BY .88
     fi-desc-set-mec AT ROW 2.54 COL 22.29 COLON-ALIGNED NO-LABEL
     mov-man.data-abe AT ROW 3.54 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     mov-man.hora-abe AT ROW 3.54 COL 43.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-man.os-visada AT ROW 3.54 COL 61.72
          VIEW-AS TOGGLE-BOX
          SIZE 13.14 BY .88
     mov-man.cod-maq AT ROW 4.54 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     fi-desc-maq-mec AT ROW 4.54 COL 25.29 COLON-ALIGNED NO-LABEL
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 2.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.mov-man
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
         HEIGHT             = 4.83
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN mov-man.cod-area IN FRAME f-main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-desc-area-mec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-maq-mec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-set-mec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN mov-man.num-os IN FRAME f-main
   NO-ENABLE 2                                                          */
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

&Scoped-define SELF-NAME mov-man.cod-area
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-area V-table-Win
ON ENTRY OF mov-man.cod-area IN FRAME f-main /* Area */
DO:
  FIND area-mec WHERE area-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-area NO-LOCK NO-ERROR.
  IF AVAIL area-mec THEN
     ASSIGN fi-desc-area-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = area-mec.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-area V-table-Win
ON LEAVE OF mov-man.cod-area IN FRAME f-main /* Area */
DO:
  FIND area-mec WHERE area-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-area NO-LOCK NO-ERROR.
  IF AVAIL area-mec THEN DO:
     ASSIGN fi-desc-area-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = area-mec.descricao.
     ASSIGN l-ok = NO.
     DO i-seq = 1 TO 999999:
        ASSIGN c-num-os = area-mec.prefixo-os + STRING(i-seq,"999999").
        FIND b-mov-man WHERE b-mov-man.num-os = c-num-os NO-LOCK NO-ERROR.
        IF NOT AVAIL b-mov-man THEN DO:
           ASSIGN l-ok = YES.
           LEAVE.
        END.
     END.
     IF NOT l-ok THEN DO:
        MESSAGE "NÆo h  n£meros dispon¡veis para o prefixo" area-mec.prefixo-os
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'entry' TO mov-man.cod-area IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
     ELSE
        ASSIGN mov-man.num-os:SCREEN-VALUE = c-num-os.
  END.
  ELSE DO:
     MESSAGE "µrea inv lida."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO mov-man.cod-area IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-area V-table-Win
ON MOUSE-SELECT-DBLCLICK OF mov-man.cod-area IN FRAME f-main /* Area */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es002.w
                     &campo     = mov-man.cod-area
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.cod-maq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-maq V-table-Win
ON ENTRY OF mov-man.cod-maq IN FRAME f-main /* Maquina */
DO:
  FIND maq-mec WHERE maq-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-maq NO-LOCK NO-ERROR.
  IF AVAIL maq-mec THEN
     ASSIGN fi-desc-maq-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = maq-mec.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-maq V-table-Win
ON LEAVE OF mov-man.cod-maq IN FRAME f-main /* Maquina */
DO:
  FIND maq-mec WHERE maq-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-maq NO-LOCK NO-ERROR.
  IF AVAIL maq-mec THEN
     ASSIGN fi-desc-maq-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = maq-mec.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-maq V-table-Win
ON MOUSE-SELECT-DBLCLICK OF mov-man.cod-maq IN FRAME f-main /* Maquina */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es019.w
                     &campo     = mov-man.cod-maq
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.cod-setor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-setor V-table-Win
ON ENTRY OF mov-man.cod-setor IN FRAME f-main /* Setor */
DO:
  FIND set-mec WHERE set-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-setor NO-LOCK NO-ERROR.
  IF AVAIL set-mec THEN
     ASSIGN fi-desc-set-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = set-mec.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-setor V-table-Win
ON LEAVE OF mov-man.cod-setor IN FRAME f-main /* Setor */
DO:
  FIND set-mec WHERE set-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-setor NO-LOCK NO-ERROR.
  IF AVAIL set-mec THEN
     ASSIGN fi-desc-set-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = set-mec.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.cod-setor V-table-Win
ON MOUSE-SELECT-DBLCLICK OF mov-man.cod-setor IN FRAME f-main /* Setor */
DO:
  {include/zoomvar.i &prog-zoom = eszoom/z01es026.w
                     &campo     = mov-man.cod-setor
                     &campozoom = codigo}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-man.hora-abe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-man.hora-abe V-table-Win
ON LEAVE OF mov-man.hora-abe IN FRAME f-main /* Hora abertura */
DO:
  IF INPUT FRAME {&FRAME-NAME} mov-man.hora-abe = ? THEN DO.
     MESSAGE "Hora cont‚m caracteres inv lidos." VIEW-AS ALERT-BOX. 
     RETURN NO-APPLY.
  END.

  IF NOT (SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,1,1) >= "0" and
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,1,1) <= "9" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,2,1) >= "0" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,2,1) <= "9" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,3,1) >= "0" and
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,3,1) <= "9" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,4,1) >= "0" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,4,1) <= "9") THEN DO:
     MESSAGE "Hora cont‚m caracteres inv lidos." VIEW-AS ALERT-BOX. 
     RETURN NO-APPLY.
  END.
  ELSE
  IF NOT (SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,1,2) >= "00" and
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,1,2) <= "23" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,3,2) >= "00" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,3,2) <= "59")  THEN DO:
     MESSAGE "Hora deve estar entre 00:00 e 23:59." VIEW-AS ALERT-BOX. 
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  mov-man.cod-area:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  mov-man.cod-setor:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
  mov-man.cod-maq:LOAD-MOUSE-POINTER ("image/lupa.cur") IN FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "mov-man"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "mov-man"}

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
  ASSIGN mov-man.hora-abe:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "?".

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
    
    /* Ponha na pi-validate todas as valida‡äes */
    /* NÆo gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
   RUN pi-validate.
   if RETURN-VALUE = 'ADM-ERROR':U then 
      return 'ADM-ERROR':U.
    
   /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /* Todos os assignïs nÆo feitos pelo assign-record devem ser feitos aqui */  
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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL mov-man THEN DO:
       FIND area-mec WHERE area-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-area NO-LOCK NO-ERROR.
       IF AVAIL area-mec THEN
          ASSIGN fi-desc-area-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = area-mec.descricao.
       
       FIND set-mec WHERE set-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-setor NO-LOCK NO-ERROR.
       IF AVAIL set-mec THEN
          ASSIGN fi-desc-set-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = set-mec.descricao.
    
       FIND maq-mec WHERE maq-mec.codigo = INPUT FRAME {&FRAME-NAME} mov-man.cod-maq NO-LOCK NO-ERROR.
       IF AVAIL maq-mec THEN
          ASSIGN fi-desc-maq-mec:SCREEN-VALUE IN FRAME {&FRAME-NAME} = maq-mec.descricao.
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
/*------------------------------------------------------------------------------
  Purpose:Validar a viewer     
  Parameters:  <none>
  Notes: NÆo fazer assign aqui. Nesta procedure
  devem ser colocadas apenas valida‡äes, pois neste ponto do programa o registro 
  ainda nÆo foi criado.       
------------------------------------------------------------------------------*/
  {include/i-vldfrm.i} /* Valida‡Æo de dicion rio */

  IF INPUT FRAME {&FRAME-NAME} mov-man.hora-abe = ? THEN DO.
     MESSAGE "Hora cont‚m caracteres inv lidos." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO mov-man.hora-abe.
     RETURN 'ADM-ERROR'.
  END.

  IF NOT (SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,1,1) >= "0" and
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,1,1) <= "9" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,2,1) >= "0" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,2,1) <= "9" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,3,1) >= "0" and
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,3,1) <= "9" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,4,1) >= "0" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,4,1) <= "9") THEN DO:
     MESSAGE "Hora cont‚m caracteres inv lidos." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO mov-man.hora-abe.
     RETURN 'ADM-ERROR'.
  END.
  ELSE
  IF NOT (SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,1,2) >= "00" and
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,1,2) <= "23" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,3,2) >= "00" AND
          SUBSTR(INPUT FRAME {&FRAME-NAME} mov-man.hora-abe,3,2) <= "59")  THEN DO:
     MESSAGE "Hora deve estar entre 00:00 e 23:59." VIEW-AS ALERT-BOX. 
     APPLY 'entry' TO mov-man.hora-abe.
     RETURN 'ADM-ERROR'.
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
  {src/adm/template/snd-list.i "mov-man"}

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

