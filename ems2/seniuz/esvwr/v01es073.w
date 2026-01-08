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
{include/i-prgvrs.i V01ES073 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-cod-estabel AS CHAR.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.
DEF VAR c-sit AS CHAR FORMAT "x(15)".

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
&Scoped-define EXTERNAL-TABLES ob-etq-trf
&Scoped-define FIRST-EXTERNAL-TABLE ob-etq-trf


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ob-etq-trf.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ob-etq-trf.num-etiqueta 
&Scoped-define ENABLED-TABLES ob-etq-trf
&Scoped-define FIRST-ENABLED-TABLE ob-etq-trf
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS ob-etq-trf.num-trf ob-etq-trf.num-etiqueta 
&Scoped-define DISPLAYED-TABLES ob-etq-trf
&Scoped-define FIRST-DISPLAYED-TABLE ob-etq-trf
&Scoped-Define DISPLAYED-OBJECTS fi-situacao fi-desc-sit fi-it-codigo ~
fi-desc-item fi-cod-refer fi-desc-refer fi-quantidade fi-codigo-comerc ~
fi-desc-comerc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-ASSIGN-FIELDS ob-etq-trf.num-trf 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
num-trf||y|espec.ob-etq-trf.num-trf
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "num-trf"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(8)" 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-codigo-comerc AS CHARACTER FORMAT "!" 
     LABEL "Corte Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 DROP-TARGET NO-UNDO.

DEFINE VARIABLE fi-desc-comerc AS CHARACTER FORMAT "X(20)" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-sit AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 21.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-quantidade AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-situacao AS INTEGER FORMAT "-9" INITIAL 1 
     LABEL "Situaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 1.25.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 6.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ob-etq-trf.num-trf AT ROW 1.25 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     ob-etq-trf.num-etiqueta AT ROW 3 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     fi-situacao AT ROW 4 COL 21 COLON-ALIGNED
     fi-desc-sit AT ROW 4 COL 24.43 COLON-ALIGNED NO-LABEL
     fi-it-codigo AT ROW 5 COL 21 COLON-ALIGNED
     fi-desc-item AT ROW 5 COL 30.29 COLON-ALIGNED NO-LABEL
     fi-cod-refer AT ROW 6 COL 21 COLON-ALIGNED
     fi-desc-refer AT ROW 6 COL 30.29 COLON-ALIGNED NO-LABEL
     fi-quantidade AT ROW 7 COL 21 COLON-ALIGNED
     fi-codigo-comerc AT ROW 8 COL 21 COLON-ALIGNED
     fi-desc-comerc AT ROW 8 COL 24.29 COLON-ALIGNED NO-LABEL
     rt-key AT ROW 1.04 COL 1.14
     rt-mold AT ROW 2.5 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.ob-etq-trf
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
         HEIGHT             = 8.42
         WIDTH              = 80.43.
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

/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-codigo-comerc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-comerc IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-sit IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-quantidade IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-situacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etq-trf.num-trf IN FRAME f-main
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

&Scoped-define SELF-NAME ob-etq-trf.num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etq-trf.num-etiqueta V-table-Win
ON LEAVE OF ob-etq-trf.num-etiqueta IN FRAME f-main /* Num Etiqueta */
DO:
   FIND ob-etq-trf WHERE
        ob-etq-trf.cod-estabel  = c-cod-estabel AND
        ob-etq-trf.num-etiqueta = INT(INPUT FRAME {&FRAME-NAME} ob-etq-trf.num-etiqueta)
        NO-LOCK NO-ERROR.
   IF AVAIL ob-etq-trf THEN DO:
      MESSAGE "Etiqueta j† est† Incluida nesta Transformaá∆o..." 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-etq-trf.num-etiqueta.
      RETURN NO-APPLY.
   END.

   FIND ob-etiqueta WHERE 
        ob-etiqueta.cod-estabel  = c-cod-estabel AND
        ob-etiqueta.num-etiqueta = INPUT FRAME {&FRAME-NAME} ob-etq-trf.num-etiqueta
        NO-LOCK NO-ERROR.

   IF NOT AVAIL ob-etiqueta THEN DO.
      MESSAGE "Etiqueta n∆o Cadastrada ou n∆o Pertence ao Estabelecimento " c-cod-estabel  VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-etq-trf.num-etiqueta.
      RETURN NO-APPLY.
   END.
   
   IF ob-etiqueta.localiz = '700003' OR
      ob-etiqueta.localiz = '700004' THEN DO.
      MESSAGE "Etiqueta no Dep¢sito de Corte..."  VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-etq-trf.num-etiqueta.
      RETURN NO-APPLY.
   END.

   IF SUBSTR(ob-etiqueta.it-codigo,1,5) <> SUBSTR(ob-trf.it-codigo,1,5) THEN DO:
      MESSAGE "O Artigo: " + TRIM(ob-etiqueta.it-codigo) +
              ",  da Etiqueta n∆o e Igual ao da Transformaá∆o ! ! !" 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-etq-trf.num-etiqueta.
      RETURN NO-APPLY.
   END.

   IF ob-etiqueta.cod-refer <> ob-trf.cod-refer THEN DO:
      MESSAGE "A Referància: " + TRIM(ob-etiqueta.cod-refer) +
              ",  da Etiqueta n∆o e Igual ao da Transformaá∆o ! ! !" 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-etq-trf.num-etiqueta.
      RETURN NO-APPLY.
   END.
 
   IF ob-etiqueta.nr-lote <> ob-trf.nr-lote THEN DO:
      MESSAGE "O Lote: " + TRIM(ob-etiqueta.nr-lote) +
              ",  da Etiqueta n∆o e Igual ao da Transformaá∆o ! ! !" 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-etq-trf.num-etiqueta.
      RETURN NO-APPLY.
   END.

   IF ob-etiqueta.corte-comerc = ob-trf.corte-comerc THEN DO:
      MESSAGE "O Corte Comercial: " + TRIM(ob-etiqueta.corte-comerc) +
              ",  da Etiqueta Ç Igual ao da Transformaá∆o ! ! !" 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-etq-trf.num-etiqueta.
      RETURN NO-APPLY.
   END.

   {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-sit} 

   IF ob-etiqueta.situacao <> 3  THEN DO:
      MESSAGE "Situacao da Etiqueta (" + c-sit + ") n∆o permite ser Transformada..." SKIP
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO ob-etq-trf.num-etiqueta.
      RETURN NO-APPLY.
   END.

   ASSIGN fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.quantidade)
          fi-situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.situacao)
          fi-desc-sit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-sit.

   FIND corte-comerc WHERE 
        corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
   IF AVAIL corte-comerc THEN
      ASSIGN fi-codigo-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.codigo
             fi-desc-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = corte-comerc.descricao.

   FIND ITEM WHERE 
        ITEM.it-codigo = ob-etiqueta.it-codigo  NO-LOCK NO-ERROR.
   IF AVAIL item THEN 
      ASSIGN fi-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.it-codigo
             fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.

   FIND referencia WHERE 
        referencia.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.
   IF AVAIL referencia THEN
      ASSIGN fi-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = referencia.cod-refer
             fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etq-trf.num-etiqueta V-table-Win
ON MOUSE-SELECT-DBLCLICK OF ob-etq-trf.num-etiqueta IN FRAME f-main /* Num Etiqueta */
DO:
  {include/zoomvar.i &prog-zoom=eszoom\z02es049.w
                     &campo=ob-etq-trf.num-etiqueta
                     &campozoom=num-etiqueta}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etq-trf.num-etiqueta V-table-Win
ON RETURN OF ob-etq-trf.num-etiqueta IN FRAME f-main /* Num Etiqueta */
DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  ob-etq-trf.num-etiqueta:LOAD-MOUSE-POINTER("image/lupa.cur"). 

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

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "ob-etq-trf"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ob-etq-trf"}

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
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND ob-trf WHERE ROWID(ob-trf) = v-row-parent NO-LOCK NO-ERROR.
  IF AVAIL ob-trf THEN 
     ASSIGN ob-etq-trf.num-trf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-trf.num-trf).
            /*
            fi-trf-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-trf.it-codigo
            fi-trf-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-trf.cod-refer
            fi-trf-corte-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-trf.corte-comerc
            fi-trf-nr-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-trf.nr-lote.
            */

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
       RETURN  'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    FIND ob-etiqueta WHERE 
         ob-etiqueta.cod-estabel  = c-cod-estabel AND
         ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-ERROR.
    IF AVAIL ob-etiqueta THEN
       ob-etiqueta.situacao = 6. /* Etiqueta em REPROCESSO */

    FIND CURRENT ob-etq-trf SHARE-LOCK.
    ASSIGN ob-etq-trf.cod-estabel = c-cod-estabel.

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
   
    IF INT(INPUT FRAME {&frame-name} ob-etq-trf.num-etiqueta) = 0 THEN DO:
       MESSAGE "O Codigo da Etiqueta n∆o pode ser zero."  VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO ob-etq-trf.num-etiqueta.
       RETURN NO-APPLY.
    END. 

    IF fi-it-codigo:SCREEN-VALUE = '' THEN DO:
       MESSAGE "Item Invalido ! ! !"  VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO ob-etq-trf.num-etiqueta.
       RETURN NO-APPLY.
    END.

    FIND ob-etq-trf WHERE 
         ob-etq-trf.cod-estabel  = c-cod-estabel AND
         ob-etq-trf.num-etiqueta = INT(INPUT FRAME {&FRAME-NAME} ob-etq-trf.num-etiqueta)
         NO-LOCK NO-ERROR.
    IF AVAIL ob-etq-trf THEN DO:
       MESSAGE "O Codigo da Etiqueta ja foi Incluida."  VIEW-AS ALERT-BOX. 
       APPLY 'entry' TO ob-etq-trf.num-etiqueta.
       RETURN no-apply.
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
  {src/adm/template/sndkycas.i "num-trf" "ob-etq-trf" "num-trf"}

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
  {src/adm/template/snd-list.i "ob-etq-trf"}

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

