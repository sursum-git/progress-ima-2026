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
{include/i-prgvrs.i V01ES049 2.04.00.000}

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

DEF TEMP-TABLE tt-etiqueta LIKE ob-etiqueta.
DEF VAR c-results AS CHAR.
DEF VAR i-ct AS INT.

DEF VAR i-tp-embal AS INT.

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
&Scoped-define EXTERNAL-TABLES ob-etiqueta
&Scoped-define FIRST-EXTERNAL-TABLE ob-etiqueta


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ob-etiqueta.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ob-etiqueta.nr-cortes ob-etiqueta.nr-lote ~
ob-etiqueta.nuance ob-etiqueta.situacao 
&Scoped-define ENABLED-TABLES ob-etiqueta
&Scoped-define FIRST-ENABLED-TABLE ob-etiqueta
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS ob-etiqueta.cod-estabel ~
ob-etiqueta.num-etiqueta ob-etiqueta.dt-emissao ob-etiqueta.it-codigo ~
ob-etiqueta.cod-refer ob-etiqueta.peso-bruto ob-etiqueta.nr-cortes ~
ob-etiqueta.nr-lote ob-etiqueta.nuance ob-etiqueta.situacao 
&Scoped-define DISPLAYED-TABLES ob-etiqueta
&Scoped-define FIRST-DISPLAYED-TABLE ob-etiqueta
&Scoped-Define DISPLAYED-OBJECTS fi-nome-estab fi-desc-item fi-quantidade ~
fi-localiz ed-motivo 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */

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
DEFINE VARIABLE ed-motivo AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 200 SCROLLBAR-VERTICAL
     SIZE 28 BY 6.63 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 41.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-localiz AS CHARACTER FORMAT "xxx/xxx":U 
     LABEL "Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 0 NO-UNDO.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-quantidade AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 8.29 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 4.5.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 8.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ob-etiqueta.cod-estabel AT ROW 1.25 COL 16 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
     fi-nome-estab AT ROW 1.25 COL 20.29 COLON-ALIGNED NO-LABEL WIDGET-ID 24
     ob-etiqueta.num-etiqueta AT ROW 2.25 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
          FONT 6
     ob-etiqueta.dt-emissao AT ROW 2.25 COL 58.14 COLON-ALIGNED WIDGET-ID 4
          LABEL "Emiss∆o"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     ob-etiqueta.it-codigo AT ROW 3.25 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     fi-desc-item AT ROW 3.25 COL 27.43 COLON-ALIGNED NO-LABEL
     ob-etiqueta.cod-refer AT ROW 4.25 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .88
     ob-etiqueta.peso-bruto AT ROW 4.25 COL 58 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
     fi-quantidade AT ROW 6 COL 16 COLON-ALIGNED
     fi-localiz AT ROW 7 COL 16 COLON-ALIGNED
     ob-etiqueta.nr-cortes AT ROW 8 COL 16 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 5 BY .88
     ob-etiqueta.nr-lote AT ROW 9 COL 16 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 4 BY .88
          FONT 0
     ob-etiqueta.nuance AT ROW 10.04 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .88
     ob-etiqueta.situacao AT ROW 6 COL 40.29 NO-LABEL WIDGET-ID 6
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Impressa", 1,
"Em Produá∆o", 2,
"Em Estoque", 3,
"Reservada", 4,
"Faturada", 5,
"Em Reprocesso", 6,
"Consumido Corte", 7,
"Bloqueado", 8,
"Consumido", 9
          SIZE 15.14 BY 7.54
     ed-motivo AT ROW 6.88 COL 60 NO-LABEL WIDGET-ID 26
     " Motivo Alteraá∆o" VIEW-AS TEXT
          SIZE 28 BY .79 AT ROW 6 COL 60 WIDGET-ID 28
          FGCOLOR 12 FONT 6
     "Situaá∆o:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 6.04 COL 33.14 WIDGET-ID 30
     rt-key AT ROW 1 COL 1
     rt-mold AT ROW 5.75 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: espec.ob-etiqueta
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
         HEIGHT             = 13
         WIDTH              = 88.29.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ob-etiqueta.cod-estabel IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.cod-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.dt-emissao IN FRAME f-main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR EDITOR ed-motivo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-localiz IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-quantidade IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.it-codigo IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.num-etiqueta IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.peso-bruto IN FRAME f-main
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

&Scoped-define SELF-NAME fi-localiz
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-localiz V-table-Win
ON VALUE-CHANGED OF fi-localiz IN FRAME f-main /* Localizaá∆o */
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-localiz.

  IF fi-localiz <> '' AND
     (SUBSTR(fi-localiz,LENGTH(fi-localiz),1) < '0' OR
      SUBSTR(fi-localiz,LENGTH(fi-localiz),1) > '9') THEN DO.
      APPLY 'backspace' TO SELF.
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
  {src/adm/template/row-list.i "ob-etiqueta"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ob-etiqueta"}

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
    

    ASSIGN INPUT FRAME {&FRAME-NAME} fi-localiz
           INPUT FRAME {&FRAME-NAME} fi-quantidade
           INPUT FRAME {&FRAME-NAME} ed-motivo.

    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    RUN pi-validate.
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.
    

    EMPTY TEMP-TABLE tt-etiqueta.
    CREATE tt-etiqueta.
    BUFFER-COPY ob-etiqueta TO tt-etiqueta.


    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN  
       RETURN 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN ob-etiqueta.localiz = fi-localiz.

    IF fi-quantidade:SENSITIVE = YES THEN
       ASSIGN ob-etiqueta.quantidade = fi-quantidade.

    BUFFER-COMPARE ob-etiqueta TO tt-etiqueta SAVE RESULT IN c-results.
    IF c-results <> '' THEN DO.
       DO i-ct = 1 TO NUM-ENTRIES(c-results).
          CASE ENTRY(i-ct,c-results).
              WHEN 'situacao' THEN DO.
                  CREATE movto-etq.
                  ASSIGN movto-etq.cod-estabel = ob-etiqueta.cod-estabel
                         movto-etq.dt-trans = TODAY
                         movto-etq.esp-docto = "ALT"
                         movto-etq.nro-docto = STRING(ob-etiqueta.nr-ob)
                         movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
                         movto-etq.quantidade = ob-etiqueta.quantidade
                         movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                                            "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                                            "SITUACAO Anterior " + STRING(tt-etiqueta.situacao,"9") +  FILL(" ",10) +
                                            "Programa: essp0135".
              END.
              WHEN 'quantidade' THEN DO.
                   CREATE movto-etq.
                   ASSIGN movto-etq.cod-estabel = ob-etiqueta.cod-estabel
                          movto-etq.dt-trans = TODAY
                          movto-etq.esp-docto = "ALT"
                          movto-etq.nro-docto = STRING(ob-etiqueta.nr-ob)
                          movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
                          movto-etq.quantidade = ob-etiqueta.quantidade
                          movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                                             "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                                             "Qtde Anterior " + TRIM(STRING(tt-etiqueta.quantidade,">>>,>>9.99")) +  FILL(" ",10) +
                                             "Programa: essp0135"+ FILL(" ",10) + 
                                             ed-motivo.

              END.
              WHEN 'nr-lote' THEN DO.
                  FIND CURRENT ob-etiqueta SHARE-LOCK NO-ERROR.
                  CASE ob-etiqueta.nr-lote.
                     WHEN 'RP' THEN
                        ASSIGN ob-etiqueta.cod-qualid = 'B'.
                     WHEN 'RD' THEN
                        ASSIGN ob-etiqueta.cod-qualid = 'D'.
                  END.

                  FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.

                  CREATE movto-etq.
                  ASSIGN movto-etq.cod-estabel = ob-etiqueta.cod-estabel
                         movto-etq.dt-trans = TODAY
                         movto-etq.esp-docto = "ALT"
                         movto-etq.nro-docto = STRING(ob-etiqueta.nr-ob)
                         movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
                         movto-etq.quantidade = ob-etiqueta.quantidade
                         movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                                            "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                                            "Lote Anterior: " + tt-etiqueta.nr-lote +  FILL(" ",10) +
                                            "Programa: essp0135" + FILL(" ",10) + 
                                             ed-motivo.
              END.
              WHEN 'localiz' THEN DO.
                  CREATE movto-etq.
                  ASSIGN movto-etq.cod-estabel = ob-etiqueta.cod-estabel
                         movto-etq.dt-trans = TODAY
                         movto-etq.esp-docto = "ALT"
                         movto-etq.nro-docto = STRING(ob-etiqueta.nr-ob)
                         movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
                         movto-etq.quantidade = ob-etiqueta.quantidade
                         movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                                            "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                                            "Localizaá∆o Anterior " + tt-etiqueta.localiz +  FILL(" ",10) +
                                            "Programa: essp0135" + FILL(" ",10) + 
                                            ed-motivo.
              END.
          END CASE.
       END.
    END.

    /* 
    
  IF INPUT FRAME {&FRAME-NAME} rs-situacao <> ob-etiqueta.situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} OR 
     INPUT FRAME {&FRAME-NAME} fi-corte-comerc <> ob-etiqueta.corte-comerc OR 
     INPUT FRAME {&FRAME-NAME} ob-etiqueta.dt-emissao <> ob-etiqueta.dt-emissao OR 
     INPUT FRAME {&FRAME-NAME} fi-localizacao <> ob-etiqueta.localizacao THEN DO:
     IF INPUT FRAME {&FRAME-NAME} rs-situacao <> "5" THEN DO: /* Elmina romaneio */
        FIND ped-item-rom WHERE ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
        IF AVAIL ped-item-rom THEN
           DELETE ped-item-rom.
     END.
     
     FIND CURRENT ob-etiqueta EXCLUSIVE-LOCK NO-ERROR.
     /*
     FIND corte-comerc WHERE corte-comerc.codigo = fi-corte-comerc:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       NO-LOCK. */
     ASSIGN ob-etiqueta.situacao = INT(INPUT FRAME {&FRAME-NAME} rs-situacao)
            ob-etiqueta.situacao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = INPUT FRAME {&FRAME-NAME} rs-situacao
            ob-etiqueta.dt-emissao = INPUT FRAME {&FRAME-NAME} ob-etiqueta.dt-emissao
            ob-etiqueta.localizacao = fi-localizacao:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            ob-etiqueta.quantidade = INPUT FRAME {&FRAME-NAME} ob-etiqueta.quantidade
            ob-etiqueta.peso-bruto = DEC(INPUT FRAME {&FRAME-NAME} fi-peso-bruto)
            SUBSTR(ob-etiqueta.char-1,50,200) = SUBSTR(ob-etiqueta.char-1,50,200) + ed-motivo:SCREEN-VALUE.
  END.
    
    */

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
    
    ASSIGN fi-quantidade:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           fi-localiz:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           ed-motivo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

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

    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAIL ob-etiqueta THEN DO.
       FIND item WHERE
            item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item THEN
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
    
       FIND estabelec WHERE
            estabelec.cod-estabel = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAIL estabelec THEN
          ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.

       ASSIGN fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.quantidade)
              fi-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.localizacao.
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

    ASSIGN fi-localiz:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           ed-motivo:SENSITIVE IN FRAME {&FRAME-NAME} = YES
           fi-quantidade:SENSITIVE = NO.

    ASSIGN ob-etiqueta.situacao:SENSITIVE = NO.

    IF c-seg-usuario = 'super' OR
       c-seg-usuario = 'apanzera' OR
       c-seg-usuario = 'esoares' THEN DO.
       ASSIGN fi-quantidade:SENSITIVE = YES
              ob-etiqueta.situacao:SENSITIVE = YES.

       APPLY 'ENTRY' TO fi-quantidade.
    END.
    ELSE
       APPLY 'ENTRY' TO fi-localiz.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-deleta V-table-Win 
PROCEDURE pi-deleta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND CURRENT ob-etiqueta NO-ERROR.

    CREATE movto-etq.
    ASSIGN movto-etq.cod-estabel = ob-etiqueta.cod-estabel
           movto-etq.dt-trans = TODAY
           movto-etq.esp-docto = "DEL"
           movto-etq.nro-docto = STRING(ob-etiqueta.nr-ob)
           movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
           movto-etq.quantidade = ob-etiqueta.quantidade
           movto-etq.char-1 = "Usuario: " + c-seg-usuario + FILL(" ",10) +
                              "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                              "Etiqueta ELIMINADA" +  FILL(" ",10) +
                              "Programa: essp0135".

    DELETE ob-etiqueta.
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
    
    /*
    ASSIGN i-tp-embal = 1.

    FIND corte-comerc WHERE
         corte-comerc.compr-min <= INPUT FRAME {&FRAME-NAME} ob-etiqueta.quantidade AND
         corte-comerc.compr-max >= INPUT FRAME {&FRAME-NAME} ob-etiqueta.quantidade AND
         corte-comerc.tp-embalag = i-tp-embal AND 
         corte-comerc.un = item.un NO-LOCK NO-ERROR.

    IF NOT AVAIL corte-comerc THEN DO.
       MESSAGE "Metragem informada n∆o pertece a nenhum Corte Cadastrado..." 
           VIEW-AS ALERT-BOX.
       APPLY 'entry' TO ob-etiqueta.quantidade.
       RETURN 'ADM-ERROR'.
    END.
    */

    IF fi-localiz <> '' THEN DO.
       FIND ob-localiz WHERE
            ob-localiz.cod-localiz = fi-localiz
            NO-LOCK NO-ERROR.

        IF NOT AVAIL ob-localiz THEN DO.
           MESSAGE "Localizaá∆o Informada NAO est† Cadastrada..."
                VIEW-AS ALERT-BOX.
           APPLY 'entry' TO fi-localiz.
           RETURN 'ADM-ERROR'.
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
  {src/adm/template/snd-list.i "ob-etiqueta"}

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

