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

DEF BUFFER b-ob-etiqueta FOR ob-etiqueta.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEF TEMP-TABLE tt-etiqueta LIKE ob-etiqueta.
DEF VAR c-results AS CHAR.
DEF VAR c-nr-pedcli AS CHAR.
DEF VAR i-ct AS INT.

DEF VAR i-tp-embal AS INT.
DEF VAR i-num-rolo-imp LIKE ob-etiqueta.num-rolo-imp.

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
&Scoped-Define ENABLED-FIELDS ob-etiqueta.quantidade ob-etiqueta.nr-cortes ~
ob-etiqueta.nr-lote ob-etiqueta.nuance ob-etiqueta.situacao ~
ob-etiqueta.acondic 
&Scoped-define ENABLED-TABLES ob-etiqueta
&Scoped-define FIRST-ENABLED-TABLE ob-etiqueta
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS ob-etiqueta.cod-estabel ~
ob-etiqueta.num-etiqueta ob-etiqueta.dt-emissao ob-etiqueta.it-codigo ~
ob-etiqueta.cod-refer ob-etiqueta.nr-container ob-etiqueta.num-rolo-imp ~
ob-etiqueta.cod-depos ob-etiqueta.quantidade ob-etiqueta.nr-cortes ~
ob-etiqueta.nr-lote ob-etiqueta.nuance ob-etiqueta.peso-bruto ~
ob-etiqueta.situacao ob-etiqueta.acondic 
&Scoped-define DISPLAYED-TABLES ob-etiqueta
&Scoped-define FIRST-DISPLAYED-TABLE ob-etiqueta
&Scoped-Define DISPLAYED-OBJECTS fi-nome-estab fi-desc-item fi-localiz 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define ADM-CREATE-FIELDS ob-etiqueta.cod-estabel ~
ob-etiqueta.it-codigo ob-etiqueta.cod-refer ob-etiqueta.nr-container ~
ob-etiqueta.peso-bruto 
&Scoped-define ADM-ASSIGN-FIELDS ob-etiqueta.cod-estabel ~
ob-etiqueta.num-etiqueta ob-etiqueta.dt-emissao ob-etiqueta.it-codigo ~
ob-etiqueta.cod-refer ob-etiqueta.nr-container ob-etiqueta.num-rolo-imp ~
ob-etiqueta.cod-depos ob-etiqueta.peso-bruto 
&Scoped-define List-6 ob-etiqueta.quantidade ob-etiqueta.nr-cortes ~
ob-etiqueta.nr-lote ob-etiqueta.nuance ob-etiqueta.situacao 

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
DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-localiz AS CHARACTER FORMAT "xxx/xxx":U 
     LABEL "Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FONT 0 NO-UNDO.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47.72 BY .88 NO-UNDO.

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
     ob-etiqueta.dt-emissao AT ROW 2.25 COL 58 COLON-ALIGNED WIDGET-ID 4
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
     ob-etiqueta.nr-container AT ROW 4.25 COL 40 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 9 BY .88
     ob-etiqueta.num-rolo-imp AT ROW 4.25 COL 62 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 6.14 BY .88
     ob-etiqueta.cod-depos AT ROW 4.25 COL 79 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 6 BY .88
     ob-etiqueta.quantidade AT ROW 6 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.29 BY .88
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
     ob-etiqueta.peso-bruto AT ROW 11 COL 16 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 11 BY .88
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
     ob-etiqueta.acondic AT ROW 6.88 COL 60 NO-LABEL WIDGET-ID 26
          VIEW-AS EDITOR MAX-CHARS 200 SCROLLBAR-VERTICAL
          SIZE 28 BY 6.63
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

/* SETTINGS FOR FILL-IN ob-etiqueta.cod-depos IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ob-etiqueta.cod-estabel IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ob-etiqueta.cod-refer IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ob-etiqueta.dt-emissao IN FRAME f-main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-localiz IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.it-codigo IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ob-etiqueta.nr-container IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ob-etiqueta.nr-cortes IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN ob-etiqueta.nr-lote IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN ob-etiqueta.nuance IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR FILL-IN ob-etiqueta.num-etiqueta IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ob-etiqueta.num-rolo-imp IN FRAME f-main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN ob-etiqueta.peso-bruto IN FRAME f-main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN ob-etiqueta.quantidade IN FRAME f-main
   6                                                                    */
/* SETTINGS FOR RADIO-SET ob-etiqueta.situacao IN FRAME f-main
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

&Scoped-define SELF-NAME ob-etiqueta.cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etiqueta.cod-estabel V-table-Win
ON LEAVE OF ob-etiqueta.cod-estabel IN FRAME f-main /* Estabelecimento */
DO:
   FIND estabelec WHERE
        estabelec.cod-estabel = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAIL estabelec THEN DO.
      MESSAGE 'Estabelecimento Inv†lio...'
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.

   FIND FIRST usuar-depos WHERE
              usuar-depos.cod-estab = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAIL usuar-depos THEN DO.
      MESSAGE 'N∆o encontrado Dep¢sito relacionado ao Estabelecimento' SKIP
              'Utilize cd1760' 
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN ob-etiqueta.cod-depos:SCREEN-VALUE = usuar-depos.cod-depos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME ob-etiqueta.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etiqueta.it-codigo V-table-Win
ON LEAVE OF ob-etiqueta.it-codigo IN FRAME f-main /* Item */
DO:
  FIND item WHERE
       item.it-codigo = SELF:INPUT-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAIL item THEN DO.
     MESSAGE 'Item n∆o Cadastrado...'
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ob-etiqueta.nr-container
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ob-etiqueta.nr-container V-table-Win
ON LEAVE OF ob-etiqueta.nr-container IN FRAME f-main /* Container */
DO:
   ASSIGN i-num-rolo-imp = 0.
   IF SELF:INPUT-VALUE <> 0 THEN DO.
      FOR EACH b-ob-etiqueta WHERE
               b-ob-etiqueta.cod-estabel = INPUT FRAME {&FRAME-NAME} ob-etiqueta.cod-estabel AND
               b-ob-etiqueta.nr-container = INPUT FRAME {&FRAME-NAME} ob-etiqueta.nr-container AND
               b-ob-etiqueta.it-codigo = INPUT FRAME {&FRAME-NAME} ob-etiqueta.it-codigo AND
               b-ob-etiqueta.cod-refer = INPUT FRAME {&FRAME-NAME} ob-etiqueta.cod-refer NO-LOCK
          BY b-ob-etiqueta.num-rolo-imp.
          ASSIGN i-num-rolo-imp = b-ob-etiqueta.num-rolo-imp.
      END.
      ASSIGN i-num-rolo-imp = i-num-rolo-imp + 1.
   END.
           
   ASSIGN ob-etiqueta.num-rolo-imp:SCREEN-VALUE = STRING(i-num-rolo-imp).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* Code placed here will execute PRIOR to standard behavior. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
    
    /* Code placed here will execute AFTER standard behavior.    */

    ASSIGN ob-etiqueta.dt-emissao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).

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
    
    ASSIGN INPUT FRAME {&FRAME-NAME} fi-localiz.

    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */

    RUN pi-validate.
    IF RETURN-VALUE = 'ADM-ERROR':U THEN 
       RETURN 'ADM-ERROR':U.
    
    IF adm-new-record THEN DO.
       CASE INPUT FRAME {&FRAME-NAME} ob-etiqueta.cod-estabel.
           WHEN '1' THEN
                ASSIGN ob-etiqueta.num-etiqueta:SCREEN-VALUE = STRING(NEXT-VALUE(seq-etq-estoq-ima)).
           WHEN '5' THEN
                ASSIGN ob-etiqueta.num-etiqueta:SCREEN-VALUE = STRING(NEXT-VALUE(seq-etq-estoq-med)).
           WHEN '505' THEN
                ASSIGN ob-etiqueta.num-etiqueta:SCREEN-VALUE = STRING(NEXT-VALUE(seq-etq-estoq-itj)).
       END CASE.
    END.
    ELSE DO.
       EMPTY TEMP-TABLE tt-etiqueta.
       CREATE tt-etiqueta.
       BUFFER-COPY ob-etiqueta TO tt-etiqueta.
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    IF RETURN-VALUE = 'ADM-ERROR':U THEN  
       RETURN 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN ob-etiqueta.localiz = fi-localiz.

    /* Cria Etiqueta Nova Logistica */
    IF adm-new-record THEN DO.
        ASSIGN i-tp-embal = 1.
    
        FIND item WHERE
             item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
    
        FIND corte-comerc WHERE
             corte-comerc.compr-min <= ob-etiqueta.quantidade AND
             corte-comerc.compr-max >= ob-etiqueta.quantidade AND
             corte-comerc.tp-embalag = i-tp-embal AND 
             corte-comerc.un = item.un NO-LOCK NO-ERROR.
    
        ASSIGN ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
               ob-etiqueta.nr-lote         = IF ob-etiqueta.cod-refer   = '888'
                                             THEN 'RD' ELSE 'RP'
               ob-etiqueta.cod-qualid      = IF ob-etiqueta.cod-refer = '888'
                                             THEN 'D' ELSE 'B'
               ob-etiqueta.corte-comerc    = IF AVAIL corte-comerc
                                             THEN corte-comerc.codigo
                                             ELSE ''.
    END.
    ELSE DO.
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
                                                 ob-etiqueta.acondic.
    
                  END.
                  WHEN 'nr-lote' THEN DO.
                      FIND CURRENT ob-etiqueta SHARE-LOCK NO-ERROR.
                      CASE ob-etiqueta.nr-lote.
                         WHEN 'RP' THEN
                            ASSIGN ob-etiqueta.cod-qualid = 'B'.
                         WHEN 'RD' THEN
                            ASSIGN ob-etiqueta.cod-qualid = 'D'.
                      END.
    
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
                                                ob-etiqueta.acondic.
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
                                                ob-etiqueta.acondic.
                  END.
              END CASE.
           END.
        END.
    
        IF ob-etiqueta.situacao = 3 THEN DO. /* Elmina romaneio */
           FIND ped-item-rom WHERE 
                ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta AND 
                ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel NO-ERROR.
           IF AVAIL ped-item-rom THEN
              DELETE ped-item-rom.
        END.
    END.
         
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
    
    ASSIGN fi-localiz:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

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
       FIND estabelec WHERE
            estabelec.cod-estabel = ob-etiqueta.cod-estabel NO-LOCK NO-ERROR.
       IF AVAIL estabelec THEN
          ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.

       FIND item WHERE
            item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item THEN
          ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.

       ASSIGN fi-localiz:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.localizacao.

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

    ASSIGN fi-localiz:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    FIND usuar_grp_usuar WHERE
         usuar_grp_usuar.cod_usuario = c-seg-usuario AND
         usuar_grp_usuar.cod_grp_usuar = 'SUP' NO-LOCK NO-ERROR.

    IF NOT AVAIL usuar_grp_usuar THEN
       DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.

    FIND im-param WHERE
         im-param.cod-param = "USR_ALT_QTDE_135" NO-LOCK NO-ERROR.

    IF AVAIL im-param AND
       LOOKUP(c-seg-usuario,im-param.val-param) > 0 THEN DO.

       ENABLE {&list-6} WITH FRAME {&FRAME-NAME}.

       APPLY 'ENTRY' TO ob-etiqueta.quantidade.
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
    FIND CURRENT ob-etiqueta EXCLUSIVE-LOCK NO-ERROR.

    IF ob-etiqueta.situacao <> 3 THEN DO.
       MESSAGE 'Situaá∆o da Etiqueta n∆o permite Eliminaá∆o...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN 'ADM-ERROR'.
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime V-table-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN esapi/imp-etq-estoque.p (INPUT ob-etiqueta.cod-estabel,
                               INPUT ob-etiqueta.num-etiqueta,
                               INPUT NO).
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
    
    IF adm-new-record THEN DO.
       FIND b-ob-etiqueta WHERE
            b-ob-etiqueta.cod-estabel = INPUT FRAME {&FRAME-NAME} ob-etiqueta.cod-estabel AND
            b-ob-etiqueta.nr-container = INPUT FRAME {&FRAME-NAME} ob-etiqueta.nr-container AND
            b-ob-etiqueta.it-codigo = INPUT FRAME {&FRAME-NAME} ob-etiqueta.it-codigo AND
            b-ob-etiqueta.cod-refer = INPUT FRAME {&FRAME-NAME} ob-etiqueta.cod-refer AND
            b-ob-etiqueta.num-rolo-imp = INPUT FRAME {&FRAME-NAME} ob-etiqueta.num-rolo-imp 
            NO-LOCK NO-ERROR.
       IF AVAIL b-ob-etiqueta THEN DO.
           MESSAGE "J† existe a Etiqueta " b-ob-etiqueta.num-etiqueta " com o Rolo Importaáao" SKIP
                   "Inclus∆o n∆o Permitida"
                 VIEW-AS ALERT-BOX.
           APPLY 'entry' TO ob-etiqueta.quantidade.
           RETURN 'ADM-ERROR'.
       END.
    END.
    ELSE DO.
        IF ob-etiqueta.situacao <> 3 AND
           INPUT FRAME {&FRAME-NAME} ob-etiqueta.quantidade <> ob-etiqueta.quantidade THEN DO.
            
           MESSAGE "Alteraá∆o Permitida apenas para Etiquetas em ESTOQUE..."
                 VIEW-AS ALERT-BOX.
           APPLY 'entry' TO ob-etiqueta.quantidade.
           RETURN 'ADM-ERROR'.
        END.
    
        IF ob-etiqueta.situacao = 4 AND 
           INPUT FRAME {&FRAME-NAME} ob-etiqueta.situacao <> ob-etiqueta.situacao THEN DO.
           FIND ped-item-rom WHERE
                ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
    
           ASSIGN c-nr-pedcli = IF AVAIL ped-item-rom
                                THEN ped-item-rom.nr-pedcli 
                                ELSE "".
    
           MESSAGE "Alteraá∆o N«P Permitida, Etiqueta est† reservada, para o pedido " c-nr-pedcli SKIP
                   "Se Processguir, a Reserva ser† Eliminada" SKIP
                   "Confirma Alteraá∆o ?"
                 VIEW-AS ALERT-BOX QUESTION UPDATE l-conf AS LOGICAL.
           IF NOT l-conf THEN DO.
              APPLY 'entry' TO ob-etiqueta.situacao.
              RETURN 'ADM-ERROR'.
           END.
        END.
    
        IF ob-etiqueta.situacao = 5 THEN DO.
           FIND ped-item-rom WHERE
                ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
                ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
    
           ASSIGN c-nr-pedcli = IF AVAIL ped-item-rom
                                THEN ped-item-rom.nr-pedcli 
                                ELSE "".
    
           MESSAGE "Atená∆o, Etiqueta foi FATURADA, para o pedido " c-nr-pedcli SKIP
                   "Aconselhamos n∆o Retornar para Estoque." SKIP
                   "Se Processguir, o pedido do Cliente ficar† Inconsistente" SKIP
                   "Confirma Alteraá∆o ?"
                 VIEW-AS ALERT-BOX QUESTION UPDATE l-conf2 AS LOGICAL.
           IF NOT l-conf2 THEN DO.
              APPLY 'entry' TO ob-etiqueta.situacao.
              RETURN 'ADM-ERROR'.
           END.
        END.
    END.

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

