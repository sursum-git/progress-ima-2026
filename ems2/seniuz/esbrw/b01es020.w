&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created by this procedure.
   This is a good default which assures that this procedure's triggers and 
   internal procedures will execute in this procedure's storage, and that 
   proper cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

def temp-table tt-acbd
  field line as int /*:T Este campo Ç obrigat¢rio */
  field num-revis    LIKE mov-est-acbd.num-revis
  field nom-revis    AS   CHAR FORMAT "x(18)"
  field num-maq      LIKE mov-est-acbd.num-maq
  field cod-acond    LIKE mov-est-acbd.cod-acond
  field num-acond    LIKE mov-est-acbd.num-acond
  field classif      LIKE mov-est-acbd.classif
  field cod-tipo-def LIKE mov-est-acbd.cod-tipo-def
  field cod-defeito  LIKE mov-est-acbd.cod-defeito
  field qtd-defeit   LIKE mov-est-acbd.qtd-defeit.

DEF VAR h-container     AS   HANDLE.
DEF VAR h-v01es021      AS   HANDLE.

DEF VAR qtd-tot-sanfor  LIKE mov-est-acbm.qtd-tot-perf.
DEF VAR qtd-tot-perf    LIKE mov-est-acbm.qtd-tot-perf.
DEF VAR qtd-tot-def     LIKE mov-est-acbm.qtd-tot-def. 
DEF VAR qtd-tot-sob     LIKE mov-est-acbm.qtd-tot-sob.

DEF VAR wh-pesquisa AS WIDGET-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowseDigitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-acbd

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-acbd.num-revis tt-acbd.nom-revis tt-acbd.num-maq tt-acbd.cod-acond tt-acbd.num-acond tt-acbd.classif tt-acbd.cod-tipo-def tt-acbd.cod-defeito tt-acbd.qtd-defeit   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-acbd.num-revis ~
tt-acbd.num-maq ~
 tt-acbd.cod-acond ~
tt-acbd.num-acond ~
tt-acbd.classif ~
 tt-acbd.cod-tipo-def   tt-acbd.cod-defeito   tt-acbd.qtd-defeit   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-acbd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-acbd
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-acbd NO-LOCK
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY {&self-name} FOR EACH tt-acbd NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-digita tt-acbd
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-acbd


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-name
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-name
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-eliminar 
     LABEL "&Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-incluir 
     LABEL "&Incluir" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-modificar 
     LABEL "&Modificar" 
     SIZE 10 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-acbd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita B-table-Win _FREEFORM
  QUERY br-digita NO-LOCK DISPLAY
      tt-acbd.num-revis
    tt-acbd.nom-revis   LABEL "Nome"
    tt-acbd.num-maq     
    tt-acbd.cod-acond   
    tt-acbd.num-acond   
    tt-acbd.classif 
    tt-acbd.cod-tipo-def
    tt-acbd.cod-defeito 
    tt-acbd.qtd-defeit
ENABLE
    tt-acbd.num-revis   
    tt-acbd.num-maq     
    tt-acbd.cod-acond   
    tt-acbd.num-acond   
    tt-acbd.classif     
    tt-acbd.cod-tipo-def
    tt-acbd.cod-defeito 
    tt-acbd.qtd-defeit
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 86 BY 5.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-digita AT ROW 1 COL 1
     bt-incluir AT ROW 6.88 COL 1.29
     bt-modificar AT ROW 6.88 COL 11.57
     bt-eliminar AT ROW 6.88 COL 21.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowseDigitacao
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 6.92
         WIDTH              = 86.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-brows5.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br-digita 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-eliminar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-incluir IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON bt-modificar IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY {&self-name} FOR EACH tt-acbd NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON ENTER OF br-digita IN FRAME F-Main
ANYWHERE
DO:
    apply 'tab':U to self.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON ROW-LEAVE OF br-digita IN FRAME F-Main
DO:
    
    /* Do not disable this code or no updates will take place except
       by pressing the Save button on an Update SmartPanel. */
    {src/adm/template/brsleave.i}

    run pi-row-leave.

    APPLY 'entry':u TO bt-modificar IN FRAME {&FRAME-NAME}. 
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:
  RUN pi-calc-defeito IN h-v01es021 (INPUT "-",
                                     INPUT tt-acbd.qtd-defeit).
  run pi-elimina.
  RUN pi-habilita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir B-table-Win
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Incluir */
DO:
    tt-acbd.nom-revis:COLUMN-READ-ONLY IN BROWSE br-digita = NO. 
    tt-acbd.num-maq:COLUMN-READ-ONLY IN BROWSE br-digita = NO. 
    tt-acbd.cod-acond:COLUMN-READ-ONLY IN BROWSE br-digita = NO.
    tt-acbd.num-acond:COLUMN-READ-ONLY IN BROWSE br-digita = NO.
    tt-acbd.classif:COLUMN-READ-ONLY IN BROWSE br-digita = NO.
    tt-acbd.cod-tipo-def:COLUMN-READ-ONLY IN BROWSE br-digita = NO.
    tt-acbd.cod-defeito:COLUMN-READ-ONLY IN BROWSE br-digita = NO. 
    tt-acbd.qtd-defeit:COLUMN-READ-ONLY IN BROWSE br-digita = NO.

    RUN insere-registro.
    APPLY 'entry':U TO tt-acbd.nom-revis IN BROWSE {&browse-name}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar B-table-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
  tt-acbd.qtd-defeit:COLUMN-READ-ONLY IN BROWSE br-digita = NO.  
  APPLY 'entry':U TO tt-acbd.qtd-defeit IN BROWSE {&browse-name}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

tt-acbd.nom-revis:COLUMN-READ-ONLY = YES.   
tt-acbd.num-maq:COLUMN-READ-ONLY = YES.     
tt-acbd.cod-acond:COLUMN-READ-ONLY = YES.   
tt-acbd.num-acond:COLUMN-READ-ONLY = YES.   
tt-acbd.classif:COLUMN-READ-ONLY = YES.     
tt-acbd.cod-tipo-def:COLUMN-READ-ONLY = YES.
tt-acbd.cod-defeito:COLUMN-READ-ONLY = YES. 
tt-acbd.qtd-defeit:COLUMN-READ-ONLY = YES.
/*
tt-acbd.cod-tipo-def:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE {&browse-name}.
tt-acbd.cod-defeito:LOAD-MOUSE-POINTER("image/lupa.cur") IN BROWSE {&browse-name}.

ON 'f5':U OF tt-acbd.cod-tipo-def IN BROWSE {&browse-name} OR 
    'mouse-select-dblclick':U OF tt-acbd.cod-tipo-def IN BROWSE {&browse-name} DO:
    {include/zoomvar.i &prog-zoom=eszoom/z01es037.w
                       &campo=tt-acbd.cod-tipo-def
                       &campozoom=cod-tipo-def
                       &browse=br-digita}
END.
*/

ON 'entry':U OF BROWSE br-digita DO:
   IF tt-acbd.qtd-defeit:COLUMN-READ-ONLY IN BROWSE {&browse-name} = YES THEN DO.
      APPLY 'entry':u TO bt-modificar IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
END.

ON 'leave':U OF tt-acbd.cod-acond DO:
   IF LOOKUP(INPUT BROWSE {&browse-name} tt-acbd.cod-acond,"Rl,Pc,Rt") = 0 THEN DO.
      MESSAGE 'C¢digo de Acondicionamento Inv†lido...' VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON 'leave':U OF tt-acbd.classif DO:
   IF LOOKUP(INPUT BROWSE {&browse-name} tt-acbd.classif,"Rg,Ld,Rt") = 0 THEN DO.
      MESSAGE 'Classificaá∆o Inv†lida...' VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
END.

ON 'leave':U OF tt-acbd.cod-defeito DO:
    FIND defeito WHERE
         defeito.cod-tipo-def = INPUT BROWSE {&browse-name} tt-acbd.cod-tipo-def AND
         defeito.cod-defeito = INPUT BROWSE {&browse-name} tt-acbd.cod-defeito
         NO-LOCK NO-ERROR.
    IF NOT AVAIL defeito THEN DO.
       MESSAGE 'Defeito n∆o Cadastrado....' VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
END.

ON 'leave':U OF tt-acbd.cod-tipo-def DO:
    FIND tipo-def WHERE
         tipo-def.cod-tipo-def = INPUT BROWSE {&browse-name} tt-acbd.cod-tipo-def
         NO-LOCK NO-ERROR.
    IF NOT AVAIL tipo-def THEN DO.
       MESSAGE 'Tipo de Defeito n∆o Cadastrado....' VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
END.

ON 'leave':U OF tt-acbd.qtd-defeit IN BROWSE {&browse-name} DO:
   IF tt-acbd.qtd-defeit:COLUMN-READ-ONLY IN BROWSE br-digita = NO AND
      INPUT BROWSE {&browse-name} tt-acbd.qtd-defeit - tt-acbd.qtd-defeit > qtd-tot-sob THEN DO.
      MESSAGE 'Quantidade Perfeita + Defeituosa EXCEDE Total Sanforiz' VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END. 

   IF tt-acbd.qtd-defeit:COLUMN-READ-ONLY IN BROWSE {&browse-name} = NO THEN DO.
      RUN pi-calc-defeito IN h-v01es021 (INPUT "-",
                                         INPUT tt-acbd.qtd-defeit). 

      RUN pi-calc-defeito IN h-v01es021 (INPUT "+",
                                         INPUT INPUT BROWSE {&browse-name} tt-acbd.qtd-defeit). 
   END.
   
   tt-acbd.nom-revis:COLUMN-READ-ONLY IN BROWSE br-digita = YES. 
   tt-acbd.num-maq:COLUMN-READ-ONLY IN BROWSE br-digita = YES. 
   tt-acbd.cod-acond:COLUMN-READ-ONLY IN BROWSE br-digita = YES.
   tt-acbd.num-acond:COLUMN-READ-ONLY IN BROWSE br-digita = YES.
   tt-acbd.classif:COLUMN-READ-ONLY IN BROWSE br-digita = YES.
   tt-acbd.cod-tipo-def:COLUMN-READ-ONLY IN BROWSE br-digita = YES.
   tt-acbd.cod-defeito:COLUMN-READ-ONLY IN BROWSE br-digita = YES. 
   tt-acbd.qtd-defeit:COLUMN-READ-ONLY IN BROWSE br-digita = YES.
   
   RUN pi-habilita.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-assign-record B-table-Win 
PROCEDURE pi-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER r_mov-est-acbm AS ROWID.

    FIND mov-est-acbm WHERE
         ROWID(mov-est-acbm) = r_mov-est-acbm NO-LOCK NO-ERROR.


    FOR EACH mov-est-acbd WHERE
             mov-est-acbd.cod-estabel = mov-est-acbm.cod-estabel AND
             mov-est-acbd.data-mov    = mov-est-acbm.data-mov    AND
             mov-est-acbd.num-lote    = mov-est-acbm.num-lote    AND
             mov-est-acbd.it-codigo   = mov-est-acbm.it-codigo   AND
             mov-est-acbd.cod-refer   = mov-est-acbm.cod-refer.


/*    FOR EACH mov-est-acbd OF mov-est-acbm. */
        DELETE mov-est-acbd.
    END.

    FOR EACH tt-acbd.
        CREATE mov-est-acbd.
        ASSIGN mov-est-acbd.data-mov     = mov-est-acbm.data-mov
               mov-est-acbd.num-lote     = mov-est-acbm.num-lote
               mov-est-acbd.it-codigo    = mov-est-acbm.it-codigo
               mov-est-acbd.cod-refer    = mov-est-acbm.cod-refer
               mov-est-acbd.num-revis    = tt-acbd.num-revis    
               mov-est-acbd.num-maq      = tt-acbd.num-maq      
               mov-est-acbd.cod-acond    = tt-acbd.cod-acond    
               mov-est-acbd.num-acond    = tt-acbd.num-acond    
               mov-est-acbd.classif      = tt-acbd.classif      
               mov-est-acbd.cod-tipo-def = tt-acbd.cod-tipo-def 
               mov-est-acbd.cod-defeito  = tt-acbd.cod-defeito  
               mov-est-acbd.qtd-defeit   = tt-acbd.qtd-defeit.
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-registro B-table-Win 
PROCEDURE pi-cria-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*:T Aqui devem ser lidos os registros da temp-table, criados os registros na tabela f°sica
  e atualizados os valores na tabela f°sica, pois as validaá‰es s∆o feitas na pi-salva-rel.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-desabilita B-table-Win 
PROCEDURE pi-desabilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-entry B-table-Win 
PROCEDURE pi-entry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-habilita B-table-Win 
PROCEDURE pi-habilita :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN Get-Field-Screen-Value IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "fi-tot-sanfor").
    ASSIGN qtd-tot-sanfor = DEC(RETURN-VALUE).

    RUN Get-Field-Screen-Value IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "qtd-tot-perf").
    ASSIGN qtd-tot-perf = DEC(RETURN-VALUE).

    RUN Get-Field-Screen-Value IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "qtd-tot-def").
    ASSIGN qtd-tot-def = DEC(RETURN-VALUE).

    RUN Get-Field-Screen-Value IN adm-broker-hdl
        (INPUT THIS-PROCEDURE,
         INPUT "qtd-tot-sob").
    ASSIGN qtd-tot-sob = DEC(RETURN-VALUE). 

    RUN pi-desabilita.

    IF qtd-tot-sob > 0 THEN  
       ASSIGN bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = YES.


    IF AVAIL tt-acbd THEN
       ASSIGN bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-obtem-handle B-table-Win 
PROCEDURE pi-obtem-handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p1 AS HANDLE.
    DEF INPUT PARAMETER p2 AS HANDLE.
    ASSIGN h-container = p1
            h-v01es021 = p2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse B-table-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER r_mov-est-acbm AS ROWID.

    FIND mov-est-acbm WHERE
         ROWID(mov-est-acbm) = r_mov-est-acbm NO-LOCK NO-ERROR.

    FOR EACH tt-acbd.
        DELETE tt-acbd.
    END.
    FOR EACH mov-est-acbd WHERE
             mov-est-acbd.cod-estabel = mov-est-acbm.cod-estabel AND
             mov-est-acbd.data-mov    = mov-est-acbm.data-mov    AND
             mov-est-acbd.num-lote    = mov-est-acbm.num-lote    AND
             mov-est-acbd.it-codigo   = mov-est-acbm.it-codigo   AND
             mov-est-acbd.cod-refer   = mov-est-acbm.cod-refer NO-LOCK.
    
/*    FOR EACH mov-est-acbd OF mov-est-acbm NO-LOCK. */

        FIND usuar_mestre WHERE
             usuar_mestre.cod_usuar = "r" + STRING(mov-est-acbd.num-revis) NO-LOCK NO-ERROR.

        CREATE tt-acbd.
        ASSIGN tt-acbd.num-revis    = mov-est-acbd.num-revis
               tt-acbd.nom-revis    = IF AVAIL usuar_mestre
                                      THEN ENTRY(1,usuar_mestre.nom_usuar," ") + " " +
                                           ENTRY(2,usuar_mestre.nom_usuar," ")
                                      ELSE ""
               tt-acbd.num-maq      = mov-est-acbd.num-maq
               tt-acbd.cod-acond    = mov-est-acbd.cod-acond
               tt-acbd.num-acond    = mov-est-acbd.num-acond
               tt-acbd.classif      = mov-est-acbd.classif
               tt-acbd.cod-tipo-def = mov-est-acbd.cod-tipo-def
               tt-acbd.cod-defeito  = mov-est-acbd.cod-defeito
               tt-acbd.qtd-defeit   = mov-est-acbd.qtd-defeit.
    END.
    {&OPEN-QUERY-br-digita}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-acbd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

