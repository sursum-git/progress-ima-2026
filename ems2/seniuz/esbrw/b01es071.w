&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/browserd.w

/* Parameters Definitions ---                                           */
 
/* Local Variable Definitions ---                                       */

/*:T Variaveis usadas internamente pelo estilo, favor nao elimina-las     */

/*:T v†ri†veis de uso globla */
def new global shared var v-row-parent as rowid no-undo.

/*:T vari†veis de uso local */
def var v-row-table  as rowid.

/*:T fim das variaveis utilizadas no estilo */
DEF NEW GLOBAL SHARED VAR g-cod-estabel AS CHAR.

DEF VAR c-obs AS CHAR FORMAT "x(500)".
DEF VAR i-lin AS INT.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR l-ok                AS LOG.
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ob-etiqueta
&Scoped-define FIRST-EXTERNAL-TABLE ob-etiqueta


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ob-etiqueta.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES movto-etq

/* Definitions for BROWSE br-table                                      */
&Scoped-define FIELDS-IN-QUERY-br-table movto-etq.dt-trans ~
movto-etq.esp-docto fn-obs() @ c-obs 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-table 
&Scoped-define QUERY-STRING-br-table FOR EACH movto-etq WHERE TRUE /* Join to ob-etiqueta incomplete */ ~
      AND movto-etq.cod-estab = ob-etiqueta.cod-estab AND ~
movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-table OPEN QUERY br-table FOR EACH movto-etq WHERE TRUE /* Join to ob-etiqueta incomplete */ ~
      AND movto-etq.cod-estab = ob-etiqueta.cod-estab AND ~
movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-table movto-etq
&Scoped-define FIRST-TABLE-IN-QUERY-br-table movto-etq


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-table ed-log bt-imprime 
&Scoped-Define DISPLAYED-OBJECTS ed-log 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-imprime 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
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
&BROWSE-NAME
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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-obs B-table-Win 
FUNCTION fn-obs RETURNS CHARACTER
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 11 BY 1.13.

DEFINE VARIABLE ed-log AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 43 BY 8.46
     FGCOLOR 12 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-table FOR 
      movto-etq SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table B-table-Win _STRUCTURED
  QUERY br-table NO-LOCK DISPLAY
      movto-etq.dt-trans COLUMN-LABEL "Dt Transaá∆o" FORMAT "99/99/9999":U
            WIDTH 10.43
      movto-etq.esp-docto FORMAT "X(3)":U
      fn-obs() @ c-obs COLUMN-LABEL "Usuario" FORMAT "x(15)":U
            WIDTH 19.86
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 42 BY 8.5
         FONT 1 ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-table AT ROW 1 COL 1
     ed-log AT ROW 1.04 COL 44 NO-LABEL WIDGET-ID 2
     bt-imprime AT ROW 9.63 COL 1.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: espec.ob-etiqueta
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 9.88
         WIDTH              = 86.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{include/c-browse.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
/* BROWSE-TAB br-table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-imprime IN FRAME F-Main
   4                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-table
/* Query rebuild information for BROWSE br-table
     _TblList          = "espec.movto-etq WHERE espec.ob-etiqueta <external> ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "movto-etq.cod-estab = ob-etiqueta.cod-estab AND
movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta"
     _FldNameList[1]   > espec.movto-etq.dt-trans
"movto-etq.dt-trans" "Dt Transaá∆o" ? "date" ? ? ? ? ? ? no ? no no "10.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = espec.movto-etq.esp-docto
     _FldNameList[3]   > "_<CALC>"
"fn-obs() @ c-obs" "Usuario" "x(15)" ? ? ? ? ? ? ? no ? no no "19.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br-table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br-table IN FRAME F-Main
DO:
    RUN New-State("DblClick, SELF":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-ENTRY OF br-table IN FRAME F-Main
DO:

  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON ROW-LEAVE OF br-table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table B-table-Win
ON VALUE-CHANGED OF br-table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  /* run new-state('New-Line|':U + string(rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}))). */

  ASSIGN ed-log:READ-ONLY = YES.
  IF AVAIL movto-etq THEN
     ASSIGN ed-log:SCREEN-VALUE = movto-etq.char-1.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime B-table-Win
ON CHOOSE OF bt-imprime IN FRAME F-Main /* Impress∆o */
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "value-changed" TO br-table IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  apply 'value-changed':U to {&browse-name} in frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec B-table-Win 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT "IMA TECIDOS DA MODA LTDA."               AT   1
     "DATA: "                                  AT  77
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  83
     "HORA: "                                  AT 116
     STRING(TIME,"hh:mm:ss")                   AT 122
     SKIP(1).
                                                           
 PUT "RELATORIO DE DETALHAMENTO DA MOVIMENTACAO DE UMA ETIQUETA" AT 43 SKIP(1).
 PUT "ETIQUETA  TRANSACAO  ESPECIE NF/PEDIDO REGISTRO DAS OCORRENCIAS" AT 1.
 PUT "--------- ---------- ------- --------- ----------------------------------------------------------------------------------------------------" AT 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime B-table-Win 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR h-prog  AS HANDLE NO-UNDO.
 DEF VAR i-ct    AS INT.
 DEF VAR c-cod-estabel AS CHAR.
 DEF VAR c-docto AS CHAR FORMAT "x(9)".

 RUN utp/ut-utils.p PERSISTENT SET h-prog.


 RUN esapi/saida-imp.p (OUTPUT i-saida,
                        OUTPUT c-saida,
                        OUTPUT i-num-copias,
                        OUTPUT l-ok).

 CASE i-saida:
     WHEN 1 THEN DO.
         OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 74.
         PUT CONTROL "~033E~033(s18H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0148.tmp".
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     END.
 END CASE.


 RUN Get-Field-Screen-Value IN adm-broker-hdl
                    (INPUT THIS-PROCEDURE /* HANDLE */,
                     INPUT "cod-estabel" /* CHARACTER */).
 ASSIGN c-cod-estabel = RETURN-VALUE.


 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-lin            = 99.

    FOR EACH movto-etq WHERE
             movto-etq.cod-estabel = ob-etiqueta.cod-estabel AND
             movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK.
        ASSIGN c-docto = "".
        FIND ped-item-rom WHERE
             ped-item-rom.cod-estabel  = c-cod-estabel AND
             ped-item-rom.num-etiqueta = movto-etq.num-etiqueta NO-LOCK NO-ERROR.
        IF AVAIL ped-item-rom THEN DO:
           IF movto-etq.esp-docto = "RES" THEN
              ASSIGN c-docto = ped-item-rom.nr-pedcli.
           FIND ped-item-res WHERE
                ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
                ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli  AND
                ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
           IF AVAIL ped-item-res AND movto-etq.esp-docto = "FAT" THEN
              ASSIGN c-docto = STRING(ped-item-res.nr-nota-fis, ">>>>>>>>9").
        END.

        IF i-lin > 74 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        PUT movto-etq.num-etiqueta   AT  1
            movto-etq.dt-trans       AT 11
            movto-etq.esp-docto      AT 22
            TRIM(c-docto)            AT 30
            fn-obs() FORMAT "x(100)" AT 40
            ""                       AT  1.
    END.
    IF i-saida = 1 THEN DO:
       PAGE.
       PUT "" AT 1.
    END.
 END.
 IF i-saida = 3 THEN DO.
   RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                         INPUT c-saida).
   DELETE PROCEDURE h-prog.
 END.
 OUTPUT CLOSE.

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
  {src/adm/template/snd-list.i "ob-etiqueta"}
  {src/adm/template/snd-list.i "movto-etq"}

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
  /*run pi-trata-state (p-issuer-hdl, p-state).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-obs B-table-Win 
FUNCTION fn-obs RETURNS CHARACTER
  ( ) :

  /*RETURN TRIM(SUBSTR(movto-etq.char-1,1,20)).   */

  IF TRIM(ENTRY(2,SUBSTR(movto-etq.char-1,1,30),":")) = "Hora" THEN
     RETURN "".
  ELSE
     RETURN TRIM(ENTRY(2,SUBSTR(movto-etq.char-1,1,30),":")).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

