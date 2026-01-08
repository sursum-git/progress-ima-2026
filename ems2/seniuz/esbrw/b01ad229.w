&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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

DEF NEW GLOBAL SHARED temp-table tt-cotas
    field line as int /*:T Este campo Ç obrigat¢rio */
    FIELD it-codigo   LIKE cota-rep.it-codigo
    FIELD desc-item   AS CHAR FORMAT "x(36)"
    field qt-prevista LIKE cota-rep.qt-prevista.

def var c-cod-lista-obj as char no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowseDigitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-digita

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cotas

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-cotas.it-codigo tt-cotas.desc-item tt-cotas.qt-prevista[1] tt-cotas.qt-prevista[2] tt-cotas.qt-prevista[3] tt-cotas.qt-prevista[4] tt-cotas.qt-prevista[5] tt-cotas.qt-prevista[6] tt-cotas.qt-prevista[7] tt-cotas.qt-prevista[8] tt-cotas.qt-prevista[9] tt-cotas.qt-prevista[10] tt-cotas.qt-prevista[11] tt-cotas.qt-prevista[12]   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita tt-cotas.it-codigo tt-cotas.qt-prevista[1]  tt-cotas.qt-prevista[2]  tt-cotas.qt-prevista[3]  tt-cotas.qt-prevista[4]  tt-cotas.qt-prevista[5]  tt-cotas.qt-prevista[6]  tt-cotas.qt-prevista[7]  tt-cotas.qt-prevista[8]  tt-cotas.qt-prevista[9]  tt-cotas.qt-prevista[10] tt-cotas.qt-prevista[11] tt-cotas.qt-prevista[12]   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-digita tt-cotas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-digita tt-cotas
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-cotas NO-LOCK
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY {&self-name} FOR EACH tt-cotas NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-digita tt-cotas
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-cotas


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita bt-incluir bt-modificar ~
bt-eliminar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 bt-incluir bt-modificar bt-eliminar 

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
      tt-cotas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita B-table-Win _FREEFORM
  QUERY br-digita NO-LOCK DISPLAY
      tt-cotas.it-codigo       LABEL "Item" 
tt-cotas.desc-item       LABEL "Descriá∆o" 
tt-cotas.qt-prevista[1]  LABEL "Janeiro"
tt-cotas.qt-prevista[2]  LABEL "Fevereiro"
tt-cotas.qt-prevista[3]  LABEL "Maráo"
tt-cotas.qt-prevista[4]  LABEL "Abril"
tt-cotas.qt-prevista[5]  LABEL "Maio"
tt-cotas.qt-prevista[6]  LABEL "Junho"
tt-cotas.qt-prevista[7]  LABEL "Julho"
tt-cotas.qt-prevista[8]  LABEL "Agosto"
tt-cotas.qt-prevista[9]  LABEL "Setembro"
tt-cotas.qt-prevista[10] LABEL "Outubro"
tt-cotas.qt-prevista[11] LABEL "Novembro"
tt-cotas.qt-prevista[12] LABEL "Dezembro"
ENABLE
tt-cotas.it-codigo
tt-cotas.qt-prevista[1]  
tt-cotas.qt-prevista[2]  
tt-cotas.qt-prevista[3]  
tt-cotas.qt-prevista[4]  
tt-cotas.qt-prevista[5]  
tt-cotas.qt-prevista[6]  
tt-cotas.qt-prevista[7]  
tt-cotas.qt-prevista[8]  
tt-cotas.qt-prevista[9]  
tt-cotas.qt-prevista[10] 
tt-cotas.qt-prevista[11] 
tt-cotas.qt-prevista[12]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 83 BY 9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-digita AT ROW 1 COL 1
     bt-incluir AT ROW 10.13 COL 1.29
     bt-modificar AT ROW 10.13 COL 11.57
     bt-eliminar AT ROW 10.13 COL 21.86
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
         HEIGHT             = 10.29
         WIDTH              = 84.14.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-digita 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-eliminar IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON bt-incluir IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON bt-modificar IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY {&self-name} FOR EACH tt-cotas NO-LOCK.
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
ON DEL OF br-digita IN FRAME F-Main
DO:
  run pi-elimina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON END-ERROR OF br-digita IN FRAME F-Main
ANYWHERE 
DO:
  if  br-digita:new-row in frame {&frame-name} then do:
      if  avail tt-cotas then
          delete tt-cotas.
      if  br-digita:delete-current-row() in frame {&frame-name} then. 
  end.                                                               
  else do:
      get current br-digita.
      display tt-cotas.it-codigo
              tt-cotas.desc-item
              tt-cotas.qt-prevista[1]  
              tt-cotas.qt-prevista[2]  
              tt-cotas.qt-prevista[3]  
              tt-cotas.qt-prevista[4]  
              tt-cotas.qt-prevista[5]  
              tt-cotas.qt-prevista[6]  
              tt-cotas.qt-prevista[7]  
              tt-cotas.qt-prevista[8]  
              tt-cotas.qt-prevista[9]  
              tt-cotas.qt-prevista[10] 
              tt-cotas.qt-prevista[11] 
              tt-cotas.qt-prevista[12] 
              with browse br-digita. 
  end.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON INS OF br-digita IN FRAME F-Main
DO:
  run pi-save-record. 
  run insere-registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON OFF-END OF br-digita IN FRAME F-Main
DO:
  run pi-off-end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON RETURN OF br-digita IN FRAME F-Main
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

    ASSIGN INPUT BROWSE {&browse-name} tt-cotas.desc-item.

    run pi-row-leave.
    RUN pi-disable.
    RUN pi-enable. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON VALUE-CHANGED OF br-digita IN FRAME F-Main
DO:
    /* This ADM trigger code must be preserved in order to notify other
       objects when the browser's current row changes. */
    {src/adm/template/brschnge.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-eliminar B-table-Win
ON CHOOSE OF bt-eliminar IN FRAME F-Main /* Eliminar */
DO:
  run pi-elimina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-incluir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-incluir B-table-Win
ON CHOOSE OF bt-incluir IN FRAME F-Main /* Incluir */
DO:
  IF NUM-RESULTS("br-digita":U) > 0 THEN 
     br-digita:INSERT-ROW("after":U) IN FRAME {&FRAME-NAME}.
  ELSE DO TRANSACTION:
     CREATE tt-cotas.
     OPEN QUERY br-digita FOR EACH tt-cotas.
  END.
  RUN pi-entry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modificar B-table-Win
ON CHOOSE OF bt-modificar IN FRAME F-Main /* Modificar */
DO:
  run pi-entry.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
ON 'leave':U OF tt-cotas.it-codigo IN BROWSE br-digita DO:
   FIND item WHERE
        item.it-codigo = INPUT BROWSE br-digita tt-cotas.it-codigo
        NO-LOCK NO-ERROR.

   IF NOT AVAIL item THEN DO.
      MESSAGE 'Item n∆o Cadastrado...' VIEW-AS ALERT-BOX.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN tt-cotas.desc-item:SCREEN-VALUE IN BROWSE br-digita = item.desc-item.
END.

br-digita:NUM-LOCKED-COLUMNS = 2.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-disable B-table-Win 
PROCEDURE pi-disable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN br-digita:READ-ONLY IN FRAME {&FRAME-NAME} = YES
           bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
           bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME} = NO NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-enable B-table-Win 
PROCEDURE pi-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

    IF NUM-RESULTS("br-digita":U) > 0 THEN DO.
       ASSIGN bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = YES
              bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
    END.
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
   ASSIGN bt-incluir:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-modificar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-eliminar:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          br-digita:READ-ONLY IN FRAME {&FRAME-NAME} = NO.

    APPLY "entry":U TO tt-cotas.it-codigo IN BROWSE {&browse-name}.
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
    DEFINE INPUT PARAMETER p-row-table AS ROWID.
    
    FOR EACH tt-cotas.
        DELETE tt-cotas.
    END.

    FIND repres WHERE
         ROWID(repres) = p-row-table NO-LOCK NO-ERROR.

    FOR EACH cota-rep OF repres NO-LOCK.
        FIND item WHERE
             item.it-codigo = cota-rep.it-codigo NO-LOCK NO-ERROR.
        CREATE tt-cotas.
        ASSIGN tt-cotas.it-codigo = cota-rep.it-codigo
               tt-cotas.desc-item = IF AVAIL item
                                    THEN item.desc-item
                                    ELSE ""
               tt-cotas.qt-prevista[1] = cota-rep.qt-prevista[1]
               tt-cotas.qt-prevista[2] = cota-rep.qt-prevista[2].
               tt-cotas.qt-prevista[3] = cota-rep.qt-prevista[3].
               tt-cotas.qt-prevista[4] = cota-rep.qt-prevista[4].
               tt-cotas.qt-prevista[5] = cota-rep.qt-prevista[5].
               tt-cotas.qt-prevista[6] = cota-rep.qt-prevista[6].
               tt-cotas.qt-prevista[7] = cota-rep.qt-prevista[7].
               tt-cotas.qt-prevista[8] = cota-rep.qt-prevista[8].
               tt-cotas.qt-prevista[9] = cota-rep.qt-prevista[9].
               tt-cotas.qt-prevista[10] = cota-rep.qt-prevista[10].
               tt-cotas.qt-prevista[11] = cota-rep.qt-prevista[11].
               tt-cotas.qt-prevista[12] = cota-rep.qt-prevista[12].
    END.
    RUN pi-disable.
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
  {src/adm/template/snd-list.i "tt-cotas"}

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

