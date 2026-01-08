&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          ems2cad          PROGRESS
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

DEF temp-table tt-socios LIKE socio-emit
    field line            as int /*:T Este campo Ç obrigat¢rio */.

def var c-cod-lista-obj as char no-undo.

/* Variaveis para Zoom quando n∆o ser† chamado de uma viewer */
def var wh-pesquisa as widget-handle.
def new global shared var l-implanta as logical init no.
def new global shared var wh-composi    as widget-handle no-undo.
def new global shared var wh-window  as handle no-undo.
def new global shared var adm-broker-hdl as handle no-undo.

DEF BUFFER b-emitente FOR emitente.

DEF TEMP-TABLE tt-clientes LIKE emitente.

DEF VAR c-cpf-calc     AS CHAR FORMAT "x(19)".
DEF VAR c-lst-cpf      AS CHAR.
DEF VAR c-gr-cpf       AS CHAR.
DEF VAR c-nome-cpf     AS CHAR.
DEF VAR lista-aux      AS CHAR.
DEF VAR c-cpf          AS CHAR.
DEF VAR i-cod-emit     AS INT.
DEF VAR i-cont         AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowseDigitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-socios

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-socios

/* Definitions for BROWSE br-socios                                     */
&Scoped-define FIELDS-IN-QUERY-br-socios tt-socios.cnpj-cpf tt-socios.nome-socio   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-socios   
&Scoped-define SELF-NAME br-socios
&Scoped-define QUERY-STRING-br-socios FOR EACH tt-socios NO-LOCK
&Scoped-define OPEN-QUERY-br-socios OPEN QUERY {&self-name} FOR EACH tt-socios NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-socios tt-socios
&Scoped-define FIRST-TABLE-IN-QUERY-br-socios tt-socios


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emitente.int-2 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS br-socios bt-det RECT-4 RECT-7 
&Scoped-Define DISPLAYED-FIELDS emitente.int-2 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS fi-cpf fi-socio 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 bt-del 
&Scoped-define List-4 bt-confirma fi-cpf fi-socio 

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
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/imt-chck1.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.05.

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "bt cnae 2" 
     SIZE 4.43 BY 1.25 TOOLTIP "Eliminar S¢cio".

DEFINE BUTTON bt-det AUTO-GO 
     IMAGE-UP FILE "image/im-det.bmp":U
     LABEL "" 
     SIZE 4.43 BY 1.25 TOOLTIP "Detalhar Clientes do S¢cio"
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE fi-cpf AS CHARACTER FORMAT "XXXXXXXXXXXXXXXX":U 
     LABEL "CNPJ/CPF" 
     VIEW-AS FILL-IN 
     SIZE 18.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-socio AS CHARACTER FORMAT "X(40)":U 
     LABEL "Nome S¢cio" 
     VIEW-AS FILL-IN 
     SIZE 47 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 2.29.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 6 BY 9.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-socios FOR 
      tt-socios SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-socios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-socios B-table-Win _FREEFORM
  QUERY br-socios NO-LOCK DISPLAY
      tt-socios.cnpj-cpf COLUMN-LABEL "CNPJ/CPF" 
      tt-socios.nome-socio COLUMN-LABEL "Nome S¢cios"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 78 BY 9.25
         FONT 1
         TITLE "S¢cios do Cliente".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-socios AT ROW 3.5 COL 1
     emitente.int-2 AT ROW 1.25 COL 81 COLON-ALIGNED NO-LABEL WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 2 BY .79
     bt-confirma AT ROW 2.17 COL 60.86 WIDGET-ID 32
     fi-cpf AT ROW 1.25 COL 11.29 COLON-ALIGNED WIDGET-ID 34
     fi-socio AT ROW 2.25 COL 11.29 COLON-ALIGNED WIDGET-ID 36
     bt-del AT ROW 11.25 COL 81 WIDGET-ID 44
     bt-det AT ROW 9.79 COL 81 WIDGET-ID 46
     RECT-4 AT ROW 1 COL 1 WIDGET-ID 2
     RECT-7 AT ROW 3.5 COL 80 WIDGET-ID 48
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowseDigitacao
   External Tables: mgcad.emitente
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
         HEIGHT             = 11.92
         WIDTH              = 85.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
/*{include/c-brows5.i}*/
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br-socios 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-confirma IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-del IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cpf IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-socio IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-socios
/* Query rebuild information for BROWSE br-socios
     _START_FREEFORM
OPEN QUERY {&self-name} FOR EACH tt-socios NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE br-socios */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-socios
&Scoped-define SELF-NAME br-socios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-socios B-table-Win
ON VALUE-CHANGED OF br-socios IN FRAME F-Main /* S¢cios do Cliente */
DO:
    ASSIGN fi-cpf:SCREEN-VALUE = ''.

    ASSIGN bt-det:SENSITIVE = NO
           bt-del:SENSITIVE = NO.
    IF AVAIL tt-socios THEN DO.
       ASSIGN bt-del:SENSITIVE = fi-cpf:SENSITIVE.

       FIND socio-emit WHERE
            socio-emit.cnpj-cpf = tt-socios.cnpj-cpf NO-LOCK NO-ERROR.

       IF AVAIL socio-emit AND 
          socio-emit.cod-emit <> emitente.cod-emit THEN
          ASSIGN bt-det:SENSITIVE = YES.

       IF AMBIGUOUS socio-emit THEN
          ASSIGN bt-det:SENSITIVE = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma B-table-Win
ON CHOOSE OF bt-confirma IN FRAME F-Main /* Button 1 */
DO:
   IF LENGTH(fi-cpf:SCREEN-VALUE) = 11 THEN DO:
      RUN pi-calc-cpf.
      IF c-cpf-calc <> fi-cpf:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN DO:
         MESSAGE "O N£mero do CPF informado Ç n£mero INVALIDO ! ! !" VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-cpf.
         RETURN 'ADM-ERROR':U.
      END.
   END.
   ELSE DO:
      RUN pi-calc-cnpj.
      IF c-cpf-calc <> fi-cpf:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN DO:
         MESSAGE "O N£mero do CNPJ informado Ç n£mero INVALIDO ! ! !" VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-cpf.
         RETURN 'ADM-ERROR':U.
      END.
   END.

   IF fi-socio:SCREEN-VALUE = '' THEN DO:
      MESSAGE "NOME DO S‡CIO inv†lido...." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO fi-socio.
      RETURN 'ADM-ERROR':U.
   END.

    FIND tt-socios WHERE
         tt-socios.cnpj-cpf = fi-cpf:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL tt-socios THEN DO.
       MESSAGE "CPF/CNPJ j† est† Cadastrado...." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO fi-cpf.
       RETURN 'ADM-ERROR':U.
    END.

    CREATE tt-socios.
    ASSIGN tt-socios.cod-emit = emitente.cod-emit
           tt-socios.cnpj-cpf = fi-cpf:SCREEN-VALUE
           tt-socios.nome-socio = fi-socio:SCREEN-VALUE.

    ASSIGN fi-cpf:SCREEN-VALUE   = ""
           fi-socio:SCREEN-VALUE = "".

    {&OPEN-QUERY-br-socios}
     APPLY 'VALUE-CHANGED' TO br-socios.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del B-table-Win
ON CHOOSE OF bt-del IN FRAME F-Main /* bt cnae 2 */
DO:

   DELETE tt-socios.
    
   {&OPEN-QUERY-br-socios}
   APPLY 'VALUE-CHANGED' TO br-socios.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-det
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-det B-table-Win
ON CHOOSE OF bt-det IN FRAME F-Main
DO:
    EMPTY TEMP-TABLE tt-clientes.

    IF fi-cpf:SCREEN-VALUE <> '' THEN DO.
       FOR EACH socio-emit WHERE
                socio-emit.cnpj-cpf = fi-cpf:SCREEN-VALUE NO-LOCK.
           FIND b-emitente WHERE 
                b-emitente.cod-emitente = socio-emi.cod-emitente NO-LOCK.
           CREATE tt-clientes.
           BUFFER-COPY b-emitente TO tt-clientes.
       END.

       FIND b-emitente WHERE 
            b-emitente.cgc = fi-cpf:SCREEN-VALUE USE-INDEX cgc NO-LOCK NO-ERROR.
       IF AVAIL b-emitente THEN DO.
          CREATE tt-clientes.
          BUFFER-COPY b-emitente TO tt-clientes.
       END.
    END.
    ELSE DO.
       FOR EACH socio-emit WHERE
                socio-emit.cnpj-cpf = tt-socios.cnpj-cpf NO-LOCK.
           FIND b-emitente WHERE 
                b-emitente.cod-emitente = socio-emi.cod-emitente NO-LOCK.
           CREATE tt-clientes.
           BUFFER-COPY b-emitente TO tt-clientes.
       END.

       FIND b-emitente WHERE 
            b-emitente.cgc = tt-socios.cnpj-cpf USE-INDEX cgc NO-LOCK NO-ERROR.
       IF AVAIL b-emitente THEN DO.
          CREATE tt-clientes.
          BUFFER-COPY b-emitente TO tt-clientes.
       END.
    END.

    RUN esp/escd0704.p (INPUT TABLE tt-clientes).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cpf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cpf B-table-Win
ON ENTRY OF fi-cpf IN FRAME F-Main /* CNPJ/CPF */
DO:
  ASSIGN bt-det:SENSITIVE = NO
         bt-del:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cpf B-table-Win
ON LEAVE OF fi-cpf IN FRAME F-Main /* CNPJ/CPF */
DO:
    ASSIGN fi-socio:SCREEN-VALUE = "".
    
    FIND FIRST socio-emit WHERE 
               socio-emit.cnpj-cpf = SELF:SCREEN-VALUE USE-INDEX indice1 NO-LOCK NO-ERROR.
    IF AVAIL socio-emit THEN DO.
       ASSIGN fi-socio:SCREEN-VALUE = socio-emit.nome-socio.
       ASSIGN bt-det:SENSITIVE = YES.
    END.

    FIND b-emitente WHERE 
         b-emitente.cgc = SELF:SCREEN-VALUE USE-INDEX cgc NO-LOCK NO-ERROR.
    IF AVAIL b-emitente THEN 
       ASSIGN bt-det:SENSITIVE = YES.

    ASSIGN bt-confirma:SENSITIVE = YES.

    bt-confirma:LOAD-IMAGE("image\im-chck1.bmp").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-socio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-socio B-table-Win
ON LEAVE OF fi-socio IN FRAME F-Main /* Nome S¢cio */
DO:
    ASSIGN bt-confirma:SENSITIVE = YES.
    bt-confirma:LOAD-IMAGE("image\im-chck1.bmp").

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
  {src/adm/template/row-list.i "emitente"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "emitente"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) . */

  /* Code placed here will execute AFTER standard behavior.    */

  FOR EACH socio-emit WHERE
           socio-emit.cod-emitente = emitente.cod-emit SHARE-LOCK.  
      DELETE socio-emit.
  END.

  FOR EACH tt-socios.
      CREATE socio-emit.
      BUFFER-COPY tt-socios TO socio-emit.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fi-cpf:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         fi-socio:SENSITIVE IN FRAME {&FRAME-NAME} = NO
         bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

  ASSIGN bt-del:SENSITIVE = NO. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-cpf:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
           fi-socio:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    EMPTY TEMP-TABLE tt-socios.
    FOR EACH socio-emit WHERE
             socio-emit.cod-emitente = emitente.cod-emitente NO-LOCK.  
        CREATE tt-socios.
        BUFFER-COPY socio-emit TO tt-socios.
    END.
    {&OPEN-QUERY-br-socios}
    APPLY 'VALUE-CHANGED' TO br-socios.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN fi-cpf:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         fi-socio:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         bt-confirma:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

   APPLY 'VALUE-CHANGED' TO br-socios IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-cnpj B-table-Win 
PROCEDURE pi-calc-cnpj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR i-list-fat2 AS INT EXTENT 13 INIT [6,5,4,3,2,9,8,7,6,5,4,3,2].
 DEF VAR i-list-fat1 AS INT EXTENT 10 INIT [11,10,9,8,7,6,5,4,3,2].
 DEF VAR i-digito    AS INT.
 DEF VAR i-cont      AS INT.
 DEF VAR i-soma      AS INT.

 ASSIGN c-cpf-calc = SUBSTR(fi-cpf:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,12).
 ASSIGN i-soma = 0.
 DO i-cont = 2 TO 13:
    ASSIGN i-soma = i-soma + (INT(SUBSTR(c-cpf-calc,I-cont - 1,1)) * i-list-fat2[i-cont]).
 END.
 ASSIGN i-digito = 11 - (i-soma MODULO 11).
 IF i-digito > 9 THEN
    ASSIGN i-digito = 0.
 ASSIGN c-cpf-calc = c-cpf-calc + STRING(i-digito,"9").
 ASSIGN i-soma = 0.
 DO i-cont = 1 to 13:
    ASSIGN i-soma = i-soma + (INT(SUBSTR(c-cpf-calc,I-cont,1)) *  i-list-fat2[i-cont]).
 END.
 ASSIGN i-digito = 11 - (i-soma MODULO 11).
 IF i-digito > 9 THEN
    ASSIGN i-digito = 0.
 ASSIGN c-cpf-calc = c-cpf-calc + STRING(i-digito,"9").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-cpf B-table-Win 
PROCEDURE pi-calc-cpf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR i-list-fat1 AS INT EXTENT 10 INIT [11,10,9,8,7,6,5,4,3,2].
 DEF VAR i-digito    AS INT.
 DEF VAR i-cont      AS INT.
 DEF VAR i-soma      AS INT.

 ASSIGN c-cpf-calc = SUBSTR(fi-cpf:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,9).

 ASSIGN i-soma = 0.
 DO i-cont = 2 TO 10:
    ASSIGN i-soma = i-soma + (INT(SUBSTR(c-cpf-calc,I-cont - 1,1)) *
                                  i-list-fat1[i-cont]).
 END.
 ASSIGN i-digito = 11 - (i-soma MODULO 11).
 IF i-digito > 9 THEN
    ASSIGN i-digito = 0.
 ASSIGN c-cpf-calc = c-cpf-calc + STRING(i-digito,"9").
 ASSIGN i-soma = 0.
 DO i-cont = 1 TO 10:
    ASSIGN i-soma = i-soma + (INT(SUBSTR(c-cpf-calc,I-cont,1)) *
                              i-list-fat1[i-cont]).
 END.
 ASSIGN i-digito = 11 - (i-soma MODULO 11).
 IF i-digito > 9 THEN
    ASSIGN i-digito = 0.
 ASSIGN c-cpf-calc = c-cpf-calc + STRING(i-digito,"9").


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-validate B-table-Win 
PROCEDURE pi-validate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  {src/adm/template/snd-list.i "emitente"}
  {src/adm/template/snd-list.i "tt-socios"}

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

