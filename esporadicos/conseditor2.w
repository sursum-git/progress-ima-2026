&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttCampos
       FIELD labelCampo         AS CHAR FORMAT 'x(100)'
       FIELD nomeCampo          AS CHAR FORMAT 'x(100)'
       FIELD tipo               AS CHAR FORMAT 'x(50)'
       FIELD formato            AS CHAR FORMAT 'x(50)'
       /*FIELD valor              AS CHAR FORMAT 'x(4000)'*/
       FIELD iEXTENT            AS INT INIT 0
       FIELD registro           AS INT.

DEFINE TEMP-TABLE ttValores
    FIELD registro AS INT
    FIELD conteudo AS CHAR FORMAT 'x(4000)'
    FIELD regValor AS INT.


DEFINE VARIABLE hQuery          AS HANDLE  NO-UNDO.
DEFINE VARIABLE hQueryMD        AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBf             AS HANDLE  NO-UNDO EXTENT 10.
DEFINE VARIABLE iNumVar         AS INTEGER NO-UNDO INITIAL 10. 
DEFINE VARIABLE i               AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDB             AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2              AS INTEGER     NO-UNDO.
DEFINE VARIABLE cListaCampos    AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cBanco          AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE pTabela         AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE bhFile          AS HANDLE      NO-UNDO.
DEFINE VARIABLE bhField         AS HANDLE      NO-UNDO.
DEFINE VARIABLE bhTabelas       AS HANDLE      NO-UNDO EXTENT 18.
DEFINE VARIABLE cCondicao       AS CHARACTER   NO-UNDO FORMAT 'x(60)'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button fiTabela edCondicao btexecutar 
&Scoped-Define DISPLAYED-OBJECTS fiTabela edCondicao 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btexecutar 
     LABEL "Buscar Dados" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE edCondicao AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 59 BY 8.25 NO-UNDO.

DEFINE VARIABLE fiTabela AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tabela" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiTabela AT ROW 3.75 COL 16 COLON-ALIGNED WIDGET-ID 2
     edCondicao AT ROW 5 COL 18 NO-LABEL WIDGET-ID 4
     btexecutar AT ROW 13.5 COL 18 WIDGET-ID 6
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 14.25
         WIDTH              = 90
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 90
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 90
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btexecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btexecutar w-livre
ON CHOOSE OF btexecutar IN FRAME f-cad /* Buscar Dados */
DO:
  RUN pibuscardados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fiTabela:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fiTabela edCondicao 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fiTabela edCondicao btexecutar 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "XX9999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pibuscarDados w-livre 
PROCEDURE pibuscarDados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*UPDATE pTabela cCondicao WITH 1 COL .*/
DEFINE VARIABLE cValor              AS CHARACTER   NO-UNDO FORMAT 'x(4000)'.
DEFINE VARIABLE cLinhaDescCampo     AS CHARACTER   NO-UNDO FORMAT 'x(4000)'.
DEFINE VARIABLE cLinhanomeCampo     AS CHARACTER   NO-UNDO FORMAT 'x(4000)'.
DEFINE VARIABLE cLinhaTipoCampo     AS CHARACTER   NO-UNDO FORMAT 'x(4000)'.
DEFINE VARIABLE cLinhaFormatoCampo  AS CHARACTER   NO-UNDO FORMAT 'x(4000)'.
DEFINE VARIABLE cLinhaExtentCampo   AS CHARACTER   NO-UNDO FORMAT 'x(4000)'.
DEFINE VARIABLE cLinhaValorCampo    AS CHARACTER   NO-UNDO FORMAT 'x(8000)'.
DEFINE VARIABLE iCont               AS INTEGER     NO-UNDO.
DEFINE VARIABLE i                   AS INTEGER     NO-UNDO.

REPEAT iDB = 1 TO NUM-DBS:
    ASSIGN cBanco = LDBNAME(iDB).
    CREATE QUERY hQueryMD.
    CREATE BUFFER bhFile  FOR TABLE cBanco + '._file'.
    CREATE BUFFER bhField FOR TABLE cBanco + '._field'.
    /*hQueryMD:SET-BUFFERS(bhFile:HANDLE).*/
    hQueryMD:ADD-BUFFER(bhFile).
    hQueryMD:ADD-BUFFER(bhField).
    hQueryMD:QUERY-PREPARE('for each ' + cBanco + '._file no-lock where _file-name ="' + fiTabela:SCREEN-VALUE IN FRAME {&FRAME-NAME} + '" , each _field of _file no-lock ') NO-ERROR.
    /*hQueryMD:QUERY-PREPARE('for each _file no-lock  , each _field of _file no-lock ').*/
    hQueryMD:QUERY-OPEN. 
    REPEAT:
      hQueryMD:GET-NEXT().
      IF hQueryMD:QUERY-OFF-END THEN LEAVE.
      IF(bhField:BUFFER-FIELD('_extent'):BUFFER-VALUE() > 0) THEN DO:
        DO i = 1 TO bhField:BUFFER-FIELD('_field-name'):EXTENT:
           ASSIGN i = i + 1.
           CREATE ttCampos.
           ASSIGN ttCampos.labelCampo = bhField:BUFFER-FIELD('_label'):BUFFER-VALUE()
                  ttCampos.nomeCampo  = bhField:BUFFER-FIELD('_field-name'):BUFFER-VALUE() + "-" + bhField:BUFFER-FIELD('_extent'):BUFFER-VALUE()
                  ttCampos.tipo       = bhField:BUFFER-FIELD('_data-type'):BUFFER-VALUE() 
                  ttCampos.formato    = bhField:BUFFER-FIELD('_format'):BUFFER-VALUE()
                  ttCampos.iExtent    = bhField:BUFFER-FIELD('_Extent'):BUFFER-VALUE() 
                  ttCampos.registro   = i NO-ERROR .
        END.
      END.
      ELSE DO :
          CREATE ttCampos.
          ASSIGN ttCampos.labelCampo = bhField:BUFFER-FIELD('_label'):BUFFER-VALUE()
                 ttCampos.nomeCampo  = bhField:BUFFER-FIELD('_field-name'):BUFFER-VALUE()
                 ttCampos.tipo       = bhField:BUFFER-FIELD('_data-type'):BUFFER-VALUE() 
                 ttCampos.formato    = bhField:BUFFER-FIELD('_format'):BUFFER-VALUE()
                 ttCampos.registro   = i.
      END.
      
    END.
    hQueryMD:QUERY-CLOSE().
    bhFile:BUFFER-RELEASE().
    bhField:BUFFER-RELEASE().
    DELETE OBJECT hQueryMD.
    DELETE OBJECT bhFile.
    DELETE OBJECT bhField.
END.
CREATE QUERY hQuery.
CREATE BUFFER bhTabelas[1]  FOR TABLE fiTabela:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
hQuery:ADD-BUFFER(bhTabelas[1]).
hQuery:QUERY-PREPARE('for each ' + fiTabela:SCREEN-VALUE IN FRAME {&FRAME-NAME} + ' no-lock ' + edCondicao:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
hQuery:QUERY-OPEN. 
REPEAT:
    hQuery:GET-NEXT().
    IF hQuery:QUERY-OFF-END THEN LEAVE.
    ASSIGN iCont = iCont + 1.
    FOR EACH ttCampos:
        CREATE ttValores.
        ASSIGN ttValores.registro = ttCampos.registro
               ttValores.regValor  = i.
        IF ttCampos.iExtent > 0 THEN DO:
           ASSIGN ttValores.conteudo = string(bhTabelas[1]:BUFFER-FIELD(ttCampos.nomeCampo):BUFFER-VALUE(ttCampos.iExtent)).
        END.
        ELSE DO:
           ASSIGN ttValores.conteudo = string(bhTabelas[1]:BUFFER-FIELD(ttCampos.nomeCampo):BUFFER-VALUE()). 
        END.
        IF ttCampos.tipo = 'character' THEN
           ASSIGN ttValores.conteudo = REPLACE(ttValores.conteudo,CHR(10), "*")
                  ttValores.conteudo = REPLACE(ttValores.conteudo,CHR(13), "*").
        IF ttValores.conteudo = ? THEN
           ASSIGN ttValores.conteudo = '?'.
    END.
END.
hQuery:QUERY-CLOSE().
bhTabelas[1]:BUFFER-RELEASE().
DELETE OBJECT hQuery.
DELETE OBJECT bhTabelas[1].


FOR EACH ttCampos:
    ASSIGN  cLinhaDescCampo     = IF cLinhaDescCampo    = ''        THEN ttCampos.labelCampo        ELSE cLinhaDescCampo    + "|"   + ttCampos.labelCampo
            cLinhaNomeCampo     = IF cLinhaNomeCampo    = ''        THEN ttCampos.nomeCampo         ELSE cLinhaNomeCampo    + "|"   + ttCampos.nomeCampo
            cLinhaTipoCampo     = IF cLinhaTipoCampo    = ''        THEN ttCampos.tipo              ELSE cLinhaTipoCampo    + "|"   + ttCampos.tipo
            cLinhaFormatoCampo  = IF cLinhaFormatoCampo = ''        THEN ttCampos.formato           ELSE cLinhaFormatoCampo + "|"   + ttCampos.formato
            cLinhaExtentCampo   = IF cLinhaExtentCampo  = ''        THEN string(ttCampos.iExtent)   ELSE cLinhaExtentCampo  + "|"   + string(ttCampos.iExtent).
END.

FOR EACH ttValores BREAK BY ttValores.regValor :
    ASSIGN  cLinhaValorCampo    = IF cLinhaValorCampo   = ''        THEN ttValores.conteudo             ELSE cLinhaValorCampo   + "|"   + ttValores.conteudo .
    IF LAST-OF(ttValores.regValor) THEN DO:
       ASSIGN cLinhavalorCampo  = cLinhaValorCampo + CHR(10) + CHR(13).
    END.
END.
OUTPUT TO c:\temp\ttCampos.txt NO-CONVERT .
   PUT cLinhaDescCampo    SKIP
       cLinhaNomeCampo    SKIP
       cLinhaTipoCampo    SKIP
       cLinhaFormatoCampo SKIP
       cLinhaExtentCampo  SKIP
       cLinhaValorCampo   SKIP.
OUTPUT CLOSE.
OUTPUT TO c:\temp\ttCampos1.txt NO-CONVERT .
  FOR EACH ttCampos:
      EXPORT DELIMITER "|" ttCampos.
  END.
OUTPUT CLOSE.

OS-COMMAND SILENT VALUE(" start excel /t t:/especificos/excel/conseditor.xls ").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

