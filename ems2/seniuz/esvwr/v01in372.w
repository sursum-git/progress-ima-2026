&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i V99XX999 9.99.99.999}

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
DEF VAR OKpressed  AS LOG.

DEF NEW GLOBAL SHARED VAR h-b01in372  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-essp0189  AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bt-seleciona fi-cod-estabel tg-entrada ~
tg-saida RECT-5 RECT-6 RECT-61 RECT-8 rt-key 
&Scoped-Define DISPLAYED-OBJECTS fi-grup-terc fi-cod-estabel fi-nome-estab ~
tg-entrada tg-saida fi-cliente fi-cod-estabel-ori fi-serie-ori ~
fi-nat-sai-kg fi-nat-sai-un fi-nr-nota-fis-ori fi-serie-sai-kg ~
fi-serie-sai-un fi-nat-oper-ori fi-arquivo-entrada fi-nat-sai-terc ~
fi-serie-sai-terc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 fi-cod-estabel-ori fi-serie-ori fi-nr-nota-fis-ori ~
fi-nat-oper-ori 
&Scoped-define List-5 fi-grup-terc fi-cliente fi-nat-sai-kg fi-nat-sai-un ~
fi-serie-sai-kg fi-serie-sai-un bt-arquivo-entrada fi-arquivo-entrada ~
fi-nat-sai-terc fi-serie-sai-terc 

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
DEFINE BUTTON bt-arquivo-entrada 
     IMAGE-UP FILE "image\im-sea":U
     IMAGE-INSENSITIVE FILE "image\ii-sea":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON bt-seleciona 
     IMAGE-UP FILE "image/img-detnf.bmp":U NO-FOCUS
     LABEL "Processa Separaá∆o Autom†tica" 
     SIZE 90 BY 1.75 TOOLTIP "Processa Separaá∆o Autom†tica".

DEFINE VARIABLE fi-arquivo-entrada AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cliente AS CHARACTER FORMAT "x(12)" 
     LABEL "Cliente":R15 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estab. Destino" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel-ori AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estabelecimento Origem" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-grup-terc AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "GE" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 TOOLTIP "Grupo de Estoque para Terceiros" NO-UNDO.

DEFINE VARIABLE fi-nat-oper-ori AS CHARACTER FORMAT "x(06)" 
     LABEL "Nat Operaá∆o":R15 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nat-sai-kg AS CHARACTER FORMAT "x(06)" 
     LABEL "Nat Operaá∆o":R15 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nat-sai-terc AS CHARACTER FORMAT "x(06)" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nat-sai-un AS CHARACTER FORMAT "x(06)" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis-ori AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nota Fiscal de Origem" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-ori AS CHARACTER FORMAT "X(256)":U 
     LABEL "SÇrie Origem" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88 NO-UNDO.

DEFINE VARIABLE fi-serie-sai-kg AS CHARACTER FORMAT "X(256)":U 
     LABEL "SÇrie" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .79 NO-UNDO.

DEFINE VARIABLE fi-serie-sai-terc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .79 NO-UNDO.

DEFINE VARIABLE fi-serie-sai-un AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 7.5.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 7.5.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 2.25.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.29 BY 2.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 13.25.

DEFINE VARIABLE tg-entrada AS LOGICAL INITIAL no 
     LABEL "Recebimento" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.29 BY .83
     FGCOLOR 1 FONT 6 NO-UNDO.

DEFINE VARIABLE tg-saida AS LOGICAL INITIAL no 
     LABEL "Vendas" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .83
     FGCOLOR 1 FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     fi-grup-terc AT ROW 8.42 COL 80.57 COLON-ALIGNED
     bt-seleciona AT ROW 12.13 COL 3
     fi-cod-estabel AT ROW 2.13 COL 20 COLON-ALIGNED
     fi-nome-estab AT ROW 2.13 COL 25.43 COLON-ALIGNED NO-LABEL
     tg-entrada AT ROW 3.92 COL 4.29
     tg-saida AT ROW 3.88 COL 46.43
     fi-cliente AT ROW 4.75 COL 56 COLON-ALIGNED HELP
          "Natureza da Operaá∆o"
     fi-cod-estabel-ori AT ROW 5.96 COL 20 COLON-ALIGNED
     fi-serie-ori AT ROW 6.96 COL 20 COLON-ALIGNED
     fi-nat-sai-kg AT ROW 6.5 COL 56 COLON-ALIGNED HELP
          "Natureza da Operaá∆o"
     fi-nat-sai-un AT ROW 6.5 COL 66 COLON-ALIGNED HELP
          "Natureza da Operaá∆o" NO-LABEL
     fi-nr-nota-fis-ori AT ROW 7.96 COL 20 COLON-ALIGNED
     fi-serie-sai-kg AT ROW 7.5 COL 56 COLON-ALIGNED
     fi-serie-sai-un AT ROW 7.5 COL 66 COLON-ALIGNED NO-LABEL
     fi-nat-oper-ori AT ROW 9.71 COL 20 COLON-ALIGNED HELP
          "Natureza da Operaá∆o"
     bt-arquivo-entrada AT ROW 10.21 COL 87.72 HELP
          "Escolha do nome do arquivo"
     fi-arquivo-entrada AT ROW 10.25 COL 45.57 COLON-ALIGNED NO-LABEL
     fi-nat-sai-terc AT ROW 6.5 COL 80.57 COLON-ALIGNED HELP
          "Natureza da Operaá∆o" NO-LABEL
     fi-serie-sai-terc AT ROW 7.5 COL 80.57 COLON-ALIGNED NO-LABEL
     " Arquivo de Entrada" VIEW-AS TEXT
          SIZE 14 BY .5 AT ROW 9.25 COL 47.57
     "Kg e M" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 5.75 COL 58.29
          FGCOLOR 12 FONT 6
     "Un" VIEW-AS TEXT
          SIZE 7 BY .75 AT ROW 5.75 COL 68
          FGCOLOR 12 FONT 6
     "Terceiros" VIEW-AS TEXT
          SIZE 8 BY .75 AT ROW 5.75 COL 82.57
          FGCOLOR 12 FONT 6
     RECT-5 AT ROW 4.25 COL 3
     RECT-6 AT ROW 4.25 COL 45
     RECT-61 AT ROW 1.5 COL 3
     RECT-8 AT ROW 9.5 COL 46
     rt-key AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
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
         HEIGHT             = 13.29
         WIDTH              = 94.43.
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

/* SETTINGS FOR BUTTON bt-arquivo-entrada IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-arquivo-entrada IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-cliente IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-cod-estabel-ori IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-grup-terc IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-nat-oper-ori IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-nat-sai-kg IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-nat-sai-terc IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-nat-sai-un IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-nota-fis-ori IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-serie-ori IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-serie-sai-kg IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-serie-sai-terc IN FRAME f-main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi-serie-sai-un IN FRAME f-main
   NO-ENABLE 5                                                          */
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

&Scoped-define SELF-NAME bt-arquivo-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-arquivo-entrada V-table-Win
ON CHOOSE OF bt-arquivo-entrada IN FRAME f-main
DO:
    SYSTEM-DIALOG GET-FILE fi-arquivo-entrada
        TITLE      "Escolha o Arquivo"
        FILTERS    "Listagens  (*.lst)"   "*.lst",
                   "Textos  (*.txt)"   "*.txt",
                   "Todos Arquivos  (*.*)" "*.*"
        INITIAL-DIR SESSION:TEMP-DIRECTORY
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
      
    IF OKpressed = TRUE THEN
       ASSIGN fi-arquivo-entrada:SCREEN-VALUE = fi-arquivo-entrada.
  


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-seleciona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-seleciona V-table-Win
ON CHOOSE OF bt-seleciona IN FRAME f-main /* Processa Separaá∆o Autom†tica */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-cod-estabel
          INPUT FRAME {&FRAME-NAME} tg-entrada
          INPUT FRAME {&FRAME-NAME} tg-saida
          INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ori
          INPUT FRAME {&FRAME-NAME} fi-serie-ori
          INPUT FRAME {&FRAME-NAME} fi-nr-nota-fis-ori
          INPUT FRAME {&FRAME-NAME} fi-nat-oper-ori
          INPUT FRAME {&FRAME-NAME} fi-cliente
          INPUT FRAME {&FRAME-NAME} fi-nat-sai-kg
          INPUT FRAME {&FRAME-NAME} fi-nat-sai-un
          INPUT FRAME {&FRAME-NAME} fi-nat-sai-terc
          INPUT FRAME {&FRAME-NAME} fi-serie-sai-kg
          INPUT FRAME {&FRAME-NAME} fi-serie-sai-un
          INPUT FRAME {&FRAME-NAME} fi-serie-sai-terc
          INPUT FRAME {&FRAME-NAME} fi-grup-terc
          INPUT FRAME {&FRAME-NAME} fi-arquivo-entrada.

   RUN pi-validate.
   IF RETURN-VALUE = "ADM-ERROR" THEN 
      RETURN NO-APPLY.

   RUN pi-select-page IN h-essp0189 (INPUT 2).
   RUN pi-processa IN h-b01in372 (INPUT fi-cod-estabel,
                                  INPUT tg-entrada,
                                  INPUT tg-saida,
                                  INPUT fi-cod-estabel-ori,
                                  INPUT fi-serie-ori,
                                  INPUT fi-nr-nota-fis-ori,
                                  INPUT fi-nat-oper-ori,
                                  INPUT fi-cliente,
                                  INPUT fi-nat-sai-kg,
                                  INPUT fi-nat-sai-un,
                                  INPUT fi-nat-sai-terc,
                                  INPUT fi-serie-sai-kg,
                                  INPUT fi-serie-sai-un,
                                  INPUT fi-serie-sai-terc,
                                  INPUT fi-grup-terc,
                                  INPUT fi-arquivo-entrada).

   RUN local-initialize-fields.
   APPLY 'ENTRY' TO fi-cod-estabel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel V-table-Win
ON LEAVE OF fi-cod-estabel IN FRAME f-main /* Estab. Destino */
DO:
   IF INPUT FRAME {&FRAME-NAME} fi-cod-estabel <> "" THEN DO.
      FIND estabelec WHERE
           estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel
           NO-LOCK NO-ERROR.
      IF NOT AVAIL estabelec THEN DO.
         MESSAGE 'Estabelecimento n∆o Cadastrado'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO SELF.
         RETURN NO-APPLY.
      END.
      ASSIGN fi-nome-estab:SCREEN-VALUE IN FRAME {&FRAME-NAME} = estabelec.nome.

      IF tg-saida:SCREEN-VALUE = 'YES' THEN
         APPLY 'VALUE-CHANGED' TO tg-saida.

      IF tg-entrada:SCREEN-VALUE = 'YES' THEN
         APPLY 'VALUE-CHANGED' TO tg-entrada.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel IN FRAME f-main /* Estab. Destino */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-cod-estabel
                     &campozoom=cod-estabel}
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cod-estabel-ori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-ori V-table-Win
ON LEAVE OF fi-cod-estabel-ori IN FRAME f-main /* Estabelecimento Origem */
DO:
   FIND estabelec WHERE
        estabelec.cod-estabel = INPUT FRAME {&FRAME-NAME} fi-cod-estabel-ori
        NO-LOCK NO-ERROR.
   IF NOT AVAIL estabelec THEN DO.
      MESSAGE 'Estabelecimento Origem n∆o Cadastrado'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cod-estabel-ori V-table-Win
ON MOUSE-SELECT-DBLCLICK OF fi-cod-estabel-ori IN FRAME f-main /* Estabelecimento Origem */
DO:
  {include/zoomvar.i &prog-zoom=adzoom/z01ad107.w
                     &campo=fi-cod-estabel-ori
                     &campozoom=cod-estabel}
                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-entrada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-entrada V-table-Win
ON VALUE-CHANGED OF tg-entrada IN FRAME f-main /* Recebimento */
DO:
   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.

   IF SELF:INPUT-VALUE = YES THEN DO.
      ASSIGN tg-saida:SCREEN-VALUE = 'NO'.
      APPLY 'VALUE-CHANGED' TO tg-saida.

      ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.

      ASSIGN fi-cod-estabel-ori:SCREEN-VALUE IN FRAME {&FRAME-NAME} = param-estoq.estabel-pad
             fi-serie-ori:SCREEN-VALUE IN FRAME {&FRAME-NAME} = para-fat.serie-pad
             fi-nat-oper-ori:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '122b'.  

      APPLY 'entry' TO fi-nr-nota-fis-ori.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-saida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-saida V-table-Win
ON VALUE-CHANGED OF tg-saida IN FRAME f-main /* Vendas */
DO:
    DISABLE {&list-5} WITH FRAME {&FRAME-NAME}.

    IF SELF:INPUT-VALUE = YES THEN DO.
       ASSIGN tg-entrada:SCREEN-VALUE = 'NO'.
       APPLY 'VALUE-CHANGED' TO tg-entrada.

       ENABLE {&list-5} WITH FRAME {&FRAME-NAME}.

       FIND emitente WHERE
            emitente.cgc = estabelec.cgc NO-LOCK NO-ERROR.

       ASSIGN fi-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME} = emitente.nome-abrev
              fi-nat-sai-kg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF INPUT FRAME {&FRAME-NAME} fi-cod-estabel = '4'
                                                                  THEN '511tc4' ELSE '511tc1'
              fi-nat-sai-un:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF INPUT FRAME {&FRAME-NAME} fi-cod-estabel = '4'
                                                                  THEN '511cp4' ELSE '511cp1'
              fi-nat-sai-terc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF INPUT FRAME {&FRAME-NAME} fi-cod-estabel = '4'
                                                                    THEN '512cff' ELSE '511cfb'
              fi-serie-sai-kg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0001'  
              fi-serie-sai-un:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '001'
              fi-serie-sai-terc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '01'
              fi-grup-terc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '70'.

       APPLY 'entry' TO fi-cliente.
       RETURN NO-APPLY.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  fi-cod-estabel:LOAD-MOUSE-POINTER("image/lupa.cur").
  fi-cod-estabel-ori:LOAD-MOUSE-POINTER("image/lupa.cur").
  fi-nr-nota-fis-ori:LOAD-MOUSE-POINTER("image/lupa.cur").

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
    
    /*:T Ponha na pi-validate todas as validaá‰es */
    /*:T N∆o gravar nada no registro antes do dispatch do assign-record e 
       nem na PI-validate. */
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
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

    FIND FIRST para-fat NO-LOCK NO-ERROR.
    FIND FIRST param-estoq NO-LOCK NO-ERROR.

    APPLY 'ENTRY' TO fi-cod-estabel IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize-fields V-table-Win 
PROCEDURE local-initialize-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-cod-estabel-ori = ""
           fi-serie-ori = ""
           fi-nr-nota-fis-ori = ""
           fi-nat-oper-ori = ""
           fi-cliente = ""
           fi-nat-sai-kg = ""
           fi-nat-sai-un = ""
           fi-serie-sai-kg = ""
           fi-serie-sai-un = ""
           tg-entrada = NO
           tg-saida = NO.

    DISP fi-cod-estabel-ori 
         fi-serie-ori 
         fi-nr-nota-fis-ori 
         fi-nat-oper-ori
         fi-cliente
         fi-nat-sai-kg
         fi-nat-sai-un
         fi-serie-sai-kg
         fi-serie-sai-un
         tg-entrada
         tg-saida
         WITH FRAME {&FRAME-NAME}.     

    APPLY 'VALUE-CHANGED' TO tg-entrada.
    APPLY 'VALUE-CHANGED' TO tg-saida.
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
    IF tg-entrada THEN DO.
       FIND nota-fiscal WHERE
            nota-fiscal.cod-estabel = fi-cod-estabel-ori AND
            nota-fiscal.serie = fi-serie-ori AND
            nota-fiscal.nr-nota-fis = fi-nr-nota-fis-ori
            NO-LOCK NO-ERROR.
    
       IF NOT AVAIL nota-fiscal THEN DO.
          MESSAGE 'Nota Fiscal n∆o Encontrada....'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO fi-nr-nota-fis-ori IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR'.
       END.
    
       FIND estabelec WHERE
            estabelec.cod-estabel = fi-cod-estabel NO-LOCK NO-ERROR.
    
       FIND emitente WHERE
            emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.
    
       IF emitente.cgc <> estabelec.cgc THEN DO.
          MESSAGE 'Nota Fiscal n∆o foi faturada para o Estabelecimento informado....'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO fi-nr-nota-fis-ori IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR'.
       END.

       FIND docum-est WHERE
            docum-est.declaracao-import = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
       IF AVAIL docum-est THEN DO.
          MESSAGE 'Nota Fiscal j† foi Digitada no Recebimento... '
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO fi-nr-nota-fis-ori IN FRAME {&FRAME-NAME}.
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

