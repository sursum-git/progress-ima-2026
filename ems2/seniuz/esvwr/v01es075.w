&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
{include/i-prgvrs.i V01ES074 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&Scop adm-attribute-dlg support/viewerd.w

/* global variable definitions */

DEF BUFFER b-emitente FOR emitente.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var v-row-parent as rowid no-undo.

DEF VAR b-cod-emitente LIKE emitente.cod-emitente.
DEF VAR b-nome-abrev   LIKE emitente.nome-abrev.
DEF VAR b-cidade       LIKE emitente.cidade.
DEF VAR b-estado       LIKE emitente.estado.
DEF VAR c-cpf-calc  AS CHAR FORMAT "x(19)".
DEF VAR c-lst-cpf   AS CHAR.
DEF VAR c-gr-cpf    AS CHAR.
DEF VAR c-nome-cpf  AS CHAR.
DEF VAR lista-aux   AS CHAR.
DEF VAR c-cpf       AS CHAR.
DEF VAR i-cod-emit  AS INT.
DEF VAR i-cont      AS INT.

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
&Scoped-define EXTERNAL-TABLES emitente
&Scoped-define FIRST-EXTERNAL-TABLE emitente


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR emitente.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS emitente.int-2 
&Scoped-define ENABLED-TABLES emitente
&Scoped-define FIRST-ENABLED-TABLE emitente
&Scoped-Define ENABLED-OBJECTS rt-key rt-mold 
&Scoped-Define DISPLAYED-FIELDS emitente.int-2 
&Scoped-define DISPLAYED-TABLES emitente
&Scoped-define FIRST-DISPLAYED-TABLE emitente
&Scoped-Define DISPLAYED-OBJECTS fi-cpf fi-socio sl-cpf-nome 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ADM-MODIFY-FIELDS,List-4,List-5,List-6 */
&Scoped-define List-4 bt-confirma fi-cpf fi-socio sl-cpf-nome 

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
DEFINE BUTTON bt-confirma 
     IMAGE-UP FILE "image/imt-chck1.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13.

DEFINE VARIABLE fi-cpf AS CHARACTER FORMAT "XXXXXXXXXXXXXXXX":U 
     LABEL "CNPJ/CPF" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-socio AS CHARACTER FORMAT "X(40)":U 
     LABEL "Nome S¢cio" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-key
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 84 BY 1.75.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 84 BY 9.25.

DEFINE VARIABLE sl-cpf-nome AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 83 BY 8.75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     bt-confirma AT ROW 1.25 COL 80.14
     fi-cpf AT ROW 1.38 COL 9.43 COLON-ALIGNED
     fi-socio AT ROW 1.38 COL 34.14 COLON-ALIGNED
     emitente.int-2 AT ROW 1.38 COL 75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2 BY .79
     sl-cpf-nome AT ROW 3 COL 2 NO-LABEL
     rt-key AT ROW 1.08 COL 1
     rt-mold AT ROW 3 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: mgcad.emitente
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
         HEIGHT             = 11.29
         WIDTH              = 84.72.
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

/* SETTINGS FOR BUTTON bt-confirma IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-cpf IN FRAME f-main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-socio IN FRAME f-main
   NO-ENABLE 4                                                          */
ASSIGN 
       emitente.int-2:HIDDEN IN FRAME f-main           = TRUE.

/* SETTINGS FOR SELECTION-LIST sl-cpf-nome IN FRAME f-main
   NO-ENABLE 4                                                          */
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

&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma V-table-Win
ON CHOOSE OF bt-confirma IN FRAME f-main /* Button 1 */
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

    IF LENGTH(fi-socio:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "NOME DO S‡CIO inv†lido...." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO fi-socio.
       RETURN 'ADM-ERROR':U.
    END.

    ASSIGN c-gr-cpf   = fi-cpf:SCREEN-VALUE + ' - ' 
           c-nome-cpf = c-gr-cpf + fi-socio:SCREEN-VALUE 
           c-lst-cpf  = sl-cpf-nome:LIST-ITEMS.

   IF bt-confirma:IMAGE-UP = "image\im-era.bmp" THEN DO.
       ASSIGN c-lst-cpf = c-lst-cpf + ",".
       ASSIGN c-lst-cpf = REPLACE(c-lst-cpf,sl-cpf-nome:SCREEN-VALUE + ",","").
       
       IF LENGTH(c-lst-cpf) <> 0 THEN
          OVERLAY(c-lst-cpf,LENGTH(c-lst-cpf),1) = "".
       ELSE
          bt-confirma:LOAD-IMAGE("image\im-chck1.bmp").
    END.
    ELSE DO.
        IF INDEX(c-lst-cpf,c-gr-cpf) > 0 AND sl-cpf-nome:SCREEN-VALUE = ? THEN DO.
            MESSAGE "CPF/CNPJ j† est† Cadastrado...." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY 'entry' TO fi-cpf.
            RETURN 'ADM-ERROR':U.
        END.

        IF INDEX(c-lst-cpf,c-gr-cpf) > 0 THEN 
           ASSIGN c-lst-cpf = REPLACE(c-lst-cpf,sl-cpf-nome:SCREEN-VALUE,c-nome-cpf).
        ELSE
           ASSIGN c-lst-cpf = IF sl-cpf-nome:LIST-ITEMS = ?
                                THEN c-nome-cpf 
                                ELSE c-lst-cpf + "," + c-nome-cpf.
    END.

    ASSIGN sl-cpf-nome:LIST-ITEMS = IF c-lst-cpf = "" THEN ? ELSE c-lst-cpf.

    IF c-lst-cpf = '' THEN
       ASSIGN SELF:SENSITIVE = NO.

    ASSIGN fi-cpf:SCREEN-VALUE   = ""
           fi-socio:SCREEN-VALUE = "".

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-cpf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-cpf V-table-Win
ON LEAVE OF fi-cpf IN FRAME f-main /* CNPJ/CPF */
DO:
    FIND b-emitente USE-INDEX cgc WHERE b-emitente.cgc = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAIL b-emitente  THEN DO:
       ASSIGN b-cod-emitente = b-emitente.cod-emitente
              b-nome-abrev   = b-emitente.nome-abrev
              b-cidade       = b-emitente.cidade
              b-estado       = b-emitente.estado.
       FIND FIRST socio-emit USE-INDEX indice1 WHERE socio-emit.cnpj-cpf = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAIL socio-emit THEN DO:
          FIND b-emitente WHERE b-emitente.cod-emitente = socio-emit.cod-emitente NO-LOCK NO-ERROR.
          MESSAGE "CPF/CNPJ  ?  foi encontrado no cadastro de clientes" SKIP
                  "Cliente: " + STRING(b-cod-emitente) + " - " + b-nome-abrev SKIP
                  "Cidade.: " + TRIM(b-cidade) + "/" + b-estado SKIP
                  "-------------------------------------------------------------------" SKIP
                  "FOI ENCONTRADO TAMBêM NO CADASTRO DE S‡CIOS" SKIP
                  "Cliente: " + STRING(b-emitente.cod-emitente) + " - " + b-emitente.nome-abrev SKIP
                  "Cidade.: " + TRIM(b-emitente.cidade) + "/" + b-emitente.estado
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
       ELSE
           MESSAGE "CPF/CNPJ  ?  foi encontrado no cadastro de clientes" SKIP
                   "Cliente: " + STRING(b-emitente.cod-emitente) + " - " + b-emitente.nome-abrev SKIP
                   "Cidade.: " + TRIM(b-emitente.cidade) + "/" + b-emitente.estado
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.

    END.
    ELSE DO:
       FIND FIRST socio-emit USE-INDEX indice1 WHERE socio-emit.cnpj-cpf = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF AVAIL socio-emit THEN DO:
          FIND b-emitente WHERE b-emitente.cod-emitente = socio-emit.cod-emitente NO-LOCK NO-ERROR.
          MESSAGE "CPF/CNPJ  ?  foi encontrado no cadastro de S‡CIOS" SKIP
                  "Cliente: " + STRING(b-emitente.cod-emitente) + " - " + b-emitente.nome-abrev SKIP
                  "Cidade.: " + TRIM(b-emitente.cidade) + "/" + b-emitente.estado
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
    END.
    ASSIGN bt-confirma:SENSITIVE = YES
           fi-socio:SCREEN-VALUE = "".
    bt-confirma:LOAD-IMAGE("image\im-chck1.bmp").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-socio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-socio V-table-Win
ON LEAVE OF fi-socio IN FRAME f-main /* Nome S¢cio */
DO:
    ASSIGN bt-confirma:SENSITIVE = YES.
    bt-confirma:LOAD-IMAGE("image\im-chck1.bmp").
    IF LENGTH(fi-socio:SCREEN-VALUE) = 0 THEN DO:
       APPLY 'entry' TO fi-socio.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl-cpf-nome
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl-cpf-nome V-table-Win
ON VALUE-CHANGED OF sl-cpf-nome IN FRAME f-main
DO:
    ASSIGN fi-cpf:SCREEN-VALUE = ENTRY(1,SELF:SCREEN-VALUE,"-")
           fi-socio:SCREEN-VALUE = TRIM(ENTRY(2,SELF:SCREEN-VALUE,"-")).
    bt-confirma:LOAD-IMAGE("image\im-era.bmp").  
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

  ASSIGN lista-aux   = "".
  ASSIGN sl-cpf-nome:LIST-ITEMS IN FRAME {&FRAME-NAME} = lista-aux. 

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

    ASSIGN i-cod-emit = emitente.cod-emitente
           lista-aux  = sl-cpf-nome:LIST-ITEMS IN FRAME {&FRAME-NAME}.
    
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
    if RETURN-VALUE = 'ADM-ERROR':U then 
        return 'ADM-ERROR':U.
    
    /*:T Todos os assignÔs n∆o feitos pelo assign-record devem ser feitos aqui */  
    /* Code placed here will execute AFTER standard behavior.    */

    IF lista-aux <> "" THEN DO.

       /* DELETAR O(s) REGISTROS DA LISTA DA CARGA */
       /* ---------------------------------------- */
      FOR EACH socio-emit WHERE
               socio-emit.cod-emitente = i-cod-emit.  
          DELETE socio-emit.
      END.

      /* GRAVAR A NOVA LISTA */
      /* ------------------- */
      DO i-cont = 1 TO NUM-ENTRIES(lista-aux,",").
         ASSIGN c-gr-cpf = ENTRY(i-cont,lista-aux,",").
         ASSIGN c-cpf = TRIM(SUBSTR(c-gr-cpf,1,INDEX(c-gr-cpf,"-") - 1)).
         FIND socio-emit WHERE socio-emit.cod-emitente = i-cod-emit AND
                               socio-emit.cnpj-cpf     = c-cpf 
                               NO-ERROR.
         IF NOT AVAIL socio-emit THEN DO:
             CREATE socio-emit.
             ASSIGN socio-emit.cod-emitente = i-cod-emit
                    socio-emit.cnpj-cpf     = c-cpf
                    socio-emit.nome-socio   = TRIM(SUBSTR(c-gr-cpf, INDEX(c-gr-cpf,"-") + 1,40)).
         END.
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
    DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    ASSIGN sl-cpf-nome:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
    
    
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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN lista-aux   = ""
           sl-cpf-nome = "".
    FOR EACH socio-emit WHERE
             socio-emit.cod-emitente = emitente.cod-emitente NO-LOCK.  
           ASSIGN lista-aux = IF lista-aux = ""
                              THEN STRING(socio-emit.cnpj-cpf) + " - " +
                                   socio-emit.nome-socio
                              ELSE lista-aux + "," + STRING(socio-emit.cnpj-cpf) + " - " +
                                   socio-emit.nome-socio.
    END.
    ASSIGN sl-cpf-nome:LIST-ITEMS IN FRAME {&FRAME-NAME} = lista-aux. 


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
    ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
    ASSIGN emitente.int-2:SENSITIVE IN FRAME {&FRAME-NAME} = NO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-cnpj V-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-calc-cpf V-table-Win 
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
    
   DO i-cont = 1 TO NUM-ENTRIES(sl-cpf-nome:LIST-ITEMS,",").
      ASSIGN c-gr-cpf = ENTRY(i-cont,sl-cpf-nome:LIST-ITEMS,",").
      ASSIGN c-cpf = TRIM(SUBSTR(c-gr-cpf,1,INDEX(c-gr-cpf,"-") - 1)).
      ASSIGN c-nome-cpf = TRIM(SUBSTR(c-gr-cpf,INDEX(c-gr-cpf,"-") + 1, 40)).

      IF LENGTH(c-nome-cpf) = 0 THEN DO:
         MESSAGE "NOME DO S‡CIO Inv†lido...." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO fi-socio.
         RETURN 'ADM-ERROR':U.
      END.
      IF LENGTH(c-cpf) <> 11 AND LENGTH(c-cpf) <> 14 THEN DO:
         MESSAGE "CPF/CNPJ Inv†lido...." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'entry' TO fi-cpf.
         RETURN 'ADM-ERROR':U.
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
  {src/adm/template/snd-list.i "emitente"}

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

