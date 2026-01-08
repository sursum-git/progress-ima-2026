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
{include/i-prgvrs.i V03ES049 2.04.00.000}

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
DEF VAR c-situacao AS CHAR.
DEF VAR c-tipo-ordem AS CHAR.

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
&Scoped-Define ENABLED-FIELDS ob-etiqueta.num-etiqueta ~
ob-etiqueta.cod-estabel ob-etiqueta.dt-emissao ob-etiqueta.corte-comerc ~
ob-etiqueta.localizacao ob-etiqueta.nuance ob-etiqueta.nr-lote ~
ob-etiqueta.acondic ob-etiqueta.it-codigo ob-etiqueta.cod-refer ~
ob-etiqueta.quantidade ob-etiqueta.nr-ob ob-etiqueta.peso-bruto ~
ob-etiqueta.cod-qualid 
&Scoped-define ENABLED-TABLES ob-etiqueta
&Scoped-define FIRST-ENABLED-TABLE ob-etiqueta
&Scoped-Define ENABLED-OBJECTS rt-mold RECT-44 bt-desenho 
&Scoped-Define DISPLAYED-FIELDS ob-etiqueta.num-etiqueta ~
ob-etiqueta.cod-estabel ob-etiqueta.dt-emissao ob-etiqueta.progressivo ~
ob-etiqueta.corte-comerc ob-etiqueta.localizacao ob-etiqueta.nuance ~
ob-etiqueta.nr-lote ob-etiqueta.acondic ob-etiqueta.it-codigo ~
ob-etiqueta.cod-refer ob-etiqueta.quantidade ob-etiqueta.nr-ob ~
ob-etiqueta.peso-bruto ob-etiqueta.cod-qualid 
&Scoped-define DISPLAYED-TABLES ob-etiqueta
&Scoped-define FIRST-DISPLAYED-TABLE ob-etiqueta
&Scoped-Define DISPLAYED-OBJECTS fi-desc-corte fi-origem fi-fornec ~
fi-desc-item fi-desc-refer fi-un fi-nr-pedcli fi-cliente fi-desc-qualid ~
fi-nr-nota-fis fi-tipo-ordem fi-dt-fat fi-situacao 

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
it-codigo||y|espec.ob-etiqueta.it-codigo
cod-refer||y|espec.ob-etiqueta.cod-refer
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "it-codigo,cod-refer"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-desenho 
     IMAGE-UP FILE "image/im-show.bmp":U
     LABEL "" 
     SIZE 3.86 BY 1 TOOLTIP "Visualiza Imagem do Desenho"
     BGCOLOR 8 .

DEFINE VARIABLE fi-cliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-corte AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 50.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-qualid AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 34.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-fat AS DATE FORMAT "99/99/9999":U 
     LABEL "Dt Saida" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fornec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-nota-fis AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-origem AS CHARACTER FORMAT "X(50)" 
     LABEL "Container" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .88.

DEFINE VARIABLE fi-situacao AS CHARACTER FORMAT "X(20)":U 
     LABEL "Situacao" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tipo-ordem AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tipo da Ordem" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-un AS CHARACTER FORMAT "X(2)" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 4.75.

DEFINE RECTANGLE rt-mold
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 12.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     ob-etiqueta.num-etiqueta AT ROW 1.25 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .88
          FONT 6
     ob-etiqueta.cod-estabel AT ROW 1.25 COL 43 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .88
     ob-etiqueta.dt-emissao AT ROW 1.25 COL 63 COLON-ALIGNED
          LABEL "Emiss∆o Etiqueta"
          VIEW-AS FILL-IN 
          SIZE 10 BY .88
     ob-etiqueta.progressivo AT ROW 2.25 COL 17 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 15.43 BY .88
     ob-etiqueta.corte-comerc AT ROW 2.25 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .88
     fi-desc-corte AT ROW 2.25 COL 65.29 COLON-ALIGNED NO-LABEL
     ob-etiqueta.localizacao AT ROW 3.25 COL 17 COLON-ALIGNED
          LABEL "Doca" FORMAT "XXX/XXX"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .88
     ob-etiqueta.nuance AT ROW 3.25 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .88
     ob-etiqueta.nr-lote AT ROW 3.25 COL 63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     ob-etiqueta.acondic AT ROW 4.25 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.14 BY .88
     fi-origem AT ROW 4.25 COL 63 COLON-ALIGNED WIDGET-ID 20
     fi-fornec AT ROW 4.25 COL 70.43 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     ob-etiqueta.it-codigo AT ROW 5.25 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     fi-desc-item AT ROW 5.25 COL 34.43 COLON-ALIGNED NO-LABEL
     bt-desenho AT ROW 6.21 COL 66.72 WIDGET-ID 4
     ob-etiqueta.cod-refer AT ROW 6.25 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .88
     fi-desc-refer AT ROW 6.25 COL 29.43 COLON-ALIGNED NO-LABEL
     ob-etiqueta.quantidade AT ROW 7.25 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     fi-un AT ROW 7.25 COL 29.86 COLON-ALIGNED NO-LABEL
     ob-etiqueta.nr-ob AT ROW 8.25 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .88
     fi-nr-pedcli AT ROW 9 COL 58 COLON-ALIGNED WIDGET-ID 8
     ob-etiqueta.peso-bruto AT ROW 9.25 COL 17 COLON-ALIGNED
          LABEL "Peso Bruto (KG)"
          VIEW-AS FILL-IN 
          SIZE 8.86 BY .88
     fi-cliente AT ROW 10 COL 58 COLON-ALIGNED WIDGET-ID 18
     ob-etiqueta.cod-qualid AT ROW 10.25 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.14 BY .88
     fi-desc-qualid AT ROW 10.25 COL 20.14 COLON-ALIGNED NO-LABEL
     fi-nr-nota-fis AT ROW 11 COL 58 COLON-ALIGNED WIDGET-ID 10
     fi-tipo-ordem AT ROW 11.25 COL 17 COLON-ALIGNED
     fi-dt-fat AT ROW 12 COL 58 COLON-ALIGNED WIDGET-ID 16
     fi-situacao AT ROW 12.25 COL 17 COLON-ALIGNED
     " Reserva / Faturamento" VIEW-AS TEXT
          SIZE 18 BY .54 AT ROW 8.25 COL 53 WIDGET-ID 14
     rt-mold AT ROW 1 COL 1
     RECT-44 AT ROW 8.5 COL 50 WIDGET-ID 12
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
         HEIGHT             = 12.75
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME f-main:SCROLLABLE       = FALSE
       FRAME f-main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ob-etiqueta.dt-emissao IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi-cliente IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-corte IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-qualid IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-fat IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-fornec IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-nota-fis IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-pedcli IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-origem IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-situacao IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tipo-ordem IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-un IN FRAME f-main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.localizacao IN FRAME f-main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ob-etiqueta.peso-bruto IN FRAME f-main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ob-etiqueta.progressivo IN FRAME f-main
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

&Scoped-define SELF-NAME bt-desenho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-desenho V-table-Win
ON CHOOSE OF bt-desenho IN FRAME f-main
DO:
   RUN esdlg/d01-desenho.w (INPUT ob-etiqueta.cod-refer).
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
     RUN pi-mostra-etiqueta.

     ASSIGN fi-nr-pedcli:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
            fi-cliente:SCREEN-VALUE = ''
            fi-nr-nota-fis:SCREEN-VALUE = ''
            fi-dt-fat:SCREEN-VALUE = ''.

     FIND ped-item-rom WHERE
          ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
          ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
          NO-LOCK NO-ERROR.

     IF AVAIL ped-item-rom THEN DO.
        ASSIGN fi-nr-pedcli:SCREEN-VALUE = ped-item-rom.nr-pedcli
               fi-cliente:SCREEN-VALUE = ped-item-rom.nome-abrev.

        FIND ped-item-res WHERE
             ped-item-res.nr-pedcli = ped-item-rom.nr-pedcli AND 
             ped-item-res.nome-abrev = ped-item-rom.nome-abrev AND
             ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia
             NO-LOCK NO-ERROR.

        IF NOT AVAIL ped-item-res THEN DO.
           FIND nota-fiscal WHERE
                nota-fiscal.cod-estabel = ped-item-res.cod-estabel AND
                nota-fiscal.nr-nota-fis = STRING(ped-item-res.nr-nota-fis,"9999999") AND
                nota-fiscal.serie = ped-item-res.serie
                NO-LOCK  NO-ERROR.
           IF AVAIL nota-fiscal THEN
              ASSIGN fi-nr-nota-fis:SCREEN-VALUE = nota-fiscal.nr-nota-fis
                     fi-dt-fat:SCREEN-VALUE = STRING(nota-fiscal.dt-saida,"99/99/9999").
        END.
     END.

     ASSIGN fi-origem:SCREEN-VALUE = ''.
     IF ob-etiqueta.nr-lote <> 'FD' THEN DO.
        ASSIGN fi-origem:SCREEN-VALUE = ob-etiqueta.ob-origem.

        FIND pp-container WHERE
             pp-container.nr-container = INTEGER(ob-etiqueta.ob-origem) NO-LOCK NO-ERROR.
        IF AVAIL pp-container THEN 
           ASSIGN fi-fornec:SCREEN-VALUE = pp-container.nome-ab-forn.
     END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra-etiqueta V-table-Win 
PROCEDURE pi-mostra-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 IF AVAIL ob-etiqueta THEN DO.
     FIND item WHERE
          item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
     IF AVAIL item THEN
        ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item
               fi-un:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.un.

     bt-desenho:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
     /*FIND item-ext WHERE
          item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
     IF AVAIL item-ext AND item-ext.indigo = NO THEN
        bt-desenho:SENSITIVE IN FRAME {&FRAME-NAME} = YES.*/

     FIND referencia WHERE
          referencia.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.
     IF AVAIL referencia THEN
        ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.

     FIND qualid-tecido WHERE
          qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.
     IF AVAIL qualid-tecido THEN
      ASSIGN fi-desc-qualid:SCREEN-VALUE IN FRAME {&FRAME-NAME} = qualid-tecido.descricao.

     FIND corte-comerc WHERE
          corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
     IF AVAIL corte-comerc THEN
        ASSIGN fi-desc-corte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = corte-comerc.descricao.

     /*
     FIND ordem-benefic WHERE
          ordem-benefic.nr-ob    = ob-etiqueta.nr-ob AND 
          ordem-benefic.dt-ob    = ob-etiqueta.dt-ob AND 
          ordem-benefic.nr-carro = ob-etiqueta.nr-carro NO-LOCK NO-ERROR.
     IF AVAIL ordem-benefic THEN
      ASSIGN fi-tear:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ordem-benefic.tipo-tear.
     */

     {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}  


       IF ob-etiqueta.tipo-ordem = 1 THEN
             ASSIGN fi-tipo-ordem:SCREEN-VALUE = "Produá∆o".

       ELSE 
           IF ob-etiqueta.tipo-ordem = 2 THEN
              ASSIGN fi-tipo-ordem:SCREEN-VALUE = "Retrabalho".
             
       ELSE 
           IF ob-etiqueta.tipo-ordem = 3 THEN
              ASSIGN fi-tipo-ordem:SCREEN-VALUE = "Conserto".

       ELSE 
           IF ob-etiqueta.tipo-ordem = 4 THEN
              ASSIGN fi-tipo-ordem:SCREEN-VALUE = "Industrializaá∆o".

      ASSIGN fi-situacao:SCREEN-VALUE = c-situacao.

 END.

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
    
/*:T    Segue um exemplo de validaá∆o de programa */
/*       find tabela where tabela.campo1 = c-variavel and               */
/*                         tabela.campo2 > i-variavel no-lock no-error. */
      
      /*:T Este include deve ser colocado sempre antes do ut-msgs.p */
/*       {include/i-vldprg.i}                                             */
/*       run utp/ut-msgs.p (input "show":U, input 7, input return-value). */
/*       return 'ADM-ERROR':U.                                            */


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
  {src/adm/template/sndkycas.i "it-codigo" "ob-etiqueta" "it-codigo"}
  {src/adm/template/sndkycas.i "cod-refer" "ob-etiqueta" "cod-refer"}

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

