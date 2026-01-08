&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE tt-arquivo
    FIELD num-etiqueta AS CHAR.

/* Parameters Definitions ---                                           */
DEF BUFFER b-nota-fiscal FOR nota-fiscal.

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-form-epl    AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl    AS CHAR FORMAT "x(50)".
DEF VAR i-tot-etq     AS INT.
DEF VAR i-etq-imp     AS INT.
DEF VAR c-seq         AS CHAR.
DEF VAR c-origem      AS CHAR.
DEF VAR c-emissao     AS CHAR.
DEF VAR c-pedido      AS CHAR.
DEF VAR c-localiz     AS CHAR. 
DEF VAR c-cod-estabel LIKE nota-fiscal.cod-estabel INIT '2'.
DEF VAR c-desc-item   AS CHAR FORMAT "x(40)".
DEF VAR c-comando     AS CHAR.
DEF VAR c-impressora  AS CHAR.
DEF VAR l-erro        AS LOG.
DEF VAR i-digito      AS INT NO-UNDO.

{esinc/sz-pcl.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-50 RECT-51 fi-nr-nota-fis ~
fi-num-etiqueta bt-imprime bt-importa bt-confirma bt-cancela bt-undo ~
bt-sair 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-nota-fis fi-nr-nota-tri fi-volumes ~
fi-num-etiqueta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 bt-fardo bt-imprime bt-importa bt-confirma bt-cancela ~
bt-undo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancela AUTO-GO 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.58 TOOLTIP "Cancela Etiquetas Impressas".

DEFINE BUTTON bt-confirma AUTO-GO 
     IMAGE-UP FILE "image/im-chck1.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.58 TOOLTIP "Confirma Nota".

DEFINE BUTTON bt-fardo AUTO-GO 
     IMAGE-UP FILE "image/im-res-i.bmp":U NO-FOCUS
     LABEL "" 
     SIZE 5 BY 1.5 TOOLTIP "Alteraá‰es Fardos"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-importa AUTO-GO 
     IMAGE-UP FILE "image/im-upima.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.58 TOOLTIP "Importar dados do Coletor para impress∆o de Etiquetas.".

DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-f-dv.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.58 TOOLTIP "Imprime Etiqueta".

DEFINE BUTTON bt-sair AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-exi.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.58 TOOLTIP "Sair".

DEFINE BUTTON bt-undo AUTO-GO 
     IMAGE-UP FILE "image/im-can.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.58 TOOLTIP "Encerra Nota".

DEFINE VARIABLE fi-nr-nota-fis AS CHARACTER FORMAT "x(10)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.75
     FONT 20 NO-UNDO.

DEFINE VARIABLE fi-nr-nota-tri AS CHARACTER FORMAT "x(9)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.75
     FONT 20 NO-UNDO.

DEFINE VARIABLE fi-num-etiqueta AS CHARACTER FORMAT "x(10)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.75
     FONT 20 NO-UNDO.

DEFINE VARIABLE fi-volumes AS CHARACTER FORMAT "999/999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.75
     FONT 20 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 43 BY 2
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 13.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bt-fardo AT ROW 7.08 COL 45.43 WIDGET-ID 10
     fi-nr-nota-fis AT ROW 2.75 COL 27 COLON-ALIGNED NO-LABEL
     fi-nr-nota-tri AT ROW 4.75 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fi-volumes AT ROW 6.75 COL 27 COLON-ALIGNED NO-LABEL
     fi-num-etiqueta AT ROW 8.75 COL 27 COLON-ALIGNED NO-LABEL
     bt-imprime AT ROW 11.5 COL 29.86
     bt-importa AT ROW 11.5 COL 36.14 WIDGET-ID 16
     bt-confirma AT ROW 11.5 COL 46.43
     bt-cancela AT ROW 11.5 COL 52.72
     bt-undo AT ROW 11.5 COL 59
     bt-sair AT ROW 11.5 COL 65.29
     "Volumes:" VIEW-AS TEXT
          SIZE 18.29 BY 1.5 AT ROW 6.83 COL 10.72 WIDGET-ID 6
          FONT 20
     "NF Triang.:" VIEW-AS TEXT
          SIZE 21.86 BY 1.5 AT ROW 5 COL 7.14 WIDGET-ID 4
          FONT 20
     "Nota Fiscal:" VIEW-AS TEXT
          SIZE 22.43 BY 1.5 AT ROW 2.88 COL 6
          FONT 20
     "Etiqueta:" VIEW-AS TEXT
          SIZE 17 BY 1.5 AT ROW 8.83 COL 11.86
          FONT 20
     RECT-50 AT ROW 11.25 COL 29
     RECT-51 AT ROW 1 COL 1 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.14 BY 13.29
         CANCEL-BUTTON bt-sair.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Emiss∆o de Etiqueta da Expediá∆o - ESSP0139"
         HEIGHT             = 13.29
         WIDTH              = 79.14
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON bt-cancela IN FRAME DEFAULT-FRAME
   4                                                                    */
/* SETTINGS FOR BUTTON bt-confirma IN FRAME DEFAULT-FRAME
   4                                                                    */
/* SETTINGS FOR BUTTON bt-fardo IN FRAME DEFAULT-FRAME
   NO-ENABLE 4                                                          */
/* SETTINGS FOR BUTTON bt-importa IN FRAME DEFAULT-FRAME
   4                                                                    */
/* SETTINGS FOR BUTTON bt-imprime IN FRAME DEFAULT-FRAME
   4                                                                    */
/* SETTINGS FOR BUTTON bt-undo IN FRAME DEFAULT-FRAME
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-nr-nota-tri IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-volumes IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Emiss∆o de Etiqueta da Expediá∆o - ESSP0139 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Emiss∆o de Etiqueta da Expediá∆o - ESSP0139 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela C-Win
ON CHOOSE OF bt-cancela IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    MESSAGE "Tem Certeza que deseja Cancelar TODAS as etiquetas Impressas para essa Nota ?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            TITLE "Cancelamento de Etiquetas" UPDATE choice AS LOGICAL.
    IF choice = YES THEN DO.
       FOR EACH ped-item-res WHERE
                ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
                ped-item-res.serie       = nota-fiscal.serie AND
                ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
                ped-item-res.faturado    = YES NO-LOCK
                BREAK BY STRING(ped-item-res.volume-ini) + 
                         STRING(ped-item-res.volume-fim).
    
           IF ped-item-res.lote <> '' AND
              LOOKUP(SUBSTR(ped-item-res.sigla-emb,1,1),'P,F') > 0 
              THEN NEXT.

           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia 
                    SHARE-LOCK.
               ASSIGN ped-item-rom.marca = ''.
               /*
               FIND ob-etiqueta WHERE
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    USE-INDEX indice4 SHARE-LOCK NO-ERROR.
               IF AVAIL ob-etiqueta THEN
                  ASSIGN ob-etiqueta.situacao = 4.
               */   
           END.
       END.

       ASSIGN fi-nr-nota-fis:SENSITIVE = YES
              fi-nr-nota-fis:SCREEN-VALUE = ''
              fi-volumes:SCREEN-VALUE = ''.
    
       APPLY 'entry' TO fi-nr-nota-fis.
    END.
    ELSE 
       APPLY 'entry' TO fi-num-etiqueta.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-confirma
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-confirma C-Win
ON CHOOSE OF bt-confirma IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    ASSIGN l-erro = NO.
    FOR EACH ped-item-res WHERE
             ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
             ped-item-res.serie       = nota-fiscal.serie AND
             ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
             ped-item-res.faturado    = YES 
             NO-LOCK.

        FOR EACH ped-item-rom WHERE
                 ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                 ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                 ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia 
                 SHARE-LOCK.
            IF ped-item-rom.marca = '' THEN DO.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = nota-fiscal.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta 
                    USE-INDEX indice4 SHARE-LOCK NO-ERROR.

               MESSAGE "A Etiqueta " ped-item-rom.num-etiqueta " n∆o foi Impressa para essa Nota..." SKIP(1)
                       "Localizaá∆o: " STRING(ob-etiqueta.localizacao,"999/999")
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               ASSIGN l-erro = YES.
            END.
        END.
    END.

    IF NOT l-erro THEN DO.
       ASSIGN fi-nr-nota-fis:SENSITIVE = YES
              fi-nr-nota-fis:SCREEN-VALUE = ''
              fi-volumes:SCREEN-VALUE = ''.

       APPLY 'entry' TO fi-nr-nota-fis.
    END.
    ELSE 
       APPLY 'entry' TO fi-num-etiqueta.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-fardo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-fardo C-Win
ON CHOOSE OF bt-fardo IN FRAME DEFAULT-FRAME
DO:
   RUN esp/essp0154d.p (INPUT nota-fiscal.nr-pedcli).
   RUN pi-volumes (INPUT INTEGER(nota-fiscal.nr-pedcli)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-importa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-importa C-Win
ON CHOOSE OF bt-importa IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    RUN pi-importa.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    /*  
    IF i-digito <> INT(fn-calc-digito(INPUT fi-num-etiqueta:SCREEN-VALUE)) THEN DO.
       MESSAGE "Somente Leitura com o Coletor Ç Permitido ! ! !"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'entry' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.
    */ 

    IF NOT AVAIL nota-fiscal THEN DO.
       APPLY 'ENTRY' TO fi-nr-nota-fis.
       RETURN NO-APPLY.
    END.

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel  = nota-fiscal.cod-estabel AND
         ob-etiqueta.num-etiqueta = INTEGER(fi-num-etiqueta:SCREEN-VALUE) 
         USE-INDEX indice4 SHARE-LOCK NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN DO.
       MESSAGE 'Etiqueta n∆o Cadastrada no Sistema...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY 'entry' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.

    FIND ped-item-rom WHERE
         ped-item-rom.nome-abrev = nota-fiscal.nome-ab-cli AND
         ped-item-rom.nr-pedcli  = nota-fiscal.nr-pedcli AND
         ped-item-rom.cod-estabel = nota-fiscal.cod-estabel AND
         ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
         NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item-rom THEN DO.
       FIND ped-item-rom WHERE
            ped-item-rom.nome-abrev = b-nota-fiscal.nome-ab-cli AND
            ped-item-rom.nr-pedcli  = b-nota-fiscal.nr-pedcli AND
            ped-item-rom.cod-estabel = b-nota-fiscal.cod-estabel AND
            ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta
            NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-item-rom THEN DO.
          MESSAGE 'Etiqueta n∆o Encontrada nas Reservas da NOTA FISCAL'
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'entry' TO fi-num-etiqueta.
          RETURN NO-APPLY.
       END.
    END.
                    
    IF ped-item-rom.marca <> '' AND c-seg-usuario <> 'super' THEN DO. /* Etiqueta Impressa */
       RUN pi-valida.
       IF RETURN-VALUE = 'NOK' THEN DO.
          APPLY 'entry' TO fi-num-etiqueta.
          RETURN NO-APPLY.
       END.
    END.
    
    FIND it-nota-fisc OF nota-fiscal WHERE
         it-nota-fisc.it-codigo = ob-etiqueta.it-codigo AND
         it-nota-fisc.cod-refer = ob-etiqueta.cod-refer AND
         it-nota-fisc.nr-seq-ped = ped-item-rom.nr-sequencia
         NO-LOCK NO-ERROR.
    IF NOT AVAIL it-nota-fisc THEN DO.
       MESSAGE 'Etiqueta n∆o Pertence a essa NOTA FISCAL'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.

       APPLY 'entry' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.

    RUN pi-etiqueta.

    ASSIGN fi-num-etiqueta:SCREEN-VALUE = ''.
    APPLY 'entry' TO fi-num-etiqueta.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair C-Win
ON CHOOSE OF bt-sair IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-undo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-undo C-Win
ON CHOOSE OF bt-undo IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    ASSIGN fi-nr-nota-fis:SENSITIVE = YES
           fi-nr-nota-fis:SCREEN-VALUE = ''
           fi-volumes:SCREEN-VALUE = ''
           fi-num-etiqueta:SCREEN-VALUE = ''.
    
    APPLY 'entry' TO fi-nr-nota-fis.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-nota-fis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-fis C-Win
ON ENTRY OF fi-nr-nota-fis IN FRAME DEFAULT-FRAME
DO:
   ASSIGN fi-nr-nota-tri:SCREEN-VALUE = ''.
   ASSIGN fi-nr-nota-tri:SENSITIVE = NO.
   ASSIGN bt-sair:SENSITIVE = YES.
   DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-fis C-Win
ON LEAVE OF fi-nr-nota-fis IN FRAME DEFAULT-FRAME
DO:

  FIND nota-fiscal WHERE
       nota-fiscal.cod-estabel = c-cod-estabel      AND
       nota-fiscal.serie       = para-fat.serie-pad AND 
       nota-fiscal.nr-nota-fis = STRING(INT(fi-nr-nota-fis:SCREEN-VALUE),"9999999")
       NO-LOCK NO-ERROR.
  
  IF NOT AVAIL nota-fiscal THEN DO.
     MESSAGE 'Nota Fiscal n∆o Cadastrada no Sistema...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
  END.
  
  IF nota-fiscal.dt-embarque <> ? THEN DO.
     MESSAGE 'Nota Fiscal j† foi Despachada pela Portaria,' SKIP
             'Imposs°vel emitir Etiquetas...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
  END.

  FIND b-nota-fiscal OF nota-fiscal NO-LOCK NO-ERROR.  /* Igual o Buffer a Tabela */

  ASSIGN SELF:SENSITIVE = NO
         bt-sair:SENSITIVE = NO.

  ENABLE {&list-4} WITH FRAME {&FRAME-NAME}.
  RUN pi-volumes (INPUT INTEGER(nota-fiscal.nr-pedcli)).

  IF nota-fiscal.nome-abrev <> '' AND /* Ç Triangular */
     nota-fiscal.nome-abrev <> nota-fiscal.nome-ab-cli THEN DO.  /* Ç a nota de Origem */
     ASSIGN fi-nr-nota-tri:SENSITIVE = YES.
     APPLY "ENTRY" TO fi-nr-nota-tri.
     RETURN NO-APPLY.
  END.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-fis C-Win
ON RETURN OF fi-nr-nota-fis IN FRAME DEFAULT-FRAME
DO:
   APPLY 'TAB' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-nota-tri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-tri C-Win
ON LEAVE OF fi-nr-nota-tri IN FRAME DEFAULT-FRAME
DO:
  /* Poisicona o Buffer na Nota Triangular */
  FIND b-nota-fiscal WHERE
       b-nota-fiscal.cod-estabel = c-cod-estabel      AND
       b-nota-fiscal.serie       = para-fat.serie-pad AND 
       b-nota-fiscal.nr-nota-fis = STRING(INT(fi-nr-nota-tri:SCREEN-VALUE),"9999999")
       NO-LOCK NO-ERROR.
  
  IF NOT AVAIL b-nota-fiscal THEN DO.
     MESSAGE 'Nota Fiscal n∆o Cadastrada no Sistema...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.

     ASSIGN fi-nr-nota-fis:SENSITIVE = YES
            fi-volumes:SCREEN-VALUE = ''.

     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
  END.

  IF b-nota-fiscal.nome-abrev = '' THEN DO.
     MESSAGE 'Nota Fiscal N∆o Ç Triangular.....'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN fi-nr-nota-fis:SENSITIVE = YES
            fi-volumes:SCREEN-VALUE = ''.

     APPLY 'entry' TO fi-nr-nota-fis.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-nota-tri C-Win
ON RETURN OF fi-nr-nota-tri IN FRAME DEFAULT-FRAME
DO:
   APPLY 'TAB' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON RETURN OF fi-num-etiqueta IN FRAME DEFAULT-FRAME
DO:
   APPLY 'CHOOSE' TO bt-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON VALUE-CHANGED OF fi-num-etiqueta IN FRAME DEFAULT-FRAME
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     (SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) < '0' OR
     SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) > '9') THEN DO.
     BELL.
     APPLY 'backspace' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN i-digito = 99.
  IF LENGTH(SELF:SCREEN-VALUE) = 10 THEN DO.
     ASSIGN i-digito = INT(SUBSTRING(SELF:SCREEN-VALUE,10,1)).
     ASSIGN SELF:SCREEN-VALUE = SUBSTR(SELF:SCREEN-VALUE,1,9).
     APPLY 'END' TO SELF.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-volumes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-volumes C-Win
ON LEAVE OF fi-volumes IN FRAME DEFAULT-FRAME
DO:
   IF SELF:SCREEN-VALUE <> '' THEN DO.
      FIND nota-fiscal WHERE
           nota-fiscal.cod-estabel = c-cod-estabel AND
           nota-fiscal.serie = para-fat.serie-pad AND
           nota-fiscal.nr-nota-fis = STRING(INT(fi-nr-nota-fis:SCREEN-VALUE),"9999999")
           NO-LOCK NO-ERROR.
    
      IF NOT AVAIL nota-fiscal THEN DO.
         MESSAGE 'Nota Fiscal n∆o Cadastrada no Sistema...'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-nr-nota-fis.
         RETURN NO-APPLY.
      END.
    
      ASSIGN i-tot-etq = 0.
      FOR EACH nota-embal WHERE
               nota-embal.cod-estabel = nota-fiscal.cod-estabel AND
               nota-embal.serie       = nota-fiscal.serie AND
               nota-embal.nr-nota-fis = nota-fiscal.nr-nota-fis NO-LOCK 
               USE-INDEX ch-nota-emb BREAK BY nota-embal.sigla-emb:
    
          ACCUMULATE nota-embal.qt-volumes (TOTAL).
    
          IF LAST (nota-embal.sigla-emb) THEN
             ASSIGN i-tot-etq = ACCUM TOTAL nota-embal.qt-volumes.
      END.
   END.
   ASSIGN SELF:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

FIND FIRST para-fat NO-LOCK NO-ERROR.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
                /*
  RUN btb/btb910za.p.
  IF RETURN-VALUE = 'NOK' THEN
     APPLY 'choose' TO bt-sair.
  
   ON 'return':U ANYWHERE DO:
      APPLY 'CHOOSE' TO bt-imprime IN FRAME {&FRAME-NAME}.
   END.
  */

  ASSIGN c-cod-estabel = '1'.

  ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + c-seg-usuario + ".epl"
         c-form-epl = "M:\ems206\especificos\etiqueta\form-exp-ima.epl".

/*   RUN esapi/busca-estab-usuar.p (INPUT c-seg-usuario,   */
/*                                  OUTPUT c-cod-estabel). */
  IF c-cod-estabel = "" THEN DO:
     MESSAGE "Usu†rio: " + c-seg-usuario + " n∆o cadastrado no parÉmetro de permiss‰es."
         VIEW-AS ALERT-BOX WARNING BUTTONS OK.
     APPLY "CLOSE":U TO THIS-PROCEDURE.
     RETURN NO-APPLY.
  END.

  IF c-cod-estabel = '1' THEN /* Ima Textil */
     ASSIGN c-form-epl = "M:\ems206\especificos\etiqueta\form-nf-ima.epl".

  APPLY 'ENTRY' TO fi-nr-nota-fis.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY fi-nr-nota-fis fi-nr-nota-tri fi-volumes fi-num-etiqueta 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-50 RECT-51 fi-nr-nota-fis fi-num-etiqueta bt-imprime bt-importa 
         bt-confirma bt-cancela bt-undo bt-sair 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiqueta C-Win 
PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

    FIND item WHERE
         item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    ASSIGN c-desc-item = ITEM.descricao-1 + TRIM(ITEM.descricao-2).

    ASSIGN c-origem = ""
           c-emissao = "Emissao: " + STRING(b-nota-fiscal.dt-emis,"99/99/9999").
    FIND emitente WHERE
         emitente.nome-abrev = b-nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
    IF emitente.natureza = 3 THEN
       ASSIGN c-origem = "ORIGEN: BRASIL"
              c-emissao = "Emision: " + STRING(b-nota-fiscal.dt-emis,"99/99/9999").

/*     ASSIGN c-pedido = "Pedido: " + nota-fiscal.nr-pedcli. */

    FIND transporte WHERE
         transporte.nome-abrev = nota-fiscal.nome-trans NO-LOCK NO-ERROR.

    IF ped-item-rom.marca = '' THEN
       ASSIGN i-etq-imp = i-etq-imp + 1
              c-seq     = TRIM(STRING(i-etq-imp)) + "/" + TRIM(STRING(i-tot-etq)). 
    ELSE
       ASSIGN c-seq = TRIM(STRING(INT(SUBSTR(ped-item-rom.marca,5,3)))) + "/" + TRIM(STRING(i-tot-etq)).

    ASSIGN c-localiz = SUBSTR(ob-etiqueta.localizacao,1,3) + "/" +
                       SUBSTR(ob-etiqueta.localizacao,4,3).

    OUTPUT TO VALUE(c-prog-epl) APPEND. 
       PUT UNFORMATTED 
           "A145,132,0,4,1,2,N," '"' TRIM(nota-fiscal.nr-pedcli) '"' SKIP
           "A365,132,0,4,1,2,N," '"' TRIM(b-nota-fiscal.nr-nota-fis) '"' SKIP
           "A540,142,0,1,1,2,N," '"' TRIM(c-emissao) '"' SKIP 
           "A125,188,0,1,1,2,N," '"' TRIM(emitente.nome-emit) '"' SKIP
           "A490,138,0,1,1,3,N," '"' TRIM(c-origem) '"' SKIP
           "A125,220,0,1,1,2,N," '"' TRIM(transporte.nome) '"' SKIP 
           "A647,219,0,1,1,2,N," '"' TRIM(c-localiz) '"' SKIP
           "A30,276,0,1,1,3,N," '"' TRIM(c-desc-item) '"' SKIP
           "A660,276,0,3,1,2,N," '"' STRING(ob-etiqueta.quantidade,">>9.99") '"' SKIP
           "A30,370,0,3,2,4,N," '"' TRIM(c-seq) '"' SKIP 
           "A330,360,0,4,3,3,N," '"' STRING(ob-etiqueta.num-etiqueta,"999999999") '"' SKIP
           "LE305,328,480,130" SKIP.

       PUT UNFORMATTED
           "P1" SKIP.
    OUTPUT CLOSE.

     FIND imprsor_usuar WHERE 
         imprsor_usuar.nom_impressora = "rabbit" AND 
         imprsor_usuar.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
    IF AVAIL imprsor_usuar THEN DO:
       ASSIGN c-comando = "copy /Y /b " + c-prog-epl + " " + imprsor_usuar.nom_disposit_so. 
       
        OS-COMMAND SILENT VALUE(c-comando).
    END.
    
    FIND CURRENT ped-item-rom SHARE-LOCK NO-ERROR.

    IF ped-item-rom.marca = '' THEN
       ASSIGN ob-etiqueta.situacao = 5
              ped-item-rom.marca = 'IMP' + " " + STRING(i-etq-imp,"999").

    ASSIGN fi-volumes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-etq-imp,"999") + STRING(i-tot-etq,"999").

    FIND CURRENT ped-item-rom NO-LOCK NO-ERROR.
    FIND CURRENT ped-item-rom NO-LOCK NO-ERROR.
    FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importa C-Win 
PROCEDURE pi-importa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT FROM "c:\temp\form.dat".
        REPEAT.
            CREATE tt-arquivo.
            IMPORT DELIMITER ";" tt-arquivo.
        END.
    INPUT CLOSE.
    
    FOR EACH tt-arquivo.
        IF tt-arquivo.num-etiqueta = "" THEN NEXT.
        ASSIGN fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(tt-arquivo.num-etiqueta,7,9).
        APPLY "CHOOSE" TO bt-imprime.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida C-Win 
PROCEDURE pi-valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR c-ok AS CHAR.
 DEF VAR c-usuarios AS CHAR.

 FOR EACH usuar_grp_usuar WHERE 
          usuar_grp_usuar.cod_grp_usuar = "ex1" NO-LOCK.
     ASSIGN c-usuarios = c-usuarios + usuar_grp_usuar.cod_usuar + ','.
 END.

 RUN btb/btb910zc.p (INPUT c-usuarios, INPUT YES, INPUT YES, OUTPUT c-ok).
 IF c-ok = ? THEN 
    RETURN 'NOK'. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-volumes C-Win 
PROCEDURE pi-volumes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER p-nr-pedido AS INT.

  ASSIGN i-tot-etq = 0
         i-etq-imp = 0.

  FIND ped-venda-ext WHERE
       ped-venda-ext.nr-pedido = p-nr-pedido
       NO-LOCK NO-ERROR.
  ASSIGN i-tot-etq = 0.
  IF AVAIL ped-venda-ext THEN
     ASSIGN i-tot-etq = ped-venda-ext.qt-fardos.

  /*
  FOR EACH ped-item-res WHERE
           ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
           ped-item-res.serie       = nota-fiscal.serie AND
           ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
           ped-item-res.faturado    = YES NO-LOCK
           BREAK BY STRING(ped-item-res.volume-ini) + 
                    STRING(ped-item-res.volume-fim).

       FOR EACH ped-item-rom WHERE
                ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
                ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia 
                NO-LOCK.

           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                NO-LOCK NO-ERROR.
           IF AVAIL ob-etiqueta AND
              NOT ob-etiqueta.nr-lote BEGINS 'PP' THEN
              ASSIGN i-tot-etq = i-tot-etq + 1.

           IF SUBSTR(ped-item-rom.marca,1,3) = 'IMP' THEN
              ASSIGN i-etq-imp = i-etq-imp + 1.
       END.
  END.
  */
  ASSIGN fi-volumes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-etq-imp,"999") + STRING(i-tot-etq,"999").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

