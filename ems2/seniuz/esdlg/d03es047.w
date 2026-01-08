&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR i-ct AS INT.
DEF VAR i-lote AS INT.
DEF VAR c-acondic AS CHAR.
DEF VAR c-tipo-def AS CHAR.
DEF VAR c-defeitos AS CHAR.
DEF VAR c-revisadeira AS CHAR.
DEF VAR de-qt-prod AS INT FORMAT ">>>9".

def var h-acomp as handle no-undo.

DEF VAR c-desc-item  AS CHAR FORMAT "x(36)".
DEF VAR c-composicao LIKE composi.descricao EXTENT 3.
DEF VAR c-dv         LIKE ref-item-ext.dv.   

DEF VAR v-defeito    AS CHAR EXTENT 3.

DEF VAR c-cores AS CHAR INIT "Vermelha,12,Amarela,14,Azul,9,Verde,2,Preta,0,Laranja,16".

DEF VAR l-existe-ld AS LOG.
DEF VAR l-defeitos AS LOG.
DEF VAR l-perfeito AS LOG.

DEF TEMP-TABLE tt-etiqueta LIKE ob-etiqueta.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ordem-benefic

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH ordem-benefic SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH ordem-benefic SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog ordem-benefic
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog ordem-benefic


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-nr-ob fi-nr-seq fi-nuance fi-quantidade ~
fi-tipo-def1 fi-cod-def1 fi-tipo-def2 fi-cod-def2 fi-tipo-def3 fi-cod-def3 ~
bt-ok bt-cancela bt-ajuda RECT-2 RECT-3 rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-ob fi-nr-seq fi-it-codigo ~
fi-cod-refer cb-lote fi-qualidade fi-nuance fi-qt-cortes fi-quantidade ~
fi-tipo-def1 fi-cod-def1 fi-tipo-def2 fi-cod-def2 fi-tipo-def3 fi-cod-def3 ~
fi-desc-item 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-it-codigo fi-cod-refer cb-lote fi-qualidade ~
fi-qt-cortes 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-area D-Dialog 
FUNCTION fn-area RETURNS CHARACTER
  (input pos-X as integer, input pos-Y as integer, input comprimento as INTEGER, INPUT area AS INTEGER,INPUT intensidade AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-code25 D-Dialog 
FUNCTION fn-code25 RETURNS CHARACTER
   (INPUT posicaoX AS INTEGER, input posicaoY AS INTEGER, INPUT texto AS CHARACTER, INPUT sentido AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-ean13 D-Dialog 
FUNCTION fn-ean13 RETURNS CHARACTER
  ( INPUT posicaoX AS INTEGER, INPUT posicaoY AS INTEGER, INPUT num-ean13 AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-grava-macro D-Dialog 
FUNCTION fn-grava-macro RETURNS CHARACTER
  ( INPUT arq-image AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-imp-macro D-Dialog 
FUNCTION fn-imp-macro RETURNS CHARACTER
  ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y AS INTEGER,INPUT macroid AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-linha D-Dialog 
FUNCTION fn-linha RETURNS CHARACTER
  ( INPUT posicaoX AS INTEGER, INPUT posicaoY AS INTEGER, INPUT tamanho AS INTEGER, INPUT espessura AS INTEGER, INPUT orientacao AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-retangulo D-Dialog 
FUNCTION fn-retangulo RETURNS CHARACTER
  ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y AS INTEGER, INPUT pos-f-X AS INTEGER, INPUT pos-f-Y AS INTEGER, INPUT espessura AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-texto D-Dialog 
FUNCTION fn-texto RETURNS CHARACTER
  ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y as integer, INPUT texto AS CHARACTER,INPUT fonte AS INTEGER, INPUT tamanho AS INTEGER, INPUT negrito AS INTEGER, INPUT italico AS INTEGER, INPUT fixo AS INTEGER, INPUT sentido AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 14 BY 1.75
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Sair" 
     SIZE 9.57 BY 1.75
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok 
     LABEL "&Etiqueta" 
     SIZE 17 BY 1.75
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE cb-lote AS CHARACTER FORMAT "X(256)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","1-RP","2-PP","3-RD","4-PD" 
     DROP-DOWN-LIST
     SIZE 14 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-cod-def1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-cod-def2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-cod-def3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-nr-ob AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-nr-seq AS INTEGER FORMAT "999":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-nuance AS CHARACTER FORMAT "X":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qt-cortes AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qualidade AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-quantidade AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-tipo-def1 AS CHARACTER FORMAT "X":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-tipo-def2 AS CHARACTER FORMAT "X":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-tipo-def3 AS CHARACTER FORMAT "X":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 122 BY 16.79.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 27.29 BY 8.13
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 122 BY 2.25
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      ordem-benefic SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-nr-ob AT ROW 1.5 COL 38 COLON-ALIGNED NO-LABEL
     fi-nr-seq AT ROW 3.25 COL 38 COLON-ALIGNED NO-LABEL
     fi-it-codigo AT ROW 5 COL 38 COLON-ALIGNED NO-LABEL
     fi-cod-refer AT ROW 6.75 COL 38 COLON-ALIGNED NO-LABEL
     cb-lote AT ROW 8.5 COL 38 COLON-ALIGNED NO-LABEL
     fi-qualidade AT ROW 10.42 COL 38 COLON-ALIGNED NO-LABEL
     fi-nuance AT ROW 12.25 COL 38 COLON-ALIGNED NO-LABEL
     fi-qt-cortes AT ROW 14 COL 38 COLON-ALIGNED NO-LABEL
     fi-quantidade AT ROW 15.75 COL 38 COLON-ALIGNED NO-LABEL
     fi-tipo-def1 AT ROW 9.5 COL 80 COLON-ALIGNED NO-LABEL
     fi-cod-def1 AT ROW 9.5 COL 90 COLON-ALIGNED NO-LABEL
     fi-tipo-def2 AT ROW 11.25 COL 80 COLON-ALIGNED NO-LABEL
     fi-cod-def2 AT ROW 11.25 COL 90 COLON-ALIGNED NO-LABEL
     fi-tipo-def3 AT ROW 13 COL 80 COLON-ALIGNED NO-LABEL
     fi-cod-def3 AT ROW 13 COL 90 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 18.54 COL 2.43
     bt-cancela AT ROW 18.54 COL 20.43
     bt-ajuda AT ROW 18.54 COL 107.86
     fi-desc-item AT ROW 5 COL 55 COLON-ALIGNED NO-LABEL
     RECT-2 AT ROW 1.21 COL 2
     RECT-3 AT ROW 7.38 COL 77.72
     rt-buttom AT ROW 18.25 COL 1
     "Nuance:" VIEW-AS TEXT
          SIZE 16 BY 1.38 AT ROW 12.25 COL 38 RIGHT-ALIGNED
          FONT 10
     "Cortes:" VIEW-AS TEXT
          SIZE 14 BY 1.38 AT ROW 14.04 COL 38 RIGHT-ALIGNED
          FONT 10
     "N£mero da OB:" VIEW-AS TEXT
          SIZE 29 BY 1.5 AT ROW 1.5 COL 38 RIGHT-ALIGNED
          FONT 10
     "Lote:" VIEW-AS TEXT
          SIZE 10 BY 1.38 AT ROW 8.5 COL 39 RIGHT-ALIGNED
          FONT 10
     "Quantidade:" VIEW-AS TEXT
          SIZE 23 BY 1.38 AT ROW 15.92 COL 38 RIGHT-ALIGNED
          FONT 10
     "Item:" VIEW-AS TEXT
          SIZE 10 BY 1.38 AT ROW 5 COL 38.43 RIGHT-ALIGNED
          FONT 10
     "Referància:" VIEW-AS TEXT
          SIZE 22 BY 1.38 AT ROW 6.75 COL 38.72 RIGHT-ALIGNED
          FONT 10
     " Defeitos" VIEW-AS TEXT
          SIZE 17 BY 1.25 AT ROW 6.96 COL 83
          BGCOLOR 8 FONT 10
     "CODIGO" VIEW-AS TEXT
          SIZE 8.72 BY .54 AT ROW 8.75 COL 92.14
          BGCOLOR 8 FONT 9
     "TIPO" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 8.75 COL 83
          BGCOLOR 8 FONT 9
     "       <F12>-Imprimir Etiqueta                <F2>-Sair" VIEW-AS TEXT
          SIZE 76 BY 1.71 AT ROW 18.54 COL 30.86
          FONT 0
     "Sequància:" VIEW-AS TEXT
          SIZE 21 BY 1.38 AT ROW 3.25 COL 38 RIGHT-ALIGNED
          FONT 10
     "Qualidade:" VIEW-AS TEXT
          SIZE 20.57 BY 1.38 AT ROW 10.29 COL 38.57 RIGHT-ALIGNED
          FONT 10
     SPACE(84.99) SKIP(9.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Produá∆o de Acabados"
         DEFAULT-BUTTON bt-ok CANCEL-BUTTON bt-cancela.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   Custom                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX cb-lote IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-qt-cortes IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi-qualidade IN FRAME D-Dialog
   NO-ENABLE 1                                                          */
/* SETTINGS FOR TEXT-LITERAL "N£mero da OB:"
          SIZE 29 BY 1.5 AT ROW 1.5 COL 38 RIGHT-ALIGNED                */

/* SETTINGS FOR TEXT-LITERAL "Sequància:"
          SIZE 21 BY 1.38 AT ROW 3.25 COL 38 RIGHT-ALIGNED              */

/* SETTINGS FOR TEXT-LITERAL "Item:"
          SIZE 10 BY 1.38 AT ROW 5 COL 38.43 RIGHT-ALIGNED              */

/* SETTINGS FOR TEXT-LITERAL "Referància:"
          SIZE 22 BY 1.38 AT ROW 6.75 COL 38.72 RIGHT-ALIGNED           */

/* SETTINGS FOR TEXT-LITERAL "Lote:"
          SIZE 10 BY 1.38 AT ROW 8.5 COL 39 RIGHT-ALIGNED               */

/* SETTINGS FOR TEXT-LITERAL "Qualidade:"
          SIZE 20.57 BY 1.38 AT ROW 10.29 COL 38.57 RIGHT-ALIGNED       */

/* SETTINGS FOR TEXT-LITERAL "Nuance:"
          SIZE 16 BY 1.38 AT ROW 12.25 COL 38 RIGHT-ALIGNED             */

/* SETTINGS FOR TEXT-LITERAL "Cortes:"
          SIZE 14 BY 1.38 AT ROW 14.04 COL 38 RIGHT-ALIGNED             */

/* SETTINGS FOR TEXT-LITERAL "Quantidade:"
          SIZE 23 BY 1.38 AT ROW 15.92 COL 38 RIGHT-ALIGNED             */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "espec.ordem-benefic"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Produá∆o de Acabados */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Sair */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* Etiqueta */
DO:
      SESSION:SET-WAIT-STATE("general":U).
      
      IF INPUT FRAME {&FRAME-NAME} fi-quantidade = 0 THEN DO.
         MESSAGE "Quantidade deve ser Informada..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO fi-quantidade.
         RETURN NO-APPLY.
      END.
    
      IF INPUT FRAME {&FRAME-NAME} cb-lote = '' THEN DO.
         MESSAGE "Lote deve ser Informado..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cb-lote.
         RETURN NO-APPLY.
      END.
    
      IF SUBSTR(INPUT FRAME {&frame-name} fi-cod-refer,1,6) = '010619' AND
         INPUT FRAME {&FRAME-NAME} fi-nuance = '' THEN DO.
         MESSAGE "Nuance deve ser Informada..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO fi-nuance.
         RETURN NO-APPLY.
      END.

      FOR EACH tt-etiqueta.
          DELETE tt-etiqueta.
      END.
    
      CREATE tt-etiqueta.
      ASSIGN tt-etiqueta.nr-ob = INPUT FRAME {&FRAME-NAME} fi-nr-ob
             tt-etiqueta.nr-seq = INPUT FRAME {&FRAME-NAME} fi-nr-seq
             tt-etiqueta.it-codigo = INPUT FRAME {&FRAME-NAME} fi-it-codigo
             tt-etiqueta.cod-refer = INPUT FRAME {&FRAME-NAME} fi-cod-refer
             tt-etiqueta.nr-cortes = INPUT FRAME {&FRAME-NAME} fi-qt-cortes
             tt-etiqueta.nuance = INPUT FRAME {&FRAME-NAME} fi-nuance
             tt-etiqueta.qualidade = INPUT FRAME {&FRAME-NAME} fi-qualidade
             tt-etiqueta.nr-lote = ENTRY(2,INPUT FRAME {&FRAME-NAME} cb-lote,"-")
             tt-etiqueta.quantidade = INPUT FRAME {&FRAME-NAME} fi-quantidade.

      ASSIGN v-defeito[1] = INPUT FRAME {&FRAME-NAME} fi-tipo-def1 + "   " + INPUT FRAME {&FRAME-NAME} fi-cod-def1.
      ASSIGN v-defeito[2] = INPUT FRAME {&FRAME-NAME} fi-tipo-def2 + "   " + INPUT FRAME {&FRAME-NAME} fi-cod-def2.
      ASSIGN v-defeito[3] = INPUT FRAME {&FRAME-NAME} fi-tipo-def3 + "   " + INPUT FRAME {&FRAME-NAME} fi-cod-def3.
    
      FIND ob-etiqueta WHERE
           ob-etiqueta.nr-ob = tt-etiqueta.nr-ob AND
           ob-etiqueta.nr-sequencia = tt-etiqueta.nr-sequencia NO-ERROR.

      IF NOT AVAIL ob-etiqueta THEN DO.
         MESSAGE "Etiqueta n∆o Cadastrada no Sistema... " SKIP(1)
                 "Deseja Cadastra-la ?"
                 VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                 TITLE "Etiqueta n∆o Encontrada" UPDATE l-opc AS LOGICAL.
         IF l-opc = NO THEN RETURN NO-APPLY.

         CREATE ob-etiqueta.
         BUFFER-COPY tt-etiqueta TO ob-etiqueta
              ASSIGN ob-etiqueta.dt-ob = TODAY
                     ob-etiqueta.resp-revisao = c-seg-usuario
                     ob-etiqueta.dt-emissao = TODAY
                     ob-etiqueta.hr-emissao = STRING(TIME,"HH:MM")
                     ob-etiqueta.situacao = 3.
      END.
      ELSE DO.
         FIND ordem-benefic OF ob-etiqueta NO-LOCK NO-ERROR.
         IF ordem-benefic.it-codigo <> tt-etiqueta.it-codigo OR
            ordem-benefic.cod-refer <> tt-etiqueta.cod-refer THEN DO.
            MESSAGE "Atená∆o, Vocà est† trocando o Item da Etiqueta Original... " SKIP(1)
                    "Tem certeza que deseja fazer isso ?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "  A T E N Ä « O ! ! !"  UPDATE l-opc2 AS LOGICAL.
            IF l-opc2 = NO THEN RETURN NO-APPLY.
         END.
      END.
      ASSIGN ob-etiqueta.quantidade = tt-etiqueta.quantidade.

      RUN pi-etiqueta.  
    
      SESSION:SET-WAIT-STATE("":U).

      APPLY 'entry' TO fi-nr-ob.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-lote D-Dialog
ON LEAVE OF cb-lote IN FRAME D-Dialog
DO:
   IF SUBSTR(INPUT FRAME {&FRAME-NAME} cb-lote,3,1) = "P" THEN DO.
      APPLY 'entry' TO fi-quantidade.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo D-Dialog
ON LEAVE OF fi-it-codigo IN FRAME D-Dialog
DO:
    FIND ITEM WHERE
         ITEM.it-codigo = self:SCREEN-VALUE  NO-LOCK NO-ERROR.

    ASSIGN fi-desc-item:SCREEN-VALUE = ITEM.descricao-1 + TRIM(ITEM.descricao-2).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-seq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-seq D-Dialog
ON LEAVE OF fi-nr-seq IN FRAME D-Dialog
DO:
   FIND ob-etiqueta WHERE
        ob-etiqueta.nr-ob = INPUT FRAME {&FRAME-NAME} fi-nr-ob AND
        ob-etiqueta.nr-sequencia = INPUT FRAME {&FRAME-NAME} fi-nr-seq
        NO-ERROR.

   DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
   ASSIGN fi-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
          fi-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
          fi-qt-cortes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
          fi-nuance:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
          fi-qualidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
          cb-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
          fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '
          fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '.

   IF AVAIL ob-etiqueta THEN DO.
      ASSIGN fi-it-codigo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.it-codigo
             fi-cod-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.cod-refer
             fi-qt-cortes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.nr-cortes) 
             fi-nuance:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.nuance
             fi-qualidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.qualidade) 
             cb-lote:SCREEN-VALUE IN FRAME {&FRAME-NAME} = IF ob-etiqueta.nr-lote = 'RP'
                                                           THEN '1-RP'
                                                           ELSE IF ob-etiqueta.nr-lote = 'PP'
                                                                THEN '2-PP'
                                                                ELSE IF ob-etiqueta.nr-lote = 'RD' 
                                                                     THEN '3-RD' ELSE '4-PD'
             fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.quantidade).

      FIND ITEM WHERE
           ITEM.it-codigo = ob-etiqueta.it-codigo  NO-LOCK NO-ERROR.

      ASSIGN fi-desc-item:SCREEN-VALUE = ITEM.descricao-1 + TRIM(ITEM.descricao-2).
   END.
   ELSE DO.
      ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
      APPLY 'entry' TO fi-it-codigo IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nuance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nuance D-Dialog
ON VALUE-CHANGED OF fi-nuance IN FRAME D-Dialog
DO:
  ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qualidade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qualidade D-Dialog
ON LEAVE OF fi-qualidade IN FRAME D-Dialog
DO:
   IF SUBSTR(INPUT FRAME {&frame-name} fi-cod-refer,1,6) = '010619' THEN DO.
      APPLY 'entry' TO fi-nuance.
      RETURN NO-APPLY.
   END.
   ELSE DO.
      APPLY 'entry' TO fi-qt-cortes.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

ON 'F11':U ANYWHERE DO:
   APPLY 'choose' TO bt-cancela IN FRAME {&FRAME-NAME}.
   RETURN NO-APPLY.
END.

ON 'F12':U ANYWHERE DO:
   APPLY 'choose' TO bt-ok IN FRAME {&FRAME-NAME}.
END.

ON 'return':U ANYWHERE DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
END.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fi-nr-ob fi-nr-seq fi-it-codigo fi-cod-refer cb-lote fi-qualidade 
          fi-nuance fi-qt-cortes fi-quantidade fi-tipo-def1 fi-cod-def1 
          fi-tipo-def2 fi-cod-def2 fi-tipo-def3 fi-cod-def3 fi-desc-item 
      WITH FRAME D-Dialog.
  ENABLE fi-nr-ob fi-nr-seq fi-nuance fi-quantidade fi-tipo-def1 fi-cod-def1 
         fi-tipo-def2 fi-cod-def2 fi-tipo-def3 fi-cod-def3 bt-ok bt-cancela 
         bt-ajuda RECT-2 RECT-3 rt-buttom 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    {utp/ut9000.i "D01ES047" "2.04.00.001"}

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Carga_de_Imagens_na_Impressora *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem Logotipo Tear"). 
    fn-grava-macro("n:\especificos\image\logo-etq10.prn"). 

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 11"). 
    fn-grava-macro("n:\especificos\image\rlgp11.prn").

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 12"). 
    fn-grava-macro("n:\especificos\image\rlgp12.prn"). 

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 13"). 
    fn-grava-macro("n:\especificos\image\rlgp13.prn").

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 14"). 
    fn-grava-macro("n:\especificos\image\rlgp14.prn"). 

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 15"). 
    fn-grava-macro("n:\especificos\image\rlgp15.prn"). 

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 16"). 
    fn-grava-macro("n:\especificos\image\rlgp16.prn"). 

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 17"). 
    fn-grava-macro("n:\especificos\image\rlgp17.prn"). 

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 18"). 
    fn-grava-macro("n:\especificos\image\rlgp18.prn"). 

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 19"). 
    fn-grava-macro("n:\especificos\image\rlgp19.prn"). 

    RUN pi-acompanhar IN h-acomp (INPUT "Carregando Imagem RLGP 20"). 
    fn-grava-macro("n:\especificos\image\rlgp20.prn"). 

    RUN pi-finalizar in h-acomp.

    APPLY 'entry' TO fi-nr-ob IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-etiqueta D-Dialog 
PROCEDURE pi-etiqueta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     OUTPUT TO PRINTER. 
    
        PUT UNFORMATTED 
            "~033&l26A"
            "~033&l2E".
    
        FIND ITEM WHERE
             ITEM.it-codigo = tt-etiqueta.it-codigo NO-LOCK NO-ERROR.

        ASSIGN c-desc-item = ITEM.descricao-1 + TRIM(ITEM.descricao-2).

        ASSIGN c-dv = "".
        IF ITEM.tipo-con-est = 4 THEN DO.
           FIND referencia WHERE
                referencia.cod-refer = tt-etiqueta.cod-refer NO-LOCK NO-ERROR.
        
           IF NOT AVAIL referencia THEN DO.
              MESSAGE "Referencia " tt-etiqueta.cod-refer " n∆o Cadastrada..." VIEW-AS ALERT-BOX.
              RETURN "ADM-ERROR":U.
           END.

           FIND FIRST ref-item-ext WHERE 
                      ref-item-ext.it-codigo = tt-etiqueta.it-codigo AND 
                      ref-item-ext.cod-refer = tt-etiqueta.cod-refer NO-LOCK NO-ERROR.

           IF NOT AVAIL ref-item-ext THEN DO.
              MESSAGE "Extens∆o da Referencia n∆o Cadastrada..." VIEW-AS ALERT-BOX.
              RETURN "ADM-ERROR":U.
           END.

           ASSIGN c-desc-item = c-desc-item + " " + referencia.descricao
                         c-dv = ref-item-ext.dv.
        END.
    
        FIND FIRST item-ext WHERE
                   item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    
        IF AVAIL item-ext THEN DO.
           FIND FIRST composi WHERE
                      composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.
        
           ASSIGN c-composicao = "".
           IF AVAIL composi THEN DO.
              DO i-ct = 1 TO NUM-ENTRIES(composi.descricao).
                 ASSIGN c-composicao[INT(i-ct / 2)] = c-composicao[INT(i-ct / 2)] + ENTRY(i-ct,composi.descricao).
              END.
           END.
        END.

        CASE tt-etiqueta.nr-lote.
            WHEN "RP" THEN ASSIGN i-lote = 1.
            WHEN "PP" THEN ASSIGN i-lote = 2.
            WHEN "RD" THEN ASSIGN i-lote = 3.
            WHEN "PD" THEN ASSIGN i-lote = 4.
            OTHERWISE DO.
                MESSAGE "Lote Invalido..." VIEW-AS ALERT-BOX.
                RETURN "ADM-ERROR":U.
            END.
        END CASE.

        RUN pi-imp-etiqueta (INPUT 50).
    OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-etiqueta D-Dialog 
PROCEDURE pi-imp-etiqueta :
DEFINE INPUT PARAMETER i-col AS INT.

  PUT UNFORMATTED 
      fn-retangulo(input i-col + 10, input 0, input i-col + 875, INPUT 430, INPUT 6). 

  PUT UNFORMATTED 
      fn-texto(INPUT i-col + 50, INPUT 40, INPUT "PRODUTO",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 25, INPUT 100, INPUT tt-etiqueta.it-codigo,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 210, input 0, INPUT 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 225, INPUT 40, INPUT "ACB/DES/COR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 220, INPUT 100, INPUT SUBSTR(tt-etiqueta.cod-refer,1,2),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 295, INPUT 100, INPUT SUBSTR(tt-etiqueta.cod-refer,3,4),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 420, input 0, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 473, INPUT 40, INPUT "VAR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 480, INPUT 100, INPUT SUBSTR(tt-etiqueta.cod-refer,7,1),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 570, input 0, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 605, INPUT 40, INPUT "DV",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 605, INPUT 100, INPUT c-dv,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 670, input 0, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 725, INPUT 40, INPUT "NUANCE",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 750, INPUT 100, INPUT tt-etiqueta.nuance,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 10, input 105, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      fn-texto(INPUT i-col + 35, INPUT 150, INPUT c-desc-item, INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 10, input 160, input 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      fn-texto(INPUT i-col + 40, INPUT 200, INPUT "Seq",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 50, INPUT 255, INPUT STRING(tt-etiqueta.nr-sequencia,"999"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 130, INPUT 160, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 145, INPUT 200, INPUT "Partida",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 170, INPUT 255, INPUT STRING(tt-etiqueta.nr-ob),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 275, INPUT 160, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 295, INPUT 200, INPUT "Largura",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      IF AVAIL item-ext
         THEN fn-texto(INPUT i-col + 330, INPUT 255, INPUT STRING(item-ext.largura,"9.99"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
         ELSE fn-texto(INPUT i-col + 330, INPUT 255, INPUT " ",INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 435, input 160, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 460, INPUT 200, INPUT "Metros",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 455, INPUT 255, INPUT STRING(tt-etiqueta.quantidade,">>>9.99"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 585, input 160, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 605, INPUT 200, INPUT "Cortes",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 630, INPUT 255, INPUT STRING(tt-etiqueta.nr-cortes),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 720, input 160, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 740, INPUT 200, INPUT "Lote",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 765, INPUT 255, INPUT tt-etiqueta.nr-lote,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 10, input 280, input 870, INPUT 6, INPUT "H").
  
  PUT UNFORMATTED
      fn-code25 (input i-col + 38, input 300,
                 input TRIM(tt-etiqueta.it-codigo) +
                       STRING(INT(tt-etiqueta.cod-refer),"9999999") +
                       STRING(tt-etiqueta.quantidade * 10,"9999") +
                       STRING(tt-etiqueta.nr-ob,"99999") + 
                       STRING(tt-etiqueta.nr-sequencia,"999") + 
                       STRING(i-lote,"9"),
                 INPUT "H").

  PUT UNFORMATTED
      fn-retangulo(input i-col + 10, input 600, input i-col + 875, INPUT 1670, INPUT 6)
      fn-imp-macro(INPUT i-col + 25, INPUT 700, INPUT 10)  
      fn-texto(INPUT i-col + 30, INPUT 665, INPUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA",INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 250, INPUT 700, INPUT "Av. General David Sarnoff, 5005 - D",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 250, INPUT 735, INPUT "32210-110 - CONTAGEM - MG - BRASIL",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 250, INPUT 770, INPUT "CNPJ  03.123.987/0002-00",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 250, INPUT 805, INPUT "Insc. Est.  186020807.0187",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 180, input 810, input 700, INPUT 6, INPUT "H").
  
  PUT UNFORMATTED
      fn-linha(input i-col + 180, input 810, input 865, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 220, INPUT 850, INPUT "PRODUTO",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 195, INPUT 910, INPUT SUBSTR(tt-etiqueta.it-codigo,1,6),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 385, input 810, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 420, INPUT 850, INPUT "ACB/DES/COR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 405, INPUT 910, INPUT SUBSTR(tt-etiqueta.cod-refer,1,2),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 485, INPUT 910, INPUT SUBSTR(tt-etiqueta.cod-refer,3,4),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 620, input 810, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 673, INPUT 850, INPUT "VAR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 680, INPUT 910, INPUT SUBSTR(tt-etiqueta.cod-refer,7,1),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 770, input 810, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 805, INPUT 850, INPUT "DV",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 805, INPUT 910, INPUT c-dv,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 180, input 915, input 700, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      fn-texto(INPUT i-col + 200, INPUT 960, INPUT c-desc-item, INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 180, input 970, input 700, INPUT 6, INPUT "H")
      fn-texto(INPUT i-col + 193, INPUT 1010, INPUT "Seq",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 205, INPUT 1065, INPUT STRING(tt-etiqueta.nr-sequencia,"999"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 259, input 970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 272, INPUT 1010, INPUT "Partida",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 275, INPUT 1065, INPUT STRING(tt-etiqueta.nr-ob),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 390, input 970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 403, INPUT 1010, INPUT "Largura",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      IF AVAIL item-ext
         THEN fn-texto(INPUT i-col + 440, INPUT 1065, INPUT STRING(item-ext.largura,"9.99"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
         ELSE fn-texto(INPUT i-col + 440, INPUT 1065, INPUT " ",INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 536, input 970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 549, INPUT 1010, INPUT "Metros",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 550, INPUT 1065, INPUT STRING(tt-etiqueta.quantidade,">>>9.9"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 665, input 970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 678, INPUT 1010, INPUT "Cortes",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 720, INPUT 1065, INPUT string(tt-etiqueta.nr-cortes),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 785, input 970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 798, INPUT 1010, INPUT "Lote",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 817, INPUT 1065, INPUT tt-etiqueta.nr-lote,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 180, input 1090, input 700, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      fn-texto(INPUT i-col + 250, INPUT 1130, INPUT "Em caso de Reclamacao, favor",INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 340, INPUT 1170, INPUT "devolver esta etiqueta",INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 180, input 1180, input 700, INPUT 6, INPUT "H")
      fn-linha(input i-col + 600, input 1180, input 300, INPUT 6, INPUT "V").

  PUT UNFORMATTED
      fn-texto(INPUT i-col + 200, INPUT 1215, INPUT "Composicao",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 200, INPUT 1245, INPUT c-composicao[1],INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 200, INPUT 1275, INPUT c-composicao[2],INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 200, INPUT 1305, INPUT c-composicao[3],INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 680, INPUT 1215, INPUT "Nuance",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      IF tt-etiqueta.nuance <> "W"
         THEN fn-texto(INPUT i-col + 630, INPUT 1460, INPUT tt-etiqueta.nuance,INPUT 16602, INPUT 75, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
         ELSE fn-texto(INPUT i-col + 620, INPUT 1460, INPUT tt-etiqueta.nuance,INPUT 16602, INPUT 65, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 180, input 1315, input 420, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      fn-code25 (input i-col + 38, input 1630,
                 input TRIM(tt-etiqueta.it-codigo) +
                       STRING(INT(tt-etiqueta.cod-refer),"9999999") +
                       STRING(tt-etiqueta.quantidade * 10,"9999") +
                       STRING(tt-etiqueta.nr-ob,"99999") + 
                       STRING(tt-etiqueta.nr-sequencia,"999") + 
                       STRING(i-lote,"9"),
                 INPUT "V")
      IF AVAIL ref-item-ext
         THEN fn-ean13(INPUT i-col + 250, INPUT 1340, 
                       INPUT STRING(ref-item-ext.cod-ean,"999999999999"))
         ELSE fn-ean13(INPUT i-col + 250,  INPUT 1340, 
                       INPUT "000000000000")
      fn-linha(INPUT i-col + 180, input 1480, input 700, INPUT 6, INPUT "H")
      fn-linha(INPUT i-col + 730, input 1480, input 190, INPUT 6, INPUT "V"). 

  IF AVAIL item-ext THEN DO. /*Escolhendo a imagem para jogar na etiqueta*/
     CASE item-ext.cod-rlgp:
        WHEN 1 THEN
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col + 190, INPUT 1540, INPUT 11) 
                fn-imp-macro(INPUT i-col + 295, INPUT 1540, INPUT 12)
                fn-imp-macro(INPUT i-col + 400, INPUT 1540, INPUT 13)
                fn-imp-macro(INPUT i-col + 505, INPUT 1560, INPUT 14)
                fn-imp-macro(INPUT i-col + 610, INPUT 1530, INPUT 15).
        WHEN 2 THEN
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col + 190, INPUT 1540, INPUT 11) 
                fn-imp-macro(INPUT i-col + 295, INPUT 1540, INPUT 12)
                fn-imp-macro(INPUT i-col + 400, INPUT 1540, INPUT 16)
                fn-imp-macro(INPUT i-col + 505, INPUT 1560, INPUT 14)
                fn-imp-macro(INPUT i-col + 610, INPUT 1530, INPUT 15).
        WHEN 3 THEN 
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col + 190, INPUT 1540, INPUT 17) 
                fn-imp-macro(INPUT i-col + 295, INPUT 1540, INPUT 12)
                fn-imp-macro(INPUT i-col + 400, INPUT 1540, INPUT 16)
                fn-imp-macro(INPUT i-col + 505, INPUT 1560, INPUT 14)
                fn-imp-macro(INPUT i-col + 610, INPUT 1530, INPUT 15).
        WHEN 4 THEN 
            PUT UNFORMATTED
                fn-imp-macro(INPUT i-col + 190, INPUT 1540, INPUT 18) 
                fn-imp-macro(INPUT i-col + 290, INPUT 1540, INPUT 12)
                fn-imp-macro(INPUT i-col + 400, INPUT 1540, INPUT 19)
                fn-imp-macro(INPUT i-col + 505, INPUT 1560, INPUT 20)
                fn-imp-macro(INPUT i-col + 610, INPUT 1530, INPUT 15).
     END CASE.
  END.

  PUT UNFORMATTED
      fn-texto(INPUT i-col + 765, INPUT 1550, INPUT v-defeito[1], INPUT 16602, INPUT 8, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 765, INPUT 1590, INPUT v-defeito[2], INPUT 16602, INPUT 8, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 765, INPUT 1630, INPUT v-defeito[3], INPUT 16602, INPUT 8, INPUT 0, INPUT 0, INPUT 1, INPUT "H").

  IF SUBSTR(INPUT FRAME {&FRAME-NAME} cb-lote,4,1) = "D" THEN 
     PUT UNFORMATTED
         fn-area(INPUT i-col + 600, INPUT 1180, INPUT 280, INPUT 300, INPUT 80).

  PUT UNFORMATTED
      fn-retangulo(input i-col + 10, input 1810, input i-col + 875, INPUT 2240, INPUT 6)
      fn-texto(INPUT i-col + 50, INPUT 1850, INPUT "PRODUTO",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 25, INPUT 1910, INPUT SUBSTR(tt-etiqueta.it-codigo,1,6),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(INPUT i-col + 210, INPUT 1810, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 225, INPUT 1850, INPUT "ACB   DES/COR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 220, INPUT 1910, INPUT SUBSTR(tt-etiqueta.cod-refer,1,2),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 295, INPUT 1910, INPUT SUBSTR(tt-etiqueta.cod-refer,3,4),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(INPUT i-col + 420, INPUT 1810, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 473, INPUT 1850, INPUT "VAR",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 480, INPUT 1910, INPUT SUBSTR(tt-etiqueta.cod-refer,7,1),INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(INPUT i-col + 570, INPUT 1810, input 110, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 605, INPUT 1850, INPUT "DV",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 605, INPUT 1910, INPUT c-dv,INPUT 16602, INPUT 13, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 670, input 1810, input 280, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 725, INPUT 1850, INPUT "NUANCE",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      IF tt-etiqueta.nuance <> "W" 
         THEN fn-texto(INPUT i-col + 680, INPUT 2070, INPUT tt-etiqueta.nuance,INPUT 16602, INPUT 65, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
         ELSE fn-texto(INPUT i-col + 680, INPUT 2070, INPUT tt-etiqueta.nuance,INPUT 16602, INPUT 50, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 10, input 1915, input 660, INPUT 6, INPUT "H").

  IF SUBSTR(INPUT FRAME {&FRAME-NAME} cb-lote,4,1) = "D" THEN 
     PUT UNFORMATTED
         fn-area(INPUT i-col + 670, INPUT 1810, INPUT 210, INPUT 280, INPUT 80).

  PUT UNFORMATTED
      fn-texto(INPUT i-col + 35, INPUT 1960, INPUT c-desc-item, INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 10, input 1970, input 660, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      fn-texto(INPUT i-col + 20, INPUT 2010, INPUT "Seq",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 30, INPUT 2065, INPUT STRING(tt-etiqueta.nr-sequencia,"999"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 90, input 1970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 105, INPUT 2010, INPUT "Partida",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 105, INPUT 2065, INPUT STRING(tt-etiqueta.nr-ob),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 225, input 1970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 240, INPUT 2010, INPUT "Larg.",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 250, INPUT 2065, INPUT STRING(item-ext.largura,"9.99"),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 325, input 1970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 340, INPUT 2010, INPUT "Metros",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 345, INPUT 2065, INPUT STRING(tt-etiqueta.quantidade,">>9.9"),INPUT 16602, INPUT 9, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 455, input 1970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 468, INPUT 2010, INPUT "Cortes",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 510, INPUT 2065, INPUT STRING(tt-etiqueta.nr-cortes),INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(input i-col + 583, input 1970, input 120, INPUT 6, INPUT "V")
      fn-texto(INPUT i-col + 595, INPUT 2010, INPUT "Lote",INPUT 16602, INPUT 8, INPUT 3, INPUT 0, INPUT 1, INPUT "H")
      fn-texto(INPUT i-col + 610, INPUT 2065, INPUT tt-etiqueta.nr-lote,INPUT 16602, INPUT 6, INPUT 0, INPUT 0, INPUT 1, INPUT "H")
      fn-linha(INPUT i-col + 10, INPUT 2090, INPUT 870, INPUT 6, INPUT "H").

  PUT UNFORMATTED
      fn-code25 (INPUT i-col + 38, INPUT 2110,
                 INPUT TRIM(tt-etiqueta.it-codigo) +
                       STRING(INT(tt-etiqueta.cod-refer),"9999999") +
                       STRING(tt-etiqueta.quantidade * 10,"9999") +
                       STRING(tt-etiqueta.nr-ob,"99999") + 
                       STRING(tt-etiqueta.nr-sequencia,"999") + 
                       STRING(i-lote,"9"),
                 INPUT "H").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ordem-benefic"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
  
  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-area D-Dialog 
FUNCTION fn-area RETURNS CHARACTER
  (input pos-X as integer, input pos-Y as integer, input comprimento as INTEGER, INPUT area AS INTEGER,INPUT intensidade AS INTEGER):
    DEFINE VARIABLE conteudo AS CHARACTER.
    ASSIGN conteudo = '~033*p' + STRING(pos-X) + 'x' + STRING(pos-Y) + 'Y' +
                      '~033*c' + STRING(comprimento) + 'a' + STRING(area) + 'b' + string(intensidade) + 'G' + 
                      '~033*c2P'. 
    RETURN conteudo.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-code25 D-Dialog 
FUNCTION fn-code25 RETURNS CHARACTER
   (INPUT posicaoX AS INTEGER, input posicaoY AS INTEGER, INPUT texto AS CHARACTER, INPUT sentido AS CHAR):
    define variable heigth    as decimal.
    define variable small-bar as decimal.
    define variable wide-bar  as decimal.
    define variable dpl       as decimal.
    define variable i-ct         as integer.
    define variable j         as integer.

    define variable inicio    as character.
    define variable fim       as character.
    define variable pp        as character.
    define variable pg        as character.
    define variable bp        as character.
    define variable bg        as character.
    define variable chars     as character.
    define variable cbarra    as character extent 10.
    define variable barras    as character.

    DEF VAR num-1 AS CHAR.
    DEF VAR num-2 AS CHAR.

    ASSIGN chars  = '1234567890'
           inicio = '~033*p-50Y'
           fim    = '~033*p+50Y'.

    assign heigth    = 2                          /* Altura das linhas */
           small-bar = 3.5                        /* Numero de pontos da linha */
           wide-bar  = round(small-bar * 2.7, 0)  /* Numero de pontos da barra */
           dpl       = 50.                        /* Pontos por linha 300 dpi/6lpi = 50dpl */

    ASSIGN pp = '~033*c'  +  string(small-bar, "99") + 'a' + string(heigth * dpl) + 'b0p~033*p+' + string(small-bar, "99") + "X"
           pg = '~033*c'  +  string(wide-bar,  "99") + 'a' + string(heigth * dpl) + 'b0p~033*p+' + string(wide-bar,  "99") + "X"
           bp = '~033*p+' +  string(small-bar, "99") + "X"
           bg = '~033*p+' +  string(wide-bar,  "99") + "X".

    ASSIGN cbarra[01] = "GPPPG" /* 1 */ 
           cbarra[02] = "PGPPG" /* 2 */
           cbarra[03] = "GGPPP" /* 3 */
           cbarra[04] = "PPGPG" /* 4 */
           cbarra[05] = "GPGPP" /* 5 */
           cbarra[06] = "PGGPP" /* 6 */
           cbarra[07] = "PPPGG" /* 7 */
           cbarra[08] = "GPPGP" /* 8 */
           cbarra[09] = "PGPGP" /* 9 */
           cbarra[10] = "PPGGP" /* 0 */

    barras = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y'.
    barras = barras + (IF sentido  = "V" THEN "~033&a90P" ELSE "~033&a0P"). 

    barras = barras + pp + bp + pp + bp.   /* Inicio */ 
    DO i-ct = 1 TO LENGTH(texto) BY 2:
       ASSIGN num-1 = SUBSTRING(texto, i-ct,1)
              num-2 = SUBSTRING(texto, i-ct + 1,1).

       DO j = 1 TO 5.
          ASSIGN barras = barras + IF SUBSTR(cbarra[INDEX(chars,num-1)],j,1) = "G" 
                                   THEN pg ELSE pp.

          ASSIGN barras = barras + IF SUBSTR(cbarra[INDEX(chars,num-2)],j,1) = "G" 
                                   THEN bg ELSE bp.
       END.
    END.
    barras = barras + pg + bp + pp.   /* Fim */
    barras = barras + "~033&a0P".

    IF sentido = "H" THEN
       barras = barras + fn-texto(INPUT posicaoX + 260, INPUT posicaoY + 120, INPUT texto,INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT sentido).
    ELSE
       barras = barras + fn-texto(INPUT posicaoX + 130, INPUT posicaoY - 260, INPUT texto,INPUT 16602, INPUT 5, INPUT 3, INPUT 0, INPUT 1, INPUT sentido).

    RETURN barras.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-ean13 D-Dialog 
FUNCTION fn-ean13 RETURNS CHARACTER
  ( INPUT posicaoX AS INTEGER, INPUT posicaoY AS INTEGER, INPUT num-ean13 AS CHARACTER):
    DEFINE VARIABLE b-ean13   AS CHAR.        /* Composiá∆o das Barras */
    DEFINE VARIABLE b0        AS CHARACTER.   /* Barra Branca */
    DEFINE VARIABLE b1        AS CHARACTER.   /* Barra Preta */
    DEFINE VARIABLE bg1       AS CHARACTER.   /* Barra Preta Grande para os delimitadores */
    DEFINE VARIABLE i         AS INTEGER.
    DEFINE VARIABLE j         AS INTEGER.
    DEFINE VARIABLE i-num     AS INTEGER.
    DEFINE VARIABLE barra-c   AS CHAR.
    DEFINE VARIABLE num-c     AS CHAR.
    DEFINE VARIABLE i-ct      AS INT.
    DEFINE VARIABLE tot-e     AS INT.
    DEFINE VARIABLE i-dv      AS INT.   /* Digito Verificador do EAN-13*/
    DEFINE VARIABLE tab-ean13 AS CHAR EXTENT 10 INIT
                   ["1110010","1100110","1101100","1000010","1011100",
                    "1001110","1010000","1000100","1001000","1110100"].

    ASSIGN bg1 = '~033*c2a120b0p' + '~033*p+3X'
            b1 = '~033*c2a100b0p' + '~033*p+3X'
            b0 = '~033*p+3X'.

    /* Delimitador Inicial */
    ASSIGN b-ean13 = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y' +
                      bg1 + b0 + bg1.

    /* Escreve as Barras do 7890413 - Codigo do Brasil (789) + Codigo da Tear  (0413) */
    ASSIGN b-ean13 = b-ean13 + 
                     b0 + b1 + b1 + b0 + b1 + b1 + b1 + b0 + b0 + b1 + b0 + b1 +
                     b1 + b1 + b0 + b0 + b0 + b1 + b1 + b0 + b1 + b0 + b0 + b1 +
                     b1 + b1 + b0 + b1 + b0 + b0 + b1 + b1 + b0 + b0 + b1 + b0 +
                     b1 + b0 + b0 + b0 + b0 + b1.

    /* Delimitardor Central */
    ASSIGN b-ean13 = b-ean13 + b0 + bg1 + b0 + bg1 + b0.

    /* Calcula digito verificador ean 13 */
    DO i-ct = 1 TO 12.
       IF i-ct MODULO 2 = 0 THEN
          ASSIGN tot-e = tot-e + INT(SUBSTR(num-ean13,i-ct,1)) * 3. 
       ELSE
          ASSIGN tot-e = tot-e + INT(SUBSTR(num-ean13,i-ct,1)). 
    END.
    ASSIGN i-dv = IF tot-e MODULO 10 <> 0 
                  THEN 10 - (tot-e MODULO 10)
                  ELSE tot-e MODULO 10
           num-c = SUBSTR(num-ean13,8,6) + STRING(i-dv,"9").

    /* Escreve Barras do resto do c¢digo + digito (Parte C) */

    DO i = 1 TO LENGTH(num-c).
       ASSIGN i-num = INT(SUBSTR(num-c,i,1))
              barra-c = tab-ean13[i-num + 1].

       DO j = 1 TO LENGTH(barra-c).
          ASSIGN b-ean13 = b-ean13 + IF SUBSTR(barra-c,j,1) = '0'
                                       THEN b0 ELSE b1.
       END.
    END.

    /* Delimitador Final */
    ASSIGN b-ean13 = b-ean13 + bg1 + b0 + bg1.

    /* Escreve Texto abaixo das Barras */
    DO i = 1 TO LENGTH(num-c) BY 2.
       ASSIGN num-c = SUBSTR(num-c,1,i) + " " + SUBSTR(num-c,i + 1).
    END.
    ASSIGN b-ean13 = b-ean13 + 
                     fn-texto(INPUT posicaoX - 18, INPUT posicaoY + 120, INPUT "7",INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H") +
                     fn-texto(INPUT posicaoX + 13, INPUT posicaoY + 120, INPUT "8 9 0 4 1 3" ,INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H") +
                     fn-texto(INPUT posicaoX + 153, INPUT posicaoY + 120, INPUT num-c ,INPUT 16602, INPUT 6, INPUT 3, INPUT 0, INPUT 1, INPUT "H").

    RETURN b-ean13.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-grava-macro D-Dialog 
FUNCTION fn-grava-macro RETURNS CHARACTER
  ( INPUT arq-image AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Gravar Imagens na Mem¢ria da Impressora 
    Notes: Esta funcao Grava as imagens em macros, lembrando que a definiá∆o das
           macros est∆o inclusas nas imagens e foram feitas com um editor bin†rio 
------------------------------------------------------------------------------*/
    DEF VAR c-comando AS CHAR.
    ASSIGN c-comando = "net use lpt2: " + IF SESSION:PRINTER-NAME BEGINS "\\" 
                                          THEN SESSION:PRINTER-NAME
                                          ELSE SESSION:PRINTER-PORT.
    OS-COMMAND SILENT VALUE(c-comando).  

    ASSIGN c-comando = "copy /Y /b " + arq-image + " lpt2". 
    OS-COMMAND SILENT VALUE(c-comando). 

    ASSIGN c-comando = "net use lpt2: /DELETE".
    OS-COMMAND SILENT VALUE(c-comando).  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-imp-macro D-Dialog 
FUNCTION fn-imp-macro RETURNS CHARACTER
  ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y AS INTEGER,INPUT macroid AS INTEGER):
/*------------------------------------------------------------------------------
  Purpose: Imprimir as Imagens gravadas na Mem¢ria da Impressora 
------------------------------------------------------------------------------*/
    DEFINE VARIABLE macro AS CHARACTER.
    ASSIGN macro = '~033*p' + STRING(pos-i-X) + 'x' + STRING(pos-i-Y) + 'Y' +
                   '~033&f' + STRING(macroid) + 'y2X'.
    RETURN macro.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-linha D-Dialog 
FUNCTION fn-linha RETURNS CHARACTER
  ( INPUT posicaoX AS INTEGER, INPUT posicaoY AS INTEGER, INPUT tamanho AS INTEGER, INPUT espessura AS INTEGER, INPUT orientacao AS CHARACTER):
    DEFINE VARIABLE linha as CHARACTER.

    ASSIGN linha = '~033*p' + STRING(posicaoX) + 'x' + STRING(posicaoY) + 'Y'.

    IF orientacao = "H" THEN
       ASSIGN linha = linha + '~033*c' + STRING(tamanho) + 'a' + STRING(espessura) + 'b' + '0P'.
    ELSE
       ASSIGN linha = linha + '~033*c' + STRING(espessura) + 'a' + STRING(tamanho) + 'b' + '0P'.

    RETURN linha.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-retangulo D-Dialog 
FUNCTION fn-retangulo RETURNS CHARACTER
  ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y AS INTEGER, INPUT pos-f-X AS INTEGER, INPUT pos-f-Y AS INTEGER, INPUT espessura AS INTEGER):

    DEFINE VARIABLE retangulo as CHARACTER.
    ASSIGN retangulo = '~033*p' + STRING(pos-i-X) + 'x' + STRING(pos-i-Y) + 'Y' +
                       '~033*c' + STRING(pos-f-X - pos-i-X) + 'a' + STRING(espessura) + 'b' + '0P' + 
                       '~033*c' + STRING(espessura) + 'a' + STRING(pos-f-Y - pos-i-Y) + 'b' + '0P' + 
                       '~033*p' + STRING(pos-i-X) + 'x' + string(pos-f-Y) + 'Y' +
                       '~033*c' + STRING(pos-f-X - pos-i-X + espessura) + 'a' + string(espessura) + 'b' + '0P' +
                       '~033*p' + STRING(pos-f-X) + 'x' + string(pos-i-Y) + 'Y' +
                       '~033*c' + STRING(espessura) + 'a' + string(pos-f-Y - pos-i-Y) + 'b' + '0P'.
    RETURN retangulo.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-texto D-Dialog 
FUNCTION fn-texto RETURNS CHARACTER
  ( INPUT pos-i-X AS INTEGER, INPUT pos-i-Y as integer, INPUT texto AS CHARACTER,INPUT fonte AS INTEGER, INPUT tamanho AS INTEGER, INPUT negrito AS INTEGER, INPUT italico AS INTEGER, INPUT fixo AS INTEGER, INPUT sentido AS CHAR):

    DEFINE VARIABLE c-texto as CHARACTER.

    ASSIGN c-texto = '~033*p' + STRING(pos-i-X) + 'x' + STRING(pos-i-Y) + 'Y' + 
                     (IF sentido  = "V" THEN "~033&a90P" ELSE "~033&a0P") +
                     '~033(s' + STRING(fixo) + 'p' + 
                                STRING(tamanho) + (IF fixo = 1 THEN 'v' ELSE 'h') +
                                STRING(italico) + 's' +
                                STRING(negrito) + 'b' +
                                STRING(fonte)   + 'T' +
                     texto + (IF sentido = "V" THEN "~033&a0P" ELSE "").

    RETURN c-texto.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

