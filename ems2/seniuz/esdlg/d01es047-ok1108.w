&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
def input parameter v-row-table  as rowid         no-undo.  

/* Local Variable Definitions ---                                       */
DEF SHARED VAR i-rev AS INT.
DEF VAR i-ct AS INT.
DEF VAR i-lote AS INT.
DEF VAR c-lote AS CHAR FORMAT 'X'.
DEF VAR c-acondic AS CHAR.
DEF VAR c-tipo-def AS CHAR.
DEF VAR c-defeitos AS CHAR.
DEF VAR de-qt-prod AS INT FORMAT ">>>9".

DEF VAR c-desc-item  AS CHAR FORMAT "x(36)".
DEF VAR c-composicao LIKE composi.descricao EXTENT 3.
DEF VAR c-dv         LIKE ref-item-ext.dv.   

DEF VAR v-defeito    AS CHAR EXTENT 3.

DEF VAR l-escolha AS LOG INIT YES.
DEF VAR l-existe-ld AS LOG.
DEF VAR l-defeitos AS LOG.
DEF VAR l-perfeito AS LOG.

DEF VAR i-tp-embal AS INT.

DEF VAR c-usuario AS CHAR.

/* Variaveis para API do Windows */
DEF VAR sBuffer  AS CHAR FORMAT "x(256)" NO-UNDO.
DEF VAR lSize    AS INT INIT 255.

PROCEDURE GetUserNameA EXTERNAL "advapi32.dll":
    DEFINE OUTPUT       PARAMETER lpBuffer  AS CHARACTER.
    DEFINE INPUT-OUTPUT PARAMETER nSize AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cb-qualidade fi-nuance fi-qt-cortes ~
fi-quantidade cb-classe1 cb-tipo-def1 cb-cod-def1 cb-classe2 cb-tipo-def2 ~
cb-cod-def2 cb-classe3 cb-tipo-def3 cb-cod-def3 cb-classe4 cb-tipo-def4 ~
cb-cod-def4 cb-classe5 cb-tipo-def5 cb-cod-def5 cb-classe6 cb-tipo-def6 ~
cb-cod-def6 bt-ok bt-cancela tg-emb-neutra tg-finaliza c-hora RECT-2 RECT-3 ~
RECT-4 rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-ob fi-carro fi-revisador ~
fi-nr-revisadeira fi-dt-ini-revisao fi-hr-ini-revisao fi-nr-seq fi-corte ~
fi-qt-corte fi-qt-prod cb-qualidade fi-nuance fi-qt-cortes fi-quantidade ~
fi-un cb-classe1 cb-tipo-def1 cb-cod-def1 fi-qtd-def1 cb-classe2 ~
cb-tipo-def2 cb-cod-def2 fi-qtd-def2 cb-classe3 cb-tipo-def3 cb-cod-def3 ~
fi-qtd-def3 cb-classe4 cb-tipo-def4 cb-cod-def4 fi-qtd-def4 cb-classe5 ~
cb-tipo-def5 cb-cod-def5 fi-qtd-def5 cb-classe6 cb-tipo-def6 cb-cod-def6 ~
fi-qtd-def6 fi-descricao tg-emb-neutra tg-finaliza c-hora fi-cod-estabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-qtd-def1 fi-qtd-def2 fi-qtd-def3 fi-qtd-def4 ~
fi-qtd-def5 fi-qtd-def6 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Sair" 
     SIZE 10 BY 1.75
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 17 BY 1.75
     BGCOLOR 8 FONT 10.

DEFINE VARIABLE cb-classe1 AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","LD","RG","RT" 
     DROP-DOWN-LIST
     SIZE 9.57 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-classe2 AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","LD","RG","RT" 
     DROP-DOWN-LIST
     SIZE 9.57 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-classe3 AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","LD","RG","RT" 
     DROP-DOWN-LIST
     SIZE 9.57 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-classe4 AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","LD","RG","RT" 
     DROP-DOWN-LIST
     SIZE 9.57 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-classe5 AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","LD","RG","RT" 
     DROP-DOWN-LIST
     SIZE 9.57 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-classe6 AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","LD","RG","RT" 
     DROP-DOWN-LIST
     SIZE 9.57 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-cod-def1 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION
     SIZE 9 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-cod-def2 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION
     SIZE 9 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-cod-def3 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION
     SIZE 9 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-cod-def4 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION
     SIZE 9 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-cod-def5 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION
     SIZE 9 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-cod-def6 AS CHARACTER 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN AUTO-COMPLETION
     SIZE 9 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-qualidade AS CHARACTER FORMAT "X(256)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","A - ‡tima","B - Boa","C - Regular","D - Direcionada","R - Retalho" 
     DROP-DOWN-LIST
     SIZE 36.86 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-tipo-def1 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 8 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-tipo-def2 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 8 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-tipo-def3 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 8 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-tipo-def4 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 8 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-tipo-def5 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 8 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE cb-tipo-def6 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 8 BY 1
     FONT 10 NO-UNDO.

DEFINE VARIABLE c-hora AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17 BY 1.75
     FGCOLOR 9 FONT 10 NO-UNDO.

DEFINE VARIABLE fi-carro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-cod-estabel AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FGCOLOR 12 FONT 10 NO-UNDO.

DEFINE VARIABLE fi-corte AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 25.14 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-descricao AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.96
     BGCOLOR 8 FONT 9 NO-UNDO.

DEFINE VARIABLE fi-dt-ini-revisao AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-hr-ini-revisao AS CHARACTER FORMAT "99:99" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-nr-ob AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-nr-revisadeira AS INTEGER FORMAT ">9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-nr-seq AS INTEGER FORMAT "999999999":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.5
     FGCOLOR 12 FONT 10 NO-UNDO.

DEFINE VARIABLE fi-nuance AS CHARACTER FORMAT "XX":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qt-corte AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qt-cortes AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qt-prod AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qtd-def1 AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.71
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qtd-def2 AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.71
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qtd-def3 AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.71
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qtd-def4 AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.71
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qtd-def5 AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.71
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-qtd-def6 AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.71
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-quantidade AS DECIMAL FORMAT ">>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-revisador AS CHARACTER FORMAT "X(12)" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1.5
     FONT 10 NO-UNDO.

DEFINE VARIABLE fi-un AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.5
     FONT 10 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 122 BY 22.04.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 45 BY 13.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 74 BY 1.96
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 122 BY 2.25
     BGCOLOR 7 .

DEFINE VARIABLE tg-emb-neutra AS LOGICAL INITIAL no 
     LABEL "EMBALAGEM NEUTRA" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .63
     BGCOLOR 8 FONT 0 NO-UNDO.

DEFINE VARIABLE tg-finaliza AS LOGICAL INITIAL no 
     LABEL "FINALIZAR REVIS«O DO CARRO" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .63
     BGCOLOR 8 FONT 0 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi-nr-ob AT ROW 1.5 COL 38 COLON-ALIGNED NO-LABEL
     fi-carro AT ROW 1.5 COL 106.43 COLON-ALIGNED NO-LABEL
     fi-revisador AT ROW 3.25 COL 38 COLON-ALIGNED NO-LABEL
     fi-nr-revisadeira AT ROW 3.25 COL 106.43 COLON-ALIGNED NO-LABEL
     fi-dt-ini-revisao AT ROW 5 COL 38 COLON-ALIGNED NO-LABEL
     fi-hr-ini-revisao AT ROW 5 COL 64 COLON-ALIGNED NO-LABEL
     fi-nr-seq AT ROW 7.25 COL 38 COLON-ALIGNED NO-LABEL
     fi-corte AT ROW 9 COL 37.86 COLON-ALIGNED NO-LABEL
     fi-qt-corte AT ROW 10.75 COL 50 COLON-ALIGNED NO-LABEL
     fi-qt-prod AT ROW 10.75 COL 38 COLON-ALIGNED NO-LABEL
     cb-qualidade AT ROW 13 COL 38.14 COLON-ALIGNED NO-LABEL
     fi-nuance AT ROW 15 COL 38 COLON-ALIGNED NO-LABEL
     fi-qt-cortes AT ROW 16.75 COL 38.14 COLON-ALIGNED NO-LABEL
     fi-quantidade AT ROW 18.54 COL 38.14 COLON-ALIGNED NO-LABEL
     fi-un AT ROW 18.54 COL 52.14 COLON-ALIGNED NO-LABEL
     cb-classe1 AT ROW 8.38 COL 77.57 COLON-ALIGNED NO-LABEL
     cb-tipo-def1 AT ROW 8.25 COL 88.14 COLON-ALIGNED NO-LABEL
     cb-cod-def1 AT ROW 8.25 COL 97.14 COLON-ALIGNED NO-LABEL
     fi-qtd-def1 AT ROW 8.25 COL 107.14 COLON-ALIGNED NO-LABEL
     cb-classe2 AT ROW 10.38 COL 77.57 COLON-ALIGNED NO-LABEL
     cb-tipo-def2 AT ROW 10.25 COL 88.14 COLON-ALIGNED NO-LABEL
     cb-cod-def2 AT ROW 10.25 COL 97.14 COLON-ALIGNED NO-LABEL
     fi-qtd-def2 AT ROW 10.25 COL 107.14 COLON-ALIGNED NO-LABEL
     cb-classe3 AT ROW 12.38 COL 77.57 COLON-ALIGNED NO-LABEL
     cb-tipo-def3 AT ROW 12.25 COL 88.14 COLON-ALIGNED NO-LABEL
     cb-cod-def3 AT ROW 12.25 COL 97.14 COLON-ALIGNED NO-LABEL
     fi-qtd-def3 AT ROW 12.25 COL 107.14 COLON-ALIGNED NO-LABEL
     cb-classe4 AT ROW 14.38 COL 77.57 COLON-ALIGNED NO-LABEL
     cb-tipo-def4 AT ROW 14.25 COL 88.14 COLON-ALIGNED NO-LABEL
     cb-cod-def4 AT ROW 14.25 COL 97.14 COLON-ALIGNED NO-LABEL
     fi-qtd-def4 AT ROW 14.25 COL 107.14 COLON-ALIGNED NO-LABEL
     cb-classe5 AT ROW 16.38 COL 77.57 COLON-ALIGNED NO-LABEL
     cb-tipo-def5 AT ROW 16.25 COL 88.14 COLON-ALIGNED NO-LABEL
     cb-cod-def5 AT ROW 16.25 COL 97.14 COLON-ALIGNED NO-LABEL
     fi-qtd-def5 AT ROW 16.25 COL 107.14 COLON-ALIGNED NO-LABEL
     cb-classe6 AT ROW 18.38 COL 77.57 COLON-ALIGNED NO-LABEL
     cb-tipo-def6 AT ROW 18.25 COL 88.14 COLON-ALIGNED NO-LABEL
     cb-cod-def6 AT ROW 18.25 COL 97.14 COLON-ALIGNED NO-LABEL
     fi-qtd-def6 AT ROW 18.25 COL 107.14 COLON-ALIGNED NO-LABEL
     fi-descricao AT ROW 20.75 COL 76 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 23.79 COL 3.43
     bt-cancela AT ROW 23.79 COL 21.29
     tg-emb-neutra AT ROW 21.5 COL 6 NO-TAB-STOP 
     tg-finaliza AT ROW 21.5 COL 40 NO-TAB-STOP 
     c-hora AT ROW 23.75 COL 104 COLON-ALIGNED NO-LABEL
     fi-cod-estabel AT ROW 1.5 COL 76.86 COLON-ALIGNED NO-LABEL
     "Qtde Cortes:" VIEW-AS TEXT
          SIZE 24.14 BY 1.38 AT ROW 16.79 COL 38.14 RIGHT-ALIGNED
          FONT 10
     "Revisador:" VIEW-AS TEXT
          SIZE 20 BY 1.75 AT ROW 3.25 COL 38.14 RIGHT-ALIGNED
          FONT 10
     "Sequància:" VIEW-AS TEXT
          SIZE 21 BY 1.38 AT ROW 7.25 COL 38 RIGHT-ALIGNED
          FONT 10
     "Revisadeira:" VIEW-AS TEXT
          SIZE 23 BY 1.25 AT ROW 3.33 COL 84.57
          FONT 10
     "In°cio da Revis∆o:" VIEW-AS TEXT
          SIZE 33 BY 1.5 AT ROW 5.13 COL 38 RIGHT-ALIGNED
          FONT 10
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     "Qualidade:" VIEW-AS TEXT
          SIZE 20.57 BY 1.38 AT ROW 13 COL 38.57 RIGHT-ALIGNED
          FONT 10
     "Estab.:" VIEW-AS TEXT
          SIZE 13 BY 1.25 AT ROW 1.5 COL 65.43
          FONT 10
     "Nuance:" VIEW-AS TEXT
          SIZE 16 BY 1.38 AT ROW 15 COL 38 RIGHT-ALIGNED
          FONT 10
     "    <F12>-Confirma      <F5>-Observaá∆o da OB      <F2>-Sair" VIEW-AS TEXT
          SIZE 73 BY 1.71 AT ROW 23.75 COL 32
          FONT 0
     " Defeitos" VIEW-AS TEXT
          SIZE 17 BY 1.25 AT ROW 6.25 COL 90.14
          BGCOLOR 8 FONT 10
     "CLASSE" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 7.5 COL 79.57
          BGCOLOR 8 FONT 9
     "C‡DIGO" VIEW-AS TEXT
          SIZE 8.72 BY .54 AT ROW 7.5 COL 99.29
          BGCOLOR 8 FONT 9
     "QUANTIDADE" VIEW-AS TEXT
          SIZE 12.86 BY .54 AT ROW 7.5 COL 109.14
          BGCOLOR 8 FONT 9
     "TIPO" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 7.5 COL 90.14
          BGCOLOR 8 FONT 9
     "N£mero da OB:" VIEW-AS TEXT
          SIZE 29 BY 1.5 AT ROW 1.5 COL 38 RIGHT-ALIGNED
          FONT 10
     "Qtde Produzida:" VIEW-AS TEXT
          SIZE 30 BY 1.38 AT ROW 10.75 COL 38 RIGHT-ALIGNED
          FONT 10
     "Carro:" VIEW-AS TEXT
          SIZE 12 BY 1.25 AT ROW 1.63 COL 95.86
          FONT 10
     "Quantidade:" VIEW-AS TEXT
          SIZE 23 BY 1.38 AT ROW 18.54 COL 38 RIGHT-ALIGNED
          FONT 10
     "Corte:" VIEW-AS TEXT
          SIZE 12 BY 1.38 AT ROW 9 COL 38 RIGHT-ALIGNED
          FONT 10
     RECT-2 AT ROW 1.13 COL 2
     RECT-3 AT ROW 6.96 COL 78
     RECT-4 AT ROW 20.75 COL 3
     rt-buttom AT ROW 23.5 COL 2
     SPACE(0.57) SKIP(0.25)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Revis∆o de Tecidos - D01ES047".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-carro IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-estabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-corte IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-descricao IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-ini-revisao IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-hr-ini-revisao IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-ob IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-revisadeira IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-seq IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-corte IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-prod IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qtd-def1 IN FRAME Dialog-Frame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-def2 IN FRAME Dialog-Frame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-def3 IN FRAME Dialog-Frame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-def4 IN FRAME Dialog-Frame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-def5 IN FRAME Dialog-Frame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qtd-def6 IN FRAME Dialog-Frame
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-revisador IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-un IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TEXT-LITERAL "N£mero da OB:"
          SIZE 29 BY 1.5 AT ROW 1.5 COL 38 RIGHT-ALIGNED                */

/* SETTINGS FOR TEXT-LITERAL "Revisador:"
          SIZE 20 BY 1.75 AT ROW 3.25 COL 38.14 RIGHT-ALIGNED           */

/* SETTINGS FOR TEXT-LITERAL "In°cio da Revis∆o:"
          SIZE 33 BY 1.5 AT ROW 5.13 COL 38 RIGHT-ALIGNED               */

/* SETTINGS FOR TEXT-LITERAL "Sequància:"
          SIZE 21 BY 1.38 AT ROW 7.25 COL 38 RIGHT-ALIGNED              */

/* SETTINGS FOR TEXT-LITERAL "Corte:"
          SIZE 12 BY 1.38 AT ROW 9 COL 38 RIGHT-ALIGNED                 */

/* SETTINGS FOR TEXT-LITERAL "Qtde Produzida:"
          SIZE 30 BY 1.38 AT ROW 10.75 COL 38 RIGHT-ALIGNED             */

/* SETTINGS FOR TEXT-LITERAL "Qualidade:"
          SIZE 20.57 BY 1.38 AT ROW 13 COL 38.57 RIGHT-ALIGNED          */

/* SETTINGS FOR TEXT-LITERAL "Nuance:"
          SIZE 16 BY 1.38 AT ROW 15 COL 38 RIGHT-ALIGNED                */

/* SETTINGS FOR TEXT-LITERAL "Qtde Cortes:"
          SIZE 24.14 BY 1.38 AT ROW 16.79 COL 38.14 RIGHT-ALIGNED       */

/* SETTINGS FOR TEXT-LITERAL "Quantidade:"
          SIZE 23 BY 1.38 AT ROW 18.54 COL 38 RIGHT-ALIGNED             */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME Dialog-Frame:HANDLE
       ROW             = 18.5
       COLUMN          = 7
       HEIGHT          = 1.67
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENTRY OF FRAME Dialog-Frame /* Revis∆o de Tecidos - D01ES047 */
DO:
   RUN GetUserNameA (OUTPUT sBuffer, INPUT-OUTPUT lSize).
 
   FIND FIRST ob-param NO-LOCK NO-ERROR.
 
   FIND ordem-benefic WHERE
        ROWID(ordem-benefic) = v-row-table SHARE-LOCK NO-ERROR.
   ASSIGN ordem-benefic.situacao = 2.
 
   ASSIGN c-usuario = TRIM(sBuffer).
 
   ASSIGN fi-nr-ob:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ordem-benefic.nr-ob)
          fi-carro:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ordem-benefic.nr-carro
          fi-revisador:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-usuario
          fi-cod-estabel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ordem-benefic.cod-estabel
          fi-nr-revisadeira:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(i-rev)
          fi-dt-ini-revisao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
          fi-hr-ini-revisao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME,"HH:MM")
          fi-un:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ordem-benefic.un.
 
   FIND FIRST ob-etiqueta WHERE
              ob-etiqueta.cod-estabel = ordem-benefic.cod-estabel AND
              ob-etiqueta.nr-ob = ordem-benefic.nr-ob AND
              ob-etiqueta.dt-ob = ordem-benefic.dt-ob AND
              ob-etiqueta.nr-carro = ordem-benefic.nr-carro AND
              ob-etiqueta.situacao = 1
              USE-INDEX indice1 SHARE-LOCK NO-ERROR.
 
   IF NOT AVAIL ob-etiqueta THEN
      APPLY 'choose' TO bt-cancela.
 
   ASSIGN fi-nr-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.num-etiqueta)
          fi-corte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.acondic
          fi-qt-corte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ordem-benefic.qtd-planejada)
          fi-qt-prod:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ordem-benefic.qtd-produzida).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Revis∆o de Tecidos - D01ES047 */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela Dialog-Frame
ON CHOOSE OF bt-cancela IN FRAME Dialog-Frame /* Sair */
DO:
  DO TRANSACTION:
     FIND ordem-benefic WHERE
          ROWID(ordem-benefic) = v-row-table SHARE-LOCK NO-ERROR.
    
     IF INPUT FRAME {&FRAME-NAME} tg-finaliza THEN DO.
        FOR EACH ob-etiqueta OF ordem-benefic WHERE
                 ob-etiqueta.situacao = 1 SHARE-LOCK.

            CREATE movto-etq.
            ASSIGN movto-etq.dt-trans = TODAY
                   movto-etq.esp-docto = 'DEL'
                   movto-etq.nro-docto = STRING(ob-etiqueta.nr-ob)
                   movto-etq.num-etiqueta = ob-etiqueta.num-etiqueta 
                   movto-etq.quantidade = ob-etiqueta.quantidade
                   movto-etq.tipo-trans = NO
                   movto-etq.char-1 = "Usuario: " + c-usuario + FILL(" ",10) +
                                      "Hora: " + STRING(TIME,"HH:MM:SS") + FILL(" ",10) +
                                      "Situacao: " + STRING(ob-etiqueta.situacao).

            DELETE ob-etiqueta.
        END.
        ASSIGN ordem-benefic.situacao = 4.
     END.
     ELSE DO.
        FIND LAST ob-etiqueta WHERE
                  ob-etiqueta.nr-ob = ordem-benefic.nr-ob AND
                  ob-etiqueta.dt-ob = ordem-benefic.dt-ob AND
                  ob-etiqueta.nr-carro = ordem-benefic.nr-carro USE-INDEX indice2
                  SHARE-LOCK NO-ERROR.
        
        IF AVAIL ob-etiqueta THEN
           ASSIGN ordem-benefic.situacao = 3.
        ELSE
           ASSIGN ordem-benefic.situacao = 1.
     END.
  END.
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   DO TRANSACTION:
      SESSION:SET-WAIT-STATE("general":U).
      
      FIND ordem-benefic WHERE
           ROWID(ordem-benefic) = v-row-table SHARE-LOCK NO-ERROR.

      FIND FIRST ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel = ordem-benefic.cod-estabel AND
                 ob-etiqueta.num-etiqueta = INPUT FRAME {&FRAME-NAME} fi-nr-seq
                 USE-INDEX indice4 SHARE-LOCK NO-ERROR.

      FIND ob-acondic WHERE
           ob-acondic.cod-estabel = ordem-benefic.cod-estabel AND
           ob-acondic.nr-ob    = ordem-benefic.nr-ob AND 
           ob-acondic.dt-ob    = ordem-benefic.dt-ob AND
           ob-acondic.nr-carro = ordem-benefic.nr-carro AND
           ob-acondic.acondic  = ob-etiqueta.acondic 
           NO-LOCK NO-ERROR.

      IF INPUT FRAME {&FRAME-NAME} fi-quantidade = 0 THEN DO.
         MESSAGE "Quantidade deve ser Informada..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO fi-quantidade.
         RETURN NO-APPLY.
      END.

      IF ob-etiqueta.acondic BEGINS "ROLO" THEN
         ASSIGN c-lote = 'R'
                i-tp-embal = 1.
      ELSE
      IF ob-etiqueta.acondic BEGINS "PECA" THEN
         ASSIGN c-lote = 'P'
                i-tp-embal = 2.
      ELSE
      IF ob-etiqueta.acondic BEGINS "CORTE" THEN
         ASSIGN c-lote = 'C'
                i-tp-embal = 4.
      ELSE
         ASSIGN c-lote = 'S'
                i-tp-embal = 3.
    
      FIND corte-comerc WHERE
           corte-comerc.compr-min <= INPUT FRAME {&FRAME-NAME} fi-quantidade AND
           corte-comerc.compr-max >= INPUT FRAME {&FRAME-NAME} fi-quantidade AND
           corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

      IF NOT AVAIL corte-comerc THEN DO.
         MESSAGE "Metragem informada n∆o pertece a nenhum Corte Cadastrado..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO fi-qt-prod.
         RETURN NO-APPLY.
      END.

      IF corte-comerc.descricao <> INPUT FRAME {&FRAME-NAME} fi-corte THEN DO.
         MESSAGE "Corte para o Rolo Ç diferente do Planejado..." SKIP(1)
                 "Altera o Corte ?"  
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  TITLE "" UPDATE choice LIKE l-escolha.
         IF NOT choice THEN DO.
            APPLY 'entry' TO fi-quantidade.
            RETURN NO-APPLY.
         END.
      END.

      FIND item-ext WHERE
           item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
      IF AVAIL item-ext AND
         SUBSTR(ob-etiqueta.cod-refer,3,4) <> "0520" AND
         item-ext.indigo = YES AND
         INPUT FRAME {&FRAME-NAME} fi-nuance = '' THEN DO.
         MESSAGE "Nuance deve ser Informada..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO fi-nuance.
         RETURN NO-APPLY.
      END.
      /* Validaá∆o genÇrica da nuance */
      IF INPUT FRAME {&FRAME-NAME} fi-nuance <> '' THEN DO:
         IF NOT((SUBSTR(INPUT FRAME {&FRAME-NAME} fi-nuance,1,1) >= "a" AND
                 SUBSTR(INPUT FRAME {&FRAME-NAME} fi-nuance,1,1) <= "z" AND
                 SUBSTR(INPUT FRAME {&FRAME-NAME} fi-nuance,2,1) >= "1" AND
                 SUBSTR(INPUT FRAME {&FRAME-NAME} fi-nuance,2,1) <= "9") OR
                 SUBSTR(INPUT FRAME {&FRAME-NAME} fi-nuance,2,1) = "") THEN DO:
            MESSAGE "Nuance Inv†lida..." VIEW-AS ALERT-BOX.
            APPLY 'entry' TO fi-nuance.
            RETURN NO-APPLY.
         END.
      END.
    
      ASSIGN INPUT FRAME {&FRAME-NAME} cb-classe1  INPUT FRAME {&FRAME-NAME} cb-classe2
             INPUT FRAME {&FRAME-NAME} cb-classe3  INPUT FRAME {&FRAME-NAME} cb-classe4
             INPUT FRAME {&FRAME-NAME} cb-classe5  INPUT FRAME {&FRAME-NAME} cb-classe6.
    
      ASSIGN INPUT FRAME {&FRAME-NAME} cb-tipo-def1  INPUT FRAME {&FRAME-NAME} cb-tipo-def2
             INPUT FRAME {&FRAME-NAME} cb-tipo-def3  INPUT FRAME {&FRAME-NAME} cb-tipo-def4
             INPUT FRAME {&FRAME-NAME} cb-tipo-def5  INPUT FRAME {&FRAME-NAME} cb-tipo-def6.
    
      ASSIGN INPUT FRAME {&FRAME-NAME} cb-cod-def1  INPUT FRAME {&FRAME-NAME} cb-cod-def2
             INPUT FRAME {&FRAME-NAME} cb-cod-def3  INPUT FRAME {&FRAME-NAME} cb-cod-def4
             INPUT FRAME {&FRAME-NAME} cb-cod-def5  INPUT FRAME {&FRAME-NAME} cb-cod-def6.
    
      ASSIGN INPUT FRAME {&FRAME-NAME} fi-qtd-def1  INPUT FRAME {&FRAME-NAME} fi-qtd-def2
             INPUT FRAME {&FRAME-NAME} fi-qtd-def3  INPUT FRAME {&FRAME-NAME} fi-qtd-def4
             INPUT FRAME {&FRAME-NAME} fi-qtd-def5  INPUT FRAME {&FRAME-NAME} fi-qtd-def6.
    
      IF (cb-classe1 = "RT" AND fi-qtd-def1 = 0) OR (cb-classe2 = "RT" AND fi-qtd-def2 = 0) OR
         (cb-classe3 = "RT" AND fi-qtd-def3 = 0) OR (cb-classe4 = "RT" AND fi-qtd-def4 = 0) OR
         (cb-classe5 = "RT" AND fi-qtd-def5 = 0) OR (cb-classe6 = "RT" AND fi-qtd-def6 = 0) THEN DO.
          MESSAGE "Quantidade de Retalho deve ser Informada..." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO cb-classe1.
          RETURN NO-APPLY.
      END.
    
      ASSIGN l-existe-ld = NO
             l-defeitos = NO
             v-defeito = "".

      FIND qualid-tecido WHERE
           qualid-tecido.codigo = SUBSTR(INPUT FRAME {&FRAME-NAME} cb-qualidade,1,1)
           NO-LOCK NO-ERROR.
    
      RUN pi-vrf-defeitos (INPUT cb-classe1, INPUT cb-tipo-def1:HANDLE, INPUT cb-cod-def1:HANDLE).
      IF RETURN-VALUE <> '' THEN RETURN NO-APPLY.
      RUN pi-vrf-defeitos (INPUT cb-classe2, INPUT cb-tipo-def2:HANDLE, INPUT cb-cod-def2:HANDLE).
      IF RETURN-VALUE <> '' THEN RETURN NO-APPLY.
      RUN pi-vrf-defeitos (INPUT cb-classe3, INPUT cb-tipo-def3:HANDLE, INPUT cb-cod-def3:HANDLE).
      IF RETURN-VALUE <> '' THEN RETURN NO-APPLY.
      RUN pi-vrf-defeitos (INPUT cb-classe4, INPUT cb-tipo-def4:HANDLE, INPUT cb-cod-def4:HANDLE).
      IF RETURN-VALUE <> '' THEN RETURN NO-APPLY.
      RUN pi-vrf-defeitos (INPUT cb-classe5, INPUT cb-tipo-def5:HANDLE, INPUT cb-cod-def5:HANDLE).
      IF RETURN-VALUE <> '' THEN RETURN NO-APPLY.
      RUN pi-vrf-defeitos (INPUT cb-classe6, INPUT cb-tipo-def6:HANDLE, INPUT cb-cod-def6:HANDLE).
      IF RETURN-VALUE <> '' THEN RETURN NO-APPLY.
    
      IF qualid-tecido.obriga-def = YES AND NOT l-defeitos THEN DO.
         MESSAGE "Favor informar os Defeitos para a qualidade Digitada..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cb-classe1.
         RETURN NO-APPLY.
      END.
      
      IF qualid-tecido.obriga-def = NO AND l-defeitos THEN DO.
         MESSAGE "N∆o podem ser informados Defeitos para Qualidade Perfeita..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cb-classe1.
         RETURN NO-APPLY.
      END.

      IF qualid-tecido.class-qualid = 3 AND l-existe-ld THEN DO.
         MESSAGE "Essa Qualidade n∆o pode conter defeitos LD..." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO cb-classe1.
         RETURN NO-APPLY.
      END.

      IF qualid-tecido.class-qualid = 2 AND NOT l-existe-ld THEN DO.
          MESSAGE "Essa Qualidade s¢ pode conter defeitos LD..." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO cb-classe1.
          RETURN NO-APPLY.
       END.

      IF INPUT FRAME {&FRAME-NAME} fi-qt-prod >= ordem-benefic.qtd-planejada THEN DO.
         MESSAGE "Quantidade Produzida Ç MAIOR que Quantidade Solicitada" SKIP(1)
                 "Deseja Continuar ?"  
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                  TITLE "" UPDATE l-conf-corte LIKE l-escolha.
         IF NOT l-conf-corte THEN DO.
            APPLY 'entry' TO fi-quantidade.
            RETURN NO-APPLY.
         END.
      END.

      ASSIGN ordem-benefic.qtd-produzida = ordem-benefic.qtd-produzida + 1.

      ASSIGN ob-etiqueta.dt-emissao = TODAY
             ob-etiqueta.hr-emissao = STRING(TIME,"HH:MM")
             ob-etiqueta.tipo-ordem = ordem-benefic.tipo-ordem
             ob-etiqueta.acondic = corte-comerc.descricao
             ob-etiqueta.corte-comerc = corte-comerc.codigo
             ob-etiqueta.situacao = 2 
             ob-etiqueta.resp-revisao = INPUT FRAME {&FRAME-NAME} fi-revisador
             ob-etiqueta.nr-revisadeira = INPUT FRAME {&FRAME-NAME} fi-nr-revisadeira 
             ob-etiqueta.nr-cortes = INPUT FRAME {&FRAME-NAME} fi-qt-cortes 
             ob-etiqueta.nuance = INPUT FRAME {&FRAME-NAME} fi-nuance 
             ob-etiqueta.cod-qualid = SUBSTR(INPUT FRAME {&FRAME-NAME} cb-qualidade,1,1) 
             ob-etiqueta.nr-lote = c-lote + IF i-tp-embal = 4 
                                            THEN 'A'
                                            ELSE IF qualid-tecido.class-qualid = 2
                                                 THEN 'D' ELSE 'P' 
             ob-etiqueta.quantidade = INPUT FRAME {&FRAME-NAME} fi-quantidade
             ob-etiqueta.qtd-original = INPUT FRAME {&FRAME-NAME} fi-quantidade 
             ob-etiqueta.emb-neutra = INPUT FRAME {&FRAME-NAME} tg-emb-neutra
             ob-etiqueta.un = ordem-benefic.un.

      IF SUBSTR(ob-etiqueta.it-codigo,6,1) = '0' THEN
         ASSIGN ob-etiqueta.nr-lote = "".

      IF ob-etiqueta.tipo-ordem = 1 THEN DO.
         FIND mov-est-acbm WHERE
              mov-est-acbm.cod-estabel = ordem-benefic.cod-estabel AND
              mov-est-acbm.data-mov    = ob-etiqueta.dt-emissao AND
              mov-est-acbm.num-lote    = ordem-benefic.nr-ob AND
              mov-est-acbm.it-codigo   = ordem-benefic.it-codigo AND
              mov-est-acbm.cod-refer   = ordem-benefic.cod-refer
              SHARE-LOCK NO-ERROR.
    
         IF NOT AVAIL mov-est-acbm THEN DO.
            CREATE mov-est-acbm.
            ASSIGN mov-est-acbm.cod-estabel = ordem-benefic.cod-estabel
                   mov-est-acbm.data-mov    = ob-etiqueta.dt-emissao
                   mov-est-acbm.hora-mov    = TIME
                   mov-est-acbm.num-lote    = ordem-benefic.nr-ob 
                   mov-est-acbm.it-codigo   = ordem-benefic.it-codigo
                   mov-est-acbm.cod-refer   = ordem-benefic.cod-refer
                   mov-est-acbm.tipo-tear   = ordem-benefic.tipo-tear.
         END.
         IF qualid-tecido.class-qualid = 1 AND NOT l-defeitos THEN 
            ASSIGN mov-est-acbm.qtd-tot-perf = mov-est-acbm.qtd-tot-perf + ob-etiqueta.quantidade.
    
         RUN pi-grava-defeitos (INPUT cb-classe1, INPUT cb-tipo-def1, INPUT cb-cod-def1, INPUT fi-qtd-def1).
         RUN pi-grava-defeitos (INPUT cb-classe2, INPUT cb-tipo-def2, INPUT cb-cod-def2, INPUT fi-qtd-def2).
         RUN pi-grava-defeitos (INPUT cb-classe3, INPUT cb-tipo-def3, INPUT cb-cod-def3, INPUT fi-qtd-def3).
         RUN pi-grava-defeitos (INPUT cb-classe4, INPUT cb-tipo-def4, INPUT cb-cod-def4, INPUT fi-qtd-def4).
         RUN pi-grava-defeitos (INPUT cb-classe5, INPUT cb-tipo-def5, INPUT cb-cod-def5, INPUT fi-qtd-def5).
         RUN pi-grava-defeitos (INPUT cb-classe6, INPUT cb-tipo-def6, INPUT cb-cod-def6, INPUT fi-qtd-def6).
      END.
    
      IF AVAIL mov-est-acbm AND ob-etiqueta.nuance <> "" THEN
         RUN pi-grava-nuance.
    
      FIND CURRENT mov-est-acbm NO-LOCK NO-ERROR.
    
      FIND FIRST ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel = ordem-benefic.cod-estabel AND
                 ob-etiqueta.nr-ob = ordem-benefic.nr-ob AND
                 ob-etiqueta.dt-ob = ordem-benefic.dt-ob AND
                 ob-etiqueta.nr-carro = ordem-benefic.nr-carro AND
                 ob-etiqueta.situacao = 1
                 USE-INDEX indice1 SHARE-LOCK NO-ERROR.

      IF NOT AVAIL ob-etiqueta THEN
         ASSIGN tg-finaliza:SCREEN-VALUE = 'YES'.
      ELSE
         ASSIGN fi-nr-seq:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ob-etiqueta.num-etiqueta)
                fi-corte:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ob-etiqueta.acondic
                fi-qt-prod:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ordem-benefic.qtd-produzida)
                cb-qualidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " "
                fi-nuance:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " " 
                fi-qt-cortes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0"
                fi-quantidade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".

      ASSIGN cb-classe1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " "
             cb-classe2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " "
             cb-classe3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " "
             cb-classe4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " "
             cb-classe5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " "
             cb-classe6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = " ".

      APPLY 'value-changed' TO cb-classe1.
      APPLY 'value-changed' TO cb-classe2.
      APPLY 'value-changed' TO cb-classe3.
      APPLY 'value-changed' TO cb-classe4.
      APPLY 'value-changed' TO cb-classe5.
      APPLY 'value-changed' TO cb-classe6.

      SESSION:SET-WAIT-STATE("":U).
  END.

  IF INPUT FRAME {&FRAME-NAME} tg-finaliza THEN 
     APPLY 'choose' TO bt-cancela.
  ELSE
     APPLY 'entry' TO cb-qualidade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-classe1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe1 Dialog-Frame
ON ENTRY OF cb-classe1 IN FRAME Dialog-Frame
DO:
    RUN pi-monta-tpdef (INPUT cb-tipo-def1:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe1 Dialog-Frame
ON VALUE-CHANGED OF cb-classe1 IN FRAME Dialog-Frame
DO:
  RUN pi-classe (INPUT SELF:HANDLE, INPUT cb-tipo-def1:HANDLE,
                 INPUT cb-cod-def1:HANDLE, INPUT fi-qtd-def1:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-classe2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe2 Dialog-Frame
ON ENTRY OF cb-classe2 IN FRAME Dialog-Frame
DO:
    RUN pi-monta-tpdef (INPUT cb-tipo-def2:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe2 Dialog-Frame
ON VALUE-CHANGED OF cb-classe2 IN FRAME Dialog-Frame
DO:
    RUN pi-classe (INPUT SELF:HANDLE, INPUT cb-tipo-def2:HANDLE,
                   INPUT cb-cod-def2:HANDLE, INPUT fi-qtd-def2:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-classe3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe3 Dialog-Frame
ON ENTRY OF cb-classe3 IN FRAME Dialog-Frame
DO:
    RUN pi-monta-tpdef (INPUT cb-tipo-def3:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe3 Dialog-Frame
ON VALUE-CHANGED OF cb-classe3 IN FRAME Dialog-Frame
DO:
    RUN pi-classe (INPUT SELF:HANDLE, INPUT cb-tipo-def3:HANDLE,
                   INPUT cb-cod-def3:HANDLE, INPUT fi-qtd-def3:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-classe4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe4 Dialog-Frame
ON ENTRY OF cb-classe4 IN FRAME Dialog-Frame
DO:
    RUN pi-monta-tpdef (INPUT cb-tipo-def4:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe4 Dialog-Frame
ON VALUE-CHANGED OF cb-classe4 IN FRAME Dialog-Frame
DO:
    RUN pi-classe (INPUT SELF:HANDLE, INPUT cb-tipo-def4:HANDLE,
                   INPUT cb-cod-def4:HANDLE, INPUT fi-qtd-def4:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-classe5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe5 Dialog-Frame
ON ENTRY OF cb-classe5 IN FRAME Dialog-Frame
DO:
    RUN pi-monta-tpdef (INPUT cb-tipo-def5:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe5 Dialog-Frame
ON VALUE-CHANGED OF cb-classe5 IN FRAME Dialog-Frame
DO:
    RUN pi-classe (INPUT SELF:HANDLE, INPUT cb-tipo-def5:HANDLE,
                   INPUT cb-cod-def5:HANDLE, INPUT fi-qtd-def5:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-classe6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe6 Dialog-Frame
ON ENTRY OF cb-classe6 IN FRAME Dialog-Frame
DO:
    RUN pi-monta-tpdef (INPUT cb-tipo-def6:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-classe6 Dialog-Frame
ON VALUE-CHANGED OF cb-classe6 IN FRAME Dialog-Frame
DO:
    RUN pi-classe (INPUT SELF:HANDLE, INPUT cb-tipo-def6:HANDLE,
                   INPUT cb-cod-def6:HANDLE, INPUT fi-qtd-def6:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cod-def1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def1 Dialog-Frame
ON ENTRY OF cb-cod-def1 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def1 Dialog-Frame
ON LEAVE OF cb-cod-def1 IN FRAME Dialog-Frame
DO:
  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def1 Dialog-Frame
ON VALUE-CHANGED OF cb-cod-def1 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cod-def2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def2 Dialog-Frame
ON ENTRY OF cb-cod-def2 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def2 Dialog-Frame
ON LEAVE OF cb-cod-def2 IN FRAME Dialog-Frame
DO:
  IF LOOKUP(SELF:SCREEN-VALUE,c-defeitos) = 0 THEN DO:
     MESSAGE "C¢digo de Defeito Invalido..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def2 Dialog-Frame
ON VALUE-CHANGED OF cb-cod-def2 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cod-def3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def3 Dialog-Frame
ON ENTRY OF cb-cod-def3 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def3 Dialog-Frame
ON LEAVE OF cb-cod-def3 IN FRAME Dialog-Frame
DO:
  IF LOOKUP(SELF:SCREEN-VALUE,c-defeitos) = 0 THEN DO:
     MESSAGE "C¢digo de Defeito Invalido..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def3 Dialog-Frame
ON VALUE-CHANGED OF cb-cod-def3 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cod-def4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def4 Dialog-Frame
ON ENTRY OF cb-cod-def4 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def4 Dialog-Frame
ON LEAVE OF cb-cod-def4 IN FRAME Dialog-Frame
DO:
  IF LOOKUP(SELF:SCREEN-VALUE,c-defeitos) = 0 THEN DO:
     MESSAGE "C¢digo de Defeito Invalido..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def4 Dialog-Frame
ON VALUE-CHANGED OF cb-cod-def4 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cod-def5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def5 Dialog-Frame
ON ENTRY OF cb-cod-def5 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def5 Dialog-Frame
ON LEAVE OF cb-cod-def5 IN FRAME Dialog-Frame
DO:
  IF LOOKUP(SELF:SCREEN-VALUE,c-defeitos) = 0 THEN DO:
     MESSAGE "C¢digo de Defeito Invalido..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def5 Dialog-Frame
ON VALUE-CHANGED OF cb-cod-def5 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-cod-def6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def6 Dialog-Frame
ON ENTRY OF cb-cod-def6 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def6 Dialog-Frame
ON LEAVE OF cb-cod-def6 IN FRAME Dialog-Frame
DO:
  IF LOOKUP(SELF:SCREEN-VALUE,c-defeitos) = 0 THEN DO:
     MESSAGE "C¢digo de Defeito Invalido..." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO SELF.
     RETURN NO-APPLY.
  END.

  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-cod-def6 Dialog-Frame
ON VALUE-CHANGED OF cb-cod-def6 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-def (INPUT INPUT FRAME {&FRAME-NAME} cb-tipo-def6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tipo-def1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def1 Dialog-Frame
ON ENTRY OF cb-tipo-def1 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def1 Dialog-Frame
ON LEAVE OF cb-tipo-def1 IN FRAME Dialog-Frame
DO:
  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def1 Dialog-Frame
ON VALUE-CHANGED OF cb-tipo-def1 IN FRAME Dialog-Frame
DO:
   RUN pi-monta-def (INPUT cb-cod-def1:HANDLE).
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tipo-def2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def2 Dialog-Frame
ON ENTRY OF cb-tipo-def2 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def2 Dialog-Frame
ON LEAVE OF cb-tipo-def2 IN FRAME Dialog-Frame
DO:
  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def2 Dialog-Frame
ON VALUE-CHANGED OF cb-tipo-def2 IN FRAME Dialog-Frame
DO:
   RUN pi-monta-def (INPUT cb-cod-def2:HANDLE).
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tipo-def3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def3 Dialog-Frame
ON ENTRY OF cb-tipo-def3 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def3 Dialog-Frame
ON LEAVE OF cb-tipo-def3 IN FRAME Dialog-Frame
DO:
  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def3 Dialog-Frame
ON VALUE-CHANGED OF cb-tipo-def3 IN FRAME Dialog-Frame
DO:
   RUN pi-monta-def (INPUT cb-cod-def3:HANDLE).
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tipo-def4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def4 Dialog-Frame
ON ENTRY OF cb-tipo-def4 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def4 Dialog-Frame
ON LEAVE OF cb-tipo-def4 IN FRAME Dialog-Frame
DO:
  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def4 Dialog-Frame
ON VALUE-CHANGED OF cb-tipo-def4 IN FRAME Dialog-Frame
DO:
   RUN pi-monta-def (INPUT cb-cod-def4:HANDLE).
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tipo-def5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def5 Dialog-Frame
ON ENTRY OF cb-tipo-def5 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def5 Dialog-Frame
ON LEAVE OF cb-tipo-def5 IN FRAME Dialog-Frame
DO:
  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def5 Dialog-Frame
ON VALUE-CHANGED OF cb-tipo-def5 IN FRAME Dialog-Frame
DO:
   RUN pi-monta-def (INPUT cb-cod-def5:HANDLE).
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tipo-def6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def6 Dialog-Frame
ON ENTRY OF cb-tipo-def6 IN FRAME Dialog-Frame
DO:
   RUN pi-monta-tpdef (INPUT SELF:HANDLE).
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def6 Dialog-Frame
ON LEAVE OF cb-tipo-def6 IN FRAME Dialog-Frame
DO:
  ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tipo-def6 Dialog-Frame
ON VALUE-CHANGED OF cb-tipo-def6 IN FRAME Dialog-Frame
DO:
   RUN pi-mostra-tpdef (INPUT SELF:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte Dialog-Frame
ON VALUE-CHANGED OF fi-corte IN FRAME Dialog-Frame
DO:
  ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nuance
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nuance Dialog-Frame
ON VALUE-CHANGED OF fi-nuance IN FRAME Dialog-Frame
DO:
  ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME} = UPPER(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON 'F5':U ANYWHERE DO:
   MESSAGE ordem-benefic.observacao VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

ON 'F11':U ANYWHERE DO:
   APPLY 'choose' TO bt-cancela IN FRAME Dialog-Frame.
   RETURN NO-APPLY.
END.

ON 'F12':U ANYWHERE DO:
   APPLY 'choose' TO bt-ok IN FRAME Dialog-Frame.
   RETURN NO-APPLY.
END.

ON 'return':U ANYWHERE DO:
   APPLY 'TAB' TO SELF.
   RETURN NO-APPLY.
END.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "d01es047-ok1108.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "d01es047-ok1108.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlFrame.PsTimer.Tick Dialog-Frame 
PROCEDURE CtrlFrame.PsTimer.Tick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN c-hora:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME,"HH:MM:SS").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY fi-nr-ob fi-carro fi-revisador fi-nr-revisadeira fi-dt-ini-revisao 
          fi-hr-ini-revisao fi-nr-seq fi-corte fi-qt-corte fi-qt-prod 
          cb-qualidade fi-nuance fi-qt-cortes fi-quantidade fi-un cb-classe1 
          cb-tipo-def1 cb-cod-def1 fi-qtd-def1 cb-classe2 cb-tipo-def2 
          cb-cod-def2 fi-qtd-def2 cb-classe3 cb-tipo-def3 cb-cod-def3 
          fi-qtd-def3 cb-classe4 cb-tipo-def4 cb-cod-def4 fi-qtd-def4 cb-classe5 
          cb-tipo-def5 cb-cod-def5 fi-qtd-def5 cb-classe6 cb-tipo-def6 
          cb-cod-def6 fi-qtd-def6 fi-descricao tg-emb-neutra tg-finaliza c-hora 
          fi-cod-estabel 
      WITH FRAME Dialog-Frame.
  ENABLE cb-qualidade fi-nuance fi-qt-cortes fi-quantidade cb-classe1 
         cb-tipo-def1 cb-cod-def1 cb-classe2 cb-tipo-def2 cb-cod-def2 
         cb-classe3 cb-tipo-def3 cb-cod-def3 cb-classe4 cb-tipo-def4 
         cb-cod-def4 cb-classe5 cb-tipo-def5 cb-cod-def5 cb-classe6 
         cb-tipo-def6 cb-cod-def6 bt-ok bt-cancela tg-emb-neutra tg-finaliza 
         c-hora RECT-2 RECT-3 RECT-4 rt-buttom 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-classe Dialog-Frame 
PROCEDURE pi-classe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-classe AS HANDLE.
    DEF INPUT PARAMETER p-tipo-def AS HANDLE.
    DEF INPUT PARAMETER p-cod-def AS HANDLE.
    DEF INPUT PARAMETER p-qtd-def AS HANDLE.

    DEF VAR i-qt-def AS INT.

    /* Calcular a quantidade de defeitos digitados */
    ASSIGN i-qt-def = 0.
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe1,"RG,LD") > 0 THEN ASSIGN i-qt-def = i-qt-def + 1.
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe2,"RG,LD") > 0 THEN ASSIGN i-qt-def = i-qt-def + 1.
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe3,"RG,LD") > 0 THEN ASSIGN i-qt-def = i-qt-def + 1.
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe4,"RG,LD") > 0 THEN ASSIGN i-qt-def = i-qt-def + 1.
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe5,"RG,LD") > 0 THEN ASSIGN i-qt-def = i-qt-def + 1.
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe6,"RG,LD") > 0 THEN ASSIGN i-qt-def = i-qt-def + 1.

    ASSIGN p-qtd-def:SCREEN-VALUE = "0"
           p-qtd-def:SENSITIVE = NO.
    IF p-classe:SCREEN-VALUE = "RT" THEN
       ASSIGN p-qtd-def:SENSITIVE = YES.
    ELSE DO.
       IF LOOKUP(p-classe:SCREEN-VALUE,"RG,LD") > 0 AND
          INPUT FRAME {&FRAME-NAME} fi-quantidade = 0 THEN DO.
          MESSAGE "Para digitar os Defeitos, informe primeiro a Quantidade Produzida..."
                  VIEW-AS ALERT-BOX.
          ASSIGN p-classe:SCREEN-VALUE = " ".
          RETURN.
       END.
       ELSE DO.
          IF p-tipo-def:LIST-ITEMS <> ? THEN
             ASSIGN p-tipo-def:SCREEN-VALUE = " ".
          IF p-cod-def:LIST-ITEMS <> ? THEN
             ASSIGN p-cod-def:SCREEN-VALUE = " ".
       END.
    END.
    
    /* Zerar as Quantidades Calculadas Automaticamente para as classes diferentes de RT */
    IF INPUT FRAME {&FRAME-NAME} cb-classe1 <> "RT" THEN
       ASSIGN fi-qtd-def1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
    IF INPUT FRAME {&FRAME-NAME} cb-classe2 <> "RT" THEN
       ASSIGN fi-qtd-def2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
    IF INPUT FRAME {&FRAME-NAME} cb-classe3 <> "RT" THEN
       ASSIGN fi-qtd-def3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
    IF INPUT FRAME {&FRAME-NAME} cb-classe4 <> "RT" THEN
       ASSIGN fi-qtd-def4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
    IF INPUT FRAME {&FRAME-NAME} cb-classe5 <> "RT" THEN
       ASSIGN fi-qtd-def5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
    IF INPUT FRAME {&FRAME-NAME} cb-classe6 <> "RT" THEN
       ASSIGN fi-qtd-def6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.
    
    /* Calcular novas quantidades para as Classes RG e LD */
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe1,"RG,LD") > 0 THEN
       ASSIGN fi-qtd-def1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} fi-quantidade / i-qt-def).
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe2,"RG,LD") > 0 THEN
       ASSIGN fi-qtd-def2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} fi-quantidade / i-qt-def).
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe3,"RG,LD") > 0 THEN
       ASSIGN fi-qtd-def3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} fi-quantidade / i-qt-def).
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe4,"RG,LD") > 0 THEN
       ASSIGN fi-qtd-def4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} fi-quantidade / i-qt-def).
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe5,"RG,LD") > 0 THEN
       ASSIGN fi-qtd-def5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} fi-quantidade / i-qt-def).
    IF LOOKUP(INPUT FRAME {&FRAME-NAME} cb-classe6,"RG,LD") > 0 THEN
       ASSIGN fi-qtd-def6:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INPUT FRAME {&FRAME-NAME} fi-quantidade / i-qt-def).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-del-nuance Dialog-Frame 
PROCEDURE pi-del-nuance :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-ind AS INT.

    DO i-ind = 1 TO EXTENT(mov-est-acbm.nuance-cla).
       IF mov-est-acbm.nuance-cla[i-ind] = ob-etiqueta.nuance THEN LEAVE.
    END.
    ASSIGN mov-est-acbm.nuance-qtd[i-ind] = mov-est-acbm.nuance-qtd[i-ind] - ob-etiqueta.quantidade.

    IF mov-est-acbm.nuance-qtd[i-ind] = 0 THEN
       ASSIGN mov-est-acbm.nuance-cla[i-ind] = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-defeitos Dialog-Frame 
PROCEDURE pi-grava-defeitos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-classif  AS CHAR FORMAT "!X".
    DEF INPUT PARAMETER p-tipo-def AS CHAR FORMAT "X".
    DEF INPUT PARAMETER p-cod-def  AS CHAR FORMAT "X(2)".
    DEF INPUT PARAMETER p-qtd-def  AS DEC FORMAT ">>9.9".

    IF p-classif <> "" THEN DO.
       CREATE mov-est-acbd.
       ASSIGN mov-est-acbd.cod-estabel  = mov-est-acbm.cod-estabel
              mov-est-acbd.data-mov     = mov-est-acbm.data-mov
              mov-est-acbd.num-lote     = mov-est-acbm.num-lote
              mov-est-acbd.num-etiqueta = ob-etiqueta.num-etiqueta
              mov-est-acbd.nr-carro     = ob-etiqueta.nr-carro
              mov-est-acbd.acondic      = ob-etiqueta.acondic
              mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia
              mov-est-acbd.it-codigo    = mov-est-acbm.it-codigo
              mov-est-acbd.cod-refer    = mov-est-acbm.cod-refer
              mov-est-acbd.num-revis    = IF SUBSTR(ob-etiqueta.resp-revisao,2,1) <= '9' 
                                          THEN INT(SUBSTR(ob-etiqueta.resp-revisao,2,LENGTH(ob-etiqueta.resp-revisao) - 1))
                                          ELSE 99
              mov-est-acbd.num-maq      = ob-etiqueta.nr-revisadeira
              mov-est-acbd.cod-acond    = ob-etiqueta.embalagem
              mov-est-acbd.num-acond    = ob-etiqueta.nr-sequencia
              mov-est-acbd.classif      = p-classif
              mov-est-acbd.cod-tipo-def = p-tipo-def
              mov-est-acbd.cod-defeito  = p-cod-def
              mov-est-acbd.qtd-defeit   = p-qtd-def.

       ASSIGN mov-est-acbm.qtd-tot-def = mov-est-acbm.qtd-tot-def + p-qtd-def.
       /*
       DO i-ct = 1 TO EXTENT(v-defeito).
          IF v-defeito[i-ct] = "" AND p-classif = "LD" THEN DO.
             ASSIGN v-defeito[i-ct] = p-tipo-def + "   " + p-cod-def.
             LEAVE.
          END.
       END.
       */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-nuance Dialog-Frame 
PROCEDURE pi-grava-nuance :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-ct AS INT.
    DEF VAR i-ind AS INT.

    ASSIGN i-ind = 0.
    DO i-ct = 1 TO EXTENT(mov-est-acbm.nuance-cla).
       IF mov-est-acbm.nuance-cla[i-ct] = ob-etiqueta.nuance THEN DO.
          ASSIGN i-ind = i-ct.
          LEAVE.
       END.
    END.

    IF i-ind = 0 THEN DO i-ct = 1 TO EXTENT(mov-est-acbm.nuance-qtd).
       IF mov-est-acbm.nuance-cla[i-ct] = "" THEN DO.
          ASSIGN mov-est-acbm.nuance-cla[i-ct] = ob-etiqueta.nuance
                 mov-est-acbm.nuance-qtd[i-ct] = ob-etiqueta.quantidade.
          LEAVE.
       END.
    END.
    ELSE 
       ASSIGN mov-est-acbm.nuance-qtd[i-ind] = mov-est-acbm.nuance-qtd[i-ind] + ob-etiqueta.quantidade.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-def Dialog-Frame 
PROCEDURE pi-monta-def :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER p-cod-def AS HANDLE.

   ASSIGN c-defeitos = "".
   FOR EACH defeito WHERE
            defeito.cod-tipo-def = SELF:SCREEN-VALUE NO-LOCK.
       ASSIGN c-defeitos = IF c-defeitos = ""
                           THEN defeito.cod-defeito
                           ELSE c-defeitos + "," + defeito.cod-defeito.
   END.
   ASSIGN c-defeitos = c-defeitos + "," + " ".
   ASSIGN p-cod-def:LIST-ITEMS = c-defeitos.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-monta-tpdef Dialog-Frame 
PROCEDURE pi-monta-tpdef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER p-tipo-def AS HANDLE.

   IF p-tipo-def:LIST-ITEMS = ? THEN DO.
      ASSIGN c-tipo-def = "".
      FOR EACH tipo-def NO-LOCK.
          ASSIGN c-tipo-def = IF c-tipo-def = ""
                              THEN tipo-def.cod-tipo-def
                              ELSE c-tipo-def + "," + tipo-def.cod-tipo-def.
      END.
      ASSIGN c-tipo-def = c-tipo-def + "," + " ".

      ASSIGN p-tipo-def:LIST-ITEMS = c-tipo-def.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra-def Dialog-Frame 
PROCEDURE pi-mostra-def :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-tipo-def AS CHAR.
    FIND defeito WHERE
         defeito.cod-tipo-def = p-tipo-def AND
         defeito.cod-defeito = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.

    IF AVAIL defeito THEN
       ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = defeito.descricao.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-mostra-tpdef Dialog-Frame 
PROCEDURE pi-mostra-tpdef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER p-tipo-def AS HANDLE.
   FIND tipo-def WHERE
        tipo-def.cod-tipo-def = p-tipo-def:SCREEN-VALUE NO-LOCK NO-ERROR.

   IF AVAIL tipo-def THEN
      ASSIGN fi-descricao:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tipo-def.descricao.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-vrf-defeitos Dialog-Frame 
PROCEDURE pi-vrf-defeitos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-classif  AS CHAR FORMAT "!X".
    DEF INPUT PARAMETER p-tipo-def AS HANDLE.
    DEF INPUT PARAMETER p-cod-def  AS HANDLE.

    IF p-classif <> "" THEN DO.
       IF qualid-tecido.class-qualid = 1 AND p-classif = "LD" THEN DO.
          MESSAGE "Lote PERFEITO n∆o pode conter Defeitos..." VIEW-AS ALERT-BOX.
          APPLY 'entry' TO cb-qualidade IN FRAME {&FRAME-NAME}.
          RETURN 'ADM-ERROR'.
       END.

       FIND tipo-def WHERE
            tipo-def.cod-tipo-def = p-tipo-def:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF NOT AVAIL tipo-def THEN DO.
          MESSAGE "Tipo de Defeito Inv†lido" VIEW-AS ALERT-BOX.
          APPLY 'entry' TO p-tipo-def.
          RETURN 'ADM-ERROR'.
       END.

       FIND defeito WHERE
            defeito.cod-tipo-def = p-tipo-def:SCREEN-VALUE AND
            defeito.cod-defeito = p-cod-def:SCREEN-VALUE NO-LOCK NO-ERROR.
       IF NOT AVAIL defeito THEN DO.
          MESSAGE "C¢digo Defeito Inv†lido" VIEW-AS ALERT-BOX.
          APPLY 'entry' TO p-cod-def.
          RETURN 'ADM-ERROR'.
       END.

       IF p-classif = "LD" THEN
          ASSIGN l-existe-ld = YES.

       IF p-classif <> "RT" THEN
          ASSIGN l-defeitos = YES.

       DO i-ct = 1 TO EXTENT(v-defeito).
          IF v-defeito[i-ct] = "" AND p-classif = "LD" THEN DO.
             ASSIGN v-defeito[i-ct] = defeito.cod-tipo-def + "   " + defeito.cod-defeito.
             LEAVE.
          END.
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

