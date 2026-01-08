&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* PROGRAMA: ADR002BRP.P                                                    **
** DATA    : 13/juLho/2007                                                  **
** AUTOR   : Tadeu Silva / Eduardo Magno / M rcio Teixeira                  **
** OBJETIVO: Listagem de Clientes ativos/inativos por per¡odo               **
******************************************************************************/

/* Programa de controle de versao e seguran»a do Datasul EMS */
{include/i-prgvrs.i adr002b 2.04.00.001}

DEF BUFFER empresa FOR mgcad.empresa.

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}


/* ***************************  Definitions  ************************** */

/* Excel */
DEFINE VARIABLE chExcelApp   AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook   AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE coluna       AS INTEGER    NO-UNDO.
DEFINE VARIABLE i-linha      AS INTEGER    NO-UNDO.
DEFINE VARIABLE total-repres AS INTEGER    NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE salvar-como AS CHARACTER  NO-UNDO.

&IF DEFINED(EXCLUDE-fn-letra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-letra Procedure 
FUNCTION fn-letra RETURNS CHARACTER
  ( coluna AS INT )  FORWARD.

&ANALYZE-RESUME

&ENDIF

/******************** Acompanhar ***********************************/
def var h-acomp as handle no-undo.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD cod-emitente-ini LIKE emitente.cod-emitente
    FIELD cod-emitente-fim LIKE emitente.cod-emitente
    FIELD nome-abrev-ini   LIKE emitente.nome-abrev
    FIELD nome-abrev-fim   LIKE emitente.nome-abrev
    FIELD cod-rep-ini      LIKE emitente.cod-rep
    FIELD cod-rep-fim      LIKE emitente.cod-rep
    FIELD nome-ab-rep-ini  LIKE repres.nome-abrev
    FIELD nome-ab-rep-fim  LIKE repres.nome-abrev
    FIELD cidade-ini       LIKE emitente.cidade
    FIELD cidade-fim       LIKE emitente.cidade
    FIELD uf-ini           LIKE emitente.estado
    FIELD uf-fim           LIKE emitente.estado
    FIELD cod-ramo-ini     LIKE ext-emitente.cod-ramo-ativ
    FIELD cod-ramo-fim     LIKE ext-emitente.cod-ramo-ativ
    FIELD rs-dias          AS INT
    FIELD dt-ini           AS DATE
    FIELD dt-fim           AS DATE
    FIELD ativo            AS LOG
    FIELD inativo          AS LOG
    FIELD semcompra        AS LOG
    FIELD ordenar          AS INT
    FIELD tp-relatorio     AS INTEGER.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.    
 /* Parametros de entrada logica obrigatoria */

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.   
/*variaveis locais*/
DEF VAR tel2        AS CHAR.
DEF VAR telfax      AS CHAR.
DEF VAR telefones   AS CHAR.
DEF VAR sub-titulo  AS CHAR.
DEF VAR tit-rep     AS CHAR FORMAT "x(50)".

DEF TEMP-TABLE tt-cli  NO-UNDO
    FIELD cod-emitente AS INT  FORMAT "<<<<9"
    FIELD nome-emit    AS CHAR FORMAT "x(30)"
    FIELD endereco     AS CHAR FORMAT "x(40)"
    FIELD bairro       AS CHAR FORMAT "x(30)"
    FIELD cidade       AS CHAR FORMAT "x(25)"
    FIELD estado       AS CHAR FORMAT "x(02)"
    FIELD nome-ab-rep  AS CHAR FORMAT "x(12)"
    FIELD c-status     AS CHAR
    FIELD tel1         AS CHAR
    FIELD tel2         AS CHAR
    FIELD i-dias       AS INT
    INDEX rep IS PRIMARY c-status nome-ab-rep i-dias  .

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{include/i-rpvar.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 

{include/i-rpout.i}
{include/i-rpcab.i}
    /* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
    FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
    IF NOT AVAIL empresa THEN
       RETURN "ADM-ERROR":U.

    /* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */
    ASSIGN c-programa     = "ADR002B.W":U
           c-versao       = "2.04":U
           c-revisao      = ".00.001":U
           c-empresa      = empresa.razao-social
           c-sistema      = "EMS":U.

/*cria‡Æo temp-table de bancos*/

ASSIGN c-titulo-relat = "Listagem de Clientes Ativos/Inativos por per¡odo ":U.              
IF tt-param.semcompra = YES THEN ASSIGN sub-titulo = "Clientes sem registro de compra no Sistema Datasul".
ELSE IF ativo = YES AND inativo = YES THEN ASSIGN sub-titulo = "Clientes Ativos e Inativos".
ELSE IF ativo = YES AND inativo = NO  THEN ASSIGN sub-titulo = "Clientes Ativos".
ELSE IF ativo = NO  AND inativo = YES THEN ASSIGN sub-titulo = "Clientes Inativos".

VIEW FRAME f-rodape.

RUN esapi/connect-ima-med.p.
IF tt-param.tp-relatorio = 2 THEN DO.
    RUN esrp/adr002b1rp.p (INPUT TABLE tt-param).
    IF CONNECTED("dbaux") THEN
       DISCONNECT dbaux.

    RUN pi-finalizar in h-acomp.
    RETURN 'OK'.
END.

// Se chegar aqui ‚ porque selecionou Clientes

RUN esrp/adr002b2rp.p (INPUT TABLE tt-param,
                       OUTPUT TABLE tt-cli).

IF CONNECTED("dbaux") THEN
    DISCONNECT dbaux.

/*FORM HEADER sub-titulo WITH FRAME f-cabec.*/
FORM HEADER 
     "COD"         AT 001 
     "NOME ABREV"  AT 007
     "ENDERE€O"    AT 038
     "BAIRRO"      AT 079
     "CIDADE"      AT 100
     "UF"          AT 121
     "TELEFONE"    AT 124 
     "DIAS"        AT 140
     SKIP
     FILL("=",144) FORMAT "x(144)" AT 1
     WITH FRAME f-cabec WIDTH 172 NO-LABEL PAGE-TOP  NO-ATTR-SPACE.
VIEW FRAME f-cabec.

ASSIGN i-linha = 1.

IF (tt-param.ordenar = 1) THEN DO:
    FOR EACH tt-cli
        BREAK BY tt-cli.c-status
              BY tt-cli.nome-ab-rep
              BY tt-cli.i-dias:

        FIND emitente WHERE
             emitente.cod-emit = tt-cli.cod-emit NO-LOCK NO-ERROR.

        IF tt-param.semcompra = YES AND tt-cli.c-status  <> "Sem Compra" THEN NEXT.
        IF tt-param.ativo     = NO  AND tt-cli.c-status  = "Ativo" THEN NEXT.
        IF tt-param.inativo   = NO  AND tt-cli.c-status  = "Inativo" THEN NEXT.
        IF FIRST-OF(tt-cli.nome-ab-rep) THEN DO:
           FIND repres WHERE
                repres.nome-abrev = tt-cli.nome-ab-rep
                NO-LOCK NO-ERROR.
            /*PUT sub-titulo.*/
            RUN pi-imprime-repres.
        END.
        RUN pi-imprime.
        IF LAST-OF(tt-cli.nome-ab-rep) THEN DO:
           RUN pi-imprime-total-repres.
           PAGE.
        END.
    END.
END.
ELSE DO:
   IF (tt-param.ordenar = 2) THEN DO:
        FOR EACH tt-cli BREAK  BY tt-cli.c-status BY tt-cli.nome-ab-rep BY tt-cli.cod-emitente BY tt-cli.i-dias:
            FIND emitente WHERE
                 emitente.cod-emit = tt-cli.cod-emit NO-LOCK NO-ERROR.

            IF tt-param.semcompra = YES AND tt-cli.c-status  <> "Sem Compra" THEN NEXT.
            IF tt-param.ativo     = NO  AND tt-cli.c-status  = "Ativo" THEN NEXT.
            IF tt-param.inativo   = NO  AND tt-cli.c-status  = "Inativo" THEN NEXT.
            IF FIRST-OF(tt-cli.nome-ab-rep) THEN DO:
               FIND repres WHERE repres.nome-abrev = tt-cli.nome-ab-rep NO-LOCK NO-ERROR.
               /*PUT sub-titulo.*/
               RUN pi-imprime-repres.
            END.
            RUN pi-imprime.
            IF LAST-OF(tt-cli.nome-ab-rep) THEN DO:
               RUN pi-imprime-total-repres.
               PAGE.
            END.
        END.
    END.
    ELSE DO:
        IF (tt-param.ordenar = 3) THEN DO:
            FOR EACH tt-cli
                BREAK BY tt-cli.c-status 
                      BY tt-cli.nome-ab-rep 
                      BY tt-cli.nome-emit 
                      BY tt-cli.i-dias:

                FIND emitente WHERE
                     emitente.cod-emit = tt-cli.cod-emit NO-LOCK NO-ERROR.

                IF tt-param.semcompra = YES AND tt-cli.c-status  <> "Sem Compra" THEN NEXT.
                IF tt-param.ativo     = NO  AND tt-cli.c-status  = "Ativo" THEN NEXT.
                IF tt-param.inativo   = NO  AND tt-cli.c-status  = "Inativo" THEN NEXT.
                IF FIRST-OF(tt-cli.nome-ab-rep) THEN DO:
                     FIND repres WHERE repres.nome-abrev = tt-cli.nome-ab-rep NO-LOCK NO-ERROR.
                    /*PUT sub-titulo.*/
                  RUN pi-imprime-repres.
                  END.
                  RUN pi-imprime.
                  IF LAST-OF(tt-cli.nome-ab-rep) THEN DO:
                     RUN pi-imprime-total-repres.
                     PAGE.
                  END.
            END.
        END.
        ELSE DO:
            IF (tt-param.ordenar = 4) THEN DO:
                FOR EACH tt-cli BREAK  BY tt-cli.c-status BY tt-cli.nome-ab-rep BY tt-cli.cidade BY tt-cli.i-dias:

                    FIND emitente WHERE
                         emitente.cod-emit = tt-cli.cod-emit NO-LOCK NO-ERROR.

                    IF tt-param.semcompra = YES AND tt-cli.c-status  <> "Sem Compra" THEN NEXT.
                    IF tt-param.ativo     = NO  AND tt-cli.c-status  = "Ativo" THEN NEXT.
                    IF tt-param.inativo   = NO  AND tt-cli.c-status  = "Inativo" THEN NEXT.
                    IF FIRST-OF(tt-cli.nome-ab-rep) THEN DO:
                       FIND repres WHERE repres.nome-abrev = tt-cli.nome-ab-rep NO-LOCK NO-ERROR.
                        /*PUT sub-titulo.*/
                       RUN pi-imprime-repres.
                    END.
                    RUN pi-imprime.
                    IF LAST-OF(tt-cli.nome-ab-rep) THEN DO:
                       RUN pi-imprime-total-repres.
                       PAGE.
                    END.
                       
                END.
            END.
            ELSE DO:
                IF (tt-param.ordenar = 5) THEN DO:
                    CREATE "Excel.Application" chExcelApp.
                           /*chWorkBook = chExcelApp:Workbooks:Add("M:\Ems204\especificos\modelo-xlt\mala_direta.xlt").*/
                           chWorkBook = chExcelApp:Workbooks:Add().
                           chWorkSheet = chExcelApp:Sheets:Item(1).
                /*           chexcelapp:VISIBLE = TRUE.*/
                           chWorkSheet:Range(fn-letra(1) + STRING(i-linha)):VALUE = "Codigo".
                           chWorkSheet:Range(fn-letra(2) + STRING(i-linha)):VALUE = "Nome Abrev".
                           chWorkSheet:Range(fn-letra(3) + STRING(i-linha)):VALUE = "Endere‡o".
                           chWorkSheet:Range(fn-letra(4) + STRING(i-linha)):VALUE = "Cidade".
                           chWorkSheet:Range(fn-letra(5) + STRING(i-linha)):VALUE = "Bairro".
                           chWorkSheet:Range(fn-letra(6) + STRING(i-linha)):VALUE = "UF".
                           chWorkSheet:Range(fn-letra(7) + STRING(i-linha)):VALUE = "Telefone".
                           chWorkSheet:Range(fn-letra(8) + STRING(i-linha)):VALUE = "Dias".
                           chWorkSheet:Range(fn-letra(9) + STRING(i-linha)):VALUE = "Repres".
                           chWorkSheet:Range(fn-letra(10) + STRING(i-linha)):VALUE = "DtImplant".
                           ASSIGN i-linha = i-linha + 1.
                    
                    FOR EACH tt-cli BREAK BY tt-cli.c-status BY tt-cli.nome-ab-rep BY tt-cli.i-dias:

                        FIND emitente WHERE
                             emitente.cod-emit = tt-cli.cod-emit NO-LOCK NO-ERROR.

                        IF tt-param.semcompra = YES AND tt-cli.c-status  <> "Sem Compra" THEN NEXT.
                        IF tt-param.ativo     = NO  AND tt-cli.c-status  = "Ativo" THEN NEXT.
                        IF tt-param.inativo   = NO  AND tt-cli.c-status  = "Inativo" THEN NEXT.
                        IF  FIRST-OF(tt-cli.nome-ab-rep) THEN DO:
                            FIND repres WHERE repres.nome-abrev = tt-cli.nome-ab-rep NO-LOCK NO-ERROR.
                            /*PUT sub-titulo.*/
                            RUN pi-imprime-repres.
                        END.
                
                        RUN  pi-acompanhar IN  h-acomp (INPUT "Gerando xls - " + STRING(i-linha) + '-' + STRING(tt-cli.nome-emit)).
                
                        chWorkSheet:Range(fn-letra(1) + STRING(i-linha)):VALUE = STRING (tt-cli.cod-emitente).
                        chWorkSheet:Range(fn-letra(2) + STRING(i-linha)):VALUE = tt-cli.nome-emit.
                        chWorkSheet:Range(fn-letra(3) + STRING(i-linha)):VALUE = tt-cli.endereco.
                        chWorkSheet:Range(fn-letra(4) + STRING(i-linha)):VALUE = tt-cli.cidade.
                        chWorkSheet:Range(fn-letra(5) + STRING(i-linha)):VALUE = tt-cli.bairro.
                        chWorkSheet:Range(fn-letra(6) + STRING(i-linha)):VALUE = tt-cli.estado.
                        chWorkSheet:Range(fn-letra(7) + STRING(i-linha)):VALUE = tt-cli.tel1.
                        chWorkSheet:Range(fn-letra(8) + STRING(i-linha)):VALUE = tt-cli.i-dias.
                        chWorkSheet:Range(fn-letra(9) + STRING(i-linha)):VALUE = tt-cli.nome-ab-rep.
                        chWorkSheet:Range(fn-letra(10) + STRING(i-linha)):VALUE = emitente.data-implant.
                
                        ASSIGN i-linha = i-linha + 1.
                        RUN pi-imprime.
                
                        IF LAST-OF(tt-cli.nome-ab-rep) THEN PAGE.
                           
                    END.
                    chexcelapp:VISIBLE = TRUE.
                    RELEASE OBJECT chWorkSheet.
                    /*chWorkBook:Save().        
                    chWorkBook:Close().  
                    chExcelApp:Quit().   */
                    RELEASE OBJECT chWorkBook.
                    RELEASE OBJECT chExcelApp.
                END.
            END.
        END.
    END.
END.
run pi-finalizar in h-acomp.

/* ************************  Function Implementations ***************** */
&IF DEFINED(EXCLUDE-fn-letra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-letra Procedure 
FUNCTION fn-letra RETURNS CHARACTER
  ( coluna AS INT ) :
DEF VAR col-letra AS CHAR.
CASE coluna:
    WHEN 1  THEN ASSIGN col-letra = 'A'.
    WHEN 2  THEN ASSIGN col-letra = 'B'.
    WHEN 3  THEN ASSIGN col-letra = 'C'.
    WHEN 4  THEN ASSIGN col-letra = 'D'.
    WHEN 5  THEN ASSIGN col-letra = 'E'.
    WHEN 6  THEN ASSIGN col-letra = 'F'. 
    WHEN 7  THEN ASSIGN col-letra = 'G'. 
    WHEN 8  THEN ASSIGN col-letra = 'H'. 
    WHEN 9  THEN ASSIGN col-letra = 'I'. 
    WHEN 10 THEN ASSIGN col-letra = 'J'. 
    WHEN 11 THEN ASSIGN col-letra = 'K'. 
    WHEN 12 THEN ASSIGN col-letra = 'L'. 
    WHEN 13 THEN ASSIGN col-letra = 'M'. 
    WHEN 14 THEN ASSIGN col-letra = 'N'. 
    WHEN 15 THEN ASSIGN col-letra = 'O'. 
    WHEN 16 THEN ASSIGN col-letra = 'P'. 
    WHEN 17 THEN ASSIGN col-letra = 'Q'. 
    WHEN 18 THEN ASSIGN col-letra = 'R'. 
    WHEN 19 THEN ASSIGN col-letra = 'S'. 
    WHEN 20 THEN ASSIGN col-letra = 'T'. 
    WHEN 21 THEN ASSIGN col-letra = 'U'.
    WHEN 22 THEN ASSIGN col-letra = 'V'. 
    WHEN 23 THEN ASSIGN col-letra = 'W'. 
    WHEN 24 THEN ASSIGN col-letra = 'X'. 
    WHEN 25 THEN ASSIGN col-letra = 'Y'. 

END CASE.

  RETURN col-letra.   

END FUNCTION.


&ANALYZE-RESUME

&ENDIF

/* ************************  Procedure Implementations ***************** */
PROCEDURE pi-imprime :
  PUT tt-cli.cod-emitente AT 001 FORMAT "99999"
      tt-cli.nome-emit    AT 007 FORMAT "x(30)"
      tt-cli.endereco     AT 038 FORMAT "x(40)"
      tt-cli.bairro       AT 079 FORMAT "x(20)"
      tt-cli.cidade       AT 100 FORMAT "x(20)"
      tt-cli.estado       AT 121 FORMAT "x(02)"
      tt-cli.tel1         AT 124 FORMAT "x(15)"
      emitente.data-implant AT 140 FORMAT "99/99/9999"
      tt-cli.i-dias       AT 151 FORMAT "ZZZ9".
  ASSIGN total-repres = total-repres + 1.
END PROCEDURE.

PROCEDURE pi-imprime-repres:
  IF AVAIL repres THEN
     FORM HEADER SKIP
     "Status: " tt-cli.c-status    SKIP
     "Repres: " repres.cod-rep " - " tt-cli.nome-ab-rep SKIP
     FILL("-",140) FORMAT "x(140)" AT 1
     WITH FRAME f-cabec WIDTH 172 NO-LABEL PAGE-TOP  NO-ATTR-SPACE.
     ASSIGN total-repres = 0.
END PROCEDURE.

PROCEDURE pi-imprime-total-repres:
    PUT SKIP(1)
        "TOTAL" total-repres FORMAT "zzzz9"
        " REGISTROS DO REPRESENTANTE: " repres.cod-rep " - " tt-cli.nome-ab-rep.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
