&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :relat¢rio saldo dos grupos correspondentes ao setor de compras

    Author(s)   :Tadeu Silva Parreiras
    Created     :24 de julho de 2007
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

 /* Programa de controle de versao e seguran»a do Datasul EMS */
{include/i-prgvrs.i rel002mcc 2.06.00.001}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

    DEF BUFFER empresa FOR mgadm.empresa.
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
        FIELD cod-estabel-ini  AS CHAR
        FIELD cod-estabel-fim  AS CHAR
        FIELD it-codigo-ini    AS CHAR
        FIELD it-codigo-fim    AS CHAR
        FIELD rs-ordenar       AS INT.
       



DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.    

/* Parametros de entrada logica obrigatoria */

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.   


CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEF VAR h-acomp AS HANDLE  NO-UNDO.
DEF VAR descricao AS CHAR  NO-UNDO.

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


/* ***************************  Main Block  *************************** */

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp (INPUT "Processando..").
/* ABERTURA DO ARQUIVO DE SAÖDA (ARQUIVO/IMPRESSORA) CORREPONDE A INCLUDE CDP/CD9520.I (MAGNUS) */
{include/i-rpout.i}
{include/i-rpcab.i}
    /* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
    FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
    IF NOT AVAIL empresa THEN
       RETURN "ADM-ERROR":U.

    /* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */
    ASSIGN c-programa     = "REL001MCC.W":U
           c-versao       = "2.04":U
           c-revisao      = ".00.001":U
           c-empresa      = empresa.razao-social
           c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "SALDO DE ESTOQUE - COMPRAS":U.              

FORM HEADER 
      
      "=========================================================================================================================" AT 1 SKIP 
      "ESTAB."     AT   1
      "DEP."       AT   8
      "ITEM"       AT   13
      "REFER"      AT   21
      "DESCRI€ÇO"  AT   29
      "UM."        AT   118
      "SALDO"      AT   127 SKIP 
      "========================================================================================================================="
      WITH FRAME F-CABEC WIDTH 132 NO-LABEL PAGE-TOP  NO-ATTR-SPACE . 
      
VIEW FRAME f-cabec.
VIEW FRAME f-rodape.
                                                                                                                                 
IF (rs-ordenar = 1) THEN DO:
    FOR EACH saldo-estoq WHERE
             saldo-estoq.cod-estabel >= tt-param.cod-estabel-ini AND
             saldo-estoq.cod-estabel <= tt-param.cod-estabel-fim AND
             saldo-estoq.it-codigo   >= tt-param.it-codigo-ini   AND
             saldo-estoq.it-codigo   <= tt-param.it-codigo-fim   AND 
             saldo-estoq.qtidade-atu > 0 NO-LOCK,
        EACH ITEM OF saldo-estoq WHERE /*(ITEM.ge-codigo = 40 
                                    OR ITEM.ge-codigo = 80 )
                                   AND*/ integer(ITEM.ge-codigo) = 40
                                   OR integer(ITEM.ge-codigo)   = 80
                                    NO-LOCK BY INT(saldo-estoq.it-codigo) 
                                            BY saldo-estoq.cod-estabel 
                                            BY saldo-estoq.cod-depos .

        RUN pi-acompanhar IN h-acomp (INPUT saldo-estoq.cod-estabel + '-' + saldo-estoq.it-codigo).
        
        IF saldo-estoq.cod-refer <> "" THEN DO:
           FIND referencia WHERE referencia.cod-refer = saldo-estoq.cod-refer NO-LOCK.
           IF AVAIL referencia THEN
               ASSIGN descricao =  ITEM.desc-item + " - " + referencia.descricao.
           ELSE
               ASSIGN descricao =  ITEM.desc-item.
        END.
        ELSE
           ASSIGN descricao =  ITEM.desc-item.
        
        PUT saldo-estoq.cod-estabel FORMAT "x(2)" AT 1
            saldo-estoq.cod-depos FORMAT "x(4)"   AT 8
            saldo-estoq.it-codigo FORMAT "x(6)"   AT 13
            saldo-estoq.cod-refer FORMAT "x(8)"   AT 21
            descricao FORMAT "x(60)"              AT 29
            ITEM.un FORMAT "x(3)"                 AT 118
            (saldo-estoq.qtidade-atu - saldo-estoq.qt-aloc-prod - saldo-estoq.qt-aloc-ped - saldo-estoq.qt-alocada) AT 122 SKIP.
    
    END.
END.
ELSE DO:

    FOR EACH saldo-estoq WHERE
         saldo-estoq.cod-estabel >= tt-param.cod-estabel-ini AND
         saldo-estoq.cod-estabel <= tt-param.cod-estabel-fim AND
         saldo-estoq.it-codigo   >= tt-param.it-codigo-ini   AND
         saldo-estoq.it-codigo   <= tt-param.it-codigo-fim   AND 
         saldo-estoq.qtidade-atu > 0 NO-LOCK,
    EACH ITEM OF saldo-estoq WHERE /*(ITEM.ge-codigo = 40 
                                OR ITEM.ge-codigo = 80 )
                                AND*/ integer(ITEM.ge-codigo) = 40
                                AND   integer(ITEM.ge-codigo) = 80
                                NO-LOCK BY ITEM.desc-item
                                        BY INT(saldo-estoq.it-codigo) 
                                        BY saldo-estoq.cod-estabel 
                                        BY saldo-estoq.cod-depos .

    IF saldo-estoq.cod-refer <> "" THEN DO:
           FIND referencia WHERE referencia.cod-refer = saldo-estoq.cod-refer NO-LOCK.
           IF AVAIL referencia THEN
               ASSIGN descricao =  ITEM.desc-item + " - " + referencia.descricao.
           ELSE
               ASSIGN descricao =  ITEM.desc-item.
        END.
        ELSE
           ASSIGN descricao =  ITEM.desc-item.
        
        PUT saldo-estoq.cod-estabel FORMAT "x(2)" AT 1
            saldo-estoq.cod-depos FORMAT "x(4)"   AT 8
            saldo-estoq.it-codigo FORMAT "x(6)"   AT 13
            saldo-estoq.cod-refer FORMAT "x(8)"   AT 21
            descricao FORMAT "x(60)"              AT 29
            ITEM.un FORMAT "x(3)"                 AT 118
            (saldo-estoq.qtidade-atu - saldo-estoq.qt-aloc-prod - saldo-estoq.qt-aloc-ped - saldo-estoq.qt-alocada) AT 122 SKIP.
    END.

END.




RUN pi-finalizar in h-acomp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


