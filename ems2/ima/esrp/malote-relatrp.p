/* PROGRAMA: RELAT-ALGODAORP.P                                               **
** DATA    : 09/05/2011                                                      **
** AUTOR   :Anderson Fagner                                                  **
** OBJETIVO: RELATàRIO DE MALOTES                                            **
******************************************************************************/

/* Programa de controle de versao e seguran»a do Datasul EMS */
{include/i-prgvrs.i malote-relatrp 2.04.00.001}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}
DEFINE BUFFER empresa FOR mgcad.empresa.
define temp-table tt-param no-undo
    field destino                 as integer
    field arquivo                 as char format "x(35)"
    field usuario                 as char format "x(12)"
    field data-exec               as date
    field hora-exec               as integer
    field classifica              as integer
    field desc-classifica         as char format "x(40)"
    field modelo-rtf              as char format "x(35)"
    field l-habilitaRtf           as LOG
    field fi-num-malote-ini       as int 
    field fi-num-malote-fim       as int 
    field fi-cod-estabel-orig-ini as int 
    field fi-cod-estabel-orig-fim as int 
    field fi-cod-estabel-dest-ini as int 
    field fi-cod-estabel-dest-fim as int 
    field fi-percurso-ini         as int 
    field fi-percurso-fim         as int 
    field fi-responsavel-ini      as char  
    FIELD fi-responsavel-fim      as char
    FIELD fi-estado-ini           AS CHAR
    FIELD fi-estado-fim           AS CHAR
    FIELD tg-imp-origem           AS LOG
    FIELD tg-qt-parcial           AS LOG.
    

DEFINE NEW SHARED TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem              AS INTEGER   FORMAT ">>>>9"
    FIELD nr-pedcli          AS CHAR
    FIELD nome-abrev         AS CHAR
    FIELD l-status           AS LOG.

DEFINE TEMP-TABLE tt-malote
    FIELD num-malote       as INT
    field descricao        as char
    field responsavel      as char
    field percurso         as INT
    field num-contrato     as DEC
    field mov-completo     as LOG
    FIELD cod-estabel-orig AS INT      
    field contato-orig     as char
    field cidade-orig      as char
    field estado-orig      as char
    FIELD cod-estabel-dest AS INT      
    field contato-dest     as char
    field cidade-dest      as char
    field estado-dest      as char.

DEFINE VARIABLE v-qt-total   AS INTEGER     NO-UNDO.
DEFINE VARIABLE v-qt-parcial AS INTEGER     NO-UNDO.

    
    /* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.   
                           
   
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

FOR EACH tt-raw-digita :
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

DEF VAR h-acomp     AS HANDLE  NO-UNDO.



{include/i-rpvar.i}   

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
ASSIGN c-programa     = "malote-relat.w":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "RELATàRIO DE MALOTES":U.              

FORM HEADER   
    "Malote"            at 001            
    "Descri‡Æo"         at 008
    "Responsavel"       at 041
    "Percurso"          at 056
    "Contrato"          at 065
    "M/C"               at 077 
    WITH FRAME f-cabec. 
/*
IF tt-param.tg-imp-origem THEN DO:
   FORM HEADER   
        "Origem"            at 081
        /*"Contato"           AT 085*/
        "Cidade"            AT 100
        "UF"                AT 121
        SKIP 
        WITH FRAME f-cabec. 
END.
*/
FORM HEADER   
    "Destino"           at 081
    /*"Contato"           AT 085*/
    "Cidade"            AT 100
    "UF"                AT 121
    SKIP
    FILL("-",132) FORMAT "x(132)"
    SKIP
    WITH FRAME f-cabec. 

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.
/*CASE tt-param.classifica:
    WHEN 1 THEN DO:
        &SCOPED-DEFINE sd-by BY malote.percurso         ~
                             BY malote.cod-estabel-dest ~
                             BY malote.num-malote
    END.
    WHEN 2 THEN DO:
        &SCOPED-DEFINE sd-by BY malote.responsavel      ~
                             BY malote.percurso         ~
                             BY malote.cod-estabel-dest ~
                             BY malote.num-malote       
    END.
    WHEN 3 THEN DO:
        &SCOPED-DEFINE sd-by BY malote.num-malote       ~
                             BY malote.percurso         ~
                             BY malote.cod-estabel-dest 
    END.
    WHEN 4 THEN DO:
        &SCOPED-DEFINE sd-by BY malote.cod-estabel-dest ~
                             BY malote.percurso         ~
                             BY malote.num-malote
    END.
END CASE.
*/
PROCEDURE pi-put.
    ASSIGN v-qt-total   = v-qt-total   + 1
           v-qt-parcial = v-qt-parcial + 1.

    PUT UNFORMAT tt-malote.num-malote          AT 003 FORMAT "999"
                 tt-malote.descricao           at 008 FORMAT "x(32)"
                 tt-malote.responsavel         at 041 FORMAT "x(15)"
                 tt-malote.percurso            at 057
                 tt-malote.num-contrato        at 065
                 tt-malote.mov-completo        at 077.
    IF tt-param.tg-imp-origem THEN DO:
       PUT UNFORMAT tt-malote.cod-estabel-orig AT 081
                 tt-malote.contato-orig     AT 085
                 tt-malote.cidade-orig      AT 100 FORMAT "x(20)"
                 tt-malote.estado-orig      AT 121
                 SKIP.

    END.
    PUT UNFORMAT tt-malote.cod-estabel-dest AT 081
                 tt-malote.contato-dest     AT 085
                 tt-malote.cidade-dest      AT 100 FORMAT "x(20)"
                 tt-malote.estado-dest      AT 121
                 SKIP.
    
END PROCEDURE.

PROCEDURE pi-last.

    PUT UNFORMAT FILL("_",120) FORMAT "x(132)" AT 03
                 SKIP
                 "Quantidade: " AT 03
                  v-qt-parcial
                 SKIP(1).
    ASSIGN v-qt-parcial = 0.

END PROCEDURE.

FOR EACH malote WHERE malote.cod-estabel-orig  >= tt-param.fi-cod-estabel-orig-ini
                  and malote.cod-estabel-orig  <= tt-param.fi-cod-estabel-orig-fim
                  AND malote.cod-estabel-dest  >= tt-param.fi-cod-estabel-dest-ini 
                  and malote.cod-estabel-dest  <= tt-param.fi-cod-estabel-dest-fim 
                  and malote.num-malote        >= tt-param.fi-num-malote-ini       
                  and malote.num-malote        <= tt-param.fi-num-malote-fim       
                  and malote.percurso          >= tt-param.fi-percurso-ini         
                  and malote.percurso          <= tt-param.fi-percurso-fim         
                  and malote.responsavel       >= tt-param.fi-responsavel-ini      
                  and malote.responsavel       <= tt-param.fi-responsavel-fim      
               NO-LOCK BREAK {&sd-by}.
    FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = malote.cod-estabel-dest
                             AND estabel-aux.estado >= tt-param.fi-estado-ini 
                             AND estabel-aux.estado <= tt-param.fi-estado-fim 
                           NO-LOCK NO-ERROR.
    IF NOT avail estabel-aux THEN NEXT.

    CREATE tt-malote.
    ASSIGN tt-malote.num-malote   = malote.num-malote  
           tt-malote.descricao    = malote.descricao   
           tt-malote.responsavel  = malote.responsavel 
           tt-malote.percurso     = malote.percurso    
           tt-malote.num-contrato = malote.num-contrato
           tt-malote.mov-completo = malote.mov-completo.

           
              
           

    /*
    ASSIGN v-qt-total   = v-qt-total   + 1
           v-qt-parcial = v-qt-parcial + 1.

    PUT UNFORMAT malote.num-malote          AT 003 FORMAT "999"
                 malote.descricao           at 008 FORMAT "x(32)"
                 malote.responsavel         at 041 FORMAT "x(15)"
                 malote.percurso            at 057
                 malote.num-contrato        at 065
                 malote.mov-completo        at 077.
    */
    IF tt-param.tg-imp-origem THEN DO:
        FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = malote.cod-estabel-orig NO-LOCK NO-ERROR.
        IF AVAIL estabel-aux THEN
           ASSIGN tt-malote.cod-estabel-orig = malote.cod-estabel-orig
                  tt-malote.contato-orig     = estabel-aux.contato
                  tt-malote.cidade-orig      = estabel-aux.cidade 
                  tt-malote.estado-orig      = estabel-aux.estado. 
        /*
           PUT UNFORMAT malote.cod-estabel-orig AT 081  
                        /*"-"                     AT 083 
                        estabel-aux.nome  */
                        estabel-aux.contato     AT 085
                        estabel-aux.cidade      AT 100 FORMAT "x(20)"
                        estabel-aux.estado      AT 121
                        SKIP.*/
    END.
    FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = malote.cod-estabel-dest NO-LOCK NO-ERROR.
    IF AVAIL estabel-aux THEN
       ASSIGN tt-malote.cod-estabel-dest = malote.cod-estabel-dest
              tt-malote.contato-dest     = estabel-aux.contato
              tt-malote.cidade-dest      = estabel-aux.cidade 
              tt-malote.estado-dest      = estabel-aux.estado.
/*
       PUT UNFORMAT malote.cod-estabel-dest AT 081
                    /*"-"                     AT 083 
                    estabel-aux.nome  */
                    estabel-aux.contato     AT 085
                    estabel-aux.cidade      AT 100 FORMAT "x(20)"
                    estabel-aux.estado      AT 121
                    SKIP.
                    /* malote.cod-barras-orig    at 000
                 malote.cod-barras-dest    at 000
                 SKIP(1).*/

    IF LAST-OF(malote.percurso) AND tg-qt-percurso THEN DO:
       PUT UNFORMAT FILL("_",120) FORMAT "x(132)" AT 03
                    SKIP
                    "Quantidade: " AT 03
                     v-qt-parcial
                    SKIP(1).
       ASSIGN v-qt-parcial = 0.
    END.
*/
END.
CASE tt-param.classifica:
    WHEN 1 THEN DO:
        FOR EACH tt-malote NO-LOCK BREAK BY tt-malote.percurso
                                         BY tt-malote.cod-estabel-dest
                                         BY tt-malote.num-malote.
            RUN pi-put.
            IF LAST-OF(tt-malote.percurso) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.
    WHEN 2 THEN DO:
        FOR EACH tt-malote NO-LOCK BREAK BY tt-malote.responsavel
                                         BY tt-malote.percurso
                                         BY tt-malote.cod-estabel-dest
                                         BY tt-malote.num-malote.
            RUN pi-put.
            IF LAST-OF(tt-malote.responsavel) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.                  
    WHEN 3 THEN DO:       
        FOR EACH tt-malote NO-LOCK BREAK BY tt-malote.num-malote
                                         BY tt-malote.percurso
                                         BY tt-malote.cod-estabel-dest.
            RUN pi-put.
            IF LAST-OF(tt-malote.num-malote) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.                  
    WHEN 4 THEN DO:       
        FOR EACH tt-malote NO-LOCK BREAK BY tt-malote.cod-estabel-dest
                                         BY tt-malote.percurso
                                         BY tt-malote.num-malote.
            RUN pi-put.
            IF LAST-OF(tt-malote.cod-estabel-dest) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.
    WHEN 5 THEN DO:       
        FOR EACH tt-malote NO-LOCK BREAK BY tt-malote.estado-dest
                                         BY tt-malote.cidade-dest
                                         BY tt-malote.percurso
                                         BY tt-malote.num-malote
                                         BY tt-malote.cod-estabel-dest.
                                         
            RUN pi-put.
            IF LAST-OF(tt-malote.estado-dest) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.
END CASE.

PUT UNFORMAT SKIP(1)
             FILL("-",132) FORMAT "x(132)"
             SKIP
             "Quantidade de Malotes: "
             v-qt-total.
    
    /* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.
