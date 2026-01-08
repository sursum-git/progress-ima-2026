/* PROGRAMA: RELAT-ALGODAORP.P                                               **
** DATA    : 09/05/2011                                                      **
** AUTOR   :Anderson Fagner                                                  **
** OBJETIVO: RELATàRIO DE MALOTES                                            **
******************************************************************************/

/* Programa de controle de versao e seguran»a do Datasul EMS */
{include/i-prgvrs.i malote-mov-relatrp 2.04.00.001}

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
    FIELD fi-data-ini             AS DATE
    FIELD fi-data-fim             AS DATE
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
    FIELD tg-qt-parcial           AS LOG.
    
DEFINE NEW SHARED TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem              AS INTEGER   FORMAT ">>>>9"
    FIELD nr-pedcli          AS CHAR
    FIELD nome-abrev         AS CHAR
    FIELD l-status           AS LOG.

DEFINE NEW SHARED TEMP-TABLE tt-malote-mov
    field data-mov         as DATE
    field tipo-mov         as char
    field num-malote       as INT
    field hora-mov         as INT
    field minuto-mov       as INT 
    field usuario          as CHAR 
    field descricao        as char
    field responsavel      as char
    field percurso         as INT
    field cod-estabel-dest as INT
    field estabel-contato  as char
    field estabel-cidade   as char
    field estabel-estado   as char.
    
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



{include/i-rpvar.i}   

DEF VAR h-acomp     AS HANDLE  NO-UNDO.
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
ASSIGN c-programa     = "malote-mov-relat.w":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "RELATàRIO DE MOVIMENTA€ÇO DOS MALOTES":U.              

FORM HEADER
    /*"Destino"         AT 110
    SKIP
    FILL("-",48)     AT 085 FORMAT "x(48)"
    SKIP*/
    "Data"            AT 007           
    "Tipo"            at 019
    "Usu rio"         at 027
    "Malote"          AT 037
    "Resp."           at 067
    "Perc."           at 078
    "Cod"             at 085
    "Contato"         at 089
    "Cidade"          AT 105
    "UF"              at 130
    SKIP
    FILL("-",132)     AT 001 FORMAT "x(132)"
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

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

PROCEDURE pi-put.

    ASSIGN v-qt-total   = v-qt-total   + 1
           v-qt-parcial = v-qt-parcial + 1.

    PUT UNFORMAT tt-malote-mov.data-mov          AT 001 FORMAT "99/99/9999"
                 tt-malote-mov.hora-mov          at 012 
                 ":"                             AT 014
                 tt-malote-mov.minuto-mov        AT 015 
                 tt-malote-mov.tipo-mov          at 019 
                 tt-malote-mov.usuario           at 027 FORMAT "x(10)"
                 tt-malote-mov.num-malote        AT 037 FORMAT "999"
                 tt-malote-mov.descricao         at 041 FORMAT "x(30)"
                 tt-malote-mov.responsavel       at 073 FORMAT "x(16)"
                 tt-malote-mov.cod-estabel-dest  at 094
                 tt-malote-mov.percurso          at 098
                 /*tt-malote-mov.estabel-contato   at 089 FORMAT "x(16)"*/
                 tt-malote-mov.estabel-cidade    at 105 FORMAT "x(20)"
                 tt-malote-mov.estabel-estado    at 130
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

FOR EACH  malote-mov WHERE malote-mov.data-mov >= tt-param.fi-data-ini
                       AND malote-mov.data-mov <= tt-param.fi-data-fim
                       and malote-mov.num-malote   >= tt-param.fi-num-malote-ini       
                       and malote-mov.num-malote   <= tt-param.fi-num-malote-fim
                     NO-LOCK,
    EACH malote WHERE malote-mov.num-malote    =  malote.num-malote
                  AND malote.cod-estabel-orig  >= tt-param.fi-cod-estabel-orig-ini
                  and malote.cod-estabel-orig  <= tt-param.fi-cod-estabel-orig-fim
                  AND malote.cod-estabel-dest  >= tt-param.fi-cod-estabel-dest-ini 
                  and malote.cod-estabel-dest  <= tt-param.fi-cod-estabel-dest-fim 
                  and malote.percurso          >= tt-param.fi-percurso-ini         
                  and malote.percurso          <= tt-param.fi-percurso-fim         
                  and malote.responsavel       >= tt-param.fi-responsavel-ini      
                  and malote.responsavel       <= tt-param.fi-responsavel-fim      
               NO-LOCK.

    FIND FIRST estabel-aux WHERE estabel-aux.cod-estabel-aux = malote.cod-estabel-dest
                             AND estabel-aux.estado >= tt-param.fi-estado-ini 
                             AND estabel-aux.estado <= tt-param.fi-estado-fim 
                           NO-LOCK NO-ERROR.

    IF NOT avail estabel-aux THEN NEXT.

    CREATE tt-malote-mov.
    ASSIGN tt-malote-mov.data-mov         = malote-mov.data-mov
           tt-malote-mov.tipo-mov         = IF malote-mov.tipo-mov THEN "Entrada" ELSE "Saida"
           tt-malote-mov.hora-mov         = malote-mov.hora-mov   
           tt-malote-mov.minuto-mov       = malote-mov.minuto-mov 
           tt-malote-mov.usuario          = malote-mov.usuario
           tt-malote-mov.num-malote       = malote.num-malote      
           tt-malote-mov.descricao        = malote.descricao       
           tt-malote-mov.responsavel      = malote.responsavel     
           tt-malote-mov.percurso         = malote.percurso
           tt-malote-mov.cod-estabel-dest = malote.cod-estabel-dest
           tt-malote-mov.estabel-contato  = estabel-aux.contato    
           tt-malote-mov.estabel-cidade   = estabel-aux.cidade     
           tt-malote-mov.estabel-estado   = estabel-aux.estado.
END.
    /*{esrp/malote-mov-relatimp.p (INPUT tt-param.tg-qt-parcial) VALUE(var-by)}*/

CASE tt-param.classifica:
    WHEN 1 THEN DO:
        FOR EACH tt-malote-mov NO-LOCK BREAK BY tt-malote-mov.data-mov
                                             BY tt-malote-mov.hora-mov      
                                             BY tt-malote-mov.minuto-mov
                                             BY tt-malote-mov.percurso        
                                             BY tt-malote-mov.cod-estabel-dest
                                             BY tt-malote-mov.num-malote      
                                             BY tt-malote-mov.tipo-mov.
            RUN pi-put.
            IF LAST-OF(tt-malote-mov.data-mov) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.
    WHEN 2 THEN DO:
        FOR EACH tt-malote-mov NO-LOCK BREAK BY tt-malote-mov.responsavel
                                             BY tt-malote-mov.percurso        
                                             BY tt-malote-mov.cod-estabel-dest
                                             BY tt-malote-mov.num-malote      
                                             BY tt-malote-mov.data-mov
                                             BY tt-malote-mov.hora-mov      
                                             BY tt-malote-mov.minuto-mov.
            RUN pi-put.
            IF LAST-OF(tt-malote-mov.responsavel) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.
    WHEN 3 THEN DO:
        FOR EACH tt-malote-mov NO-LOCK BREAK BY tt-malote-mov.num-malote
                                             BY tt-malote-mov.data-mov
                                             BY tt-malote-mov.hora-mov      
                                             BY tt-malote-mov.minuto-mov
                                             BY tt-malote-mov.percurso         
                                             BY tt-malote-mov.cod-estabel-dest.
            RUN pi-put.
            IF LAST-OF(tt-malote-mov.num-malote) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.                  
    WHEN 4 THEN DO:       
        FOR EACH tt-malote-mov NO-LOCK BREAK BY tt-malote-mov.cod-estabel-dest
                                             BY tt-malote-mov.percurso   
                                             BY tt-malote-mov.num-malote 
                                             BY tt-malote-mov.data-mov
                                             BY tt-malote-mov.hora-mov
                                             BY tt-malote-mov.minuto-mov
                                             BY tt-malote-mov.tipo-mov.
            RUN pi-put.
            IF LAST-OF(tt-malote-mov.cod-estabel-dest) AND tt-param.tg-qt-parcial THEN DO:
               RUN pi-last.
            END.
        END.
    END.
END CASE.

PUT UNFORMAT SKIP(1)
             FILL("-",132) FORMAT "x(132)"
             SKIP
             "Quantidade de Movimentos dos Malotes: "
             v-qt-total.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.
