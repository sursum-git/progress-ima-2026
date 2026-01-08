/* Programa: ESCR0015.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Receber
** Objetivo: Listar Comissäes sobre Cheques contra Duplicatas
** Autor...: Gilvando Souza Araujo - Julho/2005
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCR0015RP 2.04.00.000}

define temp-table tt-param   no-undo
       field destino         as integer
       field arquivo         as char format "x(35)"
       field usuario         as char format "x(12)"
       field data-exec       as date
       field hora-exec       as integer
       FIELD ep-codigo       LIKE cheque.ep-codigo-mvto
       FIELD cod-estabel-ini LIKE cheque.cod-estabel-mvto
       FIELD cod-estabel-fin LIKE cheque.cod-estabel-mvto
       FIELD cod-rep-ini     LIKE titulo.cod-rep
       FIELD cod-rep-fin     LIKE titulo.cod-rep
       FIELD dt-deposito-ini LIKE cheque.dt-deposito
       FIELD dt-deposito-fin LIKE cheque.dt-deposito
       FIELD salta-pagina    AS LOGICAL FORMAT "Sim/NÆo"
       FIELD impr-param      AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def buffer b-cheque for cheque.

DEFINE TEMP-TABLE tt-work
       FIELD ep-codigo-mvto    LIKE cheque.ep-codigo-mvto  
       FIELD cod-estabel-mvto  LIKE cheque.cod-estabel-mvto
       FIELD cod-esp-mvto      LIKE cheque.cod-esp-mvto    
       FIELD serie-mvto        LIKE cheque.serie-mvto      
       FIELD nr-docto-mvto     LIKE cheque.nr-docto-mvto   
       FIELD parcela-mvto      LIKE cheque.parcela-mvto    
       INDEX ch-work ep-codigo-mvto   
                     cod-estabel-mvto
                     cod-esp-mvto    
                     serie-mvto      
                     nr-docto-mvto   
                     parcela-mvto.    

DEF VAR de-comis-emi    AS DEC.
DEF VAR de-comis-tot    AS DEC.
DEF VAR de-vlr-devol    AS DEC.
DEF VAR de-comissao     AS DEC.
DEF VAR de-imp-renda    AS DEC.
DEF VAR de-val-liq      AS DEC.
DEF VAR l-pagar-com     AS LOG.
DEF VAR de-tot-tit-rep  AS DEC.
DEF VAR de-tot-tit-ger  AS DEC.
DEF VAR de-tot-dev-rep  AS DEC.
DEF VAR de-tot-dev-ger  AS DEC.
DEF VAR de-tot-com-rep  AS DEC.
DEF VAR de-tot-com-ger  AS DEC.
DEF VAR de-tot-ir-rep   AS DEC.
DEF VAR de-tot-ir-ger   AS DEC.
DEF VAR de-tot-liq-rep  AS DEC.
DEF VAR de-tot-liq-ger  AS DEC.
DEF VAR i-cont-dep      AS INT.
DEF VAR de-sld-titulo   LIKE titulo.vl-original.

form
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.ep-codigo        LABEL "Empresa........" AT 1
    tt-param.cod-estabel-ini  label "Estabelecimento" AT 1
    "a"  AT 29
    tt-param.cod-estabel-fin  NO-LABELS
    tt-param.cod-rep-ini      LABEL "Representante.." AT 1
    "a"  AT 29
    tt-param.cod-rep-fin      NO-LABELS
    tt-param.dt-deposito-ini  label "Data Dep¢sito.." AT 1
    "a"  AT 29
    tt-param.dt-deposito-fin  NO-LABELS
    tt-param.salta-pagina     LABEL "Saltar P gina.." AT 1 SKIP(1)
    with no-box side-labels width 132 stream-io frame f-param.


form header
    "Cliente      Esp Nr.Doct Pa   Dt.Vencto   Vlr.Titulo Bco Agenc   Cheque   Vlr.Cheque    Vlr.Devol Sld.Comiss  Imp.Renda  Vlr.Liquido"
    skip
    "------------ --- ------- --  ---------- ------------ --- ----- -------- ------------ ------------ ---------- ---------- ------------"
    with no-labels no-attr-space no-box page-top width 132 STREAM-IO 1 DOWN frame f-cab-dados.

form
    titulo.nome-abrev   
    titulo.cod-esp         FORMAT "x(3)"
    titulo.nr-docto        FORMAT "x(8)"
    titulo.parcela         FORMAT "x(2)"
    titulo.dt-vencimen  
    titulo.vl-original     FORMAT ">,>>>,>>9.99"
    cheque.cod-banco       FORMAT "999"
    cheque.agencia         FORMAT "x(4)"
    cheque.nr-cheque    
    cheque.vl-cheque       FORMAT ">,>>>,>>9.99"
    de-vlr-devol            FORMAT "->>>>,>>9.99"
    de-comissao            FORMAT "->>,>>9.99"
    de-imp-renda        
    de-val-liq             FORMAT "->>>>,>>9.99"
    WITH NO-BOX NO-LABEL 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe.

FORM
    "Representante: " 
    repres.cod-rep " - " 
    repres.nome
    WITH NO-LABEL 2 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-repres.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FINANCAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Comissäes_sobre_Cheques_contra_Duplicatas * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cab-dados.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH cheque USE-INDEX movto 
    WHERE cheque.ep-codigo-mvto   =  tt-param.ep-codigo
      AND cheque.cod-estabel-mvto >= tt-param.cod-estabel-ini
      AND cheque.cod-estabel-mvto <= tt-param.cod-estabel-fin
      AND cheque.cod-esp-mvto     =  "dp"
    NO-LOCK:

    run pi-acompanhar in h-acomp (input "Cheque: " + string(cheque.nr-cheque) + 
                                        " - Agˆncia: " + cheque.agencia).
    
    /*- Verifica a data do 1§ dep¢sito do cheque -*/
    FIND FIRST movto-cheque USE-INDEX num-id-cheque
         WHERE movto-cheque.num-id-cheque = cheque.num-id-cheque
           AND movto-cheque.transacao     = 6 /* Dep¢sito */
        NO-LOCK NO-ERROR.
    IF NOT AVAIL movto-cheque THEN
       NEXT.
    ELSE
       IF movto-cheque.dt-movto > tt-param.dt-deposito-fin THEN
          NEXT.

    FIND titulo WHERE titulo.ep-codigo   = cheque.ep-codigo-mvto  
                  AND titulo.cod-estabel = cheque.cod-estabel-mvto
                  AND titulo.cod-esp     = cheque.cod-esp-mvto    
                  AND titulo.serie       = cheque.serie-mvto      
                  AND titulo.nr-docto    = cheque.nr-docto-mvto   
                  AND titulo.parcela     = cheque.parcela-mvto
                NO-LOCK NO-ERROR.
    ASSIGN de-sld-titulo = 0.
    IF AVAIL titulo THEN DO:
       ASSIGN de-sld-titulo = titulo.vl-original.
       FOR EACH mov-tit OF titulo
          WHERE mov-tit.dt-movto <= tt-param.dt-deposito-fin NO-LOCK:
          IF mov-tit.transacao =  2 OR /* Bax */
             mov-tit.transacao =  3 OR /* Dev */
             mov-tit.transacao =  4 OR /* Ima */
             mov-tit.transacao = 13    /* Ava */ THEN DO:
             IF mov-tit.lancamento = 2 THEN
                ASSIGN de-sld-titulo = de-sld-titulo + mov-tit.vl-baixa.
             ELSE
                ASSIGN de-sld-titulo = de-sld-titulo - mov-tit.vl-baixa.
          END.
       END.
    END.
    IF de-sld-titulo <> 0 THEN NEXT.

    /*--- Verifica se pelo menos um dos cheques teve o 1§dep¢sito no intervalo de data selecionado e 
                   se nÆo h  nenhum cheque pendente ---*/
    ASSIGN l-pagar-com = NO.
    FOR EACH b-cheque WHERE b-cheque.ep-codigo-mvto   = cheque.ep-codigo-mvto
                        AND b-cheque.cod-estabel-mvto = cheque.cod-estabel-mvto
                        AND b-cheque.cod-esp-mvto     = cheque.cod-esp-mvto
                        AND b-cheque.serie-mvto       = cheque.serie-mvto
                        AND b-cheque.nr-docto-mvto    = cheque.nr-docto-mvto
                        AND b-cheque.parcela-mvto     = cheque.parcela-mvto
                      NO-LOCK:
        
        FIND FIRST movto-cheque USE-INDEX num-id-cheque
             WHERE movto-cheque.num-id-cheque = b-cheque.num-id-cheque
               AND movto-cheque.transacao     = 6 /* Dep¢sito */
            NO-LOCK NO-ERROR.
        IF AVAIL movto-cheque THEN DO:
           IF movto-cheque.num-id-cheque =  b-cheque.num-id-cheque AND
              movto-cheque.dt-movto      >= tt-param.dt-deposito-ini AND
              movto-cheque.dt-movto      <= tt-param.dt-deposito-fin THEN
              ASSIGN l-pagar-com = YES.
           IF movto-cheque.num-id-cheque = b-cheque.num-id-cheque AND   
              movto-cheque.dt-movto > tt-param.dt-deposito-fin THEN DO:
              ASSIGN l-pagar-com = NO.
              LEAVE.
           END.
        END.
        ELSE DO:
           ASSIGN l-pagar-com = NO. /* Nunca foi depositado */
           LEAVE.
        END.
    END.

    IF l-pagar-com THEN DO:
       FIND titulo WHERE titulo.ep-codigo   = cheque.ep-codigo-mvto  
                     AND titulo.cod-estabel = cheque.cod-estabel-mvto
                     AND titulo.cod-esp     = cheque.cod-esp-mvto    
                     AND titulo.serie       = cheque.serie-mvto      
                     AND titulo.nr-docto    = cheque.nr-docto-mvto   
                     AND titulo.parcela     = cheque.parcela-mvto
                   NO-LOCK NO-ERROR.
       IF AVAIL titulo AND (titulo.cod-rep < tt-param.cod-rep-ini OR
                            titulo.cod-rep > tt-param.cod-rep-fin) THEN
          ASSIGN l-pagar-com = NO.
    END.

    IF l-pagar-com THEN DO:
       FIND tt-work WHERE tt-work.ep-codigo-mvto   = cheque.ep-codigo-mvto                                    
                      AND tt-work.cod-estabel-mvto = cheque.cod-estabel-mvto
                      AND tt-work.cod-esp-mvto     = cheque.cod-esp-mvto    
                      AND tt-work.serie-mvto       = cheque.serie-mvto      
                      AND tt-work.nr-docto-mvto    = cheque.nr-docto-mvto   
                      AND tt-work.parcela-mvto     = cheque.parcela-mvto
                    NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-work THEN DO:
          CREATE tt-work.
          ASSIGN tt-work.ep-codigo-mvto   = cheque.ep-codigo-mvto  
                 tt-work.cod-estabel-mvto = cheque.cod-estabel-mvto
                 tt-work.cod-esp-mvto     = cheque.cod-esp-mvto    
                 tt-work.serie-mvto       = cheque.serie-mvto      
                 tt-work.nr-docto-mvto    = cheque.nr-docto-mvto   
                 tt-work.parcela-mvto     = cheque.parcela-mvto.    
       END.
    END.
END.

FOR EACH cheque no-lock,
    EACH tt-work WHERE tt-work.ep-codigo-mvto   = cheque.ep-codigo-mvto  
                   AND tt-work.cod-estabel-mvto = cheque.cod-estabel-mvto
                   AND tt-work.cod-esp-mvto     = cheque.cod-esp-mvto    
                   AND tt-work.serie-mvto       = cheque.serie-mvto      
                   AND tt-work.nr-docto-mvto    = cheque.nr-docto-mvto   
                   AND tt-work.parcela-mvto     = cheque.parcela-mvto
                  NO-LOCK,
    EACH titulo WHERE titulo.ep-codigo   = cheque.ep-codigo-mvto  
                  AND titulo.cod-estabel = cheque.cod-estabel-mvto
                  AND titulo.cod-esp     = cheque.cod-esp-mvto    
                  AND titulo.serie       = cheque.serie-mvto      
                  AND titulo.nr-docto    = cheque.nr-docto-mvto   
                  AND titulo.parcela     = cheque.parcela-mvto
                NO-LOCK
                BREAK BY titulo.cod-rep
                      BY titulo.nome-abrev
                      BY titulo.nr-docto
                      BY titulo.parcela:

    IF FIRST-OF(titulo.cod-rep) THEN DO:
       FIND repres WHERE repres.cod-rep = titulo.cod-rep NO-LOCK NO-ERROR.
       DISPLAY repres.cod-rep 
               repres.nome
               with frame f-repres.
       DOWN(2) with frame f-repres.
    END.

    IF FIRST-OF(titulo.parcela) THEN DO:
       ASSIGN de-vlr-devol = 0.
       FOR EACH mov-tit WHERE mov-tit.ep-codigo   = titulo.ep-codigo   
                          AND mov-tit.cod-estabel = titulo.cod-estabel 
                          AND mov-tit.cod-esp     = titulo.cod-esp     
                          AND mov-tit.serie       = titulo.serie       
                          AND mov-tit.nr-docto    = titulo.nr-docto    
                          AND mov-tit.parcela     = titulo.parcela     
                        NO-LOCK:
           IF mov-tit.transacao   =  3 OR   /* Dev */
             (mov-tit.vl-abatimen <> 0 AND mov-tit.transacao = 2 AND (mov-tit.nr-docto-emp = "cnab" OR
                                                                      mov-tit.nr-docto-emp = "abat")) THEN 
              ASSIGN de-vlr-devol = de-vlr-devol + mov-tit.vl-baixa.
       END.
       FIND repres WHERE repres.cod-rep = titulo.cod-rep NO-LOCK NO-ERROR.
       ASSIGN de-comis-emi = ROUND((repres.comis-direta * repres.comis-emis * 0.01) * titulo.vl-original * 0.01,2)
              de-comis-tot = ROUND(repres.comis-direta * (titulo.vl-original - de-vlr-devol) * 0.01,2).
       
       ASSIGN de-comissao  = de-comis-tot - de-comis-emi
              de-imp-renda = round(de-comissao * 0.015,2)
              de-val-liq   = de-comissao - de-imp-renda.
    END.
    ELSE
       ASSIGN de-vlr-devol = 0
              de-comissao  = 0
              de-imp-renda = 0
              de-val-liq   = 0.


    DISPLAY titulo.nome-abrev  WHEN FIRST-OF(titulo.nome-abrev)
            titulo.cod-esp     WHEN FIRST-OF(titulo.parcela) 
            titulo.nr-docto    WHEN FIRST-OF(titulo.parcela)
            titulo.parcela     WHEN FIRST-OF(titulo.parcela)
            titulo.dt-vencimen WHEN FIRST-OF(titulo.parcela)
            titulo.vl-original WHEN FIRST-OF(titulo.parcela)
            cheque.cod-banco  
            cheque.agencia    
            cheque.nr-cheque  
            cheque.vl-cheque 
            de-vlr-devol       WHEN FIRST-OF(titulo.parcela)
            de-comissao        WHEN FIRST-OF(titulo.parcela)
            de-imp-renda       WHEN FIRST-OF(titulo.parcela)
            de-val-liq         WHEN FIRST-OF(titulo.parcela)
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

    ASSIGN de-tot-tit-rep = de-tot-tit-rep + cheque.vl-cheque
           de-tot-tit-ger = de-tot-tit-ger + cheque.vl-cheque
           de-tot-dev-rep = de-tot-dev-rep + de-vlr-devol
           de-tot-dev-ger = de-tot-dev-ger + de-vlr-devol
           de-tot-com-rep = de-tot-com-rep + de-comissao
           de-tot-com-ger = de-tot-com-ger + de-comissao
           de-tot-ir-rep  = de-tot-ir-rep +  de-imp-renda 
           de-tot-ir-ger  = de-tot-ir-ger +  de-imp-renda 
           de-tot-liq-rep = de-tot-liq-rep + de-val-liq      
           de-tot-liq-ger = de-tot-liq-ger + de-val-liq.

    IF LAST-OF(titulo.cod-rep) THEN DO:
       DOWN(1) WITH FRAME f-detalhe.
       DISPLAY "Total Repres" @ titulo.nome-abrev
               de-tot-tit-rep @ cheque.vl-cheque
               de-tot-dev-rep @ de-vlr-devol
               de-tot-com-rep @ de-comissao
               de-tot-ir-rep  @ de-imp-renda
               de-tot-liq-rep @ de-val-liq
               WITH FRAME f-detalhe.
       IF tt-param.salta-pagina THEN 
          PAGE.
       ELSE
          DOWN 2 WITH FRAME f-detalhe.

       ASSIGN de-tot-tit-rep = 0
              de-tot-dev-rep = 0
              de-tot-com-rep = 0
              de-tot-ir-rep  = 0
              de-tot-liq-rep = 0.
    END.
END.

DISPLAY "Total Geral"  @ titulo.nome-abrev
        de-tot-tit-ger @ cheque.vl-cheque
        de-tot-dev-ger @ de-vlr-devol
        de-tot-com-ger @ de-comissao
        de-tot-ir-ger  @ de-imp-renda
        de-tot-liq-ger @ de-val-liq
        WITH FRAME f-detalhe.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.ep-codigo      
           tt-param.cod-estabel-ini
           tt-param.cod-estabel-fin
           tt-param.cod-rep-ini
           tt-param.cod-rep-fin
           tt-param.dt-deposito-ini  
           tt-param.dt-deposito-fin 
           tt-param.salta-pagina
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

