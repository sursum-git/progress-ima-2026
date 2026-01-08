/* Programa: ESCB0001.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Caixa e Bancos
** Objetivo: Listar Cheques por Tipo de Receita
** Autor...: Gilvando de Souza Araujo - Julho/2005
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCB0001RP 2.04.00.000}

define temp-table tt-param         no-undo
       field destino               as integer
       field arquivo               as char format "x(35)"
       field usuario               as char format "x(12)"
       field data-exec             as date
       field hora-exec             as integer
       FIELD ep-codigo             LIKE cheque.ep-codigo-mvto
       FIELD cod-estabel-ini       LIKE cheque.cod-estabel-mvto
       FIELD cod-estabel-fin       LIKE cheque.cod-estabel-mvto
       FIELD cod-banco-ini         LIKE cheque.cod-banco 
       FIELD cod-banco-fin         LIKE cheque.cod-banco
       FIELD dt-vencimento-ini     LIKE cheque.dt-vencimento
       FIELD dt-vencimento-fin     LIKE cheque.dt-vencimento
       FIELD dt-deposito-ini       LIKE cheque.dt-deposito
       FIELD dt-deposito-fin       LIKE cheque.dt-deposito
       FIELD dt-emissao-ini        LIKE cheque.dt-emissao
       FIELD dt-emissao-fin        LIKE cheque.dt-emissao
       FIELD tp-codigo-ini         LIKE cheque.tp-codigo
       FIELD tp-codigo-fin         LIKE cheque.tp-codigo
       FIELD cod-emitente-ini      LIKE cheque.cod-emitente
       FIELD cod-emitente-fin      LIKE cheque.cod-emitente
       FIELD nom-emit-cheque-ini   LIKE cheque.nom-emit-cheque
       FIELD nom-emit-cheque-fin   LIKE cheque.nom-emit-cheque
       FIELD tipo-cheque           AS INT
       FIELD desc-tipo-cheque      AS CHAR FORMAT "x(17)"
       FIELD sit-chq-pend          AS LOGICAL FORMAT "Sim/NÆo"
       FIELD sit-chq-depos         AS LOGICAL FORMAT "Sim/NÆo"
       FIELD sit-chq-caucao        AS LOGICAL FORMAT "Sim/NÆo"
       FIELD sit-chq-subst         AS LOGICAL FORMAT "Sim/NÆo"
       FIELD sit-chq-devolv        AS LOGICAL FORMAT "Sim/NÆo"
       FIELD sit-chq-canc          AS LOGICAL FORMAT "Sim/NÆo"
       FIELD sit-chq-desc          AS LOGICAL FORMAT "Sim/NÆo" 
       FIELD sit-chq-comp          AS LOGICAL FORMAT "Sim/NÆo" 
       FIELD sit-chq-pgemp         AS LOGICAL FORMAT "Sim/NÆo" 
       FIELD tipo-rel              AS INT
       FIELD desc-tipo-rel         AS CHAR FORMAT "x(10)"
       FIELD impr-param            AS LOGICAL.

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

def var de-tot-dia    as dec format ">>>,>>>,>>9.99".
def var de-tot-rec    as dec format ">>>,>>>,>>9.99".
def var de-tot-ger    as dec format ">>>,>>>,>>9.99".
DEF VAR i-qtd-chq-dia AS INT.
DEF VAR i-qtd-chq-rec AS INT.
DEF VAR i-qtd-chq-ger AS INT.

form
    "*----------------- Parƒmetros/Sele‡Æo ------------------*" SKIP
    tt-param.ep-codigo           LABEL "Empresa........" AT 1
    tt-param.cod-estabel-ini     LABEL "Estabelecimento" AT 1
    "a"  AT 29
    tt-param.cod-estabel-fin     NO-LABELS
    tt-param.cod-banco-ini       LABEL "Banco.........." AT 1
    "a"  AT 29                
    tt-param.cod-banco-fin       NO-LABELS
    tt-param.dt-vencimento-ini   LABEL "Data Vencimento" AT 1
    "a"  AT 29                
    tt-param.dt-vencimento-fin   NO-LABELS
    tt-param.dt-deposito-ini     LABEL "Data Dep¢sito.." AT 1
    "a"  AT 29
    tt-param.dt-deposito-fin     NO-LABELS
    tt-param.dt-emissao-ini      LABEL "Data EmissÆo..." AT 1
    "a"  AT 29                                               
    tt-param.dt-emissao-fin      NO-LABELS                   
    tt-param.tp-codigo-ini       LABEL "Tp.Receita/Desp" AT 1
    "a"  AT 29                                               
    tt-param.tp-codigo-fin       NO-LABELS
    tt-param.cod-emitente-ini    LABEL "C¢digo Emitente" AT 1
    "a"  AT 29                                               
    tt-param.cod-emitente-fin    NO-LABELS                   
    tt-param.nom-emit-cheque-ini LABEL "Emitente Cheque" AT 1
    tt-param.nom-emit-cheque-fin LABEL "            Ate" AT 1
    tt-param.desc-tipo-cheque    LABEL "Tipo Cheque...." AT 1
    tt-param.sit-chq-pend        LABEL "Chq-Pendente..." AT 1
    tt-param.sit-chq-depos       LABEL "Chq-Depositado." AT 1
    tt-param.sit-chq-caucao      LABEL "Chq-Cau‡Æo....." AT 1
    tt-param.sit-chq-subst       LABEL "Chq-Substitu¡do" AT 1
    tt-param.sit-chq-devolv      LABEL "Chq-Devolvido.." AT 1
    tt-param.sit-chq-canc        LABEL "Chq-Cancelado.." AT 1
    tt-param.sit-chq-desc        LABEL "Chq-Descontado." AT 1
    tt-param.sit-chq-comp        LABEL "Chq-Compensado." AT 1
    tt-param.sit-chq-pgemp       LABEL "Chq-Pagto.Empr." AT 1
    tt-param.desc-tipo-rel       LABEL "Tipo Relatorio." AT 1 SKIP(1)
    with no-box side-labels width 132 stream-io frame f-param.

FORM
    "Tipo Receita: "
    cheque.tp-codigo
    " - "
    tipo-rec-desp.descricao
    SKIP(1)
    WITH NO-LABEL 1 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-cab-tprec.

FORM HEADER
    "Nome do Emitente                         Bco Agencia Conta-corrente    Nr.Cheque    Valor Chq Dt.Emiss.  Dt.Vencto  Dt.Depos"
    "---------------------------------------- --- ------- ----------------- --------- ------------ ---------- ---------- ----------"
    WITH NO-LABELS NO-ATTR-SPACE NO-BOX PAGE-TOP WIDTH 132 STREAM-IO 1 DOWN FRAME f-cab-dados.

form
    cheque.nom-emit-cheque      
    cheque.cod-banco        FORMAT "999"
    cheque.agencia          FORMAT "x(7)"
    cheque.conta-corren     FORMAT "x(17)"
    cheque.nr-cheque        
    cheque.vl-cheque        FORMAT ">,>>>,>>9.99"
    cheque.dt-emissao       
    cheque.dt-vencimento       
    cheque.dt-deposito      
    with no-box NO-LABEL 55 down width 134 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FINANCEIRO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Cheques_por_Tipo_de_Receita * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cab-dados.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH cheque WHERE cheque.ep-codigo-mvto   =  tt-param.ep-codigo
                  AND cheque.cod-estabel-mvto >= tt-param.cod-estabel-ini
                  AND cheque.cod-estabel-mvto <= tt-param.cod-estabel-fin
                  AND cheque.cod-banco        >= tt-param.cod-banco-ini
                  AND cheque.cod-banco        <= tt-param.cod-banco-fin
                  AND cheque.dt-vencimento    >= tt-param.dt-vencimento-ini
                  AND cheque.dt-vencimento    <= tt-param.dt-vencimento-fin
                  AND (cheque.dt-deposito     >= tt-param.dt-deposito-ini OR tt-param.dt-deposito-ini = ?)
                  AND (cheque.dt-deposito     <= tt-param.dt-deposito-fin OR tt-param.dt-deposito-fin = ?)
                  AND cheque.dt-emissao       >= tt-param.dt-emissao-ini
                  AND cheque.dt-emissao       <= tt-param.dt-emissao-fin
                  AND cheque.tp-codigo        >= tt-param.tp-codigo-ini
                  AND cheque.tp-codigo        <= tt-param.tp-codigo-fin
                  AND cheque.cod-emitente     >= tt-param.cod-emitente-ini
                  AND cheque.cod-emitente     <= tt-param.cod-emitente-fin
                  AND cheque.nom-emit-cheque  >= tt-param.nom-emit-cheque-ini
                  AND cheque.nom-emit-cheque  <= tt-param.nom-emit-cheque-fin
                  AND (cheque.tipo-cheque      = tt-param.tipo-cheque OR
                                                 tt-param.tipo-cheque = 3)
                  AND ((cheque.situacao-cheque = 1 AND tt-param.sit-chq-pend) OR
                       (cheque.situacao-cheque = 2 AND tt-param.sit-chq-depos) OR
                       (cheque.situacao-cheque = 3 AND tt-param.sit-chq-caucao) OR
                       (cheque.situacao-cheque = 4 AND tt-param.sit-chq-subst) OR
                       (cheque.situacao-cheque = 5 AND tt-param.sit-chq-devolv) OR
                       (cheque.situacao-cheque = 6 AND tt-param.sit-chq-canc) OR
                       (cheque.situacao-cheque = 7 AND tt-param.sit-chq-desc) OR
                       (cheque.situacao-cheque = 8 AND tt-param.sit-chq-comp) OR
                       (cheque.situacao-cheque = 9 AND tt-param.sit-chq-pgemp))
                NO-LOCK
                BREAK BY cheque.tp-codigo
                      BY cheque.dt-vencimento
                      BY cheque.nom-emit-cheque:

    RUN pi-acompanhar IN h-acomp (INPUT "TpReceita: " + STRING(cheque.tp-codigo) + 
                                        " Banco: " + STRING(cheque.cod-banco) +
                                        " Cheque: " + STRING(cheque.nr-cheque)).

    IF FIRST-OF(cheque.tp-codigo) THEN DO:
       FIND tipo-rec-desp WHERE tipo-rec-desp.tp-codigo = cheque.tp-codigo
                          NO-LOCK NO-ERROR.
       DISPLAY cheque.tp-codigo
               tipo-rec-desp.descricao
               WITH FRAME f-cab-tprec.
    END.

    ASSIGN de-tot-dia = de-tot-dia + cheque.vl-cheque
           de-tot-rec = de-tot-rec + cheque.vl-cheque
           de-tot-ger = de-tot-ger + cheque.vl-cheque.

    IF tt-param.tipo-rel = 1 THEN DO: /* Detalhado */
       DISPLAY cheque.nom-emit-cheque 
               cheque.cod-banco       
               cheque.agencia         
               cheque.conta-corren    
               cheque.nr-cheque       
               cheque.vl-cheque 
               cheque.dt-emissao
               cheque.dt-vencimento   
               cheque.dt-deposito     
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
    END.

    ASSIGN i-qtd-chq-dia = i-qtd-chq-dia + 1
           i-qtd-chq-rec = i-qtd-chq-rec + 1
           i-qtd-chq-ger = i-qtd-chq-ger + 1.

    IF tt-param.tipo-rel = 2 AND LAST-OF(cheque.dt-vencimento) THEN DO:
       DISPLAY "Total do Dia " + string(cheque.dt-vencimento) + ":" 
                               @ cheque.nom-emit-cheque 
               i-qtd-chq-dia   @ cheque.nr-cheque
               de-tot-dia      @ cheque.vl-cheque
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
       assign de-tot-dia    = 0
              i-qtd-chq-dia = 0.
    END.

    IF LAST-OF(cheque.tp-codigo) THEN DO:
       DISPLAY "Total Tipo Receita:" @ cheque.nom-emit-cheque 
               i-qtd-chq-rec         @ cheque.nr-cheque
               de-tot-rec            @ cheque.vl-cheque
               WITH FRAME f-detalhe.
       DOWN 2 WITH FRAME f-detalhe.
       ASSIGN de-tot-rec    = 0
              i-qtd-chq-rec = 0.
    end.
end.

DISPLAY "Total Geral:" @ cheque.nom-emit-cheque
        i-qtd-chq-ger  @ cheque.nr-cheque
        de-tot-ger     @ cheque.vl-cheque
        WITH FRAME f-detalhe.
assign de-tot-ger    = 0
       i-qtd-chq-ger = 0.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.ep-codigo      
           tt-param.cod-estabel-ini
           tt-param.cod-estabel-fin
           tt-param.cod-banco-ini   
           tt-param.cod-banco-fin   
           tt-param.dt-vencimento-ini    
           tt-param.dt-vencimento-fin
           tt-param.dt-deposito-ini
           tt-param.dt-deposito-fin
           tt-param.dt-emissao-ini
           tt-param.dt-emissao-fin
           tt-param.tp-codigo-ini 
           tt-param.tp-codigo-fin 
           tt-param.cod-emitente-ini
           tt-param.cod-emitente-fin
           tt-param.nom-emit-cheque-ini  
           tt-param.nom-emit-cheque-fin 
           tt-param.desc-tipo-cheque
           tt-param.sit-chq-pend    
           tt-param.sit-chq-depos   
           tt-param.sit-chq-caucao  
           tt-param.sit-chq-subst   
           tt-param.sit-chq-devolv  
           tt-param.sit-chq-canc    
           tt-param.sit-chq-desc    
           tt-param.sit-chq-comp    
           tt-param.sit-chq-pgemp   
           tt-param.desc-tipo-rel
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

