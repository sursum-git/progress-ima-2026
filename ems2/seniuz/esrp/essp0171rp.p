/* Programa: ESSP0171.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Gerar os Saldos da movimenta‡Æo do estoque
** Autor...: F bio Coelho Lanza - Fevereiro/2008
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0171RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param   NO-UNDO
    FIELD destino            AS INTEGER
    FIELD arquivo            AS CHAR FORMAT "x(35)"
    FIELD usuario            AS CHAR FORMAT "x(12)"
    FIELD data-exec          AS DATE
    FIELD hora-exec          AS INTEGER
    FIELD classifica         AS INTEGER
    FIELD desc-classifica    AS CHAR FORMAT "x(40)"
    FIELD dt-ini             AS DATE
    FIELD dt-fin             AS DATE
    FIELD imp-param          AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}


/* defini‡Æo de vari veis  */
DEF VAR h-acomp        AS HANDLE NO-UNDO.
DEF VAR c-periodo      AS CHAR.
DEF VAR c-dia          AS CHAR.
DEF VAR da-dt-ini      AS DATE.
DEF VAR da-dt-fin      AS DATE.
DEF VAR da-ult-dia-fat AS DATE.
DEF VAR i-mes          AS INT.
DEF VAR i-ano          AS INT.
DEF VAR c-periodo-ant  AS CHAR.

DEF BUFFER b-ob-sl-estoq-per FOR ob-sl-estoq-per.

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

/*  Bloco para Displayar Mensagens no Video 
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
*/

/* Monta o Periodo ATUAL */
ASSIGN c-periodo = STRING(MONTH(TODAY - 1),'99') + STRING(YEAR(TODAY - 1),'9999').
RUN esapi/ret-udm.p (c-periodo, OUTPUT c-dia).
ASSIGN da-dt-ini = DATE(MONTH(TODAY - 1),1,YEAR(TODAY - 1))
       da-dt-fin = DATE(MONTH(TODAY - 1),INT(c-dia),YEAR(TODAY - 1)).

/* Monta o Periodo ANTERIOR */
ASSIGN i-mes = INT(SUBSTR(c-periodo,1,2)) - 1
       i-ano = INT(SUBSTR(c-periodo,3,4)).
IF i-mes = 0 THEN
   ASSIGN i-mes = 12
          i-ano = i-ano - 1.
ASSIGN c-periodo-ant = STRING(i-mes, "99") + STRING(i-ano, "9999").

/* Limpar o periodo */
FOR EACH ob-sl-estoq-per WHERE
         ob-sl-estoq-per.periodo = c-periodo SHARE-LOCK.
    ASSIGN ob-sl-estoq-per.qtd-entr-est  = 0
           ob-sl-estoq-per.qtd-transf    = 0
           ob-sl-estoq-per.qtd-devolvida = 0
           ob-sl-estoq-per.qtd-faturada  = 0
           ob-sl-estoq-per.qtd-final     = 0
           ob-sl-estoq-per.qtd-real      = 0.
END.

/* Gerar o Estoque Inicial do Periodo */
FIND FIRST ob-sl-estoq-per WHERE
           ob-sl-estoq-per.periodo = c-periodo NO-LOCK NO-ERROR.
IF NOT AVAIL ob-sl-estoq-per THEN DO:
    FOR EACH b-ob-sl-estoq-per WHERE
             b-ob-sl-estoq-per.periodo  = c-periodo-ant AND 
             b-ob-sl-estoq-per.qtd-real > 0 NO-LOCK.
         CREATE ob-sl-estoq-per.
         ASSIGN ob-sl-estoq-per.periodo      = c-periodo
                ob-sl-estoq-per.it-codigo    = b-ob-sl-estoq-per.it-codigo
                ob-sl-estoq-per.cod-refer    = b-ob-sl-estoq-per.cod-refer  
                ob-sl-estoq-per.nr-lote      = b-ob-sl-estoq-per.nr-lote
                ob-sl-estoq-per.corte-comerc = b-ob-sl-estoq-per.corte-comerc
                ob-sl-estoq-per.cod-qualid   = b-ob-sl-estoq-per.cod-qualid
                ob-sl-estoq-per.qtd-inicial  = b-ob-sl-estoq-per.qtd-real.
    END.
END.

/* Gerando Entradas */
FOR EACH  ob-etiqueta WHERE 
          ob-etiqueta.dt-emissao >= da-dt-ini AND
          ob-etiqueta.dt-emissao <= da-dt-fin AND
          ob-etiqueta.situacao   >= 3         AND  
          ob-etiqueta.tipo-ordem  = 1 NO-LOCK. /* PRODU€ÇO */

    FIND ob-sl-estoq-per WHERE
         ob-sl-estoq-per.periodo      = c-periodo               AND
         ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo   AND
         ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer   AND
         ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote     AND
         ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comer AND 
         ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid  NO-ERROR.
    IF NOT AVAIL ob-sl-estoq-per THEN DO:
       CREATE ob-sl-estoq-per.
       ASSIGN ob-sl-estoq-per.periodo      = c-periodo
              ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo
              ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer
              ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote
              ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comerc
              ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid.
    END.
    ASSIGN ob-sl-estoq-per.qtd-entr-est = ob-sl-estoq-per.qtd-entr-est +
                                          ob-etiqueta.quantidade.
END.

/* Gerando Estoque Atual */
FOR EACH  ob-etiqueta WHERE 
          ob-etiqueta.situacao >= 3 AND    /* EM ESTOQUE */
          ob-etiqueta.situacao <= 4 NO-LOCK. /* RESERVADA */

    FIND ob-sl-estoq-per WHERE
         ob-sl-estoq-per.periodo      = c-periodo               AND
         ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo   AND
         ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer   AND
         ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote     AND
         ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comer AND 
         ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid  NO-ERROR.
    IF NOT AVAIL ob-sl-estoq-per THEN DO:
       CREATE ob-sl-estoq-per.
       ASSIGN ob-sl-estoq-per.periodo      = c-periodo
              ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo
              ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer
              ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote
              ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comerc
              ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid.
    END.
    ASSIGN ob-sl-estoq-per.qtd-real = ob-sl-estoq-per.qtd-real +
                                      ob-etiqueta.quantidade.

END.

/* Gerando Transforma‡äes */
FOR EACH ob-trf WHERE 
         ob-trf.dt-solic >= da-dt-ini AND 
         ob-trf.dt-solic <= da-dt-fin NO-LOCK.
    FIND FIRST ordem-benefic WHERE /* Soma as Etiquetas Geradas na Transforma‡Æo */
               ordem-benefic.num-trf   = ob-trf.num-trf AND 
               ordem-benefic.it-codigo = ob-trf.it-codigo AND 
               ordem-benefic.cod-refer = ob-trf.cod-refer
               NO-LOCK NO-ERROR.
    IF AVAIL ordem-benefic  THEN DO: 
       FOR EACH ob-etiqueta OF ordem-benefic NO-LOCK.
           FIND ob-sl-estoq-per WHERE
                ob-sl-estoq-per.periodo      = c-periodo               AND
                ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo   AND
                ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer   AND
                ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote     AND
                ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comer AND 
                ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid  NO-ERROR.
           IF NOT AVAIL ob-sl-estoq-per THEN DO:
              CREATE ob-sl-estoq-per.
              ASSIGN ob-sl-estoq-per.periodo      = c-periodo
                     ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo
                     ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer
                     ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote
                     ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comerc
                     ob-sl-estoq-per.cod-qualid   = ob-sl-estoq-per.cod-qualid.
           END.
           ASSIGN ob-sl-estoq-per.qtd-transf = ob-sl-estoq-per.qtd-transf +
                                               ob-etiqueta.quantidade.
       END.
    END.
    FOR EACH ob-etq-trf OF ob-trf NO-LOCK.
        FIND ob-etiqueta WHERE /* Baixa as Etiquetas a Serem Transformadas */
             ob-etiqueta.cod-estabel  = ob-etq-trf.cod-estabel AND
             ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta NO-LOCK NO-ERROR.
        IF NOT AVAIL ob-etiqueta THEN NEXT.

        FIND ob-sl-estoq-per WHERE
             ob-sl-estoq-per.periodo      = c-periodo               AND
             ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo   AND
             ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer   AND
             ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote     AND
             ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comer AND 
             ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid  NO-ERROR.
        IF NOT AVAIL ob-sl-estoq-per THEN DO:
           CREATE ob-sl-estoq-per.
           ASSIGN ob-sl-estoq-per.periodo      = c-periodo
                  ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo
                  ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer
                  ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote
                  ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comerc
                  ob-sl-estoq-per.cod-qualid   = ob-sl-estoq-per.cod-qualid.
        END.
        ASSIGN ob-sl-estoq-per.qtd-transf = ob-sl-estoq-per.qtd-transf -
                                            ob-etiqueta.quantidade.
    END.
END.

/* Gerando Devolu‡äes */
FOR EACH docum-est WHERE
         docum-est.dt-trans >= da-dt-ini AND
         docum-est.dt-trans <= da-dt-fin NO-LOCK.

    IF docum-est.cod-estabel <> "2" OR docum-est.esp-fiscal <> "NFD" THEN NEXT. 
    
    IF docum-est.cod-emitente = 8 OR  /* Tear Textil */
       docum-est.cod-emitente = 9 THEN NEXT.

    FOR EACH item-doc-est OF docum-est NO-LOCK. 
        
        FIND nota-fiscal WHERE
             nota-fiscal.cod-estabel  = docum-est.cod-estabel   AND
             nota-fiscal.serie        = item-doc-est.serie-comp AND
             nota-fiscal.nr-nota-fis  = item-doc-est.nro-comp 
             NO-LOCK NO-ERROR.
        IF AVAIL nota-fiscal THEN DO.
           FOR EACH dev-item-rom WHERE
                    dev-item-rom.nome-abrev   = nota-fiscal.nome-ab-cli AND
                    dev-item-rom.nr-pedcli    = nota-fiscal.nr-pedcli  AND
                    dev-item-rom.nr-sequencia = item-doc-est.nr-pd-seq
                    NO-LOCK.

               FIND ped-venda WHERE
                    ped-venda.nr-pedcli = dev-item-rom.nr-pedcli AND
                    ped-venda.nome-abrev = dev-item-rom.nome-abrev NO-LOCK NO-ERROR.
               IF NOT AVAIL ped-venda THEN NEXT.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-venda.cod-estabel AND
                    ob-etiqueta.num-etiqueta = dev-item-rom.num-etiqueta NO-LOCK NO-ERROR.
               IF AVAIL ob-etiqueta THEN DO: 
                  FIND ob-sl-estoq-per WHERE
                       ob-sl-estoq-per.periodo      = c-periodo               AND
                       ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo   AND
                       ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer   AND
                       ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote     AND
                       ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comer AND 
                       ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid NO-ERROR.
                  IF NOT AVAIL ob-sl-estoq-per THEN DO:
                     CREATE ob-sl-estoq-per.
                     ASSIGN ob-sl-estoq-per.periodo      = c-periodo
                            ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo
                            ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer
                            ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote
                            ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comerc
                            ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid.
                  END.
                  ASSIGN ob-sl-estoq-per.qtd-devol = ob-sl-estoq-per.qtd-devol +
                                                     ob-etiqueta.quantidade.
               END.
           END.
         END.
    END.
END.

/* Gerando Faturamento */
FOR EACH  nota-fiscal USE-INDEX ch-distancia WHERE
          nota-fiscal.dt-emis-nota >= da-dt-ini AND 
          nota-fiscal.dt-emis-nota <= da-dt-fin AND
          nota-fiscal.ind-sit-nota <> 4         AND /* CANCELADA */
          nota-fiscal.dt-cancela    = ?  NO-LOCK. 
    
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        FIND ITEM WHERE
             ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN
           IF ITEM.ge-codigo < 50 OR ITEM.ge-codigo > 59 THEN NEXT.

        FIND natur-oper WHERE 
             natur-oper.nat-operacao = it-nota-fisc.nat-operacao 
             NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper OR
                     natur-oper.baixa-estoq  = NO THEN /* NÆo Baixa Estoque */
           NEXT.

        /* Etiquetas das Notas FATURADAS */
        FOR EACH ped-item-rom WHERE
                 ped-item-rom.nome-abrev   = it-nota-fisc.nome-ab-cli AND
                 ped-item-rom.nr-pedcli    = it-nota-fisc.nr-pedcli  AND
                 ped-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped
                 NO-LOCK.

            RUN pi-grava-movto (INPUT ped-item-rom.num-etiqueta,
                                INPUT c-periodo,
                                INPUT nota-fiscal.cod-estabel).
        END.

        /* Etiquetas das Notas DEVOLVIDAS */
        FOR EACH dev-item-rom WHERE
                 dev-item-rom.nome-abrev   = it-nota-fisc.nome-ab-cli AND
                 dev-item-rom.nr-pedcli    = it-nota-fisc.nr-pedcli  AND
                 dev-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped
                 NO-LOCK.

            RUN pi-grava-movto (INPUT dev-item-rom.num-etiqueta,
                                INPUT c-periodo,
                                INPUT nota-fiscal.cod-estabel).

        END.

        /* Tratamento Retalho */
        IF SUBSTR(it-nota-fisc.it-codigo,6,1) = '9' THEN  DO:
           FIND ob-sl-estoq-per WHERE
                ob-sl-estoq-per.periodo      = c-periodo              AND
                ob-sl-estoq-per.it-codigo    = it-nota-fisc.it-codigo AND
                ob-sl-estoq-per.cod-refer    = it-nota-fisc.cod-refer AND
                ob-sl-estoq-per.nr-lote      = 'SC'                   AND
                ob-sl-estoq-per.corte-comerc = 'J'                    AND 
                ob-sl-estoq-per.cod-qualid   = ' ' NO-ERROR.
           IF NOT AVAIL ob-sl-estoq-per THEN DO:
              CREATE ob-sl-estoq-per.
              ASSIGN ob-sl-estoq-per.periodo      = c-periodo
                     ob-sl-estoq-per.it-codigo    = it-nota-fisc.it-codigo
                     ob-sl-estoq-per.cod-refer    = it-nota-fisc.cod-refer
                     ob-sl-estoq-per.nr-lote      = 'SC'
                     ob-sl-estoq-per.corte-comerc = 'J'
                     ob-sl-estoq-per.cod-qualid   = ' '.
           END.
           ASSIGN ob-sl-estoq-per.qtd-faturada = ob-sl-estoq-per.qtd-faturada +
                                                 it-nota-fisc.qt-faturada[1].
        END.
    END.
END.

/* Gerando o Estoque Final */
FOR EACH ob-sl-estoq-per WHERE
         ob-sl-estoq-per.periodo = c-periodo.

    ASSIGN ob-sl-estoq-per.qtd-final = ob-sl-estoq-per.qtd-inicial   +
                                       ob-sl-estoq-per.qtd-entr-est  +
                                       ob-sl-estoq-per.qtd-transf    +
                                       ob-sl-estoq-per.qtd-devolvida -
                                       ob-sl-estoq-per.qtd-faturada.

END.

/* P  R  O  C  E  D  I  M  E  N  T  O  S */
/* ------------------------------------- */

PROCEDURE pi-grava-movto.

    DEF INPUT PARAMETER p-num-etiqueta  LIKE ob-etiqueta.num-etiqueta.
    DEF INPUT PARAMETER p-periodo       AS CHAR.
    DEF INPUT PARAMETER p-cod-estabel   LIKE nota-fiscal.cod-estabel.


    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel  = p-cod-estabel AND
         ob-etiqueta.num-etiqueta = p-num-etiqueta NO-LOCK NO-ERROR.
    IF AVAIL ob-etiqueta THEN DO:
        FIND ob-sl-estoq-per WHERE
             ob-sl-estoq-per.periodo      = p-periodo               AND
             ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo   AND
             ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer   AND
             ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote     AND
             ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comer AND 
             ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid  NO-ERROR.
        IF NOT AVAIL ob-sl-estoq-per THEN DO:
           CREATE ob-sl-estoq-per.
           ASSIGN ob-sl-estoq-per.periodo      = p-periodo
                  ob-sl-estoq-per.it-codigo    = ob-etiqueta.it-codigo
                  ob-sl-estoq-per.cod-refer    = ob-etiqueta.cod-refer
                  ob-sl-estoq-per.nr-lote      = ob-etiqueta.nr-lote
                  ob-sl-estoq-per.corte-comerc = ob-etiqueta.corte-comerc
                  ob-sl-estoq-per.cod-qualid   = ob-etiqueta.cod-qualid.
        END.
        ASSIGN ob-sl-estoq-per.qtd-faturada = ob-sl-estoq-per.qtd-faturada +
                                              ob-etiqueta.quantidade.
    END.

END PROCEDURE.



PROCEDURE pi-limpa-per.

    DEF INPUT PARAMETER p-periodo AS CHAR.

    FOR EACH ob-sl-estoq-per WHERE
             ob-sl-estoq-per.periodo = p-periodo SHARE-LOCK.
        ASSIGN ob-sl-estoq-per.qtd-entr-est  = 0
               ob-sl-estoq-per.qtd-transf    = 0
               ob-sl-estoq-per.qtd-devolvida = 0
               ob-sl-estoq-per.qtd-faturada  = 0
               ob-sl-estoq-per.qtd-final     = 0
               ob-sl-estoq-per.qtd-real      = 0.
    END.

END PROCEDURE.
