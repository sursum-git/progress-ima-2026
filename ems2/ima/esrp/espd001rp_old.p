/* Programa: ESPD001
** Modulo..: Controle do Portal
** Objetivo: Analisa pedidos do portal, se os mesmos estiverem
**           status OK, a importaá∆o ser† efetuada.
** Autor...: Toninho - DEZEMBRO/2018
**
*/

DEFINE TEMP-TABLE tt-param  NO-UNDO
    FIELD destino           AS INTEGER
    FIELD arquivo           AS CHAR FORMAT "x(35)"
    FIELD usuario           AS CHAR FORMAT "x(12)"
    FIELD data-exec         AS DATE
    FIELD hora-exec         AS INTEGER
    FIELD enviar-e-mail     AS LOG FORMAT "Sim/N∆o"
    FIELD destinatarios     AS CHAR
    FIELD l-batch           AS LOG.

DEFINE TEMP-TABLE tt-digita NO-UNDO 
    FIELD ordem            AS INTEGER   FORMAT ">>>>9"
    FIELD exemplo          AS CHARACTER FORMAT "x(30)"
    INDEX id ordem.

DEFINE TEMP-TABLE tt-ped-venda NO-UNDO LIKE ped-venda 
       FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item
       FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-ped-repre NO-UNDO LIKE ped-repre
       FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE wt-ped-repre  NO-UNDO LIKE ped-repre
       FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-cond-ped NO-UNDO LIKE cond-ped
       FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE wt-cond-ped NO-UNDO LIKE cond-ped
       FIELD r-rowid AS ROWID.


DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEFINE TEMP-TABLE tt-erros-local
    FIELD cod-erro  As INTEGER 
    FIELD desc-erro As Character FORMAT "x(50)"
    FIELD desc-arq  As Character.

DEF BUFFER b-emitente FOR emitente.
DEF BUFFER b-transporte FOR transporte.
DEF BUFFER moeda FOR mgcad.moeda.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.  
 
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* definiá∆o de vari†veis  */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-acomp    AS HANDLE NO-UNDO.
DEF VAR h-bodi018  AS HANDLE.
DEF VAR h-bonat001 AS HANDLE.

DEF VAR c-mens-erro      AS CHAR.
DEF VAR c-mens-erro-it   AS CHAR.
DEF VAR c-mensagem       AS CHAR.
DEF VAR c-remetente      AS CHAR INITIAL "imatextil@imatextil.com.br".
DEF VAR c-destinatario   LIKE param-dis.destinatario.
DEF VAR l-erro           AS LOGICAL.
DEF VAR i-cont           AS INT.
DEF VAR i-tp-frete       AS INT.
DEF VAR i-ct             AS INT.
DEF VAR i-nr-sequencia   AS INT.
DEF VAR da-dt-now        AS DATETIME.
DEF VAR de-comis-direta  AS DECIMAL.
DEF VAR de-vl-preori     AS DECIMAL.
DEF VAR c-tpped-cred-aut AS CHAR INIT "∑ Vista,Exportaá∆o,Amostra,Amostra Exportaá∆o,Bonificaá∆o,Doaá∆o".
DEF VAR c-natur-oper     AS CHAR.
DEF VAR c-pct-desconto   AS CHAR.
DEF VAR i-fin-nat        AS INTEGER.
DEF VAR i-moeda          AS INTEGER.
DEF VAR c-cnae           AS CHAR.
DEF VAR c-erro-nat       AS CHAR.
DEF VAR c-arq-gerado-pdf AS CHAR.
DEF VAR i-param-nat      AS INTEGER.
DEF VAR l-ok             AS LOGICAL.
DEF VAR c-base           AS CHAR.

DEF VAR de-qt-vend       AS DEC.


// Definiá‰es para e-mail HTML
{utp/utapi019.i}

DEF TEMP-TABLE tt-aux 
    FIELD c-linha AS CHAR.

/* Code placed here will execute AFTER standard behavior.    */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST param-dis NO-LOCK NO-ERROR.

//ASSIGN tt-param.enviar-e-mail = tt-param.l-batch.
ASSIGN tt-param.enviar-e-mail = YES.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

IF NOT VALID-HANDLE(h-bodi018) OR 
   h-bodi018:TYPE      <> "PROCEDURE":U OR
   h-bodi018:FILE-NAME <> "dibo/bodi018.p":U THEN
   RUN dibo/bodi018.p PERSISTENT SET h-bodi018.

IF NOT VALID-HANDLE(h-bonat001) THEN
   RUN esbo/bonat001.p PERSISTENT SET h-bonat001.

RUN esapi/busca-base.p (OUTPUT c-base).

// Acerta Saldo dos Containers
RUN pi-act-sld-container.

// Vence Pedidos Parados
ASSIGN da-dt-now = NOW.
FOR EACH peds_web WHERE
         peds_web.ind_sit_ped_web = 1 SHARE-LOCK.

    IF peds_web.cod_tipo_pedido = 'PI' THEN
       FIND im-param WHERE 
            im-param.cod-param = 'PORTAL_MINUTOS_DESALOCAR_PI' NO-LOCK NO-ERROR.
    ELSE
       FIND im-param WHERE 
            im-param.cod-param = 'PORTAL_MINUTOS_DESALOCAR_PE' NO-LOCK NO-ERROR.

    IF NOT AVAIL im-param THEN NEXT.

    IF peds_web.dt_hr_venc <> ? THEN DO.
       IF da-dt-now > peds_web.dt_hr_venc THEN
          ASSIGN peds_web.ind_sit_ped_web = 6.  // Vencido
    END.
    ELSE DO.
       IF ADD-INTERVAL(da-dt-now,- INT(im-param.val-param),"minutes") > peds_web.dt_hr_registro THEN
          ASSIGN peds_web.ind_sit_ped_web = 6.  // Vencido
    END.
END.
RELEASE peds_web.

// Integra Pedidos Efetivados

// 1-Em Digitaá∆o   2-Efetivado   3-Cancelado   4-Integrado   5-Rejeitado   6-Vencido  7-Aprovado
FOR EACH peds_web WHERE
         peds_web.ind_sit_ped_web = 2 SHARE-LOCK.

    FIND FIRST itens_ped_web WHERE
               itens_ped_web.ped_web_id = peds_web.ped_web_id AND 
               itens_ped_web.ind_sit_itens_ped_web <> 3 NO-LOCK NO-ERROR.
    IF NOT AVAIL itens_ped_web THEN DO.
       ASSIGN c-mens-erro = "Pedido WEB Efetiviado sem Itens, Imposs°vel importar Pedido... " + CHR(10) +
                            'ID: ' + STRING(peds_web.cliente_id).

       IF tt-param.l-batch = NO THEN 
          MESSAGE c-mens-erro
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

       IF tt-param.enviar-e-mail THEN
          RUN pi-envia-email-erro (INPUT c-mens-erro).

       ASSIGN peds_web.ind_sit_ped_web = 5
              peds_web.descr_rejeicao = c-mens-erro.

       NEXT.
    END.

    FIND repres WHERE 
         repres.cod-rep = peds_web.repres_id NO-LOCK NO-ERROR.
    IF NOT AVAIL repres THEN NEXT.

    IF tt-param.l-batch = NO THEN
       RUN pi-acompanhar IN h-acomp (INPUT "Pedido: " + STRING(peds_web.ped_web_id) ).

    IF peds_web.nr_pedido_erp <> 0 THEN DO.
       FIND ped-venda WHERE
            ped-venda.nr-pedido = peds_web.nr_pedido_erp NO-LOCK NO-ERROR.
       IF AVAIL ped-venda THEN DO.
          ASSIGN peds_web.ind_sit_ped_web = 4.
          NEXT.
       END.
    END.

    FIND FIRST ped-venda-ext WHERE
               ped-venda-ext.cod-estab = peds_web.cod_estabel AND
               ped-venda-ext.ped_web_id = peds_web.ped_web_id NO-LOCK NO-ERROR.
    IF AVAIL ped-venda-ext THEN DO.
       FIND ped-venda WHERE
            ped-venda.nr-pedido = ped-venda-ext.nr-pedido NO-LOCK NO-ERROR.
       IF AVAIL ped-venda THEN DO.
          ASSIGN peds_web.nr_pedido_erp = ped-venda-ext.nr-pedido
                 peds_web.ind_sit_ped_web = 4.
          NEXT.
       END.

       FIND CURRENT ped-venda-ext EXCLUSIVE-LOCK NO-ERROR.
       DELETE ped-venda-ext.
    END.

    IF peds_web.nr_container <> 0 THEN DO.
       FIND pp-container WHERE
            pp-container.nr-container = peds_web.nr_container NO-LOCK NO-ERROR.
       IF NOT AVAIL pp-container THEN DO.
          ASSIGN c-mens-erro = "Container Informado n∆o foi Encontrado no Sistema, Imposs°vel importar Pedido..." + CHR(10) +
                               'CONTAINER: ' + STRING(peds_web.nr_container) + CHR(10) +
                               'ID: ' + STRING(peds_web.cliente_id).

          IF tt-param.l-batch = NO THEN 
             MESSAGE c-mens-erro
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.

          IF tt-param.enviar-e-mail THEN
             RUN pi-envia-email-erro (INPUT c-mens-erro).

          ASSIGN peds_web.ind_sit_ped_web = 5
                 peds_web.descr_rejeicao = c-mens-erro.

          NEXT.
       END.

       IF pp-container.situacao <> 1 THEN DO.
          ASSIGN c-mens-erro = "Situaá∆o do Container Informado n∆o est† Dispon°vel, Imposs°vel importar Pedido..." + CHR(10) +
                               'CONTAINER: ' + STRING(peds_web.nr_container) + CHR(10) +
                               'ID: ' + STRING(peds_web.cliente_id).

          IF tt-param.l-batch = NO THEN 
             MESSAGE c-mens-erro
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.

          IF tt-param.enviar-e-mail THEN
             RUN pi-envia-email-erro (INPUT c-mens-erro).

          ASSIGN peds_web.ind_sit_ped_web = 5
                 peds_web.descr_rejeicao = c-mens-erro.

          NEXT.
       END.
    END.


    IF peds_web.login_preposto <> '' THEN DO.
       FIND user-web WHERE
            user-web.usuario = repres.nome-abrev AND
            user-web.tp-usuario = 5 AND 
            user-web.login = peds_web.login_preposto NO-LOCK.
       IF NOT AVAIL user-web THEN DO.
          ASSIGN c-mens-erro = "Preposto n∆o foi Encontrado no Sistema, Imposs°vel importar Pedido..." + CHR(10) +
                               'PREPOSTO: ' + peds_web.login_preposto + CHR(10) +
                               'ID: ' + STRING(peds_web.cliente_id).

          IF tt-param.l-batch = NO THEN 
             MESSAGE c-mens-erro
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.

          IF tt-param.enviar-e-mail THEN
             RUN pi-envia-email-erro (INPUT c-mens-erro).

          ASSIGN peds_web.ind_sit_ped_web = 5
                 peds_web.descr_rejeicao = c-mens-erro.

          NEXT.
       END.
    END.

    IF peds_web.log_novo_cliente THEN
       FIND emitente WHERE
            emitente.cgc = peds_web.cnpj_novo_cliente NO-LOCK NO-ERROR.
    ELSE
       FIND emitente WHERE  
   		    emitente.cod-emit = peds_web.cliente_id NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN DO.
       ASSIGN c-mens-erro = "Cliente n∆o foi Encontrado no Sistema, Imposs°vel importar Pedido... (2)" + CHR(10) +
                            'CNPJ: ' + peds_web.cnpj_novo_cliente + CHR(10) +
                            'ID: ' + STRING(peds_web.cliente_id).

       IF tt-param.l-batch = NO THEN 
          MESSAGE c-mens-erro
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

       IF tt-param.enviar-e-mail THEN
          RUN pi-envia-email-erro (INPUT c-mens-erro).

        ASSIGN peds_web.ind_sit_ped_web = 5
               peds_web.descr_rejeicao = c-mens-erro.

       NEXT.
    END.

    IF peds_web.log_operac_triang THEN DO.
       IF peds_web.log_novo_cliente_triang THEN
          FIND b-emitente WHERE
               b-emitente.cgc = peds_web.cnpj_novo_cliente_triang NO-LOCK NO-ERROR.
       ELSE
          FIND b-emitente WHERE
               b-emitente.cod-emit = peds_web.cliente_triang_id NO-LOCK NO-ERROR.

       IF NOT AVAIL b-emitente THEN DO.
          ASSIGN c-mens-erro = "Cliente Triangular n∆o foi Encontrado no Sistema, Imposs°vel importar Pedido... (3)" + CHR(10) +
                               'CNPJ: ' + peds_web.cnpj_novo_cliente_triang + CHR(10) +
                               'ID: ' + STRING(peds_web.cliente_triang_id).

          IF tt-param.l-batch = NO THEN 
             MESSAGE c-mens-erro
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.

          IF tt-param.enviar-e-mail THEN
             RUN pi-envia-email-erro (INPUT c-mens-erro).

          ASSIGN peds_web.ind_sit_ped_web = 5
                 peds_web.descr_rejeicao = c-mens-erro.

          NEXT.
       END.
    END.


    // Transportadora
    IF peds_web.transp_id = 0 THEN 
       ASSIGN peds_web.transp_id = 50.

    FIND transporte WHERE 
         transporte.cod-transp = peds_web.transp_id NO-LOCK NO-ERROR.
    IF NOT AVAIL transporte THEN NEXT.

    IF peds_web.transp_redesp_id <> 0 THEN
       FIND b-transporte WHERE 
            b-transporte.cod-transp = peds_web.transp_redesp_id NO-LOCK NO-ERROR.

    IF peds_web.cod_tipo_frete <> 3 AND  // N∆o Ç FOB, busca Tansp. do Cliente
       emitente.cod-transp <> 0 THEN
       FIND transporte WHERE 
            transporte.cod-transp = emitente.cod-transp NO-LOCK NO-ERROR.


    // Moeda
    IF peds_web.cod_moeda = 'Real' OR  peds_web.cod_moeda = '1' THEN
       ASSIGN i-moeda = 0.
    ELSE
       ASSIGN i-moeda = 3.

    FIND moeda WHERE
         moeda.mo-codigo = i-moeda NO-LOCK NO-ERROR.
    IF NOT AVAIL moeda THEN NEXT.
    
    FOR EACH tt-ped-venda.
        DELETE tt-ped-venda.
    END.

    CREATE tt-ped-venda.
    ASSIGN tt-ped-venda.cod-estabel = peds_web.cod_estabel
           tt-ped-venda.nr-pedido = IF peds_web.nr_pedido_erp <> 0
                                    THEN peds_web.nr_pedido_erp
                                    ELSE NEXT-VALUE(seq-nr-pedido)
           tt-ped-venda.nr-pedcli = STRING(tt-ped-venda.nr-pedido)
           tt-ped-venda.nome-abrev = emitente.nome-abrev
           tt-ped-venda.cod-portador = emitente.portador
           tt-ped-venda.modalidade = emitente.modalidade
           tt-ped-venda.per-max-canc = 99
           tt-ped-venda.cod-entrega = emitente.cod-entrega
           tt-ped-venda.dt-userimp = TODAY
           tt-ped-venda.observacoes = peds_web.comentario
           tt-ped-venda.char-1 = STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM") + ' DIGITAÄ«O PORTAL'.

    // Busca Finalizade de Venda e Natureza de Operaá∆o
    RUN limparErros IN h-bonat001.
    RUN retornarfinalidadecliente IN h-bonat001 (INPUT emitente.cod-emitente,
                                                 OUTPUT i-fin-nat, 
                                                 OUTPUT c-cnae).
    RUN retornarerros IN h-bonat001 (OUTPUT c-erro-nat).
    IF c-erro-nat <> '' THEN DO.
       ASSIGN c-mens-erro = "Erro ao buscar a Finalidade do Cliente " + CHR(10) + 
                            c-erro-nat.

       ASSIGN peds_web.ind_sit_ped_web = 5
              peds_web.descr_rejeicao = c-mens-erro.

       IF tt-param.l-batch = NO THEN 
          MESSAGE c-mens-erro
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

       IF tt-param.enviar-e-mail THEN
          RUN pi-envia-email-erro (INPUT c-mens-erro).

       NEXT.
    END.

    IF NOT peds_web.log_operac_triang THEN DO.
       ASSIGN tt-ped-venda.cidade = emitente.cidade
              tt-ped-venda.estado = emitente.estado.

       RUN pi-calc-natur-oper (INPUT emitente.cod-emit,
                               INPUT "").

       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    END.
    ELSE DO.
       ASSIGN tt-ped-venda.nome-abrev-tri = b-emitente.nome-abrev
              tt-ped-venda.cidade = emitente.cidade
              tt-ped-venda.estado = emitente.estado.

       RUN pi-calc-natur-oper (INPUT emitente.cod-emit,
                               INPUT b-emitente.nome-abrev).

       IF RETURN-VALUE = 'ADM-ERROR' THEN NEXT.
    END.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = c-natur-oper NO-LOCK NO-ERROR.

    IF c-seg-usuario = 'tcassimiro' THEN
        MESSAGE   'nat:' c-natur-oper
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    // Desconto e Prioridade
    ASSIGN c-pct-desconto = ''.
    CASE peds_web.cod_prioridade.
        WHEN 15 THEN 
           ASSIGN c-pct-desconto = '50'.
        WHEN 16 THEN 
           ASSIGN c-pct-desconto = '40'.
        WHEN 17 THEN 
           ASSIGN c-pct-desconto = '30'.
        WHEN 18 THEN 
           ASSIGN c-pct-desconto = '20'.
    END CASE.

    ASSIGN tt-ped-venda.tp-preco       = 1
           tt-ped-venda.des-pct-descon = c-pct-desconto
           tt-ped-venda.cod-priori     = peds_web.cod_prioridade.

    IF peds_web.log_a_vista THEN
       FIND cond-pagto WHERE
            cond-pagto.cod-cond-pag = 1 NO-LOCK NO-ERROR.
    ELSE
       FIND cond-pagto WHERE
            cond-pagto.cod-cond-pag = peds_web.cond_pagto_id NO-LOCK NO-ERROR.

    // Demais campos
    ASSIGN tt-ped-venda.cod-mensagem   = natur-oper.cod-mensagem
           tt-ped-venda.cod-cond-pag   = 0
           tt-ped-venda.mo-codigo      = moeda.mo-codigo
           tt-ped-venda.no-ab-reppri   = repres.nome-abrev
           tt-ped-venda.cod-cond-pag   = IF AVAIL cond-pagto
                                         THEN cond-pagto.cod-cond-pag ELSE 0
           tt-ped-venda.nr-tab-finan   = IF AVAIL cond-pagto 
                                         THEN cond-pagto.nr-tab-fin ELSE 1
           tt-ped-venda.nr-ind-finan   = IF AVAIL cond-pagto
                                         THEN cond-pagto.nr-ind-fin ELSE 1
           tt-ped-venda.tp-pedido      = peds_web.cod_tipo_pedido
           tt-ped-venda.nome-transp    = transporte.nome-abrev
           tt-ped-venda.nome-tr-red    = IF AVAIL b-transporte
                                         THEN b-transporte.nome-abrev ELSE ""
           tt-ped-venda.ind-fat-par    = NO
           tt-ped-venda.dsp-pre-fat    = YES
           tt-ped-venda.ind-lib-nota   = YES.

    IF peds_web.cod_prioridade = 0 THEN
       ASSIGN peds_web.cod_prioridade = 10.

    IF peds_web.cod_tipo_frete =  1 OR 
       peds_web.cod_tipo_frete =  2 THEN 
       ASSIGN tt-ped-venda.cidade-cif = tt-ped-venda.cidade
              tt-ped-venda.ind-tp-frete = 1.
    ELSE
       ASSIGN tt-ped-venda.cidade-cif = ''
              tt-ped-venda.ind-tp-frete = 2.

    // Cria o Pedido
    ASSIGN c-mens-erro = "".
    RUN esapi/cria-pedvenda-portal.p (INPUT TABLE tt-ped-venda,
                                      OUTPUT c-mens-erro).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
       ASSIGN c-mens-erro = 'Erro ao Criar o Pedido...' + CHR(10) + 
                             c-mens-erro.

       ASSIGN peds_web.ind_sit_ped_web = 5
              peds_web.descr_rejeicao = c-mens-erro.

       IF tt-param.l-batch = NO THEN 
          MESSAGE c-mens-erro
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

       IF tt-param.enviar-e-mail THEN
          RUN pi-envia-email-erro (INPUT c-mens-erro).

       NEXT.
    END.

    FIND ped-venda WHERE
         ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli AND
         ped-venda.nome-abrev = tt-ped-venda.nome-abrev SHARE-LOCK NO-ERROR.

    RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                   INPUT tt-ped-venda.nome-abrev,
                                   INPUT "Implantado o Pedido WEB: " + tt-ped-venda.nr-pedcli + "   Cliente:" + tt-ped-venda.nome-abrev,
                                   INPUT NO).


    // Crua extens∆o do Pedido de Venda
    FIND ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido SHARE-LOCK NO-ERROR.

    CREATE ped-venda-ext.
    ASSIGN ped-venda-ext.cod-estabel  = ped-venda.cod-estabel 
           ped-venda-ext.nr-pedido    = ped-venda.nr-pedido      
           ped-venda-ext.nome-abrev   = ped-venda.nome-abrev
           ped-venda-ext.l-etiqueta   = NO
           ped-venda-ext.nr-pedcli    = ped-venda.nr-pedcli   
           ped-venda-ext.nr-container = peds_web.nr_container
           ped-venda-ext.ped_web_id   = peds_web.ped_web_id 
           ped-venda-ext.origem       = 5  // ImaOnline
           ped-venda-ext.tp-pagto     = 'Normal'
           ped-venda-ext.preposto     = peds_web.login_preposto
           ped-venda-ext.cod_finalidade_venda = i-fin-nat.
    
    ASSIGN ped-venda-ext.tp-pedido = 'Normal'.
    IF peds_web.log_a_vista THEN DO.
       ASSIGN ped-venda-ext.tp-pedido = "∑ Vista"
              ped-venda-ext.tp-pagto  = 'Caixa'.

       ASSIGN ped-venda.cod-portador = 99
              ped-venda.modalidade = 6.
    END.

    // Calucla Tipo de Frete
    CASE peds_web.cod_tipo_frete:
        WHEN 1 THEN ASSIGN ped-venda-ext.tp-frete = "Cif Total".
        WHEN 2 THEN ASSIGN ped-venda-ext.tp-frete = "Cif atÇ Redesp".
        WHEN 3 THEN ASSIGN ped-venda-ext.tp-frete = "Fob Total".
    END CASE.

    IF ped-venda.nome-abrev-tri <> "" THEN
       ASSIGN ped-venda-ext.tp-pedido = "Operaá∆o Triangular".

    // Trata Itens do Pedido
    ASSIGN c-mens-erro = ""
           l-erro = NO.

    ASSIGN i-nr-sequencia = 0.
    FOR EACH itens_ped_web WHERE
             itens_ped_web.ped_web_id = peds_web.ped_web_id AND 
             itens_ped_web.ind_sit_itens_ped_web <> 3 NO-LOCK.

        ASSIGN i-nr-sequencia = i-nr-sequencia + 10.

        IF itens_ped_web.vl_informado <> 0 THEN
           ASSIGN de-vl-preori = itens_ped_web.vl_informado.
        ELSE IF itens_ped_web.vl_unit_tabela <> 0 THEN
           ASSIGN de-vl-preori = itens_ped_web.vl_unit_tabela.
        ELSE IF itens_ped_web.vl_unit_final <> 0 THEN
           ASSIGN de-vl-preori = itens_ped_web.vl_unit_final.

        IF de-vl-preori = 0 THEN DO.
           IF c-mens-erro = '' THEN
              ASSIGN c-mens-erro = 'Item: ' + itens_ped_web.it_codigo + 'Ref.: ' + itens_ped_web.cod_refer + ' Sem Preáo Informado'.
           ELSE
              ASSIGN c-mens-erro = c-mens-erro + CHR(10) +
                                   'Item: ' + itens_ped_web.it_codigo + 'Ref.: ' + itens_ped_web.cod_refer + ' Sem Preáo Informado'.

           ASSIGN l-erro = YES.
           NEXT.
        END.

        RUN esapi/cria-peditem-portal.p (INPUT tt-ped-venda.nr-pedcli,
                                         INPUT i-nr-sequencia,
                                         INPUT itens_ped_web.it_codigo,
                                         INPUT itens_ped_web.cod_refer,
                                         INPUT itens_ped_web.qt_pedida,
                                         INPUT de-vl-preori,
                                         OUTPUT c-mens-erro-it).

        IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
           ASSIGN c-mens-erro = 'ERRO ao Implantar o Item: ' + itens_ped_web.it_codigo +  ' Ref.: ' + itens_ped_web.cod_refer + CHR(10) + CHR(10) + 
                                c-mens-erro-it.

           ASSIGN l-erro = YES.
           NEXT.
        END.

        CREATE ped-item-ext.
        ASSIGN ped-item-ext.cod-estabel   = tt-ped-venda.cod-estabel 
               ped-item-ext.nome-abrev    = tt-ped-venda.nome-abrev
               ped-item-ext.nr-pedcli     = tt-ped-venda.nr-pedcli
               ped-item-ext.nr-sequencia  = i-nr-sequencia
               ped-item-ext.it-codigo     = itens_ped_web.it_codigo
               ped-item-ext.cod-refer     = itens_ped_web.cod_refer
               ped-item-ext.retirar-corte = NO
               ped-item-ext.reservado     = NO
               ped-item-ext.lote          = 'RP' + itens_ped_web.cod_refer.

        FIND FIRST liquida-ima WHERE
                   liquida-ima.cod-estabel = tt-ped-venda.cod-estabel AND
                   liquida-ima.it-codigo = ped-item-ext.it-codigo AND
                   liquida-ima.cod-refer = ped-item-ext.cod-refer AND 
                   liquida-ima.dt-final = ? 
                   NO-LOCK NO-ERROR.
        IF AVAIL liquida-ima THEN
           ASSIGN ped-item-ext.liquida-ima = YES
                  ped-item-ext.num-id-liquida-ima = liquida-ima.num-id-liquida-ima.
    END.  // Criou os Itens

    IF l-erro = YES THEN DO.
       ASSIGN peds_web.ind_sit_ped_web = 5
              peds_web.descr_rejeicao = c-mens-erro.

       FOR EACH ped-item OF ped-venda.
           RUN esapi/elimina-peditem.p (INPUT ped-venda.nr-pedcli,
                                        INPUT ped-item.nr-sequencia).
       END.
       FIND FIRST ped-item OF ped-venda NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-item THEN DO.
          FIND CURRENT ped-venda EXCLUSIVE-LOCK NO-ERROR.
          DELETE ped-venda.
       END.

       IF tt-param.l-batch = NO THEN 
          MESSAGE c-mens-erro
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

       IF tt-param.enviar-e-mail THEN
          RUN pi-envia-email-erro (INPUT c-mens-erro).

       NEXT. 
    END.


    /* Cria Condiá∆o de Pagamento Especial */
    EMPTY TEMP-TABLE tt-cond-ped.
    IF NOT AVAIL cond-pagto AND 
       peds_web.cond_pagto_id = 0 AND 
       peds_web.dias_cond_pagto_esp <> '' THEN DO.
       ASSIGN i-nr-sequencia = 0.
       DO i-cont = 1 TO NUM-ENTRIES(peds_web.dias_cond_pagto_esp).
          ASSIGN i-nr-sequencia = i-nr-sequencia + 10.
    
          CREATE tt-cond-ped.
          ASSIGN tt-cond-ped.nr-pedido = tt-ped-venda.nr-pedido
                 tt-cond-ped.nr-sequencia = i-nr-sequencia 
                 tt-cond-ped.nr-dias-venc = INTEGER(ENTRY(i-cont,peds_web.dias_cond_pagto_esp)).
       END.
       RUN pi-perc-pagto (INPUT INT(NUM-ENTRIES(peds_web.dias_cond_pagto_esp))).
    
       ASSIGN c-mens-erro = "".
       FIND FIRST tt-cond-ped NO-ERROR.
       IF AVAIL tt-cond-ped THEN DO.
          RUN pi-acompanhar IN h-acomp (INPUT "Criando Condiá∆o Especial de Pagamento...").
          RUN openQueryStatic IN h-bodi018 (INPUT "Main":U).
          RUN emptyRowErrors IN h-bodi018.
          FOR EACH tt-cond-ped NO-LOCK.
              FOR EACH wt-cond-ped.
                  DELETE wt-cond-ped.
              END.
              CREATE wt-cond-ped.
              BUFFER-COPY tt-cond-ped TO wt-cond-ped.
    
              RUN setRecord IN h-bodi018 (INPUT TABLE wt-cond-ped).
              RUN createRecord IN h-bodi018.
          END.
          RUN getRowErrors IN h-bodi018 (OUTPUT TABLE RowErrors).
          IF CAN-FIND(FIRST RowErrors 
                      WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

             ASSIGN c-mens-erro = "Erro ao Criar Condiá∆o de Pagamento Especial - ERRO: " + CHR(10).
             FOR EACH rowerrors WHERE
                      RowErrors.ErrorSubType = "ERROR":U:
                 ASSIGN c-mens-erro = c-mens-erro + rowerrors.errordescription.
             END.
             ASSIGN c-mens-erro = c-mens-erro + CHR(10) +
                                  'Verifique se o Cliente pode Comprar a Prazo'.

             ASSIGN peds_web.ind_sit_ped_web = 5
                    peds_web.descr_rejeicao = c-mens-erro.
    
             FOR EACH ped-item OF ped-venda.
                 RUN esapi/elimina-peditem.p (INPUT ped-venda.nr-pedcli,
                                              INPUT ped-item.nr-sequencia).
             END.
             FIND FIRST ped-item OF ped-venda NO-LOCK NO-ERROR.
             IF NOT AVAIL ped-item THEN
                DELETE ped-venda.

             IF tt-param.l-batch = NO THEN 
                MESSAGE c-mens-erro
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
             IF tt-param.enviar-e-mail THEN
                RUN pi-envia-email-erro (INPUT c-mens-erro).
    
             NEXT. 
          END.
       END.
    END.

    /* Cria Representantes do Pedido */
    ASSIGN c-mens-erro = "".
    RUN pi-cria-repres.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
       ASSIGN peds_web.ind_sit_ped_web = 5
              peds_web.descr_rejeicao = 'Erro ao Criar o Representante...' + CHR(10) +
                                        c-mens-erro.

       FOR EACH ped-item OF ped-venda.
           RUN esapi/elimina-peditem.p (INPUT ped-venda.nr-pedcli,
                                        INPUT ped-item.nr-sequencia).
       END.
       FIND FIRST ped-item OF ped-venda NO-LOCK NO-ERROR.
       IF NOT AVAIL ped-item THEN
          DELETE ped-venda.

       IF tt-param.l-batch = NO THEN 
          MESSAGE c-mens-erro
              VIEW-AS ALERT-BOX INFO BUTTONS OK.

       IF tt-param.enviar-e-mail THEN
          RUN pi-envia-email-erro (INPUT c-mens-erro).

       NEXT. 
    END.


    // Atualiza Entrega
    FOR EACH ped-ent OF ped-venda SHARE-LOCK.
        ASSIGN ped-ent.tipo-atend = 2.
    END.

    ASSIGN ped-venda.cod-sit-ped = 4.
    IF LOOKUP(ped-venda-ext.tp-pedido,c-tpped-cred-aut) > 0 THEN 
       ASSIGN ped-venda.cod-sit-ped = 1.

    IF tt-ped-venda.tp-pedido = 'PI' THEN
       ASSIGN ped-venda-ext.l-nao-aprovar = YES.
    
    IF peds_web.log_a_vista OR
       peds_web.dias_cond_pagto_esp = '' THEN
       ASSIGN ped-venda-ext.l-bloqueio = YES.
    
    RUN gravarparamnaturpedvenda IN h-bonat001 (INPUT i-param-nat,
                                                INPUT ped-venda.nr-pedido,
                                                INPUT ped-venda.cod-estabel,
                                                OUTPUT l-ok).

	/* Valida CrÇdito do Pedido*/
    RUN esapi/credito-pedvenda.p.

    /* Validar Desconto*/
    IF ped-venda.des-pct-desconto-inform <> "" AND 
       ped-venda.ind-sit-desconto <> 2 THEN DO. /* Tem Desconto e n∆o foi A*/
       ASSIGN ped-venda.ind-sit-desconto = 1.
       RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                      INPUT ped-venda.nome-abrev,
                                      INPUT "Desconto Informado, Pedido Requer Aprovaá∆o",
                                      INPUT NO).
    END.

    /* Valida Frete */
    ASSIGN ped-venda.cod-sit-com = 2.
    RUN esapi/valida-frete-cif.p (INPUT ped-venda.nr-pedcli,
                                  OUTPUT l-ok).
    IF l-ok = NO THEN DO.
       RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                      INPUT ped-venda.nome-abrev,
                                      INPUT "Valor do Pedido Inv†lido para Frete CIF, Pedido Requer Aprovaá∆o",
                                      INPUT NO).
    END.

    /* Valida Preáo */
    ASSIGN ped-venda.log-ped-bonif-pendente = NO
           ped-venda.cod-sit-preco = 2.
    IF ped-venda.tp-preco = 1 THEN DO.
       RUN esapi/valida-preco.p (INPUT ped-venda.nr-pedcli,
                                OUTPUT l-ok).
       IF l-ok = NO THEN DO.
          RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                         INPUT ped-venda.nome-abrev,
                                         INPUT "Preáo Inv†lido, Pedido Requer Aprovaá∆o",
                                         INPUT NO).
       END.
    END.

    /* Valida Comiss∆o */
    FIND ped-repre WHERE
         ped-repre.nr-pedido = ped-venda.nr-pedido AND
         ped-repre.nome-ab-rep = ped-venda.no-ab-reppri SHARE-LOCK NO-ERROR.
    IF AVAIL ped-repre THEN DO.
       IF ped-repre.perc-comis <> repres.comis-direta THEN DO.
          ASSIGN ped-repre.cod-classif = IF ped-repre.cod-classif = ""
                                         THEN "NAO_AVALIADO"
                                         ELSE ped-repre.cod-classif.

          RUN esapi/cria-log-pedvenda.p (INPUT tt-ped-venda.nr-pedcli,
                                         INPUT tt-ped-venda.nome-abrev,
                                         INPUT "Comiss∆o Inv†lida para o Representante, Pedido Requer Aprovaá∆o",
                                         INPUT NO).
       END.
       ELSE
          ASSIGN ped-repre.cod-classif = "".
    END.


    IF ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval = 4 THEN
       ASSIGN ped-venda.cod-sit-ped = 1
              ped-venda.quem-aprovou = ""
              ped-venda.dt-apr-cred = ?.

    //RUN pi-envia-email-pedido (INPUT "PDF").
    RUN pi-envia-email-pedido (INPUT "HTML").

    ASSIGN peds_web.ind_sit_ped_web = 4
           peds_web.nr_pedido_erp = tt-ped-venda.nr-pedido.
END.

IF tt-param.l-batch = NO THEN
   RUN pi-finalizar IN h-acomp.



/* ------ Procedures------ */
PROCEDURE pi-calc-natur-oper:
   DEF INPUT PARAMETER p-cod-emitente LIKE emitente.cod-emitente.
   DEF INPUT PARAMETER p-nome-abrev-tri LIKE emitente.nome-abrev. 
   
   RUN limparErros IN h-bonat001.
   RUN buscarnatoperacao IN h-bonat001 (INPUT i-fin-nat, 
                                        INPUT tt-ped-venda.cod-estabel,
                                        INPUT p-cod-emitente,
                                        INPUT p-nome-abrev-tri,  
                                        OUTPUT c-natur-oper,
                                        OUTPUT i-param-nat).
   RUN retornarerros IN h-bonat001 (OUTPUT c-erro-nat).
   IF c-erro-nat <> '' THEN DO.
      ASSIGN peds_web.ind_sit_ped_web = 5
             peds_web.descr_rejeicao = c-erro-nat.

      IF tt-param.l-batch = NO THEN 
         MESSAGE c-erro-nat
             VIEW-AS ALERT-BOX INFO BUTTONS OK.

      IF tt-param.enviar-e-mail THEN
         RUN pi-envia-email-erro (INPUT c-erro-nat).

      RETURN 'ADM-ERROR'.
   END.
   ASSIGN tt-ped-venda.nat-operacao = c-natur-oper.

END PROCEDURE.

PROCEDURE pi-cria-repres.
    FIND ped-repre WHERE
         ped-repre.nr-pedido   = INT(tt-ped-venda.nr-pedcli) AND
         ped-repre.nome-ab-rep = tt-ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-repre THEN DO.
       ASSIGN de-comis-direta = repres.comis-direta.

       RUN esapi/cria-repres2.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                 INPUT tt-ped-venda.no-ab-reppri,
                                 INPUT peds_web.perc_comis,
							     OUTPUT TABLE tt-erros-local).
	   FIND FIRST tt-erros-local NO-LOCK NO-ERROR.	   
       IF AVAIL tt-erros-local THEN DO.
          FOR EACH tt-erros-local.
              ASSIGN c-mens-erro = 'Erro ao Criar o Representante..' + CHR(10) +
                                   STRING(tt-erros-local.cod-erro) + " " + tt-erros-local.desc-erro + " " + tt-erros-local.desc-arq.
          END.
       END.
    END.

    
    /* Verificar Desconto Informado */ 
    FIND ped-repre WHERE
         ped-repre.nr-pedido = INT(tt-ped-venda.nr-pedcli) AND
         ped-repre.nome-ab-rep = 'Fulano' NO-LOCK NO-ERROR.
    
    IF AVAIL ped-repre THEN
       RUN esapi/elimina-repres.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                   INPUT 'Fulano').
    
    IF tt-ped-venda.des-pct-desconto-inform <> "" THEN DO:   /* Cria Fulano */
       FIND ped-repre WHERE
            ped-repre.nr-pedido = INT(tt-ped-venda.nr-pedcli) AND
            ped-repre.nome-ab-rep = 'Fulano' NO-LOCK NO-ERROR.

       RUN esapi/cria-repres.p (INPUT INT(tt-ped-venda.nr-pedcli),
                                INPUT 'Fulano',
                                INPUT DEC(tt-ped-venda.des-pct-desconto-inform) ).

       IF RETURN-VALUE = 'ADM-ERROR' THEN 
          RETURN 'ADM-ERROR'.
    END.
    
END PROCEDURE.

PROCEDURE pi-perc-pagto :
    DEF INPUT PARAMETER p-qtd-seq AS INTEGER.

    ASSIGN i-ct = 1.
    FOR EACH tt-cond-ped. 
        ASSIGN tt-cond-ped.perc-pagto = 100 / p-qtd-seq
               i-ct = i-ct + 1.
    END.

    ASSIGN i-ct = i-ct - 1.
    FIND LAST tt-cond-ped NO-ERROR.
    IF AVAIL tt-cond-ped THEN
       IF i-ct * tt-cond-ped.perc-pagto <> 100 THEN
          ASSIGN tt-cond-ped.perc-pagto = tt-cond-ped.perc-pagto + 
                                           (100 - (i-ct * tt-cond-ped.perc-pagto)).
END PROCEDURE.


PROCEDURE pi-envia-email-erro.
   DEF INPUT PARAMETER c-mens-erro AS CHAR.

   // e-mail s¢ de 08:00h as 18:00h
   IF TIME < 28800 OR 
      TIME > 64800 THEN 
      RETURN 'ADM-ERROR'.

   FIND im-param WHERE
        im-param.cod-param = 'DEST_ERROS_PEDIDOS_WEB' NO-LOCK NO-ERROR.

   IF c-base = 'base-pro' AND AVAIL im-param THEN 
      ASSIGN c-destinatario = im-param.val-param + "," + repres.e-mail. 
   ELSE
      ASSIGN c-destinatario = "imatextil@imatextil.com.br". 

   ASSIGN c-mensagem = "Prezados," + CHR(13) + 
                       "O Pedido " + STRING(peds_web.ped_web_id) + " implantado no portal FOI REJEITADO em " + STRING(TODAY,"99/99/9999") + "." + CHR(13) +
                       "Motivo: " + c-mens-erro + CHR(13) + CHR(13) +
                       "Atenciosamente, " + CHR(13) + CHR(13)  +
                       "Medtextil Importaá∆o e Exportaá∆o Ltda".

   IF tt-param.l-batch = YES THEN 
      RUN esapi/esapi002.p (INPUT c-remetente,                         /* e-mail remetente */
                            INPUT c-destinatario,                      /* e-mail destinat†rio */
                            INPUT "Rejeiá∆o de PEDIDO - " + STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS"), /* Assunto */
                            INPUT c-mensagem,                          /* Mensagem */
                            INPUT "",                                  /* Anexo */
                            INPUT YES).                                /* Mostra Erros */ 
   ELSE
      MESSAGE c-mensagem
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.


PROCEDURE pi-envia-email-pedido.
    DEF INPUT PARAMETER p-tipo AS CHAR.

    FIND emitente WHERE
         emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
    
    FIND cont-emit OF emitente WHERE
         cont-emit.area = 'COMERCIAL' NO-LOCK NO-ERROR.
    IF NOT AVAIL cont-emit THEN RETURN 'ADM-ERROR'.

    FIND repres WHERE
         repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.

    IF c-base = 'base-pro' THEN DO.
       IF repres.e-mail <> '' THEN
          ASSIGN c-destinatario = repres.e-mail.

       IF cont-emit.e-mail <> '' THEN
          ASSIGN c-destinatario = c-destinatario + "," + cont-emit.e-mail.
    
       IF AVAIL peds_web THEN DO.
          IF peds_web.emails_adicionais <> '' THEN
             ASSIGN c-destinatario = c-destinatario + "," + peds_web.emails_adicionais.
    
          IF peds_web.login_preposto <> '' AND user-web.email <> '' THEN
             ASSIGN c-destinatario = c-destinatario + "," + user-web.email.
       END.
    END.
    ELSE
       ASSIGN c-destinatario = "ti@imatextil.com.br". 

    ASSIGN c-mensagem = "Prezado Cliente," + CHR(13) + 
                        "Foi efetuado o Pedido de Venda " + ped-venda.nr-pedcli + " que segue em Anexo, pelo Representante " + repres.nome + CHR(13) +
                        "Atenciosamente, " + CHR(13) + CHR(13)  +
                        "Medtextil Importaá∆o e Exportaá∆o Ltda".


    IF p-tipo = 'PDF' THEN DO.
       ASSIGN c-arq-gerado-pdf = SESSION:TEMP-DIRECTORY + STRING(ped-venda.nr-pedcli) + ".pdf".
    
       RUN esapi/imp-ped-venda-pdf.p (INPUT ped-venda.nr-pedido,
                                      INPUT 2).

       IF SEARCH(c-arq-gerado-pdf) = ? THEN DO.
          ASSIGN c-mens-erro = "N∆o foi possivel Enviar o e-mail do Pedido (" +
                               ped-venda.nr-pedcli + ")... PDF n∆o Encontrado... ".
    
          IF tt-param.l-batch = NO THEN 
             MESSAGE c-mens-erro
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
          IF tt-param.enviar-e-mail THEN
             RUN pi-envia-email-erro (INPUT c-mens-erro).
    
          RETURN 'ADM-ERROR'.
       END.

       RUN esapi/esapi002.p (INPUT c-remetente,                         /* e-mail remetente */
                             INPUT c-destinatario,                      /* e-mail destinat†rio */
                             INPUT "Pedido de Venda - IMATEXIL",        /* Assunto */
                             INPUT c-mensagem,                          /* Mensagem */
                             INPUT c-arq-gerado-pdf,                    /* Anexo */
                             INPUT YES).                                /* Mostra Erros */ 
    END.
    ELSE IF p-tipo = 'HTML' THEN DO.
       RUN esapi/imp-ped-venda-html.p (INPUT ped-venda.nr-pedido,
                                       OUTPUT TABLE tt-mensagem).
                                       
       RUN esapi/esapi003.p (INPUT c-remetente,                         /* e-mail remetente */
                             INPUT c-destinatario,                      /* e-mail destinat†rio */
                             INPUT "Pedido de Venda - IMATEXIL",        /* Assunto */
                             INPUT TABLE tt-mensagem,                   /* Mensagem */
                             INPUT '',                                  /* Anexo */
                             INPUT YES).                                /* Mostra Erros */ 
    END.
END PROCEDURE.

PROCEDURE pi-act-sld-container.
    FOR EACH pp-container WHERE
             pp-container.situacao = 1 NO-LOCK.
    
        FOR EACH pp-it-container OF pp-container SHARE-LOCK.
            ASSIGN de-qt-vend = 0.
            RUN pi-ver-ped.
    
            IF de-qt-vend = pp-it-container.qt-vendida THEN NEXT.
    
            ASSIGN pp-it-container.qt-vendida = de-qt-vend.
        END.
    END.
END PROCEDURE.

PROCEDURE pi-ver-ped.
    FOR EACH ped-item WHERE
             ped-item.it-codigo = pp-it-container.it-codigo AND
             ped-item.cod-refer = pp-it-container.cod-refer NO-LOCK.

        IF ped-item.cod-sit-item = 6 THEN NEXT.

        FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda THEN NEXT.
        IF ped-venda.cod-sit-ped = 6 THEN NEXT.
             
        FIND ped-venda-ext WHERE
             ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
             ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-venda-ext THEN NEXT.

        IF ped-venda-ext.nr-container <> pp-container.nr-container THEN NEXT.

        ASSIGN de-qt-vend = de-qt-vend + ped-item.qt-pedida.
    END.
END PROCEDURE.
