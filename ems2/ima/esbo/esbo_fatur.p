/******************************************************************************************************************
Programa: esbo/bo_fatur.p
Objetivo: Extrair os dados de faturamento, devolu‡Æo e metas para tabelas tempor rias
Autor: Tadeu Silva
Data: Sem Data Inicial Registrada
Altera‡Æo: 11/01/2024 - tadeu - acrescimo de filtros de estab, serie, nota e representante
           20/02/2024 - tadeu - acrescimo dos campos de tabela de pre‡o e rubix que ‚ preenchido quando o registro
           ‚ de faturamento.
*******************************************************************************************************************/


{esbo\esbo_fatur.i}
{esp/util.i}
DEF VAR i-tot-etq              AS INT.
DEF VAR da-dt-emis-nota-ini    AS DATE INIT 01.01.0001.
DEF VAR da-dt-emis-nota-fin    AS DATE INIT 12.31.2099.
DEF VAR i-ct                   AS INT.
DEF VAR i-prz                  AS INT.
DEF VAR c-cond-pagto           AS CHAR.
DEF VAR i-cod-vencto           AS INT.
DEF VAR l-outros-fat           AS LOG INIT NO.
DEF VAR c-regiao               AS CHAR.
DEF VAR de-qtd                 AS DEC.
DEF VAR de-vlr                 AS DEC.
DEF VAR de-desc                AS DEC.
DEF VAR de-vlr-custo           AS DEC.
DEF VAR rep_ini                AS INT INIT 0.
DEF VAR rep_fim                AS INT INIT 9999999.
DEFINE VARIABLE cSerieIni      AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE cSerieFim      AS CHARACTER   NO-UNDO INIT 'zzzzz' .
DEFINE VARIABLE cNotaIni       AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE cNotaFim       AS CHARACTER   NO-UNDO INIT 'zzzzzzzzzzzzz'.
DEFINE VARIABLE cEstabIni      AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE cEstabFim      AS CHARACTER   NO-UNDO INIT 'zzzzz'.
DEFINE VARIABLE LOG12          AS LOGICAL     NO-UNDO.
                                           

DEFINE VARIABLE hBoRepres       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoOpcaoLista   AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoEmitente     AS HANDLE      NO-UNDO.
DEFINE VARIABLE logRetirarAcomp AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cMsg            AS CHARACTER   NO-UNDO.
{utp/ut-glob.i}

FUNCTION calcVlCom12 RETURNS DECIMAL(valor AS DECIMAL, percDescPed AS DECIMAL):

    DEFINE VARIABLE dRetorno AS DECIMAL     NO-UNDO.
    IF (1 - percDescPed) > 0 AND percDescPed <> 0 THEN
       ASSIGN dRetorno = valor / (1 - percDescPed).
    ELSE
       ASSIGN dRetorno = valor.

    /*IF c-seg-usuario = 'super' THEN  DO:
       MESSAGE 'vl.original:' valor SKIP
       'perc.desconto:' percDescPed SKIP
       'retorno:' dRetorno
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.    
        
    END.   */
    
    
    RETURN  dRetorno . 


END FUNCTION.


PROCEDURE iniciarBos:

     IF NOT VALID-HANDLE(hBoRepres) THEN
        RUN esbo/boRepres.p PERSISTENT SET hBoRepres.
     IF NOT VALID-HANDLE(hBoEmitente) THEN
        RUN esbo/boEmitente.p PERSISTENT SET hBoEmitente.
     IF NOT VALID-HANDLE(hBoOpcaoLista) THEN
        RUN esbo/boOpcaoLista.p PERSISTENT SET hBoOpcaoLista.

    

END PROCEDURE.


PROCEDURE finalizarBos:  

     IF VALID-HANDLE(hBoEmitente) THEN
       DELETE PROCEDURE hBoEmitente.
     IF VALID-HANDLE(hBoRepres) THEN
       DELETE PROCEDURE hBoRepres.
     IF VALID-HANDLE(hBoOpcaoLista) THEN
        DELETE PROCEDURE hBoOpcaoLista.

END PROCEDURE.

PROCEDURE limparTtFatur:

    EMPTY TEMP-TABLE tt-fatur.

END PROCEDURE.


PROCEDURE setIntervalRepres:

    DEFINE INPUT  PARAMETER pRepresIni AS INT        NO-UNDO.
    DEFINE INPUT  PARAMETER pRepresFim AS INT        NO-UNDO.
    ASSIGN rep_ini = pRepresIni
           rep_fim = pRepresFim.

END PROCEDURE.

PROCEDURE setIntervalDtEmisNota:

    DEFINE INPUT  PARAMETER pDataIni AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pDataFim AS DATE        NO-UNDO.
    ASSIGN da-dt-emis-nota-ini = pDataIni
           da-dt-emis-nota-fin = pDataFim.


END PROCEDURE.

PROCEDURE setIntervalSerie:

    DEFINE INPUT  PARAMETER pSerieIni  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pSerieFim  AS CHARACTER   NO-UNDO.

    ASSIGN cSerieIni = pSerieIni
           cSerieFim = pSerieFim.


END PROCEDURE.


PROCEDURE setIntervalEstab:

    DEFINE INPUT  PARAMETER pEstabIni  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEstabFim  AS CHARACTER   NO-UNDO.

    ASSIGN cEstabIni = pEstabIni
           cEstabFim = pEstabFim .

END PROCEDURE.


PROCEDURE setIntervalNota:

    DEFINE INPUT  PARAMETER pNotaIni  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNotaFim  AS CHARACTER   NO-UNDO.

    ASSIGN cNotaIni = pNotaIni
           cNotaFim = pNotaFim.

END PROCEDURE.





PROCEDURE getPercDescPed:

    DEFINE INPUT  PARAMETER rRowid AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER percDesc AS DECIMAL  INIT 0   NO-UNDO.
    FOR FIRST ped-venda FIELDS( cod-estabel nr-pedido nr-pedcli nome-abrev val-desconto-total vl-tot-ped val-desconto-total) WHERE ROWID(ped-venda) = rRowid NO-LOCK.
    END.
    IF AVAIL ped-venda THEN DO:
        ASSIGN percDesc = ped-venda.val-desconto-total / (ped-venda.vl-tot-ped + ped-venda.val-desconto-total).
    END.
        

END PROCEDURE.

PROCEDURE retirarAcomp:

    DEFINE INPUT  PARAMETER pRetirar AS LOGICAL     NO-UNDO.
    ASSIGN logRetirarAcomp = pRetirar.

END PROCEDURE.

PROCEDURE _getDescTbPreco:

    DEFINE INPUT  PARAMETER pTbPrecoId AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cDescricao AS CHARACTER   NO-UNDO.

    FOR FIRST tbs_preco FIELDS(tb_preco_id descricao)
         WHERE tbs_preco.tb_preco_id = pTbPrecoId NO-LOCK.
    END.         
    IF AVAIL tbs_preco THEN DO:
       ASSIGN cDescricao = tbs_preco.descricao .
    END.

END PROCEDURE.

PROCEDURE _getPercComisReppri:
    DEFINE INPUT  PARAMETER nomeAbrevRep AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER dPerc        AS DECIMAL     NO-UNDO.

    FOR FIRST ped-repre FIELDS(nome-ab-rep perc-comis) OF ped-venda 
         WHERE ped-repre.nome-ab-rep = ped-venda.no-ab-reppri NO-LOCK .
    END.     
    IF AVAIL ped-repre THEN
       ASSIGN dperc = ped-repre.perc-comis .

END PROCEDURE.


PROCEDURE set12:
    DEFINE INPUT  PARAMETER PL12 AS LOGICAL     NO-UNDO.
    ASSIGN log12 = PL12.    
    
END PROCEDURE.

PROCEDURE buscarFaturados.


DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.

RUN utp/ut-acomp.p PERSIST SET h-acomp.
RUN pi-inicializar IN h-acomp(INPUT "Buscando NFs Venda").

 DEFINE VARIABLE percDescPed AS DECIMAL  NO-UNDO.

 ASSIGN i-tot-etq = 0.
 FOR EACH nota-fiscal FIELDS( cod-emitente dt-emis-nota cod-estabel serie nr-nota-fis nr-pedcli nome-abrev nome-ab-cli no-ab-reppri nat-operacao cod-cond-pag nr-fatura cidade estado cod-rep)
          //USE-INDEX ch-distancia 
          USE-INDEX nfftrm-20
          WHERE      
          nota-fiscal.dt-emis-nota >= da-dt-emis-nota-ini AND 
          nota-fiscal.dt-emis-nota <= da-dt-emis-nota-fin AND
          nota-fiscal.cod-estabel  >= cEstabIni AND
          nota-fiscal.cod-estabel  <= cEstabFim AND
          nota-fiscal.serie        >= cSerieIni AND
          nota-fiscal.serie        <= cSerieFim AND
          nota-fiscal.nr-nota-fis  >= cNotaIni  AND
          nota-fiscal.nr-nota-fis  <= cNotaFim  AND
          nota-fiscal.cod-rep >= rep_ini AND
          nota-fiscal.cod-rep <= rep_fim AND
          nota-fiscal.dt-cancela    = ? 
         NO-LOCK ,FIRST natur-oper FIELDS(nat-operacao denominacao tipo tp-rec-desp tipo-compra) OF nota-fiscal NO-LOCK
         WHERE natur-oper.tp-rec-desp = 1 
         AND  natur-oper.tipo-compra <> 3
         AND  natur-oper.tipo <> 1 //movto entrada
           .
     //IF NOT AVAIL natur-oper THEN NEXT.
    
     IF logRetirarAcomp = NO THEN
        RUN pi-acompanhar IN h-acomp(string(nota-fiscal.dt-emis-nota) + "-" + Nota-fiscal.nr-nota-fis).

     FOR FIRST ped-venda FIELDS(cod-estabel nr-pedido nr-pedcli nome-abrev dt-implant no-ab-reppri) 
         WHERE ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli  
         AND   ped-venda.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK.
     END.
     IF AVAIL ped-venda THEN DO:
        FOR FIRST ped-venda-ext FIELDS(tp-frete tb_preco_id l_tab_x nr-container)
            WHERE ped-venda-ext.cod-estabel = ped-venda.cod-estabel
            AND   ped-venda-ext.nr-pedido   = ped-venda.nr-pedido
         NO-LOCK .
        END.           
     END.
     RUN getPercDescPed(ROWID(ped-venda), OUTPUT percDescPed).    
     
     IF CAN-FIND(FIRST estabelec WHERE estabelec.cgc = nota-fiscal.cgc) THEN NEXT .
     

     IF nota-fiscal.nome-abrev-tri <> "" AND 
        nota-fiscal.nome-abrev-tri = nota-fiscal.nome-ab-cli THEN NEXT. /* Nota fiscal Triangular */               
     

     ASSIGN i-ct         = 0
            c-cond-pagto = ""
            i-cod-vencto = 0
            i-prz        = 0.

     FOR EACH fat-duplic WHERE
              fat-duplic.cod-estabel = nota-fiscal.cod-estabel AND
              fat-duplic.serie       = nota-fiscal.serie       AND
              fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK.
              ASSIGN i-prz        = i-prz + (fat-duplic.dt-vencimen - fat-duplic.dt-emissao)
                     i-ct         = i-ct + 1
                     i-cod-vencto = fat-duplic.cod-vencto.
     END.
     IF i-prz <> 0 OR i-cod-vencto = 2 OR i-cod-vencto = 9 THEN DO: 
        IF i-cod-vencto = 2 THEN
           ASSIGN c-cond-pagto = " A Vista".
        ELSE
        IF i-cod-vencto = 9 THEN
           ASSIGN c-cond-pagto = " Contra Apresenta‡Æo".
        ELSE DO:
           i-prz = INT(i-prz / i-ct).
           IF i-prz <= 30 THEN
              ASSIGN c-cond-pagto = " De 01 At‚ 30 Dias".
           ELSE
           IF i-prz <= 60 THEN
              ASSIGN c-cond-pagto = " De 31 At‚ 60 Dias".
           ELSE
           IF i-prz <= 90 THEN
              ASSIGN c-cond-pagto = " De 61 At‚ 90 Dias".
           ELSE
              ASSIGN c-cond-pagto = "ÿAcima de 90 Dias". /* 1¦ Pos ALT 164 */
        END.
     END.
     ELSE DO:
        FOR FIRST cond-pagto FIELDS(cod-cond-pag descricao log-2)
        WHERE  cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-LOCK .
        END.
        IF AVAIL cond-pagto THEN DO:
           ASSIGN c-cond-pagto = cond-pagto.descricao.
           IF cond-pagto.log-2 THEN /* Fat VENDOR */
              ASSIGN c-cond-pagto = "ÿVendor". /* 1¦ Pos ALT 164 */
        END.
     END.
     IF c-cond-pagto = "" THEN DO:
         IF l-outros-fat THEN
            ASSIGN c-cond-pagto = "Outros Faturamento".
         ELSE
            ASSIGN c-cond-pagto = " Cupom Fiscal".
     END.
     IF c-regiao = "Exporta‡Æo" THEN
        ASSIGN c-cond-pagto = "ÿExporta‡Æo". /* 1¦ Pos ALT 164 */

     ASSIGN de-qtd   = 0
            de-desc  = 0
            de-vlr   = 0.

     FOR EACH it-nota-fisc FIELDS(it-codigo char-2 cod-refer un-fatur nr-seq-fat qt-faturada vl-preuni vl-tot-item val-desconto-total) OF nota-fiscal NO-LOCK.
         
         IF NOT CAN-FIND(item WHERE  item.it-codigo = it-nota-fisc.it-codigo ) THEN NEXT.        
           
         
         /*FOR FIRST ped-item-ext fields( ) WHERE
              ped-item-ext.cod-estabel = ped-venda.cod-estabel  AND
              ped-item-ext.nr-pedcli = ped-venda.nr-pedcli      AND
              ped-item-ext.nome-abrev = ped-venda.nome-abrev    AND
              ped-item-ext.nr-sequencia = it-nota-fisc.nr-seq-ped NO-LOCK .*/

         IF nota-fiscal.nr-pedcli <> "" THEN DO:            
                 
            ASSIGN de-vlr-custo = 0.
            FOR FIRST pr-it-per FIELDS(it-codigo cod-estabel periodo val-unit-ggf-m[1] val-unit-mat-m[1] val-unit-mob-m[1]) 
            WHERE pr-it-per.it-codigo   = it-nota-fisc.it-codigo 
            AND   pr-it-per.cod-estabel = nota-fiscal.cod-estabel 
            AND   pr-it-per.periodo     = da-dt-emis-nota-fin  NO-LOCK .
            
            END.
            IF NOT AVAIL pr-it-per   THEN
               FOR FIRST pr-it-per FIELDS(it-codigo cod-estabel periodo val-unit-ggf-m[1] val-unit-mat-m[1] val-unit-mob-m[1]) WHERE
                         pr-it-per.it-codigo   = it-nota-fisc.it-codigo  AND  
                         pr-it-per.cod-estabel = nota-fiscal.cod-estabel NO-LOCK .
               END.
               IF AVAIL pr-it-per THEN
                  ASSIGN de-vlr-custo = pr-it-per.val-unit-ggf-m[1] +  
                                        pr-it-per.val-unit-mat-m[1] + 
                                        pr-it-per.val-unit-mob-m[1].              
            FOR FIRST repres FIELDS(nome-abrev cod-rep)
                WHERE repres.nome-abrev = nota-fiscal.no-ab-reppri NO-LOCK .
            END.
            IF NOT AVAIL repres THEN
               FOR FIRST repres FIELDS(cod-rep) 
                   WHERE repres.cod-rep = 1 NO-LOCK.
               END.
         END.
         ELSE DO:         
            FOR FIRST repres FIELDS(nome-abrev cod-rep) WHERE
                 repres.nome-abrev = nota-fiscal.no-ab-reppri NO-LOCK .
            END.                 
            IF NOT AVAIL repres THEN
               FOR FIRST repres FIELDS(nome-abrev cod-rep) 
                   WHERE repres.cod-rep = 1 NO-LOCK .
               END.
           
         END.
         ASSIGN de-qtd       = de-qtd  + it-nota-fisc.qt-faturada[1]
                de-vlr       = de-vlr  + it-nota-fisc.vl-tot-item  
                de-desc      = de-desc + IF dec(SUBSTR(it-nota-fisc.char-2,1500,10)) = 0 THEN it-nota-fisc.val-desconto-total ELSE dec(SUBSTR(it-nota-fisc.char-2,1500,10))
                de-vlr-custo = 0.


        CREATE tt-fatur.
        ASSIGN 
               tt-fatur.tipo            = "fat"
               tt-fatur.nr_nf           = nota-fiscal.nr-nota-fis
               tt-fatur.serie           = nota-fiscal.serie
               tt-fatur.cod_estabel     = nota-fiscal.cod-estabel
               tt-fatur.dt_emis         = nota-fiscal.dt-emis-nota
               tt-fatur.cod_cli         = nota-fiscal.cod-emitente    
               tt-fatur.nome_abrev      = nota-fiscal.nome-ab-cli
               tt-fatur.cidade          = nota-fiscal.cidade  
               tt-fatur.estado          = nota-fiscal.estado
               tt-fatur.cod_repres      = nota-fiscal.cod-rep    
               tt-fatur.nome_rep        = IF AVAIL repres THEN repres.nome-abrev ELSE "Representante nÆo cadastrado "
               tt-fatur.cod_item        = it-nota-fisc.it-codigo                                  
               tt-fatur.vl_tot_it_nf    = calcVlCom12(it-nota-fisc.vl-tot-item,percDescPed) 
                //valor comentado, pois j  est  incluso no vl_tit_it_nf
               //tt-fatur.vl_12           = tt-fatur.vl_tot_it_nf * percDescPed
               //tt-fatur.perc_12         = de-desc / tt-fatur.vl_tot_it_nf 
               tt-fatur.cod-refer       = it-nota-fisc.cod-refer
               tt-fatur.un              = it-nota-fisc.un-fatur[1]
               tt-fatur.descCondPgto    = c-cond-pagto
               tt-fatur.tp_frete        = IF AVAIL ped-venda-ext THEN ped-venda-ext.tp-frete ELSE 'semext'
               tt-fatur.tb_preco_id     = IF AVAIL ped-venda-ext THEN ped-venda-ext.tb_preco_id ELSE 0
               tt-fatur.LOG_rubix       = IF AVAIL ped-venda-ext THEN ped-venda-ext.l_tab_x    ELSE NO
               tt-fatur.nat_operacao    = nota-fiscal.nat-operacao
               tt-fatur.nr_seq_fat      = it-nota-fisc.nr-seq-fat
               tt-fatur.nr_pedido       = INT(nota-fiscal.nr-pedcli)
               tt-fatur.qt_nf           = it-nota-fisc.qt-faturada[1]
               .
          IF log12 THEN DO:
          
              ASSIGN  
              tt-fatur.preco_uni_nf    = calcVlCom12(it-nota-fisc.vl-preuni,percDescPed)                  
              tt-fatur.preco_tot_item  = calcVlCom12(it-nota-fisc.qt-faturada[1] * it-nota-fisc.vl-preuni,percDescPed).             
             
          END.
          ELSE DO:          
              ASSIGN  
              tt-fatur.preco_uni_nf    = it-nota-fisc.vl-preuni                  
              tt-fatur.preco_tot_item  = it-nota-fisc.qt-faturada[1] * it-nota-fisc.vl-preuni.
              
          END.                
          
         /* MESSAGE 'valor total item:' it-nota-fisc.vl-tot-item  ' - depois calc12:'  calcVlCom12(it-nota-fisc.vl-tot-item,percDescPed)  SKIP              
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
               
               
        IF AVAIL ped-venda THEN        
           RUN _getPercComisReppri(INPUT ped-venda.no-ab-reppri,OUTPUT tt-fatur.perc_comis_ped).
        IF AVAIL ped-venda-ext THEN
           RUN _getDesctbPreco(tt-fatur.tb_preco_id, OUTPUT tt-fatur.DESC_tb_preco).

        RUN buscarDadosCliente(nota-fiscal.cod-emitente, OUTPUT tt-fatur.nome_emit, OUTPUT tt-fatur.ramo_atividade, OUTPUT tt-fatur.desc_ramo_ativ, OUTPUT tt-fatur.cnae,
                                                         OUTPUT tt-fatur.desc_cnae, OUTPUT tt-fatur.desc_atividade,OUTPUT tt-fatur.coligada).

        RUN buscarDadosRepres (nota-fiscal.cod-rep,      OUTPUT tt-fatur.nome_rep, OUTPUT tt-fatur.nome_rep_compl, OUTPUT tt-fatur.ativo, OUTPUT tt-fatur.tp_repres,
                                                         OUTPUT tt-fatur.comis_padrao, OUTPUT tt-fatur.desc_reg_ger_rep).

        RUN buscarDadosItem (it-nota-fisc.it-codigo,     OUTPUT tt-fatur.desc_item, OUTPUT tt-fatur.grupo_item, OUTPUT tt-fatur.grupo_it_desc,
                                                         OUTPUT tt-fatur.it_familia, OUTPUT tt-fatur.desc_familia, OUTPUT tt-fatur.it_fam_com,
                                                         OUTPUT tt-fatur.desc_fam_com).
        
        RUN buscarPrecosItem (it-nota-fisc.it-codigo,
                              it-nota-fisc.cod-refer, 
                              IF AVAIL ped-venda-ext  THEN  ped-venda-ext.nr-container ELSE 0,  
                              IF AVAIL ped-venda      THEN ped-venda.dt-implant ELSE tt-fatur.dt_emis ,   
                              tt-fatur.dt_emis,
                              OUTPUT tt-fatur.preco_tab,
                              OUTPUT tt-fatur.preco_out_atual , 
                              OUTPUT tt-fatur.preco_out_epoca,
                              OUTPUT tt-fatur.preco_custo,
                              OUTPUT tt-fatur.id_preco_tab).

        
        /*RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_tab).
        RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_out_atual).
        RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_out_epoca).*/
                              

        IF tt-fatur.preco_out_atual <> 0  THEN
          ASSIGN tt-fatur.preco_tab_ref_atual = tt-fatur.preco_out_atual.
        ELSE 
          ASSIGN tt-fatur.preco_tab_ref_atual  = tt-fatur.preco_tab. 

        ASSIGN tt-fatur.perc_desconto   = 1 - tt-fatur.preco_uni_nf / tt-fatur.preco_tab_ref_atual. 
        /*MESSAGE   tt-fatur.preco_uni_nf SKIP 
                  tt-fatur.preco_tab_ref_atual
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

        RUN retornarFaixa(tt-fatur.perc_desconto,OUTPUT tt-fatur.descFaixa).
        

     END.
 END.

 IF VALID-HANDLE(h-acomp) THEN
    RUN pi-finalizar IN h-acomp.

END PROCEDURE.

PROCEDURE buscarDadosCliente.

    DEFINE INPUT  PARAMETER iCliente        AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cNomeEmit       AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iCodRamoAtiv    AS INT         NO-UNDO.
    DEFINE OUTPUT PARAMETER cDescRamoAtiv   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER codCNAE         AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER descCNAE        AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER descAtividade   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cColigada       AS CHAR        NO-UNDO.

    FOR FIRST emitente FIELDS(nome-emit cod-emitente ) WHERE
         emitente.cod-emitente = iCliente NO-LOCK.         
    END.
    IF AVAIL emitente THEN DO:
       ASSIGN cNomeEmit = emitente.nome-emit.
       FOR EACH ext-emitente FIELDS(cod-ramo-ativ LOG_varejo LOG_atacado LOG_industria LOG_servico ) OF emitente NO-LOCK.
       END.
       IF AVAIL ext-emitente THEN DO:
           ASSIGN iCodRamoAtiv = ext-emitente.cod-ramo-ativ.

           IF ext-emitente.LOG_varejo THEN
              assign descAtividade = "Varejo".
           ELSE 
               IF ext-emitente.LOG_atacado THEN
                  ASSIGN descAtividade = "Atacado".
               ELSE 
                  IF ext-emitente.LOG_industria THEN
                     ASSIGN descAtividade = "Industria".
                  ELSE
                     IF ext-emitente.LOG_servico THEN
                        ASSIGN descAtividade = "Servi‡o".
           FOR FIRST ramo-ativ FIELDS(descricao cod-ramo-ativ) WHERE ramo-ativ.cod-ramo-ativ = iCodRamoAtiv NO-LOCK .
           END.
           IF AVAIL ramo-ativ THEN DO:
              ASSIGN cDescRamoAtiv = ramo-ativ.descricao.
           END.

       END.
       FOR FIRST emitente_cnae FIELDS(cod_cnae cod_emitente cod_tipo_cnae)
           WHERE emitente_cnae.cod_emitente = iCliente 
           AND   emitente_cnae.cod_tipo_cnae = 1 NO-LOCK.
       END.     
       IF AVAIL emitente_cnae THEN DO:
          ASSIGN codCNAE = emitente_cnae.cod_cnae.
       END.

       FOR FIRST cnaes FIELDS(cod_cnae descricao)
       WHERE cnaes.cod_cnae = codCNAE NO-LOCK .
       END.
       IF AVAIL cnaes THEN DO:
           ASSIGN descCNAE = cnaes.descricao.
       END.
    END.
    RUN getColigadaCliente(iCliente, OUTPUT cColigada).
END PROCEDURE.

PROCEDURE buscarDadosRepres.

    DEFINE INPUT   PARAMETER iRepres        AS INTEGER     NO-UNDO.
    DEFINE OUTPUT  PARAMETER nomeRep        AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT  PARAMETER nomeRepCompl   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT  PARAMETER sitRepres      AS INT     NO-UNDO.
    DEFINE OUTPUT  PARAMETER tpRepres       AS INT   NO-UNDO.
    DEFINE OUTPUT  PARAMETER comisRepres    AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER  descRegGerRep  AS CHARACTER   NO-UNDO.

    FOR FIRST repres FIELDS(nome-abrev nome ind-situacao tipo-repres cod-rep ) WHERE
            repres.cod-rep = iRepres NO-LOCK.
    END.        
    IF AVAIL repres THEN DO:
       ASSIGN nomeRep      = repres.nome-abrev
              nomeRepCompl = repres.nome
              sitRepres    = repres.ind-situacao
              tpRepres     = repres.tipo-repres.
    
       FOR FIRST repres_financ FIELDS(cdn_repres cod_empresa val_perc_comis_repres)
           WHERE repres_financ.cdn_repres = iRepres 
           AND   repres_financ.cod_empresa = "500".
       END.
       IF AVAIL repres_financ THEN DO:
          ASSIGN comisRepres  = repres_financ.val_perc_comis_repres.
       END.

       RUN getDescRegRepres(repres.cod-rep, OUTPUT descRegGerRep).

    END.
            
END PROCEDURE.

PROCEDURE buscarDadosItem.

    DEFINE INPUT   PARAMETER cItem          LIKE ITEM.it-codigo NO-UNDO.
    DEFINE OUTPUT  PARAMETER descItem       AS CHAR   NO-UNDO.
    DEFINE OUTPUT  PARAMETER grupoItem      AS INT     NO-UNDO.
    DEFINE OUTPUT  PARAMETER descGrupItem   AS CHAR   NO-UNDO.
    DEFINE OUTPUT  PARAMETER itFamilia      AS CHAR     NO-UNDO.
    DEFINE OUTPUT  PARAMETER itDescFamilia  AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT  PARAMETER itFamComer     LIKE fam-comerc.fm-cod-com  NO-UNDO.
    DEFINE OUTPUT  PARAMETER itDescFamComer  LIKE fam-comerc.descricao  NO-UNDO.

        FOR FIRST ITEM   FIELDS(it-codigo desc-item ge-codigo fm-codigo fm-cod-com)
        WHERE ITEM.it-codigo = cItem NO-LOCK .
        END.
        IF AVAIL ITEM THEN
            ASSIGN  descItem   = ITEM.desc-item
                    grupoItem  = ITEM.ge-codigo
                    itFamilia  = ITEM.fm-codigo
                    itfamComer = ITEM.fm-cod-com .

        FOR FIRST grup-estoque FIELDS(ge-codigo descricao)
            WHERE grup-estoque.ge-codigo = grupoItem NO-LOCK .
        END.
        IF AVAIL grup-estoque THEN DO:
            ASSIGN descGrupItem = grup-estoque.descricao.
        END.

        FOR FIRST familia FIELDS(fm-codigo descricao) 
        WHERE  familia.fm-codigo = itFamilia NO-LOCK.
        END.

        IF AVAIL familia THEN DO:
            ASSIGN itDescFamilia = familia.descricao.
        END.

        FOR FIRST fam-comerc FIELDS(fm-cod-com descricao)
        WHERE fam-comerc.fm-cod-com = itfamComer NO-LOCK.
        END.
        IF AVAIL fam-comerc THEN DO:
           itDescFamComer = fam-comerc.descricao.
        END.

        
END PROCEDURE.
    
PROCEDURE buscarPrecosItem.

     DEFINE INPUT  PARAMETER pItCodigo     AS CHARACTER   NO-UNDO.
     DEFINE INPUT  PARAMETER pCodRefer     AS CHARACTER   NO-UNDO.
     DEFINE INPUT  PARAMETER pContainer    AS INTEGER     NO-UNDO.
     DEFINE INPUT  PARAMETER pDtPedido     AS DATE        NO-UNDO.
     DEFINE INPUT  PARAMETER pDtNF        AS DATE        NO-UNDO.
     DEFINE OUTPUT PARAMETER precoTabItem  AS DEC         NO-UNDO.
     DEFINE OUTPUT PARAMETER PrecoOutItem  AS DECIMAL     NO-UNDO.
     DEFINE OUTPUT PARAMETER precoOutEpoca AS DECIMAL     NO-UNDO.
     DEFINE OUTPUT PARAMETER precoCusto    AS DECIMAL     NO-UNDO.
     DEFINE OUTPUT PARAMETER idPrecoTab    AS INT         NO-UNDO.

     DEFINE VARIABLE idOutlet              AS INTEGER     NO-UNDO.
     DEFINE VARIABLE lAchouControlePreco   AS LOGICAL     NO-UNDO.

     DEFINE VARIABLE dVlReal               AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE dVlDolar              AS DECIMAL     NO-UNDO.
     DEFINE VARIABLE idPreco               AS INT64       NO-UNDO.
     DEFINE VARIABLE iTipo                 AS INTEGER     NO-UNDO.
     DEFINE VARIABLE descrTipPreco         AS CHARACTER   NO-UNDO.
     //DEFINE OUTPUT PARAMETER precoUni AS DECIMAL     NO-UNDO.
     //DEFINE OUTPUT PARAMETER precoTotItem AS DECIMAL     NO-UNDO.
     DEFINE VAR tabPreco                   AS CHARACTER   NO-UNDO.
     DEFINE VARIABLE iCont                 AS INTEGER     NO-UNDO.     
     DEFINE VARIABLE dtRefer               AS DATE        NO-UNDO.
     
     /*IF AVAIL ped-item-ext AND ped-item-ext.cod_controle_preco > 0 THEN DO:

        RUN esapi/busca-preco-id.p(
            ped-item-ext.cod_controle_preco,
            OUTPUT dVlReal    ,         
            OUTPUT dVlDolar   ,         
            OUTPUT descrTipPreco
          ).

        IF dVlReal > 0 OR dVlDolar > 0 THEN DO:
           ASSIGN idPreco = ped-item-ext.cod_controle_preco.
           ASSIGN lAchouControlePreco = TRUE.
        END.
     END.*/
     
     REPEAT iCont = 1 TO 2:        
        CASE iCont:
            WHEN 1 THEN DO:
                ASSIGN dtRefer = pDtPedido.                
            END.
            WHEN 2 THEN DO:
                ASSIGN dtRefer = pDtNf.           
                IF dtRefer = ?  THEN
                   ASSIGN dtRefer = TODAY.                                 
            END.
        END CASE.        
     
        FOR  LAST ITEM_custos FIELDS(it_codigo dt_hr_custo vl_unit_novo)
          WHERE ITEM_custos.it_codigo = pItCodigo
          AND   date(ITEM_custos.dt_hr_custo) <= dtRefer NO-LOCK .
        END.
        
        IF AVAIL ITEM_custos THEN DO:
           ASSIGN precoCusto = ITEM_custos.vl_unit_novo.       
           LEAVE.
        END.           
     END.      

     /*FIND ITEM WHERE
             ITEM.it-codigo = pItCodigo NO-LOCK NO-ERROR.*/

     /*IF AVAIL ped-venda THEN do:
        IF ped-venda.tp-pedido = 'pe' THEN
           ASSIGN iTipo =  1 .
        ELSE 
          ASSIGN iTipo  = 2  .
     END.
     ELSE DO:
         ASSIGN iTipo = 1.
     END.*/

     IF pContainer > 0 THEN
        ASSIGN iTipo = 2.
     ELSE
        ASSIGN iTipo = 1.
    
     IF lAchouControlePreco = FALSE THEN DO:
        RUN esapi/getPrecoPrazoItemRef.p(
             IF AVAIL ped-venda-ext THEN ped-venda-ext.tb_Preco_id ELSE 0,
             INPUT dtRefer,
             INPUT pItCodigo,    
             INPUT pCodRefer,
             INPUT  pContainer,
             iTipo,
             INPUT i-prz,
             IF AVAIL ped-venda THEN  ped-venda.estado ELSE '',
             OUTPUT dVlReal,
             OUTPUT dVlDolar,
             OUTPUT idPreco,
             OUTPUT precoOutEpoca,
             OUTPUT idOutlet ).
        ASSIGN lAchouControlePreco = idPreco <> 0.
     END.

     IF dVlDolar > 0 AND dVlReal = 0 THEN DO:
       FOR LAST hist_cotacoes_bc fields(data valor) NO-LOCK
          WHERE hist_cotacoes_bc.data <= dtRefer.
       END.
       IF AVAIL hist_cotacoes_bc THEN
         ASSIGN precoTabItem = dVlDolar * hist_cotacoes_bc.valor.
       ELSE
         ASSIGN precoTabItem = dVlDolar .
     END.
     ELSE DO:
         ASSIGN precoTabItem = dVlReal.
     END.
     ASSIGN idPrecoTab  = idPreco.
     //  caso o pre‡o de outlet esteja preenchido o mesmo ‚ sobreposto.
     IF idOutlet <> 0 THEN
        ASSIGN idPrecoTab = idOutlet.


     //nÆo est  tratado o valor atual do outlet e da tabela, se necess rio teve-se implementar aqui depois


   
END PROCEDURE.

PROCEDURE retornarTtFatur.

    DEFINE OUTPUT PARAMETER TABLE FOR tt-fatur.


END PROCEDURE.


PROCEDURE exportarTtFatur.

    DEFINE INPUT  PARAMETER cArquivo AS CHARACTER   NO-UNDO.

    {esp/exportarTabela2.i tt-fatur " " " " "fatur.txt" "|" " "}
   /* OUTPUT TO VALUE (cArquivo).

    PUT " Tipo | NF | Serie | Cod Estabel | Dt Emissao | Dia | Mes | Ano | Semana Mes | Semana Ano | Bimestre | Trimestre | Semestre | Cod Cliente |Nome Abrev | Nome Completo | Cidade |". 
    PUT " Estado | Categoria | Cnae | Descricao Cnae | Matriz Cliente | Ramo Atividade | Descricao Ramo | Atividade | Descri‡ao atividade | Cod Repres | Nome Abrev Rep |".
    PUT " Nome Rep Completo | Ativo | Tipo Rep | % ComissÆo padrÆo | Cod ITEM | DESC ITEM | Grupo ITEM | DESC Grupo ITEM |ITEM Familia | Descr Familia | Preco TAB ITEM | Preco Pi Atual |".
    PUT " Preco PI Epoca | Preco Outlet Epoca | Preco Outlet Atual | Preco TAB Ref Atual | Preco TAB Ref Epoca | Preco Uni NF | Quantidade |vl_tot_it_nf |cod-refer | UN ".
    PUT  "|% Desc| Descri‡Æo Faixa| Descri‡Æo Cond.Pagto | Cod.Fam. Comercial | Descri‡Æo  Fam. Comercial| Vl.12|%12|Descri‡Æo RegiÆo Gerencial REPRES|Coligada|Tb.Pre‡o| % ComissÆo Pedido  " skip.

    FOR EACH tt-fatur.

        EXPORT DELIMITER "|" tt-fatur.
    
    END.

    OUTPUT CLOSE.*/

END PROCEDURE.


PROCEDURE buscarDadosData:

    DEFINE INPUT  PARAMETER data            AS DATE        NO-UNDO.
    DEFINE OUTPUT PARAMETER iAno            AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iMes            AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iDia            AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iDiaSemana      AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cDiaSemana      AS CHAR     NO-UNDO.
    DEFINE OUTPUT PARAMETER semanaMes       AS INT   NO-UNDO.
    DEFINE OUTPUT PARAMETER semanaano       AS INT   NO-UNDO.
    DEFINE OUTPUT PARAMETER bimestre        AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER trimestre       AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER quadrimestre    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cListaDiasSemana AS CHARACTER   NO-UNDO.

    ASSIGN cListaDiasSemana = 'Domingo,Segunda,Ter‡a,Quarta,Quinta,Sexta,S bado'
           iAno = YEAR(data)
           iMes = MONTH(data)
           iDia = DAY(data)
           iDiaSemana = WEEKDAY(data)
           cDiaSemana = ENTRY(iDiaSemana,ClistaDiasSemana).




END PROCEDURE.
/*
PROCEDURE buscarDevolucao.

 DEF VAR c-natureza     AS CHAR.
 DEF VAR de-fator       AS DEC.
 DEF VAR de-vl-desconto LIKE ped-item.val-desconto-total.
 DEF VAR l-erro-item    AS LOG.
 DEFINE VARIABLE nomeAbrevRepresNF       AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE nrNF                    AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE serieNF                 AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE codRepNF                AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iEmitNF                 AS INTEGER     NO-UNDO.
 DEFINE VARIABLE dEmisNF                 AS DATE        NO-UNDO.
 DEFINE VARIABLE lDevolNfPropria         AS LOGICAL     NO-UNDO.
 DEFINE VARIABLE nomeAbrevRepPropria     AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE lSemNfVenda             AS LOGICAL     NO-UNDO.
 DEFINE VARIABLE natOperNF               AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE percDescPed             AS DECIMAL     NO-UNDO.
 DEFINE BUFFER bfnf      FOR nota-fiscal.
 DEFINE BUFFER bfped     FOR ped-venda.
 DEFINE BUFFER bfpeditem FOR ped-item.
 //DEFINE BUFFER nfEmitNota FOR emitente .
 DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.
 RUN utp/ut-acomp.p PERSIST SET h-acomp.
 RUN pi-inicializar IN h-acomp(INPUT "Buscando NFs Devolu‡Æo").

 FOR EACH docum-est FIELDS(cod-estabel serie-docto nro-docto nat-operacao cod-emitente dt-trans)
          WHERE             
          docum-est.dt-trans     >= da-dt-emis-nota-ini    AND
          docum-est.dt-trans     <= da-dt-emis-nota-fin    AND
          docum-est.cod-estabel  >= cEstabIni              AND
          docum-est.cod-estabel  <= cEstabFim              AND
          docum-est.serie-docto  >= cSerieIni              AND
          docum-est.serie-docto  <= cSerieFim              AND
          docum-est.nro-docto    >= cNotaIni               AND
          docum-est.nro-docto    <= cNotaFim  NO-LOCK ,
          EACH emitente FIELDS(cod-emitente nome-abrev cidade estado) OF docum-est,
          EACH natur-oper FIELDS(nat-operacao tipo-compra) 
          WHERE natur-oper.nat-operacao = docum-est.nat-operacao
          AND natur-oper.tipo-compra = 3  NO-LOCK .         
     

     FOR FIRST  bfnf FIELDS(cod-estabel serie nr-nota-fis no-ab-reppri)
         WHERE bfnf.cod-estabel =  docum-est.cod-estabel
         AND   bfnf.serie       =  docum-est.serie-docto
         AND   bfnf.nr-nota-fis =  docum-est.nro-docto NO-LOCK .
     END.
     IF AVAIL bfnf THEN DO:
        ASSIGN lDevolNfPropria = YES
               nomeAbrevRepPropria = bfnf.no-ab-reppri .
     END.
     ELSE DO: 
       ASSIGN  lDevolNfPropria       = NO
               nomeAbrevRepPropria   = ''.
     END.
 
     FOR EACH item-doc-est FIELDS( serie-docto cod-emitente   nro-docto  sequencia cod-refer it-codigo serie-comp nro-comp preco-unit quantidade preco-total nat-operacao un) 
            OF docum-est NO-LOCK.    
 
 
         /* Pegar o Lote do Item */
         IF CAN-FIND(rat-lote WHERE
              rat-lote.serie-docto  = docum-est.serie-docto  AND
              rat-lote.nro-docto    = docum-est.nro-docto    AND 
              rat-lote.cod-emitente = docum-est.cod-emitente AND
              rat-lote.nat-operacao = docum-est.nat-operacao AND
              rat-lote.sequencia    = item-doc-est.sequencia AND 
              rat-lote.lote         = item-doc-est.cod-refer ) THEN NEXT.         
 
         IF NOT CAN-FIND(item WHERE item.it-codigo = item-doc-est.it-codigo) THEN NEXT.
         
             
         ASSIGN i-ct           = 0       c-cond-pagto   = ""
                i-cod-vencto   = 0       de-vl-desconto = 0
                c-natureza     = "Sem NF de Origem"
                i-prz          = 0.

         FOR FIRST nota-fiscal FIELDS(cod-estabel serie nr-nota-fis nr-pedcli nome-abrev no-ab-reppri cod-rep cod-emitente  nat-operacao cod-cond-pag nr-fatura)
             WHERE nota-fiscal.cod-estabel  = docum-est.cod-estabel   
             AND   nota-fiscal.serie        = item-doc-est.serie-comp 
             AND   nota-fiscal.nr-nota-fis  = item-doc-est.nro-comp   
             AND   nota-fiscal.emite-duplic NO-LOCK .
             
         END.     
         FOR FIRST ped-venda FIELDS(cod-estabel nr-pedido no-ab-reppri dt-implant)
             WHERE ped-venda.nr-pedcli    =  nota-fiscal.nr-pedcli
             AND   ped-venda.nome-abrev   = nota-fiscal.nome-abrev NO-LOCK.
         END.
         IF AVAIL ped-venda THEN  DO:
            RUN getPercDescPed(ROWID(ped-venda), OUTPUT percDescPed). 
            FOR FIRST ped-venda-ext FIELDS(cod-estabel nr-pedido tp-frete tb_preco_id l_tab_x nr-container) 
            WHERE ped-venda-ext.cod-estabel =  ped-venda.cod-estabel
            AND   ped-venda-ext.nr-pedido   =  ped-venda.nr-pedido NO-LOCK .
            END.            
         END.
         


         ASSIGN l-erro-item = NO.
         IF AVAIL nota-fiscal THEN DO:   
            IF nota-fiscal.cod-rep < rep_ini OR nota-fiscal.cod-rep > rep_fim THEN NEXT.
            ASSIGN nomeAbrevRepresNF = nota-fiscal.no-ab-reppri
                   nrNF              = nota-fiscal.nr-nota-fis
                   serieNF           = nota-fiscal.serie
                   codRepNF          = nota-fiscal.cod-rep
                   demisNF           = nota-fiscal.dt-emis-nota
                   iemitNF           = nota-fiscal.cod-emitente  
                   natOperNF         = nota-fiscal.nat-operacao   
                   .           

            IF CAN-FIND( FIRST estabelec WHERE estabelec.cgc = nota-fiscal.cgc) THEN NEXT.            
            
            ASSIGN c-natureza = nota-fiscal.nat-operacao.
            FOR EACH fat-duplic WHERE
                     fat-duplic.cod-estabel = nota-fiscal.cod-estabel AND
                     fat-duplic.serie       = nota-fiscal.serie       AND
                     fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK.
                ASSIGN i-prz        = i-prz + (fat-duplic.dt-vencimen - fat-duplic.dt-emissao)
                       i-ct         = i-ct + 1
                       i-cod-vencto = fat-duplic.cod-vencto.
            END.
            IF i-prz <> 0 OR i-cod-vencto = 2 OR i-cod-vencto = 9 THEN DO: 
               IF i-cod-vencto = 2 THEN
                  ASSIGN c-cond-pagto = " A Vista".
               ELSE
               IF i-cod-vencto = 9 THEN
                  ASSIGN c-cond-pagto = " Contra Apresenta‡Æo".
               ELSE DO:
                  i-prz = INT(i-prz / i-ct).
                  IF i-prz <= 30 THEN
                     ASSIGN c-cond-pagto = " De 01 At‚ 30 Dias".
                  ELSE
                  IF i-prz <= 60 THEN
                     ASSIGN c-cond-pagto = " De 31 At‚ 60 Dias".
                  ELSE
                  IF i-prz <= 90 THEN
                     ASSIGN c-cond-pagto = " De 61 At‚ 90 Dias".
                  ELSE
                     ASSIGN c-cond-pagto = "ÿAcima de 90 Dias". /* 1¦ Pos ‚ ALT 164 */
               END.
            END.
            ELSE DO:
               FIND cond-pagto WHERE
                    cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-LOCK NO-ERROR.
               IF AVAIL cond-pagto THEN DO:
                  ASSIGN c-cond-pagto = cond-pagto.descricao.
                  IF cond-pagto.log-2 THEN /* Fat ‚ para VENDOR */
                     ASSIGN c-cond-pagto = "ÿVendor". /* 1¦ Pos ‚ ALT 164 */
               END.
            END.
            IF c-cond-pagto = "" THEN
               ASSIGN c-cond-pagto = " Cupom Fiscal".
           
            IF c-regiao = "Exporta‡Æo" THEN
               ASSIGN c-cond-pagto = "ÿExporta‡Æo". /* 1¦ Pos ‚ ALT 164 */

            FOR EACH it-nota-fisc FIELDS(nr-nota-fis serie cod-estabel nr-seq-fat vl-preuni qt-faturada nr-seq-ped it-codigo cod-refer)
                OF nota-fiscal 
                WHERE it-nota-fisc.nr-seq-fat = item-doc-est.seq-comp NO-LOCK.

                /* Calcula Devolu‡Æo do Desconto Proporcial … NF Devolvida */
                ASSIGN de-fator = (item-doc-est.preco-total[1] / (it-nota-fisc.vl-preuni * it-nota-fisc.qt-faturada[1])).
                ASSIGN de-vl-desconto = 0.
                FOR FIRST bfped FIELDS(nome-abrev nr-pedcli cod-estabel nr-pedido) WHERE
                     bfped.nome-abrev = nota-fiscal.nome-ab-cli AND
                     bfped.nr-pedcli  = nota-fiscal.nr-pedcli NO-LOCK.
                END.
                
                FOR FIRST bfpeditem fields(nome-abrev nr-pedcli val-desconto-total)OF bfped WHERE
                     bfpeditem.nr-sequencia = it-nota-fisc.nr-seq-ped NO-LOCK.
                END.     
 
                IF AVAIL bfpeditem THEN DO:
                   IF bfpeditem.val-desconto-total > 0 THEN
                      ASSIGN de-vl-desconto = de-vl-desconto + bfpeditem.val-desconto-total.
                END.  
                ASSIGN de-vl-desconto = (de-vl-desconto * de-fator).                   
            END.
            ASSIGN lSemNfVenda = NO.
         END.
         ELSE DO: //nao achou nota fiscal de venda(origem)
             ASSIGN nomeAbrevRepresNF = ''
                    nrNF               = ''
                    serieNF            = ''
                    codRepNF           = 0
                    demisNF            = ?
                    iemitNF            = 0 
                    natOperNf          = ''. 
             ASSIGN lSemNfVenda = YES.

         END.

         IF l-erro-item THEN NEXT.

         IF c-cond-pagto = "" THEN 
            ASSIGN c-cond-pagto = "ÿSem NF de Origem". /* 1¦ Pos ‚ ALT 164 */  

         FOR FIRST repres FIELDS(cod-rep nome-abrev)
             WHERE repres.cod-rep = codRepNF NO-LOCK.             
         END.         
         IF lSemNFVenda = YES AND lDevolNfPropria = YES THEN DO:
            FOR FIRST repres FIELDS(cod-rep nome-abrev) 
                WHERE repres.nome-abrev = nomeAbrevRepPropria NO-LOCK.
            END.   
            IF AVAIL repres THEN DO:
               IF repres.cod-rep < rep_ini OR repres.cod-rep > rep_fim THEN NEXT.
            END.
         END.

         CREATE tt-fatur.
         ASSIGN 
             tt-fatur.tipo            = "dev"
             tt-fatur.nr_nf           = nrNF
             tt-fatur.serie           = serieNF
             tt-fatur.cod_estabel     = docum-est.cod-estabel
             tt-fatur.dt_emis         = docum-est.dt-trans
             tt-fatur.cod_cli         = docum-est.cod-emitente    
             tt-fatur.nome_abrev      = emitente.nome-abrev
             tt-fatur.cidade          = emitente.cidade  
             tt-fatur.estado          = emitente.estado
             tt-fatur.cod_repres      = codRepNF
             tt-fatur.nome_rep        = IF AVAIL repres THEN repres.nome-abrev ELSE "Representante NÆo Encontrado"
             tt-fatur.cod_item        = item-doc-est.it-codigo                   
             tt-fatur.preco_uni_nf    = item-doc-est.preco-unit[1]    
             tt-fatur.qt_nf           = ITEM-doc-est.quantidade * -1
             tt-fatur.vl_tot_it_nf    = (item-doc-est.preco-total[1] + de-vl-desconto) * -1    
             //tt-fatur.vl_12           = de-vl-desconto  * -1
             //tt-fatur.perc_12         = IF tt-fatur.vl_tot_it_nf > 0 THEN de-vl-desconto /  tt-fatur.vl_tot_it_nf * -1 ELSE 0
             tt-fatur.cod-refer       = item-doc-est.cod-refer
             tt-fatur.un              = item-doc-est.un
             tt-fatur.nat_operacao    = natOperNf
             tt-fatur.nat_devol       = item-doc-est.nat-operacao
             tt-fatur.nr_seq_devol    = item-doc-est.sequencia
             tt-fatur.nr_nf_devol     = docum-est.nro-docto
             tt-fatur.serie_devol     = docum-est.serie-docto
             tt-fatur.tp_frete        = IF AVAIL ped-venda-ext THEN ped-venda-ext.tp-frete ELSE 'semext'
             tt-fatur.tb_preco_id     = IF AVAIL ped-venda-ext THEN  ped-venda-ext.tb_preco_id ELSE  1
             tt-fatur.LOG_rubix       = IF AVAIL ped-venda-ext THEN ped-venda-ext.l_tab_x  ELSE false
             tt-fatur.nr_pedido       = IF AVAIL nota-fiscal THEN INT(nota-fiscal.nr-pedcli) ELSE 0.
             .
       IF log12 THEN DO:
          ASSIGN tt-fatur.preco_uni_nf    = calcVlCom12(item-doc-est.preco-unit[1],percDescPed) *  -1 
                 tt-fatur.preco_tot_item  = calcVlCom12(ITEM-doc-est.quantidade * item-doc-est.preco-unit[1],percDescPed) * -1.             
                   
       END.
       
             
       IF AVAIL ped-venda THEN
          RUN _getPercComisReppri(INPUT ped-venda.no-ab-reppri,OUTPUT tt-fatur.perc_comis_ped).

       IF AVAIL ped-venda-ext THEN
          RUN _getDesctbPreco(ped-venda-ext.tb_preco_id, OUTPUT tt-fatur.DESC_tb_preco).

        RUN buscarDadosCliente(iEmitNF, OUTPUT tt-fatur.nome_emit, OUTPUT tt-fatur.ramo_atividade, OUTPUT tt-fatur.desc_ramo_ativ, OUTPUT tt-fatur.cnae,
                                                         OUTPUT tt-fatur.desc_cnae, OUTPUT tt-fatur.desc_atividade,OUTPUT tt-fatur.coligada).

        RUN buscarDadosRepres (codrepNF, OUTPUT tt-fatur.nome_rep, OUTPUT tt-fatur.nome_rep_compl, OUTPUT tt-fatur.ativo, OUTPUT tt-fatur.tp_repres,
                                                         OUTPUT tt-fatur.comis_padrao, OUTPUT tt-fatur.desc_reg_ger_rep).

        RUN buscarDadosItem (item-doc-est.it-codigo,     OUTPUT tt-fatur.desc_item, OUTPUT tt-fatur.grupo_item, OUTPUT tt-fatur.grupo_it_desc,
                                                         OUTPUT tt-fatur.it_familia, OUTPUT tt-fatur.desc_familia, OUTPUT tt-fatur.it_fam_com,
                                                         OUTPUT tt-fatur.desc_fam_com).

        
        RUN buscarPrecosItem (item-doc-est.it-codigo,
                              item-doc-est.cod-refer,
                              IF AVAIL ped-venda-ext THEN ped-venda-ext.nr-container  ELSE  0,
                              IF AVAIL bfped THEN bfped.dt-implant ELSE dEmisNF ,   
                              dEmisNF,
                              OUTPUT tt-fatur.preco_tab, 
                              OUTPUT tt-fatur.preco_out_atual,
                              OUTPUT tt-fatur.preco_out_epoca,
                              OUTPUT tt-fatur.preco_custo,
                              OUTPUT tt-fatur.id_preco_tab).
                              

        /*RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_tab).
        RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_out_atual).
        RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_out_epoca).*/
        

        
     END.
 END.
 IF VALID-HANDLE(h-acomp) THEN 
    RUN pi-finalizar IN h-acomp.



END PROCEDURE.
 */
 
PROCEDURE buscarDevolucao.

 DEF VAR c-natureza     AS CHAR.
 DEF VAR de-fator       AS DEC.
 DEF VAR de-vl-desconto LIKE ped-item.val-desconto-total.
 DEF VAR l-erro-item    AS LOG.
 DEFINE VARIABLE nomeAbrevRepresNF       AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE nrNF                    AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE serieNF                 AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE codRepNF                AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iEmitNF                 AS INTEGER     NO-UNDO.
 DEFINE VARIABLE dEmisNF                 AS DATE        NO-UNDO.
 DEFINE VARIABLE lDevolNfPropria         AS LOGICAL     NO-UNDO.
 DEFINE VARIABLE nomeAbrevRepPropria     AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE lSemNfVenda             AS LOGICAL     NO-UNDO.
 DEFINE VARIABLE natOperNF               AS CHARACTER   NO-UNDO.
 DEFINE BUFFER bfnf FOR nota-fiscal.
 //DEFINE BUFFER nfEmitNota FOR emitente .
 DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.
 RUN utp/ut-acomp.p PERSIST SET h-acomp.
 RUN pi-inicializar IN h-acomp(INPUT "Buscando NFs Devolu‡Æo").

 FOR EACH docum-est WHERE             
          docum-est.dt-trans     >= da-dt-emis-nota-ini    AND
          docum-est.dt-trans     <= da-dt-emis-nota-fin    AND
          docum-est.cod-estabel  >= cEstabIni              AND
          docum-est.cod-estabel  <= cEstabFim              AND
          docum-est.serie-docto  >= cSerieIni              AND
          docum-est.serie-docto  <= cSerieFim              AND
          docum-est.nro-docto    >= cNotaIni               AND
          docum-est.nro-docto    <= cNotaFim  NO-LOCK .

 
     FIND natur-oper WHERE
          natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.
     IF NOT AVAIL natur-oper THEN NEXT.
     IF natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */
 
     FIND emitente WHERE
          emitente.cod-emit = docum-est.cod-emitente NO-LOCK NO-ERROR.
     IF NOT AVAIL emitente THEN NEXT.

     FIND bfnf 
         WHERE bfnf.cod-estabel =  docum-est.cod-estabel
         AND   bfnf.serie       =  docum-est.serie-docto
         AND   bfnf.nr-nota-fis =  docum-est.nro-docto NO-LOCK NO-ERROR.
     IF AVAIL bfnf THEN DO:
        ASSIGN lDevolNfPropria = YES
               nomeAbrevRepPropria = bfnf.no-ab-reppri .
     END.
     ELSE DO: 
       ASSIGN  lDevolNfPropria       = NO
               nomeAbrevRepPropria   = ''.
     END.
 
     FOR EACH item-doc-est OF docum-est NO-LOCK.    
 
         /* Pegar o Lote do Item */
         FIND rat-lote WHERE
              rat-lote.serie-docto  = docum-est.serie-docto  AND
              rat-lote.nro-docto    = docum-est.nro-docto    AND 
              rat-lote.cod-emitente = docum-est.cod-emitente AND
              rat-lote.nat-operacao = docum-est.nat-operacao AND
              rat-lote.sequencia    = item-doc-est.sequencia AND 
              rat-lote.lote         = item-doc-est.cod-refer NO-LOCK NO-ERROR.
         IF NOT AVAIL rat-lote THEN NEXT.
 
         FIND item WHERE
              item.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR.
         IF NOT AVAIL ITEM THEN NEXT.
             
         ASSIGN i-ct           = 0       c-cond-pagto   = ""
                i-cod-vencto   = 0       de-vl-desconto = 0
                c-natureza     = "Sem NF de Origem"
                i-prz          = 0.

         FIND FIRST nota-fiscal WHERE
              nota-fiscal.cod-estabel  = docum-est.cod-estabel   AND
              nota-fiscal.serie        = item-doc-est.serie-comp AND
              nota-fiscal.nr-nota-fis  = item-doc-est.nro-comp   AND
              nota-fiscal.emite-duplic NO-LOCK NO-ERROR.
         FIND FIRST ped-venda 
             WHERE ped-venda.nr-pedcli =  nota-fiscal.nr-pedcli
             AND   ped-venda.no-ab-reppri = nota-fiscal.nome-abrev NO-LOCK NO-ERROR.
         FIND FIRST ped-venda-ext 
             WHERE ped-venda-ext.cod-estabel =  ped-venda.cod-estabel
             AND   ped-venda-ext.nr-pedido   =  ped-venda.nr-pedido NO-LOCK NO-ERROR.


         ASSIGN l-erro-item = NO.
         IF AVAIL nota-fiscal THEN DO:   
            IF nota-fiscal.cod-rep < rep_ini OR nota-fiscal.cod-rep > rep_fim THEN NEXT.
            ASSIGN nomeAbrevRepresNF = nota-fiscal.no-ab-reppri
                   nrNF              = nota-fiscal.nr-nota-fis
                   serieNF           = nota-fiscal.serie
                   codRepNF          = nota-fiscal.cod-rep
                   demisNF           = nota-fiscal.dt-emis-nota
                   iemitNF           = nota-fiscal.cod-emitente  
                   natOperNF         = nota-fiscal.nat-operacao   
                   .           

            FIND estabelec WHERE
                 estabelec.cgc = nota-fiscal.cgc NO-LOCK NO-ERROR.
            IF AVAIL estabelec THEN NEXT.             
            
            ASSIGN c-natureza = nota-fiscal.nat-operacao.
            FOR EACH fat-duplic WHERE
                     fat-duplic.cod-estabel = nota-fiscal.cod-estabel AND
                     fat-duplic.serie       = nota-fiscal.serie       AND
                     fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis NO-LOCK.
                ASSIGN i-prz        = i-prz + (fat-duplic.dt-vencimen - fat-duplic.dt-emissao)
                       i-ct         = i-ct + 1
                       i-cod-vencto = fat-duplic.cod-vencto.
            END.
            IF i-prz <> 0 OR i-cod-vencto = 2 OR i-cod-vencto = 9 THEN DO: 
               IF i-cod-vencto = 2 THEN
                  ASSIGN c-cond-pagto = " A Vista".
               ELSE
               IF i-cod-vencto = 9 THEN
                  ASSIGN c-cond-pagto = " Contra Apresenta‡Æo".
               ELSE DO:
                  i-prz = INT(i-prz / i-ct).
                  IF i-prz <= 30 THEN
                     ASSIGN c-cond-pagto = " De 01 At‚ 30 Dias".
                  ELSE
                  IF i-prz <= 60 THEN
                     ASSIGN c-cond-pagto = " De 31 At‚ 60 Dias".
                  ELSE
                  IF i-prz <= 90 THEN
                     ASSIGN c-cond-pagto = " De 61 At‚ 90 Dias".
                  ELSE
                     ASSIGN c-cond-pagto = "ÿAcima de 90 Dias". /* 1¦ Pos ‚ ALT 164 */
               END.
            END.
            ELSE DO:
               FIND cond-pagto WHERE
                    cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-LOCK NO-ERROR.
               IF AVAIL cond-pagto THEN DO:
                  ASSIGN c-cond-pagto = cond-pagto.descricao.
                  IF cond-pagto.log-2 THEN /* Fat ‚ para VENDOR */
                     ASSIGN c-cond-pagto = "ÿVendor". /* 1¦ Pos ‚ ALT 164 */
               END.
            END.
            IF c-cond-pagto = "" THEN
               ASSIGN c-cond-pagto = " Cupom Fiscal".
           
            IF c-regiao = "Exporta‡Æo" THEN
               ASSIGN c-cond-pagto = "ÿExporta‡Æo". /* 1¦ Pos ‚ ALT 164 */

            FOR EACH it-nota-fisc OF nota-fiscal WHERE 
                     it-nota-fisc.nr-seq-fat = item-doc-est.seq-comp NO-LOCK.

                /* Calcula Devolu‡Æo do Desconto Proporcial … NF Devolvida */
                ASSIGN de-fator = (item-doc-est.preco-total[1] / (it-nota-fisc.vl-preuni * it-nota-fisc.qt-faturada[1])).
                
                FIND ped-venda WHERE
                     ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
                     ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.


 
                ASSIGN de-vl-desconto = 0.
                FIND ped-item OF ped-venda WHERE
                     ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped NO-LOCK NO-ERROR.
 
                IF AVAIL ped-item THEN DO:
                   IF ped-item.val-desconto-total > 0 THEN
                      ASSIGN de-vl-desconto = de-vl-desconto + ped-item.val-desconto-total.
                END.
                
 
                ASSIGN de-vl-desconto = (de-vl-desconto * de-fator).                   
            END.
            ASSIGN lSemNfVenda = NO.
         END.
         ELSE DO: //nao achou nota fiscal de venda(origem)
             ASSIGN nomeAbrevRepresNF = ''
                    nrNF               = ''
                    serieNF            = ''
                    codRepNF           = 0
                    demisNF            = ?
                    iemitNF            = 0 
                    natOperNf          = ''. 
             ASSIGN lSemNfVenda = YES.

         END.

         IF l-erro-item THEN NEXT.

         IF c-cond-pagto = "" THEN 
            ASSIGN c-cond-pagto = "ÿSem NF de Origem". /* 1¦ Pos ‚ ALT 164 */  

         FIND FIRST repres
             WHERE repres.cod-rep = codRepNF NO-LOCK NO-ERROR.
         
         IF lSemNFVenda = YES AND lDevolNfPropria = YES THEN DO:
            FIND repres 
                WHERE repres.nome-abrev = nomeAbrevRepPropria
                NO-LOCK NO-ERROR.
            IF AVAIL repres THEN DO:
               IF repres.cod-rep < rep_ini OR repres.cod-rep > rep_fim THEN NEXT.
            END.
         END.

         CREATE tt-fatur.
         ASSIGN 
             tt-fatur.tipo            = "dev"
             tt-fatur.nr_nf           = nrNF
             tt-fatur.serie           = serieNF
             tt-fatur.cod_estabel     = docum-est.cod-estabel
             tt-fatur.dt_emis         = docum-est.dt-trans
             tt-fatur.cod_cli         = docum-est.cod-emitente    
             tt-fatur.nome_abrev      = emitente.nome-abrev
             tt-fatur.cidade          = emitente.cidade  
             tt-fatur.estado          = emitente.estado
             tt-fatur.cod_repres      = codRepNF
             tt-fatur.nome_rep        = IF AVAIL repres THEN repres.nome-abrev ELSE "Representante NÆo Encontrado"
             tt-fatur.cod_item        = item-doc-est.it-codigo                   
             tt-fatur.preco_uni_nf    = item-doc-est.preco-unit[1]    
             tt-fatur.qt_nf           = ITEM-doc-est.quantidade * -1
             tt-fatur.vl_tot_it_nf    = (item-doc-est.preco-total[1] + de-vl-desconto) * -1    
             //tt-fatur.vl_12           = de-vl-desconto  * -1
             //tt-fatur.perc_12         = IF tt-fatur.vl_tot_it_nf > 0 THEN de-vl-desconto /  tt-fatur.vl_tot_it_nf * -1 ELSE 0
             tt-fatur.cod-refer       = item-doc-est.cod-refer
             tt-fatur.un              = item-doc-est.un
             tt-fatur.nat_operacao    = natOperNf
             tt-fatur.nat_devol       = item-doc-est.nat-operacao
             tt-fatur.nr_seq_devol    = item-doc-est.sequencia
             tt-fatur.nr_nf_devol     = docum-est.nro-docto
             tt-fatur.serie_devol     = docum-est.serie-docto
             tt-fatur.tp_frete         = IF AVAIL ped-venda-ext THEN ped-venda-ext.tp-frete ELSE 'semext'
             tt-fatur.tb_preco_id     = IF AVAIL ped-venda-ext THEN  ped-venda-ext.tb_preco_id ELSE  1
             tt-fatur.LOG_rubix       = IF AVAIL ped-venda-ext THEN ped-venda-ext.l_tab_x  ELSE false
             tt-fatur.nr_pedido       = IF AVAIL nota-fiscal THEN INT(nota-fiscal.nr-pedcli) ELSE 0.
             .
       IF AVAIL ped-venda THEN
          RUN _getPercComisReppri(INPUT ped-venda.no-ab-reppri,OUTPUT tt-fatur.perc_comis_ped).

       IF AVAIL ped-venda-ext THEN
          RUN _getDesctbPreco(ped-venda-ext.tb_preco_id, OUTPUT tt-fatur.DESC_tb_preco).

        RUN buscarDadosCliente(iEmitNF, OUTPUT tt-fatur.nome_emit, OUTPUT tt-fatur.ramo_atividade, OUTPUT tt-fatur.desc_ramo_ativ, OUTPUT tt-fatur.cnae,
                                                         OUTPUT tt-fatur.desc_cnae, OUTPUT tt-fatur.desc_atividade,OUTPUT tt-fatur.coligada).

        RUN buscarDadosRepres (codrepNF, OUTPUT tt-fatur.nome_rep, OUTPUT tt-fatur.nome_rep_compl, OUTPUT tt-fatur.ativo, OUTPUT tt-fatur.tp_repres,
                                                         OUTPUT tt-fatur.comis_padrao, OUTPUT tt-fatur.desc_reg_ger_rep).

        RUN buscarDadosItem (item-doc-est.it-codigo,     OUTPUT tt-fatur.desc_item, OUTPUT tt-fatur.grupo_item, OUTPUT tt-fatur.grupo_it_desc,
                                                         OUTPUT tt-fatur.it_familia, OUTPUT tt-fatur.desc_familia, OUTPUT tt-fatur.it_fam_com,
                                                         OUTPUT tt-fatur.desc_fam_com).

        
        RUN buscarPrecosItem (item-doc-est.it-codigo,
                              item-doc-est.cod-refer,
                              IF AVAIL ped-venda-ext THEN ped-venda-ext.nr-container  ELSE  0,
                              IF  AVAIL ped-venda THEN ped-venda.dt-implant ELSE dEmisNF ,   
                              dEmisNF,
                              OUTPUT tt-fatur.preco_tab, 
                              OUTPUT tt-fatur.preco_out_atual,
                              OUTPUT tt-fatur.preco_out_epoca,
                              OUTPUT tt-fatur.preco_custo,
                              OUTPUT tt-fatur.id_preco_tab).
                              
                              
         

        /*RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_tab).
        RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_out_atual).
        RUN retornarPrecoFinanc (i-prz, INPUT-OUTPUT tt-fatur.preco_out_epoca).*/
        

        
     END.
 END.
 IF VALID-HANDLE(h-acomp) THEN 
    RUN pi-finalizar IN h-acomp.



END PROCEDURE. 
PROCEDURE buscarMetasRepres.

    DEFINE VARIABLE iMesIni AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iMesFim AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAnoIni AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAnoFim AS INTEGER     NO-UNDO.
    
    ASSIGN iMesIni = (MONTH(da-dt-emis-nota-ini)).
    ASSIGN iMesFim = (MONTH(da-dt-emis-nota-fin)).
    ASSIGN iAnoIni = (YEAR(da-dt-emis-nota-ini)).
    ASSIGN iAnoFim = (YEAR(da-dt-emis-nota-fin)).

    FOR EACH metas_repres WHERE
             metas_repres.mes >= iMesIni AND
             metas_repres.mes <= iMesFim AND
             metas_repres.ano >= iAnoIni AND
             metas_repres.ano <= iAnoFim NO-LOCK.
       
        CREATE tt-fatur.
    
        ASSIGN tt-fatur.tipo            = "metas"
               tt-fatur.dt_emis         = DATE(metas_repres.mes,1,metas_repres.ano)
               tt-fatur.cod_repres      = metas_repres.cod_rep
               tt-fatur.vl_tot_it_nf    = metas_repres.vl_meta.
    
        RUN buscarDadosRepres (metas_repres.cod_rep,  
                               OUTPUT tt-fatur.nome_rep, 
                               OUTPUT tt-fatur.nome_rep_compl, 
                               OUTPUT tt-fatur.ativo, 
                               OUTPUT tt-fatur.tp_repres,
                               OUTPUT tt-fatur.comis_padrao,
                               OUTPUT tt-fatur.desc_reg_ger_rep).

    END.

END PROCEDURE.

PROCEDURE retornarFaixa:

    DEFINE INPUT  PARAMETER dPerc AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER cDescFaixa AS CHARACTER   NO-UNDO.

    IF dPerc > 0.20 THEN
       ASSIGN cDescFaixa = "0- Maior que 20% Abaixo do Preco de Tabela".
    ELSE
       IF dPerc < 0.20 AND dPerc >= 0.10 THEN
          ASSIGN cDescFaixa = "1- Entre 10% e 20% abaixo do Pre‡o de Tabela".
       ELSE
          IF dPerc < 0.10 AND dPerc >= 0.05 THEN
             ASSIGN cDescFaixa = "2- Entre 5% e 9.99% abaixo do Pre‡o de Tabela".
          ELSE 
             IF  dPerc < 0.05 AND dPerc > 0 THEN
             ASSIGN cDescFaixa = "3- At‚ 4.99% abaixo de Desconto".
             ELSE 
                 IF dPerc = 0 THEN
                    ASSIGN cDescFaixa = "4- Pre‡o de Tabela".
                 ELSE
                    ASSIGN cDescFaixa = "5- Acima do Pre‡o de Tabela".

END PROCEDURE.

PROCEDURE buscarIndFinanc.

    DEFINE INPUT  PARAMETER iPrazo AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER dIndice AS DECIMAL     NO-UNDO.

   
    FIND FIRST tab-finan NO-LOCK NO-ERROR.
    IF AVAIL tab-finan THEN DO:
        FIND FIRST tab-finan-indice OF tab-finan 
        WHERE tab-finan-indice.tab-dia-fin  >= iPrazo
        NO-LOCK NO-ERROR.
        IF NOT AVAIL tab-finan-indice THEN DO:
            FIND FIRST tab-finan-indice OF tab-finan 
            WHERE tab-finan-indice.tab-dia-fin = 90 NO-LOCK NO-ERROR.
        END.
        ASSIGN dIndice = tab-finan-indice.tab-ind-fin.
    END.

END PROCEDURE.

PROCEDURE retornarPrecoFinanc.


    DEFINE INPUT  PARAMETER iPrazo          AS INTEGER     NO-UNDO.
    DEFINE INPUT-OUTPUT  PARAMETER dValor   AS DEC        NO-UNDO.
    DEFINE VARIABLE dIndice                 AS DECIMAL     NO-UNDO.

    RUN buscarIndFinanc(iPrazo, OUTPUT dIndice).
    ASSIGN dValor = dValor * dIndice.




END PROCEDURE.

PROCEDURE getDescRegRepres:
    DEFINE INPUT  PARAMETER pCodRep         AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cDescrRegiao    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iRegiao AS INTEGER     NO-UNDO.
    RUN setCodRep IN hBoRepres(pCodRep).
    RUN getRegiaoGerencialRepres IN hBoRepres(OUTPUT iRegiao).
    RUN setLista IN hBoOpcaoLista(6).
    RUN setOpcao IN hBoOpcaoLista(string(iRegiao)).
    RUN getDescrOpcao IN hBoOpcaoLista(OUTPUT cDescrRegiao).

END PROCEDURE.

PROCEDURE getColigadaCliente:
    DEFINE INPUT  PARAMETER iCliente  AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cColigada AS CHAR    NO-UNDO.
    RUN setCodEmitente IN hBoEmitente(iCliente).
    RUN getColigPrinc IN hBoEmitente(OUTPUT cColigada).


END PROCEDURE.

PROCEDURE gerarLog:

    DEFINE INPUT  PARAMETER pArquivo        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pItCodigo       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pContainer      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pDtRefer        AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pIdPreco        AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pVlPrecoReal    AS DECIMAL     NO-UNDO.

    ASSIGN cMsg = ''.
    RUN incrValor(INPUT-OUTPUT cMsg,"tabela preco:" + IF AVAIL ped-venda-ext THEN string(ped-venda-ext.tb_Preco_id) ELSE '0',CHR(13) ).
    RUN incrValor(INPUT-OUTPUT cMsg,"data refer.:"  + STRING(pDtRefer)  ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"item:"         + pItCodigo             ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"refer:"        + pCodRefer             ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"container:"    + string(pContainer)    ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"Tipo Pre‡o:3"                          ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"Prazo M‚dio:"  + string(i-Prz)         ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg,"UF:" + IF AVAIL ped-venda THEN  ped-venda.estado ELSE '' ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg, 'Preco:'    + string(pVlPrecoReal) ,'-' ).
    RUN incrValor(INPUT-OUTPUT cMsg, 'ID Preco:' + string(pIdPreco)    ,'-' ).

RUN gravarTextoEmArquivo(pArquivo,cMsg).


END PROCEDURE.


PROCEDURE getDtUltFatCli:

    DEFINE INPUT  PARAMETER pCliente    AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER dtUltFat    AS DATE        NO-UNDO.
    DEFINE VARIABLE nomeAbrev           AS CHARACTER   NO-UNDO.
    RUN setCodEmitente IN hBoEmitente(pCliente).
    RUN getNomeAbrev IN hBoEmitente(OUTPUT nomeAbrev).

    FIND LAST nota-fiscal NO-LOCK
    USE-INDEX ch-emi-nota
    WHERE  nota-fiscal.nome-ab-cli = nomeAbrev
    AND   nota-fiscal.dt-cancela    = ? 
    AND CAN-FIND(FIRST natur-oper OF nota-fiscal 
                 WHERE natur-oper.tp-rec-desp = 1 AND  natur-oper.tipo-compra <> 3 )
    NO-ERROR.
    IF AVAIL nota-fiscal THEN
       ASSIGN dtUltFat = nota-fiscal.dt-emis-nota.

END PROCEDURE.
