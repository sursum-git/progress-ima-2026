{esp/util.i}

DEFINE TEMP-TABLE ttResult NO-UNDO
    FIELD codEstabel                AS CHAR                             COLUMN-LABEL "Estab."
    FIELD nrContainer               AS INT                              COLUMN-LABEL "Nr.Container"
    FIELD sitContainer              AS CHAR FORMAT 'x(20)'              COLUMN-LABEL "Situa‡Æo Container"
    FIELD nomeCliente               AS CHAR FORMAT 'x(50)'              COLUMN-LABEL "Nome Cliente"
    FIELD dtRecebimento             AS DATE                             COLUMN-LABEL "Dt.Recebimento"
    FIELD dtPrevChegada             AS DATE                             COLUMN-LABEL "Dt.Prev.Chegada"
    FIELD dtPrevChegadaLoja         AS DATE                             COLUMN-LABEL "Dt.Prev.Chegada Loja"
    FIELD dtPrevEmbarque            AS DATE                             COLUMN-LABEL "Dt.Prev.Embarque"
    FIELD dtRealEmbarque            AS DATE                             COLUMN-LABEL "Dt.Real Embarque"
    FIELD ptax                      AS DECIMAL                          COLUMN-LABEL "D¢lar PTax"
    FIELD vlDolarCompra             AS DECIMAL                          COLUMN-LABEL "Vl.Dolar Compra"
    FIELD vlDolarVenda              AS DECIMAL                          COLUMN-LABEL "Vl.Dolar Venda"
    FIELD vlCustoUnitImportacao     AS DECIMAL                          COLUMN-LABEL "Vl.Unit.Custo Importa‡Æo"
    FIELD vlCustoUnitAposImportacao AS DECIMAL                          COLUMN-LABEL "Vl.Unit.Custo Ap¢s Importa‡Æo"
    FIELD vlUnitVenda               AS DECIMAL                          COLUMN-LABEL "Vl.Unit.Custo Importa‡Æo"
    FIELD vlCustoImportacao         AS DECIMAL                          COLUMN-LABEL "Vl.Total Custo Importa‡Æo"
    FIELD vlCustoAposImportacao     AS DECIMAL                          COLUMN-LABEL "Vl.Total Custo Ap¢s Importa‡Æo"
    FIELD vlMargemContribuicao      AS DECIMAL                          COLUMN-LABEL "Vl.Margem Contr.C/Imp.Venda"
    FIELD produto                   AS CHAR FORMAT 'X(50)'              COLUMN-LABEL "Produto"
    FIELD itCodigo                  AS CHAR         
    FIELD qtComprada                AS DECIMAL                          COLUMN-LABEL "Qt.Comprada"
    FIELD qtVendidaPI               AS DECIMAL                          COLUMN-LABEL "Qt.Venda Antes Chegada"
    FIELD qtFaturada                AS DECIMAL                          COLUMN-LABEL "Qt.Faturada"
    FIELD qtDevolvida               AS DECIMAL                          COLUMN-LABEL "Qt.Devolvida"
    FIELD vlFaturado                AS DECIMAL                          COLUMN-LABEL "Vl.Faturado"
    FIELD vlDevolvido               AS DECIMAL                          COLUMN-LABEL "Vl.Devolvido"
    FIELD percVendido               AS DECIMAL                          COLUMN-LABEL "% Vendido"
    FIELD vlLucroContainer          AS DECIMAL FORMAT '->>>,>>>,>>9.99' COLUMN-LABEL "Vl.Lucro Container"
    FIELD vlLucroProduto            AS DECIMAL FORMAT '->>>,>>>,>>9.99' COLUMN-LABEL "Vl.Lucro Produto"
    FIELD percLucroContainer        AS DECIMAL FORMAT '->>>,>>>,>>9.99' COLUMN-LABEL "% Lucro Container"
    FIELD percLucroProduto          AS DECIMAL FORMAT '->>>,>>>,>>9.99' COLUMN-LABEL "% Lucro Produto"
    INDEX primario IS PRIMARY nrContainer produto
    .
/*
FOR EACH pp-container NO-LOCK.
    FOR EACH pp-it-container OF pp-container NO-LOCK.

    END.
END.*/

FUNCTION getDescrSit RETURNS CHAR(sit AS INT):

    CASE sit:
        WHEN 1 THEN
            RETURN 'Aberto'.
        WHEN 2 THEN
            RETURN 'Suspenso'.
        WHEN 3 THEN
            RETURN 'Fechado'.
    END CASE.


END FUNCTION.
DISP   'somando valores de compra do container' FORMAT 'x(50)' NOW .
FOR EACH  pp-it-container NO-LOCK,
    EACH pp-container OF pp-it-container NO-LOCK,
    EACH ITEM OF pp-it-container NO-LOCK,
    EACH emitente NO-LOCK
    WHERE emitente.cod-emitente = pp-container.cod-emit-forn .
    .
    FIND ttResult 
        WHERE ttResult.nrContainer = pp-container.nr-container
        AND   ttResult.itCodigo    = pp-it-container.it-codigo
        NO-ERROR.
    IF NOT AVAIL ttResult THEN DO:
       CREATE ttResult. 
       ASSIGN ttResult.codEstabel               = pp-container.cod-estabel
              ttResult.nrContainer              = pp-container.nr-container
              ttResult.sitContainer             = getDescrSit(pp-container.situacao)
              ttResult.nomeCliente              = STRING(pp-container.cod-emit-forn) + "-" + emitente.nome-abrev
              ttResult.dtRecebimento            = pp-container.dt-recebimento
              ttResult.dtPrevChegada            = pp-container.dt_prev_chegada
              ttResult.dtPrevChegadaLoja        = pp-container.dt_prev_chegada_loja
              ttResult.dtPrevEmbarque           = pp-container.dt_prev_embarque
              ttResult.dtRealEmbarque           = pp-container.dt_real_embarque
              ttResult.ptax                     = pp-container.ptax
              ttResult.vlDolarCompra            = pp-container.vl-dolar-compra
              ttResult.vlDolarVenda             = pp-container.vl-dolar-venda
              ttResult.produto                  = pp-it-container.it-codigo + "-" + item.desc-item
              ttResult.itCodigo                 = pp-it-container.it-codigo
              .
        
    END.

    ASSIGN ttResult.qtComprada     = ttResult.qtComprada  +  IF pp-it-container.qt-recebida > 0 
                                                             THEN pp-it-container.qt-recebida 
                                                             ELSE pp-it-container.qt-pedida  
           ttResult.qtVendidaPI        = ttResult.qtVendidaPI +  pp-it-container.qt-vendida 
                                                                 .
           
END.
DISP   'somando valores faturados' FORMAT 'x(50)' NOW .
//busca os valores faturados
FOR EACH ttResult:
    FOR EACH fats_04 NO-LOCK
        WHERE fats_04.num_origem = ttResult.nrContainer
        AND   fats_04.it_Codigo  = ttResult.itCodigo .
        ASSIGN 
        ttResult.qtFaturada         = ttResult.qtFaturada  + fats_04.qt_total
        ttResult.vlFaturado         = ttResult.vlFaturado  + fats_04.vl_total
        ttResult.qtDevolvida        = ttResult.qtDevolvida + fats_04.qt_dev_total
        ttResult.vlDevolvido        = ttResult.vlDevolvido + fats_04.vl_dev_total
        .
    END.
END.
DISP   'buscando custo do produto' FORMAT 'x(50)' NOW .
//busca o custo do produto no container
FOR EACH ttResult:
    FOR LAST versoes_item_custo_container NO-LOCK
            WHERE container_id = ttResult.nrContainer,
            EACH item_container_custos OF versoes_item_custo_container,
            EACH item_custos OF ITEM_container_custos
            WHERE ITEM_custos.it_codigo = ttResult.itcodigo .
            ASSIGN ttResult.vlCustoUnitImportacao       = item_custos.vl_unit_container
                   ttResult.vlCustoUnitAposImportacao   = item_custos.vl_unit_novo.
    END.
    
END.




//calcula valores
FOR EACH ttResult.

    ASSIGN ttResult.percVendido         = ttResult.qtFaturada / ttResult.qtComprada
           ttResult.vlUnitVenda         = (ttResult.vlFaturado  - ttResult.vlDevolvido)  / (ttResult.qtFaturada  - ttResult.qtDevolvida)
           ttResult.vlLucroContainer    = (ttResult.qtFaturada  - ttResult.qtDevolvida) * (ttResult.vlUnitVenda - ttResult.vlCustoUnitImportacao)
           ttResult.vlLucroProduto      = (ttResult.qtFaturada  - ttResult.qtDevolvida) * (ttResult.vlUnitVenda - (ttResult.vlUnitVenda * 0.3) - ttResult.vlCustoUnitAposImportacao)
           ttResult.percLucroContainer  = ttResult.vlLucroContainer /   (ttResult.vlFaturado  - ttResult.vlDevolvido)
           ttResult.percLucroProduto    = ttResult.vlLucroProduto  /   (ttResult.vlFaturado  - ttResult.vlDevolvido)
           .
END.

{esp/exportarTabelacsv3.i ttResult " " " " "  "ttResult" }

/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-estabel                      char
   20 nr-container                     inte        i
   30 num-pedido                       inte        m
   40 nome-ab-forn                     char
   50 cod-emit-forn                    inte        i
   60 comprador                        char
   70 narrativa                        char
   80 usuario                          char
   90 dt-compra                        date
  100 dt-prev-chegada                  date        i
  110 dt-recebimento                   date        i
  111 situacao                         inte        i
  120 char-1                           char
  130 dec-1                            deci-8
  140 int-1                            inte
  150 log-1                            logi
  160 data-1                           date
  170 vlr-frete                        deci-2
  180 vlr-seguro                       deci-2
  190 vlr-despesas                     deci-2
  200 exclusivo                        logi
  210 vl-dolar-venda                   deci-5
  220 vl-dolar-compra                  deci-5
  230 log_liberar_custo_realizado      logi
  240 medidas_container                char
  250 dt_prev_embarque                 date
  260 dt_real_embarque                 date
  261 ind_dt_embarque                  inte
  280 situacao_container_id            inte        i
  290 ind_dt_prev_chegada              inte
  310 log_pagto_final                  logi
  320 cod_agente_cargas                inte
  330 cod_armador                      inte
  340 cod_tipo_calculo                 inte
  350 perc_custo                       deci-2
  360 vl_dolar_di                      deci-5
  370 log_gerar_saldo_a_pagar          logi
  380 log_gerar_distr_custo            logi
  390 ordem_busca_dolar_compra         char
  400 ordem_busca_dolar_di             char
  410 cod_tipo_proj_dolar              inte
  420 vl_proj_dolar                    deci-5
  430 log_versao_custo_desatualizada   logi        i
  440 dt_hr_ult_alt_versao_custo       datetm
  450 ptax                             deci-5
  460 cod-depos                        char
  470 dt_prev_chegada_loja             date

*/
