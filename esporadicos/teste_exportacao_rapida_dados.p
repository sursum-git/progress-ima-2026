DEFINE VARIABLE tempo AS INT64     NO-UNDO.

ASSIGN tempo = ETIME(YES).

FUNCTION convDtEn RETURNS CHAR(data AS DATE):
    DEFINE VARIABLE cData AS CHARACTER   NO-UNDO.
    IF data = ? THEN
       RETURN ' '.
    RETURN STRING(YEAR(data))  + "-" +
           STRING(MONTH(data),'99') + "-" +
           STRING(DAY(data),'99')  .

END FUNCTION.
    
OUTPUT TO c:\temp\dados-nota-fiscal.txt.
PUT NOW SKIP.
FOR EACH nota-fiscal
    WHERE nota-fiscal.dt-emis-nota >= 01.01.2024.
    EXPORT DELIMITER "|" bairro caixa-postal cd-sit-desp cd-vendedor cdd-embarq cep cgc char-1 char-2 check-sum cidade 
        cidade-cif cn-codigo cod-canal-venda cod-chave-aces-nf-eletro cod-cond-pag cod-dep-ext cod-des-merc cod-emitente 
        cod-entrega cod-estab-estoq cod-estab-proces-export cod-estabel cod-imagem cod-lacre[1] cod-lacre[2] cod-mensagem 
        cod-placa-2 cod-portador cod-protoc cod-rep cod-rma cod-rota cod-safra cod-tax cod-tax-div cod-tax-emb cod-tax-fre 
        cod-tax-seg cod-uf-placa-2 cod-usuar-contingen-nf-eletro cond-redespa data-1 data-2 dec-1 dec-2 des-espec-volum 
        des-pct-desconto-inform desc-cancela desc-valor-nota desc-valor-ped descto1 descto2 distancia docto-orig 
        convdtEn(dt-at-ct) convdtEn(dt-at-est) convdtEn(dt-at-ofest) convdtEn(dt-atual-ap) convdtEn(dt-atual-cr) convdtEn(dt-atualiza) convdtEn(dt-cancela) 
        convdtEn(dt-confirma) convdtEn(dt-embarque) convdtEn(dt-emis-nota) convdtEn(dt-entr-cli) convdtEn(dt-prvenc) convdtEn(dt-saida) emite-duplic endereco endereco_text esp-docto estado fat-retro hr-atualiza hr-confirma hr-entr-cli hra-atualiz hra-emis-nf identific idi-forma-emis-nf-eletro idi-sit-nf-eletro ind-contabil ind-lib-nota ind-orig-entrada ind-sit-nota ind-tip-nota ind-tp-frete ind-via-envio ins-estadual int-1 int-2 invoice-id log-1 log-2 log-estorn-comis-repres log-juros-prorate log-possui-retenc log-usa-tabela-desconto log-var-cambial marca-volume mercado mo-codigo modalidade nat-operacao nivel-restituicao no-ab-reppri nome-ab-cli nome-ab-reg nome-abrev-tri nome-tr-red nome-transp nr-embarque nr-fat-retro nr-fatura nr-ind-finan nr-invoice nr-nota-ant nr-nota-fis nr-parcelas nr-pedcli nr-pedido-nf-orig nr-praz-med nr-proc-exp nr-resumo nr-siscomex nr-tab-finan nr-tabpre nr-volumes nro-nota-orig nro-proc-entrada nro-proc-saida num-cx-financ num-process-negoc num-rma-orig num-romaneio obs-gerada observ-nota pais pc-restituicao per-des-icms perc-desco1 perc-desco2 perc-tax-div perc-tax-emb perc-tax-fre perc-tax-seg peso-bru-tot peso-liq-tot placa preco-saida refer-cr refer-ct replica-nf serie serie-ant serie-orig tab-ind-fin tax-desp tax-div-me tax-emb tax-emb-me tax-fre-me tax-frete tax-seg tax-seg-me taxa-orig taxa-real tipo-fat tp-pedido tp-preco uf-placa user-calc val-desc-cofins-zfm val-desc-pis-zfm val-desconto-total val-desp-outros val-desp-outros-inf val-iof val-pct-desconto-tab-preco val-pct-desconto-total val-pct-desconto-valor vl-acum-dup vl-acum-dup-me vl-acumdup-e[1] vl-acumdup-e[2] vl-acumdup-e[3] vl-comis-nota vl-comis-nota-me vl-cotacao-fatur vl-cotacao-pedido vl-desconto vl-desconto-me vl-embalagem vl-embalagem-e[1] vl-embalagem-e[2] vl-embalagem-e[3] vl-embalagem-me vl-fatura vl-fatura-me vl-frete vl-frete-e[1] vl-frete-e[2] vl-frete-e[3] vl-frete-me vl-merc-tot-fat vl-merc-tot-fat-me vl-mercad vl-mercad-e[1] vl-mercad-e[2] vl-mercad-e[3] vl-mercad-me vl-seguro vl-seguro-e[1] vl-seguro-e[2] vl-seguro-e[3] vl-seguro-me vl-taxa-exp vl-taxaexp-e[1] vl-taxaexp-e[2] vl-taxaexp-e[3] vl-tot-com vl-tot-com-me vl-tot-ipi vl-tot-itens-fat vl-tot-itens-fat-me vl-tot-iva vl-tot-iva-me vl-tot-nota vl-tot-nota-me vl-totnota-e[1] vl-totnota-e[2] vl-totnota-e[3] zip-code .
END.
PUT NOW SKIP.

OUTPUT CLOSE.

DISP ETIME.

