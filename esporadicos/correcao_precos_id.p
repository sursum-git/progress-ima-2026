
DEFINE VARIABLE h-bo        AS HANDLE       NO-UNDO.
DEFINE VARIABLE vlReal      AS DECIMAL      NO-UNDO.
DEFINE VARIABLE vlDolar     AS DECIMAL      NO-UNDO.
DEFINE VARIABLE idPrecoPE     AS INT        NO-UNDO.
DEFINE VARIABLE idPrecoOutlet AS INT        NO-UNDO.
DEFINE VARIABLE idPreco     AS INTEGER      NO-UNDO.
DEFINE VARIABLE tipoRetorno AS CHARACTER    NO-UNDO.  
DEFINE VARIABLE dtRefer     AS DATE          NO-UNDO.
    
RUN esbo/boPrecosItemRef.p PERSISTENT SET h-bo.
RUN iniciarBos      IN h-bo.
OUTPUT TO value('c:\temp\correcao_precos_' + STRING(TIME) + '.txt').
FOR EACH ped-venda 

    WHERE ped-venda.tp-pedido = 'pe'
    AND ped-venda.dt-implant >= 01.01.2018
    AND ped-venda.dt-implant  < 01.01.2024  NO-LOCK.
    FIND LAST nota-fiscal 
        WHERE nota-fiscal.nome-ab-cli = ped-venda.nome-abrev
        AND   nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli
        AND   nota-fiscal.dt-cancela  = ?
        NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN
       ASSIGN dtRefer = nota-fiscal.dt-emis-nota.
    ELSE
       ASSIGN dtRefer = ped-venda.dt-implant.

    FIND ped-venda-ext 
         WHERE ped-venda-ext.cod-estabel  =  ped-venda.cod-estabel
         AND   ped-venda-ext.nr-pedido    =  ped-venda.nr-pedido 
        NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ped-venda-ext THEN
       FIND LAST ped-venda-ext 
            WHERE ped-venda-ext.nome-abrev  =  ped-venda.nome-abrev
            AND   ped-venda-ext.nr-pedcli   =  ped-venda.nr-pedcli 
            NO-LOCK NO-ERROR.
    
    IF NOT AVAIL ped-venda-ext THEN DO:
       EXPORT DELIMITER "|" ped-venda.nr-pedido ped-venda.nome-abrev dtRefer
               IF AVAIL nota-fiscal THEN nota-fiscal.nr-nota-fis ELSE ''
               'sem ped-venda-ext' NOW.
       NEXT.
    END.

    FOR EACH ped-item OF ped-venda NO-LOCK.

        RUN limparTTPreco   IN h-bo.
        RUN limparTTMsg     IN h-bo.
        RUN setTbPreco      IN h-bo(ped-venda-ext.tb_preco). //1-padrao   2-rubi
        RUN setItem         IN h-bo(ped-item.it-codigo). 
        RUN setRef          IN h-bo(ped-item.cod-refer). 
        RUN setNrContainer  IN h-bo(ped-venda-ext.nr-container).
        RUN setTipoBusca    IN h-bo(0). // 0- todos, 1- pe, 2- pi
        RUN setDtRefer      IN h-bo(dtRefer).
        RUN buscarPrecos    IN h-bo.
        
        RUN getPrecoPrazo   IN h-bo (INPUT 'outlet',
                             OUTPUT vlReal,
                             OUTPUT vlDolar,
                             OUTPUT idPreco).
        ASSIGN idPrecoOutlet  = idPreco.

        RUN getPrecoPrazo   IN h-bo (INPUT 'pe',
                             OUTPUT vlReal,
                             OUTPUT vlDolar,
                             OUTPUT idPreco).

        ASSIGN idPrecoPE     = idPreco.
        
        FIND LAST ped-item-ext
            WHERE ped-item-ext.cod-estabel  = ped-venda.cod-estabel
            AND   ped-item-ext.nome-abrev   = ped-item.nome-abrev
            AND   ped-item-ext.nr-pedcli    = ped-item.nr-pedcli
            AND   ped-item-ext.nr-sequencia = ped-item.nr-sequencia
            AND   ped-item-ext.it-codigo    = ped-item.it-codigo
            AND   ped-item-ext.cod-refer    = ped-item.cod-refer
            NO-LOCK NO-ERROR.
        IF AVAIL ped-item-ext THEN DO:
           EXPORT DELIMITER "|"
               ped-venda.nr-pedido 
               ped-venda.nome-abrev 
               dtRefer
               IF AVAIL nota-fiscal THEN nota-fiscal.nr-nota-fis ELSE ''
               ped-venda.tp-pedido
               ped-venda-ext.tb_preco
               ped-item.it-codigo
               ped-item.cod-refer
               ped-item-ext.cod_controle_preco
               ped-item-ext.liquida-ima
               ped-item-ext.num-id-liquida-ima
               ped-item.vl-preuni
               ped-item.vl-preori
               idPrecoPE
               idPrecoOutlet
               idPrecoPE <> ped-item-ext.cod_controle_preco
               (ped-item-ext.liquida = YES OR ped-item-ext.num-id-liquida-ima <> '' )  AND idPrecoOutlet  = 0
               NOW .
        END.  
    END.
END.
OUTPUT CLOSE.


RUN finalizarBos IN h-bo.
/*
    
    
    
   IF ped-venda-ext.tb_preco = 1 AND (ped-venda-ext.tp-preco = 1 OR ped-venda-ext.tp-preco = 3)THEN DO: //padrao
              IF ped-item-ext.liquida = YES OR ped-item-ext.num-id-liquida-ima <> '' THEN DO:

              END.
              ELSE DO: //tabela padrao

              END.
           END.
           ELSE DO:

           END. 
    
    
    RUN setPrazoMedio   IN h-bo(prazoMedio:SCREEN-VALUE).
    
    //RUN setUfCliente    IN h-bo(). como n∆o tem mais IMA n∆o precisa
    RUN buscarPrecos    IN h-bo.

    IF nrContainer:SCREEN-VALUE = '0' THEN DO:
      IF cbTb:SCREEN-VALUE = '1' THEN DO:
         RUN getPrecoPrazo   IN h-bo (INPUT 'outlet',
                             OUTPUT vlReal,
                             OUTPUT vlDolar,
                             OUTPUT idPreco).
         ASSIGN vlidPrecoOutlet:SCREEN-VALUE =  STRING(vlReal)
                ididPrecoOutlet:SCREEN-VALUE = STRING(idPreco).

      END.
      RUN getPrecoPrazo   IN h-bo (INPUT 'pe',
                             OUTPUT vlReal,
                             OUTPUT vlDolar,
                             OUTPUT idPreco).
      ASSIGN vlPreco:SCREEN-VALUE =  STRING(vlReal)
             idPrecoTabela:SCREEN-VALUE = STRING(idPreco)
             vlPrecoDolar:SCREEN-VALUE = '0' 
             idPrecoTabelaDolar:SCREEN-VALUE = '0'.

    END.
    ELSE DO:
      RUN getPrecoPrazo   IN h-bo (INPUT 'pi',
                             OUTPUT vlReal,
                             OUTPUT vlDolar,
                             OUTPUT idPreco ).  

      ASSIGN vlPrecoDolar:SCREEN-VALUE =  STRING(vlDolar)
             idPrecoTabelaDolar:SCREEN-VALUE = STRING(idPreco)
             vlPreco:SCREEN-VALUE = STRING(vlReal) 
             idPrecoTabela:SCREEN-VALUE = STRING(idPreco)
             vlidPrecoOutlet:SCREEN-VALUE = '0'
             ididPrecoOutlet:SCREEN-VALUE = '0'.
    END.*/
