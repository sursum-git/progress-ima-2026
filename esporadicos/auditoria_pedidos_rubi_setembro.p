DEFINE VARIABLE iMedia      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtparc     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iqtDias     AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDescrPreco AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lAbaixoTb   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dTotTb      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTotInf     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE percTb      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dVlUnit     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lErrado     AS LOGICAL     NO-UNDO.


OUTPUT TO c:\temp\ped-venda-rubi.txt NO-CONVERT.
PUT "Dt.Implant|Nr.Pedido|Repres.|Nome Abrev.Cliente|Vl.Tot.Pedido|UF|Tp.Frete|% Comis.|Situa‡Æo|M‚dia Prazo|Moeda|Pre‡o Abaixo Tb.|% Tb.|Tot.Informado|Tot.Tabela|Errado?|Cond.Pagto" SKIP.
FOR EACH ped-venda NO-LOCK
    WHERE ped-venda.dt-implant >= 09.01.2023
    AND   ped-venda.dt-implant <= 09.30.2023 
    AND   ped-venda.cod-sit-ped <> 6. //diferente de pedido cancelado

   
    ASSIGN lErrado = NO.
    FIND repres
        WHERE repres.nome-abrev = ped-venda.no-ab-reppri
        NO-LOCK NO-ERROR.
    IF NOT AVAIL repres THEN NEXT.

    IF AVAIL repres THEN DO:
       IF repres.tipo-repres = 1 THEN NEXT. //funcionario
       IF repres.natureza  = 1 THEN NEXT. //PF 
    END.
    FIND ped-venda-ext 
        WHERE ped-venda-ext.nome-abrev = ped-venda.nome-abrev
        AND   ped-venda-ext.nr-pedcli  = ped-venda.nr-pedcli
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda-ext THEN NEXT .
    

    IF ped-venda-ext.tb_preco_id <> 2 THEN NEXT. //diferente de rubi
    
    FIND FIRST ped-repre OF ped-venda NO-LOCK NO-ERROR.

    IF NOT AVAIL ped-repre THEN NEXT.

    

    IF ped-venda.cod-cond-pag <> 0 THEN DO:
       FIND cond-pagto OF ped-venda NO-LOCK NO-ERROR. 
       ASSIGN iMedia = cond-pagto.qtd-dias-prazo-medio.
    END.
    ELSE DO:
        ASSIGN iqtParc = 0
               iQtDias = 0.
        FOR EACH cond-ped OF ped-venda NO-LOCK.
            ASSIGN iQtParc  = iQtParc + 1
                   iQtDias  = iQtDias + 1.
        END.
        ASSIGN iMedia = IF iQtParc > 0 THEN iQtDias / iQtParc ELSE 0.
    END.
    ASSIGN lAbaixoTb = NO
          dTotTb     = 0
          dTotInf    = 0
          percTb     = 0
          .   
    

    FOR EACH ped-item OF ped-venda
        WHERE ped-item.cod-sit-item <> 6 NO-LOCK. //diferente de cancelado
        FIND ped-item-ext
            WHERE ped-item-ext.cod-estabel      = ped-venda.cod-estabel
            AND   ped-item-ext.nome-abrev       = ped-venda.nome-abrev
            AND   ped-item-ext.nr-pedcli        = ped-venda.nr-pedcli
             AND  ped-item-ext.nr-sequencia     = ped-item.nr-sequencia
            AND ped-item-ext.it-codigo          = ped-item.it-codigo
            AND ped-item-ext.cod-refer          = ped-item.cod-refer NO-LOCK NO-ERROR.
        FIND controle_preco
            WHERE controle_preco.cod_controle_preco = ped-item-ext.cod_controle_preco
            NO-LOCK NO-ERROR.
        IF ped-venda.mo-codigo = 0 THEN DO:
           ASSIGN dVlUnit = controle_Preco.vl_real.  
        END.
        ELSE DO:
            ASSIGN dVlUnit = controle_Preco.vl_dolar.
        END.

        IF ped-item.vl-preori < dVlUnit THEN DO:
              IF lAbaixoTb = NO  THEN
                 ASSIGN lAbaixoTb = TRUE.
              
        END.     
        /*IF ped-item.qt-pedida = 0 THEN
           MESSAGE 'quantidade zerada'
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

        IF ped-item.vl-preori = 0 THEN
           MESSAGE 'vl.unitario'
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        */
        /*IF ped-venda.nr-pedido = 315560 THEN
           MESSAGE vl-preori SKIP
                   dVlunit   SKIP
                  lAbaixoTb
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        ASSIGN dTotTb  = dTotTb + ped-item.qt-pedida * dVlUnit
               dTotInf = dTotInf + ped-item.qt-pedida * ped-item.vl-preori.

/*         IF ped-venda.nr-pedido = 315530 THEN             */
/*            MESSAGE dTotTb SKIP                           */
/*                    dTotInf                               */
/*                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK. */

    END.    
    IF dTotTb <> 0 THEN
       ASSIGN percTb = dTotInf / dTotTb.
    ELSE 
       ASSIGN perctb = 0.
    IF ped-repre.perc-comis = 3 AND 
        (lAbaixoTb OR ped-venda.vl-tot-ped < 2500 OR 
         iMedia > 90 OR
         substr(ped-venda-ext.tp-frete,1,3) = "FOB"
         
         )
       THEN
       ASSIGN lErrado = YES.


    IF ped-repre.perc-comis <> 3  AND 
        (lAbaixoTb  = NO AND ped-venda.vl-tot-ped >= 2500 AND 
         iMedia > 90 AND 
         substr(ped-venda-ext.tp-frete,1,3) <> "FOB"
         )
       THEN
       ASSIGN lErrado = YES.

    
    
    EXPORT DELIMITER "|" 
        ped-venda.dt-implant
        ped-venda.nr-pedcli 
        ped-venda.no-ab-reppri 
        ped-venda.nome-abrev
        ped-venda.vl-tot-ped
        ped-venda.estado 
        //ped-venda.cidade-cif 
        ped-venda-ext.tp-frete
        ped-repre.perc-comis
        IF ped-venda.cod-sit-ped = 3 THEN 'FATURADO' ELSE "ABERTO"
        iMedia
        IF ped-venda.mo-codigo  = 0 THEN "REAL" ELSE "DOLAR"
        IF lAbaixoTb THEN "SIM" ELSE "NÇO"
        round(percTb * 100,2)
        round(dTotinf,2)
        round(dTotTb ,2)
        IF lErrado THEN "SIM" ELSE "NÇO"
        ped-venda.cod-cond-pag    
             
        .

END.
