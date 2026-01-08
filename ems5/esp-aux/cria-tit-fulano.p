DEF BUFFER b-ped-repre FOR ped-repre.

FOR EACH peds_web WHERE
         peds_web.ind_sit_ped_web = 4 NO-LOCK.


    FIND ped-venda WHERE 
         ped-venda.nr-pedido = peds_web.nr_pedido_erp NO-LOCK NO-ERROR.     
    IF ped-venda.cod-sit-ped = 6 THEN NEXT.

    IF ped-venda.cod-priori = 10 THEN NEXT.

    FIND ped-venda-ext WHERE
         ped-venda-ext.cod-estabel = '5' AND
         ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

    /* Verificar Desconto Informado */ 
    FIND ped-repre WHERE
         ped-repre.nr-pedido = INT(ped-venda.nr-pedcli) AND
         ped-repre.nome-ab-rep = 'Fulano' NO-LOCK NO-ERROR.

    IF AVAIL ped-repre THEN NEXT.


    FIND nota-fiscal WHERE
         nota-fiscal.nr-pedcli = ped-venda.nr-pedcli NO-LOCK NO-ERROR.

    DISP ped-venda.nr-pedido
         ped-venda.dt-implant
         DEC(ped-venda.des-pct-desconto-inform)
         nota-fiscal.nr-nota-fis
         WITH WIDTH 550.

    /*
    FIND FIRST ped-repre WHERE
               ped-repre.nr-pedido = INT(ped-venda.nr-pedcli) NO-LOCK NO-ERROR.

    CREATE b-ped-repre.
    BUFFER-COPY ped-repre TO b-ped-repre
        ASSIGN b-ped-repre.nome-ab-rep = 'Fulano'
               b-ped-repre.perc-comis = DEC(ped-venda.des-pct-desconto-inform).
    */


     /*

    FOR EACH tit_acr WHERE
             tit_acr.cod_tit_acr = nota-fiscal.nr-nota-fis NO-LOCK.
        /*
        CREATE repres_tit_acr.
        ASSIGN repres_tit_acr.cod_estab = tit_acr.cod_estab
               repres_tit_acr.num_id_tit_acr = tit_acr.num_id_tit_acr
               repres_tit_acr.cdn_repres = 99999
               repres_tit_acr.cod_livre_2 = 'm'.

        ASSIGN repres_tit_acr.val_perc_comis_repres = DEC(ped-venda.des-pct-desconto-inform).
        */
    END.
    */
END.


