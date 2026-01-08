DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-saldo-estoq
    FIELD  cod-estabel   LIKE movadm.saldo-estoq.cod-estabel 
    FIELD  empresa       LIKE mgadm.empresa.nome
    FIELD  it-codigo     LIKE movadm.saldo-estoq.it-codigo
    field  cod-depos     like movadm.saldo-estoq.cod-depos
    field  cod-refer     like movadm.saldo-estoq.cod-refer 
    FIELD  qt-disponivel like movadm.saldo-estoq.qtidade-atu
    field  qt-aloc-ped   like movadm.saldo-estoq.qt-aloc-ped 
    field  qt-alocada    like movadm.saldo-estoq.qt-alocada 
    field  qtidade-atu   like movadm.saldo-estoq.qtidade-atu
    field  qt-aloc-prod  LIKE movadm.saldo-estoq.qt-aloc-prod
    FIELD  qt-aloc-pi    LIKE movadm.saldo-estoq.qtidade-atu.

DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-estab-ini AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-estab-fim AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-refer-ini     AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-refer-fim     AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-depos-ini AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-cod-depos-fim AS CHARACTER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-estab         AS INT         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-frame         AS HANDLE      NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-outlet        AS INT         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-ge-ini        AS INTEGER   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE var-glb-ge-fim        AS INTEGER   NO-UNDO.

DEF VAR c-cod-depos AS CHAR.

DEF INPUT PARAMETER p-item AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER TABLE FOR tt-saldo-estoq.

FIND fnd_usuar_univ WHERE
     fnd_usuar_univ.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

FIND FIRST movadm.para-ped NO-LOCK NO-ERROR.

//ASSIGN c-emp-aux = IF fnd_usuar_univ.cod_empresa = '1' THEN '5' ELSE '1'
//       c-estab-aux = IF movadm.para-ped.estab-padrao = '1' THEN '5' ELSE '1'.

FOR EACH movadm.saldo-estoq WHERE
         movadm.saldo-estoq.cod-estab >= var-glb-cod-estab-ini AND
         movadm.saldo-estoq.cod-estab <= var-glb-cod-estab-fim AND
         movadm.saldo-estoq.it-codigo   = p-item AND 
         movadm.saldo-estoq.qtidade-atu <> 0 
         USE-INDEX ITEM NO-LOCK.
    
    IF movadm.saldo-estoq.cod-refer <> movadm.saldo-estoq.lote THEN NEXT.

    IF movadm.saldo-estoq.cod-depos = 'TFA' THEN NEXT.

    FIND tt-saldo-estoq WHERE 
         tt-saldo-estoq.it-codigo   = movadm.saldo-estoq.it-codigo AND 
         tt-saldo-estoq.cod-refer   = movadm.saldo-estoq.cod-refer AND 
         tt-saldo-estoq.cod-estabel = movadm.saldo-estoq.cod-estabel AND
         tt-saldo-estoq.cod-depos   = movadm.saldo-estoq.cod-depos NO-ERROR.
    IF NOT avail tt-saldo-estoq THEN DO.
       FIND mgadm.empresa WHERE 
            mgadm.empresa.ep-codigo = fnd_usuar_univ.cod_empresa NO-LOCK NO-ERROR.
       
       CREATE tt-saldo-estoq.
       ASSIGN tt-saldo-estoq.cod-estabel   = movadm.saldo-estoq.cod-estabel
              tt-saldo-estoq.empresa       = mgadm.empresa.nome
              tt-saldo-estoq.it-codigo     = movadm.saldo-estoq.it-codigo
              tt-saldo-estoq.cod-depos     = movadm.saldo-estoq.cod-depos   
              tt-saldo-estoq.cod-refer     = movadm.saldo-estoq.cod-refer.

       ASSIGN c-cod-depos = 'ARM'.
       FIND FIRST usuar-depos WHERE
                  usuar-depos.cod-estabel = tt-saldo-estoq.cod-estab NO-LOCK NO-ERROR.
       IF AVAIL usuar-depos THEN
          ASSIGN c-cod-depos = usuar-depos.cod-depos.

       ASSIGN tt-saldo-estoq.cod-depos = c-cod-depos.
    END.
    ASSIGN tt-saldo-estoq.qtidade-atu = tt-saldo-estoq.qtidade-atu + movadm.saldo-estoq.qtidade-atu.
END.

// Trata Pedidos WEB
FOR EACH peds_web WHERE
         peds_web.ind_sit_ped_web <= 2 OR 
         peds_web.ind_sit_ped_web  = 5 OR
         peds_web.ind_sit_ped_web >= 8 NO-LOCK,
    EACH itens_ped_web WHERE
         itens_ped_web.ped_web_id = peds_web.ped_web_id AND 
         itens_ped_web.it_codigo = p-item NO-LOCK.

    IF peds_web.cod_tipo_pedido = 'PI' THEN NEXT.
      
    FIND tt-saldo-estoq WHERE 
         tt-saldo-estoq.it-codigo   = itens_ped_web.it_codigo AND 
         tt-saldo-estoq.cod-refer   = itens_ped_web.cod_refer AND 
         tt-saldo-estoq.cod-estabel = peds_web.cod_estabel NO-ERROR.
    IF NOT AVAIL tt-saldo-estoq THEN DO.
       FIND mgadm.empresa WHERE 
            mgadm.empresa.ep-codigo = fnd_usuar_univ.cod_empresa NO-LOCK NO-ERROR.
       
       CREATE tt-saldo-estoq.
       ASSIGN tt-saldo-estoq.cod-estabel   = peds_web.cod_estabel
              tt-saldo-estoq.empresa       = mgadm.empresa.nome
              tt-saldo-estoq.it-codigo     = itens_ped_web.it_codigo
              tt-saldo-estoq.cod-refer     = itens_ped_web.cod_refer.

       ASSIGN c-cod-depos = 'ARM'.
       FIND FIRST usuar-depos WHERE
                  usuar-depos.cod-estabel = tt-saldo-estoq.cod-estab NO-LOCK NO-ERROR.
       IF AVAIL usuar-depos THEN
          ASSIGN c-cod-depos = usuar-depos.cod-depos.

       ASSIGN tt-saldo-estoq.cod-depos = c-cod-depos.
    END.

    ASSIGN tt-saldo-estoq.qt-aloc-ped = tt-saldo-estoq.qt-aloc-ped + itens_ped_web.qt_pedida. 
END.

/* Subtrai Pedidos em Aberto */
FOR EACH movadm.ped-venda WHERE
         movadm.ped-venda.cod-sit-ped = 1 NO-LOCK,
    EACH movadm.ped-item OF movadm.ped-venda WHERE
         movadm.ped-item.cod-sit-item = 1 AND
         movadm.ped-item.it-codigo = p-item
         NO-LOCK.

    IF movadm.ped-venda.tp-pedido = 'PI' THEN NEXT.

    FIND tt-saldo-estoq WHERE 
         tt-saldo-estoq.it-codigo   = movadm.ped-item.it-codigo AND 
         tt-saldo-estoq.cod-refer   = movadm.ped-item.cod-refer AND 
         tt-saldo-estoq.cod-estabel = movadm.ped-venda.cod-estabel  NO-ERROR.
    IF NOT AVAIL tt-saldo-estoq THEN DO.
       FIND mgadm.empresa WHERE 
            mgadm.empresa.ep-codigo = fnd_usuar_univ.cod_empresa NO-LOCK NO-ERROR.
       
       CREATE tt-saldo-estoq.
       ASSIGN tt-saldo-estoq.cod-estabel   = movadm.ped-venda.cod-estabel
              tt-saldo-estoq.empresa       = mgadm.empresa.nome
              tt-saldo-estoq.it-codigo     = movadm.ped-item.it-codigo
              tt-saldo-estoq.cod-refer     = movadm.ped-item.cod-refer.

       ASSIGN c-cod-depos = 'ARM'.
       FIND FIRST usuar-depos WHERE
                  usuar-depos.cod-estabel = tt-saldo-estoq.cod-estab NO-LOCK NO-ERROR.
       IF AVAIL usuar-depos THEN
          ASSIGN c-cod-depos = usuar-depos.cod-depos.

       ASSIGN tt-saldo-estoq.cod-depos = c-cod-depos.
    END.

    ASSIGN tt-saldo-estoq.qt-aloc-ped = tt-saldo-estoq.qt-aloc-ped + movadm.ped-item.qt-pedida. 
END.

/* Subtrai Notas em Aberto */ 
FOR EACH movadm.nota-fiscal WHERE
         //movadm.nota-fiscal.cod-estabel = movadm.para-ped.estab-padrao AND
         movadm.nota-fiscal.dt-cancela = ? AND
         movadm.nota-fiscal.dt-confirma = ? NO-LOCK,
    EACH movadm.it-nota-fisc OF nota-fiscal WHERE
         movadm.it-nota-fisc.it-codigo = p-item NO-LOCK.

    IF movadm.nota-fiscal.nome-abrev <> '' THEN NEXT.  /* ‚ triangular */

    FIND tt-saldo-estoq WHERE 
         tt-saldo-estoq.it-codigo   = movadm.it-nota-fisc.it-codigo AND 
         tt-saldo-estoq.cod-refer   = movadm.it-nota-fisc.cod-refer AND 
         tt-saldo-estoq.cod-estabel = movadm.nota-fiscal.cod-estabel NO-ERROR.
    IF NOT avail tt-saldo-estoq THEN DO.
       FIND mgadm.empresa WHERE 
            mgadm.empresa.ep-codigo = fnd_usuar_univ.cod_empresa NO-LOCK NO-ERROR.

       CREATE tt-saldo-estoq.
       ASSIGN tt-saldo-estoq.cod-estabel = movadm.nota-fiscal.cod-estabel
              tt-saldo-estoq.empresa     = mgadm.empresa.nome
              tt-saldo-estoq.it-codigo   = movadm.it-nota-fisc.it-codigo
              tt-saldo-estoq.cod-refer   = movadm.it-nota-fisc.cod-refer.

       ASSIGN c-cod-depos = 'ARM'.
       FIND FIRST usuar-depos WHERE
                  usuar-depos.cod-estabel = tt-saldo-estoq.cod-estab NO-LOCK NO-ERROR.
       IF AVAIL usuar-depos THEN
          ASSIGN c-cod-depos = usuar-depos.cod-depos.

       ASSIGN tt-saldo-estoq.cod-depos = c-cod-depos.
    END.
    ASSIGN tt-saldo-estoq.qt-alocada = tt-saldo-estoq.qt-alocada + movadm.it-nota-fisc.qt-faturada[1].

END.

/*
/* ****************** Lˆ dados da Outra Empresa ************************* */

IF var-glb-estab = 9 THEN DO:
   FOR EACH dbaux.saldo-estoq WHERE 
            dbaux.saldo-estoq.cod-estabel = c-estab-aux AND
            dbaux.saldo-estoq.it-codigo   = p-item AND 
            dbaux.saldo-estoq.qtidade-atu <> 0 AND 
            dbaux.saldo-estoq.cod-refer   >= var-glb-refer-ini AND 
            dbaux.saldo-estoq.cod-refer   <= var-glb-refer-fim AND 
            dbaux.saldo-estoq.cod-depos   >= var-glb-cod-depos-ini AND 
            dbaux.saldo-estoq.cod-depos   <= var-glb-cod-depos-fim
            USE-INDEX ITEM NO-LOCK.
        
       FIND tt-saldo-estoq WHERE 
            tt-saldo-estoq.it-codigo   = dbaux.saldo-estoq.it-codigo AND 
            tt-saldo-estoq.cod-refer   = dbaux.saldo-estoq.cod-refer AND 
            tt-saldo-estoq.cod-estabel = dbaux.saldo-estoq.cod-estabel AND 
            tt-saldo-estoq.cod-depos   = dbaux.saldo-estoq.cod-depos NO-ERROR.
       IF NOT avail tt-saldo-estoq THEN DO.
          FIND mgadm.empresa WHERE
               mgadm.empresa.ep-codigo = c-emp-aux NO-LOCK NO-ERROR.
           
          CREATE tt-saldo-estoq.
          ASSIGN tt-saldo-estoq.cod-estabel = dbaux.saldo-estoq.cod-estabel
                 tt-saldo-estoq.empresa     = mgadm.empresa.nome
                 tt-saldo-estoq.it-codigo   = dbaux.saldo-estoq.it-codigo
                 tt-saldo-estoq.cod-depos   = dbaux.saldo-estoq.cod-depos   
                 tt-saldo-estoq.cod-refer   = dbaux.saldo-estoq.cod-refer.
       END.
       ASSIGN tt-saldo-estoq.qtidade-atu = tt-saldo-estoq.qtidade-atu + dbaux.saldo-estoq.qtidade-atu. 
   END.

   /* Subtrai Pedidos em Aberto da MED*/
   FOR EACH dbaux.ped-venda WHERE
            dbaux.ped-venda.cod-sit-ped = 1 AND
            dbaux.ped-venda.cod-estabel = c-estab-aux NO-LOCK,
       EACH dbaux.ped-item OF dbaux.ped-venda WHERE
            dbaux.ped-item.cod-sit-item = 1 AND
            dbaux.ped-item.it-codigo = p-item AND
            dbaux.ped-item.cod-refer >= var-glb-refer-ini AND
            dbaux.ped-item.cod-refer <= var-glb-refer-fim 
            NO-LOCK.

       IF dbaux.ped-venda.tp-pedido = 'PI' THEN NEXT.

       FIND tt-saldo-estoq WHERE 
            tt-saldo-estoq.it-codigo   = dbaux.ped-item.it-codigo AND 
            tt-saldo-estoq.cod-refer   = dbaux.ped-item.cod-refer AND 
            tt-saldo-estoq.cod-estabel = dbaux.ped-venda.cod-estabel AND 
            tt-saldo-estoq.cod-depos   = 'ARM' NO-ERROR.
       IF NOT avail tt-saldo-estoq THEN DO.
          FIND mgadm.empresa WHERE 
               mgadm.empresa.ep-codigo = c-emp-aux NO-LOCK NO-ERROR.
           
          CREATE tt-saldo-estoq.
          ASSIGN tt-saldo-estoq.cod-estabel = dbaux.ped-venda.cod-estabel
                 tt-saldo-estoq.empresa     = mgadm.empresa.nome
                 tt-saldo-estoq.it-codigo   = dbaux.ped-item.it-codigo
                 tt-saldo-estoq.cod-depos   = 'ARM'
                 tt-saldo-estoq.cod-refer   = dbaux.ped-item.cod-refer.
       END.

       ASSIGN tt-saldo-estoq.qt-aloc-ped = tt-saldo-estoq.qt-aloc-ped + dbaux.ped-item.qt-pedida. 
   END.

   
   /* Subtrai Notas em Aberto da MED */ 
   FOR EACH dbaux.nota-fiscal WHERE
            dbaux.nota-fiscal.cod-estabel = c-estab-aux AND
            dbaux.nota-fiscal.dt-cancela = ? AND
            dbaux.nota-fiscal.dt-confirma = ? NO-LOCK,
       EACH dbaux.it-nota-fisc OF nota-fiscal WHERE
            dbaux.it-nota-fisc.it-codigo = p-item AND
            dbaux.it-nota-fisc.cod-refer >= var-glb-refer-ini AND
            dbaux.it-nota-fisc.cod-refer <= var-glb-refer-fim NO-LOCK.

       IF dbaux.nota-fiscal.nome-abrev <> '' THEN NEXT.  /* ‚ triangular */

       FIND tt-saldo-estoq WHERE 
            tt-saldo-estoq.cod-estabel = dbaux.nota-fiscal.cod-estabel AND 
            tt-saldo-estoq.it-codigo   = dbaux.it-nota-fisc.it-codigo AND 
            tt-saldo-estoq.cod-refer   = dbaux.it-nota-fisc.cod-refer AND 
            tt-saldo-estoq.cod-depos   = 'ARM' NO-ERROR.
       IF NOT avail tt-saldo-estoq THEN DO.
          FIND mgadm.empresa WHERE 
               mgadm.empresa.ep-codigo = c-emp-aux NO-LOCK NO-ERROR.
           
          CREATE tt-saldo-estoq.
          ASSIGN tt-saldo-estoq.cod-estabel = dbaux.it-nota-fisc.cod-estabel
                 tt-saldo-estoq.empresa     = mgadm.empresa.nome
                 tt-saldo-estoq.it-codigo   = dbaux.it-nota-fisc.it-codigo
                 tt-saldo-estoq.cod-refer   = dbaux.it-nota-fisc.cod-refer
                 tt-saldo-estoq.cod-depos   = 'ARM'.
       END.

       ASSIGN tt-saldo-estoq.qt-alocada = tt-saldo-estoq.qt-alocada + dbaux.it-nota-fisc.qt-faturada[1].
   END.
   
END.
*/



FOR EACH tt-saldo-estoq.
    /*
    IF tt-saldo-estoq.cod-estabel < var-glb-cod-estab-ini OR
       tt-saldo-estoq.cod-estabel > var-glb-cod-estab-fim OR
       tt-saldo-estoq.cod-refer   < var-glb-refer-ini OR
       tt-saldo-estoq.cod-refer   > var-glb-refer-fim THEN DO.
       DELETE tt-saldo-estoq.
       NEXT.
    END.
    */

    ASSIGN tt-saldo-estoq.qt-disponivel = tt-saldo-estoq.qtidade-atu 
                                          - tt-saldo-estoq.qt-aloc-ped 
                                          - tt-saldo-estoq.qt-alocada.
END.


//DISCONNECT dbaux.

