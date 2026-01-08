DEF NEW SHARED TEMP-TABLE tt-saldo-estoq
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

EMPTY TEMP-TABLE tt-saldo-estoq.

RUN esapi\connect-ima-med.p.

FOR EACH ITEM WHERE
         ITEM.ge-codigo >= 50 AND 
         ITEM.ge-codigo <= 60 AND 
         ITEM.it-codigo = '530157' NO-LOCK.

	RUN esrp/esimce025rp.p (INPUT ITEM.it-codigo).          
END.


IF CONNECTED('dbaux') THEN
   DISCONNECT dbaux.


FOR EACH tt-saldo-estoq WHERE
         tt-saldo-estoq.qt-disponivel > 0  NO-LOCK
      BY tt-saldo-estoq.it-codigo
      BY tt-saldo-estoq.cod-refer
      BY tt-saldo-estoq.cod-estabel.

    DISP tt-saldo-estoq.cod-estabel
         tt-saldo-estoq.it-codigo
         tt-saldo-estoq.cod-refer
         tt-saldo-estoq.qt-disponivel.
END.


