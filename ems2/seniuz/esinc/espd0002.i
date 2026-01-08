define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD estabel-ini      LIKE ped-venda.cod-estabel
       FIELD estabel-fin      LIKE ped-venda.cod-estabel
       FIELD pedido-ini       LIKE ped-venda.nr-pedido 
       FIELD pedido-fin       LIKE ped-venda.nr-pedido
       FIELD dt-emissao-ini   LIKE ped-venda.dt-emissao
       FIELD dt-emissao-fin   LIKE ped-venda.dt-emissao
       FIELD dt-entrega-ini   LIKE ped-venda.dt-entrega   
       FIELD dt-entrega-fin   LIKE ped-venda.dt-entrega   
       FIELD cod-emit-ini     LIKE ped-venda.cod-emitente 
       FIELD cod-emit-fin     LIKE ped-venda.cod-emitente 
       FIELD no-ab-reppri-ini LIKE ped-venda.no-ab-reppri 
       FIELD no-ab-reppri-fin LIKE ped-venda.no-ab-reppri
       FIELD nome-transp-ini  LIKE ped-venda.nome-transp
       FIELD nome-transp-fin  LIKE ped-venda.nome-transp
       FIELD corte-comerc-ini LIKE ped-item-ext.corte-comerc
       FIELD corte-comerc-fin LIKE ped-item-ext.corte-comerc
       FIELD grupo-rep-ini    AS   CHAR FORMAT "x(1)"  INIT '0'
       FIELD grupo-rep-fin    AS   CHAR FORMAT "x(1)"  INIT '3'
       FIELD so-indigo        AS   LOG
       FIELD exc-indigo       AS   LOG
       FIELD it-codigo        LIKE item.it-codigo EXTENT 10
       FIELD sit-total        AS   LOG
       FIELD sit-aberto       AS   LOG 
       FIELD sit-parcial      AS   LOG
       FIELD sit-pendentes    AS   LOG 
       FIELD sit-suspensos    AS   LOG 
       FIELD sit-cancelados   AS   LOG
       FIELD cond-credito     AS   CHAR FORMAT "x"
       FIELD cond-pagto       AS   CHAR FORMAT "x"
       FIELD mercado          AS   CHAR FORMAT "x"
       FIELD tp-pedido        LIKE ped-venda.tp-pedido
       FIELD aceita-parc      AS   CHAR FORMAT "x"
       FIELD qtd-minima       AS   DEC FORMAT ">>>,>>>,>>9.99" 
       FIELD perc-minres      AS   DEC FORMAT ">>9.99"
       FIELD min-it-ares      AS   INT FORMAT ">>>9" 
       FIELD max-it-ares      AS   INT FORMAT ">>>9"
       FIELD it-reservados    AS   LOG
       FIELD nr-coletor       AS   INT.
