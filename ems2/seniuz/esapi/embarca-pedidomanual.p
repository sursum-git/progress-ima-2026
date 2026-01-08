DEF VAR p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF VAR p-nr-embarque LIKE embarque.nr-embarque.
UPDATE p-nr-pedcli p-nr-embarque.

DEFINE VARIABLE h-api  AS HANDLE     NO-UNDO.

DEF TEMP-TABLE tt-ped-venda NO-UNDO
    FIELD i-sequen    AS INT
    FIELD nr-embarque AS INT
    FIELD nome-abrev  AS CHAR FORMAT "x(12)"
    FIELD nr-pedcli   AS CHAR FORMAT "x(12)"
    FIELD ind-oper    AS INT  /* 1 - Alocar 2 - Desalocar */
    INDEX ch-pedido IS PRIMARY
          nome-abrev
          nr-pedcli.

RUN ftp/ftapi300.p PERSISTENT SET h-api.

FIND FIRST ped-venda
     WHERE ped-venda.nr-pedcli  = p-nr-pedcli
     EXCLUSIVE-LOCK NO-ERROR.
ASSIGN ped-venda.cod-sit-ped = 1.
FIND FIRST embarque
     WHERE embarque.nr-embarque = p-nr-embarque
      NO-LOCK NO-ERROR.

IF AVAIL embarque THEN DO:
   CREATE tt-ped-venda.
   ASSIGN tt-ped-venda.nr-embarque = embarque.nr-embarque  /* Embarque */
          tt-ped-venda.nome-abrev  = ped-venda.nome-abrev      
          tt-ped-venda.nr-pedcli   = ped-venda.nr-pedcli
          tt-ped-venda.ind-oper    = 1.                    /* Alocar */

   RUN pi-recebe-tt-ped-venda IN h-api (INPUT TABLE tt-ped-venda).
   RUN pi-trata-tt-ped-venda  IN h-api (INPUT YES).
END.
 ASSIGN ped-venda.cod-sit-ped = 3.
