DEF VAR i-cod-emit AS INTEGER.
DEF VAR c-finalidade AS CHAR.
DEF VAR c-cnae AS CHAR.
DEF VAR h-bonat001 AS HANDLE.
DEF VAR i-nr-pedido LIKE ped-venda.nr-pedido.
DEF VAR c-nat-oper AS CHAR.
DEF VAR i-param as INT.
DEF VAR l-ok AS LOGICAL.
DEF VAR c-erro AS CHAR.

ASSIGN i-nr-pedido = 187250.

FIND ped-venda WHERE
     ped-venda.nr-pedido = i-nr-pedido NO-LOCK NO-ERROR.

ASSIGN i-cod-emit = ped-venda.cod-emit.

RUN esbo/bonat001.p PERSISTENT SET h-bonat001.

RUN retornarfinalidadecliente IN h-bonat001 (INPUT i-cod-emit,
                                             OUTPUT c-finalidade, 
                                             OUTPUT c-cnae).
RUN retornarerros IN h-bonat001 (OUTPUT c-erro).
IF c-erro <> '' THEN DO.
   MESSAGE c-erro
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   NEXT.
END.
  
RUN buscarnatoperacao IN h-bonat001 (INPUT c-finalidade, 
                                     INPUT ped-venda.cod-estabel,
                                     INPUT i-cod-emit,
                                     INPUT ped-venda.nome-abrev-tri,  /* Cliente Triangular */
                                     OUTPUT c-nat-oper,
                                     OUTPUT i-param).

RUN retornarerros IN h-bonat001 (OUTPUT c-erro).
IF c-erro <> '' THEN DO.
   MESSAGE c-erro
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   NEXT.
END.

MESSAGE c-nat-oper SKIP ped-venda.nat-oper SKIP i-param
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*
RUN gravarparamnaturpedvenda (INPUT i-param,
                              INPUT ped-venda.nr-pedido,
                              INPUT ped-venda.cod-estabel,
                              OUTPUT l-ok).
*/

// criar campo finalidade na ped-venda-ext e gravar a finalidade
// criar campo param-nat-oper na ped-venda-ext e gravar o parametro utilizado

