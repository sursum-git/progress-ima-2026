DEF NEW GLOBAL SHARED VAR h-acomp as handle    NO-UNDO. 
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp (input RETURN-VALUE).

/* Tabelas para Abertura das Oredens de Producao ---                                       */
{cpp/cpapi301.i}

FOR EACH tt-ord-prod.
    DELETE tt-ord-prod.
END.

FOR EACH tt-erros.
    DELETE tt-erros.
END.

FIND FIRST param-cp NO-LOCK NO-ERROR.
FIND FIRST ps-param-dfo NO-LOCK NO-ERROR.

FIND lin-prod WHERE
     lin-prod.nr-linha = 4 NO-LOCK NO-ERROR.

FIND item WHERE
     item.it-codigo = ps-param-dfo.it-produzido NO-LOCK NO-ERROR.

CREATE tt-ord-prod.
ASSIGN tt-ord-prod.nr-ord-prod = 0
       tt-ord-prod.it-codigo = item.it-codigo
       tt-ord-prod.qt-ordem =  item.lote-economi
       tt-ord-prod.un = item.un
       tt-ord-prod.dt-inicio = today
       tt-ord-prod.dt-termino = today
       tt-ord-prod.cd-planejado = lin-prod.cd-planejado
       tt-ord-prod.estado = 2
       tt-ord-prod.cod-depos = item.deposito-pad
       tt-ord-prod.dt-emissao = today
       tt-ord-prod.ct-codigo = lin-prod.ct-ordem 
       tt-ord-prod.sc-codigo = lin-prod.sc-ordem
       tt-ord-prod.cod-estabel = param-cp.cod-estabel
       tt-ord-prod.nr-linha = lin-prod.nr-linha
       tt-ord-prod.tipo = 1
       tt-ord-prod.rep-prod = 1
       tt-ord-prod.ind-tipo-movto = 1
       tt-ord-prod.faixa-numeracao = 1
       tt-ord-prod.cod-versao-integracao = 003.

RUN cpp/cpapi301.p (INPUT-OUTPUT TABLE tt-ord-prod,
                    INPUT-OUTPUT TABLE tt-reapro,
                    INPUT-OUTPUT TABLE tt-erros, 
                    INPUT YES).
/*
FIND FIRST tt-erros.
DISP tt-erros WITH 1 COL WIDTH 550.
*/

FIND FIRST tt-ord-prod.
FIND ord-prod WHERE
     ROWID(ord-prod) = tt-ord-prod.rw-ord-prod NO-LOCK NO-ERROR.

MESSAGE ord-prod.nr-ord-prod VIEW-AS ALERT-BOX.


