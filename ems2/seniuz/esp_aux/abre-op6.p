/* Defini‡Æo das Includes para o Reporte */
{cdp/cdcfgman.i}
{cdp/cd0666.i}
{cpp/cpapi001.i}
{cpp/cpapi018.i}
{cpp/cpapi301.i}


FOR EACH tt-ord-prod.
    DELETE tt-ord-prod.
END.

FOR EACH tt-erro.
    DELETE tt-erro.
END.

FIND FIRST param-cp NO-LOCK NO-ERROR.

FIND ord-prod WHERE
     ord-prod.nr-ord-prod = 175170.

CREATE tt-ord-prod.
BUFFER-COPY ord-prod TO tt-ord-prod
ASSIGN tt-ord-prod.estado = 7
       tt-ord-prod.ind-tipo-movto = 2 /* 1-Inclui, 2-Modif, 3-Elim, 4-Copia */
       tt-ord-prod.cod-versao-integracao = 003.

RUN cpp/cpapi301.p (INPUT-OUTPUT TABLE tt-ord-prod,
                    INPUT-OUTPUT TABLE tt-reapro,
                    INPUT-OUTPUT TABLE tt-erro, 
                    INPUT YES).

FIND FIRST tt-erro NO-LOCK NO-ERROR.
IF AVAIL tt-erro THEN
   DISP tt-erro WITH 1 COL WIDTH 550.

