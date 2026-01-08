/* Defini‡Æo das Includes para o Reporte */
{cdp/cdcfgman.i}
{cdp/cd0666.i}
{cpp/cpapi001.i}
{cpp/cpapi018.i}
{cpp/cpapi301.i}

FIND LAST ord-prod WHERE 
          ord-prod.nr-ord-prod = 167771 NO-ERROR.

FOR EACH tt-rep-prod.
    DELETE tt-rep-prod.
END.

FOR EACH tt-res-neg.
    DELETE tt-res-neg.
END.

FIND FIRST param-cp NO-LOCK NO-ERROR.

FIND item WHERE 
     item.it-codigo = ord-prod.it-codigo NO-LOCK NO-ERROR.

CREATE tt-rep-prod.
ASSIGN tt-rep-prod.tipo         = 1
       tt-rep-prod.nr-ord-produ = ord-prod.nr-ord-prod
       tt-rep-prod.it-codigo    = ord-prod.it-codigo.

ASSIGN tt-rep-prod.qt-reporte     = 10
       tt-rep-prod.it-codigo      = '502259'
       tt-rep-prod.procura-saldos = NO
       tt-rep-prod.cod-depos-sai  = param-cp.dep-fabrica
       tt-rep-prod.data           = TODAY
       tt-rep-prod.un             = 'kg'
       tt-rep-prod.conta-contabil = param-cp.ct-ordem + param-cp.sc-ordem
       tt-rep-prod.conta-refugo   = param-cp.ct-refugo + param-cp.sc-refugo
       tt-rep-prod.nro-docto      = STRING(ord-prod.nr-ord-produ)
       tt-rep-prod.cod-depos      = item.deposito-pad
       tt-rep-prod.cod-localiz    = item.cod-localiz
       tt-rep-prod.reserva        = NO
       tt-rep-prod.finaliza-ordem = NO
       tt-rep-prod.cod-versao-integracao = 001.

FOR EACH reservas WHERE
         reservas.nr-ord-produ = ord-prod.nr-ord-prod NO-LOCK. 

    FIND FIRST tt-res-neg WHERE
               tt-res-neg.it-codigo = reservas.it-codigo NO-ERROR.

    IF NOT AVAIL tt-res-neg THEN DO.           
       CREATE tt-res-neg.
       ASSIGN tt-res-neg.nr-ord-produ = ord-prod.nr-ord-prod
              tt-res-neg.it-codigo    = reservas.it-codigo
              tt-res-neg.cod-depos    = IF AVAIL reservas 
                                        THEN reservas.cod-depos
                                        ELSE ""
              tt-res-neg.cod-localiz  = IF AVAIL reservas
                                        THEN reservas.cod-localiz
                                        ELSE ""
              tt-res-neg.positivo = IF reservas.quant-orig >= 0
                                    THEN YES ELSE NO.
    END.          
    ASSIGN tt-res-neg.quantidade = tt-res-neg.quantidade + reservas.quant-orig.
END. 

RUN cpp/cpapi001.p (INPUT-OUTPUT TABLE tt-rep-prod,
                    INPUT        TABLE tt-refugo,
                    INPUT        TABLE tt-res-neg,
                    INPUT        TABLE tt-apont-mob,
                    INPUT-OUTPUT TABLE tt-erro,
                    INPUT        YES).

FIND FIRST tt-erro NO-LOCK NO-ERROR.
IF AVAIL tt-erro THEN DO.
   FOR EACH tt-erro.
       MESSAGE tt-erro.mensagem VIEW-AS ALERT-BOX.
   END.
   RETURN "ADM-ERROR".
END.
