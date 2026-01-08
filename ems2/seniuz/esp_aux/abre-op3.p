/* Defini‡Æo das Includes para o Reporte */
{cdp/cdcfgman.i}
{cdp/cd0666.i}
{cpp/cpapi001.i}
{cpp/cpapi018.i}
{cpp/cpapi301.i}

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 1 NO-LOCK,
    FIRST ordem-benefic OF ob-etiqueta NO-LOCK.

    FIND item WHERE
         item.it-codigo = ordem-benefic.it-codigo NO-LOCK NO-ERROR.
    
    FIND FIRST item-uni-estab WHERE
               item-uni-estab.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
    FIND lin-prod WHERE
         lin-prod.nr-linha = item-uni-estab.nr-linha NO-LOCK NO-ERROR.

    FIND LAST ord-prod WHERE 
              ord-prod.it-codigo = ordem-benefic.it-codigo AND 
              ord-prod.cod-refer = ordem-benefic.cod-refer AND
              STRING(YEAR(ord-prod.dt-emissao)) + 
              STRING(MONTH(ord-prod.dt-emissao),"99") = STRING(YEAR(ob-etiqueta.dt-emissao)) + 
                                                        STRING(MONTH(ob-etiqueta.dt-emissao),"99") AND 
              ord-prod.nr-linha = lin-prod.nr-linha AND 
              ord-prod.cd-planejado = "AUT"
              USE-INDEX item-emiss NO-ERROR.

    IF NOT AVAIL ord-prod THEN DO.
       RUN pi-abr-op.

       IF RETURN-VALUE = "ADM-ERROR":U THEN DO.
          RETURN 'ADM-ERROR':U.
       END.

       FIND FIRST tt-ord-prod NO-ERROR.
       FIND ord-prod WHERE 
            ROWID(ord-prod) = tt-ord-prod.rw-ord-prod NO-ERROR.
        
       IF NOT AVAIL ord-prod THEN DO.
          MESSAGE "NÆo foi poss¡vel Abrir Ordem de Produ‡Æo" SKIP
                  "Informe ao CPD as mensagens a seguir..." VIEW-AS ALERT-BOX.
        
          FOR EACH tt-erro.
              MESSAGE tt-erro.mensagem VIEW-AS ALERT-BOX.
          END.
          RETURN 'ADM-ERROR':U.
       END.
        
       CREATE tt-dados.
       ASSIGN tt-dados.requis-por-ordem = YES
              tt-dados.estado = 1
              tt-dados.cod-versao-integracao = 001.
        
       CREATE tt-ord-prod-2.
       ASSIGN tt-ord-prod-2.nr-ord-produ = ord-prod.nr-ord-prod
              tt-ord-prod-2.it-codigo   = ord-prod.it-codigo
              tt-ord-prod-2.cod-estabel = ord-prod.cod-estabel
              tt-ord-prod-2.nr-linha    = ord-prod.nr-linha
              tt-ord-prod-2.qt-ordem    = ord-prod.qt-ordem.
        
       RUN cpp/cpapi018.p (INPUT TABLE tt-dados,
                           INPUT TABLE tt-ord-prod-2,
                           INPUT-OUTPUT TABLE tt-req-sum,
                           INPUT-OUTPUT TABLE tt-erro,
                           INPUT YES).
    END.

    RUN pi-reporta-mov.
    IF RETURN-VALUE = "ADM-ERROR":U THEN DO.
       RETURN 'ADM-ERROR':U.
    END.
END.

/*-------------------- Procedures -------------------- */

PROCEDURE pi-abre-op:
    FOR EACH tt-ord-prod.
        DELETE tt-ord-prod.
    END.
    
    FOR EACH tt-erro.
        DELETE tt-erro.
    END.
    
    CREATE tt-ord-prod.
    ASSIGN tt-ord-prod.nr-ord-prod = 0
           tt-ord-prod.it-codigo = ordem-benefic.it-codigo
           tt-ord-prod.cod-refer = ordem-benefic.cod-refer
           tt-ord-prod.qt-ordem =  500000
           tt-ord-prod.un = item.un
           tt-ord-prod.dt-inicio = 01.01.2005
           tt-ord-prod.dt-termino = 01.01.2005
           tt-ord-prod.cd-planejado = lin-prod.cd-planejado
           tt-ord-prod.estado = 2
           tt-ord-prod.cod-depos = item.deposito-pad
           tt-ord-prod.dt-emissao = 01.01.2005
           tt-ord-prod.conta-ordem = lin-prod.conta-ordem
           tt-ord-prod.ct-codigo = lin-prod.ct-ordem 
           tt-ord-prod.sc-codigo = lin-prod.sc-ordem
           tt-ord-prod.nr-linha = lin-prod.nr-linha 
           tt-ord-prod.reporte-ggf = 2
           tt-ord-prod.reporte-mob = 2
           tt-ord-prod.tipo = 1
           tt-ord-prod.rep-prod = 1
           tt-ord-prod.ind-tipo-movto = 1
           tt-ord-prod.cd-planejado = "AUT"
           tt-ord-prod.faixa-numeracao = 1
           tt-ord-prod.cod-versao-integracao = 003.
    
    RUN cpp/cpapi301.p (INPUT-OUTPUT TABLE tt-ord-prod,
                        INPUT-OUTPUT TABLE tt-reapro,
                        INPUT-OUTPUT TABLE tt-erro, 
                        INPUT YES).
    
    FIND FIRST tt-erro NO-LOCK NO-ERROR.
    IF AVAIL tt-erro THEN DO.
       FOR EACH tt-erro.
           MESSAGE tt-erro.mensagem VIEW-AS ALERT-BOX.
       END.
       RETURN "ADM-ERROR".
    END.
END PROCEDURE.

PROCEDURE pi-reporta-mov.
    FOR EACH tt-rep-prod.
        DELETE tt-rep-prod.
    END.

    FOR EACH tt-res-neg.
        DELETE tt-res-neg.
    END.

    FIND item WHERE 
         item.it-codigo = ord-prod.it-codigo NO-LOCK NO-ERROR.

    CREATE tt-rep-prod.
    ASSIGN tt-rep-prod.tipo         = 1
           tt-rep-prod.nr-ord-produ = ord-prod.nr-ord-prod
           tt-rep-prod.it-codigo    = ord-prod.it-codigo.

    ASSIGN tt-rep-prod.qt-reporte     = ob-etiqueta.quantidade
           tt-rep-prod.procura-saldos = NO
           tt-rep-prod.cod-depos-sai  = param-cp.dep-fabrica
           tt-rep-prod.data           = ob-etiqueta.dt-emissao
           tt-rep-prod.un             = ord-prod.un
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
        ASSIGN tt-res-neg.quantidade   = tt-res-neg.quantidade + reservas.quant-orig.
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
END PROCEDURE.
