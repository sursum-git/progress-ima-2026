/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-estabel                      char        im
   20 embarque                         char        im
   30 numero-ordem                     inte        im
   40 parcela                          inte        im
   50 sequencia                        inte        im
   60 quantidade                       deci-4      m
   70 lote                             char
   80 dt-vali-lote                     date
   90 cod-refer                        char
  100 cod-depos                        char
  110 cod-localiz                      char
  120 char-1                           char
  130 char-2                           char
  140 dec-1                            deci-8
  150 dec-2                            deci-8
  160 int-1                            inte
  170 int-2                            inte
  180 log-1                            logi
  190 log-2                            logi
  200 data-1                           date
  210 data-2                           date
*/
FOR EACH pedido-compr
    WHERE pedido-compr.num-pedido = 2002 NO-LOCK:
    FOR EACH ordem-compra OF pedido-compr NO-LOCK:
        FOR EACH prazo-compr OF ordem-compra NO-LOCK:
            CREATE embarque-item.
            ASSIGN embarque-item.embarque        = '1486/16'
                   embarque-item.cod-estabel     = '5'
                   embarque-item.numero-ordem    = ordem-compra.numero-ordem
                   embarque-item.parcela         = prazo-compr.parcela  
                   embarque-item.sequencia       = prazo-compr.sequencia
                   embarque-item.quantidade      = prazo-compr.quantidade
                   embarque-item.lote            = prazo-compr.cod-refer
                   embarque-item.cod-refer       = prazo-compr.cod-refer
                   embarque-item.cod-depos       = ordem-compra.dep-almoxar
                   embarque-item.cod-localiz     = ''.
        END.
        

    END.

END.
