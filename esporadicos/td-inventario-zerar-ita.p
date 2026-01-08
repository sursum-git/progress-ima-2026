{cdp/cd0666.i}
{cep/ceapi001.i}
{utp/ut-glob.i}
{esp/util.i}
 //Item;Est;Dep;Localizacao;Lote;Refer.;Validade;Dt Fabrica‡Æo;Un;Qtd Liquida
 DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

 FIND FIRST param-estoq NO-LOCK NO-ERROR.
 FOR EACH saldo-estoq NO-LOCK
    WHERE saldo-estoq.cod-depos = 'ita'
    AND saldo-estoq.qtidade-atu <> 0:   
     FOR FIRST ITEM FIELDS(it-codigo un)
      WHERE ITEM.it-codigo = saldo-estoq.it-codigo:
     END.
    
    ASSIGN iCont = iCont + 1.      
    CREATE tt-movto.
    ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
           tt-movto.cod-prog-orig           = "inv25/01"                        /* acrescentado*/
           tt-movto.usuario                 = c-seg-usuario                         /* acrescentado*/
           tt-movto.cod-estabel             = '505'                            /* param-estoq.estabel-pad */
           tt-movto.ct-codigo               = param-estoq.ct-acerto
           tt-movto.sc-codigo               = param-estoq.sc-acerto
           tt-movto.conta-contabil          = param-estoq.conta-acerto
           tt-movto.esp-docto               = 6
           tt-movto.tipo-trans              = IF saldo-estoq.qtidade-atu > 0 THEN 2 ELSE 1 
           tt-movto.cod-depos               = 'ita'
           tt-movto.dt-trans                = TODAY
           tt-movto.it-codigo               = saldo-estoq.it-codigo
           tt-movto.cod-refer               = saldo-estoq.cod-refer
           tt-movto.lote                    = saldo-estoq.lote
           tt-movto.quantidade              = abs(saldo-estoq.qtidade-atu)
           tt-movto.un                      = ITEM.un
           tt-movto.dt-vali-lote            = 12.31.9999           
           tt-movto.nro-docto               = tt-movto.cod-prog-orig  + "-" + STRING(iCont)
           .     
 END. 
 {esp/exportarTabelaCsv3.i tt-movto " " " except r-mov-inv r-mov-orig" "movto-td"} 
 
RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                    INPUT-OUTPUT TABLE tt-erro,
                    INPUT YES). /* Deleta erros? */
 
 
FIND FIRST tt-erro
WHERE cd-erro <> 56847 NO-LOCK NO-ERROR.     
IF AVAIL tt-erro THEN DO.
   FOR EACH tt-erro.
       MESSAGE "Erro ao reportar o item " SKIP
               tt-erro.mensagem
               tt-erro.cd-erro
               VIEW-AS ALERT-BOX ERROR.
   END.
END. 
 
 


 
/* 
 
DEFINE VARIABLE iEspDocto AS INT  NO-UNDO INIT 6.
DEFINE VARIABLE iTipoTrans AS INTEGER     NO-UNDO INIT 2.
FIND FIRST param-estoq NO-LOCK.
CREATE tt-movto.
ASSIGN tt-movto.cod-versao-integracao = 1                 /* acrescentado*/
       tt-movto.cod-prog-orig = "tstinv"                /* acrescentado*/
       tt-movto.usuario = c-seg-usuario                   /* acrescentado*/
       tt-movto.cod-estabel  = '505'                        /* param-estoq.estabel-pad */
       tt-movto.ct-codigo = IF iEspDocto = 33 
                            THEN param-estoq.ct-tr-transf
                            ELSE param-estoq.ct-acerto
       tt-movto.sc-codigo = IF iEspDocto = 33 
                            THEN param-estoq.sc-tr-transf
                            ELSE param-estoq.sc-acerto
       tt-movto.conta-contabil = IF iEspDocto = 33
                                 THEN param-estoq.conta-transf
                                 ELSE param-estoq.conta-acerto
       tt-movto.esp-docto = iEspDocto
       tt-movto.tipo-trans = iTipoTrans
       tt-movto.cod-depos = 'ITA'
       tt-movto.dt-trans = TODAY
       tt-movto.it-codigo = '130067'
       tt-movto.cod-refer = 'c158'
       tt-movto.lote = 'c158'
       tt-movto.quantidade = 2471.8
       tt-movto.un = 'M'
       tt-movto.dt-vali-lote = 12.31.9999.
       

RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                    INPUT-OUTPUT TABLE tt-erro,
                    INPUT YES). /* Deleta erros? */

FIND FIRST tt-erro
WHERE cd-erro <> 56847 NO-LOCK NO-ERROR.     
IF AVAIL tt-erro THEN DO.
   FOR EACH tt-erro.
       MESSAGE "Erro ao reportar o item " SKIP
               tt-erro.mensagem
               tt-erro.cd-erro
               VIEW-AS ALERT-BOX ERROR.
   END.
END.

*/
