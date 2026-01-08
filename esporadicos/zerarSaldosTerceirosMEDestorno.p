DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE VARIABLE p-erro AS CHARACTER   NO-UNDO FORMAT 'x(250)'.
DEF VAR h-ceapi001k AS HANDLE NO-UNDO.
{cdp/cd0666.i}
{cep/ceapi001k.i}
OUTPUT TO u:\lancCriadosTerceirosMEDestorno.txt.
 FOR EACH movto-estoq
    WHERE cod-prog-orig           = "zerarSaldosTerceirosMED":
    RUN criarEstoque(movto-estoq.cod-estabel,
                     movto-estoq.it-codigo,
                     movto-estoq.cod-Refer,
                     movto-estoq.cod-Refer,
                     movto-estoq.quantidade,6,1,'TERCMED',OUTPUT p-erro).
    DISP movto-estoq.cod-estabel
         movto-estoq.it-codigo
         movto-estoq.cod-Refer
         movto-estoq.cod-Refer
         movto-estoq.quantidade .
END.
OUTPUT CLOSE.



PROCEDURE criarEstoque:

    DEF INPUT  PARAMETER p-cod-estabel AS CHAR.        
    DEF INPUT  PARAMETER p-it-codigo   AS CHAR.    
    DEF INPUT  PARAMETER p-cod-refer   AS CHAR.    
    DEF INPUT  PARAMETER p-lote        AS CHAR.    
    DEF INPUT  PARAMETER p-qtde        AS DEC.     
    DEF INPUT  PARAMETER p-esp-docto   AS INT.     
    DEF INPUT  PARAMETER p-tipo-trans  AS INT.     
    DEF INPUT  PARAMETER p-justif      AS CHAR.    
    DEF OUTPUT PARAMETER p-erro        AS CHAR FORMAT 'x(250)'.   


   FIND FIRST param-estoq NO-LOCK NO-ERROR.
   
   FIND ITEM WHERE
        ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.
   
   FOR EACH tt-movto.
       DELETE tt-movto.
   END.
   
   
   CREATE tt-movto.
   ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
          tt-movto.cod-prog-orig           = "zerarSaldosTerceirosMED"              /* acrescentado*/
          tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
          tt-movto.cod-estabel             = p-cod-estabel
          tt-movto.ct-codigo               = IF p-esp-docto = 33 
                                                THEN param-estoq.ct-tr-transf
                                                ELSE param-estoq.ct-acerto
          tt-movto.sc-codigo               = IF p-esp-docto = 33 
                                                THEN param-estoq.sc-tr-transf
                                                ELSE param-estoq.sc-acerto
          tt-movto.esp-docto               = p-esp-docto
          tt-movto.tipo-trans              = p-tipo-trans
          tt-movto.cod-depos               = item.deposito-pad
          tt-movto.dt-trans                = 12.31.2015
          tt-movto.it-codigo               = p-it-codigo
          tt-movto.cod-refer               = p-cod-refer
          tt-movto.lote                    = p-lote
          tt-movto.quantidade              = p-qtde
          tt-movto.un                      = item.un
          tt-movto.dt-vali-lote            = 12.31.9999
          tt-movto.descricao-db            = p-justif.
   
   RUN cep/ceapi001k.p PERSISTENT SET h-ceapi001k.
   RUN pi-execute IN h-ceapi001k (INPUT-OUTPUT TABLE tt-movto,
                                  INPUT-OUTPUT TABLE tt-erro,
                                  INPUT YES). /* Deleta erros? */
   
   DELETE PROCEDURE h-ceapi001k.
   ASSIGN h-ceapi001k = ?.
   
   FIND FIRST tt-erro NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-erro THEN
      RETURN 'OK'.
   ELSE DO.
      ASSIGN p-erro = tt-erro.mensagem.
      RETURN 'ADM-ERROR'.
   END.
END.
