DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR h-ceapi001k AS HANDLE NO-UNDO.
DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p-erro AS CHARACTER   NO-UNDO FORMAT 'x(250)'.
DEFINE TEMP-TABLE tt
    FIELD cLinha AS CHAR.
DEFINE TEMP-TABLE ttSaldo
    FIELD codEstab AS CHAR
    FIELD itCodigo AS CHAR
    FIELD codRefer AS CHAR
    FIELD deposito AS CHAR
    FIELD qtSaldo  AS DECIMAL
    FIELD VlSaldo  AS DECIMAL .


{cdp/cd0666.i}
{cep/ceapi001k.i}

INPUT FROM VALUE( 'u:\itensTerceirosMEd.txt').
   REPEAT:
       CREATE tt.
       IMPORT UNFORM tt.cLinha. 
       /*DISP tt.clinha.*/
   END.
INPUT CLOSE.

FOR EACH tt WHERE tt.cLinha <> '':
    FOR EACH saldo-estoq NO-LOCK
        WHERE saldo-estoq.cod-estabel = '5'
        AND   saldo-estoq.it-codigo   = trim(tt.cLinha)
        AND   qtidade-atu > 0 .
        CREATE ttSaldo.
        ASSIGN ttsaldo.codEstab  = saldo-estoq.cod-estabel
               ttSaldo.itCodigo  =  saldo-estoq.it-codigo
               ttSaldo.codRefer  =  saldo-estoq.cod-refer
               ttSaldo.deposito  =  saldo-estoq.cod-depos
               ttSaldo.qtSaldo   =  saldo-estoq.qtidade-atu.
        
    END.
END.
OUTPUT TO u:\lancCriadosTerceirosMED.txt.
FOR EACH ttSaldo WHERE ttSaldo.codEstab <> '':
    EXPORT DELIMITER "|" ttSaldo.

    RUN criarEstoque(ttSaldo.codEstab,ttSaldo.itCodigo,ttSaldo.codRefer,ttSaldo.codRefer,ttSaldo.qtSaldo,6,2,'TERCMED',OUTPUT p-erro).
    EXPORT DELIMITER "|" p-Erro.
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



