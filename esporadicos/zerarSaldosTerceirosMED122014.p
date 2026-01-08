DEFINE INPUT  PARAMETER cCaminho AS CHARACTER   NO-UNDO FORMAT 'x(300)'.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR h-ceapi001k AS HANDLE NO-UNDO.
DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p-erro AS CHARACTER   NO-UNDO FORMAT 'x(250)'.
DEFINE VARIABLE calcQt AS DECIMAL     NO-UNDO.
DEFINE TEMP-TABLE tt
    FIELD cLinha AS CHAR.

DEFINE TEMP-TABLE ttEstoque
    FIELD codEstab    AS CHAR
    FIELD itCodigo    AS CHAR
    FIELD codRefer    AS CHAR
    FIELD deposito    AS CHAR
    FIELD quantidade  AS DECIMAL
    FIELD valor       AS DECIMAL
    FIELD conta       AS CHAR
    FIELD cc          AS CHAR
    FIELD codEmitente AS INT
    FIELD serieDocto  AS CHAR
    FIELD nroDocto    AS CHAR FORMAT 'x(12)'
    FIELD descricao   AS CHAR FORMAT 'x(2000)'.

{cdp/cd0666.i}
{cep/ceapi001k.i}

INPUT FROM VALUE( cCAminho).
   REPEAT:
       CREATE tt.
       IMPORT UNFORM tt.cLinha. 
       /*DISP tt.clinha.*/
   END.
INPUT CLOSE.

OUTPUT TO c:\temp\lancamentoscriados.txt.
FOR EACH ttEstoque WHERE ttEstoque.codEstab <> '':
    EXPORT DELIMITER "|" ttEstoque.
    RUN criarEstoque(ttEstoque.codEstab,ttEstoque.itCodigo,ttEstoque.codRefer,ttEstoque.codRefer,ttEstoque.qtSaldo,6,2,ttEStoque.descricao,OUTPUT p-erro, ttEstoque.conta, ttEstoque.cc,
                     ttEstoque.codEmitente, ttEStoque.serieDocto, ttEStoque.nroDocto).
    EXPORT DELIMITER "|" p-Erro.
END.
OUTPUT CLOSE.




PROCEDURE criarEstoque:

    DEF INPUT  PARAMETER p-cod-estabel          AS CHAR.        
    DEF INPUT  PARAMETER p-it-codigo            AS CHAR.    
    DEF INPUT  PARAMETER p-cod-refer            AS CHAR.    
    DEF INPUT  PARAMETER p-lote                 AS CHAR.    
    DEF INPUT  PARAMETER p-qtde                 AS DEC.     
    DEF INPUT  PARAMETER p-esp-docto            AS INT.     
    DEF INPUT  PARAMETER p-tipo-trans           AS INT.     
    DEF INPUT  PARAMETER p-justif               AS CHAR.    
    DEF OUTPUT PARAMETER p-erro                 AS CHAR FORMAT 'x(250)'.   
    DEFINE INPUT  PARAMETER p-conta             AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER p-centro-custo      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER p-cod-emitente      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER p-serie-docto       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER p-nro-docto         AS CHARACTER   NO-UNDO FORMAT 'x(12)'.

   
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
          tt-movto.ct-codigo               = p-conta 
          tt-movto.sc-codigo               = p-centro-custo
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
/*

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 it-codigo                        char        im
   20 periodo                          date        im
   30 cod-estabel                      char        im
   40 cod-depos                        char        im
   50 quantidade                       deci-4      m
  100 saldo-medio                      deci-4
  110 val-unit-mat-m                   deci-4[3]
  120 val-unit-mob-m                   deci-4[3]
  130 val-unit-ggf-m                   deci-4[3]
  140 val-unit-mat-p                   deci-4[3]
  150 val-unit-mob-p                   deci-4[3]
  160 val-unit-ggf-p                   deci-4[3]
  170 val-unit-mat-o                   deci-4[3]
  180 val-unit-mob-o                   deci-4[3]
  190 val-unit-ggf-o                   deci-4[3]
  200 valor-mat-p                      deci-4[3]
  210 valor-mob-p                      deci-4[3]
  220 valor-ggf-p                      deci-4[3]
  230 valor-mat-o                      deci-4[3]
  240 valor-mob-o                      deci-4[3]
  250 valor-ggf-o                      deci-4[3]
  260 valor-mat-m                      deci-4[3]
  270 valor-mob-m                      deci-4[3]
  280 valor-ggf-m                      deci-4[3]
  290 char-1                           char
  300 char-2                           char
  310 dec-1                            deci-8
  320 dec-2                            deci-8
  330 int-1                            inte
  340 int-2                            inte
  350 log-1                            logi
  360 log-2                            logi
  370 data-1                           date
  380 data-2                           date
  390 check-sum                        char

*/
