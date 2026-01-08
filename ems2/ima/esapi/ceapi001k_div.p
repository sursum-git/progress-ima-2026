DEFINE INPUT  PARAMETER cCaminho AS CHARACTER   NO-UNDO FORMAT 'x(300)'.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR h-ceapi001k AS HANDLE NO-UNDO.
DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO.
DEFINE VARIABLE p-erro AS CHARACTER   NO-UNDO FORMAT 'x(250)'.
DEFINE VARIABLE calcQt AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i      AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE tt
    FIELD cLinha AS CHAR.

DEFINE TEMP-TABLE ttEStoque
    FIELD tipo        AS INT
    FIELD data        AS DATE 
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
ASSIGN i = 0.
INPUT FROM VALUE( cCAminho).
   lerArquivo:
   REPEAT:
       CREATE tt.
       IMPORT UNFORM tt.cLinha. 
       ASSIGN i = i + 1.
       IF i = 1  THEN
          DELETE tt.
   END.
INPUT CLOSE.

OUTPUT TO c:\temp\registrosLidos.txt.
FOR EACH tt:
    EXPORT DELIMITER "|" tt.
    CREATE ttEstoque.
    ASSIGN ttEstoque.tipo        = int(ENTRY(1,tt.cLinha,";"))
           ttEstoque.data        = date(ENTRY(2,tt.cLinha,";"))
           ttEstoque.codEstab    = ENTRY(3,tt.cLinha,";")
           ttEstoque.itCodigo    = ENTRY(4,tt.cLinha,";")
           ttEstoque.codRefer    = ENTRY(5,tt.cLinha,";")
           ttEstoque.deposito    = ENTRY(6,tt.cLinha,";") 
           ttEstoque.quantidade  = dec(ENTRY(7,tt.cLinha,";")) 
           ttEstoque.valor       = dec(ENTRY(8,tt.cLinha,";"))
           ttEstoque.conta       = ENTRY(9,tt.cLinha,";") 
           ttEstoque.cc          = ENTRY(10,tt.cLinha,";") 
           ttEstoque.codEmitente = int(ENTRY(11,tt.cLinha,";")) 
           ttEstoque.serieDocto  = ENTRY(12,tt.cLinha,";") 
           ttEstoque.nroDocto    = ENTRY(13,tt.cLinha,";") 
           ttEstoque.descricao   = ENTRY(14,tt.cLinha,";") .
END.
OUTPUT CLOSE.
OUTPUT TO c:\temp\registrosaseremprocessados.txt.
FOR EACH ttEstoque:
    EXPORT DELIMITER "|" ttEstoque.
END.
OUTPUT CLOSE.
OUTPUT TO c:\temp\lancamentoscriados.txt.
FOR EACH ttEStoque WHERE ttEstoque.codEstab <> '':
    EXPORT DELIMITER "|" ttEstoque.
    RUN criarEstoque(ttEstoque.codEstab,
                     ttEstoque.itCodigo,
                     ttEstoque.codRefer,
                     ttEstoque.codRefer,
                     ttEstoque.quantidade,
                     6,
                     ttEstoque.tipo,
                     ttEStoque.descricao,
                     OUTPUT p-erro, 
                     ttEstoque.conta, 
                     ttEstoque.cc,
                     ttEstoque.codEmitente, 
                     ttEStoque.serieDocto, 
                     ttEStoque.nroDocto, ttEStoque.valor,
                     ttEstoque.data).
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
    DEFINE INPUT  PARAMETER p-valor             AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER p-data              AS DATE        NO-UNDO.


   
   FIND ITEM WHERE
        ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.
   FOR EACH tt-movto.
       DELETE tt-movto.
   END.
   
   CREATE tt-movto.
   ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
          tt-movto.cod-prog-orig           = "ceapi001k_div"              /* acrescentado*/
          tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
          tt-movto.cod-estabel             = p-cod-estabel
          tt-movto.ct-codigo               = p-conta 
          tt-movto.sc-codigo               = p-centro-custo
          tt-movto.esp-docto               = p-esp-docto
          tt-movto.tipo-trans              = p-tipo-trans
          tt-movto.cod-depos               = item.deposito-pad
          tt-movto.dt-trans                = p-data
          tt-movto.it-codigo               = p-it-codigo
          tt-movto.cod-refer               = p-cod-refer
          tt-movto.lote                    = p-lote
          tt-movto.quantidade              = p-qtde
          tt-movto.un                      = item.un
          tt-movto.dt-vali-lote            = 12.31.9999
          tt-movto.descricao-db            = p-justif
          tt-movto.nro-docto               = p-nro-docto
          tt-movto.serie-docto             = p-serie-docto
          tt-movto.cod-emitente            = p-cod-emitente 
          tt-movto.valor-mat-m[1]          = p-valor.
   
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
