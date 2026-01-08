/****************************************************************************
** Programa: epc-ce0403rp.p - EPC para Customizar o Dario
** Autor   : Toninho
** Data    : 04/01/2005
** Objetivo: Eliminar os movimentos do Recebimento F¡sico e 
**           limpar as narrativas do Diario Auxiliar do Estoque
*****************************************************************************/

{include/i-epc200.i1}

DEFINE INPUT        PARAMETER p-ind-event AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-epc.

DEFINE VARIABLE h-table  AS HANDLE.
DEFINE VARIABLE h-buffer AS HANDLE.
DEFINE VARIABLE h-query  AS HANDLE.

DEF NEW GLOBAL SHARED VAR wh-narrativa  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-rec-fisico AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR rw-movto-estoq AS ROWID.

FIND FIRST tt-epc NO-LOCK NO-ERROR.

IF tt-epc.cod-event = 'imprime-narrativa' AND
   tt-epc.cod-parameter = 'imprimir' THEN DO.
   IF wh-narrativa:INPUT-VALUE = NO THEN
      ASSIGN tt-epc.val-parameter = 'NO'.
END.

IF tt-epc.cod-event = 'update-fields' AND
   tt-epc.cod-parameter = 'tt-ge-mov(handle)' THEN DO.

   ASSIGN h-table = WIDGET-HANDLE(tt-epc.val-parameter).
   h-buffer = h-table:DEFAULT-BUFFER-HANDLE.
    
   CREATE QUERY h-query.
   h-query:SET-BUFFERS(h-buffer).
   h-query:QUERY-PREPARE("for each tt-ge-mov").
   h-query:QUERY-OPEN().
   h-query:GET-FIRST(). 
    
   REPEAT WHILE NOT h-query:QUERY-OFF-END:
       IF wh-narrativa:INPUT-VALUE = NO AND
          h-buffer:BUFFER-FIELD("descricao-db"):BUFFER-VALUE <> "" THEN 
          ASSIGN h-buffer:BUFFER-FIELD("descricao-db"):BUFFER-VALUE = "".
    
       IF wh-rec-fisico:INPUT-VALUE = NO AND
          h-buffer:BUFFER-FIELD("esp-docto"):BUFFER-VALUE = "NFE" THEN DO.
          ASSIGN rw-movto-estoq = h-buffer:BUFFER-FIELD("endereco"):BUFFER-VALUE.
          FIND movto-estoq WHERE
               ROWID(movto-estoq) = rw-movto-estoq NO-LOCK NO-ERROR.
          IF AVAIL movto-estoq AND
             movto-estoq.referencia BEGINS "RF" THEN
             h-buffer:BUFFER-DELETE.
       END.
       h-query:GET-NEXT().
   END.
   h-query:QUERY-CLOSE.
END.
