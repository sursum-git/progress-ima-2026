
DEFINE VARIABLE iCont AS INT.
DEFINE VARIABLE cColigada AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE TEMP-TABLE tt
       FIELD codEmitPai     AS INT
       FIELD nomePai        AS CHAR
       FIELD codEmitFilho   AS INT
       FIELD nomeFilho      AS CHAR
       FIELD logDuplicado   AS LOG INIT NO.
DEFINE BUFFER bfEmit FOR emitente.
DEFINE BUFFER bfTt FOR tt .

FOR EACH emitente WHERE emitente.identific <> 2 NO-LOCK.

 /* FIND FIRST ped-venda
      WHERE ped-venda.nome-abrev = emitente.nome-abrev
      NO-LOCK NO-ERROR.
  IF NOT AVAIL ped-venda  OR emitente.identific = 1 THEN NEXT.*/

  FIND FIRST ext-emitente OF emitente NO-LOCK NO-ERROR.
  FIND tt
      WHERE tt.codEmitFilho = emitente.cod-emitente
      NO-ERROR.
  IF AVAIL tt  THEN NEXT.
  ASSIGN cColigada = IF AVAIL ext-emitente THEN ext-emitente.coligada ELSE ''.
  REPEAT iCont = 1 TO NUM-ENTRIES(cColigada,','):
      FIND FIRST bfEmit NO-LOCK
          WHERE bfEmit.cod-emitente = INT( entry(iCont,cColigada,",")) NO-ERROR .
          

      FIND FIRST tt
          WHERE (tt.codEmitPai = emitente.cod-emitente AND tt.codEmitFilho = int(entry(iCont,cColigada,",")))
          OR (tt.codEmitPai = int(entry(iCont,cColigada,","))  AND tt.codEmitFilho = emitente.cod-emitente)
          NO-ERROR.
      IF NOT AVAIL tt THEN DO:
         CREATE tt.
         ASSIGN tt.codEmitPai   = emitente.cod-emitente
                tt.nomePai      = emitente.nome-emit
                tt.codEmitFilho = IF AVAIL bfEmit THEN bfEmit.cod-emitente ELSE 0
                tt.nomeFilho    = IF AVAIL bfEmit THEN bfEmit.nome-emit    ELSE ''.

      END.
      
  END.
  CREATE tt.
   ASSIGN tt.codEmitPai   = emitente.cod-emitente
          tt.nomePai      = emitente.nome-emit
          tt.codEmitFilho = emitente.cod-emitente                            
          tt.nomeFilho    = emitente.nome-emit  .     


END.
OUTPUT TO c:\temp\coligadas.txt.
PUT "codigo pai;nome pai;codigo filho;nome filho" SKIP.
FOR EACH tt:
    EXPORT DELIMITER ";" tt.
    FIND FIRST bfTT
        WHERE bfTT.codEmitPai = tt.codEmitFilho NO-ERROR.
    ASSIGN tt.logDuplicado = AVAIL bfTT . 

    FIND FIRST bfTT
        WHERE bfTT.codEmitFilho = tt.codEmitPai NO-ERROR.
    ASSIGN tt.logDuplicado = AVAIL bfTT . 
END.
OUTPUT CLOSE.

