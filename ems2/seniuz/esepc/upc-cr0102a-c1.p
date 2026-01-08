DEF INPUT PARAMETER p-h-browse AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-max-30  AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-buffer AS HANDLE.
DEF VAR h-browse AS HANDLE.
DEF VAR h-query AS HANDLE.

ASSIGN h-browse = p-h-browse.
ASSIGN h-query = h-browse:QUERY.

DEF VAR h-nome-abrev AS HANDLE.
DEF VAR h-campo AS HANDLE.

DEF VAR h-ttTabela AS HANDLE.

DEF VAR i-ct AS INT.
DEF VAR h-col AS HANDLE.

IF wh-max-30:SCREEN-VALUE <> '3' THEN DO.
   h-query:GET-FIRST NO-ERROR. 
   REPEAT ON ERROR UNDO, LEAVE:
      IF h-query:QUERY-OFF-END THEN LEAVE.
    
      ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
             h-nome-abrev = h-buffer:BUFFER-FIELD(5).
        
      FIND emitente WHERE
           emitente.nome-abrev = h-nome-abrev:BUFFER-VALUE NO-LOCK NO-ERROR.
    
      IF (wh-max-30:SCREEN-VALUE = '1' AND LENGTH(emitente.nome-emit) > 30) OR
         (wh-max-30:SCREEN-VALUE = '2' AND LENGTH(emitente.nome-emit) <= 30) THEN DO.
          h-query:REPOSITION-TO-ROW(h-query:CURRENT-RESULT-ROW). 
          h-browse:SELECT-FOCUSED-ROW().        
          h-query:DELETE-RESULT-LIST-ENTRY().
          h-browse:DESELECT-ROWS().        
      END.
      h-query:GET-NEXT() NO-ERROR.
   END.
END.

/*
Nome da Tablea e nome do Campo

ASSIGN h-ttTabela = h-query:GET-BUFFER-HANDLE.

ASSIGN h-campo = h-TTTABELA:BUFFER-FIELD('l-confirmado') NO-ERROR.

MESSAGE h-ttTabela:NAME SKIP
        h-campo:BUFFER-VALUE
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

*/


/*
ASSIGN h-campo = hQueryBuffer:BUFFER-FIELD('l-confirmado') NO-ERROR.

MESSAGE h-ttTabela:NAME SKIP
        h-campo:BUFFER-VALUE
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
  
/*
FOR FIRST tt-epc NO-LOCK
    WHERE tt-epc.cod-event     = "AtualizaDadosNFe":U
      AND tt-epc.cod-parameter = "ttVol":U :

    ASSIGN h-ttTabela   = WIDGET-HANDLE(tt-epc.val-parameter)
           hQueryBuffer = h-ttTabela:DEFAULT-BUFFER-HANDLE.

    IF VALID-HANDLE(hQueryBuffer) THEN DO:
        CREATE QUERY h-query.
                     h-query:SET-BUFFERS(hQueryBuffer).
                     h-query:QUERY-PREPARE('for each ' + h-ttTabela:NAME + ' no-lock').
                     h-query:QUERY-OPEN().
    
        REPEAT ON ERROR UNDO, LEAVE:
            h-query:GET-NEXT() NO-ERROR.
            IF h-query:QUERY-OFF-END THEN LEAVE.

            ASSIGN h-campo     = hQueryBuffer:BUFFER-FIELD('CodEstabelNF') NO-ERROR.
            ASSIGN c-estab     = h-campo:BUFFER-VALUE.
            ASSIGN h-campo     = hQueryBuffer:BUFFER-FIELD('SerieNF') NO-ERROR.
            ASSIGN c-serie     = h-campo:BUFFER-VALUE.
            ASSIGN h-campo     = hQueryBuffer:BUFFER-FIELD('NrNotaFisNF') NO-ERROR.
            ASSIGN c-nota      = h-campo:BUFFER-VALUE.
            ASSIGN h-campo     = hQueryBuffer:BUFFER-FIELD('SiglaEmb') NO-ERROR.
            ASSIGN c-sigla-emb = h-campo:BUFFER-VALUE.

            FIND FIRST nota-fiscal
                WHERE nota-fiscal.cod-estabel = c-estab AND
                      nota-fiscal.serie       = c-serie AND
                      nota-fiscal.nr-nota-fis = c-nota
                NO-LOCK NO-ERROR.

            FIND FIRST res-emb
                WHERE res-emb.nr-embarque = nota-fiscal.nr-embarque AND
                      res-emb.nr-resumo   = nota-fiscal.nr-resumo
                NO-LOCK NO-ERROR.
            
            IF AVAIL res-emb THEN DO:
                ASSIGN h-campo = hQueryBuffer:BUFFER-FIELD('esp') NO-ERROR.
                ASSIGN h-campo:BUFFER-VALUE = res-emb.sigla-emb.
                
                ASSIGN h-campo = hQueryBuffer:BUFFER-FIELD('marca') NO-ERROR.
                ASSIGN h-campo:BUFFER-VALUE = res-emb.marca-volume.
                
                ASSIGN h-campo = hQueryBuffer:BUFFER-FIELD('qVol') NO-ERROR.
                ASSIGN h-campo:BUFFER-VALUE  = STRING(nota-fiscal.nr-volumes).
                
                ASSIGN h-campo = hQueryBuffer:BUFFER-FIELD('nVol') NO-ERROR.
                ASSIGN h-campo:BUFFER-VALUE  = "".
            END.
        END.
    END.
END.
*/

     /*
     for each tt-destina:               
         reposition br-destina to rowid rowid(tt-destina). 
         
         br-destina:select-focused-row().        
                                                                                                                                                                       
          assign tt-destina.l-confirmado = yes.                                                                                                                                                                                                                                     
          get next br-destina.        
     end.
                            
     reposition br-destina to rowid row-destina. 
     assign br-destina:refreshable = yes.
     */
