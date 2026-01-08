{esapi/getSaldoEstoqPorEstabDtDepos.i}

DEFINE INPUT  PARAMETER pCodEstabel     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodDepos       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDtCorte        AS DATE        NO-UNDO.
DEFINE OUTPUT  PARAMETER TABLE FOR  ttQt   .

FOR EACH saldo-estoq NO-LOCK 
    WHERE saldo-estoq.cod-estabel   = pCodEstabel
    AND   saldo-estoq.cod-depos     = pCodDepos    
    :
    CREATE ttQt.
    ASSIGN ttQt.itCodigo        = saldo-estoq.it-codigo
           ttQt.codRefer        = saldo-estoq.cod-refer
           ttQt.qtSaldoAtu      = saldo-estoq.qtidade-atu.
           .                                              
END.
 
FOR EACH movto-estoq NO-LOCK
    WHERE movto-estoq.cod-estabel   = pCodEstabel
    AND  movto-estoq.cod-depos      = pCodDepos 
    AND  movto-estoq.dt-trans       > pDtCorte:
    
    FIND ttQt 
    WHERE ttQt.itCodigo =   movto-estoq.it-codigo
    AND   ttQt.codRefer =   movto-estoq.cod-refer
    NO-ERROR.
    IF AVAIL ttQt  THEN   DO:
       IF movto-estoq.tipo-trans = 1 THEN DO:
          ASSIGN ttQt.qtEntradaPos = ttQt.qtEntradaPos + movto-estoq.quantidade.           
       END.
       ELSE DO:
          ASSIGN ttQt.qtSaidaPos   =  ttQt.qtSaidaPos +  movto-estoq.quantidade.
       END.        
    END.       
END.

FOR EACH ttQt:
    ASSIGN ttQt.qtSaldoData = ttQt.qtSaldoAtu - ttQt.qtEntradaPos + ttQt.qtSaidaPos  .
    //DISP ttQt WITH WIDTH 550.
END.

