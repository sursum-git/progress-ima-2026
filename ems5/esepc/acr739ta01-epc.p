DEF INPUT PARAMETER h-query AS HANDLE.
DEF INPUT PARAMETER h-calc-col AS WIDGET-HANDLE.

DEF VAR h-buffer AS HANDLE.
DEF VAR h-cod-cliente AS HANDLE.
def var cCidade as char format 'x(50)'.

ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
       h-cod-cliente = h-buffer:BUFFER-FIELD(8).
FIND first emitente WHERE
     emitente.cod-emitente = h-cod-cliente:BUFFER-VALUE 
	 NO-LOCK NO-ERROR.
if avail emitente then 
        assign cCidade = emitente.cidade-cob  + "-"  + emitente.estado-cob.
   else
      assign cCidade = ''.
   



ASSIGN h-calc-col:SCREEN-VALUE = cCidade.

