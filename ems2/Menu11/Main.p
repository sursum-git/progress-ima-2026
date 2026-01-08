USING System.Windows.Forms.Application FROM ASSEMBLY.

DEFINE VARIABLE telaMenu AS Menu11 NO-UNDO.
DEFINE VARIABLE telaDeLogin AS Login NO-UNDO.

telaDeLogin = NEW Login ().
WAIT-FOR System.Windows.Forms.Application:Run ( telaDeLogin ).

IF telaDeLogin:getSituacaoLogin() = "OK" THEN 
    DO ON ERROR  UNDO, LEAVE
	ON ENDKEY UNDO, LEAVE
	ON STOP   UNDO, LEAVE
	ON QUIT   UNDO, LEAVE:
    
	telaMenu = NEW Menu11 ( ) .
	WAIT-FOR System.Windows.Forms.Application:Run ( telaMenu ).

END.

IF VALID-OBJECT(telaDeLogin) THEN	
    DELETE OBJECT telaDeLogin NO-ERROR.

IF VALID-OBJECT(telaMenu) THEN	
    DELETE OBJECT telaMenu NO-ERROR.

QUIT.
