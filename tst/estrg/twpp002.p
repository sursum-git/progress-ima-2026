TRIGGER PROCEDURE FOR WRITE OF pp-it-container.

IF  pp-it-container.qt-recebida = 0 AND pp-it-container.qt-pedida > 0 THEN
   ASSIGN pp-it-container.qt-recebida = pp-it-container.qt-pedida.


