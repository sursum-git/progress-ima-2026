DEF VAR de-ent AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-sai AS DEC FORMAT ">>>,>>>,>>9.99".

FOR EACH movto-estoq USE-INDEX item-data
    WHERE movto-estoq.it-codigo  >= "5"
      AND movto-estoq.it-codigo  <= "5z"
      AND movto-estoq.cod-estabel = "2"
      AND movto-estoq.cod-depos   = "exp"
      AND movto-estoq.dt-trans   >= 08/01/2005
      AND movto-estoq.dt-trans   <= 08/31/2005
      AND (movto-estoq.esp-docto  = 1 OR
           movto-estoq.esp-docto  = 8 OR 
           movto-estoq.esp-docto  = 35)
    NO-LOCK:

    IF movto-estoq.tipo-trans = 1 THEN
       ASSIGN de-ent = de-ent + movto-estoq.quantidade.
    ELSE 
       ASSIGN de-sai = de-sai + movto-estoq.quantidade.
    /*
    DISP movto-estoq.it-codigo
         movto-estoq.tipo-trans VIEW-AS FILL-IN
         movto-estoq.esp-docto VIEW-AS FILL-IN
         movto-estoq.quantidade.
    */
END.
DISP de-ent 
     de-sai
     de-ent - de-sai FORMAT ">>>,>>>,>>9.99".
