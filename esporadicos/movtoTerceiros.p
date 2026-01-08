
OUTPUT TO   c:\temp\movto-estoq.txt.
FOR EACH movto-estoq
  WHERE (ct-codigo = '11500001'
    OR   ct-codigo = '11500003'
    OR    ct-codigo = '19000017' )
    AND dt-trans >= 11/01/2014
    AND   dt-trans <= 12/31/2014 :
    DISP ct-codigo ct-saldo it-codigo dt-trans quantidade valor-mat-m WITH WIDTH 550 .


END.
