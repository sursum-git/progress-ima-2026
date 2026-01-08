/* {1} = Objeto (nome do radio set)
   {2} = Variavel char para retornar a nome de todos os radion-buttons 
   
   Obs.: definir no programa chamador uma vari vel de nome i-ct como inteira.
         def var i-ct as int.
*/
FORM {1} WITH WIDTH 550 FRAME f-aa.

ASSIGN {2} = {1}:RADIO-BUTTONS IN FRAME f-aa.


