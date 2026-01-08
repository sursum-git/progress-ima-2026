/* {1} = Objeto (nome do radio set)
   {2} = Variavel char para retornar a nome de todos os radion-buttons 
   
   Obs.: definir no programa chamador uma vari vel de nome i-ct como inteira.
         def var i-ct as int.
*/
FORM {1} WITH WIDTH 550 FRAME f-aa.

DO i-ct = 1 TO NUM-ENTRIES({1}:RADIO-BUTTONS IN FRAME f-aa) / 2.
   ASSIGN {2} = IF {2} = ""
                THEN ENTRY((i-ct - 1) * 2 + 1,{1}:RADIO-BUTTONS IN FRAME f-aa)
                ELSE {2} + "," + ENTRY((i-ct - 1) * 2 + 1,{1}:RADIO-BUTTONS IN FRAME f-aa).
END.

