/* Devolve o nome de um redio-button */
/* Toninho - SeniuZ */
FORM {1} WITH WIDTH 550 FRAME f-aa.
ASSIGN {3} = ENTRY(({2} - 1) * 2 + 1,{1}:RADIO-BUTTONS IN FRAME f-aa) NO-ERROR.
