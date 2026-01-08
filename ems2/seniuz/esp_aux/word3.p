DEFINE VARIABLE WordAppl   AS COM-HANDLE. 

CREATE "Word.Application" WordAppl.   /* Abre o Word */

wordAppl:VISIBLE = TRUE. 
wordAppl:WindowState = 1.  /* 1-normal   2-minimizado */ 

WordAppl:Documents:ADD().

/* Adiciona tabela de 2 linhas e 3 colunas  */
WordAppl:ActiveDocument:Tables:Add(Wordappl:Selection:Range, 2,2).

/* Altera o Tamanho da c‚lula */
WordAppl:SELECTION:tables:ITEM(1):COLUMNS:ITEM(1):setwidth(100,NO).
WordAppl:SELECTION:tables:ITEM(1):COLUMNS:ITEM(2):setwidth(200,NO).

/* Escreve na primeira c‚lula */
WordAppl:Selection:TypeText('Linha 1, Coluna 1').

/* Pr¢xima c‚lula */
WordAppl:Selection:MoveRight(12).
WordAppl:Selection:TypeText('Linha 1, Coluna 2').

WordAppl:Selection:MoveRight(12).
WordAppl:Selection:TypeText('Linha 2, Coluna 1').

WordAppl:Selection:MoveRight(12).
WordAppl:Selection:TypeText('Linha 2, Coluna 2').


