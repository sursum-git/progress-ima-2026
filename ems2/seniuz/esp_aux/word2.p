DEFINE VARIABLE WordAppl   AS COM-HANDLE. 
DEF VAR range AS COM-HANDLE.
DEF VAR vText AS CHAR.
DEF VAR numrows AS INT.
DEF VAR numcolumns AS INT.

CREATE "WordAppl:Application" WordAppl.   /* Abre o Word */

wordAppl:VISIBLE = TRUE. 
wordAppl:WindowState = 1.  /* 1-normal   2-minimizado */ 

WordAppl:Documents:ADD().

/*
/* Adiciona tabela de 2 linhas e 3 colunas  */
WordAppl:ActiveDocument:Tables:Add(Wordappl:Selection:Range, NumRows,NumColumns).
/* Escreve na primeira c‚lula */
WordAppl:Selection:TypevText(vText = 'Linha 1, Coluna 1').
/* Pr¢xima c‚lula */
WordAppl:Selection:MoveRight(12).
/* Escreve */
WordAppl:Selection:TypevText(vText = 'Linha 1, Coluna 2').
WordAppl:Selection:MoveRight(12).
WordAppl:Selection:TypevText(vText = 'Linha 1, Coluna 3').
WordAppl:Selection:MoveRight(12).
WordAppl:Selection:TypevText(vText = 'Linha 2, Coluna 1').
WordAppl:Selection:MoveRight(12).
WordAppl:Selection:TypevText(vText = 'Linha 2, Coluna 2').
WordAppl:Selection:MoveRight(12).
WordAppl:Selection:TypevText(vText = 'Linha 2, Coluna 3').

/* Auto-Formata */
WordAppl:Selection:Tables:Item(1):Select. /* Seleciona a 1§ tabela */
WordAppl:Selection:Cells:AutoFit. /* auto-formata */

/* Imprime 1 c¢pia */
WordAppl:ActiveDocument:PrintOut().

    /* Para salvar... */
WordAppl:ActiveDocument:SaveAs('c:\temp\Tabela.doc').

    /* Fecha documento */
WordAppl:ActiveDocument:Close().

/* Fecha o Word */
WordAppl:Quit.
*/
