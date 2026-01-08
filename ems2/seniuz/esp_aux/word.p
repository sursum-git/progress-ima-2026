DEFINE VARIABLE WordAppl   AS COM-HANDLE. 

CREATE "Word.Application" WordAppl.   /* Abre o Word */

wordAppl:VISIBLE = TRUE. 
wordAppl:WindowState = 1.  /* 1-normal   2-minimizado */ 

WordAppl:Documents:Open("C:\temp\teste.doc").



