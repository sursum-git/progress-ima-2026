DEFINE VARIABLE chWordApplication       AS COM-HANDLE. 
DEFINE VARIABLE chDocument              AS COM-HANDLE. 
DEFINE VARIABLE chBookMark              AS COM-HANDLE. 
DEFINE VARIABLE chTable                 AS COM-HANDLE. 
DEFINE VARIABLE chCell                  AS COM-HANDLE. 


CREATE "Word.application" chWordApplication. 


IF NOT VALID-HANDLE(chWordApplication) THEN 
DO: 
    MESSAGE "Nao foi possivel Iniciar o Word" VIEW-AS ALERT-BOX. 
    RETURN. 
END. 


ASSIGN chDocument = chWordApplication:Documents:Add("C:\teste.dot"). 


for each emitente where cod-emitente = 1 : 


     RUN bookmark("nome",emitente.nome-emit). 
     RUN bookmark("endereco", emitente.endereco). 
     RUN bookmark("cidade", emitente.cidade). 
     RUN bookmark("estado", emitente.estado). 
     RUN bookmark("cep1", emitente.cep). 


     RUN bookmark("cnpj1", emitente.cgc). 
     RUN bookmark("fone1",emitente.telefone[1]). 
     RUN bookmark("fax1", emitente.telefone[2]). 
     RUN bookmark("Email", emitente.e-mail). 


end. 


chTable = chDocument:tables:Item(3). 


chcell = chtable:cell(1,1). 
chCell:Range:TEXT = "000579854". 


chTable:Rows:ADD(). 


chDocument:Protect(2,yes,"156adv!@"). 


  IF NOT chWordApplication:VISIBLE THEN 
     ASSIGN chWordApplication:Visible = TRUE. 
  ELSE 
  DO: 
      chDocument:PrintOut(). 
      chDocument:CLOSE(NO). 
  END. 


IF VALID-HANDLE(chWordApplication) THEN 
    RELEASE OBJECT chWordApplication. 


IF VALID-HANDLE(chDocument) THEN 
    RELEASE OBJECT chDocument. 


IF VALID-HANDLE(chBookMark) THEN 
    RELEASE OBJECT chBookMark. 


IF VALID-HANDLE(chTable) THEN 
    RELEASE OBJECT chTable. 


IF VALID-HANDLE(chCell) THEN 
    RELEASE OBJECT chCell. 


PROCEDURE BookMark : 
/*-------------------------------------------------------------------------ð----- 
    Purpose:     Substituir o conteudo de um BookMark no Word 
    Parameters:  <none> 
    Notes: 
---------------------------------------------------------------------------ð---*/ 
    DEFINE INPUT  PARAMETER nome AS CHARACTER  NO-UNDO. 
    DEFINE INPUT  PARAMETER valor AS CHARACTER  NO-UNDO. 


    /*MESSAGE chDocument:BookMarks:EXISTS(nome)  VIEW-AS ALERT-BOX.*/ 
    IF chDocument:BookMarks:EXISTS(nome) THEN DO: 
        ASSIGN chBookMark = chDocument:BookMarks:ITEM(nome). 


        IF valor = ? THEN 
            chBookMark:range:DELETE(). 
        ELSE 
            ASSIGN chBookMark:Range:TEXT = valor. 
    END. 
END PROCEDURE. 

 
