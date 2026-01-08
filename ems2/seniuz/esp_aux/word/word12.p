DEFINE VARIABLE AppWord AS COM-HANDLE NO-UNDO.

CREATE "Word.Application" AppWord.
AppWord:visible = NO.
AppWord:Documents:Open("C:\temp\fabio.doc",False,False,False,"","",False).

AppWord:Selection:Find:Text = "$nome".
AppWord:Selection:Find:Replacement:Text = "".
AppWord:Selection:Find:Forward = True.
AppWord:Selection:Collapse.
AppWord:Selection:Find:Execute(,,,,,,,,,,1,,,,).

Appword:ActivePrinter = "PDF995".
Appword:PrintOut() NO-ERROR.
Appword:ActiveDocument:Close.
Appword:Quit().
release object appWord.
