Def Var chWord     As Com-handle No-undo.
Def Var chDocument As Com-handle No-undo. 
Def Var chOptions  As Com-handle No-undo.

Create "Word.Application":U chWord.

chDocument = chWord:Documents:Add().

chWord:Visible = Yes.

chWord:Selection:Font:bold = Yes.
chWord:Selection:Font:Size = 18.
chWord:Selection:Font:Name = "Verdana".
chWord:Selection:TypeText("www.4each.com.br > Eugˆnio Augusto Marietti...").
chWord:Selection:TypeParagraph().
chWord:Selection:TypeParagraph().

chOptions = chWord:Options.

/* Imprimir na Bandeja 1 */

chOptions:DefaultTray = 'Tray 1'.  

chWord:Selection:TypeText("Bandeja 1").
chWord:Selection:TypeParagraph().

chDocument:PrintOut().

MESSAGE 'Clique para continuar...' VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* Imprimir na Bandeja 2 */

chOptions:DefaultTray = 'Tray 2'.

chWord:Selection:TypeBackspace().
chWord:Selection:TypeBackspace().
chWord:Selection:TypeText("2").
chWord:Selection:TypeParagraph().

chDocument:PrintOut().

Release Object chOptions.
Release Object chWord.

