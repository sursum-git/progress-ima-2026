DEFINE VARIABLE chWord AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chDocument AS COM-HANDLE NO-UNDO.
CREATE "Word.Application":U chWord.
IF chWord = ? THEN DO:
   MESSAGE "Microsoft Word NÆo Instalado no PC" VIEW-AS ALERT-BOX.
   RETURN.
END.
chDocument = chWord:Documents:ADD().
chWord:Selection:FONT:bold = NO.
chWord:Selection:FONT:size = 10.
chWord:Selection:Font:Name = "Verdana".
chWord:Selection:TypeText("F bio Coelho Lanza").
chWord:VISIBLE = YES.
RELEASE OBJECT chWord.
