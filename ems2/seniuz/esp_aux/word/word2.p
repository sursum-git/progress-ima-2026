define var ch-Word as component-handle no-undo.
define var ch-Documento as component-handle no-undo.
define var ch-FormField as component-handle no-undo.
define var ch-Range as component-handle no-undo.

Create "Word.Application" ch-Word.

ch-Word:ScreenUpdating = yes.
ch-Word:WindowState = 1.
ch-Word:Visible = yes.
ch-Word:System:Cursor = 0.
ch-Documento = ch-Word:Documents:Add("c:\temp\pro_word.doc").

ch-word:Selection:Font:Bold = true.
ch-word:Selection:TypeText(" http://progressetecnologia.blogspot.com/index.html ").

ch-word:Selection:Font:Bold = false.
ch-word:Selection:TypeText(" Progress na Web!!!").

ch-Word:System:Cursor = 1.

release object ch-Documento.
release object ch-Word.
