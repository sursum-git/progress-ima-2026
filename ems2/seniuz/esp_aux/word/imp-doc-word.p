Def Var chWord     As Com-handle No-undo.
Def Var chDocument As Com-handle No-undo. 
Def Var chOptions  As Com-handle NO-UNDO.

CREATE "Word.Application":U chWord.

chDocument = chWord:Documents:OPEN("c:\temp\danfe.doc").

chWord:Visible = Yes.
/*chDocument:PrintOut(TRUE,FALSE,0,"","1","32767",0,1,"0",0,FALSE,FALSE,"",YES,0,0,0,0). */
chDocument:PrintOut(TRUE,1,TRUE,1,1).

Release Object chDocument.
Release Object chWord.
/*
Public Overridable Sub PrintOut ( _
	<OptionalAttribute> ByRef Background As Object, _
	<OptionalAttribute> ByRef Append As Object, _
	<OptionalAttribute> ByRef Range As Object, _
	<OptionalAttribute> ByRef OutputFileName As Object, _
	<OptionalAttribute> ByRef From As Object, _
	<OptionalAttribute> ByRef To As Object, _
	<OptionalAttribute> ByRef Item As Object, _
	<OptionalAttribute> ByRef Copies As Object, _
	<OptionalAttribute> ByRef Pages As Object, _
	<OptionalAttribute> ByRef PageType As Object, _
	<OptionalAttribute> ByRef PrintToFile As Object, _
	<OptionalAttribute> ByRef Collate As Object, _
	<OptionalAttribute> ByRef ActivePrinterMacGX As Object, _
	<OptionalAttribute> ByRef ManualDuplexPrint As Object, _
	<OptionalAttribute> ByRef PrintZoomColumn As Object, _
	<OptionalAttribute> ByRef PrintZoomRow As Object, _
	<OptionalAttribute> ByRef PrintZoomPaperWidth As Object, _
	<OptionalAttribute> ByRef PrintZoomPaperHeight As Object _
)
*/

/*
ASSIGN l$Background = YES
l$Append = NO
i$Range = 0 /* 0 All Document, 1 Selection, 2 Current Page, 3 From To, 4 Range Of Pages */
c$OutputFileName = ""
c$From = "1"
c$To = "32767"
i$Item = 0 /* 0 Document Content, 1 Properties, 2 Comments, 3 Styles, 4 Text Entries, 5 Key Assignments, 6 Envelope */
i$Copies = 1
c$Pages = "0" /* 0 All Pages, 1 Odd Pages Only, 2 Even Pages Only */
i$PageType = 0
l$PrintToFile = NO
l$Collate = NO
c$ActivePrinterMacGX = ""
l$ManualDuplexPrint = NO.
*/



/*
chDoc:PRINTOUT(l$Background,
l$Append,
i$Range,
c$OutputFileName,
c$From,
c$To,
i$Item,
i$Copies,
c$Pages,
i$PageType,
l$PrintToFile,
l$Collate,
c$ActivePrinterMacGX,
l$ManualDuplexPrint).
*/

/*
ByRef Background As Object, _
	ByRef Append As Object, _
	ByRef Range As Object, _
	ByRef OutputFileName As Object, _
	ByRef From As Object, _
	ByRef To As Object, _
	ByRef Item As Object, _
	ByRef Copies As Object, _
	ByRef Pages As Object, _
	ByRef PageType As Object, _
	ByRef PrintToFile As Object, _
	ByRef Collate As Object, _
	ByRef ActivePrinterMacGX As Object, _
	ByRef ManualDuplexPrint As Object, _
	ByRef PrintZoomColumn As Object, _
	ByRef PrintZoomRow As Object, _
	ByRef PrintZoomPaperWidth As Object, _
	ByRef PrintZoomPaperHeight As Object _
*/
