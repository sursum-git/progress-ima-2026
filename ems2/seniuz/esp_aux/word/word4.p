DEFINE VARIABLE chWord AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE chDoc AS COMPONENT-HANDLE NO-UNDO.

DEFINE VARIABLE c$Filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE l$ConfirmConversions AS LOGICAL NO-UNDO.
DEFINE VARIABLE l$ReadOnly AS LOGICAL NO-UNDO.
DEFINE VARIABLE l$AddToRecentFiles AS LOGICAL NO-UNDO.
DEFINE VARIABLE c$PasswordDocument AS CHARACTER NO-UNDO.
DEFINE VARIABLE c$PasswordTemplate AS CHARACTER NO-UNDO.
DEFINE VARIABLE l$Revert AS LOGICAL NO-UNDO.
DEFINE VARIABLE c$WritePassWordDocument AS CHARACTER NO-UNDO.
DEFINE VARIABLE c$WritePasswordTemplate AS CHARACTER NO-UNDO.
DEFINE VARIABLE i$Format AS INTEGER NO-UNDO.
DEFINE VARIABLE l$Background AS LOGICAL NO-UNDO.
DEFINE VARIABLE l$Append AS LOGICAL NO-UNDO.
DEFINE VARIABLE i$Range AS INTEGER NO-UNDO.
DEFINE VARIABLE c$OutputFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE c$From AS CHARACTER NO-UNDO.
DEFINE VARIABLE c$To AS CHARACTER NO-UNDO.
DEFINE VARIABLE i$Item AS INTEGER NO-UNDO.
DEFINE VARIABLE i$Copies AS INTEGER NO-UNDO.
DEFINE VARIABLE c$Pages AS CHARACTER NO-UNDO.
DEFINE VARIABLE i$PageType AS INTEGER NO-UNDO.
DEFINE VARIABLE l$PrintToFile AS LOGICAL NO-UNDO.
DEFINE VARIABLE l$Collate AS LOGICAL NO-UNDO.
DEFINE VARIABLE c$ActivePrinterMacGX AS CHARACTER NO-UNDO.
DEFINE VARIABLE l$ManualDuplexPrint AS LOGICAL NO-UNDO.


CREATE "Word.Application.8" chWord.

ASSIGN c$Filename = "c:\test.doc"
l$ConfirmConversions = YES
l$ReadOnly = YES
l$AddToRecentFiles = NO
c$PasswordDocument = ""
c$PasswordTemplate = ""
l$Revert = NO
c$WritePasswordDocument = ""
c$WritePasswordTemplate = ""
i$Format = 0.

ASSIGN chWord:VISIBLE = NO
chDoc = chWord:DOCUMENTS:OPEN(c$Filename,
l$ConfirmConversions,
l$ReadOnly,
l$AddToRecentFiles,
c$PasswordDocument,
c$PasswordTemplate,
l$Revert,
c$WritePasswordDocument,
c$WritePasswordTemplate,
i$Format).

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

chDoc:CLOSE().

PAUSE 2. /* To allow document to spool before quitting Word */

RELEASE OBJECT chDoc.

chWord:QUIT().

RELEASE OBJECT chWord.

