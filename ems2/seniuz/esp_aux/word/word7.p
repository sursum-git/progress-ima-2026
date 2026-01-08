Def Var chWord As Com-handle No-undo.
Def Var chDocument As Com-handle No-undo. 
Def Var chOptions As Com-handle No-undo.
DEF VAR chTable AS COM-HANDLE NO-UNDO.
DEF VAR chRange AS COM-HANDLE NO-UNDO.
DEF VAR chCell AS COM-HANDLE NO-UNDO.

&GLOBAL-DEFINE wdBorderTop -1
&GLOBAL-DEFINE wdBorderLeft -2
&GLOBAL-DEFINE wdBorderBottom -3
&GLOBAL-DEFINE wdBorderRight -4
&GLOBAL-DEFINE wdBorderHorizontal -5
&GLOBAL-DEFINE wdBorderVertical -6
&GLOBAL-DEFINE wdBorderDiagonalDown -7
&GLOBAL-DEFINE wdBorderDiagonalUp -8


Create "Word.Application":U chWord.
chDocument = chWord:Documents:Add().
chWord:Visible = Yes. 

chWord:Selection:Font:bold = Yes. 
chWord:Selection:Font:Size = 18.
chWord:Selection:Font:Name = "Verdana".

chWord:Selection:TypeParagraph().
chWord:Selection:TypeParagraph().
chOptions = chWord:Options.

chRange = chWord:Selection:Range.
chTable = chword:Selection:Tables:ADD(chRange, /* Range */
3, /* NumRows */
5, /* NumColumns */
1, /*wdWord9TableBehavior */
0). /* wdAutoFitFixed */

chTable:COLUMNS:ITEM(1):PreferredWidthType = 3.
chTable:COLUMNS:ITEM(1):PreferredWidth = (2.1 * 72).
chTable:COLUMNS:ITEM(2):PreferredWidthType = 3.
chTable:COLUMNS:ITEM(2):PreferredWidth = (2.1 * 72).

chTable:COLUMNS:ITEM(1):TypeText("F bio Coelho Lanza").


/*
chTable:borders:item({&wdBorderTop}):lineStyle = 0.
chTable:borders:item({&wdBorderLeft}):lineStyle = 0.
chTable:borders:item({&wdBorderBottom}):lineStyle = 0.
chTable:borders:item({&wdBorderRight}):lineStyle = 0.
chTable:borders:item({&wdBorderHorizontal}):lineStyle = 0.
chTable:borders:item({&wdBorderVertical}):lineStyle = 0.
chTable:borders:item({&wdBorderDiagonalDown}):lineStyle = 0.
chTable:borders:item({&wdBorderDiagonalUp}):lineStyle = 0.
*/
chCell = chTable:cell(1,1):SELECT.
chWord:SELECTION:FONT:NAME = 'Arial'.
chWord:SELECTION:FONT:SIZE = 10.
chWord:SELECTION:InlineShapes:AddPicture("c:\lixo\amelhor.jpg", FALSE,TRUE).

