/*BEGIN CODE*/
/* define handles */
def var chWordApplication as COM-HANDLE NO-UNDO.
def var chDocument as COM-HANDLE NO-UNDO.
def var DocFullPath as char initial "c:\temp\rodrigo.doc".
def var PdfFullPath as char initial "c:\temp\rodrigo.pdf".


LOAD "Word.Application" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR. /* Open
Registry key */
if error-status:error 
then do:
UNLOAD "Word.Application". /* Close Registry key */
message "You must have Microsoft Word 95 or Microsoft Word 97" skip
"loaded on your computer before special printing will work."
view-as alert-box.
RETURN. 
end.
UNLOAD "Word.Application". /* Close Registry key */

/* create a new Word Application object */
CREATE "Word.Application" chWordApplication.

/* Open the standard document file */
chDocument = chWordApplication:Documents:Open(DocFullPath).

/* Print the current document */
/*chWordApplication:ActivePrinter = "Acrobat PDFWriter". */


LOAD 'Software' BASE-KEY 'HKEY_CURRENT_USER'. 

/* 'USE' IT (I DON'T KNOW WHAT THIS DOES, BUT IT ONLY WORKS IF IT'S HERE) */

USE 'Software'. 

/* PLACE THIS KEY/VALUE INTO THE 'ADOBE\ACROBAT PDFWRITER' SECTION OF THE 
   'SOFTWARE' SUBKEY */ 
PUT-KEY-VALUE SECTION 'Adobe\Acrobat PDFWriter'  KEY 'PDFFileName'
VALUE(PdfFullPath). 

/* UNLOAD THE SECTION BECAUSE WE'RE DONE WRITING TO IT */ 
UNLOAD 'Software'. 

chDocument:PrintOut().

pause 2 no-message.

/* Exits the Word application */
chWordApplication:Quit().

/* release com-handles */
RELEASE OBJECT chWordApplication. 
RELEASE OBJECT chDocument.
