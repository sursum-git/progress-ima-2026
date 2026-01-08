DEFINE VARIABLE chApplication         AS COM-HANDLE.
DEFINE VARIABLE chDocument            AS COM-HANDLE.

CREATE "CrystalRuntime.Application" chApplication.

chDocument = chApplication:OpenReport("c:\lixo\rodrigo.doc", 1).
/*chDocument.SetReportVariableValue(varname, varvalue).*/
chDocument:ReadRecords.
chDocument:PrintOut(TRUE,1,TRUE,1,1).
/*
chDocument.selectPrinter(drivername, printername, portnr).
chDocument.PrintOut(promptyesno, nrofcopied, collatedyesno, startpage, stoppage).*/


/* release com-handles */
RELEASE OBJECT chApplication.      
 

