/* Programa: 
** Objetivo: Converter Um Documento WORD em PDF, e Uma Maquina
**           que Tenha o PDF995 Instalado.
** Autor...: FµBIO COELHO LANZA - NOVEMBRO/2010.
*/ 
 
 /* define handles */ 
 DEF VAR chWordApplication AS COM-HANDLE NO-UNDO. 
 DEF VAR chDocument        AS COM-HANDLE NO-UNDO. 
 DEF VAR PdfFileName       AS CHAR INITIAL "c:\temp\fabio.pdf". 
 DEF VAR c-imp-padrao      AS CHAR.
 
 /* Altera a chave do PDF995.INI informado-o para gerar o pedido em PDF automaticamente */
 LOAD "PDF995" DIR "C:\PDF995\RES". 
 USE "PDF995".
 PUT-KEY-VALUE SECTION "Parameters" KEY "Output File" VALUE PdfFileName.

 /* Open Registry key        */
 LOAD "Word.Application" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR. 
 IF ERROR-STATUS:ERROR  THEN DO: 
    UNLOAD "Word.Application". /* Close Registry key */ 
    MESSAGE "Voce deve Ter o Word Instalado na Maquina" SKIP 
            "Para poder Imprimir Relatorio." 
    VIEW-AS ALERT-BOX. 
    RETURN. 
 END. 
 UNLOAD "Word.Application". /* Close Registry key */ 
 
 /* Cria uma Nova Aplica‡Æo WORD objecto */ 
 CREATE "Word.Application" chWordApplication. 
 
 /* Abre o Documento WORD */ 
 chDocument = chWordApplication:Documents:OPEN("c:\temp\fabio.doc"). 
 
 /* Salva a Impressora Default */ 
 c-imp-padrao = chWordApplication:ActivePrinter. 

 /* Faz a Impressora PDF995 a Impressora Default */
 chWordApplication:ActivePrinter = "PDF995". 
 
 chDocument:PrintOut(). 
 PAUSE 2 NO-MESSAGE. 

 /* Restaura a Impressora Defautl do Windows */
 chWordApplication:ActivePrinter = c-imp-padrao. 

 /* Restaura o pdf995.ini original */
 PUT-KEY-VALUE SECTION "Parameters" KEY "Output File" VALUE " ".
 UNLOAD "PDF995".

/* sai a aplica‡Æo WORD */ 
 chWordApplication:Quit(). 
 
 /* release com-handles */ 
 RELEASE OBJECT chWordApplication. 
 RELEASE OBJECT chDocument. 
