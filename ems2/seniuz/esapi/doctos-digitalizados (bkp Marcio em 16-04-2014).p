DEFINE TEMP-TABLE tt-docs
       FIELD documento AS CHAR.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-emitente   AS ROWID   NO-UNDO.

DEF VAR h-prog      AS HANDLE NO-UNDO.
DEF VAR c-comando   AS CHAR FORMAT "x(100)".
DEF VAR c-arq-doc   AS CHAR.
DEF VAR c-pasta-doc AS CHAR FORMAT "x(50)".
DEF VAR c-acrobat   AS CHAR.
DEF VAR c-documento AS CHAR.
DEF VAR l-ok        AS LOG.

FIND emitente WHERE
     ROWID(emitente) = gr-emitente NO-LOCK NO-ERROR.

/* ASSIGN c-pasta-doc = "V:\" + emitente.nome-emit. */
RUN esapi/seek-diretorio.p (INPUT emitente.cod-emitente, OUTPUT c-pasta-doc).

IF c-pasta-doc = "" THEN DO:
    MESSAGE "Cliente: " + STRING(emitente.cod-emitente) + " - " + emitente.nome-emit SKIP(2)
            "N∆o foi Encontrado a Pasta do Cliente."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

ASSIGN c-arq-doc = SESSION:TEMP-DIRECTORY + c-seg-usuario + ".txt"
       c-comando = 'DIR /b "' + c-pasta-doc + '\*.pdf" > ' + c-arq-doc.

OS-COMMAND SILENT VALUE(c-comando).
    
EMPTY TEMP-TABLE tt-docs.

INPUT FROM VALUE(c-arq-doc).
REPEAT.
  CREATE tt-docs.
  IMPORT DELIMITER "%x" tt-docs.
END.
INPUT CLOSE.

FIND FIRST tt-docs where
           tt-docs.documento MATCHES "*.pdf" NO-ERROR.
IF NOT AVAIL tt-docs THEN DO.
/*   MESSAGE "N∆o foi Encontrado Arquivos Digitalizados para esse Cliente..."
       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    MESSAGE "Cliente: " + STRING(emitente.cod-emitente) + " - " + c-pasta-doc SKIP(2)
            "N∆o foi Encontrado Nenhum Arquivo Digitalizado na Pasta do Cliente."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
   RETURN.
END.

REPEAT.
    SYSTEM-DIALOG GET-FILE c-documento
            TITLE      "Escolha o Arquivo para Visualizar..."
            FILTERS    "PDF Files (*.pdf)"   "*.pdf",
                       "Todos Arquivos (*.*)"   "*.*"
            INITIAL-DIR c-pasta-doc
            MUST-EXIST
            USE-FILENAME
            UPDATE l-ok.
          
    IF NOT l-ok THEN LEAVE.

    /* Verifica a Existencia do Utilitario ADOBE READER */
    LOAD "AcroExch.Document" BASE-KEY "HKEY_CLASSES_ROOT".
    USE "AcroExch.Document".

    GET-KEY-VALUE SECTION "shell\open\command" KEY DEFAULT VALUE c-acrobat.
    UNLOAD "AcroExch.Document".

    IF c-acrobat = ? THEN DO:
       MESSAGE "O Utilitario ADOBE READER n∆o foi encontrado." SKIP
               "N∆o Ç possivel a execuá∆o do programa."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN.
    END.
    ASSIGN c-acrobat = ENTRY(2,c-acrobat,'"').

    RUN utp/ut-utils.p PERSISTENT SET h-prog.

    RUN EXECUTE IN h-prog (INPUT c-acrobat,
                           INPUT c-documento).

    DELETE PROCEDURE h-prog.
    PAUSE 2 NO-MESSAGE.
END.

