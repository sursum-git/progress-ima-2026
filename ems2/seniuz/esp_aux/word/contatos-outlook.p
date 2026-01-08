DEFINE VARIABLE chOutlook    AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chNamespace  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chFolder     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chMailItem   AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chRecipients AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iCnt         AS INTEGER    NO-UNDO.

/* DEF TEMP-TABLE ttaddress */

CREATE "Outlook.Application" chOutlook CONNECT NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    CREATE "Outlook.Application" chOutlook NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.
END. /* error-status */

ASSIGN chNamespace = chOutlook:GetNameSpace("MAPI":U)
       chFolder    = chNamespace:GetDefaultFolder(10) NO-ERROR. /* Contacts
*/

DO iCnt = 1 TO chFolder:Items:Count():

    ASSIGN chMailItem = chFolder:Items(iCnt) NO-ERROR.

    IF VALID-HANDLE(chMailItem) THEN DO:

        MESSAGE chMailItem:FullName SKIP
                chMailItem:Email1Address
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*
        DISP chMailItem:FullName FORMAT "x(30)".
        DISP chMailItem:Email1Address FORMAT "x(30)".
*/
/*         CREATE ttAddress.                                                 */
/*         ASSIGN ttAddress.ttFullName  = chMailItem:FullName                */
/*                ttAddress.ttEmailAddy = chMailItem:Email1Address NO-ERROR. */

    END.

    RELEASE OBJECT chMailItem NO-ERROR.
END. /* iCnt */

RELEASE OBJECT chOutlook   NO-ERROR.
RELEASE OBJECT chNamespace NO-ERROR.
RELEASE OBJECT chFolder    NO-ERROR.


