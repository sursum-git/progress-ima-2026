DISABLE TRIGGERS FOR DUMP OF dbaux.item.
DISABLE TRIGGERS FOR LOAD OF dbaux.item.

/* Parameters Definitions */
DEFINE PARAMETER BUFFER b-item FOR mgind.item.

/* Defini‡Æo de Variaveis */
FIND dbaux.item WHERE
     dbaux.item.it-codigo = b-item.it-codigo SHARE-LOCK NO-ERROR.
IF AVAIL dbaux.item THEN
   ASSIGN dbaux.item.fm-cod-com = b-item.fm-cod-com.



