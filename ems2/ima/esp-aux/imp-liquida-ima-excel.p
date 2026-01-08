DEF TEMP-TABLE tt-aux
    FIELD it-codigo AS CHAR
    FIELD cod-refer AS CHAR
    FIELD preco-item AS DECIMAL.

DEF VAR c-num-id AS CHAR.

/*Abre uma planilha, altera algum valor e imprime...*/

DEF VAR chExcelApp  AS COM-HANDLE NO-UNDO.
DEF VAR chWorkbook  AS COM-HANDLE NO-UNDO.
DEF var chWorksheet AS COM-HANDLE NO-UNDO.
DEF VAR cFileName   AS CHAR.
DEF VAR i-Lin       AS INT INITIAL 2.

DEF VAR c-item      AS CHAR.
DEF VAR c-refer     AS CHAR.

CREATE "Excel.Application" chExcelApp.

ASSIGN cFileName = "C:\temp\outlet.xlsx".

/* Abre a Planilha */
chExcelApp:VISIBLE = FALSE.  /* A Planilha Ficar  Visivel */
chWorkbook         = chExcelApp:Workbooks:OPEN(cFileName).
chWorkSheet        = chExcelApp:Sheets:Item(1).


/* Salva e Imprime e Fecha a Planilha */
ASSIGN i-lin = 1.
REPEAT.
   ASSIGN c-item = ENTRY(1,chWorksheet:range("A" + STRING(i-lin)):VALUE)
          c-refer = ENTRY(1,chWorksheet:range("B" + STRING(i-lin)):VALUE).

   IF c-item = '' OR 
      c-item = ? THEN LEAVE.

   FIND item WHERE
        item.it-codigo = c-item NO-LOCK NO-ERROR.
   IF AVAIL item THEN DO.
       CREATE tt-aux.
       ASSIGN tt-aux.it-codigo  = c-item
              tt-aux.cod-refer  = c-refer
              tt-aux.preco-item = chWorksheet:range("C" + STRING(i-lin)):VALUE.
   END.
   ASSIGN i-lin = i-lin + 1.
END.

/*chWorkBook:SaveAs(cFileName,,,,,,,).*/
chWorkBook:CLOSE().
chExcelApp:QUIT().

RELEASE OBJECT chExcelApp. 
RELEASE OBJECT chworkBook.
RELEASE OBJECT chworksheet.

FOR EACH tt-aux.
    IF tt-aux.it-codigo = '' OR 
       tt-aux.it-codigo = ? THEN NEXT.

    DISP tt-aux.it-codigo
         tt-aux.cod-refer
         tt-aux.preco-item.
END.


/*
FOR EACH tt-aux.
    IF tt-aux.it-codigo = '' THEN NEXT.

    DISP tt-aux.it-codigo
         tt-aux.cod-refer
         tt-aux.preco-item.


    IF tt-aux.cod-refer <> '' THEN DO.
       FIND liquida-ima WHERE
            liquida-ima.cod-estabel = '5' AND
            liquida-ima.it-codigo   = tt-aux.it-codigo  AND
            liquida-ima.cod-refer   = tt-aux.cod-refer  AND
            liquida-ima.dt-final    = ? 
            SHARE-LOCK NO-ERROR.

       IF AVAIL liquida-ima THEN
          ASSIGN liquida-ima.dt-final  = TODAY.


       // Cria Novo Registro com o novo percentual
       RUN esapi/calcula-id.p (OUTPUT c-num-id).
        
       CREATE liquida-ima.
       ASSIGN liquida-ima.cod-estabel   = '5'
              liquida-ima.it-codigo     = tt-aux.it-codigo
              liquida-ima.cod-refer     = tt-aux.cod-refer
              liquida-ima.preco-item    = tt-aux.preco-item
              liquida-ima.usuario       = 'super'
              liquida-ima.dt-inicio     = TODAY
              liquida-ima.num-id-liquida-ima = c-num-id.

    END.
    ELSE DO.
        FOR EACH saldo-estoq WHERE
                 saldo-estoq.cod-estabel = '5' AND
                 saldo-estoq.it-codigo = tt-aux.it-codigo AND 
                 saldo-estoq.qtidade-atu > 0 NO-LOCK.
    
            FIND liquida-ima WHERE
                 liquida-ima.cod-estabel = saldo-estoq.cod-estabel AND
                 liquida-ima.it-codigo   = saldo-estoq.it-codigo  AND
                 liquida-ima.cod-refer   = saldo-estoq.cod-refer  AND
                 liquida-ima.dt-final    = ? 
                 SHARE-LOCK NO-ERROR.
    
            IF AVAIL liquida-ima THEN
               ASSIGN liquida-ima.dt-final = TODAY.
    
            // Cria Novo Registro com o novo percentual
            RUN esapi/calcula-id.p (OUTPUT c-num-id).
            
            CREATE liquida-ima.
            ASSIGN liquida-ima.cod-estabel   = saldo-estoq.cod-estabel
                   liquida-ima.it-codigo     = saldo-estoq.it-codigo
                   liquida-ima.cod-refer     = saldo-estoq.cod-refer
                   liquida-ima.preco-item    = tt-aux.preco-item
                   liquida-ima.usuario       = 'super'
                   liquida-ima.dt-inicio     = TODAY
                   liquida-ima.num-id-liquida-ima = c-num-id.
        END.
    END.
END.

*/
