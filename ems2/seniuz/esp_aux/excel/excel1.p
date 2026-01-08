/* Programa: excel1.p
*/

DEFINE VAR i-canal AS INTEGER.
DEFINE VAR sys     AS INTEGER.

DEF VAR c-comando AS CHAR.

DEFINE FRAME frm_excel WITH SIZE 85.25 BY 15.42.
ENABLE ALL WITH FRAME frm_excel.

RUN pi-imp-planilha.
/* RUN pi-cria-planilha. */
IF RETURN-VALUE <> '' THEN
   RETURN ERROR.

/*RUN pi-roda-macro.*/

/*RUN pi-sair. */

PROCEDURE pi-abre-excel.
    DEF INPUT PARAMETER p-arquivo AS CHAR.

    def var h-prog as handle no-undo.
    run utp/ut-utils.p persistent set h-prog.
    
    run Execute in h-prog(input "EXCEL.EXE", input p-arquivo).

    delete procedure h-prog.
    PAUSE 5 NO-MESSAGE.
END PROCEDURE.

PROCEDURE pi-roda-macro.
    RUN pi-abre-excel (INPUT "c:\temp\teste.xls").
    
    DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
    IF sys = 0 THEN DO:
        MESSAGE "Excel n∆o Encontrado" VIEW-AS ALERT-BOX.
        RETURN.
    END.
    
    DDE EXECUTE sys COMMAND '[run("Macro1")]'. 
END PROCEDURE.

PROCEDURE pi-imp-planilha.
    RUN pi-abre-excel (INPUT "c:\temp\lixo1.xls").
    DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
    DDE EXECUTE  sys COMMAND "[app.minimize()]".   
    DDE EXECUTE  sys COMMAND "[print()]".
END PROCEDURE.

PROCEDURE pi-cria-planilha.
    RUN pi-abre-excel (INPUT "").
    PAUSE 3 NO-MESSAGE.
    DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
    IF sys = 0 THEN DO.
       MESSAGE 'Excel n∆o Encontrado...'
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.

       RETURN ERROR.
    END.

    DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".
    DDE SEND i-canal SOURCE "Nomes"   ITEM "L1C1". 
    DDE SEND i-canal SOURCE "Valores" ITEM "L1C2".
    DDE SEND i-canal SOURCE "Maria"   ITEM "L2C1".
    DDE SEND i-canal SOURCE string(100.99) ITEM "L2C2".
    DDE SEND i-canal SOURCE "Jose"    ITEM "L3C1".
    DDE SEND i-canal SOURCE string(338.95) ITEM "L3C2".
    DDE SEND i-canal SOURCE "Titulo"  ITEM "L2C3".

    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    /* Fonte, Tamanho, Negrito, It†lico, Sublinhado, Atachado, Cor(0=Autom†tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Arial Narrow~",24,False,False,False,False,3)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    /* Horizontal(1=General 2=Left 3=Center 4=Right) Wrap(retorno autom†tico), Vertical(1=Top 2=Center 3=Bottom)
       Orientation(0=Horizontal 1=Vertical) */
    DDE EXECUTE sys COMMAND "[alignment(2,true,2,0)]".
    
    DDE EXECUTE sys     COMMAND "[column.width(20)]". 
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    
    /* Horizontal(7=Center acroos selection), Wrap, Vertical(1=Top, 2=Center, 3=Bottom), Orientation(0=Horizontal, 1=Vertical) */
    DDE EXECUTE i-canal COMMAND '[select("L2C3:L2C4")]'.
    DDE EXECUTE sys     COMMAND "[alignment(7,False,3,0)]".
                                

    ASSIGN c-comando = '[save.as("c:\temp\lixo1.xls")]'.
    DDE EXECUTE sys COMMAND c-comando.
END.

PROCEDURE pi-sair.
    DDE EXECUTE   sys COMMAND "[close(0)]". 
    DDE EXECUTE   sys COMMAND "[quit()]". 
    DDE TERMINATE sys. 
END PROCEDURE.
