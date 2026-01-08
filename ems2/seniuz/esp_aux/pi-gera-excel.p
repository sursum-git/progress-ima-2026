
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER p-arq-saida AS CHAR FORMAT "x(45)".
    
    DEF VAR c-empresa LIKE empresa.razao-social.
    
    DEFINE VAR i-canal     AS INTEGER.
    DEFINE VAR sys         AS INTEGER.
    DEFINE VAR i-Lin       AS INTEGER.
    DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
    DEFINE VAR aux-command AS CHAR FORMAT "x(100)".
    
    /*DEFINE FRAME frm_excel WITH SIZE 85.25 BY 15.42.*/
    DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
    ENABLE ALL WITH FRAME frm_excel.
    
    /* bloco principal do programa */
    find first param-global no-lock no-error.
    find first empresa
         where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 
    
    assign c-empresa = (if avail empresa then empresa.razao-social else "").
    
    RUN pi-abre-excel (INPUT "").
    PAUSE 3 NO-MESSAGE.

    DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
    DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".
    
    ASSIGN c-Lin = c-empresa + " - " + " ITENS COM ESTOQUES DISPONIVEIS PARA VENDA - DATA: " +
           STRING(TODAY).
    DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
    
    DDE SEND i-canal SOURCE "ITEM"            ITEM "L3C1".
    DDE SEND i-canal SOURCE "DESCRI€ÇO"       ITEM "L3C2".
    DDE SEND i-canal SOURCE "UN"              ITEM "L3C3".
    DDE SEND i-canal SOURCE "REFERÒNCIA"      ITEM "L3C4".
    DDE SEND i-canal SOURCE "LOTE"            ITEM "L3C5".
    DDE SEND i-canal SOURCE "COR/DESENHO"     ITEM "L3C6".
    DDE SEND i-canal SOURCE "CORTE COMERCIAL" ITEM "L3C7".
    DDE SEND i-canal SOURCE "QUANTIDADE"      ITEM "L3C8".
    DDE SEND i-canal SOURCE "ACUMULADO"       ITEM "L3C9".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(6.29)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(35.71)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(2.71)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(10.71)]". 
    DDE EXECUTE sys     COMMAND "[alignment(3,true,2,0)]".
    
    DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(4.43)]". 
    DDE EXECUTE sys     COMMAND "[alignment(3,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(18.71)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(16.29)]". 
    
    DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".

    DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
    DDE EXECUTE sys     COMMAND "[column.width(13.86)]".
    DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".
    DDE EXECUTE sys     COMMAND "[alignment(4,true,2,0)]".
   
    ASSIGN i-Lin = 4.
    
    FOR EACH tt-itens NO-LOCK,
        EACH tt-movto WHERE tt-movto.row-tt-itens = ROWID(tt-itens) NO-LOCK
        BREAK BY tt-itens.it-codigo
              BY tt-itens.cod-refer
              BY tt-itens.corte-comerc:
        
        IF FIRST-OF(tt-itens.it-codigo) THEN DO:
           FIND ITEM WHERE ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
           
           DDE SEND i-canal SOURCE tt-itens.it-codigo ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
           DDE SEND i-canal SOURCE item.desc-item     ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
           DDE SEND i-canal SOURCE item.un            ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
        END.
        
        IF FIRST-OF(tt-itens.cod-refer) THEN DO:
           FIND ref-item-ext WHERE ref-item-ext.it-codigo = tt-itens.it-codigo 
                               AND ref-item-ext.cod-refer = tt-itens.cod-refer
                             NO-LOCK NO-ERROR.
            
           DDE SEND i-canal SOURCE tt-itens.cod-refer ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
           DDE SEND i-canal SOURCE tt-itens.lote      ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
           DDE SEND i-canal SOURCE ref-item-ext.cor   ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
        END.

        FIND corte-comerc WHERE corte-comerc.codigo = tt-movto.corte-comerc NO-LOCK NO-ERROR.
        ASSIGN c-lin = tt-movto.corte-comerc + "-" + IF AVAIL corte-comerc THEN corte-comerc.descricao
                                                                           ELSE "".
        DDE SEND i-canal SOURCE c-lin ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
        DDE SEND i-canal SOURCE STRING(tt-movto.qt-saldo) ITEM "L" + TRIM(STRING(i-Lin)) + "C8".

        ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.it-codigo).
        ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.cod-refer).
        ACCUMULATE tt-movto.qt-saldo (TOTAL).

        IF LAST-OF(tt-itens.cod-refer) THEN
           DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-itens.cod-refer tt-movto.qt-saldo)) ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
           
        ASSIGN i-Lin = i-Lin + 1.

        IF LAST-OF(tt-itens.it-codigo) THEN DO:
           ASSIGN i-Lin = i-Lin + 1.
           DDE SEND i-canal SOURCE "Total do Item" ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
           DDE SEND i-canal SOURCE STRING((ACCUM TOTAL BY tt-itens.it-codigo tt-movto.qt-saldo)) ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
           ASSIGN i-Lin = i-Lin + 2.
        END.
    END.
    
    IF (ACCUM TOTAL tt-movto.qt-saldo) <> 0 THEN DO:
       ASSIGN i-Lin = i-Lin + 1.
       DDE SEND i-canal SOURCE "Total Geral" ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
       DDE SEND i-canal SOURCE STRING((ACCUM TOTAL tt-movto.qt-saldo)) ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
       ASSIGN i-Lin = i-Lin + 2.
    END.

    DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C9")]'.
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,False,False,False,False,0)]".

    ASSIGN aux-command = '[select("L2C1:L' + TRIM(STRING(i-Lin)) + 'C9")]'.
    DDE EXECUTE i-canal COMMAND aux-command.
    /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
    DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",8,False,False,False,False,0)]".
    
    OS-DELETE VALUE(p-arq-saida).
    DDE EXECUTE   sys COMMAND '[save.as("' + p-arq-saida + '")]'.
    DDE EXECUTE   sys COMMAND "[close(0)]". 
    DDE EXECUTE   sys COMMAND "[quit()]". 
    DDE TERMINATE sys.
    
    HIDE FRAME frm_excel.
    CLEAR FRAME frm_excel.
    DISABLE ALL WITH FRAME frm_excel.

END PROCEDURE.


