DEFINE VARIABLE iOrigem AS INTEGER     NO-UNDO.
OUTPUT TO c:\temp\itens.txt.

PUT "Codigo | Descri‡Æo | UM | Origem | Peso B. | Peso L. " SKIP.
    
FOR EACH ITEM WHERE
         ITEM.deposito = "arm".
    ASSIGN iOrigem =  ITEM.codigo-orig + 1.
    EXPORT DELIMITER "|" ITEM.it-codigo
                         ITEM.desc-item
                         ITEM.un
                         {ininc/i18in122.i 4 iOrigem  }
                         ITEM.peso-bruto
                         ITEM.peso-liquido.
         
END.

OS-COMMAND SILENT VALUE ("start excel d:\itens.xlsx").

