

DEFINE TEMP-TABLE ttEstabDepos NO-UNDO
    FIELD codEstab AS CHAR
    FIELD codDepos AS CHAR.

DEFINE TEMP-TABLE  ttItens NO-UNDO SERIALIZE-NAME "produtos_com_saldo"
    FIELD itCodigo  LIKE ITEM.it-codigo SERIALIZE-NAME "codigo"
    FIELD descItem  LIKE ITEM.desc-item  SERIALIZE-NAME "descricao"
    FIELD logPE     AS LOGICAL           SERIALIZE-NAME "existe_saldo_pe"
    FIELD logPI     AS LOGICAL           SERIALIZE-NAME "existe_saldo_pi"
    INDEX primario IS PRIMARY itCodigo.
    
