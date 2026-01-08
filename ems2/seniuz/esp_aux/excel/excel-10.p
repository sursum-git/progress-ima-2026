DEF VAR c-arquivo AS CHAR.
DEF VAR c-lin     AS CHAR.
DEF VAR sys       AS INT.
DEF VAR i-canal   AS INT.
DEF FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1. 
ENABLE ALL WITH FRAME frm_excel.
ASSIGN c-arquivo = 'c:\temp\lixo.xls'.
    

RUN abre-excel (INPUT c-arquivo).
PAUSE 3 NO-MESSAGE.


DDE INITIATE sys     FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System" NO-ERROR.
DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1" NO-ERROR.

MESSAGE "dde initiate"  SKIP
         sys SKIP
         i-canal 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



/* Cabe‡alho  da Planilha */
ASSIGN c-Lin = " ANALISE DE DEFEITOS NA PRODU€ÇO NO PERIODO: ". 
DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
DDE EXECUTE i-canal COMMAND '[select("L1C1:L1C11")]'.
/* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
DDE EXECUTE sys COMMAND "[format.font(~"Verdana~",10,True,False,False,False,3)]".

/* Cabe‡alho dos Dados */
DDE SEND i-canal SOURCE "COD"             ITEM "L3C1".
DDE SEND i-canal SOURCE "DEFEITOS"        ITEM "L3C2".
DDE SEND i-canal SOURCE "REGULAR"         ITEM "L3C3".
DDE SEND i-canal SOURCE "  %  "           ITEM "L3C4".
DDE SEND i-canal SOURCE "LEVE DEFEITO"    ITEM "L3C5".
DDE SEND i-canal SOURCE "  %  "           ITEM "L3C6".
DDE SEND i-canal SOURCE "RETALHO"         ITEM "L3C7".
DDE SEND i-canal SOURCE "  %  "           ITEM "L3C8".
DDE SEND i-canal SOURCE "TOTAL DEFEITOS"  ITEM "L3C9".
DDE SEND i-canal SOURCE "  %  "           ITEM "L3C10".
DDE SEND i-canal SOURCE "%s/PRD"          ITEM "L3C11".




PROCEDURE abre-excel.

  DEF INPUT PARAMETER p-arquivo AS CHAR.

  def var h-prog as handle no-undo.
  run utp/ut-utils.p persistent set h-prog.

  run Execute in h-prog(input "EXCEL.EXE", input p-arquivo).

  delete procedure h-prog.
  PAUSE 5 NO-MESSAGE.


END.
