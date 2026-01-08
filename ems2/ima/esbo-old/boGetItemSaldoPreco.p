/*
programa: esbo/boGetItemSaldoPreco.p
objetivo: Buscar o saldo em estoque dos produtos passados
por parametro considerando as vendas ainda n∆o integradas
pelo portal e a quantidade j† vendida e ainda n∆o faturada
assim como os preáos conforme o tipo e tabela
Desenv: Tadeu
data: 11/2021
*/


{esp/util.i}
DEFINE VARIABLE cItem               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescrItem          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaRef           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSoOutlet           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cListaOpcoesSaldo   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaTpPreco       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSaldoMinKg         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSaldoMinMt         AS DECIMAL     NO-UNDO.


DEFINE TEMP-TABLE ttItens no-undo
   FIELD itCodigo LIKE ITEM.it-codigo 
   INDEX ind AS PRIMARY UNIQUE itCodigo.


PROCEDURE iniciarBos:



END PROCEDURE.

PROCEDURE finalizarBos:



END PROCEDURE.


PROCEDURE filtrarPorPropsItem:

   
     IF cItem <> '' THEN DO:
         //chamar procedimento da boItem
     END.
     ELSE DO :
     IF cDescrItem <> '' THEN 
         //chamar procedimento da boItem
     END.

END PROCEDURE.


PROCEDURE  setFiltro:

    DEFINE INPUT  PARAMETER cChave  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cvalor  AS CHARACTER   NO-UNDO.

    CASE cChave:

        WHEN 'cod_refer' THEN DO:
           ASSIGN cListaRef = cValor .
        END.
        WHEN 'log_so_outlet' THEN DO:
           ASSIGN lSoOutlet = INT(cvalor) = 1 .
        END.
        WHEN 'lista_opcoes_saldo' THEN DO:
           ASSIGN cListaOpcoesSaldo = cValor.  
        END.
        WHEN 'lista_tp_preco' THEN DO:
           ASSIGN cListaTpPreco = cValor . 

        END.
        WHEN 'saldo_minimo_kg' THEN DO:
            ASSIGN dSaldoMinKg = DEC(cValor).
        END.
        WHEN 'saldo_minimo_mt' THEN DO:
            ASSIGN dSaldoMinMt = DEC(cValor).
        END.
    END CASE.

END PROCEDURE.

