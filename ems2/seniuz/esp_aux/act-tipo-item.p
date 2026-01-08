FOR EACH ITEM WHERE
         ITEM.ge-codigo = 10.
     /*
     "" NÆo Informado
     0  Mercadoria para Revenda
     1  Materia Prima
     2  Embalagem
     3  Produto em Processo
     4  Produto Acabado
     5  Subproduto
     6  Produto Intermediario
     7  Material de Uso e Consumo
     8  Ativo Imobilizado
     9  Servi‡os
     a  Outros Insumos
     b  Outros
     */

    ASSIGN SUBSTR(item.char-2,212,1) = "7".

    FOR EACH item-uni-estab WHERE
             item-uni-estab.it-codigo = item.it-codigo.
        ASSIGN SUBSTR(item-uni-estab.char-1,133,1) = SUBSTR(item.char-2,212,1).
    END.
END.



