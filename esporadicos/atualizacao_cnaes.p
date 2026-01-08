DEFINE TEMP-TABLE tt
        FIELD cod_cnae              AS CHAR FORMAT 'x(20)'
        FIELD cod_tipo_atividade    AS INT
        FIELD cod_finalidade        AS INT.


INPUT FROM c:\temp\cons_cnaes_completo.csv.

REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.


INPUT CLOSE.
OUTPUT TO c:\temp\tt.txt.
FOR EACH tt:
    FIND FIRST cnaes
        WHERE  cnaes.cod_cnae = tt.cod_cnae EXCLUSIVE-LOCK NO-ERROR.
    DISP tt.cod_cnae
         tt.cod_tipo_atividade
         tt.cod_finalidade.
    IF AVAIL cnaes THEN DO:
       DISP substr(cnaes.descricao,1,100) FORMAT 'x(100)'
            cnaes.ind_tipo_atividade
            cnaes.cod_finalidade_venda WITH  WIDTH 550.
       ASSIGN cnaes.ind_tipo_atividade =   string(tt.cod_tipo_atividade)
              cnaes.cod_finalidade_venda = string(tt.cod_finalidade).

    END.
    ELSE DO:
        DISP "NÆo encontrado".
    END.
END.

/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_cnae                         char        i
   20 descricao                        char
   30 ind_tipo_atividade               char        i
   40 cod_finalidade_venda             char        i
   60 cod_classe                       char        i

*/
