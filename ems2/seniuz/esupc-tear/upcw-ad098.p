/* Programa..: upcw-ad098.p
** Objetivo 1: Atualizar as tabelas ped-item-res, ped-item-rom, ped-item-ext e hist-cred-ped, 
**             quando o nome abreviado do emitente for alterado na tabela emitente. 
** Objetivo 2: Atualizar a tabela ped-venda, quando o portador for alterado na tabela emitente,
**             somente para pedidos Abe, Atp e Sus. 
** Autor.....: Toninho/Gilvando Souza Araujo
** Data......: 19/Jan/2006
*/

DEFINE PARAMETER BUFFER p-table FOR emitente.
DEFINE PARAMETER BUFFER p-table-old FOR emitente.

IF p-table-old.nome-abrev <> "" AND 
   p-table.nome-abrev <> p-table-old.nome-abrev THEN DO:
   FOR EACH ped-item-res WHERE
            ped-item-res.nome-abrev = p-table-old.nome-abrev EXCLUSIVE-LOCK.
       ASSIGN ped-item-res.nome-abrev = p-table.nome-abrev.
   END.
   
   FOR EACH ped-item-rom WHERE
            ped-item-rom.nome-abrev = p-table-old.nome-abrev EXCLUSIVE-LOCK.
       ASSIGN ped-item-rom.nome-abrev = p-table.nome-abrev.
   END.

   FOR EACH ped-item-ext WHERE
            ped-item-ext.nome-abrev = p-table-old.nome-abrev EXCLUSIVE-LOCK.
       ASSIGN ped-item-ext.nome-abrev = p-table.nome-abrev.
   END.

   FOR EACH espec.hist-cred-ped WHERE
            espec.hist-cred-ped.nome-abrev = p-table-old.nome-abrev EXCLUSIVE-LOCK.
       ASSIGN espec.hist-cred-ped.nome-abrev = p-table.nome-abrev.
   END.
END.

IF p-table-old.nome-abrev <> "" AND 
   p-table.portador <> p-table-old.portador THEN DO:
   FOR EACH ped-venda WHERE ped-venda.nome-abrev = p-table.nome-abrev 
                        AND (ped-venda.cod-sit-ped < 3 OR
                             ped-venda.cod-sit-ped = 5)
                      EXCLUSIVE-LOCK.
       ASSIGN ped-venda.cod-portador = p-table.portador.
   END.
END.
