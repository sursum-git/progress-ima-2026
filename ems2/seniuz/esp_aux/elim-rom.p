/*"96664, 96665, 95544, 95545, 96928, 96929,
   96664, 96525, 96526, 95818, 96497, 96498, 95444, 95545" */

FIND nota-fiscal WHERE
     nota-fiscal.cod-estabel = '2' AND
     nota-fiscal.serie = '1' AND
     nota-fiscal.nr-nota-fis = '0095545'.

FOR EACH ped-item-res WHERE
         ped-item-res.nome-abrev = nota-fiscal.nome-ab-cli AND
         ped-item-res.nr-pedcli = nota-fiscal.nr-pedcli AND
         ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis).

    FOR EACH ped-item-rom WHERE
             ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
             ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
             ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia.
        DELETE ped-item-rom.
    END.
END.
