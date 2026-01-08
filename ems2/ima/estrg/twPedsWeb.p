TRIGGER PROCEDURE FOR WRITE OF peds_web
 new buffer bfNovo
 old buffer bfOld.

if bfold.ind_sit_ped_web <> 2 and bfNovo.ind_sit_ped_web = 2 then do:

output to value('c:\temp\peds_web_' + string(bfNovo.ped_web_id) +  '_' + string(time) + '.txt' ).

disp program-name(1)FORMAT "x(30)" skip
     program-name(2)FORMAT "x(30)" skip
     program-name(3)FORMAT "x(30)" skip
     program-name(4)FORMAT "x(30)" skip
     program-name(5)FORMAT "x(30)"  skip
     USERID('espec').


     
output close.

end.
