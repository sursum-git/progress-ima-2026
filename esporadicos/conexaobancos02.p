DEFINE VARIABLE cListaBancos AS CHARACTER NO-UNDO.
DEFINE VARIABLE i            AS INTEGER   NO-UNDO.

ASSIGN cListaBancos = " -db eai       -ld eai2     -S 10001 -H 192.168.0.44 -N tcp" +
                      ",-db ems2adt   -ld mgadt    -S 10002 -H 192.168.0.44 -N tcp" + 
                      ",-db ems2mp    -ld mgmp2    -S 10005 -H 192.168.0.44 -N tcp" +
                      ",-db ems2sor   -ld mgsor    -S 10006 -H 192.168.0.44 -N tcp" +
                      ",-db emsdev    -ld emsdev   -S 10009 -H 192.168.0.44 -N tcp" +
                      ",-db emsfnd    -ld emsfnd   -S 10010 -H 192.168.0.44 -N tcp" + 
                      ",-db emsinc    -ld emsinc   -S 10011 -H 192.168.0.44 -N tcp" +
                      ",-db espec     -ld espec    -S 10012 -H 192.168.0.44 -N tcp" + 
                      ",-db finance   -ld finance  -S 10013 -H 192.168.0.44 -N tcp" +
                      ",-db hcm       -ld hcm      -S 10014 -H 192.168.0.44 -N tcp" +
                      ",-db mdtcrm    -ld mdtcrm   -S 10015 -H 192.168.0.44 -N tcp" +
                      ",-db mdtfrw    -ld mdtfrw   -S 10016 -H 192.168.0.44 -N tcp" + 
                      ",-db neogrid   -ld neogrid  -S 10017 -H 192.168.0.44 -N tcp" +
                      ",-db payroll   -ld payroll  -S 10018 -H 192.168.0.44 -N tcp" +
                      ",-db srcadger  -ld srcadger -S 10019 -H 192.168.0.44 -N tcp" +
                      ",-db srmovfin  -ld srmovfin -S 10020 -H 192.168.0.44 -N tcp" + 
                      ",-db mdmerge   -ld mdmerge  -S 10021 -H 192.168.0.44 -N tcp" +
                      ",-db ems2ima   -ld mgcad    -S 10030 -H 192.168.0.44 -N tcp" +
                      ",-db ems2med   -ld mgmov    -S 10031 -H 192.168.0.44 -N tcp" +
                      ",-db ems5      -ld ems5     -S 10032 -H 192.168.0.44 -N tcp" .

REPEAT i = 1 TO NUM-ENTRIES(cListaBancos):
   MESSAGE entry(i,cListaBancos) 'antes conexao' VIEW-AS ALERT-BOX INFO BUTTONS OK.
   CONNECT value(entry(i,cListaBancos)).
   MESSAGE entry(i,cListaBancos) 'depois conexao' VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.




