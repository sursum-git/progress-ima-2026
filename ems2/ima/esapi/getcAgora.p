DEFINE OUTPUT PARAMETER cAgora AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
ASSIGN cAgora = string(YEAR(NOW)) + '_' + string(MONTH(NOW),'99') + '_' + 
     string(DAY(NOW),'99') + '_' + string(TIME,'hh:mm:ss')
       cAgora = REPLACE(cAgora,':','_').




       

