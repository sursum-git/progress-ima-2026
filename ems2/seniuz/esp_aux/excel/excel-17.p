if chExcelapplication:Version begins "8":U then 
    chWorkBook:SaveAs(t-file,39,,,,,,).
else 
    chWorkBook:SaveAs(t-file,,,,,,,).
