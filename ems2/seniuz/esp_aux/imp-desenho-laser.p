{esinc/sz-pcl.i}

fn-grava-macro("n:\especificos\Etiqueta\image\logo-etq10.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp11.prn").
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp12.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp13.prn").
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp14.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp15.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp16.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp17.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp18.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp19.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp20.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp21.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp22.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp23.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp24.prn"). 
fn-grava-macro("n:\especificos\Etiqueta\image\rlgp25.prn"). 

OUTPUT TO PRINTER. 

PUT UNFORMATTED 
    "~033&l3A" /* Pagina Oficio */
    "~033&l164F"
    "~033&l128P"
    "~033&l1L"
    "~033&l1M"
    "~033&l1E".


PUT UNFORMATTED
    fn-imp-macro(INPUT 10, INPUT   10, INPUT 11) 
    fn-imp-macro(INPUT 10, INPUT  110, INPUT 12)
    fn-imp-macro(INPUT 10, INPUT  210, INPUT 13)
    fn-imp-macro(INPUT 10, INPUT  310, INPUT 14) 
    fn-imp-macro(INPUT 10, INPUT  410, INPUT 15)
    fn-imp-macro(INPUT 10, INPUT  510, INPUT 16) 
    fn-imp-macro(INPUT 10, INPUT  610, INPUT 17)
    fn-imp-macro(INPUT 10, INPUT  710, INPUT 18)
    fn-imp-macro(INPUT 10, INPUT  810, INPUT 19)
    fn-imp-macro(INPUT 10, INPUT  910, INPUT 20)
    fn-imp-macro(INPUT 10, INPUT 1010, INPUT 21)
    fn-imp-macro(INPUT 10, INPUT 1110, INPUT 22)
    fn-imp-macro(INPUT 10, INPUT 1210, INPUT 23)
    fn-imp-macro(INPUT 10, INPUT 1310, INPUT 24)
    fn-imp-macro(INPUT 10, INPUT 1410, INPUT 25).

OUTPUT CLOSE.
