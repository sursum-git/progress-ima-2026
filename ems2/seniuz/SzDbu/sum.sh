#!/bin/bash
export TERM=ansi
tput clear

cd /usr/suporte/szdbu
PROPATH=.,/usr/suporte
export PROPATH
DLC=/usr/dlc101b;export DLC
export PATH=$PATH:/usr/java/default/bin:/usr/suporte/shell:/usr/dlc101b/bin:.

$DLC/bin/_progres -pf sum.pf -p sum-mon.p 

#************************************************************************

