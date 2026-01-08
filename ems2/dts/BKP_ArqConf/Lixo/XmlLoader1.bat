@echo on

::set JAVA_HOME="c:\Program Files (x86)\Java\jre7"
set JAVA_HOME="C:\Arquivos de programas\Java\jre7"
set PATH=%PATH%;%JAVA_HOME%\bin

echo processando arquivos xml da imatextil, aguarde...
java -jar M:\EMS206\especificos\xmlloader\ArqConf\Loader_IMA.jar M:\EMS206\especificos\xmlloader\ArqConf\arqCertif_ima.xml
echo.

echo processando arquivos xml da medtextil, aguarde...
java -jar M:\EMS206\especificos\xmlloader\ArqConf\Loader_IMA.jar M:\EMS206\especificos\xmlloader\ArqConf\arqCertif_med.xml
exit