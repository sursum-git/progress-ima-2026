excelAPP:Range("A1"):SELECT.excelAPP:ActiveSheet:Pictures:Insert("C:\temp\imagem.bmp").excelAPP:ActiveSheet:Pictures("Picture 1"):Select. /* Seleciona a figura */

excelAPP:Selection:ShapeRange:LEFT = 80.5. /* Posiciona na horizontal em 80.5 */

excelAPP:Selection:ShapeRange:TOP = 200.5. /* Posiciona na vertical em 200.5 */

excelAPP:Selection:ShapeRange:Width = 115.75. /* Define 115.75 (pixel) de altura, 1 pixel = 0,04 cm */

excelAPP:Selection:ShapeRange:Height = 33. /* Define 33 (pixel) de largura */

excelAPP:SELECTION:ShapeRange:LockAspectRatio = False. /* Desativa opcao proporcional */

excelAPP:VISIBLE = TRUE. /* Torna a planilha visivel */

excelAPP:ActiveWindow:CLOSE(NO). /* Fecha a janela */
RELEASE OBJECT excelAPP.
