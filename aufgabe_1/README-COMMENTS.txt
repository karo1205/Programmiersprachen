
Unterscheidung zwischen Zahl und Operator + oder - gefolgt von einer Zahl:
==========================================================================
In der Angabe ist nicht klar ersichtlich, wie zwischen der negativen Zahl -1 und der Eoingabe - gefolgt von einem 1 unterschieden werden sollte.
Beispiel: die Eingabe "5 3 -1" kann sowohl als das pushen der drei Zahlen 5, 4 und -1 gesehen werden, als auch als die Anwendung der operators - auf die ersten beiden Zahlen und das weitere pushen von 1.
Im zweiten Beispiel in der Angabe kommt genau so ein Fall vor: kein Leerzeichen zw. - und der nachfolgenden Zahl.

Mein Parser sieht den String "-1" als Zahl, "- 1" als operator gefolgt von 1. Daher ist es nötig, die Angabe entsprechend umzuschreiben, das heißt, ein Leerzeichen zwischen Operand und Zahl zu setzen.
Gleiches gilt übrigens auch für das "+"-Zeichen. +4 ist also die positive Zahl 4, "+ 4" der '+'-operand gefolgt von der Zahl 4. 

Operator $ (Output-Operator)
============================
Die Angabe sagt, einer der nächsten zwei Operanden, die von Stack geholt werden, ist ein Index im Display, der andere der auszugebende Character. Welcher ist nicht klar und nicht auf Grund von Syntax unterscheidbar, ich habe also die Annahme getätigt, dass der erste operand, der gepopped wird (also der topmost im Stack), den index am display angibt, der zweite das auszugebende Zeichen.
