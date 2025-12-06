Rem    This file is part of the 'File Update Tool' program.
Rem    Copyright (C) 2025 Ihar S. Areshchankau
Rem
Rem    This program is free software: you can redistribute it and/or modify
Rem    it under the terms of the GNU General Public License as published by
Rem    the Free Software Foundation, either version 3 of the License, or
Rem    (at your option) any later version.
Rem
Rem    This program is distributed in the hope that it will be useful,
Rem    but WITHOUT ANY WARRANTY; without even the implied warranty of
Rem    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Rem    GNU General Public License for more details.
Rem
Rem    You should have received a copy of the GNU General Public License
Rem    along with this program.  If not, see <https://www.gnu.org/licenses/>.
Rem
Rem ======================================
Rem Модуль обработки текстовой информации.
Rem Язык программирования: FreeBASIC.
Rem ======================================
Rem ==========================================================
Rem Пропуск указанных символов в строке.
Rem Вызов: p - позиция в строке, с которой начинать обработку,
Rem        ss - обрабатываемая строка,
Rem        sc - строка с пропускаемыми символами.
Rem Возврат: позиция в строке после пропущенных символов.
Rem ==========================================================
Public Function textWalkOver (ByVal p As Integer, ByRef ss As Const String, ByRef sc As Const String) As Integer
        While InStr (sc, Mid (ss, p, 1))
        	p += 1
        Wend
	textWalkOver = p
End Function
Rem ==============================================================
Rem Пропуск в строке символов до одного из указанных.
Rem Вызов: p - позициция в строке, с которой начинать обработку.
Rem        ss - обрабатываемая строка,
Rem        sc - строка с искомыми символами.
Rem Возврат: позиция, в которой найден один из указанных символов.
Rem ==============================================================
Public Function textWalkTo(ByVal p As Integer, ByRef ss As Const String, ByRef sc As Const String) As Integer
	Dim res As Integer = InStr (p, ss, Any sc)
	textWalkTo = Iif (res = 0, Len (ss) + 1, res)
End Function
Rem ===================================================================
Rem Подстановка значений в шаблон строки.
Rem Вызов: t - шаблон строки с позициями для вставки значений,
Rem        v - строка со значениями, разделёнными специальным символом.
Rem        c - специальный символ.
Rem Возврат: строка, построенная на основе шаблона, с подставленными
Rem          значениями.
Rem Примечание. Допускается не более 9 значений для подстановки.
Rem Пример: textSubstitute ("Hello, ^1! Good ^0.", "day^World", "^").
Rem ===================================================================
Public Function textSubstitute (ByRef t As Const String, ByRef v As Const String, ByVal c As String = "^") As String
	Const n As Integer = 10
	Dim res As String, pns(n) As Integer, p As Integer, i As Integer, _
        	lc As Integer = Len (c), d As Integer
	If "" <> v AndAlso "" <> c Then
	        Rem Определение позиций значений для подстановки.
		pns(i) = 1	' нулевое значение начинается с первой позиции
		i += 1: p = InStr (v, c)
	        While i <= n AndAlso p > 0
			pns(i) = p + lc
	                i += 1: p = InStr (p + 1, v, c)
	        Wend
	        pns(i) = 1 + Len (v) + lc	' позиция, следующая за последним элементом
	        Rem Перебор точек подстановки в шаблоне.
		i = 1			' позиция после предыдущей точки подстановки
	        p = InStr (t, c)	' позиция очередной точки подстановки
		While p > 0
	                d = Asc (t, p + lc) - Asc ("0")
	                If 0 <= d AndAlso d <= 9 AndAlso 0 < pns(d + 1) Then
	                	Rem Дополнение строки фрагментом шаблона между предыдущей
	                        Rem и текущей позициями подстановки.
				res += Mid (t, i, p - i)
	                        Rem Дополнение строки подстановочным значением.
	                        res += Mid (v, pns(d), pns(d + 1) - pns(d) - lc)
	                        Rem Обновление позиции после точки подстановки
	                        i = p + lc + 1
	                End If
	                p = InStr (p + lc + 1, t, c)
	        Wend
		Rem Дополнение строки остатком шаблона.
	        res += Mid (t, i)
        Else
        	res = t
        End If
        textSubstitute = res
End Function
