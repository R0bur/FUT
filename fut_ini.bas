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
Rem =====================================
Rem Модуль обработки файла инициализации.
Rem Язык программирования: FreeBASIC.
Rem =====================================
#include once "fut_ini.bai"
#include once "fut_text.bai"
Dim Shared Section As String, Parameter As String, Value As String, _
	QuotationMarks As String, CommentMarks As String, _
        ErrorPosition As Integer, ErrorCode As iniErrorCodes
Rem =======================================
Rem Наглядное отображение позиции в строке.
Rem Вызов: p - позиция в строке,
Rem        s - строка,
Rem        m - текст сообщения.
Rem =======================================
Sub Debug (ByVal p As Integer, ByRef s As Const String, m As Const String)
Rem	Dim r As Integer = 0
Rem	Print m
Rem	Print s
Rem        For i As Integer = 1 To p - 1
Rem		r += Iif (9 = Asc (s, i), 8 - r Mod 8, 1)
Rem        Next i
Rem        Print Spc (r); "^--("; p; ")"
End Sub
Rem =======================================================
Rem Подготовка к обработке файла инициализации.
Rem Вызов: qm - строка с допустимыми символами кавычек.
Rem        cm - строка с допустимыми символами комментария.
Rem Пример: iniParseBegin """'", ";#"
Rem =======================================================
Public Sub iniParseBegin (ByRef qm As Const String, ByRef cm As Const String)
        ErrorCode = iniEcNoError
	ErrorPosition = 0
        QuotationMarks = qm
        CommentMarks = cm
	Section = ""
        Parameter = ""
        Value = ""
End Sub
Rem ====================================================
Rem Обработка очередной строки файла инициализации.
Rem Вызов: s - строка файла инициализации
Rem Возврат: -2 - строка обработана и получен параметр.
Rem          -1 - строка обработана, параметра нет,
Rem          0 - при обработке строки обнаружена ошибка.
Rem ====================================================
Public Function iniParseLine (ByRef s As Const String) As Integer
	#define AllRight (iniEcNoError = ErrorCode)
	Enum LineCategories
		lineEmpty, lineComment, lineSection, lineParameter
        End Enum
	Dim res As Integer = -1, p As Integer, p1 As Integer, p2 As Integer, _
	        p3 As Integer, p4 As Integer, lineCategory As LineCategories, _
                c As String, QuotationMark As String*1
	Rem Сброс признака ошибки.
        ErrorCode = iniEcNoError
        ErrorPosition = 0
        Rem Пропуск начальных пробелов.
	p = textWalkOver (1, s, textWhitespaces)
        Rem Определение типа обрабатываемой строки.
	c = Mid (s, p, 1)
        lineCategory = Iif ("" = c, lineEmpty, _
        	Iif (InStr (CommentMarks, c), lineComment, _
                Iif ("[" = c, lineSection, lineParameter)))
        Rem ------------------------------------
        Rem Обработка строки, содержащей секцию.
        Rem ------------------------------------
        If lineCategory = lineSection Then
		Debug (p, s, "Line with section begin:")
                p += 1
                Rem Пропуск пробелов после открывающей квадратной скобки.
                p = textWalkOver (p, s, textWhitespaces)
                Debug (p, s, "Section name start position:")
                Rem Пропуск символов, допустимых в названии секции.
                p1 = p	' начало названия секции
                p = textWalkOver (p, s, textAbcUC + textAbcLC + textDigits + "_")
                Debug (p, s, "After section name:")
                Rem Ожидается пробельный символ или закрывающая квадратная скобка.
                c = Mid (s, p, 1)
                If InStr (textWhitespaces, c) OrElse "]" = c Then
			p2 = p	' позиция после названия секции
                Else
                	Rem В названии секции обнаружен недопустимый символ.
			ErrorPosition = p
			ErrorCode = iniEcInvalidCharacter
                End If
                If AllRight Then
			Rem Пропуск пробелов после названия секции.
	                p = textWalkOver (p, s, textWhitespaces)
                        Debug (p, s, "Square bracket expected:")
        	        Rem Ожидается закрывающая квадратная скобка.
                        If "]" = Mid (s, p, 1) Then
                        	p += 1
                        Else
                        	Rem Отсутствует закрывающая квадратная скобка.
                        	ErrorPosition = p
                                ErrorCode = iniEcNoClosingBracket
                        End If
                End If
                If AllRight Then
                	Rem После закрывающей квадратной скобки могут быть пробелы и комментарий.
                        p = textWalkOver (p, s, textWhitespaces)
                        Debug (p, s, "Comment or end of line expected:")
                        c = Mid (s, p, 1)
                        If 0 = InStr (CommentMarks, c) AndAlso "" <> c Then
                        	Rem Недопустимый символ после закрывающей скобки.
                        	ErrorPosition = p
                                ErrorCode = iniEcInvalidCharacter
                        End If
                End If
                If AllRight Then
			Section = Mid (s, p1, p2 - p1)
                Else
			res = 0	' при обработке строки обнаружена ошибка.
                End If
        End If
	Rem --------------------------------------
	Rem Обработка строки, содержащей параметр.
	Rem --------------------------------------
        If lineCategory = lineParameter Then
		Debug (p, s, "Line with parameter begin:")
		p1 = p	' начало имени параметра
                Rem Пропуск символов, допустимых в имени параметра.
		p = textWalkOver (p, s, textAbcUC + textAbcLC + textDigits + "_")
                p2 = p	' позиция после имени параметра
                Debug (p, s, "After parameter name:")
                Rem Пропуск пробельных символов после имени параметра.
                p = textWalkOver (p, s, textWhitespaces)
                Debug (p, s, "Expected = sign.")
                Rem Ожидается знак равенства.
                If "=" <> Mid (s, p, 1) Then
			ErrorPosition = p
                	ErrorCode = iniEcEqualSignExpected
                End If
                If AllRight Then
	                Rem Пропуск пробельных символов после знака равенства.
                	p = textWalkOver (p + 1, s, textWhitespaces)
                        c = Mid (s, p, 1)
	                Rem Ожидается значение параметра или комментарий.
			If InStr (QuotationMarks, c) Then
                        	Rem Значение параметра заключено в двойные кавычки.
                                QuotationMark = c
                                p3 = p + 1	' начало значения параметра.
	                        Debug (p, s, "Opening quotation mark found.")
                                Rem Поиск закрывающей кавычки.
				p = textWalkTo (p3, s, QuotationMark)
                                Debug (p, s, "Closing quiotation mark.")
                               	Rem Ожидается закрывающая двойная кавычка.
                                If QuotationMark = Mid (s, p, 1) Then
					p4 = p	' позиция после значения параметра
                                Else
					ErrorPosition = p
                                        ErrorCode = iniEcNoClosingQuotationMark
                                End If
				If AllRight Then
                                	Rem Пропуск пробелов после закрывающей кавычки.
                                        p = textWalkOver (p + 1, s, textWhitespaces)
                                        Debug (p, s, "Comment or end of line:")
					Rem Ожидается комментарий или конец строки.
                                        c = Mid (s, p, 1)
                                        If 0 = InStr (CommentMarks, c) AndAlso "" <> c Then
                                        	ErrorPosition = p
                                                ErrorCode = iniEcInvalidCharacter
                                        End If
                                End If
                        ElseIf 0 = InStr (CommentMarks, c) AndAlso "" <> c Then
				Rem Значение параметра не заключено в кавычки.
                                p3 = p	' начало значения параметра
                                Debug (p, s, "Parameter value found.")
                                Rem В значении не может быть пробелов и признаков комментария.
                                p = textWalkTo (p3, s, textWhitespaces + CommentMarks)
                                p4 = p ' позиция после значения параметра
                                Debug (p, s, "First position after parameter value:")
				Rem Пропуск пробелов после значения параметра.
				p = textWalkOver (p, s, textWhitespaces)
                                Rem Ожидается комментарий или конец строки.
                                c = Mid (s, p, 1)
                                If 0 = InStr (CommentMarks, c) AndAlso "" <> c Then
                                	ErrorPosition = p
                                        ErrorCode = iniEcInvalidCharacter
                                End If
                        Else
                        	Rem Параметр имеет пустое значение.
                                p4 = p3 = p
                        End If
                End If
                Rem Формирование результата обработки строки с параметром.
		If AllRight Then
                	Parameter = Mid (s, p1, p2 - p1)
			Value = Mid (s, p3, p4 - p3)
                        res = -2
                Else
                	res = 0		' при обработке строки обнаружена ошибка
                End If
        End If
        iniParseLine = res
End Function
Rem =========================
Rem Получение текущей секции.
Rem Возврат: текущая секция.
Rem =========================
Public Function iniGetSection As String
	iniGetSection = Section
End Function
Rem =============================
Rem Получение текущего параметра.
Rem Возврат: текущий параметр.
Rem =============================
Public Function iniGetParameter As String
	iniGetParameter = Parameter
End Function
Rem ============================
Rem Получение текущего значения.
Rem Возврат: текущее значение.
Rem ============================
Public Function iniGetValue As String
	iniGetValue = Value
End Function
Rem ======================
Rem Получение кода ошибки.
Rem Возврат: код ошибки.
Rem ======================
Public Function iniGetErrorCode As Integer
	iniGetErrorCode = ErrorCode
End Function
Rem =========================
Rem Получение позиции ошибки.
Rem Возврат: позиция ошибки.
Rem =========================
Public Function iniGetErrorPosition As Integer
	iniGetErrorPosition = ErrorPosition
End Function
Rem =========================================
Rem Завершение обработки файла инициализации.
Rem =========================================
Public Sub iniParseEnd
	Section = ""
        Parameter = ""
        Value = ""
End Sub