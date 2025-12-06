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
Rem ====================================
Rem Модуль работы с настройками системы.
Rem Язык программирования: FreeBasic.
Rem ====================================
Dim Shared Settings(Any) As String, Values(Any) As String, Initialized(Any) As Integer
Rem =========================================
Rem Подготовка к работе с перечнем настроек.
Rem Вызов: s - строка с перечнем настроек,
Rem        c - разделитель настроек в строке.
Rem =========================================
Public Sub stngInit (ByRef s As Const String, ByRef c As String = "^")
	Dim i As Integer, p As Integer, r As Integer, n As Integer, lc As Integer
        Rem Подсчёт количества настроек
	n = 0
        If "" <> s Then
        	n = 1	' первая настройка - до разделителя
		lc = Len (c)
		p = InStr (s, c)
	        While p > 0 AndAlso Asc (s, p) <> 0
				n += 1
			p = InStr (p + lc, s, c)
		Wend
        End If
        Rem Формирование массивов настроек и их значений.
        If n > 0 Then
		Redim Settings(n - 1), Values(n - 1), Initialized(n - 1)
		p = 1
		r = InStr (s, c)
                For i = 0 To n - 1
			Settings(i) = Mid (s, p, Iif (r > 0, r, 1 + Len (s)) - p)
                        p = r + lc
                        r = InStr (p, s, c)
                Next i
        End If
End Sub
Rem =======================================
Rem Поиск указанной настройки.
Rem Вызов: s - название настройки.
Rem Возврат: индекс настройки в массиве или
Rem          -1, если настройка не найдена.
Rem =======================================
Private Function stngFind (ByRef s As Const String) As Integer
	Dim i As Integer, n As Integer
	i = LBound (Settings)
        n = UBound (Settings)
        While i <= n AndAlso Settings(i) <> s
        	i += 1
        Wend
        stngFind = Iif (i > n, -1, i)
End Function
Rem ========================================
Rem Установка значения настройки.
Rem Вызов: s - название настройки,
Rem        v - значение настройки,
Rem        o - разрешение на перезапись.
Rem Возврат: -1 - значение установлено,
Rem           0 - ошибка установки значения.
Rem ========================================
Public Function stngSetValue (ByRef s As Const String, ByRef v As Const String, ByVal o As Integer) As Integer
	Dim i As Integer = stngFind (s), res As Integer = 0
	i = stngFind (s)
        If i >= 0 AndAlso (Initialized(i) = 0 OrElse o)Then
		Values(i) = v
                Initialized(i) = -1
                res = -1
        End If
	stngSetValue = res
End Function
Rem =====================================
Rem Получение значения настройки.
Rem Вызов: s - название настройки.
Rem Возврат: значение настройки.
Rem Примечание. Если настройка не задана,
Rem то возвращается пустая строка.
Rem =====================================
Public Function stngGetValue (ByRef s As Const String) As String
	Dim i As Integer = stngFind (s)
        stngGetValue = Iif (i >= 0, Values(i), "")
End Function