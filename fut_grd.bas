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
Rem =========================================================
Rem Модуль защиты путей в файловой системе.
Rem Сначала должнен быть установлен требуемый режим работы
Rem со строками - чувствительный или не чувствительный к
Rem регистру символов и решение, принимаемое по умолчанию.
Rem Потом должны быть добавлены правила. Затем должна быть
Rem выполнена подготовка модуля к работе. И лишь после этого
Rem можно использовать модуль для проверки путей в файловой
Rem системе.
Rem Язык программирования: FreeBASIC.
Rem =========================================================
#include "fut_data.bai"
#include "fut_file.bai"
Dim Shared CaseSensitive As Integer, DefaultDecision As Integer, Ready As Integer
Dim Shared Rules As StringData = StringData (128, Chr (7))
Dim Shared RuleKind (Any) As Integer
Dim Shared RuleValue (Any) As String
Rem ======================================================
Rem Установка режима чувствительности к регистру символов.
Rem Вызов: cs - режим чувствительности:
Rem             0 - регистр символов безразличен,
Rem           <>0 - регистр символов имеет значение.
Rem Нельзя изменять режим чувствительности у модуля,
Rem который уже подготовлен к работе.
Rem ======================================================
Public Sub grdSetCaseSensitivity (ByVal cs As Integer)
	CaseSensitive = Iif (0 = cs, 0, -1)
	Ready = 0
End Sub
Rem ====================================================
Rem Установка решения, которое принимается, если ни одно
Rem правило не подошло проверяемому пути.
Rem Вызов: dd - решение по умолчанию:
Rem         0 - запретительное (deny),
Rem       <>0 - разрешительное (allow).
Rem Нельзя изменять решение по умолчанию у модуля,
Rem который уже подготовлен к работе.
Rem ====================================================
Public Sub grdSetDefaultDecision (ByVal dd As Integer)
	DefaultDecision = Iif (0 = dd, 0, -1)
	Ready = 0
End Sub
Rem ===============================================================
Rem Добавление правила.
Rem Вызов: Kind - тип правила (0 - запрещающее, -1 - разрешающее).
Rem        Pref - префикс пути в файловой системе.
Rem Возврат: -1 - правило добавлено, 0 - ошибка добавления правила.
Rem В уже подготовленный модуль правила добавлять нельзя.
Rem ===============================================================
Public Function grdAddRule (ByVal Kind As Integer, ByRef Pref As Const String) As Integer
	grdAddRule = Iif (Ready, 0, Rules.Store (Iif (-1 = Kind, "+", "-") + Pref))
End Function
Rem ========================================================
Rem Подготовка модуля к работе.
Rem Возврат: -1 - модуль подготовлен, 0 - ошибка подготовки.
Rem Модуль можно подготовить к работе только один раз.
Rem ========================================================
Public Function grdPrepare As Integer
	Dim res As Integer, Rule As String, i As Integer, n As Integer
	If 0 = Ready Then
		res = -1
		n = Rules.Count
		If n > 0 Then
			ReDim RuleKind (n - 1) As Integer
			ReDim RuleValue (n - 1) As String
			For i = 0 To n - 1
				Rule =  Rules.Read
				RuleKind(i) = Iif ("+" = Left (Rule, 1), -1, 0)
				RuleValue(i) = Mid (Iif (CaseSensitive, Rule, UCase (Rule)), 2)
			Next i
			Rules.Wipe
		End If
		Ready = -1
	End If
	grdPrepare = res
End Function
Rem ==========================================================
Rem Проверка пути в файловой системе на соответствие правилам.
Rem Вызов: PathToCheck - проверяемый путь.
Rem Возврат: -1 - путь соответствует правилам,
Rem           0 - путь нарушает правила.
Rem Если ни одно из правил не подойдёт к проверяемому пути,
Rem то принимается решение, заданное "по умолчанию".
Rem Если модуль не готов к работе, то считается, что любой
Rem путь нарушает правила.
Rem ==========================================================
Public Function grdCheck (ByRef PathToCheck As Const String) As Integer
	Dim res As Integer, Path As String, Rule As String, i As Integer, ub As Integer
	If -1 = Ready Then
		Path = Iif (CaseSensitive, PathToCheck, UCase (PathToCheck))
		ub = UBound (RuleKind)
		i = LBound (RuleKind)
		res = 1	' пока что ни одно правило не подошло проверяемому пути
		While 1 = res AndAlso i <= ub
			If Left (Path, Len (RuleValue (i))) = RuleValue(i) Then
				Rem Обнаружено правило, применимое к проверяемому пути.
				res = RuleKind (i)
			End If
			i += 1
		Wend
	End If
	grdCheck = Iif (1 = res, DefaultDecision, res)
End Function