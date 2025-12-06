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
Rem ========================================================
Rem Модуль работы с состоянием системы.
Rem Состояния хранятся в строках текстового файла в формате:
Rem НазваниеСостояния=ЗначениеСостояния.
Rem Строка состояния, которое было изменено последним,
Rem будет последней строкой в файле.
Rem Язык программирования: FreeBasic.
Rem ========================================================
Const UBSTATE As Integer = 4
Dim Shared FileName As String
Rem ============================================
Rem Указание имени файла с состояниями системы.
Rem Вызов: fn - имя файла с состояниями системы.
Rem ============================================
Public Sub stteSetFileName (ByRef fn As String)
	FileName = fn
End Sub
Rem ===========================================
Rem Получение значения интересующего состояния.
Rem Вызов: StateName - наименование состояния.
Rem Возврат: строка со значением состояния
Rem          или пустая строка в случае ошибки.
Rem ===========================================
Public Function stteGetValue (ByRef StateName As Const String) As String
	Dim res As String, FileNumber As Integer, StateLine As String, _
		Pattern As String, lp As Integer
	If "" <> FileName Then
		Pattern = StateName + "="
		lp = Len (Pattern)
		FileNumber = FreeFile
		Open FileName For Input As #FileNumber
		While 0 = Err AndAlso 0 = EOF (FileNumber)
			Line Input #FileNumber, StateLine
			If 0 = Err AndAlso "" <> StateLine Then
				If Left (StateLine, lp) = Pattern Then
					res = Mid (StateLine, lp + 1)
				End If
			End If
		Wend
		Close #FileNumber
	End If
	stteGetValue = res
End Function
Rem =====================================================
Rem Установка значения состояния.
Rem Вызов: StateName - наименование состояния,
Rem        StateValue - значение состояния.
Rem Возврат: -1 - значение состояния установлено успешно,
Rem           0 - ошибка установки значения состояния
Rem =====================================================
Public Function stteSetValue (ByRef StateName As Const String, ByRef StateValue As Const String) As Integer
	Dim res As Integer, FileNumber As Integer, StateLines (UBSTATE) As String, _
		i As Integer, n As Integer, Pattern As String, PatternLen As Integer, _
		StateLine As String
	res = -1
	Pattern = StateName + "="
	PatternLen = Len (Pattern)
	If "" <> FileName Then
		Rem Считывание строк из файла состояний системы.
		FileNumber = FreeFile
		Open FileName For Input As #FileNumber
		If 0 = Err Then
			While 0 = Err AndAlso 0 = EOF (FileNumber) AndAlso n <= UBSTATE
				Line Input #FileNumber, StateLine
				StateLine = Trim (StateLine)
				Rem Пропуск пустых строк и строки с изменяемым состоянием.
				If "" <> StateLine AndAlso Left (StateLine, PatternLen) <> Pattern Then
					StateLines(n) = StateLine
					n += 1
				End If
			Wend
			If 0 <> Err Then
				Rem Ошибка при чтении файла.
				res = 0
			End If
			Close #FileNumber
		Else
			Rem Ошибка при открытии файла для чтения.
			res = 0
		End If
		If -1 = res Then
			If n <= UBSTATE Then
				Rem Добавление строки с устанавливаемым состоянием.
				StateLines(n) = Pattern + StateValue
				Rem Сохранение состояний в файл.
				i = 0
				Open FileName For Output As #FileNumber
				If 0 = Err Then
					While 0 = Err AndAlso i <= n
						Print #FileNumber, StateLines(i)
						i += 1
					Wend
					If 0 <> Err Then
						Rem Ошибка при записи в файл.
						res = 0
					End If
					Close #FileNumber
				Else
					Rem Ошибка при открытии файла для записи.
					res = 0
				End If
			Else
				Rem Нет места для нового параметра.
				res = 0
			End If
		End If
	End If
	stteSetValue = res
End Function