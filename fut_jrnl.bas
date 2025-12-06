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
Rem =================================
Rem Модуль ведения журнала операций.
Rem Язык программирования: FreeBASIC.
Rem =================================
#include once "vbcompat.bi"
#include once "windows.bi"
#include once "fut_data.bai"
Dim Shared JournalFileName As String
Dim Shared JournalRecords As StringData = StringData (2048, Chr (7))
Dim Shared Timer0 As Double	' отметка времени первой записи в журнале
Rem ==============================================
Rem Получение строки с текущими датой и временем.
Rem Возврат: строка в формате YYYY-MM-DDThh:mm:ss.
Rem ==============================================
Private Function CurrentDateTime As String
	Dim DT As String, TM As String
	DT = Date
	TM = Time
	CurrentDateTime = Mid (DT, 4, 3) + Left (DT, 3) + Right (DT, 4) + "T" + TM
End Function
Rem ================================================
Rem Подготовка к ведению журнала.
Rem Вызов: FileName - имя файла для ведения журнала.
Rem ================================================
Public Sub jrnlOpen (ByRef FileName As Const String)
	JournalFileName = FileName
End Sub
Rem ==========================================
Rem Запись в журнал заголовка блока сообщений.
Rem Вызов: Title - строка заголовка.
Rem Возврат: -1 - успешная запись в журнал,
Rem           0 - ошибка записи.
Rem ==========================================
Public Function jrnlTitle (ByRef Title As Const String) As Integer
	Timer0 = Timer
	jrnlTitle = JournalRecords.Store (">>> " + Title + " " + CurrentDateTime)
End Function
Rem =======================================
Rem Запись в журнал итоговой записи.
Rem Вызов: Result - общий результат работы:
Rem        -1 - успешно, 0 - неудачно.
Rem =======================================
Public Function jrnlFooter (ByVal Result As Integer) As Integer
	Dim h As String*2, m As String*2, s As String*2, et As UInteger
	et = Int (Timer - Timer0)
	s = Right ("0" + Str (et Mod 60), 2)
	et = Int (et / 60)
	m = Right ("0" + Str (et Mod 60), 2)
	et = Int (et / 60)
	h = Right ("0" + Str (et), 2)
	jrnlFooter = JournalRecords.Store ("<<< +" + h + ":" + m + ":" + s + Iif (Result, "", " !!!"))
End Function
Rem ======================================================
Rem Подготовка сообщения о результате выполнения операции.
Rem Вызов: Description - описание операции,
Rem        Result - результат выполнения операции:
Rem                 -1 - успешно, 0 - неудачно.
Rem ======================================================
Public Function jrnlMsgOperation (ByRef Description As Const String, ByVal Result As Integer) As String
	jrnlMsgOperation = Iif (Result, "    ", "!!! ") + Description
End Function
Rem ===================================================
Rem Запись сообщения в журнал.
Rem Вызов: Message - строка с сообщением.
Rem Возврат: -1 - успешная запись в журнал,
Rem           0 - ошибка записи.
Rem ===================================================
Public Function jrnlWrite (ByRef Message As Const String) As Integer
	jrnlWrite = JournalRecords.Store (Message)
End Function
Rem ==========================================================
Rem Выгрузка записанных сообщений в файл журнала одним блоком.
Rem Поскольку в языке программирования FreeBasic 1.01.0
Rem не работают блокировки файлов, реализация выполнена
Rem с использованием средств Win32 API.
Rem ==========================================================
Public Sub jrnlClose
	Const MAX_ATTEMPT = 20
	Dim res As Integer, FileHandle As HANDLE, Buf As String, N1 As DWORD, N2 As DWORD
	Dim i As Integer, n As Integer
	Rem По умолчанию res = 0, i = 0.
	Rem Подготовка к работе с файлом.
	Do
		i += 1
		FileHandle = CreateFile (StrPtr (JournalFileName), GENERIC_WRITE, FILE_SHARE_READ, _
			NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL)
		If INVALID_HANDLE_VALUE = FileHandle Then
			Sleep 150 + Int (100 * Rnd), 1
		End If
	Loop Until INVALID_HANDLE_VALUE <> FileHandle OrElse MAX_ATTEMPT <= i
	If INVALID_HANDLE_VALUE <> FileHandle Then
		Rem Установка указателя на конец файла.
		If INVALID_SET_FILE_POINTER <> SetFilePointer (FileHandle, 0, NULL, FILE_END) Then
			Rem Перебор записей журнала.
			n = JournalRecords.Count
			i = 1
			res = -1
			While -1 = res AndAlso i <= n
				Rem Выгрузка очередной записи журнала в файл.
				Buf = JournalRecords.Read + Chr(13) + Chr(10)
				N1 = Len (Buf)
				If 0 = WriteFile (FileHandle, StrPtr (Buf), N1, @N2, NULL) Then
					res = 0
				End If
				i += 1
			Wend
			Rem Очистка журнала.
			JournalRecords.Wipe
		End If
		Rem Завершение работы с файлом.
		CloseHandle FileHandle
	End If
End Sub