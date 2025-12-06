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
Rem ==================================
Rem Хранилище данных на основе строки.
Rem Язык программирования: FreeBASIC.
Rem ==================================
#include once "fut_data.bai"
Rem ==================================================
Rem Конструктор хранилища информации на основе строки.
Rem Вызов: ChunkSize - размер выделяемого блока,
Rem        Separator - разделитель записей.
Rem ==================================================
Public Constructor StringData (ByVal ChunkSize As Integer, ByRef Separator As Const String)
	this.chunkSize = ChunkSize
	this.separator = Separator
	this.pbuf = 1
	this.pcur = 1
End Constructor
Rem ========================================================
Rem Оператор присваивания хранилища.
Rem Вызов: that - хранилище, которое присваивается текущему.
Rem ========================================================
Public Operator StringData.Let (ByRef that As Const StringData)
	this.sbuf = that.sbuf
	this.nbuf = that.nbuf
	this.pbuf = that.pbuf
	this.pcur = that.pcur
	this.cnt = that.cnt
	this.chunkSize = that.ChunkSize
	this.separator = that.Separator
End Operator
Rem =======================================
Rem Помещение строки в хранилище.
Rem Вызов: s - добавляемая строка.
Rem Возврат: -1 - строка добавлена,
Rem           0 - ошибка добавления строки.
Rem =======================================
Public Function StringData.Store (ByRef s As Const String) As Integer
	Dim res As Integer, l1 As Integer, l2 As Integer
	Rem Проверка отсутствия разделителя в добавляемой строке.
	res = 0 = InStr (s, this.separator)
	If -1 = res Then
		Rem Подготовка достаточного пространства для размещения строки.
		l1 = Len (s)			' длина добавляемой строки
		l2 = Len (this.separator)	' длина разделителя
		While this.nbuf < this.pbuf + l1 + l2 - 1 
			this.sbuf += Space (this.chunksize)
			this.nbuf += this.chunksize
		Wend
		Rem Размещение строки в хранилище и завершение её разделителем.
		Mid (this.sbuf, this.pbuf, l1) = s
		this.pbuf += l1
		Mid (this.sbuf, this.pbuf, l2) = this.separator
		this.pbuf += l2
		this.cnt += 1
	End If
	StringData.Store = res
End Function
Rem ==============================
Rem Получение строки из хранилища.
Rem Возврат: очередная строка.
Rem ==============================
Public Function StringData.Read As String
	Dim res As String, p As Integer
	If this.pcur < this.pbuf Then
		p = InStr (this.pcur, this.sbuf, this.separator)
		res = Mid (this.sbuf, this.pcur, p - this.pcur)
		this.pcur = p + Len (this.separator)
	End If
	StringData.Read = res
End Function
Rem ====================================
Rem Сброс указателя на начало хранилища.
Rem ====================================
Public Sub StringData.Restore
	this.pcur = 1
End Sub
Rem =======================================
Rem Получение количества строк в хранилище.
Rem Возврат: количество строк в хранилище.
Rem =======================================
Public Function StringData.Count As Integer
	StringData.Count = this.cnt
End Function
Rem ======================================
Rem Удаление всей информации из хранилища.
Rem ======================================
Public Sub StringData.Wipe
	this.sbuf = ""
	this.nbuf = 0
	this.pbuf = 1
	this.pcur = 1
	this.cnt = 0
End Sub
Rem =============================================
Rem Деструктор хранилища данных на основе строки.
Rem =============================================
Public Destructor StringData
End Destructor
