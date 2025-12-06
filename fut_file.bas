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
Rem Модуль работы с файлами.
Rem Язык программирования: FreeBASIC.
Rem =================================
#include once "dir.bi"
#include once "file.bi"
#include once "fut_data.bai"
Common Shared PMMDD As Integer			' позиция фрагмента ММДД в имени файла
Common Shared TodayMMDD As String*4		' "сегодняшний" день в виде ММДД
Rem ============================================================
Rem Проверка, является ли строка записью целого неотрицательного
Rem числа в заданном диапазоне.
Rem Вызов: s - проверяемая строка,
Rem        md - минимальное количество цифр в числе,
Rem        lb - нижняя граница диапазона,
Rem        ub - верхняя граница диапазона
Rem Возврат: -1 - является, 0 - не является.
Rem ============================================================
Private Function IntInRange (ByRef s As Const String, md As Integer, lb As Integer, ub As Integer) As Integer
	Const c0 As Integer = Asc ("0"), c9 As Integer = Asc ("9")
	Dim res As Integer, n As Integer, i As Integer, v As Integer, c As Integer
	n = Len (s)
	res = md <= n
	For i = 1 To Len (s)
		c = Asc (s, i)
		If c0 <= c AndAlso c <= c9 Then
			v = 10 * v + c - c0
		Else
			res = 0
		End If
	Next i
	If -1 = res Then
		res = lb <= v AndAlso v <= ub
	End If
	IntInRange = res
End Function
Rem ==============================================
Rem Получение последней части полного имени файла.
Rem Вызов: FullName - полное имя файла.
Rem Возврат: последняя часть полного имени файла.
Rem Примечание: разделителем считается наклонная
Rem черта в любом направлении.
Rem ==============================================
Public Function fileBaseName (ByRef FullName As Const String) As String
	fileBaseName = Mid (FullName, 1 + InStrRev (FullName, Any "\/"))
End Function
Rem ================================================================
Rem Получение последней части полного имени файла без расширения.
Rem Вызов: FullName - полное имя файла.
Rem Возврат: последняя часть полного имени файла без расширения.
Rem Примечание: разделителем считается наклонная черта
Rem  в любом направлении.
Rem ================================================================
Public Function fileBaseNameOnly (ByRef FullName As Const String) As String
	Dim p1 As Integer, p2 As Integer
	p1 = 1 + InStrRev (FullName, Any "\/")
	p2 = InStrRev (FullName, ".")
	fileBaseNameOnly = Mid (FullName, p1, p2 - p1)
End Function
Rem ===============================================
Rem Получение абсолютного пути по двум компонентам:
Rem базовому пути и относительному пути.
Rem Вызов: BasePath - базовый путь,
Rem        RelativePath - относительный путь.
Rem Возврат: абсолютный путь, составленный путём
Rem присоединения относительного пути к базовому.
Rem Если в относительной части обнаруживается
Rem абсолютный путь (начинается с "\" иди "X:"),
Rem то базовая часть пути не используется.
Rem ===============================================
Public Function fileDetermineAbsolutePath (ByRef BasePath As Const String, RelativePath As Const String) As String
	Const SEP As String = "\", _
		UNCPREFIX As String = "\\"
	Dim AbsolutePath As String, nb As Integer, pb1 As Integer, pb2 As Integer, _
		nr As Integer, pr1 As Integer, pr2 As Integer, sr As String, k As Integer
	nr = Len (RelativePath)
	Rem Подготовка буфера для формирования абсолютного пути.
	If SEP <> Mid (RelativePath, 1, 1) AndAlso ":" <> Mid (RelativePath, 2, 1) Then
		Rem Вторая часть пути действительно является относительной.
		If SEP <> Right (BasePath, 1) Then
			AbsolutePath = BasePath + SEP + RelativePath
			nb = 1 + Len (BasePath)
			pb2 = nb
		Else
			AbsolutePath = BasePath + RelativePath
			nb = Len (BasePath)
			pb2 = nb
		End If
		pr2 = 0
	Else
		Rem Вторая часть пути является абсолютной.
		AbsolutePath = RelativePath
		nb = Iif (UNCPREFIX <> Left (AbsolutePath, 2), InStr (AbsolutePath, SEP), 2)
		pb2 = nb
		pr2 = pb2
	End If
	Rem Определение неизменной части абсолютного пути.
	pb1 = InStr (AbsolutePath, SEP)
	Rem Разбор относительной части пути
	While nr >= pr2
		pr1 = 1 + pr2
		pr2 = InStr (pr1, RelativePath, SEP)
		If 0 = pr2 Then pr2 = nr + 1
		If pr1 < pr2 Then
			sr = Mid (RelativePath, pr1, pr2 - pr1)
			If ".." = sr  Then
				Rem Отбрасывание последнего компонента абсолютного пути.
				Rem pb2 указывает на последний разделитель абсолютной части пути.
				pb2 = InStrRev (AbsolutePath, SEP, pb2 - 1)
				If pb2 < pb1 Then pb2 = pb1
			ElseIf "." <> sr AndAlso "" <> sr  Then
				Rem Добавление компонента абсолютного пути.
				k = Iif (nr >= pr2, pr2 - pr1 + 1, pr2 - pr1)
				Mid (AbsolutePath, pb2 + 1, k) = Mid (RelativePath, pr1, k)
				pb2 += k
			End If
		End If		
	Wend
	fileDetermineAbsolutePath = Left (AbsolutePath, pb2)
End Function
Rem ================================================================
Rem Определение положения даты на оси времени относительно эталона.
Rem Вызов: MMDD1 - эталонная дата,
Rem        MMDD2 - дата, положение которой требуется определить,
Rem        DM - количество месяцев, на которое вторая дата может
Rem             быть позже эталонной даты.
Rem Возврат: положение даты MMDD2 на оси времени относительно MMDD1:
Rem          <0 - вторая дата раньше эталонной,
Rem          =0 - вторая дата равна эталонной,
Rem          >0 - вторая дата позже эталонной.
Rem Примечание. При выполнении вычислений полагается, что в каждом
Rem             месяце 100 дней.
Rem ================================================================
Public Function fileTimePointMMDD (ByRef MMDD1 As Const String, ByRef MMDD2 As Const String, ByVal DM As Integer) As Integer
	Dim res As Integer, m0 As Integer, m1 As Integer, m2 As Integer, d1 As Integer, d2 As Integer
	m1 = Val (Left (MMDD1, 2))
	d1 = Val (Mid (MMDD1, 3, 2))
	m2 = Val (Left (MMDD2, 2))
	d2 = Val (Mid (MMDD2, 3, 2))
	Rem Определение смещения к месяцу начала отсчёта.
	m0 = (m1 + DM) Mod 12
	Rem Приведение месяцев к началу отсчёта.
	m1 = Iif (m1 > m0, m1 - m0, 12 + m1 - m0)
	m2 = Iif (m2 > m0, m2 - m0, 12 + m2 - m0)
	Rem Определение смещения второй даты относительно первой.
	res = 100 * (m2 - m1) + d2 - d1
	fileTimePointMMDD = res
End Function
Rem ========================================================
Rem Сравнение имён файлов с учётом фрагмента ММДД.
Rem Вызов: Name1 - имя первого файла,
Rem        Name2 - имя второго файла.
Rem Возврат: 1 - Name1 > Name2, 0 - Name1 = Name2,
Rem         -1 - Name1 < Name2.
Rem В первую очередь сравниваются фрагменты ММДД, затем, в
Rem случае их совпадения, оставшиеся части имён файлов.
Rem Используются глобальные переменные:
Rem 	TodayMMDD - "сегодняшний" день в виде ММДД,
Rem	PMMDD - позиция фрагмента ММДД в имени файла.
Rem ========================================================
Public Function fileCompareFileNamesMMDD (ByRef Name1 As Const String, ByRef Name2 As Const String) As Integer
	Dim res As Integer, s1 As String, s2 As String
	s1 = Mid (Name1, PMMDD, 4)
	s2 = Mid (Name2, PMMDD, 4)
	If s1 = s2 Then
		Rem Даты файлов совпадают. Требуется сравнение оставшихся частей имён файлов.
		s1 = Left (Name1, PMMDD - 1)
		s2 = Left (Name2, PMMDD - 1)
		If s1 = s2 Then
			Rem Левые части имён файлов совпадают. Требуется сравнение правых частей.
			s1 = Mid (Name1, PMMDD + 4)
			s2 = Mid (Name2, PMMDD + 4)
			res = Iif (s1 > s2, 1, Iif (s1 = s2, 0, -1))
		Else
			Rem Результат определяется сравнением левых частей имён файлов.
			res = Iif (s1 > s2, 1, -1)
		End If
	Else
		Rem Результат определяется взаимным положением дат
		Rem на временной оси относительно сегодняшнего дня.
		res = Sgn (fileTimePointMMDD (TodayMMDD, s1, 3) - fileTimePointMMDD (TodayMMDD, s2, 3))
	End If
	fileCompareFileNamesMMDD = res
End Function
Rem ==========================================
Rem Построение индекса по массиву строк.
Rem Вызов: Arr - массив строк,
Rem        Ind - пустой массив индексов,
Rem        Cmp - функция сравнения двух строк.
Rem Возврат: Ind - массив индексов.
Rem ==========================================
Public Sub fileIndexingArray (Arr() As String, Ind() As Integer, Cmp As Function (ByRef As Const String, ByRef As Const String) As Integer)
	Dim lb As Integer, ub As Integer, i As Integer, j As Integer, t As Integer
	lb = LBound (Arr)
	ub = UBound (Arr)
	If lb <= ub Then
		Rem Подготовка массива индексов.
		Redim Ind (lb To ub)
		For i = lb To ub
			Ind(i) = i
		Next i
		Rem Сортировка массива индексов методом пузырька.
		For i = lb To ub - 1
			For j = lb To ub - 1
				If Cmp (Arr(Ind(j)), Arr(Ind(j + 1))) = 1 Then
					Swap Ind(j), Ind(j + 1)
				End If
			Next j
		Next i
	Else
		Rem Очистка массива индексов.
		Erase Ind
	End If
End Sub
Rem =====================================================
Rem Проверка соответствия имени файла шаблону с ММДД.
Rem Вызов: FileName - проверяемое имя файла,
Rem        pmd - номер позиции, в которой находится ММДД,
Rem              или 0, если даты в имени не должно быть.
Rem Возврат: -1 - имя файла соответствует шаблону,
Rem           0 - имя файла шаблону не соответствует.
Rem =====================================================
Public Function fileValidFileNameMMDD (ByRef FileName As Const String, ByVal pmd As Integer) As Integer
	Dim res As Integer, smm As String*2, sdd As String*2
	If 0 < pmd Then
		smm = Mid (FileName, pmd, 2)
		sdd = Mid (FileName, pmd + 2, 2)
		res = IntInRange (smm, 2, 1, 12) AndAlso IntInRange (sdd, 2, 0, 99)
	Else
		res = -1
	End If
	fileValidFileNameMMDD = res
End Function
Rem =========================================================================
Rem Формирование массива имён файлов, удовлетворяющих маске.
Rem Вызов: PathAndMask - путь к папке и маска файлов.
Rem Возврат: ArrFileNames - массив с именами файлов, удовлетворяющих маске.
Rem Примечание. Если в маске присутствует фрагмент "####", то предполагается,
Rem что на его месте в имени файла должна быть дата в формате ММДД.
Rem Месяц ММ должен быть в диапазоне "01" - "12", а день ДД - "00" - "99".
Rem =========================================================================
Public Sub fileFillArrayWithFileNames (ByRef PathAndMask As Const String, ArrFileNames() As String)
	Const BUFCHUNKSIZE As Integer = 4096, DELIMITER As String = "^"
	Dim FileName As String, buf As String = Space (BUFCHUNKSIZE), p As Integer, _
		t As Integer, n As Integer, lfn As Integer, lb As Integer, ld As Integer, _
		PathAndMaskMod As String, pmd As Integer
	Rem Обработка фрагмента MMDD в маске имён файлов.
	p = InStrRev (PathAndMask, Any "/\")
	pmd = InStr (1 + p, PathAndMask, "####")
	PathAndMaskMod = Iif (pmd > 0, Left (PathAndMask, pmd - 1) + "????" + Mid (PathAndMask, 4 + pmd), _
		PathAndMask)
	pmd -= p
	Rem Накопление сведений о файлах.
	lb = BUFCHUNKSIZE
	ld = Len (DELIMITER)
	p = 1	' позиция в буфере
	n = 0	' количество файлов
	FileName = Dir (PathAndMaskMod, fbNormal)
	While "" <> FileName
		Rem Проверка полученного имени на соответствие фрагменту ММДД.
		If fileValidFileNameMMDD (FileName, pmd) Then
			Rem Запоминание найденного имени файла.
			Rem Контроль достаточности размера буфера.
			lfn = Len (FileName)
			While p + lfn + ld > lb
				buf += 	Space (BUFCHUNKSIZE)
				lb += BUFCHUNKSIZE
			Wend
			Rem Добавление строки в буфер.
			Mid (buf, p, lfn) = FileName
			Mid (buf, p + lfn, ld) = DELIMITER
			p += lfn + ld
			n += 1
		End If
		FileName = Dir
	Wend
	Rem Формирование массива имён файлов.
	If n > 0 Then
		Redim ArrFileNames (n - 1) As String
		p = 1
		t = InStr (buf, DELIMITER)
		For i As Integer = 0 To n - 1
			ArrFileNames(i) = Mid (buf, p, t - p)
			p = t + ld
			t = InStr (p, buf, DELIMITER)
		Next i
	Else
		Erase ArrFileNames
	End If
	buf = ""
End Sub
Rem =========================================================
Rem Проверка возможности доступа к файлу для чтения и записи.
Rem Вызов: FileName - имя файла.
Rem Возврат: -1 - к файлу есть доступ для чтения и записи,
Rem           0 - нет доступа для чтения и записи к файлу.
Rem =========================================================
Public Function fileAccessibleRW (ByRef FileName As Const String) As Integer
	Dim res As Integer = 0, FileNumber As Integer = FreeFile, EC As Integer
        Rem Открытие файла для двоичного доступа приводит к созданию файла,
        Rem если он не существовал. Если файл существовал, то его содержимое
        Rem сохраняется.
        Open FileName For Binary Access Read Write As #FileNumber
        EC = Err
	If 0 = EC Then
        	Close #FileNumber
                res = -1
        End If
        fileAccessibleRW = res
End Function
Rem =====================================
Rem Генерирование случайного имени файла.
Rem Вызов: Length - длина имени файла.
Rem Возврат: случайное имя файла.
Rem =====================================
Public Function fileRandomName (ByVal Length As Integer) As String
	Dim n As Integer, s As String = Space (Length), c As String*1
        For i As Integer = 1 To Length
		n = 35 * Rnd
		c = Iif (n < 10, Chr (Asc ("0") + n), Chr (Asc ("A") + n - 10))
                Mid (s, i, 1) = c
        Next i
        fileRandomName = S
End Function
Rem ===============================
Rem Проверка существования папки.
Rem Вызов: FolderName - имя папки.
Rem Возврат: -1 - папка существует,
Rem           0 - такой папки нет.
Rem ===============================
Public Function fileFolderExists (ByRef FolderName As Const String) As Integer
	Dim res As Integer, SavedCurDir As String
	Rem По умолчанию res = 0.
	SavedCurDir = CurDir
	If 0 = ChDir (FolderName) Then
		ChDir (SavedCurDir)
		res = -1
	End If
	fileFolderExists = res
End Function
Rem =============================================
Rem Проверка доступа к папке для чтения.
Rem Вызов: FolderName - имя папки.
Rem Возврат: -1 - к папке есть доступ для чтения.
Rem           0 - нет доступа к папке для чтения.
Rem =============================================
Public Function fileFolderAccessibleR (ByRef FolderName As Const String) As Integer
	fileFolderAccessibleR = "" <> Dir (FolderName + "\*", fbDirectory)
End Function
Rem ======================================================
Rem Проверка доступа к папке для чтения и записи.
Rem Вызов: FolderName - имя папки.
Rem Возврат: -1 - к папке есть доступ для чтения и записи.
Rem           0 - нет доступа к папке для чтения и записи.
Rem ======================================================
Public Function fileFolderAccessibleRW (ByRef FolderName As Const String) As Integer
	Dim res As Integer = 0
        Rem Проверка существования каталога.
        If "" <> Dir (FolderName, fbDirectory) Then
        	Dim FileName As String, FileNumber As Integer, EC As Integer, _
                	FullFileName As String
		Rem Генерирование уникального имени для временного файла.
		Do
                        FileName = fileRandomName (8) + ".TMP"
                        FullFileName = FolderName + "\" + FileName
		Loop Until Dir (FullFileName, fbDirectory Or fbNormal Or fbHidden Or fbSystem) <> FileName
                Rem Попытка создания файла с доступом для чтения и записи.
                FileNumber = FreeFile
                Open FullFileName For Binary Access Read Write As #FileNumber
		EC = Err
                If 0 = EC Then
			res = -1
                        Close #FileNumber
                        Kill FullFileName
                End If
        End If
        fileFolderAccessibleRW = res
End Function
Rem =========================================================
Rem Копирование содержимого одной папки в другую папку.
Rem Вызов: SrcFolder - папка с элементами для копирования,
Rem        DstFolder - целевая папка.
Rem Возврат: -1 - копирование завершено успешно,
Rem           0 - при копировании произошла ошибка.
Rem =========================================================
Public Function fileCopyFolderContents (ByRef SrcFolder As Const String, ByRef DstFolder As Const String) As Integer
	Dim res As Integer, fName As String, SrcSubFolder As String, DstSubFolder As String
	Dim folders As StringData = StringData (128, Chr (7))
	Rem Копирование файлов.
	Rem Если файл-источник и целевой файл оба имеют атрибут "скрытый" (hidden),
	Rem то копирование проходит успешно. Если же файл-источник имеет атрибут
	Rem "скрытый", а файл-приёмник - нет, то FileCopy возвращает ошибку.
	Rem Если файл-источник имеет атрибут "только для чтения", а файл-приёмник - нет,
	Rem то после копирования файл-приёмник приобретает атрибут "только для чтения".
	Rem Если файл-приёмник имеет атрибут "только для чтения", то FileCopy возвращает
	Rem ошибку. 
	res = -1
	fName = Dir (SrcFolder + "\*", fbNormal Or fbSystem Or fbHidden Or fbReadOnly)
	While -1 = res AndAlso "" <> fName
		res = 0 = FileCopy (SrcFolder + "\" + fName, DstFolder + "\" + fName)
		fName = Dir
	Wend
	Rem Подготовка списка вложенных папок.
	fName = Dir (SrcFolder + "\*", fbDirectory Or fbHidden Or fbReadOnly)
	While -1 = res AndAlso "" <> fName
		If "." <> fName AndAlso ".." <> fName Then
			res = folders.Store (fName)
		End If
		fName = Dir
	Wend
	Rem Копирование вложенных папок по списку.
	fName = folders.Read
	While -1 = res AndAlso "" <> fName
		SrcSubFolder = SrcFolder + "\" + fName
		DstSubFolder = DstFolder + "\" + fName
		If 0 = fileFolderExists (DstSubFolder) Then
			res = 0 = MkDir (DstSubFolder)
		End If
		If -1 = res Then
			res = fileCopyFolderContents (SrcSubFolder, DstSubFolder)
		End If
		fName = folders.Read
	Wend
	fileCopyFolderContents = res
End Function
Rem ==========================================================
Rem Удаление папки со всем её содержимым.
Rem Вызов: FolderName - имя удаляемой папки.
Rem Возврат: -1 - папка удалена,
Rem           0 - ошибка удаления папки.
Rem TOFIX: элементы файловой системы с установленным атрибутом
Rem "только для чтения" не могут быть удалены встроенными
Rem средствами языка программирования FreeBasic. Поэтому в
Rem дальнейшем может потребоваться доработка.
Rem ==========================================================
Public Function fileKillFolder (ByRef FolderName As Const String) As Integer
	Dim res As Integer, fName As String, i As Integer, n As Integer
	Dim folders As StringData = StringData (128, Chr (7))
	res = -1
	Rem Формирование списка подкаталогов.
	fName = Dir (FolderName + "\*", fbDirectory Or fbHidden)
	While -1= res AndAlso "" <> fName
		If "." <> fName AndAlso ".." <> fName Then
			res = folders.Store (fName)
		End If
		fName = Dir
	Wend
	Rem Удаление подкаталогов по списку.
	n = folders.Count
	i = 1
	While -1 = res AndAlso i <= n
		res = fileKillFolder (FolderName + "\" + folders.Read)
		i += 1
	Wend
	Rem Удаление файлов.
	If -1 = res Then
		fName = Dir (FolderName + "\*", fbNormal Or fbHidden Or fbSystem Or fbReadOnly)
	End If
	While -1 = res AndAlso "" <> fName 
		res = 0 = Kill (FolderName + "\" + fName)
		fName = Dir
	Wend
	Rem Удаление подкаталога.
	If -1 = res Then
		res = 0 = RmDir (FolderName)
	End If
	fileKillFolder = res
End Function
