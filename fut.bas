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
Rem ===================================================
Rem Программа для обновления файлов «File Update Tool».
Rem Язык программирования: FreeBasic.
Rem ===================================================
#include once "windows.bi"
#include once "file.bi"
#include once "fut_stng.bai"
#include once "fut_data.bai"
#include once "fut_lang.bai"
#include once "fut_gui.bai"
#include once "fut_text.bai"
#include once "fut_ini.bai"
#include once "fut_file.bai"
#include once "fut_stte.bai"
#include once "fut_work.bai"
#include once "fut_jrnl.bai"
#include once "fut_grd.bai"
#include once "fut_http.bai"
Rem ===================================================
Rem Копирование строк из структуры StringData в массив.
Rem Вызов: sd - структура данных StringData.
Rem Возврат: a - массив строк переменной длины.
Rem ===================================================
Sub StringDataToArray (ByRef sd As StringData, a() As String)
	Dim n As Integer, i As Integer
	n = sd.count
	If n > 0 Then
		ReDim a(n - 1) As String
		sd.Restore
		For i = 0 To n - 1
			a(i) = sd.Read
		Next i
	Else
		Erase a
	End If
End Sub
Rem ================================================
Rem Определение абсолютного пути в файловой системе.
Rem Вызов: BasePath - базовая часть пути,
Rem Вызов: RelativePath - относительная часть пути.
Rem Возврат: составной абсолютный путь или
Rem пустая строка, если "" = RelativePath.
Rem ================================================
Function DetermineAbsolutePath (ByRef BasePath As Const String, ByRef RelativePath As Const String) As String
	Dim AbsolutePath As String
	Rem AbsolutePath = ""
	If "" <> RelativePath Then
		If "HTTP://" <> UCase (Left (RelativePath, 7)) Then
			AbsolutePath = fileDetermineAbsolutePath (BasePath, RelativePath)
		Else
			AbsolutePath = RelativePath
		End If
	End If
	DetermineAbsolutePath = AbsolutePath
End Function
Rem ====================================================
Rem Заполнение массива именами файлов по сведениям
Rem с веб-сервера обновлений.
Rem Вызов: UserAgent - строка-идентификатор программы FUT,
Rem        Url - адрес веб-ресурса с файлами обновлений,
Rem        Mask - маска файлов обновлений.
Rem Возврат: -1 - массив FileNames заполнен,
Rem           0 - ошибка заполнения массива.
Rem ====================================================
Function FillArrayWithFileNamesFromWebServer (ByRef UserAgent As Const String, ByRef Url As Const String, ByRef Mask As Const String, FileNames () As String) As Integer
	Dim res As Integer, html As String, host As String, port As UShort, path As String
	res = -1
	httpParseUrl Url, host, port, path
	Rem Подключение к веб-серверу.
	If -1 <> httpInit (UserAgent, host, port) Then
		res = 0
	End If
	Rem Получение списка файлов и заполнение массива.
	If -1 = res Then
		If httpGetText (path, html, 4096) Then
			httpFillArrayWithFileNames html, Mask, FileNames()
		Else
			res = 0
		End If
	End If
	FillArrayWithFileNamesFromWebServer = res
End Function
Rem ===================
Rem Основная программа.
Rem ===================
Const MTHSFWD As Integer = 3		' количество месяцев после "сегодняшней" даты
Common TodayMMDD As String*4		' "сегодняшняя" дата в виде ММДД
Common PMMDD As Integer			' позиция даты в виде ММДД в именах файлов
Dim AppName As String = "File Update Tool"
Dim UserAgent As String
Dim UnpackCmd As String
Dim Station As String
Dim SavedCurDir As String		' сохранённый текущий каталог
Dim IniFileName As String
Dim buf As String*256
Dim UpdFileMask As String
Dim LastUpdateFileName As String	' имя файла последнего установленного обновления
Dim WorkThreadHandle As Any Ptr		' манипулятор потока, выполняющего основную работу
Dim WTD As workThreadData		' данные для потока, выполняющего основную работу
Dim UpdToRemove As StringData = StringData (128, Chr (7))
Dim UpdToRetrieve As StringData = StringData (128, Chr (7))
Dim UpdToInstall As StringData = StringData (128, Chr (7))
Dim BadResult As Integer = 0
Common Shared hInstance As HINSTANCE
hInstance = GetModuleHandle (NULL)
Rem --------------------------------------
Rem Обработка параметров командной строки.
Rem --------------------------------------
IniFileName = Command (1)	' параметры командной строки очищены от кавычек
langSetLanguage ("en")
If "" = IniFileName Or "" <> Command (2) Then
	Rem Командная строка должна содержать один-единственный параметр -
        Rem полное имя файла инициализации.
        guiDisplayMessage (MSGERROR, textSubstitute (langStr (1), fileBaseName (Command (0))), AppName)
        BadResult = -1
End If
Rem -----------------------------------------------------
Rem Сохранение текущего каталога и обеспечение выполнения
Rem единственного экземпляра программы FUT для данного
Rem конфигурационного файла.
Rem -----------------------------------------------------
SavedCurDir = CurDir
If -1 <> guiExclusiveWorkStart (IniFileName) Then
	guiDisplayMessage MSGERROR, "Another instance of the FUT application already started.", AppName
	BadResult = -1
End If
Rem ------------------------------------------
Rem Получение настроек из файла инициализации.
Rem ------------------------------------------
If 0 = BadResult Then
	Dim IniFile As Integer = FreeFile, s As String, i As Integer, res As Integer
	Open IniFileName For Input Access Read As #IniFile
        If 0 = Err Then
        	Rem Подготовка к работе с настройками.
                stngInit ".Language^.Station^.UpdFileMask^.UserAgent^Commands.Unpack^Commands.RunApp^" _
                	"Files.State^Files.Journal^Folders.Target^Folders.Source^Folders.Cache^" _
                	"Folders.Temp^Guard.Default^Debug.TodayMMDD", "^"
        	Rem Построчная обработка файла.
		iniParseBegin """", ";"
		While Not EOF (IniFile)
			i += 1
	        	Line Input #IniFile, s
                        res = iniParseLine (s)
                        If -2 = res Then
		        	Dim Parameter As String, Value As String
                        	Rem Получен очередной параметр.
                                Parameter = iniGetSection + "." + iniGetParameter
                                Value = iniGetValue
                                If "Guard.Allow" = Parameter Then
	                                Rem Обработка разрешительного правила из секции Guard.
	                                grdAddRule (-1, Value)
                                ElseIf "Guard.Deny" = Parameter Then
					Rem Обработка запретительного правила из секции Guard.
					grdAddRule (0, Value)
                                Else
					Rem Установка значения параметра с запретом изменения.
					If stngSetValue (Parameter, Value, 0) Then
						Rem Немедленная обработка параметра,
						Rem устанавливающего язык интерфейса.
						If ".Language" = Parameter Then
							langSetLanguage (Value)
						End If
					Else
						Rem Ошибка установки параметра.
						guiDisplayMessage MSGWARN, textSubstitute (langStr (3), Str(i) + "^" + Parameter + "^" + Value), AppName
					End If
				End If
                        ElseIf 0 = res Then
                        	Rem При обработке строки обнаружена ошибка.
		                guiDisplayMessage MSGERROR, textSubstitute (langStr(4), Str(i) + "^" + s), AppName
                        End If
                Wend
                iniParseEnd
		Close #IniFile
        Else
        	Rem Ошибка открытия файла инициализации для чтения.
        	guiDisplayMessage MSGERROR, textSubstitute (langStr (2), IniFileName), AppName
                BadResult = -1
        End If
End If
Rem --------------------
Rem Подготовка к работе.
Rem --------------------
If 0 = BadResult Then
	Rem Получение "сегодняшней" даты в формате ММДД.
	TodayMMDD = stngGetValue ("Debug.TodayMMDD")
	If "" = TodayMMDD Then
		Dim s As String
		s = Date	' "сегодняшняя" дата в формате ММ-ДД-ГГГГ.
		TodayMMDD = Left (s, 2) + Mid (s, 4, 2)
	Else
		Rem Предупреждение о действии отладочной настройки "сегодняшней" даты.
		guiDisplayMessage MSGWARN, textSubstitute (langStr (9), TodayMMDD), AppName
	End If
	Rem Маска файлов с обновлениями.
	UpdFileMask = stngGetValue (".UpdFileMask")
	PMMDD = InStr (UpdFileMask, "####")
	If 0 = PMMDD Then
		guiDisplayMessage MSGERROR, textSubstitute (langStr(5), UpdFileMask), AppName
		BadResult = -1
	End If
	Rem Получение команды для распаковки обновлений.
	UnpackCmd = stngGetValue ("Commands.Unpack")
	If "" <> UnpackCmd Then
		Dim p As Integer
		p = InStrRev (UnpackCmd, "^")
		If 0 < p Then
			WTD.UnpackProgram = fileDetermineAbsolutePath (SavedCurDir, Trim (Left (UnpackCmd, p - 1)))
			WTD.UnpackArguments = Mid (UnpackCmd, 1 + p)
		Else
			WTD.UnpackProgram = fileDetermineAbsolutePath (SavedCurDir, Trim (UnpackCmd))
			WTD.UnpackArguments = ""
		End If
	Else
		guiDisplayMessage MSGERROR, textSubstitute (langStr (12), "Command.Unpack"), AppName
		BadResult = -1
	End If
	Rem Получение идентификатора компьютера.
	Station = stngGetValue (".Station")
	If "" = Station Then
		guiDisplayMessage MSGERROR, textSubstitute (langStr (12), ".Station"), AppName
		BadResult = -1
	End If
	Rem Получение идентификатора клиента для работы с HTTP-сервером.
	UserAgent = stngGetValue (".UserAgent")
	If "" = UserAgent Then
		UserAgent = "FUT"
	End If
End If
Rem -----------------------------
Rem Проверка готовности к работе.
Rem -----------------------------
If 0 = BadResult Then
	Dim fName As String
	Rem Файл состояния системы.
        fName = fileDetermineAbsolutePath (SavedCurDir, Trim(stngGetValue ("Files.State")))
        If 0 = fileAccessibleRW (fName) Then
        	guiDisplayMessage MSGERROR, textSubstitute (langStr(6), fName), AppName
                BadResult = -1
        Else
        	Rem Настройка на работу с файлом состояния системы.
        	stteSetFileName (fName)
        End If
	Rem Имя последнего обработанного файла с обновлением.
	If 0 = BadResult Then
		LastUpdateFileName = stteGetValue ("LastUpdateFileName")
		If "" = LastUpdateFileName Then
			guiDisplayMessage MSGERROR, langStr (10), AppName
			BadResult = -1
		ElseIf 0 = fileValidFileNameMMDD (LastUpdateFileName, PMMDD) Then
			guiDisplayMessage MSGERROR, textSubstitute (langStr (11), LastUpdateFileName + "^" + UpdFileMask), AppName
			BadResult = -1
		End If
	End If
        Rem Журнал работы системы.
        fName = DetermineAbsolutePath (SavedCurDir, Trim (stngGetValue ("Files.Journal")))
	If "HTTP://" <> UCase (Left (fName, 7)) AndAlso 0 = fileAccessibleRW (fName) Then
        	guiDisplayMessage MSGERROR, textSubstitute (langStr(6), fName), AppName
                BadResult = -1
	End If
	If 0 = BadResult Then
        	Rem Подготовка к работе с журналом.
        	jrnlOpen (fName)
		jrnlTitle Station + " (" + TodayMMDD + ")"
        End If
        Rem Целевая папка.
	WTD.TargetFolder = fileDetermineAbsolutePath (SavedCurDir, Trim (stngGetValue ("Folders.Target")))
        If 0 = fileFolderAccessibleRW (WTD.TargetFolder) Then
        	guiDisplayMessage MSGERROR, textSubstitute (langStr(7), WTD.TargetFolder), AppName
                BadResult = -1
        Else
        	Rem Установка режима доступа к целевой папке по умолчанию.
        	Dim DefaultAccess As String
        	DefaultAccess = stngGetValue ("Guard.Default")
        	If "Allow" = DefaultAccess Then 
        		grdSetDefaultDecision (-1)
        	ElseIf "Deny" = DefaultAccess Then
        		grdSetDefaultDecision (0)
        	Else
        		Rem Неправильное значение режима доступа по умолчанию.
        		guiDisplayMessage MSGERROR, textSubstitute (langStr (26), DefaultAccess), AppName
        		BadResult = -1
        	End If
        	Rem Подготовка защиты целевой папки.
        	If 0 = BadResult Then
        		If 0 = grdPrepare Then
        			guiDisplayMessage MSGERROR, "Can't prepare the guard of the target folder.", AppName
        			BadResult = -1
        		End If
        	End If
        End If
        Rem Временная папка.
        WTD.TempFolder = fileDetermineAbsolutePath (SavedCurDir, Trim (stngGetValue ("Folders.Temp")))
        If 0 = fileFolderAccessibleRW (WTD.TempFolder) Then
        	guiDisplayMessage MSGERROR, textSubstitute (langStr(7), WTD.TempFolder), AppName
        	BadResult = -1
        End If
        Rem Локальная папка файлов с обновлениями.
        WTD.CacheFolder = fileDetermineAbsolutePath (SavedCurDir, Trim (stngGetValue ("Folders.Cache")))
        If 0 = fileFolderAccessibleRW (WTD.CacheFolder) Then
        	guiDisplayMessage MSGERROR, textSubstitute (langStr(7), WTD.CacheFolder), AppName
        	BadResult = -1
        End If
        Rem Эталонная папка файлов с обновлениями.
        WTD.SourceFolder = DetermineAbsolutePath (SavedCurDir, Trim (stngGetValue ("Folders.Source")))
End If
Rem --------------------------------------------------------------
Rem Синхронизация обновлений в локальной папке с эталонной папкой.
Rem --------------------------------------------------------------
If 0 = BadResult Then
	Dim CacheFiles(Any) As String, CacheFilesOrder(Any) As Integer, _
		SourceFiles(Any) As String, SourceFilesOrder(Any) As Integer, _
		i As Integer, ilb As Integer, iub As Integer, _
		j As Integer, jlb As Integer, jub As Integer, _
		SourceFile As String, CacheFile As String, c As Integer
	Rem Составление упорядоченного списка файлов в эталонной папке.
	If "" <> WTD.SourceFolder Then
		If "HTTP://" <> UCase (Left (WTD.SourceFolder, 7)) Then
			If -1 = fileFolderAccessibleR (WTD.SourceFolder) Then
				fileFillArrayWithFileNames WTD.SourceFolder + "\" + UpdFileMask, SourceFiles()
			Else
				fileFillArrayWithFileNames WTD.CacheFolder + "\" + UpdFileMask, SourceFiles ()
		        	Rem Работа будет продолжена без синхронизации с эталонной папкой.
		        	guiDisplayMessage MSGWARN, textSubstitute (langStr (8), WTD.SourceFolder), AppName
			End If
		Else
			Rem На этом этапе неявно происходит инициализация подсистемы работы с сетью.
			If -1 <> FillArrayWithFileNamesFromWebServer (UserAgent, WTD.SourceFolder, UpdFileMask, SourceFiles()) Then
				Rem Работа будет продолжена без синхронизации с эталонной папкой.
				guiDisplayMessage MSGWARN, textSubstitute (langStr (27), WTD.SourceFolder), AppName
				fileFillArrayWithFileNames WTD.CacheFolder + "\" + UpdFileMask, SourceFiles ()
			End If
		End If
	Else
		Rem Программа настроена на работу без использования эталонной папки.
		fileFillArrayWithFileNames WTD.CacheFolder + "\" + UpdFileMask, SourceFiles ()
	End If
	fileIndexingArray SourceFiles(), SourceFilesOrder(), @fileCompareFileNamesMMDD
	ilb = LBound (SourceFilesOrder)
	iub = UBound (SourceFilesOrder)
	Rem Составление упорядоченного списка файлов в локальной папке.
	fileFillArrayWithFileNames WTD.CacheFolder + "\" + UpdFileMask, CacheFiles ()
	fileIndexingArray CacheFiles(), CacheFilesOrder(), @fileCompareFileNamesMMDD
	jlb = LBound (CacheFilesOrder)
	jub = UBound (CacheFilesOrder)
	Rem Пропуск в эталонной папке файлов, которые уже были установлены.
	i = ilb
	While i <= iub AndAlso 0 >= fileCompareFileNamesMMDD (SourceFiles(SourceFilesOrder(i)), LastUpdateFileName)
		i += 1
	Wend
	Rem Удаление из локальной папки файлов, которые уже были установлены.
	j = jlb
	While j <= jub AndAlso 0 >= fileCompareFileNamesMMDD (CacheFiles(CacheFilesOrder(j)), LastUpdateFileName)
		If -1 <> UpdToRemove.Store (CacheFiles(CacheFilesOrder(j))) Then
			BadResult = 0
		End If
		j += 1
	Wend
	Rem Синхронизация локальной папки с эталонной.
	While 0 = BadResult AndAlso i <= iub AndAlso j <= jub
		Dim SourceFile As String, CachedFile As String
		SourceFile = SourceFiles(SourceFilesOrder(i))
		CacheFile = CacheFiles(CacheFilesOrder(j))
		c = fileCompareFileNamesMMDD (SourceFile, CacheFile)
		If 0 < c Then
			Rem CacheFile < SourceFile.
			Rem Надо удалить из локальной папки файл, которого нет в эталонной.
			BadResult = -1 <> UpdToRemove.Store (CacheFile)
			j += 1
		ElseIf 0 > c Then
			Rem CacheFile > SourceFile.
			Rem Надо получить из эталонной папки файл, которого нет в локальной.
			BadResult = -1 <> UpdToRetrieve.Store (SourceFile)
			Rem Надо ли устанавливать этот файл?
			If 0 = BadResult AndAlso 0 >= fileTimePointMMDD (TodayMMDD, Mid (SourceFile, PMMDD, 4), MTHSFWD) Then
				Rem Обработка файла с обновлением.
				BadResult = -1 <> UpdToInstall.Store (SourceFile)
			End If
			i += 1
		Else
			Rem CacheFile = SourceFile.
			Rem Надо ли устанавливать этот файл?
			If 0 = BadResult AndAlso 0 >= fileTimePointMMDD (TodayMMDD, Mid (CacheFile, PMMDD, 4), MTHSFWD) Then
				Rem Обработка файла с обновлением.
				BadResult = -1 <> UpdToInstall.Store (CacheFile)
			End If
			i += 1
			j += 1
		End If
	Wend
	Rem Получение из эталонной папки файлов, которых нет в локальной.
	While 0 = BadResult AndAlso i <= iub
		SourceFile = SourceFiles(SourceFilesOrder(i))
		BadResult = -1 <> UpdToRetrieve.Store (SourceFile)
		Rem Надо ли устанавливать этот файл?
		If 0 = BadResult AndAlso 0 >= fileTimePointMMDD (TodayMMDD, Mid (SourceFile, PMMDD, 4), MTHSFWD) Then
			Rem Обработка файла с обновлением.
			BadResult = -1 <> UpdToInstall.Store (SourceFile)
		End If
		i += 1
	Wend
	Rem Удаление из докальной папки файлов, которых нет в эталонной.
	While 0 = BadResult AndAlso j <= jub
		CacheFile = CacheFiles(CacheFilesOrder(j))
		BadResult = -1 <> UpdToRemove.Store (CacheFile)
		j += 1
	Wend
End If
Rem ----------------------------------------------------------------
Rem Выполнение задач, связанных с обновлением, если таковые нашлись.
Rem ----------------------------------------------------------------
If 0 = BadResult AndAlso 0 < UpdToRemove.Count + UpdToRetrieve.Count + UpdToInstall.Count Then
	Rem Подготовка списков задач для потока основной работы.
	StringDataToArray UpdToRemove, WTD.UpdToRemove()
	UpdToRemove.Wipe
	StringDataToArray UpdToRetrieve, WTD.UpdToRetrieve()
	UpdToRetrieve.Wipe
	StringDataToArray UpdToInstall, WTD.UpdToInstall()
	UpdToInstall.Wipe
	Rem Создание окна приложения.
	guiCreateMainWindow AppName, Station
	Rem Запуск потока для выполнения основной работы.
	If 0 = BadResult Then
		WorkThreadHandle = ThreadCreate (@workThread, @WTD)
	End If
	Rem Уведомление интерфейсной части о начале работ по обновлению.
	guiNotifyWorkBegins
	Rem Ожидание завершения потока, в котором выполняются задачи обновления.
	ThreadWait (WorkThreadHandle)
	BadResult = WTD.BadResult
End If
Rem ----------------------------------------------------------
Rem Информирование рабочего окружения о завершении работы,
Rem которая не допускала одновременного выполнения нескольких
Rem экземпляров приложения и восстановление текущего каталога.
Rem ----------------------------------------------------------
guiExclusiveWorkFinish
httpDone
ChDir SavedCurDir
Rem -----------------------------------------------------------
Rem Запуск приложения в случае успешного завершения обновления.
Rem -----------------------------------------------------------
If 0 = BadResult Then
	Dim RunAppCmd As String, AppProgram As String, AppArguments As String, p As Integer
	Rem Формирование команды для запуска приложения.
	RunAppCmd = stngGetValue ("Commands.RunApp")
	If "" <> RunAppCmd Then
		p = InStrRev (RunAppCmd, "^")
		If 0 < p Then
			AppProgram = Left (RunAppCmd, p - 1)
			AppArguments = Mid (RunAppCmd, 1 + p)
		Else
			AppProgram = RunAppCmd
			AppArguments = ""
		End If
		Rem Передача управления приложению.
		BadResult = Run (AppProgram, AppArguments)
	End If
End If
End BadResult