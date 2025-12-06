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
Rem ============================================
Rem Модуль графического интерфейса пользователя.
Rem Язык программирования: FreeBASIC.
Rem ============================================
#include once "windows.bi"
#include once "fut_gui.bai"
#include once "fut_lang.bai"
Common Shared hInstance As HINSTANCE
Const WM_WORKDONE As DWORD = WM_USER
Const TM_WORKDONE As DWORD = 0 + WM_APP
Const TM_PRINTLN As DWORD = 1 + WM_APP
Const IDC_EDBINFO As DWORD = 1
Const IDC_BTNNEXT As DWORD = 2
Dim Shared hMainWindow As HWND = NULL
Dim Shared IdGuiThread As DWORD = 0
Dim Shared WorkDone As Integer = 0
Enum HoldByThread
	HBT_WORK, HBT_GUI
End Enum
Dim Shared HbtPrintLn As HoldByThread
Dim Shared PrintLnS As String
Dim Shared NamedMutex As HANDLE		' мьютекс для защиты от запуска нескольких экземпляров
Rem =============================================
Rem Формирование "отпечатка" произвольной строки, 
Rem состоящего из "безопасных" символов.
Rem Вызов: s - обрабатываемая строка.
Rem Возврат: "отпечаток" обработанной строки.
Rem =============================================
Private Function SafeCharacters (ByRef s As Const String) As String
	Const t As String = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", _
		nt As Integer = Len (t)
	Dim res As String, ns As Integer, i As Integer
	res = s
	ns = Len (s)
	For i = 1 To ns
		Mid(res, i, 1) = Mid (t, 1 + Asc (s, i) Mod nt, 1)		
	Next i
	SafeCharacters = res
End Function
Rem ========================================================
Rem Обработка сообщения о создании главного окна приложения.
Rem Вызов: hWndParent - манипулятор главного окна,
Rem        cs - структура CREATESTRUCT.
Rem Возврат: 0 - сообщение обработано, можно создавать окно,
Rem         -1 - создавать окно нельзя.
Rem ========================================================
Private Function guiWmCreate (ByVal hWndParent As HWND, ByVal cs As CREATESTRUCT Ptr) As LRESULT
	Dim res As LRESULT, rc As RECT, w As Integer, h As Integer, hWnd1 As HWND, hWnd2 As HWND
	GetClientRect (hWndParent, @rc)
	w = rc.right - rc.left
	h = rc.bottom - rc.top
	hWnd1 = CreateWindow ("EDIT", NULL, WS_CHILD Or WS_VISIBLE Or WS_BORDER Or WS_VSCROLL Or ES_MULTILINE Or ES_LEFT Or ES_READONLY, _
		rc.left, rc.top, w, h - 38, hWndParent, CPtr (HMENU, IDC_EDBINFO), cs->hInstance, NULL)
	hWnd2 = CreateWindow ("BUTTON", langStr (20), WS_VISIBLE Or WS_CHILD Or WS_DISABLED Or BS_DEFPUSHBUTTON, (w - 72) / 2, h - 32, 72, 24, _
		hWndParent, CPtr (HMENU, IDC_BTNNEXT), cs->hInstance, NULL)
	guiWmCreate = res
End Function
Rem ======================================================
Rem Обработка сообщения с командой от элемента управления.
Rem Вызов: hWndParent - манипулятор главного окна,
Rem        Code - код оповещения,
Rem        Id - идентификатор отправителя,
Rem        hWndControl - манипулятор элемента управления.
Rem Возврат: 0 - сообщение обработано.
Rem ======================================================
Private Function guiWmCommand (ByVal hWndParent As HWND, ByVal Code As UShort, ByVal Id As UShort, hWndControl As HWND) As LRESULT
	Dim res As LRESULT
	If 0 = Code AndAlso IDC_BTNNEXT = Id Then
		DestroyWindow (hWndParent)
	End If
	guiWmCommand = res
End Function
Rem ===========================================
Rem Оконная процедура главного окна приложения.
Rem Вызов: hWnd - манипулятор главного окна,
Rem        uMsg - сообщение окну,
Rem        wParam - первый параметр сообщения,
Rem        lParam - второй параметр сообщения.
Rem Возврат: результат обработки сообщения.
Rem ===========================================
Private Function guiMainWndProc (ByVal hWnd As HWND, ByVal uMsg As UINT, ByVal wParam As WPARAM, ByVal lParam As LPARAM) As LRESULT
	Dim res As LRESULT, hBtnNext As HWND
	Select Case (uMsg)
	Case WM_CREATE		' Обработка сообщения о создании окна.
		res = guiWmCreate (hWnd, CPtr(CREATESTRUCT Ptr, lParam))
	Case WM_WORKDONE	' Обработка сообщения о завершении выполнения обновления
		hBtnNext = GetDlgItem (hMainWindow, IDC_BTNNEXT)
		EnableWindow hBtnNext, TRUE
		SetFocus hBtnNext
		BringWindowToTop hWnd
	Case WM_COMMAND
		res = guiWmCommand (hWnd, HiWord (wParam), LoWord (wParam), CPtr (HWND, lParam))
	Case WM_DESTROY		' Обработка запроса на уничтожение окна.
		hMainWindow = NULL
		PostQuitMessage (0)
	Case Else		' Обработка остальных сообщений.
		res = DefWindowProc (hWnd, uMsg, wParam, lParam)
	End Select
	guiMainWndProc = res
End Function
Rem =============================================================
Rem Запрос к рабочему окружению на выполнение работы, которая
Rem не допускает одновременного выполнения нескольких экземпляров
Rem приложения.
Rem Вызов: IniFileName - имя файла инициализации приложения.
Rem Возврат: -1 - выполнение работы разрешено,
Rem           0 - выполнение работы запрещено.
Rem Примечание: используется разделяемая переменная NamedMutex,
Rem в которой хранится манипулятор созданного мьютекса.
Rem =============================================================
Public Function guiExclusiveWorkStart (ByRef IniFileName As Const String) As Integer
	Dim res As Integer, e As DWORD, MutexName As String
	MutexName = SafeCharacters (IniFileName)
	NamedMutex = CreateMutex (NULL, FALSE, StrPtr (MutexName))
	e = GetLastError
	If NULL <> NamedMutex Then
		If 0 = e Then
			Rem Нет других выполняющихся экземпляров.
			res = -1
		Else
			Rem Есть другие выполняющиеся экземпляры.
			CloseHandle NamedMutex
		End If
	End If
	guiExclusiveWorkStart = res
End Function
Rem ======================================================
Rem Информирование рабочего окружения о завершении работы,
Rem которая не допускала одновременного выполнения
Rem нескольких экземпляров приложения.
Rem Используется разделяемая переменная NamedMutex, в
Rem которой хранится манипулятор мьютекса.
Rem ======================================================
Public Sub guiExclusiveWorkFinish
	If NULL <> NamedMutex Then
		CloseHandle NamedMutex
		NamedMutex = NULL
	End If
End Sub
Rem ============================
Rem Отображение сообщения.
Rem Вызов: text - текст,
Rem        title - заголовок,
Rem        category - категория.
Rem ============================
Public Sub guiDisplayMessage (ByVal category As MessageCategory, ByRef text As Const String, ByRef title As Const String)
	MessageBox hMainWindow, text, title, MB_OK Or _
        	Iif (category = MSGINFO, MB_ICONINFORMATION, _
                Iif (category = MSGWARN, MB_ICONEXCLAMATION, _
                	MB_ICONHAND))
End Sub
Rem =====================================
Rem Создание главного окна приложения.
Rem Вызов: AppName - название приложения,
Rem        Station - название компьютера.
Rem Возврат: -1 - окно создано,
Rem           0 - ошибка создания окна.
Rem =====================================
Public Function guiCreateMainWindow (ByRef AppName As Const String, ByRef Station As Const String) As Integer
	Dim res As Integer, wc As WNDCLASS, MainWindowClassName As String, MainWindowTitle As String
	MainWindowClassName = AppName + "MainWindowClass"
	MainWindowTitle = AppName + " (" + Station + ")"
	Rem Регистрация класса главного окна приложения.
	With wc
		.style = CS_HREDRAW Or CS_VREDRAW
		.lpfnWndProc = @guiMainWndProc
		.cbClsExtra = 0
		.cbWndExtra = 0
		.hInstance = hInstance
		.hIcon = LoadIcon (NULL, IDI_APPLICATION)
		.hIcon = LoadIcon (hInstance, MAKEINTRESOURCE (1))
		.hCursor = LoadCursor (NULL, IDC_ARROW)
		.hbrBackground = GetStockObject (WHITE_BRUSH)
		.lpszMenuName = NULL
		.lpszClassName = StrPtr (MainWindowClassName)
	End With
	res = FALSE <> RegisterClass (@wc)
	Rem Создание главного окна приложения.
	If -1 = res Then
		Dim R As RECT, x As Integer, y As Integer, w As Integer, h As Integer
		Rem Позиционирование окна на рабочем столе.
		GetClientRect (GetDesktopWindow, @R)
		w = R.right / 2
		h = R.bottom / 2
		x = R.right / 4
		y = R.bottom / 5
		Rem Создание окна.
		hMainWindow = CreateWindowEx (0, StrPtr (MainWindowClassName), StrPtr (MainWindowTitle), _
			WS_OVERLAPPED Or WS_SYSMENU, x, y, w, h, NULL, NULL, hInstance, NULL)
		res = NULL <> hMainWindow
	End If
	guiCreateMainWindow = res
End Function
Rem ============================================================
Rem Уведомление интерфейсной части о начале работ по обновлению.
Rem ============================================================
Public Sub guiNotifyWorkBegins
	Dim wMsg As MSG
	If NULL <> hMainWindow Then
		Rem Отображение главного окна приложения.
		ShowWindow hMainWindow, SW_NORMAL
		UpdateWindow hMainWindow
		Rem Подготовка к обработке сообщений из других потоков.
		IdGuiThread = GetCurrentThreadId
		Rem Запуск цикла обработки сообщений.
		While FALSE <> GetMessage (@wMsg, NULL, 0, 0)
			if NULL = wMsg.hwnd AndAlso TM_WORKDONE = wMsg.message Then
				Rem Обработка сообщения о завершении потока обновления.
				PostMessage hMainWindow, WM_WORKDONE, 0, 0
				WorkDone = -1
			ElseIf NULL = wMsg.hwnd AndAlso TM_PRINTLN = wMsg.message Then
				Rem Обработка запроса на печать строки.
				SendDlgItemMessage hMainWindow, IDC_EDBINFO, EM_REPLACESEL, wMsg.wParam, wMsg.lParam
				Rem Сообщение о готовности к отображению следующей строки.
				HbtPrintLn = HBT_WORK
			ElseIf hMainWindow = wMsg.hwnd AndAlso WM_CLOSE = wMsg.message Then
				Rem Обработка сообщения о завершении работы приложения.
				If WorkDone Then
					TranslateMessage (@wMsg)
					DispatchMessage (@wMsg)
				End If
			Else
				Rem Обработка всех остальных сообщений.
				TranslateMessage (@wMsg)
				DispatchMessage (@wMsg)
			End If
		Wend
	End If
End Sub
Rem ==========================================================================================
Rem Ожидание готовности потока графического интерфейса к отображению сообщений в главном окне.
Rem ==========================================================================================
Public Sub guiWaitForReady
	While 0 = IdGuiThread
		Sleep 25, 1
	Wend
End Sub
Rem ================================================================
Rem Уведомление интерфейсной части о завершении работ по обновлению.
Rem ================================================================
Public Sub guiNotifyWorkDone
	PostThreadMessage IdGuiThread, TM_WORKDONE, 0, 0
End Sub
Rem =====================================================
Rem Печать строки в главном окне приложения.
Rem Вызов: s - строка,
Rem        nl - 0 - без перевода каретки на новую строку,
Rem            -1 - с переводом каретки на новую строку.
Rem =====================================================
Public Sub guiPrintLn (ByRef s As Const String, ByVal nl As Integer)
	If 0 <> IdGuiThread Then
		Rem Ожидание готовности потока интерфейса к печати строки.
		While HBT_WORK <> HbtPrintLn
			Sleep 25, 1
		Wend
		Rem Формирование строки для печати в разделяемой потоками глобальной памяти.
		PrintLnS = Iif (nl, s + Chr(13) + Chr(10), s)
		Rem Передача управления потоку инерфейса.
		HbtPrintLn = HBT_GUI
		PostThreadMessage IdGuiThread, TM_PRINTLN, FALSE, CUInt (StrPtr (PrintLnS))
	Else
		guiDisplayMessage MSGWARN, "GUI thread isn't ready to process this request!", "guiPrintLn"
	End If
End Sub
