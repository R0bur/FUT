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
Rem ======================================================================
Rem Модуль обмена данными с сервером по протоколу HTTP.
Rem Язык программирования: FreeBASIC.
Rem Реализация выполнена по мотивам примера examples\network\http-get.bas.
Rem ======================================================================
#ifdef __FB_WIN32__
#include once "win/winsock2.bi"
#else
Rem ...
#endif
#include once "fut_data.bai"
#include once "fut_file.bai"
Const EOL As String = !"\r\n", BUFSIZE As Integer = 8193
Dim Shared NetworkReady As Integer, Socket As SOCKET, _
	Buf As ZString * BUFSIZE, UserAgent As String, _
	SrvName As String, SrvAddr As ULong, SrvPort As UShort, _
	HttpCode As Integer, HttpFileSize As Integer
Rem ======================================
Rem Подключение к HTTP-серверу.
Rem Возврат: -1 - подключение установлено,
Rem           0 - ошибка подключения.
Rem Примечание. Используются глобальные
Rem разделяемые переменные: Socket,
Rem SrvAddr, SrvPort.
Rem ======================================
Private Function httpConnect As Integer
	Dim res As Integer, SockAddr As sockaddr_in
	If 0 <> SrvAddr Then
		Rem Стандартная функция socket в FreeBASIC доступна под именем opensocket.
		Socket = opensocket (AF_INET, SOCK_STREAM, IPPROTO_TCP)
		If 0 <> Socket Then
			SockAddr.sin_port = htons (SrvPort)
			SockAddr.sin_family = AF_INET
			SockAddr.sin_addr.S_addr = SrvAddr
			If SOCKET_ERROR <> connect (Socket, CPtr (PSOCKADDR, @SockAddr), sizeof (SockAddr)) Then
				res = -1
			Else
				closesocket (Socket)
			End If
		End If
	End If
	httpConnect = res
End Function
Rem ======================================
Rem Отключение от HTTP-сервера.
Rem Возврат: -1 - подключение установлено,
Rem           0 - ошибка подключения.
Rem Примечание. Используются глобальная
Rem разделяемая переменная: Socket.
Rem ======================================
Private Sub httpDisconnect
	If 0 <> Socket Then
		shutdown (Socket, SD_BOTH)
		closesocket (Socket)
		Socket = 0
	End If
End Sub
Rem ======================================================
Rem Получение значения параметра из строки HTTP-заголовка.
Rem Вызов: S - строка HTTP-заголовка,
Rem Возврат: P - имя параметра, V - значение параметра.
Rem ======================================================
Private Sub httpHeaderParamValue (ByRef S As Const String, ByRef P As String, ByRef V As String)
	Dim t As Integer
	P = ""
	V = ""
	t = InStr (S, Any ": ")
	If 1 < t Then
		If ":" = Mid(S, t, 1) Then
			P = UCase (Left (S, t - 1))
			V = Trim (Mid (S, t + 1))
		ElseIf "HTTP/" = UCase (Left (S, 5)) Then
			P = "HTTP"
			V = Mid (S, 6)
		End If
	End If
End Sub
Rem ===============================================================
Rem Обработка заголовка ответа HTTP-сервера.
Rem Ожидается, что заголовок ответа HTTP-сервера
Rem находится в буфере Buf.
Rem Возврат: номер позиции в Buf, с которой начинается тело ответа,
Rem          начиная с единицы, или 0 в случае ошибки.
Rem ===============================================================
Private Function httpHeaderParse As Integer
	Dim res As Integer, p1 As Integer, p2 As Integer, HeaderLine As String, _
		Param As String, Value As String, i As Integer, LEOL As Integer
	LEOL = Len (EOL)
	p1 = 1
	p2 = InStr (p1, Buf, EOL)
	While p1 < p2
		i += 1
		HeaderLine = Trim (Mid (Buf, p1, p2 - p1))
		httpHeaderParamValue (HeaderLine, Param, Value)
		Select Case Param
		Case "HTTP"		' Код ответа сервера.
			p1 = InStr (6, HeaderLine, " ")
			p2 = InStr (p1 + 1, HeaderLine, " ")
			If p1 < p2 Then
				HttpCode = ValInt (Mid (HeaderLine, p1 + 1, p2 - p1 - 1))
			End If
		Case "CONTENT-LENGTH"	' Размер файла.
			HttpFileSize = ValInt (Value)
		End Select
		p1 = p2 + LEOL
		p2 = InStr (p1, Buf, EOL)
	Wend
	If p1 = p2 Then
		res = p1 + LEOL
	Else
		res = 0
	End If
	httpHeaderParse = res
End Function
Rem ====================================
Rem Преобразование адреса IPv4 в строку.
Rem Вызов: ai - адрес IPv4.
Rem Возврат: строковая нотация адреса.
Rem ====================================
Private Function debugAddrIpv4ToStr (ByVal ai As Integer) As String
	Dim au(3) As UByte, i As Integer
	For i = 0 To 3
		au(i) = ai Mod 256
		ai \= 256
	Next i
	debugAddrIpv4ToStr = Str (au(0)) + "." + Str (au(1)) + "." + Str (au(2)) + "." + Str (au(3))
End Function
Rem =====================================================================
Rem Переключение на работу с другим HTTP-сервером.
Rem Вызов: Host - доменное имя сервера или его адрес в нотации с точками,
Rem        Port - номер TCP-порта сервера.
Rem Возврат: -1 - успешное переключение,
Rem           0 - ошибка переключения.
Rem Примечание. Используются глобальные разделяемые переменные:
Rem             SrvName, SrvAddr и SrvPort.
Rem =====================================================================
Public Function httpSwitchServer (ByRef Host As Const String, ByVal Port As UShort) As Integer
	Dim res As Integer, ipv4a As in_addr, he As hostent Pointer
	res = NetworkReady
	If -1 = res Then
		SrvAddr = 0
		SrvName = ""
		SrvPort = 0
		Rem Разрешение доменного имени в сетевой адрес.
		ipv4a.S_addr = inet_addr (Host)
		If INADDR_NONE <> ipv4a.S_addr Then
			SrvAddr = ipv4a.S_addr
		Else
			he = gethostbyname (Host)
			If NULL <> he AndAlso NULL <> *he->h_addr_list Then
				SrvAddr = *CPtr (ULong Pointer, *he -> h_addr_list)
			End If
		End If
		Rem Завершение переключения.
		If 0 <> SrvAddr Then
			SrvName = Host
			SrvPort = Port
		Else
			res = 0
		End If
	End If
	httpSwitchServer = res
End Function
Rem =========================================================================
Rem Подготовка к работе с HTTP-сервером.
Rem Вызов: Browser - строка с идентификатором пользовательского агента,
Rem        HostName - доменное имя сервера или его адрес в нотации с точками,
Rem        Port - TCP-порт сервера.
Rem Возврат: -1 - подготовка к работе выполнена,
Rem           0 - ошибка подготовки к работе.
Rem Примечание. Используются глобальные разделяемые переменные:
Rem             UserAgent.
Rem =========================================================================
Public Function httpInit (ByRef Browser As Const String, ByRef Host As Const String, ByVal Port As UShort) As Integer
	Dim res As Integer
	Rem Инициализация сетевой подсистемы.
	If 0 = NetworkReady Then
		#ifdef __FB_WIN32__
		Dim d As WSAData
		res = 0 = WSAStartup (MAKEWORD (1, 1), @d)
		#else
		res = -1
		#endif
		NetworkReady = res
	EndIf
	Rem Настройка на работу с указанным сервером.
	If -1 = res Then
		UserAgent = Browser
		res = httpSwitchServer (Host, Port)
	End If
	httpInit = res
End Function
Rem =============================================
Rem Кодирование текста в соответствии с методом
Rem application/x-www-form-urlencoded, который
Rem описан в спецификации HTML.
Rem Вызов: s1 - текст для кодирования.
Rem Возврат: закодированный текст.
Rem Не подлежат преобразованию следующие символы:
Rem буквы, цифры, "-._~".
Rem =============================================
Public Function httpUrlEncode (ByRef s1 As Const String) As String
	Dim s2 As String, p1 As Integer, p2 As Integer, l1 As Integer, l2 As Integer, _
		c1 As String*1
	l1 = Len (s1)
	s2 = Space (l1 + 2) ' буфера с резервом для представления символа кодом
	l2 = l1
	p2 = 1
	For p1 = 1 To l1
		If l2 < p2 Then
			s2 += Space (l1)
			l2 = l2 + l1
		End If
		c1 = Mid (s1, p1, 1)
		If ("A" <= c1 AndAlso c1 <= "Z") OrElse ("a" <= c1 AndAlso c1 <= "z") OrElse _
			("0" <= c1 AndAlso c1 <= "9") OrElse 0 < InStr (c1, Any "-._~") Then
			Mid (s2, p2, 1) = c1
			p2 += 1
		ElseIf " " = c1 Then
			Mid (s2, p2, 1) = "+"
			p2 += 1
		Else
			Mid (s2, p2, 3) = "%" + Hex (Asc (c1), 2)
			p2 += 3
		EndIf
	Next p1
	httpUrlEncode = Left (s2, p2 - 1)
End Function
Rem =============================================================
Rem Получение текста от HTTP-сервера.
Rem Вызов: Path - путь к ресурсу на HTTP-сервере,
Rem        MaxLen - максимальная длина текста.
Rem Возврат: Text - строка с текстом, полученным от HTTP-сервера.
Rem          -1 - текст получен успешно,
Rem           0 - ошибка получения текста.
Rem Примечание. Используются глобальные разделяемые переменные.
Rem =============================================================
Public Function httpGetText (ByRef Path As Const String, ByRef Text As String, ByVal MaxLen As Integer) As Integer
	Dim res As Integer,  Request As String, p As Integer, n As Integer, m As Integer
	Rem Подключение к серверу.
	res = httpConnect
	Rem Отправка запроса к серверу.
	If -1 = res Then
		Request = "GET " + Path + " HTTP/1.0" + EOL + _
			"Host: " + SrvName + ":" + Str (SrvPort) + EOL + _
			"Accept: text/html,*/*" + EOL + _
			"Connection: Close" + EOL + _
			"User-Agent: " + UserAgent + EOL + _
			EOL
		If SOCKET_ERROR = send (Socket, StrPtr (Request), Len (Request), 0) Then
			res = 0
		End If
	End If
	Rem Получение ответа от сервера, который может поступать несколькими порциями.
	If -1 = res Then
		Mid (buf, BUFSIZE, 1) = Chr (0)
		Rem p - индекс первого свободного символа в массиве, начинается с нуля.
		p = 0
		m = Iif (BUFSIZE - 1 < MaxLen, BUFSIZE - 1, MaxLen)
		Do 
			n = recv (Socket, @buf[p], m, 0)
			If SOCKET_ERROR <> n Then
				m -= n
				p += n
				Rem Mid(buf, p + 1, 1) = Chr (0)
				buf[p] = 0
			Else
				res = 0
			End If
		Loop Until 0 = res OrElse 0 = n OrElse 0 = m
		Rem Вычисление суммарного объема полученной информации.
		n = p
	End If
	Rem Обработка заголовка ответа сервера.
	If -1 = res Then
		p = httpHeaderParse
		If 0 = p OrElse 200 <> HttpCode Then
			res = 0
		End If
	End If
	Rem Обработка тела ответа сервера.
	If -1 = res Then
		Rem Смещение p к началу тела ответа отсчитывается от начала строки с единицы.
		Text = Mid (buf, p)
	End If
	Rem Отключение от сервера.
	httpDisconnect
	httpGetText = res
End Function
Rem ================================================
Rem Загрузка файла с сервера.
Rem Вызов: SrvFileName - имя файла на сервере,
Rem        LocalFileName - имя файла для сохранения.
Rem Возврат: -1 - файл успешно загружен,
Rem           0 - ошибка загрузки файла.
Rem ================================================
Public Function httpDownloadFile (ByRef SrvFileName As Const String, ByRef LocalFileName As Const String) As Integer
	Dim res As Integer, Request As String, n As Integer, p As Integer, fn As Integer, cnt As Integer
	Rem Подключение к серверу.
	res = httpConnect
	Rem Отправка запроса к серверу.
	If -1 = res Then
		Request = "GET " + SrvFileName + " HTTP/1.0" + EOL + _
		"Host: " + SrvName + ":" + Str (SrvPort) + EOL + _
		"Accept: text/html,*/*" + EOL + _
		"User-Agent: " + UserAgent + EOL + _
		EOL
		If SOCKET_ERROR = send (Socket, StrPtr (Request), Len (Request), 0) Then
			res = 0
		End If
	End If
	Rem Получение первого ответа от сервера, который содержит HTTP-заголовок.
	If -1 = res Then
		Mid (buf, BUFSIZE, 1) = Chr (0)
		n = recv (Socket, StrPtr (buf), BUFSIZE - 1, 0)
		If SOCKET_ERROR = n Then
			res = 0
		End If
	End If
	Rem Обработка HTTP-заголовка.
	If -1 = res Then
		p = httpHeaderParse
		If 0 = p OrElse 200 <> HttpCode Then
			res = 0
		End If
	End If
	Rem Обработка тела ответа HTTP-сервера.
	If -1 = res Then
		Rem Подготовка к записи в файл информации, следующей после HTTP-заголовка.
		Rem В зависимости от веб-сервера, в первом ответе она может присутствовать или отсутствовать.
		Rem Например, веб-сервер PHP8 сначала отправляет заголовок, а потом тело ответа.
		Rem Веб-сервер Apache отправляет сразу и заголовок, и тело.
		Rem Смещение p к телу ответа отсчитывается от начала буфера-строки с единицы.
		n -= p - 1
		fn = FreeFile
		Open LocalFileName For Binary Access Write As #fn
		If 0 <> Err Then
			res = 0
		End If
		If -1 = Res Then
			Rem Запись в файл блоков, составляющих тело ответа на запрос файла.
			Rem Сначала p всегда больше единицы, а n может быть равно нулю.
			cnt = 0
			While -1 = res AndAlso (0 < n OrElse 1 < p)
				If 0 < n Then
					Rem При интерпретации буфера как массива надо вести отсчёт с нуля.
					Put #fn, , Buf[p - 1], n
					If 0 = Err Then
						cnt += n	' подсчёт количества записанных данных
					Else
						res = 0
					End If
				End If
				Rem Переключение на начало буфера-строки после обработки первого блока.
				p = 1
				Rem Получение от сервера оставшихся частей тела ответа.
				n = recv (Socket, StrPtr (buf), BUFSIZE - 1, 0)
				res = SOCKET_ERROR <> n
			Wend
			Close #fn
			Rem Если известен размер загружаемого файла, то проверка количества полученных данных.
			res = res AndAlso Iif (0 < HttpFileSize, cnt = HttpFileSize, -1)
		End If
	End If
	Rem Отключение от сервера.
	httpDisconnect
	httpDownloadFile = res
End Function
Rem ============================================
Rem Отправка текста на HTTP-сервер.
Rem Вызов: Path - путь к ресурсу сервера,
Rem        Text - текст, который надо отправить,
Rem        в формате form-urlencoded.
Rem Возврат: -1 - текст отправлен на сервер,
Rem           0 - ошибка отправки текста.
Rem ============================================
Public Function httpPostText (ByRef Path As Const String, ByRef Text As Const String) As Integer
	Dim res As Integer, Request As String, n As Integer, p As Integer
	Rem Подключение к серверу.
	res = httpConnect
	Rem Отправка запроса к серверу.
	If -1 = res Then
		Request = "POST " + Path + " HTTP/1.0" + EOL + _
		"Host: " + SrvName + ":" + Str (SrvPort) + EOL + _
		"Content-Type: application/x-www-form-urlencoded" + EOL + _
		"Content-Length: " + Str (Len (Text)) + EOL + _
		"User-Agent: " + UserAgent + EOL + _
		EOL + _
		Text
		If SOCKET_ERROR = send (Socket, StrPtr (Request), Len (Request), 0) Then
			res = 0
		End If
	End If
	Rem Получение подтверждения от сервера.
	Rem Ожидается что-то вроде:
	Rem HTTP/1.1 200 OK
	Rem Content-Type: text/html; charset=UTF-8
	Rem
	Rem <html><head><title>Success</title><body><p>Ok</p></body></head></html>
	If -1 = res Then
		Mid (buf, BUFSIZE, 1) = Chr (0)
		n = recv (Socket, StrPtr (buf), BUFSIZE - 1, 0)
		If SOCKET_ERROR = n Then
			res = 0
		End If
	End If
	Rem Обработка HTTP-заголовка.
	If -1 = res Then
		p = httpHeaderParse
		If 0 = p OrElse 200 <> HttpCode Then
			res = 0
		End If
	End If
	Rem Отключение от сервера.
	httpDisconnect
	httpPostText = res
End Function
Rem ==========================
Rem Завершение работы с сетью.
Rem ==========================
Public Sub httpDone
	If -1 = NetworkReady Then
		#ifdef __FB_WIN32__
		WSACleanup
		#endif
		NetworkReady = 0
	EndIf
End Sub
Rem ========================================================
Rem Определение имени сервера, пути к ресурсу и номера порта
Rem по адресу ресурса в формате "http://сервер:порт/путь".
Rem Вызов: Url - адрес ресурса,
Rem Возврат: Host - имя сервера,
Rem          Port - номер порта,
Rem          Path - путь к ресурсу.
Rem ========================================================
Public Sub httpParseUrl (ByRef Url As Const String, ByRef Host As String, ByRef Port As UShort, ByRef Path As String)
	Dim p1 As Integer, p2 As Integer, p3 As Integer
	p1 = InStr (Url, "://")
	p1 = Iif (0 < p1, p1 + 3, 1)
	p2 = InStr (p1, Url, ":")
	p3 = InStr (p1, Url, "/")
	if 0 = p3 Then p3 = 1 + Len (Url)
	if 0 = p2 Then p2 = p3
	Host = Mid (Url, p1, p2 - p1)
	Port = ValUInt (Mid (Url, p2 + 1, p3 - p2 - 1))
	If 0 = Port Then Port = 80
	Path = Mid (Url, p3)
	If "" = Path Then Path = "/"
End Sub
Rem ==================================================
Rem Заполнение массива именами файлов, извлечёнными из
Rem ссылок в HTML-документе.
Rem Вызов: Html - текст HTML-документа,
Rem        Mask - маска имён файлов.
Rem Возврат: ArrFileNames - массив с именами файлов.
Rem ==================================================
Public Sub httpFillArrayWithFileNames (ByRef Html As Const String, ByRef Mask As Const String, ArrFileNames() As String)
	Dim res As Integer, MaskMod As String, pmd As Integer, p1 As Integer, p2 As Integer, _
		FileName As String, FileNames As StringData = StringData (128, Chr (7)), _
		n As Integer, i As Integer
	Rem Модификация маски файлов.
	pmd = InStr (Mask, "####")
	MaskMod = Iif (0 < pmd, Left (Mask, pmd - 1) + "????" + Mid (Mask, 4 + pmd), Mask)
	Rem Извлечение имён файлов во временное хранилище.
	p1 = InStr (html, "href=""")
	While 0 < p1
		p1 += 6
		p2 = InStr (p1, html, """")
		If 0 < p2 Then
			FileName = Mid (html, p1, p2 - p1)
			If fileNameMatchMask (FileName, MaskMod) AndAlso _
				fileValidFileNameMMDD (FileName, pmd) Then
				FileNames.Store (FileName)
			End If
			p1 = InStr (p2 + 1, html, "href=""")
		Else
			p1 = 0
		End If
	Wend
	Rem Заполнение массива найденными именами файлов.
	n = FileNames.Count
	If 0 < n Then
		ReDim ArrFileNames (n - 1)
		For i = 0 To n - 1
			ArrFileNames(i) = FileNames.Read
		Next i
	Else
		Erase ArrFileNames
	End If
End Sub
