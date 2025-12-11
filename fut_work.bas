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
Rem Модуль, реализующий основную функцию программы FUT.
Rem Язык программирования: FreeBASIC.
Rem ===================================================
#include once "file.bi"
#include once "fut_work.bai"
#include once "fut_gui.bai"
#include once "fut_stte.bai"
#include once "fut_file.bai"
#include once "fut_jrnl.bai"
#include once "fut_text.bai"
#include once "fut_lang.bai"
Dim Shared Description As String
Dim Shared ErrorDetails As String
Rem ============================================
Rem Печать сообщения перед выполнением действия.
Rem Вызов: s - текст сообщения.
Rem ============================================
Private Sub workInformBefore (ByRef s As Const String)
	Description = s
	guiPrintLn s, 0
End Sub
Rem ==================================================
Rem Печать сообщения о результате выполнения действия.
Rem Вызов: r - результат выполнения действия:
Rem            -1 - успешно, 0 - неудачно.
Rem ==================================================
Private Sub workInformAfter (ByVal r As Integer)
	guiPrintLn Iif (r, langStr (13), langStr (14)), -1
	jrnlWrite jrnlMsgOperation (Description, r)
End Sub
Rem ==============================
Rem Печать сообщения прямо сейчас.
Rem Вызов: s - текст сообщения.
Rem ==============================
Private Sub workInformJustNow (ByRef s As Const String)
	guiPrintLn s, -1
	jrnlWrite s
End Sub
Rem ========================================================
Rem Установка обновления.
Rem Вызов: FullUpdFileName - полное имя файла с обновлением,
Rem        TempFolder - путь к папке для временных файлов,
Rem        TargetFolder - путь к целевой папке,
Rem        UnpackCmd - команда распаковки.
Rem Возврат: -1 - обновление успешно установлено,
Rem           0 - ошибка установки обновления.
Rem ========================================================
Private Function InstallUpdate (ByRef FullUpdFileName As Const String, ByRef TempFolder As Const String, _
	ByRef TargetFolder As Const String, ByRef UnpackProgram As Const String, _
	ByRef UnpackArguments As Const String) As Integer
	Dim TempFolderName As String, CurDirSave As String, _
		p As Integer, res As Integer, secres As Integer
	Rem Создание временной папки.
	TempFolderName = TempFolder + "\" + fileBaseNameOnly (FullUpdFileName) + ".tmp"
	res = 0 = MkDir (TempFolderName)
	If 0 = res Then
		Rem Произошла ошибка создания временной папки.
		ErrorDetails = textSubstitute (langStr(21), TempFolderName)
	Else
		Rem Распаковка обновления во временную папку.
		CurDirSave = CurDir
		ChDir TempFolderName
		Rem - workInformBefore UnpackProgram + " " + UnpackArguments
		res = 0 = Iif ("" <> UnpackArguments, _
			Exec (UnpackProgram, UnpackArguments + " " + FullUpdFileName), _
			Exec (UnpackProgram, FullUpdFileName))
		Rem - workInformAfter res
		ChDir CurDirSave
		If 0 = res Then
			Rem Произошла ошибка распаковки.
			ErrorDetails = langStr (22)
		End If
		If -1 = res Then
			Rem Проверка соответствия условиям защиты целевой папки.
			res = fileTargetFolderGuard (TempFolderName)
			If 0 = res Then
				Rem Обнаружено нарушение защиты целевой папки
				ErrorDetails = langStr (23)
			End If
		End If
		If -1 = res Then
			Rem Копирование файлов из временной папки в целевую.
			Rem - workInformBefore "Copy contents: " + TempFolderName + " -> " + TargetFolder
			res = fileCopyFolderContents (TempFolderName, TargetFolder)
			Rem -workInformAfter res
			If 0 = res Then
				Rem Произошла ошибка копирования файлов.
				ErrorDetails = langStr (24)
			End If
		End If
		Rem Удаление временной папки со всем её содержимым.
		Rem - workInformBefore "Delete folder: " + TempFolderName
		secres = fileKillFolder (TempFolderName)
		Rem - workInformAfter secres
		Rem Фиксация ошибки удаления временной папки.
		if -1 <> secres  Then
			ErrorDetails = langStr (25)
			res = 0
		End If
	End If
	InstallUpdate = res
End Function
Rem =====================================================
Rem Процедура с реализацией основной функции программы,
Rem предназначенная для работы в отдельном потоке.
Rem Вызов: UserData - указатель на дополнительные данные.
Rem =====================================================
Public Sub workThread (ByVal UserData As Any Ptr)
	Dim PWTD As workThreadData Pointer = UserData, n As Integer, _
		lb1 As Integer, ub1 As Integer, lb2 As Integer, ub2 As Integer, _
		i As Integer, j As Integer, FileName As String, _
		FFN1 As String, FFN2 As String, res As Integer, secres As Integer
	n = UBound (PWTD->UpdToRemove) - LBound (PWTD->UpdToRemove) + 1 + _
		UBound (PWTD->UpdToRetrieve) - LBound (PWTD->UpdToRetrieve) + 1 + _
		UBound (PWTD->UpdToInstall) - LBound (PWTD->UpdToInstall) + 1
	res = -1 
	Rem ---------------------------------------------------
	Rem Ожидание готовности потока интерфейса пользователя.
	Rem ---------------------------------------------------
	guiWaitForReady
	Rem -----------------------------
	Rem Удаление ненужных обновлений.
	Rem -----------------------------
	lb1 = LBound (PWTD->UpdToRemove)
	ub1 = UBound (PWTD->UpdToRemove)
	i = lb1
	While -1 = res AndAlso i <= ub1
		FileName = PWTD->UpdToRemove(i)
		FFN1 = PWTD->CacheFolder + "\" + FileName
		workInformBefore textSubstitute (langStr (15), FFN1)
		res = 0 = Kill (FFN1)
		workInformAfter res
		Rem Ошибка удаления старых обновлений не является критической,
		Rem но ошибка удаления будущих обновлений, которых нет в источнике,
		Rem является критической. Поэтому считаем любую ошибку удаления
		Rem критической.
		i += 1
	Wend
	Rem ---------------------
	Rem Установка обновлений.
	Rem ---------------------
	lb1 = LBound (PWTD->UpdToInstall)
	ub1 = UBound (PWTD->UpdToInstall)
	lb2 = LBound (PWTD->UpdToRetrieve)
	ub2 = UBound (PWTD->UpdToRetrieve)
	i = lb1
	j = lb2
	While -1 = res AndAlso i <= ub1
		FileName = PWTD->UpdToInstall(i)
		Rem Надо ли получать обновление?
		If j <= ub2 AndAlso FileName = PWTD->UpdToRetrieve(j) Then
			FFN1 = PWTD->SourceFolder + "\" + FileName
			FFN2 = PWTD->CacheFolder + "\" + FileName
			workInformBefore textSubstitute (langStr (16), FFN1 + "^" + FFN2)
			res = 0 = FileCopy (FFN1, FFN2)
			workInformAfter res
			j += 1
		End If
		Rem Установка обновления.
		If -1 = res Then
			FFN1 = PWTD->CacheFolder + "\" + FileName
			workInformBefore textSubstitute (langStr (17), FFN1)
			res = InstallUpdate (FFN1, PWTD->TempFolder, PWTD->TargetFolder, PWTD->UnpackProgram, PWTD->UnpackArguments)
			workInformAfter res
			If -1 = res Then
				Rem Фиксирование нового состояния системы.
				Rem Это, пожалуй, самая важная операция.
				res = stteSetValue ("LastUpdateFileName", FileName)
				If 0 = res Then
					workInformJustNow langStr (18)
				End If
				Rem Удаление файла установленного обновления.
				Rem Эта операция не является критичной.
				workInformBefore textSubstitute (langStr(15), FFN1)
				secres = 0 = Kill (FFN1)
				workInformAfter secres
			Else
				Rem Детализация причины ошибки обновления, если таковая имеется.
				If "" <> ErrorDetails Then
					workInformJustNow "!!! " + ErrorDetails
				End If
				Rem В случае неудачи установки обновления файл не удаляется,
				Rem чтобы можно было расследовать причину: не в нём ли дело?
			End If
			i += 1
		End If
	Wend
	Rem ---------------------------------------------
	Rem Получение обновлений для установки в будущем.
	Rem ---------------------------------------------
	While -1 = res AndAlso j <= ub2
		FileName = PWTD->UpdToRetrieve(j)
		FFN1 = PWTD->SourceFolder + "\" + FileName
		FFN2 = PWTD->CacheFolder + "\" + FileName
		workInformBefore textSubstitute (langStr (16), FFN1 + "^" + FFN2)
		res = 0 = FileCopy (FFN1, FFN2)
		workInformAfter res
		j += 1
	Wend
	Rem ----------------------------------------------------
	Rem Завершение работы с журналом, пока работа приложения
	Rem не заблокирована ожиданием реакции пользователя.
	Rem ----------------------------------------------------
	guiPrintLn langStr (19) + Iif (-1 = res, langStr (13), langStr (14)), -1
	jrnlFooter res
	jrnlClose
	Rem ----------------------------------------
	Rem Сообщение о результате выполнения задач.
	Rem ----------------------------------------
	guiNotifyWorkDone
	PWTD->BadResult = -1 <> res
End Sub