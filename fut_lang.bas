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
Rem =================================================
Rem Модуль работы со строками на национальных языках.
Rem Язык программирования: FreeBasic.
Rem =================================================
Dim Shared Language As Integer = 0, _
Languages(...) As Const ZString Ptr => {@"en", @"ru"}, _
S(..., ...) As Const ZString Ptr => { _
	{ _
	@"English language", _
	@!"Command line error.\n\nUsage: ^0 INI-file_full_name", _
	@!"Cannot open initialization file for reading:\n\n""^0""", _
	@!"Attempt to change already set parameter value or unexpected parameter found:\n\n^0: ""^1"" = ""^2""", _
	@!"Initialization file line parsing error:\n\n^0: ^1", _
	@!"Bad update file mask - template ""####"" for MMDD date missed:\n\n""^0""", _
	@!"Can't read-write access the file:\n\n""^0""", _
	@!"Can't read-write access the folder:\n\n""^0""", _
	@!"Can't read the folder:\n\n""^0""", _
	@!"Value of MMDD is set from DEBUG setting: ^0", _
	_
	@!"System state file doesn't contain information about the last update file name.", _
	@!"Last update file name ""^0"" doesn't match the mask: ""^1"".", _
	@!"Required setting isn't specified: ^0.", _
	@!" - Ok.", _
	@!" - Error!", _
	@!"Delete file: ""^0""", _
	@!"Retrieve file: ""^0""", _
	@!"Update from: ""^0""", _
	@!"!!! System state update error.", _
	@!"FINAL RESULT", _
	_	
	@!"&Next", _
	@!"Can't create folder: ""^0"".", _
	@!"Update file unpacking error.", _
	@!"Attempt to violate the target folder protection.", _
	@!"Update files copying error.", _
	@!"Temporary folder removing error.", _
	@!"Invalid value is specified for the default protection mode of the target folder: ""Guard.Default"" = ""^0"". It should be ""Allow"" or ""Deny"".", _
	@!"Can't retrieve list of updates from the network resource:\n\n^0", _
	@!"An error occurred while saving the update log!" _
	}, { _
	@"Русский язык", _
	@!"Ошибка в параметрах командной строки.\n\nИспользование: ^0 Полное_имя_INI-файла", _
	@!"Не удалось открыть файл инициализации для чтения:\n\n""^0""", _
	@!"Попытка изменения уже заданного значения параметра или встретился неизвестный параметр:\n\n^0: ""^1"" = ""^2""", _
	@!"Ошибка разбора строки файла инициализации:\n\n^0: ^1", _
	@!"Плохая маска файлов с обновлениями - отсутствует шаблон ""####"" для даты ММДД:\n\n""^0""", _
	@!"Нет доступа для чтения и записи к файлу:\n\n""^0""", _
	@!"Нет доступа для чтения и записи к папке:\n\n""^0""", _
	@!"Невозможно прочитать папку:\n\n""^0""", _
	@!"Значение ММДД установлено из ОТЛАДОЧНЫХ настроек: ^0", _
	_
	@!"Файл состояния системы не содержит сведений об имени файла последнего обновления.", _
	@!"Имя файла последнего обновления ""^0"" не соответствует маске: ""^1"".", _
	@!"Обязательная настройка не задана: ^0.", _
	@!" - Ок.", _
	@!" - Ошибка!", _
	@!"Удаление файла: ""^0""", _
	@!"Получение файла: ""^0""", _
	@!"Обновление из: ""^0""", _
	@!"!!! Ошибка обновления состояния системы.", _
	@!"ИТОГОВЫЙ РЕЗУЛЬТАТ", _
	_
	@!"&Далее", _
	@!"Не удалось создать папку: ""^0"".", _
	@!"Ошибка распаковки файла с обновлением.", _
	@!"Попытка нарушить защиту целевой папки.", _
	@!"Ошибка копирования файлов обновления.", _
	@!"Ошибка удаления временной папки.", _
	@!"Для режима защиты целевой папки по умолчанию указано неправильное значение: ""Guard.Default"" = ""^0"". Должно быть ""Allow"" или ""Deny"".", _
	@!"Не удалось получить список обновлений с сетевого ресурса:\n\n^0", _
	@!"При сохранении журнала обновлений произошла ошибка!" _
        } _
}
Rem =========================================================
Rem Установка национального языка.
Rem Вызов: v - буквенный код языка,
Rem Возврат: 1 - язык установлен, 0 - ошибка установки языка.
Rem =========================================================
Public Function langSetLanguage (ByRef v As Const String) As Integer
	Dim res As Integer = 0
        For i As Integer = LBound (Languages) To UBound (Languages)
        	If v = *Languages(i) Then
			Language = i
                        res = 1
                End If
        Next
	langSetLanguage = res
End Function
Rem =======================================
Rem Получение строки на национальном языке.
Rem Вызов: i - индекс строки в массиве.
Rem Возврат: строка на национальном языке.
Rem =======================================
Public Function langStr (i As Integer) As Const String
        langStr = IIf (LBound (S, 2) <= i AndAlso i <= UBound (S, 2), _
        	*S(Language, i), "<String not found>")
End Function