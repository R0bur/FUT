# File Update Tool

File Update Tool (FUT program) is an utility for automating the distribution
of files - components of interactive applications - to workstations. It was
developed using FreeBASIC-1.01.0-win32 compiler and is designed for use in
the Microsoft Windows operating system.

The author of the FUT program is Ihar S. Areshchankau, robursw@gmail.com

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

## DESCRIPTION

The FUT program retrieves archived update files from the server's _SOURCE_
folder into the workstation's _CACHE_ folder. It selects those that need to be
processed at the moment based on the current date and the last processed
update file name. The program processes each selected update: unpacking
the archive contents to the _TEMP_ folder and copying them to the _TARGET_ folder.

```
SOURCE\upd1110a.7z
     |  (SERVER)
    ... 
     |  (WORKSTATION, November, 10th)
     v
CACHE\upd1110a.7z ---> TEMP\UPD1110A.TMP\... ---> TARGET\...
```

The FUT program launches the updated application after the updates have been
successfully processed.

FUT program settings are stored in the _fut.ini_ file, the name of the last
processed update file is stored in the _state.txt_ file.

## COMPILATION

1. Install FreeBASIC-1.01.0-win32 compiler to the folder
   _C:\EXE\FreeBASIC-1.01.0-win32_

2. Create a new folder and copy FUT program source files into it.

3. Run the _fut-build.bat_ script from the FUT program folder. It will create
   the _fut.exe_ executable file.

## INSTALLATION

1. Place the _fut.exe_ and _fut.ini_ files somewhere on the workstation.

2. Adjust the settings in the _fut.ini_ file to match the environment on the
   workstation.

3. Create a _state.txt_ file and specify in it the file name of the last
   processed (possibly virtually) update, for example:
   _LastUpdateFileName=upd1109c.7z_
   
4. Create a shorcut to start FUT progam and specify the command line, for
   example: _somefolder\fut.exe somefolder\fut.ini_

Please note that relative file system paths taken from the initialization
file will be calculated from the current folder, not from the folders
containing _fut.exe_ or _fut.ini_.
