TITLE Includes
____________________________________________________________________________________________
____________________________________________________________________________________________
;;

'BuildEquatesTables' 'GetEquates' 
 
 
Win Equates: How it works.

The Equates.equ Files expected by the Parsers is in the form of:

A0_REG 010
A1_REG 011
A2_REG 012
A3_REG 013
A4_REG 014
A5_REG 015
AADBE_ADD_ENTRY 01
AADBE_DEL_ENTRY 02
... and so on.

That is:

* Zero blank Line.
* One line is: NAME / Space / HEXA / CR-LF
* File CR-LF ended.

Zero flexibility. Simply have 2 spaces instead of one, or a space after the Hexa, will
make it fail.

At launch time, RosAsm attempts to open "Equates.equ". If not found, it sends a 
Message telling the user to provide the path for that file.

After having open "Equates.equ", RosAsm also opens all other '*.equ" Files encounted
aside "Equates.equ", loads all of these Data and compiles them.

Compiling the Win Equates:

The Equates Names and Values are stored into two different parallel Tables, one for
the Names, one for the Values. The names are computed into encoded dWords (a kind
of CheckSum), so that the two Tables are the same size.


The NamesTable begins by a Header of dWords Pointers: In fact, not only the Equates
are encoded, but they also are divided in 'First-Char's Chunks and each Chunk is sorted
in numerical order. When searching for an Equate Value, RosAsm first re-encodes the
Name into the CheckSum, condiders the First Equate Char, read the according Pointer
in the NameTable Header (and also the next Pointer to get the size by substraction),
and begins searching in the proper Chunk only. This final search, for recovering a
Value from a given Name, is done by a fast 2n Search Algo.

____________________________________________________________________________________________
____________________________________________________________________________________________


 Reading the "Include" Files: At least: Equates.equ / Functions.api / Structures.str,
 which are the required ones for Win32. RosAsm does not run without ( > Run Help File
 if not found / Load them if found).

 All other .equ Files found in the same Directory add pasted to the same Memory Tables
 for Equates.

 Functions.api is a single File (Right-Click / Disassembly).

 .str Files will be available trough [Struc] main menu option. If several .str Files
 are found > build a child menu with an Item for each, and branch the sub Menu to
 [Struc].
;;
____________________________________________________________________________________________

;[IncludeFileName: B$ ? #&MAX_PATH]

; Called with '.ext' in eax:

; Similar to SetEquatesEquFileName
; call SetAllIncludeFilesExtension EquatesName, IncludeFileName
Proc SetAllIncludeFilesExtension:
    Argument @Input, @Output, @Extension
    Uses esi, edi

    mov esi D@Input, edi D@Output
    xor eax eax
    On B$esi = 0, ExitP

    C_call FormatStr edi, {'%s' 0}, D@Input
    add edi eax
    ...If B$edi-1 <> '\'
        Do
            dec edi
            If_Or B$edi = '\', B$edi = ':'
                inc edi
                jmp L2>
            End_If
        Loop_Until edi <= D@Output
    ...End_If
L2:
    C_call FormatStr edi, {'*%s' 0}, D@Extension
    add edi eax | mov B$edi 0

EndP

;;
Proc SetAllIncludeFilesExtension:
    Argument @Extension

    mov esi EquatesName, edi IncludeFileName

    While B$esi <> 0 | movsb | End_While
    dec edi
    While B$edi <> '.' | dec edi | End_While

L0: dec edi | cmp B$edi '\' | je L1>
              cmp B$edi ':' | je L1>
              cmp edi IncludeFileName | ja L0<
                jmp L2>
L1: inc edi
L2: mov B$edi '*' | inc edi | mov eax D@Extension | stosd | mov B$edi 0
EndP
;;
____________________________________________________________________________________________

[StructuresIncFileSize: ?    StructPopUpHandle: ?    StructuresFileOK: ?]
[StrucPopMenu: 'Struct', 0]

[StructuresItem: 'Structures', 0]

PrepareStructuresFiles:
    call SetAllIncludeFilesExtension  EquatesName, IncludeFileName, {B$ '.str', 0}
    call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | call SetFullName

    ..If eax = &INVALID_HANDLE_VALUE
        mov B$StructuresFileOK &FALSE
        ret
      ; Better let it run without Structures and Api Files if user wants to...
      ; May be temporary...

        call Help D$hwnd, B_U_AsmName, IncludeFilesHelp, ContextHlpMessage

        mov B$IncludesOK &FALSE

    ..Else  ; 'AddUserMenu'
        mov D$FindIncHandle eax
      ; Copy first the first File Path and Name in case there is only one:
        mov B$StructuresFileOK &TRUE
        mov esi FullName, edi MenuItemString, ecx &MAX_PATH | rep movsb

      ; Is there more than one File.str?
        call 'KERNEL32.FindNextFileA' D$FindIncHandle FIND_EQU

;mov eax &FALSE ; <<<<<<<<<<<<<<<< Temporary... (ToDo List...).

        .If eax = &TRUE
            call 'KERNEL32.FindClose' D$FindIncHandle

            mov D$StructureMenuID 4000, B$SeveralStructuresFiles &TRUE

            call 'USER32.CreatePopupMenu' | mov D$StructPopUpHandle eax

            call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU
            mov D$FindIncHandle eax

            Do
                call SetStructuresMenuItem
                call 'KERNEL32.FindNextFileA' D$FindIncHandle FIND_EQU
            Loop_Until eax = &FALSE



            call 'USER32.InsertMenuA' D$MenuHandle, M00_Structures,
                                  &MF_BYCOMMAND__&MF_POPUP__&MF_STRING,
                                  D$StructPopUpHandle, StructuresItem

            call 'USER32.DeleteMenu' D$MenuHandle, M00_Structures, &MF_BYCOMMAND


          ;  call 'USER32.DeleteMenu' D$MenuHandle 8 &MF_BYPOSITION
          ;  call 'USER32.InsertMenuA' D$MenuHandle 8 &MF_BYPOSITION__&MF_STRING__&MF_POPUP,
          ;                    D$StructPopUpHandle  StrucPopMenu
        .Else
          ; Else, there is only one .str File.
            mov B$SeveralStructuresFiles &FALSE

        .End_If

        call 'KERNEL32.FindClose' D$FindIncHandle

    ..End_If
ret

[StructureMenuID: ?]

; Builds the added PopUp Menu under [Struct] main Option:
; (We have a: > mov D$StructureMenuID 4000 in caller ('OpenStructuresFiles').

SetStructuresMenuItem:
    mov esi FIND_EQU_cFileName
    While B$esi <> 0 | inc esi | End_While
    dec esi
    While B$esi <> '.' | dec esi | End_While
    mov B$esi 0
L0: dec esi | cmp B$esi '\' | je L1>
              cmp B$esi ':' | je L1>
              cmp esi FIND_EQU_cFileName | ja L0<

L1: call 'USER32.AppendMenuA' D$StructPopUpHandle &MF_STRING D$StructureMenuID esi
    inc D$StructureMenuID
ret

 ________________________________________________________________________________________
 _________________________________________________________________________________________
;;
 15948 Win Equates are stored in 2 tables of dWords: The first one for the Names
 (stored as one dWord per name after string coding) and the second one for the
 relative Values. The routine for coding the names i used when building these tables
 is the same as the one use here for the Win Equates Search (see down there).

 The storage (and search) algorithm is NOT univoque. It simply revealed efficient
 upon this list (without any double values -same value for 2 names- encounted).

 The Data for OS Equates are now outside RosAsm, and computed at launch time.
;;
____________________________________________________________________________________________





[EquatesCurrentDirectory: B$ ? #&MAX_PATH]

;;
  In case user did not re-define the 'Equates.equ' Path, but set the 'Equates.equ' aside
  RosAsm, if he changes the Current Directory when working, and then run, for example [Struct]
  Menu Item, RosAsm would hang when expecting to find out 'Structures.str' in the new Current
  Path. So, we complete the Path here, if needed.
;;

AppendToCurrentDirectory:
    call 'KERNEL32.GetCurrentDirectoryA' &MAX_PATH, EquatesCurrentDirectory
    If eax <> 0
        mov edi EquatesCurrentDirectory | add edi eax
        mov al '\'
        On B$edi-1 <> al, stosb
        mov esi EquatesName | While B$esi <> 0 | movsb | End_While

        mov esi EquatesCurrentDirectory, edi EquatesName
        While B$esi <> 0 | movsb | End_While
    End_If
ret
____________________________________________________________________________________________

;[EquatesInFileTitle: 'Number of Encoded Equates:', 0]

;[EquatesIncFileSize: ?
;[EquateIncMemory: ?
[EquateIncMemoryPointer: ?]

[FindIncHandle: ?]

[FIND_EQU:
 FIND_EQU_dwFileAttributes: D$ 0
 FIND_EQU_ftCreationTime.dwLowDateTime: D$ 0
 FIND_EQU_ftCreationTime.dwHighDateTime: D$ 0
 FIND_EQU_ftLastAccessTime.dwLowDateTime: D$ 0
 FIND_EQU_ftLastAccessTime.dwHighDateTime: D$ 0
 FIND_EQU_ftLastWriteTime.dwLowDateTime: D$ 0
 FIND_EQU_ftLastWriteTime.dwHighDateTime: D$ 0
 FIND_EQU_nFileSizeHigh: D$ 0
 FIND_EQU_nFileSizeLow: D$ 0
 FIND_EQU_dwReserved0: D$ 0
 FIND_EQU_dwReserved1: D$ 0]
[FIND_EQU_cFileName: B$ 0 #&MAX_PATH]
[FIND_EQU_cAlternate: B$ 0 #14]

[FullName: ? #&MAX_PATH]

SetFullName:
    pushad
        mov esi IncludeFileName, edi FullName
        While W$esi <> '*.' | movsb | End_While
        mov esi FIND_EQU_cFileName
        While B$esi <> 0 | movsb | End_While | movsb
    popad
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; see EquatesCompletion
; return the size of the created memory in eax
; call OpenEquFiles 0, IncludeFileName, EquateIncMemory
Proc OpenEquFiles:
    Arguments @Address, @EquatesPath, @lphMem
    Local @MemSize
    Structure @Buffer 260, @Buffer.IncludeFileNameDis 0
    Uses edi

    mov eax D@EquatesPath
    call IsEquatesEquThere D@EquatesPath
    If eax = 0
        call SetEquatesEquFileName D@EquatesPath, D@Buffer, { B$ "B_U_Asm.exe", 0}
        call Help &NULL, D@Buffer, IncludeFilesHelp, RosAsmHlpMessage
        xor eax eax
        ExitP
    End_If

    call GetEquFilesMemory D@EquatesPath
    If eax = 0
        call 'User32.MessageBoxA' &NULL, RosAsmHlpMessage, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
        xor eax eax
        ExitP
    End_If

    mov D@MemSize eax

    call MemoryAlloc D@lphMem, eax
    mov edi D@lphMem
    call ReadEquatesEqu D@EquatesPath, D$edi
    If eax = 0
        call MemoryFree D$edi
        call 'KERNEL32.ExitProcess', 0
        ExitP
    End_If
    add eax D$edi
    call ReadOtherEquFiles D@EquatesPath, eax
    If eax = 0
        call MemoryFree D$edi
        call 'KERNEL32.ExitProcess', 0
    End_If

    call CleanEquateIncMemory D$edi, D@MemSize

EndP
;;
OpenEquFiles:
    call SetEquatesEquFileName EquatesName, IncludeFileName, EquatesEquFileName
    call IsEquatesEquThere IncludeFileName

    If B$IncludesOK = &TRUE
        call GetEquFilesMemory EquatesName
        call MemoryAlloc EquateIncMemory, eax
        call SetEquatesEquFileName EquatesName, IncludeFileName, EquatesEquFileName
        call ReadEquatesEqu IncludeFileName, D$EquateIncMemory
        add eax D$EquateIncMemory | mov D$EquateIncMemoryPointer eax
        call ReadOtherEquFiles EquatesName, D$EquateIncMemoryPointer
        call CleanEquateIncMemory D$EquateIncMemory, D$EquatesIncFileSize
    End_If
ret
;;
________________________________________________________________________________________

; Only copy the input path to the proper buffer
[EquatesEquFileName: B$ "Equates.equ", 0]
;call SetEquatesEquFileName EquatesName, IncludeFileName, EquatesEquFileName
; Similar to SetAllIncludeFilesExtension
Proc SetEquatesEquFileName:
    Arguments @Input, @Output, @EquatesNametoSet
    uses esi, edi, edx

    mov esi D@Input, edi D@Output
    xor eax eax
    On B$esi = 0, ExitP

    C_call FormatStr edi, {'%s' 0}, D@Input
    add edi eax
    ...If B$edi-1 <> '\'
        Do
            dec edi
            If_Or B$edi = '\', B$edi = ':'
                inc edi
                mov esi edi ; edx is the pointer to the start of the file name (without the path)
                jmp L2>
            End_If
        Loop_Until edi <= D@Output
    ...End_If
    L2:

    C_call FormatStr edi, {'%s' 0}, D@EquatesNametoSet
    add edi eax | mov B$edi 0
    mov eax esi ; Output the pointer to the start of the file name (without the path)

EndP

;;
SetEquatesEquFileName:
    mov esi EquatesName, edi IncludeFileName

    While B$esi <> 0 | movsb | End_While
    dec edi
    While B$edi <> '.' | dec edi | End_While

L0: dec edi | cmp B$edi '\' | je L1>
              cmp B$edi ':' | je L1>
              cmp edi IncludeFileName | ja L0<
                jmp L2>
L1: inc edi
L2: mov esi EquatesEquFileName
    While B$esi <> 0 | movsb | End_While | movsb
ret
;;

[Size_Of_WIN32_FIND_DATA 328]

Proc IsEquatesEquThere:
    Arguments @EquatesPath
    Structure @wfd 328, @wfd_dwFileAttributesDis 0, @wfd_ftCreationTime.dwLowDateTimeDis 4, @wfd_ftCreationTime.dwHighDateTimeDis 8,
              @wfd_ftLastAccessTime.dwLowDateTimeDis 12, @wfd_ftLastAccessTime.dwHighDateTimeDis 16, @wfd_ftLastWriteTime.dwLowDateTimeDis 20,
              @wfd_ftLastWriteTime.dwHighDateTimeDis 24, @wfd_nFileSizeHighDis 28, @wfd_nFileSizeLowDis 32, @wfd_dwReserved0Dis 36,
              @wfd_dwReserved1Dis 40, @wfd_cFileNameDis 44, @wfd_cAlternateFileNameDis 304, @wfd_dwFileTypeDis 318, @wfd_dwCreatorTypeDis 322,
              @wfd_wFinderFlagsDis 326
    Uses ecx, edx

    call 'RosMem.FastZeroMem' D@Wfd, Size_Of_WIN32_FIND_DATA
    call 'KERNEL32.FindFirstFileA' D@EquatesPath, D@Wfd

    .If eax = &INVALID_HANDLE_VALUE
        xor eax eax
    .Else
        mov eax &TRUE
    .End_If
EndP
;;
IsEquatesEquThere:
    ;call SetEquatesEquFileName
    call SetEquatesEquFileName EquatesName, IncludeFileName, EquatesEquFileName

    call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU

    .If eax = &INVALID_HANDLE_VALUE
        mov B$IncludesOK &FALSE
        call Help D$hwnd, B_U_AsmName, IncludeFilesHelp, RosAsmHlpMessage

    .Else
        mov B$IncludesOK &TRUE
    .End_If
ret
;;

[IncludeFileName: B$ 0 #&MAX_PATH]
;  024C
Proc GetEquFilesMemory:
    Arguments @EquatesPath
    Local @FindIncHandle, @StartFileName, @EquateFileSize
    Structure @wfd (Size_Of_WIN32_FIND_DATA+&MAX_PATH), @wfd.dwFileAttributesDis 0, @wfd.ftCreationTime.dwLowDateTimeDis 4, @wfd.ftCreationTime.dwHighDateTimeDis 8,
              @wfd.ftLastAccessTime.dwLowDateTimeDis 12, @wfd.ftLastAccessTime.dwHighDateTimeDis 16, @wfd.ftLastWriteTime.dwLowDateTimeDis 20,
              @wfd.ftLastWriteTime.dwHighDateTimeDis 24, @wfd.nFileSizeHighDis 28, @wfd.nFileSizeLowDis 32, @wfd.dwReserved0Dis 36,
              @wfd.dwReserved1Dis 40, @wfd.cFileNameDis 44, @wfd.cAlternateFileNameDis 304, @wfd.dwFileTypeDis 318, @wfd.dwCreatorTypeDis 322,
              @wfd.wFinderFlagsDis 326, @IncludeFileNameDis 328

    Uses ecx, edx

    call 'RosMem.FastZeroMem' D@Wfd, (Size_Of_WIN32_FIND_DATA+&MAX_PATH)

    lea eax D@IncludeFileNameDis
    call SetEquatesEquFileName D@EquatesPath, eax, { B$ "*.equ", 0}
    On eax = 0, ExitP

    mov D@StartFileName eax
    mov D@EquateFileSize 0

    lea eax D@IncludeFileNameDis
    call 'KERNEL32.FindFirstFileA' eax, D@Wfd
    If eax = &INVALID_HANDLE_VALUE
        xor eax eax | ExitP
    End_If
    mov D@FindIncHandle eax

    .Do
        ; below is unecessary. we may however, built a array of functions name using this
       ; mov edi D@StartFileName
        ;lea esi D@Wfd.cFileNameDis
        ;While B$esi <> 0 | movsb | End_While | mov B$edi 0

        ; get the LOWORD of nFileSizeLow member of WIN32_FIND_DATA structure
        mov eax D@Wfd.nFileSizeLowDis
        add D@EquateFileSize eax

        call 'KERNEL32.FindNextFileA' D@FindIncHandle, D@Wfd

    .Loop_Until eax = 0

    call 'KERNEL32.FindClose' D@FindIncHandle

    mov eax D@EquateFileSize ; This is the total size of all the equates found

EndP

;;

GetEquFilesMemory:
    call SetAllIncludeFilesExtension  EquatesName, IncludeFileName, {B$ '.equ', 0}

    call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | call SetFullName

    mov D$FindIncHandle eax, D$EquatesIncFileSize 0

L0:     mov eax D$FIND_EQU_nFileSizeLow | add D$EquatesIncFileSize eax

        call 'KERNEL32.FindNextFileA' D$FindIncHandle FIND_EQU
        call SetFullName | On eax = &TRUE, jmp L0<

    ;VirtualAlloc EquateIncMemory D$EquatesIncFileSize
    call MemoryAlloc EquateIncMemory D$EquatesIncFileSize

L9: call 'KERNEL32.FindClose' D$FindIncHandle
ret
;;
; call ReadEquatesEqu IncludeFileName, D$EquateIncMemory

________________________________________________________________________________________
; call ReadEquatesEqu IncludeFileName, D$EquateIncMemory
Proc ReadEquatesEqu:
    Arguments @EquatesPath, @Output
    Local @BytesRead, @hFile, @FileSize
    Uses ecx, edx

    call 'KERNEL32.CreateFileA' D@EquatesPath, &GENERIC_READ, &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0

    mov D@hFile eax
    mov D@BytesRead 0
    call 'KERNEL32.GetFileSize' D@hFile, 0
    If eax = 0
        call 'User32.MessageBoxA' &NULL, {B$ "ReadEquatesEqu: The loaded Equate file is empty", 0}, FileNotFound, &MB_ICONINFORMATION+&MB_SYSTEMMODAL
        call 'KERNEL32.CloseHandle' D@hFile
        xor eax eax
        ExitP
    End_If

    mov D@FileSize eax
    lea edx D@BytesRead
    call 'KERNEL32.ReadFile' D@hFile, D@Output, D@FileSize, edx, 0

    call 'KERNEL32.CloseHandle' D@hFile
    mov eax D@FileSize

EndP
________________________________________________________________________________________
;;
ReadEquatesEqu:
    ;call SetEquatesEquFileName
    call SetEquatesEquFileName EquatesName, IncludeFileName, EquatesEquFileName
    call 'KERNEL32.CreateFileA' IncludeFileName &GENERIC_READ, &FILE_SHARE_READ, 0,
                                &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0

    mov D$NumberOfReadBytes 0
    push eax
        push eax
            call 'KERNEL32.GetFileSize' eax, 0 | mov ecx eax
            If ecx = 0
                pop eax | jmp L9>
            End_If
            add eax D$EquateIncMemory | mov D$EquateIncMemoryPointer eax
        pop eax
        call 'KERNEL32.ReadFile' eax, D$EquateIncMemory, ecx, NumberOfReadBytes, 0
    pop eax

L9: call 'KERNEL32.CloseHandle' eax
ret
;;
____________________________________________________________________________________________

Proc IsItEquatesEqu:
    Arguments @String1, @String2
    uses edi, esi, ebx

    mov edi D@String1, esi D@String2

L0: mov al B$esi, bl B$edi | inc edi | inc esi
    If al = 0
        mov eax &TRUE
    Else
        or al 020 | or bl 020 | cmp al bl | je L0<
        xor eax eax
    End_If

EndP

____________________________________________________________________________________________
;;
IsItEquatesEqu:
    mov esi FIND_EQU_cFileName, edi EquatesEquFileName
L0: mov al B$esi, bl B$edi | inc edi | inc esi
    If al = 0
        cmp bl 0
    Else
        or al 020 | or bl 020 | cmp al bl | je L0<
    End_If
ret
;;
________________________________________________________________________________________

[BadEquatesFileEndMessage: B$ "The Equates File must be ended by *one* CR/FL", 0]
[BadEquatesFileTitle: B$ "Bad Equates File", 0]

Proc ReadOtherEquFiles:
    Arguments @EquatesPath, @Output
    Local @FindIncHandle, @StartFileName, @BytesRead, @NextFileHandle
    Structure @wfd (Size_Of_WIN32_FIND_DATA+&MAX_PATH), @wfd.dwFileAttributesDis 0, @wfd.ftCreationTime.dwLowDateTimeDis 4, @wfd.ftCreationTime.dwHighDateTimeDis 8,
              @wfd.ftLastAccessTime.dwLowDateTimeDis 12, @wfd.ftLastAccessTime.dwHighDateTimeDis 16, @wfd.ftLastWriteTime.dwLowDateTimeDis 20,
              @wfd.ftLastWriteTime.dwHighDateTimeDis 24, @wfd.nFileSizeHighDis 28, @wfd.nFileSizeLowDis 32, @wfd.dwReserved0Dis 36,
              @wfd.dwReserved1Dis 40, @wfd.cFileNameDis 44, @wfd.cAlternateFileNameDis 304, @wfd.dwFileTypeDis 318, @wfd.dwCreatorTypeDis 322,
              @wfd.wFinderFlagsDis 326, @IncludeFileNameDis 328
    Uses esi, edi, ecx, edx

    call 'RosMem.FastZeroMem' D@Wfd, (Size_Of_WIN32_FIND_DATA+&MAX_PATH)
    mov D@BytesRead 0

    lea eax D@IncludeFileNameDis
    call SetEquatesEquFileName D@EquatesPath, eax, { B$ "*.equ", 0}
    mov D@StartFileName eax

    lea eax D@IncludeFileNameDis
    call 'KERNEL32.FindFirstFileA' eax, D@Wfd
    mov D@FindIncHandle eax

    mov edi D@Output

    .Do

        lea eax D@Wfd.cFileNameDis
        call IsItEquatesEqu eax, EquatesEquFileName
        .If eax = &FALSE
            ; copy the path to the next file name to create the file
            mov edx D@StartFileName
            lea esi D@Wfd.cFileNameDis
            C_call FormatStr edx, {'%s' 0}, esi
            mov B$edx+eax 0

            lea eax D@IncludeFileNameDis
            call 'KERNEL32.CreateFileA' eax, &GENERIC_READ, &FILE_SHARE_READ, 0, &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
            mov D@NextFileHandle eax

            lea edx D@BytesRead
            call 'KERNEL32.ReadFile' D@NextFileHandle, edi, D@Wfd.nFileSizeLowDis, edx, 0
            add edi D@Wfd.nFileSizeLowDis
            call 'KERNEL32.CloseHandle' D@NextFileHandle
            If W$edi-2 <> CRLF
                call 'USER32.MessageBoxA' 0, BadEquatesFileEndMessage, BadEquatesFileTitle, 0
                call 'KERNEL32.FindClose' D@FindIncHandle
                xor eax eax | ExitP
            Else_If W$edi-4 = CRLF
                call 'USER32.MessageBoxA' 0, BadEquatesFileEndMessage, BadEquatesFileTitle, 0
                call 'KERNEL32.FindClose' D@FindIncHandle
                xor eax eax | ExitP
            End_If
        .End_If

        call 'KERNEL32.FindNextFileA' D@FindIncHandle, D@Wfd

    .Loop_Until eax <> &TRUE

    call 'KERNEL32.FindClose' D@FindIncHandle

    mov eax &TRUE

EndP

________________________________________________________________________________________
;;
ReadOtherEquFiles_OLd:
    call SetAllIncludeFilesExtension  EquatesName, IncludeFileName, {B$ '.equ', 0}

    call 'KERNEL32.FindFirstFileA' IncludeFileName FIND_EQU | call SetFullName

        push 0-1

        mov D$FindIncHandle eax

L0:     call IsItEquatesEqu FIND_EQU_cFileName, EquatesEquFileName | On eax = &TRUE, jmp L1>
        mov eax D$FIND_EQU_nFileSizeLow
        push eax
            call 'KERNEL32.CreateFileA' FullName &GENERIC_READ, &FILE_SHARE_READ, 0,
                                        &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0
        push eax

L1:     call 'KERNEL32.FindNextFileA' D$FindIncHandle FIND_EQU
        call SetFullName | On eax = &TRUE, jmp L0<

        mov D$NumberOfReadBytes 0
        mov edi D$EquateIncMemoryPointer
L0:     pop eax                     ; Handle
        On eax = 0-1, jmp L9>
        pop ecx                     ; Size
        push edi, ecx, eax
            call 'KERNEL32.ReadFile' eax, edi, ecx, NumberOfReadBytes, 0
        pop eax, ecx, edi

        add edi ecx

        On W$edi-2 <> 0A0D, jmp BadEquatesFileEnd
        On W$edi-4 = 0A0D, jmp BadEquatesFileEnd

        push edi | call 'KERNEL32.CloseHandle' eax | pop edi | jmp L0<

L9:     call 'KERNEL32.FindClose' D$FindIncHandle
ret
;;
________________________________________________________________________________________

Proc CleanEquateIncMemory:
    Arguments @Input, @FileSize
    uses edx, edi, esi

    mov esi D@Input, edi esi, edx esi
    add edx D@FileSize

    While esi < edx
        On B$esi = Tab, mov B$esi ' '
        inc esi
    End_While

    mov esi edi

    .While esi < edx
        lodsb | stosb
        If al = ' '
            While B$esi = ' '
                inc esi | dec D@FileSize
            End_While
        End_If
    .End_While

    mov eax D@FileSize

EndP

;;
CleanEquateIncMemory:
    mov esi D$EquateIncMemory, edi esi, edx esi | add edx D$EquatesIncFileSize

    While esi < edx
        On B$esi = Tab, mov B$esi ' '
        inc esi
    End_While

    mov esi edi

    .While esi < edx
        lodsb | stosb
        If al = ' '
            While B$esi = ' '
                inc esi | dec D$EquatesIncFileSize
            End_While
        End_If
    .End_While
ret
;;
____________________________________________________________________________________________

;[NumberOfEquates: ?]

; In fact ot only 'Count Equates', but also verify integrity of File syntax:
; One Symbol UpperCase / One space / One Hexa in RosAsm syntax / One CR/LF.

; call CountEquates D$EquateIncMemory, D$EquatesIncFileSize

[EQUATE_ERROR_PATH_LIMIT 40]
[BadEquatesFileValue: "The following equate inside one of your '.equ' files have an invalid syntax.

Equate: "
EquFileValue:
0 #EQUATE_ERROR_PATH_LIMIT,
BadEquatesFileValue2: " ...

The equates files must contains only hexadecimal strings.
Ex: 05555, 0FA, etc.. 
No dashes, or extra spaces are allowed", 0]

Proc CountEquates:
    Arguments @Input, @Size
    uses ebx, edx, esi, ecx

    mov edx D@Input
    add edx D@Size

    .If W$edx-2 <> CRLF
        call 'USER32.MessageBoxA' 0, BadEquatesFileEndMessage, BadEquatesFileTitle, 0
        ;mov edx D@Input
        ;call ReleaseEquatesMemory edx
        call MemoryFree D@Input
        call 'KERNEL32.ExitProcess', 0
        xor eax eax | ExitP
    .Else_If W$edx-4 = CRLF
        call 'USER32.MessageBoxA' 0, BadEquatesFileEndMessage, BadEquatesFileTitle, 0
        ;mov edx D@Input
        ;call ReleaseEquatesMemory edx
        call MemoryFree D@Input
        call 'KERNEL32.ExitProcess', 0
        xor eax eax | ExitP
    .End_If

    mov esi D@Input, ecx 0

    .While esi < edx
        mov ebx esi

        While B$esi > ' ' | inc esi | End_While         ; Read Symbol.

        inc esi
        call IsitHexadecimalStringinLine esi
        If eax = &FALSE
            mov edi EquFileValue
            mov ecx EQUATE_ERROR_PATH_LIMIT
            mov esi ebx
            Do
                On B$esi = 0, jmp L1>
                On W$esi = CRLF, jmp L1>
                movsb
                dec ecx
            Loop_Until ecx = 0
            L1:
            mov esi BadEquatesFileValue2
            While B$esi <> 0 | movsb | End_While
            mov B$edi 0

            call 'USER32.MessageBoxA' 0, BadEquatesFileValue, {"Equate Syntax Error", 0}, 0
            ;mov edx D@Input
            ;call ReleaseEquatesMemory edx
            call MemoryFree D@Input
            call 'KERNEL32.ExitProcess', 0
            xor eax eax | ExitP
        End_If

        mov esi eax
        add esi 2 | inc ecx

    .End_While

    mov eax ecx

EndP



; eax = 0 not a hexadecimal string
Proc IsitHexadecimalStringinLine:
    Arguments @String
    uses edi

    mov edi D@String

    L1:
        On W$edi = CRLF, jmp L1>
        On B$edi = ' ', jmp L1>
        If_and B$edi >= '0', B$edi <= '9'
        Else_If_And B$edi >= 'a', B$edi <= 'f'
        Else_If_And B$edi >= 'A', B$edi <= 'F'
        Else
            xor eax eax | ExitP
        End_If
        inc edi
    jmp L1<

L1: mov eax edi

EndP


;;
CountEquates:
    mov edx D$EquateIncMemory | add edx D$EquatesIncFileSize

    On W$edx-2 <> 0A0D, jmp BadEquatesFileEnd
    On W$edx-4 = 0A0D, jmp BadEquatesFileEnd

    mov esi D$EquateIncMemory, ecx 0

    .While esi < edx
        mov ebx esi

        While B$esi > ' ' | inc esi | End_While         ; Read Symbol.

        On B$esi <> ' ', jmp BadEquatesFile             ; One single space.

        inc esi | On B$esi <> '0', jmp BadEquatesFile

        While B$esi > ' ' | inc esi | End_While         ; One RosAsm syntax Hexa number.

        On W$esi <> 0A0D, jmp BadEquatesFile  ; CR/LF

        add esi 2 | inc ecx
    .End_While

    mov D$NumberOfEquates ecx
ret
;;
____________________________________________________________________________________________
;;
[BadEquatesFileTitle: 'Bad Equates File', 0]
[BadEquatesFileMessage: ? #10]
[BadEquatesFileEndMessage: 'The Equates File must be ended by *one* CR/FL', 0]

BadEquatesFile:
    mov esi ebx, edi BadEquatesFileMessage
    While B$esi > ' ' | movsb | End_While
    mov D$edi '...', B$edi+4 0


    call 'USER32.MessageBoxA' 0, BadEquatesFileMessage, BadEquatesFileTitle, 0
    call 'KERNEL32.ExitProcess', 0


BadEquatesFileEnd:

    call 'USER32.MessageBoxA' 0, BadEquatesFileEndMessage, BadEquatesFileTitle, 0
    call 'KERNEL32.ExitProcess', 0
;;

;;
[EquatesNumber: ?     WinEquTableLenght: ?
 NamesTable: ?    ValuesTable: ?
 SortedNamesTable: ?    SortedValuesTable: ?]
;;

;[SizeByFirstChar: ? #256] ; 26 Length (Length for Equates beginning by 'A, by 'B',... 'Z'.
; plus '[/]^_', just to have '_' >>> 31 records.
;[ApiChunksPointers: ? #256] ; Used to follow up when filling the Api List Chunks.


 ________________________________________________________________________________________





















































