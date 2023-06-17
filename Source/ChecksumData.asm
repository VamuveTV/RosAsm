TITLE ChecksumData



____________________________________________________________________________________________
____________________________________________________________________________________________

;;

    Functions used:

    ClearQwordCheckSum  CheckSum64  CheckSum16
    SaveCheckSumTable   RestoreCheckSumTable    SetQwordCheckSum    GetFromQwordCheckSum
    NoDuplication       CompareSymbols
    
    NewBuildWin32Equates
    VerifyEquatesFileConformity     BadEquateLine   NoDuplicationOfEquates
    ShowDumplicatedEquates  NewSearchForWinEquate   NewGetEquates       ReadWin32Equate
    
    TestRepartition

    External functions:
    
    NewBuildWin32Equates    OpenEquFiles    CountEquates
    SetEquatesEquFileName   IsEquatesEquThere   CleanEquateIncMemory
    SetAllIncludeFilesExtension     IsItEquatesEqu      ReadOtherEquFiles
    IsitHexadecimalStringinLine

    See Also:

        PrepareStructuresFiles
        EquatesCompletion
        ReadEquatesEqu


;;
____________________________________________________________________________________________



;;
  The Symbolic Names are encoded into a qWord CheckSum. Then, this qWord is encoded
  into one another Word CheckSum. Let's call them 'CheckSum64' and 'CheckSum16'.
  __________________
  A two Stages Table is declared to hold 010000h (for storing the distributions
  of the Records) plus 010000h Records (for storing the Linked Records in case
  of identical CheckSum16).
  
  The CheckSum16 is used as a Index to point to the Records of the first stage
  of the Table.
  
  _______________
  Each Record is: [CheckSum64, Pointer, LinkedPointer]

  * 'Pointer' points to a Name, in one of the Assembler internal Lists. It is
    nothing but the Pointer transmitted to the 'SetQwordCheckSum' Procedure,
    when called. We do not need to do do any Name String Copy, as it is already
    there, in the concerned List ('LabelList', 'MacroList', 'EquateList').
    
  * If the second half of the Records Table is empty, 'LinkedPointer' points to
    the top of this Second half Table. Otherwise, in case several 'CheckSum64'
    achieve into the same 'CheckSum16', 'LinkedPointer' points to the next same
    'CheckSum16' Record, in the second half of the Table... and so on...
    
  * The first half of the Table is filled 'randomaly-like' (depending on the
    CheckSum16 value. The second half of the Table is filled in order (Top-Down),
    each time a Record of the First Half Table is found to not be empty.
    
  _________
  Routines:
  
  * 'ClearQwordCheckSum' zeroes the Records Table and sets
    'PointerToCheckSumsLinkedRecords' to the Top of the second half Table.
  
  * 'SetQwordCheckSum' is called with a Parameter pointing to the first Char
    of a Name - in its family List - to be recorded. It also call for:
    
  * 'NoDuplication' to make sure of unique Symbolics Declarations.
  
  * 'GetFromQwordCheckSum' is called with a Parameter pointing to a Name to
     be checked. If found, the Procedure returns the Pointers that was used
     when calling 'SetQwordCheckSum', that are nothing but Pointers to the
     Lists ('MacroList', 'EquateList', 'LabelList'), that hold all infos the
     Assembler needs for doing its job. If not found, it returns zero to the
     caller.
     
  * 'TestRepartition', is just a Dev-test for viewing how all of this goes.
  
  ______________________________
  How the Records Table is used:
  
  When calling 'SetQwordCheckSum', the CheckSum64 is computed from the given
  Name. Then, the CheckSum16 is computed from the given CheckSum64.
  
  CheckSum16 is used as an Indice to the first half of the Records Table.
  For example, if CheckSum16 is 25, we point to the 25th Record, that is
  (CheckSumsRecords + (25*(8+4+4))).
  
  If this record is found empty, it is written: CheckSum64 / Pointer / Link
  
  As long as there is nothing in the second half of the Table, 'Link' points
  to the empty Record at (CheckSumsRecords + (010000h*16) ).
  
  If the first half Table is not found empty, the 'Link' Pointer is read,
  and we jump there,... and so on...
  
  The chances for having two different Symbols achieving into the same CheckSum64
  are, of course, of 1 on 01_0000_0000_0000_0000h, for the second Name, of 1 on
  01_0000_0000_0000_0000h, for the third name, and so on... So, in the very unlikely
  coming out cases when two different Names are computed into the same CheckSum64,
  the new record is linked downward, the same way the Duplications of the CheckSum16
  are Linked.
  
  The reverse case is also theorically possible: For example, you do _not_ implement,
  say, some 'GetPointer' Macro, and you use it in a Statement. It is theorically
  not impossible that some other _declared_ Macro achieves into the same CheckSum64
  as would your non existing 'GetPointer'. In such -very unlikely coming out cases-,
  the 'GetFromQwordCheckSum' would return a pointer, instead of zero. So, when a
  valid CheckSum64 is found, 'GetFromQwordCheckSum' also calls for 'CompareSymbols',
  to make it 100% secure.
;;
____________________________________________________________________________________________

; Total size is 0300004 (3.145.732) bytes
; CheckSum64 / Pointer / Link

;;

        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            mov D$CheckSumsRecords+ecx eax
            mov D$CheckSumsRecords+ecx+4 ebx
            move D$CheckSumsRecords+ecx+8 edx
CheckSum64
Symbolic Names
[SYMBOLIC_NAMES:
 SymData.dwLowCheckSum64: D$ 0 ; eax CheckSum64
 SymData.dwHighCheckSum64: D$ 0 ; ebx checksum64
 SymData.LinkedValue: D$ 0 ; edx stored value
 SymData.Pointer: D$ 0 ; edx stored value CheckSumsLinkedRecords The start of data structure ?
 ]

; This, in fact is just a array of structures. The representation can be like this:

[SYMBOLIC_NAMES:

 ; 1st array position. Pos = 0
 SymData.dwLowCheckSum64.Data0: D$ 0    ; 0 = 0*16+0
 SymData.dwHighCheckSum64.Data0: D$ 0   ; 4 = 0*16+4
 SymData.LinkedValue.Data0: D$ 0        ; 8 = 0*16+8
 SymData.Pointer.Data0: D$ 0            ; 12 = 0*16+12
 
 ; 2nd array position. Pos = 1
 SymData.dwLowCheckSum64.Data1: D$ 0    ; 16 = 1*16+0
 SymData.dwHighCheckSum64.Data1: D$ 0   ; 20 = 1*16+4
 SymData.LinkedValue.Data1: D$ 0        ; 24 = 1*16*8
 SymData.Pointer.Data1: D$ 0            ; 28 = 1*16+12
 
 ; 3rd array position. Pos = 2
 SymData.dwLowCheckSum64.Data2: D$ 0    ; 32 = 2*16+0
 SymData.dwHighCheckSum64.Data2: D$ 0   ; 36 = 2*16+4
 SymData.LinkedValue.Data2: D$ 0        ; 40 = 2*16+8
 SymData.Pointer.Data2: D$ 0            ; 44 = 2*16+12
 
...

; The size of the structure array is 16
 ]

Equates are:

[SymData.dwLowCheckSum64Dis 0
 SymData.dwHighCheckSum64Dis 4
 SymData.LinkedValueDis 8
 SymData.PointerDis 12]

[Size_Of_SymData 16]


So, the formula to compute the position of a member of a structure inside a given array, is:

NewPos = (Pos*Size_Of_SymData)+SymData.dwLowCheckSum64Dis

;;

[MAX_SYMNAME 030000]

; CheckSum64 / Pointer / Link

[CheckSumsRecords: ? ? ? ? #010000    CheckSumsLinkedRecords: ? ? ? ? #020000
 PointerToCheckSumsLinkedRecords: ?   CheckSumsEnd: ]

; Equates are:

[SymData.dwLowCheckSum64Dis 0
 SymData.dwHighCheckSum64Dis 4
 SymData.LinkedValueDis 8
 SymData.PointerDis 12]

[Size_Of_SymData 16]

Proc ClearQwordCheckSum:


    ;mov edi CheckSumsRecords, eax 0
    ;mov ecx CheckSumsEnd | sub ecx CheckSumsRecords | shr ecx 2
    ;rep stosd
    mov ecx CheckSumsEnd | sub ecx CheckSumsRecords
    call 'RosMem.FastZeroMem' CheckSumsRecords, ecx

    mov D$PointerToCheckSumsLinkedRecords CheckSumsLinkedRecords
;ret
EndP

[CheckSumImage: ?]

SaveCheckSumTable:
    pushad
        mov ecx PointerToCheckSumsLinkedRecords | sub ecx CheckSumsRecords

        ;push ecx
            ;VirtualAlloc CheckSumImage, ecx
            call MemoryAlloc CheckSumImage, ecx
        ;pop ecx

        shr ecx 2

        mov esi CheckSumsRecords, edi D$CheckSumImage | rep movsd
    popad
ret

RestoreCheckSumTable:
    pushad
        mov ecx PointerToCheckSumsLinkedRecords | sub ecx CheckSumsRecords | shr ecx 2

        mov esi D$CheckSumImage, edi CheckSumsRecords | rep movsd

        ;VirtualFree D$CheckSumImage
        call MemoryFree D$CheckSumImage
    popad
ret

____________________________________________________________________________________________

CheckSum64:
  ; esi -> Name
    mov eax 0, ebx 0, ecx 0

    While B$esi > ' ' ;LowSigns
        rol eax 1 | lodsb | mul eax | xor ebx edx | inc ecx
    End_While
    add ebx ecx
    If eax = 0
        On ebx = 0, mov eax 1
    End_If
  ; ebx:eax = CheckSum64 // ecx = Length
ret


CheckSum16:
  ; ebx:eax = CheckSum64 (not modified here))
    mov ecx eax | xor ecx ebx | mov edx ecx
    rol edx 16 | xor cx dx
    and ecx 0FFFF | shl ecx 4
  ; ecx = CheckSum16, to be used as a Displacement to the matching Record
  ; (To 'CheckSumsRecords' first half part, 16 Bytes per Record)
ret
____________________________________________________________________________________________

Proc SetQwordCheckSum:
    Argument @Pointer

    pushad

        mov esi D@Pointer

        If B$esi < '0'
            ;
        Else_If B$esi <= '9'
            error D$NumerAsSymbolPtr, D@Pointer
        End_If

        call CheckSum64 | call NoDuplication D@Pointer | call CheckSum16

      ; The List Pointer is used to test empty Records (Lists Pointers can never be zero):
        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+4 <> 0, jmp L1>
            mov D$CheckSumsRecords+ecx eax
            mov D$CheckSumsRecords+ecx+4 ebx
            move D$CheckSumsRecords+ecx+8 D@Pointer
          ; D$CheckSumsRecords+ecx+12 = 0
        .Else
L1:         If D$CheckSumsRecords+ecx+12 = 0
                move D$CheckSumsRecords+ecx+12 D$PointerToCheckSumsLinkedRecords
            Else
                mov edi D$CheckSumsRecords+ecx+12
                While D$edi+12 <> 0 | mov edi D$edi+12 | End_While
                move D$edi+12 D$PointerToCheckSumsLinkedRecords
            End_If

            mov edi D$PointerToCheckSumsLinkedRecords
            mov D$edi eax
            mov D$edi+4 ebx
            move D$edi+8 D@Pointer
            ;mov eax D$PointerToCheckSumsLinkedRecords | add eax 16
          ; D$edi+12 = 0
            ;mov D$PointerToCheckSumsLinkedRecords eax
            add D$PointerToCheckSumsLinkedRecords 16
        .End_If

    popad
EndP
____________________________________________________________________________________________

Proc GetFromQwordCheckSum:
    Argument @Pointer, @List, @Limit
    Uses esi, edi, ebx, ecx, edx

        mov esi D@Pointer

        call CheckSum64 | call CheckSum16

        lea esi D$CheckSumsRecords+ecx | mov ecx &TRUE

L0:     ..If D$esi = eax
            .If D$esi+4 = ebx
                mov eax D$esi+8
                mov ebx D@List | On eax < ebx, mov ecx &FALSE
                mov ebx D@Limit | On eax > ebx, mov ecx &FALSE

                On ecx = &TRUE, call CompareSymbols D@Pointer, eax

                If ecx = &FALSE
                    push esi
                        mov esi D@Pointer | call CheckSum64
                    pop esi

                    mov ecx &TRUE | jmp L2>
                End_If

            .Else
                jmp L2>

            .End_If

        ..Else
L2:
            ;inc D$EquatesPasses
            mov esi D$esi+12 | cmp esi 0 | je L3>

          ; If no List Pointer, this is the first empty Record in the Linked Record Table:
            mov ecx &TRUE | cmp D$esi+8 0 | ja L0<<

L3:         mov eax 0

        ..End_If
   ; hexprint D$EquatesPasses
EndP
____________________________________________________________________________________________



Proc NoDuplication:
    Argument @Pointer
    Uses eax, ebx, ecx

      ; ebx:eax = CheckSum64
        mov ecx eax | xor ecx ebx | mov edx ecx
        rol edx 16 | xor cx dx
        and ecx 0FFFF | shl ecx 4

        lea esi D$CheckSumsRecords+ecx

L0:     While D$esi+4 <> 0
            If D$esi = eax
                On D$esi+4 = ebx, call CompareSymbols D@Pointer, D$esi+8
                On ecx = &TRUE, error D$SymbolDupPtr
            End_If

            mov esi D$esi+12 | On esi = 0, ExitP

        End_While
EndP
____________________________________________________________________________________________

Proc CompareSymbols:
    Argument @Source, @Destination
    Uses eax, esi, edi

        mov esi D@Source, edi D@Destination

L0:     lodsb | cmp al LowSigns | jb L5>
        inc edi | cmp al B$edi-1 | je L0<

        mov ecx &FALSE | ExitP

L5:     If B$edi < LowSigns
            mov ecx &TRUE
        Else
            mov ecx &FALSE
        End_If
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  This test is for viewing the Records distribution, in the CheckSums Table.
  
  The first Pixels square shows the occupied Records in the first half Part of
  the Table (black Pixels). The occupied Records, in the second half of the
  Table are represented by red Pixels. They, of course, come in the form of
  a red _line_.
  
  The actual distribution seems to be pretty close to a good random one.
;;

[CheckSumsPixelsCount: ?     CheckSumsLinkedRecordsPixels: ?]
[SecondTable: ?]

TestRepartition:
    mov D$CheckSumsPixelsCount 0, D$CheckSumsLinkedRecordsPixels 0

    call 'User32.BeginPaint' D$EditWindowHandle, PAINTSTRUCT | mov D$hdc eax
    call 'User32.GetClientRect' D$EditWindowHandle, RECT

    mov esi CheckSumsRecords

L0: mov eax D$esi | or eax D$esi+4
    If eax <> 0
        mov eax esi | sub eax CheckSumsRecords | shr eax 4
        mov ebx eax | shr eax 8 | and ebx 0FF
        inc D$CheckSumsPixelsCount
        call 'GDI32.SetPixel' D$hdc, eax, ebx, 0
    End_If

    add esi 16 | cmp esi CheckSumsLinkedRecords | jb L0<

L0: mov eax D$esi | or eax D$esi+4
    If eax <> 0
        mov eax esi | sub eax CheckSumsLinkedRecords | shr eax 4
        mov ebx eax | shr eax 8 | and ebx 0FF | add eax 0100
        inc D$CheckSumsPixelsCount
        inc D$CheckSumsLinkedRecordsPixels
        call 'GDI32.SetPixel' D$hdc, ebx, eax, 0FF
    End_If

    add esi 16 | cmp esi PointerToCheckSumsLinkedRecords | jb L0<

    call 'USER32.ReleaseDC' D$EditWindowHandle, D$hdc
    call 'USER32.EndPaint' D$EditWindowHandle, PAINTSTRUCT

    If D$CheckSumsPixelsCount > 0
       call WaitForUserAction

      ; Comment out to have the total number of Pixel and how many Linked Records:
      ; (On RosAsm, for V.1.24e: 64,536 // 313)

      ; Hexprint D$CheckSumsPixelsCount
      ; hexprint D$CheckSumsLinkedRecordsPixels

;;
  __________________________
  Results on RosAsm V.1.25d:
  
  Total = 019D9 = 6617
  Links =  0139 =  313

  Total of available Records in the Table first half = 010000 = 65536

  6617 / 65536 = 10.09 % of the Table is occupied by Records(+Linked Records)

  313 / 6617 = 4.73 % of the Records require a linkage
  
  In short: 5% of the Records are Linked when 10% of the table first half is occupied
;;

    Else
        call 'USER32.MessageBoxA' 0,
      {"The Symoblics'CheckSums'Table can be viewed only after a Compilation.", 0},
      {'Nothing to show', 0}, &MB_OK

    End_If
ret

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
  Reuse of the CkeckSum64 Method for computing the Win32 Equates:
  
  The CheckSum64 Method is so fast, that it is now useless to save the computed
  Tables, 'Equates.nam' and 'Equates.num', like we did, before V.2.015c.
  
  So, each time RosAsm is started, it now loads the 'Equates.equ' File, and reuse
  the CheckSum64 Tables, to rebuild the search Table.
  
  The Records are used in different menner. Instead of "CheckSum64 / Pointer / Link",
  the 'CheckSumsRecords' for the Win32 Equates, are, "CheckSum64 / Value / Link",
  so that, - as the integrity is verified before usage of the Equates.equ File -,
  we can retrieve the Value immidiately from the CheckSum64 Table.
  
  Another difference is that the normal 'CheckSumsRecords' is Static, for the Symbols
  Jobs for the Assembler. We use it, as is, for building the Win32 Equates Table, but,
  once done, we copy this Table to a Dynamic Memory Chunk.
  
  'NewBuildWin32Equates':
  
      Build the Win32 Equates Table and saves it in 'NewWinEquatesMem'. This Routine
      includes two commented out calls to verify the conformity of the List:
    
      'VerifyEquatesFileConformity' and 'NoDuplicationOfEquates'
    
  'NewSearchForWinEquate' and 'NewGetEquates' are for retrieving the Value from
  an &EQUATE_NAME, from the Assembler ('ReplaceWin32Equates') and/or from 'RightClick'
  
  At Start-Up, you can see the Win32 Equates Table with
  [Tools] / [RosAsm Devs Tests] / [Show Symbols Repartition]
  
  Depending on the Processor, the speed improvement is between 1.5 and 5 %, compared
  to the previous Method, on an Auto-Compilation of RosAsm. For the Table built, it
  is many times faster (now, one second, on my old 95 Box with a generic Pentium, and
  the Click time on my Celeron 1.3).
;;
____________________________________________________________________________________________
____________________________________________________________________________________________


;[NewWinEquatesMem: ?
[dWordsLenOfEquatesList: ?]

; general Usage Equate Memory structure

[EQU_MEMORY:
 EQU_MEMORY.IncFileMem: EquateIncMemory: D$ 0
 EQU_MEMORY.IncMemSize: EquatesIncFileSize: D$ 0
 EQU_MEMORY.NewEquMem: NewWinEquatesMem: D$ 0
 EQU_MEMORY.IncludeFlag: IncludesOK: D$ 0]

; constants used on the EQU_MEMORY structure
[EQU_MEMORY.IncFileMemDis 0
 EQU_MEMORY.IncMemSizeDis 4
 EQU_MEMORY.NewEquMemDis 8
 EQU_MEMORY.IncludeFlagDis 12]

[Size_of_EQU_MEMORY 16]

; Called from 'Main', each time RosAsm is started:

Proc NewBuildWin32Equates: ; 'CheckSumsRecords' call OpenEquFiles
    Local @EquatesCnt

    ;call OpenEquFiles | On B$IncludesOK = &FALSE, ret
    call SetEquatesEquFileName EquatesName, IncludeFileName, EquatesEquFileName
    call OpenEquFiles 0, IncludeFileName, EquateIncMemory
    On eax = &FALSE, ExitP;ret

    mov D$IncludesOK &TRUE
    mov D$EquatesIncFileSize eax

    ;call VerifyEquatesFileConformity

    call CountEquates D$EquateIncMemory, D$EquatesIncFileSize ; >>> 'NumberOfEquates'
    ;mov D$NumberOfEquates eax
    mov D@EquatesCnt eax
    ;hexprint D$NumberOfEquates
    ;push D$NumberOfEquates

        mov esi D$EquateIncMemory

        mov D$PointerToCheckSumsLinkedRecords CheckSumsLinkedRecords

;L0:
    .Do
        mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | mov B$ebx 0

        call CheckSum64 | call CheckSum16
    ; ebx:eax = CheckSum64 // ecx = CheckSum16

    ; Get the Hexa Value into edx:
        ;call dwToHexString esi, CR
        push eax
            inc esi  ; skip over the space
            mov edx 0
L1:         shl edx 4 | mov al B$esi | sub al '0' | On al > 9, sub al 7
            or dl al
            inc esi | cmp B$esi CR | ja L1<
            add esi 2
        pop eax

        .If D$CheckSumsRecords+ecx = 0
            On D$CheckSumsRecords+ecx+SymData.dwHighCheckSum64Dis <> 0, jmp L1>
            mov D$CheckSumsRecords+ecx+SymData.dwLowCheckSum64Dis eax
            mov D$CheckSumsRecords+ecx+SymData.dwHighCheckSum64Dis ebx
            move D$CheckSumsRecords+ecx+SymData.LinkedValueDis edx
        .Else
L1:         If D$CheckSumsRecords+ecx+SymData.PointerDis = 0
                move D$CheckSumsRecords+ecx+SymData.PointerDis D$PointerToCheckSumsLinkedRecords
            Else
                mov edi D$CheckSumsRecords+ecx+SymData.PointerDis
                While D$edi+SymData.PointerDis <> 0
                    ;call NoDuplicationOfEquates
                    mov edi D$edi+SymData.PointerDis
                End_While
                ;call NoDuplicationOfEquates
                move D$edi+SymData.PointerDis D$PointerToCheckSumsLinkedRecords
            End_If

            mov edi D$PointerToCheckSumsLinkedRecords
            mov D$edi+SymData.dwLowCheckSum64Dis eax
            mov D$edi+SymData.dwHighCheckSum64Dis ebx
            move D$edi+SymData.LinkedValueDis edx
            add D$PointerToCheckSumsLinkedRecords Size_Of_SymData
        .End_If

            ;dec D$NumberOfEquates; | cmp D$NumberOfEquates 0 | jne L0<<
        dec D@EquatesCnt
    ;.Loop_Until D$NumberOfEquates = 0
    .Loop_Until D@EquatesCnt = 0

    ;pop D$NumberOfEquates

  ; Now, store the Win32 Equates CheckSums Table into Memory:

    mov ecx D$PointerToCheckSumsLinkedRecords | add ecx Size_Of_SymData | sub ecx CheckSumsRecords

    call MemoryAlloc NewWinEquatesMem, ecx
    ;push ecx
    ;    VirtualAlloc NewWinEquatesMem, ecx
    ;pop ecx

    mov esi CheckSumsRecords, edi D$NewWinEquatesMem | shr ecx 2
    mov D$dWordsLenOfEquatesList ecx | rep movsd
;;
  Adjust all of the Linked Records Pointer (above, they are pointing for a Base
  of 'CheckSumsRecords'. Now, the Base must be inside 'NewWinEquatesMem'
;;
  ; Adjustement Value:
    mov eax CheckSumsRecords | sub eax D$NewWinEquatesMem

    mov ebx CheckSumsRecords | sub ebx eax
  ; edi still points to the End of the fresh copied 'NewWinEquatesMem':
    While ebx < edi
        On D$ebx+SymData.PointerDis <> 0, sub D$ebx+SymData.PointerDis eax
        add ebx Size_Of_SymData
    End_While

    ;call ClearQwordCheckSum
;ret
EndP
____________________________________________________________________________________________

Proc dwToHexString:
    Arguments @InputString, @CharLimit, @pStringLenght
    Uses esi, ebx, ecx

    mov esi D@InputString
    mov ecx D@CharLimit
    mov ebx 0
    Do
        lodsb | sub al '0' | On al > 9, sub al 7
        shl ebx 4 | or bl al
    Loop_Until B$esi = cl
    sub esi D@InputString | mov ecx D@pStringLenght | mov D$ecx esi
    mov eax ebx

EndP
____________________________________________________________________________________________

VerifyEquatesFileConformity:
     mov esi D$EquateIncMemory, edx esi | add edx D$EquatesIncFileSize

     .While esi < edx
L0:   ; Read one Symbol;
        If W$esi = '__'
            jmp BadEquateLine
        Else_If B$esi = '_'
            ; Good
        Else_If B$esi < '0'
            jmp BadEquateLine
        Else_If B$esi > 'Z'
            jmp BadEquateLine
        End_If
        inc esi | cmp B$esi ' ' | ja L0<

      ; No trailing '_':
        On B$esi-1 = '_', jmp BadEquateLine

      ; One Space, followed by one single '0':
        inc esi
        On B$esi <> '0', jmp BadEquateLine
        On B$esi+1 = '0', jmp BadEquateLine

      ; One Hexa Value:
        While B$esi > ' '
            If B$esi < '0'
                jmp BadEquateLine
            Else_If B$esi > 'F'
                jmp BadEquateLine
            End_If

            inc esi
        End_While

      ; CRLF
        On W$esi <> CRLF, jmp BadEquateLine

        add esi 2
    .End_While
ret


[BadEquateLineMessage: "Bad Equate line in Equates.equ: 

"
 BadEquateLineLine: '                                                        ' ]

BadEquateLine:
    While B$esi > CR
        inc esi | On esi = edx, jmp L0>
    End_While

    mov D$esi 0

    dec esi

    While B$esi > LF
        dec esi | On esi = D$EquateIncMemory, jmp L0>
    End_While

L0: mov edi BadEquateLineLine
    While B$esi <> 0 | movsb | End_While | mov B$edi 0

    showme BadEquateLineMessage
ret

____________________________________________________________________________________________

[DuplicatedEquate: 'Duplicated Equates', 0]

NoDuplicationOfEquates:
    If D$edi = eax
        On D$edi+4 = ebx, call ShowDumplicatedEquates
    End_If
ret

[CheckSumEax: ?   CheckSumEbx: ?]

ShowDumplicatedEquates:
    pushad
        mov D$CheckSumEax eax, D$CheckSumEbx ebx
        mov esi D$EquateIncMemory

L0:     mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | mov B$ebx 0

        push esi
            call CheckSum64
        pop edx

        .If eax = D$CheckSumEax
            If ebx = D$CheckSumEbx
                mov edi TrashString
                While B$edx <> 0 | mov al B$edx, B$edi al | inc edx | inc edi | End_While
                mov D$edi ' == ' | add edi 4
                jmp L0>
            End_If
        .End_If
        While B$esi > CR | inc esi | End_While
        add esi 2 | jmp L0<

L0:     mov ebx esi | While B$ebx > ' ' | inc ebx | End_While | mov B$ebx 0

        push esi
            call CheckSum64
        pop edx

        .If eax = D$CheckSumEax
            If ebx = D$CheckSumEbx
                While B$edx <> 0 | mov al B$edx, B$edi al | inc edx | inc edi | End_While
                mov B$edi 0
                jmp L0>
            End_If
        .End_If
        While B$esi > CR | inc esi | End_While
        add esi 2 | jmp L0<

L0: showme TrashString
    popad
ret
____________________________________________________________________________________________

[EquateFound: ?]

NewSearchForWinEquate:
    push esi
        call NewGetEquates

        If B$EquateFound = &FALSE
            mov esi TrashString | error D$BadWinEquPtr ; error8
        End_If

        or D$imm32 eax
    pop esi

    add esi D$NewWinEquateLenght

    .If B$esi = addSign
        On B$esi+1 <> '&', ret
        add esi 2 | jmp NewSearchForWinEquate
    .Else_If B$esi = '&'
        inc esi | jmp NewSearchForWinEquate
    .Else
        While B$esi = '_'
            inc esi
        End_While
        If B$esi = '&'
            inc esi | jmp NewSearchForWinEquate
        End_If
    .End_If
ret


[NewWinEquateLenght: ?]

; Called from the Assembler and from 'RightClick'

NewGetEquates: ;New GetEquates: RightClick
    mov edi TrashString
    While B$esi > ' '
        mov ax W$esi
        If al = '+'
            jmp L1>
        Else_If al = '-'
            jmp L1>
        Else_If ax = '__'
            jmp L1>
        Else_If al = ','
            jmp L1>
        Else_If al = ';'
            jmp L1>
        Else_If ax = '_&'
            jmp L1>
        Else_If al = '&'
            jmp L1>
        Else_If al = ']'
            jmp L1>
        Else_If al = ')'
            jmp L1>
        Else
            On al >= 'a', sub al 32
            mov B$edi al | inc esi | inc edi
        End_If
    End_While

L1: mov B$edi 0 | sub edi TrashString | mov D$NewWinEquateLenght edi

  ; Simplified 'GetFromQwordCheckSum':
    mov esi TrashString

ReadWin32Equate:
    call CheckSum64 | call CheckSum16

    mov esi D$NewWinEquatesMem | add esi ecx

;L0:
    Do
        If_And D$esi+SymData.dwLowCheckSum64Dis = eax, D$esi+SymData.dwHighCheckSum64Dis = ebx
            mov eax D$esi+SymData.LinkedValueDis, B$EquateFound &TRUE | ret
        End_If
        ;.If D$esi+SymData.dwLowCheckSum64Dis = eax
         ;   If D$esi+SymData.dwHighCheckSum64Dis = ebx
          ;      mov eax D$esi+SymData.LinkedValueDis, B$EquateFound &TRUE | ret
           ; End_If
        ;.End_If

        mov esi D$esi+SymData.PointerDis; | cmp esi 0 | jne L0<
    Loop_Until esi = 0

    mov B$EquateFound &FALSE
ret

































































