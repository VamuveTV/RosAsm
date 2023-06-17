TITLE PEHeaderStructures

_______________________________________________________________________________________________________


;;

Functions Used:
    CheckPE_MZ  PECheck_ErrManager  EntryPointSizeCheck FixMzParagraphsNumber

PE Structures used:

    DosHeader   DOS_STUB   StubMsg   PeHeader   PeHeader.OptionalHeader.Magic   SectionTable    SectionsHeaders

Structures and equates used on the SectionTable:

    Import_Table_Adress
    Resource_Structures
    Load_Config_Directory_Structures
    BoundIAT_Directory_Structures
 
    Delay_Import_Address_Table_Structures


Relevant used Data:

    DisPeTagPointer

    DumpDLL

;;



;;
 This function Checks the validity of a PE File.

 It analyses for inconsistences of size, contents of the Header (Dos Header and PE Header). This function uses only
 01 local data (hWndPE) that is the handle of the address of the PE Magic Signature found.
 
 Parameters:
 
    FilePointer:    Handle of the targeted file. On our case, it is the beginning of the file.
                    Upon this, it will look for the Dos Magis Signatures (MZ), and PE magic signatures to look for
                    eventual errors.
 
    FileSize:       Size of the file in bytes.

 
 Return Values: On sucess, this function will return the address of the PE Magic signature to eax.
                On failure, it will return &FALSE.
                
 Error Messages:  This function uses the following constants that represents the error messages

;;

; Error Messages

[PE_ALERT 01]
[PE_MZNOTFOUND 02]
[PE_SIZEALLOCERR 03]
[PE_NESIGFOUND 04]
[PE_SIGNOTFOUND 05]
[PE_SIZEERR 06]
[PE_32BITSERR 07]
[PE_SECALIGNERR 08]
[PE_NOSECERR 09]
[PE_SECRAWSIZEERR 10]
[PE_NOT_DEFAULT_PROCESSOR_TYPE 11]
[PE_ENTRYPOINT_ERROR 12]


[UserDosHeaderSize: D$ 0]

Proc CheckPE_MZ:
    Arguments @FilePointer, @FileSize, @pOutPEAddr
    Local @hWndPE, @PEEnd
    Uses edx, esi

    mov eax D@FilePointer ; eax = 01EE0000
    mov edx eax
    mov D@PEEnd edx | add edx D@FileSize | mov D@PEEnd edx

    ; Stage01 - Analyse the size of file and compare it with the minimum size required for a regular PE

    .If D@FileSize < SizeOf_DosHeader+SizeOf_PeHeader+SizeOf_SectionsHeaders

        call PECheck_ErrManager PE_ALERT

            On eax = &FALSE, ExitP
            mov eax D@FilePointer;D$UserPeStart ; restore the file pointer to eax

    .End_If

    ; Stage02 - Look for the Magic DOS signature (MZ)

    .If W$eax <> &IMAGE_DOS_SIGNATURE; 'MZ'
        If W$eax <> &IMAGE_DOS_SIGNATURE_OLD_VERSION ; 'ZM' Old MZ header form...Rare, but You may find this sometimes.
            call PECheck_ErrManager PE_MZNOTFOUND | ExitP
        End_If
    .End_If


    call FixMzParagraphsNumber D@FilePointer, D@FileSize, D@pOutPEAddr;DisPeTagPointer;D$UserPeLen
    On eax = &FALSE, ExitP
    mov esi eax

    ..If esi = &FALSE

        ; Stage03a - See if we are not dealing with an 16 Bits executable.

        If W$eax = &IMAGE_OS2_SIGNATURE;, D$eax <> &IMAGE_OS2_SIGNATURE_OLD_VERSION; 'NE' 0 0
             call PECheck_ErrManager PE_NESIGFOUND | ExitP
        Else_If W$eax = &IMAGE_OS2_SIGNATURE_OLD_VERSION; 0 0 'EN' Old NE header form...Rare, but You may find this sometimes.
             call PECheck_ErrManager PE_NESIGFOUND | ExitP
        Else
            call PECheck_ErrManager PE_SIGNOTFOUND | ExitP
        End_If

    ..Else_If esi >= D@PEEnd ;D$UserPeEnd    ; Stage03 - size mismatch of allocated bytes of The PE Signature

        call PECheck_ErrManager PE_SIZEALLOCERR | ExitP

    ..Else ; Found PE, Start checking for it´s integrity

        ; The real PE Offset Address of the Magic Signature
        call CheckPEProcessorType W$esi+PeHeader.FileHeader.MachineDis
        On eax = &FALSE, ExitP
        call CheckPEIntegrity esi, D@FilePointer, D@FileSize
    ..End_If


EndP

_________________________________________________________________________________________

Proc SetUserDosHeaderSize:
    Arguments @FilePointer, @PEPointer, @Output
    uses eax, edi

    mov eax D@PEPointer
    sub eax D@FilePointer
    mov edi D@Output
    mov D$edi eax

EndP
_________________________________________________________________________________________

Proc CheckPEProcessorType:
    Arguments @PeHeaderSignature
    Uses esi

    mov eax &TRUE

    movzx esi W@PeHeaderSignature
    If esi <> &IMAGE_FILE_MACHINE_I386
        call PECheck_ErrManager PE_NOT_DEFAULT_PROCESSOR_TYPE
    End_If

EndP
_________________________________________________________________________________________

Proc CheckPEIntegrity:
    Arguments @PeHeaderSignature, @FilePointer, @FileSize
    Local @hWndPE
    Uses ecx, edx, esi


    mov eax D@PeHeaderSignature
    ; Stage05 - Check mismatch between the FileSize and the Size in bytes of the PE HEader.

    ; From now on, eax is the Address of PE Signature

    move D@hWndPE eax
    mov ecx eax           ; ecx is the address of eax (PE signature)
    sub ecx D@FilePointer ; i subtract the address (in ecx) with the address of UserPeStart (MZ header)
                          ; this will give the real size in bytes of the start of the PE Header.
    add ecx SizeOf_PeHeader+SizeOf_SectionsHeaders;0120 Point to the end of the 1st section Header

    If D@FileSize < ecx ; If the filesize is smaller then the bytes founds for the PE Signature, do error check.
                        ; It happens when a PE is broken exactly on the beginning of the PE Structure. I mean, the total size of the
                        ; file is only formed by the DOS Header and at the maximum, the PE HEader.
        call PECheck_ErrManager PE_SIZEERR | ExitP
    End_If

    ; Stage06 - Look for the Coff Magic Signature (32 bits).

    ;mov eax D@hWndPE

    If W$eax+PeHeader.OptionalHeader.MagicDis <> &IMAGE_NT_OPTIONAL_HDR32_MAGIC
        call PECheck_ErrManager PE_32BITSERR
        On eax = &FALSE, ExitP
        mov eax D@hWndPE;D$UserPeStart ; restore the file pointer to eax
    End_If

    ; Stage07 - Calculate the minimum alignment boundary required for the sections, and check if there is some section present.

    ; The arithmetic formula for this computation is:
    ; Alignment = (FileSize-PE Bytes)/32 - 32.
    ;
    ; We have 02 ways of calculate this. The 1st one is commented to you see what is being doing.
    ; The second one is not used here, because it have small gaps due to the lack of the remainder.
    ; It avoids having to push and pop eax, don't use mul opcode, and is smaller and readable, but...
    ; On big files, the 2nd method is more accurated, thought ;)
    ;
    ; For you have an idea. A file with 1490 bytes can not contain any section. 1500 bytes = 01 section etc etc
    ; (I mean, accordying to the commented method). On the usade method, < 1024 bytes = 0 sections.
;;
    ; The 2nd method is:
    
    mov ecx D$UserDosHeaderSize
    add ecx SizeOf_PeHeader
    mov edx D@FileSize
    sub edx ecx
    shr edx 5
    sub edx 32
;;


    ; The 1st method is:

    mov ecx eax ; ecx = address of the PE header
    add ecx SizeOf_PeHeader+SectionsHeaders.Name1Dis; ecx is now the address of the Section name.
                                                    ; (not needed to add SectionsHeaders.Name1Dis because it's size is 0,
                                                    ; but it is better for you to understand)
    mov edx D@FilePointer ; edx is now the address of the beginning of the file (MZ)
    sub edx ecx ; edx = FilePointer - Addr of PE => 0 - PE Bytes
    add edx D@FileSize ; edx = FilePointer - Addr of PE + FileSize => FileSize - PE Bytes

    push eax ; backup the value in eax to we don't loose the pointer to PE Signature
        mov eax 0CCCCCCCD ; ---> reminder
        mul edx
        shr edx 05 ; divide by 32 bits.
    pop eax ; restore the pointer at eax

    xor ecx ecx
    mov cx W$eax+PeHeader.FileHeader.NumberOfSectionsDis

    If ecx > edx ; The total alignment boundary is bigger then the amount of sections? No...do error check.

            call PECheck_ErrManager PE_SECALIGNERR
            On eax = &FALSE, ExitP
            mov eax D@hWndPE; restore the file pointer to eax

    Else_If ecx <= 0

            call PECheck_ErrManager PE_NOSECERR | ExitP

    End_If


    ; Stage08 - Look for the size of the section.

        Do
            .If D$eax+SizeOf_PeHeader+SectionsHeaders.SizeOfRawDataDis <> 0
                mov esi D@FileSize
                    If D$eax+SizeOf_PeHeader+SectionsHeaders.PointerToRawDataDis > esi
                        call PECheck_ErrManager PE_SECRAWSIZEERR | ExitP
                    End_If
            .End_If

            add eax SizeOf_SectionsHeaders ; will point to the next section at SectionsHeaders.SizeOfRawData
            dec ecx

        Loop_Until ecx = 0

    mov eax D@hWndPE ; restore the file pointer to eax

    ; Stage09 - Calculate the EntryPoint Size Alignment.

    mov ecx D$eax+PeHeader.OptionalHeader.AddressOfEntryPointDis
    mov edx D$eax+PeHeader.OptionalHeader.SizeOfHeadersDis

    .If ecx >= edx

        call EntryPointSizeCheck ecx edx
        dec eax ; returned size

        If eax >= D@FileSize
            call PECheck_ErrManager PE_ENTRYPOINT_ERROR
            ExitP
        End_If
    .End_If

    mov eax D@hWndPE




EndP
_________________________________________________________________________________________

    ; Error Messages administrator. It is important to you keep the order of the messages.

Proc PECheck_ErrManager:
    Arguments @ErrFlag
    uses ecx, esi

    mov eax D@ErrFlag


    .If eax = PE_ALERT

        call 'USER32.MessageBoxA' D$hwnd {"ATTENTION !!!.
Are you sure you are loading a Executable File ?

The File you are trying to load is smaller then 352 Bytes. It is possible you are dealing with any kind of file, except a 
regular executable one.

If this is a executable file (.exe or .com, etc), you can be loading a VIRUS.

It is highly recommendable that you use an anti-virus 1st. Or just DON'T load this file.

YOU REALLY WANT TO CONTINUE ANALSYING THIS ?" 0} {"Error: Weird File. - Possible Virus" 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO

            If eax = &IDNO
                mov eax &FALSE
            End_If


    .Else_If eax = PE_MZNOTFOUND

        call 'USER32.MessageBoxA' D$hwnd {"The Image_Dos_Header Magic Signature (MZ) was not found.
    
This is NOT a win executable file." 0} {"Error: MZ Not Found." 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

        mov eax &FALSE

    .Else_If eax = PE_SIZEALLOCERR

        call 'USER32.MessageBoxA' D$hwnd {"The address of the PE signature founded for this file, is higher then the amount of allocated memory of it's size.
This is not a Portable Executable (PE) File." 0} {"Error: Size mismatch." 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

        mov eax &FALSE

    .Else_If eax = PE_NESIGFOUND

        call 'USER32.MessageBoxA' D$hwnd {"This file seems to be a seems to be a 16 bit executable file. At the present moment, only 32 bit files are supported." 0} {"Error: 16 Bits Found." 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

        mov eax &FALSE


    .Else_If eax = PE_SIZEALLOCERR

        call 'USER32.MessageBoxA' D$hwnd {"The address of the PE signature founded on this file, is higher then the amount of allocated memory of it's size.
This is not a Portable Executable (PE) File." 0} {"Error: Size mismatch." 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

        mov eax &FALSE

    .Else_If eax =  PE_SIGNOTFOUND

        call 'USER32.MessageBoxA' D$hwnd {"The Portable Executable Magic Signature (PE) was not found.
    
This is NOT a Win32 Portable Executable file." 0} {"Error: PE Not Found." 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

        mov eax &FALSE


    .Else_If eax =  PE_SIZEERR

        call 'USER32.MessageBoxA' D$hwnd {"This file seems to be broken. It seems to only contains the DOS header and the PE Structure (Maybe only part of it).

What happens is that this file ends exactly before the end of the PE header Structure.

If this was a Win32 PE File, it is deeply broken. It don't seems to contains any data, except for the DOS Header and the PE HEader.

Check the integrity of this file with an hexadecimal editor. (Also, would be wise use an Anti-Virus)." 0} {"Error: Incorrect Size of PE." 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

        mov eax &FALSE

    .Else_If eax =  PE_32BITSERR

        call 'USER32.MessageBoxA' D$hwnd {"This file is not a 32 Bits Portable Executable.

You are probably loading a 64 Bits file, and we didn't implement the routine to check for such files yet.

Or you maybe also loading a broken file. A regular 32 Bits PE, contains the following equates used on the Optional Header Magic member (IMAGE_OPTIONAL_HEADER structure):

IMAGE_NT_OPTIONAL_HDR32_MAGIC 010B : 32 Bits (The same equate as IMAGE_NT_OPTIONAL_HDR_MAGIC)
IMAGE_NT_OPTIONAL_HDR64_MAGIC 020B : 64 Bits

If this is a regular 32 Bits PE, this file somehow is broken on this structure member, but RosAsm is allowed to load this file correctly. (If this is the only error found, of course)

Do you want to continue loading this file ?" 0} {"Error: 32 Bits PE Failure" 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO

            If eax = &IDNO
                mov eax &FALSE
            End_If

    .Else_If eax =  PE_SECALIGNERR

        call 'USER32.MessageBoxA' D$hwnd {"ATTENTION !!!.

The sections on this file are Terribly bad aligned. The size of this file doesn't support the amount of sections specified on the PE Header.

The formula to calculate the necessary alignment of the sections is:

    Alignment = (FileSize-PE Bytes)/32 - 32.
    
    * FileSize : Size of the File.
    * PE Bytes : Amount of bytes from the beginning of the file untill the end of the PE Header (IMAGE_NT_HEADER structure).

Note: The Alignment can be translated as the minimum amount of sections that the PE can use.

Do you want to continue loading this file ?" 0} {"Error: PE section alignment Failure" 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO

            If eax = &IDNO
                mov eax &FALSE
            End_If


    .Else_If eax =  PE_NOSECERR

        call 'USER32.MessageBoxA' D$hwnd {"ATTENTION !!!.

This PE have no sections (or they are in a negative form). If this is a regular 32 Bits PE, this file somehow is broken on this structure member (PeHeader.FileHeader.NumberOfSectionsDis).

RosAsm can't continue analysing this file without you fix it with an proper hexadecimal editor, or you make sure the next time you loadi a PE File." 0} {"Error: PE Section Check Failure" 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

                mov eax &FALSE


    .Else_If eax =  PE_SECRAWSIZEERR

        call 'USER32.MessageBoxA' D$hwnd {"ATTENTION !!!.

The size of a sections can't be bigger then the size of the file.

If this is a regular 32 Bits PE, this file somehow is broken on this structure member (SectionsHeaders.SizeOfRawData).

RosAsm can't continue analysing this file without you fix it with an proper hexadecimal editor, or you make sure the next time you loadi a PE File." 0} {"Error: PE Section Size Failure" 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

                mov eax &FALSE

    .Else_If eax = PE_NOT_DEFAULT_PROCESSOR_TYPE

        call 'USER32.MessageBoxA' D$hwnd {"ATTENTION !!!.
Sorry, RosAsm only disassemble x86 instructions." 0} {"Error: Incorrect Processor Type" 0} &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

                mov eax &FALSE

    .Else_If eax = PE_ENTRYPOINT_ERROR

        call 'USER32.MessageBoxA' D$hwnd, {"ATTENTION !!!.
I coulnd´t find the EntryPoint on this file" 0}, {"Error: EntryPoint not found" 0}, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION

                mov eax &FALSE


    .End_If

EndP

_________________________________________________________________________________________

; return value in eax
Proc EntryPointSizeCheck:
    Arguments @EntryPoint, @HdrSize
    Local @Counter
    Uses edx, esi, ecx, ebx, edi

    mov edx D@HdrSize

    If D@EntryPoint < edx
        mov eax D@EntryPoint
        ExitP
    End_if

    xor esi esi
    mov si W$eax+PeHeader.FileHeader.NumberOfSectionsDis

    ...If esi > 0

        mov ebx 1

        Do

        mov edi D$eax+SizeOf_PeHeader+SectionsHeaders.VirtualAddressDis

        ..If D@EntryPoint >= edi
            mov ecx D$eax+SizeOf_PeHeader+SectionsHeaders.SrcMiscVirtualSizeDis

            If ecx = 0
                mov ecx D$eax+SizeOf_PeHeader+SectionsHeaders.SizeOfRawDataDis
            End_If

            mov edx D@EntryPoint
            sub edx edi ; EP = EP - VirtualAddress. Note: EP = EntryPoint

            .If edx <= ecx ; (EP - VirtualAddress) <= SrcMiscVirtualSize or SizeOfRawData

                If ebx >= esi;D@Counter = esi;W$eax+PeHeader.FileHeader.NumberOfSectionsDis ; Are we in the last section ? Yes, jmp over and calculate it
                ;If ebx <> esi;D@Counter = esi;W$eax+PeHeader.FileHeader.NumberOfSectionsDis ; Are we in the last section ? Yes, jmp over and calculate it
                    ;add eax SizeOf_SectionsHeaders                     ; No....go to next section and calculate it.
                Else
                    ;jmp L1>;add eax SizeOf_SectionsHeaders
                    mov eax eax
                End_If
                ;edx is the counter

                mov ecx D$eax+SizeOf_PeHeader+SectionsHeaders.PointerToRawDataDis
                mov edx D$eax+SizeOf_PeHeader+SectionsHeaders.VirtualAddressDis
                sub ecx edx ; PointerToRawDataDis - VirtualAddress
                add ecx D@EntryPoint ; PointerToRawDataDis - VirtualAddress + EP
                mov eax ecx ; eax = PointerToRawDataDis - (VirtualAddress - EP)
                ExitP

            .End_If

        ..End_If
            add eax SizeOf_SectionsHeaders
            inc ebx
            dec esi

        Loop_Until esi = 0;ebx >= esi;D@Counter >= esi;esi = 0 ; counting the sections

    ...End_If

    xor eax eax

EndP


___________________________________________________________________________________________
;;
  Some PEs may have a wrong Paragraphs Number, in the MZ header. Fix It now to the
  correct values, because the Routines called for loading the various Resources
  make use of those values to point, through the various PE Headers, to the Resources
  Tree.
  
  We also get a Pointer to the Pe Tag (a Pointer to 'PE', in the PE header, as you
  can see, for example, at 'PeHeader'.
  
  A DOS 'Paragraph' is 16 Bytes. See Records at 'DosHeader':
  
  * At DosHeader+8 > Size of the Dos header in Paragraph units.
  

Return Values:

When the function is suceeded, eax = the pointer to PE Adrress
When the function fails eax = 0
In all cases, esi returns the pointer to the address of the possible PE/NE signatures

When he function suceeds, eax = Pointer to Possible PE/NE signature

Arguments 
    FilePointer: A pointer to the starting address that contains the file. (The 'MZ' signature)
    
    FileLen:    The len of the file
    
    pOutPEAddr: Pointer to a Buffer that will holds the Address of the PE signature

;;

[DisPeTagPointer: D$ 0]

Proc FixMzParagraphsNumber:
    Arguments @FilePointer, @FileLen, @pOutPEAddr
    Uses edx, edi, esi

    mov edi D@pOutPEAddr
    mov D$edi 0;DisPeTagPointer 0
   ; Standard manner: parag. size of dos header end > PE header address:
    mov esi D@FilePointer | movzx eax W$esi+DosHeader.e_cparhdrDis | shl eax 4 | sub eax 4
    If eax < D@FileLen
        add esi D$esi+eax
    Else
        mov eax 0
    End_If

    ..If eax <> 0
        mov edx D@FilePointer | add edx D@FileLen | sub edx 4
        .If esi < edx
            If D$esi = &IMAGE_NT_SIGNATURE; 'PE'
                mov D$edi esi
                mov eax esi | ExitP ; (No fix needed, in that case)
            Else_If D$esi = &IMAGE_NT_SIGNATURE_OLD_VERSION; 'EP' Old PE signature (The reverse of 'PE' 0 0)
                mov D$edi esi
                mov eax esi | ExitP ; (No fix needed, in that case)
            Else_If W$esi = &IMAGE_OS2_SIGNATURE ; 'NE' signature found
                ;xor eax eax | ExitP
                call PECheck_ErrManager PE_NESIGFOUND | ExitP
            Else_If W$esi = &IMAGE_OS2_SIGNATURE_OLD_VERSION ; 'EN' Old NE Signature (The reverse of 'NE')
                ;xor eax eax | ExitP
                call PECheck_ErrManager PE_NESIGFOUND | ExitP
            End_If
        .End_If
    ..End_If

  ; Fix needed for these two other methods:
    call GetPeTagMethod2 D@FilePointer
    ;On eax = &FALSE, jmp L0>
    ..If eax <> &FALSE
        mov edx D@FilePointer | add edx D@FileLen | sub edx 4
        .If eax < edx
            If D$eax = &IMAGE_NT_SIGNATURE
                mov D$edi eax | jmp L1>
            Else_If D$eax = &IMAGE_NT_SIGNATURE_OLD_VERSION
                mov D$edi eax | jmp L1>
            Else_If W$eax = &IMAGE_OS2_SIGNATURE
                ;xor eax eax | ExitP
                call PECheck_ErrManager PE_NESIGFOUND | ExitP
            Else_If W$eax = &IMAGE_OS2_SIGNATURE_OLD_VERSION
                ;xor eax eax | ExitP
                call PECheck_ErrManager PE_NESIGFOUND | ExitP
            End_If
        .End_If
    ..End_If

    ; Scan the 1st 516 bytes
    ;If D@FileLen >= 516
    call GetPeTagMethod3 D@FilePointer, 516
    ;On eax = &FALSE, ExitP
    If eax <> &FALSE
        mov D$edi eax | ExitP
    End_If
    ;End_If
L1:

    call GetPeTagMethod4 D@FilePointer
    On eax = &FALSE, ExitP
    mov D$edi eax

;  ; Test: Make sure it is now good for the default internal method:
;    mov esi D$UserPeStart | movzx eax W$esi+8 | shl eax 4 | sub eax 4 | add esi D$esi+eax
;    On D$esi <> 'PE', jmp DisFail

; With such an error, the Debugger fails, when compiling RosAsm with RosAsm...
; mov esi 0 | On D$esi <> 'PE', jmp DisFail
EndP


  ; Method 2 for searching the PE Tag:
Proc GetPeTagMethod2:
    Arguments @FilePointer
    uses esi
  ; If Origin >= 040 > PE header adress at 03C
    mov eax D@FilePointer | add eax DosHeader.e_lfarlcDis
    If W$eax >= 040
        sub eax DosHeader.e_lfarlcDis | add eax DosHeader.e_lfanewDis | movzx esi W$eax | add esi D@FilePointer
        mov eax esi
    Else
        xor eax eax
    End_If

EndP

  ; Method 3. Stupid desesparated search:
Proc GetPeTagMethod3:
    Arguments @FilePointer, @ScanSize
    uses esi, ecx

    xor eax eax
    mov ecx D@ScanSize
    On ecx < 4, ExitP
    sub ecx 4
    mov esi D@FilePointer;, ecx 512

    Do
        If D$esi = &IMAGE_NT_SIGNATURE
            mov eax esi | ExitP
        Else_If D$esi = &IMAGE_NT_SIGNATURE_OLD_VERSION
            mov eax esi | ExitP
        Else_If W$esi = &IMAGE_OS2_SIGNATURE
            ;xor eax eax | ExitP
            call PECheck_ErrManager PE_NESIGFOUND | ExitP
        Else_If W$esi = &IMAGE_OS2_SIGNATURE_OLD_VERSION
            ;xor eax eax | ExitP
            call PECheck_ErrManager PE_NESIGFOUND | ExitP
        End_If

        xor eax eax
        inc esi
        dec ecx
    Loop_Until ecx = 0
    ; If we are here, eax will always return FALSE because we didn´t found the Signature
    call PECheck_ErrManager PE_SIGNOTFOUND

EndP


  ; Method 4. For very weird PE with very small Dos Header
Proc GetPeTagMethod4:
    Arguments @FilePointer
    Uses esi, ecx, edx

    mov ecx 2
    mov esi D@FilePointer
    movzx edx W$esi+DosHeader.e_cparhdrDis ; For restoring purposes

    mov W$esi+DosHeader.e_cparhdrDis cx;, D$edi+(16+12) ecx
    add esi (16*2)
    mov eax D$esi  ; On this particular case, edi will refer to DosHeader.e_lfanew
    add eax D@FilePointer


    On D$eax = &IMAGE_NT_SIGNATURE, ExitP
    On D$eax = &IMAGE_NT_SIGNATURE_OLD_VERSION, ExitP

    If W$eax = &IMAGE_OS2_SIGNATURE
        call PECheck_ErrManager PE_NESIGFOUND
    Else_If W$eax = &IMAGE_OS2_SIGNATURE_OLD_VERSION
        call PECheck_ErrManager PE_NESIGFOUND
    Else
        call PECheck_ErrManager PE_SIGNOTFOUND
    End_If

    ;On D$eax = &IMAGE_NT_SIGNATURE, ExitP
    ;On D$eax = &IMAGE_NT_SIGNATURE_OLD_VERSION, ExitP
    ;xor eax eax

    ; restore the old paragraph Dos Header in case of failures
    mov esi D@FilePointer | mov W$esi+DosHeader.e_cparhdrDis dx

EndP


_________________________________________________________________________________________





;;
 these 2 following stubs are used to create new PE. only 'Labelled' values are
 modified according with source values. Main filling work is done by the 'Build...'
 routines.

 These two stubs must remain all in one single data set (prevent from RosAms data
 alignement).

00000000: Dos exe file header stub:
;;

 ; ------------ DOS HEADER -------------------

; This is the IMAGE_DOS_HEADER structure

[DosHeader:
 DosHeader.e_magic: B$ 'MZ' ; &IMAGE_DOS_SIGNATURE ; dos exe signature. The Magic number ("MZ", but we can find an old header written as "ZM")
                            ; It can be formed by oen of these equates:  &IMAGE_DOS_SIGNATURE &IMAGE_DOS_SIGNATURE_OLD_VERSION
 DosHeader.e_cblp: W$ 090 ; Bytes in Last Page
 DosHeader.e_cp: W$ 03 ; Total Pages in file.
 DosHeader.e_crlc: W$ 0 ; Number of relocations adresses
 DosHeader.e_cparhdr: W$ 4 ; this dos header size (16*4). Size of header in paragraphs
 DosHeader.e_minalloc: W$ 0 ; min size. Minimum extra paragraphs needed

 ; Note: If you want to make your PE real small, all you have to do is remove the members below untill DOS_STUB, DOS_StubLenght.
 ; and also you must assign "DosHeader.e_cp" and "DosHeader.e_cparhdr" to 0.
 ; Be aware that if you do this, your file may not run in all OSes.
 ; You will also have to make some modifications in RosAsm, but for your own safety, we won´t tell you how ;)

 DosHeader.e_maxalloc: W$ 0FFFF ; max size. Maximum extra paragraphs needed
 DosHeader.e_ss: W$ 0 ; SS reg. value at run time
 DosHeader.e_sp: W$ 0B8 ; SP reg. value at run time
 DosHeader.e_csum: W$ 0 ; checksum for header (Can be Zero)
 DosHeader.e_ip: W$ 0 ; IP reg. value
 DosHeader.e_cs: W$ 0 ; start of Cseg in file
 DosHeader.e_lfarlc: W$ 040 ; File address of relocation table. In fact it is a pointer to DOS_STUB.
                            ; The computation for this is: DOS_STUB-DosHeader
                             ; Guga Fix it here: StoreDataRef
 DosHeader.e_ovno: W$ 0 ; overlay default
 DosHeader.e_res_01: W$ 0  ; This is the 1st array of 04 from e_res[4] Reserved words
 DosHeader.e_res_02: W$ 0
 DosHeader.e_res_03: W$ 0
 DosHeader.e_res_04: W$ 0
 DosHeader.e_oemid: W$ 0 ; OEM identifier (for e_oeminfo)
 DosHeader.e_oeminfo: W$ 0 ; OEM information; e_oemid specific
 DosHeader.e_res2_01: W$ 0 ; This is the 1st array of 10 from e_res2[10]. Reserved words
 DosHeader.e_res2_02: W$ 0
 DosHeader.e_res2_03: W$ 0
 DosHeader.e_res2_04: W$ 0
 DosHeader.e_res2_05: W$ 0
 DosHeader.e_res2_06: W$ 0
 DosHeader.e_res2_07: W$ 0
 DosHeader.e_res2_08: W$ 0
 DosHeader.e_res2_09: MyCheckSum: W$ 0 ; 30 Our checksum to prevent viruses infection.
 DosHeader.e_res2_10: W$ 0
 DosHeader.e_lfanew: PeHeaderPointer: D$ PeHeader-DosHeader;080;PeHeader ; File adress of win header (new exe header)
; GUGA check here StartScan and all points to the old 080 byte to point to the correct value.

;;

------------ MS-DOS Stub -------------------

The MS-DOS Stub is a valid application that runs under MS-DOS and is placed at the front of the .EXE image.
The linker places a default stub here, which prints out the message "This program cannot be run in DOS mode"
when the image is run in MS-DOS. The user can specify another stub by using the /STUB linker option.

At location 0x3c, the stub has the file offset to the Portable Executable (PE) signature.

This information enables Windows NT to properly execute the image file, even though it has a DOS Stub.

This file offset is placed at location 0x3c during linking.

The stub is actually a DOS program, usually like this:

             push cs      ; Point the data segment to the code segment, since
             pop  ds      ; we're putting the data after the code to save space.
            
             mov  dx message ; Load pointer to the string for the call.
             mov  ah 09              ; 9 is the print argument for int 21h.
             int  021                ; The DOS interrupt.
             
             mov  ah 04C            ; 4C is the exit argument for int 21h
             int  021
 
            ; Put our string here
             [message: B$ '!This program cannot be run in DOS mode',0Dh,0Ah,'$']
 
             A little explanation may be required:
            
             0Dh is the 'Carriage return' ASCII code.
             0Ah is the 'Line feed' ASCII code.
             '$' is the string-terminator in DOS (like 0 is in Windows and other C based OSes)
            
;;


DOS_STUB: B$  0E, 01F, 0BA, 0E, 00, 0B4, 09, 0CD, 021, 0B8, 01, 04C, 0CD, 021
; push cs // pop ds // mov dx 0E // mov ah 09 // int 021 // mov ax 4C01 // int 021
; 18
;B$ 'Spindoz 32 spit PEfile made wiz RosAsm Assembler.$'


StubMsg: B$ "---------32 Bit Error Message ------------------
Sorry, this file must run under 32 Bit only.

RosAsm Assembler Development Team.
http://www.rosasm.org.$" 0
DOS_StubLenght: D$ DOS_StubLenght-DOS_STUB ; This lenght isn't necessary, but you may keep it if you like ;)

; If you change the above message you must also change at CheckStubMsg
; 50+18+30 = 98
; if you modify upper string you must absolutely keep the same lenght. (Not anymore...The message can be on any size)



; ------------ PE HEADER -------------------

; This is the structure IMAGE_NT_HEADERS (PeHeader + OptionalHeader + DataDirectory)

PeHeader:
     PeHeader.Signature: B$ 'PE', 0, 0 ; signature.  Equates are &IMAGE_NT_SIGNATURE &IMAGE_NT_SIGNATURE_OLD_VERSION (Old version..don´t use it)
     PeHeader.FileHeader.Machine: W$ &IMAGE_FILE_MACHINE_I386   ; 386 and more - CPU Type
                                                                ; equates used:
                                                                ; &IMAGE_FILE_MACHINE_ALPHA 0184
                                                                ; &IMAGE_FILE_MACHINE_ALPHA64 0284
                                                                ; &IMAGE_FILE_MACHINE_ARM 01C0
                                                                ; &IMAGE_FILE_MACHINE_AXP64 0284
                                                                ; &IMAGE_FILE_MACHINE_CEF 0C0EF
                                                                ; &IMAGE_FILE_MACHINE_I386 014C
                                                                ; &IMAGE_FILE_MACHINE_I486 014D
                                                                ; &IMAGE_FILE_MACHINE_I586 014E
                                                                ; &IMAGE_FILE_MACHINE_IA64 0200
                                                                ; &IMAGE_FILE_MACHINE_M68K 0268
                                                                ; &IMAGE_FILE_MACHINE_MIPS16 0266
                                                                ; &IMAGE_FILE_MACHINE_MIPSFPU 0366
                                                                ; &IMAGE_FILE_MACHINE_MIPSFPU16 0466
                                                                ; &IMAGE_FILE_MACHINE_POWERPC 01F0
                                                                ; &IMAGE_FILE_MACHINE_R10000 0168
                                                                ; &IMAGE_FILE_MACHINE_R3000 0162
                                                                ; &IMAGE_FILE_MACHINE_R6000 0163
                                                                ; &IMAGE_FILE_MACHINE_R4000 0166
                                                                ; &IMAGE_FILE_MACHINE_SH3 01A2
                                                                ; &IMAGE_FILE_MACHINE_SH3E 01A4
                                                                ; &IMAGE_FILE_MACHINE_SH4 01A6
                                                                ; &IMAGE_FILE_MACHINE_THUMB 01C2
                                                                ; &IMAGE_FILE_MACHINE_UNKNOWN 0
                                                                ; &IMAGE_FILE_MACHINE_WCEMIPSV2 0169

     PeHeader.FileHeader.NumberOfSections: NumberOfSections: W$ 04 ; 4 sections (code, data, import, resource) not 5...
     PeHeader.FileHeader.TimeDateStamp: D$ 0 ; time and date stamp
     PeHeader.FileHeader.PointerToSymbolTable: D$ 0 ; pointer to symbol table offset (for debug)
     PeHeader.FileHeader.NumberOfSymbols: D$ 0 ; number of symbol (Sym Table size)
     PeHeader.FileHeader.SizeOfOptionalHeader: W$ 0E0 ; size of 'optional header'
     PeHeader.FileHeader.Characteristics: PeHeaderCharacteristics: W$ 00100001111    ; characteristics
                                                            ; bit 0 > 1 > reloc. infos not there
                                                            ; bit 1 > 1 > Runable
                                                            ; bit 2 > 1 > no line number for debug
                                                            ; bit 3 > 1 > no bebug symbol
                                                            ; others : unknown

                                                            ; Equates used (in combination):
                                                            ; &IMAGE_FILE_32BIT_MACHINE 0100
                                                            ; &IMAGE_FILE_AGGRESIVE_WS_TRIM 010
                                                            ; &IMAGE_FILE_BYTES_REVERSED_HI 08000
                                                            ; &IMAGE_FILE_BYTES_REVERSED_LO 080
                                                            ; &IMAGE_FILE_DEBUG_STRIPPED 0200
                                                            ; &IMAGE_FILE_DLL 02000
                                                            ; &IMAGE_FILE_EXECUTABLE_IMAGE 02
                                                            ; &IMAGE_FILE_LARGE_ADDRESS_AWARE 020
                                                            ; &IMAGE_FILE_LINE_NUMS_STRIPPED 04
                                                            ; &IMAGE_FILE_LOCAL_SYMS_STRIPPED 08
                                                            ; &IMAGE_FILE_RELOCS_STRIPPED 01
                                                            ; &IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP 0400
                                                            ; &IMAGE_FILE_SYSTEM 01000
                                                            ; &IMAGE_FILE_UP_SYSTEM_ONLY 04000

                                                            ; Our uses a combination of:
                                                            ; &IMAGE_FILE_32BIT_MACHINE
                                                            ; &IMAGE_FILE_EXECUTABLE_IMAGE
                                                            ; &IMAGE_FILE_LINE_NUMS_STRIPPED
                                                            ; &IMAGE_FILE_RELOCS_STRIPPED
                                                            ; &IMAGE_FILE_LOCAL_SYMS_STRIPPED


; This is the structure _IMAGE_OPTIONAL_HEADER

     PeHeader.OptionalHeader.Magic: W$ &IMAGE_NT_OPTIONAL_HDR32_MAGIC
                                            ; referred as 'magic' (Coff)...PE32 = 010B ; PE32+ = 020B
                                            ; The equates are:
                                            ; &IMAGE_NT_OPTIONAL_HDR32_MAGIC 010B (The same as &IMAGE_NT_OPTIONAL_HDR_MAGIC)
                                            ; &IMAGE_NT_OPTIONAL_HDR64_MAGIC 020B
     PeHeader.OptionalHeader.MajorLinkerVersion: B$ 3 ; Major linker version
     PeHeader.OptionalHeader.MinorLinkerVersion: B$ 0 ; Minor linker version
     PeHeader.OptionalHeader.SizeOfCode: AppCodeSize: D$ 0 ; size of code (.text section)
     PeHeader.OptionalHeader.SizeOfInitializedData: AppAllDataSize: D$ 0 ; size of initialized data (.data + .rsrc+... + .reloc)
     PeHeader.OptionalHeader.SizeOfUninitializedData: D$ 0 ; size of uninitialised data
     PeHeader.OptionalHeader.AddressOfEntryPoint: AppRVAentryPoint: D$ 0 ; RVA entry point adress (414h in RDNrect file)
     PeHeader.OptionalHeader.BaseOfCode: AppBaseOfCode: D$ 0 ; RVA Base of code (0400 in file)
     PeHeader.OptionalHeader.BaseOfData: SHAppBaseOfData: D$ 0 ; RVA Base of data ('SH' because one more 'AppBaseOfData' down there)
     PeHeader.OptionalHeader.ImageBase: ImageBase: D$ 0400000 ; image base (linker base default)
     PeHeader.OptionalHeader.SectionAlignment: D$ PageSize ; sections alignement
     PeHeader.OptionalHeader.FileAlignment: D$ 0200 ; file alignement
     PeHeader.OptionalHeader.MajorOperatingSystemVersion: W$ 4 ; Major OS version
     PeHeader.OptionalHeader.MinorOperatingSystemVersion: W$ 0 ; Minor OS version
     PeHeader.OptionalHeader.MajorImageVersion: W$ 1 ; Major image version
     PeHeader.OptionalHeader.MinorImageVersion: W$ 0 ; Minor image version
     PeHeader.OptionalHeader.MajorSubsystemVersion: W$ 4 ; Major sub system version
     PeHeader.OptionalHeader.MinorSubsystemVersion: W$ 0 ; Minor sub system version. For windows NT v3.10
     PeHeader.OptionalHeader.Win32VersionValue: D$ 0 ; reserved
     PeHeader.OptionalHeader.SizeOfImage: AppRVAimageSize: D$ 0 ; RVA image size
     PeHeader.OptionalHeader.SizeOfHeaders: D$ 0400 ; headers size. Combined size of MS-DOS stub, PE Header, and section headers rounded up to a multiple of FileAlignment.
     PeHeader.OptionalHeader.CheckSum: CheckSum: D$ 0 ; checksum (works when zero)
     PeHeader.OptionalHeader.Subsystem: SubSystem: W$ &IMAGE_SUBSYSTEM_WINDOWS_GUI ; sub system. Equates are:
                                                         ; &IMAGE_SUBSYSTEM_EFI_APPLICATION 0A
                                                         ; &IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER 0B
                                                         ; &IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER 0C
                                                         ; &IMAGE_SUBSYSTEM_NATIVE 01
                                                         ; &IMAGE_SUBSYSTEM_NATIVE_WINDOWS 08
                                                         ; &IMAGE_SUBSYSTEM_OS2_CUI 05
                                                         ; &IMAGE_SUBSYSTEM_POSIX_CUI 07
                                                         ; &IMAGE_SUBSYSTEM_UNKNOWN 0
                                                         ; &IMAGE_SUBSYSTEM_WINDOWS_CE_GUI 09
                                                         ; &IMAGE_SUBSYSTEM_WINDOWS_CUI 03
                                                         ; &IMAGE_SUBSYSTEM_WINDOWS_GUI 02

     PeHeader.OptionalHeader.DllCharacteristics: DllCharacteristics: W$ 0   ; DllCharacteristics:
                                                        ; 0001h - Per-Process Library Initialization
                                                        ; 0002h - Per-Process Library Termination
                                                        ; 0004h - Per-Thread Library Initialization
                                                        ; 0008h - Per-Thread Library Termination
                                                        ; Equates are:
                                                        ; &IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_INIT 01
                                                        ; &IMAGE_DLLCHARACTERISTICS_PPROCESS_LIB_TERM 02
                                                        ; &IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_INIT 04
                                                        ; &IMAGE_DLLCHARACTERISTICS_PTHREAD_LIB_TERM 08
                                                        ; &IMAGE_DLLCHARACTERISTICS_NO_BIND 0800
                                                        ; &IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE 08000
                                                        ; &IMAGE_DLLCHARACTERISTICS_WDM_DRIVER 02000

     PeHeader.OptionalHeader.SizeOfStackReserve: AppStackMax: D$ 0100000 ; stack max
     PeHeader.OptionalHeader.SizeOfStackCommit: AppStackMin: D$ 01000 ; stack min
     PeHeader.OptionalHeader.SizeOfHeapReserve: AppHeapMax: D$ 0100000 ; heap max
     PeHeader.OptionalHeader.SizeOfHeapCommit: AppHeapMin: D$ 0 ; heap min
     PeHeader.OptionalHeader.LoaderFlags: D$ 0 ; loader flags. Obsolete
     PeHeader.OptionalHeader.NumberOfRvaAndSizes: D$ 0  ; number of possible entries in following section table (16 records)




; This is the structure _IMAGE_DATA_DIRECTORY

; First Dwords are the RVA adresses; second ones are the sizes
; In fact, not 'Base_of' but rather a pointer to the Import Directory.
; Some Linkers do not write the Import Directory at first place of .Import,
; but at second place (why do it simple when you can do i complicated ???...).

SectionTable:
     PeHeader.DataDirectory.Export: D$ 0  ; Export Table Adress (RVA)
     PeHeader.DataDirectory.ExportSize: D$ 0            ; Export Table Size
     PeHeader.DataDirectory.Import: AppBaseOfImport: D$ 0  ; Import Table Adress (RVA) (Directory only) (DirectoryTable-DosHeader)  ; See Import_Table_Adress
     PeHeader.DataDirectory.ImportSize: AppImportSize: D$ 0            ; Import Table Size
     PeHeader.DataDirectory.Resource: AppBaseOfRsrc: D$ 0  ; Resource Table Adress (RVA) ; See Resource_Structures
     PeHeader.DataDirectory.ResourceSize: AppRsrcSize: D$ 0            ; Resource Table Size
     PeHeader.DataDirectory.Exception: D$ 0  ; Exception Table Adress (RVA)
     PeHeader.DataDirectory.ExceptionSize: D$ 0            ; Exception Table Size
     PeHeader.DataDirectory.Certificate: D$ 0  ; Certificate Table Adress (RVA) - Security
     PeHeader.DataDirectory.CertificateSize: D$ 0            ; Certificate Table Size
     PeHeader.DataDirectory.Relocation: RelocSectionTable: D$ 0  ; Base Relocation Table Adress (RVA)
     PeHeader.DataDirectory.RelocationSize: D$ 0            ; Base Relocation Table Size
     PeHeader.DataDirectory.Debug: DebugDir: D$ 0  ; Debug Directory Adress (RVA)
     PeHeader.DataDirectory.DebugSize: D$ 0            ; Debug Directory Adress Size
     PeHeader.DataDirectory.Architecture: D$ 0  ; Architecture Specific Data Adress (RVA) . Copyright
     PeHeader.DataDirectory.ArchitectureSize: D$ 0            ; Architecture Specific Data Size
     PeHeader.DataDirectory.GPReg: D$ 0  ; Global Pointer Register Adress (RVA). machine values (mips gp and global ptr)
     PeHeader.DataDirectory.GPRegSize: D$ 0            ; Global Pointer Register Size
     PeHeader.DataDirectory.Thread: D$ 0  ; Thread Local Storage (TLS) Table Adress (RVA)
     PeHeader.DataDirectory.ThreadSize: D$ 0            ; Thread Local Storage (TLS) Table Size
     PeHeader.DataDirectory.ConfigTable: D$ 0  ; Load Configuration Table Adress (RVA) ; See Load_Config_Directory_Structures
     PeHeader.DataDirectory.ConfigTableSize: D$ 0            ; Load Configuration Table Size
     PeHeader.DataDirectory.BoundIAT: D$ 0  ; Bound Import Table Adress (RVA) ; See BoundIAT_Directory_Structures
     PeHeader.DataDirectory.BoundIATSize: D$ 0            ; Bound Import Table Size
     PeHeader.DataDirectory.IAT: AppSecondImport: D$ 0  ; Import Table Adress (RVA)
     PeHeader.DataDirectory.IATSize: AppSecondImportSize: D$ 0            ; Import Table Size
     PeHeader.DataDirectory.DelayID: D$ 0  ; Delay Import Descriptors Adress (RVA) ; See Delay_Import_Address_Table_Structures
     PeHeader.DataDirectory.DelayIDSize: D$ 0            ; Delay Import Descriptors Size
     PeHeader.DataDirectory.COM: D$ 0  ; COM+ Runtime Header Adress (RVA)
     PeHeader.DataDirectory.COMSize: D$ 0            ; COM+ Runtime Header Size
     PeHeader.DataDirectory.Reserved: D$ 0  ; Reserved Table Adress (RVA)
     PeHeader.DataDirectory.ReservedSize: D$ 0            ; Reserved Table Size


; This is the IMAGE_SECTION_HEADER structure
; In fact it is an array of XX sections ( IMAGE_SECTION_HEADER) defined by PeHeader.FileHeader.NumberOfSections

SectionsHeaders:

idataSectionHeader:         ; (import section)
     SectionHdr.Name1: B$ '.idata',0,0  ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionHdr.MiscPhysicalAddress: MiscVirtualSize: AppImportTrueSize: D$ 0   ; EndOfImport - uStartOfImport  true size (Virtual Size)
     SectionHdr.VirtualAddress: AppBaseOfImports: D$ 0      ; RVA
     SectionHdr.SizeOfRawData: AppImportAlignedSize: D$ 0   ; 200h+ImportExt (Physical File Size)
     SectionHdr.PointerToRawData: AppStartOfImport: D$ 0    ; idata ptr
     SectionHdr.PointerToRelocations: D$ 0
     SectionHdr.PointerToLinenumbers: D$ 0
     SectionHdr.NumberOfRelocations: W$ 0
     SectionHdr.NumberOfLinenumbers: W$ 0
     SectionHdr.Characteristics: D$ 0C0000040       ; Our is : readable, writable, initialised data.
                                                    ; The Equates are (in combination):
                                                    ; &IMAGE_SCN_ALIGN_1024BYTES 0B00000
                                                    ; &IMAGE_SCN_ALIGN_128BYTES 0800000
                                                    ; &IMAGE_SCN_ALIGN_16BYTES 0500000
                                                    ; &IMAGE_SCN_ALIGN_1BYTES 0100000
                                                    ; &IMAGE_SCN_ALIGN_2048BYTES 0C00000
                                                    ; &IMAGE_SCN_ALIGN_256BYTES 0900000
                                                    ; &IMAGE_SCN_ALIGN_2BYTES 0200000
                                                    ; &IMAGE_SCN_ALIGN_32BYTES 0600000
                                                    ; &IMAGE_SCN_ALIGN_4096BYTES 0D00000
                                                    ; &IMAGE_SCN_ALIGN_4BYTES 0300000
                                                    ; &IMAGE_SCN_ALIGN_512BYTES 0A00000
                                                    ; &IMAGE_SCN_ALIGN_64BYTES 0700000
                                                    ; &IMAGE_SCN_ALIGN_8192BYTES 0E00000
                                                    ; &IMAGE_SCN_ALIGN_8BYTES 0400000
                                                    ; &IMAGE_SCN_ALIGN_MASK 0F00000
                                                    ; &IMAGE_SCN_ALIGN_MASK_OLD 0100000
                                                    ; &IMAGE_SCN_CNT_CODE 020
                                                    ; &IMAGE_SCN_CNT_INITIALIZED_DATA 040
                                                    ; &IMAGE_SCN_CNT_UNINITIALIZED_DATA 080
                                                    ; &IMAGE_SCN_GPREL 08000
                                                    ; &IMAGE_SCN_LNK_COMDAT 01000
                                                    ; &IMAGE_SCN_LNK_INFO 0200
                                                    ; &IMAGE_SCN_LNK_NRELOC_OVFL 01000000
                                                    ; &IMAGE_SCN_LNK_OTHER 0100
                                                    ; &IMAGE_SCN_LNK_REMOVE 0800
                                                    ; &IMAGE_SCN_MEM_16BIT 020000
                                                    ; &IMAGE_SCN_MEM_DISCARDABLE 02000000
                                                    ; &IMAGE_SCN_MEM_EXECUTE 020000000
                                                    ; &IMAGE_SCN_MEM_FARDATA 08000
                                                    ; &IMAGE_SCN_MEM_LOCKED 040000
                                                    ; &IMAGE_SCN_MEM_NOT_CACHED 04000000
                                                    ; &IMAGE_SCN_MEM_NOT_PAGED 08000000
                                                    ; &IMAGE_SCN_MEM_PRELOAD 080000
                                                    ; &IMAGE_SCN_MEM_PURGEABLE 020000
                                                    ; &IMAGE_SCN_MEM_READ 040000000
                                                    ; &IMAGE_SCN_MEM_SHARED 010000000
                                                    ; &IMAGE_SCN_MEM_WRITE 080000000
                                                    ; &IMAGE_SCN_NO_DEFER_SPEC_EXC 04000
                                                    ; &IMAGE_SCN_SCALE_INDEX 01
                                                    ; &IMAGE_SCN_TYPE_NO_PAD 08


ResourceSectionHeader:         ; (resource section)
     SectionResHdr.Name1: B$ '.rsrc',0,0  ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionResHdr.MiscPhysicalAddress: ResMiscVirtualSize: AppRsrcTrueSize: D$ 0   ; EndOfResource-StartOfResource  true size
     SectionResHdr.VirtualAddress: AppBaseOfRsrcs: D$ 0      ; RVA
     SectionResHdr.SizeOfRawData: AppRsrcAlignedSize: D$ 0   ; 200h+ResourceExt
     SectionResHdr.PointerToRawData: AppStartOfRsrc: D$ 0    ; rsrc ptr
     SectionResHdr.PointerToRelocations: D$ 0
     SectionResHdr.PointerToLinenumbers: D$ 0
     SectionResHdr.NumberOfRelocations: W$ 0
     SectionResHdr.NumberOfLinenumbers: W$ 0
     SectionResHdr.Characteristics: D$ 040000040            ; readable initialised data


DataSectionHeader:
     SectionDataHdr.Name1: B$ '.data',0,0  ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionDataHdr.MiscPhysicalAddress: DataMiscVirtualSize: AppDataTrueSize: D$ 0   ; EndOfData-StartOfData  true size
     SectionDataHdr.VirtualAddress: AppBaseOfData: D$ 0      ; RVA
     SectionDataHdr.SizeOfRawData: AppDataAlignedSize: D$ 0   ; 200h+DataExt    aligned size
     SectionDataHdr.PointerToRawData: AppStartOfData: D$ 0    ; data ptr
     SectionDataHdr.PointerToRelocations: D$ 0
     SectionDataHdr.PointerToLinenumbers: D$ 0
     SectionDataHdr.NumberOfRelocations: W$ 0
     SectionDataHdr.NumberOfLinenumbers: W$ 0
     SectionDataHdr.Characteristics: DataCharacteristics: D$ 0C0000040 ; readable, writable, initialized data


CodeSectionHeader:  ; Code section header: (the four 'dummy' D$ and W$ are of no mean in EXE and DLL files)
     SectionCodeHdr.Name1: B$ '.text',0,0  ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionCodeHdr.MiscPhysicalAddress: CodeMiscVirtualSize: AppTrueCodeSize: D$ 0   ; true size of code in file
     SectionCodeHdr.VirtualAddress: AppCodeRVAoffset: D$ 0      ; RVA offset (aligned on 01000 boundary)
     SectionCodeHdr.SizeOfRawData: AppFileSizeOfCode: D$ 0   ; file aligned size of code (0200 aligned)
     SectionCodeHdr.PointerToRawData: AppStartOfCode: D$ 0    ; pointer to code (true first code in file - not entry point-)
     SectionCodeHdr.PointerToRelocations: D$ 0              ; dummy reloc ptr
     SectionCodeHdr.PointerToLinenumbers: D$ 0              ; dummy line number ptr
     SectionCodeHdr.NumberOfRelocations: W$ 0               ; dummy reloc number
     SectionCodeHdr.NumberOfLinenumbers: W$ 0               ; dummy number of line number
     SectionCodeHdr.Characteristics: CodeCharacteristics: D$ 060000020 ; readable, runable, code

ExportSectionHeader:        ;, if any:
     SectionExportHdr.Name1: B$ 0 #&IMAGE_SIZEOF_SHORT_NAME ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionExportHdr.MiscPhysicalAddress: ExportMiscVirtualSize: AppExpTrueSize: D$ 0   ; true size of export in file
     SectionExportHdr.VirtualAddress: AppBaseOfExp: D$ 0      ; RVA offset
     SectionExportHdr.SizeOfRawData: AppExpAlignedSize: D$ 0   ; file aligned size of export
     SectionExportHdr.PointerToRawData: AppStartOfExp: D$ 0    ; pointer to export
     SectionExportHdr.PointerToRelocations: D$ 0
     SectionExportHdr.PointerToLinenumbers: D$ 0
     SectionExportHdr.NumberOfRelocations: W$ 0
     SectionExportHdr.NumberOfLinenumbers: W$ 0
     SectionExportHdr.Characteristics: D$ 040000040             ; readable initialized data


RelocSectionHeader:         ;, if Export:
     SectionRelocHdr.Name1: B$ 0 #&IMAGE_SIZEOF_SHORT_NAME ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionRelocHdr.MiscPhysicalAddress: RelocMiscVirtualSize: AppRelocTrueSize: D$ 0   ; true size of relocs in file
     SectionRelocHdr.VirtualAddress: AppBaseOfReloc: D$ 0      ; RVA offset
     SectionRelocHdr.SizeOfRawData: AppRelocAlignedSize: D$ 0   ; file aligned size of relocs
     SectionRelocHdr.PointerToRawData: AppStartOfReloc: D$ 0    ; pointer to relocs
     SectionRelocHdr.PointerToRelocations: D$ 0
     SectionRelocHdr.PointerToLinenumbers: D$ 0
     SectionRelocHdr.NumberOfRelocations: W$ 0
     SectionRelocHdr.NumberOfLinenumbers: W$ 0
     SectionRelocHdr.Characteristics: D$ 040000040               ; readable initialized data


Dummy1SectionHeader:         ; Not used. It is just ensure to stop win search of sections.
     SectionDummy1Hdr.Name1: B$ 0 #&IMAGE_SIZEOF_SHORT_NAME ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionDummy1Hdr.MiscPhysicalAddress: Dummy1MiscVirtualSize: D$ 0
     SectionDummy1Hdr.VirtualAddress: D$ 0
     SectionDummy1Hdr.SizeOfRawData: D$ 0
     SectionDummy1Hdr.PointerToRawData: D$ 0
     SectionDummy1Hdr.PointerToRelocations: D$ 0
     SectionDummy1Hdr.PointerToLinenumbers: D$ 0
     SectionDummy1Hdr.NumberOfRelocations: W$ 0
     SectionDummy1Hdr.NumberOfLinenumbers: W$ 0
     SectionDummy1Hdr.Characteristics: D$ 0
     Dummy1ExtraByte: D$ 0 ; This does not exists on the original structure. Used only for preventing win search of sections.

Dummy2SectionHeader:         ; Not used. It is just ensure to stop win search of sections.
     SectionDummy2Hdr.Name1: B$ 0 #&IMAGE_SIZEOF_SHORT_NAME ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionDummy2Hdr.MiscPhysicalAddress: Dummy2MiscVirtualSize: D$ 0
     SectionDummy2Hdr.VirtualAddress: D$ 0
     SectionDummy2Hdr.SizeOfRawData: D$ 0
     SectionDummy2Hdr.PointerToRawData: D$ 0
     SectionDummy2Hdr.PointerToRelocations: D$ 0
     SectionDummy2Hdr.PointerToLinenumbers: D$ 0
     SectionDummy2Hdr.NumberOfRelocations: W$ 0
     SectionDummy2Hdr.NumberOfLinenumbers: W$ 0
     SectionDummy2Hdr.Characteristics: D$ 0
     Dummy2ExtraByte: D$ 0 ; This does not exists on the original structure. Used only for preventing win search of sections.

SourceSectionHeader:        ; Used by RosAsm only (not by loader: 4 sections, not 5)
     SectionSrcHdr.Name1: B$ '.src',0,0,0,0   ; Maximum size is #&IMAGE_SIZEOF_SHORT_NAME
     SectionSrcHdr.MiscPhysicalAddress: SrcMiscVirtualSize: AppSrcTrueSize: D$ 0   ;  D$SourceLen true size
     SectionSrcHdr.VirtualAddress: AppBaseOfSrc: D$ 0      ; RVA offset
     SectionSrcHdr.SizeOfRawData: AppSrcAlignedSize: D$ 0   ; 200h+ResourceExt
     SectionSrcHdr.PointerToRawData: AppStartOfSrc: D$ 0    ; pointer to src
     SectionSrcHdr.PointerToRelocations: D$ 0
     SectionSrcHdr.PointerToLinenumbers: D$ 0
     SectionSrcHdr.NumberOfRelocations: W$ 0
     SectionSrcHdr.NumberOfLinenumbers: W$ 0
     SectionSrcHdr.Characteristics: D$ 06000840             ; Not readable initialised data; don't keep; don't cache...



EOPE:
PeHeaderSize: D$  EOPE-PeHeader]  ; 'Len' unusable here
 ________________________________________________________________________________________

; Equates related to the PE Structures

; IMAGE_DOS_HEADER

[DosHeader.e_magicDis 0
 DosHeader.e_cblpDis 2
 DosHeader.e_cpDis 4
 DosHeader.e_crlcDis 6
 DosHeader.e_cparhdrDis 8
 DosHeader.e_minallocDis 10
 DosHeader.e_maxallocDis 12
 DosHeader.e_ssDis 14
 DosHeader.e_spDis 16
 DosHeader.e_csumDis 18
 DosHeader.e_ipDis 20
 DosHeader.e_csDis 22
 DosHeader.e_lfarlcDis 24
 DosHeader.e_ovnoDis 26
 DosHeader.e_res_01Dis 28
 DosHeader.e_res_02Dis 30
 DosHeader.e_res_03Dis 32
 DosHeader.e_res_04Dis 34
 DosHeader.e_oemidDis 36
 DosHeader.e_oeminfoDis 38
 DosHeader.e_res2_01Dis 40
 DosHeader.e_res2_02Dis 42
 DosHeader.e_res2_03Dis 44
 DosHeader.e_res2_04Dis 46
 DosHeader.e_res2_05Dis 48
 DosHeader.e_res2_06Dis 50
 DosHeader.e_res2_07Dis 52
 DosHeader.e_res2_08Dis 54
 DosHeader.e_res2_09Dis 56
 DosHeader.e_res2_10Dis 58
 DosHeader.e_lfanewDis 60]

[SizeOf_DosHeader 64]

; IMAGE_NT_HEADERS

[PeHeader.SignatureDis 0
 PeHeader.FileHeader.MachineDis 4
 PeHeader.FileHeader.NumberOfSectionsDis 6
 PeHeader.FileHeader.TimeDateStampDis 8
 PeHeader.FileHeader.PointerToSymbolTableDis 12
 PeHeader.FileHeader.NumberOfSymbolsDis 16
 PeHeader.FileHeader.SizeOfOptionalHeaderDis 20
 PeHeader.FileHeader.CharacteristicsDis 22
 PeHeader.OptionalHeader.MagicDis 24
 PeHeader.OptionalHeader.MajorLinkerVersionDis 26
 PeHeader.OptionalHeader.MinorLinkerVersionDis 27
 PeHeader.OptionalHeader.SizeOfCodeDis 28
 PeHeader.OptionalHeader.SizeOfInitializedDataDis 32
 PeHeader.OptionalHeader.SizeOfUninitializedDataDis 36
 PeHeader.OptionalHeader.AddressOfEntryPointDis 40
 PeHeader.OptionalHeader.BaseOfCodeDis 44
 PeHeader.OptionalHeader.BaseOfDataDis 48
 PeHeader.OptionalHeader.ImageBaseDis 52
 PeHeader.OptionalHeader.SectionAlignmentDis 56
 PeHeader.OptionalHeader.FileAlignmentDis 60
 PeHeader.OptionalHeader.MajorOperatingSystemVersionDis 64
 PeHeader.OptionalHeader.MinorOperatingSystemVersionDis 66
 PeHeader.OptionalHeader.MajorImageVersionDis 68
 PeHeader.OptionalHeader.MinorImageVersionDis 70
 PeHeader.OptionalHeader.MajorSubsystemVersionDis 72
 PeHeader.OptionalHeader.MinorSubsystemVersionDis 74
 PeHeader.OptionalHeader.Win32VersionValueDis 76
 PeHeader.OptionalHeader.SizeOfImageDis 80
 PeHeader.OptionalHeader.SizeOfHeadersDis 84
 PeHeader.OptionalHeader.CheckSumDis 88
 PeHeader.OptionalHeader.SubsystemDis 92
 PeHeader.OptionalHeader.DllCharacteristicsDis 94
 PeHeader.OptionalHeader.SizeOfStackReserveDis 96
 PeHeader.OptionalHeader.SizeOfStackCommitDis 100
 PeHeader.OptionalHeader.SizeOfHeapReserveDis 104
 PeHeader.OptionalHeader.SizeOfHeapCommitDis 108
 PeHeader.OptionalHeader.LoaderFlagsDis 112
 PeHeader.OptionalHeader.NumberOfRvaAndSizesDis 116
 PeHeader.DataDirectory.ExportDis 120
 PeHeader.DataDirectory.ExportSizeDis 124
 PeHeader.DataDirectory.ImportDis 128
 PeHeader.DataDirectory.ImportSizeDis 132
 PeHeader.DataDirectory.ResourceDis 136
 PeHeader.DataDirectory.ResourceSizeDis 140
 PeHeader.DataDirectory.ExceptionDis 144
 PeHeader.DataDirectory.ExceptionSizeDis 148
 PeHeader.DataDirectory.CertificateDis 152
 PeHeader.DataDirectory.CertificateSizeDis 156
 PeHeader.DataDirectory.RelocationDis 160
 PeHeader.DataDirectory.RelocationSizeDis 164
 PeHeader.DataDirectory.DebugDis 168
 PeHeader.DataDirectory.DebugSizeDis 172
 PeHeader.DataDirectory.ArchitectureDis 176
 PeHeader.DataDirectory.ArchitectureSizeDis 180
 PeHeader.DataDirectory.GPRegDis 184
 PeHeader.DataDirectory.GPRegSizeDis 188
 PeHeader.DataDirectory.ThreadDis 192
 PeHeader.DataDirectory.ThreadSizeDis 196
 PeHeader.DataDirectory.ConfigTableDis 200
 PeHeader.DataDirectory.ConfigTableSizeDis 204
 PeHeader.DataDirectory.BoundIATDis 208
 PeHeader.DataDirectory.BoundIATSizeDis 212
 PeHeader.DataDirectory.IATDis 216
 PeHeader.DataDirectory.IATSizeDis 220
 PeHeader.DataDirectory.DelayIDDis 224
 PeHeader.DataDirectory.DelayIDSizeDis 228
 PeHeader.DataDirectory.COMDis 232
 PeHeader.DataDirectory.COMSizeDis 236
 PeHeader.DataDirectory.ReservedDis 240
 PeHeader.DataDirectory.ReservedSizeDis 244]

[SizeOf_PeHeader 248]



; IMAGE_SECTION_HEADER

[SectionsHeaders.Name1Dis 0
 SectionsHeaders.SrcMiscVirtualSizeDis 8
 SectionsHeaders.VirtualAddressDis 12
 SectionsHeaders.SizeOfRawDataDis 16
 SectionsHeaders.PointerToRawDataDis 20
 SectionsHeaders.PointerToRelocationsDis 24
 SectionsHeaders.PointerToLinenumbersDis 28
 SectionsHeaders.NumberOfRelocationsDis 32
 SectionsHeaders.NumberOfLinenumbersDis 34
 SectionsHeaders.CharacteristicsDis 36]

[SizeOf_SectionsHeaders 40]



_______________________________________________________________________________________________________


;                           Structures and equates used on the SectionTable

_______________________________________________________________________________________________________




_______________________________________________________________________________________________________

;                                       Export Table Adress structures
_______________________________________________________________________________________________________

; The EAT structure is defined as:

Export_Table_Adress:

;;
The export symbol information begins with the export directory table, which describes the remainder of the export symbol information.
The export directory table contains address information that is used to resolve imports to the entry points within this image.

When an EXE exports code or data, it's making functions or variables usable by other EXEs. To keep things simple, I'll refer to exported
functions and exported variables by the term "symbols." At a minimum, to export something, the address of an exported symbol needs to be
obtainable in a defined manner. Each exported symbol has an ordinal number associated with it that can be used to look it up.

Also, there is almost always an ASCII name associated with the symbol. Traditionally, the exported symbol name is the same as the name of
the function or variable in the originating source file, although they can also be made to differ.

Typically, when an executable imports a symbol, it uses the symbol name rather than its ordinal. However, when importing by name,
the system just uses the name to look up the export ordinal of the desired symbol, and retrieves the address using the ordinal value.
It would be slightly faster if an ordinal had been used in the first place. Exporting and importing by name is solely a convenience for programmers.
The use of the ORDINAL keyword in the Exports section of a .DEF file tells the linker to create an import library that
forces an API to be imported by ordinal, not by name.

I'll begin with the IMAGE_EXPORT_DIRECTORY structure, which is shown as below:


[IMAGE_EXPORT_DIRECTORY:

 Characteristics: D$ 0 ; Flags for the exports. Currently, none are defined. Must be 0.
 TimeDateStamp: D$ 0 ; The time/date that the exports were created. This field has the same definition as the IMAGE_NT_HEADERS.FileHeader.
                     ; TimeDateStamp (number of seconds since 1/1/1970 GMT).

 MajorVersion: W$ 0 ; The major version number. The major and minor version numbers can be set by the user.
 MinorVersion: W$ 0 ; The minor version number.
 nName: D$ 0        ; A relative virtual address (RVA) to an ASCII string with the DLL name associated with these exports
                    ; (for example, KERNEL32.DLL).
 nBase: D$ 0        ; This field contains the starting ordinal value to be used for this executable's exports.
                    ; Normally, this value is 1, but it's not required to be so. When looking up an export by ordinal,
                    ; the value of this field is subtracted from the ordinal, with the result used as a zero-based index
                    ; into the Export Address Table (EAT).

 NumberOfFunctions: D$ 0    ; The number of entries in the EAT. Note that some entries may be 0, indicating that no code/data
                            ; is exported with that ordinal value.
 NumberOfNames: D$ 0        ; The number of entries in the Export Names Table (ENT). This value will always be less than or equal
                            ; to the NumberOf-Functions field. It will be less when there are symbols exported by ordinal only.
                            ; It can also be less if there are numeric gaps in the assigned ordinals. This field is also the size
                            ; of the export ordinal table (below).
 AddressOfFunctions: D$ 0   ; The RVA of the EAT. The EAT is an array of RVAs. Each nonzero RVA in the array corresponds to an exported symbol.
                            ; The export address table contains the address of exported entry points and exported data and absolutes
                            ; An ordinal number is used as an index into the export address table.
                            ; Each entry in the export address table is a field that uses one of two formats in the following table.
                            ; If the address specified is not within the export section (as defined by the address and length that are
                            ; indicated in the optional header), the field is an export RVA, which is an actual address in code or data.
                            ; Otherwise, the field is a forwarder RVA, which names a symbol in another DLL.
                            ; Formats:
                            ; Export RVA :  The address of the exported symbol when loaded into memory, relative to the image base. For example,
                            ;               the address of an exported function.
                            ; Forwarder RVA: The pointer to a null-terminated ASCII string in the export section. This string must be within
                            ;                the range that is given by the export table data directory entry.
                            ;                See section 3.4.3, "Optional Header Data Directories (Image Only)." 
                            ;                This string gives the DLL name and the name of the export (for example, "MYDLL.expfunc")
                            ;                or the DLL name and the ordinal number of the export (for example, "MYDLL.#27").


                            ; A forwarder RVA exports a definition from some other image, making it appear as if it were being exported
                            ; by the current image. Thus, the symbol is simultaneously imported and exported.
                            ; For example, in Kernel32.dll in Windows XP, the export named "HeapAlloc" is forwarded to the string
                            ; "NTDLL.RtlAllocateHeap." This allows applications to use the Windows XP-specific module Ntdll.dll without
                            ; actually containing import references to it. The application's import table refers only to Kernel32.dll.
                            ; Therefore, the application is not specific to Windows XP and can run on any Win32 system.
                            
                            ; How can you tell if a function is forwarded rather than exported normally? It's somewhat tricky. Normally,
                            ; the EAT contains the RVA of the exported symbol. However, if the function's RVA is inside the exports section
                            ; (as given by the VirtualAddress and Size fields in the DataDirectory), the symbol is forwarded.
                            ; When a symbol is forwarded, its RVA obviously can't be a code or data address in the current module.
                            ; Instead, the RVA points to an ASCII string of the DLL and symbol name to which it is forwarded.
                            ; In the prior example, it would be NTDLL.RtlAllocHeap.
                            ; The bottom line is if the address is anywhere inside PeHeader.DataDirectory.ExportDis+PeHeader.DataDirectory.ExportSizeDis
                            ; then it is a forward

 AddressOfNames: D$ 0       ; The RVA of the ENT. The ENT is an array of RVAs to ASCII strings. Each ASCII string corresponds to a
                            ; symbol exported by name. This table is sorted so that the ASCII strings are in order.
                            ; This allows the loader to do a binary search when looking for an exported symbol.
                            ; The sorting of the names is binary (like the C++ RTL strcmp function provides), rather than a locale-specific
                            ; alphabetic ordering.

 AddressOfNameOrdinals: D$ 0] ; The RVA of the export ordinal table. This table is an array of WORDs. This table maps an array index from
                              ; the ENT into the corresponding export address table entry.

Example:

[IMAGE_EXPORT_DIRECTORY:
 IMAGE_EXPORT_DIRECTORY.Characteristics: D$ 0
 IMAGE_EXPORT_DIRECTORY.TimeDateStamp: D$ 0498C18B0
 IMAGE_EXPORT_DIRECTORY.MajorVersion: W$ 0
 IMAGE_EXPORT_DIRECTORY.MinorVersion: W$ 0
 IMAGE_EXPORT_DIRECTORY.nName: D$ 0
 IMAGE_EXPORT_DIRECTORY.nBase: D$ 1
 IMAGE_EXPORT_DIRECTORY.NumberOfFunctions: D$ 3
 IMAGE_EXPORT_DIRECTORY.NumberOfNames: D$ 3
 IMAGE_EXPORT_DIRECTORY.AddressOfFunctions: D$ ExportAddressTable ;Pointer to Export Code or Forwarded Export
 IMAGE_EXPORT_DIRECTORY.AddressOfNames: D$ ExportLookupTable-DosHeader
 IMAGE_EXPORT_DIRECTORY.AddressOfNameOrdinals: D$ Function_BY_ORDINAL_01-DosHeader]

[ExportAddressTable: D$ CodeActivateActCtx-DosHeader ; Point to a Export CodeFunction named as CodeActivateActCtx inside this PE
                     D$ AddVectoredExceptionHandler-DosHeader ; Point to a forwarded export inside NTDLL "NTDLL.RtlAddVectoredExceptionHandler"


; IMAGE_EXPORT_BY_NAME
[ExportLookupTable:  D$ Function01-DosHeader ; Point to the name of the function
                     D$ Function02-DosHeader]

[Function01: B$ 'GetModuleHandleA',0]
[Function02: B$ 'ExitProcess',0

; export_ordinal_table
; IMAGE_EXPORT_BY_ADDRESS
[Function_BY_ORDINAL_01: W$ 01 ; A_SHAFinal
 Function_BY_ORDINAL_02: W$ 02 ; A_SHAInit
 Function_BY_ORDINAL_03: W$ 03 ; A_SHAUpdate

[AddVectoredExceptionHandler: B$ 'NTDLL.RtlAddVectoredExceptionHandler',0]

;;

[IMAGE_EXPORT_DIRECTORY.CharacteristicsDis 0
 IMAGE_EXPORT_DIRECTORY.TimeDateStampDis 4
 IMAGE_EXPORT_DIRECTORY.MajorVersionDis 8
 IMAGE_EXPORT_DIRECTORY.MinorVersionDis 10
 IMAGE_EXPORT_DIRECTORY.nNameDis 12
 IMAGE_EXPORT_DIRECTORY.nBaseDis 16
 IMAGE_EXPORT_DIRECTORY.NumberOfFunctionsDis 20
 IMAGE_EXPORT_DIRECTORY.NumberOfNamesDis 24
 IMAGE_EXPORT_DIRECTORY.AddressOfFunctionsDis 28
 IMAGE_EXPORT_DIRECTORY.AddressOfNamesDis 32
 IMAGE_EXPORT_DIRECTORY.AddressOfNameOrdinalsDis 36]

[Size_of_IMAGE_EXPORT_DIRECTORY 40]

_______________________________________________________________________________________________________

;                                       Import Table Adress structures
_______________________________________________________________________________________________________

; The IAT structure is defined as:

;;

The import information begins with the import directory table, which describes the remainder of the import information.
The import directory table contains address information that is used to resolve fixup references to the entry points
within a DLL image. The import directory table consists of an array of import directory entries, one entry for each DLL
to which the image refers. The last directory entry is empty (filled with null values), which indicates the end of the
directory table.

Typically the Import Directory table is related to the .idata section.

The Import Directory Table is formed by a array of IMAGE_IMPORT_DESCRIPTOR followed by another IMAGE_IMPORT_DESCRIPTOR
filled with 0. (That is called Import Directory Terminator)

[IMAGE_IMPORT_DESCRIPTOR:
 Characteristics: OriginalFirstThunk: D$ 0  ; The RVA of the import lookup table. This table contains a name or ordinal
                                            ; for each import. (The name "Characteristics" is used in Winnt.h, but no
                                            ; longer describes this field.)
 TimeDateStamp: D$ 0                        ; The stamp that is set to zero until the image is bound. After the image
                                            ; is bound, this field is set to the time/data stamp of the DLL.
 ForwarderChain: D$ 0                       ; The index of the first forwarder reference.
 Name1: D$ 0                                ; (NameRVA) The address of an ASCII string that contains the name of the DLL.
                                            ; This address is relative to the image base.
 FirstThunk: D$ 0]                          ; The RVA of the Import Address Table (IAT). The contents of this table are
                                            ; identical to the contents of the import lookup table until the image is bound.
                                            ; In fact it Points o a IMAGE_THUNK_DATA structure
 
 (Array of IMAGE_IMPORT_DESCRIPTOR)
 
 (Last IMAGE_IMPORT_DESCRIPTOR) ; all zeroes
 
 followed by:

Imports dwords (Import Lookup Table - ILT)

followed by the IAT

[IMAGE_IMPORT_BY_NAME:
 Hint: W$ 0
 Name1: B$ 0]
 
Example:

 LookUpRVA: D$ 03050 TimeDate: 0 ForwarderChain: 0 NameRVA: 03136 AdressRVA: 030A8
 
 Total size of IMAGE_IMPORT_DESCRIPTOR is described at AppImportSize
 On our example it goes from Kernel32.ILT-DirectoryTable
 
 ; IMAGE_IMPORT_DESCRIPTOR (Size is 20)
 
[DirectoryTable:
 DirectoryTable.Characteristics: LookUpRVA: D$ Kernel32.ILT-DosHeader
 DirectoryTable.TimeDateStamp: D$ 0
 DirectoryTable.ForwarderChain: D$ 0
 DirectoryTable.Name1: NameRVA: D$ SzKernel32_dll-DosHeader
 DirectoryTable.FirstThunk: AdressRVA: D$ GetModuleHandleA-DosHeader]
 
 ; IMAGE_IMPORT_DESCRIPTOR (Import Dir01)

[DirTbl01:
 DirTbl01.Characteristics: DirTbl01.LookUpRVA: D$ User32.ILT-DosHeader
 DirTbl01.TimeDateStamp: D$ 0
 DirTbl01.ForwarderChain: D$ 0
 DirTbl01.Name1: D$ SzUser32_dll-DosHeader
 DirTbl01.FirstThunk: D$ LoadIconA-DosHeader]

 ; IMAGE_IMPORT_DESCRIPTOR (Import directory Terminator)

[DirTerminator:
 DirTerminator.Characteristics: DirTerminator.LookUpRVA: D$ DosHeader-DosHeader
 DirTerminator.TimeDateStamp: D$ 0
 DirTerminator.ForwarderChain: D$ 0
 DirTerminator.Name1: D$ DosHeader-DosHeader
 DirTerminator.FirstThunk: D$ DosHeader-DosHeader]
 
; Import Addresses Table RVA is flow of dWords Pointer to Functions Names for each DLL. (Zero ending dWord).

[Kernel32.ILT:  D$ Function01-DosHeader
                D$ Function02-DosHeader
                D$ 0]

[User32.ILT:    D$ Function03-DosHeader
                D$ Function04-DosHeader
                D$ Function05-DosHeader
                D$ Function06-DosHeader
                D$ Function07-DosHeader
                D$ Function08-DosHeader
                D$ Function09-DosHeader
                D$ Function10-DosHeader
                D$ Function11-DosHeader
                D$ Function12-DosHeader
                D$ Function13-DosHeader
                D$ Function14-DosHeader
                D$ Function15-DosHeader
                D$ Function16-DosHeader
                D$ Function17-DosHeader
                D$ 0]
 
; The IAT Starts here

; Imports from KERNEL32 
[GetModuleHandleA:  D$ Function01-DosHeader;010E2
 ExitProcess:       D$ Function02-DosHeader;010F6
                    D$ 0]

; Imports from USER32
[LoadIconA:         D$ Function03-DosHeader;01112
 LoadCursorA:       D$ Function04-DosHeader;0111E
 RegisterClassA:    D$ Function05-DosHeader;0112C
 LoadMenuA:         D$ Function06-DosHeader;0113E
 CreateWindowExA:   D$ Function07-DosHeader;0114A
 ShowWindow:        D$ Function08-DosHeader;0115C
 UpdateWindow:      D$ Function09-DosHeader;0116A
 TranslateMessage:  D$ Function10-DosHeader;0117A
 DispatchMessageA:  D$ Function11-DosHeader;0118E
 GetMessageA:       D$ Function12-DosHeader;011A2
 DestroyWindow:     D$ Function13-DosHeader;011B0
 PostQuitMessage:   D$ Function14-DosHeader;011C0
 SendMessageA:      D$ Function15-DosHeader;011D2
 MessageBoxA:       D$ Function16-DosHeader;011E2
 DefWindowProcA:    D$ Function17-DosHeader;011F0
                    D$ 0]
 
[SzKernel32_dll: B$ 'KERNEL32.dll',0]

; IMAGE_IMPORT_BY_NAME

[Function01:
 Function01.Hint: W$ 0
 Function01.Name1: B$ 'GetModuleHandleA',0]

[Function02:
 Function02.Hint: W$ 0
 Function02.Name1: B$ 'ExitProcess',0

[SzUser32_dll: B$ 'USER32.dll',0]

[Function03:
 Function03.Hint: W$ 0
 Function03.Name1: B$ 'LoadIconA',0]

[Function04:
 Function04.Hint: W$ 0
 Function04.Name1: B$ 'LoadCursorA',0]

[Function05:
 Function05.Hint: W$ 0
 Function05.Name1: B$ 'RegisterClassA',0]

[Function06:
 Function06.Hint: W$ 0
 Function06.Name1: B$ 'LoadMenuA',0]

[Function07:
 Function07.Hint: W$ 0
 Function07.Name1: B$ 'CreateWindowExA',0]

[Function08:
 Function08.Hint: W$ 0
 Function08.Name1: B$ 'ShowWindow',0]

[Function09:
 Function09.Hint: W$ 0
 Function09.Name1: B$ 'UpdateWindow',0]

[Function10:
 Function10.Hint: W$ 0
 Function10.Name1: B$ 'TranslateMessage',0]

[Function11:
 Function11.Hint: W$ 0
 Function11.Name1: B$ 'DispatchMessageA',0]

[Function12:
 Function12.Hint: W$ 0
 Function12.Name1: B$ 'GetMessageA',0]

[Function13:
 Function13.Hint: W$ 0
 Function13.Name1: B$ 'DestroyWindow',0]

[Function14:
 Function14.Hint: W$ 0
 Function14.Name1: B$ 'PostQuitMessage',0]

[Function15:
 Function15.Hint: W$ 0
 Function15.Name1: B$ 'SendMessageA',0]

[Function16:
 Function16.Hint: W$ 0
 Function16.Name1: B$ 'MessageBoxA',0]

[Function17:
 Function17.Hint: W$ 0
 Function17.Name1: B$ 'DefWindowProcA',0]
;;

Import_Table_Adress:

[IMAGE_IMPORT_DESCRIPTOR.CharacteristicsDis 0
 IMAGE_IMPORT_DESCRIPTOR.OriginalFirstThunkDis 0
 IMAGE_IMPORT_DESCRIPTOR.TimeDateStampDis 4
 IMAGE_IMPORT_DESCRIPTOR.ForwarderChainDis 8
 IMAGE_IMPORT_DESCRIPTOR.Name1Dis 12
 IMAGE_IMPORT_DESCRIPTOR.FirstThunkDis 16]

[Size_Of_IMAGE_IMPORT_DESCRIPTOR 20]


_______________________________________________________________________________________________________

;                                       Resource structures
_______________________________________________________________________________________________________


;;

Resource structures

[IMAGE_RESOURCE_DIRECTORY:
 Characteristics: D$ 0
 TimeDateStamp: D$ 0
 MajorVersion: W$ 0
 MinorVersion: W$ 0
 NumberOfNamedEntries: W$ 0
 NumberOfIdEntries: W$ 0]

[ImgResDir.CharacteristicsDis 0
 ImgResDir.TimeDateStampDis 4
 ImgResDir.MajorVersionDis 8
 ImgResDir.MinorVersionDis 10
 ImgResDir.NumberOfNamedEntriesDis 12
 ImgResDir.NumberOfIdEntriesDis 14]

[Size_Of_IMAGE_RESOURCE_DIRECTORY 16]

; followed by an array of XX IMAGE_RESOURCE_DIRECTORY_ENTRY. The total amount of elements
 on theg array are the sum of NumberOfNamedEntries + NumberOfIdEntries.

[IMAGE_RESOURCE_DIRECTORY_ENTRY:
 Name1: Id: D$ 0
 OffsetToData: D$ 0]
 
 Name1 = This field contains either an integer ID or a pointer to a structure that contains a string name.
         If the high bit (0x80000000) is zero, this field is interpreted as an integer ID.
         Th ID is the type of the resource, such as a dialog, an icon, an bitmap image etc. It can be one of the
         following equates (This member does not allow combination of the equates):
         &RT_CURSOR, &RT_BITMAP, &RT_ICON, &RT_MENU, &RT_DIALOG, &RT_STRING, &RT_FONTDIR, &RT_FONT, &RT_ACCELERATOR,
         &RT_RCDATA, &RT_GROUP_CURSOR, &RT_GROUP_ICON, &RT_MESSAGETABLE, &RT_VERSION, &RT_DLGINCLUDE, &RT_PLUGPLAY,
         &RT_VXD, &RT_ANICURSOR, &RT_MANIFEST

            Ex.: 05 = the ID of the data resource. It is a dialog (&RT_DIALOG)
 
         If the high bit is nonzero, the lower 31 bits are an offset (relative to the start of the resources)
            to an IMAGE_RESOURCE_DIR_STRING_U structure.
            Ex.: 080000688 = 080000000+DataOffset-IMAGE_RESOURCE_DIRECTORY_ENTRY (The main one, that is the 1st found in the section)
                So, on the example we are at byte 0688 from the start of the resources section.
                At byte 0688 we will have a IMAGE_RESOURCE_DIR_STRING_U structure
            This structure contains a WORD character count, followed by a UNICODE string with the resource name.
            Yes, even PE files intended for non-UNICODE Win32 implementations use UNICODE here.
            To convert the UNICODE string to an ANSI string, use the WideCharToMultiByte function.
                [IMAGE_RESOURCE_DIR_STRING_U:
                    Length1: W$ 0   The length of the string
                    NameString: W$ 0] The unicode string. This string is non null terminated, but an additional word
                                      may be inserted after the string to make next field start on a dword boundary.

OffsetToData = This field is either an offset to another resource directory or a pointer to information about
               a specific resource instance.
               
               If the high bit (0x80000000) is set, this directory entry refers to a subdirectory.
                The lower 31 bits are an offset (relative to the start of the resources) to another IMAGE_RESOURCE_DIRECTORY.
                Ex.: 080000468 = 080000000+DataOffset-IMAGE_RESOURCE_DIRECTORY_ENTRY (The main one, that is the 1st found in the section)
                So, on the example we are at byte 0468 from the start of the resources section.
                At byte 0468 we will have another IMAGE_RESOURCE_DIRECTORY structure
               
               If the high bit isn't set, the lower 31 bits point to an IMAGE_RESOURCE_DATA_ENTRY structure.
               This is called as "leaf node".
                Ex.: 0EA0 = DataOffset-IMAGE_RESOURCE_DIRECTORY_ENTRY (The main one, that is the 1st found in the section)
                     At byte 0EA0 we will have an IMAGE_RESOURCE_DATA_ENTRY structure
                The IMAGE_RESOURCE_DATA_ENTRY structure contains the location of the resource's raw data, its size, and its code page.
                    [IMAGE_RESOURCE_DATA_ENTRY:
                        OffsetToData: D$ 0
                        Size1: D$ 0
                        CodePage: D$ 0
                        Reserved: D$ 0]

                OffsetToData = location of the actual resource data. Since this information is used primarily
                               by functions once the application has been loaded,
                               it makes more sense to make the OffsetToData field a relative virtual address.

                                This is precisely the case.
               
                                Interestingly enough, all other offsets, such as pointers from directory entries
                                to other directories, are offsets relative to the location of the root node.
                                Ex.: 0E1608 = DataOffset-ImageBase (The start of thge PE file)
               
                Size1 = size of the actual resource data.
               
                CodePage = Code page is the traditional IBM term used for a specific character encoding table:
                           a mapping in which a sequence of bits, usually a single octet representing integer values
                           0 through 255, is associated with a specific character. IBM and Microsoft often allocate a
                           code page number to a character set even if that charset is better known by another name.

                           Whilst the term code page originated from IBM's EBCDIC-based mainframe systems, the term is
                           most commonly associated with the IBM PC code pages. Microsoft, a maker of PC operating systems,
                           refers to these code pages as OEM code pages, and supplements them with its own "ANSI" code pages.

                           Most well-known code pages, excluding those for the CJK languages and Vietnamese, represent character
                           sets that fit in 8 bits and don't involve anything that can't be represented by mapping each code to a
                           simple bitmap, such as combining characters, complex scripts, etc.

                           The text mode of standard (VGA compatible) PC graphics hardware is built around using an 8 bit
                           code page, though it is possible to use two at once with some color depth sacrifice, and up to
                           8 may be stored in the display adaptor for easy switching).
                           
                           There were a selection of code pages that could be loaded into such hardware.
                           
                           However, it is now commonplace for operating system vendors to provide their own character encoding
                           and rendering systems that run in a graphics mode and bypass this system entirely.
                           
                           The character encodings used by these graphical systems (particularly Windows) are sometimes
                           called code pages as well.
                           
                            - Relationship to ASCII. -
                           The basis of the IBM PC code pages is ASCII, a 7-bit code representing 128 characters and control
                           codes. In the past, 8-bit extensions to the ASCII code often either set the top bit to zero,
                           or used it as a parity bit in network data transmissions.
                           When this bit was instead made available for representing character data, another 128 characters
                           and control codes could be represented. IBM used this extended range to encode characters used
                           by various languages.
                           No formal standard existed for these 'extended character sets'; IBM merely referred to the variants
                           as code pages, as it had always done for variants of EBCDIC encodings.
               
                            - IBM PC (OEM) code pages -

                            These code pages are most often used under MS-DOS-like operating systems;
                            they include a lot of box drawing characters. Since the original IBM PC code page (number 437)
                            was not really designed for international use, several incompatible variants emerged.
                            Microsoft refers to these as the OEM code pages. Examples include:

                                * 437 — The original IBM PC code page
                                * 737 — Greek
                                * 850 — "Multilingual (Latin-1)" (Western European languages)
                                * 852 — "Slavic (Latin-2)" (Eastern European languages)
                                * 855 — Cyrillic
                                * 857 — Turkish
                                * 858 — "Multilingual" with euro symbol
                                * 860 — Portuguese
                                * 861 — Icelandic
                                * 863 — French Canadian
                                * 865 — Nordic
                                * 866 — Cyrillic
                                * 869 — Greek

                            - Other code pages of note -

                                * 10000 — Macintosh Roman encoding (followed by several other Mac character sets)
                                * 10007 — Macintosh Cyrillic encoding
                                * 10029 — Macintosh Central European encoding
                                * 932 — Supports Japanese
                                * 936 — GBK Supports Simplified Chinese
                                * 949 — Supports Korean
                                * 950 — Supports Traditional Chinese
                                * 1200 — UCS-2LE Unicode little-endian
                                * 1201 — UCS-2BE Unicode big-endian
                                * 65001 — UTF-8 Unicode
                                * ASMO449+ — Supports Arabic

                            In modern applications, operating systems and programming languages, the IBM code pages
                            have been rendered obsolete by newer & better international standards, such as ISO 8859-1
                            and Unicode.

                            - Windows (ANSI) code pages -

                            Microsoft defined a number of code pages known as the ANSI code pages (as the first one, 1252
                            was based on an ansi draft of what became ISO 8859-1). Code page 1252 is built on ISO 8859-1
                            but uses the range 0x80-0x9F for extra printable characters rather than the C1 control codes
                            used in ISO-8859-1.
                            Some of the others are based in part on other parts of ISO 8859 but often rearranged to make
                            them closer to 1252.

                                * 1250 — East European Latin
                                * 1251 — Cyrillic
                                * 1252 — West European Latin
                                * 1253 — Greek
                                * 1254 — Turkish
                                * 1255 — Hebrew
                                * 1256 — Arabic
                                * 1257 — Baltic
                                * 1258 — Vietnamese

                Reserved = Reserved data. Do not use.
;;


Resource_Structures:


; IMAGE_RESOURCE_DIRECTORY structure

[ImgResDir.CharacteristicsDis 0
 ImgResDir.TimeDateStampDis 4
 ImgResDir.MajorVersionDis 8
 ImgResDir.MinorVersionDis 10
 ImgResDir.NumberOfNamedEntriesDis 12
 ImgResDir.NumberOfIdEntriesDis 14]

[Size_Of_IMAGE_RESOURCE_DIRECTORY 16]

; IMAGE_RESOURCE_DIRECTORY_ENTRY structure

[ImgResDirEntry.Name1Dis 0
 ImgResDirEntry.OffsetToDataDis 4]

[Size_Of_IMAGE_RESOURCE_DIRECTORY_ENTRY 8]

; IMAGE_RESOURCE_DIR_STRING_U structure

[ImgResDirStringU.Length1Dis 0]
; ImgResDirStringU.NameString the size of the unicode string depends of the value of Length1

; IMAGE_RESOURCE_DATA_ENTRY structure

[ImgResDataEntry.OffsetToDataDis 0
 ImgResDataEntry.Size1Dis 4
 ImgResDataEntry.CodePageDis 8
 ImgResDataEntry.ReservedDis 12]

[Size_Of_IMAGE_RESOURCE_DATA_ENTRY 16]



_______________________________________________________________________________________________________

;                                       Load Config Directory structures
_______________________________________________________________________________________________________

;;
structures used
 
 [IMAGE_LOAD_CONFIG_DIRECTORY_OLD:
 Size: D$ 0
 TimeDateStamp: D$ 0
 MajorVersion: W$ 0
 MinorVersion: W$ 0
 GlobalFlagsClear: D$ 0
 GlobalFlagsSet: D$ 0
 CriticalSectionDefaultTimeout: D$ 0
 DeCommitFreeBlockThreshold: D$ 0
 DeCommitTotalFreeThreshold: D$ 0
 LockPrefixTable: D$ 0  ; VAPointer
 MaximumAllocationSize: D$ 0
 VirtualMemoryThreshold: D$ 0
 ProcessHeapFlags: D$ 0
 ProcessAffinityMask: D$ 0
 CSDVersion: W$ 0
 Reserved1: W$ 0
 EditList: D$ 0;  VAPointer
 SecurityCookie: D$ 0  ; A pointer to a cookie that is used by Visual C++ or GS implementation.
 SEHandlerTable: D$ 0  ; [x86 only] The VA of the sorted table of RVAs of each valid, unique SE handler in the image.
 SEHandlerCount: D$ 0] ; [x86 only] The count of unique handlers in the table.

[IMAGE_LOAD_CONFIG_DIRECTORY:
 Characteristics: D$ 0  ; Flags that indicate attributes of the file, currently unused.
 TimeDateStamp: D$ 0    ; Date and time stamp value. The value is represented in the number of seconds that have elapsed since midnight (00:00:00), January 1, 1970, Universal Coordinated Time, according to the system clock. The time stamp can be printed by using the C runtime (CRT) time function.
 MajorVersion: W$ 0     ; Major version number.
 MinorVersion: W$ 0     ; Minor version number.
 GlobalFlagsClear: D$ 0 ; The global loader flags to clear for this process as the loader starts the process.
 GlobalFlagsSet: D$ 0   ; The global loader flags to set for this process as the loader starts the process.
 CriticalSectionDefaultTimeout: D$ 0 ; The default timeout value to use for this process's critical sections that are abandoned.
 DeCommitFreeBlockThreshold: D$ 0 ; Memory that must be freed before it is returned to the system, in bytes.
 DeCommitTotalFreeThreshold: D$ 0 ; Total amount of free memory, in bytes.
 LockPrefixTable: D$ 0   ; [x86 only] The VA of a list of addresses where the LOCK prefix is used so that they can be replaced with NOP on single processor machines.
 MaximumAllocationSize: D$ 0 ; Maximum allocation size, in bytes.
 VirtualMemoryThreshold: D$ 0 ; Maximum virtual memory size, in bytes.
 ProcessAffinityMask: D$ 0 ; Setting this field to a non-zero value is equivalent to calling SetProcessAffinityMask with this value during process startup (.exe only)
 ProcessHeapFlags: D$ 0  ; Process heap flags that correspond to the first argument of the HeapCreate function. These flags apply to the process heap that is created during process startup.
 CSDVersion: W$ 0  ; The service pack version identifier.
 Reserved1: W$ 0  ; Must be zero.
 EditList: D$ 0  ; VAPointer Reserved for use by the system.
 Reserved: D$ 0]
 
[IMAGE_LOAD_CONFIG_DIRECTORY64:
 Characteristics: D$ 0
 TimeDateStamp: D$ 0
 MajorVersion: W$ 0
 MinorVersion: W$ 0
 GlobalFlagsClear: D$ 0
 GlobalFlagsSet: D$ 0
 CriticalSectionDefaultTimeout: D$ 0
 DeCommitFreeBlockThreshold: Q$ 0
 DeCommitTotalFreeThreshold: Q$ 0
 LockPrefixTable: Q$ 0
 MaximumAllocationSize: Q$ 0
 VirtualMemoryThreshold: Q$ 0
 ProcessAffinityMask: Q$ 0
 ProcessHeapFlags: D$ 0
 CSDVersion: W$ 0
 Reserved1: W$ 0
 EditList: Q$ 0
 Reserved: D$ 0 #2]

;;

Load_Config_Directory_Structures:

[IMAGE_LOAD_CONFIG_DIRECTORY_OLD.SizeDis 0
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.TimeDateStampDis 4
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.MajorVersionDis 8
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.MinorVersionDis 10
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.GlobalFlagsClearDis 12
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.GlobalFlagsSetDis 16
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.CriticalSectionDefaultTimeoutDis 20
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.DeCommitFreeBlockThresholdDis 24
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.DeCommitTotalFreeThresholdDis 28
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.LockPrefixTableDis 32
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.MaximumAllocationSizeDis 36
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.VirtualMemoryThresholdDis 40
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.ProcessHeapFlagsDis 44
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.ProcessAffinityMaskDis 48
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.CSDVersionDis 52
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.Reserved1Dis 54
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.EditListDis 56
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.SecurityCookieDis 60
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.SEHandlerTableDis 64
 IMAGE_LOAD_CONFIG_DIRECTORY_OLD.SEHandlerCountDis 68]

[Size_Of_IMAGE_LOAD_CONFIG_DIRECTORY_OLD 72]

[IMAGE_LOAD_CONFIG_DIRECTORY.CharacteristicsDis 0
 IMAGE_LOAD_CONFIG_DIRECTORY.TimeDateStampDis 4
 IMAGE_LOAD_CONFIG_DIRECTORY.MajorVersionDis 8
 IMAGE_LOAD_CONFIG_DIRECTORY.MinorVersionDis 10
 IMAGE_LOAD_CONFIG_DIRECTORY.GlobalFlagsClearDis 12
 IMAGE_LOAD_CONFIG_DIRECTORY.GlobalFlagsSetDis 16
 IMAGE_LOAD_CONFIG_DIRECTORY.CriticalSectionDefaultTimeoutDis 20
 IMAGE_LOAD_CONFIG_DIRECTORY.DeCommitFreeBlockThresholdDis 24
 IMAGE_LOAD_CONFIG_DIRECTORY.DeCommitTotalFreeThresholdDis 28
 IMAGE_LOAD_CONFIG_DIRECTORY.LockPrefixTableDis 32
 IMAGE_LOAD_CONFIG_DIRECTORY.MaximumAllocationSizeDis 36
 IMAGE_LOAD_CONFIG_DIRECTORY.VirtualMemoryThresholdDis 40
 IMAGE_LOAD_CONFIG_DIRECTORY.ProcessAffinityMaskDis 44
 IMAGE_LOAD_CONFIG_DIRECTORY.ProcessHeapFlagsDis 48
 IMAGE_LOAD_CONFIG_DIRECTORY.CSDVersionDis 52
 IMAGE_LOAD_CONFIG_DIRECTORY.Reserved1Dis 54
 IMAGE_LOAD_CONFIG_DIRECTORY.EditListDis 56
 IMAGE_LOAD_CONFIG_DIRECTORY.ReservedDis 60]

[Size_Of_IMAGE_LOAD_CONFIG_DIRECTORY 64]

[IMAGE_LOAD_CONFIG_DIRECTORY64.CharacteristicsDis 0
 IMAGE_LOAD_CONFIG_DIRECTORY64.TimeDateStampDis 4
 IMAGE_LOAD_CONFIG_DIRECTORY64.MajorVersionDis 8
 IMAGE_LOAD_CONFIG_DIRECTORY64.MinorVersionDis 10
 IMAGE_LOAD_CONFIG_DIRECTORY64.GlobalFlagsClearDis 12
 IMAGE_LOAD_CONFIG_DIRECTORY64.GlobalFlagsSetDis 16
 IMAGE_LOAD_CONFIG_DIRECTORY64.CriticalSectionDefaultTimeoutDis 20
 IMAGE_LOAD_CONFIG_DIRECTORY64.DeCommitFreeBlockThresholdDis 24
 IMAGE_LOAD_CONFIG_DIRECTORY64.DeCommitTotalFreeThresholdDis 32
 IMAGE_LOAD_CONFIG_DIRECTORY64.LockPrefixTableDis 40
 IMAGE_LOAD_CONFIG_DIRECTORY64.MaximumAllocationSizeDis 48
 IMAGE_LOAD_CONFIG_DIRECTORY64.VirtualMemoryThresholdDis 56
 IMAGE_LOAD_CONFIG_DIRECTORY64.ProcessAffinityMaskDis 64
 IMAGE_LOAD_CONFIG_DIRECTORY64.ProcessHeapFlagsDis 72
 IMAGE_LOAD_CONFIG_DIRECTORY64.CSDVersionDis 76
 IMAGE_LOAD_CONFIG_DIRECTORY64.Reserved1Dis 78
 IMAGE_LOAD_CONFIG_DIRECTORY64.EditListDis 80
 IMAGE_LOAD_CONFIG_DIRECTORY64.ReservedDis 88]

[Size_Of_IMAGE_LOAD_CONFIG_DIRECTORY64 96]

_______________________________________________________________________________________________________

;                                       Bound IAT Directory Table structures
_______________________________________________________________________________________________________

;;
structures used

[IMAGE_BOUND_IMPORT_DESCRIPTOR:
 TimeDateStamp: D$ 0
 OffsetModuleName: W$ 0 ; Offset to the moidule name, it is computed subtracting the Start of the array with the pointer to the string
 NumberOfModuleForwarderRefs: W$ 0]

[IMAGE_BOUND_FORWARDER_REF:
 TimeDateStamp: D$ 0
 OffsetModuleName: W$ 0
 Reserved: W$ 0]

Example:

[IMAGE_BOUND_IMPORT_DESCRIPTOR:
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data1.TimeDateStamp: D$ 041109403
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data1.OffsetModuleName: W$ 050 ; IMAGE_BOUND_IMPORT_DESCRIPTOR - SzKernel32_dll
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data1.NumberOfModuleForwarderRefs: W$ 0

 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data2.TimeDateStamp: D$ 04110940C
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data2.OffsetModuleName: W$ 05C ; IMAGE_BOUND_IMPORT_DESCRIPTOR - SzUser32_dll
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data2.NumberOfModuleForwarderRefs: W$ 0

 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data3.TimeDateStamp: D$ 041109406
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data3.OffsetModuleName: W$ 069 ; IMAGE_BOUND_IMPORT_DESCRIPTOR - SzGdi32_dll
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data3.NumberOfModuleForwarderRefs: W$ 0

 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data4.TimeDateStamp: D$ 041109408
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data4.OffsetModuleName: W$ 073 ; IMAGE_BOUND_IMPORT_DESCRIPTOR - SzComdlg32_dll
 IMAGE_BOUND_IMPORT_DESCRIPTOR.Data4.NumberOfModuleForwarderRefs: W$ 0

 IMAGE_BOUND_IMPORT_DESCRIPTOR.NullTerminator.TimeDateStamp: D$ 0
 IMAGE_BOUND_IMPORT_DESCRIPTOR.NullTerminator.OffsetModuleName: W$ 0
 IMAGE_BOUND_IMPORT_DESCRIPTOR.NullTerminator.NumberOfModuleForwarderRefs: W$ 0]


[SzKernel32_dll: B$ 'KERNEL32.dll',0]
[SzUser32_dll: B$ 'USER32.dll',0]
[SzGdi32_dll: B$ 'GDI32.dll',0]
[SzComdlg32_dll: B$ 'COMDLG32.dll',0]

;;

BoundIAT_Directory_Structures:

[IMAGE_BOUND_IMPORT_DESCRIPTOR.TimeDateStampDis 0
 IMAGE_BOUND_IMPORT_DESCRIPTOR.OffsetModuleNameDis 4
 IMAGE_BOUND_IMPORT_DESCRIPTOR.NumberOfModuleForwarderRefsDis 6]

[Size_Of_IMAGE_BOUND_IMPORT_DESCRIPTOR 8]


[IMAGE_BOUND_FORWARDER_REF.TimeDateStampDis 0
 IMAGE_BOUND_FORWARDER_REF.OffsetModuleNameDis 4
 IMAGE_BOUND_FORWARDER_REF.ReservedDis 6]

[Size_Of_IMAGE_BOUND_FORWARDER_REF 8]

_______________________________________________________________________________________________________

;                                       Delay-Load Directory Table structures
_______________________________________________________________________________________________________



;;

The Delay-Load Directory Table is formed by a array of IMAGE_DELAY_DESCRIPTOR followed by another IMAGE_DELAY_DESCRIPTOR
filled with 0. (That is called Delay-Load Directory Terminator)

[IMAGE_DELAY_DESCRIPTOR:
 grAttrs: D$ 0       ; Attributes reserved, must be Zero. If 0 all the below address points to RVA. Otherwise, points to VA
 szName: D$ 0        ; The RVA of the name of the DLL to be loaded. The name resides in the read-only data section of
                     ; the image.
 phmod: D$ 0         ; The RVA of the module handle (in the data section of the image) of the DLL to be delay-loaded.
                     ; It is used for storage by the routine that is supplied to manage delay-loading.
 pIAT: D$ 0          ; Address of the IAT (TImageThunkData)
                     ; Delay Import Address Table The RVA of the delay-load import address table.
 pINT: D$ 0          ; Address of the INT (TImageThunkData)
                     ; Delay Import Name Table . The RVA of the delay-load name table, which contains the names
                     ; of the imports that might need to be loaded. This matches the layout of the import name table.
 pBoundIAT: D$ 0     ; Address of the optional bound IAT (TImageThunkData)
                     ; Bound Delay Import Table The RVA of the bound delay-load address table, if it exists.
 pUnloadIAT: D$ 0    ; Address of optional copy of original IAT (TImageThunkData)
                     ; Unload Delay Import Table The RVA of the unload delay-load address table, if it exists.
                     ; This is an exact copy of the delay import address table. If the caller unloads the DLL,
                     ; this table should be copied back over the delay import address table so that subsequent
                     ; calls to the DLL continue to use the thunking mechanism correctly.
 dwTimeStamp: D$ 0   ; The timestamp of the DLL to which this image has been bound.
                     ; 0 if not bound,
                     ; O.W. date/time stamp of DLL bound to (Old BIND)
]




Example:

 ; IMAGE_DELAY_DESCRIPTOR (Size is 32)

[DelayDirectoryTable:
 DelayDirectoryTable.grAttrs: D$ 0
 DelayDirectoryTable.szName: D$ SzKernel32_dll-DosHeader
 DelayDirectoryTable.phmod: D$ hKernel32-DosHeader
 DelayDirectoryTable.pIAT: D$ GetModuleHandleA-DosHeader ; Same as IMAGE_IMPORT_DESCRIPTOR.FirstThunkDis; but points to a function
 DelayDirectoryTable.pINT: D$ Kernel32.ILT-DosHeader
 DelayDirectoryTable.pBoundIAT: D$ Kernel32_dll.BIat-DosHeader
 DelayDirectoryTable.pUnloadIAT: D$ GetModuleHandleA-DosHeader ; Original IAT may be the pointers to .idata ?
 DelayDirectoryTable.dwTimeStamp: D$ 0] 

; Import Addresses Table RVA is flow of dWords Pointer to Functions Names for each DLL. (Zero ending dWord).

[Kernel32.ILT:  D$ Function01-DosHeader
                D$ Function02-DosHeader
                D$ 0]

; The IAT Starts here

; Imports from KERNEL32 
[GetModuleHandleA:  D$ MainFunction01-DosHeader
 ExitProcess:       D$ MainFunction02-DosHeader
                    D$ 0]


[SzKernel32_dll: B$ 'KERNEL32.dll',0]

[hKernel32: D$ 0]

; IMAGE_IMPORT_BY_NAME

[Function01:
 Function01.Hint: W$ 0
 Function01.Name1: B$ 'GetModuleHandleA',0]


[Kernel32_dll.BIat: D$ Function01-DosHeader
                    D$ Function02-DosHeader
                    D$ 0]

MainFunction01:

mov eax GetModuleHandleA
jmp MainDelayImportAdrressFunction

(...)

MainDelayImportAdrressFunction:
    push ecx
    push edx
    push eax
    push DelayDirectoryTable
    call internal_find_dll_addressing
    pop edx
    pop ecx
    jmp eax
(...)

internal_find_dll_addressing:
(......)

;;

Delay_Import_Address_Table_Structures:

[IMAGE_DELAY_DESCRIPTOR.grAttrsDis 0
 IMAGE_DELAY_DESCRIPTOR.szNameDis 4
 IMAGE_DELAY_DESCRIPTOR.phmodDis 8
 IMAGE_DELAY_DESCRIPTOR.pIATDis 12
 IMAGE_DELAY_DESCRIPTOR.pINTDis 16
 IMAGE_DELAY_DESCRIPTOR.pBoundIATDis 20
 IMAGE_DELAY_DESCRIPTOR.pUnloadIATDis 24
 IMAGE_DELAY_DESCRIPTOR.dwTimeStampDis 28]

[Size_Of_IMAGE_DELAY_DESCRIPTOR 32]













































__________________________________________________________________________________________________________________


; Interesting way to dump packed PEs Like divx.dll

[Dumped_DLL: ?]
[hDumpedDLL: D$ 0]
[DumpedFileSize: D$ 0]

Proc DumpDLL:
    call 'KERNEL32.LoadLibraryExA' {"DivX.dll", 0}, 0, &LOAD_LIBRARY_AS_DATAFILE__&LOAD_LIBRARY_AS_IMAGE_RESOURCE ; <---- THis flag is used to manage resources, but...actually it will try to convert the dll to data as many as posible
    mov edx eax
    call GetPeTagMethod3 eax, 516
    mov ecx D$eax+PeHeader.OptionalHeader.SizeOfImageDis
    mov eax edx
    dec eax
    mov D$hDumpedDLL eax
    mov D$DumpedFileSize ecx
    shl ecx 1
    VirtualAlloc Dumped_DLL, ecx

    mov esi D$hDumpedDLL
    mov edi D$Dumped_DLL
    mov ecx D$DumpedFileSize
    Do
        movsb
        dec ecx
    Loop_Until ecx = 0

    call DumpMemory 0, D$Dumped_DLL, D$DumpedFileSize

EndP
__________________________________________________________________________________________________________________

; See if a address is inside Virtual Adrress anywhere in the Section Structures
; see CheckExport

Proc dbg_IsPointtoVirtualSection:
    Arguments @InputRVA, @SectionCount, @PESection, @BaseAddress, @PEHeader
    Local @RvaSectionAlignment, @SectionRVASize, @FileAlignment
    Uses esi, ecx, edx

; ReAlignPE
; If virtualsize <= SizeinFile we have no Virtual address
; SectionsHeaders.SrcMiscVirtualSizeDis <= SectionsHeaders.SizeOfRawDataDis = no Virtual Address

; limites are:
; SectionsHeaders.VirtualAddressDis+SectionsHeaders.SizeOfRawDataDis+1 = start of virtual adress
; SectionsHeaders.VirtualAddressDis+SectionsHeaders.SrcMiscVirtualSizeDis = end of virtual adddress
; so if InputRVA is anywhere between start and end of virtual address we return true

    mov esi D@PESection
    add esi D@BaseAddress
    mov ecx D@SectionCount
    xor eax eax
    mov ebx D@InputRVA

    mov edx D@PEHeader
    add edx D@BaseAddress
    move D@RvaSectionAlignment D$edx+PeHeader.OptionalHeader.SectionAlignmentDis;01000
    move D@FileAlignment D$edx+PeHeader.OptionalHeader.FileAlignmentDis

    ; start of virtual data begins at SectionsHeaders.VirtualAddressDis+SectionsHeaders.SizeOfRawDataDis

    .Do

        mov edx D$esi+SectionsHeaders.SrcMiscVirtualSizeDis
      ; Some compiler (Watcom-C) may set the RVA to zero. So... :
        If edx < D$esi+SectionsHeaders.SizeOfRawDataDis;SECTION_FILESIZE
            mov edx D$esi+SectionsHeaders.SizeOfRawDataDis;SECTION_FILESIZE
        End_If
        Align_On_Variable D@RvaSectionAlignment edx
        add edx D$esi+SectionsHeaders.VirtualAddressDis
        mov D@SectionRVASize edx ; end of virtual address

        mov edx D$esi+SectionsHeaders.SizeOfRawDataDis | Align_On_Variable D@FileAlignment edx | add edx D$esi+SectionsHeaders.VirtualAddressDis; edx = start of virtual data

        ..If D@SectionRVASize >= edx;D$esi+SectionsHeaders.SrcMiscVirtualSizeDis > edx ; On this section, If virtualsize > Size of raw data this section contains virtual data

            .If_And ebx >= edx, ebx < D@SectionRVASize;D$esi+SectionsHeaders.SrcMiscVirtualSizeDis
                mov eax &TRUE | ExitP
            .End_If

        ..End_If
        add esi SizeOf_SectionsHeaders
        dec ecx
    .Loop_Until ecx = 0

EndP


; SEE CheckExport and ReAlignPE


;;
; IMAGE_SECTION_HEADER

[SectionsHeaders.Name1Dis 0
 SectionsHeaders.SrcMiscVirtualSizeDis 8
 SectionsHeaders.VirtualAddressDis 12
 SectionsHeaders.SizeOfRawDataDis 16
 SectionsHeaders.PointerToRawDataDis 20
 SectionsHeaders.PointerToRelocationsDis 24
 SectionsHeaders.PointerToLinenumbersDis 28
 SectionsHeaders.NumberOfRelocationsDis 32
 SectionsHeaders.NumberOfLinenumbersDis 34
 SectionsHeaders.CharacteristicsDis 36]

[SizeOf_SectionsHeaders 40]

[SizeOf_SectionsHeaders 40]
[FirstSection: D$ 0-1]
[DisRvaSectionAlignment:D$  01000]

ReAlignPE:
    mov eax D$UserPeStart | sub D$UserPEStartOfResources eax ; Gona switch...
    ; eax POints to MZ

    mov ecx D$DisNumberOfSections, D$FirstSection 0-1, D$EndOfLastSection 0
    GetPeHeader SectionsHeaders
    ; eax points to .text

  ; Search for the First Section RVA:
L0: mov ebx D$eax+SectionsHeaders.VirtualAddressDis;SECTION_RVA
    On ebx < D$FirstSection, mov D$FirstSection ebx
  ; Search for the last Section RVA and adds its RVA Size:
    push ecx
        mov ecx D$eax+SectionsHeaders.SrcMiscVirtualSizeDis;SECTION_RVASIZE
      ; Some compiler (Watcom-C) may set the RVA to zero. So... :
        If ecx < D$eax+SectionsHeaders.SizeOfRawDataDis;SECTION_FILESIZE
            mov ecx D$eax+SectionsHeaders.SizeOfRawDataDis;SECTION_FILESIZE
            Align_On_Variable D$DisRvaSectionAlignment ecx
          ; Fix it (just in case this would be needed later...):
            mov D$eax+SectionsHeaders.SrcMiscVirtualSizeDis ecx;SECTION_RVASIZE ecx
        End_If

        On ebx > D$EndOfLastSection, mov D$EndOfLastSection ebx, edx ecx
    pop ecx
    add eax SizeOf_SectionsHeaders | loop L0<;SECTIONHEADERSIZE | loop L0<

    add edx ebx | Align_On_Variable D$DisRvaSectionAlignment edx | mov D$UserPeLen edx

    VirtualAlloc TempoUserPeStart edx ;D$UserPeLen

    mov esi D$UserPeStart, edi D$TempoUserPeStart

  ; Copy the PE headers down to (including) 'SectionsHeaders':
    GetPeHeader SectionsHeaders
    mov ecx eax | sub ecx D$UserPeStart | rep movsb

    mov ecx D$DisNumberOfSections

L0: push ecx
        mov ecx SizeOf_SectionsHeaders | rep movsb
    pop ecx
    loop L0<

    ; copy all data from the end of the Dos header (After the last SectionsHeaders) untill the 1st section of the pe
    push ecx
        mov ecx D$FirstSection
        add ecx D$UserPeStart
        sub ecx esi
        rep movsb
    pop ecx


  ; Want to skip 'RelocSectionTable', if any:
    GetPeHeader RelocSectionTable | move D$DisRelocPointer D$eax
  ; Copy all Sections with Memory alignment:
    GetPeHeader SectionsHeaders | mov edx D$DisNumberOfSections

    While D$eax+SECTION_RVA <> 0
        mov esi D$eax+SECTION_FILEPOINTER | add esi D$UserPeStart
        mov edi D$eax+SECTION_RVA | On edi = D$DisRelocPointer, jmp L1>
        add edi D$TempoUserPeStart
        mov ecx D$eax+SECTION_FILESIZE | Align_On 4 ecx | shr ecx 2 | rep movsd
L1:     add eax SECTIONHEADERSIZE | dec edx | jz L2>
    End_While

L2: Exchange D$UserPeStart D$TempoUserPeStart
    mov eax D$UserPeStart | add eax D$UserPeLen | mov D$UserPeEnd eax
    VirtualFree D$TempoUserPeStart
ret



CheckExport:
    GetPeHeader SectionTable | On D$eax = 0, ret ; eax points to PeHeader.DataDirectory.ExportDis

    mov D$NumberOfForwardedExport 0

    push eax
        mov edi D$eax, ecx D$eax+4, al EXPORTFLAG; eax = 02060 ecx = 046 PeHeader.DataDirectory.ExportSizeDis
        add edi D$SectionsMap | rep stosb
    pop eax

    mov edx D$eax | add edx D$UserPeStart
    mov eax D$edx+(5*4), ebx D$edx+(6*4)
    On ebx > eax, mov eax ebx
    add edx (6*4)
    mov D$NumberOfDisExportedFunctions eax
; 0476 = 1142 in wsock32.dll !!!   04B  Forwarded Functions (forwarded to other DLLs) !!!
    On D$NumberOfDisExportedFunctions = 0, jmp L9>>
  ; 'ExportSectionComments'
    add edx 4 | mov eax D$edx | add eax D$UserPeStart
    mov D$DisExportFunctionsPointers eax

    add edx 4 | mov eax D$edx | add eax D$UserPeStart
    mov D$DisExportNamesPointers eax

    add edx 4 | mov eax D$edx | add eax D$UserPeStart
    mov D$DisExportOrdinal eax

  ; Mark the Exported Functions as Nodes in the Code Routing Table:
    mov esi D$DisExportFunctionsPointers, ecx D$NumberOfDisExportedFunctions

L0: lodsd
    .If eax = 0
        loop L0<
    .Else
        add eax D$SectionsMap
        If B$eax = EXPORTFLAG
            inc D$NumberOfForwardedExport | loop L0<
        Else
            sub eax D$SectionsMap | add eax D$RoutingMap
            or B$eax NODE+INSTRUCTION+EXPORTNODE+ACCESSED+EVOCATED+PUSH_EBP+LABEL
            sub eax D$RoutingMap | add eax D$SectionsMap | mov B$eax CODEFLAG | loop L0<
        End_If
    .End_If

    If D$NumberOfForwardedExport <> 0
        mov eax D$NumberOfForwardedExport, edi ForwardedMessage
        call WriteEaxDecimal
        While edi < Forwarded | mov B$edi ' ' | inc edi | End_While
        call 'USER32.MessageBoxA', 0, ForwardedMessage,
                                  {' Warning', 0}, 0
    End_If
ret

;;





























