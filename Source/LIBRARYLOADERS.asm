TITLE LIBRARYLOADERS









Proc TinyLoadLibraryExW:
    Arguments @lpLibFileName, @hFile, @dwFlags
    Local @DllCharacteristics, @BaseAddress, @Var1, @Var2, @AndFlag, @NTStatus, @SearchPathFlag
    Structure @UnicodeString 8, @UnicodeString.LenghtDis 0, @UnicodeString.MaximumLenghtDis 2, @UnicodeString.BufferDis 4
    Uses ebx, esi, ecx, edx

    mov eax D@dwFlags | and eax &LOAD_LIBRARY_AS_DATAFILE__&LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE | mov D@AndFlag eax
    If_Or D@lpLibFileName = 0, D@hFile <> 0, D@dwFlags > 0FFFF, eax = &LOAD_LIBRARY_AS_DATAFILE__&LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE
        call BaseSetLastNTError &STATUS_INVALID_PARAMETER
        xor eax eax
        ExitP
    End_If

    call RtlInitUnicodeStringEx D@UnicodeString, D@lpLibFileName
    If eax <> &STATUS_SUCCESS
        call BaseSetLastNTError eax
        xor eax eax
        ExitP
    Else_If W@UnicodeString.LenghtDis = 0
        call BaseSetLastNTError &STATUS_INVALID_PARAMETER
        xor eax eax
        ExitP
    End_If


    ; Verify if the string ends with spaces. If it do recalculate the lenght without them.
    movzx ecx W@UnicodeString.LenghtDis | sub ecx 2 ; don´ count the null terminated byte
    mov edx D@UnicodeString.BufferDis
    While W$edx+ecx = ' '
        sub ecx 2
    End_While
    add ecx 2 ; add the next word to we have the full lenght
    If cx = 0
        call BaseSetLastNTError &STATUS_INVALID_PARAMETER
        xor eax eax
        ExitP
    End_If
    mov W@UnicodeString.LenghtDis cx


    call LoadFlagsToSearchPathFlags D@dwFlags
    mov D@SearchPathFlag eax
    mov D@BaseAddress 0
    mov ebx D@dwFlags

    Test_If bl &LOAD_LIBRARY_AS_DATAFILE__&LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE__&LOAD_LIBRARY_AS_IMAGE_RESOURCE
        call BaseSetLastNTError &STATUS_INVALID_PARAMETER
        xor eax eax
        ExitP
    Test_End

    or D@SearchPathFlag 1
    call LoadFlagsToDllCharacteristics D@dwFlags
    mov D@DllCharacteristics eax

    lea edx D@BaseAddress
    lea ecx D@DllCharacteristics
    ;C_call DummyLoader D@SearchPathFlag, ecx, D@UnicodeString, edx
    call 'ntdll.LdrLoadDll' D@SearchPathFlag, ecx, D@UnicodeString, edx
    mov D@NTStatus eax

    If D@NTStatus >s= &STATUS_SUCCESS
        mov eax D@BaseAddress
    Else
        call BaseSetLastNTError D@NTStatus; status Parameter at ecx
        xor eax eax
    End_If

EndP

DummyLoader:

    jmp 'ntdll.LdrLoadDll'

ret




;;
       RtlInitUnicodeStringEx
 
  Initializes a buffered Unicode string.
 
 Arguuments
    Source - Pointer to a Unicode string
    Target - Pointer to a UNICODE_STRING structure
 
  RETURNS
   An appropriate NTSTATUS value.
 
    Success: STATUS_SUCCESS. target is initialized.
    Failure: STATUS_NAME_TOO_LONG, if the source string is larger than 65532 bytes. 32766*2
 
  NOTES
   Assigns source to target->Buffer. The length of source is assigned to
   target->Length and target->MaximumLength. If source is NULL the length of source is assumed to be 0.

reference:
https://stuff.mit.edu/afs/sipb/project/wine/src/wine-0.9.37/dlls/ntdll/rtlstr.c
;;

Proc RtlInitUnicodeStringEx:
    Arguments @Target, @Source
    Uses ecx, edx

    mov edx D@Target
    mov ecx D@Source

    mov W$edx+UNICODE_STRING.LenghtDis 0
    mov W$edx+UNICODE_STRING.MaximumLenghtDis 0
    mov D$edx+UNICODE_STRING.BufferDis ecx

    ...If ecx = 0
        mov eax &STATUS_SUCCESS
    ...Else
        call StrLenProcW D@Source
        If eax <= 07FFE
            shl eax 1
            mov W$edx+UNICODE_STRING.LenghtDis ax
            add eax 2
            mov W$edx+UNICODE_STRING.MaximumLenghtDis ax
            mov eax &STATUS_SUCCESS
        Else
            mov eax &STATUS_NAME_TOO_LONG
        End_If

    ...End_If
EndP

Proc LdrpDllCharacteristicsToLoadFlags:
    Arguments @DllCharacteristics
    Uses ecx

    mov eax D@DllCharacteristics
    mov ecx eax
    and eax &LOAD_PACKAGED_LIBRARY;4
    add eax eax

    Test_If cl &IMAGE_FILE_EXECUTABLE_IMAGE
        or eax &LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE;040
    Test_End

    Test_If ecx 0800000
        or eax &LOAD_LIBRARY_REQUIRE_SIGNED_TARGET;080
    Test_End

    Test_If ecx 01000
        or eax &LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR;0100
    Test_End

    If ecx <s 0
        or eax 0400000
    End_If

EndP


Proc LoadFlagsToDllCharacteristics:
    Arguments @LoadFlags
    Uses ecx

    mov ecx D@LoadFlags

    xor eax eax
    Test_If cl &DONT_RESOLVE_DLL_REFERENCES
        mov eax &IMAGE_FILE_EXECUTABLE_IMAGE
    Test_End

    ;If cl <s 0; same as Test_If cl 080 &LOAD_LIBRARY_REQUIRE_SIGNED_TARGET
     ;   or eax 0800000
    ;End_If

    Test_If cl &LOAD_LIBRARY_REQUIRE_SIGNED_TARGET; from: https://www.cyberforum.ru/blogs/172954/blog5934.html
        or eax 0800000
    Test_End

    Test_If cl &LOAD_PACKAGED_LIBRARY; from: https://www.cyberforum.ru/blogs/172954/blog5934.html
        or eax 4
    Test_End

    Test_If ecx &LOAD_LIBRARY_OS_INTEGRITY_CONTINUITY; from: https://www.cyberforum.ru/blogs/172954/blog5934.html
        or eax 080000000
    Test_End

EndP


Proc LoadFlagsToSearchPathFlags:
    Arguments @LoadFlags

    mov eax D@LoadFlags
    and eax 07F08 ; &LOAD_WITH_ALTERED_SEARCH_PATH convert to DllCharacteristics
    ; &LOAD_LIBRARY_SEARCH_DEFAULT_DIRS  &IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE
    ;mov eax (Not &IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE__&LOAD_WITH_ALTERED_SEARCH_PATH)
    ; https://www.cyberforum.ru/blogs/172954/blog5934.html
    ;mov eax (not &LOAD_LIBRARY_OS_INTEGRITY_CONTINUITY__&LOAD_WITH_ALTERED_SEARCH_PATH)

EndP












































