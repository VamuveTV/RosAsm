TITLE StrSafe


;;
    String Safe funtions as found in VisualStudio (strsafe.h)

    Main Functions (Can be used as exports):

        StringCchPrintf    StringCchPrintfW
        StringCchCopyN     StringCchCopyNW
        StringCchCopy      StringCchCopyW


    Additional Functions:
        StringValidateDest     StringLengthWorker     StringVPrintfWorker
        StringCopyWorker

        StringValidateDestW     StringLengthWorkerW     StringVPrintfWorkerW
        StringCopyWorkerW

;;


_____________________________________________________________________
_____________________________________________________________________

;;

STDAPI
StringCchPrintf(
    __out_ecount(cchDest) LPTSTR  pszDest,
    __in size_t  cchDest,
    __in __format_string  LPCTSTR pszFormat,
    ...
    );

Routine Description:

    This routine is a safer version of the C built-in function 'sprintf'.
    The size of the destination buffer (in characters) is a parameter and
    this function will not write past the end of this buffer and it will
    ALWAYS null terminate the destination buffer (unless it is zero length).

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was printed without truncation and null terminated,
    otherwise it will return a failure code. In failure cases it will return
    a truncated version of the ideal result.

Arguments:

    pszDest     -  destination string

    cchDest     -  size of destination buffer in characters
                   length must be sufficient to hold the resulting formatted
                   string, including the null terminator.

    pszFormat   -  format string which must be null terminated

    ...         -  additional parameters to be formatted according to
                   the format string

Notes:
    Behavior is undefined if destination, format strings or any arguments
    strings overlap.

    pszDest and pszFormat should not be NULL.  See StringCchPrintfEx if you
    require the handling of NULL values.

Return Value:

    S_OK           -   if there was sufficient space in the dest buffer for
                       the resultant string and it was null terminated.

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the print
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

;;


Proc StringCchPrintf:
    Arguments @pszDest, @cchDest, @pszFormat, @ArgList

    call StringValidateDest D@pszDest, D@cchDest, &NULL, &STRSAFE_MAX_CCH
    If eax = &SUCCEEDED
        lea eax D@ArgList
        call StringVPrintfWorker D@pszDest, D@cchDest, &NULL, D@pszFormat, eax
    End_If

EndSTD
_____________________________________________________________________

Proc StringValidateDest:
    Arguments @pszDest, @cchDest, @pcchDestLength, @cchMax
    Local @Result

    mov D@Result &S_OK
    mov eax D@cchDest
    If_Or eax = 0, eax > D@cchMax
        mov D@Result &STRSAFE_E_INVALID_PARAMETER
    End_If

    .If D@pcchDestLength <> 0
        If D@Result = &STRSAFE_E_INVALID_PARAMETER
            call StringLengthWorker D@pszDest, D@cchDest, D@pcchDestLength
            mov D@Result eax
        Else
            mov eax D@pcchDestLength
            mov D$eax 0
        End_If
    .End_If
    mov eax D@Result

EndP

_____________________________________________________________________

Proc StringLengthWorker:
    Arguments @psz, @cchMax, @pcchLength
    Local @Result, @cchOriginalMax
    Uses ebx

    mov D@Result &S_OK
    mov ebx D@cchMax | mov D@cchOriginalMax ebx
    mov eax D@psz

    While ebx <> 0
        ;On W$eax = 0, jmp L1>
        On B$eax = 0, jmp L1>
        ;add eax 2
        inc eax
        dec ebx
    End_While
L1:

    If ebx = 0
        mov D@Result &STRSAFE_E_INVALID_PARAMETER
    End_If

    .If D@pcchLength <> 0
        mov eax D@cchOriginalMax | sub eax ebx
        mov ebx D@pcchLength
        If D@Result = &STRSAFE_E_INVALID_PARAMETER
            mov D$ebx eax
        Else
            mov D$ebx 0
        End_If
    .End_If

    mov eax D@Result

EndP
_____________________________________________________________________

Proc StringVPrintfWorker:
    Arguments @pszDest, @cchDest, @pcchNewDestLength, @pszFormat, @argList
    Local @Result, @cchMax
    Uses edx, ecx

    mov D@Result &S_OK
    mov eax D@cchDest | dec eax | mov D@cchMax eax
    C_call 'msvcrt._vsnprintf' D@pszDest, D@cchMax, D@pszFormat, D@argList
    .If_Or eax <s 0, eax =>s D@cchMax

        If eax <> D@cchMax
            mov D@Result &STRSAFE_E_INSUFFICIENT_BUFFER
        End_If

        mov eax D@cchMax
        ;mov ecx D@pszDest | lea edx D$ecx+eax*2 | mov W$edx 0
        ;mov ecx D@pszDest | lea edx D$ecx+eax | mov B$edx 0

    ;.Else
    ;    mov ecx D@pszDest | lea edx D$ecx+eax | mov B$edx 0
    .End_If
    mov ecx D@pszDest | lea edx D$ecx+eax | mov B$edx 0

    If D@pcchNewDestLength <> 0
        mov edx D@pcchNewDestLength
        mov D$edx eax
    End_If

    mov eax D@Result

EndP
_____________________________________________________________________
_____________________________________________________________________

;;

STDAPI
StringCbCopyN(
    __out_bcount(cbDest) LPTSTR  pszDest,
    __in  size_t  cbDest,
    __in  LPCTSTR pszSrc,
    __in  size_t  cbToCopy
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncpy'.
    The size of the destination buffer (in bytes) is a parameter and this
    function will not write past the end of this buffer and it will ALWAYS
    null terminate the destination buffer (unless it is zero length).

    This routine is meant as a replacement for strncpy, but it does behave
    differently. This function will not pad the destination buffer with extra
    null termination characters if cbToCopy is greater than the size of pszSrc.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the entire string or the first cbToCopy characters were
    copied without truncation and the resultant destination string was null
    terminated, otherwise it will return a failure code. In failure cases as
    much of pszSrc will be copied to pszDest as possible, and pszDest will be
    null terminated.

Arguments:

    pszDest        -   destination string

    cbDest         -   size of destination buffer in bytes.
                       length must be = ((_tcslen(src) + 1) * sizeof(TCHAR)) to
                       hold all of the source including the null terminator

    pszSrc         -   source string

    cbToCopy       -   maximum number of bytes to copy from source string,
                       not including the null terminator.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL.  See StringCbCopyEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.

;;

[STRSAFE_MAX_LENGTH (&STRSAFE_MAX_CCH-1)]

Proc StringCchCopyN:
    Arguments @pszDest, @cchDest, @pszSrc, @cchToCopy
    Uses ecx

    call StringValidateDest D@pszDest, D@cchDest, &NULL, &STRSAFE_MAX_CCH
    .If eax = &SUCCEEDED
        If D@cchToCopy >s STRSAFE_MAX_LENGTH
            mov eax &STRSAFE_E_INVALID_PARAMETER
            mov ecx D@pszDest
            mov B$ecx 0
        Else
            call StringCopyWorker D@pszDest, D@cchDest, &NULL, D@pszSrc, D@cchToCopy
        End_If
    .End_If

EndP

_____________________________________________________________________

Proc StringCopyWorker:
    Arguments @pszDest, @cchDest, @pcchNewDestLength, @pszSrc, @cchToCopy
    Local @Result, @cchNewDestLength
    Uses ebx, esi, edi, ecx

    mov D@Result &S_OK
    mov D@cchNewDestLength 0

    mov esi D@pszSrc
    mov edi D@pszDest
    mov ecx D@cchNewDestLength

    While D@cchDest <> 0
        On D@cchToCopy = 0, jmp L1>
        ;On W$esi = 0, jmp L1>
        On B$esi = 0, jmp L1>
        ;mov bx W$esi | mov W$edi bx
        mov bl B$esi | mov B$edi bl
;        add edi 2
;        add esi 2
        inc edi
        inc esi
        dec D@cchDest
        dec D@cchToCopy
        inc ecx
    End_While
L1:
    If D@cchDest = 0
        ;sub edi 2
        dec edi
        dec ecx
        mov D@Result &STRSAFE_E_INSUFFICIENT_BUFFER
    End_If

    ;mov W$edi 0
    mov B$edi 0
    If D@pcchNewDestLength <> 0
        mov eax D@pcchNewDestLength
        mov D$eax ecx
    End_If
    mov eax D@Result

EndP

_____________________________________________________________________
_____________________________________________________________________

;;

STDAPI
StringCchPrintf(
    __out_ecount(cchDest) LPTSTR  pszDest,
    __in size_t  cchDest,
    __in __format_string  LPCTSTR pszFormat,
    ...
    );

Routine Description:

    This routine is a safer version of the C built-in function 'sprintf'.
    The size of the destination buffer (in characters) is a parameter and
    this function will not write past the end of this buffer and it will
    ALWAYS null terminate the destination buffer (unless it is zero length).

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was printed without truncation and null terminated,
    otherwise it will return a failure code. In failure cases it will return
    a truncated version of the ideal result.

Arguments:

    pszDest     -  destination string

    cchDest     -  size of destination buffer in characters
                   length must be sufficient to hold the resulting formatted
                   string, including the null terminator.

    pszFormat   -  format string which must be null terminated

    ...         -  additional parameters to be formatted according to
                   the format string

Notes:
    Behavior is undefined if destination, format strings or any arguments
    strings overlap.

    pszDest and pszFormat should not be NULL.  See StringCchPrintfEx if you
    require the handling of NULL values.

Return Value:

    S_OK           -   if there was sufficient space in the dest buffer for
                       the resultant string and it was null terminated.

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the print
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok.

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function

;;


Proc StringCchPrintfW:
    Arguments @pszDest, @cchDest, @pszFormat, @ArgList

    call StringValidateDestW D@pszDest, D@cchDest, &NULL, &STRSAFE_MAX_CCH
    If eax = &SUCCEEDED
        lea eax D@ArgList
        call StringVPrintfWorkerW D@pszDest, D@cchDest, &NULL, D@pszFormat, eax
    End_If

EndSTD
_____________________________________________________________________

Proc StringValidateDestW:
    Arguments @pszDest, @cchDest, @pcchDestLength, @cchMax
    Local @Result

    mov D@Result &S_OK
    mov eax D@cchDest
    If_Or eax = 0, eax > D@cchMax
        mov D@Result &STRSAFE_E_INVALID_PARAMETER
    End_If

    .If D@pcchDestLength <> 0
        If D@Result = &STRSAFE_E_INVALID_PARAMETER
            call StringLengthWorkerW D@pszDest, D@cchDest, D@pcchDestLength
            mov D@Result eax
        Else
            mov eax D@pcchDestLength
            mov D$eax 0
        End_If
    .End_If
    mov eax D@Result

EndP

_____________________________________________________________________

Proc StringLengthWorkerW:
    Arguments @psz, @cchMax, @pcchLength
    Local @Result, @cchOriginalMax
    Uses ebx

    mov D@Result &S_OK
    mov ebx D@cchMax | mov D@cchOriginalMax ebx
    mov eax D@psz

    While ebx <> 0
        On W$eax = 0, jmp L1>
        add eax 2
        dec ebx
    End_While
L1:

    If ebx = 0
        mov D@Result &STRSAFE_E_INVALID_PARAMETER
    End_If

    .If D@pcchLength <> 0
        mov eax D@cchOriginalMax | sub eax ebx
        mov ebx D@pcchLength
        If D@Result = &STRSAFE_E_INVALID_PARAMETER
            mov D$ebx eax
        Else
            mov D$ebx 0
        End_If
    .End_If

    mov eax D@Result

EndP
_____________________________________________________________________

Proc StringVPrintfWorkerW:
    Arguments @pszDest, @cchDest, @pcchNewDestLength, @pszFormat, @argList
    Local @Result, @cchMax
    Uses edx, ecx

    mov D@Result &S_OK
    mov eax D@cchDest | dec eax | mov D@cchMax eax

    C_call 'msvcrt._vsnwprintf' D@pszDest, D@cchMax, D@pszFormat, D@argList

    .If_Or eax <s 0, eax =>s D@cchMax

        If eax <> D@cchMax
            mov D@Result &STRSAFE_E_INSUFFICIENT_BUFFER
        End_If

        mov eax D@cchMax
        ;mov ecx D@pszDest | lea edx D$ecx+eax*2 | mov W$edx 0

    .End_If
    mov ecx D@pszDest | lea edx D$ecx+eax*2 | mov W$edx 0

    If D@pcchNewDestLength <> 0
        mov edx D@pcchNewDestLength
        mov D$edx eax
    End_If

    mov eax D@Result

EndP
_____________________________________________________________________
_____________________________________________________________________

;;

STDAPI
StringCbCopyN(
    __out_bcount(cbDest) LPTSTR  pszDest,
    __in  size_t  cbDest,
    __in  LPCTSTR pszSrc,
    __in  size_t  cbToCopy
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strncpy'.
    The size of the destination buffer (in bytes) is a parameter and this
    function will not write past the end of this buffer and it will ALWAYS
    null terminate the destination buffer (unless it is zero length).

    This routine is meant as a replacement for strncpy, but it does behave
    differently. This function will not pad the destination buffer with extra
    null termination characters if cbToCopy is greater than the size of pszSrc.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the entire string or the first cbToCopy characters were
    copied without truncation and the resultant destination string was null
    terminated, otherwise it will return a failure code. In failure cases as
    much of pszSrc will be copied to pszDest as possible, and pszDest will be
    null terminated.

Arguments:

    pszDest        -   destination string

    cbDest         -   size of destination buffer in bytes.
                       length must be = ((_tcslen(src) + 1) * sizeof(TCHAR)) to
                       hold all of the source including the null terminator

    pszSrc         -   source string

    cbToCopy       -   maximum number of bytes to copy from source string,
                       not including the null terminator.

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL.  See StringCbCopyEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.

;;

;[STRSAFE_MAX_LENGTH (&STRSAFE_MAX_CCH-1)]

Proc StringCchCopyNW:
    Arguments @pszDest, @cchDest, @pszSrc, @cchToCopy
    Uses ecx

    call StringValidateDestW D@pszDest, D@cchDest, &NULL, &STRSAFE_MAX_CCH
    .If eax = &SUCCEEDED
        If D@cchToCopy >s STRSAFE_MAX_LENGTH
            mov eax &STRSAFE_E_INVALID_PARAMETER
            mov ecx D@pszDest
            mov W$ecx 0
        Else
            call StringCopyWorkerW D@pszDest, D@cchDest, &NULL, D@pszSrc, D@cchToCopy
        End_If
    .End_If

EndP

_____________________________________________________________________

Proc StringCopyWorkerW:
    Arguments @pszDest, @cchDest, @pcchNewDestLength, @pszSrc, @cchToCopy
    Local @Result, @cchNewDestLength
    Uses ebx, esi, edi, ecx

    mov D@Result &S_OK
    mov D@cchNewDestLength 0

    mov esi D@pszSrc
    mov edi D@pszDest
    mov ecx D@cchNewDestLength

    While D@cchDest <> 0
        On D@cchToCopy = 0, jmp L1>
        On W$esi = 0, jmp L1>
        mov bx W$esi | mov W$edi bx
        add edi 2
        add esi 2
        dec D@cchDest
        dec D@cchToCopy
        inc ecx
    End_While
L1:
    If D@cchDest = 0
        sub edi 2
        dec ecx
        mov D@Result &STRSAFE_E_INSUFFICIENT_BUFFER
    End_If

    mov W$edi 0
    If D@pcchNewDestLength <> 0
        mov eax D@pcchNewDestLength
        mov D$eax ecx
    End_If
    mov eax D@Result

EndP
_____________________________________________________________________
_____________________________________________________________________

;;
STDAPI
StringCchCopy(
    __out_ecount(cchDest) LPTSTR  pszDest,
    __in  size_t  cchDest,
    __in  LPCTSTR pszSrc
    );

Routine Description:

    This routine is a safer version of the C built-in function 'strcpy'.
    The size of the destination buffer (in characters) is a parameter and
    this function will not write past the end of this buffer and it will
    ALWAYS null terminate the destination buffer (unless it is zero length).

    This routine is not a replacement for strncpy.  That function will pad the
    destination string with extra null termination characters if the count is
    greater than the length of the source string, and it will fail to null
    terminate the destination string if the source string length is greater
    than or equal to the count. You can not blindly use this instead of strncpy:
    it is common for code to use it to "patch" strings and you would introduce
    errors if the code started null terminating in the middle of the string.

    This function returns a hresult, and not a pointer.  It returns
    S_OK if the string was copied without truncation and null terminated,
    otherwise it will return a failure code. In failure cases as much of
    pszSrc will be copied to pszDest as possible, and pszDest will be null
    terminated.

Arguments:

    pszDest        -   destination string

    cchDest        -   size of destination buffer in characters.
                       length must be = (_tcslen(src) + 1) to hold all of the
                       source including the null terminator

    pszSrc         -   source string which must be null terminated

Notes:
    Behavior is undefined if source and destination strings overlap.

    pszDest and pszSrc should not be NULL. See StringCchCopyEx if you require
    the handling of NULL values.

Return Value:

    S_OK           -   if there was source data and it was all copied and the
                       resultant dest string was null terminated

    failure        -   you can use the macro HRESULT_CODE() to get a win32
                       error code for all hresult failure cases

      STRSAFE_E_INSUFFICIENT_BUFFER /
      HRESULT_CODE(hr) == ERROR_INSUFFICIENT_BUFFER
                   -   this return value is an indication that the copy
                       operation failed due to insufficient space. When this
                       error occurs, the destination buffer is modified to
                       contain a truncated version of the ideal result and is
                       null terminated. This is useful for situations where
                       truncation is ok

    It is strongly recommended to use the SUCCEEDED() / FAILED() macros to test the
    return value of this function.
;;

Proc StringCchCopy:
    Arguments @pszDest, @cchDest, @pszSrc
    Uses ecx

    call StringValidateDest D@pszDest, D@cchDest, &NULL, &STRSAFE_MAX_CCH
    If eax = &SUCCEEDED
        call StringCopyWorker D@pszDest, D@cchDest, &NULL, D@pszSrc, STRSAFE_MAX_LENGTH
    End_If

EndP

___________________________________________________________________

Proc StringCchCopyW:
    Arguments @pszDest, @cchDest, @pszSrc
    Uses ecx

    call StringValidateDestW D@pszDest, D@cchDest, &NULL, &STRSAFE_MAX_CCH
    If eax = &SUCCEEDED
        call StringCopyWorkerW D@pszDest, D@cchDest, &NULL, D@pszSrc, STRSAFE_MAX_LENGTH
    End_If

EndP

















































