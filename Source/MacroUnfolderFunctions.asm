TITLE MacroUnfolderFunctions

____________________________________________________________________________________________
____________________________________________________________________________________________
;;
           Unfolder's jobs.
           
  'ShowUnfoldMacro' ---> 'ShowUnfoldDialog' ---> 'UnfoldMacro' ---> 'AsmMain'
  
  Both 'AsmMain' and (after final RET), 'UnfoldMacro' call for 'UnfoldOutput'.

    Internal Functions:
    
        ShowUnfoldMacro     ShowUnfoldDialog
        ResizeEditControl


    Used functions outside this routines:
    
        KillDebugger    AsmMain     ReleaseAsmTables
        AskForRedrawNow

;;
____________________________________________________________________________________________
____________________________________________________________________________________________

[UnfoldedMacro: ?    InstructionToUnfold: ?    StackBeforeUnfolding: ?]

[WeAreUnfolding: D$ 0]

[ShowUnfoldDialogHandle: D$ 0]

[IDD_UNFOLDMACRO 23000]
[IDC_EDIT_MACRO 101]


Proc ShowUnfoldMacro:

    If D$DebugDialogHandle <> 0
        call KillDebugger | On eax = &IDNO, ExitP
    End_If

    .If D$ShowUnfoldDialogHandle = 0
        mov B$CompileErrorHappend &FALSE
        call 'USER32.DialogBoxParamA' D$hInstance, IDD_UNFOLDMACRO, &NULL, ShowUnfoldDialog, &NULL
    .Else
        Beep
    .End_If

EndP
____________________________________________________________________________________________

;[UnfoldTitle: B$ 'Macro Unfolding', 0]

; Tag Dialog 23000

Proc ShowUnfoldDialog:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    .If D@Message = &WM_COMMAND

        If_Or W@wParam = &IDCANCEL, W@wParam = &IDOK
            mov D$ShowUnfoldDialogHandle 0
            call 'USER32.EndDialog' D@Adressee, 0
        End_If
;;
         If W@wParam = &IDCANCEL
L0:         mov D$ShowUnfoldDialogHandle 0
            call 'USER32.EndDialog' D@Adressee, 0

         Else_If W@wParam = &IDOK
            jmp L0<

         End_If
;;
    .Else_If D@Message = &WM_SIZE

        call ResizeEditControl D$ShowUnfoldDialogHandle, IDC_EDIT_MACRO

    .Else_If D@Message = &WM_INITDIALOG

        move D$ShowUnfoldDialogHandle D@Adressee
        call 'USER32.SetClassLongA' D@Adressee, &GCL_HICON, D$wc_hIcon

        call UnfoldMacro

        If D$UnfoldCompleted = &FALSE
            ;jmp L0<
            mov D$ShowUnfoldDialogHandle 0
            call 'USER32.EndDialog' D@Adressee, 0
        Else
            call 'USER32.SendMessageA' D@Adressee, &WM_SETTEXT, &NULL, {B$ 'Macro Unfolding', 0}
            mov B$FirstCTLCOLOREDIT &TRUE
        End_If

    .Else_If D@Message = &WM_CTLCOLOREDIT
        If B$FirstCTLCOLOREDIT = &TRUE
            call 'USER32.SendMessageA' D@lParam, &EM_SETSEL, 0, 0
            mov B$FirstCTLCOLOREDIT &FALSE
        End_If
        call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
        popad | mov eax D$DialogsBackGroundBrushHandle | ExitP;jmp L9>

    .Else_If B$CompileErrorHappend = &TRUE
        mov D$ShowUnfoldDialogHandle 0
        call 'USER32.EndDialog' D@Adressee, 0

    .Else
        popad | mov eax &FALSE | ExitP;jmp L9>

    .End_If

    popad | mov eax &TRUE

;L9:

EndP
____________________________________________________________________________________________

Proc ResizeEditControl:
    Arguments @Address, @IDControl
    Structure @RECT 16, @RECT_leftDis 0, @RECT_topDis 4, @RECT_rightDis 8, @RECT_bottomDis 12
    Uses ecx, edx, ebx

    call 'USER32.GetClientRect' D@Address, D@RECT

    call 'USER32.GetDlgItem' D@Address, D@IDControl

    mov ebx D@RECT_rightDis | sub ebx D@RECT_leftDis
    mov ecx D@RECT_bottomDis | sub ecx D@RECT_topDis

    call 'USER32.MoveWindow' eax, 0, 0, ebx, ecx, &TRUE

EndP
____________________________________________________________________________________________

[UnfoldEqual: D$ ?]
[UnfoldCompleted: D$ 0]

[UnfoldStepIndice: D$ 0]

UnfoldMacro:
    call 'USER32.SetCursor' D$WaitCursor | call AskForRedrawNow

    mov D$UnfoldStepIndice 0
    mov D$WeAreUnfolding &TRUE
    mov D$TrashPointer Trash3

    push D$SourceLen, D$SourceEnd

        mov D$StackBeforeUnfolding esp

        call AsmMain
      ; 'AsmMain' stops after the Macros jobs, in cases when "D$WeAreUnfolding = &TRUE".

        call 'USER32.SetCursor' D$ActualCursor

        call UnfoldOutput D$CodeSourceA

        mov D$edi CRLF2, B$edi+4 0

      ; Show the result:
        mov eax D$CodeSourceB
        If D$eax <> 0
            call 'USER32.SetDlgItemTextA' D$ShowUnfoldDialogHandle, 101, Trash3
            mov D$UnfoldCompleted &TRUE
        Else
            mov D$UnfoldCompleted &FALSE
        End_If

L8: pop D$SourceEnd, D$SourceLen

    ;VirtualFree D$LabelList, D$MacroList, D$PlainLabelList,
    ;        D$StatementsTable, D$StatementsTable2, D$CodeSourceB, D$CodeSourceA
    call ReleaseAsmTables

    mov D$WeAreUnfolding &FALSE, B$ReadyToRun &FALSE

    ;call ReleaseAsmTables
ret

; see Ros_FindRestoredStack

UnfoldingError:
    mov D$UnfoldErrorMessage eax, B$CompileErrorHappend &TRUE, D$UnfoldCompleted &FALSE

    call 'USER32.SetCursor' D$ActualCursor

    While esp <> D$StackBeforeUnfolding
        pop ebx
    End_While

    jmp L8<<
____________________________________________________________________________________________

; Note: Fixed on 07/08/2018
[UnfoldErrorMessage: ?]

Proc GetUnfoldStatement:
    Arguments @Input, @InputFileLen, @Output, @StatementIndex
    Uses esi, ebx, ecx
    ;push esi, ebx, ecx

    mov esi D@Input;D$CodeSourceA
    mov eax 0
    mov edx esi | add edx D@InputFileLen;D$SourceLen

    While eax <> D@StatementIndex;ecx
        .If B$esi = EOI
            If B$esi+1 = OpenBracket
                    ;
            Else_If B$esi+1 = OpenVirtual
                    ;
            Else
                inc eax
            End_If
        .Else_If B$esi = OpenBracket
            inc eax
        .Else_If B$esi = OpenVirtual
            inc eax
        .End_If
            ;inc esi | On esi > edx, jmp L8>>
        inc esi | On esi > edx, ExitP
    End_While

    ; Translate into normal Ascii form:
;    mov edi D@Output;D$TrashPointer

    If B$esi-1 = OpenVirtual ; 016
        dec esi | call TranslateDeclarationToNormalAscii esi, D@Output;D$TrashPointer
        ;mov edi eax
    Else_If B$esi-1 = OpenBracket ; 014
        dec esi | call TranslateDeclarationToNormalAscii esi, D@Output;D$TrashPointer
        ;mov edi eax
    Else
        call TranslateCodeToNormalAscii esi, D@Output;D$TrashPointer
        ;mov edi eax
    End_If

;L8:
   ; pop ecx, ebx, esi
;ret
EndP
____________________________________________________________________________________________

Proc TranslateDeclarationToNormalAscii:
    Arguments @Input, @Output
    Uses esi, edi

    mov esi D@Input
    mov edi D@Output

L0:
    lodsb

    .If al = Space
        mov al ' '
    .Else_If al = EOI
        jmp L2>>
    .Else_If al = meEOI
        mov al CR | stosb | mov al LF
    .Else_If al = TextSign
        mov B$edi '"' | inc edi
        While B$esi <> TextSign
            lodsb
            If al = CR
                mov W$edi CRLF | add edi 2 | add esi 2
            Else
                stosb
            End_If
        End_While
        inc esi | mov al '"'

    .Else_If al = MemMarker
        mov al '$'
    .Else_If al = OpenBracket
        mov al '['
    .Else_If al = CloseVirtual
        mov al ']' | stosb
        mov al CR | stosb | mov al LF | stosb | jmp L2>>
    .Else_If al = CloseBracket
        mov al ']' | stosb
        mov al CR | stosb | mov al LF | stosb | jmp L2>>
    .Else_If al = OpenVirtual
        mov al '['
    .Else_If al = AddSign
        mov al '+'
    .Else_If al = SubSign
        mov al '-'
    .Else_If al = MulSign
        mov al '*'
    .Else_If al = DivSign
        mov al '/'
    .Else_If al = numSign
        mov al '#'
    .Else_If al = colonSign
        mov al ':' | stosb | mov al ' ' | stosb
    .End_If

    stosb

    If al = LF
        mov D$edi '    ' | add edi 4
    End_If

    jmp L0<<

L2:
    mov ax CRLF | stosw

    mov eax edi

EndP

TranslateDeclarationToNormalAscii_Old:
L0: lodsb

    .If al = Space
        mov al ' '
    .Else_If al = EOI
        jmp L2>>
    .Else_If al = meEOI
        mov al CR | stosb | mov al LF
    .Else_If al = TextSign
        mov B$edi '"' | inc edi
        While B$esi <> TextSign
            lodsb
            If al = CR
                mov W$edi CRLF | add edi 2 | add esi 2
            Else
                stosb
            End_If
        End_While
        inc esi | mov al '"'

    .Else_If al = MemMarker
        mov al '$'
    .Else_If al = OpenBracket
        mov al '['
    .Else_If al = CloseVirtual
        mov al ']' | stosb
        mov al CR | stosb | mov al LF | stosb | jmp L2>>
    .Else_If al = CloseBracket
        mov al ']' | stosb
        mov al CR | stosb | mov al LF | stosb | jmp L2>>
    .Else_If al = OpenVirtual
        mov al '['
    .Else_If al = AddSign
        mov al '+'
    .Else_If al = SubSign
        mov al '-'
    .Else_If al = MulSign
        mov al '*'
    .Else_If al = DivSign
        mov al '/'
    .Else_If al = numSign
        mov al '#'
    .Else_If al = colonSign
        mov al ':' | stosb | mov al ' ' | stosb
    .End_If

    stosb

    If al = LF
        mov D$edi '    ' | add edi 4
    End_If

    jmp L0<<

L2: mov ax CRLF | stosw ;| mov al 0 | stosb
ret
____________________________________________________________________________________________

Proc TranslateCodeToNormalAscii:
    Arguments @Input, @Output
    Uses esi, edi

    mov esi D@Input
    mov edi D@Output

L0:
    lodsb

    .If al = Space
        mov al ' '
    .Else_If al = EOI
        jmp L2>>
    .Else_If al = meEOI
        mov al CR | stosb | mov al LF
    .Else_If al = TextSign
        mov B$edi '"' | inc edi
        While B$esi <> TextSign
            lodsb
            If al = CR
                mov W$edi CRLF | add edi 2 | add esi 2
            Else
                stosb
            End_If
        End_While
        inc esi | mov al '"'
    .Else_If al = MemMarker
        mov al '$'
    .Else_If al = OpenBracket
        jmp L2>>
    .Else_If al = CloseVirtual
        mov al ']'
    .Else_If al = OpenVirtual
        mov al '['
    .Else_If al = AddSign
        mov al '+'
    .Else_If al = SubSign
        mov al '-'
    .Else_If al = MulSign
        mov al '*'
    .Else_If al = DivSign
        mov al '/'
    .Else_If al = numSign
        mov al '#'
    .Else_If al = colonSign
        mov al ':' | stosb | mov al CR | stosb | mov al LF
    .End_If

    stosb

    If al = LF
        mov D$edi '    ' | add edi 4
    End_If

    jmp L0<<

L2: mov ax CRLF | stosw

    mov eax edi

EndP

TranslateCodeToNormalAscii_Old:
L0: lodsb

    .If al = Space
        mov al ' '
    .Else_If al = EOI
        jmp L2>>
    .Else_If al = meEOI
        mov al CR | stosb | mov al LF
    .Else_If al = TextSign
        mov B$edi '"' | inc edi
        While B$esi <> TextSign
            lodsb
            If al = CR
                mov W$edi CRLF | add edi 2 | add esi 2
            Else
                stosb
            End_If
        End_While
        inc esi | mov al '"'
    .Else_If al = MemMarker
        mov al '$'
    .Else_If al = OpenBracket
        jmp L2>>
    .Else_If al = CloseVirtual
        mov al ']'
    .Else_If al = OpenVirtual
        mov al '['
    .Else_If al = AddSign
        mov al '+'
    .Else_If al = SubSign
        mov al '-'
    .Else_If al = MulSign
        mov al '*'
    .Else_If al = DivSign
        mov al '/'
    .Else_If al = numSign
        mov al '#'
    .Else_If al = colonSign
        mov al ':' | stosb | mov al CR | stosb | mov al LF
    .End_If

    stosb

    If al = LF
        mov D$edi '    ' | add edi 4
    End_If

    jmp L0<<

L2: mov ax CRLF | stosw ;| mov al 0 | stosb
ret
____________________________________________________________________________________________

; Note: Fixed on 28/05/2020

; pos '0' = 31

[UnfoldSteps: "
******************************
*    Macros-Engine Pass: %d  *
******************************

    ", 0]

Proc UnfoldOutput:
    Arguments @Input
    Local @FileLen, @StatementIndex

    inc D$UnfoldStepIndice
    mov edi D$TrashPointer
    C_call FormatStr edi, UnfoldSteps, D$UnfoldStepIndice
    add edi eax
    mov B$edi 0
    mov D$TrashPointer edi

    ; Count how many Statements, in the 'StatementsTable', down to our Line:
    mov ebx D$BlockStartTextPtr, esi D$StatementsTable

  ; For "some reason", leading Labels must be included in the Statement:
    mov eax ebx | dec eax | While B$eax = ' ' | dec eax | End_While
    If B$eax = ':'
        While B$eax-1 > ' ' | dec eax | End_While
        mov ebx eax
    End_If

  ; Unfold upon an Equal Pre-Parser Statement:
    If B$ebx-1 = ASCII_DOLLAR ;'$' ; CharMessage ASCII_DOLLAR
        sub ebx 2
    Else_If B$ebx-1 = ASCII_PARAGRAPH ;'$'
        sub ebx 2
    End_If

    call StrLenproc D@Input;$CodeSourceA
    mov D@FileLen eax

    call GetLenghtForUnfoldData D@Input, eax, esi, ebx, 1

    ; Several Statements are possible. Example, in Data and in Code with a Para-Macro:
    mov D@StatementIndex 1
    While D$esi <> 0
        If D$esi = ebx
            ;call GetUnfoldStatement D$CodeSourceA, D@FileLen, D$TrashPointer, D@StatementIndex;ecx
            call GetUnfoldStatement D@Input, D@FileLen, D$TrashPointer, D@StatementIndex;ecx
            mov edi eax
            mov D$edi '    ' | add edi 4
            mov D$TrashPointer edi
        End_If

        add esi 4; | inc ecx
        inc D@StatementIndex
    End_While

EndP


Proc GetLenghtForUnfoldData:
    Arguments @Input, @FileLen, @StatementTable, @Srcpos, @StatementIndex
    Uses esi, ebx, ecx

    mov ebx D@Srcpos
    mov esi D@StatementTable
    xor ecx ecx
    .While D$esi <> 0
        If D$esi = ebx
            call GetUnfoldStatementLenght D@Input, D@FileLen, &NULL, D@StatementIndex
            add ecx eax
        End_If
        add esi 4
        inc D@StatementIndex
    .End_While

    mov eax ecx

EndP

Proc GetUnfoldStatementLenght:
    Arguments @Input, @InputFileLen, @Output, @StatementIndex
    Uses esi, ebx, ecx

    mov esi D@Input
    mov eax 0
    mov edx esi | add edx D@InputFileLen

    While eax <> D@StatementIndex;ecx
        .If B$esi = EOI
            If B$esi+1 = OpenBracket
                    ;
            Else_If B$esi+1 = OpenVirtual
                    ;
            Else
                inc eax
            End_If
        .Else_If B$esi = OpenBracket
            inc eax
        .Else_If B$esi = OpenVirtual
            inc eax
        .End_If
        inc esi
        ;On esi > edx, ExitP
        If esi > edx
            xor eax eax | ExitP
        End_If
    End_While

    ; Translate into normal Ascii form:
    xor eax eax
    If B$esi-1 = OpenVirtual ; 016
        dec esi; | call TranslateDeclarationToNormalAscii esi, D@Output
        ;While B$esi <> EOI
         ;   inc eax
        ;End_While
    Else_If B$esi-1 = OpenBracket ; 014
        dec esi; | call TranslateDeclarationToNormalAscii esi, D@Output
    ;Else
        ;call TranslateCodeToNormalAscii esi, D@Output
    End_If

    While B$esi+eax <> EOI
        inc eax
    End_While

EndP




































































