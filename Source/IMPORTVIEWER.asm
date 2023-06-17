TITLE IMPORTVIEWER
_______________________________________________________________________________________
 _______________________________________________________________________________________



;;

        ShowSourceImports

        ShowSourceImports      ViewSourceImportsProc      DllSrcListViewSort

;;




;;
____________________________________________________________________________________________
____________________________________________________________________________________________

 Used DLL in the actual Source.
;;

; Main Dialog
[IDD_DLL_IAT_IN_SRC2 1110]
[IDC_DLL_LVIEW 107]
[IDC_DLL_SIGLE_LINE 101]
[IDC_DLL_MULTI_LINE 102]
[IDC_DLL_MACRO 103]
[IDC_DLL_INFO 104]
[IDC_DLL_EXIT 105]
[IDC_DLL_SEARCH_XREF 106]
[IDC_DLL_XREF_LVIEW 100]

[INITCOMMONCONTROLSEX.dwSizeDis 0
 INITCOMMONCONTROLSEX.dwICCDis 4]
[Size_Of_INITCOMMONCONTROLSEX 8]

Proc ShowSourceImports:
    Structure @INITCOMMONCONTROLSEX 8, @INITCOMMONCONTROLSEX.dwSizeDis 0,  @INITCOMMONCONTROLSEX.dwICCDis 4

    If D$ImportDialogHandle = 0
        mov D@INITCOMMONCONTROLSEX.dwSizeDis Size_Of_INITCOMMONCONTROLSEX
        mov D@INITCOMMONCONTROLSEX.dwICCDis &ICC_LISTVIEW_CLASSES
        call 'COMCTL32.InitCommonControlsEx' D@INITCOMMONCONTROLSEX
        call 'USER32.DialogBoxParamA' D$hinstance, IDD_DLL_IAT_IN_SRC2, &NULL, ViewSourceImportsProc, &NULL
    End_If

EndP

____________________________________________________________________________________________

[hDLLIATList: D$ 0]
[hDLLXRefIATList: D$ 0]
[MAKEWPARAM | ((#2 shl 16) or #1)]

[IDC_DLL_SRC 10]
[IDC_DLL_FUNCTION_SRC 11]
[IDC_DLL_REFERENCES_SRC 12]
[IDC_DLL_FIND_FIRST_SRC 3]
[IDC_DLL_FIND_NEXT_SRC 4]

[ImportDialogHandle: D$ 0]

Proc ViewSourceImportsProc:
    Arguments @Adressee, @Message, @wParam, @lParam

    pushad

    ...If D@Message = &WM_COMMAND
        mov eax D@wParam | and eax 0FFFF
        ..If eax = &IDCANCEL
            mov D$ImportDialogHandle 0
            call 'User32.EndDialog' D@Adressee, 0
            call 'ole32.OleUninitialize'

        ..Else
            .If W@wParam = IDC_DLL_SRC
                If W@wParam+2 = &LBN_SELCHANGE
                    call RestoreRealSource
                    ;call ViewDllFunctionList see this...it ws a call similar to InitImportsProcList
                    call SetPartialEditionFromPos
                End_If
                call 'USER32.SetDlgItemInt' D@Adressee, IDC_DLL_REFERENCES_SRC, 0, &FALSE
                call Disable D@adressee, IDC_DLL_FIND_FIRST_SRC
                call Disable D@adressee, IDC_DLL_FIND_NEXT_SRC

            .Else_If W@wParam = IDC_DLL_FUNCTION_SRC
                If W@wParam+2 = &LBN_SELCHANGE

                    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETCURSEL, 0, 0
                    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETITEMDATA, eax, 0

                    call 'USER32.SetDlgItemInt' D@Adressee, IDC_DLL_REFERENCES_SRC, eax, &FALSE
                    call 'USER32.SendDlgItemMessageA' D@Adressee, IDC_DLL_FIND_FIRST_SRC, &WM_ENABLE, &TRUE, 0
                    call Enable D@adressee, IDC_DLL_FIND_FIRST_SRC
                    call Disable D@adressee, IDC_DLL_FIND_NEXT_SRC

                End_If

            .Else_If W@wParam = IDC_DLL_FIND_FIRST_SRC
                call ImportFunctionFindFirst

                call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETCURSEL, 0, 0
                call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETITEMDATA, eax, 0
                On eax > 1, call Enable D@adressee, IDC_DLL_FIND_NEXT_SRC

            .Else_If W@wParam = IDC_DLL_FIND_NEXT_SRC

                call ImportFunctionFindNext

            .End_If
        ..End_If

    ...Else_If D@Message = &WM_INITDIALOG
        call 'COMCTL32.InitCommonControls'
        call 'ole32.OleInitialize' &NULL

        move D$ImportDialogHandle D@Adressee

        call 'USER32.SetClassLongA' D@Adressee &GCL_HICON D$wc_hIcon

        call 'user32.GetDlgItem' D@Adressee, IDC_DLL_XREF_LVIEW | mov D$hDLLXRefIATList eax
        call 'user32.GetDlgItem' D@Adressee, IDC_DLL_LVIEW | mov D$hDLLIATList eax
        call SetupDLLXRefIATListview D$hDLLXRefIATList
        call SetupDLLIATListview D$hDLLIATList

        call RestoreRealSource
        call InitImportsProcList D$hDLLIATList, D$hDLLXRefIATList
        call SetPartialEditionFromPos

        call CoolControl_LVBeginSort DllSrcListViewSort, DllSrcSortDecimal, D$hDLLIATList, 0
        call CoolControl_LVBeginSort DllXRefListViewSort, DllXRefSortDecimal, D$hDLLXRefIATList, 0
        call LV_SetSelectedItem D$hDLLIATList, 0

        ;call 'USER32.SetFocus' D$hDLLIATList
       ; call 'USER32.GetWindowLongA' D$hDLLIATList, &GWL_EXSTYLE ; 5011 0009
        ;call 'USER32.SetFocus' D$hDLLIATList
        If B$DLLsFoundInSource = &FALSE
            call 'USER32.MessageBoxA' D$hwnd, {'No Import Function found in this Source', 0}, {'Failure:', 0}, 0
            call 'USER32.EndDialog' D@Adressee, 0
        End_If

        ; simulate a user clicking on the listview control
        ; http://blogs.msdn.com/b/oldnewthing/archive/2004/08/02/205624.aspx
        ;call 'USER32.PostMessageA' D@Adressee, &WM_COMMAND, {MAKEWPARAM IDC_DLL_LVIEW, &NM_CLICK}, D$hDLLIATList
        call 'USER32.PostMessageA' D@Adressee, &WM_NEXTDLGCTL, D$hDLLIATList, &TRUE
        call 'USER32.ShowWindow' D@Adressee, &SW_NORMAL
        call 'USER32.UpdateWindow' D@Adressee
        call 'USER32.SetFocus' D@Adressee

    ...Else_If D@Message = &WM_NOTIFY

        call ImportViewDialog_OnNotify D@Adressee, D@lParam

    ...Else_If D@Message = &WM_CLOSE
        call LibScanCleanUp D@Adressee
        call 'USER32.EndDialog' D@Adressee, &NULL
        mov D$LibScanToolbarHandle 0
        mov B$LibScanIsFileOpen &FALSE
        call 'ole32.OleUninitialize'

    ;...Else_If D@Message =
    ;...Else_If D@Message = &WM_CTLCOLOREDIT
        ;call 'GDI32.SetBkColor' D@wParam D$DialogsBackColor
        ;popad | mov eax D$DialogsBackGroundBrushHandle | ExitP
    ;...Else_If D@Message = &WM_PAINT

    ...Else
        popad | mov eax &FALSE | ExitP
        ;call 'USER32.DefDlgProcA' D@Adressee, D@Message, D@wParam, D@lParam
        ;popad
        ;ExitP

    ...End_If

L8: popad | mov eax &TRUE

EndP
___________________________________________________________________________________________________________________________________

Proc LV_SetSelectedItem:
    Arguments @hList, @Item
    Uses edi, ebx, esi, ecx, edx

    call ListView_SetItemState D@hList, 0-1, 0, &LVIS_SELECTED ; deselect all
    call 'user32.SendMessageA' D@hList, &LVM_ENSUREVISIBLE, D@Item, &TRUE;FALSE ; Send to the Listview
    call ListView_SetItemState D@hList, D@Item, &LVIS_SELECTED, &LVIS_SELECTED
    call ListView_SetItemState D@hList, D@Item, &LVIS_FOCUSED, &LVIS_FOCUSED

EndP
___________________________________________________________________________________________________________________________________

Proc ListView_SetItemState:
    Arguments @hList, @Item, @state, @mask
    Structure @LV_ITEM 40, @LV_ITEM.imaskDis 0,  @LV_ITEM.iItemDis 4,  @LV_ITEM.iSubItemDis 8,  @LV_ITEM.stateDis 12,  @LV_ITEM.stateMaskDis 16,
                           @LV_ITEM.pszTextDis 20,  @LV_ITEM.cchTextMaxDis 24,  @LV_ITEM.iImageDis 28,  @LV_ITEM.lParamDis 32,  @LV_ITEM.iIndentDis 36
    Uses edi, ebx, esi, ecx, edx

    call ClearBuffer D@LV_ITEM, 40
    move D@LV_ITEM.stateDis D@state
    move D@LV_ITEM.stateMaskDis D@mask
    call 'user32.SendMessageA' D@hList, &LVM_SETITEMSTATE, D@Item, D@LV_ITEM

EndP
___________________________________________________________________________________________________________________________________


Proc LVSearchinSource:
    Arguments @StringtoSearch, @Pos
    Uses D$LenOfSearchedString

    pushad

        mov esi D@StringtoSearch
        mov edi SearchString
        xor ecx ecx

        Do
            movsb
            inc ecx
            On B$esi = 0, jmp L1>
        Loop_Until ecx >= 120
L1:
        mov B$edi 0

        mov D$LenOfSearchedString ecx
        call RestoreRealSource

        call LVStartSearching D@Pos

        call SetPartialEditionFromPos
        mov eax D$CodeSource | add eax D@Pos

    popad

EndP

___________________________________________________________________________________________________________________________________

Proc LVStartSearching:
    Arguments @Pos
    Local @CurUpLine
    Uses D$DownSearch, D$CaseSearch, D$WholeWordSearch

    pushad
        mov B$DownSearch &TRUE, B$CaseSearch &TRUE, B$WholeWordSearch &FALSE

        mov eax D$CodeSource | add eax D@Pos
        mov D$CurrentWritingPos eax
        If D$TiTleTable <> 0
            call GetActualPartFromPos
            move D@CurUpLine D$ActualTitle
            mov eax D$CodeSource | add eax D@Pos
            mov D$CurrentWritingPos eax
        Else
            move D@CurUpLine D$CodeSource
            mov eax D$CodeSource | add eax D@Pos
            mov D$CurrentWritingPos eax
        End_If

        call SetCaret D$CurrentWritingPos
        move D$UpperLine D@CurUpLine
        call AskForRedrawNow
        mov D$NextSearchPos 0
        call StringSearch
    popad

EndP

___________________________________________________________________________________________________________________________________

[NM_ITEMACTIVATE.hdr.hwndFromDis 0
 NM_ITEMACTIVATE.hdr.idFromDis 4
 NM_ITEMACTIVATE.hdr.codeDis 8
 NM_ITEMACTIVATE.iItemDis 12
 NM_ITEMACTIVATE.iSubItemDis 16
 NM_ITEMACTIVATE.uNewStateDis 20
 NM_ITEMACTIVATE.uOldStateDis 24
 NM_ITEMACTIVATE.uChangedDis 28
 NM_ITEMACTIVATE.ptAction.xDis 32
 NM_ITEMACTIVATE.ptAction.yDis 36
 NM_ITEMACTIVATE.lParamDis 40
 NM_ITEMACTIVATE.uKeyFlagsDis 44]

[Size_Of_NM_ITEMACTIVATE 48]

[DllSrc_TextSearchBuffer: B$ 0 #&MAXPATH]
[DllSrc_TextPosBuffer: B$ 0 #&MAXPATH]

Proc ImportViewDialog_OnNotify:
    Arguments @Adressee, @Notification
    Local @CurSelItem

    mov ebx D@Notification
    mov edx D$ebx+NMHDR.idFromDis
    mov eax D$ebx+NMHDR.codeDis

    .If edx = IDC_DLL_LVIEW
        If eax = &LVN_COLUMNCLICK
            call CoolControl_ListViewAlternateSort DllSrcListViewSort, D@Notification, DllSrcSortDecimal, D$hDLLIATList, DLLSrc_LVTotalCol
        Else_If eax = &NM_CLICK
            mov eax eax
            ;call 'USER32.GetWindowLongA' D$hDLLIATList, &GWL_EXSTYLE ; 5011 0009
        ;Else_If eax = &NM_DBLCLK
            ;call SaveOneObjectFile D@Adressee
        End_If

    .Else_If edx = IDC_DLL_XREF_LVIEW
        If eax = &LVN_COLUMNCLICK
            call CoolControl_ListViewAlternateSort DllXRefListViewSort, D@Notification, DllXRefSortDecimal, D$hDLLXRefIATList, DLLXRefSrc_LVTotalCol
        Else_If eax = &NM_DBLCLK
            ;call SaveOneObjectFile D@Adressee
            move D@CurSelItem D$ebx+NM_ITEMACTIVATE.iItemDis
            call LVRetrieveSubiTemText D$hDLLXRefIATList, DllSrc_TextSearchBuffer, D@CurSelItem, DLL_XREF_LVIEW_TEXT, &FALSE
            call LVRetrieveSubiTemText D$hDLLXRefIATList, DllSrc_TextPosBuffer, D@CurSelItem, DLL_XREF_LVIEW_REFERENCE, &FALSE
            call String2Dword DllSrc_TextPosBuffer
            call LVSearchinSource DllSrc_TextSearchBuffer, eax
        End_If
    .End_If

EndP

___________________________________________________________________________________________________________________________________

[DllSrcSortDecimal: D$ 0] ; Sort Buffer
[DllSrc_ListViewBuffer: B$ 0 #&MAXPATH]

Proc DllSrcListViewSort:
    Arguments @lParam1, @lParam2, @lParamSort
    Local @Dir1, @Dir2
    Structure @LV_ITEM 40, @LV_ITEM.imaskDis 0,  @LV_ITEM.iItemDis 4,  @LV_ITEM.iSubItemDis 8,  @LV_ITEM.stateDis 12,  @LV_ITEM.stateMaskDis 16,
                           @LV_ITEM.pszTextDis 20,  @LV_ITEM.cchTextMaxDis 24,  @LV_ITEM.iImageDis 28,  @LV_ITEM.lParamDis 32,  @LV_ITEM.iIndentDis 36
    Uses edi, ebx, esi, ecx, edx

    and D@Dir1 0
    and D@Dir2 0
    mov D@LV_ITEM.imaskDis &LVIF_TEXT
    move D@LV_ITEM.pszTextDis DllSrc_ListViewBuffer
    mov D@LV_ITEM.cchTextMaxDis &MAXPATH

    mov D@LV_ITEM.iSubItemDis DLL_LVIEW_DLLNAME
    call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
    xor eax eax
    mov al B$DllSrc_ListViewBuffer
    mov D@Dir1 eax
    call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM
    xor eax eax
    mov al B$DllSrc_ListViewBuffer
    mov D@Dir2 eax

    ; Decimal Value [Header1: B$ 'Index',0]
    ..If_Or D@lParamSort = ((2*DLL_LVIEW_INDEX)+1), D@lParamSort = ((DLL_LVIEW_INDEX+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_LVIEW_INDEX
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call String2Dword DllSrc_ListViewBuffer
        mov edi eax
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM
        call String2Dword DllSrc_ListViewBuffer

        If D@lParamSort = ((2*DLL_LVIEW_INDEX)+1)
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If

    ; Hexadecimal Value [Header2: B$ 'File Name',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_LVIEW_DLLNAME)+1), D@lParamSort = ((DLL_LVIEW_DLLNAME+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_LVIEW_DLLNAME
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call strcpy szBuff1, DllSrc_ListViewBuffer
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM

        If D@lParamSort = ((2*DLL_LVIEW_DLLNAME)+1)
            call lstrcmpi szBuff1, DllSrc_ListViewBuffer
        Else
            call lstrcmpi DllSrc_ListViewBuffer, szBuff1
        End_If

    ; [Header3: B$ 'Extension',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_LVIEW_FUNCTION)+1), D@lParamSort = ((DLL_LVIEW_FUNCTION+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_LVIEW_FUNCTION
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call strcpy szBuff1, DllSrc_ListViewBuffer
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM

        If D@lParamSort = ((2*DLL_LVIEW_FUNCTION)+1)
            call stricmp szBuff1, DllSrc_ListViewBuffer
        Else
            call stricmp DllSrc_ListViewBuffer, szBuff1
        End_If

    ; [Header4: B$ 'FileName Type',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_LVIEW_REFERENCE)+1), D@lParamSort = ((DLL_LVIEW_REFERENCE+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_LVIEW_REFERENCE
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call String2Dword DllSrc_ListViewBuffer
        mov edi eax
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM
        call String2Dword DllSrc_ListViewBuffer

        If D@lParamSort = ((2*DLL_LVIEW_REFERENCE)+1)
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If

    ; [Header5: B$ 'Path',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_LVIEW_UNDECORATED_NAME)+1), D@lParamSort = ((DLL_LVIEW_UNDECORATED_NAME+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_LVIEW_UNDECORATED_NAME
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call strcpy szBuff1, DllSrc_ListViewBuffer
        call 'USER32.SendMessageA' D$hDLLIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM

        If D@lParamSort = ((2*DLL_LVIEW_UNDECORATED_NAME)+1)
            call lstrcmpi szBuff1, DllSrc_ListViewBuffer
        Else
            call lstrcmpi DllSrc_ListViewBuffer, szBuff1
        End_If

    ..End_If
;---------- [Keep the folders on top] ----------

    .If_And B@Dir1 = '<', B@Dir2 <> '<'
        xor eax eax
        dec eax
        ExitP
    .Else_If_And B@Dir2 = '<', B@Dir1 <> '<'
        xor eax eax
        inc eax
        ExitP
    .Else_If_And B@Dir1 = '<', B@Dir2 = '<'
        xor eax eax
    .End_If

EndP
________________________________________________________________________________________

___________________________________________________________________________________________________________________________________

[DllXRefSortDecimal: D$ 0] ; Sort Buffer
[DllXRef_ListViewBuffer: B$ 0 #&MAXPATH]

Proc DllXRefListViewSort:
    Arguments @lParam1, @lParam2, @lParamSort
    Local @Dir1, @Dir2
    Structure @LV_ITEM 40, @LV_ITEM.imaskDis 0,  @LV_ITEM.iItemDis 4,  @LV_ITEM.iSubItemDis 8,  @LV_ITEM.stateDis 12,  @LV_ITEM.stateMaskDis 16,
                           @LV_ITEM.pszTextDis 20,  @LV_ITEM.cchTextMaxDis 24,  @LV_ITEM.iImageDis 28,  @LV_ITEM.lParamDis 32,  @LV_ITEM.iIndentDis 36
    Uses edi, ebx, esi, ecx, edx

    and D@Dir1 0
    and D@Dir2 0
    mov D@LV_ITEM.imaskDis &LVIF_TEXT
    move D@LV_ITEM.pszTextDis DllXRef_ListViewBuffer
    mov D@LV_ITEM.cchTextMaxDis &MAXPATH

    mov D@LV_ITEM.iSubItemDis DLL_XREF_LVIEW_DLLNAME
    call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
    xor eax eax
    mov al B$DllXRef_ListViewBuffer
    mov D@Dir1 eax
    call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM
    xor eax eax
    mov al B$DllXRef_ListViewBuffer
    mov D@Dir2 eax

    ; Decimal Value [Header1: B$ 'Index',0]
    ..If_Or D@lParamSort = ((2*DLL_XREF_LVIEW_INDEX)+1), D@lParamSort = ((DLL_XREF_LVIEW_INDEX+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_XREF_LVIEW_INDEX
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call String2Dword DllXRef_ListViewBuffer
        mov edi eax
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM
        call String2Dword DllXRef_ListViewBuffer

        If D@lParamSort = ((2*DLL_XREF_LVIEW_INDEX)+1)
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If

    ; Hexadecimal Value [Header2: B$ 'File Name',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_XREF_LVIEW_DLLNAME)+1), D@lParamSort = ((DLL_XREF_LVIEW_DLLNAME+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_XREF_LVIEW_DLLNAME
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call strcpy szBuff1, DllXRef_ListViewBuffer
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM

        If D@lParamSort = ((2*DLL_XREF_LVIEW_DLLNAME)+1)
            call lstrcmpi szBuff1, DllXRef_ListViewBuffer
        Else
            call lstrcmpi DllXRef_ListViewBuffer, szBuff1
        End_If

    ; [Header3: B$ 'Extension',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_XREF_LVIEW_FUNCTION)+1), D@lParamSort = ((DLL_XREF_LVIEW_FUNCTION+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_XREF_LVIEW_FUNCTION
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call strcpy szBuff1, DllXRef_ListViewBuffer
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM

        If D@lParamSort = ((2*DLL_XREF_LVIEW_FUNCTION)+1)
            call stricmp szBuff1, DllXRef_ListViewBuffer
        Else
            call stricmp DllXRef_ListViewBuffer, szBuff1
        End_If

    ; [Header3: B$ 'Extension',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_XREF_LVIEW_TEXT)+1), D@lParamSort = ((DLL_XREF_LVIEW_TEXT+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_XREF_LVIEW_TEXT
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call strcpy szBuff1, DllXRef_ListViewBuffer
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM

        If D@lParamSort = ((2*DLL_XREF_LVIEW_TEXT)+1)
            call stricmp szBuff1, DllXRef_ListViewBuffer
        Else
            call stricmp DllXRef_ListViewBuffer, szBuff1
        End_If

    ; [Header4: B$ 'FileName Type',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_XREF_LVIEW_REFERENCE)+1), D@lParamSort = ((DLL_XREF_LVIEW_REFERENCE+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_XREF_LVIEW_REFERENCE
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call String2Dword DllXRef_ListViewBuffer
        mov edi eax
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM
        call String2Dword DllXRef_ListViewBuffer

        If D@lParamSort = ((2*DLL_XREF_LVIEW_REFERENCE)+1)
            sub edi eax
            mov eax edi
        Else
            sub eax edi
        End_If

    ; [Header3: B$ 'Extension',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_XREF_LVIEW_LIST_TYPE)+1), D@lParamSort = ((DLL_XREF_LVIEW_LIST_TYPE+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_XREF_LVIEW_LIST_TYPE
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call strcpy szBuff1, DllXRef_ListViewBuffer
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM

        If D@lParamSort = ((2*DLL_XREF_LVIEW_LIST_TYPE)+1)
            call stricmp szBuff1, DllXRef_ListViewBuffer
        Else
            call stricmp DllXRef_ListViewBuffer, szBuff1
        End_If

    ; [Header5: B$ 'Path',0] ; string
    ..Else_If_Or D@lParamSort = ((2*DLL_XREF_LVIEW_UNDECORATED_NAME)+1), D@lParamSort = ((DLL_XREF_LVIEW_UNDECORATED_NAME+1)*2)

        mov D@LV_ITEM.iSubItemDis DLL_XREF_LVIEW_UNDECORATED_NAME
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam1, D@LV_ITEM
        call strcpy szBuff1, DllXRef_ListViewBuffer
        call 'USER32.SendMessageA' D$hDLLXRefIATList, &LVM_GETITEMTEXT, D@lParam2, D@LV_ITEM

        If D@lParamSort = ((2*DLL_XREF_LVIEW_UNDECORATED_NAME)+1)
            call lstrcmpi szBuff1, DllXRef_ListViewBuffer
        Else
            call lstrcmpi DllXRef_ListViewBuffer, szBuff1
        End_If

    ..End_If
;---------- [Keep the folders on top] ----------

    .If_And B@Dir1 = '<', B@Dir2 <> '<'
        xor eax eax
        dec eax
        ExitP
    .Else_If_And B@Dir2 = '<', B@Dir1 <> '<'
        xor eax eax
        inc eax
        ExitP
    .Else_If_And B@Dir1 = '<', B@Dir2 = '<'
        xor eax eax
    .End_If

EndP

________________________________________________________________________________________


; Used Macros


; Amount of Columns of the ListView
[DLLXRefSrc_LVTotalCol 7]

; String Data

[Dll_XRefSrc_Header1: B$ 'Index',0]
[Dll_XRefSrc_Header2: B$ 'Dll Name',0]
[Dll_XRefSrc_Header3: B$ 'Function',0]
[Dll_XRefSrc_Header4: B$ 'Text',0]
[Dll_XRefSrc_Header5: B$ 'Pos (in bytes)',0]
[Dll_XRefSrc_Header6: B$ 'Type',0]
[Dll_XRefSrc_Header7: B$ 'Undecorated Name',0]

; Our used Constants to identify what field is what
[DLL_XREF_LVIEW_INDEX 0]
[DLL_XREF_LVIEW_DLLNAME 01]
[DLL_XREF_LVIEW_FUNCTION 02]
[DLL_XREF_LVIEW_TEXT 03]
[DLL_XREF_LVIEW_REFERENCE 04]
[DLL_XREF_LVIEW_LIST_TYPE 05]
[DLL_XREF_LVIEW_UNDECORATED_NAME 06]


;[hHeader: D$ ?]
;[hList: D$ ?]

; LV_COLUMN Structure

[lvcXRefDll:
 lvcXRefDll.imask: D$ 0
 lvcXRefDll.fmt: D$ &LVCFMT_LEFT
 lvcXRefDll.lx: D$ 80  ; columnwidth
 lvcXRefDll.pszText: D$ 0
 lvcXRefDll.cchTextMax: D$ 0
 lvcXRefDll.iSubItem: D$ 0
 lvcXRefDll.iImage: D$ 0
 lvcXRefDll.iOrder: D$ 0]

Proc SetupDLLXRefIATListview:
    Arguments @h2List
    Local @hDllHeader

    ; /*Listview setup */
    mov D$lvcXRefDll.imask, &LVCF_TEXT+&LVCF_WIDTH
    mov D$lvcXRefDll.pszText, Dll_XRefSrc_Header1
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_XREF_LVIEW_INDEX, lvcXRefDll
    or D$lvcXRefDll.imask, &LVCF_FMT
    mov D$lvcXRefDll.pszText, Dll_XRefSrc_Header2
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_XREF_LVIEW_DLLNAME, lvcXRefDll
    or D$lvcXRefDll.imask, &LVCF_FMT
    mov D$lvcXRefDll.pszText, Dll_XRefSrc_Header3
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_XREF_LVIEW_FUNCTION, lvcXRefDll
    or D$lvcXRefDll.imask, &LVCF_FMT
    mov D$lvcXRefDll.pszText, Dll_XRefSrc_Header4
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_XREF_LVIEW_TEXT, lvcXRefDll
    or D$lvcXRefDll.imask, &LVCF_FMT
    mov D$lvcXRefDll.pszText, Dll_XRefSrc_Header5
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_XREF_LVIEW_REFERENCE, lvcXRefDll
    or D$lvcXRefDll.imask, &LVCF_FMT
    mov D$lvcXRefDll.pszText, Dll_XRefSrc_Header6
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_XREF_LVIEW_LIST_TYPE, lvcXRefDll
    or D$lvcXRefDll.imask, &LVCF_FMT
    mov D$lvcXRefDll.pszText, Dll_XRefSrc_Header7
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_XREF_LVIEW_UNDECORATED_NAME, lvcXRefDll



    ;/* these 5 lines create a FLAT columnheader */
    call 'user32.SendMessageA' D@h2List, &LVM_GETHEADER__&LVM_ENSUREVISIBLE__&LVM_SETCOLUMNORDERARRAY, 0, 0 ;// get handle to header;&LVM_GETHEADER, 0, 0 ;// get handle to header
    mov D@hDllHeader, eax ;// preserve header handle
    call 'user32.GetWindowLongA' D@hDllHeader, &GWL_STYLE ;// get current window styles
    xor eax, &HDS_BUTTONS
    call 'user32.SetWindowLongA' D@hDllHeader, &GWL_STYLE, eax ;// set the new header styles

    ;/* Setup extended styles like gridlines, back-foregroundcolors */
    call 'user32.SendMessageA' D@h2List, &LVM_SETEXTENDEDLISTVIEWSTYLE, 0, &LVS_EX_FULLROWSELECT__&LVS_EX_HEADERDRAGDROP__&LVS_EX_SUBITEMIMAGES__&LVS_EX_GRIDLINES__&LVS_EX_FLATSB
    call 'user32.SendMessageA' D@h2List, &LVM_SETTEXTCOLOR, 0, {RGB 186 48 38};{RGB 0 0 0} ;0
    call 'user32.SendMessageA' D@h2List, &LVM_SETBKCOLOR, 0, {RGB 255 255 255} ; 0FFFFFF
    call 'user32.SendMessageA' D@h2List, &LVM_SETTEXTBKCOLOR, 0, {RGB 240, 247, 166} ; 0A6F7F0

EndP

____________________________________________________________________________________________
___________________________________________

; Used Macros


; Amount of Columns of the ListView
[DLLSrc_LVTotalCol 5]

; String Data
[Dll_Src_Header1: B$ 'Index', 0]
[Dll_Src_Header2: B$ 'Dll Name', 0]
[Dll_Src_Header3: B$ 'Function', 0]
[Dll_Src_Header4: B$ 'References', 0]
[Dll_Src_Header5: B$ 'Undecorated Name', 0]

; Our used Constants to identify what field is what
[DLL_LVIEW_INDEX 0]
[DLL_LVIEW_DLLNAME 1]
[DLL_LVIEW_FUNCTION 2]
[DLL_LVIEW_REFERENCE 3]
[DLL_LVIEW_UNDECORATED_NAME 4]

; LV_COLUMN Structure

[lvcDll:
 lvcDll.imask: D$ 0
 lvcDll.fmt: D$ &LVCFMT_LEFT
 lvcDll.lx: D$ 80  ; columnwidth
 lvcDll.pszText: D$ 0
 lvcDll.cchTextMax: D$ 0
 lvcDll.iSubItem: D$ 0
 lvcDll.iImage: D$ 0
 lvcDll.iOrder: D$ 0]

Proc SetupDLLIATListview:
    Arguments @h2List
    Local @hDllHeader

    ; /*Listview setup */
    mov D$lvcDll.imask, &LVCF_TEXT__&LVCF_WIDTH__&LVCF_SUBITEM
    mov D$lvcDll.pszText, Dll_Src_Header1
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_LVIEW_INDEX, lvcDll
    or D$lvcDll.imask, &LVCF_FMT
    mov D$lvcDll.pszText, Dll_Src_Header2
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_LVIEW_DLLNAME, lvcDll
    or D$lvcDll.imask, &LVCF_FMT
    mov D$lvcDll.pszText, Dll_Src_Header3
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_LVIEW_FUNCTION, lvcDll
    or D$lvcDll.imask, &LVCF_FMT
    mov D$lvcDll.pszText, Dll_Src_Header4
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_LVIEW_REFERENCE, lvcDll
    or D$lvcDll.imask, &LVCF_FMT
    mov D$lvcDll.pszText, Dll_Src_Header5
    call 'user32.SendMessageA' D@h2list, &LVM_INSERTCOLUMN, DLL_LVIEW_UNDECORATED_NAME, lvcDll

    ; /* these 5 lines create a FLAT columnheader */
    call 'user32.SendMessageA' D@h2List, &LVM_GETHEADER__&LVM_ENSUREVISIBLE__&LVM_SETCOLUMNORDERARRAY, 0, 0 ; // get handle to header
    mov D@hDllHeader eax ; // preserve header handle
    call 'user32.GetWindowLongA' D@hDllHeader, &GWL_STYLE ; // get current window styles
    xor eax &HDS_BUTTONS
    call 'user32.SetWindowLongA' D@hDllHeader, &GWL_STYLE, eax ; // set the new header styles

    ; /* Setup extended styles like gridlines, back-foregroundcolors */
    call 'user32.SendMessageA' D@h2List, &LVM_SETEXTENDEDLISTVIEWSTYLE, 0, &LVS_EX_FULLROWSELECT__&LVS_EX_HEADERDRAGDROP__&LVS_EX_SUBITEMIMAGES__&LVS_EX_GRIDLINES__&LVS_EX_FLATSB
    call 'user32.SendMessageA' D@h2List, &LVM_SETTEXTCOLOR, 0, {RGB 186 48 38}
    call 'user32.SendMessageA' D@h2List, &LVM_SETBKCOLOR, 0, {RGB 255 255 255}
    call 'user32.SendMessageA' D@h2List, &LVM_SETTEXTBKCOLOR, 0, {RGB 240, 247, 166}

;;
    You will find that resizing any of the columns when a background image or watermark is set will cause the listview
    to flicker really bad. So bad that you wouldn't want to use it. 
    This is because the background is erased and redrawn before the item details are displayed every time the column is moved.
    Setting Listview1.Doublebuffered does not seem to improve the situation much.
    
    You have to set listview's REAL doublebuffer with the following call to the extended styles.
    
    Tjis also avoids flickering in backgroundcolors of the listview.
;;
    call 'user32.SendMessageA' D@h2List, &LVM_SETEXTENDEDLISTVIEWSTYLE, &LVS_EX_DOUBLEBUFFER, &LVS_EX_DOUBLEBUFFER

EndP
____________________________________________________________________________________________

ShowSourceExports:
    If D$ExportDialogHandle = 0
        call 'USER32.DialogBoxParamA' D$hinstance, 1101, &NULL,  ViewSourceExportsProc, &NULL
    End_If
ret
____________________________________________________________________________________________


[DLLsProcListHandle: ?    DLLsProcFunctionsListHandle: ?]

Proc Enable:
    Arguments @ParentHandle, @ID

    call 'User32.GetDlgItem' D@ParentHandle, D@ID
    call 'User32.EnableWindow' eax, &TRUE

EndP
____________________________________________________________________________________________

Proc Disable:
    Arguments @ParentHandle, @ID

    call 'User32.GetDlgItem' D@ParentHandle, D@ID
    call 'User32.EnableWindow' eax, &FALSE

EndP

____________________________________________________________________________________________

____________________________________________________________________________________________


ExportFunctionFindFirst:
    call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_GETCURSEL, 0, 0
    call 'USER32.SendMessageA' D$DLLsProcListHandle, &LB_GETTEXT, eax, SearchString

    .If eax <> &LB_ERR
        mov W$SearchString+eax '::' | add eax 2 | jmp L1>

ImportFunctionFindFirst:
    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETCURSEL, 0, 0
    call 'USER32.SendMessageA' D$DLLsProcFunctionsListHandle, &LB_GETTEXT, eax, SearchString

    .If eax <> &LB_ERR
        mov D$SearchString+eax "'" | inc eax
L1:     mov D$LenOfSearchedString eax

        call RestoreRealSource

        push D$DownSearch, D$CaseSearch, D$WholeWordSearch
            mov B$DownSearch &TRUE, B$CaseSearch &TRUE, B$WholeWordSearch &FALSE
            move D$CurrentWritingPos D$CodeSource
            call SetCaret D$CodeSource | move D$UpperLine D$CodeSource
            call AskForRedrawNow

                mov D$NextSearchPos 0

                call StringSearch

        pop D$WholeWordSearch, D$CaseSearch, D$DownSearch

        call SetPartialEditionFromPos

    .End_If
ret

____________________________________________________________________________________________

ImportFunctionFindNext:
ExportFunctionFindNext:
    call RestoreRealSource
        push D$DownSearch, D$CaseSearch, D$WholeWordSearch
            call StringSearch
        pop D$WholeWordSearch, D$CaseSearch, D$DownSearch
    call SetPartialEditionFromPos
ret
____________________________________________________________________________________________


[DLLsFoundInSource: ?]

[DLL_SRC.NFO:
 DLL_SRC.NFO.TotalModule: D$ 0
 DLL_SRC.NFO.TotalFunction: D$ 0]

[DLL_SRC:
 DLL_SRC.ModuleName: D$ 0
 DLL_SRC.ModuleFunctionList: D$ 0
 DLL_SRC.RefOffset: D$ 0 ; DLL_SRC.RefOffsetArray
 DLL_SRC.RefCount: D$ 0]

[DLL_SRC.RefOffsetArray: D$ 0]

[DLL_SRC_TYPE_CALL 0]
[DLL_SRC_TYPE_JMP 1]
[DLL_SRC_TYPE_MOV 2]
[DLL_SRC_TYPE_LEA 3]
[DLL_SRC_TYPE_PUSH 4]

Proc InitImportsProcList:
    Arguments @hDllList, @XRefhlist
    Local @StartText

    ; initilize index
    mov D$DllXRefSrcFunctionIndex '0000', D$DllXRefSrcFunctionIndex+4 '01'
    mov D$DllSrcFunctionIndex '0000', D$DllSrcFunctionIndex+4 '01'

    mov esi D$CodeSource, edx D$SourceEnd, B$DLLsFoundInSource &FALSE

    .While esi < edx

        ...If W$esi = ';;'

            ..If_Or esi = D$CodeSource, W$esi-2 = CRLF
                add esi 2 ; bypass 1st word
                On esi >= edx, ExitP
                If B$esi = 0D
                    call BypassMultiLineComment esi, edx | on eax = 0, ExitP
                Else
                    While B$esi = ';'
                        inc esi
                        On esi >= edx, ExitP
                    End_While
                    call BypassSingleLineComment esi, edx | on eax = 0, ExitP
                End_If
            ..Else
                While B$esi = ';'
                    inc esi
                    On esi >= edx, ExitP
                End_While
                call BypassSingleLineComment esi, edx | on eax = 0, ExitP
            ..End_If
            mov esi eax

        ...Else_If B$esi = ';'

            call BypassSingleLineComment esi, edx | on eax = 0, ExitP
            mov esi eax

        ...Else_If B$esi = '[' ; see SetColorsMap

            call BypassBracket D$CodeSource, esi, edx | on eax = 0, ExitP
            mov esi eax

        ...Else

            mov ecx edx | sub ecx esi | On ecx < 8, ExitP ; no room for small strings. Impossible addressing like: call 'kern....' | push 'user.xxx | mov eax 'comctl.xxx
            mov eax D$esi | or eax 020202020 | mov D@StartText esi
            ..If eax = 'call'
                add esi 4
                While B$esi = ' ' | inc esi | End_While
                mov al B$esi
                .If_Or al = '"', al = "'"
                    call AddDllSrcListviewItem D@XRefhlist, esi, edx, D@StartText, DLL_SRC_TYPE_CALL
                    call SysLb_DLL_FindString D@XRefhlist, D@hDllList
                    mov esi eax
                .End_If
            ..Else_If eax = 'jmp '
                add esi 3
                While B$esi = ' ' | inc esi | End_While
                mov al B$esi
                .If_Or al = '"', al = "'"
                    call AddDllSrcListviewItem D@XRefhlist, esi, edx, D@StartText, DLL_SRC_TYPE_JMP
                    call SysLb_DLL_FindString D@XRefhlist, D@hDllList
                    mov esi eax
                .End_If

            ..Else_If eax = 'mov '
                add esi 3
                While B$esi = ' ' | inc esi | End_While
                ; bypass second parameter
                While B$esi <> ' ' | inc esi | End_While
                inc esi ; bypass second ' '
                mov al B$esi
                .If_Or al = '"', al = "'"
                    call DllIsitTextParam esi, edx
                    If eax = &FALSE
                        call AddDllSrcListviewItem D@XRefhlist, esi, edx, D@StartText, DLL_SRC_TYPE_MOV
                        call SysLb_DLL_FindString D@XRefhlist, D@hDllList
                    End_If
                    mov esi eax
                .End_If

            ..Else_If eax = 'lea '
                add esi 3
                While B$esi = ' ' | inc esi | End_While
                ; bypass second parameter
                While B$esi <> ' ' | inc esi | End_While
                inc esi ; bypass second ' '
                mov al B$esi
                .If_Or al = '"', al = "'"
                    call DllIsitTextParam esi, edx
                    If eax = &FALSE
                        call AddDllSrcListviewItem D@XRefhlist, esi, edx, D@StartText, DLL_SRC_TYPE_LEA
                        call SysLb_DLL_FindString D@XRefhlist, D@hDllList
                    End_If
                    mov esi eax
                .End_If

            ..Else_If eax = 'push'
                add esi 4
                While B$esi = ' ' | inc esi | End_While
                mov al B$esi
                .If_Or al = '"', al = "'"
                    call DllIsitTextParam esi, edx
                    If eax = &FALSE
                        call AddDllSrcListviewItem D@XRefhlist, esi, edx, D@StartText, DLL_SRC_TYPE_PUSH
                        call SysLb_DLL_FindString D@XRefhlist, D@hDllList
                    End_If
                    mov esi eax
                .End_If

            ..Else_If_Or B$esi = '"', B$esi = "'"
                ; cases of  While D$esi <> '; Da' | inc esi | End_While
                mov al B$esi
                Do
                    inc esi
                Loop_Until B$esi = al
                inc esi
            ..Else
                 inc esi
            ..End_If

        ...End_If

    .End_While

EndP
____________________________________________________________________________________________

; on sucess, eax = 0. On failure, eax = the ending address at esi (after the last "'" or '"")
Proc DllIsitTextParam:
    Arguments @String, @SrcEnd
    Uses esi, ecx, ebx

    mov esi D@String
    xor ecx ecx
    mov edx D@SrcEnd
    xor eax eax

    mov bl B$esi
    Do
        inc esi | inc ecx
        On esi >= edx, ExitP
    Loop_Until B$esi = bl

    If ecx <= 5
        mov eax esi | inc eax ; bypass last "'" or '"'
    Else
        xor eax eax
    End_If

EndP
____________________________________________________________________________________________

[DllSrc_DLLNameBuffer: B$ 0 #&MAXPATH]
[DllSrc_ApiNameBuffer: B$ 0 #&MAXPATH]
;[DllSrcApiUndecoratedName
[DllSrcApiUndecoratedName2: B$ 0 #4000]

[Dll_IAT_LV_buffer: B$ 0 #&MAXPATH]
[DllSrcFunctionIndex: B$ '000001', 0]

[DllSrcReference: D$ 0, 0]

[DllSrc_RefBuffer: B$ 0 #&MAXPATH]

Proc SysLb_DLL_FindString:
    Argument @XRefhlist, @hDllList;@SrcStart, @SrcEnd, @pInitialText, @Flag
    Local @FirstFound, @Itens, @hDllItem, @StrRefPointer, @StrUndecoratePointer, @XRefItens, @CurItem
    Structure @LV_ITEM 40, @LV_ITEM.imaskDis 0,  @LV_ITEM.iItemDis 4,  @LV_ITEM.iSubItemDis 8,  @LV_ITEM.stateDis 12,  @LV_ITEM.stateMaskDis 16,
                           @LV_ITEM.pszTextDis 20,  @LV_ITEM.cchTextMaxDis 24,  @LV_ITEM.iImageDis 28,  @LV_ITEM.lParamDis 32,  @LV_ITEM.iIndentDis 36
    Uses esi, edi, edx, ecx, eax, ebx


    ; how many itens found in XRef dll lstview control ?
    call 'USER32.SendMessageA' D@XRefhlist, &LVM_GETITEMCOUNT, 0, 0 | mov D@XRefItens eax

    ; how many itens found in dll lstview control ?
    call 'USER32.SendMessageA' D@hDllList, &LVM_GETITEMCOUNT, 0, 0
    ...If eax = 0 ; no itens found. So this is the 1st item
        call LVRetrieveSubiTemText D@XRefhlist, DllSrc_DLLNameBuffer, 0, DLL_XREF_LVIEW_DLLNAME, &TRUE
        call LVRetrieveSubiTemText D@XRefhlist, DllSrc_ApiNameBuffer, 0, DLL_XREF_LVIEW_FUNCTION, &TRUE
        ; retrieve the xrefernce
        mov D$DllSrcReference 1

     ...Else
        mov D@Itens eax ; the total amount of dll index
        ; the current index to start searching is from 1 because item 0 was already filled at the very beginning when there was no itens whatsoever in both listview controls
        mov edi 0
        mov esi D@XRefItens
        .Do

            call LVRetrieveSubiTemText D@XRefhlist, DllSrc_DLLNameBuffer, edi, DLL_XREF_LVIEW_DLLNAME, &FALSE
            call LVRetrieveSubiTemText D@XRefhlist, DllSrc_ApiNameBuffer, edi, DLL_XREF_LVIEW_FUNCTION, &FALSE
            ; see if Api Name is inside the dll list
            call LVSearchString D@hDllList, DllSrc_ApiNameBuffer, 0, DLL_LVIEW_FUNCTION, &TRUE
            ..If eax = 0-1
                mov D$DllSrcReference 1 | jmp L1>> ; the string was not found, this is a new item
            ..Else
                mov D@CurItem eax
                ; make sure that the dllname+function name matches
                call LVSearchString D@hDllList, DllSrc_DLLNameBuffer, eax, DLL_LVIEW_DLLNAME, &FALSE
                .If eax <> 0-1 ; the string was already found
                    call LVRetrieveSubiTemText D@hDllList, DllSrc_RefBuffer, D@CurItem, DLL_LVIEW_INDEX, &FALSE
                    call LVRetrieveSubiTemText D@hDllList, DllSrc_RefBuffer, D@CurItem, DLL_LVIEW_REFERENCE, &FALSE

                    mov D$DllSrcReference 0 | mov D$DllSrcReference+4 0
                    call Asciito32 DllSrc_RefBuffer, DllSrcReference, &FALSE
                    inc D$DllSrcReference
                    jmp L1>>
                .End_If
            ..End_If
            inc edi

        .Loop_Until edi >= D@XRefItens

    ...End_If

L1:

    .If D$DllSrcReference = 1

    ; retrieve the index value
    call strcpy Dll_IAT_LV_buffer, DllSrcFunctionIndex

    call Dword32toAscii DllSrcReference, DllSrc_RefBuffer, (&MAXPATH-1), &FALSE | mov D@StrRefPointer eax

    ; write the full undecorated name here. WriteImportObjStrings
    call DllSrcUndecorateApiName DllSrc_ApiNameBuffer, DllSrcApiUndecoratedName2
    If eax = &TRUE
        move D@StrUndecoratePointer DllSrcApiUndecoratedName2
    Else
        move D@StrUndecoratePointer {"Not decorated", 0}
    End_If

    call LVAddItem D@hDllList | mov D@hDllItem eax
    call LVAddSubItem D@hDllList, DllSrcFunctionIndex, D@hDllItem, DLL_LVIEW_INDEX, &FALSE
    call LVAddSubItem D@hDllList, DllSrc_DLLNameBuffer, D@hDllItem, DLL_LVIEW_DLLNAME, &FALSE
    call LVAddSubItem D@hDllList, DllSrc_ApiNameBuffer, D@hDllItem, DLL_LVIEW_FUNCTION, &FALSE
    call LVAddSubItem D@hDllList, D@StrRefPointer, D@hDllItem, DLL_LVIEW_REFERENCE, &FALSE
    call LVAddSubItem D@hDllList, D@StrUndecoratePointer, D@hDllItem, DLL_LVIEW_UNDECORATED_NAME, &FALSE


    ; increment DllIndex
    lea ebx D$DllSrcFunctionIndex+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While

    .Else
        call Dword32toAscii DllSrcReference, DllSrc_RefBuffer, (&MAXPATH-1), &FALSE | mov D@StrRefPointer eax
        call LVAddSubItem D@hDllList, D@StrRefPointer, D@CurItem, DLL_LVIEW_REFERENCE, &FALSE
    .End_If

EndP

Proc LVRetrieveSubiTemText:
    Argument @Lb_Handle, @pOutput, @Item, @SubItem, @ScanfromLastItem
    Local @Itens
    Structure @LV_ITEM 40, @LV_ITEM.imaskDis 0,  @LV_ITEM.iItemDis 4,  @LV_ITEM.iSubItemDis 8,  @LV_ITEM.stateDis 12,  @LV_ITEM.stateMaskDis 16,
                           @LV_ITEM.pszTextDis 20,  @LV_ITEM.cchTextMaxDis 24,  @LV_ITEM.iImageDis 28,  @LV_ITEM.lParamDis 32,  @LV_ITEM.iIndentDis 36
    Uses esi, edi, edx, ecx


    call ClearBuffer D@LV_ITEM, 40
    mov D@LV_ITEM.imaskDis &LVIF_TEXT
    move D@LV_ITEM.iSubItemDis D@SubItem
    mov D@LV_ITEM.stateDis &LVIS_FOCUSED
    move D@LV_ITEM.pszTextDis D@pOutput;DllSrcItemBuffer
    mov D@LV_ITEM.cchTextMaxDis &MAXPATH

    If D@ScanfromLastItem = &TRUE
        call 'USER32.SendMessageA' D@Lb_Handle, &LVM_GETITEMCOUNT, 0, 0
        dec eax
    Else
        mov eax D@Item; | dec eax
    End_If
    mov D@Itens eax

    call 'USER32.SendMessageA' D@Lb_Handle, &LVM_GETITEMTEXT, eax, D@LV_ITEM
    mov edi D@pOutput | add edi eax | mov B$edi 0
    mov eax D@Itens;D@pOutput

EndP

[LV_CompareBuffer: B$ 0 #&MAXPATH]
; http://wiki.visualwebgui.com/pages/index.php/Function_to_find_the_given_string_in_the_listView_control_in_any_column
Proc LVSearchString:
    Argument @Lb_Handle, @pInput, @Item, @SubItem, @ScanfromLastItem
    Local @Itens
    Structure @LV_ITEM 40, @LV_ITEM.imaskDis 0,  @LV_ITEM.iItemDis 4,  @LV_ITEM.iSubItemDis 8,  @LV_ITEM.stateDis 12,  @LV_ITEM.stateMaskDis 16,
                           @LV_ITEM.pszTextDis 20,  @LV_ITEM.cchTextMaxDis 24,  @LV_ITEM.iImageDis 28,  @LV_ITEM.lParamDis 32,  @LV_ITEM.iIndentDis 36
    Uses esi, edi, edx, ecx


    call ClearBuffer LV_CompareBuffer, &MAXPATH
    call ClearBuffer D@LV_ITEM, 40
    mov D@LV_ITEM.imaskDis &LVIF_TEXT
    move D@LV_ITEM.iSubItemDis D@SubItem
    mov D@LV_ITEM.stateDis &LVIS_FOCUSED
    move D@LV_ITEM.pszTextDis LV_CompareBuffer
    mov D@LV_ITEM.cchTextMaxDis &MAXPATH

    ;call 'USER32.SendMessageA' D@Lb_Handle, &LVM_GETITEMCOUNT, 0, 0 | mov D@Itens eax
    If D@ScanfromLastItem = &TRUE
        call 'USER32.SendMessageA' D@Lb_Handle, &LVM_GETITEMCOUNT, 0, 0
        ;dec eax
    Else
        mov eax D@Item; | dec eax
    End_If
    mov D@Itens eax



    xor edi edi
    .Do
        call 'USER32.SendMessageA' D@Lb_Handle, &LVM_GETITEMTEXT, edi, D@LV_ITEM
        call strcmp LV_CompareBuffer, D@pInput
        If eax = 0
            ;mov eax D@Itens | sub eax edi | dec eax | ExitP
            mov eax edi | ExitP
        End_If
        inc edi
    .Loop_Until edi >= D@Itens

    mov eax 0-1

EndP

Proc LVAddSubItem:
    Argument @Lb_Handle, @pInput, @Item, @SubItem, @ScanfromLastItem
    Local @Itens
    Structure @LV_ITEM 40, @LV_ITEM.imaskDis 0,  @LV_ITEM.iItemDis 4,  @LV_ITEM.iSubItemDis 8,  @LV_ITEM.stateDis 12,  @LV_ITEM.stateMaskDis 16,
                           @LV_ITEM.pszTextDis 20,  @LV_ITEM.cchTextMaxDis 24,  @LV_ITEM.iImageDis 28,  @LV_ITEM.lParamDis 32,  @LV_ITEM.iIndentDis 36
    Uses esi, edi, edx, ecx, eax


    call ClearBuffer D@LV_ITEM, 40
    mov D@LV_ITEM.imaskDis &LVIF_TEXT
    move D@LV_ITEM.iSubItemDis D@SubItem
    mov D@LV_ITEM.stateDis &LVIS_FOCUSED
    move D@LV_ITEM.pszTextDis D@pInput;DllSrcItemBuffer
    mov D@LV_ITEM.cchTextMaxDis &MAXPATH

    If D@ScanfromLastItem = &TRUE
        call 'USER32.SendMessageA' D@Lb_Handle, &LVM_GETITEMCOUNT, 0, 0
        dec eax
    Else
        mov eax D@Item; | dec eax
    End_If
    ;mov D@Itens eax
    mov D@LV_ITEM.iItemDis eax

    call 'USER32.SendMessageA' D@Lb_Handle, &LVM_SETITEM, 0, D@LV_ITEM

EndP

Proc LVAddItem:
    Argument @Lb_Handle
    Local @Itens
    Structure @LV_ITEM 40, @LV_ITEM.imaskDis 0,  @LV_ITEM.iItemDis 4,  @LV_ITEM.iSubItemDis 8,  @LV_ITEM.stateDis 12,  @LV_ITEM.stateMaskDis 16,
                           @LV_ITEM.pszTextDis 20,  @LV_ITEM.cchTextMaxDis 24,  @LV_ITEM.iImageDis 28,  @LV_ITEM.lParamDis 32,  @LV_ITEM.iIndentDis 36
    Uses esi, edi, edx, ecx


    call ClearBuffer D@LV_ITEM, 40
    call 'USER32.SendMessageA' D@Lb_Handle, &LVM_INSERTITEM, 0, D@LV_ITEM

EndP


; LV_ITEM Structure

[Dll_XRef_IAT_LV_buffer: B$ 0 #&MAXPATH]

[Dll_XRef_Lvi:
 Dll_XRef_Lvi.imask: D$ &LVIF_TEXT
 Dll_XRef_Lvi.iItem: D$ 0
 Dll_XRef_Lvi.iSubItem: D$ 0
 Dll_XRef_Lvi.state: D$ &LVIS_FOCUSED
 Dll_XRef_Lvi.stateMask: D$ 0
 Dll_XRef_Lvi.pszText: D$ Dll_XRef_IAT_LV_buffer
 Dll_XRef_Lvi.cchTextMax: D$ &MAXPATH
 Dll_XRef_Lvi.iImage: D$ 0
 Dll_XRef_Lvi.lParam: D$ 0
 Dll_XRef_Lvi.iIndent: D$ 0]

[DllXRefSrcFunctionIndex: B$ '000001', 0]

[DllSrcName: B$ 0 #256]
[DllPosValue: B$ 0 #16]
[DllSrcApiError: B$ 0 #256]

Proc AddDllSrcListviewItem:
    Arguments @h2List, @SrcStart, @SrcEnd, @pInitialText, @Flag
    Local @ModuleNameStart, @DotStart, @LastChar, @FunctionStart, @LastCharType, @EaxBackup, @SrcPos, @FunctionEnd
    Uses esi, ecx, edx, ebx, edi


    ; 1st check the delimiters

    xor eax eax
    mov D@DotStart 0
    mov esi D@SrcStart
    mov edx D@SrcEnd
    mov al B$esi | mov D@EaxBackup eax

    inc esi | mov D@ModuleNameStart esi

    .While B$esi <> al
        If B$esi <= ' '
            ; invalid chars or a space in betwen the dll module name
            mov cl al
            xor eax eax
L2:
            While B$esi <> cl
                inc esi | On esi >= edx, ExitP
            End_While
            mov D@LastChar esi
            jmp L3>> ; invalid dll module name
        End_If
        ...If B$esi = '.'

            mov D@DotStart esi ; save the position of the dot
            inc esi | mov D@FunctionStart esi

            mov cl al
            xor eax eax
            While B$esi <> cl
                On esi <= ' ', jmp L2<<
                inc esi | On esi >= edx, ExitP
            End_While

            mov D@LastChar esi | movzx ecx B$esi | mov D@LastCharType ecx

            dec esi ; return the pointer to the byte before the "'" or '"' to we end the loop
            mov eax D@EaxBackup

        ...End_If

        inc esi
    .End_While

    ; If something went wrong while parsing the funcion name "Ex: Old rosasm disassembler outputs points to: call 'MessageBoxA' on ShowYou function

    If D@DotStart = 0
        mov D@LastChar esi
        mov edi DllSrcApiError
        mov esi D@pInitialText
        xor ecx ecx
        While B$esi <> 0D ; end on the paragrah mark
            movsb
            On esi >= D@SrcEnd, jmp L1>
            inc ecx
            On ecx >= 256, jmp L1>
        End_While
        L1:
        mov B$edi 0
        mov esi DllSrcApiError, B$ErrorLevel 4 | error D$BadLibNamePtr
        jmp L3>> ; invalid dll module name
    End_If

    ; Calculate and Display Index Value
    mov B$DLLsFoundInSource &TRUE

    call strcpy Dll_XRef_IAT_LV_buffer, DllXRefSrcFunctionIndex
    move D$Dll_XRef_Lvi.pszText Dll_XRef_IAT_LV_buffer
    mov D$Dll_XRef_Lvi.iSubItem, 0
    call 'user32.SendMessageA', D@h2list, &LVM_INSERTITEM, 0, Dll_XRef_Lvi

    ; Calculate and Display Dll Name
    mov esi D@DotStart | mov B$esi 0
    move D$Dll_XRef_Lvi.pszText D@ModuleNameStart
    inc D$Dll_XRef_Lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, Dll_XRef_Lvi
    mov esi D@DotStart | mov B$esi '.' ; restore the dot

    ; Calculate and Display Function Name
    mov esi D@LastChar | mov B$esi 0 | mov D@FunctionEnd esi
    move D$Dll_XRef_Lvi.pszText D@FunctionStart
    inc D$Dll_XRef_Lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, Dll_XRef_Lvi
    mov esi D@LastChar | mov ecx D@LastCharType | mov B$esi cl ; restore the last char type "'" or '"'

    ; Calculate and Display the text where the dll is used from
    mov esi D@pInitialText
    While B$esi <> 0D ; end on the paragrah mark
        inc esi | On esi >= D@SrcEnd, jmp L1>
    End_While
L1:
    mov D@LastChar esi | movzx ecx B$esi | mov D@LastCharType ecx
    mov B$esi 0; settle the end to zero

    move D$Dll_XRef_Lvi.pszText D@pInitialText
    inc D$Dll_XRef_Lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, Dll_XRef_Lvi
    mov esi D@LastChar | mov ecx D@LastCharType | mov B$esi cl ; restore the last char type 0D or anyother value

    ; Calculate Pos of the source
    lea ebx D@SrcPos
    mov eax D@SrcStart | sub eax D$CodeSource | mov D$ebx eax
    mov esi ebx, ecx 4
    call toUDword
    mov esi edi, edi DllPosValue
    Do | movsb | LoopUntil B$esi-1 = 0

    move D$Dll_XRef_Lvi.pszText DllPosValue
    inc D$Dll_XRef_Lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, Dll_XRef_Lvi

    ; setup the corresponding flag to detrinef the api as found from  call, jmp,  mov, lea, push instructions @Flag
    If D@Flag = DLL_SRC_TYPE_CALL
        mov D$Dll_XRef_Lvi.pszText {"call", 0}
    Else_If D@Flag = DLL_SRC_TYPE_JMP
        mov D$Dll_XRef_Lvi.pszText {"jmp", 0}
    Else_If D@Flag = DLL_SRC_TYPE_MOV
        mov D$Dll_XRef_Lvi.pszText {"mov", 0}
    Else_If D@Flag = DLL_SRC_TYPE_LEA
        mov D$Dll_XRef_Lvi.pszText {"lea", 0}
    Else_If D@Flag = DLL_SRC_TYPE_PUSH
        mov D$Dll_XRef_Lvi.pszText {"push", 0}
    Else
        mov D$Dll_XRef_Lvi.pszText {"unknown instruction", 0}
    End_If
    inc D$Dll_XRef_Lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, Dll_XRef_Lvi


    ; write the full undecorated name here. WriteImportObjStrings
    mov esi D@FunctionEnd | mov cl B$esi | mov B$esi 0
    call StrCpy DllSrcApiName, D@FunctionStart
    mov esi D@FunctionEnd | mov B$esi cl
    call DllSrcUndecorateApiName DllSrcApiName, DllSrcApiUndecoratedName
    If eax = &TRUE
        move D$Dll_XRef_Lvi.pszText DllSrcApiUndecoratedName
    Else
        move D$Dll_XRef_Lvi.pszText {"Not decorated", 0}
    End_If


    inc D$Dll_XRef_Lvi.iSubItem
    call 'user32.SendMessageA', D@h2list, &LVM_SETITEM, 0, Dll_XRef_Lvi


    mov ecx &MAXPATH
    mov edi 0
    L0:
        mov B$Dll_XRef_IAT_LV_buffer+edi 0
        inc edi
    Loop L0<


    ; increment DllIndex
    lea ebx D$DllXRefSrcFunctionIndex+5 | inc B$ebx
    While B$ebx > '9'
        mov B$ebx '0' | dec ebx | inc B$ebx
    End_While


L3:

    mov esi D@LastChar
    mov eax esi
    inc eax ; Bypass the '"' or "'" found

EndP
__________________________________________________________________

[DllSrcApiName: B$ 0 #4096]
[DllSrcApiUndecoratedName: B$ 0 #4000]

Proc DllSrcUndecorateApiName:
    Arguments @ApiName, @Output
    Uses ecx, edx

    call 'IMAGEHLP.UnDecorateSymbolName' D@ApiName, D@Output, 4000, &UNDNAME_COMPLETE

    call StrCmp D@ApiName, D@Output

    ; If the strings are the same it means that the above function failed, because the undecoretated name is not the same
    ; as the original loaded name.
    If eax = 0
        call Simple_UndecorateSymbolName D@ApiName, D@Output
    End_If

    ; compare the results
    call StrCmp D@ApiName, D@Output
    If eax = 0
        mov eax &FALSE
    Else
        mov eax &TRUE
    End_If
;;
    push esi
        mov esi Trash | While B$esi <> 0 | movsb | End_While
    pop esi
;;
EndP

__________________________________________________________________

__________________________________________________________________

Proc BypassSingleLineComment:
    Arguments @SrcStart, @SrcEnd
    Uses esi, edx, edi

    mov esi D@SrcStart
    mov edx D@SrcEnd

    xor eax eax

L1:
    If_Or B$esi = 0D, B$esi = 0A, esi >= edx;, B$esi = 0 ; Or 0D, 0A or simply 0 (that is the end of the file)
        jmp L2>
    End_If
        inc esi
    jmp L1<

L2:
    mov eax esi

EndP
__________________________________________________________________

; see SetColorsMap CheckPairings
Proc BypassBracket:
    Arguments @StartPos, @SrcStart, @SrcEnd
    Uses esi, edx, edi

    mov esi D@SrcStart
    mov edx D@SrcEnd

    xor eax eax

    .While B$esi <> ']'
        ...If B$esi = "'"

            Do
                inc esi
                On esi >= edx, ExitP
            Loop_Until B$esi = "'"
            inc esi ; bypass last "'" found

        ...Else_If B$esi = '"'

            Do
                inc esi
                On esi >= edx, ExitP
            Loop_Until B$esi = '"'
            inc esi ; bypass last '"' found

        ...Else_If W$esi = ';;'

            ..If_Or esi = D@StartPos, W$esi-2 = CRLF
                add esi 2 ; bypas 1st 2 ';;'
                On esi >= edx, ExitP
                If B$esi = 0D
                    call BypassMultiLineComment esi, edx
                Else ; another error, such as ;;RsrcRVA = BaseOfRsrc-StartOfRsrc
                    While B$esi = ';'
                        inc esi
                        On esi >= edx, ExitP
                    End_While
                    call BypassSingleLineComment esi, edx
                End_If
            ..Else
                ; error. The double comment is treated as a single comment. Ex: a space before or a triple ";;;" etc
                While B$esi = ';'
                    inc esi
                    On esi >= edx, ExitP
                End_While
                call BypassSingleLineComment esi, edx
            ..End_If

            mov esi eax

        ...Else_If B$esi = ';'

            call BypassSingleLineComment esi, edx | on eax = 0, ExitP
            mov esi eax

        ...Else

            inc esi

        ...End_If
        On esi >= edx, ExitP
    .End_While

    inc esi ; Bypass the ']' found
    mov eax esi

EndP

__________________________________________________________________


Proc BypassMultiLineComment:
    Arguments @SrcStart, @SrcEnd
    Uses esi, edx, edi

    mov esi D@SrcStart
    mov edx D@SrcEnd

    xor eax eax
    While W$esi <> ';;'
        inc esi
        On esi >= edx, ExitP
        If_And W$esi = ';;', B$esi+2 <> 0D;, B$esi-1 <> 0A ; error case such as an ';;falta fazer' in the middle og a double comment
            inc esi
        End_If
    End_While

    add esi 2 ; Bypass the ';;' found
    mov eax esi

EndP




__________________________________________________________________
__________________________________________________________________
__________________________________________________________________
; this is for test only

[DumpedIATSrcFile: D$ 0]
[DumpedIATSrcFileSize: D$ 0]

Proc DumpedIAtFileTest:
    Arguments @Adressee

    call InitImportsProcList D$hDLLIATList, D$hDLLXRefIATList
    call CImage_SelectPictureLoad D@Adressee, 0
    call 'RosMem.VMemFree' D$DumpedIATSrcFile
    mov D$DumpedIATSrcFile 0
    mov D$DumpedIATSrcFileSize 0

EndP

[OPENFILENAMEA.lStructSizeDis 0
 OPENFILENAMEA.hwndOwnerDis 4
 OPENFILENAMEA.hInstanceDis 8
 OPENFILENAMEA.lpstrFilterDis 12
 OPENFILENAMEA.lpstrCustomFilterDis 16
 OPENFILENAMEA.nMaxCustFilterDis 20
 OPENFILENAMEA.nFilterIndexDis 24
 OPENFILENAMEA.lpstrFileDis 28
 OPENFILENAMEA.nMaxFileDis 32
 OPENFILENAMEA.lpstrFileTitleDis 36
 OPENFILENAMEA.nMaxFileTitleDis 40
 OPENFILENAMEA.lpstrInitialDirDis 44
 OPENFILENAMEA.lpstrTitleDis 48
 OPENFILENAMEA.FlagsDis 52
 OPENFILENAMEA.nFileOffsetDis 56
 OPENFILENAMEA.nFileExtensionDis 58
 OPENFILENAMEA.lpstrDefExtDis 60
 OPENFILENAMEA.lCustDataDis 64
 OPENFILENAMEA.lpfnHookDis 68
 OPENFILENAMEA.lpTemplateNameDis 72]

[Size_Of_OPENFILENAMEA 76]

[Sz_OpenedFile: B$ 0 #&MAX_PATH]

[DumpReportFilter:  B$ "Supported Image Files", 0 "*.asm;*.txt;*.jpg;*.jpe;*.jpeg;*.png;*.tiff;*.ico;*.wmf;*.emf;*.exif", 0
                    B$ "Assembly (.ASM)", 0 "*.asm", 0
                    B$ "Text (.TXT)", 0 "*.txt", 0
                    B$ "Gif (.GIF)", 0 "*.gif", 0
                    B$ "Png (.PNG)", 0 "*.png", 0
                    B$ "Tiff (.TIFF)", 0 "*.tiff", 0
                    B$ "Ico (.ICO)", 0 "*.ico", 0
                    B$ "Wmf (.WMF)", 0 "*.wmf", 0
                    B$ "Emf (.EMF)", 0 "*.emf", 0
                    B$ "Exif (.EXIF)", 0 "*.exif", 0
                    B$ 'All Files', 0  '*.*', 0 0]

Proc CImage_SelectPictureLoad:
    Arguments @hWnd, @pCImgStruct
    Structure @OPENFILENAMEA 76, @OPENFILENAMEA.lStructSizeDis 0, @OPENFILENAMEA.hwndOwnerDis 4, @OPENFILENAMEA.hInstanceDis 8,
                                 @OPENFILENAMEA.lpstrFilterDis 12, @OPENFILENAMEA.lpstrCustomFilterDis 16, @OPENFILENAMEA.nMaxCustFilterDis 20,
                                 @OPENFILENAMEA.nFilterIndexDis 24, @OPENFILENAMEA.lpstrFileDis 28, @OPENFILENAMEA.nMaxFileDis 32,
                                 @OPENFILENAMEA.lpstrFileTitleDis 36, @OPENFILENAMEA.nMaxFileTitleDis 40, @OPENFILENAMEA.lpstrInitialDirDis 44,
                                 @OPENFILENAMEA.lpstrTitleDis 48, @OPENFILENAMEA.FlagsDis 52, @OPENFILENAMEA.nFileOffsetDis 56,
                                 @OPENFILENAMEA.nFileExtensionDis 58, @OPENFILENAMEA.lpstrDefExtDis 60, @OPENFILENAMEA.lCustDataDis 64,
                                 @OPENFILENAMEA.lpfnHookDis 68, @OPENFILENAMEA.lpTemplateNameDis 72
    Uses ebx, esi, edi

    call 'RosMem.FastZeroMem' D@OPENFILENAMEA, Size_Of_OPENFILENAMEA
    call 'RosMem.FastZeroMem' Sz_OpenedFile, &MAX_PATH

    mov D@OPENFILENAMEA.lStructSizeDis Size_Of_OPENFILENAMEA
    move D@OPENFILENAMEA.hwndOwnerDis D@hWnd
    call 'kernel32.GetModuleHandleA' &NULL | mov D@OPENFILENAMEA.hInstanceDis eax
    mov D@OPENFILENAMEA.nFilterIndexDis 1
    move D@OPENFILENAMEA.lpstrFilterDis DumpReportFilter
    mov D@OPENFILENAMEA.lpstrFileDis Sz_OpenedFile
    mov D@OPENFILENAMEA.nMaxFileDis &MAX_PATH
    mov D@OPENFILENAMEA.FlagsDis &OFN_EXPLORER__&OFN_HIDEREADONLY__&OFN_ENABLESIZING__&OFN_FILEMUSTEXIST__&OFN_LONGNAMES__&OFN_PATHMUSTEXIST
    mov D@OPENFILENAMEA.lCustDataDis 0
    call 'Comdlg32.GetSaveFileNameA' D@OPENFILENAMEA
    If eax = &TRUE
       call SaveLibFile, D$DumpedIATSrcFile, D$DumpedIATSrcFileSize, Sz_OpenedFile
    Else
        xor eax eax
    End_If

EndP


__________________________________________________________________
__________________________________________________________________
__________________________________________________________________




InitExportsProcList:
    mov esi D$CodeSource, edx D$SourceEnd, B$DLLsFoundInSource &FALSE

    .While esi < edx
        mov eax D$esi
        ..If al = '"'
            inc esi
            While B$esi <> '"'
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If al = "'"
            inc esi
            While B$esi <> "'"
                inc esi | On esi >= edx, jmp L9>>
            End_While

        ..Else_If eax = MLC
            add esi 4
            While D$esi <> MLC
                inc esi | On esi = edx, jmp L9>>
            End_While
            add esi 4

        ..Else_If al = ';'
                While B$esi >= ' ' | inc esi | End_While

        ..Else_If ax = '::'
                        mov B$DLLsFoundInSource &TRUE
                        push eax, edx, D$esi, esi
                            mov B$esi 0
                            While B$esi-1 > ' ' | dec esi | End_While

                            call 'USER32.SendMessageA' D$DLLsProcListHandle,
                                                       &LB_ADDSTRING, 0, esi
                        pop esi, D$esi, edx, eax
        ..End_If

        inc esi

    .End_While
L9: ret














































































