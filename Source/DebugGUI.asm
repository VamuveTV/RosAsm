TITLE DebugGUI

;;
_____________________________________________________________________________________________

                                  Debugger GUI - Ludwig Haehne
 
 Split from the debugger title for better readability.
   
 TODO:

    * Save upper & lower tab selection
    * Memory inspector address edit should handle expressions (mouse hint code reuse)    
    * Hints for toolbar buttons    
    * Context menu in address space tree (?)
    * Next instruction indicator in source editor    
    * Separate the debug dialog from the main window (but stay on top)
    * Log Exceptions (also auto-forwarded)
    * Save to file for log (from context menu)
    * Watch tab (expression-entry; add,remove&clear button; list/tree view)
    * Disassembly tab
    * Restore defaults (set flag to delete debug.cfg at the end)
    * Recover toolbar when all bands are hidden
    * Nice FPU box
    * Nice CPU Info
    * Review code address form
    * Q$ / X$ / U$ (+string) support in mousehint
    * Sane menu/combo font (registry?) (test under wine)    
    * Address Space Tree vs. Nessie
____________________________________________________________________________________________

 Paragraphs
  
  * 'CreateDebugWindow' 'DebuggerMENU' 'DebuggerTOOLBAR' 'DebuggerMOUSEHINT' 
    'DebuggerSTRINGS'
  
 Dialogs
 
  * 'DebugDlgProc', 'DataViewProc', 'MemoryInspectorProc', 'PageTableProc', 'CallStackProc',
    'MouseOverDataHint', 'ExceptionInfoProc', 'AddressSpaceFormProc', 'LogFormProc'
 
 Window handling
 
  * 'AdjustControlSize', 'AdjustControlPos', 'SelectTab', 'ReportWinError'
  
 Misc routines
 
  * 'SourceDebugPos', 'NextInstructionDecode', 'IsMultiStepInstruction', 'TestCPUFeatures'    
   
 Format Conversion Procs
 
  * 'toHex', ...

;;
____________________________________________________________________________________________

; GENERAL WINDOW HANDLING PROCS
____________________________________________________________________________________________

[WINDOW_RESIZE  &SWP_NOMOVE+&SWP_NOZORDER+&SWP_NOACTIVATE]
[WINDOW_MOVE    &SWP_NOSIZE+&SWP_NOZORDER+&SWP_NOACTIVATE]

Proc AdjustControlSize:
    Arguments @Handle, @ID, @dX, @dY
    Structure @RECT 16, @left 0, @top 4, @right 8, @bottom 12
    Uses ebx, esi, edi

        mov edi D@RECT
        call 'USER32.GetDlgItem' D@Handle D@ID | mov ebx eax
        ; Compute current width (eax) & height (esi) and add delta's
        call 'USER32.GetWindowRect' ebx edi
        mov eax D@right
        sub eax D@left
        add eax D@dX
        mov esi D@bottom
        sub esi D@top
        add esi D@dY
        call 'USER32.SetWindowPos' ebx, 0, 0, 0, eax, esi, WINDOW_RESIZE
        ; Return height error for listboxes.
        call 'USER32.GetWindowRect' ebx edi
        mov eax D@bottom
        sub eax D@top
        sub eax esi
EndP
____________________________________________________________________________________________

Proc AdjustControlPos:
    Arguments @Handle, @ID, @dX, @dY
    Structure @RECT 16, @left 0, @top 4, @right 8, @bottom 12
    Uses ebx, edi

        mov edi D@RECT
        call 'USER32.GetDlgItem' D@Handle D@ID | mov ebx eax
        ; Compute current xpos (eax) & ypos (ecx) and add delta's
        call 'USER32.GetWindowRect' ebx, edi
        call 'USER32.ScreenToClient' D@Handle, edi
        mov eax D@left
        add eax D@dX
        mov ecx D@top
        add ecx D@dY
        call 'USER32.SetWindowPos' ebx, 0, eax, ecx, 0, 0, WINDOW_MOVE
EndP
____________________________________________________________________________________________

Proc ReportWinError:
    Arguments @Caption
    Local @String

        call 'Kernel32.GetLastError'
        mov edx (&SUBLANG_DEFAULT shl 16 or &LANG_NEUTRAL)
        lea ecx D@String
        call 'Kernel32.FormatMessageA' &FORMAT_MESSAGE_ALLOCATE_BUFFER+&FORMAT_MESSAGE_FROM_SYSTEM,
            &NULL, eax, edx, ecx, 256, &NULL
        call 'User32.MessageBoxA' D$hwnd, D@String, D@Caption, &MB_ICONERROR
        call 'Kernel32.LocalFree' D@String
EndP
____________________________________________________________________________________________

Proc SelectTab:
    Arguments @DlgHandle, @TabCtrlID, @Index
    Structure @NotifyMsg 12, @Handle 0, @ID 4, @Code 8
    Uses ebx

        call 'User32.GetDlgItem' D@DlgHandle, D@TabCtrlID
        mov ebx eax

        call 'User32.SendMessageA' ebx, &TCM_SETCURSEL, D@Index, 0
        mov D@Handle ebx
        move D@ID D@TabCtrlID
        mov D@Code &TCN_SELCHANGE
        call 'User32.SendMessageA' D@DlgHandle, &WM_NOTIFY, D@TabCtrlID, D@NotifyMsg ; for Win9x!
        call 'User32.PostMessageA' D@DlgHandle, &WM_NOTIFY, D@TabCtrlID, D@NotifyMsg
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; ADDRESS SPACE TREE
____________________________________________________________________________________________
____________________________________________________________________________________________


[AddressSpaceFormSize:
 AddressSpaceForm.Width: W$ ?
 AddressSpaceForm.Height: W$ ?]

[ASForm_handle: ?]
[ASForm.Tree_handle: ?]
[ADDRESS_SPACE_TREE 2]

Proc CreateNewForm_AddressSpaceForm:
    Arguments @Parent

    call 'User32.RegisterClassExA' ASFormClass
    call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        ASFormClassName,
        {"Main Window",0},
        &WS_CHILD,
        0,0,100,200,
        D@Parent,
        0,
        D$hInstance,
        0
    mov D$ASForm_handle eax

    call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"SysTreeView32",0},
        {"New Control",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&TVS_FULLROWSELECT+&TVS_HASBUTTONS+&TVS_HASLINES+&TVS_LINESATROOT+&TVS_TRACKSELECT,
        0,0,100,200,
        D$ASForm_handle,
        ADDRESS_SPACE_TREE,
        D$hInstance,
        0

    mov D$ASForm.Tree_handle eax
    call AddressSpaceForm_OverrideTreeProc

EndP
____________________________________________________________________________________________


[ASFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ AddressSpaceFormProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ ASFormClassName
 @hIconSm:       D$ 0]
[ASFormClassName: B$ "AddressSpaceForm" 0]
____________________________________________________________________________________________

; Process WM_CREATE message.

Proc AddressSpaceDialog_OnCreate:
    Arguments @Handle

        move D$ASForm_handle D@Handle

        call 'USER32.GetClientRect' D@Handle, DebugRect
        move W$AddressSpaceForm.Width W$DebugRect@width
        move W$AddressSpaceForm.Height W$DebugRect@height

        call AddressSpace_CreatePopupMenu

        mov eax &TRUE
EndP
____________________________________________________________________________________________

; Process WM_SIZE message.

Proc AddressSpaceDialog_OnSize:
    Arguments @Handle, @WidthHeight

        movzx eax W$AddressSpaceForm.Width
        movzx esi W@WidthHeight
        sub esi eax
        movzx eax W$AddressSpaceForm.Height
        movzx edi W@WidthHeight+2
        sub edi eax
        call AdjustControlSize D@Handle, ADDRESS_SPACE_TREE, esi, edi
        call 'USER32.InvalidateRect' D@Handle, &NULL, &TRUE

        move D$AddressSpaceFormSize D@WidthHeight
        mov eax 0
EndP

Proc AddressSpaceFormProc:
    Arguments @handle @Message @wParam @lParam
    ;Uses esi edi ebx

    pushad

    .If D@Message = &WM_CREATE
        call AddressSpaceDialog_OnCreate D@Handle
        ;move D$ASForm_handle D@handle
        ;call AddressSpace_CreatePopupMenu
        mov D$AddressSpace.TVProc 0
        ;mov eax 0

    .Else_if D@Message = &WM_SHOWWINDOW
        If D$AddressSpace.TVProc = 0
            call AddressSpaceForm_OverrideTreeProc
            ;call AddressSpaceTree_Build
        EndIf
        If D@wParam = &TRUE
            call AddressSpaceTree_Build ; AddressSpaceForm_Refresh
        EndIf
    .Else_If D@Message = &WM_CLOSE
        call 'USER32.DestroyWindow' D@handle

    .Else_if D@Message = &WM_SIZE
        call AddressSpaceDialog_OnSize D@Handle, D@lParam
        ;movzx ecx W@lParam
        ;movzx edx W@lParam+2

        ;call 'USER32.SetWindowPos' D$ASForm.TreeHandle, 0, 0, 0, ecx, edx, WINDOW_RESIZE
    .Else_if D@Message = &WM_COMMAND
        call AddressSpaceForm_OnCommand D@handle, D@wParam, D@lParam

    .Else_if D@Message = &WM_NOTIFY
        mov eax D@lParam, edx D$ASForm.TreeHandle
        If D$eax+NM_TREEVIEW.hdr.hwndFromDis = edx
            call AddressSpaceForm_OnTreeNavigate eax
            ;SendMessage D$ASForm.Tree_handle, &TVM_GETVISIBLECOUNT, 0, 0
        EndIf

        ;mov eax 0

    .Else_if D@Message = WM_REFRESH_CONTENT
        call AddressSpaceTree_Build

    .Else_if D@Message = &WM_SETFONT
        SendMessage D$ASForm.TreeHandle, &WM_SETFONT, D@wParam, D@lParam
    .Else_if D@Message = &WM_DRAWITEM
        If D@wParam = 0
            call DebugDialog_OnDrawMenuItem D@lParam
        EndIf
        popad | mov eax 1 | ExitP

    .Else_if D@Message = &WM_MEASUREITEM
        mov eax D@lParam
        If D@wParam = 0
          ; menu
            call DebugDialog_OnMeasureMenuItem D@lParam
        EndIf
        popad
        mov eax 1
        ExitP
    ;.Else_if D@Message = &WM_COMMAND
     ;   call AddressSpaceForm_OnCommand D@handle, D@wParam, D@lParam


;    .Else_if D@Message = WM_REFRESH_CONTENT
 ;       call AddressSpaceTree_Build

;    .Else_if D@Message = &WM_COMMAND
 ;       call AddressSpaceForm_OnCommand D@handle, D@wParam, D@lParam


    .Else
        popad
        call 'User32.DefWindowProcA' D@handle D@Message D@wParam D@lParam
        ExitP
    .End_If

    popad
    mov eax &FALSE
EndP
____________________________________________________________________________________________

; Process WM_COMMAND message.

Proc AddressSpaceForm_OnCommand:
    Arguments @Handle, @wParam, @lParam


        movzx ecx W@wParam
        movzx eax W@wParam+2
        mov ebx D$AddressSpace.PopupMenu

        .If ecx = MAS_SHOW_MEM
            ;mov eax D$DataPointer
            ;and eax 0_FFFF_F000
            ;SendMessage D$MemoryInspectorHandle, WM_SET_PAGE, eax, D$DataPointer
            SendMessage D$MemoryInspectorHandle, WM_SET_PAGE, D$ASForm.CurrentAddress, 0
        .Else_if ecx = MAS_DUMP_PARTIAL_DATA
            ;VirtualAlloc CurrentPage 0101000;4096

            call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$ASForm.CurrentAddress, D$CurrentPage, 01000, &NULL
            ;call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$ASForm.CurrentAddress, D$CurrentPage, 0101000, &NULL
            ;call DumpMemory D@Handle, D$CurrentPage, 0101000;01000
            call DumpMemory D@Handle, D$CurrentPage, 01000

        .ElseIf ecx = MCS_SHOW_ALL
            mov D$CallStackFilter ecx
            call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            call CallStackForm_Refresh

        .ElseIf ecx = MCS_HIDE_EXTERNAL
            mov D$CallStackFilter ecx
            call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            call CallStackForm_Refresh

        .ElseIf ecx = MCS_HIDE_INTRA
            mov D$CallStackFilter ecx
            call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            call CallStackForm_Refresh

        .ElseIf ecx = MCS_SHOWLOCALS
            xor B$CallStackShowLocal 1
            mov eax D$CallStackShowLocal | shl eax 3
            call 'USER32.CheckMenuItem' ebx, MCS_SHOWLOCALS, eax
            call CallStackForm_Refresh

        .EndIf
        mov eax 0
EndP


[ASForm.CurrentItem: ? ASForm.CurrentAddress: ?]
;;
[NMHDR.hwndFromDis 0
 NMHDR.idFromDis 4
 NMHDR.codeDis 8]
;;

[NM_TREEVIEW.hdr.hwndFromDis 0
 NM_TREEVIEW.hdr.idFromDis 4
 NM_TREEVIEW.hdr.codeDis 8
 NM_TREEVIEW.actionDis 12
 NM_TREEVIEW.itemOld.imaskDis 16
 NM_TREEVIEW.itemOld.hItemDis 20
 NM_TREEVIEW.itemOld.stateDis 24
 NM_TREEVIEW.itemOld.stateMaskDis 28
 NM_TREEVIEW.itemOld.pszTextDis 32
 NM_TREEVIEW.itemOld.cchTextMaxDis 36
 NM_TREEVIEW.itemOld.iImageDis 40
 NM_TREEVIEW.itemOld.iSelectedImageDis 44
 NM_TREEVIEW.itemOld.cChildrenDis 48
 NM_TREEVIEW.itemOld.lParamDis 52
 NM_TREEVIEW.itemNew.imaskDis 56
 NM_TREEVIEW.itemNew.hItemDis 60
 NM_TREEVIEW.itemNew.stateDis 64
 NM_TREEVIEW.itemNew.stateMaskDis 68
 NM_TREEVIEW.itemNew.pszTextDis 72
 NM_TREEVIEW.itemNew.cchTextMaxDis 76
 NM_TREEVIEW.itemNew.iImageDis 80
 NM_TREEVIEW.itemNew.iSelectedImageDis 84
 NM_TREEVIEW.itemNew.cChildrenDis 88
 NM_TREEVIEW.itemNew.lParamDis 92
 NM_TREEVIEW.ptDrag.xDis 96
 NM_TREEVIEW.ptDrag.yDis 100]

[TV_DISPINFO.hdr.hwndFromDis 0
 TV_DISPINFO.hdr.idFromDis 4
 TV_DISPINFO.hdr.codeDis 8
 TV_DISPINFO.item.imaskDis 12
 TV_DISPINFO.item.hItemDis 16
 TV_DISPINFO.item.stateDis 20
 TV_DISPINFO.item.stateMaskDis 24
 TV_DISPINFO.item.pszTextDis 28
 TV_DISPINFO.item.cchTextMaxDis 32
 TV_DISPINFO.item.iImageDis 36
 TV_DISPINFO.item.iSelectedImageDis 40
 TV_DISPINFO.item.cChildrenDis 44
 TV_DISPINFO.item.lParamDis 48]

Proc AddressSpaceForm_OnTreeNavigate:
    Arguments @NotifyInfo
    Uses esi ebx
; TVN_GETDISPINFO
    mov esi D@NotifyInfo
    ;On D$esi+NMHDR.codeDis <> &TVN_SELCHANGED, ExitP
    .If D$esi+NM_TREEVIEW.hdr.codeDis = &TVN_SELCHANGED
        move D$ASForm.CurrentItem D$esi+NM_TREEVIEW.itemNew.hItemDis
        move D$ASForm.CurrentAddress D$esi+NM_TREEVIEW.itemNew.lParamDis
        ;mov ecx D$esi+NM_TREEVIEW.itemOld.pszTextDis;NM_TREEVIEW.itemNew.pszTextDis
        ;add esi 12 ; skip notify header
        ;add esi 4 ; skip action flag
        ;add esi 40 ; skip old item
        ;mov eax D$esi+4 ; hItem
        ;mov D$ASForm.CurrentItem eax
        ;mov ebx D$esi+36 ; lParam of new item
        ;mov D$ASForm.CurrentAddress ebx
    ;.Else D$esi+NM_TREEVIEW.hdr.codeDis = &TVN_SELCHANGING ;TVN_GETDISPINFO
     ;   mov eax D$esi+TV_DISPINFO.item.pszTextDis
    .End_If
EndP

____________________________________________________________________________________________

; To allow rightclick inside the treeview we override the window proc.

AddressSpaceForm_OverrideTreeProc:
    mov D$AddressSpace.TVProc 0
    call 'User32.SetWindowLongA' D$ASForm.Tree_handle, &GWL_WNDPROC, AddressSpace_HandleMouseProc
    mov D$AddressSpace.TVProc eax
ret

[AddressSpace.TVProc: ?]

Proc AddressSpace_HandleMouseProc:
    Arguments @Handle, @Message, @wParam, @lParam
    Uses ebx esi edi

    .If D@Message = &WM_LBUTTONDBLCLK
      ; Simulate left click (select item) before showing the popup menu
        SendMessage D@Handle, &WM_LBUTTONDOWN, D@wParam, D@lParam
        SendMessage D@Handle, &WM_LBUTTONUP, D@wParam, D@lParam
      ; Is this a page item?
        cmp D$ASForm.CurrentAddress 0 | je L0>>
      ; Check if the user clicked onto the item
        move D$DebugRect D$ASForm.CurrentItem
        SendMessage D@Handle, &TVM_GETITEMRECT, 1, DebugRect
        movzx eax W@lParam
        cmp eax D$DebugRect@left  | jl L0>>
        cmp eax D$DebugRect@right | ja L0>>
        movzx eax W@lParam+2
        cmp eax D$DebugRect@top    | jl L0>>
        cmp eax D$DebugRect@bottom | ja L0>>
      ; Show the popup menu at the position of the mouse-click
        SendMessage D$MemoryInspectorHandle, WM_SET_PAGE, D$ASForm.CurrentAddress, 0
    .Else_If D@Message = &WM_RBUTTONDOWN
      ; Simulate right click (select item) before showing the popup menu
        SendMessage D@Handle, &WM_LBUTTONDOWN, D@wParam, D@lParam
        SendMessage D@Handle, &WM_LBUTTONUP, D@wParam, D@lParam

        ;If D$ASForm.CurrentAddress = 0
            ;mov eax eax
            ;ASForm.CurrentItem
         ;   SendMessage D@Handle, &TVM_GETITEM, 0, TVI
        ;End_If

      ; Is this a page item?
        cmp D$ASForm.CurrentAddress 0 | je L0>>
      ; Check if the user clicked onto the item
        move D$DebugRect D$ASForm.CurrentItem
        SendMessage D@Handle, &TVM_GETITEMRECT, 1, DebugRect
        movzx eax W@lParam
        cmp eax D$DebugRect@left  | jl L0>>
        cmp eax D$DebugRect@right | ja L0>>
        movzx eax W@lParam+2
        cmp eax D$DebugRect@top    | jl L0>
        cmp eax D$DebugRect@bottom | ja L0>
      ; Show the popup menu at the position of the mouse-click
        movzx eax W@lParam
        movzx ecx W@lParam+2
        mov D$PointX eax, D$PointY ecx
        call 'USER32.ClientToScreen' D@Handle, Point
        call 'USER32.TrackPopupMenu' D$AddressSpace.PopupMenu, &TPM_LEFTALIGN, D$PointX, D$PointY, 0, D$ASForm_handle, 0 ; << TODO

;        SendMessage D$ASForm_handle, &TVM_GETVISIBLECOUNT, 0, 0
        ;SendMessage D$ASForm.CurrentItem, &TVM_GETVISIBLECOUNT, 0, 0

    ;.Else_if D@Message = &WM_COMMAND
     ;   call AddressSpaceForm_OnCommand D@handle, D@wParam, D@lParam
        ;SendMessage D@Handle, &TVM_GETCOUNT, 0, 0
    .EndIf
L0: call 'User32.CallWindowProcA' D$AddressSpace.TVProc, D@Handle, D@Message, D@wParam, D@lParam
EndP
____________________________________________________________________________________________

[AddressSpace.PopupMenu: ?]

[MAS_SHOW_MEM       3300
 MAS_DUMP_PARTIAL_DATA      3301]

;[CALLSTACK_SHOWALL 30 CALLSTACK_HIDE_EXTERNAL 31 CALLSTACK_HIDE_INTRA 32
; CALLSTACK_SHOWLOCALS 35]

AddressSpace_CreatePopupMenu:
    call 'USER32.CreatePopupMenu' | mov ebx eax, D$AddressSpace.PopupMenu eax
    call AppendMenu ebx, &MF_OWNERDRAW, MAS_SHOW_MEM, 0 ; append show memory
    call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    call AppendMenu ebx, &MF_OWNERDRAW, MAS_DUMP_PARTIAL_DATA, D$StrSaveDumpedPtr;StrShowInvokePtr
    ;call AppendMenu ebx, &MF_OWNERDRAW, MCS_SHOW_DECL, D$StrShowDeclPtr
    ;call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    ;call AppendMenu ebx, &MF_OWNERDRAW, MCS_SHOW_ALL, D$StrShowAllCallsPtr
    ;call AppendMenu ebx, &MF_OWNERDRAW, MCS_HIDE_EXTERNAL, D$StrHideModCallsPtr
    ;call AppendMenu ebx, &MF_OWNERDRAW, MCS_HIDE_INTRA, D$StrHideIMCallsPtr
    ;call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    ;call AppendMenu ebx, &MF_OWNERDRAW+&MF_CHECKED, MCS_SHOWLOCALS, D$StrShowLocalsPtr

    ;call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA, MCS_SHOWALL, &MF_BYCOMMAND
ret
____________________________________________________________________________________________


[PageDesc: B$ ? #64]

Proc AddressSpaceTree_WritePageDescription:
    Arguments @Address, @Protect
    Uses esi, edi

      ; Write address
        mov eax D@Address, edi PageDesc, ecx 4
        DwordToHex eax

      ; Write protection "XRWC GN"
        mov al ' ' | stosb
        mov ecx D@Protect

        mov D$edi '----', D$edi+4 ' -- '
        If ecx = &PAGE_READONLY
            mov B$edi+1 'R'
        Else_if ecx = &PAGE_READWRITE
            mov W$edi+1 'RW'
        Else_if ecx = &PAGE_EXECUTE
            mov B$edi 'X'
        Else_if ecx = &PAGE_EXECUTE_READ
            mov W$edi 'XR'
        Else_if ecx = &PAGE_EXECUTE_READWRITE
            mov D$edi 'XRW-'
        Else_if ecx = &PAGE_EXECUTE_WRITECOPY
            mov D$edi 'XRWC'
        Else_if ecx = &PAGE_WRITECOPY
            mov D$edi '-RWC'
        EndIf
        test ecx &PAGE_GUARD | jz L0>
        mov B$edi+5 'G'
L0:     test ecx &PAGE_NOCACHE | jz L0>
        mov B$edi+6 'N'

L0:     add edi 8
        mov D$edi 0
EndP
____________________________________________________________________________________________

Proc AddressSpaceTree_AddPages:
    Arguments @Base, @Size, @Protect, @Type
    Uses esi, edi

      ; Add item for each commited page in region
        mov esi D@Base, edi esi
        add edi D@Size

        While esi < edi
            call AddressSpaceTree_WritePageDescription esi, D@Protect
            mov D$TVI.Item.lParam esi
            mov D$TVI.Item.pszText PageDesc
            ;SendMessage D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
            call 'User32.SendMessageA' D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
            add esi 01000
        EndWhile
EndP
____________________________________________________________________________________________

Proc AddressSpaceTree_AddModule:
    Arguments @ModEntry
    Local @LowerBound, @UpperBound, @hModItem, @ExportAddr
    Uses esi, ebx

        mov esi D@ModEntry

        mov D$TVI.hParent &TVI_ROOT
        mov eax D$esi+ME_Name
        mov D$TVI.Item.pszText eax
        move D$TVI.Item.lParam 0

        call 'User32.SendMessageA' D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
        mov D@hModItem eax
        mov eax D$esi+ME_Base
        mov D@LowerBound eax
        add eax D$esi+ME_Size
        mov D@UpperBound eax

        mov eax D$esi+ME_Base | add eax D$esi+ME_ExportBase | mov D@ExportAddr eax
        mov ebx D@LowerBound
        .While ebx < D@UpperBound
            call VirtualQuery ebx
            .If eax = &TRUE
                If ebx = D$esi+ME_Base ; cases of no export function. Baseofcode is zero, there fore, it is the same address of baseaddress
                                       ; See ScanPEHeader
                    move D$TVI.hParent D@hModItem
                Else_If ebx = D$esi+ME_CodeBase
                    mov D$TVI.Item.pszText {'Code Section' 0}
                    mov D$TVI.Item.lParam 0
                    call 'User32.SendMessageA' D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
                    mov D$TVI.hParent eax
                ;Else;_If ebx = D@ExportAddr
                 ;   mov D$TVI.Item.pszText {'Data Section' 0}
                  ;  mov D$TVI.Item.lParam 0
                   ; call 'User32.SendMessageA' D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
                    ;move D$TVI.hParent D@hModItem
                Else
                    move D$TVI.hParent D@hModItem
                EndIf
                call AddressSpaceTree_AddPages ebx, D$MemoryInformation@RegionSize, D$MemoryInformation@Protect, D$MemoryInformation@Type
            .ElseIf eax = 0-1
                ExitP
            .EndIf
            add ebx D$MemoryInformation@RegionSize
        .EndWhile

;;
        mov D$TVI.Item.pszText {'Code Section' 0}
        move D$TVI.Item.lParam D$esi+ME_CodeBase
        

        mov D$TVI.Item.pszText {'Export Section' 0}
        mov eax D$esi+ME_ExportBase
        add eax D$esi+ME_Base
        mov D$TVI.Item.lParam eax
        call 'User32.SendMessageA' D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
;;

      ; The AS scanner should continue behind the module mapping area
        move D$MemoryInformation@RegionSize D$esi+ME_Size
EndP
____________________________________________________________________________________________

Proc AddressSpaceTree_AddGeneralRegion:
    Arguments @Base, @Size, @Protect, @Type

    [@LastAllocBase: ?]

      ; Check if the region belongs to the last added allocation block, otherwise
      ; add a new top-level item.
        mov eax D$MemoryInformation@AllocationBase
        If D@LastAllocBase <> eax
            mov D@LastAllocBase eax
            call IntToHexString
            mov D$TVI.hParent &TVI_ROOT
            mov D$TVI.Item.pszText HexString
            mov D$TVI.Item.lParam 0
            call 'User32.SendMessageA' D$ASForm.Tree_handle, &TVM_INSERTITEM, 0, TVI
            mov D$TVI.hParent eax
        EndIf

      ; Add item for each commited page in region
        call AddressSpaceTree_AddPages D@Base, D@Size, D@Protect, D@Type
EndP
____________________________________________________________________________________________

; Add a memory region returned by VirtualQuery

Proc AddressSpaceTree_AddRegion:
    Arguments @Base, @Size, @Protect, @Type

        mov eax D$MemoryInformation@BaseAddress
        mov D$TVI.Item.lParam eax

      ; Check if this region is a mapped module
        ;call GetModuleName eax, D$ModuleList, D$NumModules
        call GetModuleNameFromAddress eax, D$ModuleList, D$NumModules
        .If eax <> 0
            call GetModuleFromAddress D$TVI.Item.lParam, D$ModuleList, D$NumModules
            call AddressSpaceTree_AddModule eax
            ;call AddressSpaceTree_AddModule edx
        .Else
            call AddressSpaceTree_AddGeneralRegion D@Base, D@Size, D@Protect, D@Type
        .EndIf
EndP
____________________________________________________________________________________________

Proc AddressSpaceTree_Build:

    call 'User32.SendMessageA' D$ASForm.TreeHandle, &TVM_DELETEITEM, 0, &TVI_ROOT
    call 'User32.SendMessageA' D$ASForm.Tree_handle, &WM_SETREDRAW, 0, 0

    mov D$TVI.hParent &TVI_ROOT
    mov D$TVI.hInsertAfter &TVI_LAST
    mov D$TVI.Item.imask &TVIF_TEXT+&TVIF_PARAM

    mov esi D$AddressLowerBound
    While esi < D$AddressUpperBound
        call VirtualQuery esi
        If eax = 1
            call AddressSpaceTree_AddRegion esi, ecx, D$MemoryInformation@Protect, D$MemoryInformation@Type
        ElseIf eax = 0-1
            ExitP
        EndIf
        add esi D$MemoryInformation@RegionSize
    EndWhile

    call 'User32.SendMessageA' D$ASForm.Tree_handle, &WM_SETREDRAW, 1, 0
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; EVENT LOG
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CreateNewForm_LogForm:
    Arguments @Parent

    call 'User32.RegisterClassExA' LogFormClass
    call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        LogFormClassName,
        {"Main Window",0},
        &WS_CHILD+&WS_VISIBLE,
        0,0,100,200,
        D@Parent,
        0,
        D$hInstance,
        0
    mov D$LogForm_handle eax
    [LogForm_handle: ?]

    call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"LISTBOX",0},
        {"New Control",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_OVERLAPPED+&WS_VISIBLE+&WS_VSCROLL+&WS_HSCROLL+&LBS_NOINTEGRALHEIGHT,
        0,0,100,200,
        D$LogForm_handle,
        2,
        D$hInstance,
        0
    mov D$LogForm.List_handle eax
    [LogForm.List_handle: ?]

     ; this shows the horizontal scrollbar. The value must be bigger then the widthof the listbox. the style WS_HSCROLL must be set
    call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_SETHORIZONTALEXTENT, 2000, 0

EndP
____________________________________________________________________________________________
[LogFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ LogFormProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ LogFormClassName
 @hIconSm:       D$ 0]
[LogFormClassName: B$ "LogForm" 0]
____________________________________________________________________________________________

Proc LogFormProc:
    Arguments @handle, @Message, @wParam, @lParam
    Structure @DgbLogStrDis (2*MAX_DBG_LOGSTR), @DgbLogStrDataDis 0

    ;Uses esi edi ebx

    pushad

    .If D@Message = &WM_CREATE
        ;mov eax 0
        ;call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_SETHORIZONTALEXTENT, 2000, 0

    .Else_If D@Message = &WM_CLOSE
        call 'USER32.DestroyWindow' D@handle

    .Else_if D@Message = &WM_SIZE
        movzx ecx W@lParam
        movzx edx W@lParam+2

        call 'USER32.SetWindowPos' D$LogForm.ListHandle, 0, 0, 0, ecx, edx, WINDOW_RESIZE

    .Else_If D@Message = WM_LOG

        mov esi D@wParam
        If_And esi <> 0, B$esi <> 0
            call 'RosMem.FastZeroMem' D@DgbLogStrDis, (2*MAX_DBG_LOGSTR)
            mov edi D@DgbLogStrDis
            call CurrentTimeStampToString {"HH:mm:ss ", 0}, edi, &NULL, &NULL
            add edi 9
            mov esi D@wParam
            C_call FormatStr edi, {'%s' 0}, esi
            add edi eax
            mov B$edi 0
            call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_ADDSTRING, 0, D@DgbLogStrDis
            call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_SETCURSEL, eax, 0
        End_If
;;
        mov esi D@wParam
        If_And esi <> 0, B$esi <> 0
            mov eax eax
        End_If
        
        mov edi LogString
        call CurrentTimeStampToString {"HH:mm:ss", 0}, edi, &NULL, &NULL
        add edi 8
        ;mov edi LogString
        ;call TimeToStr
        mov al ' ' | stosb

        mov esi D@wParam, ecx D@lParam
        On ecx > 255, mov ecx 255
        If ecx = 0
            While B$esi+ecx <> 0 | inc ecx | EndWhile
        EndIf
        While ecx > 0
            lodsb
            On al <> 0A, stosb
            dec ecx
        EndWhile
        ;rep movsb
        mov B$edi 0
        ;call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_GETCOUNT, 0, 0
        ;mov edx eax
        call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_ADDSTRING, 0, LogString
        call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_SETCURSEL, eax, 0

        ;call 'USER32.SendMessageA' D$LogForm.ListHandle,  &LB_GETCURSEL, 0, 0; | mov D$ClipIndex eax
        ;call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_SETCURSEL, eax, 0
        ;If eax = &LB_ERR
         ;   call 'USER32.SendMessageA' D$TemplateList2, &LB_SETCURSEL, 0-1, 0
        ;End_If

        ;SendMessage D$LogForm.ListHandle, &LB_ADDSTRING, 0, LogString
        ;SendMessage D$LogForm.ListHandle, &LB_SETCURSEL, eax, 0
;;
    .Else_If D@Message = &WM_DESTROY

    .Else_if D@Message = &WM_SETFONT
        call 'User32.SendMessageA' D$LogForm.ListHandle, &WM_SETFONT, D@wParam, D@lParam

    .Else
        popad
        call 'User32.DefWindowProcA' D@handle D@Message D@wParam D@lParam
        ExitP
    .End_If

    popad
    mov eax &FALSE
EndP

[LogString: B$ ? #256]
____________________________________________________________________________________________
____________________________________________________________________________________________

; ADDRESS INPUT
____________________________________________________________________________________________
____________________________________________________________________________________________

; Tag Wizard Form "J:\Projects\RosAsm\WizardFiles\WZRDForm0000.wwf"
CreateNewForm_CodeAddressForm:
    call 'ComCtl32.InitCommonControlsEx' CodeAddressFormClassName@Init_All_Common_Controls
    call 'User32.RegisterClassExA' CodeAddressFormClass
    call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        CodeAddressFormClassName,
        {"Show code at address ...",0},
        &WS_BORDER+&WS_CAPTION+&WS_DLGFRAME+&WS_OVERLAPPED+&WS_POPUP+&WS_VISIBLE+&NULL,
        82,35,282,100,
        0,
        0,
        D$hInstance,
        0
    mov D$CodeAddressForm_handle eax
    [CodeAddressForm_handle: ?]
    call 'GDI32.CreateFontIndirectA' CodeAddressForm_LOGFONTSTRUCT | mov D$CodeAddressFormFont_handle eax
    call 'User32.SendMessageA' D$CodeAddressForm_handle  &WM_SETFONT eax &TRUE
    [CodeAddressFormFont_handle: ?]
    [CodeAddressForm_LOGFONTSTRUCT:  0  0  0  0  0  0  536870912 'MS Sans Serif' 0 0 0 0 0 0 ]
    CodeAddressForm:

    call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"BUTTON",0},
        {"Show statement",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_OVERLAPPED+&WS_VISIBLE+&BS_PUSHBUTTON+&BS_TEXT,
        159,22,101,28,
        D$CodeAddressForm_handle,
        2,
        D$hInstance,
        0
    mov D$CodeAddressForm.GotoButton_handle eax
    [CodeAddressForm.GotoButton_handle: ?]
    call 'GDI32.CreateFontIndirectA' CodeAddressForm.GotoButton_LOGFONTSTRUCT | mov D$CodeAddressForm.GotoButtonFont_handle eax
    call 'User32.SendMessageA' D$CodeAddressForm.GotoButton_handle  &WM_SETFONT eax &TRUE
    [CodeAddressForm.GotoButtonFont_handle: ?]
    [CodeAddressForm.GotoButton_LOGFONTSTRUCT:  -11  0  0  0  400  0  570491393 'MS Sans Serif' 0 0 0 0 0 0 ]

    call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"EDIT",0},
        {"",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_OVERLAPPED+&WS_VISIBLE+&ES_LEFT+&ES_RIGHT+&ES_UPPERCASE,
        22,25,125,21,
        D$CodeAddressForm_handle,
        3,
        D$hInstance,
        0
    mov D$CodeAddressForm.AddressEdit_handle eax
    [CodeAddressForm.AddressEdit_handle: ?]
    call 'GDI32.CreateFontIndirectA' CodeAddressForm.AddressEdit_LOGFONTSTRUCT | mov D$CodeAddressForm.AddressEditFont_handle eax
    call 'User32.SendMessageA' D$CodeAddressForm.AddressEdit_handle  &WM_SETFONT eax &TRUE
    [CodeAddressForm.AddressEditFont_handle: ?]
    [CodeAddressForm.AddressEdit_LOGFONTSTRUCT:  -13  0  0  0  400  0  822149635 'Courier New' 0 0 0 0 0 0 ]

ret
____________________________________________________________________________________________
[CodeAddressFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ CodeAddressFormProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ CodeAddressFormClassName
 @hIconSm:       D$ 0]
[CodeAddressFormClassName: B$ "CodeAddressForm" 0]
[@Init_All_Common_Controls:
 @Init_All_Common_Controls_dwSize: D$ len
 @Init_All_Common_Controls_dwICC:  D$ &ICC_COOL_CLASSES+&ICC_DATE_CLASSES+&ICC_INTERNET_CLASSES+&ICC_NATIVEFNTCTL_CLASS,
             +&ICC_PAGESCROLLER_CLASS+&ICC_USEREX_CLASSES+&ICC_WIN95_CLASSES]
____________________________________________________________________________________________

Proc CodeAddressFormProc:
    Arguments @handle @Message @wParam @lParam

    pushad

    ...If D@Message = &WM_CREATE

    ...Else_If D@Message = &WM_CLOSE
        call 'GDI32.DeleteObject' D$CodeAddressFormFont_handle
        call 'GDI32.DeleteObject' D$CodeAddressForm.GotoButtonFont_handle
        call 'GDI32.DeleteObject' D$CodeAddressForm.AddressEditFont_handle
        call 'USER32.DestroyWindow' D@handle

    ...Else_if D@Message = &WM_COMMAND
        mov eax D@lParam
        If eax = D$CodeAddressForm.GotoButton_handle
            call CodeAddressForm_Goto
        EndIf

    ...Else
        popad
        call 'User32.DefWindowProcA' D@handle D@Message D@wParam D@lParam
        ExitP

    ...End_If

    popad
    mov eax &FALSE

EndP
; Tag End
;;
    ...Else_if D@Message = &WM_COMMAND
        mov eax D@lParam
        If eax = D$CodeAddressForm.GotoButton_handle
            call CodeAddressForm_Goto
        EndIf
;;


CodeAddressForm_Goto:
    call 'User32.GetWindowTextA' D$CodeAddressForm.AddressEdit_handle, HexString, 10
    mov esi HexString
    call HexStringToInt
    mov ebx eax

    call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, ebx
    If eax = &TRUE
        call SourceDebugPos ebx
    Else
        call 'User32.MessageBoxA' D$CodeAddressForm_Handle, {'This is not a valid code address!' 0},
            {'Invalid address' 0}, &MB_ICONWARNING
    EndIf
    call 'User32.SendMessageA' D$CodeAddressForm_Handle, &WM_CLOSE, 0, 0
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

; CALL STACK
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc CreateNewForm_CallStackForm:
    Arguments @Parent

    call 'User32.RegisterClassExA' CSFormClass
    call 'User32.CreateWindowExA',
        &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        CSFormClassName,
        {"Callstack",0},
        &WS_CHILD,
        0,0,100,200,
        D@Parent,
        0,
        D$hInstance,
        0

    mov D$CallStackFormHandle eax

    call 'User32.CreateWindowExA',
        &WS_EX_CLIENTEDGE+&WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
        {"SysTreeView32",0},
        {"New Control",0},
        &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&TVS_FULLROWSELECT+&TVS_HASBUTTONS+&TVS_HASLINES+&TVS_LINESATROOT+&TVS_TRACKSELECT,
        0,0,100,200,
        D$CallStackFormHandle,
        CALLSTACK_TREE,
        D$hInstance,
        0

    mov D$CallStackTreeHandle eax

    call CallStack_OverrideTreeProc
EndP

[CSFormClass:
 @cbSize:        D$ len
 @style:         D$ 0
 @lpfnWndProc:   D$ CallStackProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ CSFormClassName
 @hIconSm:       D$ 0]
[CSFormClassName: B$ "CallStackForm" 0]
____________________________________________________________________________________________

; Process WM_CREATE message.

Proc CallStackDialog_OnCreate:
    Arguments @Handle

        move D$CallStackFormHandle D@Handle

        call 'USER32.GetClientRect' D@Handle, DebugRect
        move W$CallStackForm.Width W$DebugRect@width
        move W$CallStackForm.Height W$DebugRect@height

        call CallStack_CreatePopupMenu

        mov eax &TRUE
EndP
____________________________________________________________________________________________

; Process WM_SIZE message.

Proc CallStackDialog_OnSize:
    Arguments @Handle, @WidthHeight

        movzx eax W$CallStackForm.Width
        movzx esi W@WidthHeight
        sub esi eax
        movzx eax W$CallStackForm.Height
        movzx edi W@WidthHeight+2
        sub edi eax
        call AdjustControlSize D@Handle, CALLSTACK_TREE, esi, edi
        ;call AdjustControlPos D@Handle, CALLSTACK_SHOWDECLARATION, 0, edi
        ;call AdjustControlPos D@Handle, CALLSTACK_SHOWEVOKE, 0, edi
        call 'USER32.InvalidateRect' D@Handle, &NULL, &TRUE

        move D$CallStackFormSize D@WidthHeight
        mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_COMMAND message.

Proc CallStackDialog_OnCommand:
    Arguments @Handle, @wParam, @lParam

        movzx ecx W@wParam
        movzx eax W@wParam+2
        mov ebx D$CallStack.PopupMenu

        .If ecx = MCS_SHOW_INVOKE
            call CallStackForm_ShowEvoke

        .Else_if ecx = MCS_SHOW_DECL
            call CallStackForm_ShowDeclaration

        .ElseIf ecx = MCS_SHOW_ALL
            mov D$CallStackFilter ecx
            call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            call CallStackForm_Refresh

        .ElseIf ecx = MCS_HIDE_EXTERNAL
            mov D$CallStackFilter ecx
            call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            call CallStackForm_Refresh

        .ElseIf ecx = MCS_HIDE_INTRA
            mov D$CallStackFilter ecx
            call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
               ecx, &MF_BYCOMMAND
            call CallStackForm_Refresh

        .ElseIf ecx = MCS_SHOWLOCALS
            xor B$CallStackShowLocal 1
            mov eax D$CallStackShowLocal | shl eax 3
            call 'USER32.CheckMenuItem' ebx, MCS_SHOWLOCALS, eax
            call CallStackForm_Refresh

        .EndIf
        mov eax 0
EndP
____________________________________________________________________________________________

; Process CDDS_ITEMPREPAINT notification. ebx > NMTVCUSTOMDRAW

CallStackDialog_OnPaintItem:
    mov eax D$ebx+44 ; item param
    If eax <> 0

        mov al B$eax+CSE_Flags
        test al CSEF_EXTERNAL | jz L0>

        mov D$ebx+48 0_66_66_66 ; set text color to gray
    EndIf
L0: mov eax &CDRF_NEWFONT
ret
____________________________________________________________________________________________

; Process WM_NOTIFY message.

Proc CallStackDialog_OnNotify:
    Arguments @NotifyMsg

        mov ebx D@NotifyMsg

        .If D$ebx+8 = &NM_CUSTOMDRAW

            mov eax D$ebx+12 ; draw stage
            If eax = &CDDS_PREPAINT
                mov eax &CDRF_NOTIFYITEMDRAW
            Else eax = &CDDS_ITEMPREPAINT
                call CallStackDialog_OnPaintItem
            Else
                mov eax 0
            EndIf

        .ElseIf D$ebx+8 = &TVN_SELCHANGED
            call CallStackForm_OnTreeNavigate ebx
            mov eax 0

        .Else
            mov eax 0
        .EndIf
EndP
____________________________________________________________________________________________

; Tag Dialog 1015

[CallStackFormSize:
 CallStackForm.Width: W$ ?
 CallStackForm.Height: W$ ?]

[CallStackFormHandle: ? CallStackTreeHandle: ?
 CallStackShowDeclarationHandle: ? CallStackShowEvokeHandle: ?]

[CALLSTACK_TREE 20]

Proc CallStackProc:
    Arguments @handle @Message @wParam @lParam
    ;Uses ebx, esi, edi

    pushad

    .If D@Message = &WM_CREATE
        call CallStackDialog_OnCreate D@Handle
       ; mov eax 0

    .Else_if D@Message = &WM_SHOWWINDOW
        If D@wParam = &TRUE
            call CallStackForm_Refresh
        EndIf

    .Else_if D@Message = &WM_CLOSE
        call 'USER32.DestroyWindow' D@Handle

    .Else_If D@Message = &WM_DESTROY
        call DestroyCallStack
        mov D$CallStackFormHandle 0

    .Else_If D@Message = &WM_SIZE
        call CallStackDialog_OnSize D@Handle, D@lParam

    .Else_if D@Message = &WM_COMMAND
        call CallStackDialog_OnCommand D@Handle, D@wParam, D@lParam

    .Else_if D@Message = &WM_NOTIFY
        call CallStackDialog_OnNotify D@lParam

    .Else_if D@Message = WM_REFRESH_CONTENT
        call CallStackForm_Refresh

    .Else_if D@Message = &WM_SETFONT
        call 'User32.SendMessageA' D$CallStackTreeHandle, &WM_SETFONT, D@wParam, D@lParam

    .Else_if D@Message = &WM_DRAWITEM
        If D@wParam = 0
            call DebugDialog_OnDrawMenuItem D@lParam
        EndIf
        popad
        mov eax 1
        ExitP

    .Else_if D@Message = &WM_MEASUREITEM
        mov eax D@lParam
        If D@wParam = 0
          ; menu
            call DebugDialog_OnMeasureMenuItem D@lParam
        EndIf
        popad
        mov eax 1
        ExitP

    .Else
        popad
        call 'User32.DefWindowProcA' D@handle D@Message D@wParam D@lParam
        ExitP
    .EndIf

    popad
    mov eax &FALSE

EndP
____________________________________________________________________________________________

; To allow rightclick inside the treeview we override the window proc.

CallStack_OverrideTreeProc:
    mov D$CallStack.TVProc 0
    call 'User32.SetWindowLongA' D$CallStackTreeHandle, &GWL_WNDPROC, CallStack_InterceptRightClick
    mov D$CallStack.TVProc eax
ret

[CallStack.TVProc: ?]

Proc CallStack_InterceptRightClick:
    Arguments @Handle, @Message, @wParam, @lParam
    Uses ebx esi edi

    .If D@Message = &WM_RBUTTONDOWN
      ; Simulate left click (select item) before showing the popup menu
        call 'User32.SendMessageA' D@Handle, &WM_LBUTTONDOWN, D@wParam, D@lParam
        call 'User32.SendMessageA' D@Handle, &WM_LBUTTONUP, D@wParam, D@lParam
      ; Is this a proc name item? (don't show menu for sub items)
        cmp D$CallStackForm.CurrentEntry 0 | jz L0>>
      ; Check if the user right-clicked onto the item
        move D$DebugRect D$CallStackForm.CurrentItem
        call 'User32.SendMessageA' D@Handle, &TVM_GETITEMRECT, 1, DebugRect
        movzx eax W@lParam
        cmp eax D$DebugRect@left  | jl L0>
        cmp eax D$DebugRect@right | ja L0>
        movzx eax W@lParam+2
        cmp eax D$DebugRect@top    | jl L0>
        cmp eax D$DebugRect@bottom | ja L0>
      ; Show the popup menu at the position of the mouse-click
        movzx eax W@lParam
        movzx ecx W@lParam+2
        mov D$PointX eax, D$PointY ecx
        call 'USER32.ClientToScreen' D@Handle, Point
        call 'USER32.TrackPopupMenu' D$CallStack.PopupMenu, &TPM_LEFTALIGN, D$PointX, D$PointY, 0, D$CallStackFormHandle, 0 ; << TODO
    .ElseIf D@Message = &WM_LBUTTONDBLCLK
        call CallStackForm_ShowDeclaration ; *TODO* strange effect in source editor
    .EndIf
L0: call 'User32.CallWindowProcA' D$CallStack.TVProc, D@Handle, D@Message, D@wParam, D@lParam
EndP
____________________________________________________________________________________________

[CallStack.PopupMenu: ?]

[MCS_SHOW_INVOKE    3200
 MCS_SHOW_DECL      3201
 MCS_SHOW_ALL       3202
 MCS_HIDE_EXTERNAL  3203
 MCS_HIDE_INTRA     3204
 MCS_SHOW_LOCALS    3205]

;[CALLSTACK_SHOWALL 30 CALLSTACK_HIDE_EXTERNAL 31 CALLSTACK_HIDE_INTRA 32
; CALLSTACK_SHOWLOCALS 35]

CallStack_CreatePopupMenu:
    call 'USER32.CreatePopupMenu' | mov ebx eax, D$CallStack.PopupMenu eax
    call AppendMenu ebx, &MF_OWNERDRAW, MCS_SHOW_INVOKE, D$StrShowInvokePtr
    call AppendMenu ebx, &MF_OWNERDRAW, MCS_SHOW_DECL, D$StrShowDeclPtr
    call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    call AppendMenu ebx, &MF_OWNERDRAW, MCS_SHOW_ALL, D$StrShowAllCallsPtr
    call AppendMenu ebx, &MF_OWNERDRAW, MCS_HIDE_EXTERNAL, D$StrHideModCallsPtr
    call AppendMenu ebx, &MF_OWNERDRAW, MCS_HIDE_INTRA, D$StrHideIMCallsPtr
    call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    call AppendMenu ebx, &MF_OWNERDRAW+&MF_CHECKED, MCS_SHOWLOCALS, D$StrShowLocalsPtr

    call 'USER32.CheckMenuRadioItem' ebx, MCS_SHOWALL, MCS_HIDE_INTRA,
        MCS_SHOWALL, &MF_BYCOMMAND
ret

Proc CallStack_UpdatePopupMenu:
    ; TODO
EndP
____________________________________________________________________________________________

[TVI:
 TVI.hParent: ?
 TVI.hInsertAfter: ?
 TVI.Item:
 TVI.Item.imask: ?
 TVI.Item.hItem: ?
 TVI.Item.state: ?
 TVI.Item.stateMask: ?
 TVI.Item.pszText: ?
 TVI.Item.cchTextMax: ?
 TVI.Item.iImage: ?
 TVI.Item.iSelectedImage: ?
 TVI.Item.cChildren: ?
 TVI.Item.lParam: ?]
____________________________________________________________________________________________

; ebx = dword parameter / local number (starts with zero)
; edi > Param string (must have room for 100 bytes)

SearchLocalName:
  ; The first local is the parent frame pointer at offset 0.
    lea eax D$ebx*4
    If eax <> 0
        neg eax | jmp SearchStackFrameSymbol
    Else
        mov eax ' [Ca' | stosd
        mov eax 'ller' | stosd
        mov eax ' EBP' | stosd
        mov eax ']'    | stosd
    EndIf
ret

SearchParameterName:
  ; The first param is at EBP+8
    lea eax D$ebx*4+8

SearchStackFrameSymbol:
    mov edx StackFrameSymbols, ecx D$NumStackFrameSymbols
    jecxz L9>

  ; Search correspondance table - cmp the offset
L0: cmp D$edx+4 eax | je L1>
        add edx 8
    loop L0<
    jmp L9>

  ; Copy the name to the buffer at EDI > " [name]"
L1: mov edx D$edx
    mov ecx 97
    mov al ' ' | stosb
    mov al '['
    Do
        stosb
        mov al B$edx
        inc edx
        dec ecx | jz L8>
    Loop_Until al < LowSigns
L8: mov al ']' | stosb
L9: ret
____________________________________________________________________________________________

; edi > CSE (CallStack Entry)

Proc CallStackForm_InsertParameters:

  ; Parameter string reserved on stack
    sub esp 128

    mov D$TVI.Item.pszText {'Parameter' 0}, D$TVI.Item.lParam 0
    call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
    mov D$TVI.hParent eax

    mov ebx 0
    mov esi D$edi+CSE_Address

  ; esi > first parameter
    While ebx < D$edi+CSE_NumParams
        lodsd
        push edi
            lea edi D$esp+4
            DwordToHex eax
            call SearchParameterName
            mov al 0 | stosb
        pop edi
        mov D$TVI.Item.pszText esp
        call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
        inc ebx
    EndWhile

    call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_EXPAND, &TVE_EXPAND, D$TVI.hParent
EndP
____________________________________________________________________________________________

; edi > CSE (CallStack Entry)

Proc CallStackForm_InsertLocals:

  ; Local string reserved on stack
    sub esp 128

    mov D$TVI.Item.pszText {'Local data' 0}, D$TVI.Item.lParam 0
    call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
    mov D$TVI.hParent eax

    mov ebx 0
    mov esi D$edi+CSE_Address | sub esi 8

  ; esi > caller ebp
    While ebx < D$edi+CSE_NumLocals
        mov eax D$esi | sub esi 4
        push edi
            lea edi D$esp+4
            DwordToHex eax
            call SearchLocalName
            mov al 0 | stosb
        pop edi
        mov D$TVI.Item.pszText esp
        call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
        inc ebx
    EndWhile

    call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_EXPAND, &TVE_EXPAND, D$TVI.hParent
EndP
____________________________________________________________________________________________

; Check equate contents at ESI for stack frame symbols (relative to EBP). ECX is length.
; Return the signed offset in EAX if a string of the form "EBP+xx" / "EBP-xx" was found,
; zero otherwise. All registers are preserved (messy).

; Note: Only simple expressions with one offset and one op (+/-) are supported (not "EBP+8-4")

ParseFrameOffset:
    mov eax 0
    cmp ecx 5 | jb L9>

    lodsd | sub ecx 4
    cmp eax 'EBP'+0A00_0000 | je L0>
    cmp eax 'EBP'+0900_0000 | je L0>

L9: ret

L0: shr eax 24 ; move operator (+/-) in AL

    push edi ebx edx
      ; Save the byte behind the string in AH and replace with 0
        mov ah B$esi+ecx, B$esi+ecx 0
        push eax
            push esi ecx
                call TranslateAny
            pop ecx esi
        pop edx
      ; DL = operator, DH = saved byte
        mov B$esi+ecx dh
        On dl = subSign, neg eax
    pop edx ebx edi
ret
____________________________________________________________________________________________

; Scan the equates table for symbols local to the current callstack entry at EDI and
; fill the table 'StackFrameSymbols'.

[StackFrameSymbols: D$ ? #128 ; correspondance table [ Symbol name | signed offset ] (dwords)
 NumStackFrameSymbols: ?]     ; number of table entries
[MAX_STACK_FRAME_SYMBOLS 64]

Proc FindStackFrameSymbols:
    Local @PlainLabel, @LabelLen
    Uses edi

    mov D$NumStackFrameSymbols 0

    mov al B$edi+CSE_FLAGS

    test al CSEF_HAS_STACKFRAME | jz  P9>>
    ;test al CSEF_FUZZY_ADDRESS  | jnz P9>>
    test al CSEF_EXTERNAL       | jnz P9>>

  ; Get symbol name / len
    mov edi D$edi+CSE_ProcName, D@PlainLabel edi
    mov ecx 0-1, al 0
    repne scasb
    mov eax 0-2 | sub eax ecx
    mov D@LabelLen eax

  ; search the label list for symbols "THISLABEL@..."
    mov edi D$EquateList, edx D$EquateListLimit
    add edi 5
    While edi < edx
        mov esi D@PlainLabel, ecx D@LabelLen
        repe cmpsb | jne L0>

            cmp B$edi '@' | jne L0>
            inc edi

          ; Save address of local name without preceding '@'
            mov ebx D$NumStackFrameSymbols
            mov D$StackFrameSymbols+ebx*8 edi

          ; Scan to the end of the label name
            mov al EOI
            mov ecx edx | sub ecx edi
            repne scasb

          ; Parse equate contents
            mov esi D$edi, ecx D$edi+4
            call ParseFrameOffset
            cmp eax 0 | je L1>

          ; Save entry in correspondance table
            mov D$StackFrameSymbols+ebx*8+4 eax
            inc D$NumStackFrameSymbols

            On D$NumStackFrameSymbols = MAX_STACK_FRAME_SYMBOLS, ExitP

            jmp L1>

L0:     mov al EOI
        mov ecx edx | sub ecx edi
        repne scasb
L1:     add edi 10
    EndWhile
EndP
____________________________________________________________________________________________

Proc CallStackForm_BuildTree:
    Local @hProcItem
    Uses edi, ebx, esi

    call GenerateCallStack

    call 'User32.SendMessageA' D$CallStackTreeHandle, &WM_SETREDRAW, &FALSE, 0
    call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_DELETEITEM, 0, &TVI_ROOT

    mov D$TVI.hParent &TVI_ROOT
    mov D$TVI.hInsertAfter &TVI_LAST
    mov D$TVI.Item.imask &TVIF_TEXT+&TVIF_PARAM

    mov edi D$FirstCallStackEntry
    .While edi <> 0

        If D$CallStackFilter = MCS_HIDE_EXTERNAL
            test B$edi+CSE_Flags CSEF_EXTERNAL | jnz L0>>
        ElseIf D$CallStackFilter = MCS_HIDE_INTRA
          ; filter module internal calls, this is when the current CSE was called
          ; by a function in the same (external) module
            test B$edi+CSE_Flags CSEF_EXTERNAL | jz L1>
            mov eax D$edi+CSE_Next
            test B$eax+CSE_Flags CSEF_EXTERNAL | jz L1>
            call IsModuleCode D$eax+CSE_ProcAddress
            mov ebx eax
            call IsModuleCode D$edi+CSE_ProcAddress
            cmp eax ebx | je L0>>
        EndIf

L1:     mov D$TVI.hParent &TVI_ROOT
        mov D$TVI.Item.lParam edi
        mov eax D$edi+CSE_ProcName
        mov D$TVI.Item.pszText eax
        call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_INSERTITEM, 0, TVI
        mov D$TVI.hParent eax, D@hProcItem eax

        call FindStackFrameSymbols
        On D$edi+CSE_NumParams > 0,
            call CallStackForm_InsertParameters

        move D$TVI.hParent D@hProcItem

        If D$CallStackShowLocal = 1
            On D$edi+CSE_NumLocals > 0,
                call CallStackForm_InsertLocals
        EndIf

        test B$edi+CSE_Flags CSEF_EXTERNAL | jnz L0>
            call 'User32.SendMessageA' D$CallStackTreeHandle, &TVM_EXPAND, &TVE_EXPAND, D@hProcItem
L0:     mov edi D$edi+CSE_Next
    .EndWhile

    call 'User32.SendMessageA' D$CallStackTreeHandle, &WM_SETREDRAW, &TRUE, 0
    call 'USER32.InvalidateRect' D$CallStackTreeHandle, &NULL, &TRUE

EndP

; guga. Rewrite the functon and see why sometimes the memmory pointed at D$ebx+CSE_ProcAddress is being deleted
; It was causing RosAsm to crash. I hacked it with the "lea eax D$ebx+CSE_ProcAddress | sub eax ebx"
Proc CallStackForm_OnTreeNavigate:
    Arguments @NotifyInfo
    Local @IsMemory
    Uses esi, ebx

    mov esi D@NotifyInfo; | mov ebx esi
    On D$esi+NM_TREEVIEW.hdr.codeDis <> &TVN_SELCHANGED, ExitP
;;
    add esi 12 ; skip notify header
    add esi 4 ; skip action flag
    add esi 40 ; skip old item
    mov eax D$esi+4 ; hItem ; NM_TREEVIEW.itemNew.hItemDis
;;
    mov eax D$ebx+NM_TREEVIEW.itemNew.hItemDis
    mov D$CallStackForm.CurrentItem eax
    ;mov ebx D$esi+36 ; lParam of new item
    mov ebx D$ebx+NM_TREEVIEW.itemNew.lParamDis
    mov D$CallStackForm.CurrentEntry ebx

    mov eax 0
    .If ebx <> 0 ; see DebugDialog_UpdateRegisterButtons
         ; just to avoid that damn crash
         lea eax D$ebx+CSE_ProcAddress | sub eax ebx
        ;call IsProcessMemory D$CallStackForm.CurrentEntry;D$ebx+CSE_ProcAddress
        If eax > SizeOf_CSE;<> 0
;            call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D$ebx+CSE_ProcAddress
 ;           mov D@IsMemory &TRUE ; just to avoid that damn crash
             mov D@IsMemory &FALSE
        Else ; just to avoid that damn crash
            ;mov D@IsMemory &FALSE
            call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D$ebx+CSE_ProcAddress
            mov D@IsMemory &TRUE ; just to avoid that damn crash

        End_If
    .End_If

    ;On ebx <> 0, mov eax D$ebx+CSE_ProcAddress ; here it is crshing because we can´ tell if it is memory or not
    ;call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, eax

    xor eax 1
    call 'USER32.EnableMenuItem' D$CallStack.PopupMenu, MCS_SHOW_DECL, eax
    .If ebx <> 0
        If D@IsMemory = &TRUE ; just to avoid that damn crash
            mov ebx D$ebx+CSE_Address
            On ebx <> 0, mov ebx D$ebx-4
        End_If
    .End_If
    call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, ebx
    xor eax 1
    call 'USER32.EnableMenuItem' D$CallStack.PopupMenu, MCS_SHOW_INVOKE, eax

EndP

[CallStackShowLocal: 1
 CallStackFilter: MCS_SHOWALL]

Proc CallStackForm_Refresh:

    call CallStackForm_BuildTree
EndP

[CallStackForm.CurrentEntry: ? CallStackForm.CurrentItem: ?]

Proc CallStackForm_ShowEvoke:

    mov eax D$CallStackForm.CurrentEntry
    On eax = 0, ExitP
    mov edx D$eax
    mov ecx D$edx-4 ; get return address from the stack-copy
    dec ecx
    call SourceDebugPos ecx
EndP

Proc CallStackForm_ShowDeclaration:
    Uses ebx

    mov eax D$CallStackForm.CurrentEntry
    On eax = 0, ExitP

    mov ebx eax
    call EnableSourceDebugPos D$ebx+4, D$ModuleList, D$NumModules
    If eax = &TRUE
         ; address found on Debugger_OnException EncounterException EncounterWatchPoint EncounterBreakpoint
         ; When we reach here D$SourcePosCodeAddress is the same as D$C.regEip
        call SourceDebugPos D$ebx+4
    End_If

    ;call SourceDebugPos D$eax+4

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; DEBUGGER MAIN DIALOG (Registers & Execution Control)
____________________________________________________________________________________________
____________________________________________________________________________________________


[SendMessage | #=4 | call 'User32.SendMessageA' #1 #2 #3 #4]
[CopyString | #=1 | mov esi #1 | R9: | cmp B$esi 0 | je R9> | movsb | jmp R9< | R9:]
____________________________________________________________________________________________

[GPRRegMap: C.regEax C.regEbx C.regEcx C.regEdx C.regEsi C.regEdi C.regEbp C.regEsp]
[SegRegMap: C.regCs C.regDs C.regEs C.regFs C.regGs C.regSs]
[DbgRegMap: C.regEip C.iDr0 C.iDr1 C.iDr2 C.iDr3 C.iDr6 C.iDr7]

; Format / Conversion tables
[GPRFormats: D$ 9 FmtHexPtr FmtUDecPtr FmtSDecPtr FmtBinaryPtr FmtPUBPtr FmtPSBPtr FmtPUWPtr FmtPSWPtr FmtAsciiPtr]
;[GPRConvert: toHex toUDword toSDword toBinary toUByte toSByte toUWord toSWord toAscii]
[GPRConvert: toHex toUDword toSDword toBinarySSE toUByte toSByte toUWord toSWord toAscii]
;[FPUFormats: D$ 3 FmtFloatPtr FmtHexPtr FmtBinaryPtr]
[FPUFormats: D$ 3 FmtFloatPtr FmtHexPtr FmtBinaryFPUPtr]
;[FPUConvert: toExtended toHex toBinary]
[FPUConvert: toExtended toHex toBinarySSE]
;[MMXFormats: D$ 9 FmtHexPtr FmtBinaryPtr FmtPUBPtr FmtPSBPtr FmtPUWPtr FmtPSWPtr FmtPUDPtr FmtPSDPtr FmtPFPtr]
[MMXFormats: D$ 9 FmtHexPtr FmtBinaryMMXPtr FmtPUBPtr FmtPSBPtr FmtPUWPtr FmtPSWPtr FmtPUDPtr FmtPSDPtr FmtPFPtr]
;[MMXConvert: toHex toBinary toUByte toSByte toUWord toSWord toUDword toSDword toFloat]
[MMXConvert: toHex toBinarySSE toUByte toSByte toUWord toSWord toUDword toSDword toFloat]
;[SSEFormats: D$ 10 FmtHexPtr FmtBinaryPtr FmtPFPtr FmtPDPtr FmtPUBPtr FmtPSBPtr FmtPUWPtr FmtPSWPtr FmtPUDPtr FmtPSDPtr FmtPUQPtr FmtPSQPtr]
[SSEFormats: D$ 10 FmtHexPtr FmtBinarySSEPtr FmtPFPtr FmtPDPtr FmtPUBPtr FmtPSBPtr FmtPUWPtr FmtPSWPtr FmtPUDPtr FmtPSDPtr FmtPUQPtr FmtPSQPtr]
;[SSEConvert: toHex toBinary toFloat toDouble toUByte toSByte toUWord toSWord toUDword toSDword toHex toHex]
[SSEConvert: toHex toBinarySSE toFloat toDouble toUByte toSByte toUWord toSWord toUDword toSDword toHex toHex]
[SegFormats: D$ 1 FmtHexPtr]
[SegConvert: toSegHex]
[DbgFormats: D$ 1 FmtHexPtr]
[DbgConvert: toHex]
[MemFormats: D$ 6 FmtHexAnsiPtr FmtHexDWPtr FmtHexWPtr FmtFloatsPtr FmtDoublesPtr FmtHexCookedPtr]
[MemConvert: toHexWithAnsi toHexDwords toHexWords toFloats toDoubles toHexWithCookedAnsi]

; Buffers that hold the converted strings, used as sources while drawing the items
[ValueReg0: B$ ? #256
 ValueReg1: B$ ? #256
 ValueReg2: B$ ? #256
 ValueReg3: B$ ? #256
 ValueReg4: B$ ? #256
 ValueReg5: B$ ? #256
 ValueReg6: B$ ? #256
 ValueReg7: B$ ? #256]

[RegValues: ValueReg0 ValueReg1 ValueReg2 ValueReg3 ValueReg4 ValueReg5 ValueReg6 ValueReg7]

Proc DebugDialog_OnFormatChange:
    Arguments @Handle @Index

        mov B$HexSeparator 1
        mov ebx 0
        .While ebx < 8
            mov eax D$TabID, edx D@Index
            .If eax = 0 ; GPR
                mov esi D$GPRRegMap+ebx*4
                mov ecx 4
                call D$GPRConvert+edx*4
            .ElseIf eax = 1 ; FPU
                ; test for empty register slots
                mov eax D$C.FloatSave.TagWord
                ; get top of stack (ecx = TOS * 2)
                mov ecx D$C.FloatSave.StatusWord
                shr ecx 10 | and ecx 00_1110
                lea ecx D$ebx*2+ecx
                and ecx 0F
                shr eax cl | and eax 3
                ;shl eax 2 | lea ecx D$ebx*2 | shl eax cl
                ;shr eax 16 | and eax 3 ; eax = TagBits for register (11: empty)
                If eax = 0011
                    mov edi StringBuf
                    mov D$edi 'EMPT', W$edi+4 'Y'
                Else
                    mov esi C.FloatSave.RegisterArea
                    imul eax ebx 10
                    add esi eax
                    mov ecx 10
                    call D$FPUConvert+edx*4
                EndIf
            .ElseIf eax = 2 ; MMX [V2.1b]
                imul esi ebx 10
                add esi C.FloatSave.RegisterArea
                ;mov esi ebx | shl esi 4
                ;add esi C.regMM
                mov ecx 8
                call D$MMXConvert+edx*4
            .ElseIf eax = 3 ; SSE
                mov esi ebx | shl esi 4
                add esi C.regXMM
                mov ecx 16
                call D$SSEConvert+edx*4
            .ElseIf eax = 4 ; Segment
                If ebx < 6
                    mov esi D$SegRegMap+ebx*4
                    lea edi D$LinearSegmentAddresses+ebx*8
                    call toSegHex
                    ;call D$SegConvert+edx*4
                Else
                    mov edi StringBuf
                    mov B$edi 0
                EndIf
            .Else_if eax = 5 ; Debug & EIP
                If ebx < 7
                    mov esi D$DbgRegMap+ebx*4
                    mov ecx 4
                    call D$DbgConvert+edx*4
                Else
                    mov edi StringBuf
                    mov B$edi 0
                EndIf
            .EndIf

            mov esi edi, edi D$RegValues+ebx*4
            Do | movsb | Loop_Until B$esi-1 = 0

            inc ebx
        .End_While

        mov ecx D$TabID, eax D@Index, D$TabFormats+ecx*4 eax
        call 'USER32.InvalidateRect' D$RegListHandle, &NULL, &TRUE
        call DebugDialog_RedrawRegisterButtons D@Handle
EndP
____________________________________________________________________________________________

; Output register-name/value pair. The name is chosen by the tab-index, the value
; by the index provided in the DrawItemStructure passed by WM_DRAWITEM.

[GPRegs: B$ 'EAX' 0 'EBX' 0 'ECX' 0 'EDX' 0 'ESI' 0 'EDI' 0 'EBP' 0 'ESP' 0]
[STRegs: B$ 'ST0' 0 'ST1' 0 'ST2' 0 'ST3' 0 'ST4' 0 'ST5' 0 'ST6' 0 'ST7' 0]
[MMXRegs: B$ 'MM0' 0 'MM1' 0 'MM2' 0 'MM3' 0 'MM4' 0 'MM5' 0 'MM6' 0 'MM7' 0]
[SSERegs: B$ 'XMM0' 0 'XMM1' 0 'XMM2' 0 'XMM3' 0 'XMM4' 0 'XMM5' 0 'XMM6' 0 'XMM7' 0]
[SegRegs: B$ 'CS' 0 'DS' 0 'ES' 0 'FS' 0 'GS' 0 'SS' 0 ' ' 0 ' ' 0]
[DbgRegs: B$ 'EIP' 0 'DR0' 0 'DR1' 0 'DR2' 0 'DR3' 0 'DR6' 0 'DR7' 0 ' ' 0]

[RegSets: GPRegs STRegs MMXRegs SSERegs SegRegs DbgRegs]

[ItemRect: @x1: ? @y1: ? @x2: ? @y2: ?]
[ItemString: B$ ? #128]

[BackgroundCol:
 @Name:  0BB_BBBB 0CC_CCCC 0CC_FFFF
 @Value: 0DD_DDDD 0FF_FFFF 0CC_0000]

[DRAWITEM_CTLTYPE 0
 DRAWITEM_CTLID 4
 DRAWITEM_ITEMID 8
 DRAWITEM_ITEMACTION 12
 DRAWITEM_ITEMSTATE 16
 DRAWITEM_HWNDITEM 20
 DRAWITEM_HDC 24
 DRAWITEM_RCITEM_LEFT 28
 DRAWITEM_RCITEM_TOP 32
 DRAWITEM_RCITEM_RIGHT 36
 DRAWITEM_RCITEM_BOTTOM 40
 DRAWITEM_ITEMDATA 44]

Proc DebugDialog_OnDrawRegisterItem:
    Arguments @DrawItemStruc
    Local @Brush
    Uses ebx edi

    mov ebx D@DrawItemStruc

    call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0

  ; Print Register name
    mov eax D$ebx+DRAWITEM_RCITEM_LEFT | mov D$ItemRect@x1 eax
    add eax 35 | mov D$ItemRect@x2 eax
    move D$ItemRect@y1 D$ebx+DRAWITEM_RCITEM_TOP
    move D$ItemRect@y2 D$ebx+DRAWITEM_RCITEM_BOTTOM

    mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    call 'GDI32.CreateSolidBrush' D$BackgroundCol@Name+edi*4 | mov D@Brush eax
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, ItemRect, D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, D$BackgroundCol@Name+edi*4

    mov ecx D$TabID, edi D$RegSets+ecx*4
    mov edx 0, al 0
    While edx < D$ebx+DRAWITEM_ITEMID ; item index
        mov ecx 0-1
        repne scasb
        inc edx
    EndWhile

    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

  ; Hilite changed regs
    If D$TabId = 0 ; GPR
        mov ecx D$ebx+DRAWITEM_ITEMID
        bt D$GPR_Modified_Mask ecx | jnc L0>
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0C0
L0: EndIf

  ; Print value
    move D$ItemRect@x1 D$ItemRect@x2
    move D$ItemRect@x2 D$ebx+DRAWITEM_RCITEM_RIGHT

    mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    call 'GDI32.CreateSolidBrush' D$BackgroundCol@Value+edi*4 | mov D@Brush eax
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, ItemRect, D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, D$BackgroundCol@Value+edi*4

    mov ecx D$ebx+DRAWITEM_ITEMID | mov edi D$RegValues+ecx*4
    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect, (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

EndP
____________________________________________________________________________________________

; Create the tabs for the different register types. First check if they are available.

[RegGeneral: 'General' 0 RegFPU: 'FPU' 0 RegMMX: 'MMX' 0 RegSSE: 'SSE' 0 RegSSE2: 'SSE2' 0
 RegSegment: 'Segment' 0 RegDebug: 'Debug' 0]

[DebugRegTabHandle: ?]
[DebugFormatComboHandle: ?]

Proc DebugDialog_CreateRegisterTabs:
    Arguments @Handle
    Local @Index
    ; Tag Dialog 1010

      ; Create register listbox
        movzx ecx W$DebugDialog.Width | sub ecx 6
        mov edx 53 ;mov edx D$DebugDialog.RebarHeight | add edx 50

        call 'User32.CreateWindowExA',
            &WS_EX_CLIENTEDGE,
            {'LISTBOX' 0},
            {'Register' 0},
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&LBS_NOSEL+&LBS_OWNERDRAWFIXED,
            3, edx, ecx, 138,
            D$DebugDialogHandle,
            DEBUGDLG_REG_LIST,
            D$hInstance,
            0

        mov D$RegListHandle eax

      ; Create format combo
        movzx ecx W$DebugDialog.Width | sub ecx 122
        mov edx 29 ;mov edx D$DebugDialog.RebarHeight | add edx 26

        call 'User32.CreateWindowExA',
            0,
            {'COMBOBOX' 0},
            {'Representation' 0},
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&CBS_DROPDOWNLIST+&CBS_HASSTRINGS,
            120, edx, ecx, 200,
            D$DebugDialogHandle,
            DEBUGDLG_FORMAT_COMBO,
            D$hInstance,
            0

        mov D$DebugFormatComboHandle eax
        call 'User32.SendMessageA' eax, &WM_SETFONT, D$DialogFontHandle, &TRUE

      ; Create static control
        mov edx 32 ;mov edx D$DebugDialog.RebarHeight | add edx 29

        call 'User32.CreateWindowExA',
            0,
            {'STATIC' 0},
            D$StrDataFmtPtr,
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&SS_LEFT,
            7, edx, 140, 16,
            D$DebugDialogHandle,
            3,
            D$hInstance,
            0

        call 'User32.SendMessageA' eax, &WM_SETFONT, D$DialogFontHandle, &TRUE

      ; Create tab control
        movzx ecx W$DebugDialog.Width
        mov edx 6 ;mov edx D$DebugDialog.RebarHeight | add edx 4

        call 'User32.CreateWindowExA',
            0,
            {"SysTabControl32",0},
            {"RegisterTab",0},
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE, ;+&TCS_FLATBUTTONS+&TCS_HOTTRACK+&TCS_BUTTONS,
            0, edx, ecx, 184,
            D$DebugDialogHandle,
            DEBUGDLG_REGISTER_TAB,
            D$hInstance,
            0

        mov D$DebugRegTab_handle eax
        mov ebx eax
        call 'User32.SendMessageA' eax, &WM_SETFONT, D$DialogFontHandle, &TRUE

      ; Add tabs
        mov D@Index 0
        mov D$TabItem@imask &TCIF_TEXT+&TCIF_PARAM

        mov D$TabItem@pszText RegGeneral
        mov D$TabItem@lParam 0
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, D@Index, TabItem
        On eax = 0-1, call ReportWinError {'TCM_INSERTITEM' 0}
        inc D@Index

        test D$CPUFlags FLAG_FPU | jz L0>

        mov D$TabItem@pszText RegFPU
        mov D$TabItem@lParam 1
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, D@Index, TabItem
        inc D@Index

L0:     test D$CPUFlags FLAG_MMX | jz L0>

        mov D$TabItem@pszText RegMMX
        mov D$TabItem@lParam 2
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, D@Index, TabItem
        inc D@Index

L0:     test D$CPUFlags FLAG_SSE | jz L0>

        mov D$TabItem@pszText RegSSE
        mov D$TabItem@lParam 3
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, D@Index, TabItem
        inc D@Index

L0:     call SelectTab ebx, DEBUGDLG_REGISTER_TAB, 0

        call DebugDialog_InitRegisterListBox D@Handle
        call DebugDialog_CreateRegisterButtons D@Handle
EndP
____________________________________________________________________________________________

; Show/Hide tabs for advanced registers (segments, debug).

Proc DebugDialog_ShowAdvancedTabs:
    Arguments @Handle @Show
    Local @TabHandle @Index

        call 'USER32.GetDlgItem' D@Handle, DEBUGDLG_REGISTER_TAB
        mov D@TabHandle eax

        call 'User32.SendMessageA' D@TabHandle, &TCM_GETITEMCOUNT, 0, 0
        mov D@Index eax

        .If D@Show = &TRUE
            mov D$TabItem@imask &TCIF_TEXT+&TCIF_PARAM

            mov D$TabItem@pszText RegSegment
            mov D$TabItem@lParam 4
            call 'User32.SendMessageA' D@TabHandle, &TCM_INSERTITEM, D@Index, TabItem
            inc D@Index

            mov D$TabItem@pszText RegDebug
            mov D$TabItem@lParam 5
            call 'User32.SendMessageA' D@TabHandle, &TCM_INSERTITEM, D@Index, TabItem
        .Else
            call 'User32.SendMessageA' D@TabHandle, &TCM_GETCURSEL, 0, 0 | mov ebx eax
            dec D@Index
            call 'User32.SendMessageA' D@TabHandle, &TCM_DELETEITEM, D@Index, 0
            dec D@Index
            call 'User32.SendMessageA' D@TabHandle, &TCM_DELETEITEM, D@Index, 0
            On ebx >= D@Index, mov ebx 0
            call SelectTab D@Handle, DEBUGDLG_REGISTER_TAB, ebx
        .EndIf
        call 'USER32.InvalidateRect' D@Handle &NULL &TRUE
EndP
____________________________________________________________________________________________

; Initially fill the listbox with eight items. The listbox does NOT contain strings, so
; we just set the address of the buffer as item data. Another issue: When the listbox
; is enabled it blocks the mouse from clicking on the register buttons, therefore it is
; disabled.

Proc DebugDialog_InitRegisterListBox:
    Arguments @Handle

        mov eax D$RegListHandle
        call 'User32.EnableWindow' eax, &FALSE
        mov esi RegValues, ebx 0
        While ebx < 8
            lodsd
            mov B$eax 0
            call 'User32.SendMessageA' D$RegListHandle, &LB_ADDSTRING, 0, eax
            inc ebx
        EndWhile
EndP
____________________________________________________________________________________________

; Create the buttons for the general purpose registers.

Proc DebugDialog_CreateRegisterButtons:
    Arguments @Handle

        mov ebx 0, esi GPRegs
        .While ebx < 8
            ; Get position of item and transform in debug dialogs client coordinates
            call 'User32.SendMessageA' D$RegListHandle, &LB_GETITEMRECT, ebx, DebugRect
            mov ecx D$DebugRect@bottom
            sub ecx D$DebugRect@top
            mov D$DebugRect@height ecx
            call 'USER32.ClientToScreen' D$RegListHandle, DebugRect
            call 'USER32.ScreenToClient' D@Handle, DebugRect
            ; Create a button
            mov eax DEBUGDLG_FIRSTREG_BUTTON
            add eax ebx
            call 'User32.CreateWindowExA' 0, ButtonClassName, esi,
                                        &WS_CHILD,
                                        D$DebugRect@left, D$DebugRect@top, 35, D$DebugRect@height,
                                        D@Handle, eax,
                                        D$hinstance, &NULL
            add esi 4
            inc ebx
        .EndWhile
EndP
____________________________________________________________________________________________

; The dialog has been resized, adjust the width and height of the controls.

Proc DebugDialog_OnSize:
    Arguments @Handle, @WidthHeight

      ; calculate the delta values (pixel difference old -> new size)
        movzx eax W$DebugDialog.Width
        movzx esi W@WidthHeight
        sub esi eax
        movzx eax W$DebugDialog.Height
        movzx edi W@WidthHeight+2
        sub edi eax

        move D$DebugDialogSize D@WidthHeight

        call AdjustControlSize D@Handle, DEBUGDLG_REGISTER_TAB, esi, 0
        call AdjustControlSize D@Handle, DEBUGDLG_FORMAT_COMBO, esi, 0
        call AdjustControlSize D@Handle, DEBUGDLG_REG_LIST, esi, 0
        call AdjustControlSize D@Handle, DEBUGDLG_DATA_TAB, esi, edi
        call DebugDialog_AdjustDataTabSize D@Handle
        call 'USER32.InvalidateRect' D@Handle &NULL &TRUE

      ; notify rebar
        movzx eax W$DebugDialog.Width
        call 'User32.SendMessageA' D$Debug_RebarHandle, &WM_SIZE, eax, 0

        mov eax 0
EndP
____________________________________________________________________________________________

; The tab-selection has changed, refill format combo.

[TabFormats: D$ 0 #6]
[RegFormats: GPRFormats FPUFormats MMXFormats SSEFormats SegFormats DbgFormats]
[TabID: ?]

Proc DebugDialog_OnChangeRegisterTab:
    Arguments @Handle

    call 'User32.GetDlgItem' D@Handle, DEBUGDLG_REGISTER_TAB | mov ebx eax
    call 'User32.SendMessageA' ebx, &TCM_GETCURSEL, 0, 0
    call 'User32.SendMessageA' ebx, &TCM_GETITEM, eax, TabItem
    move D$TabID D$TabItem@lParam

    call DebugDialog_UpdateRegisterButtons D@Handle

    ; Refill format/representation combo
    mov ebx D$DebugFormatComboHandle
    call 'User32.SendMessageA' ebx, &CB_RESETCONTENT, 0, 0
    mov ecx D$TabID, esi D$RegFormats+ecx*4
    lodsd | mov ecx eax
D0:     push ecx
            lodsd
            If D$UnicodeStrings = 0
                call 'User32.SendMessageA' ebx, &CB_ADDSTRING, 0, D$eax
            Else
                call 'User32.SendMessageW' ebx, &CB_ADDSTRING, 0, D$eax
            EndIf
        pop ecx
    loop D0<

    mov ecx D$TabID
    call 'User32.SendMessageA' ebx, &CB_SETCURSEL, D$TabFormats+ecx*4, 0
    call 'User32.SendMessageA' D@Handle, &WM_COMMAND, (&CBN_SELCHANGE shl 16 or DEBUGDLG_FORMAT_COMBO), ebx
EndP
____________________________________________________________________________________________

[DataViewHandle: ? MemoryInspectorHandle: ? CurrentDataPageHandle: ?]

Proc DebugDialog_CreateDataTab:
    Arguments @Handle

      ; Create tab control
        movzx ecx W$DebugDialog.Width
        movzx edx W$DebugDialog.Height | sub edx 195

        call 'User32.CreateWindowExA',
            0,
            {"SysTabControl32",0},
            {"DataTab",0},
            &WS_CHILD+&WS_CLIPSIBLINGS+&WS_VISIBLE+&TCS_FLATBUTTONS, ;+&TCS_FLATBUTTONS+&TCS_HOTTRACK+&TCS_BUTTONS,
            0, 195, ecx, edx,
            D$DebugDialogHandle,
            DEBUGDLG_DATA_TAB,
            D$hInstance,
            0

        mov ebx eax
        call 'User32.SendMessageA' eax &WM_SETFONT, D$DialogFontHandle, &TRUE

      ; Add tabs
        mov D$TabItem@imask &TCIF_TEXT
        mov D$TabItem@pszText DataView
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, 0, TabItem
        mov D$TabItem@pszText MemoryInspector
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, 1, TabItem
        mov D$TabItem@pszText CallStack
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, 2, TabItem
        mov D$TabItem@pszText Log
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, 3, TabItem
        mov D$TabItem@pszText AddressSpace
        call 'User32.SendMessageA' ebx, &TCM_INSERTITEM, 4, TabItem

      ; Create sub windows
        call 'User32.CreateDialogParamA' D$hinstance, 1011, ebx, DataViewProc, 0
        mov D$DataViewHandle eax
        mov D$CurrentDataPageHandle eax
        call 'User32.CreateDialogParamA' D$hinstance, 1012, ebx, MemoryInspectorProc, 0
        mov D$MemoryInspectorHandle eax
        call 'USER32.ShowWindow' eax, &SW_HIDE

        ;call 'user32.CreateDialogParamA' D$hinstance, 1015, ebx, CallStackProc, 0
        call CreateNewForm_CallStackForm ebx
        call 'USER32.ShowWindow' D$CallStackFormHandle, &SW_HIDE

        call CreateNewForm_LogForm ebx
        call 'USER32.ShowWindow' D$LogForm_handle, &SW_HIDE

        call CreateNewForm_AddressSpaceForm ebx
        call 'USER32.ShowWindow' D$ASForm_handle, &SW_HIDE

        call DebugDialog_AdjustDataTabSize D@Handle
EndP
____________________________________________________________________________________________

[FPURndModes: FPURndNearest FPURndDown FPURndUp FPURndTrunc]

[FPUPrec24: '24 bits (single)' 0
 FPUPrec53: '53 bits (double)' 0
 FPUPrec64: '64 bits (extended)' 0]

[FPUPrecModes: FPUPrec24 0 FPUPrec53 FPUPrec64]

[NoException: 'No Exception' 0]

DebugDialog_GetFPUStatus:

    ; Output tag word
    CopyString FPUTagWord
    push edi
        mov esi C.FloatSave.TagWord, ecx 2 | call toBinary
        mov ecx StringBufTail | sub ecx edi
        mov esi edi
    pop edi
    rep movsb

    mov W$edi 0A0D | add edi 2

    ; Output control word
    CopyString FPUControlWord
    push edi
        mov esi C.FloatSave.ControlWord, ecx 2 | call toBinary
        mov ecx StringBufTail | sub ecx edi
        mov esi edi
    pop edi
    rep movsb

    mov W$edi 0A0D | add edi 2

    ; Rounding mode
    CopyString FPURoundingMode
    movzx eax W$C.FloatSave.ControlWord
    shr eax 10 | and eax 3
    CopyString D$FPURndModes+eax*4

    mov W$edi 0A0D | add edi 2

    ; Precision
    CopyString FPUPrecision
    movzx eax W$C.FloatSave.ControlWord
    shr eax 8 | and eax 3
    CopyString D$FPUPrecModes+eax*4
    mov W$edi 0A0D | add edi 2

    ; Output statusword
    CopyString FPUStatusWord
    push edi
        mov esi C.FloatSave.Statusword, ecx 2 | call toBinary
        mov ecx StringBufTail | sub ecx edi
        mov esi edi
    pop edi
    rep movsb

    mov W$edi 0A0D | add edi 2

    movzx eax W$C.FloatSave.StatusWord
    mov esi NoException
    test al 01 | jz L0>
    mov esi FLT_INVALID_OPERATION
L0: test al 02 | jz L0>
    mov esi FLT_DENORMAL_OPERAND
L0: test al 04 | jz L0>
    mov esi FLT_DIVIDE_BY_ZERO
L0: test al 08 | jz L0>
    mov esi FLT_OVERFLOW
L0: test al 010 | jz L0>
    mov esi FLT_UNDERFLOW
L0: test al 020 | jz L0>
    mov esi FLT_INEXACT_RESULT
L0: test al 040 | jz L0>
    mov esi FLT_STACK_CHECK
L0: While B$esi <> 0
        movsb
    End_While
    mov B$edi 0

ret

____________________________________________________________________________________________

[CPUFeatures: 'CPU Features:' 0]
[ConditionMove: 'Conditional Move Instructions (CMOV / FCMOV / FCOMI)' 0]

DebugDialog_GetCPUInfo:

    CopyString CPUVendor
    mov W$edi 0A0D | add edi 2

    mov esi CPUName
    While B$esi = ' '
        inc esi
    EndWhile
    While B$esi <> 0
        movsb
    EndWhile
    mov D$edi 0A0D0A0D | add edi 4

    CopyString CPUFeatures
    mov W$edi 0A0D | add edi 2

  ; Scan the general flags
    mov edx D$CPUFlags

    test edx FLAG_MMX | jz L0>
    mov D$edi 'MMX,', B$edi+4 ' '
    add edi 5

L0: test edx FLAG_SSE | jz L0>
    mov D$edi 'SSE,', B$edi+4 ' '
    add edi 5

L0: test edx FLAG_SSE2 | jz L0>
    mov D$edi 'SSE2', W$edi+4 ', '
    add edi 6
L0: sub edi 2
    mov W$edi 0A0D | add edi 2

    test edx FLAG_CMOV | jz L0>
    CopyString ConditionMove
L0: mov W$edi 0A0D | add edi 2

  ; Scan extended flags
    mov edx D$CPUFlagsEx

    test edx FLAG_EX_MMX | jz L0>
    mov D$edi 'AMD ', D$edi+4 'MMX ', D$edi+8 'Exte', D$edi+12 'nsio', D$edi+16 'ns, '
    add edi 20
L0: test edx FLAG_EX_3DNOW | jz L0>
    mov D$edi '3dno', D$edi+4 'w,  '
    add edi 7
L0: test edx FLAG_EX_3DNOWEX | jz L0>
    mov D$edi '3dno', D$edi+4 'w Ex', D$edi+8 'tens', D$edi+12 'ions', W$edi+16 ', '
    add edi 18
L0: sub edi 2
    mov W$edi 0A0D | add edi 2

    mov B$edi 0
ret
____________________________________________________________________________________________

; Set the flag checkboxes (button-style) according to the flag register

[FlagMasks: 0800 080 040 01 04 0400 0200 0100 010]
[FPUFlagMasks: 04000 0400 0200 0100]

Proc DebugDialog_UpdateFlags:
    Arguments @Handle

        ; update the eflags
        mov ebx D$C.regFlag, edi 90, esi FlagMasks
        Do
            mov edx &FALSE
            lodsd
            test ebx eax | jz L0>
            inc edx
L0:         call 'User32.SendMessageA' D$Debug_FlagBarHandle, &TB_CHECKBUTTON, edi, edx
            ;call 'User32.SendDlgItemMessageA' D@Handle, edi, &BM_SETCHECK, edx, 0
            inc edi
        Loop_Until edi > 98

        ; update FPU flags
        mov ebx D$C.FloatSave.StatusWord, edi 101, esi FPUFlagMasks
        Do
            mov edx &FALSE
            lodsd
            test ebx eax | jz L0>
            inc edx
L0:         call 'User32.SendMessageA' D$DebugFPUbarHandle, &TB_CHECKBUTTON, edi, edx
            ;call 'User32.SendDlgItemMessageA' D@Handle, edi, &BM_SETCHECK, edx, 0
            inc edi
        Loop_Until edi > 104
EndP
____________________________________________________________________________________________

Proc DebugDialog_UpdateRegisterButtons:
    Arguments @Handle

        On B$DebuggerReady = &FALSE, ExitP

        .If D$TabID = 0
            mov ebx 0
            While ebx < 8
                mov eax D$GPRRegMap+ebx*4
                call IsProcessMemory D$eax
                mov edi eax

                mov eax ebx
                add eax DEBUGDLG_FIRSTREG_BUTTON
                call 'USER32.GetDlgItem' D@Handle, eax
                mov esi eax
                call 'USER32.ShowWindow' esi, &SW_SHOW
                call 'USER32.EnableWindow' esi, edi
                inc ebx
            EndWhile
        .Else
            mov ebx DEBUGDLG_FIRSTREG_BUTTON
            While ebx <= DEBUGDLG_LASTREG_BUTTON
                call 'USER32.GetDlgItem' D@Handle, ebx
                call 'USER32.ShowWindow' eax, &SW_HIDE
                inc ebx
            EndWhile
        .EndIf
EndP

Proc DebugDialog_RedrawRegisterButtons:
    Arguments @Handle

        mov ebx DEBUGDLG_FIRSTREG_BUTTON
        While ebx <= DEBUGDLG_LASTREG_BUTTON
            call 'USER32.GetDlgItem' D@Handle, ebx
            call 'USER32.InvalidateRect' eax, &NULL, &TRUE
            inc ebx
        EndWhile
EndP
____________________________________________________________________________________________

Proc DebugDialog_AdjustDataTabSize:
    Arguments @Handle
    Local @TabHandle
    Uses ebx

        call 'USER32.GetDlgItem' D@Handle, DEBUGDLG_DATA_TAB
        mov D@TabHandle eax

        call 'USER32.GetClientRect' D@TabHandle, DebugRect
        call 'User32.SendMessageA' D@TabHandle, &TCM_ADJUSTRECT, &FALSE, DebugRect
        mov eax D$DebugRect@left
        mov ebx D$DebugRect@top
        mov ecx D$DebugRect@right
        mov edx D$DebugRect@bottom
        sub ecx eax
        sub edx ebx
        call 'USER32.SetWindowPos' D$CurrentDataPageHandle, 0, eax, ebx, ecx, edx, &SWP_NOZORDER
EndP
____________________________________________________________________________________________

[CONTINUE_RUN 0 CONTINUE_STEP 1 CONTINUE_STEPOVER 2 CONTINUE_RETURNTOCALLER 3]

[ContinueMode: ?] ; is used by debug thread

Proc DebugDialog_ContinueDebuggee:
    On B$IsDebugEvent = &FALSE, ExitP

    call DebugDialog_EnableContinueMenu &FALSE

    move D$ContinueMode D$UserWants

    If B$ContinueMode = CONTINUE_STEPOVER
        call NextInstructionDecode ; decode again - might be overwritten by callstackscanner
        call IsMultiStepInstruction
        On eax = &FALSE, mov B$ContinueMode CONTINUE_STEP
    ElseIf B$ContinueMode = CONTINUE_RUN
        mov D$LastSourcePos 0
    EndIf

    call 'User32.SetWindowTextA' D$DebugDialogHandle, {'Running ...' 0}

    call 'Kernel32.SetEvent' D$UserInputEvent
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Debug dialog image list initialisation & finalization.

[DebugDialog_ImageList: ?]

Proc DebugDialog_CreateImageList:
    Local @Image, @Mask

  ; Create the images
    call 'User32.LoadImageA' D$hInstance, 10, &IMAGE_BITMAP, 0, 0, 0
    If eax = 0
        call ReportWinError {'DebugDialog_CreateImageList: LoadImage (1)' 0}
    EndIf
    mov D@Image eax

    call 'User32.LoadImageA' D$hInstance, 11, &IMAGE_BITMAP, 0, 0, 0
    If eax = 0
        call ReportWinError {'DebugDialog_CreateImageList: LoadImage (2)' 0}
    EndIf
    mov D@Mask eax

    call 'ComCtl32.ImageList_Create' 16, 16, &ILC_COLOR8+&ILC_MASK, 8, 8
    mov D$DebugDialog_ImageList eax

    call 'ComCtl32.ImageList_Add' D$DebugDialog_ImageList, D@Image, D@Mask
    If eax = 0-1
        call ReportWinError {'Debug TB: ImageList_Add' 0}
    EndIf

    call 'GDI32.DeleteObject' D@Image
    call 'GDI32.DeleteObject' D@Mask
EndP
____________________________________________________________________________________________

DebugDialog_DestroyImageList:
    call 'COMCTL32.ImageList_Destroy' D$DebugDialog_ImageList
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

DebuggerMENU: ; Create the MENU of the main debugger window.
____________________________________________________________________________________________
____________________________________________________________________________________________

[DebugMenuTable: 31
 ; ID             | String           | Shortcut      | Imageindex
 M02_Run            StrRunPtr          StrF6           0
 M02_Step_into      StrStepIntoPtr     StrF7           3
 M02_Step_Over      StrStepOverPtr     StrF8           4
 M02_ReturnToCaller StrReturnPtr       StrCtrlF7       5
 M02_Terminate      StrTerminatePtr    StrCtrlF6       1
 M02_Pause          StrPausePtr        StrCtrlF12      2
 M02_HoldOnBP       StrHoldOnBPPtr     0               7
 M02_Inst_Level     StrInstStepPtr     0               7
 M02_Source_Level   StrSrcStepPtr      0               7
 M02_ShowAll        StrShowAllPtr      0               7
 M02_Font           StrFontPtr         0               0-1
 M02_CPU_Info       StrCPUInfoPtr      0               0-1
 M02_FPU_Status     StrFPUStatusPtr    0               0-1
 M02_ShowCodeAt     StrShowCodeAtPtr   0               0-1
 M02_About          StrAboutPtr        0               0-1
 M02_Help           StrDbgHelpPtr      StrF1           0-1

 M03_SHOW_MEM       StrShowInMemInspPtr  0             0-1
 M03_SHOW_PMEM      StrShowPInMemInspPtr 0             0-1
 M03_SHOW_DECL      StrShowDeclPtr       0             0-1
 M03_WATCH_W        StrBreakOnWPtr       0             7
 M03_WATCH_RW       StrBreakOnRWPtr      0             7
 M03_SORTBYNAME     StrSortByNamePtr     0             7
 M03_SORTBYADDRESS  StrSortByAddrPtr     0             7

 MCS_SHOW_INVOKE    StrShowInvokePtr    0              0-1
 MCS_SHOW_DECL      StrShowDeclPtr      0              0-1
 MCS_SHOW_ALL       StrShowAllCallsPtr  0              7
 MCS_HIDE_EXTERNAL  StrHideModCallsPtr  0              7
 MCS_HIDE_INTRA     StrHideIMCallsPtr   0              7
 MCS_SHOWLOCALS     StrShowLocalsPtr    0              7

 MAS_SHOW_MEM       StrShowInMemInspPtr  0             0-1
 MAS_DUMP_PARTIAL_DATA      StrSaveDumpedPtr     0              0-1]
____________________________________________________________________________________________

[M02_Menu  3000                  M02_Run  3001                   M02_Step_Into  3002
 M02_Step_Over  3003             M02_Return_to_Caller  3004      M02_Pause  3005
 M02_Terminate  3006             M02_Hold_on_BP  3007            M02_Inst_Level 3008
 M02_Source_Level 3009           M02_Show_All 3010
 M02_Font  3011                  M02_CPU_Info  3012              M02_FPU_Status  3013
 M02_Show_code_at  3014          M02_About  3015                 M02_Help  3016]
____________________________________________________________________________________________

[DebugMenubarButtons:
 ; iBitmap D, idCommand D, fsState B, fsStyle B, wPad1 W, Data D, iString D
 D$ 0-2 0              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0
 D$ 0-2 1              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0
 D$ 0-2 2              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0
 D$ 0-2 3              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0
 D$ 0-2 4              B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE+&BTNS_DROPDOWN W$ 0 D$ 0 0]

[ContinueMenu: ? BreakMenu: ? SettingsMenu: ? ExtrasMenu: ? HelpMenu: ?]
[NUM_MENUBAR_ENTRIES 5]

Proc DebugWindow_CreateMenu:
    Structure @Item 44,
        @Size 0,  @Mask 4,  @Type 8,  @State 12,  @ID 16,  @SubMenu 20,
        @bmpChecked 24,  @bmpUnchecked 28,  @ItemData 32,  @Data 36,  @cch 40

  ; Store menu handle in ebx
;    call 'User32.CreateMenu' | mov ebx eax

  ; Create menu-bar (toolbar)
    call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0,
        DEBUG_TOOLBAR_STYLE,
        0, 0, 200, 0, D$DebugDialogHandle, DEBUGDLG_MENUBAR, D$hInstance, 0

    mov ebx eax
    call 'User32.SendMessageA' ebx, &WM_SETFONT, D$DialogFont_handle, 0
    call 'User32.SendMessageA' ebx, &TB_BUTTONSTRUCTSIZE, 20, 0

    move D$DebugMenubarButtons+16   D$StrContinuePtr
    move D$DebugMenubarButtons+36   D$StrBreakPtr
    move D$DebugMenubarButtons+56   D$StrSettingsPtr
    move D$DebugMenubarButtons+76   D$StrInformationPtr
    move D$DebugMenubarButtons+96   D$StrHelpPtr

    If D$UnicodeStrings = 0
        call 'User32.SendMessageA' ebx, &TB_ADDBUTTONSA, 5, DebugMenubarButtons
    Else
        call 'User32.SendMessageA' ebx, &TB_ADDBUTTONSW, 5, DebugMenubarButtons
    EndIf

  ; Add the bands
    mov D$DebugRebarBand@hwndChild ebx
    ;or D$DebugRebarBand@fStyle &RBBS_BREAK
    call 'User32.SendMessageA' ebx, &TB_GETMAXSIZE, 0, Point
    ;call 'user32.GetClientRect'  D$Debug_ToolbarHandle, DebugRect
    ;mov eax D$DebugRect@right | sub eax D$DebugRect@left
    mov eax D$PointX | add eax 10
    mov D$DebugRebarBand@cx eax
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_INSERTBAND, BAND_MENUBAR, DebugRebarBand
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_SHOWBAND, BAND_MENUBAR, 1
    ;and D$DebugRebarBand@fStyle (not &RBBS_BREAK)
    ________________________________________________________________________________________

    mov D@Size 44
    mov D@Mask &MIIM_ID+&MIIM_TYPE
    mov D@Type &MFT_OWNERDRAW

  ; Continue menu
    call 'USER32.CreatePopupMenu' | mov edi eax | mov D$ContinueMenu eax
    ;call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrContinuePtr

    mov D@ID M02_Run
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    mov D@ID M02_Step_Into
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    mov D@ID M02_Step_Over
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    mov D@Mask &MIIM_TYPE
    mov D@Type &MFT_SEPARATOR
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    mov D@Mask &MIIM_ID+&MIIM_TYPE
    mov D@Type &MFT_OWNERDRAW
    mov D@ID M02_Return_To_Caller
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

  ; Break menu
    call 'USER32.CreatePopupMenu' | mov edi eax, D$BreakMenu eax
    ;call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrBreakPtr

    mov D@ID M02_Pause
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    mov D@ID M02_Terminate
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

  ; Settings menu
    call 'USER32.CreatePopupMenu' | mov edi eax, D$SettingsMenu eax
    ;call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrSettingsPtr

    mov D@ID M02_Hold_on_BP
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    mov D@ID M02_Inst_Level
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    mov D@ID M02_Source_Level
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    mov D@ID M02_Show_All
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    mov D@ID M02_Font
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    mov D@Mask &MIIM_TYPE
    mov D@Type &MFT_SEPARATOR
    call 'User32.InsertMenuItemA' edi, 4, 1, D@Item
    call 'User32.InsertMenuItemA' edi, 3, 1, D@Item
    call 'User32.InsertMenuItemA' edi, 1, 1, D@Item
;;
  ; Toolbar sub menu
    call 'USER32.CreatePopupMenu' | mov esi eax, D$Debug_ToolbarMenu eax
    call 'User32.AppendMenuA' edi, &MF_STRING+&MF_POPUP, esi, {'Toolbar' 0}
    ;call 'User32.AppendMenuA' ebx, &MF_STRING, DEBUGDLG_TB_CONFIGURE, {'Configure ...' 0}
    call 'User32.AppendMenuA' esi, &MF_STRING, DEBUGDLG_TB_SHOW_TEXT, {'Show text' 0}
    call 'User32.AppendMenuA' esi, &MF_SEPARATOR, 0, 0
    call 'User32.AppendMenuA' esi, &MF_STRING+&MF_CHECKED, DEBUGDLG_TB_SHOW_CMDS, {'Show commands' 0}
    call 'User32.AppendMenuA' esi, &MF_STRING+&MF_CHECKED, DEBUGDLG_TB_SHOW_FLAGS, {'Show flags' 0}
    call 'User32.AppendMenuA' esi, &MF_STRING, DEBUGDLG_TB_SHOW_FPU, {'Show FPU flags' 0}
;;
  ; Information menu
    call 'USER32.CreatePopupMenu' | mov edi eax, D$ExtrasMenu eax
    ;call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrInformationPtr

    mov D@Mask &MIIM_ID+&MIIM_TYPE
    mov D@Type &MFT_OWNERDRAW

    mov D@ID M02_CPU_Info
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    mov D@ID M02_FPU_Status
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    mov D@ID M02_Show_code_at
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

    mov D@Mask &MIIM_TYPE
    mov D@Type &MFT_SEPARATOR
    call 'User32.InsertMenuItemA' edi, 2, 1, D@Item

  ; Help menu
    call 'USER32.CreatePopupMenu' | mov edi eax, D$HelpMenu eax
    ;call AppendMenu ebx, &MF_STRING+&MF_POPUP, edi, D$StrHelpPtr

    mov D@Mask &MIIM_ID+&MIIM_TYPE
    mov D@Type &MFT_OWNERDRAW

    mov D@ID M02_About
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item
    mov D@ID M02_Help
    call 'User32.InsertMenuItemA' edi, -1, 1, D@Item

  ; Return menu handle
    mov D$MenuBarHandle ebx
    mov eax ebx
EndP
____________________________________________________________________________________________

[MenubarHook: ? CurrentMenubarIndex: ?]

; Observe mouse messages to close the popup menu when the user clicks outside the menu area
; and to switch to other submenus when the mouse hovers above another menubar item.

Proc MenubarHookProc:
    Arguments @Code, @wParam, @Msg
    Uses ebx, edi

    call 'USER32.CallNextHookEx' D$MenubarHook, D@Code, D@wParam, D@Msg
    push eax

        mov ebx D@Msg
        mov eax D$ebx+4

        .If eax = &WM_LBUTTONDOWN
          ; Collapse menu if clicked outside menu area.
            If D@Code <> &MSGF_MENU
                call 'User32.PostMessageA' D$DebugDialogHandle, WM_COLLAPSEMENU, 0, 0
            EndIf

        .ElseIf eax = &WM_RBUTTONDOWN
          ; Collapse menu if clicked outside menu area.
            If D@Code <> &MSGF_MENU
                call 'User32.PostMessageA' D$DebugDialogHandle, WM_COLLAPSEMENU, 0, 0
            EndIf

        .ElseIf eax = &WM_MOUSEMOVE
          ; Check if mouse hovered above different menubar item. Perform hittest to
          ; identify which item is selected and if selection has changed collapse the
          ; old menu and track the new menu.
            movzx eax W$ebx+12
            movzx edx W$ebx+14
            mov D$PointX eax
            mov D$PointY edx
            call 'USER32.ScreenToClient' D$MenuBarHandle, Point
            call 'User32.SendMessageA' D$MenuBarHandle, &TB_HITTEST, 0, Point
            test eax eax | js L0>
            cmp eax NUM_MENUBAR_ENTRIES | jae L0>
            mov edi eax
            If D$CurrentMenubarIndex <> eax
                call 'User32.PostMessageA' D$DebugDialogHandle, WM_COLLAPSEMENU, 0, 0
                call 'User32.PostMessageA' D$DebugDialogHandle, WM_POPUPMENU, edi, 0
            EndIf
L0:
        .ElseIf eax = &WM_KEYDOWN
          ; Select the next submenu when user presses left/right.
          ; Note: This does *not* work when the mouse is over the menubar as
          ; it continously receives WM_MOUSEMOVE messages (why?) which makes it
          ; switch back to the former menu immediately.
            If D$ebx+8 = &VK_LEFT
                call 'User32.PostMessageA' D$DebugDialogHandle, WM_COLLAPSEMENU, 0, 0
                mov eax D$CurrentMenubarIndex
                dec eax | jns L1>
                mov eax (NUM_MENUBAR_ENTRIES-1)
L1:             call 'User32.PostMessageA' D$DebugDialogHandle, WM_POPUPMENU, eax, 0
            ElseIf D$ebx+8 = &VK_RIGHT
                call 'User32.PostMessageA' D$DebugDialogHandle, WM_COLLAPSEMENU, 0, 0
                mov eax D$CurrentMenubarIndex
                inc eax | cmp eax NUM_MENUBAR_ENTRIES | jb L1>
                mov eax 0
L1:             call 'User32.PostMessageA' D$DebugDialogHandle, WM_POPUPMENU, eax, 0
            EndIf

        .EndIf

    pop eax
EndP
____________________________________________________________________________________________

[HotTracking: ? HotTrackMenu: ?]

; Track popup menu and prepare the GUI for menu navigation (install hook).
; TODO: Fix alignment of the rightmost popup menus.

Proc DebugDialog_OnPopupMenu:
    Arguments @Item
    Structure @Rect 16, @left 0,  @top 4,  @right 8,  @bottom 12

    call 'User32.SendMessageA' D$MenuBarHandle, &TB_GETRECT, D@Item, D@Rect
    move D$PointX D@left
    move D$PointY D@bottom
    call 'USER32.ClientToScreen' D$MenuBarHandle, Point

    mov edx D@Item
    If edx = 0
        mov eax D$ContinueMenu
    ElseIf edx = 1
        mov eax D$BreakMenu
    ElseIf edx = 2
        mov eax D$SettingsMenu
    ElseIf edx = 3
        mov eax D$ExtrasMenu
    ElseIf edx = 4
        mov eax D$HelpMenu
    EndIf
    mov D$HottrackMenu eax, D$CurrentMenuBarIndex edx

  ; How can I measure the width of a popup menu before it is tracked?
    call 'USER32.GetWindowRect' D$HotTrackMenu, D@Rect
    call 'USER32.GetSystemMetrics' &SM_CXSCREEN
    mov ecx D@right | sub ecx D@left
    mov edx D$PointX
    add edx ecx
    If edx > eax
        sub eax ecx
        mov D$PointX eax
    EndIf

    call 'User32.SendMessageA' D$MenuBarHandle, &TB_PRESSBUTTON, D@Item, 1

    mov D$HotTracking 1

  ; Install mouse hook
    If D$MenubarHook <> 0
        call 'kernel32.OutputDebugStringA' {'Unhook (!)' 0}
        call 'USER32.UnhookWindowsHookEx' D$MenubarHook
        mov D$MenubarHook 0
    EndIf

    call 'Kernel32.GetCurrentThreadId'
    call 'User32.SetWindowsHookExA' &WH_MSGFILTER, MenubarHookProc, D$hInstance, eax
    mov D$MenubarHook eax

    PRINTLN 'SetHook'

  ; Open the menu. Note: The call doesn't return until the menu is closed!
    call 'USER32.TrackPopupMenu' D$HotTrackMenu, &TPM_LEFTBUTTON, D$PointX, D$PointY, 0, D$DebugDialogHandle, 0

    call 'User32.SendMessageA' D$MenuBarHandle, &TB_PRESSBUTTON, D@Item, 0

    If D$MenuBarHook <> 0
        PRINTLN 'Unhook'
        call 'USER32.UnhookWindowsHookEx' D$MenubarHook
        mov D$MenubarHook 0
    EndIf

    mov D$HotTracking 0
EndP
____________________________________________________________________________________________
[PRINTLN | call 'kernel32.OutputDebugStringA' {#1 0}]

; Collapse menu.
; TODO: Check if WM_CANCELMODE works under Windows NT.

DebugDialog_OnCollapseMenu:
    call 'User32.SendMessageA' D$DebugDialogHandle, &WM_CANCELMODE, 0, 0
    mov D$HotTracking 0
    If D$MenubarHook <> 0
        PRINTLN 'Unhook (C)'
        call 'USER32.UnhookWindowsHookEx' D$MenubarHook
        mov D$MenubarHook 0
    EndIf
ret
____________________________________________________________________________________________

; Show menu when user clicks on menubar item.

Proc DebugDialog_MenubarNotify:
    Arguments @NotifyMsg, @Code, @Item

    mov eax 0

    If D@Code = &TBN_DROPDOWN
        call 'User32.PostMessageA' D$DebugDialogHandle, WM_POPUPMENU, D@Item, 0
        mov eax &TBDDRET_DEFAULT
    EndIf
EndP
____________________________________________________________________________________________

; Handle key commands [F10, ALT+x]

[PrevFocusWindow: ?]

Proc DebugDialog_OnSysCommand:
    Arguments @Handle, @Type, @lParam
    pushad

    ..If D@Type = &SC_KEYMENU

        .If D$HotTracking = 0

            mov eax D@lParam
            If eax = 0 ; F10
                ;SendMessage D$MenuBarHandle, &TB_PRESSBUTTON, 0, 0
                call 'User32.SendMessageA' D$MenuBarHandle, &TB_SETHOTITEM, 0, 0
                call 'USER32.SetFocus' D$MenuBarHandle
                ;mov D$PrevFocusWindow eax
            EndIf

        .EndIf

       ; mov eax 0

    ..Else
        popad
        call 'User32.DefWindowProcA' D@Handle, &WM_SYSCOMMAND, D@Type, D@lParam
        ExitP
    ..EndIf

    popad
    mov eax &FALSE
EndP
____________________________________________________________________________________________

; Handle keyboard menu navigation [Arrow keys, ESC]

Proc DebugDialog_OnKeyDown:
    Arguments @Handle, @Key, @lParam

    call 'User32.SendMessageA' D$MenuBarHandle, &TB_GETHOTITEM, 0, 0

    .If eax <> 0-1
        If D@Key = &VK_RIGHT
            inc eax
            On eax = NUM_MENUBAR_ENTRIES,
                mov eax 0

        ElseIf D@Key = &VK_LEFT
            dec eax
            On eax = 0-1
                mov eax NUM_MENUBAR_ENTRIES-1

        ElseIf D@Key = &VK_ESCAPE
            ;call 'USER32.SetFocus' D$PrevFocusWindow
            mov eax 0-1
        EndIf

        call 'User32.SendMessageA' D$MenuBarHandle, &TB_SETHOTITEM, eax, 0
    .EndIf

    mov eax 0
EndP
____________________________________________________________________________________________
; Call (indeed jump to) either to the ansi or unicode taste of API functions.

AppendMenu:
    cmp B$UnicodeStrings 1 | je L0>
    jmp 'User32.AppendMenuA'
L0: jmp 'User32.AppendMenuW'

DrawText:
    cmp B$UnicodeStrings 1 | je L0>
    jmp 'User32.DrawTextA'
L0: jmp 'User32.DrawTextW'
____________________________________________________________________________________________

____________________________________________________________________________________________

; Enable / Gray-out execution control commands.

Proc DebugDialog_EnableContinueMenu:
    Arguments @Enable

    If D@Enable = 1
        mov ebx &MF_ENABLED
    Else
        mov ebx &MF_GRAYED
    EndIf

    call 'USER32.EnableMenuItem' D$ContinueMenu, M02_Run, ebx
    call 'USER32.EnableMenuItem' D$ContinueMenu, M02_Step_Into, ebx
    call 'USER32.EnableMenuItem' D$ContinueMenu, M02_Step_Over, ebx
    call 'USER32.EnableMenuItem' D$ContinueMenu, M02_Return_to_Caller, ebx

    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Run, D@Enable
    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Step_Into, D@Enable
    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Step_over, D@Enable
    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Return_to_Caller, D@Enable

  ; Invert
    If D@Enable = 1
        mov ebx &MF_GRAYED
        mov D@Enable 0
    Else
        mov ebx &MF_ENABLED
        mov D@Enable 1
    EndIf
    call 'USER32.EnableMenuItem' D$BreakMenu, M02_Pause, ebx
    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_ENABLEBUTTON, M02_Pause, D@Enable

    call 'USER32.DrawMenuBar' D$DebugDialogHandle
EndP
____________________________________________________________________________________________

DebugDialog_InitDbgMenu:
    call 'USER32.CheckMenuItem' D$SettingsMenu, M02_Hold_on_BP, &MF_CHECKED
    call 'USER32.CheckMenuRadioItem' D$SettingsMenu,
        M02_Inst_Level, M02_Source_Level, M02_Inst_Level, &MF_BYCOMMAND

    call DebugDialog_EnableContinueMenu 0
ret
____________________________________________________________________________________________

; Process WM_DRAWITEM message for owner-drawn menu items.

Proc DebugDialog_OnDrawMenuItem:
    Arguments @DrawItemStruc
    Local @Brush

    mov ebx D@DrawItemStruc

    mov esi DebugMenuTable
    lodsd | mov ecx eax ; number of entries
    mov eax D$ebx+DRAWITEM_ITEMID
    While D$esi <> eax
        add esi 16
        dec ecx | jz L9>>
    EndWhile

    call 'GDI32.SelectObject' D$ebx+DRAWITEM_HDC, D$DialogFont_handle
    push eax

    Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED
        call 'USER32.GetSysColor' &COLOR_GRAYTEXT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        jmp L1>
    Test_Else_If D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
        call 'USER32.GetSysColor' &COLOR_HIGHLIGHTTEXT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax

        call 'USER32.GetSysColor' &COLOR_HIGHLIGHT
        call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, eax

        call 'USER32.GetSysColorBrush' &COLOR_HIGHLIGHT
    Test_Else
        call 'USER32.GetSysColor' &COLOR_MENUTEXT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax

L1:     call 'USER32.GetSysColor' &COLOR_MENU
        call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, eax

        call 'USER32.GetSysColorBrush' &COLOR_MENU
    Test_End

    lea edx D$ebx+DRAWITEM_RCITEM_LEFT
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, edx, eax

    mov eax D$esi+12 ; image index
    If eax <> 0-1
        mov ecx D$ebx+DRAWITEM_RCITEM_LEFT | add ecx 2
        mov edx D$ebx+DRAWITEM_RCITEM_TOP  | add edx 2
        Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED
            mov edi &ILD_MASK
        Test_Else_If D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
            mov edi &ILD_NORMAL
        Test_Else
            mov edi &ILD_TRANSPARENT
        Test_End

        Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_CHECKED
            mov eax 6
        Test_End

L0:     call 'COMCTL32.ImageList_Draw' D$DebugDialog_ImageList, eax, D$ebx+DRAWITEM_HDC, ecx, edx, edi
    EndIf

    add D$ebx+DRAWITEM_RCITEM_LEFT 22

    mov eax D$esi+4
    call DrawMenuItemText D$eax, &DT_LEFT

    mov eax D$esi+8
    If eax <> 0
      ; Draw shortcut rightaligned
        dec D$ebx+DRAWITEM_RCITEM_RIGHT
        call DrawMenuItemTextA eax, &DT_RIGHT
    EndIf

    pop eax
    call 'GDI32.SelectObject' D$ebx+DRAWITEM_HDC, eax

L9: mov eax 1
EndP
____________________________________________________________________________________________

; ebx > DRAWITEMSTRUCT

Proc DrawMenuItemText: ; Localized (ANSI or Unicode)
    Arguments @Text, @Align

  ; Grayed text is drawn white overlayed by gray (shifted 1 pixel in both directions)
    Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED

        inc D$ebx+DRAWITEM_RCITEM_TOP
        inc D$ebx+DRAWITEM_RCITEM_BOTTOM
        inc D$ebx+DRAWITEM_RCITEM_LEFT
        inc D$ebx+DRAWITEM_RCITEM_RIGHT

        call 'USER32.GetSysColor' &COLOR_3DHIGHLIGHT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        lea edx D$ebx+DRAWITEM_RCITEM_LEFT
        mov eax &DT_SINGLELINE+&DT_VCENTER | or eax D@Align
        call DrawText D$ebx+DRAWITEM_HDC, D@Text, 0-1, edx, eax

        dec D$ebx+DRAWITEM_RCITEM_TOP
        dec D$ebx+DRAWITEM_RCITEM_BOTTOM
        dec D$ebx+DRAWITEM_RCITEM_LEFT
        dec D$ebx+DRAWITEM_RCITEM_RIGHT

        call 'USER32.GetSysColor' &COLOR_GRAYTEXT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        call 'GDI32.SetBkMode' D$ebx+DRAWITEM_HDC, &TRANSPARENT

    Test_End

    lea edx D$ebx+DRAWITEM_RCITEM_LEFT
    mov eax &DT_SINGLELINE+&DT_VCENTER | or eax D@Align
    call DrawText D$ebx+DRAWITEM_HDC, D@Text, 0-1, edx, eax

EndP
____________________________________________________________________________________________

; ebx > DRAWITEMSTRUCT

Proc DrawMenuItemTextA: ; ANSI only
    Arguments @Text, @Align

  ; Grayed text is drawn white overlayed by gray (shifted 1 pixel in both directions)
    Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED

        inc D$ebx+DRAWITEM_RCITEM_TOP
        inc D$ebx+DRAWITEM_RCITEM_BOTTOM
        inc D$ebx+DRAWITEM_RCITEM_LEFT
        inc D$ebx+DRAWITEM_RCITEM_RIGHT

        call 'USER32.GetSysColor' &COLOR_3DHIGHLIGHT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        lea edx D$ebx+DRAWITEM_RCITEM_LEFT
        mov eax &DT_SINGLELINE+&DT_VCENTER | or eax D@Align
        call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, D@Text, 0-1, edx, eax

        dec D$ebx+DRAWITEM_RCITEM_TOP
        dec D$ebx+DRAWITEM_RCITEM_BOTTOM
        dec D$ebx+DRAWITEM_RCITEM_LEFT
        dec D$ebx+DRAWITEM_RCITEM_RIGHT

        call 'USER32.GetSysColor' &COLOR_GRAYTEXT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        call 'GDI32.SetBkMode' D$ebx+DRAWITEM_HDC, &TRANSPARENT

    Test_End

    lea edx D$ebx+DRAWITEM_RCITEM_LEFT
    mov eax &DT_SINGLELINE+&DT_VCENTER | or eax D@Align
    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, D@Text, 0-1, edx, eax

EndP
____________________________________________________________________________________________

Proc DebugDialog_OnMeasureMenuItem:
    Arguments @MeasureItem

    mov ebx D@MeasureItem
    mov edx D$ebx+8 ; item id
    mov esi DebugMenuTable
    lodsd ; eax = num entries
    While D$esi <> edx
        add esi 16
        dec eax | jz L9>
    EndWhile

    mov eax D$esi+4
    call MeasureStringWidth D$eax, D$DialogFontHandle

    On D$esi+8 <> 0,
        add eax 50 ; shortcut

    add eax 22 ; icon+padding

    mov D$ebx+12 eax ; width
    mov D$ebx+16 20 ; height
L9:
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

DebuggerToolbar: ; TOOLBAR and coolbar (REBAR) of debugger main window.
____________________________________________________________________________________________
____________________________________________________________________________________________

[DEBUGDLG_REBAR 331 DEBUGDLG_TOOLBAR 332 DEBUGDLG_MENUBAR 333
 DEBUGDLG_FLAGS 334 DEBUGDLG_FPUFLAGS 335]

; Flow control buttons
[DebugToolbarButtons:
 ; iBitmap D, idCommand D, fsState B, fsStyle B, wPad1 W, Data D, iString D
 D$ 0 M02_Run              B$ 0                &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 1 M02_Terminate        B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 2 M02_Pause            B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 3 M02_Step_Into        B$ 0                &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 4 M02_Step_Over        B$ 0                &BTNS_AUTOSIZE W$ 0 D$ 0 0
 D$ 5 M02_Return_to_Caller B$ 0                &BTNS_AUTOSIZE W$ 0 D$ 0 0]
 ; Strings

[FlagStr:
    @O: 'O'   0 @S: 'S'    0 @Z: 'Z'   0 @C: 'C'   0 @P: 'P' 0 @D: 'Dir' 0
    @I: 'Int' 0 @T: 'Trap' 0 @A: 'Aux' 0
    @C3: 'C3' 0 @C2: 'C2'  0 @C1: 'C1' 0 @C0: 'C0' 0]

; Standard flag buttons
[DebugFlagButtons:
 D$ 0-2 90 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@O
 D$ 0-2 91 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@S
 D$ 0-2 92 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@Z
 D$ 0-2 93 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C
 D$ 0-2 94 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@P
 D$ 0-2 95 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@D
 D$ 0-2 96 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@I
 D$ 0-2 97 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@T
 D$ 0-2 98 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@A]

; FPU flag buttons
[DebugFPUButtons:
 D$ 0-2 101 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C3
 D$ 0-2 102 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C2
 D$ 0-2 103 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C1
 D$ 0-2 104 B$ &TBSTATE_ENABLED &BTNS_AUTOSIZE__&BTNS_CHECK W$ 0 D$ 0 FlagStr@C0]

[DebugToolbarHandle: ? Debug_FlagBarHandle: ? Debug_FPUBarHandle: ? DebugRebarHandle: ? Debug_ToolbarMenu: ?]

[DebugRebarBand:
   @cbSize:     D$ len
   @fMask:      D$ &RBBIM_CHILD+&RBBIM_STYLE+&RBBIM_CHILDSIZE+&RBBIM_SIZE+&RBBIM_IDEALSIZE+&RBBIM_HEADERSIZE
   @fStyle:     D$ &RBBS_CHILDEDGE+&RBBS_GRIPPERALWAYS ;+&RBBS_BREAK
   0 0 0 0 0
   @hwndChild:  D$ 0
   @cxMinChild: D$ 10
   @cyMinChild: D$ 22
   @cx:         D$ 0 0
   @wID:        D$ 0
   @cyChild:    D$ 0
   @cyMaxChild: D$ 0
   @cyIntegral: D$ 0
   @cxIdeal:    D$ 0 0
   @cxHeader:   D$ 6 ]


[Init_Common_Controls:
 @dwSize: D$ len
 @dwICC:  D$ &ICC_COOL_CLASSES+&ICC_BAR_CLASSES]
____________________________________________________________________________________________

[DEBUG_TOOLBAR_STYLE &WS_CHILD+&CCS_ADJUSTABLE+&TBSTYLE_FLAT+&TBSTYLE_LIST+&TBSTYLE_AUTOSIZE+&TBSTYLE_TRANSPARENT+&CCS_NOPARENTALIGN+&CCS_NODIVIDER+&CCS_NORESIZE]

[DebugShowTBText: ?]

Proc DebugDialog_CreateCommandTB:
  ; Save states & clear if toolbar is REcreated
    .If D$Debug_ToolbarHandle <> 0
        call 'User32.SendMessageA' D$DebugToolbarHandle, &TB_GETSTATE, M02_Run, 0
        mov B$DebugToolbarButtons+8 al
        call 'User32.SendMessageA' D$DebugToolbarHandle, &TB_GETSTATE, M02_Terminate, 0
        mov B$DebugToolbarButtons+28 al
        call 'User32.SendMessageA' D$DebugToolbarHandle, &TB_GETSTATE, M02_Pause, 0
        mov B$DebugToolbarButtons+48 al
        call 'User32.SendMessageA' D$DebugToolbarHandle, &TB_GETSTATE, M02_Step_Into, 0
        mov B$DebugToolbarButtons+68 al
        call 'User32.SendMessageA' D$DebugToolbarHandle, &TB_GETSTATE, M02_Step_Over, 0
        mov B$DebugToolbarButtons+88 al
        call 'User32.SendMessageA' D$DebugToolbarHandle, &TB_GETSTATE, M02_Return_to_Caller, 0
        mov B$DebugToolbarButtons+108 al
        call 'User32.SendMessageA' D$Debug_RebarHandle, &RB_DELETEBAND, 1, 0
        call 'USER32.DestroyWindow' D$Debug_ToolbarHandle
    .EndIf

  ; Create toolbar
    call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0,
        DEBUG_TOOLBAR_STYLE,
        0, 0, 120, 0, D$DebugDialogHandle, DEBUGDLG_TOOLBAR, D$hInstance, 0

    mov D$DebugToolbarHandle eax
    call 'User32.SendMessageA' eax, &WM_SETFONT, D$DialogFont_handle, 0

    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_BUTTONSTRUCTSIZE, 20, 0
    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_SETIMAGELIST, 0, D$DebugDialog_ImageList

  ; Activate / Deactivate Text
    If D$DebugShowTBText = 1
        move D$DebugToolbarButtons+16   D$StrRunPtr
        move D$DebugToolbarButtons+36   D$StrTerminatePtr
        move D$DebugToolbarButtons+56   D$StrPausePtr
        move D$DebugToolbarButtons+76   D$StrStepPtr
        move D$DebugToolbarButtons+96   D$StrStepOverPtr
        move D$DebugToolbarButtons+116  D$StrRetPtr
    Else
        mov D$DebugToolbarButtons+16   0
        mov D$DebugToolbarButtons+36   0
        mov D$DebugToolbarButtons+56   0
        mov D$DebugToolbarButtons+76   0
        mov D$DebugToolbarButtons+96   0
        mov D$DebugToolbarButtons+116  0
    EndIf

    If D$UnicodeStrings = 0
        call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_ADDBUTTONSA, 6, DebugToolbarButtons
    Else
        call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_ADDBUTTONSW, 6, DebugToolbarButtons
    EndIf

  ; Add band
    move D$DebugRebarBand@hwndChild D$DebugToolbarHandle
    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_GETMAXSIZE, 0, Point
    ;call 'user32.GetClientRect'  D$Debug_ToolbarHandle, DebugRect
    ;mov eax D$DebugRect@right | sub eax D$DebugRect@left
    mov eax D$PointX | add eax 10
    mov D$DebugRebarBand@cx eax
    ;mov D$DebugRebarBand@cxIdeal eax
    ;mov D$DebugRebarBand@cxMinChild eax

    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_INSERTBAND, BAND_COMMANDBAR, DebugRebarBand
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_SHOWBAND, BAND_COMMANDBAR, 1
EndP
____________________________________________________________________________________________

[BAND_MENUBAR 0 BAND_COMMANDBAR 1 BAND_FLAGS 2 BAND_FPUFLAGS 3]

Proc DebugDialog_CreateToolbar:

    call 'ComCtl32.InitCommonControlsEx' Init_Common_Controls

  ; Create rebar
    call 'User32.CreateWindowExA',
        &WS_EX_TOOLWINDOW, {'ReBarWindow32' 0}, 0,
        &WS_VISIBLE+&WS_CHILD+&RBS_VARHEIGHT+&RBS_FIXEDORDER+&RBS_BANDBORDERS+&WS_BORDER+&RBS_VERTICALGRIPPER, ;+&CCS_NODIVIDER,
        0, 0, 0, 0,
        D$DebugDialogHandle, DEBUGDLG_REBAR,
        D$hInstance, 0

    mov D$DebugRebarHandle eax

  ; Create menubar
    call DebugWindow_CreateMenu

  ; Create flag toolbar
    call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0,
        DEBUG_TOOLBAR_STYLE,
        0, 0, 80, 0, D$DebugDialogHandle, DEBUGDLG_FLAGS, D$hInstance, 0

    mov D$Debug_FlagBarHandle eax
    call 'User32.SendMessageA' eax, &WM_SETFONT, D$DialogFont_handle, 0

  ; Create FPU flag toolbar
    call 'User32.CreateWindowExA' 0, {'ToolbarWindow32' 0}, 0,
        DEBUG_TOOLBAR_STYLE,
        0, 0, 80, 0, D$DebugDialogHandle, DEBUGDLG_FPUFLAGS, D$hInstance, 0

    mov D$Debug_FPUBarHandle eax
    call 'User32.SendMessageA' eax, &WM_SETFONT, D$DialogFont_handle, 0

  ; Send the TB_BUTTONSTRUCTSIZE message, which is required for
  ; backward compatibility.
    call 'User32.SendMessageA' D$Debug_FlagBarHandle, &TB_BUTTONSTRUCTSIZE, 20, 0
    call 'User32.SendMessageA' D$Debug_FPUBarHandle, &TB_BUTTONSTRUCTSIZE, 20, 0

    call DebugDialog_CreateCommandTB

  ; Add buttons
    call 'User32.SendMessageA' D$Debug_FlagBarHandle, &TB_ADDBUTTONS, 9, DebugFlagButtons
    call 'User32.SendMessageA' D$Debug_FPUBarHandle, &TB_ADDBUTTONS, 4, DebugFPUButtons

  ; Add the bands
    move D$DebugRebarBand@hwndChild D$Debug_FlagbarHandle
    call 'User32.SendMessageA' D$Debug_FlagbarHandle, &TB_GETMAXSIZE, 0, Point
    mov eax D$PointX ;| add eax 8
    ;call 'user32.GetClientRect'  D$Debug_FlagbarHandle, DebugRect
    ;mov eax D$DebugRect@right | sub eax D$DebugRect@left

    ;mov  D$DebugRebarBand@cxIdeal eax
    mov  D$DebugRebarBand@cx eax
    ;mov  D$DebugRebarBand@cxMinChild eax
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_INSERTBAND, BAND_FLAGS, DebugRebarBand
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_SHOWBAND, BAND_FLAGS, 1

    move D$DebugRebarBand@hwndChild D$Debug_FPUbarHandle
    call 'User32.SendMessageA' D$Debug_FPUbarHandle, &TB_GETMAXSIZE, 0, Point
    mov eax D$PointX ;| add eax 8
    ;call 'user32.GetClientRect'  D$Debug_FPUbarHandle, DebugRect
    ;mov eax D$DebugRect@right | sub eax D$DebugRect@left

    ;mov  D$DebugRebarBand@cxIdeal eax
    mov  D$DebugRebarBand@cx eax
    ;mov  D$DebugRebarBand@cxMinChild eax
    or   D$DebugRebarBand@fStyle &RBBS_HIDDEN
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_INSERTBAND, 0-1, DebugRebarBand

  ; Create the context menu
    call 'USER32.CreatePopupMenu' | mov ebx eax, D$Debug_ToolbarMenu eax
    ;call 'User32.AppendMenuA' ebx, &MF_STRING, DEBUGDLG_TB_CONFIGURE, {'Configure ...' 0}
    call 'User32.AppendMenuA' ebx, &MF_STRING, DEBUGDLG_TB_SHOW_TEXT, {'Show text' 0}
    call 'User32.AppendMenuA' ebx, &MF_SEPARATOR, 0, 0
    call 'User32.AppendMenuA' ebx, &MF_STRING+&MF_CHECKED, DEBUGDLG_TB_SHOW_CMDS, {'Show commands' 0}
    call 'User32.AppendMenuA' ebx, &MF_STRING+&MF_CHECKED, DEBUGDLG_TB_SHOW_FLAGS, {'Show flags' 0}
    call 'User32.AppendMenuA' ebx, &MF_STRING, DEBUGDLG_TB_SHOW_FPU, {'Show FPU flags' 0}

;;
    call 'User32.SendMessageA' D@TbHandle, &TB_GETITEMRECT, 0, DebugRect    
    mov eax D$DebugRect@width | shl eax 16
    call 'User32.SendMessageA' D@TbHandle, &TB_SETBUTTONWIDTH, 0, eax
;;
;;
    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_AUTOSIZE, 0, 0 
    call 'User32.SendMessageA' D$Debug_FlagbarHandle, &TB_AUTOSIZE, 0, 0 
    call 'User32.SendMessageA' D$Debug_FPUbarHandle,  &TB_AUTOSIZE, 0, 0 
;;
    ;call 'User32.ShowWindow' D@TbHandle, &SW_SHOW
EndP
____________________________________________________________________________________________

DebugDialog_DestroyToolbar:
    call 'USER32.DestroyWindow' D$DebugToolbarHandle
    mov D$DebugToolbarHandle 0
    call 'USER32.DestroyWindow' D$DebugRebarHandle
    mov D$DebugRebarHandle 0
    call 'USER32.DestroyMenu' D$Debug_ToolbarMenu
    mov D$Debug_ToolbarMenu 0
    mov D$DebugDialog.RebarHeight 0
ret
____________________________________________________________________________________________

; User rightclicked on the dialog. Test if it is in the rebar and show context menu.
; Position is given in screen coordinates.

Proc DebugDialog_RebarHitTest:
    Arguments @X @Y
    Structure @RBHitTest 16, @pt.x 0,  @pt.y 4,  @flags 8,  @iBand 12

  ; Hittest expects client coordinates
    move D@pt.x D@X, D@pt.y D@Y
    call 'USER32.ScreenToClient' D$Debug_RebarHandle, D@RBHitTest
    call 'User32.SendMessageA' D$Debug_RebarHandle, &RB_HITTEST, 0, D@RBHitTest

  ; Show context menu
    If D@iBand <> 0-1
        call 'USER32.TrackPopupMenu' D$Debug_ToolbarMenu, 0, D@X, D@Y, 0, D$DebugDialogHandle, 0
    EndIf
EndP
____________________________________________________________________________________________

; Show / Hide rebar-band containing a toolbar.
; Command references the menu item clicked.

Proc DebugDialog_ToggleToolbar:
    Arguments @Command

  ; store band index in ebx
    If D@Command = DEBUGDLG_TB_SHOW_CMDS
        mov ebx 1
    ElseIf D@Command = DEBUGDLG_TB_SHOW_FLAGS
        mov ebx 2
    ElseIf D@Command = DEBUGDLG_TB_SHOW_FPU
        mov ebx 3
    EndIf

  ; get check state of menu item, invert, toggle band and set inverted check state
    call 'User32.GetMenuState' D$Debug_ToolbarMenu, D@Command, &MF_BYCOMMAND
    push eax
        xor edx edx | test eax &MF_CHECKED | setz dl
        call 'User32.SendMessageA' D$Debug_RebarHandle, &RB_SHOWBAND, ebx, edx
    pop eax
    xor eax &MF_CHECKED | and eax &MF_CHECKED
    call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, D@Command, eax
EndP
____________________________________________________________________________________________

Proc DebugDialog_ToggleToolbarText:
    Structure @TBButtonInfo 32, @Size 0, @Mask 4, @Text 24

    mov D@Size 32, D@Mask &TBIF_TEXT

  ; get check state of menu item, invert, toggle band and set inverted check state
    call 'User32.GetMenuState' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOWTEXT, &MF_BYCOMMAND
    push eax
        xor edx edx | test eax &MF_CHECKED | setz dl
        mov B$DebugShowTBText dl
        call DebugDialog_CreateCommandTB
;;
      ; TODO show text
        mov ebx 0
        While ebx < 6
            mov D@Text 0
            call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_SETBUTTONINFOA, ebx, D@TBButtonInfo
            If eax = 0
                call ReportWinError {'SetButtonInfo' 0}
            EndIf
            inc ebx
        EndWhile
;;
    pop eax
    xor eax &MF_CHECKED | and eax &MF_CHECKED
    call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOWTEXT, eax
    call 'User32.SendMessageA' D$Debug_ToolbarHandle, &TB_AUTOSIZE, 0, 0

EndP
____________________________________________________________________________________________

[DebugDialog.RebarHeight: ?]

Proc DebugDialog_RebarNotify:
    Arguments @Message

  ; Reposition all other child windows
    .If D@Message = &RBN_HEIGHTCHANGE
        call 'User32.SendMessageA' D$DebugRebarHandle, &RB_GETBARHEIGHT, 0, 0
        mov edx D$DebugDialog.RebarHeight
        mov D$DebugDialog.RebarHeight eax
        sub eax edx | mov ebx eax

        call AdjustControlPos D$DebugDialogHandle, DEBUGDLG_REGISTER_TAB, 0, ebx
        call AdjustControlPos D$DebugDialogHandle, DEBUGDLG_FORMAT_COMBO, 0, ebx
        call AdjustControlPos D$DebugDialogHandle, DEBUGDLG_REG_LIST, 0, ebx
        call AdjustControlPos D$DebugDialogHandle, DEBUGDLG_DATA_TAB, 0, ebx
        call AdjustControlPos D$DebugDialogHandle, 3, 0, ebx
        mov esi 71
        While esi < 79
            call AdjustControlPos D$DebugDialogHandle, esi, 0, ebx
            inc esi
        EndWhile
        neg ebx
        call AdjustControlSize D$DebugDialogHandle, DEBUGDLG_DATA_TAB, 0, ebx
        call DebugDialog_AdjustDataTabSize D$DebugDialogHandle

        call 'USER32.InvalidateRect' D$DebugDialogHandle, &NULL, &TRUE
    .EndIf
EndP
____________________________________________________________________________________________

Proc DebugDialog_SaveToolbarSettings:
    Arguments @FileHandle
    Structure @Tbar 36, @Id 0, @Size 4, @Flags 8, @X1 12, @X2 16, @X3 20, @Style1 24, @Style2 28, @Style3 32

    mov D@Id 'TBar', D@Size 28

    mov al B$DebugShowTBText | mov B@Flags al

    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_GETBANDINFO, 1, DebugRebarBand
    move D@Style1 D$DebugRebarBand@fStyle
    move D@X1 D$DebugRebarBand@cx

    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_GETBANDINFO, 2, DebugRebarBand
    move D@Style2 D$DebugRebarBand@fStyle
    move D@X2 D$DebugRebarBand@cx

    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_GETBANDINFO, 3, DebugRebarBand
    move D@Style3 D$DebugRebarBand@fStyle
    move D@X3 D$DebugRebarBand@cx

    call 'KERNEL32.WriteFile' D@FileHandle, D@Tbar, 36, BytesTransfered, 0
EndP
____________________________________________________________________________________________

Proc DebugDialog_LoadToolbarSettings:
    Arguments @FileHandle @Size
    Structure @Tbar 28, @Flags 0, @X1 4, @X2 8, @X3 12, @Style1 16, @Style2 20, @Style3 24

    mov eax 0
    On D@Size <> 28, ExitP

    call 'KERNEL32.ReadFile' D@FileHandle, D@Tbar, D@Size, BytesTransfered, 0

    On B@Flags <> 0,
        call DebugDialog_ToggleToolbarText

    mov ebx D$DebugRebarBand@fMask
    mov D$DebugRebarBand@fMask &RBBIM_STYLE+&RBBIM_SIZE

    move D$DebugRebarBand@fStyle D@Style1
    move D$DebugRebarBand@cx D@X1
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_SETBANDINFO, 1, DebugRebarBand

    move D$DebugRebarBand@fStyle D@Style2
    move D$DebugRebarBand@cx D@X2
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_SETBANDINFO, 2, DebugRebarBand

    move D$DebugRebarBand@fStyle D@Style3
    move D$DebugRebarBand@cx D@X3
    call 'User32.SendMessageA' D$DebugRebarHandle, &RB_SETBANDINFO, 3, DebugRebarBand

    xor eax eax | test D@Style1 &RBBS_HIDDEN | setz al | shl al 3
    call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOW_CMDS, eax
    xor eax eax | test D@Style2 &RBBS_HIDDEN | setz al | shl al 3
    call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOW_FLAGS, eax
    xor eax eax | test D@Style3 &RBBS_HIDDEN | setz al | shl al 3
    call 'USER32.CheckMenuItem' D$Debug_ToolbarMenu, DEBUGDLG_TB_SHOW_FPU, eax

    mov D$DebugRebarBand@fMask ebx

    mov eax 1
EndP
____________________________________________________________________________________________

____________________________________________________________________________________________
____________________________________________________________________________________________

[DEBUGLOGFONT:
 DEBUGLOGFONT.lfHeight: D$ 0_FFFF_FFF5
 DEBUGLOGFONT.lfWidth: D$ 0
 DEBUGLOGFONT.lfEscapement: D$ 0
 DEBUGLOGFONT.lfOrientation: D$ 0
 DEBUGLOGFONT.lfWeight: D$ 0190
 DEBUGLOGFONT.lfItalic: B$ 0
 DEBUGLOGFONT.lfUnderline: B$ 0
 DEBUGLOGFONT.lfStrikeOut: B$ 0
 DEBUGLOGFONT.lfCharSet: B$ 0
 DEBUGLOGFONT.lfOutPrecision: B$ 03
 DEBUGLOGFONT.lfClipPrecision: B$ 02
 DEBUGLOGFONT.lfQuality: B$ 01
 DEBUGLOGFONT.lfPitchAndFamily: B$ 031]
[DEBUGLOGFONT.lfFaceName: B$ 'Courier New' 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]

[DEBUGCHOOSEFONT:
 DEBUGCHOOSEFONT.lStructSize: D$ len
 DEBUGCHOOSEFONT.hwndOwner: D$ 0
 DEBUGCHOOSEFONT.hDC: D$ 0
 DEBUGCHOOSEFONT.lpLogFont: D$ DEBUGLOGFONT
 DEBUGCHOOSEFONT.iPointSize: D$ 0
 DEBUGCHOOSEFONT.Flags: D$ &CF_SCREENFONTS__&CF_FIXEDPITCHONLY
 DEBUGCHOOSEFONT.rgbColors: D$ 0
 DEBUGCHOOSEFONT.lCustData: D$ 0
 DEBUGCHOOSEFONT.lpfnHook: D$ 0
 DEBUGCHOOSEFONT.lpTemplateName: D$ 0
 DEBUGCHOOSEFONT.hInstance: D$ 0
 DEBUGCHOOSEFONT.lpszStyle: D$ 0
 DEBUGCHOOSEFONT.nFontType: W$ 0
 DEBUGCHOOSEFONT.Alignment: W$ 0
 DEBUGCHOOSEFONT.nSizeMin: D$ 0
 DEBUGCHOOSEFONT.nSizeMax: D$ 0]

[DebugFontHandle: ?]

Proc DebugDialog_SetFont:
    Arguments @Handle

        On D$DebugFontHandle <> 0,
            call 'GDI32.DeleteObject' D$DebugFontHandle
        call 'GDI32.CreateFontIndirectA' DEBUGLOGFONT
        mov D$DebugFontHandle eax
        call MeasureFont
        call 'User32.SendMessageA' D@Handle, &WM_SETFONT, D$DebugFontHandle, &TRUE
EndP
____________________________________________________________________________________________

; Get character extents for the owner-drawn lists.
; Create a temporary device context, set the font & measure string sizes.
; (only deals with fixed sized fonts)

[DebugFontCharWidth: ?]

Proc MeasureFont:
    Local @DC

    call 'GDI32.CreateDCA' {'DISPLAY' 0}, 0, 0, 0
    mov D@DC eax

    call 'GDI32.SelectObject' D@DC, D$DebugFontHandle
    call 'GDI32.GetTextExtentPoint32A' D@DC, {'M' 0}, 1, Point

    move D$DebugFontCharWidth D$PointX

    call 'GDI32.DeleteDC' D@DC
EndP
____________________________________________________________________________________________

MeasureStringWidth:
    cmp B$UnicodeStrings 1 | je L0>
    jmp MeasureStringWidthA
L0: jmp MeasureStringWidthW

; General purpose string measurement for owner-drawn controls

Proc MeasureStringWidthA:
    Arguments @String, @Font
    Local @DC
    Uses edi

    call 'GDI32.CreateDCA' {'DISPLAY' 0}, 0, 0, 0
    mov D@DC eax

    call 'GDI32.SelectObject' D@DC, D@Font

    mov edi D@String, ecx 0-1, al 0
    repne scasb
    mov eax 0-2 | sub eax ecx

    call 'GDI32.GetTextExtentPoint32A' D@DC, D@String, eax, Point
    call 'GDI32.DeleteDC' D@DC

    mov eax D$PointX
EndP
____________________________________________________________________________________________

Proc MeasureStringWidthW:
    Arguments @String, @Font
    Local @DC
    Uses edi

    call 'GDI32.CreateDCA' {'DISPLAY' 0}, 0, 0, 0
    mov D@DC eax

    call 'GDI32.SelectObject' D@DC, D@Font

    mov edi D@String, ecx 0-1, ax 0
    repne scasw
    mov eax 0-2 | sub eax ecx

    call 'GDI32.GetTextExtentPoint32W' D@DC, D@String, eax, Point
    call 'GDI32.DeleteDC' D@DC

    mov eax D$PointX
EndP
____________________________________________________________________________________________

; Present font dialog

Proc DebugDialog_ChangeFont:
    Arguments @Handle

        move D$DEBUGCHOOSEFONT.hwndOwner D@Handle
        call 'COMDLG32.ChooseFontA' DEBUGCHOOSEFONT    ; user sets the font:
        If eax > 0
            call DebugDialog_SetFont D@Handle
        End_If
EndP
____________________________________________________________________________________________

; Compare the register contents (general purpose) for changed values since last tag.
; Mark changed registers in the bit mask (e.g. bit0=1 means: EAX has changed)

[OldGPR_Values: ? #8 GPR_Modified_Mask: ? GPR_FirstTime: ?]

Proc TagGPRModified:
    Uses esi edi

    mov ecx 0, edx 0
    mov esi GPRRegMap, edi OldGPR_Values
    Do
        lodsd | mov eax D$eax
        cmp eax D$edi | je L0>
            bts edx ecx ; tag as changed
L0:     stosd
        inc ecx
    Loop_Until ecx = 8

    If D$GPR_FirstTime = 1
        mov edx 0
        mov D$GPR_FirstTime 0
    EndIf

    mov D$GPR_Modified_Mask edx
EndP
____________________________________________________________________________________________

[DebugConfig: 'debug.cfg' 0]

; Load debugger configuration from file (in RosAsmFiles folder) if available

Proc LoadDebugConfig:
    Local @File, @Value, @Size, @Id
    Uses edi, esi

    call GetRosAsmFilesPath

    mov edi RosAsmFilesPath, eax 0, ecx 0-1
    repne scasb | dec edi
    mov esi DebugConfig, ecx 13
    rep movsb

    call 'Kernel32.CreateFileA' RosAsmFilesPath, &GENERIC_READ, &FILE_SHARE_READ, 0,
        &OPEN_EXISTING, &FILE_ATTRIBUTE_NORMAL, 0

    mov B$edi 0

    On eax = &INVALID_HANDLE_VALUE, ExitP
    mov D@File eax

L0: lea eax D@Id
    call 'KERNEL32.ReadFile' D@File, eax, 8, BytesTransfered, 0
    ..If D$BytesTransfered = 8
        .If D@Id = 'RegX'
          ; Show seg&debug regs (0=No [default]; 1=Show)
            lea eax D@Value
            call 'KERNEL32.ReadFile' D@File, eax, 4, BytesTransfered, 0
            On D@Value = 1,
                call 'User32.SendMessageA' D$DebugDialogHandle, &WM_COMMAND, M02_Show_All, 0
        .ElseIf D@Id = 'Step'
          ; Stepping mode (0=instruction level [default]; 1=source level)
            lea eax D@Value
            call 'KERNEL32.ReadFile' D@File, eax, 4, BytesTransfered, 0
            On D@Value = 1,
                call 'User32.SendMessageA' D$DebugDialogHandle, &WM_COMMAND, M02_Source_Level, 0
        .ElseIf D@Id = 'Font'
            call 'KERNEL32.ReadFile' D@File, DebugLogFont, D@Size, BytesTransfered, 0
            call DebugDialog_SetFont D$DebugDialogHandle
        .ElseIf D@Id = 'Rect'
            call 'KERNEL32.ReadFile' D@File, DebugRect, D@Size, BytesTransfered, 0
            mov eax D$DebugRect@right | sub eax D$DebugRect@left
            mov edx D$DebugRect@bottom | sub edx D$DebugRect@top
            call 'USER32.MoveWindow' D$DebugDialogHandle, D$DebugRect@left, D$DebugRect@top, eax, edx, &TRUE
        .ElseIf D@Id = 'TBar'
            call DebugDialog_LoadToolbarSettings D@File, D@Size
            cmp eax 0 | je L9>
        .ElseIf D@Id = 'LaLa'
            call DataView_LoadSettings D@File, D@Size
        .Else
          ; Unknown chunk, skip it
L9:         call 'KERNEL32.SetFilePointer' D@File, D@Size, 0, &FILE_CURRENT
        .EndIf
        jmp L0<<
    ..EndIf

    call 'KERNEL32.CloseHandle' D@File
EndP
____________________________________________________________________________________________

; Write debugger configuration to a file

Proc SaveDebugConfig:
    Local @File, @Value, @Size, @Id
    Uses edi, esi

    call GetRosAsmFilesPath

    mov edi RosAsmFilesPath, eax 0, ecx 0-1
    repne scasb | dec edi
    mov esi DebugConfig, ecx 13
    rep movsb

    call 'Kernel32.CreateFileA' RosAsmFilesPath, &GENERIC_WRITE, &FILE_SHARE_READ, 0,
        &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

    mov B$edi 0

    On eax = &INVALID_HANDLE_VALUE, ExitP
    mov D@File eax

  ; Show segment & debug regs
    call 'User32.GetMenuState' D$SettingsMenu, M02_Show_All, &MF_BYCOMMAND
    and eax &MF_CHECKED
    If eax <> 0
        mov D@Value 1
    Else
        mov D@Value 0
    EndIf
    mov D@Id 'RegX'
    mov D@Size 4
    lea eax D@Id
    call 'KERNEL32.WriteFile' D@File, eax, 12, BytesTransfered, 0

  ; Stepping Mode
    mov D@Id 'Step'
    move D@Value D$Stepping
    lea eax D@Id
    call 'KERNEL32.WriteFile' D@File, eax, 12, BytesTransfered, 0

  ; Font
    mov D@Id 'Font'
    mov D@Size 60
    lea eax D@Id
    call 'KERNEL32.WriteFile' D@File, eax, 8, BytesTransfered, 0
    call 'KERNEL32.WriteFile' D@File, DebugLogFont, D@Size, BytesTransfered, 0

  ; Position
    mov D@Id 'Rect'
    mov D@Size 16
    call 'USER32.GetWindowRect' D$DebugDialogHandle, DebugRect
    lea eax D@Id
    call 'KERNEL32.WriteFile' D@File, eax, 8, BytesTransfered, 0
    call 'KERNEL32.WriteFile' D@File, DebugRect, D@Size, BytesTransfered, 0

    call DebugDialog_SaveToolbarSettings D@File
    call DataView_SaveSettings D@File

    call 'KERNEL32.CloseHandle' D@File
EndP
____________________________________________________________________________________________

[CurrentStackPointer: ?]

Proc DebugDialog_ShowCaller:
    If D$CurrentStackPointer = 0
        move D$CurrentStackPointer D$C.regEsp
    EndIf

    call ScanStackForCodePointer D$DebugBaseOfCode, D$DebugCodeSize, D$CurrentStackPointer
    If eax <> 0
        add edx 4
        add D$CurrentStackPointer edx

        dec eax
        call SourceDebugPos eax
    ;Else
     ;   mov eax eax
    End_If
EndP
____________________________________________________________________________________________

; Process WM_DEBUGEVENT message.

Proc DebugDialog_OnDebugEvent:
    Arguments @Handle
    Local @Inside

    ; Find changed registers
    call TagGPRModified

    ; here we see if we are inside the same file/module being debugged to we adjust te source code pos on it.
    call EnableSourceDebugPos D$C.regEip, D$ModuleList, D$NumModules
    If eax = &TRUE
         ; address found on Debugger_OnException EncounterException EncounterWatchPoint EncounterBreakpoint
         ; When we reach here D$SourcePosCodeAddress is the same as D$C.regEip
        call SourceDebugPos D$SourcePosCodeAddress
    End_If

    call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D$C.regEip | mov D@Inside eax

      ; Copy exception description
        mov edi DebugCaption
        .If D$DebugEventType = DET_BP
            mov D$edi 'BP  ' | add edi 3
        .ElseIf D$DebugEventType = DET_STEP
            mov D$edi 'STEP', B$edi+4 ' ' | add edi 5
        .ElseIf D$DebugEventType = DET_WP
            mov D$edi 'WP  ' | add edi 3
        .ElseIf D$DebugEventType = DET_EXCEPTION
            mov D$edi 'EXCE', D$edi+4 'PTIO', W$edi+8 'N ' | add edi 10
        .EndIf

        ;mov esi D$BreakTitle, edi DebugCaption
        ;While B$esi <> 0 | movsb | EndWhile

        ; Under 95 we cannot access the process memory after an exception has occurred
        ; Step-Over and InstructionDecode wouldn't work for that reason. (really?)
        ...If D$ExceptionFlags = 0
            call NextInstructionDecode
            ; For source level stepping, compare the last source pos with the current
            ; if it hasn't changed, continue to step into/over.
            ; UPDATE: Only when stepping inside the process code
            mov eax D$SourcePos
            ..If eax = D$LastSourcePos
                .If D@Inside = &TRUE
                    If B$Stepping = 1
                        call DebugDialog_ContinueDebuggee
                        ExitP
                    EndIf
                .EndIf
            ..Else
                mov D$LastSourcePos eax
            ..EndIf
            ; Show next instruction in caption, e.g. "BREAKPOINT - [push eax]"
            ;mov eax ' - [' | stosd
            mov al '[' | stosb
            mov esi D$NextInstructionPtr
            While B$esi <> 0 | movsb | EndWhile
            mov al ']' | stosb
        ...End_If

        On D$ExceptionFlags = 0,
            call DebugDialog_EnableContinueMenu &TRUE
;;
        test D$ExceptionFlags E_MUSTEXIT | jnz L0>
            call DebugDialog_EnableContinueMenu &TRUE
L0:
        test D$ExceptionFlags E_OUTSIDE | jnz L0>
L0:     
;;

      ; Show current module name
      C_call FormatStr edi, {B$ " %s", 0}, D$CurrentModule
      add edi eax | mov B$edi 0
;;
        If D$CurrentModule <> 0
            push esi
                mov esi D$CurrentModule
                mov B$edi ' ' | inc edi
                While B$esi <> 0 | movsb | EndWhile
            pop esi
        EndIf
;;
      ; Show caption
;        mov B$edi 0
        call 'User32.SendMessageA' D@Handle, &WM_SETTEXT, 0, DebugCaption

        ; Refresh register content output & flags
        call DebugDialog_OnChangeRegisterTab D@Handle
        call DebugDialog_UpdateFlags D@Handle
        call DebugDialog_UpdateRegisterButtons D@Handle
        call 'User32.IsIconic' D$hwnd
        If eax = &TRUE
            call 'USER32.ShowWindow' D$hwnd, &SW_RESTORE
        EndIf
        call 'USER32.ShowWindow' D@Handle, &SW_RESTORE
        call 'USER32.SetForegroundWindow' D$hwnd
        call 'USER32.SetForegroundWindow' D@Handle

      ; Refresh the data dialogs - they shall reload the displayed data from the debuggee
        call 'User32.SendMessageA' D$CurrentDataPageHandle, WM_REFRESH_CONTENT, 0, 0

        On D$DebugEventType = DET_WP, call 'User32.SendMessageA' D$DataViewHandle, WM_SELECT_SYMBOL, D$WatchedAddress, 0

        On D$ExceptionFlags <> 0, call ShowExceptionInfo

        mov D$CurrentStackPointer 0

        ;test D$ExceptionFlags E_OUTSIDE | jz L0>
        ;    call 'USER32.MessageBoxA' D$hwnd, ErrorOutside, ErrorOutsideTitle, &MB_ICONEXCLAMATION
L0:     mov eax 0
EndP
____________________________________________________________________________________________

; check if we are inside the debugging process (exe or a dll) to adjust th source code pos on it.
; This is a variation of GetModuleNameFromAddress
Proc EnableSourceDebugPos:
    Arguments @AddresstoFind, @pModuleList, @NumModules
    Uses edx, ebx, ecx, edi

    mov edi D@AddresstoFind
    mov edx D@pModuleList, ecx D@NumModules
    .While ecx > 0 ; bugfix V2.0b
        mov ebx D$edx+ME_Size | add ebx D$edx+ME_Base
        ..If_And edi >= D$edx+ME_Base, edi <= ebx
            ; Ok, we found a matching address. Let´ see if it correpond to our file/dll.
            ; It is necessary because the process can be rebasing addresses of other dlls that
            ; may match with the one we are looking for
            .If D$DebugProcessData.Type = DEBUG_DLL ; Are we debugging a dll ?
                call StrCmp D$edx+ME_Name, D$DebugProcessData.DLLName ; See if we are debugging the dll that we opened.
                If eax = 0 ; If strings match, we are, in fact debugging our dll.
                    mov eax &TRUE
                    ExitP
                End_If
            .Else
                call StrCmp D$edx+ME_Name, D$DebugProcessData.HostName ; See if we are debugging the executable that we opened.
                If eax = 0 ; If strings match, we are, in fact debugging our dll.
                    mov eax &TRUE
                    ExitP
                End_If
            .End_If
        ..End_If
        add edx SizeOf_ModuleEntry
        dec ecx
    .End_While
    mov eax 0
;;
    ; Find position in source (only if we are inside our module. analyze eipvalue to find out)
    call GetModuleNameFromAddress D@AddresstoFind, D@pModuleList, D@NumModules
    .If D$DebugProcessData.Type = DEBUG_DLL ; Are we debugging a dll ?
        call StrCmp eax, D$DebugProcessData.DLLName ; See if we are debugging the dll that we opened.
        If eax = 0 ; If strings match, we are, in fact debugging our dll.
            mov eax &TRUE
        Else
            mov eax &FALSE
        End_If
    .Else
        call StrCmp eax, D$DebugProcessData.HostName ; See if we are debugging the executable that we opened.
        If eax = 0 ; If strings match, we are, in fact debugging our dll.
            mov eax &TRUE
        Else
            mov eax &FALSE
        End_If
    .End_If
;;
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Kill Debugger

Proc DebugDialog_KillDebugger:
  ; Tried to kill debug-dialog while debugger is still running
    mov D$TerminateDebuggee &TRUE, D$DialogKillsDebugger &TRUE

    .If D$IsDebugEvent = 1
      ; If the debug-thread waits for an user input event, simulate that the user pressed
      ; "Terminate Debuggee". If we don't do that, the debugger will wait forever--
        call DebugDialog_ContinueDebuggee
    .Else
      ; Wait for the debug-thread to terminate. After a few seconds the debuggee and the
      ; debugger thread is terminated if the debugger thread does not exit voluntarily.
        call 'KERNEL32.WaitForSingleObject' D$DebugThreadHandle, 5000
        If eax = &WAIT_TIMEOUT
            call 'User32.MessageBoxA' D$hwnd, DebugThreadHangs, CriticalError, &MB_ICONEXCLAMATION
            call CloseProcess
            call 'KERNEL32.TerminateThread' D$DebugThreadHandle, 0
        EndIf
        mov D$DebugThreadHandle &NULL
    .EndIf
EndP
____________________________________________________________________________________________

; Process WM_CREATE message. Create child windows and load configuration.

Proc DebugDialog_OnCreate:
    Arguments @Handle

        move D$DebugDialogHandle D@Handle
        ;call 'USER32.GetMenu' D@Handle | mov D$DebugMenuHandle eax

      ; Hide tree
        On D$ShowTreeHandle <> 0, call 'USER32.ShowWindow' D$ShowTreeHandle &SW_HIDE

      ; Init the flags
        mov D$GPR_FirstTime 1, D$GPR_Modified_Mask 0
        mov D$TerminateDebuggee &FALSE, D$DialogKillsDebugger &FALSE
        mov D$HoldOnBreakpoints &TRUE, D$PauseThreads &FALSE

        call 'User32.GetClientRect' D@Handle, DebugRect
        move W$DebugDialog.Width W$DebugRect@width
        move W$DebugDialog.Height W$DebugRect@height

      ; Place dialog in upper right corner of the source editor.
      ; The position is overwritten when the configuration file is loaded so it
      ; only has any effect when no config file is available.
        call 'User32.GetClientRect' D$Hwnd, DebugRect
        mov esi D$DebugRect@width
        mov edi D$DebugRect@height
        call 'User32.GetWindowRect' D@Handle, DebugRect
        mov eax D$DebugRect@left
        sub D$DebugRect@right eax ; width
        sub esi D$DebugRect@width
        mov D$DebugRect@left esi ; xpos
        mov eax D$DebugRect@top
        sub D$DebugRect@bottom eax ; height
        If edi > D$DebugRect@bottom
            mov D$DebugRect@bottom edi
        EndIf
        mov D$DebugRect@top 0
        call 'USER32.ClientToScreen' D$Hwnd, DebugRect
        call 'USER32.MoveWindow' D@Handle, D$DebugRect@left, D$DebugRect@top,
                                 D$DebugRect@right, D$DebugRect@bottom, &TRUE

      ; Key mapping
        call 'User32.CreateAcceleratorTableA' DbgAccels, DbgAccelsNum
        mov D$DbgAccelHandle eax

      ; Create default font
        mov eax D$NationalFontHandle
        If eax <> 0
            mov D$DialogFont_handle eax
        Else
            call 'GDI32.CreateFontA' 8 4 0 0 400 0 0 0 1,   ;  DEFAULT_CHARSET 1  OEM_CHARSET 255
                                     0 0 0 0 Helv
            mov D$DialogFont_handle eax
        EndIf

        ;SendMessage eax, &WM_SETFONT, D$DialogFont_handle, 1
        ;SendMessage eax, &WM_SETFONT, D@Handle, 1

      ; Create the image list
        call DebugDialog_CreateImageList

      ; Create the child windows / menu, toolbar, tabs
        call DebugDialog_CreateRegisterTabs D@Handle
        call DebugDialog_CreateDataTab D@Handle
        call DebugDialog_CreateToolbar
        call DebugDialog_InitDbgMenu

      ; Create the monospace font
        call DebugDialog_SetFont D@Handle

        call 'User32.SetWindowTextA' D@Handle, {'Running ...' 0}

      ; Create the mouse hint form and timer
        call InitMouseOverDataHints

      ; Finally load configuration from file if available. All windows must be created
      ; at this point.
        call LoadDebugConfig

        mov eax 0 ;&TRUE
EndP
____________________________________________________________________________________________

; Process WM_CLOSE message. Save configuration & send destroy message.

Proc DebugDialog_OnClose:
    Arguments @Handle

      ; Save configuration
        call SaveDebugConfig

      ; Terminate debugger if it is still running.
        mov D$DebuggerReady &FALSE
        If D$IsDebugging = &TRUE
            call DebugDialog_KillDebugger
        End_If

      ; Kill timer and destroy mouse hint window
        call DeleteMouseOverDataHints

      ; Destroy dialog
        call 'User32.DestroyWindow' D@Handle
        mov D$DebugDialogHandle 0

      ; Restore RosAsm windows
        On D$ShowTreeHandle <> 0, call 'USER32.ShowWindow' D$ShowTreeHandle, &SW_SHOW
        call 'User32.IsIconic' D$hwnd
        If eax = &TRUE
            call 'USER32.ShowWindow' D$hwnd, &SW_RESTORE
        EndIf
        call 'USER32.SetForegroundWindow' D$hwnd
        mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_DESTROY message. Free resources, destroy child windows.

Proc DebugDialog_OnDestroy:
    Arguments @Handle

      ; Destroy child windows
        call 'USER32.DestroyWindow' D$DataViewHandle
        call 'USER32.DestroyWindow' D$MemoryInspectorHandle
        call 'USER32.DestroyWindow' D$CallStackFormHandle
        call 'USER32.DestroyWindow' D$LogForm_handle
        call 'USER32.DestroyWindow' D$ASForm_handle
        mov D$CurrentDataPageHandle 0

        call DebugDialog_DestroyToolbar

      ; Free images
        call DebugDialog_DestroyImageList

      ; Destroy key map
        call 'USER32.DestroyAcceleratorTable' D$DbgAccelHandle
        mov D$DbgAccelHandle 0

      ; Delete fonts
        If D$DebugFontHandle <> 0
            call 'GDI32.DeleteObject' D$DebugFontHandle
            mov D$DebugFontHandle 0
        EndIf
        If D$DialogFontHandle <> 0
            On D$NationalFontHandle = 0,
                call 'GDI32.DeleteObject' D$DialogFontHandle
            mov D$DialogFontHandle 0
        EndIf

        mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_COMMAND message. Menu, accelerator (key-press) and button notifications.

Proc DebugDialog_OnCommand:
    Arguments @Handle, @wParam, @lParam

        movzx eax W@wParam
        movzx edx W@wParam+2

        .If eax = M02_Hold_on_BP
            call 'User32.GetMenuState' D$SettingsMenu, M02_Hold_on_BP, &MF_BYCOMMAND
            xor B$HoldOnBreakpoints 1 ; bool toggle
            xor eax &MF_CHECKED | and eax &MF_CHECKED
            call 'USER32.CheckMenuItem' D$SettingsMenu, M02_Hold_on_BP, eax

        .Else_if eax = M02_Show_All
            call 'User32.GetMenuState' D$SettingsMenu, M02_Show_All, &MF_BYCOMMAND
            push eax
                xor edx edx | test eax &MF_CHECKED | setz dl
                call DebugDialog_ShowAdvancedTabs D@Handle edx
            pop eax
            xor eax &MF_CHECKED | and eax &MF_CHECKED
            call 'USER32.CheckMenuItem' D$SettingsMenu, M02_Show_All, eax

        .Else_if eax = M02_Run
            mov B$UserWants CONTINUE_RUN
            call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Step_Over
            mov B$UserWants CONTINUE_STEPOVER
            call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Step_Into
            mov B$UserWants CONTINUE_STEP
            call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Return_to_Caller
            mov B$UserWants CONTINUE_RETURNTOCALLER
            call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Terminate
            mov B$TerminateDebuggee &TRUE
            call DebugDialog_ContinueDebuggee

        .Else_if eax = M02_Pause
            mov B$PauseThreads &TRUE

        .Else_If eax = M02_Show_code_at
            call CreateNewForm_CodeAddressForm

        .Else_if eax = M02_Inst_Level
            call 'USER32.CheckMenuRadioItem' D$SettingsMenu, M02_Inst_Level, M02_Source_Level,
                                             M02_Inst_Level, &MF_BYCOMMAND
            mov B$Stepping 0

        .Else_if eax = M02_Source_Level
            call 'USER32.CheckMenuRadioItem' D$SettingsMenu, M02_Inst_Level, M02_Source_Level,
                                             M02_Source_Level, &MF_BYCOMMAND
            mov B$Stepping 1

        .Else_if eax = M02_Font
            call DebugDialog_ChangeFont D@Handle

        .Else_if eax = M02_CPU_Info
            VirtualAlloc DebugTextBuffer 4096
            mov edi D$DebugTextBuffer
            call DebugDialog_GetCPUInfo
            call 'User32.MessageBoxA' D@Handle, D$DebugTextBuffer, {'CPU Information' 0}, &MB_OK__&MB_ICONINFORMATION
            VirtualFree D$DebugTextBuffer

        .Else_if eax = M02_FPU_Status
            VirtualAlloc DebugTextBuffer 4096
            mov edi D$DebugTextBuffer
            call DebugDialog_GetFPUStatus
            call 'User32.MessageBoxA' D@Handle, D$DebugTextBuffer, {'FPU Status' 0}, &MB_OK__&MB_ICONINFORMATION
            VirtualFree D$DebugTextBuffer

        .Else_if eax = M02_Help
            call Help D$hwnd, B_U_AsmName, {'Debugger' 0}, ContextHlpMessage

        .Else_if eax = M02_About
            call 'User32.MessageBoxA' D@Handle, AboutDebugger, DebuggerVersion, &MB_OK__&MB_ICONINFORMATION

        .Else_If eax = DEBUGDLG_FORMAT_COMBO ; Representation Combo
            If edx = &CBN_SELCHANGE
                call 'User32.SendMessageA' D@lParam, &CB_GETCURSEL, 0, 0
                On eax <> &CB_ERR, call DebugDialog_OnFormatChange D@Handle eax
            End_If

        .Else_if eax = DEBUGDLG_TB_CONFIGURE
            ; TODO
        .Else_If eax = DEBUGDLG_TB_SHOW_CMDS
            call DebugDialog_ToggleToolbar eax
        .Else_If eax = DEBUGDLG_TB_SHOW_FLAGS
            call DebugDialog_ToggleToolbar eax
        .Else_If eax = DEBUGDLG_TB_SHOW_FPU
            call DebugDialog_ToggleToolbar eax
        .Else_If eax = DEBUGDLG_TB_SHOW_TEXT
            call DebugDialog_ToggleToolbarText

        .Else_if eax >= DEBUGDLG_FIRSTREG_BUTTON
            If eax <= DEBUGDLG_LASTREG_BUTTON
                sub eax DEBUGDLG_FIRSTREG_BUTTON
                mov eax D$GPRRegMap+eax*4
                mov ecx D$eax
                and ecx 0_FFFF_F000
                call 'User32.SendMessageA' D$MemoryInspectorHandle, WM_SET_PAGE, ecx, D$eax
            End_if
        .End_If
        mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_NOTIFY message. Handles tab selection changes and rebar height change.

Proc DebugDialog_OnNotify:
    Arguments @Handle, @Notification

        mov ebx D@Notification
        mov edx D$ebx+4
        mov eax D$ebx+8
        ..If edx = DEBUGDLG_REGISTER_TAB
            If eax = &TCN_SELCHANGE
                call 'User32.SendMessageA' D$ebx, &TCM_GETCURSEL, 0, 0
                mov D$TabItem@imask &TCIF_PARAM
                call 'User32.SendMessageA' D$ebx, &TCM_GETITEM, eax, TabItem
                call DebugDialog_OnChangeRegisterTab D@Handle
            End_If
            mov eax 0
        ..ElseIf edx = DEBUGDLG_DATA_TAB
            .If eax = &TCN_SELCHANGE
                call 'User32.SendMessageA' D$ebx, &TCM_GETCURSEL, 0, 0
                If eax = 0
                    mov edi D$DataViewHandle
                ElseIf eax = 1
                    mov edi D$MemoryInspectorHandle
                ElseIf eax = 2
                    mov edi D$CallStackFormHandle
                ElseIf eax = 3
                    mov edi D$LogForm_handle
                ElseIf eax = 4
                    mov edi D$ASForm_handle
                EndIf
                call 'USER32.ShowWindow' D$CurrentDataPageHandle, &SW_HIDE
                call 'USER32.ShowWindow' edi, &SW_SHOW
                mov D$CurrentDataPageHandle edi
                call DebugDialog_AdjustDataTabSize D@Handle
            .End_If
            mov eax 0 ; mandatory for TCN_SELCHANGING !!!
        ..ElseIf edx = DEBUGDLG_REBAR
            call DebugDialog_RebarNotify eax
            mov eax 0
        ..ElseIf edx = DEBUGDLG_MENUBAR
            call DebugDialog_MenubarNotify ebx eax D$ebx+12
        ..Else
            mov eax 0
        ..EndIf

EndP
____________________________________________________________________________________________

; Process WM_SETFONT message. Propagate message to affected child windows.

Proc DebugDialog_OnSetFont:
    Arguments @Handle @hFont, @Redraw

        call 'User32.SendMessageA' D$RegListHandle, &WM_SETFONT, D@hFont, D@Redraw
        call 'User32.SendMessageA' D$DataViewHandle, &WM_SETFONT, D@hFont, D@Redraw
        call 'User32.SendMessageA' D$MemoryInspectorHandle, &WM_SETFONT, D@hFont, D@Redraw
        call 'User32.SendMessageA' D$CallStackFormHandle, &WM_SETFONT, D@hFont, D@Redraw
        call 'User32.SendMessageA' D$LogFormHandle, &WM_SETFONT, D@hFont, D@Redraw
        call 'User32.SendMessageA' D$MouseHintFormHandle, &WM_SETFONT, D@hFont, D@Redraw
        call 'User32.SendMessageA' D$ASFormHandle, &WM_SETFONT, D@hFont, D@Redraw
        mov ebx DEBUGDLG_FIRSTREG_BUTTON
        While ebx <= DEBUGDLG_LASTREG_BUTTON
            call 'User32.GetDlgItem' D@Handle, ebx
            call 'User32.SendMessageA' eax, &WM_SETFONT, D@hFont, D@Redraw
            inc ebx
        EndWhile
EndP
____________________________________________________________________________________________


____________________________________________________________________________________________

; Create the main debugger window.

[SysPageSize: D$ 0]

CreateDebugWindow:

    ; since we are usingheapalloc, heapfree, etc, the 1st thing to do is determine the pagesizeof the system to we align the heap
    call GetSysPageSize | mov D$SysPageSize eax

    move D$DebugWindowClass@hInstance D$hInstance
    move D$DebugWindowClass@hIcon D$wc_hIcon
    move D$DebugWindowClass@hIconSm D$wc_hIcon
    move D$DebugWindowClass@hCursor D$wc_hCursor

    call 'User32.RegisterClassExA' DebugWindowClass

    ;call 'USER32.LoadMenuA' D$hInstance, M02_MENU
    ;call DebugWindow_CreateMenu

    call 'User32.CreateWindowExA' 0,
        DebugWindowClassName,
        0,
        &WS_OVERLAPPEDWINDOW+&WS_VISIBLE+&WS_POPUP,
        0, 0, 160, 240,
        D$hwnd, 0,
        D$hInstance, 0

    If eax = 0
        call ReportWinError {'Debugger: CreateWindowEx' 0}
        mov eax 0 | ret
    EndIf

    mov D$DebugDialogHandle eax

    mov eax 1
ret
____________________________________________________________________________________________
[DebugWindowClass:
 @cbSize:        D$ len
 @style:         D$ 11
 @lpfnWndProc:   D$ DebugDlgProc
 @cbClsExtra:    D$ 0
 @cbWndExtra:    D$ 0
 @hInstance:     D$ 0
 @hIcon:         D$ 0
 @hCursor:       D$ 0
 @hbrBackground: D$ 1
 @lpszMenuName:  D$ 0
 @lpszClassName: D$ DebugWindowClassName
 @hIconSm:       D$ 0]
[DebugWindowClassName: B$ "RosAsmDebugWindow" 0]
____________________________________________________________________________________________
; Custom events used to interchange informations between the debugger thread and the various
; windows/subwindows.

[WM_DEBUGEVENT      &WM_USER+1  ; debugger thread -> main debug dlg : when debug event occurs
 WM_BEGIN_DEBUG     &WM_USER+2  ; debugger thread -> main debug dlg : debuggee is set up
 WM_SET_PAGE        &WM_USER+3  ; various dlgs -> mem inspector : select virtual page
 WM_REFRESH_CONTENT &WM_USER+4  ; main debug dlg -> data dlgs : refresh data from debuggee
 WM_SELECT_SYMBOL   &WM_USER+5  ; main debug dlg -> data viewer : select symbol by address
 WM_LOG             &WM_USER+6  ; debugger thread -> main debug dlg : to log debug strings
                                ; wparam = Null terminatedString
                                ; lParam = Size of the string including the nul terminated byte
 WM_POPUPMENU       &WM_USER+7  ; hook proc -> main debug dlg : popup menubar sub-menu
 WM_COLLAPSEMENU    &WM_USER+8] ; hook proc -> main debug dlg : collapse menubar sub-menu

; Accelerator table - assign key-combos to menu items

[DbgAccels:
 U$ &FVIRTKEY                   &VK_F6      M02_Run
    &FVIRTKEY                   &VK_F7      M02_Step_Into
    &FVIRTKEY                   &VK_F8      M02_Step_Over
    &FVIRTKEY+&FCONTROL         &VK_F7      M02_Return_to_Caller
    &FVIRTKEY+&FCONTROL         &VK_F12     M02_Pause
    &FVIRTKEY+&FCONTROL         &VK_F6      M02_Terminate]
    ;&FVIRTKEY+&FCONTROL         "F"         M02_FPU_Status


[DbgAccelsNum 6]
[DbgAccelHandle: ?]

;[BreakTitle: ?]
[DebugCaption: B$ ? #64]

; Debuggee is started.
[DebuggerReady: ?]

[TabItem:
 @imask: D$ ? #3
 @pszText: D$ ?
 @cchTextMax: D$ ?
 @iImage: D$ ?
 @lParam: D$ ?]

[DebugRect:
 @left: ?
 @top: ?
 @right: @width: ?
 @bottom: @height: ?]

[UserWants: ?] ; Execution control after debug event (step, run, ...)
[Stepping: ? LastSourcePos: ?]
[DbgLineHeight: 16]

[DebugDialogHandle: ? RegListHandle: ?]; DebugMenuHandle: ?]
[MenubarHandle: ?]
[HoldOnBreakpoints: ? TerminateDebuggee: ? PauseThreads: ? DialogKillsDebugger: ?]

[ExceptionText: 'Exception occurred' 0]

[DebugTextBuffer: ?]

[DebugDialogSize:
 DebugDialog.Width: W$ ?
 DebugDialog.Height: W$ ?]

[DEBUGDLG_REGISTER_TAB 40
 DEBUGDLG_DATA_TAB 200
 DEBUGDLG_FORMAT_COMBO 60
 DEBUGDLG_REG_LIST 70
 DEBUGDLG_FIRSTREG_BUTTON 71
 DEBUGDLG_LASTREG_BUTTON 78
 DEBUGDLG_TB_CONFIGURE 300
 DEBUGDLG_TB_SHOW_CMDS 301
 DEBUGDLG_TB_SHOW_FLAGS 302
 DEBUGDLG_TB_SHOW_FPU 303
 DEBUGDLG_TB_SHOW_TEXT 304]

    [DialogFontHandle: ?]
    [DialogLOGFONTSTRUCT:  0  0  0  0  0  0  536870912 'MS Sans Serif' 0 0 0 0 0 0 ]

;;
    The main debugger window - related functions (right-click)
    
    'DebugDialog_InitDbgMenu', 'DebugDialog_CreateRegisterTabs', 'DebugDialog_CreateDataTab',
    'DebugDialog_InitRegisterListBox', 'DebugDialog_CreateRegisterButtons',
    'DebugDialog_SetFont', 'DebugDialog_ChangeFont', 'DebugDialog_ContinueDebuggee',
    'DebugDialog_OnChangeRegisterTab', 'DebugDialog_OnDrawRegisterItem', 'DebugDialog_OnFormatChange',
    'DebugDialog_UpdateFlags', 'DebugDialog_UpdateRegisterButtons', 
    'DebugDialog_ShowAdvancedTabs', 'DebugDialog_GetCPUInfo', 'DebugDialog_GetFPUStatus',
    'DebugDialog_AdjustDataTabSize', 'DebugDialog_ShowCaller'
;;

Proc DebugDlgProc:
    Arguments @Handle @Message @wParam @lParam
    ;Uses ebx, esi, edi

    pushad

    .If D@Message = &WM_CREATE
        call DebugDialog_OnCreate D@Handle

    .Else_If D@Message = &WM_CONTEXTMENU
        movzx eax W@lParam
        movzx ecx W@lParam+2
        call DebugDialog_RebarHitTest eax ecx

    .Else_if D@Message = WM_BEGIN_DEBUG
        mov D$DebuggerReady &TRUE
        ;mov eax 0

;    .Else_If D@Message = WM_LOG

 ;       call 'USER32.SendMessageA' D$LogForm_Handle, D@Message, D@wParam, D@lParam
        ;call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_ADDSTRING, 0, LogString
        ;call 'USER32.SendMessageA' D$LogForm.ListHandle, &LB_SETCURSEL, eax, 0

    .Else_If D@Message = WM_DEBUGEVENT
        call DebugDialog_OnDebugEvent D@Handle

    .Else_If D@Message = WM_POPUPMENU
        call DebugDialog_OnPopupMenu D@wParam

    .Else_If D@Message = WM_COLLAPSEMENU
        call DebugDialog_OnCollapseMenu

    .Else_If D@Message = &WM_CLOSE
        call DebugDialog_OnClose D@Handle

    .Else_If D@Message = &WM_DESTROY
        call DebugDialog_OnDestroy D@Handle

    .Else_If D@Message = &WM_COMMAND
        call DebugDialog_OnCommand D@Handle, D@wParam, D@lParam

    .Else_If D@Message = &WM_SYSCOMMAND
        call DebugDialog_OnSysCommand D@Handle, D@wParam, D@lParam

    .Else_if D@Message = &WM_KEYDOWN
        call DebugDialog_OnKeyDown D@Handle, D@wParam, D@lParam

    .Else_If D@Message = &WM_NOTIFY
        call DebugDialog_OnNotify D@Handle, D@lParam

    .Else_if D@Message = &WM_GETMINMAXINFO
        mov eax D@lParam
        mov D$eax+24 200
        mov D$eax+28 480
       ; mov eax 0

    .Else_if D@Message = &WM_SIZE
        If D@wParam <> &SIZE_MINIMIZED
            call DebugDialog_OnSize D@Handle, D@lParam
        EndIf

    .Else_if D@Message = &WM_ACTIVATEAPP
        ;mov eax 0

    .Else_if D@Message = &WM_SETFONT
        call DebugDialog_OnSetFont D@Handle, D@wParam, D@lParam

    .Else_if D@Message = &WM_DRAWITEM
        If D@wParam = 0
            call DebugDialog_OnDrawMenuItem D@lParam
        Else
            call DebugDialog_OnDrawRegisterItem D@lParam
        EndIf
        popad
        mov eax &TRUE
        ExitP

    .Else_if D@Message = &WM_MEASUREITEM
        mov eax D@lParam
        If D@wParam = 0
          ; menu
            call DebugDialog_OnMeasureMenuItem D@lParam
        Else
          ; listbox
            move D$eax+16 D$DbgLineHeight
        EndIf
        popad
        mov eax &TRUE
        ExitP
    .Else
        popad
        call 'User32.DefWindowProcA' D@Handle, D@Message, D@wParam, D@lParam
        ExitP
    .End_If

    popad
    mov eax &FALSE
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; MEMORY INSPECTOR
____________________________________________________________________________________________
____________________________________________________________________________________________

[MemInspectorSize:
 MemInspector.Width: W$ ?
 MemInspector.Height: W$ ?]

[MEMINSPECTOR_DATA_LIST 150
 MEMINSPECTOR_PAGE_EDIT 160
 MEMINSPECTOR_PREV_BUTTON 151
 MEMINSPECTOR_NEXT_BUTTON 152
 MEMINSPECTOR_TABLE_BUTTON 153
 MEMINSPECTOR_FORMAT_COMBO 170]

[MemFormatConversionProc: ?]

[CurrentPageAddress: ? CurrentPage: ?]

Proc MemoryInspector_OnDrawItem:
    Arguments @DrawItemStruc
    Local @Brush @Offset @Selected

    mov ebx D@DrawItemStruc
    call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0

    mov D@Selected 0
    test D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
    setz al
    xor al 1
    mov B@Selected al

    ; Print Offset
    mov edx D$DebugFontCharWidth | shl edx 2 | add edx PADDING ; 4 chars
    mov eax D$ebx+DRAWITEM_RCITEM_LEFT | mov D$ItemRect@x1 eax
    add eax edx | mov D$ItemRect@x2 eax
    move D$ItemRect@y1 D$ebx+DRAWITEM_RCITEM_TOP
    move D$ItemRect@y2 D$ebx+DRAWITEM_RCITEM_BOTTOM

    mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    On B@Selected = 1, mov edi 2
    call 'GDI32.CreateSolidBrush' D$BackgroundCol@Name+edi*4 | mov D@Brush eax
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC D$BackgroundCol@Name+edi*4

    mov ecx D$ebx+DRAWITEM_ITEMDATA ; offset into page
    lea edi D@Offset
    mov B$edi '+'
    mov al ch | and al 0F
    add al '0'
    On al > '9', add al 7
    mov B$edi+1 al
    mov al cl | shr al 4
    add al '0'
    On al > '9', add al 7
    mov B$edi+2 al
    mov al cl | and al 0F
    add al '0'
    On al > '9', add al 7
    mov B$edi+3 al

    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 4, ItemRect,
         (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)
        ; (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)
;;
  Modification proposed by AKar, because the &CENTER+&DT is causing some miss-alignments,
  in the Memory-View, when showing chinese Unicode.
;;

    ; Print value
    move D$ItemRect@x1 D$ItemRect@x2
    move D$ItemRect@x2 D$ebx+DRAWITEM_RCITEM_RIGHT

    mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    On B@Selected = 1, mov edi 2
    call 'GDI32.CreateSolidBrush' D$BackgroundCol@Value+edi*4 | mov D@Brush eax
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, ItemRect, D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, D$BackgroundCol@Value+edi*4

    If B@Selected = 1
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0FF_FFFF
    EndIf

    mov esi D$CurrentPage
    add esi D$ebx+DRAWITEM_ITEMDATA
    mov edi ItemString
    mov ecx 8
    call D$MemFormatConversionProc

    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, ItemString, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

    ; Draw focus rect if selected
    If B@Selected = 1
        lea eax D$ebx+DRAWITEM_RCITEM_LEFT
        call 'USER32.DrawFocusRect' D$ebx+DRAWITEM_HDC, eax
    EndIf
EndP

____________________________________________________________________________________________

; Converts string in Hexadecimal (uppercase) notation to a 32 bit integer. (no leading zeros)
;   esi : String
; Output
;   eax : Integer

HexStringToInt:
    mov edx 0
    While B$esi <> 0

        lodsb

        ; Sort out invalid chars
        sub al '0' | js L6>  ; FAIL:    [esi] < '0'
        cmp al 9   | jle L0> ; SUCCESS: [esi] = '0'..'9'
        cmp al 17  | jl L6>  ; FAIL:    [esi] < 'A'
        sub al 7
        cmp al 16  | jae L6> ; FAIL:    [esi] > 'F'

L0:     shl edx 4
        or dl al

    EndWhile
    mov eax edx
ret
; Invalid character error
L6: call 'User32.MessageBoxA' 0, {'Hexadecimal notation: Only 0-9 and A-F allowed!' 0},
        {'Invalid character' 0}, &MB_ICONWARNING
    mov eax 0-1
ret
____________________________________________________________________________________________

; Converts 32 bit integer to string in hexadecimal notation. (no leading zeros)
;   eax : Integer
; Output is stored in 'HexString'.

[HexString: B$ ? #10]

IntToHexString:
    push edi
        mov ecx 8, edx eax, edi HexString

;L0:
        Do
            mov eax 0
            shld eax edx 4
            shl edx 4

            add al '0'
            On al > '9', add al 7
            stosb
        ;loop L0<
            dec ecx
        Loop_Until ecx = 0

        mov B$edi 0
    pop edi
ret
____________________________________________________________________________________________

; When the user enters an address by hand and presses enter or the 'go' button, this
; proc is called. The string is converted to an integer. Then it is verified if it is
; a valid address. If the address is valid WM_SET_PAGE is sent to the dialog.
; Otherwise the old value is restored.

Proc MemInspector_OnGoToAddress:
    Arguments @Handle
    Local @Address, @SegBase, @SegLimit
    Uses esi, edi

        call 'USER32.GetDlgItemTextA' D@Handle, MEMINSPECTOR_PAGEEDIT, HexString, 10
        mov esi HexString
      ; Segment override?
        If W$esi = 'FS'
            move D@SegBase D$FS.Linear
            move D@SegLimit D$FS.Limit
            add esi 3
        Else
            mov D@SegBase 0
            mov D@SegLimit 0_FFFF_FFFF
        EndIf
        call HexStringToInt
        If eax > D@SegLimit
            call 'User32.MessageBoxA' D@Handle, {'The offset is beyond the segment limit!' 0},
                {'Invalid address' 0}, &MB_ICONWARNING
            jmp L0>
        EndIf

        add eax D@SegBase
        mov D@Address eax

        call IsProcessMemory D@Address
        If eax > 0
            mov eax D@Address
            and eax PageBaseMask
            call 'User32.SendMessageA' D@Handle, WM_SET_PAGE, eax, D@Address
        Else
            call 'User32.MessageBoxA' D@Handle, {'This is not a valid address!' 0},
                {'Invalid address' 0}, &MB_ICONWARNING
L0:         mov eax D$CurrentPageAddress
            call IntToHexString
            call 'User32.SetDlgItemTextA' D@Handle, MEMINSPECTOR_PAGEEDIT, HexString
        EndIf
EndP
____________________________________________________________________________________________

; Tag Dialog 1012

Proc MemoryInspectorProc:
    Arguments @Handle @Message @wParam @lParam
    Uses ebx, esi, edi

    ...If D@Message = &WM_INITDIALOG
        VirtualAlloc CurrentPage 4096
        mov D$CurrentPageAddress 0

        call 'USER32.GetClientRect' D@Handle, DebugRect
        move W$MemInspector.Width W$DebugRect@width
        move W$MemInspector.Height W$DebugRect@height

        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_DATA_LIST | mov ebx eax
        mov esi 0
        While esi < 4096
            call 'User32.SendMessageA' ebx, &LB_ADDSTRING, 0, esi
            add esi 8
        EndWhile

        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_FORMAT_COMBO | mov ebx eax
        call 'User32.SendMessageA' ebx, &CB_RESETCONTENT, 0, 0
        mov esi MemFormats
        lodsd
        mov edi eax
        While edi > 0
            lodsd
            If D$UnicodeStrings = 0
                call 'User32.SendMessageA' ebx, &CB_ADDSTRING, 0, D$eax
            Else
                call 'User32.SendMessageW' ebx, &CB_ADDSTRING, 0, D$eax
            EndIf
            dec edi
        EndWhile
        call 'User32.SendMessageA' ebx, &CB_SETCURSEL, 0, 0
        move D$MemFormatConversionProc D$MemConvert

        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_PREV_BUTTON
        call 'USER32.EnableWindow' eax, &FALSE
        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_NEXT_BUTTON
        call 'USER32.EnableWindow' eax, &FALSE

        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_PAGE_EDIT
        call 'User32.SendMessageA' eax, &EM_SETLIMITTEXT, 8, 0

        mov eax &TRUE

    ...ElseIf D@Message = WM_REFRESH_CONTENT
        mov eax D@wParam
        If D$CurrentPageAddress <> 0
        ;If D$CurrentPageAddress <> eax
            call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$CurrentPageAddress, D$CurrentPage, PageSize, &NULL
            ;call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, eax, D$CurrentPage, PageSize, &NULL
            call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_DATA_LIST
            call 'USER32.InvalidateRect' eax, &NULL, &TRUE
        EndIf

        mov eax 0

    ...ElseIf D@Message = WM_SET_PAGE
        mov eax D@wParam
        If eax <> D$CurrentPageAddress
            mov D$CurrentPageAddress eax
            call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D@wParam, D$CurrentPage, 01000, &NULL
        EndIf
      ; Write Base address of page into an edit
        mov eax D@wParam
        call IntToHexString
        call 'User32.SetDlgItemTextA' D@Handle, MEMINSPECTOR_PAGE_EDIT, HexString
      ; Switch to memory inspector tab
        call SelectTab D$DebugDialogHandle, DEBUGDLG_DATA_TAB, 1
      ; Scroll list to address (lParam) and redraw list entries
        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_DATA_LIST
        mov ebx eax
        mov edi D@lParam
        and edi 0FFF
        shr edi 3
        call 'User32.SendMessageA' ebx, &LB_SETTOPINDEX, edi, 0
        call 'User32.SendMessageA' ebx, &LB_SETCURSEL, edi, 0
        call 'USER32.SetFocus' ebx
      ; Activate / Deactivate Prev & Next Buttons
        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_PREV_BUTTON
        mov ebx eax
        call FindPrevPage D$CurrentPageAddress
        On eax > 0, mov eax &TRUE
        call 'USER32.EnableWindow' ebx, eax
        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_NEXT_BUTTON
        mov ebx eax
        call FindNextPage D$CurrentPageAddress
        On eax > 0, mov eax &TRUE
        call 'USER32.EnableWindow' ebx, eax

        mov eax 0

    ...ElseIf D@Message = &WM_DESTROY
        VirtualFree D$CurrentPage
        mov eax 0
;;
    ...Else_if D@Message = &WM_KEYDOWN
        call 'USER32.GetFocus'
        call 'USER32.GetDlgCtrlID' eax
        ;.If eax = MEMINSPECTOR_PAGE_EDIT
            If W@wParam = &VK_RETURN
                nop
            EndIf
        ;.EndIf
;;
    ...Else_if D@Message = &WM_COMMAND
        ..If W@wParam = MEMINSPECTOR_FORMAT_COMBO
            .If W@wParam+2 = &CBN_SELCHANGE
                call 'User32.SendMessageA' D@lParam, &CB_GETCURSEL, 0, 0
                If eax <> &CB_ERR
                    move D$MemFormatConversionProc D$MemConvert+eax*4
                    call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_DATA_LIST
                    call 'USER32.InvalidateRect' eax, &NULL, &TRUE
                EndIf
            .End_If
        ..Else_if W@wParam = MEMINSPECTOR_PREV_BUTTON
            call FindPrevPage D$CurrentPageAddress
            If eax > 0
                call 'User32.SendMessageA' D@Handle, WM_SET_PAGE, eax, eax
            EndIf
        ..Else_if W@wParam = MEMINSPECTOR_NEXT_BUTTON
            call FindNextPage D$CurrentPageAddress
            If eax > 0
                call 'User32.SendMessageA' D@Handle, WM_SET_PAGE, eax, eax
            EndIf
        ..Else_if W@wParam = MEMINSPECTOR_TABLE_BUTTON
            call 'User32.DialogBoxParamA' D$hinstance, 1013, D$DebugDialogHandle,
                                          PageTableProc, D$CurrentPageAddress
            If eax >= 01000
                call 'User32.SendMessageA' D@Handle, WM_SET_PAGE, eax, eax
            EndIf
        ..Else_if W@wParam = MEMINSPECTOR_PAGE_EDIT
            movzx eax W@wParam+2
        ..Else_if W@wParam = &IDOK
            call MemInspector_OnGoToAddress D@Handle
        ..End_If
        mov eax 0

    ...ElseIf D@Message = &WM_SIZE
        movzx eax W$MemInspector.Width
        movzx esi W@lParam
        sub esi eax
        movzx eax W$MemInspector.Height
        movzx edi W@lParam+2
        sub edi eax
        call AdjustControlSize D@Handle, MEMINSPECTOR_DATA_LIST, esi, edi
        call AdjustControlSize D@Handle, MEMINSPECTOR_FORMAT_COMBO, esi, 0
        call AdjustControlPos D@Handle, MEMINSPECTOR_PREV_BUTTON, 0, edi
        call AdjustControlPos D@Handle, MEMINSPECTOR_NEXT_BUTTON, esi, edi
        call AdjustControlPos D@Handle, MEMINSPECTOR_TABLE_BUTTON, 0, edi
        call AdjustControlSize D@Handle, MEMINSPECTOR_TABLE_BUTTON, esi, 0
        call 'USER32.InvalidateRect' D@Handle, &NULL, &TRUE
        move D$MemInspectorSize D@lParam
        mov eax 0

    ...Else_if D@Message = &WM_SETFONT
        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_DATA_LIST
        call 'User32.SendMessageA' eax, &WM_SETFONT, D@wParam, D@lParam
        call 'USER32.GetDlgItem' D@Handle, MEMINSPECTOR_PAGE_EDIT
        call 'User32.SendMessageA' eax, &WM_SETFONT, D@wParam, D@lParam

    ...Else_if D@Message = &WM_DRAWITEM
        call MemoryInspector_OnDrawItem D@lParam
        mov eax &TRUE

    ...Else_if D@Message = &WM_MEASUREITEM
        mov eax D@lParam
        mov D$eax+16 15
        mov eax &TRUE

    ...Else
        mov eax &FALSE
    ...EndIf

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; PAGE TABLE
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc PageTable_WritePageDescription:
    Arguments @Address, @Protect
    Uses esi, edi

        ; Write address
        mov eax D@Address
        mov edi PageDesc, ecx 4, edx eax
        test eax 0FFFF | jz L0>
        mov D$edi ' >> '
        add edi 4
L0:     add edi 8

L0:     mov al dl
        mov ah al | and ah 0F | shr al 4

        add ah '0'
        On ah > '9', add ah 7
        dec edi | mov B$edi ah
        add al '0'
        On al > '9', add al 7
        dec edi | mov B$edi al

        shr edx 8
        loop L0<

        ; Write protection "XRWC GN"
        mov edi Pagedesc+9, ecx D@Protect
        On B$Pagedesc = ' ', add edi 4
        mov B$edi-1 9
        mov D$edi '----', D$edi+4 ' -- '
        If ecx = &PAGE_READONLY
            mov B$edi+1 'R'
        Else_if ecx = &PAGE_READWRITE
            mov W$edi+1 'RW'
        Else_if ecx = &PAGE_EXECUTE
            mov B$edi 'X'
        Else_if ecx = &PAGE_EXECUTE_READ
            mov W$edi 'XR'
        Else_if ecx = &PAGE_EXECUTE_READWRITE
            mov D$edi 'XRW-'
        Else_if ecx = &PAGE_EXECUTE_WRITECOPY
            mov D$edi 'XRWC'
        Else_if ecx = &PAGE_WRITECOPY
            mov D$edi '-RWC'
        EndIf
        test ecx &PAGE_GUARD | jz L0>
        mov B$edi+5 'G'
L0:     test ecx &PAGE_NOCACHE | jz L0>
        mov B$edi+6 'N'
L0:
        ; Write module filename (dll/exe), if available
        mov edi Pagedesc+17
        On B$Pagedesc = ' ', add edi 4
        mov eax 0
        ;call GetModuleName D@Address, D$ModuleList, D$NumModules
        call GetModuleNameFromAddress D@Address, D$ModuleList, D$NumModules
        If eax <> 0
            mov esi eax
L0:         movsb | cmp B$esi 0 | jne L0<
        EndIf
        mov D$edi 0
EndP

Proc PageTable_AddItem:
    Arguments @Base, @Size, @Protect, @Type
    Uses esi, edi

        mov esi D@Base, edi esi
        add edi D@Size

        While esi < edi
            call PageTable_WritePageDescription esi, D@Protect
            call 'User32.SendMessageA' ebx, &LB_ADDSTRING, 0, PageDesc
            call 'User32.SendMessageA' ebx, &LB_SETITEMDATA, eax, esi
            add esi 01000
        EndWhile
EndP
____________________________________________________________________________________________

Proc PageTable_Build:
    Uses esi

        mov esi D$AddressLowerBound
        While esi < D$AddressUpperBound
            call VirtualQuery esi
            If eax = 1
                call PageTable_AddItem esi, ecx,
                    D$MemoryInformation@Protect, D$MemoryInformation@Type
            ElseIf eax = 0-1
                ExitP
            EndIf
            add esi D$MemoryInformation@RegionSize
        EndWhile
EndP
____________________________________________________________________________________________

; Tag Dialog 1013

Proc PageTableProc:
    Arguments @Handle, @Message, @wParam, @lParam
    Uses ebx, esi, edi

    ..If D@Message = &WM_INITDIALOG
        call 'User32.GetDlgItem' D@Handle, 10
        mov ebx eax
        call 'User32.SendMessageA' ebx, &LB_RESETCONTENT, 0, 0
        call 'User32.SendMessageA' ebx, &WM_SETFONT, D$DebugFontHandle, &TRUE
        call PageTable_Build
        mov eax &TRUE

    ..Else_if D@Message = &WM_COMMAND
        .If D@wParam = 1
            call 'User32.GetDlgItem' D@Handle, 10
            mov ebx eax
            call 'User32.SendMessageA' ebx, &LB_GETCURSEL, 0, 0
            call 'User32.SendMessageA' ebx, &LB_GETITEMDATA, eax, 0
            call 'USER32.EndDialog' D@Handle, eax
        .Else_if W@wParam = 10
            If W@wParam+2 = &LBN_DBLCLK
                call 'User32.SendMessageA' D@lParam, &LB_GETCURSEL, 0, 0
                call 'User32.SendMessageA' D@lParam, &LB_GETITEMDATA, eax, 0
                call 'USER32.EndDialog' D@Handle, eax
            EndIf
        .EndIf
        mov eax 0

    ..Else_if D@Message = &WM_CLOSE
        call 'User32.EndDialog' D@Handle, 0
        mov eax 0
    ..Else
        mov eax &FALSE
    ..EndIf
EndP
____________________________________________________________________________________________

; Tag Dialog 1014

ShowExceptionInfo:
    call 'User32.CreateDialogParamA' D$hInstance, 1014, D$hwnd, ExceptionInfoProc, D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCodeDis;E.ExceptionCode
ret

[EXCEPTINFO_DESC 10 EXCEPTINFO_ADDRESS 15 EXCEPTINFO_INSTRUCTION 20 EXCEPTINFO_INFO 25]

[ExceptionMap:
 &EXCEPTION_ACCESS_VIOLATION ACCESS_VIOLATION
 &EXCEPTION_ARRAY_BOUNDS_EXCEEDED ARRAY_BOUNDS_EXCEEDED
 &EXCEPTION_DATATYPE_MISALIGNMENT DATATYPE_MISALIGNMENT
 &EXCEPTION_FLT_DENORMAL_OPERAND FLT_DENORMAL_OPERAND
 &EXCEPTION_FLT_DIVIDE_BY_ZERO FLT_DIVIDE_BY_ZERO
 &EXCEPTION_FLT_INEXACT_RESULT FLT_INEXACT_RESULT
 &EXCEPTION_FLT_INVALID_OPERATION FLT_INVALID_OPERATION
 &EXCEPTION_FLT_OVERFLOW FLT_OVERFLOW
 &EXCEPTION_FLT_STACK_CHECK FLT_STACK_CHECK
 &EXCEPTION_FLT_UNDERFLOW FLT_UNDERFLOW
 &EXCEPTION_ILLEGAL_INSTRUCTION ILLEGAL_INSTRUCTION
 &EXCEPTION_IN_PAGE_ERROR IN_PAGE_ERROR
 &EXCEPTION_INT_DIVIDE_BY_ZERO INT_DIVIDE_BY_ZERO
 &EXCEPTION_INT_OVERFLOW INT_OVERFLOW
 &EXCEPTION_PRIV_INSTRUCTION PRIV_INSTRUCTION
 &EXCEPTION_STACK_OVERFLOW STACK_OVERFLOW
 &EXCEPTION_INVALID_DISPOSITION INVALID_DISPOSITION
 &EXCEPTION_NONCONTINUABLE_EXCEPTION NONCONTINUABLE_EXCEPTION
 0 UNKNOWN_EXCEPTION]

[ExceptionCaption: B$ ? #128]

Proc ExceptionInfoProc:
    Arguments @Handle, @Message, @wParam, @lParam
    Uses ebx, esi, edi

    ..If D@Message = &WM_INITDIALOG

      ; Output module in caption
        If D$CurrentModule <> 0
            mov eax D$CurrentModule
        Else
            mov eax {'Exception in Non-Code Section' 0}
        EndIf
        call 'User32.SetWindowTextA' D@Handle, eax

      ; Output exception description
        mov eax D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCodeDis;E.ExceptionCode
        mov esi ExceptionMap
        While D$esi <> 0
            cmp eax D$esi | je L0>
            add esi 8
        EndWhile
        mov edx eax, ecx 4, edi D$esi+4
        call IntToHex
L0:     call 'USER32.GetDlgItem' D@Handle, EXCEPTINFO_DESC | mov ebx eax
        call 'User32.SendMessageA' ebx, &WM_SETFONT, D$DebugFontHandle, &TRUE
        call 'User32.SetWindowTextA' ebx, D$esi+4

      ; Output troubling instruction and its address
        mov eax D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionAddressDis;E.ExceptionAddress
        call IntToHexString
        call 'USER32.GetDlgItem' D@Handle, EXCEPTINFO_ADDRESS | mov ebx eax
        call 'User32.SendMessageA' ebx, &WM_SETFONT, D$Font1Handle, &TRUE
        call 'User32.SetWindowTextA' ebx, HexString

        call NextInstructionDecode
        call 'USER32.GetDlgItem' D@Handle, EXCEPTINFO_INSTRUCTION | mov ebx eax
        call 'User32.SendMessageA' ebx, &WM_SETFONT, D$Font1Handle, &TRUE
        call 'User32.SetWindowTextA' ebx, D$NextInstructionPtr

      ; Output further information for protection faults
        .If D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCodeDis = &EXCEPTION_ACCESS_VIOLATION
        ;.If D$E.ExceptionCode = &EXCEPTION_ACCESS_VIOLATION
            mov eax D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionInformationDis ;read/write E.ExceptionInfo ; read/write
            If eax = 0
                mov D$AV_ReadWrite 'read', D$AV_ReadWrite+4 ' fro', B$AV_ReadWrite+8 'm'
            Else
                mov D$AV_ReadWrite 'writ', D$AV_ReadWrite+4 'e at', B$AV_ReadWrite+8 ' '
            EndIf
            mov edx D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionInformationDis+4;E.ExceptionInfo+4 ; inaccessible address
            mov ecx 4, edi AV_Address
            call IntToHex
            call 'User32.SetDlgItemTextA' D@Handle, EXCEPTINFO_INFO, Exception_AV
        .EndIf

      ; If the exception can be continued hand to the SEH
        call 'USER32.GetDlgItem' D@Handle, 2
        mov edx D$ExceptionFlags | and edx E_MUSTEXIT
        If edx <> 0
            mov edx 0
        Else
            mov edx 1
        EndIf
        call 'USER32.EnableWindow' eax, edx

        call 'USER32.GetDlgItem' D@Handle, 1
        call 'USER32.SetFocus' eax
        mov eax 0

    ..Else_if D@Message = &WM_CTLCOLORSTATIC
        call 'USER32.GetDlgItem' D@Handle, EXCEPTINFO_DESC
        If eax = D@lParam
            call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
            mov eax D$DialogsBackGroundBrushHandle
            ExitP
        EndIf
        call 'USER32.GetDlgItem' D@Handle, EXCEPTINFO_INSTRUCTION
        If eax = D@lParam
            call 'GDI32.SetTextColor' D@wParam, 099
            call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
            mov eax D$DialogsBackGroundBrushHandle
            ExitP
        EndIf
        call 'USER32.GetDlgItem' D@Handle, EXCEPTINFO_ADDRESS
        If eax = D@lParam
            call 'GDI32.SetBkColor' D@wParam, D$DialogsBackColor
            mov eax D$DialogsBackGroundBrushHandle
            ExitP
        EndIf
        mov eax 0

    ..Else_if D@Message = &WM_COMMAND
        .If D@wParam = 1
            call 'USER32.EndDialog' D@Handle, 0
            mov B$TerminateDebuggee &TRUE
            call DebugDialog_ContinueDebuggee
            ;SendMessage D$DebugDialogHandle, &WM_CLOSE, 0, 0
        .ElseIf D@wParam = 2
            call 'USER32.EndDialog' D@Handle, 0
            call DebugDialog_ContinueDebuggee
        .EndIf
        mov eax 0

    ..Else_if D@Message = &WM_CLOSE
        call 'User32.EndDialog' D@Handle, 0
        mov eax 0
    ..Else
        mov eax &FALSE
    ..EndIf
EndP


____________________________________________________________________________________________
____________________________________________________________________________________________

; DATA LABEL VIEWER
____________________________________________________________________________________________
____________________________________________________________________________________________

[PADDING 4]

Proc DataView_OnDrawItem:
    Arguments @DrawItemStruc
    Local @Brush @Address @Selected

    mov ebx D@DrawItemStruc
    call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0

    mov D@Selected 0
    test D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
    setz al
    xor al 1
    mov B@Selected al

  ; Print Offset
    mov edx D$DebugFontCharWidth | shl edx 3 | add edx PADDING ; 8 chars
    mov eax D$ebx+DRAWITEM_RCITEM_LEFT | mov D$ItemRect@x1 eax
    add eax edx | mov D$ItemRect@x2 eax
    move D$ItemRect@y1 D$ebx+DRAWITEM_RCITEM_TOP
    move D$ItemRect@y2 D$ebx+DRAWITEM_RCITEM_BOTTOM

    mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    On B@Selected = 1, mov edi 2
    call 'GDI32.CreateSolidBrush' D$BackgroundCol@Name+edi*4 | mov D@Brush eax
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC D$BackgroundCol@Name+edi*4

    call IsWatchPoint D$ebx+DRAWITEM_ITEMDATA
    If eax = 1
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0CC
    ElseIf eax = 3
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 066CC
    EndIf

    mov eax D$ebx+DRAWITEM_ITEMDATA
    call IntToHexString
    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, HexString, 8, ItemRect,
         (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

  ; Print value
    move D$ItemRect@x1 D$ItemRect@x2
    move D$ItemRect@x2 D$ebx+DRAWITEM_RCITEM_RIGHT

    mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    On B@Selected = 1, mov edi 2
    call 'GDI32.CreateSolidBrush' D$BackgroundCol@Value+edi*4 | mov D@Brush eax
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, D$BackgroundCol@Value+edi*4

    If B@Selected = 1
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0FF_FFFF
    EndIf

    call 'User32.SendMessageA' D$ebx+DRAWITEM_HWNDITEM, &CB_GETLBTEXT, D$ebx+DRAWITEM_ITEMID, ItemString

    add D$ItemRect@x1 5
    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, ItemString, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)

    ; Draw focus rect if selected
    If B@Selected = 1
        lea eax D$ebx+DRAWITEM_RCITEM_LEFT
        call 'USER32.DrawFocusRect' D$ebx+DRAWITEM_HDC, eax
    EndIf
EndP
____________________________________________________________________________________________

Proc DataView_OnDrawValueItem:
    Arguments @DrawItemStruc
    Local @Brush @Address @Selected

    mov ebx D@DrawItemStruc
    On D$ebx+DRAWITEM_ITEMID = 0-1, ExitP

    call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, 0

    mov D@Selected 0
    test D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
    setz al
    xor al 1
    mov B@Selected al

  ; Print size
    mov edx D$DebugFontCharWidth | shl edx 3 | add edx PADDING ; 8 chars
    mov eax D$ebx+DRAWITEM_RCITEM_LEFT | mov D$ItemRect@x1 eax
    add eax edx | mov D$ItemRect@x2 eax
    move D$ItemRect@y1 D$ebx+DRAWITEM_RCITEM_TOP
    move D$ItemRect@y2 D$ebx+DRAWITEM_RCITEM_BOTTOM

    mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    ;On B@Selected = 1, mov edi 2
    call 'GDI32.CreateSolidBrush' D$BackgroundCol@Name+edi*4 | mov D@Brush eax
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC D$BackgroundCol@Name+edi*4

    mov eax D$ebx+DRAWITEM_ITEMDATA
    If al = 'D'
        mov eax {'32bit' 0}
    ElseIf al = 'W'
        mov eax {'16bit' 0}
    ElseIf al = 'B'
        mov eax {'8bit' 0}
    ElseIf al = 'S'
        mov eax {'ASCII' 0}
    ElseIf al = 'F'
        mov eax {'32bit FP' 0}
    ElseIf al = 'R'
        mov eax {'64bit FP' 0}
    ElseIf al = 'T'
        mov eax {'80bit FP' 0}
    ElseIf al = 'Z'
        mov eax {'ALERT' 0}
    EndIf
    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, eax, -1, ItemRect, (&DT_SINGLELINE+&DT_CENTER+&DT_VCENTER)

  ; Print value
    move D$ItemRect@x1 D$ItemRect@x2
    move D$ItemRect@x2 D$ebx+DRAWITEM_RCITEM_RIGHT

    mov edi D$ebx+DRAWITEM_ITEMID | and edi 1
    ;On B@Selected = 1, mov edi 2
    call 'GDI32.CreateSolidBrush' D$BackgroundCol@Value+edi*4 | mov D@Brush eax
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC ItemRect D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC D$BackgroundCol@Value+edi*4

    mov B$HexSeparator 3

    mov eax D$ebx+DRAWITEM_ITEMDATA, esi DataBuffer
    .If al = 'D'
        mov ecx 4 | call toHex
        sub edi 2 | mov W$edi '0x'
    .ElseIf al = 'W'
        mov ecx 2 | call toHex
        sub edi 2 | mov W$edi '0x'
    .ElseIf al = 'B'
        mov ecx 1 | call toHex
        sub edi 2 | mov W$edi '0x'
    .ElseIf al = 'S'
        mov edi DataBuffer
    .ElseIf al = 'F'
        mov ecx 4 | call toFloat
        call DataView_FPUHexRegisterFormat FloatBuf
    .ElseIf al = 'R'
        mov ecx 8 | call toDouble
        call DataView_FPUHexRegisterFormat FloatBuf
    .ElseIf al = 'T'
        mov ecx 10 | call toExtended
        call DataView_FPUHexRegisterFormat esi
    .Else_if al = 'Z'
        mov edi RebasedWarningBuffer
        C_call FormatStr edi, {'Rebased Label from 0x%x to 0x%x' 0}, D$OriginalDataPointer, D$DataPointer
    .EndIf

    add D$ItemRect@x1 5
    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect,
         (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)

    mov eax D$ItemRect@x2 | sub eax D$ItemRect@x1
    mov edx 0, ecx 3 | div ecx
    push eax
        add D$ItemRect@x1 eax

        mov eax D$ebx+DRAWITEM_ITEMDATA, esi DataBuffer
        If al = 'D'
            mov ecx 4 | call toUDWord
        ElseIf al = 'W'
            mov ecx 2 | call toUWord
        ElseIf al = 'B'
            mov ecx 1 | call toUByte
        Else
            pop eax | jmp L0>
        EndIf
        call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect, (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)
    pop eax
    add D$ItemRect@x1 eax

    mov eax D$ebx+DRAWITEM_ITEMDATA, esi DataBuffer
    If al = 'D'
        mov ecx 4 | call toSDWord
    ElseIf al = 'W'
        mov ecx 2 | call toSWord
    ElseIf al = 'B'
        mov ecx 1 | call toSByte
    Else
        pop eax | jmp L0>
    EndIf
    call 'User32.DrawTextA' D$ebx+DRAWITEM_HDC, edi, 0-1, ItemRect, (&DT_SINGLELINE+&DT_LEFT+&DT_VCENTER)

    ; Draw focus rect if selected
L0: If B@Selected = 1
        lea eax D$ebx+DRAWITEM_RCITEM_LEFT
        call 'USER32.DrawFocusRect' D$ebx+DRAWITEM_HDC, eax
    EndIf
EndP

____________________________________________________________________________________________

;;
 Displays the hexadecimal values of a given FPU Value. The Data is displayed accordying to
 the Real10 Format. Like:
 1st Word = Sign bit + Biased exponent bits
 2nd and 3rd Dwords = fraction bits of the significand
 
 The hexadecimal Values are better viewed in a structured form. Example:
 Value: -1.77115067950904132e+33
 Hexadecimal Values in a structure form:
 Data1: D$ 0         ;  fraction bits of the significand
 Data2: D$ 0AEA62100 ;  fraction bits of the significand
 Data3: W$ 0C06D     ;  Sign bit + Biased exponent bits
 
 Due to the Format of a register in a FPU Data, this function displays the hexadecimal values in reversed order, such as:
    Data3 Data2 Data1
;;

Proc DataView_FPUHexRegisterFormat:
    Arguments @Float80Pointer
    Uses edi, esi, ecx, eax

    ; Calculate the end of the initial String at edi and it´s lenght
    mov ecx 0
    While B$edi <> 0
        inc edi
        inc ecx
    End_While

    ; Place the new String at the position 26, no matter how big the previous FPU string was.
    ; (In fact, the former string must be smaller then 26).
    ; Then fill the remaining bytes between the end of the old string and the beginning of the new one with spaces.
    If ecx < 26
        mov eax 26
        sub eax ecx
        mov ecx eax

    Do
        mov B$edi ' '
        dec ecx
        inc edi
    Loop_Until ecx = 0

    Else
        mov B$edi ' '
    End_If

    push esi
        mov W$edi '0x' | add edi 2
        ;mov esi FloatBuf
        mov esi D@Float80Pointer
        add esi 8
        mov ecx 2
        call toHexWords

        mov B$edi-1 ' '
        mov W$edi '0x' | add edi 2
        ;mov esi FloatBuf
        mov esi D@Float80Pointer
        add esi 4
        mov ecx 4
        call toHexDwords

        mov B$edi-1 ' '
        mov W$edi '0x' | add edi 2
        ;mov esi FloatBuf
        mov esi D@Float80Pointer
        mov ecx 4
        call toHexDwords
    pop esi

EndP

____________________________________________________________________________________________

; Insert data label sorted by address. Brute force search.

Proc DataView_InsertDataLabel:
    Arguments @ComboHandle, @Name, @Address
    Uses ebx

    mov ebx 0

L0: call 'User32.SendMessageA' D@ComboHandle, &CB_GETITEMDATA, ebx, 0
    cmp eax &CB_ERR | je L1> ; EOL or highest address > append

    If eax > D@Address
L1:     call 'User32.SendMessageA' D@ComboHandle, &CB_INSERTSTRING, ebx, D@Name
        ExitP
    Endif

    inc ebx | jmp L0<
EndP
____________________________________________________________________________________________

; Filling the ComboBox with the Data Label Names (as string) / Addresses (as data)

[DataLabelComboHandle: ?]

Proc DataView_FillDataLabelCombo:
    Arguments @ComboHandle @SortByName

    call 'User32.SendMessageA' D@ComboHandle, &CB_RESETCONTENT, 0, 0
    move D$DataLabelComboHandle D@ComboHandle

    mov esi D$PlainLabelList
    lodsd | mov edx D$PlainLabelList | add edx eax

L0: inc esi ; '|'

L0: mov ebx esi
    While B$esi <> 2
        inc esi
    End_While
    test B$esi+5 DataLabelFlag | jz L2>

        mov edi ItemString, esi ebx
        While B$esi <> 2
            movsb
        End_While
        mov B$edi 0
        push edx
            mov ebx D$esi+1 | add ebx D$DataAjust
            If D@SortByName = 1
                call 'User32.SendMessageA' D@ComboHandle, &CB_ADDSTRING, 0, ItemString
            Else
                call DataView_InsertDataLabel D@ComboHandle, ItemString, ebx
            EndIf
            call 'User32.SendMessageA' D@ComboHandle, &CB_SETITEMDATA, eax, ebx
        pop edx
        add esi 7
        cmp esi edx | jb L0<
    ExitP

L2: add esi 7 | cmp esi edx | jb L0<
EndP
____________________________________________________________________________________________

Proc DataView_LoadSettings:
    Arguments @FileHandle @Size

  ; Copy label to stack
    mov eax D@Size | add eax 4 | and eax 0_FFFF_FFFC ; reserve for \0 and dw-align stack
    sub esp eax | mov edi esp
    call 'KERNEL32.ReadFile' D@FileHandle, edi, D@Size, BytesTransfered, 0
    mov ecx D@Size | mov B$edi+ecx 0 ; terminate string

  ; Search and select
    call 'User32.SendMessageA' D$DataLabelComboHandle, &CB_FINDSTRING, 0-1, edi
    call 'User32.SendMessageA' D$DataLabelComboHandle, &CB_SETCURSEL, eax, 0

    mov eax 1
EndP
____________________________________________________________________________________________

Proc DataView_SaveSettings:
    Arguments @FileHandle
    Structure @Settings 8, @Id 0, @Size 4

    mov D@Id 'LaLa' ; LAst LAbel

  ; Get currently selected label
    call 'User32.SendMessageA' D$DataLabelComboHandle, &CB_GETCURSEL, 0, 0
    On eax = 0-1, ExitP
    mov ebx eax
    call 'User32.SendMessageA' D$DataLabelComboHandle, &CB_GETLBTEXTLEN, ebx, 0
    mov D@Size eax

  ; Copy label to stack
    add eax 4 | and eax 0_FFFF_FFFC ; reserve for \0 and dw-align stack
    sub esp eax | mov edi esp
    call 'User32.SendMessageA' D$DataLabelComboHandle, &CB_GETLBTEXT, ebx, edi

    call 'KERNEL32.WriteFile' D@FileHandle, D@Settings, 8, BytesTransfered, 0
    call 'KERNEL32.WriteFile' D@FileHandle, edi, D@Size, BytesTransfered, 0
EndP
____________________________________________________________________________________________

[DataBuffer: B$ ? #64]
;[DataValue: B$ ? #32]
[DataPointer: ?]

[ShowRebasedWarning: D$ 0]
[RebasedWarningBuffer: B$ 0 #128]
[OriginalDataPointer: D$ 0]

Proc DataView_OnSelectDataLabel:
    Arguments @DlgHandle, @ComboHandle
    Local @LabelLen, @ListHandle

        call 'User32.SendMessageA' D@ComboHandle, &CB_GETCURSEL, 0, 0
        On eax = &CB_ERR, ExitP
        call 'User32.SendMessageA' D@ComboHandle, &CB_GETITEMDATA, eax, 0

        mov D$ShowRebasedWarning &FALSE
        If_And D$DebugProcessData.Type = DEBUG_DLL, D$DebugProcessData.IsRebased = &TRUE
            mov D$OriginalDataPointer eax
            sub eax D$DebugProcessData.DllImageBase | add eax D$DebugProcessData.DllNewImageBase
            mov D$ShowRebasedWarning &TRUE
        End_If
        mov D$DataPointer eax

        call 'USER32.GetDlgItem' D@DlgHandle, DATAVIEW_VALUE_LIST
        mov D@ListHandle eax

        call 'User32.SendMessageA' D@ListHandle, &LB_RESETCONTENT, 0, 0

        ; Read a block of memory with a max size of 64 byte
        call IsProcessMemory D$DataPointer
        On eax = 0, ExitP
        mov ebx 64
        If eax < ebx
            mov ebx eax
        EndIf
        call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D$DataPointer, DataBuffer, ebx, &NULL

        ; DWord Size
        On ebx >= 4, call 'User32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, 'D'
        ; Word Size
        On ebx >= 2, call 'User32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, 'W'
        ; Byte size
        call 'User32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, 'B'
        ; String
        call IsString DataBuffer 64
        If ah = 1
            mov B$DataBuffer+63 0
            call 'User32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, 'S'
        EndIf
        ; Float
        On ebx >= 4, call 'User32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, 'F'
        ; Double
        On ebx >= 8, call 'User32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, 'R'
        ; Extended
        On ebx >= 10, call 'User32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, 'T'
        ;Rebased warning. Shows only when loading a dll whose proces rebased the file image address
        On D$ShowRebasedWarning = &TRUE, call 'User32.SendMessageA' D@ListHandle, &LB_ADDSTRING, 0, 'Z'

        ; Activate 'More' button
        call 'USER32.GetDlgItem' D@DlgHandle, M03_SHOW_MEM
        call 'USER32.EnableWindow' eax, &TRUE
        ; Activate/Deactivate 'More From Pointer' button
        .If ebx >= 4
            call IsProcessMemory D$DataBuffer
            If eax > 0
                mov ebx &TRUE
            Else
                mov ebx &FALSE
            EndIf
            call 'User32.GetDlgItem' D@DlgHandle, M03_SHOW_PMEM
            call 'USER32.EnableWindow' eax, ebx
            If ebx = &TRUE
                mov eax &MF_ENABLED
            Else
                mov eax &MF_GRAYED
            EndIf
            call 'USER32.EnableMenuItem' D$DataView.PopupMenu, M03_SHOW_PMEM, eax
        .EndIf
EndP
____________________________________________________________________________________________

Proc DataView_SelectSymbol:
    Arguments @DlgHandle, @Address
    Local @ComboHandle

        call SelectTab D$DebugDialogHandle, DEBUGDLG_DATA_TAB, 0
        call 'USER32.GetDlgItem' D@DlgHandle, DATAVIEW_LABEL_COMBO
        mov D@ComboHandle eax
        call 'User32.SendMessageA' D@ComboHandle, &CB_GETCOUNT, 0, 0
        mov edi eax, esi 0, ebx D@Address
        While esi < edi
            call 'User32.SendMessageA' D@ComboHandle, &CB_GETITEMDATA, esi, 0
            If eax = ebx
                call 'User32.SendMessageA' D@ComboHandle, &CB_SETCURSEL, esi, 0
                ExitP
            EndIf
            inc esi
        EndWhile
EndP

____________________________________________________________________________________________

Proc DataView_ShowDeclaration:
    Arguments @DlgHandle
    Local @ComboHandle, @LabelLen

        call 'USER32.GetDlgItem' D@DlgHandle, DATAVIEW_LABEL_COMBO
        mov D@ComboHandle eax

        call 'User32.SendMessageA' D@ComboHandle, &CB_GETCURSEL, 0, 0
        On eax = &CB_ERR, ExitP
        call 'User32.SendMessageA' D@ComboHandle, &CB_GETLBTEXT, eax, ItemString
        mov D@LabelLen eax

        call RestoreRealSource

        mov edx ItemString, ebx D@LabelLen
        call InternSearch
        If B$BlockInside = &TRUE
            mov B$BlockInside &FALSE
            mov esi D$CurrentWritingPos | dec esi | call InternalRightClick
        End_If

        call SetPartialEditionFromPos
EndP
____________________________________________________________________________________________

[DataView.PopupMenu: ?]

[M03_SHOW_MEM       3100
 M03_SHOW_PMEM      3101
 M03_SHOW_DECL      3102
 M03_WATCH_W        3103
 M03_WATCH_RW       3104
 M03_SORTBYNAME     3105
 M03_SORTBYADDRESS  3106]

Proc DataView_CreatePopupMenu:
    Arguments @DlgHandle

    call 'USER32.CreatePopupMenu' | mov ebx eax, D$DataView.PopupMenu eax
    call AppendMenu ebx, &MF_OWNERDRAW, M03_SHOW_MEM, 0
    call AppendMenu ebx, &MF_OWNERDRAW, M03_SHOW_PMEM, 0
    call AppendMenu ebx, &MF_OWNERDRAW, M03_SHOW_DECL, 0
    call AppendMenu ebx, &MF_SEPARATOR, 0, 0
    call AppendMenu ebx, &MF_OWNERDRAW, M03_WATCH_W, 0
    call AppendMenu ebx, &MF_OWNERDRAW, M03_WATCH_RW, 0
    call AppendMenu ebx, &MF_SEPARATOR, 0, 0
    call AppendMenu ebx, &MF_OWNERDRAW, M03_SORTBYNAME, 0
    call AppendMenu ebx, &MF_OWNERDRAW, M03_SORTBYADDRESS, 0

    call 'USER32.CheckMenuRadioItem' D$DataView.PopupMenu, M03_SORTBYNAME, M03_SORTBYADDRESS,
        M03_SORTBYNAME, &MF_BYCOMMAND
EndP
____________________________________________________________________________________________

Proc DataView_UpdatePopupMenu:
    call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_RW, &MF_UNCHECKED
    call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_W, &MF_UNCHECKED
    call IsWatchPoint D$DataPointer
    If eax = 0011
        call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_RW, &MF_CHECKED
    ElseIf eax = 1
        call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_W, &MF_CHECKED
    EndIf
EndP
____________________________________________________________________________________________

; Process WM_DRAWITEM message for owner-drawn menu items.
;;
Proc DebugDialog_OnDrawMenuItem:
    Arguments @DrawItemStruc
    Local @Brush

    mov ebx D@DrawItemStruc

    mov esi DebugMenuTable
    lodsd | mov ecx eax ; number of entries
    mov eax D$ebx+DRAWITEM_ITEMID
    While D$esi <> eax
        add esi 16
        dec ecx | jz L9>>
    EndWhile

    call 'GDI32.SelectObject' D$ebx+DRAWITEM_HDC, D$DialogFont_handle
    push eax

    Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED
        call 'USER32.GetSysColor' &COLOR_GRAYTEXT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax
        jmp L1>
    Test_Else_If D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
        call 'USER32.GetSysColor' &COLOR_MENUTEXT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax

        call 'USER32.GetSysColor' &COLOR_HIGHLIGHTTEXT
        call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, eax

        call 'USER32.GetSysColorBrush' &COLOR_HIGHLIGHTTEXT
    Test_Else
        call 'USER32.GetSysColor' &COLOR_MENUTEXT
        call 'GDI32.SetTextColor' D$ebx+DRAWITEM_HDC, eax

L1:     call 'USER32.GetSysColor' &COLOR_MENU
        call 'GDI32.SetBkColor' D$ebx+DRAWITEM_HDC, eax

        call 'USER32.GetSysColorBrush' &COLOR_MENU
    Test_End

    lea edx D$ebx+DRAWITEM_RCITEM_LEFT
    call 'USER32.FillRect' D$ebx+DRAWITEM_HDC, edx, eax

    mov eax D$esi+12 ; image index
    If eax <> 0-1
        mov ecx D$ebx+DRAWITEM_RCITEM_LEFT | add ecx 2
        mov edx D$ebx+DRAWITEM_RCITEM_TOP  | add edx 2
        Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_GRAYED
            mov edi &ILD_MASK
        Test_Else_If D$ebx+DRAWITEM_ITEMSTATE &ODS_SELECTED
            mov edi &ILD_NORMAL
        Test_Else
            mov edi &ILD_TRANSPARENT
        Test_End

        Test_If D$ebx+DRAWITEM_ITEMSTATE &ODS_CHECKED
            mov eax 6
        Test_End

L0:     call 'COMCTL32.ImageList_Draw' D$DebugDialog_ImageList, eax, D$ebx+DRAWITEM_HDC, ecx, edx, edi
    EndIf

    add D$ebx+DRAWITEM_RCITEM_LEFT 22

    mov eax D$esi+4
    call DrawMenuItemText D$eax, &DT_LEFT

    mov eax D$esi+8
    If eax <> 0
      ; Draw shortcut rightaligned
        dec D$ebx+DRAWITEM_RCITEM_RIGHT
        call DrawMenuItemTextA eax, &DT_RIGHT
    EndIf

    pop eax
    call 'GDI32.SelectObject' D$ebx+DRAWITEM_HDC, eax

L9: mov eax 1
EndP
;;
____________________________________________________________________________________________

; To allow rightclick inside the list of a combobox we override the window proc of the list.
; The listbox is a child window of the combobox from which the handle is retrieved through
; enumerating.

[ComboListClass: 'ComboLBox' 0] ; consistent among all win OS's ?
[ComboChildClass: B$ ? #32]

Proc EnumComboChilds:
    Arguments @Handle, @Param
    Uses esi, edi

    call 'User32.GetClassNameA' D@Handle, ComboChildClass, 32
    mov edi ComboListClass, esi ComboChildClass, ecx eax
    repe cmpsb | je L1>
    mov eax 1 | ExitP ; continue search

  ; Listbox of the combo
L1: call 'User32.SetWindowLongA' D@Handle, &GWL_WNDPROC, DataView_InterceptRightClick
    mov D$DataView.LBProc eax
    mov eax 0 ; finish search
EndP
____________________________________________________________________________________________

Proc DataView_OverrideComboProc:
    Arguments @DlgHandle

    mov D$DataView.LBProc 0
    call 'USER32.GetDlgItem' D@DlgHandle, DATAVIEW_LABEL_COMBO | mov ebx eax
    call 'USER32.EnumChildWindows' ebx, EnumComboChilds, 0
    If D$DataView.LBProc = 0
        call 'User32.MessageBoxA' 0,
            {'Listbox not found in combo! Please report this problem and your OS version on the board.' 0},
            {'EnumChildWindows' 0}, &MB_OK+&MB_ICONWARNING
    EndIf
EndP

[DataView.LBProc: ?]

Proc DataView_InterceptRightClick:
    Arguments @Handle, @Message, @wParam, @lParam
    Uses ebx esi edi

    If D@Message = &WM_RBUTTONDOWN
      ; Simulate left click (select item) before showing the popup menu
        call 'User32.SendMessageA' D@Handle, &WM_LBUTTONDOWN, D@wParam, D@lParam
        call 'User32.SendMessageA' D@Handle, &WM_LBUTTONUP, D@wParam, D@lParam
      ; Show the popup menu at the position of the mouse-click
        movzx eax W@lParam
        movzx ecx W@lParam+2
        mov D$PointX eax, D$PointY ecx
        call 'USER32.ClientToScreen' D@Handle, Point
        call 'USER32.TrackPopupMenu' D$DataView.PopupMenu, &TPM_LEFTALIGN, D$PointX, D$PointY, 0, D$DataViewHandle, 0
    ElseIf D@Message = &WM_LBUTTONDBLCLK
      ; Show more
        mov eax D$DataPointer | and eax 0_FFFF_F000
        call 'User32.SendMessageA' D$MemoryInspectorHandle, WM_SET_PAGE, eax, D$DataPointer
    EndIf
    call 'User32.CallWindowProcA' D$DataView.LBProc, D@Handle, D@Message, D@wParam, D@lParam
EndP
____________________________________________________________________________________________

; Process WM_INITDIALOG message.

Proc DataViewDialog_OnCreate:
    Arguments @Handle

        call 'USER32.GetClientRect' D@Handle, DebugRect
        move W$DataView.Width W$DebugRect@width
        move W$DataView.Height W$DebugRect@height

        call 'USER32.GetDlgItem' D@Handle, DATAVIEW_LABEL_COMBO
        call DataView_FillDataLabelCombo eax 1

        call DataView_CreatePopupMenu D@Handle
        call DataView_OverrideComboProc D@Handle

        mov eax &TRUE
EndP
____________________________________________________________________________________________

; Process WM_SIZE message.

Proc DataViewDialog_OnSize:
    Arguments @Handle, @WidthHeight

        movzx eax W$DataView.Width
        movzx esi W@WidthHeight
        sub esi eax
        movzx eax W$DataView.Height
        movzx edi W@WidthHeight+2
        sub edi eax
        call AdjustControlSize D@Handle, DATAVIEW_LABEL_COMBO, esi, edi
        call AdjustControlSize D@Handle, DATAVIEW_VALUE_LIST, esi, 0
        call AdjustControlPos D@Handle, DATAVIEW_VALUE_LIST, 0, edi
        call 'USER32.InvalidateRect' D@Handle, &NULL, &TRUE

        move D$DataViewSize D@WidthHeight
        mov eax 0
EndP
____________________________________________________________________________________________

; Process WM_COMMAND message.

Proc DataViewDialog_OnCommand:
    Arguments @Handle, @wParam, @lParam

        movzx ecx W@wParam
        movzx eax W@wParam+2
        .If ecx = DATAVIEW_LABEL_COMBO
            If eax = &CBN_SELCHANGE
                call DataView_OnSelectDataLabel D@Handle, D@lParam
                call DataView_UpdatePopupMenu
            EndIf
        .ElseIf ecx = M03_SHOW_MEM
            ;mov eax D$DataPointer | and eax 0_FFFF_F000
            ;call 'User32.SendMessageA' D$MemoryInspectorHandle, WM_REFRESH_CONTENT, eax, D$DataPointer;0, 0
            mov eax D$DataPointer
            and eax 0_FFFF_F000
            call 'User32.SendMessageA' D$MemoryInspectorHandle, WM_SET_PAGE, eax, D$DataPointer
        .ElseIf ecx = M03_SHOW_PMEM
            mov eax D$DataBuffer
            and eax 0_FFFF_F000
            call 'User32.SendMessageA' D$MemoryInspectorHandle, WM_SET_PAGE, eax, D$DataBuffer
        .ElseIf ecx = M03_SHOW_DECL
            call DataView_ShowDeclaration D@Handle
        .ElseIf ecx = M03_WATCH_W
            call 'USER32.GetMenuState' D$DataView.PopupMenu, M03_WATCH_W, 0
            and eax &MF_CHECKED
            If eax = 0
                call SetWatchPoint D$DataPointer, 4, 1
                call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_W, &MF_CHECKED
            Else
                call DeleteWatchPoint
                call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_W, &MF_CHECKED
            EndIf
            call 'USER32.GetDlgItem' D@Handle, DATAVIEW_LABEL_COMBO
            call 'USER32.InvalidateRect' eax, 0, &FALSE
        .ElseIf ecx = M03_WATCH_RW
            call 'USER32.GetMenuState' D$DataView.PopupMenu, M03_WATCH_RW, 0
            and eax &MF_CHECKED
            If eax = 0
                call SetWatchPoint D$DataPointer, 4, 3
                call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_RW, &MF_CHECKED
            Else
                call DeleteWatchPoint
                call 'USER32.CheckMenuItem' D$DataView.PopupMenu, M03_WATCH_RW, &MF_CHECKED
            EndIf
            call 'USER32.GetDlgItem' D@Handle, DATAVIEW_LABEL_COMBO
            call 'USER32.InvalidateRect' eax, 0, &FALSE
        .ElseIf ecx = M03_SORTBYNAME
            call 'USER32.CheckMenuRadioItem' D$DataView.PopupMenu, M03_SORTBYNAME, M03_SORTBYADDRESS,
                                             M03_SORTBYNAME, &MF_BYCOMMAND
            call DataView_FillDataLabelCombo D$DataLabelComboHandle 1

        .ElseIf ecx = M03_SORTBYADDRESS
            call 'USER32.CheckMenuRadioItem' D$DataView.PopupMenu, M03_SORTBYNAME, M03_SORTBYADDRESS,
                                             M03_SORTBYADDRESS, &MF_BYCOMMAND
            call DataView_FillDataLabelCombo D$DataLabelComboHandle 0
        .EndIf

        mov eax 0
EndP
____________________________________________________________________________________________

[DataViewSize:
 DataView.Width: W$ ?
 DataView.Height: W$ ?]

[DATAVIEW_LABEL_COMBO 50
 DATAVIEW_VALUE_LIST 120]

; Tag Dialog 1011

Proc DataViewProc:
    Arguments @Handle, @Message, @wParam, @lParam
    Uses ebx, esi, edi

    .If D@Message = &WM_INITDIALOG
        call DataViewDialog_OnCreate D@Handle

    .Else_If D@Message = &WM_SIZE
        call DataViewDialog_OnSize D@Handle, D@lParam

    .Else_If D@Message = &WM_COMMAND
        call DataViewDialog_OnCommand D@Handle, D@wParam, D@lParam

    .Else_if D@Message = WM_REFRESH_CONTENT
        call 'USER32.GetDlgItem' D@Handle, DATAVIEW_LABEL_COMBO
        call DataView_OnSelectDataLabel D@Handle, eax

    .Else_if D@Message = WM_SELECT_SYMBOL
        call DataView_SelectSymbol D@Handle, D@wParam

    .Else_if D@Message = &WM_SETFONT
        call 'USER32.GetDlgItem' D@Handle, DATAVIEW_LABEL_COMBO
        call 'User32.SendMessageA' eax, &WM_SETFONT, D@wParam, D@lParam
        call 'USER32.GetDlgItem' D@Handle, DATAVIEW_VALUE_LIST
        call 'User32.SendMessageA' eax, &WM_SETFONT, D@wParam, D@lParam

    .Else_if D@Message = &WM_DRAWITEM
        If D@wParam = 0
            call DebugDialog_OnDrawMenuItem D@lParam
        ElseIf D@wParam = DATAVIEW_LABEL_COMBO
            call DataView_OnDrawItem D@lParam
        Else
            call DataView_OnDrawValueItem D@lParam
        EndIf
        mov eax &TRUE

    .Else_if D@Message = &WM_MEASUREITEM
        mov eax D@lParam
        If D@wParam = 0
          ; menu
            call DebugDialog_OnMeasureMenuItem D@lParam
        Else
          ; listbox
            mov D$eax+16 15
        EndIf
        mov eax &TRUE

    .Else
        mov eax &FALSE
    .EndIf
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; Conversion Routines
;   esi : Address of Register (in Context)
;   ecx : Size in Bytes
; Output
;   edi : Address of Ascii String

[HexSeparator: ?]

toHex:
    mov edi StringBufTail
    mov eax 0, B$edi 0

L0: lodsb
    mov ah al | and ah 0F | shr al 4

    add ah '0'
    On ah > '9', add ah 7
    dec edi | mov B$edi ah

    add al '0'
    On al > '9', add al 7
    dec edi | mov B$edi al

    dec ecx
    jecxz L1>

    test B$HexSeparator cl | jnz L0<
    dec edi | mov B$edi ' '

    jmp L0<

L1: ret

toAscii:
    mov edi StringBuf
    rep movsb
    mov B$edi 0
    mov edi StringBuf
ret
____________________________________________________________________________________________

; Convert dword value to hex-string onto specified buffer
;   ecx : size of value (1 | 2 | 4)
;   edx : source value (dl | dx | edx)
;   edi : address of destination
; Output
;   edi : address behind hex-string
; Uses
;   eax, edx

[DwordToHex | mov ecx 4 | mov edx #1 | call IntToHex]
[WordToHex  | mov ecx 2 | mov  dx #1 | call IntToHex]
[ByteToHex  | mov ecx 1 | mov  dl #1 | call IntToHex]

IntToHex:
    mov eax ecx | add eax eax
    add edi eax
    mov eax 0 ;, B$edi 0

    push edi

L0:     mov al dl | shr edx 8
        mov ah al | and ah 0F | shr al 4

        add ah '0'
        On ah > '9', add ah 7
        dec edi | mov B$edi ah

        add al '0'
        On al > '9', add al 7
        dec edi | mov B$edi al

        dec ecx | jnz L0<

    pop edi
ret
____________________________________________________________________________________________

; Special routine to output meaningful information for segment regs.
; The format is "ssss Base: bbbbbbbb Limit: llllllll"
;   esi: Address of Register
;   edi: Address of Base+Limit
; Output
;   edi: Ascii String

[SegInfo: 'xxxx Base: ' SegInfo.Base: 'xxxxxxxx Limit: ' SegInfo.Limit: 'xxxxxxxx' 0]

toSegHex:
    push edx
        mov ecx 2, dx W$esi
        mov esi edi
        mov edi SegInfo
        call IntToHex

        mov ecx 4, edx D$esi
        mov edi SegInfo.Base
        call IntToHex

        mov ecx 4, edx D$esi+4
        mov edi SegInfo.Limit
        call IntToHex
    pop edx
    mov edi SegInfo
ret
____________________________________________________________________________________________
; esi = datapointer 128 bits =  4 dwords
; ecx = size (in bytes) of the data
Proc toBinarySSE:
    Local @IsTenByte, @TotalLen, @Remainder
    Uses ecx

    mov edi StringBuf

    mov D@TotalLen ecx
    mov eax ecx
    shr eax 2 | shl eax 2 | mov D@Remainder eax | sub ecx eax

    .Test_If ecx 00__0000_0001 ; If ecx = 1 or 3 00__0000_0001  00__0000_0010 00__0000_0011
        dec D@TotalLen | mov eax D@TotalLen
        movzx eax B$esi+eax
        C_call FormatStr edi, {'%1 b ' 0}, eax
        add edi eax
        dec ecx
        mov eax D@TotalLen
    .Test_End

    .Test_If ecx 00__0000_0010 ; ecx = 2
        sub D@TotalLen 2 | mov eax D@TotalLen
        movzx eax W$esi+eax
        C_call FormatStr edi, {'%4 b ' 0}, eax
        add edi eax
        sub ecx 2
        mov eax D@TotalLen
    .Test_End

    mov ecx eax
    .Test_If ecx 0-1 ; If ecx is not zero, do next line
        shr ecx 2 ; divide ecx by 4
        Do
            C_call FormatStr edi, {'% b ' 0}, D$esi+ecx*4-4
            add edi eax
            dec ecx
        Test_Until_Zero ecx
    .Test_End
    mov W$edi-1 0

    mov edi StringBuf

EndP

; esi = datapointer 128 bits =  4 dwords
Proc toBinarySSE_Old:
    Local @IsTenByte
    Uses edx

    mov D@IsTenByte &FALSE
    mov edi StringBuf
    If ecx = 10 ; foun a ttenbyte
        sub ecx 2
        ;mov edx ecx
        mov D@IsTenByte &TRUE
        ;movzx eax W$esi+8
        ;C_call FormatStr edi, {'%4 b ' 0}, eax;D$esi;+ecx*4
        ;add edi eax
        ;add esi 2
        ;mov D@IsTenByte &TRUE
    End_If

    shr ecx 2
    mov edx ecx
    ;mov edi StringBuf
    xor ecx ecx
    Do
        C_call FormatStr edi, {'% b' 0}, D$esi+ecx*4
        add edi eax
        mov B$edi ' '  | inc edi
        inc ecx
    Loop_Until ecx = edx

    If D@IsTenByte = &TRUE
        movzx eax W$esi+8
        C_call FormatStr edi, {'%4 b ' 0}, eax;D$esi;+ecx*4
        add edi eax
    End_If
    mov B$edi-1 0
    mov edi StringBuf

EndP
____________________________________________________________________________________________

Proc toBinary:
    Uses edx

        mov edi StringBufTail, B$edi 0

L0:     mov dl 1
        lodsb

L1:     mov ah '0'
        test al dl | jz L2>
        inc ah
L2:     dec edi | mov B$edi ah
        shl dl 1
        jnc L1<

        dec edi | mov B$edi ' '
        loop L0<
        inc edi
EndP
____________________________________________________________________________________________

Proc toSByte:
    Uses ebx, edx

        mov edi StringBufTail, B$edi 0, ebx 10

L0:     lodsb
        mov dl al
        test dl 0_80 | jz L1>
        neg al

L1:     mov ah 0
        div bl
        add ah '0'
        dec edi | mov B$edi ah
        cmp al 0 | jnz L1<

        test dl 0_80 | jz L2>
        dec edi | mov B$edi '-'

L2:     dec edi | mov B$edi ' '
        loop L0<

        inc edi
EndP

Proc toUByte:
    Uses ebx

        mov edi StringBufTail, B$edi 0, ebx 10

L0:     lodsb

L1:     mov ah 0
        div bl
        add ah '0'
        dec edi | mov B$edi ah
        cmp al 0 | jnz L1<

        dec edi | mov B$edi ' '
        loop L0<

        inc edi
EndP
____________________________________________________________________________________________

Proc toSWord:
    Uses ebx, edx

        mov edi StringBufTail, B$edi 0, ebx 10
        shr ecx 1

L0:     lodsw

        test B$esi-1 0_80 | jz L4>
        neg ax

L4:     movzx eax ax
L1:     mov edx 0
        div ebx
        add dl '0'
        dec edi | mov B$edi dl
        cmp eax 0 | jnz L1<

        test B$esi-1 0_80 | jz L2>
        dec edi | mov B$edi '-'

L2:     dec edi | mov B$edi ' '
        loop L0<

        inc edi
EndP

Proc toUWord:
    Uses ebx, edx

        mov edi StringBufTail, B$edi 0, ebx 10
        shr ecx 1

L0:     lodsw
        movzx eax ax

L1:     mov edx 0
        div ebx
        add dl '0'
        dec edi | mov B$edi dl
        cmp eax 0 | jnz L1<

        dec edi | mov B$edi ' '
        loop L0<

        inc edi
EndP
____________________________________________________________________________________________

Proc toSDword:
    Uses ebx, edx

        mov edi StringBufTail, B$edi 0, ebx 10
        shr ecx 2

L0:     lodsd

        test B$esi-1 0_80 | jz L1>
        neg eax

L1:     mov edx 0
        div ebx
        add dl '0'
        dec edi | mov B$edi dl
        cmp eax 0 | jnz L1<

        test B$esi-1 0_80 | jz L2>
        dec edi | mov B$edi '-'

L2:     dec edi | mov B$edi ' '
        loop L0<

        inc edi
EndP

Proc toUDword:
    Uses ebx, edx

        mov edi StringBufTail, B$edi 0, ebx 10
        shr ecx 2

L0:     lodsd

L1:     mov edx 0
        div ebx
        add dl '0'
        dec edi | mov B$edi dl
        cmp eax 0 | jnz L1<

        dec edi | mov B$edi ' '
        loop L0<

        inc edi
EndP
____________________________________________________________________________________________

Proc toFloat:
    Uses edx

        shr ecx 2 | dec ecx
        mov B$StringBuf 0, edx ecx

L0:     fld D$esi+edx*4 | fstp T$FloatBuf
        mov edi StringBuf, al 0, ecx 255
        repne scasb
        mov B$edi-1 ' '
        call FloatToUString FloatBuf, edi, DEBUG_ROUND_MODE, &TRUE

        dec edx | jns L0<

        mov edi StringBuf | inc edi
EndP

Proc toDouble:
    Uses edx

        shr ecx 3 | dec ecx
        mov B$StringBuf 0, edx ecx

L0:     fld R$esi+edx*8 | fstp T$FloatBuf
        mov edi StringBuf, al 0, ecx 255
        repne scasb
        mov B$edi-1 ' '
        call FloatToUString FloatBuf edi, DEBUG_ROUND_MODE, &TRUE

        dec edx | jns L0<

        mov edi StringBuf | inc edi
EndP

toExtended:
    mov edi StringBuf
    call FloatToUString esi, edi, DEBUG_ROUND_MODE, &TRUE
ret

[StringBuf: B$ ? #255 StringBufTail: B$ ?]
[FloatBuf: T$ ?]
____________________________________________________________________________________________

toHexWithAnsi:
    push esi, ecx
L0:     lodsb
        mov ah al | and ah 0F | shr al 4
        add al '0'
        On al > '9', add al 7
        mov B$edi al | inc edi
        add ah '0'
        On ah > '9', add ah 7
        mov B$edi ah | inc edi
        mov B$edi ' ' | inc edi
        loop L0<
        mov al '"' | stosb
    pop ecx, esi
L0: lodsb
    If al < ' '
        mov al '.'
    ElseIf al = '&'
        stosb
    EndIf
    stosb
    loop L0<
    mov al '"' | stosb
    mov B$edi 0
ret
____________________________________________________________________________________________

[InversedLowSigns: B$ '.|| ..^/*-+.)(:$.][][,}.{##."...']

toHexWithCookedAnsi:
    push esi, ecx
L0:     lodsb
        mov ah al | and ah 0F | shr al 4
        add al '0'
        On al > '9', add al 7
        mov B$edi al | inc edi
        add ah '0'
        On ah > '9', add ah 7
        mov B$edi ah | inc edi
        mov B$edi ' ' | inc edi
        loop L0<
        mov al '"' | stosb
    pop ecx, esi

    xor eax eax

L0: lodsb
    On al < ' ', mov al B$InversedLowSigns+eax
    and al 07F
    stosb
    On al = '&', stosb
    loop L0<

    mov al '"' | stosb
    mov B$edi 0
ret

____________________________________________________________________________________________

toHexDwords:
    mov edx ecx
L1:     mov ecx 4
L0:         lodsb
            mov ah al | and ah 0F | shr al 4
            add ah '0'
            On ah > '9', add ah 7
            mov B$edi+ecx*2-1 ah
            add al '0'
            On al > '9', add al 7
            mov B$edi+ecx*2-2 al
        loop L0<
        mov B$edi+8 ' '
        add edi 9
        sub edx 4
    jnz L1<
    mov B$edi-1 0
ret
____________________________________________________________________________________________

toHexWords:
    mov edx ecx
L1:     mov ecx 2
L0:         lodsb
            mov ah al | and ah 0F | shr al 4
            add ah '0'
            On ah > '9', add ah 7
            mov B$edi+ecx*2-1 ah
            add al '0'
            On al > '9', add al 7
            mov B$edi+ecx*2-2 al
        loop L0<
        mov B$edi+4 ' '
        add edi 5
        sub edx 2
    jnz L1<
    mov B$edi-1 0
ret
____________________________________________________________________________________________

Proc toFloats:
    Uses edx

        shr ecx 2
        mov edx ecx
        mov ecx 0-1

        While edx > 0
            fld D$esi | fstp T$FloatBuf
            call FloatToUString FloatBuf edi, DEBUG_ROUND_MODE, &TRUE
            add esi 4
            mov al 0, ecx 255
            repne scasb
            mov B$edi-1 ' '
            dec edx
        EndWhile
        mov B$edi-1 0
EndP
____________________________________________________________________________________________

Proc toDoubles:
    Uses edx

        ;shr ecx 4
        ;mov edx ecx
        ;mov ecx 0-1

        ;While edx > 0
            fld R$esi | fstp T$FloatBuf
            call FloatToUString FloatBuf edi, DEBUG_ROUND_MODE, &TRUE
            add esi 8
            mov al 0, ecx 255
            repne scasb
            mov B$edi-1 ' '
         ;   dec edx
        ;EndWhile
        mov B$edi-1 0
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

DebuggerMOUSEHINT: ; Show a tooltip(-like) window.
____________________________________________________________________________________________
____________________________________________________________________________________________

; Show a hint when the user holds the mouse over a datalabel in the source-editor

[MouseHintTimer:    ?
 MouseHintVisible:  ?]

[MOUSE_HINT_TIMER_ID 2]

InitMouseOverDataHints:

    call CreateNewForm_MouseHint
    call 'USER32.SetTimer' &NULL, MOUSE_HINT_TIMER_ID, 200, MouseOverDataHint
    mov D$MouseHintTimer eax
ret

DeleteMouseOverDataHints:
    call 'USER32.KillTimer' &NULL, D$MouseHintTimer
    mov D$MouseHintTimer 0
    call 'USER32.DestroyWindow' D$MouseHintFormHandle
    mov D$MouseHintFormHandle 0
ret
___________________________________________________________________

; see ScanLabelListForCodeLabel
[Dbg_CurPT:
 Dbg_CurPT.x: D$ 0
 Dbg_CurPT.y: D$ 0]

[WINPOS:
 WINPOS.cx: D$ 0
 WINPOS.cy: D$ 0
 WINPOS.width: D$ 0
 WINPOS.height: D$ 0]

[MouseHint_Buffer: B$ 0 #256]

Proc MouseOverDataHint:
    Arguments @Handle, @Msg, @id, @Time
    Local @Column, @Row, @Size, @Address
    Uses ebx, esi, edi

    On D$IsDebugging = 0, jmp @Invalid

    ; Clear Points 1st
    mov D$Dbg_CurPT.x 0
    mov D$Dbg_CurPT.y 0
    mov D$WINPOS.cx 0
    mov D$WINPOS.cy 0
    mov D$WINPOS.width 0
    mov D$WINPOS.height 0

    call GetWindowPos D$EditWindowHandle, WINPOS
    ; GetEditWindowClientRectangle or RightClickOnBlock
    ; Check if mouse is inside client space
    call 'USER32.GetCursorPos' Dbg_CurPT
    call 'USER32.ScreenToClient' D$EditWindowHandle, Dbg_CurPT
    mov eax D$Dbg_CurPT.x
    mov ebx D$Dbg_CurPT.y
    If_And eax => 0, ebx => 0, eax <= D$WINPOS.width, ebx <= D$WINPOS.height
        mov W$MousePosX ax
        mov W$MousePosY bx
    Else
        jmp @Invalid
    End_If
;;
    If D$ToolbarWanted = 1
        sub ebx D$ToolBarPixelsHight
        mov W$MousePosY bx
    End_If
;;
    call SimpleMouseTextPos

    mov D@Column eax, D@Row ebx

    push D$CaretRow
        call SearchTxtPtr
    pop D$CaretRow

    mov esi eax

    call MouseHint_ScanForSeparator esi, MouseHint_Buffer
    On eax = 0-1, jmp @Invalid
    Do
        call MouseHint_LookupEquates MouseHint_Buffer
    Loop_Until eax = 0
    call MouseHint_FixStatements MouseHint_Buffer
    On eax = 0, jmp @Invalid
    call MouseHint_Evaluate MouseHint_Buffer
    On eax = 0, jmp @Invalid

    ; set the new cursor while the mousehint is over the data
   ; call 'USER32.SetCursor' D$Bp_hCursor

    call RowToX D@Column | mov D$Dbg_CurPT.x eax
    call LineToY D@Row   | add eax D$FontHeight
    ;On B$ToolbarWanted = 1, add eax D$ToolBarPixelsHight
    mov D$Dbg_CurPT.y eax
    call 'USER32.ClientToScreen' D$EditWindowHandle, Dbg_CurPT

    call 'USER32.InvalidateRect' D$MouseHintFormHandle, 0, 1
    call 'USER32.SetWindowPos' D$MouseHintFormHandle, &HWND_TOP, D$Dbg_CurPT.x, D$Dbg_CurPT.y, 0, 0, &SWP_NOSIZE+&SWP_SHOWWINDOW+&SWP_NOACTIVATE;+&SWP_NOZORDER

    mov D$MouseHintVisible 1
    ExitP

@Invalid:

    If D$MouseHintVisible = 1
        call 'USER32.SetWindowPos' D$MouseHintFormHandle, &HWND_TOP, 0, 0, 0, 0, &SWP_HIDEWINDOW+&SWP_NOMOVE+&SWP_NOSIZE+&SWP_NOACTIVATE;+&SWP_NOZORDER
        mov D$MouseHintVisible 0
    End_If

EndP

____________________________________________________________________________________________
;;
    GetWindowPos v 1.1
   
    This functions retrieves the x, y, width and heigth of any window.

    Arguments:
   
        hWnd(in): A handle to the window. The window can be a child, overlapped, a dialog a control etc.
        PosStruct(out): A pointer to a WINPOS structure to rceive the values found.

    Returned Value: If the window is a child window, the return value is a handle to the parent window. If the window is a top-level window with the WS_POPUP style, the return value is a handle to the owner window.
                    If the function fails, the return value is NULL. To get extended error information, call GetLastError.
                    This function typically fails for one of the following reasons:
                        *The window is a top-level window that is unowned or does not have the WS_POPUP style.
                        •The owner window has WS_POPUP style.

    Remarks:
        The WINPOS structure have the following format and specifications:

        [WINPOS:
         WINPOS.cx: D$ 0 ; The initial horizontal position of the window. For an overlapped or pop-up window, the x parameter is
                         ; the initial x-coordinate of the window's upper-left corner, in screen coordinates. For a child window,
                         ; x is the x-coordinate of the upper-left corner of the window relative to the upper-left corner of the
                         ; parent window's client area.

         WINPOS.cy: D$ 0 ; The initial vertical position of the window. For an overlapped or pop-up window, the y parameter is
                         ; the initial y-coordinate of the window's upper-left corner, in screen coordinates. For a child window,
                         ; y is the initial y-coordinate of the upper-left corner of the child window relative to the upper-left
                         ; corner of the parent window's client area. For a list box y is the initial y-coordinate of the upper-left
                         ; corner of the list box's client area relative to the upper-left corner of the parent window's client area.


         WINPOS.width: D$ 0 ; The width, in device units, of the window.
         WINPOS.height: D$ 0; The height, in device units, of the window.
         ]       
       

    Usage example:

    [WINPOS:
     WINPOS.cx: D$ 0
     WINPOS.cy: D$ 0
     WINPOS.width: D$ 0
     WINPOS.height: D$ 0]
   
   
    call 'USER32.GetDlgItem' D@hWnd, 156
    call GetWindowPos eax, WINPOS


    Author: Gustavo Trigueiros (aka: Beyond2000!)
    Build Date: 19/05/2012 (v 1.0)

;;

[WINPOS.cxDis 0
 WINPOS.cyDis 4
 WINPOS.widthDis 8
 WINPOS.heightDis 12]

[Size_Of_WINPOS 16]

Proc GetWindowPos:
    Arguments @hWnd, @PosStruct
    Local @width, @height, @hParent
    Structure @WINDOWINFO 64, @WINDOWINFO.cbSizeDis 0,  @WINDOWINFO.rcWindow_leftDis 4,  @WINDOWINFO.rcWindow_topDis 8,
                              @WINDOWINFO.rcWindow_rightDis 12,  @WINDOWINFO.rcWindow_bottomDis 16,  @WINDOWINFO.rcClient_leftDis 20,
                              @WINDOWINFO.rcClient_topDis 24,  @WINDOWINFO.rcClient_rightDis 28,  @WINDOWINFO.rcClient_bottomDis 32,
                              @WINDOWINFO.dwStyleDis 36,  @WINDOWINFO.dwExStyleDis 40,  @WINDOWINFO.dwWindowStatusDis 44,  @WINDOWINFO.cxWindowBordersDis 48,
                              @WINDOWINFO.cyWindowBordersDis 52,  @WINDOWINFO.atomWindowTypeDis 56,  @WINDOWINFO.wCreatorVersionDis 60
    Uses esi, ecx, edx

    call 'user32.GetAncestor' D@hWnd, &GA_PARENT
    On eax = 0, ExitP
    mov D@hParent eax
    call 'USER32.MapWindowPoints' D@hWnd, eax, D@PosStruct, 2

    call 'RosMem.FastZeroMem' D@WINDOWINFO, 64
    mov D@WINDOWINFO.cbSizeDis 64
    call 'USER32.GetWindowInfo' D@hWnd, D@WINDOWINFO
    mov esi D@PosStruct
    mov eax D@WINDOWINFO.rcWindow_rightDis | sub eax D@WINDOWINFO.rcWindow_leftDis | mov D$esi+WINPOS.widthDis eax
    mov eax D@WINDOWINFO.rcWindow_bottomDis | sub eax D@WINDOWINFO.rcWindow_topDis | mov D$esi+WINPOS.heightDis eax

    mov eax D@WINDOWINFO.cxWindowBordersDis | sub D$esi+WINPOS.cxDis eax
    mov eax D@WINDOWINFO.cyWindowBordersDis | sub D$esi+WINPOS.cyDis eax

    mov eax D@hParent

EndP


____________________________________________________________________________________________

; WNDCLASSEX structure

[WNDCLASSEX.cbSizeDis 0
 WNDCLASSEX.styleDis 4
 WNDCLASSEX.lpfnWndProcDis 8
 WNDCLASSEX.cbClsExtraDis 12
 WNDCLASSEX.cbWndExtraDis 16
 WNDCLASSEX.hInstanceDis 20
 WNDCLASSEX.hIconDis 24
 WNDCLASSEX.hCursorDis 28
 WNDCLASSEX.hbrBackgroundDis 32
 WNDCLASSEX.lpszMenuNameDis 36
 WNDCLASSEX.lpszClassNameDis 40
 WNDCLASSEX.hIconSmDis 44]

[Size_Of_WNDCLASSEX 48]

[MouseHintFormHandle: D$ 0]

[MouseHintFormClassName: B$ "MouseHintForm" 0]

[MOUSE_HINT_WIDTH 210]

Proc CreateNewForm_MouseHint:
    Structure @wc 48, @wc.cbSizeDis 0, @wc.styleDis 4, @wc.lpfnWndProcDis 8, @wc.cbClsExtraDis 12,
                      @wc.cbWndExtraDis 16, @wc.hInstanceDis 20, @wc.hIconDis 24, @wc.hCursorDis 28,
                      @wc.hbrBackgroundDis 32, @wc.lpszMenuNameDis 36, @wc.lpszClassNameDis 40, @wc.hIconSmDis 44

    call 'RosMem.FastZeroMem' D@wc, Size_Of_WNDCLASSEX

    mov D@wc.cbSizeDis Size_Of_WNDCLASSEX
    mov D@wc.lpfnWndProcDis MouseHintFormProc
    mov D@wc.lpszClassNameDis MouseHintFormClassName
    ;call 'USER32.LoadCursorA' &NULL, &IDC_ARROW | mov D@wc.hCursorDis eax
    ;mov D@wc.hbrBackgroundDis &COLOR_BACKGROUND
    call 'GDI32.GetStockObject' &WHITE_BRUSH | mov D@wc.hbrBackgroundDis eax

    call 'User32.RegisterClassExA' D@wc
    imul eax D$FontHeight 5;4
    call 'User32.CreateWindowExA' &WS_EX_LEFT+&WS_EX_LTRREADING+&WS_EX_RIGHTSCROLLBAR,
                                  MouseHintFormClassName, 0,
                                  &WS_BORDER+&WS_OVERLAPPED+&WS_POPUP,
                                  0, 0, MOUSE_HINT_WIDTH, eax,
                                  D$hwnd,
                                  0,
                                  D$hInstance,
                                  &NULL
    mov D$MouseHintFormHandle eax
    call 'User32.SendMessageA' D$MouseHintFormHandle, &WM_SETFONT, D$DebugFontHandle, &TRUE

EndP
____________________________________________________________________________________________

;[MH_Rect: D$ ? #4]

Proc MouseHintFormProc:
    Arguments @handle @Message @wParam @lParam
    Structure @PaintStruct 64 @hdc 0

    pushad

    .If D@Message = &WM_CREATE
       ; mov eax 0

    .Else_If D@Message = &WM_CLOSE
        call 'USER32.DestroyWindow' D@handle

    .Else_If D@Message = &WM_DESTROY
       ; mov eax 0

    .Else_If D@Message = &WM_PAINT
        If D$IsDebugging = 1
            call 'User32.BeginPaint' D@handle, D@PaintStruct
            call 'GDI32.SelectObject' D@hdc, D$DebugFontHandle
            call MouseHintDrawWindow D@handle, D@hdc
            call 'User32.EndPaint' D@handle D@PaintStruct
        End_If
    .Else
        popad
        call 'User32.DefWindowProcA' D@handle D@Message D@wParam D@lParam
        ExitP
    .End_If

    popad
    mov eax &FALSE

EndP
; Tag End
____________________________________________________________________________________________

; see DataView_OnSelectDataLabel ScanLabelListForCodeLabel EvaluateDataSymbol
[DEBUG_ROUND_MODE 0]

[Dgb_Line1: B$ ? #24
 Dgb_Line2: B$ ? #24
 Dgb_Line3: B$ ? #64
 Dgb_Line4: B$ ? #64]

Proc MouseHintDrawWindow:
    Arguments @handle, @hdc
    Local @Brush, @PointerAddress, @IsValidSring
    Structure @Rect 16 @left 0 @top 4 @right 8 @bottom 12

    mov D@IsValidSring 0
    call 'USER32.GetClientRect' D@handle, D@Rect
    move D@bottom D$FontHeight

    call 'GDI32.CreateSolidBrush' 0_FF_FF_FF | mov D@Brush eax
    call 'USER32.FillRect' D@hdc, D@Rect, D@Brush
    call 'GDI32.DeleteObject' D@Brush

    call 'GDI32.SetBkColor' D@hdc, 0_FF_FF_FF

    call 'GDI32.SetTextColor' D@hdc, 0_33_33_33

  ; Output size
    mov edi ItemString
    mov al ' ' | stosb
    mov al B$MouseHint_SizeMarker
    If al = 'D'
        stosb | mov eax 'WORD' | stosd
    ElseIf al = 'W'
        mov eax 'WORD' | stosd
    ElseIf al = 'B'
        mov eax 'BYTE' | stosd
    ElseIf al = 'F'
        stosb | mov eax 'LOAT' | stosd
    ElseIf al = 'R'
        mov eax 'REAL' | stosd
    ElseIf al = 'T'
        mov eax 'EXTE' | stosd
        mov eax 'NDED' | stosd
    Else_If al = 'X'
        mov eax 'SSE_' | stosd
        mov eax 'EXTE' | stosd
        mov eax 'NDED' | stosd
    EndIf
    mov B$edi 0

    call 'USER32.DrawTextA' D@hdc, ItemString, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_LEFT+&DT_VCENTER

    move D@PointerAddress D$MouseHint_Address

  ; Output address
    mov edi ItemString
    DwordToHex D@PointerAddress
    mov al ' ' | stosb
    mov B$edi 0

    call 'USER32.DrawTextA' D@hdc, ItemString, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_RIGHT+&DT_VCENTER

  ; Draw background for values
    mov eax D$FontHeight
    ;lea edx D$eax*4
    mov edx eax | imul edx 5
    move D@Top eax, D@Bottom edx

    call 'GDI32.CreateSolidBrush' 0_DD_EE_EE | mov D@Brush eax
    call 'USER32.FillRect' D@hdc, D@Rect, D@Brush
    call 'GDI32.DeleteObject' D@Brush
    call 'GDI32.SetBkColor' D@hdc, 0_DD_EE_EE

    If D$DebugMouseHint_OnDataSymbol = &TRUE
        mov eax D@PointerAddress
        sub eax D$DebugProcessData.DllImageBase | add eax D$DebugProcessData.DllNewImageBase
        mov D@PointerAddress eax
        mov D$DebugMouseHint_OnDataSymbol &FALSE ; reset the flag after used
    End_If

;;
    ; See if the process rebased the ponter. If it did, then calculate the real address
    If_And D$DebugProcessData.Type = DEBUG_DLL, D$DebugProcessData.IsRebased = &TRUE
        mov eax D$MouseHint_Address
        sub eax D$DebugProcessData.DllImageBase | add eax D$DebugProcessData.DllNewImageBase
    End_If
    mov D$MouseHint_Address eax
;;

  ; Read a block of memory with a max size of 64 byte
    call IsProcessMemory D@PointerAddress
    On eax = 0, jmp @Invalid
    mov ebx 64
    On eax < ebx, mov ebx eax
    call ReadProcessMem D@PointerAddress, DataBuffer, ebx

  ; Output value based on size selector (D$ / W$ / ...)
    mov al B$MouseHint_SizeMarker
    mov B$Dgb_Line1 0, B$Dgb_Line2 0, B$Dgb_Line3 0
    .If al = 'D'
L0:     On ebx < 4, jmp @Invalid
        mov edi Dgb_Line1
        DwordToHex D$DataBuffer
        mov B$edi 0

        mov esi DataBuffer, ecx 4
        call toUDword
        mov esi edi, edi Dgb_Line2
        Do | movsb | LoopUntil B$esi-1 = 0

        mov esi DataBuffer, ecx 4
        call toSDword
        mov esi edi, edi Dgb_Line3
        Do | movsb | LoopUntil B$esi-1 = 0

        call IsString DataBuffer 64
        If ah = 1
            mov edi Dgb_Line4
            C_call FormatStr edi, {'%s' 0}, DataBuffer
            mov D@IsValidSring &TRUE
        End_If
        ; check if the value is an address to the nodule. On this case, it caanot be a string
        call GetModuleFromAddress D$DataBuffer, D$ModuleList, D$NumModules
        If eax <> 0
            mov eax D$eax+ME_Name
            ;call GetModuleName eax, D$ModuleList, D$NumModules
            mov edi Dgb_Line4
            C_call FormatStr edi, {'%s' 0}, eax
            mov D@IsValidSring &TRUE
        End_If


    .ElseIf al = 'W'
        ; Word Size
        On ebx < 2, jmp @Invalid
        mov edi Dgb_Line1
        WordToHex W$DataBuffer
        mov B$edi 0

        mov esi DataBuffer, ecx 2
        call toUword
        mov esi edi, edi Dgb_Line2
        Do | movsb | LoopUntil B$esi-1 = 0

        mov esi DataBuffer, ecx 2
        call toSword
        mov esi edi, edi Dgb_Line3
        Do | movsb | LoopUntil B$esi-1 = 0

    .ElseIf al = 'B'
        ; Byte size
        mov edi Dgb_Line1
        ByteToHex B$DataBuffer
        mov B$edi 0

        mov esi DataBuffer, ecx 1
        call toUByte
        mov esi edi, edi Dgb_Line2
        Do | movsb | LoopUntil B$esi-1 = 0

        mov esi DataBuffer, ecx 1
        call toSByte
        mov esi edi, edi Dgb_Line3
        Do | movsb | LoopUntil B$esi-1 = 0

        call IsString DataBuffer 64
        If ah = 1
            mov edi Dgb_Line4
            C_call FormatStr edi, {'%s' 0}, DataBuffer
            mov D@IsValidSring &TRUE
        End_If

    .ElseIf al = 'F'
        On ebx < 4, jmp @Invalid
        mov edi Dgb_Line2
        fld F$DataBuffer | fstp T$FloatBuf
        call FloatToUString FloatBuf, edi, DEBUG_ROUND_MODE, &TRUE
        mov al 0, ecx 0-1 | repne scasb
    .ElseIf al = 'R'
        On ebx < 8, jmp @Invalid
        mov edi Dgb_Line2
        fld R$DataBuffer | fstp T$FloatBuf
        call FloatToUString FloatBuf, edi, DEBUG_ROUND_MODE, &TRUE
        mov al 0, ecx 0-1 | repne scasb
    .ElseIf al = 'T'
        On ebx < 10, jmp @Invalid
        mov edi Dgb_Line2
        call FloatToUString DataBuffer, edi, DEBUG_ROUND_MODE, &TRUE
        mov al 0, ecx 0-1 | repne scasb
    .Else_If al = 'X'

        mov edi Dgb_Line1
        C_call FormatStr edi, {'%s' 0}, { B$ "1st two Dwords", 0}

        mov edi Dgb_Line2 | mov D$edi 'Dwor' | mov W$edi+4 'd ' | add edi 6
        DwordToHex D$DataBuffer
        ;mov B$edi 0
        mov B$edi ' ' | inc edi
        DwordToHex D$DataBuffer+4
        mov B$edi 0

        mov edi Dgb_Line3  | mov D$edi 'Floa' | mov W$edi+4 't ' | add edi 6
        fld F$DataBuffer | fstp T$FloatBuf
        call FloatToUString FloatBuf edi, DEBUG_ROUND_MODE, &TRUE
        call StrLenProc edi
        ; truncate edi with a pace and convert again to float the next value
        ; Why 9 ? 24 (Max size)-6(size of "float " string) = 18. So, we take the half of it
        If eax <= 9
            mov B$edi+eax ' '
        Else
            mov eax 9
        End_If
        ;On eax <= 9, mov B$edi+eax ' '
        add edi eax
        ;add edi 7 | mov B$edi ' '
        mov ebx eax
        mov ecx 9 | sub ecx eax | cdq
        mov al ' ' | rep stosb

        mov edi Dgb_Line3 | add edi 15 | mov W$edi '  ' | add edi 2

        ;mov al ' '
        ;Do | stosb | LoopUntil B$esi-1 = 0

        fld F$DataBuffer+4 | fstp T$FloatBuf
        call FloatToUString FloatBuf, edi, DEBUG_ROUND_MODE, &TRUE
        ;On eax > 9, mov B$edi+9 0 ; truncate to zero in any case

    .EndIf

    mov eax D$FontHeight
    lea eax D$eax*2
    mov D@bottom eax
    call 'USER32.DrawTextA' D@hdc, Dgb_Line1, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER

    mov eax D$FontHeight
    add D@top eax
    add D@bottom eax
    call 'USER32.DrawTextA' D@hdc, Dgb_Line2, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER

    mov eax D$FontHeight
    add D@top eax
    add D@bottom eax
    call 'USER32.DrawTextA' D@hdc, Dgb_Line3, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER

    If D@IsValidSring = &TRUE
        mov eax D$FontHeight
        add D@top eax
        add D@bottom eax
        call 'USER32.DrawTextA' D@hdc, Dgb_Line4, 0-1, D@Rect,
                                &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER
    End_If

    ExitP

@Invalid:
    mov eax D$FontHeight
    lea edx D$eax*4
    move D@Top eax, D@Bottom edx
    mov eax {'Invalid address' 0}

    call 'USER32.DrawTextA' D@hdc, eax, 0-1, D@Rect,
                            &DT_SINGLELINE+&DT_CENTER+&DT_VCENTER
EndP
____________________________________________________________________________________________

[DebugMouseHint_OnDataSymbol: D$ 0] ; &TRUE or &FALSE. This flag is used when you
                                    ; are debuging a dll and the adress is rebased.
                                    ; When rebasing happens the mousehover control lost the real address
                                    ; so we need to fix it accordyly
                                    ; See: ScanLabelListForCodeLabel , DataView_OnSelectDataLabel, MouseHintDrawWindow
;[DebugMouseHint_RebasedAddress: D$ 0] ; and this is the rebased address to be showend

;MouseHint:
____________________________________________________________________________________________

; Get address of data symbol
; Input  :  esi -> name, ecx = len
; Output :  eax = value, edx = valid(1) invalid(0)

Proc MouseHint_EvaluateDataSymbol:
    Arguments @Input, @StringLenght
    Uses ecx, esi, ebx

    mov esi D@Input
    mov ecx D@StringLenght

    ; Search address of data label (reuse information already stored in the label combo)
    mov D$DebugMouseHint_OnDataSymbol &FALSE ; ALWAYS INITIALIZE IT TO FALSE
    mov edx 0
    On D$DataLabelComboHandle = 0, ExitP
    mov bl B$esi+ecx, B$esi+ecx 0 ; find routine requires zero terminated string!
    push ecx
        call 'User32.SendMessageA' D$DataLabelComboHandle, &CB_FINDSTRINGEXACT, ecx, esi
    pop ecx
    mov B$esi+ecx bl
    mov edx 0
    On eax = &CB_ERR, ExitP
    call 'User32.SendMessageA' D$DataLabelComboHandle, &CB_GETITEMDATA, eax, 0
    ; If we are debugging a dll and it was rebased, we need to retrieve his original address to display on the mousehint
    If_And D$DebugProcessData.Type = DEBUG_DLL, D$DebugProcessData.IsRebased = &TRUE
        mov D$DebugMouseHint_OnDataSymbol &TRUE
    End_If
    mov edx 1

EndP
____________________________________________________________________________________________

; Translate number representations
; Input  :  esi -> name, ecx = len
; Output :  eax = value, edx = valid(1) invalid(0)

Proc MouseHint_EvaluateNumber:
    Arguments @Input;, @IsFloat
    Uses esi, ecx, ebx, edi

    ;mov D$OldStackPointer esp
    ;Ros_SaveStack

    mov esi D@Input
    mov edx 0

    On B$esi < '0', ExitP
    On B$esi > '9', ExitP

    ; 1st we see if the number is a float
    mov ecx esi
    While B$ecx > LowSigns
        inc ecx
        If B$ecx = '.' ; possible float found
            inc ecx
            ExitP ; at the moment we are not dealing with floats
        End_If
    End_While

    If W$esi = '00'
        call TranslateBinary
    ElseIf B$esi = '0'
        call TranslateHexa
    Else
        call TranslateDecimal
    EndIf

    mov edx 1

EndP

____________________________________________________________________________________________

; Get value of register
; Input  :  esi -> name, ecx = len
; Output :  eax = value, edx = valid(1) invalid(0)

Proc MouseHint_EvaluateRegister:
    Arguments @Input, @TokenLenght
    Uses esi, ecx

    mov esi D@Input
    mov ecx D@TokenLenght

    mov edx 0
    On B$esi <> 'E', ExitP
    On ecx <> 3, ExitP

    mov eax D$esi
    and eax 0FF_FFFF
    ;mov edi GPRegs
    mov ecx 0

    While ecx < 8
        ...If D$GPRegs+ecx*4 = eax
            mov eax D$GPRRegMap+ecx*4
            mov eax D$eax
            mov edx 1
            ExitP
        ...EndIf
        inc ecx
    End_While

    mov edx 0

EndP
____________________________________________________________________________________________

Proc MouseHint_EvaluateSegmentSelector:
    Arguments @Input, @Stringlen
    Uses esi, ecx

    mov esi D@Input
    mov ecx D@Stringlen
    mov edx 0
    On ecx <> 2, ExitP
    On B$esi+1 <> 'S', ExitP

    mov edx 1
    If B$esi = 'F'
        mov eax D$FS.Linear
    Else_If B$esi = 'D'
        mov eax D$DS.Linear
    Else_If B$esi = 'C'
        mov eax D$CS.Linear
    Else_If B$esi = 'E'
        mov eax D$ES.Linear
    Else_If B$esi = 'G'
        mov eax D$GS.Linear
    Else_If B$esi = 'S'
        mov eax D$SS.Linear
    Else
        xor edx edx
    End_If

EndP
____________________________________________________________________________________________

; Get value of string token
; Input  :  esi -> name, ecx = len
; Output :  eax = value, edx = success (1) failed (0)
; pTokenType = Type of token. 1 = register, 2 = number, 3 = datasymbol
Proc MouseHint_EvaluateToken:
    Arguments @InputName, @Stringlen, @pTokenType
    Uses esi, edi, ebx, ecx

    mov edi D@pTokenType
    call MouseHint_EvaluateRegister D@InputName, D@Stringlen
    If edx = 1
        mov D$edi DBG_PARAM_TYPE_REG | ExitP
    End_If

    mov ecx D@Stringlen
    mov esi D@InputName | add esi ecx | movzx ebx B$esi | mov B$esi 0 ; change it to null terminated string
    call MouseHint_EvaluateNumber D@InputName
    If edx = 1
        mov B$esi bl ; restore previous byte
        mov D$edi DBG_PARAM_TYPE_NUMBER | ExitP
    End_If
    mov B$esi bl ; restore previous byte

    call MouseHint_EvaluateDataSymbol D@InputName, D@Stringlen
    If edx = 1
        mov D$edi DBG_PARAM_TYPE_DATA_SYMBOL
    End_If

EndP
____________________________________________________________________________________________

;;
    The parser can identify all sort of statements regardless the order of the registers, variables or equates.
    Such as:
    
    
    movups XMM2 X$esi+ebx*8;(PIX_WIDTH*4*2) ; 5*4*2
    movups XMM2 X$ebx*8+esi;(PIX_WIDTH*4*2) ; 5*4*2
    movups XMM2 X$ebx*8
    mov D$ValueA4+()+() eax
    mov D$ValueA4+(5* (guga shl 2) + 18)+(5) eax
    mov D$ValueA4+(5* (guga shl 2) + 18)+5 eax
    mov eax D$ValueA4+(5 shl 2)
    mov D$guga+ValueA4 edx
    movups xmm1 X$esi+(16+(PIX_WIDTH*4*1))
    mov eax D$ValueA4+(5* (guga shl 2) + 18)+(5)
    mov eax D$ValueA4+(5* (guga shl 2) + 18)
    mov eax D$ValueA4+(guga * 2)
    mov ebx D@Width

    It also tries to identifyempty statements, such as ones that end with extra spaces, separators or comments.
    
    Ex:
        When we find a code like this:
            mov eax D$ValueA4+(    ) | ;
        The parser will identify it as:
            mov eax D$ValueA4

    See: MouseHint_ScanForSeparator  MouseHint_LookupEquates

    Note: 
    RosAsm contains a error on asembling some weird sources.
    For example, whenever we have a code containing different expressions, a memoryto reg is only allowed for positive values
    So, we can assemble correctly instructions as:

        mov D$ValueA4+ecx eax
    
    However, there is a lack of an error messages when the instruction is negative. I mea, the 2nd statement is a register
    Such as:
        mov D$ValueA4-ecx eax ; compiled but without a error or warning.
        The above is, in fact, compiled as: D$ValueA4+ecx (Wich is correct, however an error mesage should show up)

    The same for
        mov D$ValueA4*ecx eax ; compiled but without a error or warning. Compíled as D$ValueA4+ecx
        The above is, in fact, compiled as: D$ValueA4+ecx (Wich is correct, however an error mesage should show up)

    Immediate values are ok. No errors whatsoever. Ex:
        and D$ValueA4-020 ValueA4-4 ; compile ok
        and D$ValueA4+020 ValueA4-4 ; compile ok
        mov D$ValueA4-020 eax
        mov D$ValueA4+020 eax

    But...for immediates, if we invert the order, we have an error on encodage
        mov D$020+ValueA4 eax
        mov D$020-ValueA4 eax
    Both encode as:
        mov D$ValueA4+020 eax
    So, we need another error message
    
    Also. it was assemblying weird thigs like
    mov D$+020+ValueA4 eax
    mov D$-020+ValueA4 eax (In fact it is assembled as mov D$ValueA4-020 eax but an error message show popued up here

mov D$ValueA4-ecx eax ; compiled but without a error or warning. Compíled as D$ValueA4+ecx
mov D$ValueA4+ecx eax

and D$ValueA4-020 ValueA4-4 ; compile ok
and D$ValueA4+020 ValueA4-4 ; compile ok

mov D$ValueA4-020 eax
mov D$ValueA4+020 eax


mov D$ValueA4-ecx eax ; compiled but without a error or warning. Compíled as D$ValueA4+ecx
mov D$ValueA4+ecx eax
mov D$ValueA4*ecx eax ; compiled but without a error or warning. Compíled as D$ValueA4+ecx

and D$ValueA4-020 ValueA4-4 ; compile ok
and D$ValueA4+020 ValueA4-4 ; compile ok

mov D$ValueA4-020 eax
mov D$ValueA4+020 eax


mov D$ValueA4-ecx eax ; compiled but without a error or warning. Compíled as D$ValueA4+ecx
mov D$ValueA4+ecx eax

mov D$ValueA4+ecx*4 eax
mov D$ValueA4-ecx*4 eax
mov D$eax-ecx*4 eax
mov D$ValueA4+ecx eax
mov D$ecx+ValueA4 eax
mov D$ecx*4+ValueA4 eax
mov D$ecx*4-ValueA4 eax
mov D$ecx*4*ValueA4 eax
mov D$ValueA4+ecx*4 eax
mov D$ValueA4-ecx*4 eax
mov D$ValueA4*ecx*4 eax


SearchForDis
FirstParameterAnalyze

and D$Label1+020 Label2-4

Note2: 
    To fix these things i need to review these functions:
        EncodeLines
        FirstParameterAnalyze
        SearchForSIB


weird things are being asembled as well, such as
[RealNumber: R$ (*34/25.1 -2)] ; what a hell is this ???


More errors in Rosasm encoder

push D$fs:02*4
push D$fs:08
push D$fs:02*4 ; error. encoded as push D$fs:edi*8+2
;push F$45.2
push (45) ; OK
push D$45 ; enoding error
mov D$-020+ValueA4 eax
mov D$-020+ValueA4 eax

;;

; Output :  eax = address

[MouseHint_Address: D$ 0]

; error in things like: X$esi+(PIX_WIDTH*4*0) See: MouseHint_CopyAndCook  ScanForSeparator . see ComputeExpression
[MouseHint_ValueFoundinExpression: D$ 0
 MouseHint_ReadedBytesinExpression: D$ 0]

; similar to DBG_ComputeExpression

;;
    Here is a must we analyse what kind of values are pointed by the segment selectors, registers or DataSymbols etc.
    MouseHint_EvaluateToken function identify what is a number, register or DatSymbol. However, we must
    analyse if a value was aleady identified as an address or not.
    This is because we may have expressions  containing 2 or more registers or datasymbols that maybe misidentified bith as addresses
    such as:
        "lea eax D$ebx+ecx*4", "mov D$ebx*4+ecx", "mov D$Value+ecx*4", "mov D$Value+ecx" etc
    So, the 1st statement will always be used as a addressing, and the other ones are always used as values.
    To fix it we need to see if a value (from a register or datasymbol) was already identified as an address or not.
;;

Proc MouseHint_Evaluate:
    Arguments @Input
    Local @OpType, @CurValueFound, @ExpressionEnd, @Size, @ResultValue, @IsSegmentSelector,
          @pTokenType, @ResultSegmentValue, @ResultDataSymbolValue, @DataSymbolOpType, @AddressFromReg, @AddressFromRegOpType
    Uses esi, ebx, edx, edi, ecx

    mov esi D@Input
    mov D@ExpressionEnd esi | add D@ExpressionEnd 255
    mov D$MouseHint_Address 0
    mov D$MouseHint_ValueFoundinExpression 0
    mov D$MouseHint_ReadedBytesinExpression 0
    mov D@IsSegmentSelector 0
    mov ebx 0
    mov D@OpType addSign ; always initialize with a addition
    mov D@ResultValue 0 ; always initialize to 0
    mov D@ResultSegmentValue 0
    mov D@ResultDataSymbolValue 0
    mov D@AddressFromreg 0

    ; 1st see if it is not a segment selector
    call DBG_GetTokenStringlenght esi, D@ExpressionEnd
    ...If_And eax => 3, B$esi+2 = ':'
        ;.If B$esi+2 = ':'
            ; colon only after segment selector
            call MouseHint_EvaluateSegmentSelector esi, 2
            If edx = 0
                mov eax 0
                ExitP
            Else
                mov D@IsSegmentSelector 1
                ;mov D@ResultValue eax
                mov D@ResultSegmentValue eax
                add esi 3 ; bypass the ":" after the segment selector and start adding/computing it´s values
            End_If
        ;.End_If
    ...Else
        mov ebx eax
        ; now we check if the 1st statement is a DataSymbol
        lea ecx D@pTokenType | mov D$ecx 0
        call MouseHint_EvaluateToken esi, eax, ecx
        ..If edx = 0
            mov eax 0
            ExitP
        ..Else_If D@pTokenType = DBG_PARAM_TYPE_DATA_SYMBOL
            mov D@ResultDataSymbolValue eax
            add esi ebx
            movzx ebx B$esi | mov D@DataSymbolOpType ebx
            If ebx = 0
                mov D@DataSymbolOpType addSign
                jmp L1>>
            Else
                inc esi
            End_If
        ..Else_If D@pTokenType = DBG_PARAM_TYPE_REG
            mov ecx esi | add ecx ebx
            .If B$ecx+1 <> mulSign ; NotScalar
                mov D@AddressFromreg eax
                add esi ebx
                movzx ebx B$esi | mov D@AddressFromRegOpType ebx
                If ebx = 0
                    mov D@AddressFromRegOpType addSign
                    jmp L1>>
                Else
                    inc esi
                End_If
            .End_If
        ..End_If
    ...End_If

    .Do

        On B$esi = Space, inc esi
        mov eax D$esi

        ..If al = OpenSign

            call DBG_ExpressionParser esi, MouseHint_ValueFoundinExpression, MouseHint_ReadedBytesinExpression
            On eax = 0, ExitP
            lea eax D@ResultValue
            call DBG_StoreOnExpression eax, D@OpType, D$MouseHint_ValueFoundinExpression
            On eax = 0, ExitP
            add esi D$MouseHint_ReadedBytesinExpression
        ..Else_If al = addSign
            mov D@OpType addSign
            inc esi
        ..Else_If al = subSign
            mov D@OpType subSign
            inc esi
        ..Else_If al = mulSign
            mov D@OpType mulSign
            inc esi
        ..Else_If al <= LowSigns
            ; in case we have any other values lower then Lowsigns (such as CloseSign, openSign, colonSign etc)
            ; simply copy them
            movzx eax al
            mov D@OpType eax | inc esi
        ..Else
            call DBG_GetTokenStringlenght esi, D@ExpressionEnd | mov D@Size eax
            On eax = 0, ExitP
            lea ecx D@pTokenType | mov D$ecx 0
            call MouseHint_EvaluateToken esi, eax, ecx
            mov D@CurValueFound eax
            .If edx = 0
                mov eax 0
                ExitP
            .Else
                ; do not fill ResultValue with zero here, because it will be reused inside the function
                ; accordying to the operation.
                lea eax D@ResultValue
                call DBG_StoreOnExpression eax, D@OpType, D@CurValueFound
                On eax = 0, ExitP
                add esi D@Size
            .End_If
        ..End_If
    .Loop_Until B$esi = 0

L1:

    mov eax D@ResultValue
    add eax D@ResultSegmentValue
    .If D@ResultDataSymbolValue <> 0
        If D@DataSymbolOpType = addSign
            add eax D@ResultDataSymbolValue
        Else_If D@DataSymbolOpType = subSign
            mov ebx eax
            mov eax D@ResultDataSymbolValue
            sub eax ebx
        End_If
    .Else_If D@AddressFromReg <> 0
        If D@AddressFromRegOpType = addSign
            add eax D@AddressFromReg
        Else_If D@AddressFromRegOpType = subSign
            mov ebx eax
            mov eax D@AddressFromReg
            sub eax ebx
        End_If
    .End_If
    mov D$MouseHint_Address eax
    mov eax 1

EndP
___________________________________________________________________________________________
; In order to correctly point to the addresses and valus we need to fix some assemblement errors.
; See MouseHint_Evaluate for more info.
; see ParametersCount
; 3 parameters SHLD / SHRD
Proc MouseHint_FixStatements:
    Arguments @InputName
    Local @ExpressionEnd, @FirstStatementSize, @OpType, @ParametersCount,
          @ParamOrder1, @ParamOrder2, @ParamOrder3, @IsSegmentSelector,
          @CurParamCount
    ;Structure @ParamOrder 16, @ParamOrder1Dis 0, @ParamOrder2Dis 4, @ParamOrder3Dis 8, @ParamOrderReservedDis 12
    Uses esi

    mov D@IsSegmentSelector &FALSE
    mov D@ParamOrder1 0
    mov D@ParamOrder2 0
    mov D@ParamOrder3 0
    mov esi D@InputName
    mov D@ExpressionEnd esi | add D@ExpressionEnd 255

    ; 1st see if it is not a segment selector
    call DBG_GetTokenStringlenght esi, D@ExpressionEnd
    ..If eax => 3
        .If B$esi+2 = ':'
            ; colon only after segment selector
            call MouseHint_EvaluateSegmentSelector esi, 2
            If edx = 0
                mov eax 0
                ExitP
            Else
                add esi 3 ; bypass the ":" after the segment selector and start adding/computing it´s values
                mov D@IsSegmentSelector &TRUE
            End_If
        .End_If
    ..End_If

    call DBG_GetParameters esi, DBG_Parameter1.ParamType, DBG_Parameter2.ParamType, DBG_Parameter3.ParamType
    On eax = 0, ExitP
    mov D@ParametersCount eax | mov D@CurParamCount eax
    ; Now that we´ve got the parameters, let´s analyse their contents and order
    ; the correct order of the statements are:
    ; DBG_PARAM_TYPE_DATA_SYMBOL + DBG_PARAM_TYPE_REG + DBG_PARAM_TYPE_NUMBER (or DBG_PARAM_TYPE_EXPRESSION)

    ; mov D$ValueA4+(5* (guga shl 2) + 18)+(5) eax
    ;

        mov D@ParamOrder1 DBG_Parameter1.ParamType
        mov D@ParamOrder2 DBG_Parameter2.ParamType
        mov D@ParamOrder3 DBG_Parameter3.ParamType

    ...If eax = 3
        ; mov D$ecx+(5* (guga shl 2) + 18)+ValueA4 eax
        ..If D$DBG_Parameter3.ParamType = DBG_PARAM_TYPE_DATA_SYMBOL
            Exchange D@ParamOrder1 D@ParamOrder2
            mov eax D@ParamOrder1
            mov ebx D@ParamOrder2
            mov ecx D@ParamOrder3

            ; mov D$ecx*4*ValueA4 eax or push D$fs:ecx*4*ValueA4; whattttt ???
            If_And D$ebx+DBG_ParameterInfo.IsScalarDis = &TRUE, D$eax+DBG_ParameterInfo.OpTypeDis = mulSign, D$ecx+DBG_ParameterInfo.OpTypeDis = mulSign
                xor eax eax | ExitP
                ; 1st param = -, 2nd = + then 2nd param Result = -
            Else_If_And D$eax+DBG_ParameterInfo.OpTypeDis = subSign, D$ebx+DBG_ParameterInfo.OpTypeDis = addSign
                mov D$ebx+DBG_ParameterInfo.OpTypeDis subSign

            End_If
        ..Else_If D$DBG_Parameter2.ParamType = DBG_PARAM_TYPE_DATA_SYMBOL
            Exchange D@ParamOrder3 D@ParamOrder2
        ..Else_If D$DBG_Parameter1.ParamType = DBG_PARAM_TYPE_DATA_SYMBOL
            Exchange D@ParamOrder3 D@ParamOrder1

            ; mov D$ebx*4+(5* (guga shl 2) + 18)+ecx eax ; 3rd statement is a register and one of the 1st two are registers
        ..Else_If_And D$DBG_Parameter3.ParamType = DBG_PARAM_TYPE_REG, D$DBG_Parameter3.IsScalar = &FALSE ; do nothing

        ..Else_If_And D$DBG_Parameter2.ParamType = DBG_PARAM_TYPE_REG, D$DBG_Parameter2.IsScalar = &FALSE
            ; 3rd and 2nd statement are register and the 3rd one is not scalar
            ; mov D$(5* (guga shl 2) + 18)+ecx+ebx*4 eax

            .If D$DBG_Parameter3.IsScalar = &TRUE
                Exchange D@ParamOrder3 D@ParamOrder2

            .Else_If_And D$DBG_Parameter1.ParamType = DBG_PARAM_TYPE_REG, D$DBG_Parameter1.IsScalar = &FALSE
                ; now, check which one of them are good addresses
                call Mouse_Hint_IsStatementMemory DBG_Parameter2.ParamType
                If eax = &TRUE
                    Exchange D@ParamOrder3 D@ParamOrder2
                Else
                    call Mouse_Hint_IsStatementMemory DBG_Parameter1.ParamType
                    On eax = &TRUE, Exchange D@ParamOrder3 D@ParamOrder1
                End_If
            .End_If
        ..Else_If_And D$DBG_Parameter1.ParamType = DBG_PARAM_TYPE_REG, D$DBG_Parameter1.IsScalar = &FALSE
            ; mov D$ecx+(5* (guga shl 2) + 18)+ebx*4 eax
            Exchange D@ParamOrder3 D@ParamOrder1

        ..End_If
        mov eax D@ParametersCount | dec eax | dec D@CurParamCount
    ...End_If
;     mov B$ebx+eax*8+0643530 0
    ...If eax = 1
        mov D@ParamOrder1 DBG_Parameter1.ParamType
    ...Else_If eax = 2
        ;mov D@ParamOrder1 DBG_Parameter1.ParamType
        ;mov D@ParamOrder2 DBG_Parameter2.ParamType
        mov eax D@ParamOrder1
        mov ebx D@ParamOrder2
        ..If D$ebx+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_DATA_SYMBOL
        ;..If D$DBG_Parameter2.ParamType = DBG_PARAM_TYPE_DATA_SYMBOL

            Exchange D@ParamOrder1 D@ParamOrder2
            mov eax D@ParamOrder1
            mov ebx D@ParamOrder2

            ; mov D$ecx*4*ValueA4 eax or push D$fs:ecx*4*ValueA4; whattttt ???
            If_And D$ebx+DBG_ParameterInfo.IsScalarDis = &TRUE, D$eax+DBG_ParameterInfo.OpTypeDis = mulSign
                xor eax eax | ExitP
                ; 1st param = -, 2nd = + then 2nd param Result = -
            Else_If_And D$eax+DBG_ParameterInfo.OpTypeDis = subSign, D$ebx+DBG_ParameterInfo.OpTypeDis = addSign
                mov D$ebx+DBG_ParameterInfo.OpTypeDis subSign

            End_If

        ;..Else_If D$DBG_Parameter1.ParamType = DBG_PARAM_TYPE_DATA_SYMBOL
        ..Else_If D$eax+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_DATA_SYMBOL
            ;mov eax D@ParamOrder1
            ;mov ebx D@ParamOrder2

            ; mov D$ValueA4*ecx*4 eax or push D$fs:ValueA4*ecx*4; whattttt ???
            If_And D$ebx+DBG_ParameterInfo.IsScalarDis = &TRUE, D$ebx+DBG_ParameterInfo.OpTypeDis = mulSign
                xor eax eax | ExitP
            ; 1st param = -, 2nd = + then 2nd param Result = -
            Else_If_And D$eax+DBG_ParameterInfo.OpTypeDis = subSign, D$ebx+DBG_ParameterInfo.OpTypeDis = addSign
                mov D$eax+DBG_ParameterInfo.OpTypeDis addSign
                mov D$ebx+DBG_ParameterInfo.OpTypeDis subSign
            End_If
        ;..Else_If_And D$DBG_Parameter1.IsScalar = &TRUE, D$DBG_Parameter1.ParamType = DBG_PARAM_TYPE_REG
        ..Else_If_And D$eax+DBG_ParameterInfo.IsScalarDis = &TRUE, D$eax+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_REG
            Exchange D@ParamOrder1 D@ParamOrder2
            mov eax D@ParamOrder1
            mov ebx D@ParamOrder2
            ; 1st param = -, 2nd = + then 2nd param Result = -
            If_And D$eax+DBG_ParameterInfo.OpTypeDis = subSign, D$ebx+DBG_ParameterInfo.OpTypeDis = addSign
                mov D$ebx+DBG_ParameterInfo.OpTypeDis subSign
            End_If
        ..Else_If_And D$eax+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_REG, D$ebx+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_REG

            ; 1st param = -, 2nd = + then 2nd param Result = -
            If_And D$eax+DBG_ParameterInfo.OpTypeDis = subSign, D$ebx+DBG_ParameterInfo.OpTypeDis = addSign
                mov D$ebx+DBG_ParameterInfo.OpTypeDis subSign
            End_If
        ..Else_If D$ebx+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_REG
            ;If D@ParametersCount < 3 ; 3 parameters. value+register+immediate (or expression)
             ; 3 parameters. value+register+immediate (or expression)
            ..If D@CurParamCount < 3
                If_Or D$eax+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_REG, D$eax+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_DATA_SYMBOL
                    Exchange D@ParamOrder1 D@ParamOrder2 ; 2 parameters. register+immediate (or expression)
                End_If
            ..End_If
        ..Else_If_And D$ebx+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_NUMBER, D$eax+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_EXPRESSION
            Exchange D@ParamOrder1 D@ParamOrder2
        ..End_If
;;
    ...Else
        mov D@ParamOrder1 DBG_Parameter1.ParamType
        mov D@ParamOrder2 DBG_Parameter2.ParamType
        mov D@ParamOrder3 DBG_Parameter3.ParamType
        ..If D$DBG_Parameter3.ParamType = DBG_PARAM_TYPE_DATA_SYMBOL
            mov D@ParamOrder1 DBG_Parameter2.ParamType
            mov D@ParamOrder2 DBG_Parameter1.ParamType
            mov eax D@ParamOrder1
            mov ebx D@ParamOrder2

            ; mov D$ecx*4*ValueA4 eax or push D$fs:ecx*4*ValueA4; whattttt ???
            If_And D$ebx+DBG_ParameterInfo.IsScalarDis = &TRUE, D$eax+DBG_ParameterInfo.OpTypeDis = mulSign
                xor eax eax | ExitP
                ; 1st param = -, 2nd = + then 2nd param Result = -
            Else_If_And D$eax+DBG_ParameterInfo.OpTypeDis = subSign, D$ebx+DBG_ParameterInfo.OpTypeDis = addSign
                mov D$ebx+DBG_ParameterInfo.OpTypeDis subSign

            End_If
        ..End_If
;;

    ...End_If

    mov edi D@InputName
    If D@IsSegmentSelector = &TRUE
        add edi 3
        call 'RosMem.FastZeroMem' edi, (256-3)
    Else
        call 'RosMem.FastZeroMem' edi, 256
    End_If
    mov ecx D@ParametersCount

    .If ecx = 3
        mov eax D@ParamOrder3
        lea edx D$eax+DBG_ParameterInfo.DataDis
        C_call FormatStr edi, {'%s' 0}, edx
        ;dec ecx; | on ecx = 0, ExitP
        add edi eax
        mov eax D@ParamOrder2
        lea ebx D$eax+DBG_ParameterInfo.OpTypeDis
        lea edx D$eax+DBG_ParameterInfo.DataDis
        C_call FormatStr edi, {'%s' '%s', 0}, ebx, edx

        add edi eax
        mov eax D@ParamOrder1
        lea ebx D$eax+DBG_ParameterInfo.OpTypeDis
        lea edx D$eax+DBG_ParameterInfo.DataDis
        C_call FormatStr edi, {'%s' '%s', 0}, ebx, edx

    .Else_If ecx = 2

        mov eax D@ParamOrder1
        lea edx D$eax+DBG_ParameterInfo.DataDis
        C_call FormatStr edi, {'%s' 0}, edx
        ;dec ecx | on ecx = 0, ExitP

        add edi eax
        mov eax D@ParamOrder2
        lea ebx D$eax+DBG_ParameterInfo.OpTypeDis
        lea edx D$eax+DBG_ParameterInfo.DataDis
        C_call FormatStr edi, {'%s' '%s', 0}, ebx, edx
        ;dec ecx | on ecx = 0, ExitP
    .Else
        mov eax D@ParamOrder1
        lea edx D$eax+DBG_ParameterInfo.DataDis
        C_call FormatStr edi, {'%s' 0}, edx
        ;dec ecx | on ecx = 0, ExitP
    .End_If

EndP
____________________________________________________________________________________________

Proc Mouse_Hint_IsStatementMemory:
    Arguments @pParamInfo

    mov esi D@pParamInfo

    lea eax D$esi+DBG_ParameterInfo.DataDis
    mov ecx D$esi+DBG_ParameterInfo.DataSizeDis
    call MouseHint_EvaluateRegister eax, ecx
    .If edx = 1
        call IsProcessMemory eax
        If eax <> 0
            mov eax &TRUE
        End_If
    .Else
        xor eax eax
    .End_If
EndP
____________________________________________________________________________________________

;;
    Fix all sort of incorrect syntaxes/compilations

push D$fs:02*4
push D$fs:08
push D$fs:02*4 ; error. encoded as push D$fs:edi*8+2
;push F$45.2
push (45)
push D$45
mov D$-020+ValueA4 eax
mov D$-020+ValueA4 eax

mov D$020+ValueA4 eax
mov D$020-ValueA4 eax

and D$ValueA4-020 ValueA4-4 ; compile ok
and D$ValueA4+020 ValueA4-4 ; compile ok

mov D$ValueA4-020 eax
mov D$ValueA4+020 eax


mov D$ValueA4-ecx eax ; compiled but without a error or warning. Compíled as D$ValueA4+ecx
mov D$ValueA4+ecx eax
mov D$ValueA4*ecx eax ; compiled but without a error or warning. Compíled as D$ValueA4+ecx


mov D$ValueA4+ecx*4 eax
mov D$ValueA4-ecx*4 eax ; compile error ?
;mov D$eax-ecx*4 eax
mov D$ValueA4+ecx eax
mov D$ecx+ValueA4 eax
mov D$ecx*4+ValueA4 eax
mov D$ecx*4-ValueA4 eax
mov D$ecx*4*ValueA4 eax
mov D$ValueA4+ecx*4 eax
mov D$ValueA4-ecx*4 eax
mov D$ValueA4*ecx*4 eax


    mov eax D$ebx*4+ecx

    movups XMM2 X$esi+ebx*8+   ;(PIX_WIDTH*4*2) ; 5*4*2

    movups XMM2 X$esi+ebx*8;(PIX_WIDTH*4*2) ; 5*4*2
        movups XMM2 X$ebx*8+esi;(PIX_WIDTH*4*2) ; 5*4*2
        movups XMM2 X$ebx*8

mov eax D$ValueA4+(    ) | ;

movups xmm1 X$esi+(16+(PIX_WIDTH*4*1))

mov eax D$ValueA4+(5* (guga shl 2) + 18)+(5)

mov eax D$ValueA4+(5* (guga shl 2) + 18)

;mov eax D$ValueA4+(5 or 2)

mov eax D$ValueA4+(guga * 2)

mov eax D$ValueA4+(5 shl 2)



;;

[DBG_ParameterInfo:
 DBG_Parameter1.ParamType: D$ 0 ; DBG_PARAM_TYPE_REG    DBG_PARAM_TYPE_NUMBER ...
 DBG_Parameter1.OpType: D$ 0    ; addSign  subSign  mulSign
 DBG_Parameter1.IsScalar: D$ 0
 DBG_Parameter1.DataSize: D$ 0
 DBG_Parameter1.Data: B$ 0 #256

 DBG_Parameter2.ParamType: D$ 0
 DBG_Parameter2.OpType: D$ 0
 DBG_Parameter2.IsScalar: D$ 0
 DBG_Parameter2.DataSize: D$ 0
 DBG_Parameter2.Data: B$ 0 #256

 DBG_Parameter3.ParamType: D$ 0
 DBG_Parameter3.OpType: D$ 0
 DBG_Parameter3.IsScalar: D$ 0
 DBG_Parameter3.DataSize: D$ 0
 DBG_Parameter3.Data: B$ 0 #256]

 [DBG_ParameterInfo.ParamTypeDis 0
  DBG_ParameterInfo.OpTypeDis 4
  DBG_ParameterInfo.IsScalarDis 8
  DBG_ParameterInfo.DataSizeDis 12
  DBG_ParameterInfo.DataDis 16]

[Size_of_DBG_ParameterInfo 272]

[DBG_PARAM_TYPE_REG 1]
[DBG_PARAM_TYPE_NUMBER 2]
[DBG_PARAM_TYPE_DATA_SYMBOL 3]
[DBG_PARAM_TYPE_EXPRESSION 4]

Proc DBG_GetParameters:
    Arguments @InputName, @pParameter1, @pParameter2, @pParameter3
    Local @ExpressionEnd, @FirstStatementSize, @OpType, @Found, @StartExpression, @EndExpression,
          @ParametersCount, @PreviousParamType, @CurOpType, @pCurParam, @ParamNumberisaExpression,
          @ExpressionPointers, @ExpressionPointersSize
    Structure @DataBuffer 256, @DataBuffer.StartDis 0

    call 'RosMem.FastZeroMem' D@DataBuffer, 256
    call 'RosMem.FastZeroMem' D@pParameter1, Size_of_DBG_ParameterInfo
    call 'RosMem.FastZeroMem' D@pParameter2, Size_of_DBG_ParameterInfo
    call 'RosMem.FastZeroMem' D@pParameter3, Size_of_DBG_ParameterInfo

    mov D@ExpressionPointersSize 0
    mov D@ExpressionPointers 0 ; If a data have more then 1 expression push D$fs:(02*4+1-7)+ecx+(02*4+1-7)
    mov D@ParamNumberisaExpression 0
    mov D@PreviousParamType 0
    mov D@ParametersCount 0
    mov D@Found &FALSE
    mov esi D@InputName
    mov D@ExpressionEnd esi | add D@ExpressionEnd 255

    If_Or B$esi = addSign, B$esi = subSign, B$esi = mulSign
        mov D@Found &TRUE
        movzx eax B$esi | mov D@OpType eax
        inc esi
    Else
        mov D@OpType addSign
    End_If
    mov ebx D@OpType | mov D@CurOpType ebx
    mov edi D@pParameter1

    .Do

        .If B$esi = OpenSign
            ;If D@ParametersCount > 1
             ;   movzx eax B$esi-1 ; restore optype
              ;  mov D@PreviousOpType eax
            ;End_if
            ;mov D@PreviousOpType ebx
            If D@ExpressionPointers = 0
                inc D@ParametersCount
                mov D@ExpressionPointers edi
                mov D@CurOpType ebx
                mov D$edi+DBG_ParameterInfo.OpTypeDis ebx
            Else
                ;dec D@ParametersCount
                mov D@CurOpType ebx
                mov edi D@ExpressionPointers
                mov eax D$edi+DBG_ParameterInfo.DataSizeDis; | mov D@ExpressionPointersSize eax
                ;mov D$edi+DBG_Parameter1.OpType ebx
                ;inc eax
                mov D@ExpressionPointersSize eax | inc D@ExpressionPointersSize
                lea ecx D$edi+DBG_ParameterInfo.DataDis | add ecx eax | mov B$ecx bl
            End_If
            ;inc D@ParametersCount
            mov D$edi+DBG_ParameterInfo.ParamTypeDis DBG_PARAM_TYPE_EXPRESSION
            mov ecx edi | mov edx ecx
            lea edi D$esi+DBG_ParameterInfo.DataDis
            lea eax D@StartExpression
            lea ebx D@EndExpression
            call DBG_GetExpressionDelimiterEx esi, eax, ebx
            mov D$ecx+DBG_ParameterInfo.DataSizeDis eax
            lea ecx D$ecx+DBG_ParameterInfo.DataDis | add ecx D@ExpressionPointersSize
            call StrCpyEx ecx, D@StartExpression, eax
            ;movzx ebx B$esi ; restore optype
            mov esi D@EndExpression | inc esi ; bypass last ")"
            mov edi edx; | add edi Size_of_DBG_ParameterInfo
            mov ecx DBG_PARAM_TYPE_EXPRESSION
            mov ebx D@CurOpType

            ;Do
             ;   movsb
            ;Loop_Until esi > D@EndExpression
            ;mov ebx D@PreviousOpType
        .Else
            inc D@ParametersCount
            mov D$edi+DBG_ParameterInfo.OpTypeDis ebx
            call DBG_GetInfo esi, edi
            On eax = 0, ExitP
            mov esi eax
        .End_If

;        ...If B$esi <> 0
            ;mov ecx D$esi+DBG_ParameterInfo.ParamTypeDis
            ;add edi Size_of_DBG_ParameterInfo
            movzx ebx B$esi | mov D@CurOpType ebx
                ; cases of ecx*4 . Scalar statement
            ..If_And D@PreviousParamType = DBG_PARAM_TYPE_REG, D$edi+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_NUMBER, D$edi+DBG_ParameterInfo.OpTypeDis = MulSign
                mov D@pCurParam edi; | add D@pCurParam Size_of_DBG_ParameterInfo
                lea eax D$edi+DBG_ParameterInfo.DataDis
                mov edx D$edi+DBG_ParameterInfo.OpTypeDis
                dec D@ParametersCount
                sub edi Size_of_DBG_ParameterInfo
                mov D$edi+DBG_ParameterInfo.IsScalarDis &TRUE
                ;mov D@ParamNumberisaExpression 0;edi ; save the param address that is, in fact a expression
                mov D$edi+DBG_ParameterInfo.ParamTypeDis DBG_PARAM_TYPE_REG

                mov ecx D$edi+DBG_ParameterInfo.DataSizeDis
                inc D$edi+DBG_ParameterInfo.DataSizeDis ; increment 1 so we can add the OpType for the final size
                lea ebx D$edi+DBG_ParameterInfo.DataDis | add ebx ecx | mov B$ebx dl | inc ebx
                C_call FormatStr ebx, {'%s' 0}, eax
                ; and finally add the size of the string to recompute the true size of the new expression
                add D$edi+DBG_ParameterInfo.DataSizeDis eax
                add ebx eax | mov B$ebx 0 ; just to we make sur it will end with zero
                mov ecx D$edi+DBG_ParameterInfo.ParamTypeDis
                add edi Size_of_DBG_ParameterInfo ; restore the pointer to edi. (The current parameter)
                movzx ebx B$esi ; restore optype
                ;mov D@ParamNumberisaExpression edi ; save the param address that is, in fact a expression
                call 'RosMem.FastZeroMem' D@pCurParam, Size_of_DBG_ParameterInfo

            ..Else_If_And D@PreviousParamType = DBG_PARAM_TYPE_NUMBER, D$edi+DBG_ParameterInfo.ParamTypeDis = DBG_PARAM_TYPE_NUMBER
                mov D@pCurParam edi; | add D@pCurParam Size_of_DBG_ParameterInfo
                lea eax D$edi+DBG_ParameterInfo.DataDis
                mov edx D$edi+DBG_ParameterInfo.OpTypeDis
                dec D@ParametersCount
                sub edi Size_of_DBG_ParameterInfo
                mov D@ParamNumberisaExpression edi ; save the param address that is, in fact a expression
                mov D$edi+DBG_ParameterInfo.ParamTypeDis DBG_PARAM_TYPE_NUMBER;DBG_PARAM_TYPE_EXPRESSION

                mov ecx D$edi+DBG_ParameterInfo.DataSizeDis
                inc D$edi+DBG_ParameterInfo.DataSizeDis ; increment 1 so we can add the OpType for the final size
                lea ebx D$edi+DBG_ParameterInfo.DataDis | add ebx ecx | mov B$ebx dl | inc ebx
                C_call FormatStr ebx, {'%s' 0}, eax
                ; and finally add the size of the string to recompute the true size of the new expression
                add D$edi+DBG_ParameterInfo.DataSizeDis eax
                add ebx eax | mov B$ebx 0 ; just to we make sur it will end with zero
                mov ecx D$edi+DBG_ParameterInfo.ParamTypeDis
                add edi Size_of_DBG_ParameterInfo ; restore the pointer to edi. (The current parameter)
                movzx ebx B$esi ; restore optype
                ;mov D@ParamNumberisaExpression edi ; save the param address that is, in fact a expression
                call 'RosMem.FastZeroMem' D@pCurParam, Size_of_DBG_ParameterInfo


            ..Else
                ;mov ebx D$edi+DBG_ParameterInfo.OpTypeDis
                mov ecx D$edi+DBG_ParameterInfo.ParamTypeDis
                add edi Size_of_DBG_ParameterInfo
            ..End_If

            ;mov ecx D$edi+DBG_ParameterInfo.ParamTypeDis |
            mov D@PreviousParamType ecx
            ;add edi Size_of_DBG_ParameterInfo
            On B$esi = 0, jmp L1>
            inc esi
;        ...End_If
    .Loop_Until B$esi = 0
L1:

    If D@ParamNumberisaExpression <> 0
        mov eax D@ParamNumberisaExpression
        lea edi D$eax+DBG_ParameterInfo.DataDis
        mov D$eax+DBG_ParameterInfo.ParamTypeDis DBG_PARAM_TYPE_EXPRESSION
        call StrcpyEx D@DataBuffer, edi, D$eax+DBG_ParameterInfo.DataSizeDis
        call 'RosMem.FastZeroMem' edi, 256
        mov B$edi OpenSign | inc edi
        C_call FormatStr edi, {'%s' 0}, D@DataBuffer
        add edi eax | mov B$edi CloseSign

    End_if


    mov eax D@ParametersCount

ExitP
    call DBG_GetTokenStringlenght esi, D@ExpressionEnd | mov D@FirstStatementSize eax

    mov ebx D@pParameter1  | lea edi D$ebx+DBG_ParameterInfo.DataDis
    mov ecx D@FirstStatementSize
    While ecx <> 0
        movsb
        dec ecx
    End_While
;;
    mov eax D@OpType
    If D@Found = &FALSE
        inc esi | movzx eax B$esi
    End_If
    mov D$ebx+DBG_ParameterInfo.OpTypeDis eax
;;
    lea edx D$ebx+DBG_ParameterInfo.ParamTypeDis
    lea edi D$ebx+DBG_ParameterInfo.DataDis
    call MouseHint_EvaluateToken edi, D@FirstStatementSize, edx


    inc esi | movzx eax B$esi
    mov D$ebx+DBG_ParameterInfo.OpTypeDis eax

EndP
____________________________________________________________________________________________

; retrieves the parameter type and value
Proc DBG_GetInfo:
    Arguments @InputName, @pInfoStruct
    Local @ExpressionEnd, @FirstStatementSize
    Uses esi, ebx, ecx, edi

    mov esi D@InputName

    call DBG_GetTokenStringlenght esi, D@ExpressionEnd | mov D@FirstStatementSize eax
    mov ebx D@pInfoStruct  | lea edi D$ebx+DBG_ParameterInfo.DataDis
    mov ecx D@FirstStatementSize | mov D$ebx+DBG_ParameterInfo.DataSizeDis ecx
    While ecx <> 0
        movsb
        dec ecx
    End_While
    mov B$edi 0
    lea edx D$ebx+DBG_ParameterInfo.ParamTypeDis
    lea edi D$ebx+DBG_ParameterInfo.DataDis
    call MouseHint_EvaluateToken edi, D@FirstStatementSize, edx
    If edx = 0
        xor eax eax | ExitP
    End_If

    mov eax esi


EndP

____________________________________________________________________________________________

; See: LowSigns
; The equates below were designed only for usage in the feature mouse hover in the debugger

[Dbg_Or_Op     255]
[Dbg_Xor_Op    254]
[Dbg_Shl_Op    253]
[Dbg_Shr_Op    252]
[Dbg_And_Op    251]
[Dbg_Not_Op    250]

[DGBExpression:
 DGBExpression.UseSpecialOperators: D$ 0
 DGBExpression.PtrExpressionA: D$ DGBExpression.ExpressionA
 DGBExpression.PtrExpressionB: D$ DGBExpression.ExpressionB
 DGBExpression.ExpressionA: B$ 0 #256
 DGBExpression.ExpressionB: B$ 0 #256]

; MouseHint_CopyAndCook ExpressionParser ComputeExpression ReplaceEquOnly
Proc DBG_ExpressionParser:
    Arguments @Input, @pValueFound, @pReadedBytes
    Local @ExpressionLenght, @IsInsideExpression, @SrcExpressionStart, @DstExpressionStart, @ExpressionLenght2
    Uses esi, edi, ecx, edx, ebx

    call 'RosMem.FastZeroMem' DGBExpression.ExpressionA, 256
    call 'RosMem.FastZeroMem' DGBExpression.ExpressionB, 256

    call StrCpy DGBExpression.ExpressionA, D@Input
    mov esi DGBExpression.ExpressionA | mov edx esi | add edx 256
    mov edi DGBExpression.ExpressionB

    mov D$DGBExpression.PtrExpressionA esi
    mov D$DGBExpression.PtrExpressionB edi

    mov D@IsInsideExpression &FALSE
    xor ebx ebx
    xor ecx ecx

    .While esi <= edx
        lodsb
        .If al = OpenSign
            If D@IsInsideExpression = &FALSE
                mov ebx 0, ecx 0
                mov D@SrcExpressionStart esi
                mov D@DstExpressionStart edi
            End_If
            inc ebx | inc ecx | mov D@IsInsideExpression &TRUE
        .Else_If al = CloseSign
            dec ecx
        .Else_If al = 0 ; end of data
            ; if we reach here we have an error? Or we need to recompute the size of expression ? It seems we have an error when reach here
            dec esi ; decrease esi to we get the proper ending point. We are 1 byte ahead after the true null terminated ending
            jmp L1>
        .End_If

        .If_And ebx <> 0, ecx = 0

            mov ecx esi
             ; decrease it to reach the real start
            mov eax D@SrcExpressionStart | dec eax | sub ecx eax | mov D@ExpressionLenght ecx
            call StrCpyEx D$DGBExpression.PtrExpressionA, eax, ecx

            call DBG_ComputeExpression DGBExpression.PtrExpressionA, DGBExpression.PtrExpressionB, ebx
            On eax = &FALSE, ExitP ; exit on error
            call DBG_WriteExpressionResult D$DGBExpression.PtrExpressionA, D@DstExpressionStart
             ; reaching here should already exit the file, but lets check again for 0 in esi
            mov ebx 0
            mov ecx 0
            mov D@IsInsideExpression &FALSE
            jmp L1> ; outloop, we reached the end of the expression
        .Else
            stosb
        .End_If

    .End_While

L1:

    ;mov eax esi | sub eax DGBExpression.ExpressionA | mov D@ExpressionLenght eax

    mov esi D$DGBExpression.PtrExpressionA
    call DBG_GetTokenStringlenght esi, 255
    mov ecx eax
    call MouseHint_EvaluateNumber esi
    If edx = 0
        xor eax eax
    Else
        mov ebx D@ExpressionLenght | mov edi D@pReadedBytes | mov D$edi ebx
        mov edi D@pValueFound | mov D$edi eax
        mov eax &TRUE
    End_If


EndP
____________________________________________________________________________________________

; True final result rewrite in Debug Expression Parser:

Proc DBG_WriteExpressionResult:
    Arguments @Input, @Output
    Uses esi, edi

    mov esi D@Input
    mov edi D@Output
    ; (Expression 'A' and not 'B' as source, because they are switched after each pass).

    While B$esi <> 0
        movsb
    End_While

    mov B$edi 0

EndP
____________________________________________________________________________________________

Proc DBG_ComputeExpression:
    Arguments @Input, @Output, @TotalExpressions
    Local @ExpressionEnd, @PreserveOpExpression, @OpExpressionAddress,
          @CurValueFound, @ResultValue, @Size, @IsValueChanged, @OpType, @pTokenType
    Uses esi, edi, ecx, ebx, edx

    mov ebx D@TotalExpressions

    ...Do

        mov D@ResultValue 0 ; we only zero it whenever a new expression is to be found
        mov D@OpType OpenSign
        mov esi D@Input | mov esi D$esi
        mov edi D@Output | mov edi D$edi
        xor edx edx

        mov D@ExpressionEnd esi | add D@ExpressionEnd 255
        call FindNextOpenSign esi, edi, ebx
        add edi eax
        add esi eax
        ..Do

            On B$esi = Space, inc esi
            mov eax D$esi

            ...If al = Dbg_Or_Op
                mov D@OpType Dbg_Or_Op
                inc esi
            ...Else_If al = Dbg_Xor_Op
                mov D@OpType Dbg_XOr_Op
                inc esi
            ...Else_If al = Dbg_Shl_Op
                mov D@OpType Dbg_Shl_Op
                inc esi
            ...Else_If al = Dbg_Shr_Op
                mov D@OpType Dbg_Shr_Op
                inc esi
            ...Else_If al = Dbg_And_Op
                mov D@OpType Dbg_And_Op
                inc esi
            ...Else_If al = Dbg_Not_Op
                mov D@OpType Dbg_Not_Op
                inc esi
            ...Else_If al = addSign
                mov D@OpType addSign
                inc esi
            ...Else_If al = subSign
                mov D@OpType subSign
                inc esi
            ...Else_If al = mulSign
                mov D@OpType mulSign
                inc esi
            ...Else_If al <= LowSigns
                ; in case we have any other values lower then Lowsigns (such as CloseSign, openSign, colonSign etc)
                ; simply copy them
                ; Note. Check here: ComputeExpression and ComputeRealExpression it seems to have a error at "...Else_If al < '0'...move D$Operator D$esi | mov B$Operator al | inc esi"
                movzx eax al
                mov D@OpType eax | inc esi
            ...Else

                ; the expression contains special operators (Dbg_Not_Op to Dbg_Or_Op).
                ; Let´s see on the rest of the data where is it.
                ; In case we found it, we need to exchange the byte of the special operator,
                ; because it may crash on MouseHint_EvaluateNumber
                ; MouseHint_EvaluateNumber have a limit of LowSigns and the special operators are higher then it. So we need to
                ; modify to any value below LowSigns and later restore it.
                ; see MouseHint_CopyAndCook
                mov D@IsValueChanged &FALSE
                .If D$DGBExpression.UseSpecialOperators = &TRUE
                    mov eax esi
                    Do
                        inc eax
                        If_And B$eax >= Dbg_Not_Op, B$eax <= Dbg_Or_Op
                            mov D@IsValueChanged &TRUE
                            ; special char found. Save it´ address and change the byte to TextSign (It is smaller then LowSigns)
                            mov D@OpExpressionAddress eax
                            movzx ecx B$eax | mov D@PreserveOpExpression ecx | mov B$eax TextSign | jmp L1>
                        Else_If eax >= D@ExpressionEnd
                            jmp L1> ; The data endeded and we found no special operators
                        End_If

                    Loop_Until B$eax = 0
                    L1:

                .End_If

                call DBG_GetTokenStringlenght esi, D@ExpressionEnd | mov D@Size eax
                On eax = 0, ExitP
                lea ecx D@pTokenType | mov D$ecx 0
                call MouseHint_EvaluateToken esi, eax, ecx
                mov D@CurValueFound eax

                ; If the data contained special operators, we need to recover them before exiting or continuing
                If D@IsValueChanged = &TRUE
                    mov ecx D@PreserveOpExpression
                    mov eax D@OpExpressionAddress | mov B$eax cl
                End_If

                .If edx = 0
                    mov eax 0
                    ExitP
                .Else
                    ; do not fill ResultValue with zero here, because it will be reused inside the function
                    ; accordying to the operation.
                    lea eax D@ResultValue
                    call DBG_StoreOnExpression eax, D@OpType, D@CurValueFound
                    If eax = 0
                        ExitP
                    End_If
                    add esi D@Size
                .End_If
            ...End_If

        ..Loop_Until B$esi-1 = CloseSign

        call DBG_ReWriteExpression D@Input, D@Output, ebx, D@ResultValue
        dec ebx
    ...Loop_Until ebx = 0

    mov eax &TRUE

EndP
____________________________________________________________________________________________

Proc FindNextOpenSign:
    Arguments @Input, @Output, @OpenSignCount
    Uses esi, edi, ebx

    mov esi D@Input
    mov edi D@Output
    mov ebx D@OpenSignCount

    .Do
        Do
            lodsb | stosb
        Loop_Until al = OpenSign
        dec ebx
    .Loop_Until ebx = 0

    mov eax esi | sub eax D@Input

EndP
____________________________________________________________________________________________

Proc DBG_GetTokenStringlenght:
    Arguments @Input, @EndString
    Uses esi, edx

    mov esi D@Input
    mov edx D@EndString
    xor eax eax
    While B$esi <> 0
        If_And B$esi > LowSigns, B$esi < Dbg_Not_Op
            inc eax
            inc esi
        Else_If esi => edx
            ExitP
        Else
            ExitP
        End_if
    End_While

EndP
____________________________________________________________________________________________

;;
    pPreviousValue: Pointer to a buffer that holds the previous value found.
                    i.e: The value found on the expression that preceeded. (The 1st one) as in: Expression1 * Expression2
                    The resultant value will be stored here for further looping computations (several expressions)
    OpType: The type of operator envolved.
    CurrentValue: The current value on the second expression (Tne next one)
;;

Proc DBG_StoreOnExpression:
    Arguments @pPreviousValue, @OpType, @CurrentValue
    Uses edi, edx, ecx

    mov edi D@pPreviousValue | mov eax D@CurrentValue

    ...If D@OpType = Dbg_Or_Op
        or D$edi eax
        mov eax &TRUE
    ...Else_If al = Dbg_Xor_Op
        or D$edi eax
        mov eax &TRUE
    ...Else_If D@OpType = Dbg_Shl_Op
        mov cl al
        shl D$edi cl
        mov eax &TRUE
    ...Else_If D@OpType = Dbg_Shr_Op
        mov cl al
        shr D$edi cl
        mov eax &TRUE
    ...Else_If D@OpType = Dbg_And_Op
        and D$edi eax
        mov eax &TRUE
    ...Else_If al = Dbg_Not_Op
        not eax | mov D$edi eax
        mov eax &TRUE
    ...Else_If D@OpType = addSign
        add D$edi eax
        mov eax &TRUE
    ...Else_If D@OpType = subSign
        sub D$edi eax
        mov eax &TRUE
    ...Else_If D@OpType = mulSign
        mov ecx eax
        mov eax D@pPreviousValue | mov eax D$eax
        mul ecx
        mov D$edi eax
        mov eax &TRUE
    ...Else_If D@OpType = OpenSign
        mov D$edi eax
        mov eax &TRUE
    ...Else
        xor eax eax ; error
    ...End_If

EndP
____________________________________________________________________________________________
; here we rewrite the expresseion and switch the buffers
Proc DBG_ReWriteExpression:
    Arguments @pInput, @pOutput, @TotalExpressions, @Value
    Uses ebx, esi, edi, eax

    mov esi D@pInput | mov esi D$esi
    mov edi D@pOutput | mov edi D$edi

    call FindNextOpenSign esi, edi, D@TotalExpressions
    add edi eax
    add esi eax
    ; skip first '('.
    dec edi

    ; here we are only converting it to hexadecimal form on edi. So, instead using te same routine as in ReWriteExpression
    ; we simply use FormatStr

    If D@Value = 0
        mov B$edi '0' | inc edi
    Else
        C_call FormatStr edi, {'0%x' 0}, D@Value
        add edi eax
    End_If

    ; jmp over resolved part.
    While B$esi-1 <> CloseSign
        inc esi
    End_While

     ; rewrite it, except if it is a special op or any other below memMarker such as: colonSign, addSign etc
    If_And B$esi > PartEnds, B$esi < Dbg_Not_Op
        mov al Space | stosb
    End_If

    ; recopy remainder of expression.
    While B$esi-1 > 0
        movsb
    End_While

    mov esi D@pInput | mov edi D@pOutput
    Exchange D$esi D$edi

EndP
____________________________________________________________________________________________

; Get value of equate
; Input  :  esi -> name, ecx = namelen, edx = size of destination buffer
; Output :  eax = valid(1) invalid(0)

Proc MouseHint_ReplaceEquate:
    Uses esi, ecx

  ; If naked local symbol, check if this belongs to the scope of the current code label.
    If B$esi = '@'

        ; StrLenProc
        push edi
            mov edi LabelName
            call StrLenProc edi
            add edi eax
            C_call FormatStr edi, {'%s' 0}, esi
            mov esi LabelName; | inc esi
            mov B$edi+eax 0
        pop edi
;;
        push edi ecx
            mov edi LabelName
            push ecx
                mov ecx 0-1, al 0
                repne scasb
            pop ecx
            dec edi
            rep movsb
            mov B$edi 0
        pop ecx edi
        mov esi LabelName
;;
    End_If

    call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

    ..If eax <> 0
      ; Already copied name (len=ecx) is wiped off the buffer
        add edx ecx

      ; Skip equate name
        While B$eax > LowSigns
            inc eax
        End_While
        inc eax

      ; Store equate contents. First check if there's sufficient room to store it.
        mov esi D$eax, ecx D$eax+4
        sub edx ecx | js L8>
        rep movsb

        mov eax 1
    ..Else
L8:     mov eax 0
    ..End_If
EndP
____________________________________________________________________________________________

; Copy the cooked buffer and replace equates by its values. This must be done until no more
; equates are found in the expression (in case of nested equates).

Proc MouseHint_LookupEquates:
    Arguments @Input
    Local @PreserveOpExpression, @ConvertLow, @TokenLenght, @PreserveECX
    Structure @DataBuffer 256, @DataBuffer.StartDis 0

    ; HACK - get current label into labelname
    ;mov eax D$DebugBaseOfCode
    ;If_And D$DebugProcessData.Type = DEBUG_DLL, D$DebugProcessData.IsRebased = &TRUE
     ;   mov D$OriginalDataPointer eax
      ;  sub eax D$DebugProcessData.DllImageBase | add eax D$DebugProcessData.DllNewImageBase
    ;End_If
    call 'RosMem.FastZeroMem' D@DataBuffer, 256
    call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D$C.regEIP
    If eax = 1
        call ScanLabelListForCodeLabel D$C.regEIP, 0
    Else
        mov B$LabelName 0
    EndIf

  ; output buffer on stack
    mov edi D@DataBuffer
    mov edx 255

    mov ebx 0
    mov esi D@Input
    mov ecx 0-1

    .Do
        inc ecx
        dec edx | js L8>>

        mov al B$esi+ecx
        mov B$edi+ecx al

        ...If al <= LowSigns
        ;.If al < ' '
          ; substitute equate with value
            call MouseHint_ReplaceEquate
            If eax = 1
                inc ebx
            Else
                add edi ecx
            EndIf

          ; restore operator (might have been overwritten, different len equate<->value)
            add esi ecx
            mov al B$esi | inc esi
            mov B$edi al | inc edi

            mov ecx 0-1
        ...Else_If_And al >= Dbg_Not_Op, al <= Dbg_Or_Op

            ; copy the operator extra signs (Dbg_Or_Op to Dbg_Not_Op) so we can fix it below
            movzx eax B$esi+ecx | mov D@PreserveOpExpression eax
            mov B$esi+ecx TextSign

            call MouseHint_ReplaceEquate
            If eax = 1
                inc ebx
            Else
                add edi ecx
            EndIf

            ; restore previous altered operator
            mov eax D@PreserveOpExpression | mov B$esi+ecx al

            ; restore operator (might have been overwritten, different len equate<->value)
            add esi ecx
            mov al B$esi | inc esi
            mov B$edi al | inc edi

            mov ecx 0-1

        ...Else_If_And al = ':', ecx = 2 ;  see if it is a segment selector

            movzx eax W$esi
            mov D@ConvertLow eax
            lea eax D@ConvertLow
            call LowerCase_Ex eax, 4
            mov eax D@ConvertLow
            ; see MouseHint_EvaluateSegmentSelector
            ..If_Or ax = 'fs', ax = 'ds', ax = 'cs', ax = 'es', ax = 'gs', ax = 'ss'

                add edi 3 ; bypass 1st 3 letters to edi
                call DBG_GetTokenStringlenght esi, 256 | mov D@TokenLenght eax
                ; copy the operator extra signs (Dbg_Or_Op to Dbg_Not_Op) so we can fix it below
                movzx eax B$esi+ecx | mov D@PreserveOpExpression eax
                mov B$esi+ecx TextSign

                mov D@PreserveECX ecx
                add esi 3 ; added here just to find any equate on it
                mov ecx D@TokenLenght
                call MouseHint_ReplaceEquate
                mov ecx D@PreserveECX ; restore ecx here
                .If eax = 1
                    inc ebx
                    sub esi 3 ;restore previous esi
                    ; restore previous altered operator
                    mov eax D@PreserveOpExpression | mov B$esi+ecx al
                    ; restore operator (might have been overwritten, different len equate<->value)
                    add esi D@TokenLenght
                    mov al B$esi | inc esi
                    mov B$edi al | inc edi

                .Else
                    sub esi 3 ;restore previous esi
                    ; restore previous altered operator
                    mov eax D@PreserveOpExpression | mov B$esi+ecx al
                    ; restore operator (might have been overwritten, different len equate<->value)
                    add esi 3
                .EndIf

                mov ecx 0-1
            ..End_If
        ...End_If
    .Loop_Until B$esi+ecx = 0

L8:

    mov ecx edi | sub ecx D@DataBuffer

    call 'RosMem.FastZeroMem' D@Input, 256
    call strcpyEx D@Input, D@DataBuffer, ecx

    mov eax ebx ; number of replacements

EndP
____________________________________________________________________________________________

; Input
;   esi -> $ in expression
; Output
;   @Buffer contains cooked (uppercase with spaces stripped) expression
; If eax returns TRUE, the function suceeded. If returned 0-1, it failed. Need return 0-1 in falure due to
; MouseHint_ScanForSeparator that also returns 0-1 on failure cases

Proc MouseHint_CopyAndCook:
    Arguments @Input, @Output
    Local @DataEnd, @InsideExpression, @ConvertLow, @StartExpression, @EndExpression

    mov esi D@Input
    mov edi D@Output
    mov D@DataEnd edi | add D@DataEnd 255
    mov D@InsideExpression 0
    ; scan forward & copy

    inc esi ; Bypass 1st "$" char
    mov al B$esi
    mov dl 0 ; expect separator=1 symbol=-1

  ; scan Forward
    .While al <> 36 ; B$ParagraphChar ; '$' ; Dollar
        On al = 167, jmp L9>>
        cmp dl 1 | je L0>>

        ; step over legal symbol chars: A..Z a..z 0..9 _ & : @ .
        ...If_Or al = '&', al = '.'
            mov dl 0
            stosb
        ...Else_If al = '_'
        ...Else_If_And al >= '0', al <= ':'
            mov dl 0
            stosb
        ...Else_If_And al >= '@', al <= 'Z'
            mov dl 0
            stosb
        ...Else_If_And al >= 'a', al <= 'z' ; 'a' .. 'z'
            sub al 020 ; convert to uper case ; a..z -> A..Z
            mov dl 0
            stosb
        ...Else
L0:
;;
            .If D@InsideExpression = &TRUE
                call DBG_IsAddressInsideExpression esi, D@StartExpression, D@EndExpression
                On eax = &FALSE, mov D@InsideExpression &FALSE
                movzx eax B$esi ; restore al
            .End_if
;;
            ; not a symbol char, check for 'connecting' chars ' ', '+', '-', '*'
            ..If al = '+'
                mov dl 0-1
                mov al addSign
                stosb
            ..Else_If al = '-'
                mov dl 0-1
                mov al subSign
                stosb
            ..Else_If al = '*'
                mov dl 0-1
                mov al mulSign
                stosb
            ..Else_If al = ' '
                On dl = 0, mov dl 1
            ..Else_If al = '(' ; Important. Otherwise it won´t see labels as exprssions. Ex: mov eax (Equate+53)
                ; compute the expression delimiters for each expression.
                If D@InsideExpression = &FALSE
                    lea eax D@StartExpression
                    lea ebx D@EndExpression
                    call DBG_GetExpressionDelimiter esi, eax, ebx
                    mov D@InsideExpression &TRUE
                End_If
                mov dl 0-1
                mov al openSign
                stosb
            ..Else_If al = ')' ; Important. Otherwie it won´ ee labels as exprssions. Ex: mov eax (Equate+53)
                ;mov D@InsideExpression 1
                If esi = D@EndExpression ; This is he last byte of th exprssion, so let´s restart dl to 0.
                    mov D@InsideExpression &FALSE
                    mov dl 0
                Else
                    mov dl 0-1
                End_If
                mov al closeSign
                stosb
            ..Else_If_Or al = CR, al = LF, al = ';', al = "|" ; skip over paragraph, comments or separators after this line
                ; if before the end of statement we have a extra sign (+, -, *, space) we delete it.
                ; Ex: movups XMM2 X$esi+ebx*8+;(PIX_WIDTH*4*2) ; 5*4*2
                call DBG_CleanUpCopyAndCook D@Output, edi
                ;If_Or B$edi-1 = addSign, B$edi-1 = subSign, B$edi-1 = mulSign
                 ;   mov B$edi-1 0
                ;End_If
                ; end of line found
                ;mov B$edi 0
                ;mov eax 1
                ExitP
            ..Else_If D@InsideExpression = 1 ; we are hovering inside a expression D$Value+(Equate * 4) ....D$Value+(5 shl 4) etc
                ; DBG_IsAddressInsideExpression
                ; Dbg_Shl_Op
                mov ecx D@DataEnd | sub ecx edi
                If ecx < 4
                    mov eax 0-1 | ExitP ; error
                End_If
                mov ecx D$esi
                mov D@ConvertLow ecx
                lea ecx D@ConvertLow
                call LowerCase_Ex ecx, 4
                mov ecx D@ConvertLow
                .If ecx = 'shl '
                    mov B$edi Dbg_Shl_Op | inc edi
                    add esi 3 ; bypass 'shl'. The last char ' ' don´ need to be parsed, afterall we already know it is there
                    ; so, only check for dl and then loop back to L3 to the next char after the pace
                    mov dl 0
                    mov D$DGBExpression.UseSpecialOperators &TRUE
                .Else_If ecx = 'shl '
                    mov B$edi Dbg_Shr_Op | inc edi
                    add esi 3 ; bypass 'shl'. The last char ' ' don´ need to be parsed, afterall we already know it is there
                    ; so, only check for dl and then loop back to L3 to the next char after the pace
                    mov dl 0
                    mov D$DGBExpression.UseSpecialOperators &TRUE
                .Else_If ecx = 'and '
                    mov B$edi Dbg_And_Op | inc edi
                    add esi 3 ; bypass 'shl'. The last char ' ' don´ need to be parsed, afterall we already know it is there
                    ; so, only check for dl and then loop back to L3 to the next char after the pace
                    mov dl 0
                    mov D$DGBExpression.UseSpecialOperators &TRUE
                .Else_If ecx = 'not '
                    mov B$edi Dbg_Not_Op | inc edi
                    add esi 3 ; bypass 'shl'. The last char ' ' don´ need to be parsed, afterall we already know it is there
                    ; so, only check for dl and then loop back to L3 to the next char after the pace
                    mov dl 0
                    mov D$DGBExpression.UseSpecialOperators &TRUE
                .Else_If ecx = 'xor '
                    mov B$edi Dbg_Xor_Op | inc edi
                    add esi 3 ; bypass 'shl'. The last char ' ' don´ need to be parsed, afterall we already know it is there
                    ; so, only check for dl and then loop back to L3 to the next char after the pace
                    mov dl 0
                    mov D$DGBExpression.UseSpecialOperators &TRUE
                .Else_If_And cx = 'or', B$esi+2 = ' '
                    mov B$edi Dbg_Or_Op | inc edi
                    add esi 2 ; bypass 'shl'. The last char ' ' don´ need to be parsed, afterall we already know it is there
                    ; so, only check for dl and then loop back to L3 to the next char after the pace
                    mov dl 0
                    mov D$DGBExpression.UseSpecialOperators &TRUE
                .Else
                    mov eax 0-1 | ExitP
                .End_If

            ..Else
                ; if before the end of statement we have a extra sign (+, -, *, space) we delete it.
                ; Ex: movups XMM2 X$esi+ebx*8+;(PIX_WIDTH*4*2) ; 5*4*2
                call DBG_CleanUpCopyAndCook D@Output, edi
                ;If_Or B$edi-1 = addSign, B$edi-1 = subSign, B$edi-1 = mulSign
                 ;   mov B$edi-1 0
                ;End_If
                ;mov B$edi 0
                ;mov eax 1
                ExitP
            ..End_If

        ...End_If

        inc esi
        mov al B$esi
    .End_While

L9:
    ; if before the end of statement we have a extra sign (+, -, *, space) we delete it.
    ; Ex: movups XMM2 X$esi+ebx*8+;(PIX_WIDTH*4*2) ; 5*4*2
    call DBG_CleanUpCopyAndCook D@Output, edi
    ;If_Or B$edi-1 = addSign, B$edi-1 = subSign, B$edi-1 = mulSign
     ;   mov B$edi-1 0
    ;End_If
    ;mov B$edi 0
    ;mov eax 1

EndP
____________________________________________________________________________
; Return TRUE on sucess. 0-1 on failure
; Failure cases are in expressions that starts with naked "*" signs such as:Ex: "(*(". D$ (*45) etc. The 1st token can be + or - only

Proc DBG_CleanUpCopyAndCook:
    Arguments @StartAddress, @EndAddress
    Local @StartExpression, @EndExpression, @ExpressionSize
    Structure @DataBuffer 256, @DataBuffer.StartDis 0
    Uses edx, esi, edi, ecx, ebx

    call 'RosMem.FastZeroMem' D@DataBuffer, 256
    mov edx D@EndAddress
    mov esi D@StartAddress
    mov edi D@DataBuffer

    ; 1st remove all empty expressions. Ex: mov eax D$ecx+((()))
    .Do
        ..If B$esi = OpenSign ; found 1st open parenthesis
            lea eax D@StartExpression
            lea ebx D@EndExpression
            call DBG_GetExpressionDelimiterEx esi, eax, ebx | mov D@ExpressionSize eax
            .If eax <> 0
                mov eax 1 ; See if the expression is empty or not
                mov ecx D@StartExpression ; start os parenthesis
                Do
                    If_And B$ecx = openSign, B$ecx+1 = mulSign ; Invalid statement. Ex: "(*(". D$ (*45) etc. The 1st token can be + or - only
                        mov eax 0-1 | ExitP
                    Else_If_Or B$ecx < closeSign, B$ecx > openSign
                        xor eax eax ; expression is notempty
                        jmp L1>
                    End_If
                    inc ecx
                Loop_Until ecx > D@EndExpression
                L1:
            .End_If
            If eax = 0
                mov esi D@StartExpression
                Do
                    movsb
                Loop_Until esi > D@EndExpression
            Else
                mov esi D@EndExpression | inc esi
            End_If
        ..Else
            movsb
        ..End_If
    .Loop_Until esi >= edx
    mov B$edi 0

    ; Now that we fixed the expressions, we need to see if it does not contains duplicated chars, like "+", "-", "/", "*", ".", " "
    mov edx edi
    mov esi D@DataBuffer
    mov edi D@StartAddress
    call 'RosMem.FastZeroMem' edi, 256

    Do
        movzx eax W$esi
        .If_And al = ah
            If_Or al = addSign, al = subSign, al = mulSign, , al = divSign, al = '.', al = ' '
                inc esi
            Else
                movsb
            End_If
        .Else
            movsb
        .End_If
    Loop_Until esi >= edx

    mov al B$edi-1 ; and finally if at the end we have some remainder null token we delete it
    If_Or al = addSign, al = subSign, al = mulSign, , al = divSign, al = '.', al = ' '
        mov B$edi-1 0
    End_If

    mov eax &TRUE
    ;mov ecx edi | sub ecx D@DataBuffer

    ;call 'RosMem.FastZeroMem' D@StartAddress, 256
    ;call strcpyEx D@StartAddress, D@DataBuffer, ecx

EndP
____________________________________________________________________________

;;
    This funtion retrieves the start and end of a expression.
    If the function fail, it will return 0. Otherwise, it will return the total len of the expression.
    
    Note: The start and end of the exprssion includes the "(" and ")" signs. So, it will always starts at "(" and end with ")"
;;

; MouseHint_CopyAndCook ExpressionParser ComputeExpression ReplaceEquOnly

Proc DBG_GetExpressionDelimiter:
    Arguments @Input, @pStartExpression, @pEndExpression
    Local @ExpressionLenght, @IsInsideExpression, @SrcExpressionStart
    Uses esi, edi, ecx, ebx

    mov esi D@Input
    mov D@IsInsideExpression &FALSE
    xor ebx ebx
    xor ecx ecx

    .While B$esi <> 0
        lodsb
        .If al = '('
            If D@IsInsideExpression = &FALSE
                mov ebx 0, ecx 0
                mov D@SrcExpressionStart esi
            End_If
            inc ebx | inc ecx | mov D@IsInsideExpression &TRUE
        .Else_If al = ')'
            dec ecx
        .Else_If_Or al = 0, al = CR, al = LF ; end of data  or end of line
            If_And ebx <> 0, ecx = 0
                mov ecx esi
                ; decrease it to reach the real start
                mov eax D@SrcExpressionStart | dec eax | sub ecx eax
                mov edi D@pStartExpression | mov D$edi eax
                dec esi
                mov edi D@pEndExpression | mov D$edi esi
                mov eax ecx
            Else
                xor eax eax
            End_If
            ExitP
        .End_If

        If_And ebx <> 0, ecx = 0
            mov ecx esi
             ; decrease it to reach the real start
            mov eax D@SrcExpressionStart | dec eax | sub ecx eax
            mov edi D@pStartExpression | mov D$edi eax
            dec esi
            mov edi D@pEndExpression | mov D$edi esi
            mov eax ecx
            ExitP
        End_If

    .End_While

L1:
    xor eax eax

EndP

; This is similar to DBG_GetExpressionDelimiter except that it work only for MyAsciiTable table.
; So it will work only for encoded RosAsm routines for finding the proper statements and tokens

Proc DBG_GetExpressionDelimiterEx:
    Arguments @Input, @pStartExpression, @pEndExpression
    Local @ExpressionLenght, @IsInsideExpression, @SrcExpressionStart
    Uses esi, edi, ecx, ebx

    mov esi D@Input
    mov D@IsInsideExpression &FALSE
    xor ebx ebx
    xor ecx ecx

    .While B$esi <> 0
        lodsb
        .If al = OpenSign
            If D@IsInsideExpression = &FALSE
                mov ebx 0, ecx 0
                mov D@SrcExpressionStart esi
            End_If
            inc ebx | inc ecx | mov D@IsInsideExpression &TRUE
        .Else_If al = CloseSign
            dec ecx
        .Else_If_Or al = 0, al = EOI, al = EOI ; end of data  or end of line
            If_And ebx <> 0, ecx = 0
                mov ecx esi
                ; decrease it to reach the real start
                mov eax D@SrcExpressionStart | dec eax | sub ecx eax
                mov edi D@pStartExpression | mov D$edi eax
                dec esi
                mov edi D@pEndExpression | mov D$edi esi
                mov eax ecx
            Else
                xor eax eax
            End_If
            ExitP
        .End_If

        If_And ebx <> 0, ecx = 0
            mov ecx esi
             ; decrease it to reach the real start
            mov eax D@SrcExpressionStart | dec eax | sub ecx eax
            mov edi D@pStartExpression | mov D$edi eax
            dec esi
            mov edi D@pEndExpression | mov D$edi esi
            mov eax ecx
            ExitP
        End_If

    .End_While

L1:
    xor eax eax

EndP

Proc DBG_GetExpressionDelimiterworking:
    Arguments @Input, @pStartExpression, @pEndExpression
    Local @ExpressionLenght, @IsInsideExpression, @SrcExpressionStart
    Uses esi, edi, ecx, ebx

    mov esi D@Input
    mov D@IsInsideExpression &FALSE
    xor ebx ebx
    xor ecx ecx

    .While B$esi <> 0
        lodsb
        .If_Or al = OpenSign, al = '('
            If D@IsInsideExpression = &FALSE
                mov ebx 0, ecx 0
                mov D@SrcExpressionStart esi
            End_If
            inc ebx | inc ecx | mov D@IsInsideExpression &TRUE
        .Else_If_Or al = CloseSign, al = ')'
            dec ecx
        .Else_If_Or al = 0, al = CR, al = LF ; end of data  or end of line
            xor eax eax
            ExitP
        .End_If

        If_And ebx <> 0, ecx = 0
            mov ecx esi
             ; decrease it to reach the real start
            mov eax D@SrcExpressionStart | dec eax | sub ecx eax
            mov edi D@pStartExpression | mov D$edi eax
            dec esi
            mov edi D@pEndExpression | mov D$edi esi
            mov eax ecx
            ExitP
        End_If

    .End_While

L1:
    xor eax eax

EndP
____________________________________________________________________________________________

; Check if the current address is inside a expession
; See: DBG_GetExpressionDelimiter

Proc DBG_IsAddressInsideExpression:
    Arguments @Address, @StartExpression, @EndExpression
    Uses ebx

    xor eax eax
    mov ebx D@Address
    If_And ebx >= D@StartExpression, ebx <= D@EndExpression
        mov eax &TRUE
    End_If

EndP
____________________________________________________________________________________________

; Input
;   esi -> Text Ptr
; Output
;   edi -> Start of string
;   ecx = Size of string
;   eax = Expression (1) | Data declaration (0) | Invalid (-1)

[MouseHint_SizeMarker: B$ ?]
; see MouseHint_CopyAndCook, at DBG_CleanUpCopyAndCook it will try to fix all sort of type mistaes in rosasm syntax

Proc MouseHint_ScanForSeparator:
    Arguments @Input, @Output
    Local @InsideExpression

    mov esi D@Input
    mov edi D@Output

    ; If starts with a CRLF, it means it is not preceded by '$', check for data declaration
    mov eax 0-1
    On W$esi = CRLF, ExitP

    call IsMouseHoverEmptySpace esi
    On eax = 0-1, ExitP

    call 'RosMem.FastZeroMem' edi, 256

    call isMouseInsideExpression esi
    If eax = &TRUE
        mov D@InsideExpression &TRUE
    Else
        mov D@InsideExpression &FALSE
    End_If


    mov al B$esi
    mov dl 0 ; expect separator=1 symbol=-1

  ; scan backward
    .While al <> 36 ; B$ParagraphChar ; '$' ; Dollar
        On al = 167, jmp L7>>

        cmp dl 1 | je L0>>

        ..If_Or al = CR, al = LF, al = ';'
            ; found nothing good at this line. We reached the previous line end or we are hovering a comment
            ; that have no inside variables to analyse inside of it.
            mov eax 0-1 | ExitP
        ..Else_If_Or al = '&', al = '.'; step over legal symbol chars: A..Z a..z 0..9 _ & : @ .
            mov dl 0 ; Symbol
        ..Else_If_Or al = '(', al = ')' ; Important. Otherwise it won´t see labels as exprssions. Ex: mov eax (Equate+53)
            mov dl 0 ; symbol
            mov D@InsideExpression 1
        ..Else_If al = '_'
        ;..Else_If_Or al = '_', al = '|'
        ..Else_If_And al >= '0', al <= ':'
            mov dl 0 ; symbol
        ..Else_If_And al >= '@', al <= 'Z'
            mov dl 0 ; symbol
        ..Else_If_And al >= 'a', al <= 'z' ; 'a' .. 'z'
            mov dl 0 ; symbol
        ..Else

L0:     ; not a symbol char, check for 'connecting' chars ' ', '+', '-', '*'
            .If D@InsideExpression = &TRUE
                mov dl 0
            .Else_If_Or al = '+',  al = '-',  al = '*'
                mov dl 0-1
            .Else_If al = ' '
                On dl = 0, mov dl 1
            .Else_If al = '(' ; Ex: mov eax D$ValueA4+(    ) . And the mouse is pointing on a space inside the expression (....)
                mov dl 0 ; symbol
                mov D@InsideExpression 1
                jmp L8>
            .Else_If al = ')' ; Ex: mov eax D$ValueA4+(    ) . And the mouse is pointing on a space inside the expression (....)

                If dl <> 0-1 ; If a expression is not preceeded by a math operation, it means we are on the next statement
                             ; Ex:  movups X$edi+(PIX_HEIGHT*4*0) xmm4
                             ; (When the mouse is hovering xmm4 the Mousehint class must not be showend)
                    mov eax 0-1 | ExitP
                End_If
                mov dl 0 ; symbol
                mov D@InsideExpression 1
                jmp L8>

            .Else
                ; not preceded by '$', check for naked local "D@Local"
                Do
                    inc esi
                Loop_Until B$esi <> ' '

                If_Or B$esi+1 = '(', B$esi+1 = ')'
                    ; we are inside the close bracket
                    mov dl 0 ; symbol
                    mov D@InsideExpression 1
                    jmp L8>
                Else_If B$esi+1 <> '@'
                    ; not preceded by '$', check for data declaration
                    mov eax 0-1
                Else
                    mov al B$esi
                    mov B$MouseHint_SizeMarker al
                    call MouseHint_CopyAndCook esi, D@Output
                   ; mov eax 1
                End_If
                ExitP
            .End_If

        ..End_If
L8:
        dec esi
        mov al B$esi
    .End_While

L7:
    mov al B$esi-1
    mov B$MouseHint_SizeMarker al
    call MouseHint_CopyAndCook esi, D@Output

    ;mov eax 1

EndP
__________________________________________________________________________________

; Check to see if the mosue is not hovering after a true expression or text.
; I.e: when the code contains extra spaces after it. Ex: "mov eax D$ValueA4                 ". The mouse hover the empty space
Proc IsMouseHoverEmptySpace:
    Arguments @CurMousePos
    Local @EmptyForwardChars
    Uses esi, ecx

    mov esi D@CurMousePos | mov ecx esi
    mov D@EmptyForwardChars &FALSE

    While B$ecx <> 0
        ...If_Or B$ecx = ' ', B$ecx = '_', B$ecx = ';', B$ecx = "|"
            mov D@EmptyForwardChars &TRUE
        ...Else_If W$ecx = CRLF ; reached the end of line
            jmp L1>
        ...Else
            ;mov D@EmptyForwardChars &FALSE
            mov eax 0 | ExitP ; ok, we have some good char after it
        ...End_If
        inc ecx
    End_While

L1:

    If D@EmptyForwardChars = &FALSE
        mov eax 0-1 | ExitP
    End_If
    ; So, here at curpos untill the end of line, we can have only EmptyChars "Spaces", "_" ,";", "|" or a true code
    ; inside or not a comment

    ; we now need to see backwards untll the start of the line and see if below the mouse pointer we have
    ; also only empty lines
    mov ecx esi
    While B$ecx <> 0
        ...If_Or B$ecx = ' ', B$ecx = '_', B$ecx = ';', B$ecx = "|"
            mov D@EmptyForwardChars &TRUE
        ...Else_If W$ecx = CRLF ; reached the end of line
            jmp L1>
        ...Else
            If D@EmptyForwardChars = &TRUE
                mov eax 0-1 | ExitP
            End_if
            mov D@EmptyForwardChars &FALSE
        ...End_If
        dec ecx
    End_While

L1:

    If D@EmptyForwardChars = &TRUE
        mov eax 0-1
    Else
        xor eax eax
    End_if

EndP
__________________________________________________________________________________

Proc isMouseInsideExpression:
    Arguments @Input
    Uses esi

    xor eax eax
    mov esi D@Input

    While B$esi <> 0
        If B$esi = ')' ; if we find a next Close Sign, then definitelly the mouse maybe hovering a expression
            mov eax &TRUE | ExitP
        Else_If W$esi = CRLF ;
            ExitP
        Else_If B$esi = ';' ; next bytes is comnents, so we are not inside a expression
            ExitP
        Else_If B$esi = '(' ; if the next one is a open sign, then we may are inside a expression or not.
                            ; We need simply to count back the number of open signs untill the beginning of the line
                            ; if the total amount is odd we are inside a expression, otherwise we are not.
            jmp L1>
        End_If
        inc esi
    End_While

L1:

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

; Check if the specified buffer might be a string. It is rejected if it contains
; non-printable chars or is empty (starts with zero).
; Output in AH! [1 (yes) | 0 (no)]

Proc IsString:
    Arguments @Buffer, @Size
    Uses esi

    mov ah 0, esi D@Buffer, ecx D@Size
L0: While B$esi <> 0
        lodsb
        mov ah 1
        dec ecx | jz P9>
        cmp al ' ' | jb L9>
        cmp al 07F | jb L0<
        cmp al 080 | je L0<
        cmp al 091 | je L0<
        cmp al 092 | je L0<
        cmp al 0A0 | jb L9>
    End_While
    ExitP

L9: mov ah 0
EndP

____________________________________________________________________________________________

; This is a printf clone for internal use in RosAsm.

;[FormatString | &9=0 | C_Call FormatStr #L>1]
;[C_Call | &9=&9+4 | push #2 | #+1 | call #F | add esp &9]


[C_call | &9=0 | C_Call2 #1 #L>2]
[C_Call2 | &9=&9+4 | push #2 | #+1 | call #F | add esp &9]

;;
    Formats a string
    The stack must be cleared by the caller - use FormatString macro!
    Invokation: C_call FormatStr PatternString OutputString [Linked parameters in order of occurence]

 Link  | Converted to                           | Expected parameter
 ______|________________________________________|____________________
 %s    | string                                 | address of string
 %d    | decimal                                | dword immediate
 %-d   | Signed decimal                         | dword immediate
 %x    | hex with leading 0's                   | dword immediate
 %b    | Binary with no leading 0's             | dword immediate
 % b   | Binary with no leading 0's grouped by  | dword immediate
       | Bytes separated with a space
 %_b   | Binary with no leading 0's grouped by  | dword immediate
       | Bytes separated with a underscore "_"
 %rb   | Binary with no leading 0's grouped by  | dword immediate
       | Bytes separated with Full RosAsm Syntax
       |  Ex: 0000_0000__0000_0001__1110_1010__1101_1000
 %tb   | Binary with no leading 0's grouped by  | dword immediate
       | Bytes separated with RosAsm Syntax
       | truncated on words Ex: 0000_0001__1110_1010__1101_1000

Flags

4 = Use in binary string convertion. Assume the data is a single word to convert to binary string.
    The token must be placed immediatelly after the "%" char followed by the binary token representation. Ex:
    C_call FormatStr edi, {'%4 b ' 0}, eax

1 = Use in binary string convertion Assume the data is a single byte to convert to binary string.
    The token must be placed immediatelly after the "%" char followed by the binary token representation. Ex:
    C_call FormatStr edi, {'%1 b ' 0}, eax

Ref: Updated in 05/05/2020
;;

Proc FormatStr:
    Arguments @Output, @Input, @ArgList
    Local @DataSizeLimit
    Uses esi, edi, ebx, edx, ecx

    mov esi D@Input ; pattern string >> esi
    mov edi D@Output ; output buffer >> edi
    lea eax D@ArgList | mov ebx eax
    mov D@DataSizeLimit 0
    .Do

        ...If B$esi = '%'

            If B$esi+1 = '4' ; assume word size data.
                mov D@DataSizeLimit 4
                inc esi
            Else_If B$esi+1 = '1' ; assume byte size data. (For binary convertion)
                mov D@DataSizeLimit 1
                inc esi
            End_If
            ;mov D@IsSignedDecimal &FALSE
            ;On W$esi+1 = '-d', mov D@IsSignedDecimal &TRUE

            ..If B$esi+1 = 's'

                ; Copy string to buffer
                If D$ebx <> 0
                    call CopyStringEx D$ebx, edi | add edi eax
                End_If
                add ebx 4
                add esi 2

            ..Else_If B$esi+1 = 'x'

                ; Convert to hex
                call dwtoHex_Ex2 D$ebx, edi, &FALSE
                add edi eax
                add ebx 4
                add esi 2

            ..Else_If_Or B$esi+1 = 'd', W$esi+1 = '-d'

                mov eax D$ebx
                .If W$esi+1 = '-d'
                    If eax <> 0
                        neg eax
                        mov B$edi '-' | inc edi
                    End_If
                    inc esi
                .End_If

                ; Convert integer to decimal representation
                call utoa_exLingo eax, edi
                mov edi eax
                add ebx 4
                add esi 2

            ..Else_If_Or B$esi+1 = 'b', W$esi+1 = '_b', W$esi+1 = ' b', W$esi+1 = 'rb', W$esi+1 = 'tb'

                If W$esi+1 = '_b'
                    mov eax FMT_BIN_UNDERSCORE
                    inc esi
                Else_If W$esi+1 = ' b'
                    mov eax FMT_BIN_SPACE
                    inc esi
                Else_If W$esi+1 = 'rb'
                    mov eax FMT_BIN_ROSASM
                    inc esi
                Else_If W$esi+1 = 'tb'
                    mov eax FMT_BIN_ROSASM_TRUNCATE
                    inc esi
                Else
                    mov eax FMT_BIN_DEFAULT
                End_If

                If D@DataSizeLimit = 1

                    call ByteToBin D$ebx, edi, eax
                    add edi eax
                    add ebx 4
                    add esi 2

                Else_If D@DataSizeLimit = 4

                    call WordToBin D$ebx, edi, eax
                    add edi eax
                    add ebx 4
                    add esi 2
                Else
                    call DWordToBin D$ebx, edi, eax
                    add edi eax
                    add ebx 4
                    add esi 2
                End_If
            ..Else
                inc esi ; bypass "%" and copy next byte
                movsb
            ..End_If

        ...Else
            movsb
        ...EndIf

    .Loop_Until B$esi = 0

    ; Return stringlength in eax
    mov eax edi
    sub eax D@Output
    mov B$edi 0

EndSTD



; https://stackoverflow.com/questions/40811218/creating-an-x86-assembler-program-that-converts-an-integer-to-a-16-bit-binary-st
[FMT_BIN_DEFAULT            0] ; The Binary string will be displayed complete without spaces or underscore '_'. It wll display the whole 32 bits number
[FMT_BIN_UNDERSCORE         00__0000_0001] ; A underscore sign '_' will be placed after the 15th bit. Ex: 00010000_00000000
[FMT_BIN_SPACE              00__0000_0010] ; A space ' ' will be placed after the 15th bit. Ex: 00010000 00000000
[FMT_BIN_ROSASM             00__0000_0100] ; the binary string will be written in default RosAsm style. So, grouped in words separated with single _'" and dwords separated by "__'. Ex: 16 = 00__0000_0000__0001_0000
[FMT_BIN_ROSASM_TRUNCATE    00__0000_1000] ; the binary string will be written in default RosAsm style, but truncated on the 1st bytes whenever they are 0.
                                           ; So, grouped in words separated with single _'" and dwords separated by "__'. Ex: 16 = 00__0001_0000
                                           ; 0 to 255 = 00__0000_0001 to 00__1111_1111  0FF
                                           ; 256 to 65535 = 00__0000_0001__0000_0000 to 00__1111_1111__1111_1111 0FFFF
                                           ; 65536 to 16777215 = 00__0000_0001__0000_0000__0000_0000 to 00__1111_1111__1111_1111__1111_1111 = 16777215  0FFFFFF
                                           ; 16777216 to 4294967295 = 00__0000_0001__0000_0000__0000_0000__0000_0000 to 00__1111_1111__1111_1111__1111_1111__1111_1111 = 4294967295  0FFFFFFFF

;[LODWORDDis 0
; HIDWORDDis 2]

; only used for SSSE3
[<16 shuf_broadcast_hi_lo:
        B$ 1,1,1,1, 1,1,1,1     ; broadcast the second 8 bits to the first 8 bytes
        B$ 0,0,0,0, 0,0,0,0]     ; broadcast the first 8 bits to the second 8 bytes

  ; select the relevant bit within each byte, from high to low for printing
[<16 bitmask:   B$ 128, 64, 32, 16,
            B$ 8, 4, 2, 1,
            B$ 128, 64, 32, 16,
            B$ 8, 4, 2, 1]

[<16 ascii_ones: '1' #16]

Proc DwordToBin:
    Arguments @Number, @Output, @Flag

    movd xmm0 D@Number    ; 32-bit load even though we only care about the low 16 bits.
    movzx eax W@Number+2 ; HiDword 1st. bits 16 to 31
    movd xmm1 eax
    ; print
    punpcklbw xmm1 xmm1              ; llhh      (from low to high byte elements)
    pshuflw xmm1 xmm1 {SHUFFLE 3,3,2,2};5;00_011   ; 5 hhhhllll
    punpckldq xmm1 xmm1              ; hhhhhhhhllllllll
    pand  xmm1 X$bitmask ; each input bit is now isolated within the corresponding output byte
    ; compare it against zero
    pxor    xmm2,xmm2
    pcmpeqb xmm1, xmm2          ; -1 in elements that are 0,   0 in elements with any non-zero bit.
    paddb xmm1 X$ascii_ones  ; '1' +  (-1 or 0) = '0' or 1'

    ; Now we do the LoDword. bits 0 to 15
    ; to print left-to-right, we need the high bit to go in the first (low) byte
    punpcklbw xmm0 xmm0              ; llhh      (from low to high byte elements)
    pshuflw xmm0 xmm0 {SHUFFLE 3,3,2,2};5;00_011   ; 5 hhhhllll
    punpckldq xmm0 xmm0              ; hhhhhhhhllllllll

    ; or with SSSE3:
    ; pshufb  xmm0, X$[shuf_broadcast_hi_lo]  ; SSSE3

    pand  xmm0 X$bitmask ; each input bit is now isolated within the corresponding output byte

    ; compare it against zero
    pxor    xmm2,xmm2
    pcmpeqb xmm0, xmm2          ; -1 in elements that are 0,   0 in elements with any non-zero bit.

    paddb xmm0 X$ascii_ones  ; '1' +  (-1 or 0) = '0' or 1'

    mov eax D@Flag
    ..Test_If eax FMT_BIN_UNDERSCORE

        mov eax D@Output        ; buffer pointer
        movups X$eax xmm1       ; 1st copy the hiWord to the 1st 16 bytes in output
        pshufd XMM1 XMM1 SSE_SWAP_QWORDS ; swap to we get their dwords (used here as bytes)
        mov B$eax+8 '_'         ; place the separator
        movq X$eax+9 xmm1

        ; do the same for the next word

        mov B$eax+17 '_'         ; place the separator
        movups X$eax+18 xmm0     ; 2nd we copy the LoDWord to the 2nd 16 bytes in output
        pshufd XMM0 XMM0 SSE_SWAP_QWORDS ; swap to we get their dwords (used here as bytes)
        mov B$eax+26 '_'         ; place the separator
        movq X$eax+27 xmm0

        mov B$eax+35 0    ; terminating zero
        mov eax 35

    ..Test_Else_If eax FMT_BIN_SPACE

        mov eax D@Output        ; buffer pointer
        movups X$eax xmm1       ; 1st copy the hiWord to the 1st 16 bytes in output
        pshufd XMM1 XMM1 SSE_SWAP_QWORDS ; swap to we get their dwords (used here as bytes)
        mov B$eax+8 ' '         ; place the separator
        movq X$eax+9 xmm1

        ; do the same for the next word

        mov B$eax+17 ' '         ; place the separator
        movups X$eax+18 xmm0     ; 2nd we copy the LoDWord to the 2nd 16 bytes in output
        pshufd XMM0 XMM0 SSE_SWAP_QWORDS ; swap to we get their dwords (used here as bytes)
        mov B$eax+26 ' '         ; place the separator
        movq X$eax+27 xmm0

        mov B$eax+35 0    ; terminating zero
        mov eax 35

    ..Test_Else_If eax FMT_BIN_ROSASM

        mov eax D@Output        ; buffer pointer
        movups X$eax xmm1       ; 1st copy the hiDWord to the 1st 16 bytes in output
        pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
        mov B$eax+4 '_'         ; place the separator
        movq X$eax+5 xmm1

        mov W$eax+9 '__'         ; place the separator
        pshufd XMM1 XMM1 SSE_SWAP_QWORDS ; Swap to we get the 2nd byte. We need to swap the qword to get the proper dword pos
        movups X$eax+11 xmm1
        pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get the correct byte. Swap only the low word.
        mov B$eax+15 '_'         ; place the separator
        movq X$eax+16 xmm1

        mov W$eax+20 '__'         ; place the separator on the next dword


        movups X$eax+22 xmm0       ; 1st copy the hiWord to the 1st 16 bytes in output
        pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
        mov B$eax+26 '_'         ; place the separator
        movq X$eax+27 xmm0

        mov W$eax+31 '__'         ; place the separator
        pshufd XMM0 XMM0 SSE_SWAP_QWORDS
        movups X$eax+33 xmm0
        pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
        mov B$eax+37 '_'         ; place the separator
        movq X$eax+38 xmm0
        mov B$eax+42 0    ; terminating zero
        mov eax 42

    ..Test_Else_If eax FMT_BIN_ROSASM_TRUNCATE

        mov eax D@Number
        .Test_If eax 0FF000000      ;00__1111_1111__0000_0000__0000_0000__0000_0000 ; If eax > 0FFFFFF. eax >= 16777216

            mov eax D@Output        ; buffer pointer
            movups X$eax xmm1       ; 1st copy the hiDWord to the 1st 16 bytes in output
            pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+4 '_'         ; place the separator
            movq X$eax+5 xmm1

            mov W$eax+9 '__'         ; place the separator
            pshufd XMM1 XMM1 SSE_SWAP_QWORDS ; Swap to we get the 2nd byte. We need to swap the qword to get the proper dword pos
            movups X$eax+11 xmm1
            pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get the correct byte. Swap only the low word.
            mov B$eax+15 '_'         ; place the separator
            movq X$eax+16 xmm1

            mov W$eax+20 '__'         ; place the separator on the next dword

            movups X$eax+22 xmm0       ; 1st copy the hiWord to the 1st 16 bytes in output
            pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+26 '_'         ; place the separator
            movq X$eax+27 xmm0

            mov W$eax+31 '__'         ; place the separator
            pshufd XMM0 XMM0 SSE_SWAP_QWORDS
            movups X$eax+33 xmm0
            pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+37 '_'         ; place the separator
            movq X$eax+38 xmm0
            mov B$eax+42 0    ; terminating zero
            mov eax 42

        .Test_Else_If eax 0FF0000   ;00__1111_1111__0000_0000__0000_0000 ; If eax > 0FFFF. eax >= 65536

            mov eax D@Output        ; buffer pointer
            pshufd XMM1 XMM1 SSE_SWAP_QWORDS ; Swap to we get the 2nd byte. We need to swap the qword to get the proper dword pos
            movups X$eax xmm1
            pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get the correct byte. Swap only the low word.
            mov B$eax+4 '_'         ; place the separator
            movq X$eax+5 xmm1

            mov W$eax+9 '__'         ; place the separator on the next dword

            movups X$eax+11 xmm0       ; 1st copy the hiWord to the 1st 16 bytes in output
            pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+15 '_'         ; place the separator
            movq X$eax+16 xmm0

            mov W$eax+20 '__'         ; place the separator
            pshufd XMM0 XMM0 SSE_SWAP_QWORDS
            movups X$eax+22 xmm0
            pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+26 '_'         ; place the separator
            movq X$eax+27 xmm0
            mov B$eax+31 0    ; terminating zero
            mov eax 31

        .Test_Else_If eax 0FF00   ;00__1111_1111__0000_0000 ; If eax > 0FF. eax >= 256

            mov eax D@Output        ; buffer pointer
            movups X$eax xmm0       ; 1st copy the hiWord to the 1st 16 bytes in output
            pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+4 '_'         ; place the separator
            movq X$eax+5 xmm0

            mov W$eax+9 '__'         ; place the separator
            pshufd XMM0 XMM0 SSE_SWAP_QWORDS
            movups X$eax+11 xmm0
            pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+15 '_'         ; place the separator
            movq X$eax+16 xmm0
            mov B$eax+20 0    ; terminating zero
            mov eax 20

        .Test_Else

            mov eax D@Output        ; buffer pointer
            pshufd XMM0 XMM0 SSE_SWAP_QWORDS
            movups X$eax xmm0
            pshuflw XMM0 XMM0 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+4 '_'         ; place the separator
            movq X$eax+5 xmm0
            mov B$eax+9 0    ; terminating zero
            mov eax 9

        .Test_End

    ..Test_Else

        mov eax D@Output        ; buffer pointer
        movups X$eax xmm1  ; 1st copy the hiWord to the 1st 16 bytes in output
        movups X$eax+16 xmm0  ; Then we copy the LoWord to the rest of the output
        mov B$eax+32 0    ; terminating zero
        mov eax 32

    ..Test_End

EndP

______________________________________________________________________________


[MMXHexBinTble:
MMXHexBinTbleData0: B$ '0', '0', '0', '0', '0', '0', '0', '0'
MMXHexBinTbleData1: B$ '0', '0', '0', '0', '0', '0', '0', '1'
MMXHexBinTbleData2: B$ '0', '0', '0', '0', '0', '0', '1', '0'
MMXHexBinTbleData3: B$ '0', '0', '0', '0', '0', '0', '1', '1'
MMXHexBinTbleData4: B$ '0', '0', '0', '0', '0', '1', '0', '0'
MMXHexBinTbleData5: B$ '0', '0', '0', '0', '0', '1', '0', '1'
MMXHexBinTbleData6: B$ '0', '0', '0', '0', '0', '1', '1', '0'
MMXHexBinTbleData7: B$ '0', '0', '0', '0', '0', '1', '1', '1'
MMXHexBinTbleData8: B$ '0', '0', '0', '0', '1', '0', '0', '0'
MMXHexBinTbleData9: B$ '0', '0', '0', '0', '1', '0', '0', '1'
MMXHexBinTbleData10: B$ '0', '0', '0', '0', '1', '0', '1', '0'
MMXHexBinTbleData11: B$ '0', '0', '0', '0', '1', '0', '1', '1'
MMXHexBinTbleData12: B$ '0', '0', '0', '0', '1', '1', '0', '0'
MMXHexBinTbleData13: B$ '0', '0', '0', '0', '1', '1', '0', '1'
MMXHexBinTbleData14: B$ '0', '0', '0', '0', '1', '1', '1', '0'
MMXHexBinTbleData15: B$ '0', '0', '0', '0', '1', '1', '1', '1'
MMXHexBinTbleData16: B$ '0', '0', '0', '1', '0', '0', '0', '0'
MMXHexBinTbleData17: B$ '0', '0', '0', '1', '0', '0', '0', '1'
MMXHexBinTbleData18: B$ '0', '0', '0', '1', '0', '0', '1', '0'
MMXHexBinTbleData19: B$ '0', '0', '0', '1', '0', '0', '1', '1'
MMXHexBinTbleData20: B$ '0', '0', '0', '1', '0', '1', '0', '0'
MMXHexBinTbleData21: B$ '0', '0', '0', '1', '0', '1', '0', '1'
MMXHexBinTbleData22: B$ '0', '0', '0', '1', '0', '1', '1', '0'
MMXHexBinTbleData23: B$ '0', '0', '0', '1', '0', '1', '1', '1'
MMXHexBinTbleData24: B$ '0', '0', '0', '1', '1', '0', '0', '0'
MMXHexBinTbleData25: B$ '0', '0', '0', '1', '1', '0', '0', '1'
MMXHexBinTbleData26: B$ '0', '0', '0', '1', '1', '0', '1', '0'
MMXHexBinTbleData27: B$ '0', '0', '0', '1', '1', '0', '1', '1'
MMXHexBinTbleData28: B$ '0', '0', '0', '1', '1', '1', '0', '0'
MMXHexBinTbleData29: B$ '0', '0', '0', '1', '1', '1', '0', '1'
MMXHexBinTbleData30: B$ '0', '0', '0', '1', '1', '1', '1', '0'
MMXHexBinTbleData31: B$ '0', '0', '0', '1', '1', '1', '1', '1'
MMXHexBinTbleData32: B$ '0', '0', '1', '0', '0', '0', '0', '0'
MMXHexBinTbleData33: B$ '0', '0', '1', '0', '0', '0', '0', '1'
MMXHexBinTbleData34: B$ '0', '0', '1', '0', '0', '0', '1', '0'
MMXHexBinTbleData35: B$ '0', '0', '1', '0', '0', '0', '1', '1'
MMXHexBinTbleData36: B$ '0', '0', '1', '0', '0', '1', '0', '0'
MMXHexBinTbleData37: B$ '0', '0', '1', '0', '0', '1', '0', '1'
MMXHexBinTbleData38: B$ '0', '0', '1', '0', '0', '1', '1', '0'
MMXHexBinTbleData39: B$ '0', '0', '1', '0', '0', '1', '1', '1'
MMXHexBinTbleData40: B$ '0', '0', '0', '1', '1', '0', '0', '0'
MMXHexBinTbleData41: B$ '0', '0', '0', '1', '1', '0', '0', '1'
MMXHexBinTbleData42: B$ '0', '0', '1', '0', '1', '0', '1', '0'
MMXHexBinTbleData43: B$ '0', '0', '1', '0', '1', '0', '1', '1'
MMXHexBinTbleData44: B$ '0', '0', '1', '0', '1', '1', '0', '0'
MMXHexBinTbleData45: B$ '0', '0', '1', '0', '1', '1', '0', '1'
MMXHexBinTbleData46: B$ '0', '0', '1', '0', '1', '1', '1', '0'
MMXHexBinTbleData47: B$ '0', '0', '1', '0', '1', '1', '1', '1'
MMXHexBinTbleData48: B$ '0', '0', '1', '1', '0', '0', '0', '0'
MMXHexBinTbleData49: B$ '0', '0', '1', '1', '0', '0', '0', '1'
MMXHexBinTbleData50: B$ '0', '0', '1', '1', '0', '0', '1', '0'
MMXHexBinTbleData51: B$ '0', '0', '1', '1', '0', '0', '1', '1'
MMXHexBinTbleData52: B$ '0', '0', '1', '1', '0', '1', '0', '0'
MMXHexBinTbleData53: B$ '0', '0', '1', '1', '0', '1', '0', '1'
MMXHexBinTbleData54: B$ '0', '0', '1', '1', '0', '1', '1', '0'
MMXHexBinTbleData55: B$ '0', '0', '1', '1', '0', '1', '1', '1'
MMXHexBinTbleData56: B$ '0', '0', '1', '1', '1', '0', '0', '0'
MMXHexBinTbleData57: B$ '0', '0', '1', '1', '1', '0', '0', '1'
MMXHexBinTbleData58: B$ '0', '0', '1', '1', '1', '0', '1', '0'
MMXHexBinTbleData59: B$ '0', '0', '1', '1', '1', '0', '1', '1'
MMXHexBinTbleData60: B$ '0', '0', '1', '1', '1', '1', '0', '0'
MMXHexBinTbleData61: B$ '0', '0', '1', '1', '1', '1', '0', '1'
MMXHexBinTbleData62: B$ '0', '0', '1', '1', '1', '1', '1', '0'
MMXHexBinTbleData63: B$ '0', '0', '1', '1', '1', '1', '1', '1'
MMXHexBinTbleData64: B$ '0', '1', '0', '0', '0', '0', '0', '0'
MMXHexBinTbleData65: B$ '0', '1', '0', '0', '0', '0', '0', '1'
MMXHexBinTbleData66: B$ '0', '1', '0', '0', '0', '0', '1', '0'
MMXHexBinTbleData67: B$ '0', '1', '0', '0', '0', '0', '1', '1'
MMXHexBinTbleData68: B$ '0', '1', '0', '0', '0', '1', '0', '0'
MMXHexBinTbleData69: B$ '0', '1', '0', '0', '0', '1', '0', '1'
MMXHexBinTbleData70: B$ '0', '1', '0', '0', '0', '1', '1', '0'
MMXHexBinTbleData71: B$ '0', '1', '0', '0', '0', '1', '1', '1'
MMXHexBinTbleData72: B$ '0', '1', '0', '0', '1', '0', '0', '0'
MMXHexBinTbleData73: B$ '0', '1', '0', '0', '1', '0', '0', '1'
MMXHexBinTbleData74: B$ '0', '1', '0', '0', '1', '0', '1', '0'
MMXHexBinTbleData75: B$ '0', '1', '0', '0', '1', '0', '1', '1'
MMXHexBinTbleData76: B$ '0', '1', '0', '0', '1', '1', '0', '0'
MMXHexBinTbleData77: B$ '0', '1', '0', '0', '1', '1', '0', '1'
MMXHexBinTbleData78: B$ '0', '1', '0', '0', '1', '1', '1', '0'
MMXHexBinTbleData79: B$ '0', '1', '0', '0', '1', '1', '1', '1'
MMXHexBinTbleData80: B$ '0', '1', '0', '1', '0', '0', '0', '0'
MMXHexBinTbleData81: B$ '0', '1', '0', '1', '0', '0', '0', '1'
MMXHexBinTbleData82: B$ '0', '1', '0', '1', '0', '0', '1', '0'
MMXHexBinTbleData83: B$ '0', '1', '0', '1', '0', '0', '1', '1'
MMXHexBinTbleData84: B$ '0', '1', '0', '1', '0', '1', '0', '0'
MMXHexBinTbleData85: B$ '0', '1', '0', '1', '0', '1', '0', '1'
MMXHexBinTbleData86: B$ '0', '1', '0', '1', '0', '1', '1', '0'
MMXHexBinTbleData87: B$ '0', '1', '0', '1', '0', '1', '1', '1'
MMXHexBinTbleData88: B$ '0', '1', '0', '1', '1', '0', '0', '0'
MMXHexBinTbleData89: B$ '0', '1', '0', '1', '1', '0', '0', '1'
MMXHexBinTbleData90: B$ '0', '1', '0', '1', '1', '0', '1', '0'
MMXHexBinTbleData91: B$ '0', '1', '0', '1', '1', '0', '1', '1'
MMXHexBinTbleData92: B$ '0', '1', '0', '1', '1', '1', '0', '0'
MMXHexBinTbleData93: B$ '0', '1', '0', '1', '1', '1', '0', '1'
MMXHexBinTbleData94: B$ '0', '1', '0', '1', '1', '1', '1', '0'
MMXHexBinTbleData95: B$ '0', '1', '0', '1', '1', '1', '1', '1'
MMXHexBinTbleData96: B$ '0', '1', '1', '0', '0', '0', '0', '0'
MMXHexBinTbleData97: B$ '0', '1', '1', '0', '0', '0', '0', '1'
MMXHexBinTbleData98: B$ '0', '1', '1', '0', '0', '0', '1', '0'
MMXHexBinTbleData99: B$ '0', '1', '1', '0', '0', '0', '1', '1'
MMXHexBinTbleData100: B$ '0', '1', '1', '0', '0', '1', '0', '0'
MMXHexBinTbleData101: B$ '0', '1', '1', '0', '0', '1', '0', '1'
MMXHexBinTbleData102: B$ '0', '1', '1', '0', '0', '1', '1', '0'
MMXHexBinTbleData103: B$ '0', '1', '1', '0', '0', '1', '1', '1'
MMXHexBinTbleData104: B$ '0', '1', '1', '0', '1', '0', '0', '0'
MMXHexBinTbleData105: B$ '0', '1', '1', '0', '1', '0', '0', '1'
MMXHexBinTbleData106: B$ '0', '1', '1', '0', '1', '0', '1', '0'
MMXHexBinTbleData107: B$ '0', '1', '1', '0', '1', '0', '1', '1'
MMXHexBinTbleData108: B$ '0', '1', '1', '0', '1', '1', '0', '0'
MMXHexBinTbleData109: B$ '0', '1', '1', '0', '1', '1', '0', '1'
MMXHexBinTbleData110: B$ '0', '1', '1', '0', '1', '1', '1', '0'
MMXHexBinTbleData111: B$ '0', '1', '1', '0', '1', '1', '1', '1'
MMXHexBinTbleData112: B$ '0', '1', '1', '1', '0', '0', '0', '0'
MMXHexBinTbleData113: B$ '0', '1', '1', '1', '0', '0', '0', '1'
MMXHexBinTbleData114: B$ '0', '1', '1', '1', '0', '0', '1', '0'
MMXHexBinTbleData115: B$ '0', '1', '1', '1', '0', '0', '1', '1'
MMXHexBinTbleData116: B$ '0', '1', '1', '1', '0', '1', '0', '0'
MMXHexBinTbleData117: B$ '0', '1', '1', '1', '0', '1', '0', '1'
MMXHexBinTbleData118: B$ '0', '1', '1', '1', '0', '1', '1', '0'
MMXHexBinTbleData119: B$ '0', '1', '1', '1', '0', '1', '1', '1'
MMXHexBinTbleData120: B$ '0', '1', '1', '1', '1', '0', '0', '0'
MMXHexBinTbleData121: B$ '0', '1', '1', '1', '1', '0', '0', '1'
MMXHexBinTbleData122: B$ '0', '1', '1', '1', '1', '0', '1', '0'
MMXHexBinTbleData123: B$ '0', '1', '1', '1', '1', '0', '1', '1'
MMXHexBinTbleData124: B$ '0', '1', '1', '1', '1', '1', '0', '0'
MMXHexBinTbleData125: B$ '0', '1', '1', '1', '1', '1', '0', '1'
MMXHexBinTbleData126: B$ '0', '1', '1', '1', '1', '1', '1', '0'
MMXHexBinTbleData127: B$ '0', '1', '1', '1', '1', '1', '1', '1'
MMXHexBinTbleData128: B$ '1', '0', '0', '0', '0', '0', '0', '0'
MMXHexBinTbleData129: B$ '1', '0', '0', '0', '0', '0', '0', '1'
MMXHexBinTbleData130: B$ '1', '0', '0', '0', '0', '0', '1', '0'
MMXHexBinTbleData131: B$ '1', '0', '0', '0', '0', '0', '1', '1'
MMXHexBinTbleData132: B$ '1', '0', '0', '0', '0', '1', '0', '0'
MMXHexBinTbleData133: B$ '1', '0', '0', '0', '0', '1', '0', '1'
MMXHexBinTbleData134: B$ '1', '0', '0', '0', '0', '1', '1', '0'
MMXHexBinTbleData135: B$ '1', '0', '0', '0', '0', '1', '1', '1'
MMXHexBinTbleData136: B$ '1', '0', '0', '0', '1', '0', '0', '0'
MMXHexBinTbleData137: B$ '1', '0', '0', '0', '1', '0', '0', '1'
MMXHexBinTbleData138: B$ '1', '0', '0', '0', '1', '0', '1', '0'
MMXHexBinTbleData139: B$ '1', '0', '0', '0', '1', '0', '1', '1'
MMXHexBinTbleData140: B$ '1', '0', '0', '0', '1', '1', '0', '0'
MMXHexBinTbleData141: B$ '1', '0', '0', '0', '1', '1', '0', '1'
MMXHexBinTbleData142: B$ '1', '0', '0', '0', '1', '1', '1', '0'
MMXHexBinTbleData143: B$ '1', '0', '0', '0', '1', '1', '1', '1'
MMXHexBinTbleData144: B$ '1', '0', '0', '1', '0', '0', '0', '0'
MMXHexBinTbleData145: B$ '1', '0', '0', '1', '0', '0', '0', '1'
MMXHexBinTbleData146: B$ '1', '0', '0', '1', '0', '0', '1', '0'
MMXHexBinTbleData147: B$ '1', '0', '0', '1', '0', '0', '1', '1'
MMXHexBinTbleData148: B$ '1', '0', '0', '1', '0', '1', '0', '0'
MMXHexBinTbleData149: B$ '1', '0', '0', '1', '0', '1', '0', '1'
MMXHexBinTbleData150: B$ '1', '0', '0', '1', '0', '1', '1', '0'
MMXHexBinTbleData151: B$ '1', '0', '0', '1', '0', '1', '1', '1'
MMXHexBinTbleData152: B$ '1', '0', '0', '1', '1', '0', '0', '0'
MMXHexBinTbleData153: B$ '1', '0', '0', '1', '1', '0', '0', '1'
MMXHexBinTbleData154: B$ '1', '0', '0', '1', '1', '0', '1', '0'
MMXHexBinTbleData155: B$ '1', '0', '0', '1', '1', '0', '1', '1'
MMXHexBinTbleData156: B$ '1', '0', '0', '1', '1', '1', '0', '0'
MMXHexBinTbleData157: B$ '1', '0', '0', '1', '1', '1', '0', '1'
MMXHexBinTbleData158: B$ '1', '0', '0', '1', '1', '1', '1', '0'
MMXHexBinTbleData159: B$ '1', '0', '0', '1', '1', '1', '1', '1'
MMXHexBinTbleData160: B$ '1', '0', '1', '0', '0', '0', '0', '0'
MMXHexBinTbleData161: B$ '1', '0', '1', '0', '0', '0', '0', '1'
MMXHexBinTbleData162: B$ '1', '0', '1', '0', '0', '0', '1', '0'
MMXHexBinTbleData163: B$ '1', '0', '1', '0', '0', '0', '1', '1'
MMXHexBinTbleData164: B$ '1', '0', '1', '0', '0', '1', '0', '0'
MMXHexBinTbleData165: B$ '1', '0', '1', '0', '0', '1', '0', '1'
MMXHexBinTbleData166: B$ '1', '0', '1', '0', '0', '1', '1', '0'
MMXHexBinTbleData167: B$ '1', '0', '1', '0', '0', '1', '1', '1'
MMXHexBinTbleData168: B$ '1', '0', '1', '0', '1', '0', '0', '0'
MMXHexBinTbleData169: B$ '1', '0', '1', '0', '1', '0', '0', '1'
MMXHexBinTbleData170: B$ '1', '0', '1', '0', '1', '0', '1', '0'
MMXHexBinTbleData171: B$ '1', '0', '1', '0', '1', '0', '1', '1'
MMXHexBinTbleData172: B$ '1', '0', '1', '0', '1', '1', '0', '0'
MMXHexBinTbleData173: B$ '1', '0', '1', '0', '1', '1', '0', '1'
MMXHexBinTbleData174: B$ '1', '0', '1', '0', '1', '1', '1', '0'
MMXHexBinTbleData175: B$ '1', '0', '1', '0', '1', '1', '1', '1'
MMXHexBinTbleData176: B$ '1', '0', '1', '1', '0', '0', '0', '0'
MMXHexBinTbleData177: B$ '1', '0', '1', '1', '0', '0', '0', '1'
MMXHexBinTbleData178: B$ '1', '0', '1', '1', '0', '0', '1', '0'
MMXHexBinTbleData179: B$ '1', '0', '1', '1', '0', '0', '1', '1'
MMXHexBinTbleData180: B$ '1', '0', '1', '1', '0', '1', '0', '0'
MMXHexBinTbleData181: B$ '1', '0', '1', '1', '0', '1', '0', '1'
MMXHexBinTbleData182: B$ '1', '0', '1', '1', '0', '1', '1', '0'
MMXHexBinTbleData183: B$ '1', '0', '1', '1', '0', '1', '1', '1'
MMXHexBinTbleData184: B$ '1', '0', '1', '1', '1', '0', '0', '0'
MMXHexBinTbleData185: B$ '1', '0', '1', '1', '1', '0', '0', '1'
MMXHexBinTbleData186: B$ '1', '0', '1', '1', '1', '0', '1', '0'
MMXHexBinTbleData187: B$ '1', '0', '1', '1', '1', '0', '1', '1'
MMXHexBinTbleData188: B$ '1', '0', '1', '1', '1', '1', '0', '0'
MMXHexBinTbleData189: B$ '1', '0', '1', '1', '1', '1', '0', '1'
MMXHexBinTbleData190: B$ '1', '0', '1', '1', '1', '1', '1', '0'
MMXHexBinTbleData191: B$ '1', '0', '1', '1', '1', '1', '1', '1'
MMXHexBinTbleData192: B$ '1', '1', '0', '0', '0', '0', '0', '0'
MMXHexBinTbleData193: B$ '1', '1', '0', '0', '0', '0', '0', '1'
MMXHexBinTbleData194: B$ '1', '1', '0', '0', '0', '0', '1', '0'
MMXHexBinTbleData195: B$ '1', '1', '0', '0', '0', '0', '1', '1'
MMXHexBinTbleData196: B$ '1', '1', '0', '0', '0', '1', '0', '0'
MMXHexBinTbleData197: B$ '1', '1', '0', '0', '0', '1', '0', '1'
MMXHexBinTbleData198: B$ '1', '1', '0', '0', '0', '1', '1', '0'
MMXHexBinTbleData199: B$ '1', '1', '0', '0', '0', '1', '1', '1'
MMXHexBinTbleData200: B$ '1', '1', '0', '0', '1', '0', '0', '0'
MMXHexBinTbleData201: B$ '1', '1', '0', '0', '1', '0', '0', '1'
MMXHexBinTbleData202: B$ '1', '1', '0', '0', '1', '0', '1', '0'
MMXHexBinTbleData203: B$ '1', '1', '0', '0', '1', '0', '1', '1'
MMXHexBinTbleData204: B$ '1', '1', '0', '0', '1', '1', '0', '0'
MMXHexBinTbleData205: B$ '1', '1', '0', '0', '1', '1', '0', '1'
MMXHexBinTbleData206: B$ '1', '1', '0', '0', '1', '1', '1', '0'
MMXHexBinTbleData207: B$ '1', '1', '0', '0', '1', '1', '1', '1'
MMXHexBinTbleData208: B$ '1', '1', '0', '1', '0', '0', '0', '0'
MMXHexBinTbleData209: B$ '1', '1', '0', '1', '0', '0', '0', '1'
MMXHexBinTbleData210: B$ '1', '1', '0', '1', '0', '0', '1', '0'
MMXHexBinTbleData211: B$ '1', '1', '0', '1', '0', '0', '1', '1'
MMXHexBinTbleData212: B$ '1', '1', '0', '1', '0', '1', '0', '0'
MMXHexBinTbleData213: B$ '1', '1', '0', '1', '0', '1', '0', '1'
MMXHexBinTbleData214: B$ '1', '1', '0', '1', '0', '1', '1', '0'
MMXHexBinTbleData215: B$ '1', '1', '0', '1', '0', '1', '1', '1'
MMXHexBinTbleData216: B$ '1', '1', '0', '1', '1', '0', '0', '0'
MMXHexBinTbleData217: B$ '1', '1', '0', '1', '1', '0', '0', '1'
MMXHexBinTbleData218: B$ '1', '1', '0', '1', '1', '0', '1', '0'
MMXHexBinTbleData219: B$ '1', '1', '0', '1', '1', '0', '1', '1'
MMXHexBinTbleData220: B$ '1', '1', '0', '1', '1', '1', '0', '0'
MMXHexBinTbleData221: B$ '1', '1', '0', '1', '1', '1', '0', '1'
MMXHexBinTbleData222: B$ '1', '1', '0', '1', '1', '1', '1', '0'
MMXHexBinTbleData223: B$ '1', '1', '0', '1', '1', '1', '1', '1'
MMXHexBinTbleData224: B$ '1', '1', '1', '0', '0', '0', '0', '0'
MMXHexBinTbleData225: B$ '1', '1', '1', '0', '0', '0', '0', '1'
MMXHexBinTbleData226: B$ '1', '1', '1', '0', '0', '0', '1', '0'
MMXHexBinTbleData227: B$ '1', '1', '1', '0', '0', '0', '1', '1'
MMXHexBinTbleData228: B$ '1', '1', '1', '0', '0', '1', '0', '0'
MMXHexBinTbleData229: B$ '1', '1', '1', '0', '0', '1', '0', '1'
MMXHexBinTbleData230: B$ '1', '1', '1', '0', '0', '1', '1', '0'
MMXHexBinTbleData231: B$ '1', '1', '1', '0', '0', '1', '1', '1'
MMXHexBinTbleData232: B$ '1', '1', '1', '0', '1', '0', '0', '0'
MMXHexBinTbleData233: B$ '1', '1', '1', '0', '1', '0', '0', '1'
MMXHexBinTbleData234: B$ '1', '1', '1', '0', '1', '0', '1', '0'
MMXHexBinTbleData235: B$ '1', '1', '1', '0', '1', '0', '1', '1'
MMXHexBinTbleData236: B$ '1', '1', '1', '0', '1', '1', '0', '0'
MMXHexBinTbleData237: B$ '1', '1', '1', '0', '1', '1', '0', '1'
MMXHexBinTbleData238: B$ '1', '1', '1', '0', '1', '1', '1', '0'
MMXHexBinTbleData239: B$ '1', '1', '1', '0', '1', '1', '1', '1'
MMXHexBinTbleData240: B$ '1', '1', '1', '1', '0', '0', '0', '0'
MMXHexBinTbleData241: B$ '1', '1', '1', '1', '0', '0', '0', '1'
MMXHexBinTbleData242: B$ '1', '1', '1', '1', '0', '0', '1', '0'
MMXHexBinTbleData243: B$ '1', '1', '1', '1', '0', '0', '1', '1'
MMXHexBinTbleData244: B$ '1', '1', '1', '1', '0', '1', '0', '0'
MMXHexBinTbleData245: B$ '1', '1', '1', '1', '0', '1', '0', '1'
MMXHexBinTbleData246: B$ '1', '1', '1', '1', '0', '1', '1', '0'
MMXHexBinTbleData247: B$ '1', '1', '1', '1', '0', '1', '1', '1'
MMXHexBinTbleData248: B$ '1', '1', '1', '1', '1', '0', '0', '0'
MMXHexBinTbleData249: B$ '1', '1', '1', '1', '1', '0', '0', '1'
MMXHexBinTbleData250: B$ '1', '1', '1', '1', '1', '0', '1', '0'
MMXHexBinTbleData251: B$ '1', '1', '1', '1', '1', '0', '1', '1'
MMXHexBinTbleData252: B$ '1', '1', '1', '1', '1', '1', '0', '0'
MMXHexBinTbleData253: B$ '1', '1', '1', '1', '1', '1', '0', '1'
MMXHexBinTbleData254: B$ '1', '1', '1', '1', '1', '1', '1', '0'
MMXHexBinTbleData255: B$ '1', '1', '1', '1', '1', '1', '1', '1']


Proc ByteToBin:
    Arguments @Number, @Output, @Flag

    movzx eax B@Number ; make sure we are dealing with a Byte
    movq mm0 Q$MMXHexBinTble+eax*8
    mov eax D@Flag

    ..Test_If eax FMT_BIN_ROSASM

        mov eax D@Output        ; buffer pointer
        movd D$eax mm0
        pshufw mm0 mm0 {SHUFFLE 2,3,0,1};SSE_INVERT_DWORDS;SSE_SWAP_DWORDS ; swap to we get their dwords (used here as bytes)
        mov B$eax+4 '_'         ; place the separator
        movd D$eax+5 mm0

        mov B$eax+9 0    ; terminating zero
        mov eax 9

    ..Test_Else_If eax FMT_BIN_SPACE

        mov eax D@Output        ; buffer pointer
        movd D$eax mm0
        pshufw mm0 mm0 {SHUFFLE 2,3,0,1};SSE_INVERT_DWORDS;SSE_SWAP_DWORDS ; swap to we get their dwords (used here as bytes)
        mov B$eax+4 ' '         ; place the separator
        movd D$eax+5 mm0

        mov B$eax+9 0    ; terminating zero
        mov eax 9

    ..Test_Else

        mov eax D@Output        ; buffer pointer
        movq Q$eax mm0  ; 1st copy the hiWord to the 1st 16 bytes in output
        mov B$eax+8 0    ; terminating zero
        mov eax 8

    ..Test_End

EndP
________________________________________________________________________________________

Proc WordToBin:
    Arguments @Number, @Output, @Flag

    movzx eax W@Number ; HiDword 1st. bits 16 to 31
    movd xmm1 eax

    punpcklbw xmm1 xmm1              ; llhh      (from low to high byte elements)
    pshuflw xmm1 xmm1 {SHUFFLE 3,3,2,2};5;00_011   ; 5 hhhhllll
    punpckldq xmm1 xmm1              ; hhhhhhhhllllllll
    pand  xmm1 X$bitmask ; each input bit is now isolated within the corresponding output byte
    ; compare it against zero
    pxor    xmm2 xmm2
    pcmpeqb xmm1 xmm2          ; -1 in elements that are 0,   0 in elements with any non-zero bit.
    paddb xmm1 X$ascii_ones  ; '1' +  (-1 or 0) = '0' or 1'
    mov eax D@Flag
    ..Test_If eax FMT_BIN_UNDERSCORE

        mov eax D@Output        ; buffer pointer
        movups X$eax xmm1       ; 1st copy the hiWord to the 1st 16 bytes in output
        pshufd XMM1 XMM1 SSE_SWAP_QWORDS ; swap to we get their dwords (used here as bytes)
        mov B$eax+8 '_'         ; place the separator
        movq X$eax+9 xmm1

        mov B$eax+17 0    ; terminating zero
        mov eax 17

    ..Test_Else_If eax FMT_BIN_SPACE

        mov eax D@Output        ; buffer pointer
        movups X$eax xmm1       ; 1st copy the hiWord to the 1st 16 bytes in output
        pshufd XMM1 XMM1 SSE_SWAP_QWORDS ; swap to we get their dwords (used here as bytes)
        mov B$eax+8 ' '         ; place the separator
        movq X$eax+9 xmm1

        mov B$eax+17 0    ; terminating zero
        mov eax 17

    ..Test_Else_If eax FMT_BIN_ROSASM

        mov eax D@Output        ; buffer pointer
        movups X$eax xmm1       ; 1st copy the hiDWord to the 1st 16 bytes in output
        pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
        mov B$eax+4 '_'         ; place the separator
        movq X$eax+5 xmm1

        mov W$eax+9 '__'         ; place the separator

        pshufd XMM1 XMM1 SSE_SWAP_QWORDS ; Swap to we get the 2nd byte. We need to swap the qword to get the proper dword pos
        movups X$eax+11 xmm1
        pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get the correct byte. Swap only the low word.
        mov B$eax+15 '_'         ; place the separator
        movq X$eax+16 xmm1

        mov B$eax+20 0    ; terminating zero
        mov eax 20

    ..Test_Else_If eax FMT_BIN_ROSASM_TRUNCATE

        movzx eax W@Number
        .Test_If eax 0FF00   ;00__1111_1111__0000_0000 ; If eax > 0FF. eax >= 256

            mov eax D@Output        ; buffer pointer
            movups X$eax xmm1       ; 1st copy the hiWord to the 1st 16 bytes in output
            pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+4 '_'         ; place the separator
            movq X$eax+5 xmm1

            mov W$eax+9 '__'         ; place the separator

            pshufd XMM1 XMM1 SSE_SWAP_QWORDS
            movups X$eax+11 xmm1
            pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+15 '_'         ; place the separator
            movq X$eax+16 xmm1

            mov B$eax+20 0    ; terminating zero
            mov eax 20

        .Test_Else

            mov eax D@Output        ; buffer pointer
            pshufd XMM1 XMM1 SSE_SWAP_QWORDS
            movups X$eax xmm1
            pshuflw XMM1 XMM1 {SHUFFLE 2,3,0,1} ; swap to we get their words (used here as bytes)
            mov B$eax+4 '_'         ; place the separator
            movq X$eax+5 xmm1
            mov B$eax+9 0    ; terminating zero
            mov eax 9

        .Test_End

    ..Test_Else

        mov eax D@Output        ; buffer pointer
        movups X$eax xmm1  ; 1st copy the hiWord to the 1st 16 bytes in output
        mov B$eax+16 0    ; terminating zero
        mov eax 16

    ..Test_End

EndP



Proc CopyStringEx:
    Arguments @Input, @Output
    Uses esi, edi

    mov esi D@Input
    mov edi D@Output
    .While B$esi <> 0
        movsb
    .End_While
    mov B$edi 0

    mov eax edi
    sub eax D@Output

EndP


Proc FormatStr_OldNov2019:
    Arguments @Output, @Input, @ArgList
    Uses esi, edi, ebx, edx, ecx

    lea ecx D@Output
    lea ebx D$ecx+8

    mov esi D@Input ; pattern string >> esi
    mov edi D@Output ; output buffer >> edi

    .Do

        ...If B$esi = '%'

            ..If B$esi+1 = 's'

                ; Copy string to buffer
                push esi
                    mov esi D$ebx

                    test esi esi | je L1>
                        While B$esi <> 0 | movsb | EndWhile
                    L1:
                    add ebx 4
                pop esi
                add esi 2

            ..Else_If B$esi+1 = 'x'

                ; Convert to hex
                push esi
                    mov esi D$ebx
                    call dwtoHex_Ex2 esi, edi, &FALSE
                    add edi eax
                    add ebx 4
                pop esi
                add esi 2

            ..Else_If B$esi+1 = 'd'

                ; Convert integer to decimal representation
                push esi
                    mov esi D$ebx
                    call utoa_exLingo esi, edi
                    mov edi eax
                    add ebx 4
                pop esi
                add esi 2

            ..Else
                inc esi ; bypass "%" and copy next byte
                movsb
            ..End_If

        ...Else
            movsb
        ...EndIf

    .Loop_Until B$esi = 0

    ; Return stringlength in eax
    mov eax edi
    sub eax D@Output
    mov B$edi 0

EndSTD

;;

Proc FormatStr:
    Arguments @Output, @Input, @ArgList
    Uses esi, edi, ebx, edx

    mov esi D@Input ; pattern string >> esi
    mov edi D@Output ; output buffer >> edi
    mov ebx 2 ; paramter count >> ebx

    .Do
        lodsb
        .If al = '%'
            lodsb
            If al = 's'
                ; Copy string to buffer
                push esi
                    push ecx
                        lea ecx D@Output
                        mov esi D$ecx+ebx*4
                    pop ecx
                    inc ebx
                    cmp esi 0 | je L3>
                    While B$esi <> 0 | movsb | EndWhile
                L3: pop esi
            Else_If al = 'x'

                ; Convert to hex
                push ecx
                    lea ecx D@Output
                    mov eax D$ecx+ebx*4
                pop ecx
                inc ebx
                call dwtoHex_Ex2 eax, edi, &FALSE
                add edi eax

            Else_If al = 'd'
                ; Convert integer to decimal representation
                push ecx
                    lea ecx D@Output
                    mov eax D$ecx+ebx*4
                pop ecx
                inc ebx
                call IntToStr
            Else
                stosb
            EndIf
        .Else
            stosb
        .EndIf

    .Loop_Until al = 0

    ; Return stringlength in ecx
    mov ecx edi
    sub ecx D@Output
    dec ecx
    mov B$edi 0

EndSTD
;;

Proc dwtohexEx:
    Arguments @Input, @Output
    Local @DwordStorage
    Uses eax, edi

    mov eax D@Input
    mov edi D@Output

    mov D@DwordStorage eax
    movzx eax B@DwordStorage+3 | mov ax W$hex_table+eax*2 | stosw
    movzx eax B@DwordStorage+2 | mov ax W$hex_table+eax*2 | stosw
    movzx eax B@DwordStorage+1 | mov ax W$hex_table+eax*2 | stosw
    movzx eax B@DwordStorage+0 | mov ax W$hex_table+eax*2 | stosw
    mov B$edi 0

EndP

;;
    dwtoHex_Ex2

    Optimized version of dwtoHex. It seems the fastest version built.
    On eax it shows the size of the outputed string plus the null terminated byte

    The results are in the format of 0FFFF, 0121345678, 015, 0A, 00
;;

[hex_table: B$ "000102030405060708090A0B0C0D0E0F"
            B$ "101112131415161718191A1B1C1D1E1F"
            B$ "202122232425262728292A2B2C2D2E2F"
            B$ "303132333435363738393A3B3C3D3E3F"
            B$ "404142434445464748494A4B4C4D4E4F"
            B$ "505152535455565758595A5B5C5D5E5F"
            B$ "606162636465666768696A6B6C6D6E6F"
            B$ "707172737475767778797A7B7C7D7E7F"
            B$ "808182838485868788898A8B8C8D8E8F"
            B$ "909192939495969798999A9B9C9D9E9F"
            B$ "A0A1A2A3A4A5A6A7A8A9AAABACADAEAF"
            B$ "B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF"
            B$ "C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF"
            B$ "D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF"
            B$ "E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF"
            B$ "F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF", 0]

; Fastest dwtoEx version. It converts hex dword to hex string on a short version.
; Eax returns the length of the converted string plus the null termination byte
; It inserts a leading '0' at the beginning
; Ex: Input 01125 . Ouput 01125. Input 0, output 00

Proc dwtoHex_Ex2:
    Arguments @Input, @Output, @UseLeading
    Local @DwordStorage
    Uses edi

    mov eax D@Input
    mov edi D@Output
    mov D@DwordStorage eax

    If D@UseLeading <> 0 ; Inserts a leading zero. ON true, uses leading, false don´t use
        mov B$edi '0' | inc edi
    End_If

    movzx eax B@DwordStorage+3
    Test_If eax eax
        mov ax W$hex_table+eax*2
        If al = '0'
            shr eax 8 | stosb
        Else
            stosw
        End_If
        movzx eax B@DwordStorage+2 | mov ax W$hex_table+eax*2 | stosw
        movzx eax B@DwordStorage+1 | mov ax W$hex_table+eax*2 | stosw
        movzx eax B@DwordStorage+0 | mov ax W$hex_table+eax*2 | stosw
        mov B$edi 0
        sub edi D@Output | mov eax edi
        ExitP
    Test_End

    movzx eax B@DwordStorage+2
    Test_If eax eax
        mov ax W$hex_table+eax*2
        If al = '0'
            shr eax 8 | stosb
        Else
            stosw
        End_If
        movzx eax B@DwordStorage+1 | mov ax W$hex_table+eax*2 | stosw
        movzx eax B@DwordStorage+0 | mov ax W$hex_table+eax*2 | stosw
        mov B$edi 0
        sub edi D@Output | mov eax edi
        ExitP
    Test_End

    movzx eax B@DwordStorage+1
    Test_If eax eax
        mov ax W$hex_table+eax*2
        If al = '0'
            shr eax 8 | stosb
        Else
            stosw
        End_If
        movzx eax B@DwordStorage+0 | mov ax W$hex_table+eax*2 | stosw
        mov B$edi 0
        sub edi D@Output | mov eax edi
        ExitP
    Test_End

    movzx eax B@DwordStorage+0
    Test_If eax eax
        mov ax W$hex_table+eax*2
        If al = '0'
            shr eax 8 | stosb
        Else
            stosw
        End_If
        mov B$edi 0
        sub edi D@Output | mov eax edi
    Test_Else
        mov W$edi '0' | inc edi
        sub edi D@Output | mov eax edi
    Test_End

EndP


;;
    Extremelly fast dword to ascii convertion by LIngo
;;

; http://homepage.cs.uiowa.edu/~jones/bcd/decimal.html
; This version was modified with Lingo's suggestion to balance the CALL / RET pairing.
;4096 - 00__0001_0000__0000_0000
;256  - 00__0000_0001__0000_0000
;16   -            00__0001_0000
;[chartabL: B$ "00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899" 0, 0]

; return eax contains the pointer to the end of the string (including the null terminated byte)

[chartabL:  W$ '00', '01', '02', '03', '04', '05', '06', '07', '08', '09',
            W$ '10', '11', '12', '13', '14', '15', '16', '17', '18', '19',
            W$ '20', '21', '22', '23', '24', '25', '26', '27', '28', '29',
            W$ '30', '31', '32', '33', '34', '35', '36', '37', '38', '39',
            W$ '40', '41', '42', '43', '44', '45', '46', '47', '48', '49',
            W$ '50', '51', '52', '53', '54', '55', '56', '57', '58', '59',
            W$ '60', '61', '62', '63', '64', '65', '66', '67', '68', '69',
            W$ '70', '71', '72', '73', '74', '75', '76', '77', '78', '79',
            W$ '80', '81', '82', '83', '84', '85', '86', '87', '88', '89',
            W$ '90', '91', '92', '93', '94', '95', '96', '97', '98', '99', 0]


Proc utoa_exLingo:
    Arguments @src, @lpdest
    Uses ecx, edx, esi, edi

    mov eax D@src
    mov edx 0D1B71759
    mov ecx D@lpdest

    test eax eax | jne L1>
        mov D$ecx '0'
        inc ecx
        mov eax ecx
        ;lea eax D$ecx+1
        ;mov eax D@lpdest
        ;inc eax
        ExitP
    L1:

    mov edi eax
    mul edx
    shr edx 13
    mov esi 100 | je @lbl11
    imul eax edx 10000
    sub edi eax
    mov eax 068DB9
    mul edx | jae @lbl9
    cmp edx 9 | jbe @lbl8
    movzx edx W$edx+edx+chartabL
    add ecx 8
    ;mov D$ecx-8 edx
    mov W$ecx-8 dx

@lbl0:
    mul esi

@lbl1:
    movzx edx W$edx+edx+chartabL
    ;mov D$ecx-6 edx
    mov W$ecx-6 dx

@lbl2:
    mul esi

@lbl3:
    movzx edx W$edx+edx+chartabL
    ;mov D$ecx-4 edx
    mov W$ecx-4 dx

@lbl4:
    mov eax 028F5C29
    mul edi

@lbl5:
    movzx edx W$edx+edx+chartabL
    ;mov D$ecx-2 edx
    mov W$ecx-2 dx

@lbl6:
    mul esi

@lbl7:
    movzx eax W$edx+edx+chartabL
    ;mov D$ecx eax
    mov W$ecx ax
    add ecx 2 | mov eax ecx
    ;mov eax D@lpdest
    ;lea eax D$ecx+2
    ExitP

@lbl8:
    add ecx 7
    add edx '0'
    mov D$ecx-7 edx | jne @lbl0

@lbl9:
    mul esi | jae @lbl10
    add ecx 6
    cmp edx 9 | ja @lbl1
    add edx '0' | sub ecx 1
    mov D$ecx-5 edx | jne @lbl2

@lbl10:
    mul esi | jae @lbl11
    add ecx 4
    cmp edx 9 | ja @lbl3
    add edx '0' | sub ecx 1
    mov D$ecx-3 edx | jne @lbl4

@lbl11:
    mov eax 028F5C29
    mul edi | jae @lbl12
    add ecx 2
    cmp edx 9 | ja @lbl5
    add edx '0' | sub ecx 01
    mov D$ecx-1 edx | jne @lbl6

@lbl12:
    mul esi
    cmp edx 9 | ja @lbl7
;    lea eax D$edx+'0'
;    mov D$ecx eax
    add edx '0'
    mov B$ecx dl
    inc ecx | mov eax ecx
    ;lea eax D$ecx+1
    ;mov eax D@lpdest
    ;inc eax

EndP
;;
Proc utoa_exLingo::
    Arguments @src, @lpdest
    Uses ecx, edx, esi, edi

    mov eax D@src
    mov edx 0D1B71759
    mov ecx D@lpdest
    mov edi eax
    mul edx
    shr edx 13
    mov esi 100 | je @lbl11
    imul eax edx 10000
    sub edi eax
    mov eax 068DB9
    mul edx | jae @lbl9
    cmp edx 9 | jbe @lbl8
    movzx edx W$edx+edx+chartabL
    add ecx 8
    mov D$ecx-8 edx

@lbl0:
    mul esi

@lbl1:
    movzx edx W$edx+edx+chartabL
    mov D$ecx-6 edx

@lbl2:
    mul esi

@lbl3:
    movzx edx W$edx+edx+chartabL
    mov D$ecx-4 edx

@lbl4:
    mov eax 028F5C29
    mul edi

@lbl5:
    movzx edx W$edx+edx+chartabL
    mov D$ecx-2 edx

@lbl6:
    mul esi

@lbl7:
    movzx eax W$edx+edx+chartabL
    mov D$ecx eax
    ;mov eax D@lpdest
    ;lea eax D$ecx+2
    ExitP

@lbl8:
    add ecx 7
    add edx '0'
    mov D$ecx-7 edx | jne @lbl0

@lbl9:
    mul esi | jae @lbl10
    add ecx 6
    cmp edx 9 | ja @lbl1
    add edx '0' | sub ecx 1
    mov D$ecx-5 edx | jne @lbl2

@lbl10:
    mul esi | jae @lbl11
    add ecx 4
    cmp edx 9 | ja @lbl3
    add edx '0' | sub ecx 1
    mov D$ecx-3 edx | jne @lbl4

@lbl11:
    mov eax 028F5C29
    mul edi | jae @lbl12
    add ecx 2
    cmp edx 9 | ja @lbl5
    add edx '0' | sub ecx 01
    mov D$ecx-1 edx | jne @lbl6

@lbl12:
    mul esi
    cmp edx 9 | ja @lbl7
    lea eax D$edx+'0'
    mov D$ecx eax
    ;lea eax D$ecx+1
    ;mov eax D@lpdest

EndP
;;


FormatStr_Old:
    push ebp
    mov ebp esp

    push esi edi ebx edx ; rescue regs

    mov esi D$ebp+12 ; pattern string >> esi
    mov edi D$ebp+8 ; output buffer >> edi
    mov ebx 2 ; paramter count >> ebx

L0: lodsb
    .If al = '%'
        lodsb
        If al = 's'
            ; Copy string to buffer
            push esi
                mov esi D$ebp+8+ebx*4 | inc ebx
                cmp esi 0 | je L3>
                While B$esi <> 0 | movsb | EndWhile
        L3: pop esi
        ElseIf al = 'x'
            ; Convert to hex
            mov eax D$ebp+8+ebx*4 | inc ebx
            mov edx eax | add edi 7
            std
            mov ecx 8
L1:         mov al dl | and al 0F | cmp al 0A | jb L2>
            add al 7
L2:         add al '0' | stosb | shr edx 4 | loop L1<
            cld
            add edi 9
        ElseIf al = 'd'
            ; Convert integer to decimal representation
            mov eax D$ebp+8+ebx*4 | inc ebx
            call IntToStr
        Else
            stosb
        EndIf
    .Else
        stosb
    .EndIf
    cmp al 0 | jne L0<

    ; Return stringlength in ecx
    mov ecx edi
    sub ecx D$ebp+8
    dec ecx
    mov B$edi 0
    pop edx ebx edi esi ; restore regs

    pop ebp
ret

____________________________________________________________________________________________

; Outputs the time in format hh:mm:ss
;   parameter : edi -> string (8 chars must fit in)
;   returns   : edi -> string (terminating null-char)
;   uses      : eax, ecx, edx

TimeToStr:
    sub esp 16

        call 'KERNEL32.GetLocalTime' esp
        movzx eax W$esp+8
        mov edx 0, ecx 10 | div ecx
        add al '0'
        stosb
        add dl '0' | mov al dl
        stosb

        mov al ':' | stosb

        movzx eax W$esp+10
        mov edx 0, ecx 10 | div ecx
        add al '0'
        stosb
        add dl '0' | mov al dl
        stosb

        mov al ':' | stosb

        movzx eax W$esp+12
        mov edx 0, ecx 10 | div ecx
        add al '0'
        stosb
        add dl '0' | mov al dl
        stosb

        mov B$edi 0

    add esp 16
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

DebuggerSTRINGS: ; Text visible to the user is collected here.
____________________________________________________________________________________________
____________________________________________________________________________________________

[StrRun:            'Run' 0
 StrStepInto:       'Step Into' 0
 StrStepOver:       'Step Over' 0
 StrStep:           'Step' 0
 StrReturn:         'Return to Caller' 0
 StrRet:            'Return' 0
 StrTerminate:      'Terminate' 0
 StrPause:          'Pause' 0
 StrHoldOnBp:       'Hold on breakpoints' 0
 StrInstStep:       'Instruction level stepping' 0
 StrSrcStep:        'Source level stepping' 0
 StrShowAll:        'Show segment/debug registers' 0
 StrFont:           'Choose font ...' 0
 StrCPUInfo:        'CPU info' 0
 StrFPUStatus:      'FPU status' 0
 StrShowCodeAt:     'Show code at address ...' 0
 StrAbout:          'About Debugger' 0
 StrDbgHelp:        'Debugger Help' 0
 StrContinue:       'Continue' 0
 StrBreak:          'Break' 0
 StrSettings:       'Settings' 0
 StrInformation:    'Information' 0
 StrHelp:           'Help' 0]

[StrDataFmt:        'Data Representation:' 0]

[StrF1:             'F1' 0
 StrF6:             'F6' 0
 StrF7:             'F7' 0
 StrF8:             'F8' 0
 StrCtrlF7:         'Ctrl+F7' 0
 StrCtrlF6:         'Ctrl+F6' 0
 StrCtrlF12:        'Ctrl+F12' 0]

[FPUControlWord:    'Control Word: ' 0
 FPUTagWord:        'Tag Word: ' 0
 FPURoundingMode:   'Rounding Mode: ' 0
 FPUPrecision:      'Precision: ' 0
 FPUStatusWord:     'Status Word: ' 0
 FPURndNearest:     'Nearest or even' 0
 FPURndDown:        'Round down' 0
 FPURndUp:          'Round up' 0
 FPURndTrunc:       'Truncate' 0]

[DataView:          'Data' 0
 MemoryInspector:   'Memory' 0
 CallStack:         'Call Stack' 0
 Log:               'Log' 0
 AddressSpace:      'Address Space' 0]

[FmtHex:            'Hexadecimal' 0
 FmtUDec:           'Unsigned Decimal' 0
 FmtSDec:           'Signed Decimal' 0
 FmtBinary:         'Binary (32 bits) HiWord/LowWord: From 31 to 0' 0
 FmtBinarySSE:      'Binary (128 bits) HiQword/LowQword: From 127 to 0' 0
 FmtBinaryMMX:      'Binary (64 bits) HiDword/LowDword: From 63 to 0' 0
 FmtBinaryFPU:      'Binary (80 bits) HiDword/LowDword: From 79 to 0' 0
 FmtFloat:          'Floating Point' 0
 FmtPUB:            'Packed Unsigned Byte' 0
 FmtPSB:            'Packed Signed Byte' 0
 FmtPUW:            'Packed Unsigned Word' 0
 FmtPSW:            'Packed Signed Word' 0
 FmtPUD:            'Packed Unsigned Dword' 0
 FmtPSD:            'Packed Signed Dword' 0
 FmtPUQ:            'Packed Unsigned Qword' 0
 FmtPSQ:            'Packed Signed Qword' 0
 FmtPF:             'Packed Single Precision Float' 0
 FmtPD:             'Packed Double Precision Float' 0
 FmtHexAnsi:        'Hexadecimal / ANSI' 0
 FmtHexDW:          'Hexadecimal Dwords' 0
 FmtHexW:           'Hexadecimal Words' 0
 FmtFloats:         'Single Precision Floats' 0
 FmtDoubles:        'Double Precision Floats' 0
 FmtAscii:          'Ascii Characters' 0
 FmtHexCooked:      'Hexadecimal / Cooked Ascii (RosAsm Development)' 0]

[StrShowInMemInsp:  'Show in memory inspector' 0
 StrShowPInMemInsp: 'Show memory pointed at' 0
 StrShowDecl:       'Show declaration' 0
 StrBreakOnW:       'Break on write access' 0
 StrBreakOnRW:      'Break on read/write access' 0
 StrSortByName:     'Sort by name' 0
 StrSortByAddr:     'Sort by address' 0]

[StrShowInvoke:     'Show invocation' 0
 StrShowAllCalls:   'Show all calls' 0
 StrHideModCalls:   'Hide module calls' 0
 StrHideIMCalls:    'Hide intra-module calls' 0
 StrShowLocals:     'Show Local data' 0]

[StrDumpMemory:     'Dump this segment Memory to Disk' 0]

[ACCESS_VIOLATION: B$      "ACCESS VIOLATION [C0000005]
The thread tried to read from or write to a virtual address for which it does not have the appropriate access." 0
 ARRAY_BOUNDS_EXCEEDED:    "ARRAY BOUNDS EXCEEDED [C000008C]
The thread tried to access an array element that is out of bounds and the underlying hardware supports bounds checking." 0
 DATATYPE_MISALIGNMENT:    "DATATYPE_MISALIGNMENT [80000002]
The thread tried to read or write data that is misaligned on hardware that does not provide alignment. 
For example, 16-bit values must be aligned on 2-byte boundaries; 32-bit values on 4-byte boundaries, and so on." 0
 FLT_DENORMAL_OPERAND:     "FLT DENORMAL OPERAND [C000008D]
One of the operands in a floating-point operation is denormal. 
A denormal value is one that is too small to represent as a standard floating-point value." 0
 FLT_DIVIDE_BY_ZERO:       "FLT DIVIDE BY ZERO [C000008E]
The thread tried to divide a floating-point value by a floating-point divisor of zero." 0
 FLT_INEXACT_RESULT:       "FLT INEXACT RESULT [C000008F]
The result of a floating-point operation cannot be represented exactly as a decimal fraction." 0
 FLT_INVALID_OPERATION:    "FLT INVALID OPERATION [C0000090]" 0
 FLT_OVERFLOW:             "FLT OVERFLOW [C0000091]
The exponent of a floating-point operation is greater than the magnitude allowed by the corresponding type." 0
 FLT_STACK_CHECK:          "FLT STACK CHECK [C0000092]
The stack overflowed or underflowed as the result of a floating-point operation." 0
 FLT_UNDERFLOW:            "FLT UNDERFLOW [C0000093]
The exponent of a floating-point operation is less than the magnitude allowed by the corresponding type." 0
 ILLEGAL_INSTRUCTION:      "ILLEGAL INSTRUCTION [C000001D] 
The thread tried to execute an invalid instruction." 0
 IN_PAGE_ERROR:            "IN PAGE ERROR [C0000006]
The thread tried to access a page that was not present, and the system was unable to load the page.
For example, this exception might occur if a network connection is lost while running a program over the network." 0
 INT_DIVIDE_BY_ZERO:       "INT DIVIDE BY ZERO [C0000094]
The thread tried to divide an integer value by an integer divisor of zero." 0
 INT_OVERFLOW:             "INT OVERFLOW [C0000095]
The result of an integer operation caused a carry out of the most significant bit of the result." 0
 PRIV_INSTRUCTION:         "PRIVILIGED INSTRUCTION [C0000096]
The thread tried to execute an instruction whose operation is not allowed in the current machine mode." 0
 STACK_OVERFLOW:           "STACK OVERFLOW [C00000FD]
The thread used up its stack." 0
 INVALID_DISPOSITION:      "INVALID DISPOSITION [C0000026]
An exception handler returned an invalid disposition to the exception dispatcher." 0
 NONCONTINUABLE_EXCEPTION: "NONCONTINUABLE EXCEPTION [C0000025]
The thread tried to continue execution after a noncontinuable exception occurred." 0
 UNKNOWN_EXCEPTION: "xxxxxxxx 
Unknown exception." 0]

