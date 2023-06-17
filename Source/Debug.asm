TITLE Debug

[DebuggerVersion: 'RosAsm Debugger V2.3' 0]

;;
_____________________________________________________________________________________________

                             Debugger [Version 2.3] - Gustavo Trigueiros (Beyond2000!)
    RosAsm 2.053a  [Debugger V2.3]
    * [Bugfix] UPdated DEBUG_EVENT structure
    * added &HEAP_NO_SERIALIZE when creating a growable heap at DebugRsrcManagerThread   call 'KERNEL32.HeapCreate' &HEAP_NO_SERIALIZE, 4096, 0
                                                                Debugger_OnUnloadDll     call 'KERNEL32.HeapFree' D$ModuleNameHeap, &HEAP_NO_SERIALIZE, D$edi+ME_Name
    * full review of ScanPEHeader
    * developed GetModuleSize function
    * updated: CreateNewForm_AddressSpaceForm
               AddressSpaceFormProc
               AddressSpaceForm_OnTreeNavigate
               AddressSpace_HandleMouseProc
               LogFormProc
    * implemented: AddressSpaceDialog_OnCreate
                   AddressSpaceDialog_OnSize
                   AddressSpaceForm_OnCommand
                   AddressSpace_CreatePopupMenu

    
                             Debugger [Version 2.2b] - Ludwig Haehne
     
    * [Bugfix] Added security for degenerated call stack cases
    
  RosAsm 2.033b [Debugger V2.2a]
  
    * [Bugfix] FPU flags toolbar was not effective
  
  RosAsm 2.031a [Debugger V2.2]
                 
    * [Feature] Localized user interface
    * [Feature] Recognize stack-frames in Call stack set up by ENTER intruction
    * [Change] Ctrl+F shortcut for FPU info removed (conflict with search feature)
    * [Change] Terminate debuggee immediately when debugger window is closed
    * [Change] Do not inherit startup information to debuggee
    * [Bugfix] Preserve z-order and focus of debuggee's windows across debug events
    * [Bugfix] Minimize/Restore dialog messed up child-window sizes    
    * [Bugfix] Sourceposition was wrong when a breakpoint was set on a single-byte instruction
    * [Bugfix] Error checking for VirtualQuery

  RosAsm 2.027a [Debugger V2.1b]

    * [Bugfix] Menu did not work on Win95
    * [Bugfix] MMX register contents did not show properly on Win9x
    * [Bugfix] Occasional crash when mouse hints showed up behind other windows on Win98

  RosAsm 2.025e [Debugger V2.1a]
  
    * [Bugfix] Thread synch problems fixed when running multiple debugger instances
    * [Bugfix] Logfiles now have application specific names
                             
  RosAsm 2.025a [Debugger V2.1]
  
    * [Feature] Local symbol support for mouse hints and call stack
    * [Feature] New filter capabilities in call stack (context menu)
    * [Feature] Sort data labels by Address or Name (select in context menu)
    * [Feature] Toolbar
    * [Change] Flags moved into toolbar (can be hidden)
    * [Change] General code cleanup
    * [Bugfix] Rare "Invalid path" problem fixed
____________________________________________________________________________________________
          
 Known bugs:
 
    * Check for Invalid parameter counts in callstack computation (MyPNN)
 
    * Two byte breakpoints make the debuggee crash when HoldOnBreakPoints = 0
    * Terminate leaves process and thread handle open    
    * Stepping into int 02e seeds a 'breakpoint' at the return address (TrackPopupMenu)
    * Int 3 in Hook procedures (mouse hooks) hangs the Windows GUI
   
    *   "There is an issue about focus at debug time. "Sometimes" the debuger  set  the 
        focus to main debugee window.
        when the focus was at a second overlaped window (not the main nor a  sysmodal) i want 
        the focus to go back where it was, not to the owner  window. I have a rare efect 
        even without BPs at debug time" (from Marcelo)
        
        >> Find a way to not touch the z-order of the debuggee windows after run/step-over.
        
    * Close debug window when debuggee has message box open  (except.exe)
        -> wait -> "Debug thread does not respond" -> [Ok] 
        -> Unknown exception (C0000008) in CloseHandle called by DebugRsrcManager
        (exception does NOT crash RosAsm also it does not seem to have a SEH attached !?)

 Future ;)
 
    * Window message logger
    * Structured data memory inspector
    * Conditional breakpoints
    * Editable contents (register, memory)
    * Symbolic Profiler
____________________________________________________________________________________________
                          
 Maintainer since November 2003: Ludwig Haehne, wkx@gmx.li
 
 I have almost completely rewritten the existing debugger to introduce new features and 
 have a better UI.
 
 The debugger is now roughly divided into the following sections:
 
 Main Debug Routines
 
  * 'Debugger' is called when you click on [Run] and creates the 'DebugRsrcManagerThread'
  * 'KillDebugger' is called whenever you try to modify your sources
  
 Debugger polling Thread (handles debug events)
 
  * 'DebugThread', 'ShowProcessWindows', 'ScanStackForCodePointer', 'CloseProcess',
    'ReadProcessMem', 'WriteProcessMem', 'SignalDebugEvent', 'EncounterException'
  
 Breakpoint management
 
  * 'CreateBPTable', 'DestroyBPTable', 'AddProcessBreakpoint', 'DeleteProcessBreakpoint', 
    'DisableProcessBreakpoints', 'EnableProcessBreakpoints', 'EncounterBreakpoint', 
    'KillBreakpointGroup'
    
 Breakpoint synchronization with Source editor
  
  * 'CreateBPAnteroom', 'DestroyBPAnteroom', 'InitialFillBPAnteroom', 'AddBPToAnteroom',
    'ClearBPAnteroom'

 Address Space Routines
 
  * 'VirtualQuery', 'IsProcessMemory', 'IsProcessCode', 'FindNextPage', 'FindPrevPage'
  
 Call stack routines
  
  * 'GenerateCallStack', 'DestroyCallStack', 'ReadApplicationStack', 'IsReturnAddress',
    'IsReturnAddressInSource', 'GetCodeLabelFromAddress', 'GetNearestProc', 'CountParametersAndScanStackFrame'
    
 Module management
 
  * 'IsModuleCode'
_____________________________________________________________________________________________

;;

;;

DEBUG_EVENT Structure

typedef struct _DEBUG_EVENT {
  DWORD dwDebugEventCode;
  DWORD dwProcessId;
  DWORD dwThreadId;
  union {
    EXCEPTION_DEBUG_INFO      Exception;
    CREATE_THREAD_DEBUG_INFO  CreateThread;
    CREATE_PROCESS_DEBUG_INFO CreateProcessInfo;
    EXIT_THREAD_DEBUG_INFO    ExitThread;
    EXIT_PROCESS_DEBUG_INFO   ExitProcess;
    LOAD_DLL_DEBUG_INFO       LoadDll;
    UNLOAD_DLL_DEBUG_INFO     UnloadDll;
    OUTPUT_DEBUG_STRING_INFO  DebugString;
    RIP_INFO                  RipInfo;
  } u;
} DEBUG_EVENT, *LPDEBUG_EVENT;

____________________________

typedef struct _EXCEPTION_DEBUG_INFO {
  EXCEPTION_RECORD ExceptionRecord;
  DWORD            dwFirstChance;
} EXCEPTION_DEBUG_INFO, *LPEXCEPTION_DEBUG_INFO;

typedef struct _EXCEPTION_RECORD {
  DWORD                    ExceptionCode;
  DWORD                    ExceptionFlags;
  struct _EXCEPTION_RECORD  *ExceptionRecord;
  PVOID                    ExceptionAddress;
  DWORD                    NumberParameters;
  ULONG_PTR                ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
} EXCEPTION_RECORD, *PEXCEPTION_RECORD;

____________________________

typedef struct _CREATE_THREAD_DEBUG_INFO {
  HANDLE                 hThread;
  LPVOID                 lpThreadLocalBase;
  LPTHREAD_START_ROUTINE lpStartAddress;
} CREATE_THREAD_DEBUG_INFO, *LPCREATE_THREAD_DEBUG_INFO;
____________________________

typedef struct _CREATE_PROCESS_DEBUG_INFO {
  HANDLE                 hFile;
  HANDLE                 hProcess;
  HANDLE                 hThread;
  LPVOID                 lpBaseOfImage;
  DWORD                  dwDebugInfoFileOffset;
  DWORD                  nDebugInfoSize;
  LPVOID                 lpThreadLocalBase;
  LPTHREAD_START_ROUTINE lpStartAddress;
  LPVOID                 lpImageName;
  WORD                   fUnicode;
} CREATE_PROCESS_DEBUG_INFO, *LPCREATE_PROCESS_DEBUG_INFO;
____________________________

typedef struct _EXIT_THREAD_DEBUG_INFO {
  DWORD dwExitCode;
} EXIT_THREAD_DEBUG_INFO, *LPEXIT_THREAD_DEBUG_INFO;
____________________________

typedef struct _EXIT_PROCESS_DEBUG_INFO {
  DWORD dwExitCode;
} EXIT_PROCESS_DEBUG_INFO, *LPEXIT_PROCESS_DEBUG_INFO;
____________________________

typedef struct _LOAD_DLL_DEBUG_INFO {
  HANDLE hFile;
  LPVOID lpBaseOfDll;
  DWORD  dwDebugInfoFileOffset;
  DWORD  nDebugInfoSize;
  LPVOID lpImageName;
  WORD   fUnicode;
} LOAD_DLL_DEBUG_INFO, *LPLOAD_DLL_DEBUG_INFO;
____________________________

typedef struct _UNLOAD_DLL_DEBUG_INFO {
  LPVOID lpBaseOfDll;
} UNLOAD_DLL_DEBUG_INFO, *LPUNLOAD_DLL_DEBUG_INFO;
____________________________

typedef struct _OUTPUT_DEBUG_STRING_INFO {
  LPSTR lpDebugStringData;
  WORD  fUnicode;
  WORD  nDebugStringLength;
} OUTPUT_DEBUG_STRING_INFO, *LPOUTPUT_DEBUG_STRING_INFO;

____________________________

typedef struct _RIP_INFO {
  DWORD dwError;
  DWORD dwType;
} RIP_INFO, *LPRIP_INFO;

____________________________

http://msdn.microsoft.com/en-us/library/windows/desktop/ms679308(v=vs.85).aspx

[DEBUG_EVENT:
 DEBUG_EVENT.dwDebugEventCode: D$ 0
 DEBUG_EVENT.dwProcessId: D$ 0
 DEBUG_EVENT.dwThreadId: D$ 0
 
 DEBUG_EVENT.pExceptionRecord: hThread: hFile: dwExitCode: lpBaseOfDll: lpDebugStringData: dwError: D$ 0
 DEBUG_EVENT.dwFirstChance: lpThreadLocalBase: hProcess: lpBaseOfDll: dwType: fUnicode: W$ 0
 DEBUG_EVENT.nDebugStringiLength: W$ 0
 DEBUG_EVENT.lpStartAddress: hThread: dwDebugInfoFileOffset: nDebugStringiLength: D$ 0
 DEBUG_EVENT.lpBaseOfImage: nDebugInfoSize: D$ 0
 DEBUG_EVENT.dwDebugInfoFileOffset: lpImageName: D$ 0
 DEBUG_EVENT.nDebugInfoSize: fUnicode: D$ 0
 DEBUG_EVENT.lpThreadLocalBase: D$ 0
 DEBUG_EVENT.lpStartAddress: D$ 0
 DEBUG_EVENT.lpImageName: D$ 0
 DEBUG_EVENT.fUnicode: W$ 0]

; The union structures are:
____________
; EXCEPTION_DEBUG_INFO structure
[DEBUG_EVENT.pExceptionRecord:
 DEBUG_EVENT.pExceptionRecord.ExceptionCode: D$ 0
 DEBUG_EVENT.pExceptionRecord.ExceptionFlags: D$ 0
 DEBUG_EVENT.pExceptionRecord.pExceptionRecord: D$ 0
 DEBUG_EVENT.pExceptionRecord.ExceptionAddress: D$ 0
 DEBUG_EVENT.pExceptionRecord.NumberParameters: D$ 0
 DEBUG_EVENT.pExceptionRecord.ExceptionInformation: D$ 0 #&EXCEPTION_MAXIMUM_PARAMETERS
 DEBUG_EVENT.pExceptionRecord.dwFirstChance: D$ 0]

____________

; CREATE_THREAD_DEBUG_INFO structure
[CREATE_THREAD_DEBUG_INFO:
 CREATE_THREAD_DEBUG_INFO.hThread: D$ 0
 CREATE_THREAD_DEBUG_INFO.lpThreadLocalBase: D$ 0
 CREATE_THREAD_DEBUG_INFO.lpStartAddress: D$ 0]
______________

; CREATE_PROCESS_DEBUG_INFO
[CREATE_PROCESS_DEBUG_INFO:
 CREATE_PROCESS_DEBUG_INFO.hFile: D$ 0
 CREATE_PROCESS_DEBUG_INFO.hProcess: D$ 0
 CREATE_PROCESS_DEBUG_INFO.hThread: D$ 0
 CREATE_PROCESS_DEBUG_INFO.lpBaseOfImage: D$ 0
 CREATE_PROCESS_DEBUG_INFO.dwDebugInfoFileOffset: D$ 0
 CREATE_PROCESS_DEBUG_INFO.nDebugInfoSize: D$ 0
 CREATE_PROCESS_DEBUG_INFO.lpThreadLocalBase: D$ 0
 CREATE_PROCESS_DEBUG_INFO.lpStartAddress: D$ 0
 CREATE_PROCESS_DEBUG_INFO.lpImageName: D$ 0
 CREATE_PROCESS_DEBUG_INFO.fUnicode: W$ 0]
______________

; EXIT_THREAD_DEBUG_INFO structure
[EXIT_THREAD_DEBUG_INFO:
 EXIT_THREAD_DEBUG_INFO.dwExitCode: D$ 0]
______________

[EXIT_PROCESS_DEBUG_INFO:
 EXIT_PROCESS_DEBUG_INFO.dwExitCode: D$ 0]
______________

[LOAD_DLL_DEBUG_INFO:
 LOAD_DLL_DEBUG_INFO.hFile: D$ 0
 LOAD_DLL_DEBUG_INFO.lpBaseOfDll: D$ 0
 LOAD_DLL_DEBUG_INFO.dwDebugInfoFileOffset: D$ 0
 LOAD_DLL_DEBUG_INFO.nDebugInfoSize: D$ 0
 LOAD_DLL_DEBUG_INFO.lpImageName: D$ 0
 LOAD_DLL_DEBUG_INFO.fUnicode: W$ 0]
______________

;UNLOAD_DLL_DEBUG_INFO
[UNLOAD_DLL_DEBUG_INFO:
 UNLOAD_DLL_DEBUG_INFO.lpBaseOfDll: D$ 0]
____________________________

; OUTPUT_DEBUG_STRING_INFO
[OUTPUT_DEBUG_STRING_INFO:
 OUTPUT_DEBUG_STRING_INFO.lpDebugStringData: D$ 0
 OUTPUT_DEBUG_STRING_INFO.fUnicode: W$ 0
 OUTPUT_DEBUG_STRING_INFO.nDebugStringiLength: W$ 0]
____________________________
[RIP_INFO:
 RIP_INFO.dwError: D$ 0
 RIP_INFO.dwType: D$ 0]

_____________________

Conclusion...In fact the DEGUV_EVENT structure reacts to dwDebugEventCode. If this member is &CREATE_PROCESS_DEBUG_EVENT the used structure is DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO
If it is &CREATE_THREAD_DEBUG_EVENT the used structure is DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO and so On. To make things a bit easier i made the structures named properly

; EXCEPTION_DEBUG_INFO structure
[DEBUG_EVENT_EXCEPTION_DEBUG_INFO:
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.dwDebugEventCode: D$ &EXCEPTION_DEBUG_EVENT
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.dwProcessId: D$ 0
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.dwThreadId: D$ 0
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCode: D$ 0
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionFlags: D$ 0
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.pExceptionRecord: D$ 0
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionAddress: D$ 0
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.NumberParameters: D$ 0
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionInformation: D$ 0 #&EXCEPTION_MAXIMUM_PARAMETERS
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.dwFirstChance: D$ 0]

____________

; CREATE_THREAD_DEBUG_INFO structure
[DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO:
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.dwDebugEventCode: D$ &CREATE_THREAD_DEBUG_EVENT
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.dwProcessId: D$ 0
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.dwThreadId: D$ 0
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.hThread: D$ 0
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.lpThreadLocalBase: D$ 0
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.lpStartAddress: D$ 0]
______________

; CREATE_PROCESS_DEBUG_INFO
[DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO:
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.dwDebugEventCode: D$ &CREATE_PROCESS_DEBUG_EVENT
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.dwProcessId: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.dwThreadId: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.hFile: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.hProcess: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.hThread: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.lpBaseOfImage: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.dwDebugInfoFileOffset: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.nDebugInfoSize: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.lpThreadLocalBase: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.lpStartAddress: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.lpImageName: D$ 0
 DEBUG_EVENT__CREATE_PROCESS_DEBUG_INFO.fUnicode: W$ 0]
______________

; EXIT_THREAD_DEBUG_INFO structure
[DEBUG_EVENT__EXIT_THREAD_DEBUG_INFO:
 DEBUG_EVENT__EXIT_THREAD_DEBUG_INFO.dwDebugEventCode: D$ &EXIT_THREAD_DEBUG_EVENT
 DEBUG_EVENT__EXIT_THREAD_DEBUG_INFO.dwProcessId: D$ 0
 DEBUG_EVENT__EXIT_THREAD_DEBUG_INFO.dwThreadId: D$ 0
 DEBUG_EVENT__EXIT_THREAD_DEBUG_INFO.dwExitCode: D$ 0]
______________

[DEBUG_EVENT__EXIT_PROCESS_DEBUG_INFO:
 DEBUG_EVENT__EXIT_PROCESS_DEBUG_INFO.dwDebugEventCode: D$ &EXIT_PROCESS_DEBUG_EVENT
 DEBUG_EVENT__EXIT_PROCESS_DEBUG_INFO.dwProcessId: D$ 0
 DEBUG_EVENT__EXIT_PROCESS_DEBUG_INFO.dwThreadId: D$ 0
 DEBUG_EVENT__EXIT_PROCESS_DEBUG_INFO.dwExitCode: D$ 0]
______________

[DEBUG_EVENT__LOAD_DLL_DEBUG_INFO:
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.dwDebugEventCode: D$ &LOAD_DLL_DEBUG_EVENT
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.dwProcessId: D$ 0
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.dwThreadId: D$ 0
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.hFile: D$ 0
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.lpBaseOfDll: D$ 0
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.dwDebugInfoFileOffset: D$ 0
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.nDebugInfoSize: D$ 0
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.lpImageName: D$ 0
 DEBUG_EVENT__LOAD_DLL_DEBUG_INFO.fUnicode: W$ 0]
______________

;UNLOAD_DLL_DEBUG_INFO
[DEBUG_EVENT__UNLOAD_DLL_DEBUG_INFO:
 DEBUG_EVENT__UNLOAD_DLL_DEBUG_INFO.dwDebugEventCode: D$ &UNLOAD_DLL_DEBUG_EVENT
 DEBUG_EVENT__UNLOAD_DLL_DEBUG_INFO.dwProcessId: D$ 0
 DEBUG_EVENT__UNLOAD_DLL_DEBUG_INFO.dwThreadId: D$ 0
 DEBUG_EVENT__UNLOAD_DLL_DEBUG_INFO.lpBaseOfDll: D$ 0]
____________________________

; OUTPUT_DEBUG_STRING_INFO
[DEBUG_EVENT__OUTPUT_DEBUG_STRING_INFO:
 DEBUG_EVENT__OUTPUT_DEBUG_STRING_INFO.dwDebugEventCode: D$ &OUTPUT_DEBUG_STRING_EVENT
 DEBUG_EVENT__OUTPUT_DEBUG_STRING_INFO.dwProcessId: D$ 0
 DEBUG_EVENT__OUTPUT_DEBUG_STRING_INFO.dwThreadId: D$ 0
 DEBUG_EVENT__OUTPUT_DEBUG_STRING_INFO.lpDebugStringData: D$ 0
 DEBUG_EVENT__OUTPUT_DEBUG_STRING_INFO.fUnicode: W$ 0
 DEBUG_EVENT__OUTPUT_DEBUG_STRING_INFO.nDebugStringiLength: W$ 0]
____________________________
[DEBUG_EVENT__RIP_INFO:
 DEBUG_EVENT__RIP_INFO.dwDebugEventCode: D$ &RIP_EVENT
 DEBUG_EVENT__RIP_INFO.dwProcessId: D$ 0
 DEBUG_EVENT__RIP_INFO.dwThreadId: D$ 0
 DEBUG_EVENT__RIP_INFO.dwError: D$ 0
 DEBUG_EVENT__RIP_INFO.dwType: D$ 0]


; So...the maximum size of the DEBUG_EVENT structure is Size_Of_DEBUG_EVENT_EXCEPTION_DEBUG_INFO that is the bigger union member structured

;;

[DEBUG_EVENT.dwDebugEventCodeDis 0
 DEBUG_EVENT.dwProcessIdDis 4
 DEBUG_EVENT.dwThreadIdDis 8]

[DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.dwProcessIdDis 4
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.dwThreadIdDis 8
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.hFileDis 12
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.hProcessDis 16
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.hThreadDis 20
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.lpBaseOfImageDis 24
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.dwDebugInfoFileOffsetDis 28
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.nDebugInfoSizeDis 32
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.lpThreadLocalBaseDis 36
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.lpStartAddressDis 40
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.lpImageNameDis 44
 DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.fUnicodeDis 48]

[Size_Of_DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO 52]

[DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.dwProcessIdDis 4
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.dwThreadIdDis 8
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.hThreadDis 12
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.lpThreadLocalBaseDis 16
 DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.lpStartAddressDis 20]

[Size_Of_DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO 24]

[DEBUG_EVENT_EXCEPTION_DEBUG_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.dwProcessIdDis 4
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.dwThreadIdDis 8
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCodeDis 12
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionFlagsDis 16
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.pExceptionRecordDis 20
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionAddressDis 24
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.NumberParametersDis 28
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionInformationDis 32
 DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.dwFirstChanceDis 92]

[Size_Of_DEBUG_EVENT_EXCEPTION_DEBUG_INFO 96]

[DEBUG_EVENT_EXIT_PROCESS_DEBUG_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_EXIT_PROCESS_DEBUG_INFO.dwProcessIdDis 4
 DEBUG_EVENT_EXIT_PROCESS_DEBUG_INFO.dwThreadIdDis 8
 DEBUG_EVENT_EXIT_PROCESS_DEBUG_INFO.dwExitCodeDis 12]

[Size_Of_DEBUG_EVENT_EXIT_PROCESS_DEBUG_INFO 16]

[DEBUG_EVENT_EXIT_THREAD_DEBUG_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_EXIT_THREAD_DEBUG_INFO.dwProcessIdDis 4
 DEBUG_EVENT_EXIT_THREAD_DEBUG_INFO.dwThreadIdDis 8
 DEBUG_EVENT_EXIT_THREAD_DEBUG_INFO.dwExitCodeDis 12]

[Size_Of_DEBUG_EVENT_EXIT_THREAD_DEBUG_INFO 16]

[DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.dwProcessIdDis 4
 DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.dwThreadIdDis 8
 DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.hFileDis 12
 DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.lpBaseOfDllDis 16
 DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.dwDebugInfoFileOffsetDis 20
 DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.nDebugInfoSizeDis 24
 DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.lpImageNameDis 28
 DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.fUnicodeDis 32]

[Size_Of_DEBUG_EVENT_LOAD_DLL_DEBUG_INFO 36]

[DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.dwProcessIdDis 4
 DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.dwThreadIdDis 8
 DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.lpDebugStringDataDis 12
 DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.fUnicodeDis 16
 DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.nDebugStringiLengthDis 18]

[Size_Of_DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO 20]

[DEBUG_EVENT_RIP_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_RIP_INFO.dwProcessIdDis 4
 DEBUG_EVENT_RIP_INFO.dwThreadIdDis 8
 DEBUG_EVENT_RIP_INFO.dwErrorDis 12
 DEBUG_EVENT_RIP_INFO.dwTypeDis 16]

[Size_Of_DEBUG_EVENT_RIP_INFO 20]

[DEBUG_EVENT_UNLOAD_DLL_DEBUG_INFO.dwDebugEventCodeDis 0
 DEBUG_EVENT_UNLOAD_DLL_DEBUG_INFO.dwProcessIdDis 4
 DEBUG_EVENT_UNLOAD_DLL_DEBUG_INFO.dwThreadIdDis 8
 DEBUG_EVENT_UNLOAD_DLL_DEBUG_INFO.lpBaseOfDllDis 12]

[Size_Of_DEBUG_EVENT_UNLOAD_DLL_DEBUG_INFO 16]

____________________________________________________________________________________________

[DEBUG_EVENT: B$ 0 #Size_Of_DEBUG_EVENT_EXCEPTION_DEBUG_INFO]

____________________________________________________________________________________________

[CONTEXT:
 C.ContextFlags: D$ ?
 C.iDr0: D$ ?
 C.iDr1: D$ ?
 C.iDr2: D$ ?
 C.iDr3: D$ ?
 C.iDr6: D$ ?
 C.iDr7: D$ ?
 C.FloatSave.ControlWord: D$ ?
 C.FloatSave.StatusWord: D$ ?
 C.FloatSave.TagWord: D$ ?
 C.FloatSave.ErrorOffset: D$ ?
 C.FloatSave.ErrorSelector: D$ ?
 C.FloatSave.DataOffset: D$ ?
 C.FloatSave.DataSelector: D$ ?
 C.FloatSave.RegisterArea: B$ ? #&SIZE_OF_80387_REGISTERS
 C.FloatSave.Cr0NpxState: D$ ?
 C.regGs: D$ ?
 C.regFs: D$ ?
 C.regEs: D$ ?
 C.regDs: D$ ?
 C.regEdi: D$ ?
 C.regEsi: D$ ?
 C.regEbx: D$ ?
 C.regEdx: D$ ?
 C.regEcx: D$ ?
 C.regEax: D$ ?
 C.regEbp: D$ ?
 C.regEip: D$ ?
 C.regCs: D$ ?
 C.regFlag: D$ ?
 C.regEsp: D$ ?
 C.regSs: D$ ?
 C.ExtendedRegisters: B$ ? #32
 C.regMM: B$ ? #128
 C.regXMM: B$ ? #128
 C.UnknownExtendedRegs: B$ ? #224
 EndOfContext: ?]

[ContextSize (EndOfContext-Context)]
____________________________________________________________________________________________

; All user-visible messages are stored here for easier proof-reading / localization.


[ErrorOutside:
"An exception occurred outside the Application. 

This may be due to wrong parameters in an api call or a buggy library.

The debugger highlights the last call of your application before the 
exception has occurred. Verify that the parameters are correctly set.

If the debugger points to the end of the source, either the caller 
couldn't be  identified or the exception occurred after the application 
has terminated. In the latter  case, just test if your application runs 
flawlessly without the debugger.", 0

ErrorOutsideTitle: 'External Error' 0]

[AboutDebugger:
"RosAsm's integrated Win32 debugger.

For bug reports / feature requests use 
RosAsm board or contact me via e-mail. 

Good luck! 

Ludwig Hähne <wkx@gmx.li>" 0]

[DebugThreadHangs:
"The debugger thread does not respond. 
This should not happen! The thread is now terminated!" 0]

[CriticalError: "Critical error" 0]

[MessageKillDebugger:
"You cannot modify the Source in a Debug Session    
    
                     Close the Debugger?" 0]

[DebuggerRunning: "Debugger running ..." 0]
____________________________________________________________________________________________

________________________________________________________________________________________

[Smode: '/S', 0]      ; To run Screen Savers in Saver Mode from the [Run] Button.

[UserHitF9: ?]
____________________________________________________________________________________________

[MY_CONTEXT_FULL &CONTEXT_FULL__&CONTEXT_FLOATING_POINT__&CONTEXT_DEBUG_REGISTERS__&CONTEXT_EXTENDED_REGISTERS]

; Main Routine of the Debugger.

[IsDebugging: &FALSE  Compiling: &FALSE]


[DebugRsrcManagerThreadId: ?]

Proc Debugger:
    On D$IsDebugging = &TRUE, ExitP

    call TestCPUFeatures

    ; initialize all variables
    call 'RosMem.FastZeroMem' DebugProcessData, Size_Of_DebugProcessData

    ; Get OS version information - The main difference between Win9x / WinNT
    ; concerning the debugger is the address space layout.
    mov D$OSVersionInfo.Size OSVI_SIZE
    call 'Kernel32.GetVersionExA' OsVersionInfo
    If D$OsVersionInfo.PlatformId = &VER_PLATFORM_WIN32_WINDOWS
        mov D$AddressLowerBound 0
        mov D$AddressUpperBound 0_BFFF_FFFF
    EndIf

    ; Create the debug dialog that displays registers, labels and memory.
    call CreateDebugWindow

    call 'Kernel32.CreateThread' &NULL, 0, DebugRsrcManagerThread, 0,
                                 &THREAD_PRIORITY_NORMAL, DebugRsrcManagerThreadId
    If eax = &NULL
        call ReportWinError {'Debugger: CreateThread (Resource Manager)' 0}
    Else
        ; we don't need the thread handle
        call 'Kernel32.CloseHandle' eax
    EndIf
EndP
____________________________________________________________________________________________

;;
  User attempts to modify the Source in a Debug Session. 
  Impossible > offer him to either close the Debug Session or to abort the Source Edition
;;

KillDebugger:
    call 'USER32.MessageBoxA', D$hwnd, MessageKillDebugger, DebuggerRunning,
        &MB_SYSTEMMODAL__&MB_YESNO

    push eax
        On eax = &IDYES,
            call 'USER32.SendMessageA', D$DebugDialogHandle, &WM_CLOSE, 0, 0
    pop eax
ret
____________________________________________________________________________________________

; Debugger resource manager thread. This thread takes care of initializing and freeing
; all resources (objects, files, memory). It is separated from the polling
; thread to cleanup properly even if the thread hangs (for whatever reason) and must be
; terminated; which (very rarely) caused deadlocks with the past solution.

[DebugThreadHandle: ? DebugThreadId: ?] ; the polling thread
[UserInputEvent: ?] ; synch dialog with polling thread
[ThreadIDHandleTable: ? NumThreads: ?] ; maps thread id's to handles

;;
[PROCESS_INFORMATION: ; the debuggee's process and (main) thread handle & id
 PI.hProcess: ?
 PI.hThread: ?
 PI.dwProcessId: ?
 PI.dwThreadId: ?]
;;

 ; the debuggee's process and (main) thread handle & id

[PROCESS_INFORMATION:
 PI.hProcess: D$ 0
 PI.hThread: D$ 0
 PI.dwProcessId: D$ 0
 PI.dwThreadId: D$ 0]

;;
[STARTUPINFO: SI_cb: ?              SI_lpReserved: ?       SI_lpDesktop: ?
              SI_lpTitle: ?         SI_dwX: ?              SI_dwY: ?
              SI_dwXSize: ?         SI_dwYSize: ?          SI_dwXCountChars: ?
              SI_dwYCountChars: ?   SI_dwFillAttribute: ?  SI_dwFlags: ?
              SI_wShowWindow: W$ ?  SI_cbReserved2: W$ ?   SI_lpReserved2: D$ ?
              SI_hStdInput: ?       SI_hStdOutput: ?       SI_hStdError: ?]
;;

[STARTUPINFO:
 SI_cb: D$ 0
 SI_lpReserved: D$ 0
 SI_lpDesktop: D$ 0
 SI_lpTitle: D$ 0
 SI_dwX: D$ 0
 SI_dwY: D$ 0
 SI_dwXSize: D$ 0
 SI_dwYSize: D$ 0
 SI_dwXCountChars: D$ 0
 SI_dwYCountChars: D$ 0
 SI_dwFillAttribute: D$ 0
 SI_dwFlags: D$ 0
 SI_wShowWindow: W$ 0
 SI_cbReserved2: W$ 0
 SI_lpReserved2: D$ 0
 SI_hStdInput: D$ 0
 SI_hStdOutput: D$ 0
 SI_hStdError: D$ 0]

[STARTUPINFO.cbDis 0
 STARTUPINFO.lpReservedDis 4
 STARTUPINFO.lpDesktopDis 8
 STARTUPINFO.lpTitleDis 12
 STARTUPINFO.dwXDis 16
 STARTUPINFO.dwYDis 20
 STARTUPINFO.dwXSizeDis 24
 STARTUPINFO.dwYSizeDis 28
 STARTUPINFO.dwXCountCharsDis 32
 STARTUPINFO.dwYCountCharsDis 36
 STARTUPINFO.dwFillAttributeDis 40
 STARTUPINFO.dwFlagsDis 44
 STARTUPINFO.wShowWindowDis 48
 STARTUPINFO.cbReserved2Dis 50
 STARTUPINFO.lpReserved2Dis 52
 STARTUPINFO.hStdInputDis 56
 STARTUPINFO.hStdOutputDis 60
 STARTUPINFO.hStdErrorDis 64]

[Size_Of_STARTUPINFO 68]

;;
[SYSTEM_INFO:
 SYSTEM_INFO.wProcessorArchitecture: W$ 0
 SYSTEM_INFO.wReserved: W$ 0
 SYSTEM_INFO.dwPageSize: D$ 0
 SYSTEM_INFO.lpMinimumApplicationAddress: D$ 0
 SYSTEM_INFO.lpMaximumApplicationAddress: D$ 0
 SYSTEM_INFO.dwActiveProcessorMask: D$ 0
 SYSTEM_INFO.dwNumberOfProcessors: D$ 0
 SYSTEM_INFO.dwProcessorType: D$ 0
 SYSTEM_INFO.dwAllocationGranularity: D$ 0
 SYSTEM_INFO.wProcessorLevel: W$ 0
 SYSTEM_INFO.wProcessorRevision: W$ 0]
;;

DebugRsrcManagerThread:

  ; Create an autoreset event to synchronize the debug dialog with the debugthread.
    call 'Kernel32.CreateEventA' 0, 0, 0, 0
    If eax = &NULL
        call ReportWinError {'Debugger: CreateEvent' 0}
        jmp @Exit
    EndIf
    mov D$UserInputEvent eax

    call CreateBPTable

  ; Create a lookup table to retrieve thread handles. The reason we need this is that
  ; we get a thread handle only with the Create-Thread-Debug-Event. For exception handling
  ; only the thread ID is provided, so the handle and the corresponding ID must be saved.
    VirtualAlloc ThreadIDHandleTable (4096*18) ; enough room for hundreds of modules files loaded on the debug. Ex: in SonyVegas it load hundreds of modules
    mov D$NumThreads 0


    If D$ModuleNameHeap <> 0
        call 'KERNEL32.HeapDestroy' D$ModuleNameHeap | mov D$ModuleNameHeap 0
    End_If

    ;call HeapCreateEx &HEAP_NO_SERIALIZE, 4096, 0, D$SysPageSize
    call HeapCreateEx 0, 4096, 0, D$SysPageSize
    ;call 'KERNEL32.HeapCreate' &HEAP_NO_SERIALIZE, 4096, 0 ; Create a growable heap.
    mov D$ModuleNameHeap eax

    VirtualAlloc ModuleList (4096*18*SizeOf_ModuleEntry) ; enough room for hundreds of modules files loaded on the debug. Ex: in SonyVegas it load hundreds of modules
    mov D$NumModules 0

  ; Create the thread that creates and observes the debuggee.
    call 'Kernel32.CreateThread' &NULL, 0, DebugThread, 0,
                                &THREAD_PRIORITY_NORMAL+&CREATE_SUSPENDED, DebugThreadId
    If eax = &NULL
        call ReportWinError {'Debugger: CreateThread' 0}
        ExitP
    EndIf
    mov D$DebugThreadHandle eax

  ; Create synchronization table (like a printer-spool) for dynamic breakpoints.
    call CreateBPAnteroom
    call InitialFillBPAnteroom

    call InitWatchpointResources

  ; Resume (start) the thread. The thread is created suspended to be sure that the
  ; handle is set correctly.
    call 'Kernel32.ResumeThread' D$DebugThreadHandle

  ; Enter debug event polling
    call 'KERNEL32.WaitForSingleObject' D$DebugThreadHandle, &INFINITE

  ; Now the polling thread is dead- either the debuggee has terminated or the thread
  ; was killed. Clean up.

    call FreeWatchpointResources

    call DestroyBPAnteroom

    call 'Kernel32.CloseHandle' D$DebugThreadHandle
    mov D$DebugThreadHandle 0

  ; Cleanup
    VirtualFree D$ModuleList

    call 'KERNEL32.HeapDestroy' D$ModuleNameHeap | mov D$ModuleNameHeap 0
    VirtualFree D$ThreadIDHandleTable

    call 'Kernel32.CloseHandle' D$PI.hThread
    call 'Kernel32.CloseHandle' D$PI.hProcess

    call DestroyBPTable

  ; we post a close signal no matter if the dialog still exists or not
    call 'User32.PostMessageA' D$DebugDialogHandle, &WM_CLOSE, &NULL, &NULL

    call 'Kernel32.CloseHandle' D$UserInputEvent
    mov D$UserInputEvent 0

  ; If the debuggee has called 'Kernel32.OutputDebugString' there is a file in which the
  ; output has been logged. Close the file now.
    If D$DebugLogFile <> 0
        call 'KERNEL32.CloseHandle' D$DebugLogFile
        mov D$DebugLogFile 0
    End_If

  ; Clear data structures (safety)
    mov ecx EndOfContext | sub ecx Context | shr ecx 2
    mov edi Context, eax 0
    rep stosd

@Exit:
    call 'Kernel32.ExitThread' 0
ret
____________________________________________________________________________________________

[FilterEXE: B$ 'Executables (*.exe)' 0 '*.exe' 0 0]
[HostAppFileName: B$ 0 #&MAX_PATH]
[HostAppTitle: B$ 'Host application ...' 0]
; OpenSourceStruc
[SelectHostAppDialog:
 HostApp.lStructSize: D$ len
 HostApp.hwndOwner: D$ 0
 HostApp.hInstance: D$ 0
 HostApp.lpstrFilter: D$ FilterEXE
 HostApp.lpstrCustomFilter: D$ 0
 HostApp.nMaxCustFilter: D$ 0
 HostApp.nFilterIndex: D$ 0
 HostApp.lpstrFile: D$ DebuggeeExe
 HostApp.nMaxFile: D$ &MAX_PATH
 HostApp.lpstrFileTitle: D$ 0
 HostApp.nMaxFileTitle: D$ 0
 HostApp.lpstrInitialDir: D$ 0
 HostApp.lpstrTitle: D$ HostAppTitle
 HostApp.Flags: D$ &OFN_FILEMUSTEXIST+&OFN_EXPLORER+&OFN_NOCHANGEDIR
 HostApp.nFileOffset: W$ 0
 HostApp.nFileExtension: W$ 0
 HostApp.lpstrDefExt: D$ 0
 HostApp.lCustData: D$ 0
 HostApp.lpfnHook: D$ 0
 HostApp.lpTemplateName: D$ 0]

[DebuggeePath: B$ 0 #&MAX_PATH]
[DebuggeeExe: B$ 0 #&MAX_PATH]

[CommandLineString: B$ 0 #&MAX_PATH]

;;

The flags envolving CreateProcess are:

References:
    https://msdn.microsoft.com/pt-br/library/windows/desktop/ms684863%28v=vs.85%29.aspx?f=255&MSPPError=-2147217396
    ReactOS proc.c
    http://www.pinvoke.net/default.aspx/advapi32/CREATE_PROCESS_FLAGS.html
    https://msdn.microsoft.com/pt-br/library/windows/desktop/ms685100%28v=vs.85%29.aspx?f=255&MSPPError=-2147217396
    https://msdn.microsoft.com/en-us/library/windows/desktop/ms683211%28v=vs.85%29.aspx?f=255&MSPPError=-2147217396
    https://msdn.microsoft.com/en-us/library/windows/desktop/ms686219(v=vs.85).aspx
    http://www.kernelmode.info/forum/viewtopic.php?f=10&t=2992
    https://developer.mbed.org/users/phlb/code/lepton-rtos/raw-file/cb8964e005ea/sys/root/src/kernel/core/ucore/embOSW32_100/win32/WinBase.h
    https://msdn.microsoft.com/en-us/library/ms885182.aspx?f=255&MSPPError=-2147217396
    http://www.kegel.com/wine/process.c
    https://searchcode.com/codesearch/raw/17828018/
    https://searchcode.com/codesearch/view/17828018/
    https://gist.github.com/Benvie/1893415#file-fork-cpp
    https://japan.emc.com/collateral/TechnicalDocument/docu5728.pdf
    https://msdn.microsoft.com/en-us/library/windows/hardware/ff539058%28v=vs.85%29.aspx?f=255&MSPPError=-2147217396
    http://www.xtremevbtalk.com/archive/index.php/t-178659.html
    http://securityxploded.com/dll-injection-and-hooking.php
    https://www.codeproject.com/Articles/4610/Three-Ways-to-Inject-Your-Code-into-Another-Proces

Process Creation Flags:

CREATE_BREAKAWAY_FROM_JOB           0x01000000
CREATE_DEFAULT_ERROR_MODE           0x04000000
CREATE_NEW_CONSOLE                  0x00000010
CREATE_NEW_PROCESS_GROUP            0x00000200
CREATE_NO_WINDOW                    0x08000000
CREATE_PRESERVE_CODE_AUTHZ_LEVEL    0x02000000
CREATE_PROTECTED_PROCESS            0x00040000
CREATE_SEPARATE_WOW_VDM             0x00000800
CREATE_SHARED_WOW_VDM               0x00001000
CREATE_SUSPENDED                    0x00000004
CREATE_UNICODE_ENVIRONMENT          0x00000400
DEBUG_ONLY_THIS_PROCESS             0x00000002
DEBUG_PROCESS                       0x00000001
DETACHED_PROCESS                    0x00000008
EXTENDED_STARTUPINFO_PRESENT        0x00080000
INHERIT_PARENT_AFFINITY             0x00010000

Priority Class (From GetPriorityClass and SetPriorityClass):

ABOVE_NORMAL_PRIORITY_CLASS         0x00008000  (CreateProcess/GetPriorityClass/SetPriorityClass)
BELOW_NORMAL_PRIORITY_CLASS         0x00004000  (CreateProcess/GetPriorityClass/SetPriorityClass)
HIGH_PRIORITY_CLASS                 0x00000080  (CreateProcess/GetPriorityClass/SetPriorityClass)
IDLE_PRIORITY_CLASS                 0x00000040  (CreateProcess/GetPriorityClass/SetPriorityClass)
NORMAL_PRIORITY_CLASS               0x00000020  (CreateProcess/GetPriorityClass/SetPriorityClass)
PROCESS_MODE_BACKGROUND_BEGIN       0x00100000  (CreateProcess(?)/SetPriorityClass - WinServer 2003/XP not supported)
PROCESS_MODE_BACKGROUND_END         0x00200000  (CreateProcess(?)/SetPriorityClass - WinServer 2003/XP not supported)
REALTIME_PRIORITY_CLASS             0x00000100  (CreateProcess/GetPriorityClass/SetPriorityClass)


INHERIT_CALLER_PRIORITY             0x00020000  (CreateProcess/GetPriorityClass/SetPriorityClass - WinCE/Compact2013)
PROFILE_KERNEL                      0x20000000  (CreateProcess/CreateProcessAsUser - Obsolete. It was for Win 3.10)
PROFILE_SERVER                      0x40000000  (CreateProcess/CreateProcessAsUser - Obsolete. It was for Win 3.10)
PROFILE_USER                        0x10000000  (CreateProcess/CreateProcessAsUser - Obsolete. It was for Win 3.10)
CREATE_FORCEDOS                     0x00002000  (flag causes the system to perform MS-DOS program, built-in 16-bit OS / 2 application.)
CREATE_IGNORE_SYSTEM_DEFAULT        0x80000000  (flag for CDb command line options)
STACK_SIZE_PARAM_IS_A_RESERVATION   0x00010000  (Threads only)

;;

; Idapro uses: 0x04000023 = &CREATE_DEFAULT_ERROR_MODE+&NORMAL_PRIORITY_CLASS+&DEBUG_PROCESS+&DEBUG_ONLY_THIS_PROCESS
; http://stackoverflow.com/questions/42637637/writting-a-debugger-how-to-debug-a-dll-accessed-through-loadlibray

;[DirPath: D$ 0 #260]
;[DLLPath: D$ 0 #260]

Proc CreateDebuggeeProcess:
    Local @Success, @DebuggeeParams, @PathLenght

    mov D@Success &FALSE

    ; Allocate buffer for the command line, check if a command line file is provided and
    ; create the debuggee with debug rights for RosAsm.

    call 'RosMem.FastZeroMem' STARTUPINFO, Size_Of_STARTUPINFO
    mov D$SI_cb Size_Of_STARTUPINFO
    call 'KERNEL32.GetStartupInfoA' STARTUPINFO
    mov D$SI_lpReserved &NULL
    mov D$SI_dwFlags 0
    mov W$SI_cbReserved2 0
    mov D$SI_lpReserved2 &NULL

    If D$SaveFileType = FILETYPE_DLL
        call 'Comdlg32.GetOpenFileNameA' SelectHostAppDialog
        On eax = &FALSE, ExitP
        mov D$DebugProcessData.Type DEBUG_DLL
        mov D$DebugProcessData.DLLName DllDebugName ; created in ReloadForDissassembler, ChangeName
        move D$DebugProcessData.DllImageBase D$LinkerDllDefault
    Else
        mov D$DebugProcessData.Type DEBUG_EXE
        mov D$DebugProcessData.DLLName 0

        mov edi DebuggeeExe, esi MainName
        While B$esi <> 0
            movsb
        EndWhile
        move D$edi D$SavingExtension
        mov B$edi+4 0
    End_If

    ; DebuggeeExe contains the full name of the host and it´s path
    call GetDebugCurDirPath DebuggeeExe, DebuggeePath
    mov D@PathLenght eax
    call 'Kernel32.SetCurrentDirectoryA' DebuggeePath

    mov eax DebuggeeExe | add eax D@PathLenght | mov D$DebugProcessData.HostName eax
    mov D$DebugProcessData.HostImageBase LINKERDEFAULT

    call SetupCommandLine
    mov D@DebuggeeParams eax

    ; Assemble the commandline
    call ConfigCommandLineforDebug DebuggeeExe, CommandLineString, D@DebuggeeParams

; creation flags = 4000023
;mov eax &CREATE_DEFAULT_ERROR_MODE+&DEBUG_PROCESS+&DEBUG_ONLY_THIS_PROCESS+&CREATE_NEW_CONSOLE+&CREATE_SUSPENDED+&DETACHED_PROCESS
;;
On ida, this is called from
win32_user.plw:0612757B
 text:00407576                 call    CreateProcessA
 
 and finally on idag.exe came from
 idag.exe:004CD51C call    dword ptr [ecx+3Ch]
;;
    ; Create the process with debug rights GUGA modified this

    ; error before. PROCESS_TERMINATE is a flag for Openprocess
;    call 'KERNEL32.CreateProcessA' 0, CommandLineString, &NULL, &NULL, &FALSE,
 ;                                  &DEBUG_PROCESS+&DEBUG_ONLY_THIS_PROCESS+&PROCESS_TERMINATE,
  ;                                 &NULL, DebuggeePath, STARTUPINFO, PROCESS_INFORMATION

    ;create process in suspended state for injection CREATE_SUSPENDED
    ; accordying to msdn we also need PROCESS_QUERY_INFORMATION and PROCESS_VM_READ to grab info abut the dll name for example

    call 'KERNEL32.CreateProcessA' DebuggeeExe, CommandLineString, &NULL, &NULL, &FALSE,
                                   &CREATE_DEFAULT_ERROR_MODE+&NORMAL_PRIORITY_CLASS+&DEBUG_PROCESS+&DEBUG_ONLY_THIS_PROCESS,
                                   &NULL, DebuggeePath, STARTUPINFO, PROCESS_INFORMATION
    mov D@Success eax
    If eax = &FALSE
        call ReportWinError {'Debugger: CreateProcess' 0}
        On eax = &ERROR_DIRECTORY,
            call 'User32.MessageBoxA' 0, DebuggeePath, {'Directory is:' 0}, &MB_OK
    End_If

    ; Allocated by SetupCommandLine (dirty, but necessary if param-string is static [/S])
    VirtualFree D$CommandLinePtr

    mov eax D@Success
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; eax returns the full size of the lenght string
Proc GetDebugCurDirPath:
    Arguments @Input, @Output
    Uses esi, edi

    mov esi D@Input, edi D@Output
    While B$esi <> 0
        movsb
    EndWhile

    On B$edi <> '\', dec edi ; !!!

    While B$edi <> '\'
        mov B$edi 0
        dec edi
    EndWhile

    inc edi
    sub edi D@Output
    mov eax edi
EndP
____________________________________________________________________________________________

Proc ConfigCommandLineforDebug:
    Arguments @Input, @Output, @CmdLineParams
    Uses esi, edi, eax

    mov edi D@Output, ecx &MAX_PATH
    mov al '"' | stosb | dec ecx
    mov esi D@Input
    While B$esi <> 0
        movsb | dec ecx
    EndWhile
    mov ax '" ' | stosw | sub ecx 2
    mov esi D@CmdLineParams
    cmp esi 0 | je L0>
    While B$esi <> 0
        On ecx = 1, jmp L0>
        movsb | dec ecx
    End_While
L0:
   mov B$edi 0

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; Debug (polling) Thread
____________________________________________________________________________________________
____________________________________________________________________________________________

; The actual debug work is done by this thread, it creates the debuggee and waits
; for debug-events. If these occur it transfers control to the user who can decide how to
; proceed.

[IsDebugEvent: ? DebugStart: ?]
[BreakpointsEnabled: ?]
[BreakpointOp: D$ 0CC];B$ 0CC]
;[ByteBuf: B$ ?]
[ContinueStatus: D$ ?]

Proc DebugThread:
    Local @Exit, @CloseSent

    call CreateDebuggeeProcess
    If eax = &FALSE
        jmp L9>>
    EndIf

    mov D$BreakpointsEnabled &TRUE
    mov D$IsDebugging &TRUE
    mov D$IsDebugEvent &FALSE
    mov B$DebugStart &FALSE
    mov B$ExceptionFlags 0

    ; Main loop, as long as we don't want to quit, wait for debug events and process them.
    mov D@Exit &FALSE, B$DebugStart &FALSE, D@CloseSent 0

    .While D@Exit = &FALSE

L0:     call ClearBuffer DEBUG_EVENT, Size_Of_DEBUG_EVENT_EXCEPTION_DEBUG_INFO
        call 'KERNEL32.WaitForDebugEvent' DEBUG_EVENT, 100

      ; If we receive false, the timeout occurred. Process any user input that makes sense
      ; while the debuggee is running (e.g. killing it or generating breakpoints)
        ..If eax = &FALSE

          ; The user requested the termination of the debuggee while it was running. Send
          ; WM_CLOSE messages to the main windows and then wait a second. Be sure to process
          ; debug messages (do *not* call WaitForSingleObject) inbetween. If the process is
          ; still alive after a second it is terminated.
            .If D$TerminateDebuggee = &TRUE
;;
                mov D$HoldOnBreakpoints 0
                If D@CloseSent = 0
                    call CloseMainWindows
                    mov D@CloseSent 1
                    jmp L0<
                Else
                    inc D@CloseSent
                    On D@CloseSent < 10, jmp L0<
;;
                    call 'KERNEL32.TerminateProcess' D$PI.hProcess, 0
                    ; call SafeTerminateProcess D$PI.hProcess, 0;D$ExitCode ; See CloseProcess
                    jmp L9>>
                ;EndIf
            .End_If

          ; Check if we either need to enable or disable the breakpoints. (HoldOnBreakpoint
          ; has changed).
            mov eax D$HoldOnBreakpoints
            .If eax <> D$BreakpointsEnabled
                If B$HoldOnBreakpoints = &TRUE
                    call EnableProcessBreakpoints
                Else
                    call DisableProcessBreakpoints
                EndIf
            .EndIf

          ; User requested Pause: Generate breakpoint to halt all threads
            If D$PauseThreads = &TRUE
                mov ebx 0, esi D$ThreadIDHandleTable
                While ebx < D$NumThreads
                    call HaltThread D$esi+ebx*8+4
                    inc ebx
                EndWhile
                mov D$PauseThreads &FALSE
            EndIf

          ; Look for BP changes in the source editor
            call ClearBPAnteroom D$DebugBaseOfCode
            jmp L0<<

        ..End_If

        mov eax D$DEBUG_EVENT+DEBUG_EVENT.dwProcessIdDis;DE.dwProcessId
        If eax <> D$PI.dwProcessId
          ; If this debug event belongs to an other (child) process, continue execution
            call 'Kernel32.ContinueDebugEvent' D$DEBUG_EVENT+DEBUG_EVENT.dwProcessIdDis, D$DEBUG_EVENT+DEBUG_EVENT.dwThreadIdDis, &DBG_CONTINUE
            ;call 'Kernel32.ContinueDebugEvent' D$DE.dwProcessId, D$DE.dwThreadId, &DBG_CONTINUE
            jmp L0<<
        End_If

        mov D$ContinueStatus &DBG_CONTINUE

        .If D$DEBUG_EVENT+DEBUG_EVENT.dwDebugEventCodeDis = &EXIT_PROCESS_DEBUG_EVENT
            mov D$DebugProcessData.IsProcessActivated &FALSE
            mov D@Exit &TRUE
        .Else_If D$DEBUG_EVENT+DEBUG_EVENT.dwDebugEventCodeDis = &CREATE_THREAD_DEBUG_EVENT
            call Debugger_OnCreateThread
        .Else_If D$DEBUG_EVENT+DEBUG_EVENT.dwDebugEventCodeDis = &EXIT_THREAD_DEBUG_EVENT
            call Debugger_OnExitThread
        .Else_If D$DEBUG_EVENT+DEBUG_EVENT.dwDebugEventCodeDis = &LOAD_DLL_DEBUG_EVENT
            call Debugger_OnLoadDll D$DEBUG_EVENT+DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.lpBaseOfDllDis
        .Else_If D$DEBUG_EVENT+DEBUG_EVENT.dwDebugEventCodeDis = &UNLOAD_DLL_DEBUG_EVENT
            call Debugger_OnUnloadDll D$DEBUG_EVENT+DEBUG_EVENT_UNLOAD_DLL_DEBUG_INFO.lpBaseOfDllDis
        .Else_If D$DEBUG_EVENT+DEBUG_EVENT.dwDebugEventCodeDis = &EXCEPTION_DEBUG_EVENT
            call Debugger_OnException D$DebugBaseOfCode, D$DebugCodeSize
            mov D@Exit eax
        .Else_If D$DEBUG_EVENT+DEBUG_EVENT.dwDebugEventCodeDis = &CREATE_PROCESS_DEBUG_EVENT
            call Debugger_OnCreateProcess
        .Else_If D$DEBUG_EVENT+DEBUG_EVENT.dwDebugEventCodeDis = &OUTPUT_DEBUG_STRING_EVENT
            call HandleDebugString
            ;call 'RosMem.VMemFree' eax
        .Else
            mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
        .End_If

        call 'Kernel32.ContinueDebugEvent' D$DEBUG_EVENT+DEBUG_EVENT.dwProcessIdDis, D$DEBUG_EVENT+DEBUG_EVENT.dwThreadIdDis, D$ContinueStatus

    .End_While

L9: mov B$IsDebugging &FALSE
    call 'Kernel32.ExitThread' 0
EndP
____________________________________________________________________________________________

[MAX_DBG_LOGSTR 256]
[DgbLogStrDis: D$ 0 #MAX_DBG_LOGSTR]

; When a thread is created in the debuggee. Add Thread ID/Handle pair to table.

Proc Debugger_OnCreateThread:

    call 'RosMem.FastZeroMem' DgbLogStrDis, MAX_DBG_LOGSTR

    mov esi D$ThreadIDHandleTable, ecx D$NumThreads
    move D$esi+ecx*8 D$DEBUG_EVENT+DEBUG_EVENT.dwThreadIdDis
    move D$esi+ecx*8+4 D$DEBUG_EVENT+DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.hThreadDis
    inc D$NumThreads

    call GetModuleNameFromAddress D$DEBUG_EVENT+DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.lpStartAddressDis, D$ModuleList, D$NumModules
    If eax = 0
        mov eax { B$ "Unknown Dll", 0}
    End_If

    C_call FormatStr DgbLogStrDis, {'Thread ID=%d created, handle=0x%x, Data Block=0x%x, Start at=0x%x (%s)' 0},
                                    D$DEBUG_EVENT+DEBUG_EVENT.dwThreadIdDis,
                                    D$DEBUG_EVENT+DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.hThreadDis,
                                    D$DEBUG_EVENT+DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.lpThreadLocalBaseDis,
                                    D$DEBUG_EVENT+DEBUG_EVENT_CREATE_THREAD_DEBUG_INFO.lpStartAddressDis,
                                    eax


    ;call 'User32.PostMessageA' D$LogForm_Handle, WM_LOG, D@DgbLogStrDis, eax
    ;call 'User32.PostMessageA' D$DebugDialogHandle, WM_LOG, DgbLogStrDis, eax
    call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax

EndP
____________________________________________________________________________________________

; Overwrite Thread ID/Handle pair in table with last entry.

; Here we can make it global because when debug acess exitthread event, it closes the handle to the process
; therefore it may cause some errors when listing the values on the log file/window
Proc Debugger_OnExitThread:

    call 'RosMem.FastZeroMem' DgbLogStrDis, MAX_DBG_LOGSTR

    dec D$NumThreads
    mov ecx D$NumThreads, edx 0
    mov esi D$ThreadIDHandleTable, eax D$DEBUG_EVENT+DEBUG_EVENT.dwThreadIdDis
    While D$esi+edx*8 <> eax
        inc edx
    End_While
    move D$esi+edx*8 D$esi+ecx*8
    move D$esi+edx*8+4 D$esi+ecx*8+4

    C_call FormatStr DgbLogStrDis, {'Thread ID=%d terminated' 0}, D$DEBUG_EVENT+DEBUG_EVENT.dwThreadIdDis
    ;call 'User32.PostMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax
    ;call 'User32.PostMessageA' D$DebugDialogHandle, WM_LOG, DgbLogStrDis, eax
    call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax

EndP
____________________________________________________________________________________________

; When process is created. Add main thread ID/handle to table.

[DebugBaseOfCode: ?  DebugCodeSize: ?]

Proc Debugger_OnCreateProcess:
    Local @pErrMode, @DllSize, @IsPacked

    ;call 'KERNEL32.GetFileSize' D$CPDI.hFile, 0 | mov D$PE_hdr_ProcessSize eax
    call 'RosMem.FastZeroMem' DgbLogStrDis, MAX_DBG_LOGSTR

    call 'KERNEL32.CloseHandle' D$DEBUG_EVENT+DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.hFileDis

    mov D$DebugBaseOfCode 0, D$DebugCodeSize 0

    mov esi D$ThreadIDHandleTable
    move D$esi D$DEBUG_EVENT+DEBUG_EVENT.dwThreadIdDis
    move D$esi+4 D$DEBUG_EVENT+DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.hThreadDis
    inc D$NumThreads

    lea eax D@pErrMode | mov D$eax 0
    lea ebx D@IsPacked | mov D$ebx 0
    lea ecx D@DllSize | mov D$ecx 0
    call ScanPEHeader D$DEBUG_EVENT+DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.lpBaseOfImageDis, ecx, ebx, eax
    call GetModuleName D$DEBUG_EVENT+DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.lpBaseOfImageDis, D$ModuleList, D$NumModules
    mov ebx eax

     ; The process was activated for the 1st time and we are using as a host an exe ?
    ...If_And D$DebugProcessData.IsProcessActivated = &FALSE, D$DebugProcessData.Type = DEBUG_EXE

        call StrCmp ebx, D$DebugProcessData.HostName
        ..If eax = 0 ; If strings match. Check. make sure it is the proper file and was not reallocated
            ;call GetCurModuleArrayPtr
            mov eax D$NumModules | dec eax ; dec eax because it was incremented at ScanPEHeader. So, it is never zero.
            call GetCurModuleArrayPtr D$ModuleList, eax
            mov edi D$DebugProcessData.HostImageBase
            .If D$eax+ME_Base = edi
                mov edi D$eax+ME_Size | mov D$DebugProcessData.HostImageSize edi
                mov edi D$eax+ME_CodeBase | mov D$DebugProcessData.HostCodeBase edi
                mov ecx D$eax+ME_CodeSize | mov D$DebugProcessData.HostCodeSize ecx ; same as AppRVAimageSize
                mov D$DebugBaseOfCode edi
                mov D$DebugCodeSize ecx
                mov D$DebugProcessData.IsProcessActivated &TRUE
            .End_If
            ;If_And D$DebugProcessData.Type = DEBUG_EXE, D$eax+ME_Base = LINKERDEFAULT
;                mov D$DebugBaseOfCode edi
  ;              mov D$DebugCodeSize ecx
 ;               mov D$DebugProcessData.IsProcessActivated &TRUE
            ;End_If
        ..End_If

    ...End_If
;;
        ; This is responsible for setting the breakpoint
        mov eax D@BaseAddress, ecx D@CodeSize, edx D@CodeBase
        .If_Or D$SavingExtension = '.DLL', D$SaveFileType = FILETYPE_DLL
            If eax = D$LinkerDllDefault
                mov D$DebugBaseOfCode edx
                mov D$DebugCodeSize ecx
            End_If
        .Else
            If eax = LINKERDEFAULT
                mov D$DebugBaseOfCode edx
                mov D$DebugCodeSize ecx
            End_If
        .EndIf
;;

    mov eax ebx
    C_call FormatStr DgbLogStrDis, {'%s ID=%d mapped at 0x%x' 0}, eax, D$DEBUG_EVENT+DEBUG_EVENT.dwProcessIdDis, D$DEBUG_EVENT+DEBUG_EVENT_CREATE_PROCESS_DEBUG_INFO.lpBaseOfImageDis
    call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax


;ret
EndP

____________________________________________________________________________________________

; When exception/breakpoint is encountered.

[DebugEventType: ? WatchedAddress: ?]
[DET_BP 1 DET_STEP 2 DET_WP 3 DET_EXCEPTION 4]

; Exception flags
[ExceptionFlags: 0]
[E_HAPPENED 1     E_OUTSIDE 2     E_MUSTEXIT 4]

[CurrentModule: ?]

Proc Debugger_OnException:
    Arguments @BaseOfCode, @CodeSize
    Local @hThread, @ExitExceptCode


    ;call GetFullFileNameFromHandle D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionAddressDis;D@BaseAddress

    mov D@ExitExceptCode &FALSE

    ;mov edi ExceptionTypes, eax D$E.ExceptionCode, ecx 18
    ;repne scasd | jne P9>>
    ;mov eax ExceptionStrings | mov ebx 17 | sub ebx ecx | shl ebx 2 | add eax ebx
    ;mov eax D$eax, D$BreakTitle eax

    ; Retrieve handle of the thread that has thrown the exception
    mov esi D$ThreadIDHandleTable, eax D$DEBUG_EVENT+DEBUG_EVENT.dwThreadIdDis;DE.dwThreadID
    While D$esi <> eax
        add esi 8
    End_While
    move D@hThread D$esi+4

    ;mov D$C.ContextFlags MY_CONTEXT_FULL
    ;call 'KERNEL32.GetThreadContext' D@hThread, CONTEXT

    ...If B$DebugStart = &TRUE


    ;If D$DebugBaseOfCode = 0 DebugCodeSize
     ;   mov eax eax

    ;End_If

      ; Take note which window currently has the focus
        call 'USER32.GetForegroundWindow' | mov D$ActiveWindow eax

        mov D$C.ContextFlags MY_CONTEXT_FULL
        call 'KERNEL32.GetThreadContext' D@hThread, CONTEXT

        call IsModuleCode D$C.regEip
        If eax <> 0
            move D$CurrentModule D$eax+ME_Name
        Else
            mov D$CurrentModule 0
        EndIf

        ..If D@BaseOfCode <> 0 ;  the exception was found outise the debugged file (dll or it´s host). see Debugger_OnLoadDll
            ; need to see here if we are in the same target dll
            call IsProcessCode D@BaseOfCode, D@CodeSize, D$C.regEip
            .If eax = 1
                move D$SourcePosCodeAddress D$C.regEip
            .Else
                call ScanStackForCodePointer D@BaseOfCode, D@CodeSize, D$C.regEsp
                ;On eax = 0, dec D$SourcePosCodeAddress
                If_And eax = 0, D$SourcePosCodeAddress <> 0
                    dec D$SourcePosCodeAddress
                End_If
            .EndIf
        ;..Else
            ;mov eax eax
        ..End_If
        call ResolveSegmentAddresses D@hThread

        ; Write-back dynamic breakpoints. BPPending points at an entry in the
        ; breakpoint table-
        mov ebx D$BPPending
        If ebx <> 0
            call WriteProcessMem D$ebx+BPTable.AddressDis, BreakpointOp, 1
            mov B$ebx+BPTable.StateDis BP_ENABLED
            mov D$BPPending 0
        EndIf

        mov D$ContinueMode CONTINUE_RUN ; default

        .If D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCodeDis = &EXCEPTION_SINGLE_STEP
            mov D$DebugEventType DET_STEP

            call EncounterWatchPoint

            If D$RunAfterWriteBack = &FALSE
                On D$HoldOnBreakpoints = &TRUE, call SignalDebugEvent        ; <<<<<<<<<<<<<<<<<<<<<<<<<<
            Else
                mov D$ContinueMode CONTINUE_RUN
                mov D$RunAfterWriteBack &FALSE
            EndIf

        .Else_If D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCodeDis = &EXCEPTION_BREAKPOINT
            mov D$DebugEventType DET_BP

            call EncounterBreakpoint
            On D$HoldOnBreakpoints = &TRUE, call SignalDebugEvent        ; <<<<<<<<<<<<<<<<<<<<<<<<<<

        .Else
            mov D$DebugEventType DET_EXCEPTION

            call EncounterException D@BaseOfCode, D@CodeSize
            mov D@ExitExceptCode eax

            ;On D$ContinueStatus <> &DBG_EXCEPTION_NOT_HANDLED,
            ;    call SignalDebugEvent        ; <<<<<<<<<<<<<<<<<<<<<<<<<<

            ;If D$E.ExceptionFlags = 0
            ;    call NextInstructionDecode
            ;    mov eax D$InstructionLength
            ;    add D$C.regEip eax ; TODO this is a dirty hack and no real solution!!
            ;    mov D@Exit 0
            ;EndIf
        .End_If

        .If D@ExitExceptCode = &FALSE
            ; For step over instructions, we overwrite the process memory after
            ; the next instruction with a breakpoint. The overwritten byte is
            ; preserved with the code address in the breakpoint table.
            If D$ContinueMode = CONTINUE_STEPOVER
                mov ebx D$C.regEip | add ebx D$InstructionLength
                call AddProcessBreakpoint ebx, BP_ONESHOT, BP_ENABLED, 0
            Else_If D$ContinueMode = CONTINUE_RETURNTOCALLER
                call ScanStackForCodePointer D@BaseOfCode, D@CodeSize, D$C.regEsp
                On eax <> 0, call AddProcessBreakpoint eax, BP_ONESHOT, BP_ENABLED, 0
            End_If

            If D$BPPending <> 0
                On D$ContinueMode <> CONTINUE_STEP, mov D$RunAfterWriteBack &TRUE
                mov D$ContinueMode CONTINUE_STEP
            EndIf

            If D$ContinueMode = CONTINUE_STEP
                or D$C.regFlag 0100 ; set trap flag
            Else
                and D$C.regFlag 0_FFFF_FEFF ; in case of Exceptions it's not auto-cleared
            EndIf

            On D$ContinueMode = CONTINUE_RUN, call ShowProcessWindows

            On D$TerminateDebuggee = &TRUE, mov D@ExitExceptCode &TRUE

            ; Take care of user-activated/deactivated dynamic breakpoints.
            call ClearBPAnteroom D@BaseOfCode

            call TransferWatchpoints

            mov D$C.ContextFlags MY_CONTEXT_FULL
            call 'Kernel32.SetThreadContext' D@hThread, CONTEXT

        .EndIf

    ...Else

        If D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCodeDis = &EXCEPTION_BREAKPOINT
            mov B$DebugStart &TRUE
            call ClearBPAnteroom D@BaseOfCode
            call 'User32.PostMessageA' D$DebugDialogHandle, WM_BEGIN_DEBUG, 0, 0
        EndIf

    ...End_If
    mov eax D@ExitExceptCode

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

;[LDT_ENTRY: B$ ? #8]

[LDT_ENTRY:
 LDT_ENTRY.LimitLow: W$ 0
 LDT_ENTRY.BaseLow: W$ 0
 LDT_ENTRY.HighWord1BaseMid: B$ 0
 LDT_ENTRY.HighWord1Flags1: B$ 0
 LDT_ENTRY.HighWord1Flags2: B$ 0
 LDT_ENTRY.HighWord1BaseHi: B$ 0]

[LDT_ENTRY.LimitLowDis 0
 LDT_ENTRY.BaseLowDis 2
 LDT_ENTRY.HighWord1BaseMidDis 4
 LDT_ENTRY.HighWord1Flags1Dis 5
 LDT_ENTRY.HighWord1Flags2Dis 6
 LDT_ENTRY.HighWord1BaseHiDis 7]

[Size_Of_LDT_ENTRY 8]

[LinearSegmentAddresses:
 CS.Linear: ? CS.Limit: ? DS.Linear: ? DS.Limit: ? ES.Linear: ? ES.Limit: ?
 FS.Linear: ? FS.Limit: ? GS.Linear: ? GS.Limit: ? SS.Linear: ? SS.Limit: ?]

Proc ResolveSegmentAddresses:
    Arguments @hThread
    Uses ebx, ecx, edi, esi, edx, eax

    mov esi SegRegMap, edi LinearSegmentAddresses, ebx 6

    While ebx > 0
        lodsd
        call 'KERNEL32.GetThreadSelectorEntry' D@hThread, D$eax, LDT_Entry
        If eax = &TRUE
            mov al B$LDT_ENTRY+LDT_ENTRY.HighWord1BaseMidDis, ah B$LDT_ENTRY+LDT_ENTRY.HighWord1BaseHiDis
            shl eax 16
            mov ax W$LDT_ENTRY+LDT_ENTRY.BaseLowDis
            stosd
            movzx eax W$LDT_ENTRY+LDT_ENTRY.LimitLowDis
            stosd
        EndIf
        dec ebx
    EndWhile
EndP
____________________________________________________________________________________________

; The 'ModuleList' points at a list of module information entries that are currently
; loaded and mapped into the address space of the debuggee.

; Entry in module list
[ME_Base 0          ; The base VA of the mapped module
 ME_Size 4          ; size in bytes
 ME_Name 8          ; pointer to filename
 ME_CodeBase 12     ; base VA of code section
 ME_CodeSize 16     ; size in bytes of code section
 ME_ExportBase 20   ; base RVA of export section
 ME_ExportSize 24]  ; size in bytes of export section
[SizeOf_ModuleEntry 28]

[ModuleList: ? NumModules: ? ModuleNameHeap: ?]

;[new | imul eax D$Num#1s SizeOf_#1Entry | add eax D$#1List | inc D$NumModules | mov #2 eax]
;;
[delete | push esi edi ecx | dec D$Num#1s |
    mov ecx SizeOf_#1Entry | imul esi D$Num#1s SizeOf_#1Entry | add esi D$#1List |
    mov edi #2 | rep movsb |
    pop ecx edi esi]
;;
____________________________________________________________________________________________

; After messing with the toolhelp and psapi and having endless problems enumerating and
; getting module info I finally decided to use none of these evil APIs and read information
; needed directly from the PE image in the debuggee as soon as it is loaded. Most of the
; addresses are stored in the header. Only the module name must be read out of the export
; section or, in case of executables, from the commandline.

[PE_hdr_Process: D$ 0]
[PE_hdr_ProcessSize: D$ 0]

[Sz_ModuleName: B$ 0 #512]
[PESectionBuffer: D$ ?]
[PESectionSize: D$ 0]

; pErrorMode
[ERR_MODULE_NOTQUERY 0] ; Returned 0 in VirtualQueryEx inside GetModuleSize
[ERR_MODULE_READ_PROCESS 1]; ReadProcessMemory returned 0 inside ReadProcessMem
[ERR_MODULE_NOT_PE 2]; CheckPE_MZ returned 0. This is not a true PE/MZ file
[ERR_MODULE_NO_HANDLE 3]; Process handle is null.
[ERR_MODULE_NO_BASE_ADDRESS 4]; Module address is null.
[ERR_MODULE_NO_EXPORT 5]; A dll without an export table?? Maybe only for resources
[ERR_MODULE_NO_EXPORT_NOT_MAPPED 6]; Module could not be mapped. Error in GetMappedFileName while trying to retrieve the nme for a dll containing no export

; This stops the debugging and the oler one (fixed. Not...Why ?)

;;
    The following error mode return 0 and don´t fill ModuleList
    ERR_MODULE_NO_HANDLE, ERR_MODULE_NO_BASE_ADDRESS, ERR_MODULE_NOTQUERY,
    ERR_MODULE_READ_PROCESS, ERR_MODULE_NOT_PE, ERR_MODULE_NO_EXPORT_NOT_MAPPED
    
    The following error return FALSE and fill the ModuleList
    ERR_MODULE_NO_EXPORT
    
;;

Proc ScanPEHeader:
    Arguments @BaseAddress, @DllSize, @IsPacked, @pErrorMode
    Local @Size, @CodeBase, @CodeSize, @ExportBaseRVA, @ExportSize, @Name,
          @IsExe, @SectionsCount, @PESection, @PEHdr, @IsErrExport, @ExportDirName
    Uses esi, edi, ebx, edx, ecx


    xor eax eax
    mov D@IsErrExport 0
    mov edi D@pErrorMode
    If D$PI.hProcess = 0
        mov D$edi ERR_MODULE_NO_HANDLE
        ExitP
    Else_If D@BaseAddress = 0
        mov D$edi ERR_MODULE_NO_BASE_ADDRESS
        ExitP
    End_If

    call GetModuleSize D$PI.hProcess, D@BaseAddress, PE_hdr_ProcessSize
    mov D$edi ERR_MODULE_NOTQUERY
    On eax = 0, ExitP


    mov D@IsExe 0
    mov D@SectionsCount 0
    ; Read size of image, base-address and size of the code section & export table
    ; from PE image header.
    mov D$PEBuffer 0
    mov D$PESectionBuffer 0
    mov D$PESectionSize 0

    VirtualAlloc PEBuffer, D$PE_hdr_ProcessSize

    call ReadProcessMem D@BaseAddress, D$PEBuffer, D$PE_hdr_ProcessSize
    mov edi D@pErrorMode | mov D$edi ERR_MODULE_READ_PROCESS
    On eax = 0, jmp @Error

    call CheckPE_MZ D$PEBuffer, D$PE_hdr_ProcessSize, PE_hdr_Process
    mov D$edi ERR_MODULE_NOT_PE
    On eax = 0, jmp @Error
L1:
    On W$eax+PeHeader.OptionalHeader.MagicDis <> &IMAGE_NT_OPTIONAL_HDR32_MAGIC, jmp @Error ; check optional header ID
    mov esi eax

    Test_If_Not W$esi+PeHeader.FileHeader.CharacteristicsDis &IMAGE_FILE_DLL
        mov D@IsExe &TRUE
    Test_End

    move D@Size D$esi+PeHeader.OptionalHeader.SizeOfImageDis
    mov eax D$esi+PeHeader.OptionalHeader.BaseOfCodeDis | add eax D@BaseAddress ; Baseofcode can be zero when we have No export functions
    mov D@CodeBase eax
    move D@CodeSize D$esi+PeHeader.OptionalHeader.SizeOfCodeDis
    move D@ExportBaseRVA D$esi+PeHeader.DataDirectory.ExportDis
    move D@ExportSize D$esi+PeHeader.DataDirectory.ExportSizeDis
    movzx eax W$esi+PeHeader.FileHeader.NumberOfSectionsDis
    mov D@SectionsCount eax
    mov edx esi | add edx SizeOf_PeHeader
    sub esi D$PEBuffer
    mov D@PEHdr esi ; RVA of PE signature
    sub edx D$PEBuffer
    mov D@PESection edx ; RVA of PE Section

    ; A PE can be a DLL or an EXE (and some other formats that RosAsm does not deal with yet).
    ; If it is an executable its name is extracted from the commandline. Otherwise (DLL) the
    ; name is expected in the export table.
    ...If D@IsExe = &FALSE

        ; 1St see if te dll is packed.
        mov eax D@IsPacked | mov D$eax &FALSE

        mov eax D@ExportBaseRVA | add eax D$PEBuffer | mov esi D$eax+IMAGE_EXPORT_DIRECTORY.nNameDis | mov D@ExportDirName esi
        .If esi <> 0 ; See if the module is packed or not
            call dbg_IsPointtoVirtualSection esi, D@SectionsCount, D@PESection, D$PEBuffer, D@PEHdr
            If eax = &TRUE
                mov eax D@IsPacked | mov D$eax &TRUE
            End_If
            add esi D$PEBuffer
        .End_If

        ; A dll without an export table?? Maybe only for resources
        ..If_Or D@ExportBaseRVA = 0, D@ExportSize = 0;, D@ExportDirName = 0 (When BaseRVa = 0 or Size = 0, it is common that Nme is 0FFFF or 0)
            call FindModuleFileNameNoExport D@BaseAddress, lpBaseName ; a variation of GetFileNameFromHandle and GetFullFileNameFromHandle
            If eax <> 0
                mov esi eax
                mov D$edi ERR_MODULE_NO_EXPORT
                mov D@IsErrExport &TRUE
            Else
                mov D$edi ERR_MODULE_NO_EXPORT_NOT_MAPPED
                jmp @Error
            End_If
        ..End_If
;;
            mov eax D@ExportBaseRVA | add eax D$PEBuffer | mov esi D$eax+IMAGE_EXPORT_DIRECTORY.nNameDis
            call dbg_IsPointtoVirtualSection esi, D@SectionsCount, D@PESection, D$PEBuffer, D@PEHdr
            If eax = &TRUE
                mov esi {"Packed Dll", 0}
            Else
                add esi D$PEBuffer
            End_If
;;
;L7:

        call StrLenProc esi
        call Heap_Operator_NewEx D$ModuleNameHeap, &HEAP_ZERO_MEMORY, eax, D$SysPageSize
        mov D@Name eax, edi D@Name

        While B$esi <> 0 | movsb | End_While
        mov B$edi 0

        ; TODO is it possible that exe files are used as library from the actual application exe?
        ; If really possible this branch would be wrong. (ntoskrnl.exe !)
    ...Else
        move D@Name D$DebugProcessData.HostName
    ...EndIf

L2:

        VirtualFree D$PEBuffer

        ;On D$NumModules <> 0, inc D$NumModules
        ;inc D$NumModules
        call GetCurModuleArrayPtr D$ModuleList, D$NumModules
        mov edi eax
        inc D$NumModules

        move D$edi+ME_Base D@BaseAddress
        move D$edi+ME_Size D@Size
        move D$edi+ME_Name D@Name
        move D$edi+ME_CodeBase D@CodeBase
        move D$edi+ME_CodeSize D@CodeSize
        move D$edi+ME_ExportBase D@ExportBaseRVA
        move D$edi+ME_ExportSize D@ExportSize
;;
        ; This is responsible for setting the breakpoint
        mov eax D@BaseAddress, ecx D@CodeSize, edx D@CodeBase
        .If_Or D$SavingExtension = '.DLL', D$SaveFileType = FILETYPE_DLL
            If eax = D$LinkerDllDefault
                mov D$DebugBaseOfCode edx
                mov D$DebugCodeSize ecx
            End_If
        .Else
            If eax = LINKERDEFAULT
                mov D$DebugBaseOfCode edx
                mov D$DebugCodeSize ecx
            End_If
        .EndIf
;;
        mov eax &TRUE

        If D@IsErrExport = &TRUE
            xor eax eax
        End_If

        ExitP
@Error:
        VirtualFree D$PEBuffer
        mov eax &FALSE
EndP
____________________________________________________________________________________________
;;
Proc IsExportSectionPacked:
    Arguments @InputRVA, @SectionCount, @PESection, @BaseAddress, @PEHeader

    mov eax D@ExportBaseRVA
    add eax D$PEBuffer
    mov esi D$eax+IMAGE_EXPORT_DIRECTORY.nNameDis
    call dbg_IsPointtoVirtualSection esi, D@SectionsCount, D@PESection, D$PEBuffer, D@PEHdr
    If eax = &TRUE
                ;move D@Name {"Packed Dll", 0} | jmp L2>>
                mov esi {"Packed Dll", 0}
    Else
                add esi D$PEBuffer
    End_If
EndP
;;
____________________________________________________________________________________________

[DEBUG_EXE 0]
[DEBUG_DLL 1]

[DllDebugName: B$ 0 #&MAXPATH]

[DebugProcessData:
 DebugProcessData.IsProcessActivated: D$ 0 ; 1st time the process was created. &TRUE or &FALSE
 DebugProcessData.Type: D$ 0 ; DEBUG_EXE, DEBUG_DLL
 DebugProcessData.HostName: D$ 0
 DebugProcessData.HostImageBase: D$ 0
 DebugProcessData.HostImageSize: D$ 0
 DebugProcessData.HostCodeBase: D$ 0
 DebugProcessData.HostCodeSize: D$ 0
 DebugProcessData.IsRebased: D$ 0
 DebugProcessData.DLLName: D$ 0
 DebugProcessData.DllImageBase: D$ 0
 DebugProcessData.DllImageSize: D$ 0
 DebugProcessData.DllCodeBase: D$ 0
 DebugProcessData.DllCodeSize: D$ 0
 DebugProcessData.DllExportBase: D$ 0
 DebugProcessData.DllExportSize: D$ 0
 DebugProcessData.DllpNewImageBase: D$ 0 ; Pointer to New ImageBase address in memory inside the PE/Dll
 DebugProcessData.DllNewImageBase: D$ 0 ; The new image base adress
 DebugProcessData.DllNewImageSize: D$ 0
 DebugProcessData.DllNewCodeBase: D$ 0
 DebugProcessData.DllNewCodeSize: D$ 0
 DebugProcessData.DllNewExportBase: D$ 0
 DebugProcessData.DllNewExportSize: D$ 0]

[DebugProcessData.IsProcessActivatedDis 0
 DebugProcessData.TypeDis 4
 DebugProcessData.HostNameDis 8
 DebugProcessData.HostImageBaseDis 12
 DebugProcessData.HostImageSizeDis 16
 DebugProcessData.HostCodeBaseDis 20
 DebugProcessData.HostCodeSizeDis 24
 DebugProcessData.IsRebasedDis 28
 DebugProcessData.DLLNameDis 32
 DebugProcessData.DllImageBaseDis 36
 DebugProcessData.DllImageSizeDis 40
 DebugProcessData.DllCodeBaseDis 44
 DebugProcessData.DllCodeSizeDis 48
 DebugProcessData.DllExportBaseDis 52
 DebugProcessData.DllExportSizeDis 56
 DebugProcessData.DllNewImageBaseDis 60
 DebugProcessData.DllNewImageSizeDis 64
 DebugProcessData.DllNewCodeBaseDis 68
 DebugProcessData.DllNewCodeSizeDis 72
 DebugProcessData.DllNewExportBaseDis 76
 DebugProcessData.DllNewExportSizeDis 80]

[Size_Of_DebugProcessData 84]

[PImage_NTHeader: D$ 0]
____________________________________________________________________________________________

; Retrieve current loaded module structure array pointer

Proc GetCurModuleArrayPtr:
    Arguments @pModuleList, @NumModules
    Uses edx

    mov eax D@NumModules; | dec eax ; dec eax because it was incremented at ScanPEHeader. So, it is never zero.
    xor edx edx
    imul eax SizeOf_ModuleEntry
    add eax D@pModuleList

EndP

____________________________________________________________________________________________

____________________________________________________________________________________________

; MEMORY_BASIC_INFORMATION

[mbi:
 mbi.BaseAddress: D$ 0
 mbi.AllocationBase: D$ 0
 mbi.AllocationProtect: D$ 0
 mbi.RegionSize: D$ 0
 mbi.State: D$ 0
 mbi.Protect: D$ 0
 mbi.lType: D$ 0]

[MEMORY_BASIC_INFORMATION.BaseAddressDis 0
 MEMORY_BASIC_INFORMATION.AllocationBaseDis 4
 MEMORY_BASIC_INFORMATION.AllocationProtectDis 8
 MEMORY_BASIC_INFORMATION.RegionSizeDis 12
 MEMORY_BASIC_INFORMATION.StateDis 16
 MEMORY_BASIC_INFORMATION.ProtectDis 20
 MEMORY_BASIC_INFORMATION.lTypeDis 24]

[Size_Of_MEMORY_BASIC_INFORMATION 28]

Proc GetModuleSize:
    Arguments @hProcess, @ImageBase, @Size
    Local @bfound, @QueryAddress
    Uses ecx, edi, ecx


    mov D@bfound 0
    ; Scan the address space of the process and determine where the memory region
    ; allocated for the module ends (that is, we are looking for the first range
    ; of pages whose AllocationBase is not the same as the load address of the module)
    xor eax eax
    mov edi D@Size
    mov D$edi 0

    On D@hProcess = &NULL, ExitP ; Process handle is null.
    On D@ImageBase = 0, ExitP ; Module address is null.

    move D@QueryAddress D@ImageBase

    L1:
        call 'KERNEL32.VirtualQueryEx' D@hProcess, D@QueryAddress, mbi, Size_Of_MEMORY_BASIC_INFORMATION
        If eax <> Size_Of_MEMORY_BASIC_INFORMATION
            xor eax eax | ExitP
        End_If

        mov eax D@ImageBase
        If D$mbi.AllocationBase <> eax
            ; size = D@QueryAddress - D@ImageBase
            mov eax D@QueryAddress
            sub eax D@ImageBase
            mov edi D@Size
            mov D$edi eax | EXitP
        End_If

        mov ecx D$mbi.RegionSize
        add D@QueryAddress ecx
    jmp L1<<

EndP

____________________________________________________________________________________________

[lpBaseName : B$ 0 #256]

[Sz_DbgDll_LogStr: B$ "%s mapped at 0x%x, handle: 0x%x", 0]
[Sz_DbgDllremap_LogStr: B$ "%s Remapped at 0x%x, handle: 0x%x", 0]
; For walking on the source code see: DebugDialog_OnDebugEvent
Proc Debugger_OnLoadDll:
    Arguments @BaseAddress
    Local @pErrMode, @IsPacked, @DllSize, @DgbFmtStr, @BasetoShow

    call 'RosMem.FastZeroMem' DgbLogStrDis, MAX_DBG_LOGSTR
    call 'Kernel32.CloseHandle' D$DEBUG_EVENT+DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.hFileDis

    lea eax D@pErrMode | mov D$eax 0
    lea ebx D@IsPacked | mov D$ebx 0
    lea ecx D@DllSize | mov D$ecx 0
    call ScanPEHeader D@BaseAddress, ecx, ebx, eax
    If eax = 0
        call GetDLLNameFromExportError D@BaseAddress, D@DllSize, D@IsPacked, D@pErrMode
        ExitP

    Else
        call GetModuleName D@BaseAddress, D$ModuleList, D$NumModules
    End_If
    mov ebx eax
    mov D@DgbFmtStr Sz_DbgDll_LogStr
    move D@BasetoShow D@BaseAddress
;;
    If eax = 0
        mov ebx {B$ "( ???? )", 0}
    End_If
;;
    ...If D$DebugProcessData.Type = DEBUG_DLL ; Are we debugging a dll ?

        call StrCmp ebx, D$DebugProcessData.DLLName ; See if we are debugging the dll that we opened.
        ..If eax = 0 ; If strings match, we are, in fact debugging our dll.

            ; Here the module is being loaded from the debugged dll. (The original process)
            mov eax D$NumModules | dec eax ; dec eax because it was incremented at ScanPEHeader. So, it is never zero.
            call GetCurModuleArrayPtr D$ModuleList, eax
            mov edi D$DebugProcessData.DllImageBase
            .If D$eax+ME_Base = edi ; If dll name is the same and image base is the same, then the loaded module is ok
                mov edi D$eax+ME_Size | mov D$DebugProcessData.DllImageSize edi
                mov edi D$eax+ME_ExportBase | mov D$DebugProcessData.DllExportBase edi
                mov edi D$eax+ME_ExportSize | mov D$DebugProcessData.DllExportSize edi
                mov edi D$eax+ME_CodeBase | mov D$DebugProcessData.DllCodeBase edi
                mov ecx D$eax+ME_CodeSize | mov D$DebugProcessData.DllCodeSize ecx ; same as AppRVAimageSize
                mov D$DebugBaseOfCode edi
                mov D$DebugCodeSize ecx
                move D@BasetoShow D$DebugProcessData.DllImageBase;D$DebugBaseOfCode ;D$DebugProcessData.DllNewImageBase
                ;call SetupBreakPoints D$DebugBaseOfCode, D$BPAnteroom
                mov D$DebugProcessData.IsRebased &FALSE

                ;mov D$DebugProcessData.IsProcessActivated &TRUE
            .Else ; The codebase was changed. let´ try rebasing it.
                  ; The debugged process might load the DLL at a different address than the DLL in this process,
                  ; but the offset of the function from base of the DLL remains the same in both processes.

                mov D$DebugProcessData.IsRebased &TRUE
                mov edi D$eax+ME_Base | mov D$DebugProcessData.DllNewImageBase edi
                mov edi D$eax+ME_Size | mov D$DebugProcessData.DllNewImageSize edi
                mov edi D$eax+ME_ExportBase | mov D$DebugProcessData.DllNewExportBase edi
                mov edi D$eax+ME_ExportSize | mov D$DebugProcessData.DllNewExportSize edi
                mov edi D$eax+ME_CodeBase | mov D$DebugProcessData.DllNewCodeBase edi
                mov ecx D$eax+ME_CodeSize | mov D$DebugProcessData.DllNewCodeSize ecx ; same as AppRVAimageSize
                mov D$DebugBaseOfCode edi
                mov D$DebugCodeSize ecx
                move D@BasetoShow D$DebugProcessData.DllNewImageBase

                ; Now we get the codebase accordying to the proper original address
;                move D$eax+ME_Base D$DebugProcessData.DllImageBase ;  restore the origina magebase
;                move D$eax+ME_Size D$DebugProcessData.DllNewImageSize ; the new image size of this process.

                ; retrieve the true code base address. Get the offset of the new codebase and uses it on the restoring
;                mov edi D$DebugProcessData.DllNewCodeBase | sub edi D$DebugProcessData.DllNewImageBase
;                add edi D$DebugProcessData.DllImageBase | mov D$eax+ME_CodeBase edi | mov D$DebugBaseOfCode edi

;                mov edi D$DebugProcessData.DllNewCodeSize | mov D$eax+ME_CodeSize edi | mov D$DebugCodeSize edi

                mov D@DgbFmtStr Sz_DbgDllremap_LogStr
                ;move D@BasetoShow D$DebugBaseOfCode ;D$DebugProcessData.DllNewImageBase
            .End_If

            mov edi D$BPAnteroom
            If D$edi+BPAnteroom.AddressDis = 0
                call InitialFillBPAnteroom
            End_If
            ; This is the main routine that enables or disables a breakpoint. On this case, it is used when debugging a dll.
            ;call SetupBreakPoints D@BasetoShow, D$BPAnteroom
            call SetupBreakPoints D$DebugBaseOfCode, D$BPAnteroom
            If eax = 0
                call ReportWinError { B$ "Debugger_OnLoadDll", 0}
            End_If
        ..End_If
    ...End_If

    mov eax ebx
    C_call FormatStr DgbLogStrDis, D@DgbFmtStr, eax, D@BasetoShow, D$DEBUG_EVENT+DEBUG_EVENT_LOAD_DLL_DEBUG_INFO.hFileDis

    call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax

EndP
____________________________________________________________________________________________

Proc FindModuleFileNameNoExport:
    Arguments @BaseAddress, @Output
    Structure @DgbLogStrDis (2*MAX_DBG_LOGSTR), @DgbLogStrDataDis 0
    Uses edi, ecx, edx

    mov edi D@Output
    call 'psapi.GetMappedFileNameA' D$PI.hProcess, D@BaseAddress, edi, 256
    .If eax <> 0
        ; the size returned includes the null terminated byte. So we can bypass it
        dec eax
        mov ecx eax
        add eax edi
        If eax = '\' ; The last byte is a "\" ? Is it a directory ????
            C_call FormatStr D@DgbLogStrDis, {'( ? Err: 1) %s', 0}, eax
            mov eax D@DgbLogStrDis
            ExitP
        End_If
        Do
            If B$eax = '\'
                inc eax
                ExitP
            End_If
            dec eax
            dec ecx
        Loop_Until ecx = 0
        L1:
        C_call FormatStr D@DgbLogStrDis, {'( ? Err: 2) %s', 0}, eax
        mov eax D@DgbLogStrDis
    .Else
        ExitP
    .End_if

EndP
____________________________________________________________________________________________

;;
    This function tries to retrieve the Module name from the error found in ScanPEHeader.
    When it is found it puts the proper message on the LogForm window.
    In some cases, the sring collected is incorrect. It may be related to a directory (somewhow) and not a filename.
    On those special and rare situations, the returned string is a "( ???? )"
    
    Return Value:
        If the function suceeds, it returns TRUE
        If the function fails, it return zero
;;

Proc GetDLLNameFromExportError:
    Arguments @BaseAddress, @DllSize, @IsPacked, @pErrMode
    Local @DllName, @hLib, @PETag, @PEFileStart, @Size, @CodeBase, @CodeSize, @ExportBaseRVA, @ExportSize, @SectionsCount,
          @PEHdr, @PESection, @IsExe, @Name, @IsExport, @DllNameLenght, @DllIsPacked

    ...If D@pErrMode = ERR_MODULE_NO_EXPORT ; Here the ModuleLisst is already filled and NumModules incremented

        mov eax D$NumModules | dec eax ; dec eax because it was incremented at ScanPEHeader. So, it is never zero.
        call GetCurModuleArrayPtr D$ModuleList, eax
        mov eax D$eax+ME_Name
        If D@IsPacked = &TRUE
            C_call FormatStr DgbLogStrDis, {'%s (Packed) mapped at 0x%x (No export on this module)', 0}, eax, D@BaseAddress
            call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax
        Else
            C_call FormatStr DgbLogStrDis, {'%s mapped at 0x%x (No export on this module)', 0}, eax, D@BaseAddress
            call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax
        End_If
;        ExitP
    ...Else_If D@pErrMode = ERR_MODULE_READ_PROCESS

        mov D@IsExe &FALSE
        mov D@IsExport &TRUE ; assume a dll have export functions.

        mov D@Size 0
        mov D@Name 0
        mov D@CodeBase 0
        mov D@CodeSize 0
        mov D@ExportBaseRVA 0
        mov D@ExportSize 0

        ; 1st we scan fo the full name of the file (and it´s path)
        call GetFullFileNameFromProcessEx D$PI.hProcess, D@BaseAddress, lpBaseName
        On eax = 0, ExitP ; exit function on error (Should not happen, but...on a weird error, we can safelly exit)
        ; 2nd we get the address of the only the filename from the path above
        call StrLenProc lpBaseName
        ; the size returned includes the null terminated byte. So we can bypass it
        dec eax
        mov ebx eax ; save the original lenght to compute the size of only the file name
        mov ecx eax ; save it to be used as a counter
        add eax lpBaseName
        Do
            If B$eax = '\'
                inc eax | mov D@DllName eax | jmp L1>
            End_If
            dec eax
            dec ecx
        Loop_Until ecx = 0
        L1:
        sub ebx ecx | mov D@DllNameLenght ebx

        ; 3rd we try to load the module that is unacessible and try to grab all necessary info
        call 'kernel32.LoadLibraryExA' lpBaseName, &NULL, &LOAD_LIBRARY_AS_DATAFILE_EXCLUSIVE
        ..If eax <> 0
            mov D@hLib eax
            call GetFileSizeFromFullName lpBaseName
            On eax = 0-1, xor eax eax ; On error, zero the result
            ; No need to check for the exe/dll integrity using CheckPE_MZ, since loadlibrary already checked the file
            ; all we need is the address of PE tag
            ; even if the above funtion it can´t grab the file size we still can try finding the PE tag.
            call FixMzParagraphsNumber D@hLib, eax, PE_hdr_Process
            mov D@PETag eax

            If W$eax+PeHeader.OptionalHeader.MagicDis <> &IMAGE_NT_OPTIONAL_HDR32_MAGIC
                call 'kernel32.FreeLibrary' D@hLib
                xor eax eax | ExitP
            End_If
            mov esi eax

            Test_If_Not W$esi+PeHeader.FileHeader.CharacteristicsDis &IMAGE_FILE_DLL
                mov D@IsExe &TRUE ; We are loading a exe as a dll ??? Weird, but it may happens.
            Test_End

            move D@Size D$esi+PeHeader.OptionalHeader.SizeOfImageDis
            mov eax D$esi+PeHeader.OptionalHeader.BaseOfCodeDis | add eax D@BaseAddress
            mov D@CodeBase eax
            move D@CodeSize D$esi+PeHeader.OptionalHeader.SizeOfCodeDis
            move D@ExportBaseRVA D$esi+PeHeader.DataDirectory.ExportDis
            move D@ExportSize D$esi+PeHeader.DataDirectory.ExportSizeDis
            movzx eax W$esi+PeHeader.FileHeader.NumberOfSectionsDis
            mov D@SectionsCount eax
            mov edx esi | add edx SizeOf_PeHeader
            sub esi D@hLib
            mov D@PEHdr esi ; RVA of PE signature
            sub edx D@hLib
            mov D@PESection edx ; RVA of PE Section

            ; 1st see if te dll is packed.
            mov D@DllIsPacked &FALSE

            mov eax D@ExportBaseRVA | add eax D@hLib | mov esi D$eax+IMAGE_EXPORT_DIRECTORY.nNameDis
            .If esi <> 0 ; See if the module is packed or not
                call dbg_IsPointtoVirtualSection esi, D@SectionsCount, D@PESection, D@hLib, D@PEHdr
                If eax = &TRUE
                    mov D@DllIsPacked &TRUE
                End_If
            .End_If

            ; A dll without an export table?? Maybe only for resources
            If_Or D@ExportBaseRVA = 0, D@ExportSize = 0;, D@ExportDirName = 0 (When BaseRVa = 0 or Size = 0, it is common that Nme is 0FFFF or 0)
                mov D@IsExport &FALSE
            End_If

            ; everything was ok, we can now free the loaded library
            call 'kernel32.FreeLibrary' D@hLib
        ..End_If

        ; 4th. Create the name buffeer for the module, dispites the necessary info above was grabbed or not.
        call Heap_Operator_NewEx D$ModuleNameHeap, &HEAP_ZERO_MEMORY, D@DllNameLenght, D$SysPageSize
        mov D@Name eax, edi D@Name

        mov esi D@DllName
        While B$esi <> 0 | movsb | End_While
        mov B$edi 0

        ;inc D$NumModules
        call GetCurModuleArrayPtr D$ModuleList, D$NumModules
        mov edi eax
        inc D$NumModules

        move D$edi+ME_Base D@BaseAddress
        move D$edi+ME_Size D@Size
        move D$edi+ME_Name D@Name
        move D$edi+ME_CodeBase D@CodeBase
        move D$edi+ME_CodeSize D@CodeSize
        move D$edi+ME_ExportBase D@ExportBaseRVA
        move D$edi+ME_ExportSize D@ExportSize

        ; 5th finally we can set te proper messages to the debug log
        mov eax D@Name
        .If D@DllIsPacked = &TRUE
            If D@IsExport = &FALSE
                C_call FormatStr DgbLogStrDis, {'%s (Packed) mapped at 0x%x (No export on this module)', 0}, eax, D@BaseAddress
            Else
                C_call FormatStr DgbLogStrDis, {'%s (Packed) mapped at 0x%x', 0}, eax, D@BaseAddress
            End_If
            ;call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax
        .Else
            If D@IsExport = &FALSE
                C_call FormatStr DgbLogStrDis, {'%s mapped at 0x%x (No export on this module)', 0}, eax, D@BaseAddress
            Else
                C_call FormatStr DgbLogStrDis, {'%s mapped at 0x%x', 0}, eax, D@BaseAddress
            End_If
            ;call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax
        .End_If
        call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax
        ;ExitP
    ...Else
        xor eax eax
    ...End_If


EndP
____________________________________________________________________________________________
; see AddressSpaceTree_AddModule  AddressSpaceTree_Build
Proc Debugger_OnUnloadDll:
    Arguments @BaseAddress
    Local @ModuleFound, @Modulename
    Uses esi, edi

    call 'RosMem.FastZeroMem' DgbLogStrDis, MAX_DBG_LOGSTR
    call GetModuleName D@BaseAddress, D$ModuleList, D$NumModules
    ...If eax = 0
        ; Probably the base address was remapped on the process while it was unmapped. Try finding it the hard way.
        lea eax D@ModuleFound | mov D$eax 0
        call GetModuleNameBruteForce D@BaseAddress, eax
        If eax = 0
            C_call FormatStr DgbLogStrDis, {'Something weird was unmapped at 0x%x' 0}, D@BaseAddress
            call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax
            ExitP
        Else
            C_call FormatStr DgbLogStrDis, {'%s unmapped (BruteForced)' 0}, eax
            call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax
            mov edi D@ModuleFound
            ; Do cleanup work. Free the memory for the module name.
            call Heap_Operator_DeleteEx D$ModuleNameHeap, 0, D$edi+ME_Name
            call DeleteModuleFromBaseAddress edi, D$ModuleList, D$NumModules
            dec D$NumModules
            ExitP
        End_If
    ...End_If
    mov D@Modulename eax
    C_call FormatStr DgbLogStrDis, {'%s unmapped' 0}, eax
    call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, DgbLogStrDis, eax

    ; Now that we unmapped we need to check if we unmapped our dll that was being debuged.
    ; If we just unmapped it, we see if the image base was unmapped and reset the flag.
    ; On this wa we can dsplay the address properly on the address tab
    ..If D$DebugProcessData.Type = DEBUG_DLL ; Are we debugging a dll ?

        call StrCmp D@Modulename, D$DebugProcessData.DLLName ; See if we are debugging the dll that we opened.
        .If eax = 0 ; If strings match, we are, in fact debugging our dll.
            If D$DebugProcessData.IsRebased = &TRUE ; If it was already remmaped, reset the flag
                mov D$DebugProcessData.IsRebased &FALSE
            End_If
        .End_If
    ..End_If

    ; Search module in table. See GetCurModuleArrayPtr

    call GetModuleFromBaseAddress D@BaseAddress, D$ModuleList, D$NumModules
    On eax = 0, ExitP
    mov edi eax

    ; Do cleanup work. Free the memory for the module name.
    call Heap_Operator_DeleteEx D$ModuleNameHeap, 0, D$edi+ME_Name

    call DeleteModuleFromBaseAddress edi, D$ModuleList, D$NumModules
    dec D$NumModules

EndP
____________________________________________________________________________________________

Proc GetModuleNameBruteForce:
    Arguments @BaseAddress, @ModuleFound
    Uses esi, ebx, edx

    mov esi D$ModuleList
    mov ebx D$AddressLowerBound
    .While ebx < D$AddressUpperBound
        call VirtualQuery ebx
        ..If eax = &TRUE
            call GetModuleFromAddress D$MemoryInformation@BaseAddress, D$ModuleList, D$NumModules
            .If eax <> 0
                mov edx D$eax+ME_Size | add edx D$MemoryInformation@BaseAddress
                If_And D@BaseAddress >= eax, D@BaseAddress <= edx
                    mov edx D@ModuleFound | mov D$edx eax
                    mov eax D$eax+ME_Name
                    ExitP
                End_If
            .End_If
        ..Else_If eax = 0-1
            xor eax eax | ExitP
        ..EndIf
        add ebx D$MemoryInformation@RegionSize
    .End_While

EndP
____________________________________________________________________________________________

; Search in the modulelist for the name of the module starting at the base address passed.

Proc GetModuleName:
    Arguments @Baseaddress, @pModuleList, @NumModules
    Uses edx, ecx

    mov eax D@Baseaddress
    mov edx D@pModuleList, ecx D@NumModules
    While ecx > 0 ; bugfix V2.0b
        If eax = D$edx+ME_Base
            mov eax D$edx+ME_Name
            ExitP
        End_If
        add edx SizeOf_ModuleEntry
        dec ecx
    End_While
    mov eax 0
EndP

; Search the module list for a given base address
Proc GetModuleFromBaseAddress:
    Arguments @Baseaddress, @pModuleList, @NumModules
    Uses edx, ecx

    mov eax D@Baseaddress
    mov edx D@pModuleList, ecx D@NumModules
    While ecx > 0 ; bugfix V2.0b
        If eax = D$edx+ME_Base
            mov eax edx
            ExitP
        End_If
        add edx SizeOf_ModuleEntry
        dec ecx
    End_While
    mov eax 0

EndP

; Search the module list for a given address
Proc GetModuleFromAddress:
    Arguments @Baseaddress, @pModuleList, @NumModules
    Uses edx, ecx, ebx

    mov eax D@Baseaddress
    mov edx D@pModuleList, ecx D@NumModules
    While ecx > 0 ; bugfix V2.0b
        mov ebx D$edx+ME_Size | add ebx D$edx+ME_Base
        If_And eax >= D$edx+ME_Base, eax <= ebx
            mov eax edx
            ExitP
        End_If
        add edx SizeOf_ModuleEntry
        dec ecx
    End_While
    mov eax 0

EndP

Proc DeleteModuleFromBaseAddress:
    Arguments @pModuletoDelete, @pModuleList, @NumModules
    Uses ecx, edi, esi, ebx

    mov edi D@pModuletoDelete

    ; get the last module stucture
    dec D@NumModules ; decrease it to we get the start of the last structure
    call GetCurModuleArrayPtr D@pModuleList, D@NumModules
    mov ebx eax ; save the location for deletion
    ; Copy the last stucture to the location of the one that was found and delete the last one
    mov esi eax
    mov ecx SizeOf_ModuleEntry
    rep movsb
    call 'RosMem.FastZeroMem' ebx, SizeOf_ModuleEntry

EndP


;;
GetModuleName:
    mov eax D$esp+4
    mov edx D$ModuleList, ecx D$NumModules
    While ecx > 0 ; bugfix V2.0b
        cmp eax D$edx+ME_Base | je L1>
        add edx SizeOf_ModuleEntry
        dec ecx
    EndWhile
    mov eax 0
ret 4
L1: mov eax D$edx+ME_Name
ret 4

;;
____________________________________________________________________________________________
; Search in the modulelist for the name of the module starting at a given address.
Proc GetModuleNameFromAddress:
    Arguments @Address, @pModuleList, @NumModules
    Uses edx, ecx, ebx

    mov eax D@Address
    mov edx D@pModuleList, ecx D@NumModules
    While ecx > 0 ; bugfix V2.0b
        mov ebx D$edx+ME_Size | add ebx D$edx+ME_Base
        If_And eax >= D$edx+ME_Base, eax <= ebx
            mov eax D$edx+ME_Name
            ExitP
        End_If
        add edx SizeOf_ModuleEntry
        dec ecx
    End_While
    mov eax 0
EndP
____________________________________________________________________________________________

; Find the module from an address. Return the address of the module entry or NULL if no
; module with such a code address could be found.

Proc IsModuleCode:
    Arguments @CodeAddress
    Uses esi

        mov ecx 0, esi D$ModuleList
        While ecx < D$NumModules
            mov eax D$esi+ME_CodeBase, edx D$esi+ME_CodeSize
            .If D@CodeAddress >= eax
                add edx eax
                If D@CodeAddress < edx
                    mov eax esi
                    ExitP
                EndIf
            .EndIf
            add esi SizeOf_ModuleEntry | inc ecx
        End_While
        mov eax 0
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

;[DebugLogString: B$ ? #256]

; Log debug output to a file. First read debug string from process memory of the debuggee
; to a local buffer (the stack).

;[DebugStringBuffer: ?]
[DebugLogFile: ? DebugLogFileName: B$ ? #&MAXPATH]
[DebugLogName: '_dbg.log' 0]

;;
    http://unixwiz.net/techtips/outputdebugstring.html

    Passing of data between the application and the debugger is done via a 4kbyte chunk of
    shared memory, with a Mutex and two Event objects protecting access to it.
    These are the four kernel objects involved: 

    object name         object type
    ______________________________________________
    DBWinMutex          Mutex
    DBWIN_BUFFER        Section (shared memory)
    DBWIN_BUFFER_READY  Event
    DBWIN_DATA_READY    Event 

    The mutex generally remains on the system all the time, but the other three are only present
    if a debugger is around to accept the messages. Indeed - if a debugger finds the last three
    objects already exist, it will refuse to run.

    The DBWIN_BUFFER, when present, is organized like this structure. The process ID shows where
    the message came from, and string data fills out the remainder of the 4k.
    By convention, a NUL byte is always included at the end of the message.

    struct dbwin_buffer {
        DWORD   dwProcessId;
        char    data[4096-sizeof(DWORD)];
    };


    A pseudo-code reverse engeneered of OutputDebugString in ReactOS shows that may uses mutex or not.
    Therefore, it seems to have a limit of the debug sting that is defined as 4096 as in dbwin_buffer structure.

    http://unixwiz.net/techtips/OutputDebugString.txt

;;

Proc HandleDebugString:
    Local @Size
    Structure @DgbLogStrDis 4096, @DgbLogStrDataDis 0

    call 'RosMem.FastZeroMem' D@DgbLogStrDis, 4096

    ; Get mem on the stack for the debug string
    movzx eax W$DEBUG_EVENT+DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.nDebugStringiLengthDis
    mov D@Size eax

    lea edi D@DgbLogStrDataDis
    call strcpy edi, {'Debug String: "', 0}
    add edi 15 ; Bypass 'Debug String: "'
    ; Read the debug string
    call ReadProcessMem D$DEBUG_EVENT+DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.lpDebugStringDataDis, edi, D@Size
    add edi D@Size | sub edi 2 | mov B$edi '"' ;  Replace last char 0A with '"' to close the sentence "Debug String "xxxxx"
    add D@Size (15-1) ; Extra 15 bytes that forms the sentence "debug string"
    ;call ReadProcessMem D$DEBUG_EVENT+DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.lpDebugStringDataDis, D$DebugStringBuffer, D@Size
    ;mov edi D@DgbLogStrDis
    ;call StrLenProc edi

    ;C_call FormatStr D@DgbLogStrDis, {'Debug String: "%s" mapped at 0x%x', 0}, eax, D@BaseAddress
    ;call 'User32.PostMessageA' D$LogForm_Handle, WM_LOG, D@DgbLogStrDis, D@Size
    ;call 'User32.PostMessageA' D$DebugDialogHandle, WM_LOG, D@DgbLogStrDis, D@Size
    call 'User32.SendMessageA' D$LogForm_Handle, WM_LOG, D@DgbLogStrDis, D@Size

    .If D$DebugLogFile = 0

        ; Build logfile-name of the form "[Path]\[AppName]_dbg.log"
        mov edi DebugLogFileName
        C_call FormatStr edi, {'%s' 0}, DebuggeeExe
        add edi eax
        While B$edi <> '.'
            dec edi
        End_While
        C_call FormatStr edi, {'%s' 0}, DebugLogName
        add edi eax
        mov B$edi 0

        call 'Kernel32.CreateFileA' DebugLogFileName, &GENERIC_WRITE, &FILE_SHARE_READ, 0,
            &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        If eax = &INVALID_HANDLE_VALUE
            call ReportWinError {'HandleDebugString: CreateFile' 0}
            ExitP
        Else
            mov D$DebugLogFile eax
        EndIf

    .EndIf

    mov eax D@Size
    mov edi D@DgbLogStrDis | add edi D@Size | mov W$edi CRLF | add eax 2
    call 'KERNEL32.WriteFile' D$DebugLogFile, D@DgbLogStrDis, eax, BytesTransfered, 0


EndP
;;
Proc HandleDebugString_Old:
    Local @Size

    ;movzx eax W$DEBUG_EVENT+DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.fUnicodeDis
    ;mov eax D$DEBUG_EVENT+DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.lpDebugStringDataDis
    ; Get mem on the stack for the debug string
    movzx eax W$DEBUG_EVENT+DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.nDebugStringiLengthDis;ODS.StringLen
    Align_on 4 eax
    mov D@Size eax
    sub esp D@Size
    mov D$DebugStringBuffer esp

  ; Read the debug string
    ;call ReadProcessMem D$ODS.DebugString, D$DebugStringBuffer, D@Size
    call ReadProcessMem D$DEBUG_EVENT+DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.lpDebugStringDataDis, D$DebugStringBuffer, D@Size

    ;call 'User32.SendMessageA' D$DebugDialogHandle, WM_LOG, D$DebugStringBuffer, D@Size
    call 'User32.PostMessageA' D$LogForm_Handle, WM_LOG, D$DebugStringBuffer, D@Size

    .If D$DebugLogFile = 0

        ; Build logfile-name of the form "[Path]\[AppName]_dbg.log"
        mov edi DebugLogFileName
        C_call FormatStr edi, {'%s' 0}, DebuggeeExe
        add edi eax
        While B$edi <> '.'
            dec edi
        End_While
        C_call FormatStr edi, {'%s' 0}, DebugLogName
        add edi eax
        mov B$edi 0

        call 'Kernel32.CreateFileA' DebugLogFileName, &GENERIC_WRITE, &FILE_SHARE_READ, 0,
            &CREATE_ALWAYS, &FILE_ATTRIBUTE_NORMAL, 0

        If eax = &INVALID_HANDLE_VALUE
            call ReportWinError {'HandleDebugString: CreateFile' 0}
            jmp L9>
        Else
            mov D$DebugLogFile eax
        EndIf

    .EndIf

    movzx eax W$DEBUG_EVENT+DEBUG_EVENT_OUTPUT_DEBUG_STRING_INFO.nDebugStringiLengthDis | dec eax;ODS.StringLen | dec eax
    call 'KERNEL32.WriteFile' D$DebugLogFile, D$DebugStringBuffer, eax, BytesTransfered, 0

    mov W$esp 0A0D
    call 'KERNEL32.WriteFile' D$DebugLogFile, D$DebugStringBuffer, 2, BytesTransfered, 0

  ; Free mem from the stack
L9: add esp D@Size
EndP
;;
____________________________________________________________________________________________

; Called when an exception in the debuggee occurs. Check if the exception is continueable.
; Returns 1 if the debuggee must be terminated, otherwise 0.
; see also DebugDialog_OnDebugEvent
Proc EncounterException:
    Arguments @BaseOfCode, @CodeSize
    Local @Exit, @Dummy

    mov D$ExceptionFlags E_HAPPENED

    ..If D@BaseOfCode <> 0
        call IsProcessCode D@BaseOfCode, D@CodeSize, D$C.regEip
        .If eax = 0
            call ScanStackForCodePointer D@BaseOfCode, D@CodeSize, D$C.regEsp
            If_And eax = 0, D$SourcePosCodeAddress <> 0
                dec D$SourcePosCodeAddress
            End_If
        .End_If
    ..End_If

    ...If D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.dwFirstChanceDis <> 0

        ..If D@BaseOfCode = 0
            mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
            mov D$ContinueMode CONTINUE_RUN
            mov eax 0
        ..Else
            ; check if exception handler is inside debuggee code
            lea eax D@Dummy | mov D@Dummy 0 | call ReadProcessMem D$FS.Linear, eax, 4
            mov ecx D@Dummy | add ecx 4 ; get handler address
            lea eax D@Dummy | mov D@Dummy 0 | call ReadProcessMem ecx, eax, 4
            call IsProcessCode D@BaseOfCode, D@CodeSize, D@Dummy ; handler address
            .If eax = 1
                call IsProcessCode D@BaseOfCode, D@CodeSize, D$C.regEip
                On eax = 0, or D$ExceptionFlags E_OUTSIDE

                call SignalDebugEvent
                If D$TerminateDebuggee = &TRUE
                    mov eax 1
                Else
                    mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
                    mov D$ContinueMode CONTINUE_RUN
                    mov eax 0
                EndIf
            .Else
                mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
                mov D$ContinueMode CONTINUE_RUN
                mov eax 0
            .EndIf
        ..End_If

    ...Else
        or D$ExceptionFlags E_MUSTEXIT
        call SignalDebugEvent
        mov eax 1
    ...EndIf

    mov D$ExceptionFlags 0 ; was only needed for UI, clear it so we could proceed (SEH)

;;
  ; Check if Exception happened outside of application code. 
    call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D$C.regEip
    .If eax = 1
        
        On D$E.FirstChance = 0, 
            or D$ExceptionFlags E_MUSTEXIT
            
        call SignalDebugEvent
        
        If D$TerminateDebuggee = &TRUE
            mov eax 1
        Else
            mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
            mov D$ContinueMode CONTINUE_RUN
            mov eax 0
        EndIf
        
    .Else
    
        or D$ExceptionFlags E_OUTSIDE
      
      ; Give the module the possibility to handle the exception by its per-thread
      ; exception handler. If it does not catch the exception, show the exception
      ; dialog.
      
        If D$E.FirstChance = 0
            or D$ExceptionFlags E_MUSTEXIT            
            
            call ScanStackForCodePointer D$DebugBaseOfCode, D$DebugCodeSize,  D$C.regEsp
            dec eax
            mov D$SourcePosCodeAddress eax
            
            call SignalDebugEvent
          
            mov eax 1
        Else
            mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
            mov D$ContinueMode CONTINUE_RUN
            mov eax 0
        EndIf
    
    .EndIf    
    
    mov D$ExceptionFlags 0 ; was only needed for UI, clear it so we could proceed (SEH)
                
    .If D$E.FirstChance = 0
        mov D$ExceptionFlags E_HAPPENED

        call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D$C.regEIP
        If eax = &FALSE
            
        EndIf

        ; Exception is continueable?
        If D$E.ExceptionFlags <> 0
            or D$ExceptionFlags E_MUSTEXIT
            mov D@Exit 1
        EndIf

        ;call SignalDebugEvent

        mov eax D@Exit
        On D$TerminateDebuggee = &TRUE, mov eax 1
    .Else
    
      ; try to pass to apps exception handler
        mov D$ContinueStatus &DBG_EXCEPTION_NOT_HANDLED
        mov eax 0
        
    .EndIf
;;
EndP
____________________________________________________________________________________________

; Bring all visible windows of the debuggee to front.

[ActiveWindow: ?]

Proc EnumThreadWindowProc:
    Arguments @Handle, @Param

    call 'USER32.SetWindowPos' D$hwnd, D@Handle, 0, 0, 0, 0, &SWP_NOMOVE+&SWP_NOSIZE

    mov eax D@Handle
    If eax = D$ActiveWindow
        call 'USER32.SetForegroundWindow' D$ActiveWindow
    EndIf

    mov eax 1
EndP

ShowProcessWindows:
    mov ebx 0, esi D$ThreadIDHandleTable
    While ebx < D$NumThreads
        call 'User32.EnumThreadWindows' D$esi+ebx*8, EnumThreadWindowProc, 0
        inc ebx
    EndWhile
ret
____________________________________________________________________________________________

; close all non-child windows. Used when the debuggee is terminated while running.

Proc EnumCloseWindowsProc:
    Arguments @Handle, @Param

    call 'User32.PostMessageA' D@Handle, &WM_CLOSE, 0, 0
    mov eax &TRUE
EndP

CloseMainWindows:
    mov ebx 0, esi D$ThreadIDHandleTable
    While ebx < D$NumThreads
        call 'User32.EnumThreadWindows' D$esi+ebx*8, EnumCloseWindowsProc, 0
        inc ebx
    EndWhile
ret
____________________________________________________________________________________________

; STACK SCANNER
____________________________________________________________________________________________

;;
    The stack has the following structure (for each stackframe)
    
        Parameter n
        [...]
        Parameter 2
    ___ Parameter 1
        Return Address
        Saved EBP (if proc has stackframe)
        Local data
    
  * To differ between procedures we first have to find the return addresses. Then the number
    of parameters must be estimated. Given that information a complete call-stack can
    be generated.
    
  * First, all return addresses on the stack are collected bottom-up. The proc is tried to
    be identified. 
    
  * Note that life would be much easier if every proc was guaranteed to have a stack frame.
    We could just take the saved ebp as a pointer to the callers stack frame. However,
    in Assembly you are free to use ebp for whatever you like, and creating a stack frame is
    optional. Therefore the return addresses are used to identify called procedures.
    
  * Misinterpretation problems arise from "stack pollution", that is procs reserving space on 
    the stack without freeing it (e.g. sub esp 0100). Return addresses from former execution
    pollute the stack in this region. This has been partially solved by validating the stack 
    frames top-down in the third pass (ignore the locals region). The remaining problems
    are due to dynamic stack allocation (e.g. sub esp ecx). 
;;

[BufferOverrun:
"Detected buffer-overrun. 
Contact RosAsm dev team if this happens regularly." 0]

[CallStackDesc: ? CallStackEntries: ? FirstCallStackEntry: ?]
[MAX_CALLSTACK_ENTRIES 512]
[CSE_Address 0      ; points at the ret-address+4 in the LOCAL stack copy
 CSE_ProcAddress 4  ; (estimated) address of the called function
 CSE_ProcName 8     ; points at the name (+estimation info) of the funtion
 CSE_NumParams 12   ; number of paramters (after ret-address)
 CSE_NumLocals 16   ; number of locals (before ret-address)
 CSE_Rating 20      ; probability of correctness
 CSE_Flags 21       ; flags
 CSE_Next 24        ; address of next call-stack entry
 SizeOf_CSE 32]     ; align on cache line

[CSEF_HAS_STACKFRAME 01  ; CSE has a stackframe (push ebp | ...)
 CSEF_FUZZY_ADDRESS  02  ; address of proc is not exact
 CSEF_EXTERNAL       04] ; proc is outside debugged module


[ProcNameHeap: ?]

Proc GenerateCallStack:
    Local @Pointer, @CurrentSize, @ProcAddr, @LastCodeAddr, @Exact, @NumLocals

    call DestroyCallStack

    ;On D$ProcNameHeap = 0, call 'KERNEL32.HeapCreate' &HEAP_NO_SERIALIZE, 4096, 0 ;0, 01000, 0 ; create a growable heap
    If D$ProcNameHeap = 0
        ;call HeapCreateEx &HEAP_NO_SERIALIZE, 4096, 0, D$SysPageSize
        call HeapCreateEx 0, 4096, 0, D$SysPageSize
    End_If
    mov D$ProcNameHeap eax

    call ReadApplicationStack D$C.regEsp
    On eax = &FALSE, ExitP
    On D$CallStackDesc = 0, VirtualAlloc CallStackDesc, (04000*18)

    call CallStack.Pass1
    If D$CallStackEntries = 0
        mov eax 0
        ExitP
    EndIf
    call CallStack.Pass2
    call CallStack.Pass3
;;
    mov esi D$StackBuffer, edi D$CallStackDesc, D$CallStackEntries 0
    mov D$NextStackFrame 0, D@NumLocals 0
    move D@CurrentSize D$StackSize
    move D@LastCodeAddr D$C.regEip
    .While D@CurrentSize > 0
        lodsd | mov D@Pointer eax

        call IsReturnAddress eax, edi
        .If eax = &TRUE

            mov D$edi+CSE_Address esi

            call 'Kernel32.HeapAlloc' D$ProcNameHeap, 0, 040
            mov D$edi+CSE_ProcName eax

            ; Check if the exact address of the procedure called could be estimated.
            ; If not (call reg / call mem / ...) use the code address which is
            ; somewhere in the proc to find the procedure address and label.
            mov eax D$edi+CSE_ProcAddress
            If eax <> 0
                mov D@ProcAddr eax
                mov D@Exact &TRUE
            Else
                move D@ProcAddr D@LastCodeAddr
                mov D@Exact &FALSE
            EndIf
            call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D@ProcAddr
            If eax = &TRUE
                call ScanLabelListForCodeLabel D@ProcAddr, D@Exact
            Else
                call ScanExportTableForCodeLabel D@ProcAddr, D@Exact
            EndIf

            mov D$edi+CSE_ProcAddress eax

            push esi edi
                mov esi LabelName, edi D$edi+CSE_ProcName
                Do | movsb | Loop_Until B$esi-1 = 0
            pop edi esi

            call CountParametersAndScanStackFrame edi

            ; Local data - Begin to count local data from the last return address.
            ; Therefore NumLocals start with a negative number if parameters were
            ; passed to the last function.
            mov eax D@NumLocals
            On eax l 0, mov eax 0
            mov D$edi+CSE_NumLocals eax

            mov eax D$edi+CSE_NumParams | neg eax
            mov D@NumLocals eax

            move D@LastCodeAddr D@Pointer
            add edi SizeOf_CSE
            inc D$CallStackEntries
        .Else
            inc D@NumLocals
        .EndIf
        sub D@CurrentSize 4
    .EndWhile
;;
    mov eax &TRUE
EndP

____________________________________________________________________________________________

; First pass - search all possible return addresses

Proc Callstack.Pass1:
    Local @CurrentSize, @LastCodeAddr, @Pointer, @ProcAddr, @Exact
    Uses esi, edi

    mov esi D$StackBuffer, edi D$CallStackDesc, D$CallStackEntries 0
    move D@CurrentSize D$StackSize
    move D@LastCodeAddr D$C.regEip
    .While D@CurrentSize > 0
        lodsd | mov D@Pointer eax

        call IsReturnAddress eax, edi
        .If eax = &TRUE
            mov D$edi+CSE_Address esi

            ;call 'Kernel32.HeapAlloc' D$ProcNameHeap, &HEAP_ZERO_MEMORY, (64*18);512;040
            call Heap_Operator_NewEx D$ProcNameHeap, &HEAP_ZERO_MEMORY, (64*18), D$SysPageSize
            ;call Heap_Operator_New (64*18)
            mov D$edi+CSE_ProcName eax

          ; Check if the exact address of the procedure called could be estimated.
          ; If not (call reg / ...) use the code address which is
          ; somewhere in the proc to find the procedure address and label.
            mov B$edi+CSE_Flags 0
            mov eax D$edi+CSE_ProcAddress
            If eax <> 0
                mov D@ProcAddr eax
                mov D@Exact &TRUE
            Else
                move D@ProcAddr D@LastCodeAddr
                or B$edi+CSE_Flags CSEF_FUZZY_ADDRESS
                mov D@Exact &FALSE
            EndIf
            call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D@ProcAddr
            If eax = &TRUE
                call ScanLabelListForCodeLabel D@ProcAddr, D@Exact
            Else
                call ScanExportTableForCodeLabel D@ProcAddr, D@Exact
                or B$edi+CSE_Flags CSEF_EXTERNAL
            EndIf

            mov D$edi+CSE_ProcAddress eax

            If B$LabelName <> 0
                push eax
                    call StrLenProc LabelName | on eax >= 512, mov eax 511
                    call strcpyEx D$edi+CSE_ProcName, LabelName, eax
                pop eax
            Else
                mov D$edi+CSE_ProcName 0
            End_If
            ;push esi edi
             ;   mov esi LabelName, edi D$edi+CSE_ProcName
              ;  Do
               ;     movsb
                ;    dec ecx
                ;Loop_Until ecx = 0
            ;pop edi esi

            move D@LastCodeAddr D@Pointer
            add edi SizeOf_CSE
            inc D$CallStackEntries

          ; Prevent buffer overrun
            If D$CallStackEntries >= MAX_CALLSTACK_ENTRIES
                call 'User32.MessageBoxA' D$DebugDialogHandle, BufferOverrun, {'Callstack generation' 0}, &MB_OK+&MB_ICONEXCLAMATION
                mov eax &FALSE
                ExitP
            EndIf
        .EndIf
        sub D@CurrentSize 4
    .End_While
    mov eax &TRUE
EndP
____________________________________________________________________________________________

; Second pass - scan stack frames and rate procs

[CSRatingFailed: ?] ; indicate that rating wasn't successful -> don't rely on the ratings

Proc CallStack.Pass2:
    Uses esi, edi, ebx

    mov esi D$StackBuffer, edi D$CallStackDesc, ecx D$CallStackEntries
    mov edx esi | sub edx D$C.regEsp

    mov ebx D$C.regEbp | add ebx edx

L0:     lea eax D$ebx+8
        While eax <> D$edi
            add edi SizeOf_CSE
            dec ecx | jz L9>
        EndWhile
        inc B$edi+CSE_Rating

      ; Check for invalid stackframe ptrs - a stackframe ptr is invalid if it points
      ; to somewhere outside the used stack.
        If ebx < esi
            mov D$CSRatingFailed 1
            ExitP
        EndIf
        mov eax esi | add eax D$StackSize
        If ebx >= eax
            mov D$CSRatingFailed 1
            ExitP
        EndIf

        mov ebx D$ebx | add ebx edx ; next stackframe ptr
    jmp L0<

L9: mov D$CSRatingFailed 0
EndP
____________________________________________________________________________________________

; Third pass - validate, count params, locals, ...

Proc CallStack.Pass3:
    Local @LowerBound, @UpperBound, @LastCSE
    Uses esi, edi, ebx

  ; Traverse the callstack top-down, begin with the last entry
    mov ebx D$CallStackEntries | dec ebx
    imul edi ebx SizeOf_CSE | add edi D$CallStackDesc
    mov esi D$edi+CSE_address ; esi -> params of the root function

    mov eax D$StackBuffer | mov D@LowerBound eax
    add eax D$StackSize   | mov D@UpperBound eax

    mov D@LastCSE 0

    .While ebx ge 0

      ; UpperBound - address of last CSE
      ; LowerBound - limit given through esp

      ; The bounds are stored to detect collisions of CSEs. Collision (or overlaps)
      ; occur if some interpretation mechanism has failed (locals, params) or if the
      ; whole CSE is a ghost entry (old ret addresses in non-overwritten locals)

        mov eax D$edi+CSE_Address
        .If D@UpperBound < eax
            mov edx D@LastCSE | mov al B$edx+CSE_Rating
            If al g B$edi+CSE_Rating
              ; overlapped by local data of a higher rated proc, considered
              ; as stack pollution -> throw away
                jmp L8>
            Else_If al = B$edi+CSE_Rating
              ; overlapped by local data of equally rated proc, the assumption
              ; about the size of local data of the last CSE might be wrong, correct it (set to zero)
                mov D$edx+CSE_NumLocals 0
                ;mov eax D$edi+CSE_Address | sub eax D@Upperbound | shr eax 2
                ;sub D$edx+CSE_NumLocals eax
                move D@UpperBound D$edi+CSE_Address
            Else
              ; overlapped by local data of lower rated proc (the interpretation failed)
              ; throw away the last(!) CSE
                mov eax D$edx+CSE_Next | mov D@LastCSE eax
            End_If
        .End_If

        call CountParametersAndScanStackFrame edi

      ; Check parameters size - the parameter sizes should be ok in most cases.
      ; however, if some ill "ret 086D" statement is somewhere in the source (e.g. MyPNN)
      ; the boundaries are exceeded easily. If this occurs in other apps too, better merge
      ; the validation of param and local sizes and do something more intelligent ...
        mov eax D$edi+CSE_NumParams | shl eax 2
        add eax D$edi+CSE_Address
        If D@UpperBound < eax
          ; MyPNN phenomenon, consider cleaner way to deal with this issue
            mov D$edi+CSE_NumParams 0
        End_If


        If D$CSRatingFailed = 0
          ; proc with stackframes should be rated if rating was successful
            test B$edi+CSE_Flags CSEF_HAS_STACKFRAME | jz L0>
                cmp B$edi+CSE_Rating 1 | jl L8>
L0:     End_If

      ; recompute upper bound
        mov eax D$edi+CSE_NumLocals | inc eax | shl eax 2 ; SizeOf(Locals+RetAddress)
        mov edx D$edi+CSE_Address | sub edx eax
        mov D@UpperBound edx

      ; if we just entered the proc the stack might not be fully filled - either we
      ; must handle it or the one who displays it. Otherwise we'd read below the stackptr.
        If edx < D@LowerBound
            sub edx D@Lowerbound ; edx is negative!
            neg edx | shr edx 2
            sub D$edi+CSE_NumLocals edx ; subtract from localnumber
        End_If

        move D$edi+CSE_Next D@LastCSE
        mov D@LastCSE edi

L8:     sub edi SizeOf_CSE
        dec ebx | js L9>
    .End_While

L9: move D$FirstCallStackEntry D@LastCSE

EndP
____________________________________________________________________________________________
____________________________________________________________________________________________

Proc DestroyCallStack:
    mov D$StackSize 0
    VirtualFree D$StackBuffer
    mov D$CallStackEntries 0, D$FirstCallStackEntry 0
    VirtualFree D$CallStackDesc
    If D$ProcNameHeap <> 0
        call 'KERNEL32.HeapDestroy' D$ProcNameHeap
        mov D$ProcNameHeap 0
    EndIf
EndP
____________________________________________________________________________________________

; Copy the complete stack from the address space of the debuggee into our address space.

[StackBuffer: ? StackSize: ?]

Proc ReadApplicationStack:
    Arguments @StackPointer

        ; Get size of current stack
        mov eax D@StackPointer
        On eax = 0, ExitP
        and eax 0_FFFF_FFFC ; dword align
        mov D@StackPointer eax
        call IsProcessMemory D@StackPointer
        On eax = 0, ExitP
        mov D$StackSize eax
        ; Copy stack, starting at the stackpointer
        VirtualAlloc StackBuffer, eax
        call ReadProcessMem D@StackPointer, D$StackBuffer, D$StackSize
EndP
____________________________________________________________________________________________

; Check if the given value is an return address.
; Returns:
;   eax - TRUE / FALSE

; NextStackFrame is the so called ghost-return-address prevention. In external modules
; all procs are assumed to have a stackframe (which is not true). If those procs use the
; stack for local data (sub esp imm) but do not clear/use it, old return addresses might
; still be there (this happens _very_ often).
[NextStackFrame: ?]

Proc IsReturnAddress:
    Arguments @Address, @CSE

    mov eax D@CSE
    mov D$eax+CSE_ProcAddress 0

    call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D@Address
    ..If eax = 1
        call IsReturnAddressInSource D@Address, D@CSE
    ..Else
        mov eax 0
        call IsModuleCode D@Address
        .If eax <> 0
            If edi > D$NextStackFrame
                call IsReturnAddressInModule D@Address, D@CSE, eax
            Else
                mov eax 0
            EndIf
        .EndIf
    ..EndIf
EndP
____________________________________________________________________________________________

; Determine if the given address is a return address which was pushed on the stack by a
; call. We look into process memory if the instruction preceding the address is call.
; There are some different encodings of calls:
;   call Label      >> E8 ## ## ## ##
;   call D$Label    >> FF 15 ## ## ## ##
;   call reg32      >> FF D#
;   call D$reg32    >> FF 1# (eax/ecx/edx/ebx/edi/esi)
;   call D$esp      >> FF 14 24
;   call D$ebp      >> FF 55 00
; And with address arithmetic there are more variants. So we first check for the standard
; call opcode E8. If it is different we scan backward for the opcode FF and try a decode.
; If the decoded instruction is a call and equals the number of bytes searched back
; IsReturnAddress returns 1 otherwise 0 (in eax).

[PrecedingCode: B$ ? #96 PrecedingCodeSize: D$ ?]

Proc IsReturnAddressInModule:
    Arguments @Address, @CSE, @ModuleBase
    Uses ebx, edi

        mov eax D@ModuleBase, ebx D@Address, ecx ebx
        sub ecx D$eax+ME_CodeBase
        If ecx > 8
            mov ecx 8
        ElseIf ecx < 2
            ; the minimum length of a call is two bytes (FF ##)
            jmp L9>>
        Else
            mov D$PrecedingCode 0, D$PrecedingCode+4 0
        EndIf
        mov D$PrecedingCodeSize ecx | sub ebx ecx
        call ReadProcessMem ebx, PrecedingCode, ecx

        mov edi PrecedingCode, ecx D$PrecedingCodeSize
        If B$edi+ecx-5 = 0E8 ; call imm
            mov eax &TRUE
            ; convert the immediate to a virtual address
            mov edx D$edi+ecx-4
            add edx D@Address
            mov ecx D@CSE
            mov D$ecx+CSE_ProcAddress edx
            ExitP
        ElseIf W$edi+ecx-6 = 015FF ; call D$Label
            mov eax D$edi+ecx-4
            call ReadProcessMem eax, PrecedingCode, 4
            mov edx D$PrecedingCode
            mov ecx D@CSE
            mov D$ecx+CSE_ProcAddress edx
            mov eax &TRUE
            ExitP
        EndIf
L1:     mov al 0FF
            repne scasb | jne L9>
            dec edi

            call InstructionDecode edi
            mov eax D$NextInstructionPtr
            .If D$eax = 'call'
                inc ecx
                If ecx = D$InstructionLength
                    mov eax &TRUE
                    ExitP
                EndIf
                dec ecx
            .EndIf
            inc edi
        jmp L1<

L9:     mov eax &FALSE
EndP
____________________________________________________________________________________________

; Simplified version of IsReturnAddress. Here we use the IpTable to find the preceding
; instruction. This is much safer as no data can be mistaken as an opcode.

Proc IsReturnAddressInSource:
    Arguments @Address, @CSE
    Uses ebx, edi, esi

        ; >>> TODO
        ; Most mistaken return addresses are indeed function addresses which are pushed
        ; on the stack (callback functions, window procs). We should take care of the
        ; following case:
        ; Main:
        ; [...]
        ; call 'kernel32.ExitProcess'
        ; Proc MainWindowProc:
        ; [...]
        ; EndP
        ; The address of the window proc is saved on the stack multiple times, therefore
        ; messing up the callstack because it is preceded by call and mistaken as a return
        ; address.
        ; <<< TODO

        mov eax D@Address

      ; Search for return address in the instruction table.
        sub eax D$DebugBaseOfCode
        mov edi D$IpTable, ecx D$IpTablePtr
        sub ecx edi
        shr ecx 2
            jz L9>> ; if IpTable is empty/freed (which should never happen)
        While eax >= D$edi
            dec ecx | jz L9>>
            add edi 4
        EndWhile

      ; Search back for the preceding instruction. We can't just use [edi-8] because
      ; also Labels are recorded in the IpTable.
        sub edi 4
        Do
            On edi < D$IpTable, jmp L9>>
            mov edx D$edi
            sub edi 4
        Loop_Until edx <> eax
        mov ebx eax | sub ebx edx ; statements length
        add edx D$DebugBaseOfCode
        call ReadProcessMem edx, PrecedingCode, ebx

      ; The statement might contain several instruction (call-macro!), so decode
      ; every instruction until we find the last one of that statement.
        mov esi PrecedingCode
        Do
            call InstructionDecode esi
            mov eax D$InstructionLength
            add esi eax
            sub ebx eax
                js L9> ; how is this possible ? it happens... [Bugfix V2.0]
        Loop_Until ebx = 0

        mov eax D$NextInstructionPtr
        .If D$eax = 'call'
            mov eax 1, edx 0
            sub esi D$InstructionLength
            If B$esi = 0E8
                mov edx D$esi+1
                add edx D@Address
                mov ecx D@CSE
                mov D$ecx+CSE_ProcAddress edx
            ElseIf W$esi = 015FF
                mov edx D$esi+2
                call ReadProcessMem edx, PrecedingCode, 4
                mov eax D$PrecedingCode
                mov ecx D@CSE
                mov D$ecx+CSE_ProcAddress eax
                mov eax 1
            EndIf
            ExitP
        .EndIf

L9:     mov eax 0
EndP
____________________________________________________________________________________________

; Lookup the address of the code label after which the given code address follows IOW
; search the procedure which contains the code at the given address.

Proc GetNearestProc:
    Arguments @Address
    Local @NearestProc
    Uses esi, edi, ebx

    mov D@NearestProc 0
    mov edi D$PlainLabelList, ebx D$EndOfPlainLabelList, edx D@Address
    mov ecx D$edi
    add edi 5 | sub ecx 5

    While edi < ebx

        ; address of string
        mov esi edi
        mov al EOI
        repne scasb | jne L9>
        mov eax D$edi | add eax D$CodeAjust
        If eax <= edx
            On eax > D@NearestProc, mov D@NearestProc eax
        EndIf
        add edi 6 | sub ecx 6

    EndWhile

L9: mov eax D@NearestProc
EndP
____________________________________________________________________________________________

[LabelName: B$ ? #128]

CopyStringFromLabelList:
    push esi, edi

        mov edi LabelName
        While B$esi <> EOI
            movsb
        EndWhile
        mov B$edi 0

    pop edi, esi
ret
____________________________________________________________________________________________

; Lookup the label of the procedure starting at the given address.
; see BuildPlainLabelList

Proc ScanLabelListForCodeLabel:
    Arguments @Address, @Exact
    Local @NearestProc @NearestProcName
    Uses esi, edi, ebx, edx

    mov edi D$PlainLabelList, ebx D$EndOfPlainLabelList, edx D@Address
    mov ecx D$edi+PlainLabelList.LenDis
    add edi Size_Of_PlainLabelList

    ; If we are ebugging a dll and it was rebased, we need to retrieve his original address to display on the mousehint
    If_And D$DebugProcessData.Type = DEBUG_DLL, D$DebugProcessData.IsRebased = &TRUE
        sub edx D$DebugProcessData.DllNewImageBase | add edx D$DebugProcessData.DllImageBase
    End_If


    .If D@Exact = &TRUE

        .While edi < ebx
            ; Address of string
            mov esi edi
            While B$edi <> EOI
                inc edi | On edi >= ebx, jmp L9>>
            End_While
            inc edi ; bypass ending EOI

            Test_If_Not B$edi+PlainLabelArray.FlagTypeDis DataLabelFlag
                mov eax D$edi+PlainLabelArray.offsetDis | add eax D$CodeAjust
                If eax = edx
                    call CopyStringFromLabelList
                    ExitP
                EndIf
            Test_End
            add edi Size_Of_PlainLabelArray
        .End_While

    .Else

        mov D@NearestProc 0, D@NearestProcName 0
        .While edi < ebx

            ; Address of string
            mov esi edi
            While B$edi <> EOI
                inc edi | On edi >= ebx, jmp L9>>
            End_While
            inc edi ; bypass ending EOI

            Test_If_Not B$edi+PlainLabelArray.FlagTypeDis DataLabelFlag
                mov eax D$edi+PlainLabelArray.offsetDis | add eax D$CodeAjust
                If eax <= edx
                    On eax > D@NearestProc, mov D@NearestProc eax, D@NearestProcName esi
                EndIf
            Test_End
            add edi Size_Of_PlainLabelArray
        .End_While

        mov esi D@NearestProcName
        On esi <> 0, call CopyStringFromLabelList
        mov eax D@NearestProc
        On eax <> 0, ExitP

    .EndIf

L9: mov eax D@Address
    call IntToHexString
    move D$LabelName D$HexString, D$LabelName+4, D$HexString+4
    mov B$LabelName+8 0
    mov eax D@Address

EndP

____________________________________________________________________________________________

; Lookup the label of the procedure starting at the given address. The export table of
; the module which is referenced by the code address is scanned.

[PEBuffer: ?]

Proc ScanExportTableForCodeLabel:
    Arguments @CodeAddress, @Exact
    Local @BaseAddress, @ExportBaseRVA, @ExportSize, @NumNames, @ProcAddressRVA, @AddressTableEntry
    Uses ebx, esi, edi

    call IsModuleCode D@CodeAddress
    If eax = 0
        ExitP
    EndIf
    move D@BaseAddress D$eax+ME_Base
    move D@ExportBaseRVA D$eax+ME_ExportBase
    move D@ExportSize D$eax+ME_ExportSize

    On D@ExportBaseRVA = 0, ExitP
    On D@ExportSize = 0, ExitP

    ; Load the export table.
    VirtualAlloc PEBuffer, D@ExportSize

        mov eax D@ExportBaseRVA | add eax D@BaseAddress
        call ReadProcessMem eax, D$PEBuffer, D@ExportSize

        mov esi D$PEBuffer, edi LabelName
        ; address conversion: RVA -> Linear address in copied export-table
        mov eax esi
        sub eax D@ExportBaseRVA
        mov D$ExportTableAdjust eax
        ; copy module name
        mov edx D$esi+IMAGE_EXPORT_DIRECTORY.nNameDis
        add edx D$ExportTableAdjust
        Do
            mov al B$edx
            If al = 0 ; no library name?
                mov eax '???.'
                stosd
                jmp L0>
            EndIf
            stosb
            inc edx
        Loop_Until al = '.'
        ; number of exported functions
L0:     mov ecx D$esi+IMAGE_EXPORT_DIRECTORY.NumberOfFunctionsDis
        move D@NumNames D$esi+IMAGE_EXPORT_DIRECTORY.NumberOfNamesDis
        ; Convert address table RVA to pointer in local buffer
        mov eax D$esi+IMAGE_EXPORT_DIRECTORY.AddressOfFunctionsDis | add eax D$ExportTableAdjust
        ; Convert code address to RVA
        mov ebx D@CodeAddress | sub ebx D@BaseAddress

        ; ebx = Code RVA
        ; eax -> Address table (list of function RVA's)
        ; ecx = Number of functions

        mov D@ProcAddressRVA 0
        .If D@Exact = &TRUE

            mov D@ProcAddressRVA ebx
            While ecx > 0
                If D$eax = ebx
                    call SearchNameInExportTable
                    cmp edx 0 | je L8> ; function has no name (comctl32.dll !)
                    sub edx D@ExportBaseRVA
                    cmp edx D@ExportSize | ja L8> ; ignore invalid function name pointers (does happen!)
                    Do
                        mov al B$esi+edx
                        stosb
                        inc edx
                    Loop_until al = 0
                    jmp L9>>
                EndIf
                add eax 4
                dec ecx
            EndWhile
L8:         DwordToHex D@CodeAddress
            mov B$edi 0

        .Else

            While ecx > 0
                If D$eax <= ebx
                    mov edx D$eax
                    On edx > D@ProcAddressRVA,
                        mov D@ProcAddressRVA edx, D@AddressTableEntry eax
                EndIf
                add eax 4
                dec ecx
            EndWhile
            mov eax D@AddressTableEntry
            call SearchNameInExportTable
            If edx <> 0
                sub edx D@ExportBaseRVA
                cmp edx D@ExportSize | ja L8> ; ignore invalid function name pointers (does happen!)
                Do
                    mov al B$esi+edx
                    stosb
                    inc edx
                Loop_until al = 0
                dec edi
                mov D$edi ' (< ' | add edi 3
                mov edx ebx | sub edx D@ProcAddressRVA
                cmp edx 0_FFFF | ja L8> ; function bigger as 64k ??
                mov ecx 2 | call IntToHex
                mov W$edi ')'
            Else
L8:             mov D@ProcAddressRVA ebx
                mov al '<' | stosb
                DwordToHex D@CodeAddress
                mov B$edi 0
            EndIf
        .EndIf

L9: VirtualFree D$PEBuffer

    mov eax D@ProcAddressRVA
    add eax D@BaseAddress
EndP

[ExportTableAdjust: ?]

; Input
;   esi -> copied Export section
;   eax -> Address table entry
;   ecx = Number of functions
; Output
;   edx = Name RVA  /  zero (no name in export table -ordinal only-)

SearchNameInExportTable:
    mov edx 0
    push eax ecx edi
        sub eax D$ExportTableAdjust ; convert back to RVA
        sub eax D$esi+IMAGE_EXPORT_DIRECTORY.AddressOfFunctionsDis | shr eax 2 ; eax = index of current function
        mov ecx D$esi+IMAGE_EXPORT_DIRECTORY.NumberOfNamesDis ; ecx = number of names
        mov edi D$esi+IMAGE_EXPORT_DIRECTORY.AddressOfNameOrdinalsDis | add edi D$ExportTableAdjust ; ordinal table
        repne scasw ; search the ordinal table
            jne L9>
        sub edi D$ExportTableAdjust
        sub edi D$esi+IMAGE_EXPORT_DIRECTORY.AddressOfNameOrdinalsDis | shl edi 1
        add edi D$esi+IMAGE_EXPORT_DIRECTORY.AddressOfNamesDis
        add edi D$ExportTableAdjust
        mov edx D$edi-4
L9: pop edi ecx eax
ret
____________________________________________________________________________________________

; Try to find out how many parameters are passed to the procedure. If a stackframe is
; found (entry sequence 55 8B EC), the code is scanned for the exit sequence [8B E5]5D | C9.
;
; After that the ret instruction tells how many bytes are removed from the stack, which
; is the number of params multiplied by 4:
;   ret       >> C3 (no params)
;   ret ####  >> C2 ## ##
; If there is a naked ret (C3) then either there are no parameters, or the params get
; removed by the caller (C_Call). Check the code at the return address for:
;   add esp imm >> 83 C4 ##
;
; If no stackframe is found, the assumption is made that no parameters were passed
; on the stack (which may not be true for some asm hardcore code-styles).

[ProcBuffer: ?]

[GUARD_BYTES 4] ; protect from dissassembler reading data below the ip ('op00')

Proc CountParametersAndScanStackFrame:
    Arguments @CSE
    Local @Procedure, @RetAddress, @Result, @Size, @NumLocals
    Uses edi, esi, ebx

    VirtualAlloc ProcBuffer, 01000

        mov D@Result 0

        mov eax D@CSE
        mov D$eax+CSE_NumParams 0
        move D@Procedure D$eax+CSE_ProcAddress
        mov eax D$eax+CSE_Address
        move D@RetAddress D$eax-4

        call IsProcessMemory D@Procedure
        On eax > 01000-GUARD_BYTES, mov eax 01000-GUARD_BYTES
        mov D@Size eax

        mov esi D$ProcBuffer | add esi GUARD_BYTES
        call ReadProcessMem D@Procedure, esi, D@Size
        On eax = &FALSE, jmp @Exit

        mov ecx D@Size
        sub ecx 16 ; guard bytes to prevent from disassembly overflow

      ; Stack frame?
        cmp B$esi 0C8   | je @WithStackFrame ; enter x x
        cmp W$esi 08B55 | jne @WithOutStackFrame
        cmp B$esi+2 0EC | jne @WithOutStackFrame
;;
    the proc-entry has the following structure:
    [in most API calls local and seh section exchanged]
    
    stackframe      push ebp        55
                    mov ebp esp     8B EC
                    
    locals          sub esp imm8/32 83/81 EC im
    
    SEH            (push imm32)     68 imm32 / 6A imm8
                   (push imm32)
                    push imm32
                    mov eax D$fs:0  64 A1 00 00 00 00
                    push eax        50
                    mov D$fs:0 esp  64 89 25 00 00 00 00                 
                   (push ecx)
                   (push ecx)
                   
    save regs       push ebx
                    ...
    
;;

@WithStackFrame:
        mov eax D@CSE | or B$eax+CSE_Flags CSEF_HAS_STACKFRAME

      ; Scan proc entry - size of local data in EBX. Local data is
      ; reserved with "sub esp xx" but also everything that is pushed
      ; _before_ the sub is considered as local data (SEH in Win32 code).

        mov ebx 4, edx 0 ; ebx = sizeof locals, edx = sizeof pushed data
        If B$esi = 0C8 ; enter x x
            add bx W$esi+1
        EndIf

        add esi 3 | sub ecx 3

L0:     call InstructionDecode esi
            If D$eax = 'push'
                add edx 4
            ElseIf D$eax = 'mov '
                ;nop
            ElseIf W$esi = 0EC83 ; sub esp b
                movzx eax B$esi+2
                add ebx eax
                add ebx edx
            ElseIf W$esi = 0EC81 ; sub esp dw
                mov eax D$esi+2
                add ebx eax
                add ebx edx
            Else
                jmp L1>
            EndIf
            mov eax D$InstructionLength
            sub ecx eax | js @Exit
            add esi eax
        jmp L0<

      ; Search exit sequence - might be 'pop ebp' / 'leave'
L1:     dec ecx | jz @Exit ; we need at min 2 bytes left (leave + ret)
            lodsb
            cmp al 05D | je L2> ; pop ebp
            cmp al 0C9 | je L2> ; leave
        jmp L1<

        ; mov esp ebp is not really necessary...
        ;cmp W$esi-3 0E58B | jne L1<

L2:     If B$esi = 0C3 ; ret
            ; C_Call ?
            call ReadProcessMem D@RetAddress, D$ProcBuffer, 4
            mov esi D$ProcBuffer
            cmp W$esi 0C483 | jne @Exit
            movzx eax B$esi+2
            shr eax 2
            mov D@Result eax
            jmp @Exit
        ElseIf B$esi = 0C2 ; ret imm
            movzx eax W$esi+1
            shr eax 2
            mov D@Result eax
            jmp @Exit
        Else ; no ret, maybe data mistaken as exit sequence?
            mov edx 0DEADC0DE ; debug (I-was-here) marker
            jmp L1<
        EndIf
        jmp @Exit

@WithOutStackFrame:
        ; No stack-frame, decode every instruction to find the ret.

L1:     ; Do not scan behind the end of code, may happen if there is a function
        ; without ret (e.g. Main: [...] call 'Kernel32.ExitProcess')

        mov ebx 0
        On D$esi = 0, jmp @Exit
        call InstructionDecode esi
        .If W$eax = 're' ; must be re(t)
            If B$esi = 0C3 ; ret
                ; C_Call ?
                call ReadProcessMem D@RetAddress, D$ProcBuffer, 4
                mov esi D$ProcBuffer
                cmp W$esi 0C483 | jne @Exit
                movzx eax B$esi+2
                shr eax 2
                mov D@Result eax
                jmp @Exit
            ElseIf B$esi = 0C2 ; ret imm
                movzx eax W$esi+1
                shr eax 2
                mov D@Result eax
                jmp @Exit
            Else ; ???
                jmp @Exit
            EndIf
        .EndIf
        add esi D$InstructionLength
        sub ecx D$InstructionLength | jns @WithOutStackFrame
        jmp @Exit

@Exit:
    VirtualFree D$ProcBuffer

    mov eax D@CSE
    move D$eax+CSE_NumParams D@Result
    shr ebx 2
    move D$eax+CSE_NumLocals ebx
EndP
____________________________________________________________________________________________

[StackFragment: B$ ? #32]

Proc ScanStackForCodePointer:
    Arguments @BaseOfCode, @CodeSize, @StackPointer
    Local @StackSize, @Pointer, @Offset
    Uses ebx esi edi

        ; Get size of current stack
        mov edi D@StackPointer
        call IsProcessMemory edi
        mov D@StackSize eax
        mov ebx 32
        mov D@Offset 0

        ; Read stack in 32byte fragments
        While D@StackSize >s 0
            On D@StackSize < ebx, mov ebx D@StackSize
            call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, edi, StackFragment, ebx, &NULL
            sub D@StackSize ebx
            add edi ebx
            mov ecx ebx
            shr ecx 2 | jecxz L2> ; Bug fix Betov.
            mov esi StackFragment
L1:         lodsd
                mov D@Pointer eax
                ;call IsProcessCode D@BaseOfCode, D@CodeSize, eax
                push ecx ; bugfix V2.0b
                    call CheckReturnAddress D@BaseOfCode, D@CodeSize, eax
                pop ecx
                cmp eax &TRUE | je L2>
                add D@Offset 4
            loop L1<
        End_While
        mov D@Pointer 0

L2:     mov eax D@Pointer
        mov edx D@Offset
EndP

Proc CheckReturnAddress:
    Arguments @BaseOfCode, @CodeSize, @AddresstoFind
    Local @Result
    Structure @Dummy SizeOf_CSE

    call IsProcessCode D@BaseOfCode, D@CodeSize, D@AddresstoFind
    If eax = 1
        pushad
            call IsReturnAddressInSource D@AddresstoFind, D@Dummy
            mov D@Result eax
        popad
        mov eax D@Result
    End_If
EndP
____________________________________________________________________________________________

; Nothing works for fully close the Debuggee. Something remain attached:

CloseProcess:
    call 'KERNEL32.GetExitCodeProcess' D$PI.hProcess, ExitCode
;;
    ; consider using the SafeTerminateProcess function after cleaning up rosasm code and seeking for stack problems or mem alloca erros
    .If eax = 0
        call ReportWinError {'CloseProcess: GetExitCodeProcess' 0}
        call SafeTerminateProcess D$PI.hProcess, D$ExitCode
    .End_If
;;
    call 'KERNEL32.TerminateProcess'  D$PI.hProcess, D$ExitCode

    call 'KERNEL32.CloseHandle' D$PI.hThread       ; should be of
    call 'KERNEL32.CloseHandle' D$PI.hProcess      ; no use.
ret
____________________________________________________________________________________________

; http://hyacinth.byus.net/moniwiki/wiki.php/C%2B%2B/SafeTerminateProcess

Proc SafeTerminateProcess:
    Arguments @hProcess, @uExitCode
    Local @dwTID, @dwCode, @dwErr, @hProcessDup, @hRT, @hKernel, @bDup, @hHandle, @pfnExitProc, @bSuccess

    mov D@dwTID 0
    mov D@dwCode 0
    mov D@dwErr 0
    mov D@hProcessDup &INVALID_HANDLE_VALUE
    mov D@hRT &NULL
    mov D@bSuccess &FALSE
    call 'kernel32.GetModuleHandleA' {B$ "kernel32", 0} | mov D@hKernel eax

    call 'kernel32.GetCurrentProcess' | mov ebx eax
    call 'kernel32.GetCurrentProcess'
    lea edx D@hProcessDup
    call 'kernel32.DuplicateHandle' eax, D@hProcess, ebx, edx, &PROCESS_ALL_ACCESS, &FALSE, 0
    mov D@bDup eax

    If eax <> 0
        mov eax D@hProcessDup | mov D@hHandle eax
    Else
        mov eax D@hProcess | mov D@hHandle eax
    End_If

    lea eax D@dwCode
    call 'KERNEL32.GetExitCodeProcess' D@hHandle, eax

    ...If_And eax <> 0, D@dwCode = &STILL_ACTIVE

        call 'kernel32.GetProcAddress' D@hKernel, {B$ "ExitProcess", 0}
        mov D@pfnExitProc eax

        If D@bDup <> 0
            mov eax D@hProcessDup | mov D@hHandle eax
        Else
            mov eax D@hProcess | mov D@hHandle eax
        End_If

        lea eax D@dwTID
        call 'kernel32.CreateRemoteThread' D@hHandle, &NULL, 0, D@pfnExitProc, D@uExitCode, 0, eax
        mov D@hRT eax
        If eax = 0
            call 'kernel32.GetLastError'
            mov D@dwErr eax
        End_If
    ...Else
        mov D@dwErr &ERROR_PROCESS_ABORTED
    ...End_If

    ..If D@hRT <> 0

        If D@bDup <> 0
            mov eax D@hProcessDup | mov D@hHandle eax
        Else
            mov eax D@hProcess | mov D@hHandle eax
        End_If
        call 'kernel32.WaitForSingleObject' D@hHandle, &INFINITE
        call 'KERNEL32.CloseHandle' D@hRT
        mov D@bSuccess &TRUE

    ..End_If

    If D@bDup <> 0
        call 'KERNEL32.CloseHandle' D@hProcessDup
    End_If

    If D@bSuccess = &FALSE
        call 'kernel32.SetLastError' D@dwErr
    End_If

    mov eax D@bSuccess

EndP

____________________________________________________________________________________________

; TODO
;  * write back real opcodes (into copy!) when reading code sections with dynamic breakpoints inside
;  * reading beyond 2GB in Win9x ? if yes, just return ptr as this section is global

Proc ReadProcessMem:
    Arguments @Source, @Dest, @Size
    Uses ebx, esi, ecx, edx

    call 'KERNEL32.ReadProcessMemory' D$PI.hProcess, D@Source, D@Dest, D@Size, &NULL
    On eax = 0, ExitP

    ; Iterate through breakpoint-table, write back real opcodes
    mov esi D$BPTable, ecx D$NumBreakpoints
L0: While ecx > 0
        mov eax D$esi+BPTable.AddressDis
        .If eax >= D@Source
            sub eax D@Source ; eax = offset into buffer
            If eax < D@Size
                mov edx D@Dest
                mov bl B$esi+BPTable.OverwrittenDis
                mov B$edx+eax bl ; restore original byte
            EndIf
        .EndIf
        add esi Size_Of_BPTable
        dec ecx
    End_While

    mov eax 1
EndP
____________________________________________________________________________________________

Proc WriteProcessMem:
    Arguments @Dest @Source @Size

    call 'KERNEL32.WriteProcessMemory' D$PI.hProcess, D@Dest, D@Source, D@Size, &NULL
    If eax = &TRUE
      ; should not be needed on x86
        call 'KERNEL32.FlushInstructionCache' D$PI.hProcess, D@Dest, D@Size
    EndIf
EndP
____________________________________________________________________________________________

[ExitCode: ?]

[NumberOfBytesRead: ?]


;;
SetCodeRVA:
    move D$DebugBaseOfCode D$CPDI.lpBaseOfImage

    VirtualAlloc DebugHeaderImage 0400 | mov D$NumberOfBytesRead 0

    call 'KERNEL32.ReadProcessMemory' D$CPDI.hProcess, D$DebugBaseOfCode,
                                     D$DebugHeaderImage, 0400,  NumberOfBytesRead
    mov edi D$DebugHeaderImage, al '.', ecx 0400
L0: repne scasb | jne L9>
       cmp D$edi 'text' | jne L0<
           dec edi
           mov eax D$edi+0C
           add D$DebugBaseOfCode eax
           move D$DebugCodeSize D$edi+8

L9: VirtualFree D$DebugHeaderImage
ret
;;
____________________________________________________________________________________________

SignalDebugEvent:
    mov B$IsDebugEvent &TRUE
    call 'User32.PostMessageA' D$DebugDialogHandle, WM_DEBUGEVENT, 0, 0
    call 'Kernel32.WaitForSingleObject' D$UserInputEvent, &INFINITE
    mov B$IsDebugEvent &FALSE
ret

____________________________________________________________________________________________
____________________________________________________________________________________________

WATCHPOINTS:

; Watchpoints mean everything in RosAsm that uses the debug registers (DR0-DR7).
____________________________________________________________________________________________

; Mutex to control access to shared WP data structures of debug thread and UI thread.
; To prevent deadlocks, do not make API calls while holding the mutex.

[WPSynchMutex: ?]

; Maps to the debug registers DR0, DR1, DR2, DR3, DR7. All r/w access must be protected
; by WPSynchMutex. These values are written by the UI thread and read by the debug thread.
; Exception is WPChanged which is set to 1 by UI and zeroed by the debugger.

[WPSlot0: ?     ; DR0
 WPSlot1: ?     ; DR1
 WPSlot2: ?     ; DR2
 WPSlot3: ?     ; DR3
 WPControl: ?   ; DR7
 WPChanged: ?]  ; Signal changed data
____________________________________________________________________________________________

; Init data and mutex - called by resource manager thread.

InitWatchpointResources:
    mov eax 0
    mov D$WPSlot0 eax
    mov D$WPSlot1 eax
    mov D$WPSlot2 eax
    mov D$WPSlot3 eax
    mov D$WPControl eax
    mov D$WPChanged eax
    call 'Kernel32.CreateMutexA' &NULL, &FALSE, &NULL
    mov D$WPSynchMutex eax
    If eax = 0
        call ReportWinError {'CreateWPMutex' 0}
        mov eax 0
    EndIf
ret
____________________________________________________________________________________________

; Destroy mutex and clear data fields - called by resource manager thread.

FreeWatchpointResources:
    call 'KERNEL32.CloseHandle' D$WPSynchMutex
    mov eax 0
    mov D$WPSlot0 eax
    mov D$WPSlot1 eax
    mov D$WPSlot2 eax
    mov D$WPSlot3 eax
    mov D$WPControl eax
    mov D$WPChanged eax
ret
____________________________________________________________________________________________

; Add watchpoint - called by UI.

Proc SetWatchPoint:
    Arguments @Address, @Size, @ReadWrite

    call 'KERNEL32.WaitForSingleObject' D$WPSynchMutex, &INFINITE

        mov eax D@Address
        mov D$WPSlot0 eax
        and D$WPControl 0_FFF0_FFFF
        or  D$WPControl 1 ; activate DR0
        mov eax D@ReadWrite
        and eax 0011
        shl eax 16
        or  D$WPControl eax
        If eax <> 0
            ; 00 1-byte length
            ; 01 2-byte length
            ; 10 Undefined
            ; 11 4-byte length
            mov eax D@Size | dec eax
            shl eax 18
            or  D$WPControl eax
        EndIf

        mov D$WPChanged 1

    call 'KERNEL32.ReleaseMutex' D$WPSynchMutex
EndP
____________________________________________________________________________________________

; Delete watchpoint - called by UI.

Proc DeleteWatchPoint:
    call 'KERNEL32.WaitForSingleObject' D$WPSynchMutex, &INFINITE

        and D$WPControl 0_FFF0_FFFE
        mov D$WPSlot0 0

        mov D$WPChanged 1

    call 'KERNEL32.ReleaseMutex' D$WPSynchMutex
EndP
____________________________________________________________________________________________

; Take over any changed values into thread context structure - called by debugger.

Proc TransferWatchpoints:
    call 'KERNEL32.WaitForSingleObject' D$WPSynchMutex, &INFINITE

        move D$C.iDr0 D$WPSlot0
        move D$C.iDr1 D$WPSlot1
        move D$C.iDr2 D$WPSlot2
        move D$C.iDr3 D$WPSlot3
        move D$C.iDr7 D$WPControl

        mov D$WPChanged 0

    call 'KERNEL32.ReleaseMutex' D$WPSynchMutex
EndP
____________________________________________________________________________________________

; Test if address is watched - called by UI.
; No need to use the mutual exclusion here as the debug thread doesn't write to the fields.

Proc IsWatchPoint:
    Arguments @Address

    mov eax D@Address
    If eax = D$WPSlot0
        mov eax D$WPControl
        shr eax 16
        and eax 0011
    Else
        mov eax 0
    EndIf
EndP
____________________________________________________________________________________________

; Test if a watched data access has taken place - called by debugger.

Proc EncounterWatchPoint:

    test D$C.iDr6 01 | jz L9>

    mov D$DebugEventType DET_WP
    move D$WatchedAddress D$C.iDr0
    ;dec D$SourcePosCodeAddress
    On D$SourcePosCodeAddress <> 0, dec D$SourcePosCodeAddress

  ; clear status
    mov D$C.iDr6 0
L9:
EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

; BREAKPOINTS

; Every breakpoint is saved in the breakpoint table. An entry has the following structure:
; DWORD Address
; BYTE  Type (see type flags)
; BYTE  State (see state flags)
; BYTE  Reserved (can be used for bp-group ids)
; BYTE  Overwritten byte

;;
[BPTable:
 BPTable.Address: D$ 0
 BPTable.Type: B$ 0
 BPTable.State: B$ 0
 BPTable.Reserved: B$ 0
 BPTable.Overwritten: B$ 0]

;;

[BPTable: ? NumBreakpoints: ?]

; Type flags
[BP_STATIC 0 BP_STATIC2BYTE 01 BP_DYNAMIC 02 BP_ONESHOT 04]
; State flags
[BP_ENABLED 1 BP_DISABLED 0 BP_ISPENDING 2]


[BPTable.AddressDis 0
 BPTable.TypeDis 4
 BPTable.StateDis 5
 BPTable.ReservedDis 6
 BPTable.OverwrittenDis 7]

[Size_Of_BPTable 8]


[MAX_BREAKPOINT 8192]
[BPTABLE_ARRAY (MAX_BREAKPOINT*Size_Of_BPTable)]

; BPPending has the address of a dynamic breakpoint that is currently deactivated to
; execute the real code that was overwritten by the bp-op.
; Otherwise it is zero- indicating that no BP must be written back to process mem ATM.
[BPPending: ?]

CreateBPTable:
    VirtualFree D$BPTable
    VirtualAlloc BPTable, BPTABLE_ARRAY;01_0000 ; 64k table
    mov D$NumBreakpoints 0
    mov D$BPPending 0
ret

DestroyBPTable:
    mov D$BPPending 0
    mov D$NumBreakpoints 0
    VirtualFree D$BPTable
ret

[RunAfterWriteBack: ?]
____________________________________________________________________________________________

; Add breakpoint to table and, if enabled, to process memory.

Proc AddProcessBreakpoint:
    Arguments @Address, @Type, @State, @GroupID
    Local @SourceByteOverwrite
    Uses edi

    mov D@SourceByteOverwrite 090
    mov edi D$BPTable, ecx D$NumBreakpoints
    lea edi D$edi+ecx*Size_Of_BPTable

    mov eax D@Address   | stosd
    mov al B@Type       | stosb
    mov al B@State      | stosb
    mov al B@GroupID    | stosb

    .If B@Type <> BP_STATIC
        call ReadProcessMem D@Address, edi, 1
        If B@State = BP_ENABLED
            call WriteProcessMem D@Address, BreakpointOp, 1
        EndIf
    .Else
        If B@State = BP_DISABLED
            ;mov B$ByteBuf 090
            lea eax D@SourceByteOverwrite
            ;call WriteProcessMem D@Address, ByteBuf, 1
            call WriteProcessMem D@Address, eax, 1
        EndIf
    .EndIf
    inc D$NumBreakpoints

        ;call 'Kernel32.OutputDebugStringA' {'BP manager: BP added' 0}
EndP
____________________________________________________________________________________________

; Delete breakpoint from table and from process memory.

Proc DeleteProcessBreakpoint:
    Arguments @Address
    Local @SourceByteOverwrite
    Uses esi

        On D$NumBreakpoints = 0, jmp L9>>

        ; save last entry on stack, search for breakpoint in table, do the cleanup
        ; work (write back process memory), and overwrite entry with last one from stack
        mov esi D$BPTable, ecx D$NumBreakpoints, eax D@Address
        dec ecx
        push D$esi+ecx*Size_Of_BPTable, D$esi+ecx*Size_Of_BPTable+BPTable.TypeDis
            While D$esi+BPTable.AddressDis <> eax
                add esi Size_Of_BPTable
                dec ecx | js L9>
            End_While

            .If B$esi+BPTable.StateDis = BP_ENABLED
                If B$esi+BPTable.TypeDis <> BP_STATIC
                    ;mov al B$esi+BPTable.OverwrittenDis
                    movzx eax B$esi+BPTable.OverwrittenDis
                Else
                    ;mov al 090
                    mov eax 090
                EndIf
                mov D@SourceByteOverwrite eax
                ;mov B$ByteBuf al
                ;call WriteProcessMem D@Address, ByteBuf, 1
                lea eax D@SourceByteOverwrite
                call WriteProcessMem D@Address, eax, 1

            .ElseIf B$esi+BPTable.StateDis = BP_ISPENDING
                ; fix: if breakpoint is currently deactivated and waits for write-back
                ; the write-back command must be killed with the breakpoint
                mov D$BPPending 0
            .EndIf

        pop D$esi+BPTable.TypeDis, D$esi+BPTable.AddressDis
        dec D$NumBreakpoints
        ;call 'Kernel32.OutputDebugStringA' {'BP manager: BP deleted' 0}
        ExitP

L9:     call 'User32.MessageBoxA' D$hwnd, {'Tried to delete non-existing breakpoint!' 0},
                                  {'Debugger error' 0}, &MB_ICONERROR
EndP
____________________________________________________________________________________________

; When [Hold On Breakpoint] is deactivated we disable all known breakpoints and delete
; all oneshot breakpoints.

Proc DisableProcessBreakpoints:
    Local @SourceByteOverwrite
    Uses esi, edi

    ; Iterate through breakpoint-table, delete oneshot and disable all other bp's.
    mov esi D$BPTable, edi D$NumBreakpoints
    mov D@SourceByteOverwrite 090
L0:
    While edi > 0
        If B$esi+BPTable.TypeDis = BP_ONESHOT
            call DeleteProcessBreakpoint D$esi+BPTable.AddressDis
            dec edi
            jmp L0<
        ElseIf B$esi+BPTable.TypeDis = BP_DYNAMIC
            mov B$esi+BPTable.StateDis BP_DISABLED
            lea eax D$esi+BPTable.OverwrittenDis
            call WriteProcessMem D$esi+BPTable.AddressDis, eax, 1
        ElseIf B$esi+BPTable.TypeDis = BP_STATIC
            mov B$esi+BPTable.StateDis BP_DISABLED
            ;mov B$ByteBuf 090
            lea eax D@SourceByteOverwrite
            ;call WriteProcessMem D$esi+BPTable.AddressDis, ByteBuf, 1
            call WriteProcessMem D$esi+BPTable.AddressDis, eax, 1
        EndIf
        add esi Size_Of_BPTable
        dec edi
    End_While
    mov B$BreakpointsEnabled &FALSE

EndP
____________________________________________________________________________________________

; When [Hold On Breakpoint] is reactivated we enable all known breakpoints

Proc EnableProcessBreakpoints:
    Uses esi, ebx

        ; Iterate through breakpoint-table, write breakpoint ops to process memory.
        mov esi D$BPTable, ebx D$NumBreakpoints ; bugfix V2.0b
L0:     While ebx > 0
            call WriteProcessMem D$esi+BPTable.AddressDis, BreakpointOp, 1
            mov B$esi+BPTable.StateDis BP_ENABLED
            add esi Size_Of_BPTable
            dec ebx
        EndWhile
        mov B$BreakpointsEnabled &TRUE
EndP
____________________________________________________________________________________________

; Deal with encountered breakpoints. Adjusts EIP according to breakpoint type.

Proc EncounterBreakpoint:
    Uses esi, ebx

        ; Search breakpoint in table
        mov esi D$BPTable, ecx D$NumBreakpoints
        mov ebx D$C.regEip | dec ebx
        On ecx = 0, jmp L1>>
        While D$esi+BPTable.AddressDis <> ebx
            add esi Size_Of_BPTable
            dec ecx | jz L1>>
        EndWhile

        ; One shot breakpoints are set by step-over, step-out and pause. The user will
        ; receive them as normal stepping (well...), so they are deleted as soon as they are hit.
        ; They can be grouped (for pause), so that if one is hit all others in the group
        ; are deleted with it.
        ...If B$esi+BPTable.TypeDis = BP_ONESHOT
            lea eax D$esi+BPTable.OverwrittenDis
            call WriteProcessMem ebx, eax, 1
            If B$esi+BPTable.ReservedDis <> 0
                movzx eax B$esi+BPTable.ReservedDis
                call KillBreakpointGroup eax
            Else
                call DeleteProcessBreakpoint ebx
            EndIf
            ;dec D$C.regEIP, D$SourcePosCodeAddress
            dec D$C.regEIP
            On D$SourcePosCodeAddress <> 0, dec D$SourcePosCodeAddress

        ; Dynamic breakpoints are user-defined which appear as bp-marks in the source
        ; editor. When encountered the original operation must be restored, executed
        ; (single-stepped), and written back when the next SINGLE_STEP Debug event occurs.
        ...Else_If B$esi+BPTable.TypeDis = BP_DYNAMIC
            lea eax D$esi+BPTable.OverwrittenDis
            call WriteProcessMem ebx, eax, 1
            mov B$esi+BPTable.StateDis BP_ISPENDING
            If B$HoldOnBreakpoints = &TRUE
                mov D$BPPending esi
            EndIf
            ; Weird case when a 'int 3' is marked with a breakpoint. It would loop into infinity
            ; so we just skip over it.
            ;On B$esi+BPTable.OverwrittenDis <> 0CC, dec D$C.regEIP, D$SourcePosCodeAddress
            .If B$esi+BPTable.OverwrittenDis <> 0CC
                dec D$C.regEIP
                On D$SourcePosCodeAddress <> 0, dec D$SourcePosCodeAddress
            .End_If

        ; Static breakpoints need not to be dealt with. Execution can continue
        ...EndIf

        ExitP

L1:     ; New static breakpoint encountered.
        call AddProcessBreakpoint ebx, BP_STATIC, D$HoldOnBreakpoints, 0
EndP
____________________________________________________________________________________________

Proc KillBreakpointGroup:
    Arguments @GroupID
    Uses esi, edi, ebx

        ; Search breakpoint with given group-id in table. If one is found, delete it.
        mov esi D$BPTable, edi D$NumBreakpoints, ebx D@GroupID
L0:     While edi > 0
            If B$esi+BPTable.ReservedDis = bl
                call DeleteProcessBreakpoint D$esi+BPTable.AddressDis
                dec edi
                jmp L0<
            EndIf
            add esi Size_Of_BPTable
            dec edi
        EndWhile
EndP
____________________________________________________________________________________________

; Interrupts the execution of a thread by setting a breakpoint at the location pointed at by
; the instruction pointer. If it is outside of process code, the stack is scanned for a
; valid code pointer.

Proc HaltThread:
    Arguments @ThreadHandle

        call 'KERNEL32.SuspendThread' D@ThreadHandle
        mov D$C.ContextFlags MY_CONTEXT_FULL
        call 'KERNEL32.GetThreadContext' D@ThreadHandle, Context
        call IsProcessCode D$DebugBaseOfCode, D$DebugCodeSize, D$C.regEip
        .If eax = &TRUE
            call AddProcessBreakpoint D$C.regEip, BP_ONESHOT, BP_ENABLED, 0
        .Else
            call ScanStackForCodePointer D$DebugBaseOfCode, D$DebugCodeSize, D$C.regEsp
            If eax <> 0
                call AddProcessBreakpoint eax, BP_ONESHOT, BP_ENABLED, 0
            EndIf
        .EndIf
        call 'Kernel32.ResumeThread' D@ThreadHandle
EndP
____________________________________________________________________________________________

; SYNCHRONIZE BREAKPOINTS WITH SOURCE EDITOR

[BPAnteroom: ? BPSyncMutex: ?]

; MAX_BREAKPOINT
[BPAnteroom.AddressDis 0
 BPAnteroom.SyncMutexSetDis 4]

[Size_Of_BPAnteroom 8]

CreateBPAnteroom:
    call 'Kernel32.CreateMutexA' 0, 0, 0
    mov D$BPSyncMutex eax
    VirtualAlloc BPAnteroom (MAX_BREAKPOINT*Size_Of_BPAnteroom);01000

    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Created BP synch objects' 0}
ret

DestroyBPAnteroom:
    VirtualFree D$BPAnteroom
    call 'KERNEL32.CloseHandle' D$BPSyncMutex

    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Destroyed BP synch objects' 0}
ret

; Copy initial set of breakpoints from the "OnTable" to the Anteroom. This proc is
; called before the debugger thread starts so we don't need to sync with the mutex.

InitialFillBPAnteroom:
    On D$BPOnTable = 0, ret
    ;call 'KERNEL32.WaitForSingleObject' D$BPSyncMutex, &INFINITE
    mov esi D$BPOnTable
    mov edi D$BPAnteroom
    While D$esi+BpOnTableDis <> 0
        movsd
        mov eax &TRUE
        stosd
        call 'Kernel32.OutputDebugStringA' {'BP synch: BP set' 0}
    End_While
    ;call 'KERNEL32.ReleaseMutex' D$BPSyncMutex
    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Initial fill complete' 0}
ret

; FILL Anteroom: This proc is called by the UI thread (RosAsm's mainthread)

Proc AddBPToAnteroom:
    Arguments @Address, @Set

        On edi = 0, ExitP

        call 'KERNEL32.WaitForSingleObject' D$BPSyncMutex, &INFINITE
        mov edi D$BPAnteroom
        While D$edi+BPAnteroom.AddressDis <> 0
            add edi Size_Of_BPAnteroom
        End_While
        move D$edi+BPAnteroom.AddressDis D@Address
        move D$edi+BPAnteroom.SyncMutexSetDis D@Set
        call 'KERNEL32.ReleaseMutex' D$BPSyncMutex

        ;call 'Kernel32.OutputDebugStringA' {'BP synch: BP set' 0}
EndP

; CLEAR Anteroom: This proc is called by the debugger thread

Proc ClearBPAnteroom:
    Arguments @BaseOfCode

    If D@BaseOfCode = 0
        xor eax eax
        ExitP
    End_If

    call 'KERNEL32.WaitForSingleObject' D$BPSyncMutex, &INFINITE
    call SetupBreakPoints D@BaseOfCode, D$BPAnteroom
    call 'KERNEL32.ReleaseMutex' D$BPSyncMutex
    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Anteroom cleared' 0}
EndP

_________________________________________________________________

; This is the main function necssary to enable/disable the breakpoints
Proc SetupBreakPoints:
    Arguments @BaseOfCode, @pBPAnteroom
    Uses edi, ebx, ecx, esi, edx;, eax

    mov edi D@pBPAnteroom

    While D$edi+BPAnteroom.AddressDis <> 0

        call GetcodeBreakPointPosFromSourcePointer D@BaseOfCode, D$edi+BPAnteroom.AddressDis
        If D$edi+BPAnteroom.SyncMutexSetDis = &TRUE ; add breakpoint
            call AddProcessBreakpoint eax, BP_DYNAMIC, D$HoldOnBreakpoints, 0
        Else
            call DeleteProcessBreakpoint eax
        EndIf

        mov D$edi+BPAnteroom.AddressDis 0, D$edi+BPAnteroom.SyncMutexSetDis 0
        add edi Size_Of_BPAnteroom
    End_While

EndP
_________________________________________________________________

;;
Proc ClearBPAnteroom_Old: ; working 24/2/2017

    call 'KERNEL32.WaitForSingleObject' D$BPSyncMutex, &INFINITE
    mov edi D$BPAnteroom

    While D$edi+BPAnteroom.AddressDis <> 0

        call GetcodeBreakPointPosFromSourcePointer D$DebugBaseOfCode, D$edi+BPAnteroom.AddressDis
        If D$edi+BPAnteroom.SyncMutexSetDis = &TRUE ; add breakpoint
            call AddProcessBreakpoint eax, BP_DYNAMIC, D$HoldOnBreakpoints, 0
        Else
            call DeleteProcessBreakpoint eax
        EndIf

        mov D$edi+BPAnteroom.AddressDis 0, D$edi+BPAnteroom.SyncMutexSetDis 0
        add edi Size_Of_BPAnteroom
    End_While

    call 'KERNEL32.ReleaseMutex' D$BPSyncMutex

    ;call 'Kernel32.OutputDebugStringA' {'BP synch: Anteroom cleared' 0}
EndP
;;
____________________________________________________________________________________________
____________________________________________________________________________________________

; ADDRESS SPACE ROUTINES
____________________________________________________________________________________________
____________________________________________________________________________________________

[PageOffsetMask     PageSize-1]
[PageBaseMask       0_FFFF_F000]

[AddressLowerBound:  0_1000]
[AddressUpperBound:  0_7FFF_0000]

[MemoryInformation:
 @BaseAddress: ?
 @AllocationBase: ?
 @AllocationProtect: ?
 @RegionSize: ?
 @State: ?
 @Protect: ?
 @Type: ?]

; Wrap routine for VirtualQueryEx
;   Parameter
;       Virtual Address
;   Output
;       EAX : 1=commited 0=free -1=error
;       ECX : region size

Proc VirtualQuery:
    Arguments @Address

        mov eax D@Address
        If eax >= D$AddressUpperBound
            mov eax &FALSE
            ExitP
        EndIf

        call 'KERNEL32.VirtualQueryEx' D$PI.hProcess, D@Address, MemoryInformation, 28
        If eax <> 28
            mov eax 0-1
            ;call ReportWinError {'VirtualQueryEx reported error:' 0}
            ExitP
        EndIf

        ; There is a bug in the implementation of VirtualQuery under WinNT.
        ; Jeffrey Richter has written a workaround (see VMMap in his book) which I
        ; implemented here.
        test D$MemoryInformation@AllocationBase 0FFF | jz L0>
        inc D$MemoryInformation@AllocationBase

L0:     test D$MemoryInformation@RegionSize 0FFF | jz L0>
        inc D$MemoryInformation@RegionSize

L0:     .If D$MemoryInformation@State <> &MEM_FREE
            If D$MemoryInformation@AllocationProtect = 0
                mov D$MemoryInformation@AllocationProtect &PAGE_READONLY
            EndIf
        .EndIf

        mov ecx D$MemoryInformation@RegionSize

        If D$MemoryInformation@State = &MEM_COMMIT
            mov eax &TRUE
        Else
            mov eax &FALSE
        Endif
EndP
____________________________________________________________________________________________

; Determines if the address points to a commited page in the debuggees address space.
; Returns the size of the block starting at the given address in eax.

Proc IsProcessMemory:
    Arguments @Address
    Local @Offset
    Uses ecx, edx

    ; Offset from start of Page
    mov eax D@Address
    and eax PageOffsetMask
    mov D@Offset eax
    ; Page address
    mov edx D@Address
    sub edx eax
    ; Query information for corresponding page:
    ;  * if eax is 1 the page was commited
    ;  * ecx contains the number of commited bytes from the page address
    call VirtualQuery edx
    If eax = 1
        mov eax ecx
        sub eax D@Offset
    Else
        mov eax 0
    EndIf

EndP
____________________________________________________________________________________________

Proc FindNextPage:
    Arguments @Address
    Uses ebx

        ; align address on page boundary
        mov ebx D@Address
        and ebx PageBaseMask
        add ebx PageSize
        If ebx >= D$AddressUpperBound
            mov eax 0
            ExitP
        EndIf
        ; advance the regions until a commited page is found or the upper bound is met
        call VirtualQuery ebx
        While eax = 0
            add ebx ecx
            If ebx >= D$AddressUpperBound
                mov eax 0
                ExitP
            EndIf
            call VirtualQuery ebx
            If eax = 0-1
                xor eax eax
                ExitP
            EndIf
        EndWhile
        If eax = 0-1
            xor eax eax
        Else
            mov eax ebx
        EndIf
EndP
____________________________________________________________________________________________

Proc FindPrevPage:
    Arguments @Address
    Uses ebx

        ; align address on page boundary
        mov ebx D@Address
        and ebx PageBaseMask
        sub ebx PageSize
        If ebx < D$AddressLowerBound
            mov eax 0
            ExitP
        EndIf
        ; advance the regions until a commited page is found or the upper bound is met
        call VirtualQuery ebx
        While eax = 0
            sub ebx PageSize
            If ebx < D$AddressLowerBound
                mov eax 0
                ExitP
            EndIf
            call VirtualQuery ebx
        EndWhile
        If eax = 0-1
            xor eax eax
        Else
            mov eax ebx
        EndIf
EndP
____________________________________________________________________________________________

Proc IsProcessCode:
    Arguments @BaseOfCode, @CodeSize, @AddresstoFind
    Uses edx

    mov eax &FALSE
    mov edx D@BaseOfCode;$DebugBaseOfCode
    If D@AddresstoFind >= edx
        add edx D@CodeSize;$DebugCodeSize
        On D@AddresstoFind < edx, mov eax &TRUE
    End_If
EndP
____________________________________________________________________________________________

Proc IsProcessStack:
    Arguments @Address

        mov eax D@Address
        .If eax >= D$C.regEsp
            call IsProcessMemory D$C.regEsp
            mov edx D@Address
            sub edx D$C.regEsp
            If edx < eax
                mov eax &TRUE
            Else
                mov eax &FALSE
            EndIf
        .Else
            mov eax &FALSE
        .EndIf
EndP


____________________________________________________________________________________________
____________________________________________________________________________________________

; Source Editor integration
____________________________________________________________________________________________
____________________________________________________________________________________________
;;
OldSourceDebugPos:
    mov ebx D$C.regEip | sub ebx D$DebugBaseOfCode

    mov esi D$IpTable, ecx D$IpTablePtr, D$StatementsCounter 0
    sub ecx esi | shr ecx 2

    If D$E.ExceptionCode = &EXCEPTION_BREAKPOINT
       ; mov W$DD_Dims 0C
    Else_If D$E.ExceptionCode = &EXCEPTION_SINGLE_STEP
       ; mov W$DD_Dims 0C
    Else
      ;  mov W$DD_Dims 0B |
      inc D$StatementsCounter              ; hide [Step]
    End_If


L0: lodsd | inc D$StatementsCounter | cmp eax ebx | je L2>
                                                    ja L1>
    loop L0<

    jmp L2> ; If last one, don't dec!!!

  ; ret  ; Abort if faultive statement  not found (????????!!!!!!!!!)
  ; or >>> point end of source ???

L1: dec D$StatementsCounter

L2: move D$StatementsPtr D$StatementsTable
    mov eax D$StatementsCounter | dec eax ;| dec eax |
    shl eax 2
    add D$StatementsPtr eax

    call SetEndOfErrorText | call SetDebuggeeText | call AskForRedraw
ret
;;
____________________________________________________________________________________________

;[ApiWarningDone: ?]

[SourcePos: ?]

;;
  Two parallel Tables:
  
* 'IpTable': Each Record is the Displacement for the Origin of Code (for each encoded
  Instruction. In case of Labels (That _are_ parsed by the Encoder, the Label and the
  relative Instruction are both recorded, with the same Displacement).
  
* 'StatementsTable': Each Record is a Pointer to Source Statement. For example, in cases
  of
  
  > mov eax 1, ebx 2, ecx 3
  
  there will be only one Pointer, in 'StatementsTable', that is a Pointer to this 'm'
  Char inside the user Source.
;;

[SourcePosCodeAddress: ?]

Proc SourceDebugPos:
    Arguments @CodeAddress

    call RestoreRealSource

    mov ebx D@CodeAddress | sub ebx D$DebugBaseOfCode

    mov esi D$IpTable, ecx D$IpTablePtr | sub ecx esi | shr ecx 2

    move D$StatementsPtr D$StatementsTable
;;
  Cases of critical error: C_regEip is not update (eip yet pointing to the faultive
  instruction).
  
  Cases of Int 3 and Stepping: The instruction is executed and C_regEip is updated
  (pointing to the next Instruction).
;;
    call ActualDebugPos

  ; eax = 'StatementsPtr' >>> Pointing to a Source Pos Record in 'StatementsTable'.
    mov eax D$eax

    mov D$SourcePos eax ; <<< for stepping

    .If eax <> 0
        ;mov B$ApiWarningDone &FALSE
L5:     call SetEndOfErrorText | call SetDebuggeeText | call AskForRedraw

    .Else_If D$DEBUG_EVENT+DEBUG_EVENT_EXCEPTION_DEBUG_INFO.pExceptionRecord.ExceptionCodeDis <> &EXCEPTION_SINGLE_STEP
      ; Error outside > Point to end of Source:
        sub D$StatementsPtr 4 | jmp L5<

    .End_If

    call SetPartialEditionFromPos
EndP

; Updated 27/05/2020
ActualDebugPos:
    .Do
        lodsd
        .If eax = ebx
            ; Cases of Labels in the 'IpTable':
            While D$esi = eax | add esi 4 | add D$StatementsPtr 4 | End_While
            jmp L2>
        .Else_If eax > ebx
            sub D$StatementsPtr 4 | jmp L2>
        .End_If
        add D$StatementsPtr 4
        dec ecx
    .Loop_Until ecx = 0
L2: mov eax D$StatementsPtr
    If eax < D$StatementsTable
        mov eax D$StatementsTable | add D$StatementsPtr 4
    End_If
ret
; Updated 27/05/2020
PreviousDebugPos:
    .Do
        lodsd
        .If eax => ebx
            sub D$StatementsPtr 4 | jmp L2>
        .End_If
        add D$StatementsPtr 4
        dec ecx
    .Loop_Until ecx = 0
L2:
    mov eax D$StatementsPtr
    If eax < D$StatementsTable
        mov eax D$StatementsTable | add D$StatementsPtr 4
    End_If
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

[CodeBuf: B$ ? #32]

NextInstructionDecode:
    call 'Kernel32.ReadProcessMemory' D$PI.hProcess, D$C.regEip, CodeBuf,
                                      32, NumberOfBytesRead
    call InstructionDecode CodeBuf
ret
____________________________________________________________________________________________

[InstructionLength: ?]
[DecodedInstruction: B$ ? #50]
[NextInstructionPtr: ?]

Proc InstructionDecode:
    Arguments @CodeBuffer

    pushad

    ; Clear buffer, set disassembler flags for simple decode and feed the disassembler
    ; with the code in the codebuffer.

    mov edi DecodedInstruction, ecx 50, eax 0
    rep stosb

    mov B$SimpleScan &TRUE, D$LastCodeRef 0
    mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
    mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
    mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
    mov B$WithCommentedHexa &FALSE

    mov esi D@CodeBuffer, edi DecodedInstruction
    add edi 10 ; disassembler sometimes writes in front of edi, reserve 10 guard bytes
L0: movzx eax B$esi | inc esi | call D$DisOp1+eax*4
    On B$DisFlag = DISDONE, jmp L0<
    mov D$edi 0
    sub esi D@CodeBuffer | mov D$InstructionLength esi
    mov B$SimpleScan &FALSE

  ; Dirty method to find the real beginning of the instruction string:
    .If D$LastCodeRef <> 0
        If B$edi-1 = '$'
            mov eax D$LastCodeRef | call WriteEax
        End_If
    .End_If

    mov edi DecodedInstruction, ecx 50, eax 0
    repe scasb | dec edi
L1: If B$edi = ' '
        inc edi | jmp L1<
    ElseIf B$edi = '|'
        inc edi | jmp L1<
    EndIf
    mov D$NextInstructionPtr edi

    popad

    mov eax D$NextInstructionPtr
EndP
____________________________________________________________________________________________

IsMultiStepInstruction:
    mov esi D$NextInstructionPtr
    lodsd
    If eax = 'call'
        mov eax 1 | ret
    EndIf
    and eax 0FF_FFFF
    If eax = 'rep'
        mov eax 1 | ret
    EndIf
    mov eax 0
ret
____________________________________________________________________________________________

[CPUName: B$ ? #48]
[CPUVendor: B$ ? #16]
[CPUFlags: ? CPUFlagsEx: ?]

[FLAG_FPU 1 FLAG_CMOV 0_8000 FLAG_MMX 080_0000 FLAG_SSE 0200_0000 FLAG_SSE2 0400_0000]
[FLAG_EX_MMX 040_0000 FLAG_EX_3DNOW 0_4000_0000 FLAG_EX_3DNOWEX 0_8000_0000]

Proc TestCpuFeatures:
    Local @MaxFunc @MaxExFunc

    mov D$CPUFlags 0, D$CPUFlagsEx 0, B$CPUName 0

    ; Check if CPUID instruction is available
    pushfd | pop eax
    mov ebx eax
    xor eax 0200000
    push eax | popfd
    pushfd | pop eax

    .If eax <> ebx
        mov eax 0
        cpuid
        mov D@MaxFunc eax
        mov D$CPUVendor ebx, D$CPUVendor+4 edx, D$CPUVendor+8 ecx, D$CPUVendor+12 0
        ; Get general flags
        If D@MaxFunc >= 1
            mov eax 1
            cpuid
            mov D$CPUFlags edx
        End_If

        mov eax 0_8000_0000
        cpuid
        mov D@MaxExFunc eax
        ; Extended flags
        If D@MaxExFunc >= 0_8000_0001
            mov eax 0_8000_0001
            cpuid
            mov D$CPUFlagsEx edx
        End_If
        ; CPU name
        If D@MaxExFunc >= 0_8000_0004
            mov eax 0_8000_0002
            cpuid
            mov D$CPUName eax, D$CPUName+4 ebx, D$CPUName+8 ecx, D$CPUName+12 edx
            mov eax 0_8000_0003
            cpuid
            mov D$CPUName+16 eax, D$CPUName+20 ebx, D$CPUName+24 ecx, D$CPUName+28 edx
            mov eax 0_8000_0004
            cpuid
            mov D$CPUName+32 eax, D$CPUName+36 ebx, D$CPUName+40 ecx, D$CPUName+44 edx
        End_If
    .End_If
EndP



























































