TITLE DETOURS_LIBRARY


;;
    Main functions:
    
        DetourCreateProcessWithDll
    
    Aditional functions:

        DetourUpdateProcessWithDll  EnumerateModulesInProcess
        UpdateImports32             PadToDwordPtr                       PadToDword
        FindAndAllocateNearBase     FinishUpdating                      StringCchCopy
        ReplaceOptionalSizeA        DetourVirtualProtectSameExecuteEx   DetourPageProtectAdjustExecute
        DetourCopyPayloadToProcess

        InjectDll

;;



[rlpDlls: D$ 0 #2]

[PROCESS_INFORMATION.hProcessDis 0
 PROCESS_INFORMATION.hThreadDis 4
 PROCESS_INFORMATION.dwProcessIdDis 8
 PROCESS_INFORMATION.dwThreadIdDis 12]

[Size_Of_PROCESS_INFORMATION 16]


;;
    Ex:
    call DetourCreateProcessWithDll DebuggeeExe, CommandLineString, &NULL, &NULL, &FALSE,
                                   &CREATE_DEFAULT_ERROR_MODE+&NORMAL_PRIORITY_CLASS+&DEBUG_PROCESS+&DEBUG_ONLY_THIS_PROCESS,
                                   &NULL, DebuggeePath, STARTUPINFO, PROCESS_INFORMATION,
                                   DllDebugName, &NULL    
;;

Proc DetourCreateProcessWithDll:
    Arguments @lpApplicationName, @lpCommandLine, @lpProcessAttributes, @lpThreadAttributes, @bInheritHandles,
              @dwCreationFlags, @lpEnvironment, @lpCurrentDirectory, @lpStartupInfo, @lpProcessInformation,
              @lpDllName, @pfCreateProcessA
    Local @dwMyCreationFlags, @nDlls
    Structure @PROCESS_INFORMATION 16, @PROCESS_INFORMATION.hProcessDis 0, @PROCESS_INFORMATION.hThreadDis 4,
                                       @PROCESS_INFORMATION.dwProcessIdDis 8, @PROCESS_INFORMATION.dwThreadIdDis 12
    Uses ebx, esi, edi


    mov eax D@dwCreationFlags | or eax &CREATE_SUSPENDED | mov D@dwMyCreationFlags eax
    If D@pfCreateProcessA = 0
        mov eax 'KERNEL32.CreateProcessA' | mov D@pfCreateProcessA eax
    End_If

    call D@pfCreateProcessA D@lpApplicationName, D@lpCommandLine, D@lpProcessAttributes, D@lpThreadAttributes,
                            D@bInheritHandles, D@dwMyCreationFlags, D@lpEnvironment, D@lpCurrentDirectory,
                            D@lpStartupInfo, D@PROCESS_INFORMATION ; <--- Error in Rosasm. right click on this variable and it will search for the global variable of it, instead doint nothing.
                                                                   ; This is becasue the structure name of the equate is the same one as the global variable
    On eax = 0, ExitP

    mov D@nDlls 0
    If D@lpDllName <> 0
        mov eax D@nDlls
        mov ecx D@lpDllName
        mov D$rlpDlls+eax*4 ecx
        inc D@nDlls
    End_If

    call DetourUpdateProcessWithDll D@PROCESS_INFORMATION.hProcessDis, rlpDlls, D@nDlls
    If eax = 0
        ; consider using call SafeTerminateProcess D$PI.hProcess, D$ExitCode
        call 'KERNEL32.TerminateProcess' D@PROCESS_INFORMATION.hProcessDis, 0-1
        xor eax eax
        ExitP
    End_If

    If D@lpProcessInformation <> 0
        C_call 'msvcrt.memcpy' D@lpProcessInformation, D@PROCESS_INFORMATION, Size_Of_PROCESS_INFORMATION
    End_If

    mov eax D@dwCreationFlags
    and eax &CREATE_SUSPENDED | jne L1>
        call 'KERNEL32.ResumeThread' D@PROCESS_INFORMATION.hThreadDis
    L1:
    mov eax &TRUE


EndP
_________________________________________________________________

[DETOUR_EXE_RESTORE_GUID: D$ 02ED7A3FF
                          W$ 03339
                          W$ 04A8D
                          B$ 080, 05C, 0D4, 098, 015, 03F, 0C2, 08F]


[IMAGE_NT_HEADERS:
 IMAGE_NT_HEADERS.Signature: D$ 0
 IMAGE_NT_HEADERS.FileHeader.Machine: W$ 0
 IMAGE_NT_HEADERS.FileHeader.NumberOfSections: W$ 0
 IMAGE_NT_HEADERS.FileHeader.TimeDateStamp: D$ 0
 IMAGE_NT_HEADERS.FileHeader.PointerToSymbolTable: D$ 0
 IMAGE_NT_HEADERS.FileHeader.NumberOfSymbols: D$ 0
 IMAGE_NT_HEADERS.FileHeader.SizeOfOptionalHeader: W$ 0
 IMAGE_NT_HEADERS.FileHeader.Characteristics: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.Magic: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.MajorLinkerVersion: B$ 0
 IMAGE_NT_HEADERS.OptionalHeader.MinorLinkerVersion: B$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfCode: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfInitializedData: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfUninitializedData: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.AddressOfEntryPoint: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.BaseOfCode: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.BaseOfData: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.ImageBase: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SectionAlignment: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.FileAlignment: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.MajorOperatingSystemVersion: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.MinorOperatingSystemVersion: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.MajorImageVersion: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.MinorImageVersion: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.MajorSubsystemVersion: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.MinorSubsystemVersion: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.Win32VersionValue: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfImage: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfHeaders: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.CheckSum: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.Subsystem: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.DllCharacteristics: W$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfStackReserve: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfStackCommit: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfHeapReserve: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfHeapCommit: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.LoaderFlags: D$ 0
 IMAGE_NT_HEADERS.OptionalHeader.NumberOfRvaAndSizes: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Export: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ExportSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Import: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ImportSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Resource: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ResourceSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Exception: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ExceptionSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Certificate: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.CertificateSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Relocation: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.RelocationSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Debug: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.DebugSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Architecture: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ArchitectureSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.GPReg: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.GPRegSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Thread: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ThreadSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ConfigTable: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ConfigTableSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.BoundIAT: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.BoundIATSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.IAT: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.IATSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.DelayID: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.DelayIDSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.COM: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.COMSize: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.Reserved: D$ 0
 IMAGE_NT_HEADERS.DataDirectory.ReservedSize: D$ 0]

[IMAGE_NT_HEADERS.SignatureDis 0
 IMAGE_NT_HEADERS.FileHeader.MachineDis 4
 IMAGE_NT_HEADERS.FileHeader.NumberOfSectionsDis 6
 IMAGE_NT_HEADERS.FileHeader.TimeDateStampDis 8
 IMAGE_NT_HEADERS.FileHeader.PointerToSymbolTableDis 12
 IMAGE_NT_HEADERS.FileHeader.NumberOfSymbolsDis 16
 IMAGE_NT_HEADERS.FileHeader.SizeOfOptionalHeaderDis 20
 IMAGE_NT_HEADERS.FileHeader.CharacteristicsDis 22
 IMAGE_NT_HEADERS.OptionalHeader.MagicDis 24
 IMAGE_NT_HEADERS.OptionalHeader.MajorLinkerVersionDis 26
 IMAGE_NT_HEADERS.OptionalHeader.MinorLinkerVersionDis 27
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfCodeDis 28
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfInitializedDataDis 32
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfUninitializedDataDis 36
 IMAGE_NT_HEADERS.OptionalHeader.AddressOfEntryPointDis 40
 IMAGE_NT_HEADERS.OptionalHeader.BaseOfCodeDis 44
 IMAGE_NT_HEADERS.OptionalHeader.BaseOfDataDis 48
 IMAGE_NT_HEADERS.OptionalHeader.ImageBaseDis 52
 IMAGE_NT_HEADERS.OptionalHeader.SectionAlignmentDis 56
 IMAGE_NT_HEADERS.OptionalHeader.FileAlignmentDis 60
 IMAGE_NT_HEADERS.OptionalHeader.MajorOperatingSystemVersionDis 64
 IMAGE_NT_HEADERS.OptionalHeader.MinorOperatingSystemVersionDis 66
 IMAGE_NT_HEADERS.OptionalHeader.MajorImageVersionDis 68
 IMAGE_NT_HEADERS.OptionalHeader.MinorImageVersionDis 70
 IMAGE_NT_HEADERS.OptionalHeader.MajorSubsystemVersionDis 72
 IMAGE_NT_HEADERS.OptionalHeader.MinorSubsystemVersionDis 74
 IMAGE_NT_HEADERS.OptionalHeader.Win32VersionValueDis 76
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfImageDis 80
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfHeadersDis 84
 IMAGE_NT_HEADERS.OptionalHeader.CheckSumDis 88
 IMAGE_NT_HEADERS.OptionalHeader.SubsystemDis 92
 IMAGE_NT_HEADERS.OptionalHeader.DllCharacteristicsDis 94
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfStackReserveDis 96
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfStackCommitDis 100
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfHeapReserveDis 104
 IMAGE_NT_HEADERS.OptionalHeader.SizeOfHeapCommitDis 108
 IMAGE_NT_HEADERS.OptionalHeader.LoaderFlagsDis 112
 IMAGE_NT_HEADERS.OptionalHeader.NumberOfRvaAndSizesDis 116
 IMAGE_NT_HEADERS.DataDirectory.ExportDis 120
 IMAGE_NT_HEADERS.DataDirectory.ExportSizeDis 124
 IMAGE_NT_HEADERS.DataDirectory.ImportDis 128
 IMAGE_NT_HEADERS.DataDirectory.ImportSizeDis 132
 IMAGE_NT_HEADERS.DataDirectory.ResourceDis 136
 IMAGE_NT_HEADERS.DataDirectory.ResourceSizeDis 140
 IMAGE_NT_HEADERS.DataDirectory.ExceptionDis 144
 IMAGE_NT_HEADERS.DataDirectory.ExceptionSizeDis 148
 IMAGE_NT_HEADERS.DataDirectory.CertificateDis 152
 IMAGE_NT_HEADERS.DataDirectory.CertificateSizeDis 156
 IMAGE_NT_HEADERS.DataDirectory.RelocationDis 160
 IMAGE_NT_HEADERS.DataDirectory.RelocationSizeDis 164
 IMAGE_NT_HEADERS.DataDirectory.DebugDis 168
 IMAGE_NT_HEADERS.DataDirectory.DebugSizeDis 172
 IMAGE_NT_HEADERS.DataDirectory.ArchitectureDis 176
 IMAGE_NT_HEADERS.DataDirectory.ArchitectureSizeDis 180
 IMAGE_NT_HEADERS.DataDirectory.GPRegDis 184
 IMAGE_NT_HEADERS.DataDirectory.GPRegSizeDis 188
 IMAGE_NT_HEADERS.DataDirectory.ThreadDis 192
 IMAGE_NT_HEADERS.DataDirectory.ThreadSizeDis 196
 IMAGE_NT_HEADERS.DataDirectory.ConfigTableDis 200
 IMAGE_NT_HEADERS.DataDirectory.ConfigTableSizeDis 204
 IMAGE_NT_HEADERS.DataDirectory.BoundIATDis 208
 IMAGE_NT_HEADERS.DataDirectory.BoundIATSizeDis 212
 IMAGE_NT_HEADERS.DataDirectory.IATDis 216
 IMAGE_NT_HEADERS.DataDirectory.IATSizeDis 220
 IMAGE_NT_HEADERS.DataDirectory.DelayIDDis 224
 IMAGE_NT_HEADERS.DataDirectory.DelayIDSizeDis 228
 IMAGE_NT_HEADERS.DataDirectory.COMDis 232
 IMAGE_NT_HEADERS.DataDirectory.COMSizeDis 236
 IMAGE_NT_HEADERS.DataDirectory.ReservedDis 240
 IMAGE_NT_HEADERS.DataDirectory.ReservedSizeDis 244]

[Size_Of_IMAGE_NT_HEADERS 248]


[IMAGE_NT_HEADERS64.SignatureDis 0
 IMAGE_NT_HEADERS64.FileHeader.MachineDis 4
 IMAGE_NT_HEADERS64.FileHeader.NumberOfSectionsDis 6
 IMAGE_NT_HEADERS64.FileHeader.TimeDateStampDis 8
 IMAGE_NT_HEADERS64.FileHeader.PointerToSymbolTableDis 12
 IMAGE_NT_HEADERS64.FileHeader.NumberOfSymbolsDis 16
 IMAGE_NT_HEADERS64.FileHeader.SizeOfOptionalHeaderDis 20
 IMAGE_NT_HEADERS64.FileHeader.CharacteristicsDis 22
 IMAGE_NT_HEADERS64.OptionalHeader.MagicDis 24
 IMAGE_NT_HEADERS64.OptionalHeader.MajorLinkerVersionDis 26
 IMAGE_NT_HEADERS64.OptionalHeader.MinorLinkerVersionDis 27
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfCodeDis 28
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfInitializedDataDis 32
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfUninitializedDataDis 36
 IMAGE_NT_HEADERS64.OptionalHeader.AddressOfEntryPointDis 40
 IMAGE_NT_HEADERS64.OptionalHeader.BaseOfCodeDis 44
 IMAGE_NT_HEADERS64.OptionalHeader.ImageBaseDis 48
 IMAGE_NT_HEADERS64.OptionalHeader.SectionAlignmentDis 56
 IMAGE_NT_HEADERS64.OptionalHeader.FileAlignmentDis 60
 IMAGE_NT_HEADERS64.OptionalHeader.MajorOperatingSystemVersionDis 64
 IMAGE_NT_HEADERS64.OptionalHeader.MinorOperatingSystemVersionDis 66
 IMAGE_NT_HEADERS64.OptionalHeader.MajorImageVersionDis 68
 IMAGE_NT_HEADERS64.OptionalHeader.MinorImageVersionDis 70
 IMAGE_NT_HEADERS64.OptionalHeader.MajorSubsystemVersionDis 72
 IMAGE_NT_HEADERS64.OptionalHeader.MinorSubsystemVersionDis 74
 IMAGE_NT_HEADERS64.OptionalHeader.Win32VersionValueDis 76
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfImageDis 80
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfHeadersDis 84
 IMAGE_NT_HEADERS64.OptionalHeader.CheckSumDis 88
 IMAGE_NT_HEADERS64.OptionalHeader.SubsystemDis 92
 IMAGE_NT_HEADERS64.OptionalHeader.DllCharacteristicsDis 94
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfStackReserveDis 96
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfStackCommitDis 104
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfHeapReserveDis 112
 IMAGE_NT_HEADERS64.OptionalHeader.SizeOfHeapCommitDis 120
 IMAGE_NT_HEADERS64.OptionalHeader.LoaderFlagsDis 128
 IMAGE_NT_HEADERS64.OptionalHeader.NumberOfRvaAndSizesDis 132
 IMAGE_NT_HEADERS64.DataDirectory.ExportDis 136
 IMAGE_NT_HEADERS64.DataDirectory.ExportSizeDis 140
 IMAGE_NT_HEADERS64.DataDirectory.ImportDis 144
 IMAGE_NT_HEADERS64.DataDirectory.ImportSizeDis 148
 IMAGE_NT_HEADERS64.DataDirectory.ResourceDis 152
 IMAGE_NT_HEADERS64.DataDirectory.ResourceSizeDis 156
 IMAGE_NT_HEADERS64.DataDirectory.ExceptionDis 160
 IMAGE_NT_HEADERS64.DataDirectory.ExceptionSizeDis 164
 IMAGE_NT_HEADERS64.DataDirectory.CertificateDis 168
 IMAGE_NT_HEADERS64.DataDirectory.CertificateSizeDis 172
 IMAGE_NT_HEADERS64.DataDirectory.RelocationDis 176
 IMAGE_NT_HEADERS64.DataDirectory.RelocationSizeDis 180
 IMAGE_NT_HEADERS64.DataDirectory.DebugDis 184
 IMAGE_NT_HEADERS64.DataDirectory.DebugSizeDis 188
 IMAGE_NT_HEADERS64.DataDirectory.ArchitectureDis 192
 IMAGE_NT_HEADERS64.DataDirectory.ArchitectureSizeDis 196
 IMAGE_NT_HEADERS64.DataDirectory.GPRegDis 200
 IMAGE_NT_HEADERS64.DataDirectory.GPRegSizeDis 204
 IMAGE_NT_HEADERS64.DataDirectory.ThreadDis 208
 IMAGE_NT_HEADERS64.DataDirectory.ThreadSizeDis 212
 IMAGE_NT_HEADERS64.DataDirectory.ConfigTableDis 216
 IMAGE_NT_HEADERS64.DataDirectory.ConfigTableSizeDis 220
 IMAGE_NT_HEADERS64.DataDirectory.BoundIATDis 224
 IMAGE_NT_HEADERS64.DataDirectory.BoundIATSizeDis 228
 IMAGE_NT_HEADERS64.DataDirectory.IATDis 232
 IMAGE_NT_HEADERS64.DataDirectory.IATSizeDis 236
 IMAGE_NT_HEADERS64.DataDirectory.DelayIDDis 240
 IMAGE_NT_HEADERS64.DataDirectory.DelayIDSizeDis 244
 IMAGE_NT_HEADERS64.DataDirectory.COMDis 248
 IMAGE_NT_HEADERS64.DataDirectory.COMSizeDis 252
 IMAGE_NT_HEADERS64.DataDirectory.ReservedDis 256
 IMAGE_NT_HEADERS64.DataDirectory.ReservedSizeDis 260]

[Size_Of_IMAGE_NT_HEADERS64 264]

[IMAGE_SECTION_HEADER:
 IMAGE_SECTION_HEADER.Name1: B$ 0 #&IMAGE_SIZEOF_SHORT_NAME
 IMAGE_SECTION_HEADER.MiscPhysicalAddress: D$ 0
 IMAGE_SECTION_HEADER.VirtualAddress: D$ 0
 IMAGE_SECTION_HEADER.SizeOfRawData: D$ 0
 IMAGE_SECTION_HEADER.PointerToRawData: D$ 0
 IMAGE_SECTION_HEADER.PointerToRelocations: D$ 0
 IMAGE_SECTION_HEADER.PointerToLinenumbers: D$ 0
 IMAGE_SECTION_HEADER.NumberOfRelocations: W$ 0
 IMAGE_SECTION_HEADER.NumberOfLinenumbers: W$ 0
 IMAGE_SECTION_HEADER.Characteristics: D$ 0]

[IMAGE_SECTION_HEADER.Name1Dis 0
 IMAGE_SECTION_HEADER.MiscPhysicalAddressDis 8
 IMAGE_SECTION_HEADER.MiscVirtualSizeDis 8
 IMAGE_SECTION_HEADER.VirtualAddressDis 12
 IMAGE_SECTION_HEADER.SizeOfRawDataDis 16
 IMAGE_SECTION_HEADER.PointerToRawDataDis 20
 IMAGE_SECTION_HEADER.PointerToRelocationsDis 24
 IMAGE_SECTION_HEADER.PointerToLinenumbersDis 28
 IMAGE_SECTION_HEADER.NumberOfRelocationsDis 32
 IMAGE_SECTION_HEADER.NumberOfLinenumbersDis 34
 IMAGE_SECTION_HEADER.CharacteristicsDis 36]

[Size_Of_IMAGE_SECTION_HEADER 40]


[IMAGE_DOS_HEADER:
 IMAGE_DOS_HEADER.e_magic: W$ 0
 IMAGE_DOS_HEADER.e_cblp: W$ 0
 IMAGE_DOS_HEADER.e_cp: W$ 0
 IMAGE_DOS_HEADER.e_crlc: W$ 0
 IMAGE_DOS_HEADER.e_cparhdr: W$ 0
 IMAGE_DOS_HEADER.e_minalloc: W$ 0
 IMAGE_DOS_HEADER.e_maxalloc: W$ 0
 IMAGE_DOS_HEADER.e_ss: W$ 0
 IMAGE_DOS_HEADER.e_sp: W$ 0
 IMAGE_DOS_HEADER.e_csum: W$ 0
 IMAGE_DOS_HEADER.e_ip: W$ 0
 IMAGE_DOS_HEADER.e_cs: W$ 0
 IMAGE_DOS_HEADER.e_lfarlc: W$ 0
 IMAGE_DOS_HEADER.e_ovno: W$ 0
 IMAGE_DOS_HEADER.e_res: W$ 0 #4
 IMAGE_DOS_HEADER.e_oemid: W$ 0
 IMAGE_DOS_HEADER.e_oeminfo: W$ 0
 IMAGE_DOS_HEADER.e_res2: W$ 0 #10
 IMAGE_DOS_HEADER.e_lfanew: D$ 0]

[Size_of_IMAGE_DOS_HEADER 64]

;;
[IMAGE_IMPORT_DESCRIPTOR.OriginalFirstThunkDis 0
 IMAGE_IMPORT_DESCRIPTOR.TimeDateStampDis 4
 IMAGE_IMPORT_DESCRIPTOR.ForwarderChainDis 8
 IMAGE_IMPORT_DESCRIPTOR.Name1Dis 12
 IMAGE_IMPORT_DESCRIPTOR.FirstThunkDis 16]

[Size_Of_IMAGE_IMPORT_DESCRIPTOR 20]
;;
;;

typedef struct _DETOUR_CLR_HEADER
{
    // Header versioning
    ULONG                   cb;
    USHORT                  MajorRuntimeVersion;
    USHORT                  MinorRuntimeVersion;

    // Symbol table and startup information
    IMAGE_DATA_DIRECTORY    MetaData;
    ULONG                   Flags;

    // Followed by the rest of the IMAGE_COR20_HEADER
} DETOUR_CLR_HEADER, *PDETOUR_CLR_HEADER;


For info. The CLR_HEADRER stucture have this format in In CorHdr.h or WinNT.h,

// CLR 2.0 header structure.
typedef struct IMAGE_COR20_HEADER
{
    // Header versioning
    ULONG cb;
    USHORT MajorRuntimeVersion;
    USHORT MinorRuntimeVersion;
    
    // Symbol table and startup information
    IMAGE_DATA_DIRECTORY MetaData;
    ULONG Flags;
    ULONG EntryPointToken;
    
    // Binding information
    IMAGE_DATA_DIRECTORY Resources;
    IMAGE_DATA_DIRECTORY StrongNameSignature;
    
    // Regular fixup and binding information
    IMAGE_DATA_DIRECTORY CodeManagerTable;
    IMAGE_DATA_DIRECTORY VTableFixups;
    IMAGE_DATA_DIRECTORY ExportAddressTableJumps;
    
    // Precompiled image info (internal use only - set to zero)
    IMAGE_DATA_DIRECTORY ManagedNativeHeader;
} IMAGE_COR20_HEADER;

;;


[DETOUR_CLR_HEADER:
 DETOUR_CLR_HEADER.cb: D$ 0
 DETOUR_CLR_HEADER.MajorRuntimeVersion: W$ 0
 DETOUR_CLR_HEADER.MinorRuntimeVersion: W$ 0
 DETOUR_CLR_HEADER.Metadata.VirtualAddress: D$ 0
 DETOUR_CLR_HEADER.Metadata.isize: D$ 0
 DETOUR_CLR_HEADER.Flags: D$ 0]

[Size_of_DETOUR_CLR_HEADER 20]

;;
typedef struct _DETOUR_EXE_RESTORE
{
    DWORD               cb;
    DWORD               cbidh;
    DWORD               cbinh;
    DWORD               cbclr;

    PBYTE               pidh;
    PBYTE               pinh;
    PBYTE               pclr;

    IMAGE_DOS_HEADER    idh;
    union {
        IMAGE_NT_HEADERS    inh;
        IMAGE_NT_HEADERS32  inh32;
        IMAGE_NT_HEADERS64  inh64;
        BYTE                raw[sizeof(IMAGE_NT_HEADERS64) +
                                sizeof(IMAGE_SECTION_HEADER) * 32];
    };
    DETOUR_CLR_HEADER   clr;

} DETOUR_EXE_RESTORE, *PDETOUR_EXE_RESTORE;
;;


[DETOUR_EXE_RESTORE:
 DETOUR_EXE_RESTORE.cb: D$ len
 DETOUR_EXE_RESTORE.cbidh: D$ Size_of_IMAGE_DOS_HEADER
 DETOUR_EXE_RESTORE.cbinh: D$ 0
 DETOUR_EXE_RESTORE.cbclr: D$ 0
 DETOUR_EXE_RESTORE.pidh: D$ 0
 DETOUR_EXE_RESTORE.pinh: D$ 0
 DETOUR_EXE_RESTORE.pclr: D$ 0
 DETOUR_EXE_RESTORE.idh.e_magic: W$ 0
 DETOUR_EXE_RESTORE.idh.e_cblp: W$ 0
 DETOUR_EXE_RESTORE.idh.e_cp: W$ 0
 DETOUR_EXE_RESTORE.idh.e_crlc: W$ 0
 DETOUR_EXE_RESTORE.idh.e_cparhdr: W$ 0
 DETOUR_EXE_RESTORE.idh.e_minalloc: W$ 0
 DETOUR_EXE_RESTORE.idh.e_maxalloc: W$ 0
 DETOUR_EXE_RESTORE.idh.e_ss: W$ 0
 DETOUR_EXE_RESTORE.idh.e_sp: W$ 0
 DETOUR_EXE_RESTORE.idh.e_csum: W$ 0
 DETOUR_EXE_RESTORE.idh.e_ip: W$ 0
 DETOUR_EXE_RESTORE.idh.e_cs: W$ 0
 DETOUR_EXE_RESTORE.idh.e_lfarlc: W$ 0
 DETOUR_EXE_RESTORE.idh.e_ovno: W$ 0
 DETOUR_EXE_RESTORE.idh.e_res: W$ 0 #4
 DETOUR_EXE_RESTORE.idh.e_oemid: W$ 0
 DETOUR_EXE_RESTORE.idh.e_oeminfo: W$ 0
 DETOUR_EXE_RESTORE.idh.e_res2: W$ 0 #10
 DETOUR_EXE_RESTORE.idh.e_lfanew: D$ 0
 DETOUR_EXE_RESTORE.Byte: B$ 0 #(Size_Of_IMAGE_NT_HEADERS64+(Size_Of_IMAGE_SECTION_HEADER*32))
 DETOUR_EXE_RESTORE.clr.cb: D$ 0
 DETOUR_EXE_RESTORE.clr.MajorRuntimeVersion: W$ 0
 DETOUR_EXE_RESTORE.clr.MinorRuntimeVersion: W$ 0
 DETOUR_EXE_RESTORE.clr.Metadata.VirtualAddress: D$ 0
 DETOUR_EXE_RESTORE.clr.Metadata.isize: D$ 0
 DETOUR_EXE_RESTORE.clr.Flags: D$ 0]

[DETOUR_EXE_RESTORE.cbDis 0
 DETOUR_EXE_RESTORE.cbidhDis 4
 DETOUR_EXE_RESTORE.cbinhDis 8
 DETOUR_EXE_RESTORE.cbclrDis 12
 DETOUR_EXE_RESTORE.pidhDis 16
 DETOUR_EXE_RESTORE.pinhDis 20
 DETOUR_EXE_RESTORE.pclrDis 24
 DETOUR_EXE_RESTORE.idh.e_magicDis 28
 DETOUR_EXE_RESTORE.idh.e_cblpDis 30
 DETOUR_EXE_RESTORE.idh.e_cpDis 32
 DETOUR_EXE_RESTORE.idh.e_crlcDis 34
 DETOUR_EXE_RESTORE.idh.e_cparhdrDis 36
 DETOUR_EXE_RESTORE.idh.e_minallocDis 38
 DETOUR_EXE_RESTORE.idh.e_maxallocDis 40
 DETOUR_EXE_RESTORE.idh.e_ssDis 42
 DETOUR_EXE_RESTORE.idh.e_spDis 44
 DETOUR_EXE_RESTORE.idh.e_csumDis 46
 DETOUR_EXE_RESTORE.idh.e_ipDis 48
 DETOUR_EXE_RESTORE.idh.e_csDis 50
 DETOUR_EXE_RESTORE.idh.e_lfarlcDis 52
 DETOUR_EXE_RESTORE.idh.e_ovnoDis 54
 DETOUR_EXE_RESTORE.idh.e_resDis 56
 DETOUR_EXE_RESTORE.idh.e_oemidDis 64
 DETOUR_EXE_RESTORE.idh.e_oeminfoDis 66
 DETOUR_EXE_RESTORE.idh.e_res2Dis 68
 DETOUR_EXE_RESTORE.idh.e_lfanewDis 88
 DETOUR_EXE_RESTORE.ByteDis 92
 DETOUR_EXE_RESTORE.clr.cbDis 1636
 DETOUR_EXE_RESTORE.clr.MajorRuntimeVersionDis 1640
 DETOUR_EXE_RESTORE.clr.MinorRuntimeVersionDis 1642
 DETOUR_EXE_RESTORE.clr.Metadata.VirtualAddressDis 1644
 DETOUR_EXE_RESTORE.clr.Metadata.isizeDis 1648
 DETOUR_EXE_RESTORE.clr.FlagsDis 1652]

[Size_Of_DETOUR_EXE_RESTORE 1656]

[Size_of_DETOUR_EXE_RESTORE.Byte (Size_Of_IMAGE_NT_HEADERS64+(Size_Of_IMAGE_SECTION_HEADER*32))]

Proc DetourUpdateProcessWithDll:
    Arguments @hProcess, @plpDlls, @nDlls
    Local @mach32Bit, @mach64Bit, @exe32Bit, @hModule, @hLast, @dwProtect
    Uses ebx, esi, edi

    mov D@mach32Bit 0
    mov D@mach64Bit 0
    mov D@exe32Bit 0
    mov D@hModule 0
    mov D@hLast 0

    call EnumerateModulesInProcess D@hProcess, D@hLast, IMAGE_NT_HEADERS
    mov D@hLast eax
    .While D@hLast <> 0
        .Test_If W$IMAGE_NT_HEADERS.FileHeader.Characteristics &IMAGE_FILE_DLL
            If W$IMAGE_NT_HEADERS.OptionalHeader.Magic = &IMAGE_NT_OPTIONAL_HDR32_MAGIC
                movzx eax W$IMAGE_NT_HEADERS.FileHeader.Machine | mov D@mach32Bit eax
            Else_If W$IMAGE_NT_HEADERS.OptionalHeader.Magic = &IMAGE_NT_OPTIONAL_HDR64_MAGIC
                movzx eax W$IMAGE_NT_HEADERS.FileHeader.Machine | mov D@mach64Bit eax
            End_If
        .Test_Else
            mov eax D@hLast | mov D@hModule eax
            If W$IMAGE_NT_HEADERS.OptionalHeader.Magic = &IMAGE_NT_OPTIONAL_HDR32_MAGIC
                movzx eax W$IMAGE_NT_HEADERS.FileHeader.Machine | mov D@exe32Bit eax
            End_If
        .Test_End
        call EnumerateModulesInProcess D@hProcess, D@hLast, IMAGE_NT_HEADERS
        mov D@hLast eax
    .End_While

    If D@hModule = 0
        call 'KERNEL32.SetLastError' &ERROR_INVALID_OPERATION
        xor eax eax
        ExitP
    End_If

    ; Save the various headers for DetourRestoreAfterWith.

    C_call 'msvcrt.memset' DETOUR_EXE_RESTORE, 0, Size_Of_DETOUR_EXE_RESTORE
    mov D$DETOUR_EXE_RESTORE.cb Size_Of_DETOUR_EXE_RESTORE
    mov eax D@hModule | mov D$DETOUR_EXE_RESTORE.pidh eax
    call 'KERNEL32.ReadProcessMemory' D@hProcess, D$DETOUR_EXE_RESTORE.pidh, DETOUR_EXE_RESTORE.idh.e_magic,
                                      Size_of_IMAGE_DOS_HEADER, &NULL
    On eax = 0, ExitP


    ; We read the NT header in two passes to get the full size.
    ; First we read just the Signature and FileHeader.

    mov eax D$DETOUR_EXE_RESTORE.pidh | add eax D$DETOUR_EXE_RESTORE.idh.e_lfanew
    mov D$DETOUR_EXE_RESTORE.pinh eax
    mov D$DETOUR_EXE_RESTORE.cbinh IMAGE_NT_HEADERS.OptionalHeader.MagicDis
    call 'KERNEL32.ReadProcessMemory' D@hProcess, D$DETOUR_EXE_RESTORE.pinh, DETOUR_EXE_RESTORE.Byte,
                                      D$DETOUR_EXE_RESTORE.cbinh, &NULL
    On eax = 0, ExitP

    ; Second we read the OptionalHeader and Section headers.
    movzx eax W$DETOUR_EXE_RESTORE.Byte+IMAGE_NT_HEADERS.FileHeader.SizeOfOptionalHeaderDis
    movzx ecx W$DETOUR_EXE_RESTORE.Byte+IMAGE_NT_HEADERS.FileHeader.NumberOfSectionsDis
    imul ecx ecx Size_Of_IMAGE_SECTION_HEADER
    lea edx D$eax+ecx+IMAGE_NT_HEADERS.OptionalHeader.MagicDis | mov D$DETOUR_EXE_RESTORE.cbinh edx

    If D$DETOUR_EXE_RESTORE.cbinh > Size_of_DETOUR_EXE_RESTORE.Byte
        xor eax eax
        ExitP
    End_If

    call 'KERNEL32.ReadProcessMemory' D@hProcess, D$DETOUR_EXE_RESTORE.pinh, DETOUR_EXE_RESTORE.Byte,
                                      D$DETOUR_EXE_RESTORE.cbinh, &NULL
    On eax = 0, ExitP

    ; Third, we read the CLR header
    If W$DETOUR_EXE_RESTORE.Byte+IMAGE_NT_HEADERS.OptionalHeader.MagicDis = &IMAGE_NT_OPTIONAL_HDR32_MAGIC
        mov eax D@hModule | add eax D$DETOUR_EXE_RESTORE+IMAGE_NT_HEADERS.DataDirectory.COMDis
        mov D$DETOUR_EXE_RESTORE.pclr eax
    Else_If W$DETOUR_EXE_RESTORE.Byte+IMAGE_NT_HEADERS64.OptionalHeader.MagicDis = &IMAGE_NT_OPTIONAL_HDR64_MAGIC
        mov eax D@hModule | add eax D$DETOUR_EXE_RESTORE+IMAGE_NT_HEADERS64.DataDirectory.COMDis
        mov D$DETOUR_EXE_RESTORE.pclr eax
    End_If

    If D$DETOUR_EXE_RESTORE.pclr <> 0
        mov D$DETOUR_EXE_RESTORE.cbclr Size_of_DETOUR_CLR_HEADER
        call 'KERNEL32.ReadProcessMemory' D@hProcess, D$DETOUR_EXE_RESTORE.pclr, DETOUR_EXE_RESTORE.clr.cb,
                                      D$DETOUR_EXE_RESTORE.cbclr, &NULL
        On eax = 0, ExitP
    End_If

    ; Fourth, adjust for a 32-bit WOW64 process.

    .If_And D@exe32Bit <> 0, D@mach64Bit <> 0

        If D$DETOUR_EXE_RESTORE.pclr <> 0
            .Test_If D$DETOUR_EXE_RESTORE.clr.Flags &COMIMAGE_FLAGS_ILONLY
                Test_If_Not D$DETOUR_EXE_RESTORE.clr.Flags &COMIMAGE_FLAGS_32BITREQUIRED
                    jmp L2>
                Test_end
            .Test_End
        End_If
        mov D@mach64Bit 0
        If D@mach32Bit = 0
            mov eax D@exe32Bit | mov D@mach32Bit eax
        End_If
L2:
    .End_If

    ; Now decide if we can insert the detour.

    ; 64-bit native or 64-bit managed process.
    ; Can't detour a 64-bit process with 32-bit code.
    ; Note: This happens for 32-bit PE binaries containing only
    ; manage code that have been marked as 64-bit ready.

    If_Or D@mach32Bit = 0, D@mach64Bit <> 0 ; the source code is incorrect. it is a OR  and not an AND
        call 'KERNEL32.SetLastError' &ERROR_INVALID_HANDLE
        xor eax eax | ExitP
    ;End_If
    Else_If D@mach32Bit <> 0
        ; 32-bit native or 32-bit managed process on any platform.
        call UpdateImports32 D@hProcess, D@hModule, D@plpDlls, D@nDlls
        On eax = 0, ExitP
    Else
        ; Who knows!?
        call 'KERNEL32.SetLastError' &ERROR_INVALID_HANDLE
        xor eax eax | ExitP
    End_If

    ; Update the CLR header.

    .If D$DETOUR_EXE_RESTORE.pclr <> 0
        C_call 'msvcrt.memcpy' DETOUR_CLR_HEADER, DETOUR_EXE_RESTORE.clr.cb, Size_of_DETOUR_CLR_HEADER

        ; Clear the IL_ONLY flag.
        mov eax D$DETOUR_CLR_HEADER.Flags ; Error here. it is turning to 0 ?
        and eax (not &COMIMAGE_FLAGS_ILONLY);0-02
        mov D$DETOUR_CLR_HEADER.Flags eax

        lea eax D@dwProtect
        call 'KERNEL32.VirtualProtectEx' D@hProcess, D$DETOUR_EXE_RESTORE.pclr, Size_of_DETOUR_CLR_HEADER,
                                         &PAGE_READWRITE, eax
        On eax = 0, ExitP
        call 'KERNEL32.WriteProcessMemory' D@hProcess, D$DETOUR_EXE_RESTORE.pclr,
                                           DETOUR_CLR_HEADER, Size_of_DETOUR_CLR_HEADER, &NULL
        On eax = 0, ExitP
        lea eax D@dwProtect
        call 'KERNEL32.VirtualProtectEx' D@hProcess, D$DETOUR_EXE_RESTORE.pclr,
                                         Size_of_DETOUR_CLR_HEADER, D@dwProtect, eax
        On eax = 0, ExitP
    .End_If

    ; Save the undo data to the target process.
    call DetourCopyPayloadToProcess D@hProcess, DETOUR_EXE_RESTORE_GUID, DETOUR_EXE_RESTORE, Size_Of_DETOUR_EXE_RESTORE
    If eax <> 0 ; not needed. the function returns tru or false already
        mov eax &TRUE
    End_If

EndP

__________________________________________

; Enumerate through modules in the target process.
;;
[MEMORY_BASIC_INFORMATION.BaseAddressDis 0
 MEMORY_BASIC_INFORMATION.AllocationBaseDis 4
 MEMORY_BASIC_INFORMATION.AllocationProtectDis 8
 MEMORY_BASIC_INFORMATION.RegionSizeDis 12
 MEMORY_BASIC_INFORMATION.StateDis 16
 MEMORY_BASIC_INFORMATION.ProtectDis 20
 MEMORY_BASIC_INFORMATION.lTypeDis 24]

[Size_Of_MEMORY_BASIC_INFORMATION 28]
;;

Proc EnumerateModulesInProcess:
    Arguments @hProcess, @hModuleLast, @pNtHeader
    Local @pbLast
    Structure @MEMORY_BASIC_INFORMATION 28, @MEMORY_BASIC_INFORMATION.BaseAddressDis 0,
                                            @MEMORY_BASIC_INFORMATION.AllocationBaseDis 4,
                                            @MEMORY_BASIC_INFORMATION.AllocationProtectDis 8,
                                            @MEMORY_BASIC_INFORMATION.RegionSizeDis 12,
                                            @MEMORY_BASIC_INFORMATION.StateDis 16,
                                            @MEMORY_BASIC_INFORMATION.ProtectDis 20,
                                            @MEMORY_BASIC_INFORMATION.lTypeDis 24
    Uses ebx, esi, edi

    If D@hModuleLast = 0
        mov D@pbLast 010000
    Else
        mov eax D@hModuleLast | add eax 010000 | mov D@pbLast eax
    End_If

    C_call 'msvcrt.memset' D@MEMORY_BASIC_INFORMATION, 0, Size_Of_MEMORY_BASIC_INFORMATION
    call 'KERNEL32.VirtualQueryEx' D@hProcess, D@pbLast, D@MEMORY_BASIC_INFORMATION, Size_Of_MEMORY_BASIC_INFORMATION

    .While eax <> 0
        mov eax D@MEMORY_BASIC_INFORMATION.RegionSizeDis | and eax 0FFF
        mov ebx D@MEMORY_BASIC_INFORMATION.BaseAddressDis | add ebx D@MEMORY_BASIC_INFORMATION.RegionSizeDis
        If_Or eax = 0FFF, ebx < D@pbLast
            xor eax eax | ExitP
        End_If

        ; Skip uncommitted regions and guard pages.
        ..Test_If D@MEMORY_BASIC_INFORMATION.StateDis &MEM_COMMIT ; 010 020
            .Test_If D@MEMORY_BASIC_INFORMATION.ProtectDis &PAGE_GUARD__&PAGE_NOACCESS
            .Test_Else
                call 'KERNEL32.ReadProcessMemory' D@hProcess, D@pbLast, IMAGE_DOS_HEADER, Size_of_IMAGE_DOS_HEADER, &NULL
                ;mov edx D@MEMORY_BASIC_INFORMATION.RegionSizeDis
                mov ebx D$IMAGE_DOS_HEADER.e_lfanew
                ;.If_And eax <> 0, W$IMAGE_DOS_HEADER.e_magic = &IMAGE_DOS_SIGNATURE, ebx >= Size_of_IMAGE_DOS_HEADER, ebx <= edx;D@MEMORY_BASIC_INFORMATION.RegionSizeDis
                ;.If_And eax <> 0, W$IMAGE_DOS_HEADER.e_magic = &IMAGE_DOS_SIGNATURE_OLD_VERSION, ebx >= Size_of_IMAGE_DOS_HEADER, ebx <= D@MEMORY_BASIC_INFORMATION.RegionSizeDis
                ..If_And eax <> 0, ebx >= Size_of_IMAGE_DOS_HEADER, ebx <= D@MEMORY_BASIC_INFORMATION.RegionSizeDis
                    .If_Or W$IMAGE_DOS_HEADER.e_magic = &IMAGE_DOS_SIGNATURE, W$IMAGE_DOS_HEADER.e_magic = &IMAGE_DOS_SIGNATURE_OLD_VERSION
                        mov ecx D@pbLast | add ecx D$IMAGE_DOS_HEADER.e_lfanew
                        call 'KERNEL32.ReadProcessMemory' D@hProcess, ecx, D@pNtHeader, Size_Of_IMAGE_NT_HEADERS, &NULL
                        mov ebx D@pNtHeader
                        If_And eax <> 0, D$ebx+IMAGE_NT_HEADERS.SignatureDis = &IMAGE_NT_SIGNATURE
                            mov eax D@pbLast | ExitP
                        End_If
                    .End_If
                ..End_If
            .Test_End
            ;..End_If
        ..Test_End


;;
        ; Skip uncommitted regions and guard pages.
        ..If D@MEMORY_BASIC_INFORMATION.StateDis <> &MEM_COMMIT ; 010 020
        ..Else
            .Test_If D@MEMORY_BASIC_INFORMATION.ProtectDis &PAGE_GUARD__&PAGE_NOACCESS
            .Test_Else
                call 'KERNEL32.ReadProcessMemory' D@hProcess, D@pbLast, IMAGE_DOS_HEADER, Size_of_IMAGE_DOS_HEADER, &NULL
                mov ebx D$IMAGE_DOS_HEADER.e_lfanew
                .If_And eax <> 0, W$IMAGE_DOS_HEADER.e_magic = &IMAGE_DOS_SIGNATURE,
                                  ebx >= Size_of_IMAGE_DOS_HEADER, ebx <= D@MEMORY_BASIC_INFORMATION.RegionSizeDis

                    mov ecx D@pbLast | add ecx D$IMAGE_DOS_HEADER.e_lfanew
                    call 'KERNEL32.ReadProcessMemory' D@hProcess, ecx, D@pNtHeader, Size_Of_IMAGE_NT_HEADERS, &NULL
                    mov ebx D@pNtHeader
                    If_And eax <> 0, D$ebx+IMAGE_NT_HEADERS.SignatureDis = &IMAGE_NT_SIGNATURE
                        mov eax D@pbLast | ExitP
                    End_If
                .End_If
            .Test_End
        ..End_If
;;
        mov eax D@MEMORY_BASIC_INFORMATION.BaseAddressDis | add eax D@MEMORY_BASIC_INFORMATION.RegionSizeDis | mov D@pbLast eax
        call 'KERNEL32.VirtualQueryEx' D@hProcess, D@pbLast, D@MEMORY_BASIC_INFORMATION, Size_Of_MEMORY_BASIC_INFORMATION

    .End_While

EndP
_______________________________________________________

Proc UpdateImports32:
    Arguments @hProcess, @hModule, @plpDlls, @nDlls
    Local @fSucceeded, @cbNew, @pbNew, @pbModule, @cbRead, @dwSec, @iCounter, @nOldDlls, @obRem,
          @obOld, @obTab, @obDll, @obStr, @pbBase, @pbNext, @pbNewIid, @piid, @obBase,
          @dwProtect, @nOffset
    Uses ebx, esi, edi

    mov D@fSucceeded &FALSE
    mov D@cbNew 0
    mov D@pbNew &NULL

    mov eax D@hModule
    mov D@pbModule eax
    C_call 'msvcrt.memset' IMAGE_DOS_HEADER, 0, Size_of_IMAGE_DOS_HEADER

    lea eax D@cbRead | mov D@cbRead 0
    call 'KERNEL32.ReadProcessMemory' D@hProcess, D@pbModule, IMAGE_DOS_HEADER, Size_of_IMAGE_DOS_HEADER, eax
    If_Or eax = 0, D@cbRead < Size_of_IMAGE_DOS_HEADER
        xor eax eax
        ExitP
    End_If

    C_call 'msvcrt.memset' IMAGE_NT_HEADERS, 0, Size_Of_IMAGE_NT_HEADERS

    lea eax D@cbRead | mov D@cbRead 0
    mov edx D@pbModule | add edx D$IMAGE_DOS_HEADER.e_lfanew
    call 'KERNEL32.ReadProcessMemory' D@hProcess, edx, IMAGE_NT_HEADERS, Size_Of_IMAGE_NT_HEADERS, eax
    If_Or eax = 0, D@cbRead < Size_Of_IMAGE_NT_HEADERS
        xor eax eax
        ExitP
    End_If

    If W$IMAGE_NT_HEADERS.OptionalHeader.Magic <> &IMAGE_NT_OPTIONAL_HDR32_MAGIC
        call 'KERNEL32.SetLastError' &ERROR_INVALID_BLOCK
        xor eax eax
        ExitP
    End_If

    ; Zero out the bound table so loader doesn't use it instead of our new table.
    mov D$IMAGE_NT_HEADERS.DataDirectory.BoundIAT 0
    mov D$IMAGE_NT_HEADERS.DataDirectory.BoundIATSize 0

    ; Find the size of the mapped file.
    movzx eax W$IMAGE_NT_HEADERS.FileHeader.SizeOfOptionalHeader
    mov ecx D$IMAGE_DOS_HEADER.e_lfanew
    lea edx D$ecx+eax+IMAGE_NT_HEADERS64.OptionalHeader.MagicDis
    mov D@dwSec edx

    mov D@iCounter 0
    movzx eax W$IMAGE_NT_HEADERS.FileHeader.NumberOfSections

    .While D@iCounter < eax

        C_call 'msvcrt.memset' IMAGE_SECTION_HEADER, 0, Size_Of_IMAGE_SECTION_HEADER

        lea ebx D@cbRead | mov D@cbRead 0
        mov edx D@pbModule | add edx D@dwSec
        mov eax D@iCounter |imul eax eax Size_Of_IMAGE_SECTION_HEADER | add edx eax
        call 'KERNEL32.ReadProcessMemory' D@hProcess, edx, IMAGE_SECTION_HEADER, Size_Of_IMAGE_SECTION_HEADER, ebx
        If_Or eax = 0, D@cbRead < Size_Of_IMAGE_SECTION_HEADER ; on the original disasm, it says to >= Size_of.. ?
            xor eax eax
            ExitP
        End_If

        ; If the file didn't have an IAT_DIRECTORY, we assign it...
        mov ebx D$IMAGE_NT_HEADERS.DataDirectory.Import
        mov edx D$IMAGE_SECTION_HEADER.VirtualAddress | add edx D$IMAGE_SECTION_HEADER.SizeOfRawData
        If_And D$IMAGE_NT_HEADERS.DataDirectory.IAT = 0, ebx >= D$IMAGE_SECTION_HEADER.VirtualAddress,
               D$IMAGE_NT_HEADERS.DataDirectory.Import < edx
            mov eax D$IMAGE_SECTION_HEADER.VirtualAddress | mov D$IMAGE_NT_HEADERS.DataDirectory.IAT eax
            mov eax D$IMAGE_SECTION_HEADER.SizeOfRawData | mov D$IMAGE_NT_HEADERS.DataDirectory.IATSize eax
        End_If

        inc D@iCounter
        movzx eax W$IMAGE_NT_HEADERS.FileHeader.NumberOfSections

    .End_While

    mov eax D$IMAGE_NT_HEADERS.DataDirectory.ImportSize;Dis
    xor edx edx
    mov ecx Size_Of_IMAGE_IMPORT_DESCRIPTOR
    div ecx
    mov D@nOldDlls eax

    mov eax D@nDlls
    imul eax eax Size_Of_IMAGE_IMPORT_DESCRIPTOR
    mov D@obRem eax

    mov eax D@nOldDlls
    imul eax eax Size_Of_IMAGE_IMPORT_DESCRIPTOR | add eax D@obRem
    mov D@obOld eax

    call PadToDwordPtr D@obOld
    mov D@obTab eax

    mov eax D@nDlls | shl eax 4 | add eax D@obTab | mov D@obDll eax

    mov D@obStr eax
    mov D@cbNew eax

    mov D@iCounter 0
    mov eax D@iCounter
    While eax < D@nDlls
        mov ecx D@plpDlls
        C_call 'msvcrt.strlen' D$ecx+eax*4
        inc eax
        call PadToDword eax
        add eax D@cbNew | mov D@cbNew eax
        inc D@iCounter
        mov eax D@iCounter
    End_While

    ; allocate new memory
    C_call 'msvcrt.??2@YAPAXI@Z' D@cbNew
    On eax = 0, ExitP
    mov D@pbNew eax

    C_call 'msvcrt.memset' D@pbNew, 0, D@cbNew

    mov eax D@pbModule
    mov D@pbBase eax
    mov eax D@pbBase | add eax D$IMAGE_NT_HEADERS.OptionalHeader.BaseOfCode
    add eax D$IMAGE_NT_HEADERS.OptionalHeader.SizeOfCode | add eax D$IMAGE_NT_HEADERS.OptionalHeader.SizeOfInitializedData
    add eax D$IMAGE_NT_HEADERS.OptionalHeader.SizeOfUninitializedData | mov D@pbNext eax

    If D@pbBase < eax
        mov D@pbBase eax
    End_If

    call FindAndAllocateNearBase D@hProcess, D@pbBase, D@cbNew
    mov D@pbNewIid eax
    If eax = 0
        call FinishUpdating D@pbNew, D@fSucceeded
        ExitP
    End_If

    mov eax D@pbNew
    mov D@piid eax
    mov eax D@pbNewIid | sub eax D@pbModule | mov D@obBase eax
    mov D@dwProtect 0

    .If D$IMAGE_NT_HEADERS.DataDirectory.Import <> 0
        lea ebx D@cbRead | mov D@cbRead 0
        mov ecx D@nOldDlls | imul ecx ecx Size_Of_IMAGE_IMPORT_DESCRIPTOR
        mov edx D@nDlls | imul edx edx Size_Of_IMAGE_IMPORT_DESCRIPTOR | add edx D@piid
        mov eax D@pbModule | add eax D$IMAGE_NT_HEADERS.DataDirectory.Import
        call 'KERNEL32.ReadProcessMemory' D@hProcess, eax, edx, ecx, ebx
        mov edi eax
        mov ebx D@nOldDlls | imul ebx ebx Size_Of_IMAGE_IMPORT_DESCRIPTOR
        If_Or edi = 0, D@cbRead < ebx
            call FinishUpdating D@pbNew, D@fSucceeded
            ExitP
        End_If
    .End_If

    mov D@iCounter 0
    mov eax D@iCounter

    .While eax < D@nDlls

        mov ecx D@plpDlls
        mov edx D$ecx+eax*4
        mov eax D@cbNew | sub eax D@obStr
        mov ecx D@pbNew | add ecx D@obStr
        call StringCchCopy ecx, eax, edx
        If eax <> &S_OK
            call FinishUpdating D@pbNew, D@fSucceeded
            ExitP
        End_If

        ; After copying the string, we patch up the size "??" bits if any.
        mov eax D@cbNew | sub eax D@obStr
        mov ecx D@pbNew | add ecx D@obStr
        call ReplaceOptionalSizeA ecx, eax, {B$ "32", 0}
        If eax <> &S_OK
            call FinishUpdating D@pbNew, D@fSucceeded
            ExitP
        End_If

        mov edi D@piid
        mov esi D@iCounter | imul esi Size_Of_IMAGE_IMPORT_DESCRIPTOR

        mov eax D@iCounter | shl eax 2
        mov ecx D@obTab | lea edx D$ecx+eax*4 | mov D@nOffset edx

        mov eax D@obBase | add eax D@nOffset
        mov D$edi+esi+IMAGE_IMPORT_DESCRIPTOR.OriginalFirstThunkDis eax

        mov eax D@pbNew | add eax D@nOffset; | mov D@pIMAGE_THUNK_DATA eax
        mov D$eax (&IMAGE_ORDINAL_FLAG+1)
        mov D$eax+4 0

        mov eax D@iCounter | shl eax 2
        mov ecx D@obTab | lea edx D$ecx+eax*4+8 | mov D@nOffset edx

        mov eax D@obBase | add eax D@nOffset
        mov D$edi+esi+IMAGE_IMPORT_DESCRIPTOR.FirstThunkDis eax

        mov eax D@pbNew | add eax D@nOffset; | mov D@pIMAGE_THUNK_DATA eax
        mov D$eax (&IMAGE_ORDINAL_FLAG+1)
        mov D$eax+4 0

        mov D$edi+esi+IMAGE_IMPORT_DESCRIPTOR.TimeDateStampDis 0
        mov D$edi+esi+IMAGE_IMPORT_DESCRIPTOR.ForwarderChainDis 0

        mov eax D@obBase | add eax D@obStr
        mov D$edi+esi+IMAGE_IMPORT_DESCRIPTOR.Name1Dis eax

        mov eax D@iCounter
        mov ecx D@plpDlls
        C_call 'msvcrt.strlen' D$ecx+eax*4
        inc eax

        call PadToDword eax
        add eax D@obStr | mov D@obStr eax

        inc D@iCounter
        mov eax D@iCounter

    .End_While

    call 'KERNEL32.WriteProcessMemory' D@hProcess, D@pbNewIid, D@pbNew, D@obStr, &NULL
    If eax = 0
        call FinishUpdating D@pbNew, D@fSucceeded
        ExitP
    End_If

    If D$IMAGE_NT_HEADERS.DataDirectory.IAT = 0
        mov eax D@obBase | mov D$IMAGE_NT_HEADERS.DataDirectory.IAT eax
        mov eax D@cbNew |  mov D$IMAGE_NT_HEADERS.DataDirectory.IATSize eax
    End_If

    mov eax D@obBase | mov D$IMAGE_NT_HEADERS.DataDirectory.Import eax
    mov eax D@cbNew | mov D$IMAGE_NT_HEADERS.DataDirectory.ImportSize eax

    ; Update the NT header for the new import directory.
    lea eax D@dwProtect
    call DetourVirtualProtectSameExecuteEx D@hProcess, D@pbModule, D$IMAGE_NT_HEADERS.OptionalHeader.SizeOfHeaders,
                                           &PAGE_EXECUTE_READWRITE, eax
    If eax = 0
        call FinishUpdating D@pbNew, D@fSucceeded
        ExitP
    End_If

    mov D$IMAGE_NT_HEADERS.OptionalHeader.CheckSum 0
    call 'KERNEL32.WriteProcessMemory' D@hProcess, D@pbModule, IMAGE_DOS_HEADER, Size_of_IMAGE_DOS_HEADER, &NULL
    If eax = 0
        call FinishUpdating D@pbNew, D@fSucceeded
        ExitP
    End_If

    mov ecx D@pbModule | add ecx D$IMAGE_DOS_HEADER.e_lfanew
    call 'KERNEL32.WriteProcessMemory' D@hProcess, ecx, IMAGE_NT_HEADERS, Size_Of_IMAGE_NT_HEADERS, &NULL
    If eax = 0
        call FinishUpdating D@pbNew, D@fSucceeded
        ExitP
    End_If

    lea eax D@dwProtect
    call 'KERNEL32.VirtualProtectEx' D@hProcess, D@pbModule, D$IMAGE_NT_HEADERS.OptionalHeader.SizeOfHeaders,
                                     D@dwProtect, eax
    If eax <> 0
        mov eax &TRUE
    End_If
    call FinishUpdating D@pbNew, eax

EndP


Proc FinishUpdating:
    Arguments @pbNew, @fSucceeded

    If D@pbNew <> 0
        C_call 'msvcrt.??3@YAXPAX@Z' D@pbNew
        mov D@pbNew 0
    End_If

    mov eax D@fSucceeded

EndP
_______________________________________________________

Proc PadToDwordPtr:
    Arguments @dw

    mov eax D@dw
    add eax 7
    and eax 0-8

EndP
_______________________________________________________

Proc PadToDword:
    Arguments @dw

    mov eax D@dw
    add eax 3
    and eax 0-4

EndP
_______________________________________________________

; Find a region of memory in which we can create a replacement import table.

Proc FindAndAllocateNearBase:
    Arguments @hProcess, @pbBase, @cbAlloc
    Local @pbLast, @pbAddress
    Structure @MEMORY_BASIC_INFORMATION 28, @MEMORY_BASIC_INFORMATION.BaseAddressDis 0,
                                            @MEMORY_BASIC_INFORMATION.AllocationBaseDis 4,
                                            @MEMORY_BASIC_INFORMATION.AllocationProtectDis 8,
                                            @MEMORY_BASIC_INFORMATION.RegionSizeDis 12,
                                            @MEMORY_BASIC_INFORMATION.StateDis 16,
                                            @MEMORY_BASIC_INFORMATION.ProtectDis 20,
                                            @MEMORY_BASIC_INFORMATION.lTypeDis 24
    Uses ebx, esi, edi

    C_call 'msvcrt.memset' D@MEMORY_BASIC_INFORMATION, 0, Size_Of_MEMORY_BASIC_INFORMATION

    mov eax D@pbBase
    mov D@pbLast eax
    C_call 'msvcrt.memset' D@MEMORY_BASIC_INFORMATION, 0, Size_Of_MEMORY_BASIC_INFORMATION
    call 'KERNEL32.VirtualQueryEx' D@hProcess, D@pbLast, D@MEMORY_BASIC_INFORMATION, Size_Of_MEMORY_BASIC_INFORMATION
    .While eax <> 0

        mov eax D@MEMORY_BASIC_INFORMATION.RegionSizeDis | and eax 0FFF
        If eax = 0FFF
            xor eax eax | ExitP
        End_If

        ; Skip anything other than a pure free region.
        .If D@MEMORY_BASIC_INFORMATION.StateDis = &MEM_FREE
            mov eax D@MEMORY_BASIC_INFORMATION.BaseAddressDis | add eax 0FFFF | and eax 0FFFF0000 | mov D@pbAddress eax
            mov eax D@MEMORY_BASIC_INFORMATION.BaseAddressDis | add eax D@MEMORY_BASIC_INFORMATION.RegionSizeDis
            While D@pbAddress < eax
                call 'KERNEL32.VirtualAllocEx' D@hProcess, D@pbAddress, D@cbAlloc, &MEM_RESERVE, &PAGE_READWRITE
                If eax <> 0
                    call 'KERNEL32.VirtualAllocEx' D@hProcess, D@pbAddress, D@cbAlloc, &MEM_COMMIT, &PAGE_READWRITE
                    On eax <> 0, ExitP
                End_If
                mov eax D@pbAddress | add eax 010000 | mov D@pbAddress eax
                mov eax D@MEMORY_BASIC_INFORMATION.BaseAddressDis | add eax D@MEMORY_BASIC_INFORMATION.RegionSizeDis
            End_While
        .End_If

        mov eax D@MEMORY_BASIC_INFORMATION.BaseAddressDis | add eax D@MEMORY_BASIC_INFORMATION.RegionSizeDis | mov D@pbLast eax
        C_call 'msvcrt.memset' D@MEMORY_BASIC_INFORMATION, 0, Size_Of_MEMORY_BASIC_INFORMATION
        call 'KERNEL32.VirtualQueryEx' D@hProcess, D@pbLast, D@MEMORY_BASIC_INFORMATION, Size_Of_MEMORY_BASIC_INFORMATION

    .End_While

EndP

_______________________________________________________

Proc ReplaceOptionalSizeA:
    Arguments @pszDest, @cchDest, @pszSize
    Uses ebx, esi, edi

    mov eax D@pszSize
    If_Or D@cchDest = 0, D@pszDest = 0, D@pszSize = 0, B$eax = 0, B$eax+1 = 0, B$eax+2 <> 0
        mov eax &ERROR_INVALID_PARAMETER
        ExitP
    End_If

    mov edi D@pszDest
    mov esi D@pszSize
    mov eax D@cchDest

    While eax >=  2
        If W$edi = '??'
            mov ax W$esi | mov W$edi ax
            mov eax &S_OK | ExitP
        End_If
        dec eax
        inc edi
    End_While

    mov eax &S_OK

EndP

_______________________________________________________

;;
    Some systems do not allow executability of a page to change. This function applies
    dwNewProtect to [pAddress, nSize), but preserving the previous executability.
    This function is meant to be a drop-in replacement for some uses of VirtualProtectEx.
    When "restoring" page protection, there is no need to use this function.
;;

Proc DetourVirtualProtectSameExecuteEx:
    Arguments @hProcess, @pAddress, @nSize, @dwNewProtect, @pdwOldProtect
    Structure @MEMORY_BASIC_INFORMATION 28, @MEMORY_BASIC_INFORMATION.BaseAddressDis 0,
                                            @MEMORY_BASIC_INFORMATION.AllocationBaseDis 4,
                                            @MEMORY_BASIC_INFORMATION.AllocationProtectDis 8,
                                            @MEMORY_BASIC_INFORMATION.RegionSizeDis 12,
                                            @MEMORY_BASIC_INFORMATION.StateDis 16,
                                            @MEMORY_BASIC_INFORMATION.ProtectDis 20,
                                            @MEMORY_BASIC_INFORMATION.lTypeDis 24
    Uses ebx, esi, edi

    C_call 'msvcrt.memset' D@MEMORY_BASIC_INFORMATION, 0, Size_Of_MEMORY_BASIC_INFORMATION
    call 'KERNEL32.VirtualQueryEx' D@hProcess, D@pAddress, D@MEMORY_BASIC_INFORMATION, Size_Of_MEMORY_BASIC_INFORMATION
    If eax <> 0
        call DetourPageProtectAdjustExecute D@MEMORY_BASIC_INFORMATION.ProtectDis, D@dwNewProtect
        call 'KERNEL32.VirtualProtectEx' D@hProcess, D@pAddress, D@nSize, eax, D@pdwOldProtect
    End_If

EndP
_________________________________________________________

; Copy EXECUTE from dwOldProtect to dwNewProtect.

[DETOUR_PAGE_EXECUTE_ALL (&PAGE_EXECUTE__&PAGE_EXECUTE_READ__&PAGE_EXECUTE_READWRITE__&PAGE_EXECUTE_WRITECOPY)]
[DETOUR_PAGE_NO_EXECUTE_ALL (&PAGE_NOACCESS__&PAGE_READONLY__&PAGE_READWRITE__&PAGE_WRITECOPY)]
[DETOUR_PAGE_ATTRIBUTES <(not (DETOUR_PAGE_EXECUTE_ALL+DETOUR_PAGE_NO_EXECUTE_ALL))>]


Proc DetourPageProtectAdjustExecute:
    Arguments @dwOldProtect, @dwNewProtect
    Local @fOldExecute, @fNewExecute
    Uses ebx, esi, edi

    mov D@fOldExecute 0
    Test_If D@dwOldProtect DETOUR_PAGE_EXECUTE_ALL
        mov D@fOldExecute 1
    Test_End

    mov D@fNewExecute 0
    Test_If D@dwNewProtect DETOUR_PAGE_EXECUTE_ALL
        mov D@fNewExecute 1
    Test_End


    If_And D@fOldExecute <> 0, D@fNewExecute = 0
        mov eax D@dwNewProtect | and eax DETOUR_PAGE_NO_EXECUTE_ALL | shl eax 4
        mov ecx D@dwNewProtect | and ecx DETOUR_PAGE_ATTRIBUTES | or eax ecx | mov D@dwNewProtect eax
    Else_If_And D@fOldExecute = 0, D@fNewExecute <> 0
        mov eax D@dwNewProtect | and eax DETOUR_PAGE_EXECUTE_ALL | shr eax 4
        mov ecx D@dwNewProtect | and ecx DETOUR_PAGE_ATTRIBUTES | or eax ecx | mov D@dwNewProtect eax
    End_If
    mov eax D@dwNewProtect

EndP

_________________________________________________________

[DETOUR_SECTION_HEADER:
 DETOUR_SECTION_HEADER.cbHeaderSize: D$ 0
 DETOUR_SECTION_HEADER.nSignature: D$ 0
 DETOUR_SECTION_HEADER.nDataOffset: D$ 0
 DETOUR_SECTION_HEADER.cbDataSize: D$ 0
 DETOUR_SECTION_HEADER.nOriginalImportVirtualAddress: D$ 0
 DETOUR_SECTION_HEADER.nOriginalImportSize: D$ 0
 DETOUR_SECTION_HEADER.nOriginalBoundImportVirtualAddress: D$ 0
 DETOUR_SECTION_HEADER.nOriginalBoundImportSize: D$ 0
 DETOUR_SECTION_HEADER.nOriginalIatVirtualAddress: D$ 0
 DETOUR_SECTION_HEADER.nOriginalIatSize: D$ 0
 DETOUR_SECTION_HEADER.nOriginalSizeOfImage: D$ 0
 DETOUR_SECTION_HEADER.cbPrePE: D$ 0
 DETOUR_SECTION_HEADER.nOriginalClrFlags: D$ 0
 DETOUR_SECTION_HEADER.reserved1: D$ 0
 DETOUR_SECTION_HEADER.reserved2: D$ 0
 DETOUR_SECTION_HEADER.reserved3: D$ 0]

[DETOUR_SECTION_HEADER.cbHeaderSizeDis 0
 DETOUR_SECTION_HEADER.nSignatureDis 4
 DETOUR_SECTION_HEADER.nDataOffsetDis 8
 DETOUR_SECTION_HEADER.cbDataSizeDis 12
 DETOUR_SECTION_HEADER.nOriginalImportVirtualAddressDis 16
 DETOUR_SECTION_HEADER.nOriginalImportSizeDis 20
 DETOUR_SECTION_HEADER.nOriginalBoundImportVirtualAddressDis 24
 DETOUR_SECTION_HEADER.nOriginalBoundImportSizeDis 28
 DETOUR_SECTION_HEADER.nOriginalIatVirtualAddressDis 32
 DETOUR_SECTION_HEADER.nOriginalIatSizeDis 36
 DETOUR_SECTION_HEADER.nOriginalSizeOfImageDis 40
 DETOUR_SECTION_HEADER.cbPrePEDis 44
 DETOUR_SECTION_HEADER.nOriginalClrFlagsDis 48
 DETOUR_SECTION_HEADER.reserved1Dis 52
 DETOUR_SECTION_HEADER.reserved2Dis 56
 DETOUR_SECTION_HEADER.reserved3Dis 60]

[Size_Of_DETOUR_SECTION_HEADER 64]

[DETOUR_SECTION_RECORD:
 DETOUR_SECTION_RECORD.cbBytes: D$ 0
 DETOUR_SECTION_RECORD.nReserved: D$ 0
 DETOUR_SECTION_RECORD.guid.Data1: D$ 0
 DETOUR_SECTION_RECORD.guid.Data2: W$ 0
 DETOUR_SECTION_RECORD.guid.Data3: W$ 0
 DETOUR_SECTION_RECORD.guid.Data4: B$ 0 #8]

[DETOUR_SECTION_RECORD.cbBytesDis 0
 DETOUR_SECTION_RECORD.nReservedDis 4
 DETOUR_SECTION_RECORD.guid.Data1Dis 8
 DETOUR_SECTION_RECORD.guid.Data2Dis 12
 DETOUR_SECTION_RECORD.guid.Data3Dis 14
 DETOUR_SECTION_RECORD.guid.Data4Dis 16]

[Size_Of_DETOUR_SECTION_RECORD 24]

[GUID.Data1Dis 0
 GUID.Data2Dis 4
 GUID.Data3Dis 6
 GUID.Data4Dis 8]

[TOTAL_DETOUR_STRUCT_SIZE (Size_of_IMAGE_DOS_HEADER+Size_of_IMAGE_NT_HEADERS+Size_of_IMAGE_SECTION_HEADER+Size_of_DETOUR_SECTION_HEADER+Size_of_DETOUR_SECTION_RECORD)]

[DETOUR_SECTION_HEADER_SIGNATURE 0727444] ; 'rtD'

Proc DetourCopyPayloadToProcess:
    Arguments @hProcess, @rguid, @pData, @cbData
    Local @pbBase, @pbTarget, @cbWrote
    Uses ebx, esi, edi

    mov eax D@cbData | add eax TOTAL_DETOUR_STRUCT_SIZE; | mov D@cbTotal eax
    call 'KERNEL32.VirtualAllocEx' D@hProcess, &NULL, eax, &MEM_COMMIT, &PAGE_READWRITE
    On eax = 0, ExitP
    mov D@pbBase eax
    mov D@pbTarget eax
    mov D@cbWrote 0
    C_call 'msvcrt.memset' IMAGE_DOS_HEADER, 0, Size_of_IMAGE_DOS_HEADER
    mov W$IMAGE_DOS_HEADER.e_magic &IMAGE_DOS_SIGNATURE
    mov D$IMAGE_DOS_HEADER.e_lfanew Size_of_IMAGE_DOS_HEADER
    lea eax D@cbWrote
    call 'KERNEL32.WriteProcessMemory' D@hProcess, D@pbTarget, IMAGE_DOS_HEADER, Size_of_IMAGE_DOS_HEADER, eax
    If_Or eax = 0, D@cbWrote <> Size_of_IMAGE_DOS_HEADER
        xor eax eax
        ExitP
    End_If

    mov eax D@pbTarget | add eax Size_of_IMAGE_DOS_HEADER | mov D@pbTarget eax
    C_call 'msvcrt.memset' IMAGE_NT_HEADERS, 0, Size_Of_IMAGE_NT_HEADERS

    mov D$IMAGE_NT_HEADERS.Signature &IMAGE_NT_SIGNATURE
    mov W$IMAGE_NT_HEADERS.FileHeader.SizeOfOptionalHeader (Size_Of_IMAGE_NT_HEADERS-IMAGE_NT_HEADERS.OptionalHeader.MagicDis)
    mov W$IMAGE_NT_HEADERS.FileHeader.Characteristics &IMAGE_FILE_DLL
    mov W$IMAGE_NT_HEADERS.FileHeader.NumberOfSections 1
    mov W$IMAGE_NT_HEADERS.OptionalHeader.Magic &IMAGE_NT_OPTIONAL_HDR_MAGIC

    lea eax D@cbWrote
    call 'KERNEL32.WriteProcessMemory' D@hProcess, D@pbTarget, IMAGE_NT_HEADERS, Size_Of_IMAGE_NT_HEADERS, eax
    If_Or eax = 0, D@cbWrote <> Size_Of_IMAGE_NT_HEADERS
        xor eax eax
        ExitP
    End_If

    mov eax D@pbTarget | add eax Size_Of_IMAGE_NT_HEADERS | mov D@pbTarget eax
    C_call 'msvcrt.memset' IMAGE_SECTION_HEADER, 0, Size_Of_IMAGE_SECTION_HEADER
    C_call 'msvcrt.memcpy' IMAGE_SECTION_HEADER, {B$ ".detour", 0}, &IMAGE_SIZEOF_SHORT_NAME

    mov eax D@pbTarget | add eax Size_Of_IMAGE_SECTION_HEADER | sub eax D@pbBase | mov D$IMAGE_SECTION_HEADER.VirtualAddress eax
    mov eax D@cbData | add eax (Size_Of_DETOUR_SECTION_HEADER+Size_Of_DETOUR_SECTION_RECORD) | mov D$IMAGE_SECTION_HEADER.SizeOfRawData eax

    lea eax D@cbWrote
    call 'KERNEL32.WriteProcessMemory' D@hProcess, D@pbTarget, IMAGE_SECTION_HEADER, Size_Of_IMAGE_SECTION_HEADER, eax
    If_Or eax = 0, D@cbWrote <> Size_Of_IMAGE_SECTION_HEADER
        xor eax eax
        ExitP
    End_If

    mov eax D@pbTarget | add eax Size_Of_IMAGE_SECTION_HEADER | mov D@pbTarget eax
    C_call 'msvcrt.memset' DETOUR_SECTION_HEADER, 0, Size_Of_DETOUR_SECTION_HEADER

    mov D$DETOUR_SECTION_HEADER.cbHeaderSize Size_Of_DETOUR_SECTION_HEADER
    mov D$DETOUR_SECTION_HEADER.nSignature DETOUR_SECTION_HEADER_SIGNATURE
    mov D$DETOUR_SECTION_HEADER.nDataOffset Size_Of_DETOUR_SECTION_HEADER
    mov eax D@cbData | add eax (Size_Of_DETOUR_SECTION_HEADER+Size_Of_DETOUR_SECTION_RECORD) | mov D$DETOUR_SECTION_HEADER.cbDataSize eax

    lea eax D@cbWrote
    call 'KERNEL32.WriteProcessMemory' D@hProcess, D@pbTarget, DETOUR_SECTION_HEADER, Size_Of_DETOUR_SECTION_HEADER, eax
    If_Or eax = 0, D@cbWrote <> Size_Of_DETOUR_SECTION_HEADER
        xor eax eax
        ExitP
    End_If

    mov eax D@pbTarget | add eax Size_Of_DETOUR_SECTION_HEADER | mov D@pbTarget eax

    C_call 'msvcrt.memset' DETOUR_SECTION_RECORD, 0, Size_Of_DETOUR_SECTION_RECORD

    mov eax D@cbData | add eax Size_Of_DETOUR_SECTION_RECORD | mov D$DETOUR_SECTION_RECORD.cbBytes eax
    mov D$DETOUR_SECTION_RECORD.nReserved 0
    mov eax D@rguid | mov ecx D$eax | mov D$DETOUR_SECTION_RECORD.guid.Data1 ecx
    mov edx D$eax+04 | mov D$DETOUR_SECTION_RECORD.guid.Data2 edx
    mov ecx D$eax+08 | mov D$DETOUR_SECTION_RECORD.guid.Data4 ecx
    mov edx D$eax+0C | mov D$DETOUR_SECTION_RECORD.guid.Data4+4 edx

    lea eax D@cbWrote
    call 'KERNEL32.WriteProcessMemory' D@hProcess, D@pbTarget, DETOUR_SECTION_RECORD, Size_Of_DETOUR_SECTION_RECORD, eax
    If_Or eax = 0, D@cbWrote <> Size_Of_DETOUR_SECTION_RECORD
        xor eax eax
        ExitP
    End_If

    mov eax D@pbTarget | add eax Size_Of_DETOUR_SECTION_RECORD | mov D@pbTarget eax
    lea eax D@cbWrote
    call 'KERNEL32.WriteProcessMemory' D@hProcess, D@pbTarget, D@pData, D@cbData, eax
    mov ebx D@cbWrote
    If_Or eax = 0, ebx <> D@cbData
        xor eax eax
    Else
        mov eax &TRUE
    End_If

EndP

_________________________________________________________





; For tests Inject dll onto process
Proc InjectDll:
    ;Arguments @hProcess
    Local @mAddr, @lpAddr, @DllLen, @VPointer

    call 'kernel32.LoadLibraryA' { B$ "kernel32.dll", 0} | mov D@mAddr eax
    call 'kernel32.GetProcAddress' D@mAddr, { B$ "LoadLibraryA", 0} | mov D@lpAddr eax

    call StrLenProc { B$ "D:\RosAsm\Debug\Testing\rnd.dll", 0} | mov D@DllLen eax
    ; Allocate the space into the newly created process
    ;call 'kernel32.VirtualAllocEx' D$PI.hProcess, &NULL, D@DllLen, &MEM_COMMIT, &PAGE_EXECUTE_READWRITE
    call 'kernel32.VirtualAllocEx' D$PI.hProcess, &NULL, D@DllLen, &MEM_COMMIT__&MEM_RESERVE, &PAGE_READWRITE
    mov D@VPointer eax

    ; Write DLL name into the allocated space
    call WriteProcessMem D@VPointer, { B$ "D:\RosAsm\Debug\Testing\rnd.dll", 0}, D@DllLen

    ; Execute the LoadLibrary function using CreateRemoteThread into the previously created process
    call 'Kernel32.CreateRemoteThread' D$PI.hProcess, &NULL, 0, D@lpAddr, D@vpointer, 0, &NULL
    ;call 'kernel32.Sleep' 1000

    ; Finally resume the process main thread.
    ;call 'kernel32.ResumeThread' D$PI.hThread

EndP











