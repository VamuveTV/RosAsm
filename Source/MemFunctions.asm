TITLE MemFunctions


;;
    FOR DEVELOPERS
    
    All memory allocation and dealocation needs to be called from here for making esier to track on the debuger
    The memmory managment is done only with RosMem dll. So, please use them or update RosMem.dll
    with the necessary functions when needed
    
;;


Proc MemoryAlloc:
    Arguments @pMem, @Size

    call 'RosMem.VMemAlloc' D@pMem, D@Size

EndP



Proc MemoryFree:
    Arguments @pMem

    If D@pMem <> 0
        call 'RosMem.VMemFree' D@pMem
    End_If

EndP

;;
; Macros used
[OpDelete | cmp #1 0 | je V9>
                    call 'RosMem.Operator_Delete', #1
               mov #1 0 | V9: | #+1]


[OpNew | xor eax eax | cmp #1 0 | je V8>
                    call 'RosMem.Operator_New', #1
               V8: | #+1]

;;
































