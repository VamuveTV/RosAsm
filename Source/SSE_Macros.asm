TITLE SSE_Macros





;;

    ______________________________________________________________________________________________

    pshufd works shuffling the order of dwords inside a xmm register. So, it computes all 128 bits and rearrange
    the order of dwords of it.
    
    A 128bit xmm have the follwoing structure
    
    [DataOutput:
        DataOutput.Data4: D$ 0 ; bit 96 to 127
        DataOutput.Data3: D$ 0 ; bit 64 to 95
        DataOutput.Data2: D$ 0 ; bit 31 to 63
        DataOutput.Data1: D$ 0 ; bit 0 to 31
    ]

    Represented as the following displacements
    
    [DataOutput.Data4Dis 0
     DataOutput.Data3Dis 4
     DataOutput.Data2Dis 8
     DataOutput.Data1Dis 12]

    [Size_Of_DataOutput 16]

    From the debugger we see that the order (in binary is biased in word pos (in bits)):
      F-E    |    D-C    |   B-A |    9-8    |  7-6 |   5-4 |   3-2 |   1-0  | => 31...0..|.63...32..|..95...64..|.127..96
    15...01     31...16
    
    In Dwords the order is expressed as:
    
    D1 D2 D3 D4 = 1 2 3 4 (Pos) --> HiQWord to Low Qword
        18 11 -16 -17
        DataOutput.Data4: D$ -17 ; bit 96 to 127 Least significand doubleword for SSE operand 
        DataOutput.Data3: D$ -16 ; bit 64 to 95 doubledword 1 of SSE operand
        DataOutput.Data2: D$ 11 ; bit 31 to 63  doubledword 2 of SSE operand
        DataOutput.Data1: D$ 18 ; bit 0 to 31   Most significand doubleword for SSE operand 

    D4 and D3 = Low Quadword of SSE operand
    D1 and D2 = High Quadword of SSE operand


    The pshufd rearranges (shuffle) the order of te Dwords, floats, ints etc as from D1|D2|D3|D4 to another one you wish. Ex:
    
    pshufd XMM0 XMM0 {SHUFFLE 3,3,3,2} ; will put the values in D3 to D1, D3 to D2, D3 to D4 and D4 to D2

    The limit of the values used on the macro must be between 1 and 4.
    
    [SHUFFLE | (((4-#1) shl 6) or ((4-#2) shl 4) or ((4-#3) shl 2) or (4-#4))]
    [pshufd | pshufd #1 #2 #3]


    Alternativelly you can also represent the order in termos of 0 to 3 rather then 1 to 4, similar we have in masm (but, in the reverted order of the position)
    The Masm version can be written as (Tks to Sikemanski):

    Shuffle MACRO V0,V1,V2,V3
        EXITM %((V0 shl 6) or (V1 shl 4) or (V2 shl 2) or (V3))
    ENDM

        pshufd  xmm0,xmm0,Shuffle(1,0,3,2)
            is equal to:
        pshufd  xmm0,xmm0,01001110b
        
        In RosAsm the order is inverted, like this: pshufd XMM0 XMM0 {SHUFFLE2 2,3,0,1}

    So, in RosAsm, the structure of XMM data can also be represented as:

    D0 D1 D2 D3 = 0  1  2  3 (Pos) --> HiQWord to Low Qword

        DataOutput.Data3: D$ -17 ; bit 96 to 127 Least significand doubleword for SSE operand 
        DataOutput.Data2: D$ -16 ; bit 64 to 95 doubledword 1 of SSE operand
        DataOutput.Data1: D$ 11 ; bit 31 to 63  doubledword 2 of SSE operand
        DataOutput.Data0: D$ 18 ; bit 0 to 31   Most significand doubleword for SSE operand 

    On this case ou can use the following macro:

    [SHUFFLE2 | (255 - ((#1 shl 6) or (#2 shl 4) or (#3 shl 2) or #4))]
    [pshufd | pshufd #1 #2 #3]
    
    Ex:

        pshufd XMM0 XMM0 {SHUFFLE2 2,3,0,1}
            which is the same as:
        pshufd XMM0 XMM0 {SHUFFLE 3,4,1,2}
        

    For standardization the best would be choose one of them to use always. So to make easier understand (and also port to masm, if needed)
    we choose the 2nd macro. So the limits of the values are from 0 to 3 and can be written simply as:
    
    ; Shuffle Dword original order (Pos) = 0 1 2 3 (Most significand dword to least sigfnificand dword of SSE data)
    [SHUFFLE | (255 - ((#1 shl 6) or (#2 shl 4) or (#3 shl 2) or #4))]
    [pshufd | pshufd #1 #2 #3]
    
    Ex: pshufd XMM0 XMM0 {SHUFFLE 2,3,0,1}

______________________________________________________________________________________________


    A few Full Name talking equates to make easier to follow. Below i included a set of some equates to be used with pshufd mnemonic to make easier
    to perform the Dword operations rather then using the SHUFFLE paramacro form.


;[SSE_SWAP_QWORDS 78] SSE_COPY_1234_3412 SSE_ROTATE_LEFT_64BITS SSE_ROTATE_RIGHT_64BITS

[SSE_ROTATE_RIGHT_96BITS   147] ; Rotate Right 96 bits the data in a xmm register. Dwords in xmm are copied from 0123 to 1230 ordering. Therefore, it is rotating right 96 bits. (Which is the same as rotating left 32 bits).
                                ; The same as: pshufd XMM0 XMM0 {SHUFFLE 1,2,3,0}
[SSE_ROTATE_LEFT_32BITS 147]    ; the same things and result as SSE_ROTATE_RIGHT_96BITS

[SSE_ROTATE_LEFT_96BITS     57] ; Rotate Left 96 bits the data in a xmm register. Dwords in xmm are copied from 0123 to 3012 ordering. Therefore, it is rotating left 96 bits. (Which is the same as rotating right 32 bits)
                                ; The same as: pshufd XMM0 XMM0 {SHUFFLE 3,0,1,2}
[SSE_ROTATE_RIGHT_32BITS  57]   ; the same things and result as SSE_ROTATE_LEFT_96BITS


    for 64 bits (The 2 qwords of a xmm), we have a simple Swaping of both Qwords inside xmm, therefore we are rotating (left and right) 64 bits. Rotating left and right 64 bits are the same thing

[SSE_SWAP_QWORDS 78]            ; Dwords in xmm are copied from 0123 to 2301 ordering. Therefore, it is rotating right and left left 64 bits. The same as: pshufd XMM0 XMM0 {SHUFFLE 2,3,0,1}
[SSE_ROTATE_LEFT_64BITS 78]     ; the same things and result as SSE_SWAP_QWORDS, SSE_ROTATE_RIGHT_64BITS, SSE_ROTATE_64BITS
[SSE_ROTATE_RIGHT_64BITS 78]    ; same as above
[SSE_ROTATE_64BITS 78]          ; same as above


    We can also use pshufd to invert the order of dwords simple as:
[SSE_INVERT_DWORDS 27]      ; Dwords in xmm are copied from 0123 to 3210 ordering. The same as: pshufd XMM0 XMM0 {SHUFFLE 3,2,1,0}

[SSE_SWAP_DWORDS 78]        ; Dwords in xmm are copied from 0123 to 1032 ordering. The same as: pshufd XMM0 XMM0 {SHUFFLE 1,0,3,2}

[SSE_SWAP_LOQWORD 225]      ; Only the 2 dwords in the LowQword are swaped. Dwords in xmm are copied from 0123 to 0132 ordering. The same as: pshufd XMM0 XMM0 {SHUFFLE 0,1,3,2}
[SSE_SWAP_HIQWORD 180]      ; Only the 2 dwords in the HighQword are swaped. Dwords in xmm are copied from 0123 to 1023 ordering. The same as: pshufd XMM0 XMM0 {SHUFFLE 1,0,2,3}

Example of usage:

pshufd xmm1 xmm0 SSE_INVERT_DWORDS ; xmm1 will contains the inverted order of dwords in xmm0
pshufd xmm0 xmm0 SSE_INVERT_DWORDS ; inverted the order of dwords in xmm0

pshufd xmm1 xmm0 SSE_SWAP_QWORDS ; swap qwords and copy them from xmm0 to xmm1
pshufd xmm0 xmm0 SSE_SWAP_QWORDS ; swap qwords in xmm0

pshufd xmm1 xmm0 SSE_ROTATE_RIGHT_96BITS ; rotate 96 bits left in xmm0 and copy them onto xmm1

etc etc

;;

[SSE_ROTATE_RIGHT_96BITS    147]
[SSE_ROTATE_LEFT_32BITS     147]

[SSE_ROTATE_LEFT_96BITS     57]
[SSE_ROTATE_RIGHT_32BITS    57]

[SSE_SWAP_QWORDS 78]         ; the same things and result as SSE_SWAP_QWORDS, SSE_ROTATE_RIGHT_64BITS, SSE_ROTATE_LEFT_64BITS, SSE_ROTATE_64BITS
[SSE_ROTATE_LEFT_64BITS 78]  ; the same things and result as SSE_SWAP_QWORDS, SSE_ROTATE_RIGHT_64BITS, SSE_ROTATE_LEFT_64BITS, SSE_ROTATE_64BITS
[SSE_ROTATE_RIGHT_64BITS 78] ; the same things and result as SSE_SWAP_QWORDS, SSE_ROTATE_RIGHT_64BITS, SSE_ROTATE_LEFT_64BITS, SSE_ROTATE_64BITS
[SSE_ROTATE_64BITS 78]       ; the same things and result as SSE_SWAP_QWORDS, SSE_ROTATE_RIGHT_64BITS, SSE_ROTATE_LEFT_64BITS, SSE_ROTATE_64BITS

[SSE_INVERT_DWORDS 27]      ; invert the order of dwords
[SSE_SWAP_DWORDS 177]       ; Dwords in xmm are copied from 0123 to 1032 ordering. The same as: pshufd XMM0 XMM0 {SHUFFLE 1,0,3,2}
[SSE_SWAP_LOQWORD 225]      ; Only the 2 dwords in the LowQword are swaped. Dwords in xmm are copied from 0123 to 0132 ordering. The same as: pshufd XMM0 XMM0 {SHUFFLE 0,1,3,2}
[SSE_SWAP_HIQWORD 180]      ; Only the 2 dwords in the HighQword are swaped. Dwords in xmm are copied from 0123 to 1023 ordering. The same as: pshufd XMM0 XMM0 {SHUFFLE 1,0,2,3}



; using values from 0 to 3
[SHUFFLE | (255 - ((#1 shl 6) or (#2 shl 4) or (#3 shl 2) or #4))]


[pshufd | pshufd #1 #2 #3]
























































