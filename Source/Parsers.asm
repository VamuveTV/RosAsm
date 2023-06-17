TITLE Parsers
____________________________________________________________________________________________
; Maintainer: Rikkert Wiggerink (EvilBro)
; Started: Somewhere in april 2003
; Email: r.wiggerink@student.utwente.nl
____________________________________________________________________________________________
;;
29-4-2003:
 This is the first version of the rewritten parsers in an official RosAsm release.
 This is by no means the end of the revision. :)
  
 The philosophy behind this revision:
    The routines that previously cleaned up the source have been replaced and split up into 
    many different routines (duplicate actions were removed). This is done for maintainance 
    purposes. Anyone who has taken a look at the old routines will see the importance of this 
    split up (from a maintainance point of view anyway :) )
    
    There will be some people who will think the clean up needs to be done in a single routine
    (Yes, I mean Kenny :) ). This will not work. It is what Betov started with when the old
    routines were written (and we all know the hell that followed from that... at least I do).
    There is also no need for the extra performance a single clean up routine would bring.
    On my Pentium 90 compilation of RosAsm takes 39.8 seconds with the new routines and 
    38.5 seconds with the old routines (average timing values used). That means that the new 
    routines are roughly a second slower (on a source like RosAsm). However, this second is 
    completely insignificant compared to the total compile time.

 All parsers I've written expect the source to be in CodeSourceA. They will either modify the 
 source in CodeSourceA directly or copy/modify the source to CodeSourceB. On exiting a routine
 the source will be in CodeSourceA. This is done for simplicity.

 Anyway, I expect that now this version is implemented in an official RosAsm release, errors 
 are bound to turn up. Just post them on the RosAsm forum or mail them directly to me and 
 I'll fix them.

 BTW 'Kill' means 'replace by spaces'. 'Strip' means 'replace by nothing'.
;;
____________________________________________________________________________________________
____________________________________________________________________________________________
; NewCopyToCodeSourceA replaces CopyToCodeSourceA which always used CodeSource as the <Source>.
; usage: call NewCopyToCodeSourceA <Source> <SourceLength>

Proc NewCopyToCodeSourceA:
    Arguments @Pointer, @Length

        mov esi D@Pointer, ecx D@Length, edi D$CodeSourceA, D$StripLen ecx
        rep movsb
EndP

Proc InjectedCopyToCodeSourceA:
    Arguments @Pointer, @Length

        mov esi InjectedTIME_COUNT, edi D$CodeSourceA, ecx D$InjectedTIME_COUNT_Len
        mov D$StripLen ecx | rep movsb

        mov esi D@Pointer, ecx D@Length | add D$StripLen ecx | rep movsb
EndP
____________________________________________________________________________________________
____________________________________________________________________________________________
; CoolParsers are all parsers that do not change the position of statements in the source.

CoolParsers:
    call CheckTextDelimitersPairing
    call KillMultiLineComments ; and Comments
   ; call KillSingleLineComments
    call NewKillVirtualCRLF
    call KillMeaninglessCommas
    call CheckandKillPreParsers
        On B$ParseIncInclude = &TRUE, call ClearIncludeStateMentsFromSource ;call IncParser
        On B$ParseAlternates = &TRUE, call AlternatesPreparsers
      ; +0.2 seconds (2.850 >>> 3.650) on RosAsm 4 Megas, with a Celeron 1.3.
    call KillTitles ; + Old 'ConvertTextSigns'
   ; call CheckBracketsPairing
    ;call CheckNestedBracketsPairing
    call CheckPairings
    call ReplaceParaMacrosBrackets
   ; call CheckOpenCloseSignPairing
ret

; CoolParsersOnInc:
        call CheckTextDelimitersPairing
        call KillMultiLineComments ; and Comments
       ; call KillSingleLineComments
        call NewKillVirtualCRLF
        call KillMeaninglessCommas


CoolParsersOnInc: ; CoolParsers
    push D$CodeSourceA, D$StripLen

        move D$CodeSourceA D$bininc.mem, D$StripLen D$bininc.filesize

        call CheckTextDelimitersPairing
        call KillMultiLineComments ; and Comments
       ; call KillSingleLineComments
        call NewKillVirtualCRLF
        call KillMeaninglessCommas

    pop D$StripLen, D$CodeSourceA
ret

____________________________________________________________________________________________
; HotParsers are parsers that can change the position of statements in the source.

HotParsers:
    call TranslateAsciiToMyAscii

    call StripUnderscore
    On B$ProfilerFlag = &TRUE, call InjectDashLines

    call RemoveDuplicatedSpaces
    call RemoveDuplicatedEOI
    call StripUnneededSpaces
    call ConvertCommasToSpace
    call StripUnneededEOI
    call ConvertEOIinBracketsTOmeEOI
   ; call ConvertTextSigns    ; This one 'needs' to be done sooner. Would simplify earlier routines
    call ExtendLocalSymbols
    call IdentifyVirtualData

    ;call CheckDataIntegrity
    ; need a routine here to fix the error described in StripUnneededSpaces . See: StoreDatas

    call ReorderSource
ret
____________________________________________________________________________________________

; Added on 18/07/2018. This is absolutelly mandatory. Don´ remove this. We must remove all duplications of EOI 1st to avoid them to be
; incorrectly removed from inside StripUnneededSpaces. See HotParsers and follow the example below:

;;

[FloatMatrix4x4a: F$   7
                  F$ 1,   D$ 2,  F$ 5,   10,
                       1.0,    0,   0, -1.0,
                         0,  1.0,   0,  1.0,
                      +1.0, -1.0, 1.0,    5
                      F$ 18 15.9,
                      W$ 17, B$ "guga"
                      R$ 18, D$ 15]

Main:

push FloatMatrix4x4a
ret

The older version was eating the EOI immediatelly after the word "guga" and on all other values that don´t have a comma separator inside the data chain
such as in the 2nd Float (F$) or after the 5 value

;;

Proc RemoveDuplicatedEOI:

    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    ; Ensure that the source starts with an EOI
    .If B$esi <> EOI
        mov B$edi EOI
        inc edi
        If B$esi = space ; and if it starts with a space, bypass it
            inc esi
        End_If
    .End_If

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        .Else_If B$esi = EOI
            If B$edi-1 = EOI
                inc esi
            Else_If B$edi-1 = space ; cannot have spaces before or after EOI
                dec edi
                movsb
            Else
                movsb
            End_If
        .Else_If B$esi = space
            If B$edi-1 = EOI
                inc esi
            Else_If B$edi-1 = space ; cannot have spaces before or after EOI
                inc esi
                ;dec edi
                ;movsb
            Else
                movsb
            End_If
        .Else
            movsb
        .End_If
    .End_While


    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB

EndP

; check to see if the data notations and values are ok:

;;
Necessary to fix things like this that was causing an error added on 18/07/2018

[FloatMatrix4x4a: F$   7,    2,   5,   10,
                       1.0,    0,   0, -1.0,
                         0,  1.0,   0,  1.0,
                      -1.0, -1.0, 1.0,    5]

Main:

push FloatMatrix4x4a
ret
;;

Proc RemoveDuplicatedSpaces:

    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        .Else_If_Or B$esi = space, B$esi = CommaSign
            If B$edi-1 = space
                inc esi
            Else_If B$edi-1 = CommaSign
                mov B$edi-1 space
                inc esi
            Else
                movsb
            End_If
        .Else
            movsb
        .End_If
    .End_While


    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB

EndP

; For tests only. Not used so far in 18/07/2018. I created it mainly to fix the errors at StripUnneededEOI
Proc CheckDataIntegrity:
    Local @IsInsideBracket

    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    mov D@IsInsideBracket &FALSE
    ..While esi < ecx

        ...If B$esi = OpenBracket
            mov D@IsInsideBracket &TRUE
            movsb
        ...Else
            ..If D@IsInsideBracket = &TRUE
                .If B$esi = colonSign
                    ; Ok, it seems we are inside a data variable. Let´s make sure.
                    movsb ; copy the 1st colon sign found and update the adddres before finishing the check
                    call IsInsideData esi, edi ecx
                    mov D@IsInsideBracket &FALSE
                .Else_If B$esi = CloseBracket
                    mov D@IsInsideBracket &FALSE
                    movsb
                .Else
                    movsb
                .End_If
            ..Else
                movsb
            ..End_If
        ...End_If

    ..End_While

    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB

EndP
; For tests only. Not used so far in 18/07/2018. I created it mainly to fix the errors at StripUnneededEOI
Proc IsInsideData:
    Arguments @Input, @Output, @EndSource
    Local @InsideMath

    mov esi D@Input
    mov edi D@Output
    mov ecx D@EndSource
    mov D@InsideMath &FALSE

    ..While esi < ecx
        ...If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        ...Else_If B$esi = CloseBracket ; ended check for the data
            movsb | ExitP
        ...Else_If B$esi = openSign
            movsb
            mov D@InsideMath &TRUE
        ...Else_If B$esi = closeSign
            movsb
            mov D@InsideMath &FALSE
        ...Else_If_And B$edi-1 >= '0', B$edi-1 <= '9', B$esi = subSign, D@InsideMath = &FALSE
            If_Or B$edi-2 = memMarker, B$esi-2 = space;, B$edi-2 >= NoSpaceAfterThis ; cases of isolated numbers such as; [AutoBlurValue: D$ 0-1]. Those are ok. Or labels such as: BIT31+10
                movsb
            Else
                mov B$edi space | inc edi
                movsb
            End_If
        ...Else_If_And B$edi-1 >= '0', B$edi-1 <= '9', B$esi = addSign, D@InsideMath = &FALSE
            If_Or B$edi-2 = memMarker, B$esi-2 = space;, B$edi-2 >= NoSpaceAfterThis ; cases of isolated numbers such as; [AutoBlurValue: D$ 0-1]. Those are ok. Or labels such as: BIT31+10
                movsb
            Else
                mov B$edi space | inc edi
                movsb
            End_If
        ...Else
            movsb
        ...End_If
    ..End_While

EndP

____________________________________________________________________________________________
____________________________________________________________________________________________

CheckTextDelimitersPairing:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$esi-1 LF      ; for MultiLineComment starting on the first line

    .While esi < ecx
        .If B$esi = '"'
            mov edx esi
            Do
                inc esi | cmp esi ecx | je L9>  ; Error: no closing delimiter found inside source.
            Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            mov edx esi
            Do
                inc esi
                cmp esi ecx | je L9>        ; Error: no closing delimiter found inside source.
                cmp B$esi CR | je L9>       ; Error: ' isn't allowed to be multiline.
            Loop_Until B$esi = "'"
        .Else_If B$esi = ';'
            If D$esi-1 = MLC
                Do
                    inc esi | cmp esi ecx | je L8>
                Loop_Until D$esi = MLC
                inc esi
            Else
                Do
                    inc esi
                Loop_Until B$esi < ' '
            End_If
        .End_If
        inc esi
    .End_While
L8: ret

L9: ;ERROR! Unpaired textdelimiter.
    mov esi edx
    While B$esi > LF | dec esi | End_While
    mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: mov B$edi 0

    mov B$Errorlevel 9 | error D$OpenTextPtr
ret
____________________________________________________________________________________________
; Multiline comments are converted to spaces.

KillMultiLineComments:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$esi-1 LF      ; for MultiLineComment starting on the first line

    .While esi < ecx
        ..If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        ..Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        ..Else_If B$esi = ';'
            .If D$esi-1 = MLC
                Do
                    mov B$esi ' '
                    inc esi | On esi >= ecx, ret
                Loop_Until D$esi = MLC
                mov D$esi 0D202020  ; Replace 'LF ; ; CR' with 'Space Space Space CR'.
                add esi 3
            .Else
                ;On D$esi+1 = ' Tag', call AssemblyTag
                Do | mov B$esi ' ' | inc esi | Loop_Until B$esi < ' '
            .End_If
        ..End_If
        inc esi
    .End_While
L8: ; KillMultiLineComments might have killed the closing CRLF, thus it is restored.
    mov W$esi-2 0A0D
ret
________________________________________________________________________________________________
; Singleline comments are converted to spaces.
; Warning: This routine will also kill ";;", so if it is run before MultiLineComment are killed
; then you can be pretty sure, you won't be able to strip MultiLineComments correctly.

KillSingleLineComments:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = ';'
            Do | mov B$esi ' ' | inc esi | Loop_Until B$esi < ' '
        .End_If
        inc esi
    .End_While
L8: ret
________________________________________________________________________________________________
; Titles are converted to spaces.

KillTitles: ; ConvertTextSigns
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = '"'
            mov B$esi TextSign
        .Else_If B$esi = "'"
            mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = "'"
            mov B$esi TextSign
        .Else_If D$esi = 'TITL'
            If B$esi-1 = LF
                On W$esi+4 <> 'E ', jmp L0>
                    Do | mov B$esi ' ' | inc esi | Loop_Until B$esi < ' '
L0:
            End_If
        .End_If

        inc esi

    .End_While

L8: ret

________________________________________________________________________________________________
; VirtualCRLFs are converted to spaces.

[DisableWarning: B$ 0]

NewKillVirtualCRLF:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    ..While esi < ecx
        ...If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        ...Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        ...Else_If B$esi = ','
            push esi
            .While W$esi-1 <> CRLF
                dec esi
                    ..If_Or B$esi = '+', B$esi = '-', B$esi = '/', B$esi = '*', B$esi = '^'
                        .If B$DisableWarning = &FALSE
                            SyntaxErrorInMacro1 D$BadSyntaxBeforeCommaPtr esi
                            pushad
                            call 'USER32.MessageBoxA' 0, {"Disable previous warning ?", 0}, {'Syntax Error Found', 0}, &MB_SYSTEMMODAL__&MB_ICONEXCLAMATION__&MB_YESNO
                            If eax = &IDYES
                                mov B$DisableWarning &TRUE
                            End_If
                            popad

                        .End_If
                        mov B$esi ' '
                    ..Else_If B$esi = ' '
                    ..Else ; any other char, exit
                        jmp L5>
                    ..End_If
            .End_While
         L5: | pop esi
        ...Else_If B$esi = CR
            mov edi esi, al ' '
            While B$edi <= ' '
                dec edi | On edi < D$CodeSourceA, jmp L7>
            End_While
            If B$edi = ','
                Do
                    stosb | cmp edi ecx | ja L8>
                Loop_Until B$edi > ' '
            End_If
        ...End_If

L7:     inc esi

    ..End_While
L8: ret
________________________________________________________________________________________________
; Meaningless commas are converted to spaces.
; Note: This routine will probably move to HotParsers in the future.

KillMeaninglessCommas:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = ','
            mov edi esi
            Do
                inc edi
            Loop_Until B$edi <> ' '

            If B$edi = '+'
                mov esi edi
            Else_If B$edi = '-'
                mov esi edi
            Else
                mov B$esi ' '
            End_If
        .End_If

        inc esi

    .End_While
ret
____________________________________________________________________________________________
; Checks for PREPARSE statements, sets the right flags and then kills the PREPARSE statements.
; Note that any amount of PREPARSE statements can be used in the source with this new routine.

[ParseAlternates: B$ ?    ParseEqual: ?    ParseOOA: ?    ParseNew: ?
 ParseBinInclude: ?       ParseIncInclude: ?]

CheckandKillPreParsers:
    mov B$ParseAlternates &FALSE, B$ParseEqual &FALSE, B$ParseOOA &FALSE, B$Dynamic &FALSE

    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If D$esi = 'PREP'             ; PREPARSE
            ..If D$esi+4 = 'ARSE'
                If B$esi-1 = LF
                    On B$esi+8 <> ' ', jmp L6>
                    call NewCheckPreparser
                    Do
                        mov B$esi ' '
                        inc esi
                    Loop_Until B$esi < ' '
L6:
                End_If
            ..End_If
        .End_If

L7:     inc esi

    .End_While
L8: ret
_______________________________________________________________________________________________
; This routine is not really altered at this point compared to the old routine. This will be
; done in a future version as this version is really unreadable (and thus hard to maintain).

[Dynamic: ?  MemReservation: ?]

NewCheckPreparser:
    mov D$MemReservation 0

    mov edi esi
    add edi 9

L1: While B$edi = ' ' | inc edi | End_While

    mov eax D$edi | and eax (not 020202020)     ; Convert to uppercase.
    ...If eax = 'ALTE'                          ; ALTERNATES
        mov eax D$edi+4 | and eax (not 020202020)
        ..If eax = 'RNAT'
            mov ax W$edi+8 | and eax (not 02020)
            .If ax = 'ES'
                If B$edi+10 <= ' '
                    mov B$ParseAlternates &TRUE | add edi 11 | jmp L8>>
                End_If
            .End_If
    ...Else_If eax = 'EQUA'                     ; EQUAL
        mov al B$edi+4 | and eax (not 020)
        .If al = 'L'
            If B$edi+5 <= ' '
                mov B$ParseEqual &TRUE | add edi 6 | jmp L8>>
            End_If
        .End_If
    ...Else_If eax = 'BINI'                     ; BinIncluder
        mov eax D$edi+4 | and eax (not 020202020)
        .If eax = 'NCLU'
            mov eax D$edi+7 | and eax (not 020202020)
            If eax = 'UDER'
                mov B$ParseBinInclude &TRUE | add edi 12 | jmp L8>>
            End_If
        .End_If
    ...Else_If eax = 'INCI'                     ; IncIncluder
        mov eax D$edi+4 | and eax (not 020202020)
        .If eax = 'NCLU'
            mov eax D$edi+7 | and eax (not 020202020)
            If eax = 'UDER'
                mov B$ParseIncInclude &TRUE | add edi 12 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'OO'                        ; OOA
        mov al B$edi+2 | and eax (not 020)
        .If al = 'A'
            If B$edi+3 <= ' '
                mov B$ParseOOA &TRUE | add edi 4 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'NE'                        ; Prepare New
        mov al B$edi+2 | and eax (not 020)
        .If al = 'W'
            If B$edi+3 <= ' '
                mov B$ParseNew &TRUE | add edi 4 | jmp L8>>
            End_If
        .End_If
    ...Else_If ax = 'EN'                        ; Preparse EntryPoint
        mov eax D$edi+2 | and eax (not 020202020)
        .If eax = 'TRYP'
            mov eax D$edi+6 | and eax (not 020202020)
            If eax = 'OINT'
                call TakeNewEntryPoint | ret   ; Must be separated on one line
            End_If
        .End_If
    ...Else_If eax = 'DYNA'                        ; Preparse Dynamic
        mov eax D$edi+3 | and eax (not 020202020)
        If eax = 'AMIC'
            mov B$Dynamic &TRUE | add edi 8 | jmp L8>
        End_If
;;
  ; Must be run before this time...
  
    ...Else_If eax = 'RESE'                        ; Preparse Reserve
        mov eax D$edi+3 | and eax (not 020202020)
        If eax = 'ERVE'
            call ReadMemoryReservation
            add edi 8 | jmp L8>
        End_If
;;

    ...End_If

    mov B$edi-1 0, esi edi
    While B$edi > ' ' | inc edi | End_While | mov B$edi 0
    mov B$ErrorLevel 9 | error D$BadPreParsePtr, esi

L8:  On B$edi >= ' ', jmp L1<<

; Possible add of multiple Pre-Parsers conflicts checking...
ret


ReadMemoryReservation:
    push esi
        mov esi edi
        While B$esi > ' ' | inc esi | End_While
        While B$esi = ' ' | inc esi | End_While
        If B$esi = '0'
            call TranslateHexa
        Else
            call TranslateDecimal
        End_If

        mov D$MemReservation eax
    pop esi
ret


[BadEntryDef: 'Bad definition of EntryPoint in "Preparse EntryPoint"', 0]

TakeNewEntryPoint: ; Preparse EntryPoint Name
    push esi, edi, ecx
        mov esi edi | add esi 11 | mov edi EntryPointLabel, ecx 0

        While B$esi > ' '
            lodsb | and eax (not 020) | stosb | inc ecx
        End_While
        mov B$edi 0

        mov B$ErrorLevel 9
        mov D$EntryPointLabelLen ecx | On ecx = 0, error BadEntryDef
    pop ecx, edi, esi
ret
________________________________________________________________________________________________
[InsideBrackets: B$ ?]

CheckBracketsPairing:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$InsideBrackets &FALSE

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = '['
            If B$InsideBrackets = &TRUE
                jmp L9>
            Else
                mov edx esi
                mov B$InsideBrackets &TRUE
            End_If
        .Else_If B$esi = ']'
            If B$InsideBrackets = &TRUE
                mov B$InsideBrackets &FALSE
            Else
                mov edx esi
                jmp L9>
            End_If
        .End_If
        inc esi
    .End_While

    On B$InsideBrackets = &TRUE, jmp L9>

L8: ret

L9: ;ERROR! Unpaired bracket
    mov esi edx
    While B$esi > LF | dec esi | End_While
    mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: mov B$edi 0

    mov B$Errorlevel 9 | error D$OrphanBracketPtr
ret


L8: ;ERROR! Unpaired open/close-sign
    mov esi edx
    While B$esi > LF | dec esi | End_While
    mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: mov B$edi 0

    mov B$Errorlevel 9 | error D$ParenthesisPtr
ret
________________________________________________________________________________________________

[VirtualBracketsCount: ?
 FirstBracket: ?         LastBracket: ?
 FirstVirtualBracket: ?  LastVirtualBracket: ?
 FirstParenthesis: ?     LastParenthesis: ?]

CheckPairings:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$InsideBrackets &FALSE, edx esi
    mov D$OpenSignsCount 0, D$VirtualBracketsCount 0

    .While esi < ecx
        mov al B$esi

        .If al = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign

        .Else_If al = '['
            If B$InsideBrackets = &TRUE
                jmp L9>>
            Else_If D$VirtualBracketsCount <> 0
                jmp L9>>
            Else_If D$OpenSignsCount <> 0
                jmp L9>>
            Else
                mov B$InsideBrackets &TRUE, D$FirstBracket esi
            End_If

        .Else_If al = ']'
            mov D$LastBracket esi
            If D$VirtualBracketsCount <> 0
                jmp L9>>
            Else_If B$OpenSignsCount <> 0
                jmp L9>>
            Else_If B$InsideBrackets = &FALSE
                jmp L9>>
            End_If
            mov B$InsideBrackets &FALSE

        .Else_If al = '{'
            mov D$FirstVirtualBracket esi
            If B$InsideBrackets = &FALSE
                On D$VirtualBracketsCount > 0, jmp L9>> ; <<<<<<<<<<<<<<<<<<<<
            End_If
            inc D$VirtualBracketsCount

        .Else_If al = '}'
            mov D$LastVirtualBracket esi
            dec D$VirtualBracketsCount | On D$VirtualBracketsCount = 0-1, jmp L9>>

        .Else_If al = '('
            On B$OpenSignsCount = 0, mov D$FirstParenthesis esi
            inc B$OpenSignsCount

        .Else_If al = ')'
            mov D$LastParenthesis esi
            dec D$OpenSignsCount | On D$OpenSignsCount = 0-1, jmp L9>>
        .End_If

        inc esi
    .End_While

    If B$InsideBrackets = &TRUE
        mov B$esi '[' | jmp L9>
    Else_If D$VirtualBracketsCount <> 0
        mov B$esi '{' | jmp L9>
    Else_If B$OpenSignsCount <> 0
        mov B$esi ')' | jmp L9>
    End_If
ret

L9: ; Pointing the unpairing error:
    push esi
        .If B$esi = '['
            If D$VirtualBracketsCount <> 0
                mov esi D$FirstVirtualBracket
            Else_If D$OpenSignsCount <> 0
                mov esi D$FirstParenthesis
            Else
                mov esi D$FirstBracket
            End_If

        .Else_If B$esi = ']'
            If D$VirtualBracketsCount <> 0
                mov esi D$FirstVirtualBracket
            Else_If D$OpenSignsCount <> 0
                mov esi D$FirstParenthesis
            Else
                ;
            End_If

        .Else_If B$esi = '{'
            mov esi D$FirstVirtualBracket
        .Else_If B$esi = '}'
            ;
        .Else_If B$esi = '('
            mov esi D$FirstParenthesis
        .Else_If B$esi = ')'
            ;
        .End_If

        sub esi D$CodeSourceA | add esi D$CodeSource
        mov eax esi
        While B$esi > LF | dec esi | End_While | inc esi
        While B$eax > LF | inc eax | End_While | dec eax
        mov D$BlockStartTextPtr esi, D$BlockEndTextPtr eax, B$BlockInside &TRUE
        mov D$UpperLine esi
        call UpOneLine | call UpOneLine | call UpOneLine
    pop esi

  ; Set the Error Message Text:
    .If B$esi = '['
        If D$VirtualBracketsCount <> 0
            mov eax UnPairedNestedBrackets
        Else_If D$OpenSignsCount <> 0
            mov eax D$ParenthesisPtr
        Else
            mov eax D$OrphanBracketPtr
        End_If

    .Else_If B$esi = ']'
        If D$VirtualBracketsCount <> 0
            mov eax D$UnPairedNestedBracketsPtr
        Else_If D$OpenSignsCount <> 0
            mov eax D$ParenthesisPtr
        Else
            mov eax D$OrphanBracketPtr
        End_If

    .Else_If B$esi = '{'
        mov eax D$UnPairedNestedBracketsPtr

    .Else_If B$esi = '}'
        mov eax D$UnPairedNestedBracketsPtr

    .Else_If B$esi = '('
        mov eax D$ParenthesisPtr

    .Else_If B$esi = ')'
        mov eax D$ParenthesisPtr

    .Else
        mov eax D$unknownPtr

    .End_If

    mov edi CookedErrorMessage, esi D$BlockStartTextPtr
    While esi < D$BlockEndTextPtr
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While

L2: mov B$edi 0

    mov B$Errorlevel 9 | error eax
ret
____________________________________________________________________________________________

[InsideParaMacro: ?]

ReplaceParaMacrosBrackets:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$InsideBrackets &FALSE, B$InsideParaMacro &FALSE, ebx 0, edx 0

    .While esi < ecx
        lodsb

        ..If al = TextSign
            While B$esi <> TextSign | inc esi | End_While | inc esi
        ..Else_If al = '['
            mov B$InsideBrackets &TRUE
        ..Else_If al = ']'
            mov B$InsideBrackets &FALSE
        ..Else_If al = '{'
            .If B$InsideBrackets = &FALSE
                mov B$esi-1 OpenParaMacro
                mov B$InsideParaMacro &TRUE
            .Else
                mov ebx esi | dec ebx
                While B$ebx <> '['
                    dec ebx
                    On B$ebx = '|', jmp L2>
                    On B$ebx = LF, jmp L2>
                    If B$ebx > ' '
                        mov B$esi-1 OpenParaMacro
                        mov B$InsideParaMacro &TRUE | jmp L2>
                    End_If
                End_While
            .End_If

        ..Else_If al = '}'
            If B$InsideParaMacro = &TRUE
                mov B$esi-1 CloseParaMacro
                mov B$InsideParaMacro &FALSE
            End_If

        ..End_If
L2: .End_While
ret
________________________________________________________________________________________________
; This routine will be moved to Hotparsers at a later time.
[OpenSignsCount: B$ ?]

CheckOpenCloseSignPairing:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$OpenSignsCount 0

    .While esi < ecx
        .If B$esi = '"'
            Do | inc esi | Loop_Until B$esi = '"'
        .Else_If B$esi = "'"
            Do | inc esi | Loop_Until B$esi = "'"
        .Else_If B$esi = '('
            If B$OpenSignsCount = 0
                mov edx esi
            End_If
            inc B$OpenSignsCount
        .Else_If B$esi = ')'
            If B$OpenSignsCount = 0
                jmp L9>
            End_If
            dec B$OpenSignsCount
        .End_If
        inc esi
    .End_While
L8: ret

L9: ;ERROR! Unpaired open/close-sign
    mov esi edx
    While B$esi > LF | dec esi | End_While
    mov edi CookedErrorMessage
    While B$esi <> CR
        movsb | On edi = EndOfCookedErrorMessage, jmp L2>
    End_While
L2: mov B$edi 0

    mov B$Errorlevel 9 | error D$ParenthesisPtr
ret
____________________________________________________________________________________________
________________________________________________________________________________________________
NewCountStatements:
  ; How many statements:
    mov B$ErrorLevel 0
    mov esi D$CodeSourceA, D$StatementsCounter 0, D$LinesCounter 0
    mov B$DontCountNext &FALSE
    mov ebx esi | add ebx D$StripLen

    .While esi < ebx
        ..If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        ..Else_If B$esi = LF
            mov B$DontCountNext &FALSE
        ..Else_If B$esi = '|'
            ;.If D$CodeSourceA > esi
             ;   If_Or B$esi-1 <> ' ', B$esi+1 <> ' '
              ;      mov B$Errorlevel 9 | error D$OrphanPrefixPtr | ret
               ; End_If
            ;.End_If
            mov B$DontCountNext &FALSE
        ..Else_If B$esi = '['
            inc D$LinesCounter
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = ']'
            mov B$DontCountNext &FALSE
        ..Else_If B$esi > ' '
            .If B$esi <> '_'
                If B$DontCountNext = &FALSE
                    inc D$LinesCounter
                    mov B$DontCountNext &TRUE
                End_If
            .End_If
        ..End_If

        inc esi
    .End_While

; set mem tables:

L9: If D$LinesCounter = 0
        call CloseProgressBar
        call 'USER32.MessageBoxA' 0, {"RosAsm can't compile empty files", 0},
                                     {' Sorry', 0}, 0
        mov B$CompileErrorHappend &TRUE
        ;mov esp D$OldStackPointer | ret ; direct error
        Ros_RestoreStack | ret ; direct error
       ; pop eax | ret                  ; Abort, pop caller and return to Message Loop
    End_If

    mov eax D$LinesCounter | add eax 20 | shl eax 3  ; 2 > dword +1 > security
    push eax
        VirtualAlloc StatementsTable eax
    pop eax
    VirtualAlloc StatementsTable2 eax

;StoreStatements:

    mov ecx D$CodeSource | sub ecx D$CodeSourceA    ; Ajust from CodeSource to CodeSourceA.
    mov esi D$CodeSourceA, edi D$StatementsTable
    mov B$DontCountNext &FALSE
    move D$StatementsPtr D$StatementsTable | move D$edi esi | add D$edi ecx
    mov ebx esi | add ebx D$StripLen

    .While esi < ebx
        ..If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        ..Else_If B$esi = LF
            mov B$DontCountNext &FALSE
        ..Else_If B$esi = '|'
            mov B$DontCountNext &FALSE
        ..Else_If B$esi = '['

            mov eax esi | add eax ecx | stosd | add D$StatementsPtr 4

            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If

                inc esi
            .Loop_Until B$esi = ']'

            mov B$DontCountNext &FALSE

        ..Else_If B$esi > ' '
            .If B$esi <> '_'
                If B$DontCountNext = &FALSE
                    mov eax esi | add eax ecx | stosd | add D$StatementsPtr 4
                    mov B$DontCountNext &TRUE
                End_If
            .End_If
        ..End_If

        inc esi
    .End_While

    mov eax 0 | stosd
ret
________________________________________________________________________________________________
________________________________________________________________________________________________
; As of now a '_' outside a Win_equate is the same '' like it is supposed to be according to
; RosAsm help.

StripUnderscore:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign
        .Else_If B$esi = '&'
            If B$esi+1 > '9'
                Do | movsb | Loop_Until B$esi <= ' '
            End_If
        .Else_If B$esi = '_'
            Do | inc esi | Loop_Until B$esi <> '_'
        .End_If
        movsb
    .End_While

    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi
    sub ecx D$CodeSourceB
    mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB
ret
________________________________________________________________________________________________
; To simplify operations, the source is converted to a special format called MyAscii.

[LowSigns            31
    TextSign            30

  NoSpaceAfterThis    29
    numSign             28   ; #  01C
    IfNumSign           27   ; Substitute of # for the Conditional macros #If, ... 01B

    OpenParaMacro       26   ; { for ParaMacros  01A
  NoSpaceBeforeThis   25
    CloseParaMacro      24   ; } for ParaMacros

    CommaSign           23   ; ,

    OpenVirtual         22   ; [   016 (Macros expanded '[' -{-)
    CloseVirtual        21   ; ]   015 (Macros expanded ']' -}-) 019
    OpenBracket         20   ; [   014
    CloseBracket        19   ; ]   013
; 18, 17 >>> NewOpenBracket / NewCloseBracket
  PartEnds            16
    memMarker           15   ; $ or $  exemple: MOV B$MYVALUE 1
    colonSign           14   ; :
    openSign            13   ; (
    closeSign           12   ; )

  OperatorSigns       11
    addSign             10   ; +
    subSign              9   ; -
    mulSign              8   ; *
    divSign              7   ; /
    expSign              6   ; ^
; 5
  Separators          4
   ; Statement           0FF
    Space               3    ; space
    EOI                 2    ; |  End Of Instruction (separator)
    meEOI               1]   ; |  End Of Instruction in macro expansion
                             ; 0 is used as erase sign inside treatements


[MyAsciiTable: B$ 0,1,2,3,4,5,6,7,8,Space,EOI,11,12,EOI,14,15,16,17,18,19,20,21,
 22,23,24,25,26,27,28,29,30,31,Space,'!','"',NumSign,memMarker,'%','&',39,OpenSign,
 CloseSign, MulSign,AddSign,CommaSign,SubSign,'.',DivSign,48,49,50,51,52,53,54,
 55,56,57, ColonSign,';','<',61,'>','?','@'
 65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90 ; (A > Z)
 Openbracket,'\',Closebracket,expSign,95,96
 65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90 ; (A > Z)
 '{',EOI,'}',126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143
 144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164
 165,166,memMarker,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,
 184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,
 205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224,225,
 226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,
 247,248,249,250,251,252,253,254,255]

TranslateAsciiToMyAscii:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen, ebx 0
    add ecx D$CodeSourceA

    .While esi < ecx
        If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign

        Else
            mov bl B$esi
            mov al B$ebx+MyAsciiTable
            mov B$esi al

        End_If

        inc esi
    .End_While
ret
________________________________________________________________________________________________
;;
This breaks rosasm (fixed)

[FloatMatrix4x4a: F$   7,    2,   5,   10,
                       1.0,    0,   0, -1.0,
                         0,  1.0,   0,  1.0,
                      -1.0, -1.0, 1.0,    5]

Main:

push FloatMatrix4x4a
ret

--------------

also this (not fixed). a message of bad real number shows up. see later the proper place to fix
 the error is being generated due to the convertion to qword inside the parenthesis (0-9):
[FloatMatrix4x4a: F$   7,    2,   5,   10,
                       1.0,    0,   0, -1.0,
                         0,  1.0,   0,  1.0,
                      -1.0, -1.0, 1.0,    5
                      (0-9), 0-18 0 - 18]

Main:

push FloatMatrix4x4a
ret
;;
____________________________________________________________________________________________
Proc StripUnneededSpaces:
    ;Local @IsFloat, @IsInsideParaMacro

    ;mov D@IsFloat &FALSE
    ;mov D@IsInsideParaMacro &FALSE
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    ; Ensure that the source starts with an EOI
    .If B$esi <> EOI
        mov B$edi EOI
        inc edi
        If B$esi = space ; and if it starts with a space, bypass it
            inc esi
        End_If
    .End_If

    .While esi < ecx

        ;If B$esi = OpenParaMacro
         ;   mov D@IsInsideParaMacro &TRUE
        ;Else_If B$esi = CloseParaMacro
         ;   mov D@IsInsideParaMacro &FALSE
        ;End_If

        ...If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        ...Else_if_And B$esi <= NoSpaceBeforeThis, B$esi <> space
            mov al B$esi
            ..If_And B$edi-1 = al, al < Space ; cannot have duplicated spaces or EOI or spaces
                inc esi
            ..Else_If B$edi-1 = space ; cannot have space before neither after

                .If_And B$esi = openSign, B$edi-2 > NoSpaceBeforeThis ;  Ex: mov eax (Size_Of_Kernel*64)
                    movsb ; The exception is when we are dealign with "(" and 2 bytes before we have no other signs that prevents the space after them.
                .Else_If_And B$esi = subSign, B$edi-2 > NoSpaceBeforeThis ;  Ex:   mov eax -1
                    movsb
                .Else_If_And B$esi = addSign, B$edi-2 > NoSpaceBeforeThis ;  Ex:   mov eax +1
                    movsb
                .Else
                    dec edi
                    movsb
                .End_If

            ..Else
                movsb
            ..End_If

        ...Else_if_And B$esi <= NoSpaceAfterThis, B$esi <> space
            mov al B$esi
            ..If_And B$edi-1 = al, al < Space ; cannot have duplicated spaces or EOI or spaces
                inc esi
            ..Else_If B$edi-1 <= NoSpaceBeforeThis ; cannot have space before neither after
                ;mov eax eax ; just for testing.
                movsb
            ..Else
                movsb
            ..End_If

        ...Else_If B$esi = Space

            ..If B$edi-1 <= NoSpaceBeforeThis ; no paces before op after this one. Ex: OpenVirtual, colonSign etc. Must be isolated no spaces in between)
                If_And B$edi-1 = CloseParaMacro, B$edi-2 > NoSpaceBeforeThis ;     .If_And eax >s= {MAKE_HRESULT_IMM &SEVERITY_ERROR, &FACILITY_ITF, 512}, eax <s=  ({MAKE_HRESULT_IMM &SEVERITY_ERROR, (&FACILITY_ITF+1), 0} -1)
                    movsb
                Else_If_And B$edi-1 = closeSign, B$edi-2 > NoSpaceBeforeThis ;
                    movsb ; allow space after closesign
                Else_If_And B$edi-1 = closeSign, B$edi-2 = closesign, B$esi+1 > NoSpaceBeforeThis ; Ex: call 'ntdll.NtOpenDirectoryObject' BaseNamedObjectDirectory, (&DIRECTORY_ALL_ACCESS and (not(&DELETE__&WRITE_DAC__&WRITE_OWNER))), D@ObjAttrib
                    movsb ; allow space after 2 closesign and one data not rfelated to the private ones
                Else
                    inc esi
                End_If
            ..Else_If B$edi-1 <= NoSpaceAfterThis; cannot have space before neither after
                inc esi
            ..Else_If B$esi+1 = '}'
                inc esi
            ..Else_If B$esi-1 = '{'
                inc esi
            ..Else
                movsb
            ..End_If
        ...Else
            movsb
        ...End_If
    .End_While

    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

   ; call ShowDumpedData ; For development only. Used to check the flags. Don´t remove this commnent. 18/07/2018
    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB

EndP

____________________________________________________________________________
;;

; development only (17/07/2018)- Don´ remove this !. THis is to check the flaqs that contrains spaces after or before.
; Due to changed made in RosAsm it is necessary to see exactly where the proper flags are in case of errors
; This function is used inside HotParsers at StripUnneededSpaces but can be used on other routines as well.
; On StripUnneededSpaces the disposition of the flags are:

Flags:
2 = Don´t allow spaces before and after
4 = Allow Before and don´t allow after
1 = Allow After and don´t allow before
0 = Flags are not used on this stage.

Allow space     Value   Before  After
addSign           2        No    No
CloseBracket      2        No    No
CloseParaMacro    2        No    No
closeSign         2        No    No
CloseVirtual      0  
colonSign         2        No    No
CommaSign         0  
divSign           2        No    No
EOI               2        No    No
expSign           0  
IfNumSign         0  
LowSigns          0  
meEOI             0  
memMarker         2        No    No
mulSign           2        No    No
NoSpaceAfterThis  0  
NoSpaceBeforeThis 0  
numSign           4        Yes   No
OpenBracket       2        No    No
OpenParaMacro     4        Yes   No
openSign          2        No    No
OpenVirtual       0  
OperatorSigns     0  
PartEnds          0  
Separators        0  
Space             2        No    No
subSign           2        No    No
TextSign          1        No    Yes


;;

[Data.addSign: D$ 0
Data.CloseBracket: D$ 0
Data.CloseParaMacro: D$ 0
Data.closeSign: D$ 0
Data.CloseVirtual: D$ 0
Data.colonSign: D$ 0
Data.CommaSign: D$ 0
Data.divSign: D$ 0
Data.EOI: D$ 0
Data.expSign: D$ 0
Data.IfNumSign: D$ 0
Data.LowSigns: D$ 0
Data.meEOI: D$ 0
Data.memMarker: D$ 0
Data.mulSign: D$ 0
Data.NoSpaceAfterThis: D$ 0
Data.NoSpaceBeforeThis: D$ 0
Data.numSign: D$ 0
Data.OpenBracket: D$ 0
Data.OpenParaMacro: D$ 0
Data.openSign: D$ 0
Data.OpenVirtual: D$ 0
Data.OperatorSigns: D$ 0
Data.PartEnds: D$ 0
Data.Separators: D$ 0
Data.Space: D$ 0
Data.subSign: D$ 0
Data.TextSign: D$ 0]

Proc ShowDumpedData:
    Uses edi, esi, eax
    ;call 'USER32.DialogBoxParamA' D$hinstance, 32520, &NULL, ScanLibFile, &NULL

    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA
    call 'RosMem.FastZeroMem' Data.addSign, 112
    .While esi < ecx
        xor eax eax
        .If B$esi = meEOI
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.meEOI eax
        .Else_If B$esi = EOI
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.EOI eax
        .Else_If B$esi = Space
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.Space eax
        .Else_If B$esi = Separators
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.Separators eax
        .Else_If B$esi = expSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.expSign eax
        .Else_If B$esi = divSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.divSign eax
        .Else_If B$esi = mulSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.mulSign eax
        .Else_If B$esi = subSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.subSign eax
        .Else_If B$esi = addSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.addSign eax
        .Else_If B$esi = OperatorSigns
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.OperatorSigns eax
        .Else_If B$esi = closeSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.closeSign eax
        .Else_If B$esi = openSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.openSign eax
        .Else_If B$esi = colonSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.colonSign eax
        .Else_If B$esi = memMarker
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.memMarker eax
        .Else_If B$esi = PartEnds
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.PartEnds eax
        .Else_If B$esi = CloseBracket
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.CloseBracket eax
        .Else_If B$esi = OpenBracket
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.OpenBracket eax
        .Else_If B$esi = CloseVirtual
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.CloseVirtual eax
        .Else_If B$esi = OpenVirtual
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.OpenVirtual eax
        .Else_If B$esi = CommaSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.CommaSign eax
        .Else_If B$esi = CloseParaMacro
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.CloseParaMacro eax
        .Else_If B$esi = NoSpaceBeforeThis
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.NoSpaceBeforeThis eax
        .Else_If B$esi = OpenParaMacro
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.OpenParaMacro eax
        .Else_If B$esi = IfNumSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.IfNumSign eax
        .Else_If B$esi = numSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.numSign eax
        .Else_If B$esi = NoSpaceAfterThis
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.NoSpaceAfterThis eax
        .Else_If B$esi = TextSign
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.TextSign eax
        .Else_If B$esi = LowSigns
            If B$esi-1 = space
                or eax 00_100
            Else_If B$esi+1 = space
                or eax 00_001
            Else
                or eax 00_010
            End_If
            mov D$Data.LowSigns eax
        .End_If

        inc esi
    .End_While

    mov eax eax

EndP


____________________________________________________________________________________________

; This function strips uneeded data from the end of the chunck during encoding

Proc RemoveUneededDataCodeSource:
    Arguments @SrcStart, @ActualSrcPos, @Srclen
    uses edi, ecx
;;
    mov edi D@ActualSrcPos
    mov ecx D@SrcStart
    add ecx D@Srclen

    While edi <= ecx
        mov B$edi 0
        inc edi
    End_While
;;
    mov edi D@ActualSrcPos
;    mov ecx D@SrcStart
;    add ecx D@Srclen

    While B$edi <> 0;<= ecx
        mov B$edi 0
        inc edi
    End_While

EndP
____________________________________________________________________________________________

; Remaining some valid Comma. Example "mov eax, -1".

ConvertCommasToSpace:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    While esi < ecx
        On B$esi = CommaSign, mov B$esi Space
        inc esi
    End_While
ret
________________________________________________________________________________________________
StripUnneededEOI:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

; Ensure that the source starts with an EOI
    If B$esi <> EOI
        mov B$edi EOI
        inc edi
    End_If

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb
        .Else_If B$esi = EOI
            ..If B$esi+1 = EOI
                inc esi
            ..Else_If B$esi+1 = CloseBracket
                inc esi
            ..Else_If B$esi+1 = OpenBracket
                If B$edi-1 = CloseBracket
                    inc esi
                Else
                    movsb
                End_If
            ..Else
                movsb
            ..End_If
        .Else
            movsb
        .End_If
    .End_While

    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi
    sub ecx D$CodeSourceB
    mov D$StripLen ecx

    mov B$edi EOI,  B$edi+1 EOI | add D$Striplen 2          ; write end mark '||'
    ; call ShowDumpedData For development only. Don´ remove this comment. Changed in 18/07/2018
    Exchange D$CodeSourceA D$CodeSourceB
ret
____________________________________________________________________________________________

ConvertEOIinBracketsTOmeEOI:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen, ebx 0 | add ecx D$CodeSourceA

    mov B$InsideBrackets &FALSE

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_if B$esi = OpenBracket
            mov B$InsideBrackets &TRUE
        .Else_if B$esi = CloseBracket
            mov B$InsideBrackets &FALSE
        .Else_if B$esi = EOI
            If B$InsideBrackets = &TRUE
                mov B$esi meEOI
            End_If
        .End_If
        inc esi
    .End_While
ret
________________________________________________________________________________________________
ConvertTextSigns:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = '"'
            mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = '"'
            mov B$esi TextSign
        .ElseIf B$esi = "'"
            mov B$esi TextSign
            Do | inc esi | Loop_Until B$esi = "'"
            mov B$esi TextSign
        .End_If
        inc esi
    .End_While
ret
____________________________________________________________________________________________
____________________________________________________________________________________________

;;
  Automatic Labels (created by the Assembler, for example, with the "&0" Macro Key,
  are 8 Bytes Long. Example: ZZZZZZZZ: (See 'NoMeanLabel').
  
  The user cannot make use of this form of Labels.
;;

NoAutomaticLabel:
    mov esi D$CodeSourceA, edx esi | add edx D$Striplen

    .While esi < edx
        If B$esi = TextSign
            inc esi
            While B$esi <> TextSign | inc esi | End_While
        End_If

        ..If B$esi = 'Z'
            .If B$esi-1 < LowSigns
                If D$esi = 'ZZZZ'
                  ; We have something begining with 'ZZZZ'. Must 8 chars long: 'ZZZZZZZ'
                    cmp B$esi+8 LowSigns | ja L2>

                    cmp B$esi+7 LowSigns | jb L2>
                    cmp B$esi+6 LowSigns | jb L2>
                    cmp B$esi+5 LowSigns | jb L2>
                    cmp B$esi+4 LowSigns | jb L2>

                        mov B$esi+9 0, B$Errorlevel 9 | error ZZZZreserved, esi

                End_If
            .End_If
        ..End_If

L2:     inc esi
    .End_While
ret
________________________________________________________________________________________________
; The way local symbols are defined in the RosAsm syntax needs to be reviewed. Until that is
; properly done (somewhere in the future) this routine will have to do. :)

[LastFoundLabel: B$ ? #80]
;;
  Beware: This Routine is called twice:
  
  First time from inside the HotParsers, to expand the 'normal' @Locals
  
  A second time for 'AsmMain', after the Equates and Macros Jobs, to expand the
  @Locals inserted by Macros Evocations.
;;
Proc ExtendLocalSymbols:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$LastFoundLabel 0

    .While esi < ecx
        ...If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign

        ...Else_If B$esi = ColonSign
          ; On '::' don't process (as the label is already stored):
            ..If B$esi-1 < LowSigns
                ; Error holded downward.

            ..Else_If B$esi-1 <> ColonSign
              ; nonlocal label, as local labels are always 2 characters:
                .If B$esi-3 > LowSigns
                    push esi
                    mov al 0
                    Do | dec esi | On B$esi = '@', mov al 1 | Loop_Until B$esi < LowSigns
                    If al = 1
                        pop esi

                    Else
                        pop eax
                        inc esi
                        mov ebx LastFoundLabel
                        Do | lodsb | mov B$ebx al | inc ebx | Loop_Until B$esi = ColonSign
                        mov B$ebx 0
                    End_If

                .End_If
            ..End_If

        ...Else_If B$esi = OpenBracket
            mov ebx esi | inc ebx
            While B$ebx > LowSigns | inc ebx | End_While
            If B$ebx = meEOI
                While B$esi <> CloseBracket | movsb | End_While
            End_If

        ...Else_If B$esi = '@'
            If B$esi-1 < LowSigns
                mov ebx LastFoundLabel
                While B$ebx <> 0
                    mov al B$ebx | inc ebx | stosb
                End_While
            Else_if B$esi-2 < LowSigns
                mov al memMarker | stosb
                mov ebx LastFoundLabel
                While B$ebx <> 0
                    mov al B$ebx | inc ebx | stosb
                End_While
            End_If

            Do | movsb | Loop_Until B$esi < LowSigns
        ...End_If

        movsb
    .End_While

    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB

EndP

ExtendLocalSymbols_OLd:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    mov B$LastFoundLabel 0

    .While esi < ecx
        ...If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign

        ...Else_If B$esi = ColonSign
          ; On '::' don't process (as the label is already stored):
            ..If B$esi-1 < LowSigns
                ; Error holded downward.

            ..Else_If B$esi-1 <> ColonSign
              ; nonlocal label, as local labels are always 2 characters:
                .If B$esi-3 > LowSigns
                    push esi
                    mov al 0
                    Do | dec esi | On B$esi = '@', mov al 1 | Loop_Until B$esi < LowSigns
                    If al = 1
                        pop esi

                    Else
                        pop eax
                        inc esi
                        mov ebx LastFoundLabel
                        Do | lodsb | mov B$ebx al | inc ebx | Loop_Until B$esi = ColonSign
                        mov B$ebx 0
                    End_If

                .End_If
            ..End_If

        ...Else_If B$esi = OpenBracket
            mov ebx esi | inc ebx
            While B$ebx > LowSigns | inc ebx | End_While
            If B$ebx = meEOI
                While B$esi <> CloseBracket | movsb | End_While
            End_If

        ...Else_If B$esi = '@'
            If B$esi-1 < LowSigns
                mov ebx LastFoundLabel
                While B$ebx <> 0
                    mov al B$ebx | inc ebx | stosb
                End_While
            Else_if B$esi-2 < LowSigns
                mov al memMarker | stosb
                mov ebx LastFoundLabel
                While B$ebx <> 0
                    mov al B$ebx | inc ebx | stosb
                End_While
            End_If

            Do | movsb | Loop_Until B$esi < LowSigns
        ...End_If

        movsb
    .End_While

    ;call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    ;call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

    Exchange D$CodeSourceA D$CodeSourceB
ret

________________________________________________________________________________________________
[VirtualDataFlag: B$ ?]

IdentifyVirtualData:
    mov esi D$CodeSourceA, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
        .Else_If B$esi = OpenBracket
            mov edx esi, B$VirtualDataFlag &FALSE
            .Do
                inc esi
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                Else_If B$esi = '{'
                    Do | inc esi | Loop_Until B$esi = '}'
                Else_If B$esi = '?'
                    mov B$VirtualDataFlag &TRUE
                End_If
            .Loop_Until B$esi = CloseBracket

            If B$VirtualDataFlag = &TRUE
                mov B$edx OpenVirtual, B$esi CloseVirtual
            End_If
        .End_If
        inc esi
    .End_While
ret
________________________________________________________________________________________________
; The source in CodeSourceA is reorders to: Data/Equates/Macro, Virtual Data, Code..

ReorderSource:
    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    move D$StatementsPtr D$StatementsTable, D$StatementsPtr2  D$StatementsTable2

; Copy Brackets to CodeSourceB:

  ; this might be needed to skip first EOI if present.
    On B$esi = EOI, inc esi

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign | inc esi

        .Else_If B$esi = OpenBracket
            lea edx D$esi+1

            .Do
                If B$esi = TextSign
                    Do | movsb | Loop_Until B$esi = TextSign
                End_If
                movsb

            .Loop_Until B$esi = CloseBracket
            movsb

            mov eax D$StatementsPtr, eax D$eax
            mov ebx D$StatementsPtr2, D$ebx eax
            add D$StatementsPtr2 4 | On D$edx <> 'ZZZZ', add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseVirtual
            inc esi

            add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                add D$StatementsPtr 4
            EndIf
            inc esi
        .Else
            inc esi
        .End_If
    .End_While

    mov esi D$CodeSourceA

  ; Copy Virtual to CodeSourceB.
  ; this might be needed to skip first EOI if present.
    If B$esi = EOI
        inc esi
    End_If

    move D$StatementsPtr D$StatementsTable

    .While esi < ecx
        .If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign | inc esi

        .Else_If B$esi = OpenBracket
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseBracket
            inc esi

            add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            lea edx D$esi+1

            .Do
                If B$esi = TextSign
                    Do | movsb | Loop_Until B$esi = TextSign
                End_If
                movsb
            .Loop_Until B$esi = CloseVirtual
            movsb

            mov eax D$StatementsPtr, eax D$eax
            mov ebx D$StatementsPtr2, D$ebx eax
            add D$StatementsPtr2 4 | On D$edx <> 'ZZZZ', add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                add D$StatementsPtr 4
            EndIf
            inc esi

        .Else
            inc esi

        .End_If
    .End_While


    mov B$edi EOI
    inc edi

    mov esi D$CodeSourceA

  ; Copy the other statements to CodeSourceB.
  ; this might be needed to skip first EOI if present.
    If B$esi = EOI
        inc esi
    End_If

    move D$StatementsPtr D$StatementsTable

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb

        .Else_If B$esi = OpenBracket
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseBracket
            inc esi
            add D$StatementsPtr 4

        .Else_If B$esi = OpenVirtual
            .Do
                If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                End_If
                inc esi
            .Loop_Until B$esi = CloseVirtual
            inc esi
            add D$StatementsPtr 4

        .Else_If B$esi = EOI
            If B$esi-1 = CloseBracket
                ; nop
            Else_If B$esi-1 = CloseVirtual
                ; nop
            Else_If B$esi-1 = EOI
                ; nop
            Else
                mov eax D$StatementsPtr, eax D$eax
                mov ebx D$StatementsPtr2
                mov D$ebx eax

                add D$StatementsPtr 4
                add D$StatementsPtr2 4
            EndIf
            movsb

        .Else
            movsb

        .End_If
    .End_While

    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi
    sub ecx D$CodeSourceB
    mov D$StripLen ecx

    mov eax D$StatementsPtr2, D$eax 0

    Exchange D$CodeSourceA D$CodeSourceB
    Exchange D$StatementsTable D$StatementsTable2

    call StripNewlyAddedUnneededEOI
ret
____________________________________________________________________________________________

; called by 'ReorderSource' only. 'StripUnneededEOI' ('HotParsers') is a bit similar.

StripNewlyAddedUnneededEOI:

    mov esi D$CodeSourceA, edi D$CodeSourceB, ecx D$StripLen | add ecx D$CodeSourceA

    .While esi < ecx
        .If B$esi = TextSign
            Do | movsb | Loop_Until B$esi = TextSign | movsb

        .Else_If B$esi = EOI
            ..If B$esi+1 = EOI
                inc esi
            ..Else
                movsb
            ..End_If

        .Else
            movsb

        .End_If
    .End_While

    call RemoveUneededDataCodeSource D$CodeSourceB, edi, D$SourceLen
    call RemoveUneededDataCodeSource D$CodeSourceA, esi, D$SourceLen

    mov ecx edi | sub ecx D$CodeSourceB | mov D$StripLen ecx

  ; Write the end mark '||':
    mov B$edi EOI | inc D$Striplen
    If B$edi-1 <> EOI
        mov B$edi+1 EOI | inc D$Striplen
    End_If

    Exchange D$CodeSourceA D$CodeSourceB
ret
________________________________________________________________________________________________
____________________________________________________________________________________________

NewPrepareExport:
    VirtualFree D$ExportListAPtr, D$ExportListBPtr

    call NewHowManyExport

    If B$ExportsectionWanted = &TRUE
        call NewStoreToExportListA
        call NewSortExportListA
    End_If
ret


NewHowManyExport:
    mov B$ExportsectionWanted &FALSE

    mov esi D$CodeSourceA, ecx esi, ebx 0, edx 0 | add ecx D$StripLen

    .While esi < ecx
        .If W$esi = '::'
            mov B$ErrorLevel 9
            If B$esi+2 = ':'
                mov B$esi-1, EOI, B$esi+3 EOI
                error D$WhatIsThisPtr esi
            End_If
            While B$esi-1 > ' ' | dec esi | End_While
            While B$esi <> ':' | inc esi | inc ebx | End_While
            inc ebx
            inc edx
        .Else_If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign
       ; .Else_If B$esi = '['
       ;     .Do
       ;         If B$esi = TextSign
       ;             Do | inc esi | Loop_Until B$esi = TextSign
       ;         End_If
       ;         inc esi
       ;     .Loop_Until B$esi = ']'
        .End_If
        inc esi
    .End_While

    If edx > 0
        mov B$ExportsectionWanted &TRUE
        mov D$NumberOfExportedFunctions edx
        add ebx 40                           ; Header
        add ebx 255                          ; name room
        shl edx 4                            ; pointers > n*16 (*10 would be enough..)
        add ebx edx
        mov D$ExportSectionLen ebx
        push ebx
            VirtualAlloc ExportListAPtr ebx
        pop ebx
        VirtualAlloc ExportListBPtr ebx
    End_If
ret


NewStoreToExportListA:
    mov esi D$CodeSourceA, edi D$ExportListAPtr
    mov ecx esi | add ecx D$StripLen

    ..While esi < ecx
        ..If W$esi = '::'
            While B$esi-1 > ' ' | dec esi | End_While
            If B$esi = '@'
                mov B$esi-1 EOI
                While B$esi <> ':' | inc esi | End_While | mov B$esi+2 EOI
                mov B$ErrorLevel 9
                Error BadLabel, esi
            End_If
            While B$esi <> ':' | movsb | End_While
            mov al EOI | stosb

        ..Else_If B$esi = TextSign
            Do | inc esi | Loop_Until B$esi = TextSign

        ..Else_If B$esi = '['
            .Do
                .If B$esi = TextSign
                    Do | inc esi | Loop_Until B$esi = TextSign
                .Else_If W$esi = '::'
                    While B$esi-1 > ' ' | dec esi | End_While
                    If B$esi = '@'
                        mov B$esi-1 EOI
                        While B$esi <> ':' | inc esi | End_While | mov B$esi+2 EOI
                        mov B$ErrorLevel 9
                        Error BadLabel, esi
                    End_If
                    On B$esi = '[', inc esi
                    While B$esi <> ':' | movsb | End_While | movsb
                    mov al EOI | stosb
                    mov B$esi Space
                .End_If
                inc esi
            .Loop_Until B$esi = ']'

        ..End_If
        inc esi
    ..End_While
ret


; EvilBro: I haven't actually rewritten this routine yet.
NewSortExportListA:
    mov edi D$ExportListBPtr, ecx D$NumberOfExportedFunctions

L0: push ecx
        mov esi D$ExportListAPtr, ecx D$NumberOfExportedFunctions, edx 0, bl 0FF

L1:     lodsb
        .If al = 0FF
            ; nop
        .Else_If al < bl
            mov bl al | lea edx D$esi-1
        .Else_If al = bl
            push ebx
                push edx, esi
                    While al = bl
                        lodsb | inc edx | mov bl B$edx
                        cmp al EOI | je L2>
                    End_While
L2:             pop esi, edx
                On al < bl, lea edx D$esi-1
            pop ebx
        .End_If

        While B$esi > EOI
            inc esi
        End_While
        inc esi | loop L1<

        If edx > 0
            mov esi edx
            While B$esi > EOI
                movsb | mov B$esi-1 0FF
            End_While
            mov al EOI | stosb
        End_If

    pop ecx | dec ecx | cmp ecx 0 | ja L0<<

    Exchange D$ExportListAPtr D$ExportListBPtr
    mov edi D$ExportListBPtr, eax 0 | stosd | stosd | stosd | stosd | stosd
    mov eax D$NumberOfExportedFunctions | stosd | stosd
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; This shouldn't be here, but is now for development purposes.

FromDataToStructure:
    mov D$DisScale 4, D$EquateValue 0

    call 'User32.GetDlgItemTextA' D$DataToStructureDialogHandle, 10, D$DataTextTable, (MAX_STRUCT_EDIT_TEXTBUFF*4);01000
    On eax < 10, ret

    mov B$WeAreInTheCodeBox &TRUE
;    push D$CodeSource, D$SourceLen, D$SourceEnd
        ;mov eax esp, D$OldStackPointer eax
        Ros_SaveStack
        mov B$CompileErrorHappend &FALSE

        mov eax D$DataTextTable
        While B$eax > 0
            inc eax
        End_While
        mov B$eax CR, B$eax+1 LF | add eax 2
        inc eax

        sub eax D$DataTextTable
        push eax
            call GetAsmTables
        pop eax
        call NewCopyToCodeSourceA D$DataTextTable, eax ; D$DataTextTableLen

        call Coolparsers

        call NewCountStatements

        On B$CompileErrorHappend = &TRUE, jmp L9>>

        call Hotparsers | On B$CompileErrorHappend = &TRUE, jmp L9>>


        mov esi D$CodeSourceA, edi D$StructureTextTable, D$FirstDataLabel 0

        On B$esi = OpenBracket, inc esi
        mov B$edi '[' | inc edi

L0:     .While B$esi > EOI
            mov ebx esi
            While B$ebx > LowSigns | inc ebx | End_While
            .If B$ebx = ColonSign
                On D$FirstDataLabel = 0, mov D$FirstDataLabel esi
                While B$esi <> ColonSign | movsb | End_While
                mov B$edi ' ' | inc edi
                mov eax D$EquateValue | call WriteEax
                mov B$edi CR, B$edi+1 LF, B$edi+2 ' ' | add edi 3
            .Else_If B$ebx = MemMarker
                If B$ebx-1 = 'D'
                    mov D$DisScale 4
                Else_If B$ebx-1 = 'W'
                    mov D$DisScale 2
                Else_If B$ebx-1 = 'U'
                    mov D$DisScale 2
                Else_If B$ebx-1 = 'B'
                    mov D$DisScale 1
                Else_If B$ebx-1 = 'Q'
                    mov D$DisScale 8
                Else_If B$ebx-1 = 'R'
                    mov D$DisScale 8
                Else_If B$ebx-1 = 'F'
                    mov D$DisScale 4
                Else_If B$ebx-1 = 'T'
                    mov D$DisScale 10
                End_If
            .Else_If B$ebx = NumSign
                inc esi
                If B$esi = '0'
                    call TranslateHexa
                Else
                    call TranslateDecimal
                End_If
                mul D$DisScale | sub eax D$DisScale | add D$EquateValue eax | jmp L1>>
            .Else_If B$esi = '?'
                mov eax D$DisScale | add D$EquateValue eax
            .Else_If B$esi < '0'

            .Else_If B$esi > '9'

            .Else
                mov eax D$DisScale | add D$EquateValue eax
                While B$esi > LowSigns | inc esi | End_While
            .End_If
            inc esi
L1:     .End_While
        On D$FirstDataLabel = 0, jmp L9>

        inc esi | cmp B$esi EOI | ja L0<<

L2:     mov B$edi CR, B$edi+1 LF | add edi 2
        mov esi D$FirstDataLabel
        While B$esi <> ColonSign
            On B$esi = 0, jmp L9>
            movsb
        End_While
        mov D$edi 'SIZE', B$edi+4 ' ' | add edi 5
        mov eax D$EquateValue | call WriteEax
        mov B$edi ']', B$edi+1 0
        call 'User32.SetDlgItemTextA' D$DataToStructureDialogHandle, 11, D$StructureTextTable

L9:     call ReleaseAsmTables

;    pop D$SourceEnd, D$SourceLen, D$CodeSource
    mov B$WeAreInTheCodeBox &FALSE
ret

____________________________________________________________________________________________

EncodeDecode:
    mov B$WeAreInTheCodeBox &TRUE
;    push D$CodeSource, D$SourceLen, D$SourceEnd
      ; ('AsmMain' 'OutOnError')
        ;mov eax esp, D$OldStackPointer eax
        Ros_SaveStack
        mov B$CompileErrorHappend &FALSE

; What on earth is EncodeSource???
        mov eax EncodeSource ;, D$CodeSource eax
        While B$eax > 0
            inc eax
        End_While
        mov B$eax CR, B$eax+1 LF | add eax 2
        inc eax

;        mov D$SourceEnd eax |
[EncodeSourceLen: D$ ?]
        sub eax EncodeSource | mov D$EncodeSourceLen eax

        call GetAsmTables
        call NewCopyToCodeSourceA EncodeSource D$EncodeSourceLen

        call Coolparsers

        call NewCountStatements

        call ClearQwordCheckSum

        On B$CompileErrorHappend = &TRUE, jmp L9>>

        call Hotparsers

        On B$CompileErrorHappend = &TRUE, jmp L9>>
        call InitIndex1 | call InitIndex2

        Exchange D$CodeSourceA D$CodesourceB
        push D$SourceLen
            move D$SourceLen D$EncodeSourceLen
            move D$AsmTablesLength D$SourceLen
            call ReuseSourceAForCodeList
        pop D$SourceLen
        call InitIndex3

        call BuildData                          ; result 'CodeSourceB' > 'CodeSourceB'

        On B$CompileErrorHappend = &TRUE, jmp L9>>
        call InitDebugIpTable
        mov B$ErrorLevel 7                      ; For outOnError, Error

        call ReCodeLine D$CodeSourceB | On B$CompileErrorHappend = &TRUE, jmp L9>>

      ; Prepare Text to show in the Code Hexa view:
        mov esi D$CodeOrigine, edi HexaCodeText
        While esi < D$CodeListPtr
            movzx eax B$esi | inc esi
            mov ebx eax | shr ebx 4
            and eax 0F | and ebx 0F
            mov al B$HexaTable+eax, bl B$HexaTable+ebx
            shl eax 8 | or eax ebx | or eax 020200000 | stosd
        End_While
        mov D$edi 0

      ; Disassemble: (DisMain)
        mov B$DisFlag 0, D$SegmentOverride 0, B$AddressSizeOverride 0
        mov B$OperandSizeOverride 0, W$DisSizeMarker 'D$'
        mov B$DisCodeDisplacement &FALSE, B$EscapePrefix &FALSE
        mov esi D$CodeOrigine, edi DecodeText
L0:     movzx eax B$esi | inc esi | call D$DisOp1+eax*4
        On B$DisFlag = DISDONE, jmp L0<
        mov D$edi 0

      ; In case of text organisation (sub edi 6, for example), we reset:
        If D$DummyDecodeText+4 <> 0
            mov eax DecodeText
            While B$eax-1 > 0
                dec eax
            End_While
            mov ecx DecodeText | sub ecx eax
            mov esi DecodeText | add esi 160-1 | mov edi esi | sub esi ecx
            std
L0:             movsb | cmp edi DecodeText | jae L0<
            cld
            mov D$DummyDecodeText 0, D$DummyDecodeText+4 0
        End_If

L9:     call ReleaseAsmTables

;    pop D$SourceEnd, D$SourceLen, D$CodeSource
    mov B$WeAreInTheCodeBox &FALSE

    mov D$EncodeSecurity 0 ; Ensure the Buffer never overflows
ret


EncodeError:
;L0: mov ebx, esp | cmp ebx, D$OldStackPointer | jnb L1>
;    pop ebx | jmp L0<
;L1:
    Ros_FindRestoredStack
    sub esp 8 | call ErrorMessageBox 0, D$ErrorMessagePtr
ret
____________________________________________________________________________________________
____________________________________________________________________________________________
; Some routines I haven't got round to deleting. :)

[StripLen: ?  TextDelimiter: ?  TextGoingOn: ?]

IsItFirstText:
    cmp B$TextGoingOn &FALSE | je L1>            ; if OFF > test if ON  needed
L0:   cmp al, B$TextDelimiter | jne L9>         ; if ON  > test if OFF needed
        Jmp L3>
L1: cmp al '"' | je L2>
      cmp al "'" | jne L9>
L2: mov B$TextDelimiter al
L3: mov al, TextSign | xor B$TextGoingOn &TRUE
L9: cmp B$TextGoingOn &TRUE
ret

IsItText:                                       ; called by many routines after cleaner
    cmp al, TextSign | jne L9>
L2: xor B$TextGoingOn &TRUE
L9: cmp B$TextGoingOn &TRUE
    ret

[IfItIsText | cmp al TextSign | jne M9>         ; macro a bit faster than 'call IsIttext'
 M0: stosb | lodsb | cmp al TextSign | jne M0<     ; when it fits
    jmp #1
 M9:]


; same as IsItFirstText, but simplified: no error check, no modification of AL. This
; is for other text ruling routines:

____________________________________________________________________________________________
____________________________________________________________________________________________


