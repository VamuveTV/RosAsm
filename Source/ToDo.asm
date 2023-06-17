TITLE ToDo

;;
____________________________________________________________________________________________

What is this strange 'MemSaveFilter'?
____________________________________________________________________________________________

In some cases of RegistryData modifications, it may be corrupted.
____________________________________________________________________________________________

il manque la fonction clic droit sur les API terminées par W.

____________________________________________________________________________________________

NOPE addition on case of "empty" Macro Evocation: File with:

RosAsm/Ludwig/Noname.exe and debug_me.exe

Needs a re-organization of the Macros jobs, with intermediate copy of one Statement.
____________________________________________________________________________________________

Disassembler: Win32Dasm. At 040796E, there is a list of Pointers. Internaly, the
first one should be Flagged Code, and not Data.
____________________________________________________________________________________________

Disassembler: One Export may have severl Names Exported. Example:

        MyAdvapi32.dll: BuildExplicitAccessWithNameW / A

'WriteExportedFunctionLabel' needs another loop somewhere.
____________________________________________________________________________________________

For Guga:

Dllscanner Tool: It fails showing all Imports on WZCAB.DLL (in the [Disassembled] Folder).
____________________________________________________________________________________________

Review all EBX preservations, from 'Dis_rm8_r8'

____________________________________________________________________________________________

Review the Strings analyzes in the Disassembler. Some 9, 10, 13, are replaced by
Space, when isolated (should be the reverse).
____________________________________________________________________________________________

>     mov D$eax+0BC   ; ----------------> Here missing the register. It 
> should be mov D$eax+0BC es
>     mov D$eax+098 ds
>     mov D$eax+094 ss
>     mov D$eax+090 ss
>     mov D$eax+08C cs
>     mov D$eax+0C8 cs

!!!!!!!!!!!!!!!!!!!!!!!!
____________________________________________________________________________________________

Search for (OpD1) PFMUL 3D Now.
____________________________________________________________________________________________

Extend the "Right-Click on Numbers" Functionalities (FPU? Signed Values?...)
____________________________________________________________________________________________


nop  ; <<<<<<<<< Error manager pointing here, because of the Duplication of
     ; 'SIZEOF_materials'

DeclareTable materials 1 1 SAMPLE_Material.size

[sizeof_materials 34]
[DeclareTable| {SIZEOF_#1 #2  SIZEOF_#1_CHUNK #3   SIZEOF_#1_ELEMENT #4}]

Main: call 'Kernel32.ExitProcess'

____________________________________________________________________________________________

I didn't realise that Q$ is only used for integers.
Maybe an entry into B_U_Asm along the lines of Scarmatil's explanation would be
appropriate?
____________________________________________________________________________________________

From 'MAXDIALOG', and friends... Make it all Dynamic as soon as possible.
____________________________________________________________________________________________

When setting a bp, the caret moves to that line.
Should not happen.
____________________________________________________________________________________________

Clip File:

Review the Doc.
____________________________________________________________________________________________

Disassembler: Looki Report, in ...\Eudora\Attach\Looki.

The last Point is a real miss-interpretation.
____________________________________________________________________________________________

'DeleteIcon':

Looki says it is possible to have left over data after removal. to be Reviewed.

____________________________________________________________________________________________


fnstv D$eax sbb D$eax <<< RightClick SBB eax 'OpCodeList' / 'SearchMneMonic'

____________________________________________________________________________________________

Linux-LINE: Chuck reports:

> As it turns out, there is a problem with the debugger. Wine handles the
> KERNEL32.VirtualQueryEx call with its NtQueryVirtualMemory routine, and
> returns "Unsupported on other process", causing the debugger to display
> "VirtualQueryEx reported error".
____________________________________________________________________________________________

[list - tree/import/export] Not assuming anything but CALL '...'.
____________________________________________________________________________________________

Error-Box with Copy&Paste enable.
____________________________________________________________________________________________

Error Message window:
It should be possible to copy text from the edits.
There should be no cursor.
The window should be fixed. Not sizeable. 
____________________________________________________________________________________________

2. There is a cursor in the error message window, and it is possible to read and write inside the EDITs.
What for? It's funny...
____________________________________________________________________________________________

Code Completion:
When RosAsm can't find the equate, a messagebox pops and asks if you want to build a list, and takes the focus from the Editor.
This is a VERY annoying way to tell you that you mistyped an equate.
It has do be done maybe like that:
* If matched completion found, underline (and bold?).
* when no longer matches, remove the line (or if choosed to bold, unbold?).
* If the last chars were deleted and it matches again, put the underline back.
For example:
&CW_USED
&CW_USEDEDAU
&CW_USEDE
and the option to build the list will appear... (sorry, no idea. maybe as text telling you that it's optional in the main menu).
It can't remain like that.
What do you think?
____________________________________________________________________________________________

B_U_Asm Selection should not reload the actual page.
____________________________________________________________________________________________

We have found out one user not understanding the Cnfiguration Dialog Tab:

>>> ToDo: Make the [Companion Files] Tab the first open one.
____________________________________________________________________________________________

Is the Header KILLFALGed?

____________________________________________________________________________________________

 Disassembler: With the Tests DLL, the Exported Names are wrongly two Byte backwarded.

____________________________________________________________________________________________

A user reports having seen a hang at:

>Proc DataView_FillDataLabelCombo:
>    Arguments @ComboHandle @SortByName
>
>    call 'User32.SendMessageA' D@ComboHandle, &CB_RESETCONTENT, 0, 0
>    move D$DataLabelComboHandle D@ComboHandle
>
>    mov esi D$PlainLabelList
>    lodsd ; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

... which is quite "strange"...

____________________________________________________________________________________________

>- dans RosAsm (je sais pas si c'est toujours le cas) me semble
>que quand on sélectionne replace source, il ne travaille plus
>sur le fichier initialement chargé mais sur l'exe correspondant
>au .asm chargé. Moi j'aurai plutôt vu qu'il remplaçait le source
>et c'est tout. (pour open source only, là je suis d'accord qu'il
>le fasse par contre)
____________________________________________________________________________________________

'OpA3': "mov D$FS:0, eax", with 0 in Word form >>> Other cases.

____________________________________________________________________________________________


Review the 'IsItCode' tuning, from the 'IsItNoReturnCall' comments.

____________________________________________________________________________________________

Implement a PUSHW for pushing imm 16 with negative cases assumed.
____________________________________________________________________________________________

Review 'CheckBracketExpression'
____________________________________________________________________________________________

In 'AnalyzeOfCmParameter' study the possibility of branching Imm reals, from the
'memMarker' Case
____________________________________________________________________________________________

Try to improve the Blocks saving, and to separate into some isolated Folders.
Mabe depending on a Time&Date Stamp.
____________________________________________________________________________________________

once RosAsm has tried to install, and failed to,
even if the the "RosAsmFiles" Folder is copied aside, with all required
Files, it does not try to Auto-Install, the way it does, at the very
first try.
____________________________________________________________________________________________

Review the 'CompileErrorHappend' Flag (not always effective, and does not work for
the last Dialog of the Unfolder (wishable to not show an empty bow after error),
at the end of 'UnfoldMacro'
____________________________________________________________________________________________

With some Selected Blocks (ex: Double-Click), the ScrollBar does not work.

____________________________________________________________________________________________

Name001
Name002
Name003
Name004

utiliser :

Remplace Name par EQUATE_Name (par exemple)...

____________________________________________________________________________________________

> In most schemes, you can define a macro called "at-compile-time" which
> will do calculations at compile time.  So, we could do:
> 
> (define sqrt-table (at-compile-time (list->vector (map (lambda (x)
> (sqrt x)) (range 0 500)))))

____________________________________________________________________________________________

In "Proc @ReplaceEquate:" don't forget to tell Ludwig to do:

call GetFromQwordCheckSum esi, D$EquateList, D$EquateListLimit

____________________________________________________________________________________________

Il me semble que dans un source utilisant les TITLES, si une erreur de type :
"Symbol ne correspondant à aucun label" pointe vers une page différente de celle
où tu te trouves quand tu refermes la fenêtre de message d'erreur tu ne te retrouves
pas sur l'erreur comme d'habitude...

; -----------------------------------------------------------

In 'WriteFloatingPointOperationCode'

...Test_If_Not_And bx FloatingPointOperand, dx FloatingPointOperand
;...

..Test_Else_If_And bx ConstantOperand, dx ConstantOperand
       ; mov eax merde
            EPP_Code '{??:R$ #1}|FLD R$?|{??:R$ #2}|FLD R$?|' | 
        mov eax merde

The error Manager seesm to be perverted by this "EPP_Code" Macro

As the same Macro is Evocated upward, can is be something inside
the String, when parsing Conditional Macros (???!!!...), or can
it be something wrong in the ParaMacros Parser (???!!!...).

What relationship with the Statements Counter?

____________________________________________________________________________________________

Tools -> Configuration -> Help Files

Serait-il possible de d finir le r pertoire courant comme r pertoire par d faut et d'attribuer au diff rents chemins d'Help :
RosAsm\RosAsmFiles\xxxxx.xxx si ils sont pr sents.
____________________________________________________________________________________________

Wish list: 

- Remember the width of the debugger window. 

- Make the Edit functions works while using multiple instances of RosAsm, or at least an option to not show the "You are running multiple instances of Rosasm"... box. 

- For new files ask for the name of the exe the first time it compiles and not before. 
- and for the "New File" box allow user-defined templates for new files. Or a menu item called "New from template". 


Also, and most important: 

- Error in B_U_Asm > Mnemonics Ref: In Strings_Op and List_All_Op, MOVSD incorrectly links to the SSE mnemonic. And in SSE_Op, MOVSD is missed. 

- Limitation: Deleting all String resources in a PE is not allowed. 

____________________________________________________________________________________________

A short lesson in Correct code benchmarking:

CLI        ;not yet  
CPUID      ;or any other serialising
RDTSC
...        ;store edx:eax
test:
...        ;code to test (no loops here, execpt if part of test)
RDTSC      
STI        ;if 
...        ;sub edx:eax,[stored]
           ;sub 11 (the time for one RDTSC)
result in edx:eax, and that's the only true figure.


____________________________________________________________________________________________


'OQregRegImm8'

to do List:

Hello Betov, 

Your quite right in doubting the mnemonic for 

1) pextrw 

mov al B$SecondReg | shl al 3 | or al 0011_000_000 | or al B$FirstReg 
should be 
mov al B$FirstReg | shl al 3 | or al 0011_000_000 | or al B$SecondReg 

2) pinsrw - though there's an error in the doc's we've got this one right. 

3) movm$kps - this one doesn't seem to work the way they've described, 
possibly been corrupted by microsoft and friends... :) :) :) 


Werewolf
____________________________________________________________________________________________

In 'WriteMacroVariable', i comment out the 5th line, without recalling why i wrote
this, previously (it was for the Conditional macros, anyway, but it seems to work
the same without... Wait and see...)

Also to be reviewed, in 'ReplaceFromMacroData', after the 'call StoreMacroVariableByNumber',
the 'NOPE' output should probably not be there, but after the 'call WriteMacroVariableByNumber'
Seems out of logic...
____________________________________________________________________________________________

For Debug Tool-Tips: keep the Equates List alive the same way as the CheckSum Table,
so that the Expression could be parsed?

How to re-Encode? What of the Label?

____________________________________________________________________________________________

Add an error message for Macros stripping the last String Delimiter.

____________________________________________________________________________________________

Encode / Decode Box does not parse the Win32 Equates.

____________________________________________________________________________________________


If you really want me to report the small errors, I will of course start 
doing this. But, as I can fully happily edit all my templates, and my 
apps, to my heart contents with RosAsm, at no problems in 99.9% of cases, 
I do not figure these small details to be important.

As it is to me, a much greater irritant, to use the working windows OS 
menues, than to use a RosAsm that has some rare crash states. Dont know if 
you found the SHIFT+DELETE problem yet. Sometimes, it seems to accumulate 
an error that makes the REPLACE function misbehave.

If I reset RosAsm, by a restart, the REPLACE function works correctly, but 
after some time using it, the replacement function can replace more than 
the exact number of chars. Sometimes it means that a SPACE is added so 
that when I use labels like Button.Close , and I want to replace it with
SkinButton.Close, I end up with SkinButton .Close (which does not compile).

I try to make a list from now on, and post it each week.

But theese small problemes with RosAsm has workarounds, that I use 
instead. For instance, I rarly need the replace functions, so when I do, I 
save, and exit, and restart, because I know it works perfectly then.

____________________________________________________________________________________________

I remember trying to find this in RosAsm's source (long ago) but I couldn't fix it .
Perhaps you could try calling LoadLibrary explicitly with the full path
c:\path\to\program\LibraryToLoad.dll?
It is obvious that there's something funky with setting the current directory.
Btw, I'm also using XP.

____________________________________________________________________________________________


SSE3 Instructions:

FISTTP  DF /1 FISTTP m16int
FISTTP  DB /1 FISTTP m32int
FISTTP  DD /1 FISTTP m64int
LDDQU
MOVSHDUP, MOVSLDUP, MOVDDUP
ADDSUBPS, ADDSUBPD
HADDPS, HSUBPS
HADDPD, HSUBPD
MONITOR
MWAIT

____________________________________________________________________________________________


>@ PI2FW  AMD 3Dnow Packed Integer Word to Floating-Point Conversion-  0Fh 0Fh / 0Ch
>                   - Found in Disassembler ONLY


____________________________________________________________________________________________

What i can do, is, in case of overflow error, go on checking up to the end, so that, 
in case of trailing specifier, it could forward it to the appropriated Routine.

Done for 'TranslateDecimal'.
____________________________________________________________________________________________

Review the BitMap Types. Example, Cursor sizes.
____________________________________________________________________________________________

After the new release of ludwig Debugger (coming after V.2.007d), recall of
the problem of TD_170Graph Demo, that aborts, when trying to Load/Save a File.
____________________________________________________________________________________________

si tu tapes "b tement" &NUL au lieu de &NULL dans un source en contenant plusieurs, 
l'erreur point e est incorrecte :

&NUL

Unknown Win equate name

Certes, mais le saut dans le code se fait, apparemment, sur la premi re  vocation et non sur l'erreur.
____________________________________________________________________________________________

le clic droit sur les appels Api fait appara tre une fen tre "Api call infos" qui
n'est plus redimensionnable. Le probl me est que dans plusieurs cas la largeur 
n'est pas suffisante pour afficher tout le contenu sans retours   la lignes qui 
rendent confus et mal ais  la lecture...

____________________________________________________________________________________________

If the selection is not 3 line after the TITLE, in Search Functions, it is not
shown, because of the pos computation.
____________________________________________________________________________________________

>Apr s un test de d sassemblage d'une petite application utilisant
des TrackBars (r alis e avec RosAsm) tout le code est correctement
>restitu  mais il manque le :
>
>Call 'COMCTL32.InitCommonControls'

____________________________________________________________________________________________


>ici, avec PREPARSE Equal
>
>eax  = 0-32768 produit une erreur
>eax = -32768 produit une erreur
>eax = (-32768) ne produit pas d'erreur
____________________________________________________________________________________________

Hugin/ Nessie/ Nessie.asm: Problem of error not pointed out on Bad Dec. because
of the Dash-Lines considerations, to be implemented, first, into the [Search] Box.
____________________________________________________________________________________________

'LenghtOfBracketStatements' is bad since the modification of the Local Labels
expansions. Used only in 'SearchForApis'. Maybe not worthy the complication...
____________________________________________________________________________________________

For me personally, it would be good if we could configure code completion to match
after a certain, userselectable char. Or maybe match in the whole string ? I write 
"Application.WMSize" or "Application.WMMove", or "SkinSection.GetVisible" or 
"SkinSection.SetText". So code compeltion is a bit useless to me. If I could write 
"WMS" and RosAsm suggested : Application.WMSize, then code completion would be 
_very_ useful. 

And also if matching more than one identifier, the list could be cycled by just 
pressing CTRL+SPACE a second time, or third time. 
____________________________________________________________________________________________

Disassembler: 'NamedIdSubstitution' is wrong with MASM ShowDib2 Demo
____________________________________________________________________________________________

There is a issue in rosasm with local label calls:

Proc DoThis:
 ;code
 call @locall; DoThis@locall
 ;code

Endp
@locall:
 ;code
 ;code
ret

Proc DoThat:
 ;code
 call @locall; DoThat@locall
 ;code

Endp
@locall:
 ;code
 ;code
ret

The code works as espected.
The issue is with right click and tree navigation tools: They go to the  
first label always. Tree shows calls to locals sometimes as childs (as  
must be) but sometimes as orphans.
____________________________________________________________________________________________

[IMAGE_SECTION_HEADER:]
[Name1: B$ 0 #&IMAGE_SIZEOF_SHORT_NAME]
[MiscPhysicalAddress: MiscVirtualSize: D$ 0
 VirtualAddress: D$ 0
 SizeOfRawData: D$ 0
 PointerToRawData: D$ 0
 PointerToRelocations: D$ 0
 PointerToLinenumbers: D$ 0
 NumberOfRelocations: W$ 0
 NumberOfLinenumbers: W$ 0
 Characteristics: D$ 0]

[Name1Dis 0
 VirtualAddressDis 1
 SizeOfRawDataDis 5
 PointerToRawDataDis 9
 PointerToRelocationsDis 13
 PointerToLinenumbersDis 17
 NumberOfRelocationsDis 21
 NumberOfLinenumbersDis 23
 CharacteristicsDis 25]
 
____________________________________________________________________________________________

Is the 066 Prefix whishable or not for the encoding of ARPL ???
 
____________________________________________________________________________________________

add a [Save all TITLEs as Asm Files] in the [Ctrl] [S] feature Dialog
____________________________________________________________________________________________

Resize the Choose Menu Dialog at 90% of the Screen Width.
____________________________________________________________________________________________

Reuse the Trash1/2 Buffers everywhere possible.
____________________________________________________________________________________________

Add a Warning Edit Control in the Statistics.

When Building .Import, with calls to Comctl32.dll, verify that InitCommonControls
is called. If not, output a warning Message.

See: 'StoreDllName' >>> After the .Import is built, search for COMCTL32.
Found >>> Search for in InitCommonControls in 'ApiListB'.
____________________________________________________________________________________________

The "Peter Ctrl-Z" Bug has been fixed by implementing a security in the 'TextPos'
Routine >>> Rewrite all of the Ctrl-Z Functionalities from scratch when possible.
____________________________________________________________________________________________

Titles after failure of a Disassembling attempt >> Todo List
____________________________________________________________________________________________

Extend the sensitive area of Blank-Right-Click in the four directions.
____________________________________________________________________________________________

Redifine the 'FloatToUString' with Ludwig. ecx is not preserved.
____________________________________________________________________________________________

Is there a limit to the Data Alignment ([<??? ...) ? Is there a validity check for
the Number ?
____________________________________________________________________________________________

In the search/Replace Dialog the Tab-Key will not work properly,
the focus will set only on the selected radio button and can only moved with the arrow keys
____________________________________________________________________________________________

It would be very good to have more files in the MRU list, 
perhaps user- defined in the config setting?
____________________________________________________________________________________________

Implement a Table Switch for Strings Recognitions Table (for foreign languages).
____________________________________________________________________________________________

add a Routine to verify that (in Data) a Pointer does not break a pointer.
Examples in the Disasembly of Guga H2INC.
____________________________________________________________________________________________

Re-Assembling C:\ProgramFiles\... '7zFM' and '7zFM' hangs in 'ResetForNewBrackets'. 
A missing Bracket undetected problem.

To fix: Borbid any use of '?' in Code.
____________________________________________________________________________________________

lea al B$esi ; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
____________________________________________________________________________________________

Old: Dialog Editor: The Child Style is broken when re-organising a set of Dialogs 
 (insertion of a new Dialog in a List of Dialogs with ID modified >>> WS_POPUP.
 
 Something is wrong in Dialog Editor. Mouse Pointing to the various Controls inside
 [Other Files] Configuration Tab... This seems to be a problem with Group Boxes.
 
 To be verified first: Where must the Group box be in the Z order, in order to enable
 groups of Radio Buttons to work by themselves. JohnFound said, by placing the Group
 Box *after* the Radio Buttons... (???...).
 
 
 --------------
 
 Answer from Wilhelm Zadrapa:
 
 The Group Box must have the WS_GROUP style.

 The controls after that box, up the the next WS_GROUP style are part of the group.
 
 --------------

Dialog Editor:

* Sort the Dialogs each time we leave the Resources Editor.

* When Loading the Debugger's Dialogs Files, problems:

- Child Style not preserved (!!!???...)

- The is a bug outputing weird char(s) in the Class Record.
____________________________________________________________________________________________

Simplify 'IsItaLabel' and 'StoreBookMark': Store the Label to be BookMarked,
in all cases, to a renamed 'LocalBookMark'.

____________________________________________________________________________________________

Yes, Bookmarking Local Data Labels is not implemented. I never though of it, but this is,
evidently, a must have.

"Number forms"... EditControl, for easy copy paste... Yes.


____________________________________________________________________________________________
____________________________________________________________________________________________



; (8 ) 

How come this is not ok? 

Code:



[Label: ? #50 
 EndOfLabel: ?] 
mov ecx EndOfLabel-Label ; Unable to resolve this parameter 
mov ecx (EndOfLabel-Label) ; Immediate only in expressions

____________________________________________________________________________________________

Add a content Checking in the Function for loading a .dlg (not a RosAsm File > Abort).

____________________________________________________________________________________________

When creating a drop-down combo box in the dialog editor, it will not allow you to select the &CBS_DROPDOWNLIST flag - clicking this will only select the &CBS_DROPDOWN flag. 
____________________________________________________________________________________________

Rewrite 'RightClick', 'InternalRightClick', 'InternSearch'. More modular, more actual
Style.
____________________________________________________________________________________________


 To be verified:
 
 When parsing the Sources, there may be a problem with Comments vs MLC, that is:
 
 When skipping Comment it must stop at CR, not at LF, as this might eat a following
 MLC.
____________________________________________________________________________________________

i try to compile your 06midi sample and
it give me this message.
Orphan colon encoun....
at Proc Midi Stream.
Is it bug or something is change the 
assembler?
____________________________________________________________________________________________


____________________________________________________________________________________________


____________________________________________________________________________________________

 
____________________________________________________________________________________________

review the upper char in Clip operations.
 
____________________________________________________________________________________________

 
____________________________________________________________________________________________

What is this:

; 1 = My MF_POPUP substitution.

Example in 'ResetThisMenuIDs', 'TurnThisMenuToExType'. Why did i substitute 1 to 010 ?
____________________________________________________________________________________________

 
 
 Do not send an error message for Api call by number with same Number in two different
 DLL. Force DLL Name for such calls.
 Disassembler, set the Jumps Table Labels' Names in the form of: "DllName0xxx:"
 
 PMULLW > (done)

____________________________________________________________________________________________

'ResetForNewBrackets' diserves a complete rewrite
";!!!!!!!!!" was the thing that made 'Base3 Uses' (without any parameter) hang. I turn it:
"jae L9>>", but i don't understand what the comment means (!!!...). The hell!
 
____________________________________________________________________________________________


____________________________________________________________________________________________


____________________________________________________________________________________________


____________________________________________________________________________________________

____________________________________________________________________________________________

default icon might be removed from the PE
____________________________________________________________________________________________


____________________________________________________________________________________________

 
____________________________________________________________________________________________

 Occasional Problems with Registry Modifications. Is it possible to directely 'refresh'?
 (Delete // re-Create)?

____________________________________________________________________________________________



____________________________________________________________________________________________

In Iczelion 31ListView.exe:

WIN32_FIND_DATA

[WFD_cAlternate: B$ 0 #14] hangs (in Find File Functions), under 2000, with long Names.
____________________________________________________________________________________________

 
error in the Dx Demo:

I think i found a very serious Errors in (RosAsm414b version). i using this macro

[DxCall | mov eax D$#1 | mov eax D$eax | call D$eax+#3 D$#1 #4>L]

when i write this code ( DxCall lpdd '->' Release) , by unfolding or showing in Debuger
it looks so: 

mov eax D$lpdd 
mov eax D$eax 

where is the rest!!! > ; that's because there is no fourth Parameter !! 

instead i use this one:

[DxCall | mov eax D$#1 | mov eax D$eax | push #L>3 | push D$#1 | call D$eax+#2]

____________________________________________________________________________________________

____________________________________________________________________________________________

    
____________________________________________________________________________________________

 
 Long Jumps optimized to short, when possible by Configuration Flag???...

____________________________________________________________________________________________
 
 
 Namings Check on [Include].

 implement a Memory remaping instead of 1 Mega Limit.
 
 ____________________________________________________________________________________________
 
 
 Scrolling text horizontally sucks
 
 By the way, i don't know how the Errors Manager deals with Run-Time errors
 in DB... I'll have to take a look at this... (To-Do List...)

____________________________________________________________________________________________
____________________________________________________________________________________________

 
 Unify ProgressBar creations, scaling, destructions. 
 
______________________________________________
 
 >[Data: DD 0] , without PREPARSE alternate >>> error pointing to the very first 'D' in
 the Source (!!!) (unknown symbol in 'BuildRelocationAndFillSymbols')... So, revue the
 way the Error Management in that computing search for the faultive Statement.
 
 General clean up of the Sources Editor needed. For example, 'StripBackSpace' is no
 more of any use. 
 
 Follow up with Jonne about Prefetch. 
 (Commenting out "cmp B$EregInside &TRUE | je L1>"  OK ???).
 
 File name when loading .asm!!!! Whishable to change? Without Title... yes...
 
 Bug inside the User Menu definition: Only the first Item was effective. Set *all*
 Paths to &MAX_PATH.
 
 Tree View: suppress the reduce Button in the bar, when runing in Auto-Hide Mode.

 Turn all Api calls into Macros, and store all system calls in one [System] TITLE.

 'DebugActiveProcessStop' >>> Download a more recent Win Help...
 
 Might hangs on upload of a non RosAsm written PE. Maybe the concerned PE did got a '.scr'
 section... See this next time.

 May hang when loading sources with broken resources.

 
 To do???: turn [ ... | ... #1>L | ... | #+1] possible. Actually, only #1, #2,...
 can be rolled.

 Problem with CreateDialog... vs DialogBox... Exit does not behave as Win Doc says.

 We could have a 'ReDo' feature if commenting out the 'ClearNextUndoRecord' call in
 'ControlZ' (keep one for the 'TitleMoveFlag'), and implemeting a 'ControlShiftZ' Routine.

 Add something for Extended Styles in the Dialog Editor. Usefull, for example
 for having a ToolWindow Style Dialog, without modifying in Init.

 In the Dialog Editor: Save to ClipBord >>> turn Styles into Win Equates expressions.

 Reset the overall -general purpose- Comments at Top of each TITLE Parts (partially done).
 
 DLL without anything to Reloc > RelocSize = 8 !!!!!!!!!!!!!!!!! (seems to work OK).
 May be this is even required by the OS (it seems to effectively be required).


__________________
 
 The Api Function calls by Numbers seems wrong in the Disassembly.
 

Disassembler Menus: Menu of Win32Dasm incomplete.
 
____________________________

Add a 0 to 9 UpAndDown Control in the Structure Dialog for multiple Structures.

Re-Write the DkStructures.rtf. Add Examples with Equates forms.

compatible symb.table for external Debuggers

____________________________

The 'EditDialogBoxProc' organisation is now unreadable. An important enhancement
should be done with implementing a Tab Control, under the Main List Box. This Tab 
should say [Style][Dim][ID/Menu][Class][Title][Font/Cdata]. Then, Holding the Tab
Index would much simplify and organise the holding of incoming Messages from the 
various Definitions Controls.

Add the Extended Styles. Search first for what Extended Styles may be coming
with the controls. (Even unsure for the Dialog...). Limit to the ensured ones.

Rewrite, in B_U_Asm [Editors][Dialog_Editor], because the guys do not understand
why some things available in RadAsm are not made available in RosAsm Dialog Editor,
Explain the 'MustHaveBitTable', and the 'ExcludeBitTable' implementation (it
seems nobody noticed this... :().

____________________________________________________________________________________________
____________________________________________________________________________________________

In Asm32Tuts:

Strings: Description / Cases / Endings // Length // Searches //
Copying // Pasting / manipulation.
__________________

 Re-Write C_To_Asm, as HLL_To_Asm. Memory / Pointers / Data / Constructs / Size vs Types
 ...

____________________________________________________________________________________________
____________________________________________________________________________________________

Main Implementations to be entirely done:

* HLL Parser(s). -Non Assembly syntax- . Anybody can do it. A Start point is at 'NewParser'.

* OOA Parsers. -Non Assembly syntax- . Experiment a "Couple of hours" tasks managment.

* Conditional Assembly. Start point at 'MacroWithIf'.

* Version Info Resource. I do not know what this Resource is, physically, but there is
  a description in GorC Resources Compiler Manual.

* Code symbolic Profiler. Only me can do do it.

* Source Ripper. Only me can do it.

* Wizards. (Visual Editors sets, in a DLL, for ToolBars, all Windows Types, from simple
  Buttons to MDI Editors, the various readers and players,... ). Anybody can do it. As
  soon as a volunteer raises his hand up for one of these, open a [User Project] for the
  Wizard Collection, at the Board.
  
* Flirt recognition in the Disassembler. Download and study IdaPro and Dede Disassemblers
  first...
  
* Implement ROS Drivers output. First, find the NT Drivers specifications.
  - They are PEs, but, for example, NT KeyBoard.sys seems a raw Binary.
  - How many Types of Drivers? What .ext?
  - 'PeHeaderCharacteristics' should have something specific.
  - 'ImageBase' ?
  - What Entry Point organisation?
  - What developements rules?
  - What Sections?
  
* A new tool would be great for 'tracking' a Variable. Example, after xxx modifications
  of the Assembler, when fixing a bug inside 'StoreVirtualData, i don't remember what i 
  am doing with 'D$DataListPtr''. The question i wish the answer to is: "Do i make any 
  use of this Variable *after* this given point of the Computation? If yes, where?".
  I think i don't use any more this Variable downward, but, if i turn it zero, nothing
  works... So, i must use it somewhere... Maybe 're-use' for something completely 
  different, that would better require another name...
  
  I imagine an added Double-Click Menu Option saying [Track] and outputing something 
  like a Tree-View of the outines making use of it. It should work the same way for Labels
  (Data and Code), Equates and Macros, and would be great for restructuring, renaming,
  and so on. May be, simply the existing Tree-View, but with the concerned Routines 
  written in Red, or something like this.


;;


; Create a function to retrieve the filename from handle during debugging

; See http://msdn.microsoft.com/en-us/library/windows/desktop/aa366789(v=vs.85).aspx

;;


#include <windows.h>
#include <stdio.h>
#include <tchar.h>
#include <string.h>
#include <psapi.h>
#include <strsafe.h>

#define BUFSIZE 512

BOOL GetFileNameFromHandle(HANDLE hFile) 
{
  BOOL bSuccess = FALSE;
  TCHAR pszFilename[MAX_PATH+1];
  HANDLE hFileMap;

  // Get the file size.
  DWORD dwFileSizeHi = 0;
  DWORD dwFileSizeLo = GetFileSize(hFile, &dwFileSizeHi); 

  if( dwFileSizeLo == 0 && dwFileSizeHi == 0 )
  {
     _tprintf(TEXT("Cannot map a file with a length of zero.\n"));
     return FALSE;
  }

  // Create a file mapping object.
  hFileMap = CreateFileMapping(hFile, 
                    NULL, 
                    PAGE_READONLY,
                    0, 
                    1,
                    NULL);

  if (hFileMap) 
  {
    // Create a file mapping to get the file name.
    void* pMem = MapViewOfFile(hFileMap, FILE_MAP_READ, 0, 0, 1);

    if (pMem) 
    {
      if (GetMappedFileName (GetCurrentProcess(), 
                             pMem, 
                             pszFilename,
                             MAX_PATH)) 
      {

        // Translate path with device name to drive letters.
        TCHAR szTemp[BUFSIZE];
        szTemp[0] = '\0';

        if (GetLogicalDriveStrings(BUFSIZE-1, szTemp)) 
        {
          TCHAR szName[MAX_PATH];
          TCHAR szDrive[3] = TEXT(" :");
          BOOL bFound = FALSE;
          TCHAR* p = szTemp;

          do 
          {
            // Copy the drive letter to the template string
            *szDrive = *p;

            // Look up each device name
            if (QueryDosDevice(szDrive, szName, MAX_PATH))
            {
              size_t uNameLen = _tcslen(szName);

              if (uNameLen < MAX_PATH) 
              {
                bFound = _tcsnicmp(pszFilename, szName, uNameLen) == 0
                         && *(pszFilename + uNameLen) == _T('\\');

                if (bFound) 
                {
                  // Reconstruct pszFilename using szTempFile
                  // Replace device path with DOS path
                  TCHAR szTempFile[MAX_PATH];
                  StringCchPrintf(szTempFile,
                            MAX_PATH,
                            TEXT("%s%s"),
                            szDrive,
                            pszFilename+uNameLen);
                  StringCchCopyN(pszFilename, MAX_PATH+1, szTempFile, _tcslen(szTempFile));
                }
              }
            }

            // Go to the next NULL character.
            while (*p++);
          } while (!bFound && *p); // end of string
        }
      }
      bSuccess = TRUE;
      UnmapViewOfFile(pMem);
    } 

    CloseHandle(hFileMap);
  }
  _tprintf(TEXT("File name is %s\n"), pszFilename);
  return(bSuccess);
}

int _tmain(int argc, TCHAR *argv[])
{
    HANDLE hFile;

    if( argc != 2 )
    {
        _tprintf(TEXT("This sample takes a file name as a parameter.\n"));
        return 0;
    }
    hFile = CreateFile(argv[1], GENERIC_READ, FILE_SHARE_READ, NULL,
        OPEN_EXISTING, 0, NULL);

    if(hFile == INVALID_HANDLE_VALUE)
    {
        _tprintf(TEXT("CreateFile failed with %d\n"), GetLastError());
        return 0;
    }
    GetFileNameFromHandle( hFile );
}


;;

; http://stackoverflow.com/questions/65170/how-to-get-name-associated-with-open-handle
;;

from ctypes import * 
# get handle to  c:\boot.ini to test 
handle = windll.kernel32.CreateFileA("c:\\boot.ini", 0x80000000, 3, 0, 3, 0x80, 0) 
hfilemap = windll.kernel32.CreateFileMappingA(handle, 0, 2, 0, 1, 0) 
pmem = windll.kernel32.MapViewOfFile(hfilemap, 4, 0, 0, 1) 
name = create_string_buffer(1024) 
windll.psapi.GetMappedFileNameA(windll.kernel32.GetCurrentProcess(), pmem, name, 1024) 
print "The name for the handle 0x%08x is %s" % (handle, name.value) 
# convert device name to drive letter 
buf = create_string_buffer(512) 
size = windll.kernel32.GetLogicalDriveStringsA(511, buf) 
names = buf.raw[0:size-1].split("\0") 
for drive in names: 
    windll.kernel32.QueryDosDeviceA(drive[0:2], buf, 512) 
    if name.value.startswith(buf.value): 
        print "%s%s" % (drive[0:2], name.value[len(buf.value):]) 
        break 

;;

; http://stackoverflow.com/questions/65170/how-to-get-name-associated-with-open-handle
;;

There is a correct (although undocumented) way to do this on Windows XP which also works with directories
 -- the same method GetFinalPathNameByHandle uses on Windows Vista and later.
Here are the eneded declarations. Some of these are already in WInternl.h and MountMgr.h but I just put them here anyway:
#include "stdafx.h" 
#include <Windows.h> 
#include <assert.h> 
 
enum OBJECT_INFORMATION_CLASS { ObjectNameInformation = 1 }; 
enum FILE_INFORMATION_CLASS { FileNameInformation = 9 }; 
struct FILE_NAME_INFORMATION { ULONG FileNameLength; WCHAR FileName[1]; }; 
struct IO_STATUS_BLOCK { PVOID Dummy; ULONG_PTR Information; }; 
struct UNICODE_STRING { USHORT Length; USHORT MaximumLength; PWSTR Buffer; }; 
struct MOUNTMGR_TARGET_NAME { USHORT DeviceNameLength; WCHAR DeviceName[1]; }; 
struct MOUNTMGR_VOLUME_PATHS { ULONG MultiSzLength; WCHAR MultiSz[1]; }; 
 
extern "C" NTSYSAPI NTSTATUS NTAPI NtQueryObject(IN HANDLE Handle OPTIONAL, 
    IN OBJECT_INFORMATION_CLASS ObjectInformationClass, 
    OUT PVOID ObjectInformation OPTIONAL, IN ULONG ObjectInformationLength, 
    OUT PULONG ReturnLength OPTIONAL); 
extern "C" NTSYSAPI NTSTATUS NTAPI NtQueryInformationFile(IN HANDLE FileHandle, 
    OUT PIO_STATUS_BLOCK IoStatusBlock, OUT PVOID FileInformation, 
    IN ULONG Length, IN FILE_INFORMATION_CLASS FileInformationClass); 
 
#define MOUNTMGRCONTROLTYPE ((ULONG) 'm') 
#define IOCTL_MOUNTMGR_QUERY_DOS_VOLUME_PATH \ 
    CTL_CODE(MOUNTMGRCONTROLTYPE, 12, METHOD_BUFFERED, FILE_ANY_ACCESS) 
 
union ANY_BUFFER { 
    MOUNTMGR_TARGET_NAME TargetName; 
    MOUNTMGR_VOLUME_PATHS TargetPaths; 
    FILE_NAME_INFORMATION NameInfo; 
    UNICODE_STRING UnicodeString; 
    WCHAR Buffer[USHRT_MAX]; 
}; 
Here's the core function:
LPWSTR GetFilePath(HANDLE hFile) 
{ 
    static ANY_BUFFER nameFull, nameRel, nameMnt; 
    ULONG returnedLength; IO_STATUS_BLOCK iosb; NTSTATUS status; 
    status = NtQueryObject(hFile, ObjectNameInformation, 
        nameFull.Buffer, sizeof(nameFull.Buffer), &returnedLength); 
    assert(status == 0); 
    status = NtQueryInformationFile(hFile, &iosb, nameRel.Buffer, 
        sizeof(nameRel.Buffer), FileNameInformation); 
    assert(status == 0); 
    //I'm not sure how this works with network paths... 
    assert(nameFull.UnicodeString.Length >= nameRel.NameInfo.FileNameLength); 
    nameMnt.TargetName.DeviceNameLength = (USHORT)( 
        nameFull.UnicodeString.Length - nameRel.NameInfo.FileNameLength); 
    wcsncpy(nameMnt.TargetName.DeviceName, nameFull.UnicodeString.Buffer, 
        nameMnt.TargetName.DeviceNameLength / sizeof(WCHAR)); 
    HANDLE hMountPointMgr = CreateFile(_T("\\\\.\\MountPointManager"), 
        0, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, 
        NULL, OPEN_EXISTING, 0, NULL); 
    __try 
    { 
        DWORD bytesReturned; 
        BOOL success = DeviceIoControl(hMountPointMgr, 
            IOCTL_MOUNTMGR_QUERY_DOS_VOLUME_PATH, &nameMnt, 
            sizeof(nameMnt), &nameMnt, sizeof(nameMnt), 
            &bytesReturned, NULL); 
        assert(success && nameMnt.TargetPaths.MultiSzLength > 0); 
        wcsncat(nameMnt.TargetPaths.MultiSz, nameRel.NameInfo.FileName, 
            nameRel.NameInfo.FileNameLength / sizeof(WCHAR)); 
        return nameMnt.TargetPaths.MultiSz; 
    } 
    __finally { CloseHandle(hMountPointMgr); } 
} 
and here's an example usage:
int _tmain(int argc, _TCHAR* argv[]) 
{ 
    HANDLE hFile = CreateFile(_T("\\\\.\\C:\\Windows\\Notepad.exe"), 
        0, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL); 
    assert(hFile != NULL && hFile != INVALID_HANDLE_VALUE); 
    __try 
    { 
        wprintf(L"%s\n", GetFilePath(hFile)); 
        //  Prints: 
        //  C:\Windows\notepad.exe 
    } 
    __finally { CloseHandle(hFile); } 
    return 0; 
} 



;;

Proc GetFileNameFromHandle:
    Arguments @hWnd, @Fname
    Local @hfilemap, @pMem, @MapMaxSize

    mov D@MapMaxSize 1
    call 'kernel32.CreateFileMappingA' D@hWnd, &NULL, &PAGE_READONLY, 0, D@MapMaxSize, &NULL
    mov D@hfilemap eax

    call 'kernel32.MapViewOfFile' D@hfilemap, &FILE_MAP_READ, 0, 0, D@MapMaxSize
    mov D@pMem eax

    call 'kernel32.GetCurrentProcess'

    call 'psapi.GetMappedFileNameA' eax, D@pMem, D@Fname, 1024

EndP

;;
Proc ParseFileNameFromHandle:
    Arguments @Fname, @BuffSize
    Local @hfilemap, @pMem, @MapMaxSize

    mov D@MapMaxSize 1
    call 'kernel32.CreateFileMappingA' D@hWnd, &NULL, &PAGE_READONLY, 0, D@MapMaxSize, &NULL
    mov D@hfilemap eax

    call 'kernel32.MapViewOfFile' D@hfilemap, &FILE_MAP_READ, 0, 0, D@MapMaxSize
    mov D@pMem eax

    call 'kernel32.GetCurrentProcess'

    call 'psapi.GetMappedFileNameA' eax, D@pMem, D@Fname, 1024

EndP
;;


;;
    GetFullFileNameFromHandleW
    GetFullFileNameFromHandle

    These functions retrieves te full file name and path of a given module handle.
    It is specially used for the cases where the file is on a server or another location
    from where the original Drive is not showed.
;;


[MAX_SIZE &MAX_PATH]
[BUFSIZE (2*MAX_SIZE)];512]

[szDrive: W$ 0 #4;3
 SzTemp: W$ 0 #BUFSIZE
 szTempFile: W$ 0 #&MAX_PATH
 szName: W$ 0 #&MAX_PATH]

Proc GetFullFileNameFromHandleW:
    Arguments @hFile, @pszFilename
    Local @dwFileSizeHi, @dwFileSizeLo, @hFileMap, @pMem, @uNameLen
    Uses ebx, esi, edi, ecx, edx

    mov D@dwFileSizeHi 0
    lea eax D@dwFileSizeHi
    call 'KERNEL32.GetFileSize' D@hFile, eax
    If_And D@dwFileSizeHi = 0, eax = 0
        xor eax eax
        ExitP
    End_If

    ; Create a file mapping object.
    call 'KERNEL32.CreateFileMappingW' D@hFile, &NULL, &PAGE_READONLY, 0, 1, &NULL
    If eax = &NULL
        xor eax eax
        ExitP
    End_If
    mov D@hFileMap eax

    ;  Create a file mapping to get the file name.
    call 'KERNEL32.MapViewOfFile' D@hFileMap, &FILE_MAP_READ, 0, 0, 1
    If eax = 0
        call 'KERNEL32.CloseHandle' D@hFileMap
        xor eax eax
        ExitP
    End_If
    mov D@pMem eax

    call 'KERNEL32.GetCurrentProcess'
    call 'PSAPI.GetMappedFileNameW' eax, D@pMem, D@pszFilename, &MAX_PATH
    mov ebx eax ; Copy result to see if we wll exit or not
    ; we can now safelly unmap and close the handle. We don´ need them anylonger
    If eax <> 0
        call 'KERNEL32.UnmapViewOfFile' D@pMem
    End_If
    call 'KERNEL32.CloseHandle' D@hFileMap

    If ebx = 0 ; If we wasn´t able to map the file we closed and unmapped previously, then Exit.
        xor eax eax
        ExitP
    End_If

    ; Translate path with device name to drive letters.
    call 'KERNEL32.GetLogicalDriveStringsW' (BUFSIZE-1), SzTemp
    If eax = 0
        xor eax eax
        ExitP
    End_If

    ;mov D$szDrive { U$ " :", 0} Whatch it. RosAsm gives an error here ? The strings are not being used as unicode
    mov esi szTemp

    .Do

        mov cx W$esi | mov W$szDrive cx
        mov cx W$esi+2 | mov W$szDrive+2 cx
        mov cx W$esi+4 | mov W$szDrive+4 0

        call 'KERNEL32.QueryDosDeviceW' szDrive, szName, &MAX_PATH
        ...If eax <> 0
            C_call 'msvcrt.wcslen' szName
            mov D@uNameLen eax
            ..If D@uNameLen < &MAX_PATH
                C_call 'msvcrt._wcsnicmp' D@pszFilename, szName, D@uNameLen
                .If eax = 0
                    mov edi D@pszFilename
                    mov eax D@uNameLen
                    movzx ecx W$edi+eax*2
                    If ecx = '\'
                        mov eax D@uNameLen
                        lea ecx D$edi+eax*2
                        C_call StringCchPrintfW szTempFile, &MAX_PATH, { U$ "%s%s", 0}, szDrive, ecx
                        C_call 'msvcrt.wcslen' szTempFile
                        call StringCchCopyNW edi, (&MAX_PATH+1), szTempFile, eax
                        mov eax &TRUE | ExitP
                    End_If
                .End_If
            ..End_If
        ...End_If

        Do
            movzx ecx W$esi
            add esi 2
        Loop_Until ecx = 0

    .Loop_Until W$esi = 0

    xor eax eax

EndP
_____________________________________________________________________________________________________________

[MAX_SIZE2 &MAX_PATH]
[BUFSIZE2 MAX_SIZE2]

[SzTemp2: B$ 0 #BUFSIZE
 szName2: B$ 0 #&MAX_PATH]

Proc GetFullFileNameFromHandle:
    Arguments @hFile, @pszFilename
    Local @dwFileSizeHi, @dwFileSizeLo, @hFileMap, @pMem, @uNameLen, @szDrive, @InputStringLen
    Uses ebx, esi, edi, ecx, edx

    mov D@dwFileSizeHi 0
    lea eax D@dwFileSizeHi
    call 'KERNEL32.GetFileSize' D@hFile, eax
    If_And D@dwFileSizeHi = 0, eax = 0
        xor eax eax
        ExitP
    End_If

    ; Create a file mapping object.
    call 'KERNEL32.CreateFileMappingA' D@hFile, &NULL, &PAGE_READONLY, 0, 1, &NULL
    If eax = &NULL
        xor eax eax
        ExitP
    End_If
    mov D@hFileMap eax

    ;  Create a file mapping to get the file name.
    call 'KERNEL32.MapViewOfFile' D@hFileMap, &FILE_MAP_READ, 0, 0, 1
    If eax = 0
        call 'KERNEL32.CloseHandle' D@hFileMap
        xor eax eax
        ExitP
    End_If
    mov D@pMem eax

    call 'KERNEL32.GetCurrentProcess'
    call 'PSAPI.GetMappedFileNameA' eax, D@pMem, D@pszFilename, &MAX_PATH
    mov ebx eax ; Copy result to see if we wll exit or not
    ; we can now safelly unmap and close the handle. We don´ need them anylonger
    If eax <> 0
        call 'KERNEL32.UnmapViewOfFile' D@pMem
    End_If
    call 'KERNEL32.CloseHandle' D@hFileMap

    If ebx = 0 ; If we wasn´t able to map the file we closed and unmapped previously, then Exit.
        xor eax eax
        ExitP
    End_If

    ; Translate path with device name to drive letters.
    call 'KERNEL32.GetLogicalDriveStringsA' (BUFSIZE2-1), SzTemp2
    If eax = 0
        xor eax eax
        ExitP
    End_If


    C_call 'msvcrt.strlen' D@pszFilename
    add eax 2; added 2 bytes in case we find the proper drivers lettrs "A:", "B:", "C:" etc etc
    mov D@InputStringLen eax
    ;mov D$szDrive { U$ " :", 0} Whatch it. RosAsm gives an error here ? The strings are not being used as unicode
    mov esi szTemp2
    lea ebx D@szDrive
    .Do

        movzx ecx W$esi | mov D$ebx ecx
        call 'KERNEL32.QueryDosDeviceA' ebx, szName2, &MAX_PATH
        On eax = 0, jmp L1>>
        ; eax hold the len of the SzName2, but in tchars. So, it may include extra zeroes at the end
        ; Instead recalculating the string len it is easier to we compute them through the end of the string
        ;  and scanning it untill no more zero ending are found. Once we found the last non zero byte
        ; we exit the loop nd increment the counter again to it gets the real len of the string.
        While B$szName2+eax = 0
            dec eax
        End_While
        inc eax
        ...If eax < &MAX_PATH
            mov D@uNameLen eax
            C_call 'msvcrt._strnicmp' D@pszFilename, szName2, D@uNameLen
            ..If eax = 0
                mov edi D@pszFilename
                mov eax D@uNameLen
                movzx ecx B$edi+eax
                .If ecx = '\'
                    mov eax D@uNameLen
                    lea ecx D$edi+eax
                    C_call StringCchPrintf edi, &MAX_PATH, { B$ "%s%s", 0}, ebx, ecx

                    ; To make things faster, i simply recalculated the remainder bytes of the whole string (that is, in fact uNameLen)
                    ; insetad having to use temporary buffer to compute a strlen function followed by StringCchCopyN;
                    ; Then i found the end of the real string and simply zero all the rest that is after it.
                    ; InputStringLen was incremented with 2 bytes to hold the driver letter followed by the colon. Ex: "C:"

                    mov edi D@InputStringLen | sub edi D@uNameLen | add edi D@pszFilename
                    ; our string ends here at edi All the rest is pure garbage
                    mov ecx D@uNameLen ; get the remainder bytes
                    xor eax eax
                    rep stosb ; zero al the garbage
                    mov eax &TRUE | ExitP
                .End_If
            ..End_If
        ...End_If
        L1:
        Do
            movzx ecx B$esi
            inc esi
        Loop_Until ecx = 0

    .Loop_Until B$esi = 0

    xor eax eax

EndP


; for debugging purposes
Proc GetFullFileNameFromProcessEx:
    Arguments @hProcess, @BaseAddress, @pszFilename
    Local @dwFileSizeHi, @dwFileSizeLo, @hFileMap, @uNameLen, @szDrive, @InputStringLen
    Uses ebx, esi, edi, ecx, edx

    call 'PSAPI.GetMappedFileNameA' D@hProcess, D@BaseAddress, D@pszFilename, &MAX_PATH
    mov ebx eax ; Copy result to see if we wll exit or not
    ; we can now safelly unmap and close the handle. We don´ need them anylonger
    ;If eax <> 0
    ;    call 'KERNEL32.UnmapViewOfFile' D@pMem
    ;End_If
    ;call 'KERNEL32.CloseHandle' D@hFileMap

    If ebx = 0 ; If we wasn´t able to map the file we closed and unmapped previously, then Exit.
        xor eax eax
        ExitP
    End_If

    ; Translate path with device name to drive letters.
    call 'KERNEL32.GetLogicalDriveStringsA' (BUFSIZE2-1), SzTemp2
    If eax = 0
        xor eax eax
        ExitP
    End_If


    C_call 'msvcrt.strlen' D@pszFilename
    add eax 2; added 2 bytes in case we find the proper drivers lettrs "A:", "B:", "C:" etc etc
    mov D@InputStringLen eax
    ;mov D$szDrive { U$ " :", 0} Whatch it. RosAsm gives an error here ? The strings are not being used as unicode
    mov esi szTemp2
    lea ebx D@szDrive
    .Do

        movzx ecx W$esi | mov D$ebx ecx
        call 'KERNEL32.QueryDosDeviceA' ebx, szName2, &MAX_PATH
        On eax = 0, jmp L1>>
        ; eax hold the len of the SzName2, but in tchars. So, it may include extra zeroes at the end
        ; Instead recalculating the string len it is easier to we compute them through the end of the string
        ;  and scanning it untill no more zero ending are found. Once we found the last non zero byte
        ; we exit the loop nd increment the counter again to it gets the real len of the string.
        While B$szName2+eax = 0
            dec eax
        End_While
        inc eax
        ...If eax < &MAX_PATH
            mov D@uNameLen eax
            C_call 'msvcrt._strnicmp' D@pszFilename, szName2, D@uNameLen
            ..If eax = 0
                mov edi D@pszFilename
                mov eax D@uNameLen
                movzx ecx B$edi+eax
                .If ecx = '\'
                    mov eax D@uNameLen
                    lea ecx D$edi+eax
                    C_call StringCchPrintf edi, &MAX_PATH, { B$ "%s%s", 0}, ebx, ecx

                    ; To make things faster, i simply recalculated the remainder bytes of the whole string (that is, in fact uNameLen)
                    ; insetad having to use temporary buffer to compute a strlen function followed by StringCchCopyN;
                    ; Then i found the end of the real string and simply zero all the rest that is after it.
                    ; InputStringLen was incremented with 2 bytes to hold the driver letter followed by the colon. Ex: "C:"

                    mov edi D@InputStringLen | sub edi D@uNameLen | add edi D@pszFilename
                    ; our string ends here at edi All the rest is pure garbage
                    mov ecx D@uNameLen ; get the remainder bytes
                    xor eax eax
                    rep stosb ; zero all the garbage
                    mov eax &TRUE | ExitP
                .End_If
            ..End_If
        ...End_If
        L1:
        Do
            movzx ecx B$esi
            inc esi
        Loop_Until ecx = 0

    .Loop_Until B$esi = 0

    xor eax eax

EndP


;;
Get file size under windows

Windows doesn’t have an API to get a file size based on file name. This small function does that.

It returns -1 if a file doesn’t exist.

It doesn’t handle files > 2 GB (max positive number for 32 bit signed value).
It’s quite easy to extend it to 64-bits if you know what is the 64 bit integer type in your compiler
(unfortunately there’s no standard).

A better design might be BOOL GetFileSize(const TCHAR *fileName, unsigned long *fileSizeOut)
i.e. returning false if file doesn’t exist and putting the file size into fileSizeOut.

long GetFileSize(const TCHAR *fileName)
{
    BOOL                        fOk;
    WIN32_FILE_ATTRIBUTE_DATA   fileInfo;

    if (NULL == fileName)
        return -1;

    fOk = GetFileAttributesEx(fileName, GetFileExInfoStandard, (void*)&fileInfo);
    if (!fOk)
        return -1;
    assert(0 == fileInfo.nFileSizeHigh);
    return (long)fileInfo.nFileSizeLow;
}

https://blog.kowalczyk.info/article/8f/Get-file-size-under-windows.html
;;

[WIN32_FILE_ATTRIBUTE_DATA.dwFileAttributesDis 0
 WIN32_FILE_ATTRIBUTE_DATA.ftCreationTime_dwLowDateTimeDis 4
 WIN32_FILE_ATTRIBUTE_DATA.ftCreationTime_dwHighDateTimeDis 8
 WIN32_FILE_ATTRIBUTE_DATA.ftLastAccessTime_dwLowDateTimeDis 12
 WIN32_FILE_ATTRIBUTE_DATA.ftLastAccessTime_dwHighDateTimeDis 16
 WIN32_FILE_ATTRIBUTE_DATA.ftLastWriteTime_dwLowDateTimeDis 20
 WIN32_FILE_ATTRIBUTE_DATA.ftLastWriteTime_dwHighDateTimeDis 24
 WIN32_FILE_ATTRIBUTE_DATA.nFileSizeHighDis 28
 WIN32_FILE_ATTRIBUTE_DATA.nFileSizeLowDis 32]

[Size_Of_WIN32_FILE_ATTRIBUTE_DATA 36]

Proc GetFileSizeFromFullName:
    Arguments @pfileName
    Structure @WIN32_FILE_ATTRIBUTE_DATA 36, @WIN32_FILE_ATTRIBUTE_DATA.dwFileAttributesDis 0,
                                             @WIN32_FILE_ATTRIBUTE_DATA.ftCreationTime_dwLowDateTimeDis 4,
                                             @WIN32_FILE_ATTRIBUTE_DATA.ftCreationTime_dwHighDateTimeDis 8,
                                             @WIN32_FILE_ATTRIBUTE_DATA.ftLastAccessTime_dwLowDateTimeDis 12,
                                             @WIN32_FILE_ATTRIBUTE_DATA.ftLastAccessTime_dwHighDateTimeDis 16,
                                             @WIN32_FILE_ATTRIBUTE_DATA.ftLastWriteTime_dwLowDateTimeDis 20,
                                             @WIN32_FILE_ATTRIBUTE_DATA.ftLastWriteTime_dwHighDateTimeDis 24,
                                             @WIN32_FILE_ATTRIBUTE_DATA.nFileSizeHighDis 28,
                                             @WIN32_FILE_ATTRIBUTE_DATA.nFileSizeLowDis 32

    If D@pfileName = 0
        mov eax 0-1
    End_If
    call 'RosMem.FastZeroMem' D@WIN32_FILE_ATTRIBUTE_DATA, Size_Of_WIN32_FILE_ATTRIBUTE_DATA
    call 'kernel32.GetFileAttributesExA' D@pfileName, &GETFILEEXINFOSTANDARD, D@WIN32_FILE_ATTRIBUTE_DATA
    If eax = 0
        mov eax 0-1 | ExitP
    End_If

    mov eax D@WIN32_FILE_ATTRIBUTE_DATA.nFileSizeLowDis

EndP


_______________________________________________________________________________

;;

    Need to fix all this syntax mistakes in RosAsm. It allow compilation of weird things lie below.
    See: MouseHint_FixStatements and the mousehint functions such as DBG_CleanUpCopyAndCook
         FirstParameterAnalyze

; 5x4 ; remainder= 12
[<16 Teste5x4:  F$ 1, 2, 3, 4, 5,
                F$ 6, 7, 8, 9, 10,
                F$ 11, 12, 13, 14, 15,
                F$ 16, 17, 18, 19, 20]

[ValueA4:   D$  1,
            D$  6,
            D$ 11,
            D$ 16]
[guga 7]

[PIX_WIDTH 20]

[<16 Output: B$ 0 #4096]
[RealNumber: R$ (*34/25.1 -2)] ; what a hell is this ???

mov eax D$-ebx+ecx*4 ; weird. Should not happen because D$ebx-ecx*4 is a impossible effective address
mov eax D$ecx*4+ebx
mov eax D$ebx*4+ecx
mov D$fs:guga+ValueA4 edx ; failed
push D$fs:ecx*4+9
push D$fs:(02*4+1-7)+ecx+(02*4+1-7)
push D$fs:ecx+(02*4+1-7)
push D$fs:02*4+1-7
;mov D$ValueA4+ecx*4 eax
mov D$ecx*4+ValueA4 eax
mov D$020+ValueA4 eax
mov D$020-ValueA4 eax

; push D$-45 ????
; push F$45.......2
push D$(*45) ;???
 mov eax D$ecx+(((+*-))) ; what is this ?
 mov eax D$ecx+((()))
push F$45.......2 ???????????
push 45++++++++ ????????
push (45++++++++) ??????????
mov D$ValueA4+()+() eax ?????????

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

call Matrix_Transpose_SSEmanus3 Teste5x4, Output, 5, 4

mov D$ValueA4+()+() eax

mov D$ValueA4+(5* (guga shl 2) + 18)+(5) eax

mov D$ValueA4+(5* (guga shl 2) + 18)+5 eax


mov eax D$ValueA4+(5 shl 2)



mov D$guga+ValueA4 edx



mov D$fs:guga+ValueA4 edx ; failed
mov D$fs:7-3 edx
mov D$fs:02*4 edx



mov D$guga+ValueA4 edx

mov D$fs:7-3 edx
mov D$fs:guga+ValueA4 edx

mov D$fs:02*4 edx

mov D$fs:07 edx


mov eax D$ValueA4+(    ) | ;

movups xmm1 X$esi+(16+(PIX_WIDTH*4*1))

mov eax D$ValueA4+(5* (guga shl 2) + 18)+(5)

mov eax D$ValueA4+(5* (guga shl 2) + 18)

;mov eax D$ValueA4+(5 or 2)

mov eax D$ValueA4+(guga * 2)

mov eax D$ValueA4+(5 shl 2)

call 'KERNEL32.ExitProcess' eax

;;

________________________________________________

;;
    FIX DatatoEquates routine

    Pasting this will lead to a crash here CheckPairings inside FromDataToStructure
[ DBG_Parameter2.Data: B$ 0 #256
 DBG_Parameter3.ParamType: D$ 0
 DBG_Parameter3.OpType: D$ 0
 DBG_Parameter3.DataSize: D$ 0
 DBG_Parameter3.Data: B$ 0 #256

;;
____________________________________________________

;;
    In MemoryInspectorProc, allow the memory to be refreshed
    
    ...ElseIf D@Message = WM_REFRESH_CONTENT
    
    The above have no effect when we select the menu to show the memory at DataViewDialog_OnCommand
    
    Sometimes, the adresses are changed and the memory is not updated on the proper viewer (MemoryInspectorProc)
;;



; Here is also crashing StoreUserAction




























































































































































































































































































































































































































































































































































































