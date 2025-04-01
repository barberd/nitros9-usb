********************************************************************
* KeyDrv - Keyboard Driver for USB
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   0      2024/01/25  Don Barber

                    nam       KeyDrv
                    ttl       Keyboard Driver for USB Keyboards

                  IFP1
                    use       defsfile
                    use       cocovtio.d
                    use       usb.d
                  ENDC

tylg                set       Systm+Objct
atrv                set       ReEnt+rev
rev                 set       $00
edition             equ       0

* These VTIO Global memory locations are used by the standard coco3
* KeyDrv, but are not referenced outside by vtio.asm or any system calls.
* Therefore a third-party keyboard driver can use them in other ways
* Those marked with an * are used in this implementation
* But not necessarily for the same purpose as keydrv_cc3.asm.
* if needed or desired:
* G.KeyMem (8 bytes)   *
* G.KTblLC
* G.KeyFlg    *  Used as a flag for repeat key checks instead
* G.LKeyCd    *
* G.2Key1     *  These 6 bytes store previous calls' data to determine
* G.2Key2     *  the new key presses
* G.2Key3     *
* G.Key1      *
* G.Key2      *
* G.Key3      *
* G.CapLok    *
* G.AltDwn    *
* Additionally, these bytes do not seem to be used by vtio or any other drivers.
* However, these run the risk that they may be used by other third-party
* drivers with the same idea:
* G.CapLok+1
* g0025

* These bytes are written by the Coco3 keyboard driver, and are used
* by vtio.asm and/or by system calls. Therefore this driver should update them
* in the same way:
* G.KySame used for repeat delay
* G.CntlDn used for keyboard mouse
* G.ShftDn used for keyboard mouse
* G.Clear  Used for keyboard mouse
* G.KySns  Used for keyboard mouse and SS.KySns system call

                    org       0
UsbKbd.DeviceId     rmb       1
UsbKbd.EndpointIn   rmb       1
UsbKbd.DataFlag     rmb       1
UsbKbd.MemSize      equ       .

                    mod       eom,name,tylg,atrv,entry,size
size                equ       .

name                fcs       /KeyDrv/
                    fcb       edition
usbmanname          fcs       /usbman/

NonAlphaTable
* Plain, shift, ctrl
                    fcb       $31,$21,$7c         1 ! |
                    fcb       $32,$40,$00         2 @ null
                    fcb       $33,$23,$7e         '3,'#,'~
                    fcb       $34,$24,$1d         '4,'$,GS (was null)
                    fcb       $35,$25,$1e         '5,'%,RS (was null)
                    fcb       $36,$5E,$1f         '6,'^,US (was null)
                    fcb       $37,$26,$5e         '7,'&','^
                    fcb       $38,$2A,$5b         '8,'*,'[
                    fcb       $39,$28,$5d         '9,'(,']
                    fcb       $30,$29,$81         '0,'),$81 (caps lock toggle)
                    fcb       $0d,$0d,$0d         ENTER key
                    fcb       $05,$03,$1b         BREAK key (ENQ,ETX,ESC)
                    fcb       $08,$08,$08         Backspace
                    fcb       $82,$83,$84         Tab (but set to same as clear on CoCo keyboard)
                    fcb       $20,$20,$20         SPACEBAR
                    fcb       $2d,$5f,$5f         '-,'_,'_
                    fcb       $3d,$2b,$5f         = + _
                    fcb       $5b,$7b,$00         [ { null
                    fcb       $5d,$7d,$00         ] } null
                    fcb       $5c,$7c,$00         \ | null
                    fcb       0,0,0               null,null,null
                    fcb       $3b,$3a,$00         ; : null
                    fcb       $27,$22,$7b         ' "" {
                    fcb       $60,$7E,$00         ` ~
                    fcb       $2C,$3C,$00         , <
                    fcb       $2E,$3E,$00         . >
                    fcb       $2F,$3F,$00         / ?
                    fcb       $81,$81,$81         Caps Lock
                    fcb       $B1,$B3,$B5         F1
                    fcb       $B2,$B4,$B6         F2
                    fcb       $09,$19,$11         RIGHT ARROW: HT, EM,DC1
                    fcb       $08,$18,$10         LEFT ARROW:  BS,CAN,DLE
                    fcb       $0a,$1a,$12         DOWN ARROW:  LF,SUB,DC2
                    fcb       $0c,$1c,$13         UP ARROW:    FF, FS,DC3

KbdDevMatch         fdb       $0000               0 VendorId
                    fdb       $0000               2 Mask
                    fdb       $0000               4 ProductId
                    fdb       $0000               6 Mask
                    fcb       $00                 8 DeviceClass
                    fcb       $00                 DeviceSubClass
                    fcb       $00                 10 ClassMask
                    fcb       $00                 SubClassMask
                    fcb       $00                 12 DeviceProtocol
                    fcb       $00                 13 Mask
                    fcb       $03                 14 InterfaceClass
                    fcb       $01                 InterfaceSubClass
                    fcb       $FF                 16 ClassMask
                    fcb       $FF                 SubClassMask
                    fcb       $01                 18 InterfaceProtocol
                    fcb       $FF                 19 Mask

entry               equ       *

* Init - Initialize keyboard
                    lbra      Init

* Term - Terminate keyboard
                    lbra      Term

* Function - Test for function keys
                    lbra      FuncKeys

* Read - read keys if pressed
* Exit: A = key pressed
ReadKys             pshs      dp
                    leas      -8,s
                    tst       UsbKbd.DeviceId,u
                    lbeq      nokey@
                    tfr       s,x
                    lbsr      KbdGetReport
                    tfr       d,y
                    ldu       <D.CCMem            Get VTIO global memory into U
                    tfr       u,d
                    tfr       a,dp
                    tfr       y,d
                    bcc       goodreport@
                    cmpb      #$2A                NAK report meaning no change
                    lbne      nokey@              if not a NAK, bail out here
                    tst       <G.LKeyCd           if last key was null then report no key
                    lbeq      nokey@
                    leas      -1,s                set up stack to work with repeatkey@
                    lbra      repeatkey@          $2A is legit repeat so treat like repeat key
goodreport@
                  IFNE    H6309
                    clrd
                  ELSE
                    clra
                    clrb
                  ENDC
                    std       <G.ShftDn           Also clears G.CntlDn
                    sta       <G.AltDwn
* First check for modifer keys in the first byte
* and set up flags
                    lda       ,x
                    #ldb      <G.KySns
                    clrb
                    bita      #$11
                    beq       notctrl@
                    inc       <G.CntlDn
                    orb       #CNTRLBIT
notctrl@
                    bita      #$22
                    beq       notshift@
                    com       <G.ShftDn
                    orb       #SHIFTBIT
notshift@
                    bita      #$44
                    beq       notalt@
                    inc       <G.AltDwn
                    orb       #ALTERBIT
notalt@
                    stb       <G.KySns
* Done with modifier keys.
* Now loop over 6 byte packet for KySns
* This loop just checks for space and arrow keys for KySns
* The packet will be looped over again to check for newly pressed keys below
                    leax      2,x
                    lda       #6
                    pshs      a
kysnsloop@
                    lda       ,x+
                    cmpa      #$2C
                    bne       notspace@
                    ldb       #SPACEBIT
                    bra       setkysns@
notspace@
* Handle arrow keys just for KySns here
                    cmpa      #$4F
                    blt       nextkey@
                    cmpa      #$52
                    bgt       nextkey@
                    suba      #$4F
                    pshs      a
                    lda       #$05
                    suba      ,s+
                    clrb
                    orcc      #Carry
bitloop@            rolb
                    deca
                    bne       bitloop@
                    rolb
                    rolb
                    rolb
setkysns@
                    orb       <G.KySns
                    stb       <G.KySns
nextkey@            dec       ,s
                    bne       kysnsloop@
                    leas      1,s
                    ldy       <G.CurDev           point X to device's static memory
                    tst       <V.KySnsFlg,y       is key sense on?
                    beq       maincheck@
                    #puls     cc
                    orcc      #Negative
                    lbra      finish@             if so, bail out here to save time
* Ok, now do the main loop to check for changed keys and process those
maincheck@
                    leax      -6,x                back up to beginning of key list
                    leay      G.2Key1,u
                    clr       <G.KeyFlg
                    lda       #6
                    pshs      a
changeloop@
                    lda       ,x+
                    cmpa      <G.LKeyCd
                    bne       notlastkey@
                    inc       <G.KeyFlg
notlastkey@
                    cmpa      ,y
                    beq       nochange@
                    sta       ,y                  Update to mark this key processed
                    tsta
                    bne       foundchange@        If its 0 that means no key pressed
nochange@
                    leay      1,y
                    dec       ,s
                    bne       changeloop@
* No changes, but check if last pressed key is still held down
* If so, return that
                    tst       <G.KeyFlg
                    bne       repeatkey@
                    leas      1,s                 get rid of counter
                    bra       nokey@              no keys at all are pressed, return nothing
repeatkey@
                    lda       <G.KySame
                    lda       <G.LKeyCd
                    inc       <G.KySame           flag for repeat key handling in vtio
foundchange@
                    leas      1,s                 get rid of counter
                    sta       <G.LKeyCd
* If here, we found a newly pressed key. Process.
* At this point, A contains USB keycode of a newly hit key
                    cmpa      #$04
                    blt       nokey@              no event or error
                    cmpa      #$1D
                    bgt       notalpha@
* Process alpha keys here
                    adda      #$3D                convert to ascii
                    ldb       <G.ShftDn
                    ldy       <G.CurDev
                    eorb      <V.ULCase,y
                    andb      #CapsLck
                    bne       wrapup@
                    adda      #$20                make lowercase
                    bra       wrapup@
notalpha@
                    cmpa      #$52                greater than chart so end
                    bgt       nokey@
                    cmpa      #$3B                F2 and lower proceed through
                    bls       lookup@
                    cmpa      #$4F
                    blt       nokey@              F3 through PageDown ignored
                    suba      #$13                shift arrow keys ($4F-$52) down to be at end of table
* Look up character on the NonAlphaTable
lookup@             suba      #$1E
                    ldb       #$03
                    mul                           convert to table offset
                    lda       <G.ShftDn
                    beq       notshift2@
                    incb
                    bra       dolookup@
notshift2@
                    lda       <G.CntlDn
                    beq       dolookup@
                    addb      #$02
dolookup@
                    leax      NonAlphaTable,pcr
                    lda       b,x
                    bmi       special@            if >=$80 then handle special
                    bra       wrapup@
special@            inc       <G.CapLok
                    inc       <G.Clear
                    bra       wrapend@
nokey@              clra
                    tst       <G.CapLok
                    bne       keeplkey@           if a special key, don't clear LKeyCd
                    sta       <G.LKeyCd
keeplkey@
                    sta       <G.CapLok
                    ldb       #$FF
                    andcc     #Negative
                    bra       finish@
wrapup@             ldb       <G.AltDwn
                    beq       wrapend@
                    cmpa      #$3F                ?
                    bls       doalt@
                    cmpa      #$5B                [
                    bhs       doalt@              skip to doalt if not a capital letter
                    ora       #$20                convert capitals to lower case
doalt@              ora       #$80
wrapend@            andcc     #^Negative
finish@             leas      8,s
                    puls      dp
                    rts

* U is G.KeyMem memory (located in D.CCMem)
* X is 8 byte buffer for report
* Returns carry set if no change
KbdGetReport        pshs      x,y,u
                    ldy       <D.USBManMem
* This is needed here because a keyboard read is
* running in system where it does not timeslice
* out. When another process happens to be using
* usb already, the keyboard routine does not yield
* time to allow the other job that has usb locked
* to run, resulting in a deadlock. Avoid that by
* just bailing out here if USB is locked.
                    tst       ,y                  This is USBLock in the USB manager memory area
                    bne       error@              Bail out if usb device is already locked
                    leas      -9,s
                    stx       USBITS.BufferPtr,s
                    ldd       #$0008
                    std       USBITS.BufferLength,s
                    stb       USBITS.MaxPacketSize,s max packet is always 8 for kbd boot protocol
                    stb       USBITS.NakFlag,s    bubble up NAKs, no retry
                    ldd       UsbKbd.DeviceId,u
                    std       USBITS.DeviceId,s   device and endpoint
                    lda       UsbKbd.DataFlag,u   data0/data1 flag
                    sta       USBITS.DataFlag,s
                    leax      ,s
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                    ldd       D.Proc              get curr proc ptr
                    pshs      d                   save on stack
                    ldd       D.SysPrc            get system process desc ptr
                    std       D.Proc              and make current proc
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    jsr       USBInTransfer,y
                  IFGT    Level-1
                    puls      x                   get curr proc ptr
                    stx       D.Proc              restore
                  ENDC
                    pshs      cc
                    lda       1+USBITS.DataFlag,s
                    sta       UsbKbd.DataFlag,u   save data0/data1 flag
                    puls      cc
                    leas      9,s
                    bra       finish@
error@              comb
finish@             puls      u,x,y,pc

* This entry point tests for the F1/F2 function keys on a CoCo 3
* keyboard.
* Exit: A = Function keys pressed (Bit 0 = F1, Bit 2 = F2)
FuncKeys            leas      -8,s
                    tst       UsbKbd.DeviceId,u
                    beq       error@
                    tfr       s,x
                    lbsr      KbdGetReport
                    bcs       error@
                    leax      2,s
                    lda       #6
                    sta       ,-s
                    clra
loop@               ldb       ,x+
                    cmpb      #$3A
                    bne       notf1@
                    inca                          set bit 0
notf1@              cmpb      #$3B
                    bne       notf2@
                    ora       #$04                set bit 2
notf2@
                    dec       ,s
                    bne       loop@
                    leas      1,s
                    bra       finish@
error@              clra
finish@             leas      8,s
                    rts

Init
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    bne       usbmanalready@
                    ldb       #UsbKbd.MemSize
                    pshs      u
                    clra
clrloop@            sta       ,u+
                    decb
                    bne       clrloop@
                  IFGT    Level-1
                    ldx       <D.Proc
                    pshs      x
                    ldx       <D.SysPrc
                    stx       <D.Proc
                  ENDC
                    leax      usbmanname,pcr
                    os9       F$Link
                  IFGT    Level-1
                    puls      x
                    stx       <D.Proc
                  ENDC
                    bcs       error@
                    jsr       ,y                  call USBMan init routine
                    puls      u                   restore u
                    bcs       finish@
usbmanalready@
                    leas      -8,s
                    leax      KbdDevMatch,pcr
                    stx       ,s
                    stu       2,s
                    leax      KbdProbe,pcr
                    stx       4,s
                    leax      KbdDisconnect,pcr
                    stx       6,s
                    tfr       s,x
                    jsr       USBRegisterDriver,y
                    leas      8,s
                    bra       finish@
error@              leas      2,s                 drop pushed u
                    andcc     #^Carry
finish@             rts

* U Memory Location
* Y Interface table entry
* X Interface entry in memory
* Returns Carry Clear if accept
KbdProbe            pshs      x,y
                    tst       UsbKbd.DeviceId,u
                    bne       error@
                    lda       USBInterfaceDeviceId,y
                    sta       UsbKbd.DeviceId,u
                    clr       UsbKbd.DataFlag,u
* Start looking for endpoint here
loop1@              lda       1,x                 descriptor type field
                    cmpa      #$05                $05 is endpoint descriptor
                    beq       foundendpoint@
                    clra
                    ldb       ,x
                    leax      d,x
                    bra       loop1@
foundendpoint@
* Record EndpointIn here
                    lda       2,x
                    sta       UsbKbd.EndpointIn,u
* Set boot protocol
                    leas      -13,s
                    leax      5,s
                    stx       USBCTS.SetupPktPtr,s
                    lda       UsbKbd.DeviceId,u
                    sta       USBCTS.DeviceId,s
                    ldd       #$210B
                    std       5,s
                    lda       USBInterfaceNum,y
                    sta       9,s
                    ldd       #$0000
                    std       7,s                 a=0=boot protocol
                    stb       10,s
                    std       11,s                0 data length
                    tfr       s,x
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    jsr       USBControlTransfer,y
                    leas      13,s
                    bcc       finish@
error@              
* Clear out local memory
                    clra
                    leax      ,u
                    ldb       #UsbKbd.MemSize
clrloop@            sta       ,x+
                    decb
                    bne       clrloop@
                    comb
finish@             puls      x,y,pc

KbdDisconnect
                    pshs      d,x
                    lda       ,x
                    cmpa      UsbKbd.DeviceID,u
                    bne       error@
                    clra
                    leax      ,u
                    ldb       #UsbKbd.MemSize
clrloop@            sta       ,x+
                    decb
                    bne       clrloop@
error@              comb
finish@             puls      d,x,pc

Term                pshs      y
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    leax      KbdProbe,pcr
                    jsr       USBDeregisterDriver,y
                    clrb
                    puls      y,pc

                    emod
eom                 equ       *
                    end
