********************************************************************
* JoyDrv - Joystick Driver for CoCo JoyStick + USB Mouse
*
* CoCo PIA used for Joystick routines J$JyBtn and J$JyXY 
* and USB Mouse used for Mouse routines J$MsBtn and J$MsXY
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   0      2025/02/18  Don Barber

                    nam       JoyDrv
                    ttl       Joystick Driver for USB Mouse

                    ifp1
                    use       defsfile
                    use       usb.d
                    endc

tylg                set       Systm+Objct
atrv                set       ReEnt+rev
rev                 set       $00
edition             set       0

HResMaxX            equ       639
HResMaxY            equ       191

* JoyMem storage area (max 8 bytes)
                    org       0
M.Button            rmb       1
M.X                 rmb       2
M.Y                 rmb       2
M.DeviceId          rmb       1
M.EndpointIn        rmb       1
M.DataFlag          rmb       1
                    mod       eom,name,tylg,atrv,start,0

name                fcs       /JoyDrv/
                    fcb       edition
usbmanname          fcs       /usbman/

MouseDevMatch  FDB       $0000               0 VendorId
               FDB       $0000               2 Mask
               FDB       $0000               4 ProductId
               FDB       $0000               6 Mask
               FCB       $00                 8 DeviceClass
               FCB       $00                 DeviceSubClass
               FCB       $00                 10 ClassMask
               FCB       $00                 SubClassMask
               FCB       $00                 12 DeviceProtocol
               FCB       $00                 13 Mask
               FCB       $03                 14 InterfaceClass
               FCB       $01                 InterfaceSubClass
               FCB       $FF                 16 ClassMask
               FCB       $FF                 SubClassMask
               FCB       $02                 18 InterfaceProtocol
               FCB       $FF                 19 Mask

start               lbra      Init
                    lbra      Term
                    lbra      SSMsBtn
                    lbra      SSMsXY
                    lbra      SSJoyBtn

SSJoyXY             pshs      x,b,a
                    ldx       #PIA0Base
                    lda       <$23,x
                    ldb       <$20,x
                    pshs      b,a
                    anda      #$F7
                    sta       <$23,x
                    lda       $01,x
                    ldb       $03,x
                    pshs      b,a
                    andb      #$F7
                    lsr       $04,s
                    bcs       L0043
                    orb       #$08
L0043               stb       $03,x
                    lda       ,s
                    ora       #$08
                    bsr       L0065
                    std       $06,s
                    lda       ,s
                    anda      #$F7
                    bsr       L0065
                    std       $04,s
                    puls      b,a
                    sta       $01,x
                    stb       $03,x
                    puls      b,a
                    stb       <$20,x
                    sta       <$23,x
                    puls      pc,y,x
L0065               sta       $01,x
                    lda       #$7F
                    ldb       #$40
                    bra       L0078
L006D               lsrb
                    cmpb      #$01
                    bhi       L0078
                    lsra
                    lsra
                    tfr       a,b
                    clra
                    rts
L0078               pshs      b
                    sta       <$20,x
                    tst       ,x
                    bpl       L0085
                    adda      ,s+
                    bra       L006D
L0085               suba      ,s+
                    bra       L006D

***
* JoyDrv Initialization.
*
* INPUT:  U = JoyDrv data area address (8 bytes)
*
* OUTPUT:  IRQ service entry installed
*          D, X, Y registers may be altered
*
* ERROR OUTPUT:  CC = Carry set
*                B = error code
Init                lbsr      ClrMem
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    bne       usbmanready@
                    pshs      u
                  IFGT    Level-1
                    ldx       <D.Proc
                    pshs      x
                    ldx       <D.SysPrc
                    stx       <D.Proc
                  ENDC
                    clra
                    leax      usbmanname,pcr
                    os9       F$Link
                  IFGT    Level-1
                    puls      x
                    stx       <D.Proc
                  ENDC
                    bcs       linkfailed@
                    jsr       ,y                  call USBMan init routine
linkfailed@         puls      u                   restore u
                    bcs       error@
usbmanready@
* Then register with USBMan
                    leas      -8,s
                    leax      MouseDevMatch,pcr
                    stx       ,s
                    stu       2,s
                    leax      MouseProbe,pcr
                    stx       4,s
                    leax      MouseDisconnect,pcr
                    stx       6,s
                    tfr       s,x
                    jsr       USBRegisterDriver,y
                    leas      8,s
                    bcc       finish@
error@              comb
finish@             rts

* U Memory Location
* Y Interface table entry
* X Interface entry in memory
* Returns Carry Clear if accept
MouseProbe          pshs      x,y
                    tst       M.DeviceId,u
                    bne       error@              We already have a mouse
                    lda       ,y
                    sta       M.DeviceId,u
                    clr       M.DataFlag,u
* Start looking for endpoint here
                    #lda      #$05
loop1@              lda       1,x
                    cmpa      #$05
                    beq       foundendpoint@
                    clra
                    ldb       ,x
                    leax      d,x
                    bra       loop1@
foundendpoint@
* Record EndpointIn here
                    lda       2,x
                    anda      #$7F
                    sta       M.EndpointIn,u
* Set boot protocol
                    leas      -13,s
                    leax      5,s
                    stx       USBCTS.SetupPktPtr,s
                    sta       9,s                 store EndpointIn
                    lda       M.DeviceId,u
                    sta       USBCTS.DeviceId,s
                    ldd       #$210B
                    std       5,s
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
                    bsr       ClrMem
                    comb
finish@             puls      x,y,pc

MouseDisconnect     pshs      d,u
                    lda       ,x
                    cmpa      M.DeviceId,u
                    bne       error@              did not find entry
                    bsr       ClrMem
                    bra       finish@
error@              comb
finish@             puls      u,d,pc

ClrMem              ldd       #$0007
clrloop@            sta       b,u
                    decb
                    bpl       clrloop@
                    rts

***
* JoyDrv Termination.
*
* INPUT:  U = JoyDrv data area address (8 bytes)
*
* OUTPUT:  IRQ service entry removed
*          D, X, and U registers may be altered
*
* ERROR OUTPUT:  CC = Carry set
*                B = error code
Term
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    leax      MouseProbe,pcr
                    jsr       USBDeregisterDriver,y
TermExit            rts

***
* Joystick button(s) status check.
*
* INPUT:  U = JoyDrv data area address (8 bytes)
*
* OUTPUT:  B = button or "clear" status
*              button(s)     = xxxxLRLR
*          A, X, and U registers may be altered
*
* ERROR OUTPUT:  none
SSJoyBtn            ldb       #$FF
                    ldx       #PIA0Base
                    stb       $02,x
                    ldb       ,x
                    comb
                    andb      #$0F
                    rts

* This adds a 'ballistic' response wherein
* a mouse movement greater than 8 steps results in adding almost
* 50%. This allows for fast jumps but tight precision.
DoBallistic         sex
                    pshs      d                   Save original
                    bpl       PosAdjust@
                    orb       #%00000111          Do nothing if > -8
                    addd      #1                  "fix" negative offset
                    bra       RShift
PosAdjust@          andb      #%11111000          Do nothing if < 8
RShift              asra                          Calculate 50%
                    rorb
                    addd      ,s++                Add to original
                    rts

* Read USB Mouse Packet here
* Store in Memory area
* Carry clear if success, set if error
ReadPkt             lda       M.DeviceId,u
                    beq       error@              if no mouse, then error
                    leas      -16,s               make room on stack
                    ldb       M.DeviceId,u
                    std       USBITS.DeviceId,s
                    leax      9,s
                    stx       USBITS.BufferPtr,s
                    ldd       #$0008
                    std       USBITS.BufferLength,s
                    stb       USBITS.NakFlag,s    bubble up NAKs
                    lda       M.DataFlag,u
                    ldb       #$08                boot protocol is always 8
                    std       USBITS.DataFlag,s
                    tfr       s,x
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    jsr       USBInTransfer,y
                    pshs      cc
                    lda       1+USBITs.DataFlag,s
                    sta       M.DataFlag,u
                    puls      cc
                    bcc       goodpacket@
                    cmpb      #$2E                 Got NAK, valid no update
                    bne       errorstack@
                    clrb
                    leas      16,s
                    bra       finish@
goodpacket@
                    ldb       10,s                 8 bit signed X delta
                    bsr       DoBallistic
                    addd      M.X,u
                    bpl       xpos@
                   IFNE       H6309
                    clrd
                   ELSE
                    clra
                    clrb
                   ENDC
                    bra       xstore@
xpos@               cmpd      #HResMaxX
                    ble       xstore@
                    ldd       #HResMaxX
xstore@             std       M.X,u
                    ldb       11,s                 8 bit signed Y delta
                    bsr       DoBallistic
                    addd      M.Y,u
                    bpl       ypos@
                   IFNE       H6309
                    clrd
                   ELSE
                    clra
                    clrb
                   ENDC
                    bra       ystore@
ypos@               cmpd      #HResMaxY*2
                    ble       ystore@
                    ldd       #HResMaxY*2
ystore@             std       M.Y,u
                    lda       9,s                  button byte to return
                    clrb
                    leas      16,s
                    bra       finish@
errorstack@         leas      16,s
error@              clra
                    comb
finish@             rts

***
* Mouse button(s) status check.
*
* INPUT:  U = JoyDrv data area address (8 bytes)
*
* OUTPUT:  B = button or "clear" status
*              button(s)     = xxxxLRLR <- buttons 1
*                                  ^^
*                                  buttons 2
*              clear         = 10000000
*              shift-clear   = 11000000
*          A, X, registers may be altered
*
* ERROR OUTPUT:  none
SSMsBtn             bsr       ReadPkt             returns usb button state in A
                    clrb
                    bita      #%00000001
                    beq       notleftbtn@
                    orb       #%00000011
notleftbtn@         bita      #000000110          treat middle as right too
                    beq       notrightbtn@
                    orb       #%00001100
notrightbtn@        lda       M.Button,u          load prior state
                    anda      #%00001100          test button 2
                    beq       finish@             was not down, go finish
                    pshs      a
                    tfr       b,a
* maybe conditional H6309 code for BEOR or BIEOR here?
                    eora      ,s+                 test if flipped
                    beq       finish@             has not release, go finish
                    orb       #%10000000          btn 2 was clicked and released
                    pshs      b
                    ldd       M.X,u
                    cmpd      #HResMaxX/2
                    puls      b
                    bge       finish@             finish if on right side
                    orb       #%01000000          its on left so shift-click 
finish@             stb       M.Button,u          store prior state
                    rts

***
* Joystick/Mouse XY coordinate read.
*
* INPUT:  A = side flag (1 = right, 2 = left)
*         Y = resolution (0 = low, 1 = high)
*         U = JoyDrv data area address (8 bytes)
*
* OUTPUT:  X = horizontal coordinate (left edge = 0)
*              low resolution = 0 to 63
*              high resolution = 0 to HResMaxX
*          Y = vertical coordinate (top edge = 0)
*              low resolution = 0 to 63
*              high resolution = 0 to HResMaxY
*          D and U registers may be altered
*
* ERROR OUTPUT:  none
* Note: ReadPkt is not called here. That is done by vtio calling MsBtn
* right before its keyboard read routine.
SSMsXY              ldx       M.X,u
                    ldd       M.Y,u
                    lsra
                    rorb
                    tfr       d,y
                    rts

                    emod
eom                 equ       *
                    end
