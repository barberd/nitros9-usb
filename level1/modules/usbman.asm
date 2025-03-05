********************************************************************
* USBMan - NitrOS-9 Level One USB Manager module
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*  01	   2023/12/31  Don Barber
* USB Manager written around the CH376 chip
* See https://github.com/barberd/cocousbhost for hardware design for CoCo.

                    nam       USBMan
                    ttl       USB Manager with Low Level Subroutines for CH376 Chip

                    use       defsfile
                    use       usb.d
                    use       ch376.d

USBMaxDevices       equ       6
USBMaxDrivers       equ       5                   Hub+MSD+KBD+Mouse+Printer
USBMaxInterfaces    equ       16
USBMaxHubs          equ       3

* USB Hub Record offsets
                    org       0
USBHubDeviceId      rmb       1
USBHubNumPorts      rmb       1
USBHubStatusEndpoint rmb       1
USBHubWMaxPacketSize rmb       2
USBHubDataFlag      rmb       1
USBHubRecLength     equ       .

* Static Memory starting at U goes here
                    org       0
USBLock             rmb       1
USBIntStatus        rmb       1
USBWakePid          rmb       1
USBPortConnected    rmb       1
USBDeviceTable      rmb       USBDeviceRecLength*USBMaxDevices
USBInterfaceTable   rmb       USBInterfaceRecLength*USBMaxInterfaces
USBDriverTable      rmb       USBDriverRecLength*USBMaxDrivers
USBHubTable         rmb       USBMaxHubs*USBHubRecLength
USBDeviceIdCache    rmb       1
USBNakRetryCache    rmb       1
USBMemSize          equ       .

tylg                set       Sbrtn+Objct
atrv                set       ReEnt+rev
rev                 set       $00

                    mod       eom,name,tylg,atrv,start,0

* Static vars go here
name                fcs       /USBMan/
UIrqFlip            fcb       $80
UIrqMask            fcb       $80
UIrqPriority        fcb       $C0
HubDevMatch         fdb       $0000               0 VendorId
                    fdb       $0000               2 Mask
                    fdb       $0000               4 ProductId
                    fdb       $0000               6 Mask
                    fcb       $09                 8 DeviceClass
                    fcb       $00                 9 DeviceSubClass
                    fcb       $FF                 10 ClassMask
                    fcb       $FF                 11 SubClassMask
                    fcb       $00                 12 DeviceProtocol
                    fcb       $00                 13 Mask
                    fcb       $09                 14 InterfaceClass
                    fcb       $00                 15 InterfaceSubClass
                    fcb       $FF                 16 ClassMask
                    fcb       $FF                 17 SubClassMask
                    fcb       $00                 18 InterfaceProtocol
                    fcb       $FF                 19 Mask

DeviceDesc          fcb       $80                 bmRequestType
                    fcb       $06                 bRequest
                    fdb       $0001               wValue
                    fdb       $0000               wIndex
                    fdb       $0800               wLength (08 bytes)

ConfigDesc          fcb       $80                 bmRequestType
                    fcb       $06                 bRequest
                    fdb       $0002               wValue
                    fdb       $0000               wIndex
*These 2 bytes would normally be included but is replaced by routines
*                fdb     $0800                wLength (08 bytes)

SvcTbl              fcb       F$USBPollHubs
                    fdb       PollHubs-*-2
                    fcb       $80

start               bra       Init
                    nop
                    lbra      RegisterDriver
                    lbra      DeregisterDriver
                    lbra      InTransfer
                    lbra      OutTransfer
                    lbra      ControlTransfer
                    lbra      ClearStall
                    lbra      SetConfiguration
                    lbra      ResetDevice
                    lbra      PollHubs

* initialize ch376
Init
                    pshs      u,y,x,d
                    pshs      cc
                    orcc      #IntMasks           mask interrupts while we set up everything
* Check if device exists first
                    lda       #CH376_RESET_ALL
                    sta       CH376_CMDREG
                    lbsr      Delay3Tk            Datasheet says wait 40ms. Delay3Tk is 50ms.
                    lda       #CH376_GET_STATUS   clear any interrupts
                    sta       CH376_CMDREG
                    lda       CH376_DATAREG
                    lda       #CH376_CHECK_EXIST  will return bitwise inverse
                    sta       CH376_CMDREG
                    lda       #$57
                    sta       CH376_DATAREG
                    lda       CH376_DATAREG
                    cmpa      #$A8                check if got inverse
                    lbne      InitErr@            if not, ch376 not found
* Get Memory
                    ldd       #USBMemSize
                    os9       F$SRqMem
                    lbcs      InitErr@
* Clear out memory
                    pshs      u
                    ldx       #USBMemSize
                    clra
clrloop@            sta       ,u+
                    leax      -1,x
                    bne       clrloop@
                    puls      u
                    lda       #$FF
                    sta       USBDeviceIdCache,u  Invalidate Device Id
* Store U into direct page for later retrieval
* This is also signal to usb drivers that this module is ready for
* registration
                    leax      start,pcr
                  IFGT    Level-1
                    stu       <D.USBManMem
                    stx       <D.USBManSubAddr
                  ELSE
                    stu       >D.USBManMem
                    stx       >D.USBManSubAddr
                  ENDC
* install IRQ handler for handling ch376 interrupt
                    ldd       #CH376_FLAGREG
                    leax      UIrqFlip,pcr
                    leay      IrqHandler,pcr
                    os9       F$IRQ
                   IFEQ      Level-1
                    lbcs      InitErrClrMem@
                   ELSE
                    bcs       InitErrClrMem@
                   ENDC
* Level 1 never sets up interrupt handling, so do that here
                   IFEQ      Level-1  
                    ldx       >D.FIRQ
                    pshs      x                   Store original D.FIRQ
                    leax      >FIRQRtn,pcr
                    stx       >D.FIRQ
* Set PIA to fire FIRQ on falling edge of CART*
                   IFNE      H6309
                    aim       #$FC,>PIA1Base+3
                    oim       #$01,>PIA1Base+3
                   ELSE
                    lda       >PIA1Base+3
                    anda      #$FC
                    ora       #$01
                    sta       >PIA1Base+3
                   ENDC
                   ENDC
                    lda       #CH376_GET_STATUS   clear any interrupts
                    sta       CH376_CMDREG
                    lda       CH376_DATAREG
                    lda       >PIA1Base+2         clear latched interrupts
* Register built-in Hub Drivers
                    leas      -8,s
                    leax      HubDevMatch,pcr
                    stx       ,s
                    stu       2,s
                    leax      HubProbe,pcr
                    stx       4,s
                    leax      HubDisconnect,pcr
                    stx       6,s
                    tfr       s,x
                    lbsr      RegisterDriver
                    leas      8,s
                    bcs       InitErrRemoveInt@
* Now set chip to USB Host Mode
                    lda       #CH376_SET_USB_MODE
                    sta       CH376_CMDREG
                    lda       #$05                set host mode, no SOF package
                    sta       CH376_DATAREG
                    bsr       Delay1Tk
                    ldb       CH376_DATAREG
                    cmpb      #CH376_CMD_RET_SUCCESS
                    bne       InitErrRemoveHubDriver@
                   IFEQ       Level-1
                    leas      2,s                 remove original D.FIRQ from stack
                   ENDC
                    puls      cc
                    leay      SvcTbl,pcr          register F$USBPollHubs system call
                    os9       F$SSvc
                    tfr       cc,a
                    anda      #IntMasks
                    beq       noirq@
* Interrupts are off, so check flag register manually.
* This is necessary while the system is still booting to check for
* a device connection.
                    bsr       Delay3Tk
                    lda       CH376_FLAGREG
                    bmi       noirq@
                    lbsr      IrqHandler
                    lda       >PIA1Base+2         reset any latched CART interrupt
noirq@              clra                          ensure success
                    bra       InitEx@
InitErrRemoveHubDriver@
                    leax      HubProbe,pcr
                    lbsr      DeregisterDriver
InitErrRemoveInt@
                   IFEQ       Level-1
                    puls      x
                    stx       >D.FIRQ             restore original D.FIRQ
                   ENDC
                    ldd       #CH376_FLAGREG
                    ldx       #0                  Y is still set from before
                    os9       F$IRQ
InitErrClrMem@
                    ldd       #USBMemSize
                    addd      #$00FF              round up
                    clrb
                    os9       F$SRtMem            U is still set from before
                    ldx       #0
                  IFGT    Level-1
                    stx       <D.USBManMem
                    stx       <D.USBManSubAddr
                  ELSE
                    stx       >D.USBManMem
                    stx       >D.USBManSubAddr
                  ENDC
InitErr@            puls      cc
                    comb
InitEx@             puls      d,x,y,u,pc

Delay1Tk            pshs      x                   (5) (4+)
                  IFGT    Level-1
                    ldx       <D.Proc             Get current process pointer
                    cmpx      <D.SysPrc           system?
                    beq       hw@                 system cannot sleep
                    ldx       <D.AProcQ           Get active proc queue
                  ELSE
                    ldx       >D.AProcQ           Get active proc queue
                  ENDC
                    beq       hw@                 if no waiting processes, do hardware loop
                    ldx       #1                  This is 1/60 second (16.6ms)
                    os9       F$Sleep
                    puls      x,pc
hw@
                  IFEQ    Level-1
                  IFNE    H6309
                    ldx       #1235               (5) (4) 6309 @ .89
                  ELSE
                    ldx       #988                (5) (4) 6809 @ .89
                  ENDC
                  ELSE
                  IFNE    H6309
                    ldx       #2486               (5) (4) 6309 @ 1.79
                  ELSE
                    ldx       #1988               (5) (4) 6809 @ 1.79
                  ENDC
                  ENDC
w@                  leax      -1,x                (4+) (4+)
                    exg       a,a                 (8) (5)
                    bne       w@                  (3) (3)
                    puls      x,pc                (5) (4+)

Delay3Tk            pshs      x                   (5) (4+)
                  IFGT    Level-1
                    ldx       <D.Proc             Get current process pointer
                    cmpx      <D.SysPrc           system?
                    beq       hw@                 system cannot sleep
                    ldx       <D.AProcQ           Get active proc queue
                  ELSE
                    ldx       >D.AProcQ           Get active proc queue
                  ENDC
                    beq       hw@                 if no waiting processes, do hardware loop
                    ldx       #3                  This is 3/60 second (50ms)
                    os9       F$Sleep
                    cmpx      #0                  test if slept whole time
                    bne       hw@                 if not, revert to hw wait
                    puls      x,pc
hw@
                  IFEQ    Level-1
                  IFNE    H6309
                    ldx       #1235*3             (5) (4) 6309 @ .89
                  ELSE
                    ldx       #988*3              (5) (4) 6809 @ .89
                  ENDC
                  ELSE
                  IFNE    H6309
                    ldx       #2486*3             (5) (4) 6309 @ 1.79
                  ELSE
                    ldx       #1988*3             (5) (4) 6809 @ 1.79
                  ENDC
                  ENDC
w@                  leax      -1,x                (4+) (4+)
                    exg       a,a                 (8) (5)
                    bne       w@                  (3) (3)
                    puls      x,pc                (5) (4+)

* This routine waits for an interrupt.
* Return Carry Clear, Status in A
WaitIrqResult
                    pshs      x,u
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    lda       USBIntStatus,u      if already got status, return
                    bne       finish@
                    tfr       cc,a
                    anda      #IntMasks
                    beq       multitasking@
* If interrupts are off, check flag register manually.
* This is necessary while the system is still booting.
manualintloop@      lda       CH376_FLAGREG
                    bmi       manualintloop@
                    lbsr      IrqHandler
                    lda       >PIA1Base+2         reset any latched CART interrupt
                    lda       USBIntStatus,u
                    bra       finish@
multitasking@       
loop@               lda       USBIntStatus,u      sleep/loop until interrupt received by handler
                    bne       finish@
                  IFGT    Level-1
                    ldx       <D.Proc             Get current process pointer
                    cmpx      <D.SysPrc           system?
                    beq       loop@               system cannot sleep
                  ENDC
                    ldx       #0                  Sleep until received Interrupt
                    os9       F$Sleep
                    bra       loop@
finish@             puls      u,x,pc

                   IFEQ      Level-1             
FIRQRtn             tst       ,s                  'Entire' bit of carry set?
                    bmi       L003B               branch if so
                    leas      -$01,s              make room on stack
                    pshs      y,x,dp,b,a          save regs
                    lda       $08,s               get original CC on stack
                    stu       $07,s               save U
                    ora       #$80                set 'Entire' bit
                    pshs      a                   save CC
L003B               lda       >PIA1Base+2         clear latched interrupts
                    jmp       [>D.SvcIRQ]         jump to IRQ service routine
                   ENDC

*
* F$IRQ handler,
*
* Input :
*       A       = Status byte XOR flip
*       U       = USBMan data area
IrqHandler          lda       #CH376_GET_STATUS
                    sta       CH376_CMDREG
                    lda       CH376_DATAREG
                    cmpa      #CH376_USB_INT_CONNECT
                    lbeq      DirectConnect
                    cmpa      #CH376_USB_INT_DISCONNECT
                    beq       DirectDisconnect
                    sta       USBIntStatus,u
                    lda       USBWakePid,u
                    beq       IRQSVC80
                    ldb       #S$Wake
                    os9       F$Send              signal to unblock
IRQSVC80            clrb
                    rts

* DirectDisconnect comes in from the IRQ Handler
* Also since this is a disconnect, and a ch376 only has one port, this means
* every device has been removed from the bus.
DirectDisconnect
                    lda       #CH376_SET_USB_MODE
                    sta       CH376_CMDREG
                    ldb       #$05
                    stb       CH376_DATAREG
                    lbsr      Delay1Tk
                    ldb       CH376_DATAREG
                   IFNE      H6309
                    clrd
                   ELSE
                    ldd       #0
                   ENDC
                    lbsr      DetachDevice
                    clr       USBPortConnected,u
                    rts

* Set directly connected device to device id 0 and return
DirectPortReset
                    pshs      x,d,cc
                    orcc      #IntMasks           mask interrupts while we reset usb bus
                    lda       #CH376_SET_USB_MODE
                    sta       CH376_CMDREG
                    ldb       #$07
                    stb       CH376_DATAREG
                    lbsr      Delay1Tk
                    ldb       CH376_DATAREG
                    cmpb      #CH376_CMD_RET_SUCCESS
                    bne       error@
                    ldb       #CH376_GET_STATUS   clear any interrupts generated
                    stb       CH376_CMDREG
                    ldb       CH376_DATAREG
                    lbsr      Delay1Tk
                    sta       CH376_CMDREG
                    ldb       #$06                set host mode, with SOF package
                    stb       CH376_DATAREG
                    lbsr      Delay1Tk
                    ldb       CH376_DATAREG
                    cmpb      #CH376_CMD_RET_SUCCESS
                    bne       error@
                    lda       #CH376_GET_STATUS   clear any interrupts generated
                    sta       CH376_CMDREG
                    lda       CH376_DATAREG
                    lda       >PIA1Base+2         clear latched interrupts
                    clrb
                    bra       finish@
error@              comb
finish@             puls      cc,d,x,pc

* DirectConnect comes in from the IRQ Handler
* Also since this is a connect, and a ch376 only has one port, this has to be
* the only device in play right now...so don't worry about getting a USB Lock
DirectConnect
                    tst       USBPortConnected,u
                    *         Error               because we got a second connection event before a disconnect
                    bne       error@
                    lbsr      DirectPortReset
                    bcs       error@
                    inc       USBPortConnected,u
                   IFNE      H6309
                    clrd
                   ELSE
                    ldd       #0
                   ENDC
                    lbra      AttachDevice
error@              comb
                    rts

* This routine resets a device, assigns it back the same device ID,
* and sets Configuration 1. It can be used by drivers to reset locked
* devices, but should never be needed.
* A is Device ID
ResetDevice
                    pshs      u,x,d
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    *find     Device
                    ldb       #USBMaxDevices
                    leax      USBDeviceTable,u
findloop@           cmpa      USBDeviceId,x
                    beq       found@
                    leax      USBDeviceRecLength,x
                    decb
                    bne       findloop@
                    bra       error@
found@              ldd       USBDeviceHub,x
                   IFNE      H6309
                    tstd
                   ELSE
                    cmpd      #0
                   ENDC
                    bne       hubdevice@
                    clra                          talking to device id 0
                    bsr       GetUSBLock
                    lbsr      DirectPortReset
                    bra       cont@
hubdevice@          *find     Hub                 record in Y
                    pshs      y,b
                    leay      USBHubTable,u
                    ldb       #USBMaxHubs
hfindloop@          cmpa      USBHubDeviceId,y
                    beq       hfound@
                    decb
                    beq       hfindlooperr@
                    leay      USBHubRecLength,y
                    bra       hfindloop@
hfindlooperr@       puls      b,y
                    bra       error@
hfound@             puls      b
                    lbsr      HubResetPort
                    puls      y
                    clra
                    bsr       GetUSBLock
cont@
                    lda       #CH376_SET_ADDRESS
                    sta       CH376_CMDREG
                    lda       USBDeviceId,x       load device id in record
                    sta       CH376_DATAREG       set earlier assigned ID
                    lbsr      WaitIrqResult
                    lbsr      FreeUSBLock
                    cmpa      #CH376_USB_INT_SUCCESS
                    bne       error@
                    lda       USBDeviceId,x
                    ldb       #1
                    lbsr      SetConfiguration
                    bcs       error@
                    bra       finish@
error@              comb
finish@             puls      d,x,u,pc

* This function is normally called by functions that are called externally,
* so do not assume it has the U pointer to the data area coming in
GetUSBLock
                    pshs      u,x,d
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    ldb       #$FF
spin@               pshs      cc
                    orcc      #IntMasks           mask interrupts
                    lda       USBLock,u
                    stb       USBLock,u
                    puls      cc
                    tsta
                    beq       gotlock@
                  IFGT    Level-1
                    ldx       <D.Proc             Get current process pointer
                    cmpx      <D.SysPrc           system?
                    beq       spin@               system cannot sleep
                  ENDC
                    ldx       #1
                    os9       F$Sleep
                    bra       spin@
gotlock@
                    clr       USBIntStatus,u
* Now that we have the lock, configure the chip to talk to the device we want.
* Skip if this was the last device communicated with.
                    lda       ,s                  restore device id from stack
                    cmpa      USBDeviceIdCache,u
                    beq       skipsetaddr@        chip already set, so skip to end
                    sta       USBDeviceIdCache,u
                    ldb       #CH376_SET_USB_ADDR now set address
                    stb       CH376_CMDREG
                    sta       CH376_DATAREG
skipsetaddr@
                  IFGT    Level-1
                    ldx       <D.Proc             Get current process pointer
                    cmpx      <D.SysPrc           system?
                    beq       done@
                  ELSE
                    ldx       >D.Proc             Get current process pointer
                  ENDC
                    lda       P$ID,x
                    sta       USBWakePid,u
done@               puls      d,x,u,pc

* This function is normally called by functions that are called externally,
* so do not assume it has the U pointer to the data area coming in
FreeUSBLock
                    pshs      u,cc
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    clr       USBWakePid,u
                    clr       USBLock,u
                    puls      cc,u,pc

* X struct of
*   Device Match Table Ptr
*   U Memory Location of device driver
*   Probe Address
*   Disconnect Address
* This function is normally called externally, so does not
* have the U pointer to the data area coming in
RegisterDriver
                    pshs      d,x,y,u
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    tfr       x,y
                    leax      USBDriverTable,u
                    ldb       #USBMaxDrivers
loop@               pshs      b
                    ldd       USBDriverDevMatchPtr,x
                   IFNE      H6309
                    tstd
                   ELSE
                    cmpd      #0
                   ENDC
                    puls      b
                    beq       foundslot@
                    decb
                    lbeq      error@              Too many drivers in system
                    leax      USBDriverRecLength,x
                    bra       loop@
foundslot@          ldd       ,y
                    std       USBDriverDevMatchPtr,x
                    ldd       2,y
                    std       USBDriverMemoryPtr,x
                    ldd       4,y
                    std       USBDriverProbeFuncPtr,x
                    ldd       6,y
                    std       USBDriverDisconnectFuncPtr,x
* Now go through all unclaimed interfaces, and run FindProbeDriver again
                    leay      USBInterfaceTable,u
                    ldb       #USBMaxInterfaces
loop1@              pshs      b                   store counter
                    tst       USBInterfaceDeviceId,y is this record populated?
                    beq       skipentry@
                    ldd       USBInterfaceDriverRecord,y check driver record
                    bne       skipentry@          this one is already claimed
                    lda       USBInterfaceDeviceId,y
                    lbsr      GetDeviceRec
                    ldd       USBDeviceConfigurationWLength,x size of configuration wlength
                    addd      #$00FF              round up to page
                    clrb
                    pshs      d                   put size of requested memory on stack
                    tfr       u,x                 save u memory location
                    os9       F$SRqMem
                    exg       u,x                 restore u and put mem pointer in X
                    bcc       goodmemreq@
                    leas      3,s                 remove counter and req size from stack
                    bra       error@
goodmemreq@
                    pshs      x                   save mem pointer for allocated buffer
                    lda       USBInterfaceDeviceId,y Get it a second time
                    lbsr      GetDeviceRec        returns device record in X
                    pshs      y
                    tfr       x,y
                    ldx       2,s                 restore x pointer to allocated buffer
getconfig@          bsr       GetConfigDescriptor fill allocated buffer with conf desc
                    bcc       go@
                    bra       getconfig@
go@                 puls      y
* Loop over X, stop when finding an interface descriptor matching the ID
loop2@              lda       USBDescriptorType,x
                    cmpa      #$04
                    bne       notthis@
                    lda       USBInterfaceNum,y
                    cmpa      USBIDBInterfaceNumber,x
                    beq       foundinterface@
notthis@            lda       USBDescriptorLength,x get size of descriptor
                    leax      a,x                 move to next descriptor
                    bra       loop2@
foundinterface@     lbsr      FindProbeDriver
skipprobe@          tfr       u,x                 save U memory ptr
                    puls      u
                    puls      d
                    os9       F$SRtMem
                    tfr       x,u                 restore U memory ptr
skipentry@          leay      USBInterfaceRecLength,y
                    puls      b
                    decb
                    bne       loop1@
                    clrb
                    bra       finish@
error@              comb
finish@             puls      u,y,x,d,pc

* Y is pointer to device record
* X is buffer to store into
* Return carry clear/set if success/error
GetConfigDescriptor
                    pshs      d,x,y
                    lda       USBInterfaceDeviceId,y
                    pshs      x                   save X
                    bsr       GetDeviceRec        puts device record into X
                    ldd       USBDeviceConfigurationWLength,x
                    puls      x                   restore X
                    exg       a,b                 make little endian
                    pshs      d                   set wLength in setup packet
                    leay      ConfigDesc+6,pcr
                    ldb       #6
loopdesc@           lda       ,-y
                    pshs      a
                    decb
                    bne       loopdesc@
                    leay      ,s
                    leas      -5,s
                    sty       USBCTS.SetupPktPtr,s
                    stx       USBCTS.BufferPtr,s
                    ldy       17,s                restore y pointer to device record
                    lda       USBDeviceId,y
                    sta       USBCTS.DeviceId,s
                    tfr       s,x
                    lbsr      ControlTransfer
                    leas      13,s
                    puls      y,x,d,pc

* A is device Id
* Returns Pointer to Device Record in X
GetDeviceRec
                  IFGT    Level-1
                    ldx       <D.USBManMem
                  ELSE
                    ldx       >D.USBManMem
                  ENDC
                    leax      USBDeviceTable,x
finddevloop@        cmpa      USBDeviceId,x       it has to be there unless
                    beq       founddev@           the table has been corrupted
                    leax      USBDeviceRecLength,x
                    bra       finddevloop@
founddev@           rts

* X is Drivers Probe location
* This function is normally called externally, so does not
* have the U pointer to the data area coming in
DeregisterDriver
                    pshs      u,y,d
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    leay      USBDriverTable,u
                    ldb       #USBMaxDrivers
loop0@              cmpx      USBDriverProbeFuncPtr,y
                    beq       found@
                    decb
                    beq       error@              got to end of table, error
                    leay      USBDriverRecLength,y
                    bra       loop0@
* Found device, now look for entries in Interface Table
found@              leau      USBInterfaceTable,u
                    ldb       #USBMaxInterfaces
loop1@              cmpy      USBInterfaceDriverRecord,u
                    bne       next@
                    clra
                    sta       USBInterfaceDriverRecord,u clear interface of driver
                    sta       USBInterfaceDriverRecord+1,u
next@               decb
                    beq       doneinterfaces@
                    leau      USBInterfaceRecLength,u
                    bra       loop1@
* Now clear out driver entry itself
doneinterfaces@     ldb       #USBDriverRecLength
                    clra
loop2@              sta       ,y+
                    decb
                    bne       loop2@
                    bra       done@
error@              comb
done@               puls      u,y,d,pc

* Attach a new device, already reset and listening on device id 0
* Hub and Port in D
* U is pointer to USBMan Memory
* Return
* DeviceId in A
* B is destroyed
AttachDevice
* Find empty slot in DeviceTable
                    pshs      x,y,u
                    tfr       d,x
* Find free Device Id
                    lda       #1
findfidoloop@       leay      USBDeviceTable,u
                    ldb       #USBMaxDevices
findfidiloop@       cmpa      USBDeviceId,y
                    beq       nextid@
                    leay      USBDeviceRecLength,y
                    decb
                    beq       foundfid@           Got through entire table
                    bra       findfidiloop@
nextid@             inca
                    bpl       findfidoloop@
                    lbra      error@              127 is max id
foundfid@           pshs      a
* Go through table a second time, but this time
* look for a free DeviceTable entry
                    leay      USBDeviceTable,u
                    ldb       #USBMaxDevices
findloop0@          lda       USBDeviceId,y
                    beq       foundslot0@         unused slot
                    leay      USBDeviceRecLength,y
                    decb
                    bne       findloop0@
                    leas      1,s                 remove device id from stack
                    lbra      error@
foundslot0@
                    puls      a
                    sta       USBDeviceId,y       store device id in device record
                    stx       USBDeviceHub,y      store hub and port in device record
* Now assign the identified ID to the device
                    clra                          send to device id 0
                    lbsr      GetUSBLock
                    lda       #CH376_SET_ADDRESS
                    sta       CH376_CMDREG
                    lda       USBDeviceId,y       load device id in record
                    sta       CH376_DATAREG       #set earlier assigned ID
                    lbsr      WaitIrqResult
                    lbsr      FreeUSBLock
                    cmpa      #CH376_USB_INT_SUCCESS
                    lbne      error@
                    lbsr      Delay3Tk
* Now, get first 8 bytes to get bMaxPacketSize0
                    lda       #$08
                    sta       USBDeviceMaxPacketSize,y store 8 byte min temporarily
                    leax      DeviceDesc,pcr
                    leas      -13,s
                    stx       USBCTS.SetupPktPtr,s
                    leax      5,s
                    stx       USBCTS.BufferPtr,s
                    *clra                          device id 0 at this point
                    lda       USBDeviceId,y 
                    sta       USBCTS.DeviceId,s
                    leax      ,s
                    lbsr      ControlTransfer
                    bcc       goodxfer0@
                    leas      13,s
                    lbra      error@
goodxfer0@          ldb       5+USBDDBMaxPacketSize0,s load bMaxPacketSize0 here
                    leas      13,s
                    stb       USBDeviceMaxPacketSize,y store in device record
* Next, get the full record now that we know the bMaxPacketSize0
                    ldd       #$1200              device descriptors always 18 bytes
                    pshs      d
                    leax      DeviceDesc+6,pcr
                    ldb       #6
loopdd1@            lda       ,-x
                    pshs      a
                    decb
                    bne       loopdd1@
                    tfr       s,x
                    leas      -23,s
                    stx       USBCTS.SetupPktPtr,s
                    leax      5,s
                    stx       USBCTS.BufferPtr,s
                    lda       USBDeviceId,y
                    sta       USBCTS.DeviceId,s
                    tfr       s,x
                    lbsr      ControlTransfer
                    bcc       goodxfer1@
                    leas      31,s
                    lbra      error@
goodxfer1@
                    ldd       5+USBDDIdVendor,s   Vendor ID
                    exg       a,b
                    std       USBDeviceVendorId,y
                    ldd       5+USBDDIdProduct,s  Product ID
                    exg       a,b
                    std       USBDeviceProductId,y
                    ldd       5+USBDDBDeviceClass,s Device Class and SubClass
                    std       USBDeviceClass,y
                    lda       5+USBDDBDeviceProtocol,s Device Protocol
                    sta       USBDeviceProtocol,y
* Load all strings here because some devices lock up if you
* don't ask for them. Don't do anything with them.
                    lbsr      Delay3Tk
                    lda       USBDeviceId,y
                    ldb       5+USBDDIManufacturer,s
                    lbsr      GetString
                    ldb       5+USBDDIProduct,s
                    lbsr      GetString
                    ldb       5+USBDDISerialNumber,s
                    lbsr      GetString
                    leas      31,s
                    lbsr      Delay3Tk
* Now collect configuration and store interfaces
* Collect just first 9 bytes to get wTotalLength
* To allocate enough room for entire collection
                    ldd       #$0900
                    pshs      d
                    leax      ConfigDesc+6,pcr
                    ldb       #6
loopdesc@           lda       ,-x
                    pshs      a
                    decb
                    bne       loopdesc@
                    tfr       s,x
                    leas      -14,s
                    stx       USBCTS.SetupPktPtr,s
                    leax      5,s
                    stx       USBCTS.BufferPtr,s
                    lda       USBDeviceId,y
                    sta       USBCTS.DeviceId,s
                    tfr       s,x
                    lbsr      ControlTransfer
                    bcc       goodxfer2@
                    leas      22,s
                    lbra      error@
goodxfer2@          lbsr      Delay3Tk
                    ldd       5+USBCDWTotalLength,s get wTotalLength
                    exg       a,b
                    std       USBDeviceConfigurationWLength,y store in record
                    leas      22,s
* Now request wTotalLength of space
                    addd      #$00FF              round up to page
                    clrb
                    pshs      d
                    tfr       u,x                 save u
                    os9       F$SRqMem
                    exg       u,x                 restore u and populate in X
                    bcc       goodmemreq@
                    puls      d
                    lbra      error@
goodmemreq@
                    pshs      x
                    lbsr      GetConfigDescriptor
                    bcc       goodxfer3@
                    puls      u
                    puls      d
                    os9       F$SRtMem
                    lbra      error@
goodxfer3@
* Set Configuration 1
* All devices are always put in configuration 1, even devices without drivers
* This should probably be fixed, but will complicate logic since drivers
* attach to interfaces, not devices. So all interfaces for a device would
* have to be checked to make sure a driver wasn't attached for any of them.
* Then matching code would need to be put into RegisterDriver as well.
                    lda       USBDeviceId,y
                    ldb       #1
                    lbsr      SetConfiguration
                    bcc       goodconfig@
                    puls      u
                    puls      d
                    os9       F$SRtMem
                    lbra      error@
goodconfig@
* Keep going with parsing interfaces
                    pshs      y                   save DeviceTable entry record pointer
* Parse through packet here, store into InterfaceTable
                    ldb       USBCDBNumInterfaces,x bNumInterfaces
                    pshs      b
                    lda       USBDescriptorLength,x
                    leax      a,x                 skip to first record
findfreeinterfacerecordloop@
                    lda       USBDescriptorType,x
                    cmpa      #$04                check if interface record
                    bne       notinterface@
                    tst       USBIDBAlternateSetting,x
                    bne       notinterface@       only use AlternateSetting 0 for match
* Found an interface, so now find
* a free Interface Table Entry
                    leay      USBInterfaceTable,u
                    ldb       #USBMaxInterfaces
finditloop@         lda       USBInterfaceDeviceId,y
                    beq       foundit@            empty slot
                    leay      USBInterfaceRecLength,y
                    decb
                    bne       finditloop@
* Did not find free slot
                    leas      3,s                 clean up stored b and DeviceTable record Pointer
                    puls      u
                    puls      d                   restore stack
                    os9       F$SRtMem
                    bra       error@              then error
foundit@            lda       [1,s]               device id on bus
                    sta       USBInterfaceDeviceId,y
                   IFNE      H6309
                    clrd
                   ELSE
                    ldd       #0                  initially set to no driver
                   ENDC
                    std       USBInterfaceDriverRecord,y
                    lda       USBIDBInterfaceNumber,x interface number
                    sta       USBInterfaceNum,y
                    ldd       USBIDBInterfaceClass,x class and subclass
                    std       USBInterfaceClass,y
                    lda       USBIDBInterfaceProtocol,x protocol
                    sta       USBInterfaceProtocol,y
                    lbsr      FindProbeDriver
                    puls      b
                    decb
                    beq       doneinterfaces@     now have parsed all interfaces
                    pshs      b                   if not done, put new counter back on stack
notinterface@       lda       USBDescriptorLength,x move pointer to next record
                    beq       doneinterfacesearly@ handle misformed records
                    leax      a,x
                    bra       findfreeinterfacerecordloop@
doneinterfacesearly@
                    leas      1,s
doneinterfaces@
                    puls      y                   restore y pointer to DeviceTable record
                    puls      u
                    puls      d
                    os9       F$SRtMem
                    clrb
                    bra       finish@
error@              comb
finish@             puls      u,y,x,pc

* A is device
* B is string index
* No return
GetString           pshs      d
                    leas      -33,s
                    leax      5,s
                    stx       USBCTS.SetupPktPtr,s
                    leax      13,s
                    stx       USBCTS.BufferPtr,s
                    sta       USBCTS.DeviceId,s
                    stb       7,s
                    ldd       #$8006
                    std       5,s
                    lda       #3
                    sta       8,s
                    ldd       #$0904
                    std       9,s
                    ldd       #$1400
                    std       11,s
                    leax      ,s
                    lbsr      ControlTransfer
                    leas      33,s
finish@             puls      d,pc


* Hub and Port in D
* Return carry clear/set in success/failure
DetachDevice
                    pshs      d,x,y,u
                    leax      USBDeviceTable,u
                    ldb       #USBMaxDevices
loop0@              pshs      b
                    ldd       USBDeviceHub,x
                    cmpd      1,s
                    puls      b
                    beq       found@
                    decb
                    beq       error@              tried all devices, fail
                    leax      USBDeviceRecLength,x
                    bra       loop0@
found@
* Found device, now loop over its interfaces, run disconnect on its drivers,
* then remove the interface entries
                    leay      USBInterfaceTable,u
                    ldb       #USBMaxInterfaces
loop2@              pshs      b
                    lda       USBInterfaceDeviceId,y
                    cmpa      USBDeviceId,x       check if same device id
                    bne       next@               if not, move to next
* Find the driver and call its disconnect function
                    ldu       USBInterfaceDriverRecord,y
                    beq       nodriver@           no driver loaded
                    ldd       USBDriverDisconnectFuncPtr,u
                    beq       nodriver@           skip if no disconnect
                    pshs      d
                    ldu       USBDriverMemoryPtr,u
* X is Device Rec, Y is Interface Rec, U is memory
                    jsr       [,s++]
* Now clear the interface table record
nodriver@           tfr       y,u
                    clra
                    ldb       #USBInterfaceRecLength
loop3@              sta       ,u+
                    decb
                    bne       loop3@
next@               puls      b
                    decb
                    beq       doneinterfaces@
                    leay      USBInterfaceRecLength,y
                    bra       loop2@
doneinterfaces@
* Finally, remove the device entry from the table
                    ldb       #USBDeviceRecLength
                    clra
loop1@              sta       ,x+
                    decb
                    bne       loop1@
                    bra       finish@
error@              comb
finish@             puls      u,y,x,d,pc

SetConfiguration
                    pshs      d
                    lbsr      GetUSBLock
                    lda       #CH376_SET_CONFIG
                    sta       CH376_CMDREG
                    stb       CH376_DATAREG
                    lbsr      WaitIrqResult
                    lbsr      FreeUSBLock
                    cmpa      #CH376_USB_INT_SUCCESS
                    bne       error@
                    clrb
                    bra       finish@
error@              orcc      #Carry
finish@             puls      d,pc

* Checks Interface against all drivers and calls probe function if match
* Y Interface Record
* X Interface Descriptor
* U USBMan Memory Area
* Returns nothing but carry clear/set if driver found/not found.
FindProbeDriver
                    pshs      d,x,y,u
                    leax      USBDriverTable,u    must have memory area pointer intact
                    pshs      x
                    lda       USBInterfaceDeviceId,y
                    lbsr      GetDeviceRec
                    tfr       x,u                 populate U with Device record ptr
                    puls      x
                    ldb       #USBMaxDrivers
loop@
                    *         X                   is driver entry
                    *         U                   is Device Record
                    pshs      b
                    ldy       USBDriverDevMatchPtr,x this is location of devmatch
                    lbeq      next@               if this slot empty, move to next
                    ldd       USBDeviceVendorId,u Vendor ID
                   IFNE      H6309
                    andd      USBDevMatchVendorIdMask,y
                   ELSE
                    anda      USBDevMatchVendorIdMask,y
                    andb      USBDevMatchVendorIdMask+1,y
                   ENDC
                    cmpd      USBDevMatchVendorId,y
                    bne       next@
                    ldd       USBDeviceProductId,u Product ID
                   IFNE      H6309
                    andd      USBDevMatchProductIdMask,y
                   ELSE
                    anda      USBDevMatchProductIdMask,y
                    andb      USBDevMatchProductIdMask+1,y
                   ENDC
                    cmpd      USBDevMatchProductId,y
                    bne       next@
                    ldd       USBDeviceClass,u    DeviceClass and SubClass
                   IFNE      H6309
                    andd      USBDevMatchDeviceClassMask,y
                   ELSE
                    anda      USBDevMatchDeviceClassMask,y
                    andb      USBDevMatchDeviceSubClassMask,y
                   ENDC
                    cmpd      USBDevMatchDeviceClass,y
                    bne       next@
                    lda       USBDeviceProtocol,u DeviceProtocol
                    anda      USBDevMatchDeviceProtocolMask,y
                    cmpa      USBDevMatchDeviceProtocol,y
                    bne       next@
* Done with device matching, now move on to interface matching
                    ldu       5,s                 load original 'y' interface record
                    ldd       USBInterfaceClass,u InterfaceClass and SubClass
                   IFNE      H6309
                    andd      USBDevMatchInterfaceClassMask,y
                   ELSE
                    anda      USBDevMatchInterfaceClassMask,y
                    andb      USBDevMatchInterfaceSubClassMask,y
                   ENDC
                    cmpd      USBDevMatchInterfaceClass,y
                    bne       next@
                    lda       USBInterfaceProtocol,u
                    anda      USBDevMatchInterfaceProtocolMask,y
                    cmpa      USBDevMatchInterfaceProtocol,y
                    bne       next@
* wow, so all that matched. This looks like the right driver.
* load up its U and call its probe function.
* pass in Y as the pointer to the interface table
                    tfr       u,y                 move interface table entry to y
                    ldu       USBDriverMemoryPtr,x load driver memory area
                    ldd       USBDriverProbeFuncPtr,x load pointer to probe function
                    pshs      x                   save driver table pointer
                    ldx       5,s                 load X pointer to interface desc
                    pshs      d
                    jsr       [,s++]              call Probe Function
                    puls      x                   restore driver table pointer
                    bcs       next@
success@            stx       USBInterfaceDriverRecord,y save claimed driver to interface
                    leas      1,s                 remove driver counter from stack
                    bra       finish@
next@               puls      b
                    decb
                    beq       error@              have gone through all drivers
                    leax      USBDriverRecLength,x
                    lbra      loop@
error@              comb                          error if no driver found
finish@             puls      u,y,x,d,pc

* Clear Stall condition on endpoint
* Input: DeviceId in A, EndpointId in B (bit 7 high if input endpoint)
* Output: Carry clear if success, set if error
ClearStall
                    pshs      d
                    lbsr      GetUSBLock
                    lda       #CH376_CLR_STALL
                    sta       CH376_CMDREG
                    stb       CH376_DATAREG
                    lbsr      WaitIrqResult
                    lbsr      FreeUSBLock
                    cmpa      #CH376_USB_INT_SUCCESS
                    bne       error@
                    clrb      
                    bra       finish@
error@              comb
finish@             puls      d,pc

* X is pointer to struct
*   0 (2) Pointer to buffer
*   2 (2) Length of buffer
*   4 (1) USB Device ID
*   5 (1) Endpoint ID
*   6 (1) DATA0/DATA1 flag
*   7 (1) Max Packet for this endpoint
*   8 (1) NAK behavior: 0 is default retry, anything else is no retry
* Return
* D bytes transferred to buffer
* This function is normally called externally, so does not
* have the U pointer to the data area coming in
InTransfer
                    pshs      u,y
                    ldu       USBITS.BufferPtr,x
                    ldy       USBITS.BufferLength,x
                    lda       USBITS.EndpointId,x
                    asla                          shift endpoint up four bits
                    asla
                    asla
                    asla
                    ora       #CH376_DEF_USB_PID_IN IN pid is 1001b
                    pshs      a                   store transaction attribute
bigloop@
                    lda       USBITS.DeviceId,x   load device id
                    lbsr      GetUSBLock
                    pshs      u
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    lda       USBITS.NakFlag,x
                    cmpa      USBNakRetryCache,u  if same as last time, skip change
                    beq       skipnak@
                    ldb       #CH376_SET_RETRY
                    stb       CH376_CMDREG
                    ldb       #$25
                    stb       CH376_DATAREG
                    sta       USBNakRetryCache,u
                    tsta
                    bne       returnnak@
                    lda       #$8F                default from datasheet
                    fcb       $21                 Skip next instruction (BRN)
returnnak@
                    clra
                    sta       CH376_DATAREG
skipnak@
                    puls      u
                    ldb       #CH376_ISSUE_TKN_X
                    stb       CH376_CMDREG
                    ldb       USBITS.DataFlag,x   pull DATA0 or DATA1 flag for this endp
                    stb       CH376_DATAREG
                    lda       ,s                  grab transaction attribute off stack
                    sta       CH376_DATAREG
                    lbsr      WaitIrqResult
                    cmpa      #CH376_USB_INT_SUCCESS
                    lbne      error@
                   IFNE      H6309
                    eim       #$80;USBITS.DataFlag,x
                   ELSE
                    ldb       USBITS.DataFlag,x
                    eorb      #$80                flip and store flag for next time
                    stb       USBITS.DataFlag,x
                   ENDC
doread@
                    lda       #CH376_RD_USB_DATA0
                    sta       CH376_CMDREG
                    clra
                    ldb       CH376_DATAREG       D now contains num bytes to read
                    bne       readbuf@            non-zero, so go read it
                    lbsr      FreeUSBLock
                    bra       wrapup@
readbuf@            pshs      d
* check against buffer size
* if too big, just error out entirely
                    cmpy      ,s
                    blt       sizeerr@
                   IFNE      H6309
                    subr      d,y
                    pshsw
                    tfr       d,w
                    ldd       #CH376_DATAREG
                    pshs      cc
                    orcc      #IntMasks
                    tfm       d,u+
                    puls      cc
                    pulsw
                   ELSE
loop0@              lda       CH376_DATAREG
                    sta       ,u+
                    leay      -1,y
                    decb
                    bne       loop0@
                   ENDC
donepkt@            lbsr      FreeUSBLock
                    puls      d
                    cmpy      #0                  if buffer is full, then exit
                    beq       wrapup@
                    cmpb      USBITS.MaxPacketSize,x see if this was a full packet
                    lbeq      bigloop@            if so then loop for more to read
wrapup@             ldd       USBITS.BufferLength,x
                   IFNE      H6309
                    subr      y,d
                   ELSE
                    pshs      y
                    subd      ,s++
                   ENDC
                    bra       finish@
sizeerr@            
                    leas      2,s                 get rid of size to read on stack
                    lda       #E$BufSiz           give error code
error@              lbsr      FreeUSBLock
                    comb
                    tfr       a,b                 return interrupt received in B
finish@             leas      1,s                 get rid of transaction attribute
                    puls      u,y,pc

* Make sure default Nak behavior is set.
* Only used for ControlTransfer and OutTransfer.
* Custom behavior passed in for InTransfer.
SetDefaultNak
                    pshs      u,a
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    tst       USBNakRetryCache,u  if nak is not default, change to default
                    beq       skipnak@
                    lda       #CH376_SET_RETRY
                    sta       CH376_CMDREG
                    lda       #$25
                    sta       CH376_DATAREG
                    lda       #$8F                default from datasheet
                    sta       CH376_DATAREG
                    clr       USBNakRetryCache,u
skipnak@            puls      u,a,pc

* X is pointer to struct
*   0 (2) Pointer to buffer
*   2 (2) Length of buffer
*   4 (1) USB Device ID
*   5 (1) Endpoint ID
*   6 (1) DATA0/DATA1 flag
*   7 (1) Max Packet for this endpoint
* Return
* D bytes transferred to buffer
* This function is normally called externally, so does not
* have the U pointer to the data area coming in
OutTransfer
                    pshs      u,y
                    ldu       USBOTS.BufferPtr,x
                    ldy       USBOTS.BufferLength,x
                    lda       USBOTS.EndpointId,x get endpoint id
                    asla                          shift endpoint up four bits
                    asla
                    asla
                    asla
                    ora       #CH376_DEF_USB_PID_OUT OUT pid is 0001
                    pshs      a                   store transaction attribute
bigloop@
                    lda       USBOTS.DeviceId,x   load device id
                    lbsr      GetUSBLock
                    bsr       SetDefaultNak
                    lda       #CH376_WR_HOST_DATA
                    sta       CH376_CMDREG
* test against packet size
                    clra
                    ldb       USBOTS.MaxPacketSize,x
                   IFNE      H6309
                    cmpr      d,y
                   ELSE
                    pshs      d
                    cmpy      ,s++
                   ENDC
                    bge       doxfer@
                    tfr       y,d                 in case y is less than a packet size
doxfer@
                    stb       CH376_DATAREG
                    tstb
                    beq       donedata@
                   IFNE      H6309
                    subr      d,y
                    pshsw
                    tfr       d,w
                    ldd       #CH376_DATAREG
                    pshs      cc
                    orcc      #IntMasks
                    tfm       u+,d
                    puls      cc
                    pulsw
                   ELSE
loop@               lda       ,u+
                    sta       CH376_DATAREG
                    leay      -1,y
                    decb
                    bne       loop@
                   ENDC
donedata@
                    ldb       #CH376_ISSUE_TKN_X
                    stb       CH376_CMDREG
                    ldb       USBOTS.DataFlag,x   pull DATA0 or DATA1 flag for this endp
                    stb       CH376_DATAREG
                    lda       ,s
                    sta       CH376_DATAREG
                    lbsr      WaitIrqResult
                    lbsr      FreeUSBLock
                    cmpa      #CH376_USB_INT_SUCCESS
                    bne       error@
                   IFNE      H6309
                    eim       #$40;USBOTS.DataFlag,x
                   ELSE
                    ldb       USBOTS.DataFlag,x
                    eorb      #$40                flip and store flag for next time
                    stb       USBOTS.DataFlag,x
                   ENDC
                    cmpy      #0                  if buffer is done, then exit
                    bne       bigloop@
wrapup@             ldd       USBOTS.BufferLength,x
                   IFNE      H6309
                    subr      y,d
                   ELSE
                    pshs      y
                    subd      ,s++
                   ENDC
                    bra       finish@
error@              comb
                    tfr       a,b
finish@             leas      1,s                 remove stored transaction attribute
                    puls      u,y,pc

* X is pointer to struct
*   0 (2) pointer to 8 byte SETUP packet
*   2 (2) Pointer to buffer
*   4 (1) USB Device ID
* Note that SETUP packet has length of buffer in it
* Return
*   D bytes sent/received
* This function is normally called externally, so does not
* have the U pointer to the data area coming in
ControlTransfer
                    pshs      u
                    lda       USBCTS.DeviceId,x   load device id
                    lbsr      GetUSBLock
                    lbsr      SetDefaultNak
                    lda       #CH376_WR_HOST_DATA
                    sta       CH376_CMDREG
                    ldu       USBCTS.SetupPktPtr,x
                    ldb       #8
                    stb       CH376_DATAREG
loop0@              beq       donesetuppacket@
                    lda       ,u+
                    sta       CH376_DATAREG
                    decb
                    bra       loop0@
donesetuppacket@
                    lda       #CH376_ISSUE_TKN_X
                    sta       CH376_CMDREG
                    clrb
                    stb       CH376_DATAREG       set to DATA0
                    ldb       #CH376_DEF_USB_PID_SETUP SETUP pid is 1101. endpoint is 0.
                    stb       CH376_DATAREG
                    lbsr      WaitIrqResult
                    lbsr      FreeUSBLock
                    tfr       a,b
                    cmpa      #CH376_USB_INT_SUCCESS
                    lbne      error@
* Check data direction on top of setup packet buffer
                    tst       [USBCTS.SetupPktPtr,x]
                    lbmi      dev2host@
* Do Host-to-Device (for example, Set Configuration)
* Create struct and call OutTransfer for DATA stage
* Grab wLength from SETUP packet
                    ldu       USBCTS.SetupPktPtr,x
                    ldd       6,u                 This is the size field of the SETUP Packet
                    exg       a,b                 switch from little to big endian
                    beq       h2dnodata@
                    pshs      x
                    leas      -8,s
                    std       USBOTS.BufferLength,s store size
                    ldd       USBCTS.BufferPtr,x
                    std       USBOTS.BufferPtr,s
                    lda       USBCTS.DeviceId,x
                    clrb
                    std       USBOTS.DeviceId,s
                    lbsr      GetDeviceRec
                    ldb       USBDeviceMaxPacketSize,x
                    lda       #$40
                    std       USBOTS.DataFlag,s
                    leax      ,s
                    lbsr      OutTransfer
                    leas      8,s
                    puls      x
                    lbcs      error@
h2dnodata@
                    pshs      x,d
* Create struct and call InTransfer for STATUS stage
                    leas      -9,s
                   IFNE      H6309
                    clrd
                   ELSE
                    ldd       #0
                   ENDC
                    std       USBITS.BufferPtr,s
                    std       USBITS.BufferLength,s
                    lda       USBCTS.DeviceId,x
                    clrb
                    std       USBITS.DeviceId,s
                    stb       USBITS.NakFlag,s    NAK behavior default
                    lbsr      GetDeviceRec
                    lda       #$80
                    ldb       USBDeviceMaxPacketSize,x
                    std       USBITS.DataFlag,s
                    leax      ,s
                    lbsr      InTransfer
                    bcc       goodstatus@
statuserror@
                    leas      11,s
                    puls      x
                    bra       error@              Got other error (probably stall) so error
goodstatus@
                    leas      9,s
                    puls      x,d
                    bra       finish@
* Do Device-to-Host (for example, Get Configuration)
dev2host@
* Grab wLength from SETUP packet
                    ldu       USBCTS.SetupPktPtr,x
                    ldd       6,u                 this is the size field of the SETUP Packet
                    exg       a,b                 switch from little to big endian
                    beq       d2hnodata@          if zero, skip data stage
* Create struct and call InTransfer for DATA stage
                    pshs      x
                    leas      -9,s
                    std       USBITS.BufferLength,s store buffer length
                    ldd       USBCTS.BufferPtr,x
                    std       USBITS.BufferPtr,s  pointer to buffer
                    lda       USBCTS.DeviceId,x
                    clrb
                    std       USBITS.DeviceId,s   device and endpoint
                    stb       USBITS.NakFlag,s    nak behavior default
                    lbsr      GetDeviceRec
                    ldb       USBDeviceMaxPacketSize,x
                    lda       #$80
                    std       USBITS.DataFlag,s   data0/data1 flag
                    leax      ,s
                    lbsr      InTransfer
                    leas      9,s
                    puls      x
                    bcs       error@
d2hnodata@
                    pshs      x,d
* Create struct and call OutTransfer for STATUS stage
                    leas      -8,s
                   IFNE      H6309
                    clrd
                   ELSE
                    ldd       #0
                   ENDC
                    std       USBOTS.BufferPtr,s
                    std       USBOTS.BufferLength,s
                    lda       USBCTS.DeviceId,x
                    clrb
                    std       USBOTS.DeviceId,s
                    lbsr      GetDeviceRec
                    ldb       USBDeviceMaxPacketSize,x
                    lda       #$40
                    std       USBOTS.DataFlag,s
                    leax      ,s
                    lbsr      OutTransfer
                    leas      8,s
                    puls      x,d
                    bcc       finish@
error@              orcc      #Carry              set carry
                    puls      u,pc
finish@
                    andcc     #^Carry
                    puls      u,pc

* Loop through all hubs, poll their ports
PollHubs            pshs      u,y,b
                  IFGT    Level-1
                    ldu       <D.USBManMem
                  ELSE
                    ldu       >D.USBManMem
                  ENDC
                    leay      USBHubTable,u
                    ldb       #USBMaxHubs
loop@               tst       USBHubDeviceId,y
                    beq       next@
                    bsr       PollHubPorts
next@               decb
                    beq       finish@
                    leay      USBHubRecLength,y
                    bra       loop@
finish@             puls      u,y,b,pc

* Query Status Change endpoint
* and process changed ports
* Y is Hub Table Entry
* U is USBMan Memory (needed for HubProcessPortChange)
* Return with carry set if no change in ports
PollHubPorts
                    pshs      x,d
                   IFNE      H6309
                    clrd
                   ELSE
                    ldd       #0
                   ENDC
                    subd      USBHubWMaxPacketSize,y make room on stack
* Should probably add a check here that allocating this won't overflow
* the stack, but most hubs only take 1 or 2 bytes as each port is one bit
                   IFNE      H6309
                    addr      d,s
                   ELSE
                    leas      d,s                 wMaxPacketSize
                   ENDC
                    leax      ,s
                    leas      -9,s                make room on stack
                    stx       USBITS.BufferPtr,s  pointer to buffer
                    ldd       USBHubWMaxPacketSize,y
                    std       USBITS.BufferLength,s length of buffer
                    lda       USBHubDeviceId,y
                    ldb       USBHubStatusEndpoint,y
                    std       USBITS.DeviceId,s   usb device id+endpoint id
                    lda       USBHubDataFlag,y
                    sta       USBITS.DataFlag,s   DATA0/DATA1 flag
                    ldd       USBHubWMaxPacketSize,y
                    cmpd      #64
                    ble       goodvalue@
                    ldb       #64
goodvalue@          stb       USBITS.MaxPacketSize,s maxpacket
* The hub will send a NAK if there is no change, so
* instruct the chip to not do a retry, and instead
* bubble up the NAK. Thus, this procedure will return
* with carry set if no change in ports.
* Note this differs from most usage of a set carry (in which it represents an error).
* In this case, carry set just means no change.
                    lda       #1
                    sta       USBITS.NakFlag,s    set to allow NAK
                    tfr       s,x
                    lbsr      InTransfer
                    pshs      cc
                    lda       1+USBITS.DataFlag,s store DATA0/DATA1 flag
                    sta       USBHubDataFlag,y
                    puls      cc
                    leas      9,s
                    bcs       error@
goodxfer@
* Parse bitmap here
                    tfr       s,x                 set x to top of stack
                    lda       ,x+                 load first byte
                    rora                          Ignore Hub power status
                    ldb       #1                  start with port 1
loop0@              rora                          rotate bit 0 to carry
                    bcc       notthisport@        carry clear means no change
                    lbsr      HubProcessPortChange process port if changed
notthisport@        incb                          go to next bit
                    bitb      #$07                if at a byte boundary
                    bne       notbyteboundary@
                    lda       ,x+                 then load next byte
notbyteboundary@
                    cmpb      USBHubNumPorts,y    compare to number of ports
                    ble       loop0@              keep going for all ports
                    ldd       USBHubWMaxPacketSize,y restore stack
                   IFNE      H6309
                    addr      d,s
                   ELSE
                    leas      d,s
                   ENDC
                    bra       finish@
error@              ldd       USBHubWMaxPacketSize,y restore stack
                   IFNE      H6309
                    addr      d,s
                   ELSE
                    leas      d,s
                   ENDC
                    comb
finish@             puls      x,d,pc

* X is Buffer
* Y is Hub Table Record
* B is port
* Return carry clear/set if success/error
HubGetPortStatus
* See 11.24.2 on page 420 of the USB 2.0 Spec
                    pshs      d,x
                    leas      -13,s
                    stx       USBCTS.BufferPtr,s
                    leax      5,s
                    stx       USBCTS.SetupPktPtr,s
                    lda       USBHubDeviceId,y
                    sta       USBCTS.DeviceId,s
                    lda       #$A3
                    sta       5,s
                    clra                          GET_STATUS
                    sta       6,s
                    sta       7,s
                    sta       8,s
                    stb       9,s                 store port
                    sta       10,s
                    sta       12,s
                    lda       #4
                    sta       11,s
                    tfr       s,x
                    lbsr      ControlTransfer     carry will propogate through
                    leas      13,s
error@              puls      x,d,pc

* A is feature selector (USB 2.0 specification table 11-17 on page 421)
* B is port
* Y is hub table record
* Return is carry clear/set if success/error
SetPortFeature
                    pshs      d,x
                    lda       #$03                SET FEATURE
                    bra       mergeclear@
* A is feature selector (USB 2.0 specification table 11-17 on page 421)
* B is port
* Y is hub table record
* Return is carry clear/set if success/error
ClearPortFeature
                    pshs      d,x
                    lda       #$01                CLEAR FEATURE
mergeclear@         leas      -13,s               merge in from SetPortFeature
                    sta       6,s                 bRequest
                    lda       13,s                restore A to feature
* Page 422 Section 11.24.2.2 of the USB 2.0 spec (usb_20.pdf) is confusing
                    sta       7,s                 wValue(L)
                    stb       9,s                 store port in wIndex(L)
                    leax      5,s
                    stx       USBCTS.SetupPktPtr,s
                    lda       USBHubDeviceId,y
                    sta       USBCTS.DeviceId,s
                    lda       #$23                See USB 2.0 Spec 11.24.2.2
                    sta       5,s                 bmRequestType
                   IFNE      H6309
                    clrd
                   ELSE
                    ldd       #0
                   ENDC
                    std       11,s                wLength
                    sta       8,s                 wValue(H)
                    sta       10,s                wIndex(H)
                    leax      ,s
                    lbsr      ControlTransfer     carry propagates up
                    leas      13,s
                    puls      x,d,pc

* Reset device connected to a USB Hub to ID 0
* In
*  Y is the Hub Device Table Entry
*  B is Port #
*  U is USBMan Memory Address
* Out
*  Carry Set if error
HubResetPort        pshs      y
                    lda       #$04                PORT_FEAT_RESET
                    lbsr      SetPortFeature
                    bcs       error@
                    leas      -4,s
                    leax      ,s                  x is 4 bytes result
retry@              lbsr      Delay1Tk
                    lbsr      HubGetPortStatus
                    bcc       goodxfer@
                    leas      4,s
                    bra       error@
goodxfer@           
                   IFNE      H6309
                    tim       #$10;2,s
                   ELSE
                    lda       #$10                $10 is port_reset done
                    bita      2,s
                   ENDC
                    beq       retry@              loop until port reset done
                    leas      4,s
                    lda       #20
                    lbsr      ClearPortFeature
                    bra       finish@
error@              comb
finish@             puls      x,pc

* This will process and clear all port status changes on the hub
* See Table 11-22 on page 431 of the USB 2.0 Specification
* B is port number
* Y is hub table entry
* U is USBMan Memory (needed for AttachDevice)
HubProcessPortChange
* Call port status
                    pshs      d,x
                    leas      -4,s
                    leax      ,s                  x is 4 bytes result
                    lbsr      HubGetPortStatus
                    lbcs      error@
                    ldb       5,s                 reload port number off stack
                    lda       #$01
                    bita      2,s
                    lbeq      noportconnectionchange@
* Find out if connected or not
                    bita      ,s
                    lbeq      portdisconnected@
* New Device Connected here
                    bsr       HubResetPort
                    bcs       error@
                    lda       USBHubDeviceId,y
                    lbsr      AttachDevice
                    bcs       error@
                    bra       clearportconnchange@
portdisconnected@
                    *         Old                 Device Disconnected here
                    lda       USBHubDeviceId,y    load hub device no
                    lbsr      DetachDevice
                    bcs       error@
clearportconnchange@
* Do a ClearPortFeature(C_PORT_CONNECTION) to clear
                    ldb       5,s                 reload port number off stack
                    lda       #$10                16  C_PORT_CONNECTION
                    lbsr      ClearPortFeature
                    bcs       error@
noportconnectionchange@
                   IFNE      H6309
                    tim       #$02;2,s
                   ELSE
                    lda       #$02                C_PORT_ENABLE
                    bita      2,s
                   ENDC
                    beq       noportenable@
* This is just saying if the port is enabled or not
* so don't handle connection/disconnection events here
* that is handled with C_PORT_CONNECTION instead
                    lda       #17
                    lbsr      ClearPortFeature
                    bcs       error@
noportenable@       
                   IFNE      H6309
                    tim       #$04;2,s
                   ELSE
                    lda       #$04                C_PORT_SUSPEND
                    bita      2,s
                   ENDC
                    beq       noportsuspend@
* Just clearing the status, not doing anything else
* as support for suspending devices is not (yet) implemented in this
* driver so this should never trigger anyway
                    lda       #18                 C_PORT_SUSPEND
                    lbsr      ClearPortFeature
                    bcs       error@
noportsuspend@      lda       #$08                C_PORT_OVER_CURRENT
                    bita      2,s
                    beq       noportovercurrent@
* Current protection is not properly implemented in this driver
* so if a device is over current, just disable that port
* Problem: how to inform the user this has been done? Print to screen
* with D.BtBug ? Maybe add a system call for a userspace app to query
* for disabled ports?
                    bita      ,x
                    beq       notovercurrent@
* Disable port here
                    lda       #1                  PORT_ENABLE
                    lbsr      ClearPortFeature
                    bcs       error@
notovercurrent@
                    lda       #19                 C_PORT_OVER_CURRENT
                    lbsr      ClearPortFeature
                    bcs       error@
noportovercurrent@
                    bra       finish@
error@              comb
finish@             leas      4,s
                    puls      x,d,pc

* U Memory Location Pointer
* Y Interface table entry
* X Interface descriptor pointer
* Returns Carry Clear if accept
HubProbe
                    pshs      d,x,y,u
                    ldb       #$05                loop until found endpoint descriptor
loop0@              cmpb      USBDescriptorType,x
                    beq       foundendp@
                    lda       USBDescriptorLength,x
                    leax      a,x
                    bra       loop0@
foundendp@          
* X now contains endpoint descriptor
                    leau      USBHubTable,u
                    ldb       #USBMaxHubs
loop1@              tst       USBHubDeviceId,u
                    beq       found@
                    decb
                    lbeq      error@              reach max hubs in system
                    leau      USBHubRecLength,u
                    bra       loop1@
found@              
* U now contains Hub record entry
* This records the packet size for the port change endpoint request
                    lda       USBInterfaceDeviceId,y Device id from intf table
                    sta       USBHubDeviceId,u
                    lda       USBEDBEndpointAddress,x endpoint id from endpoint desc
                    anda      #$0F
                    sta       USBHubStatusEndpoint,u
                    ldd       USBEDWMaxPacketSize,x wMaxPacketSize from endpoint desc
                    exg       a,b                 make big endian
                    std       USBHubWMaxPacketSize,u and store
* Get port count and store into Hub Table
* See 11.24.2 on page 420 of USB 2.0 Spec
                    pshs      X
                    leas      -21,S
                    lda       #$A0
                    sta       5,S                 bRequestType
                    lda       #6
                    sta       6,S                 bRequest
                    ldd       #$0029
                    std       7,S                 wValue
                   IFNE      H6309
                    clrd
                   ELSE
                    ldd       #0
                   ENDC
                    std       9,S                 wIndex
                    lda       #$08
                    std       11,S                wLength
                    leax      5,S
                    stx       USBCTS.SetupPktPtr,S
                    leax      13,S
                    stx       USBCTS.BufferPtr,S
                    lda       USBInterfaceDeviceId,y
                    sta       USBCTS.DeviceId,S
                    tfr       S,X
                    lbsr      ControlTransfer
                    bcc       goodxfer@
                    leas      21,s
                    puls      X
                    bra       xfrerror@
* See 11.23.21 Hub Descriptor on page 417 of USB 2.0 Spec
goodxfer@           lda       15,s                port count is at this location
                    leas      21,s
                    puls      X
                    sta       USBHubNumPorts,u    store port count into record
* Turn power on all ports
                    tfr       a,b
                    lda       #$08                POWER_FEAT_POWER
powerloop@          lbsr      SetPortFeature
                    decb
                    bne       powerloop@
* Now we poll the ports for our new hub
* This might recurse down into another new hub. Fun!
                    tfr       u,y                 move hub entry to y (throw away Y)
                    ldu       6,s                 restore USBManMem pointer
pollagain@
                    lbsr      Delay1Tk
                    lbsr      PollHubPorts
                    bcc       pollagain@          poll until no change
                    andcc     #^Carry             mark carry clear
                    bra       finish@
xfrerror@           clra
                    ldb       #USBHubRecLength
clrloop@            sta       ,u+
                    decb
                    bne       clrloop@
error@              comb
finish@             puls      d,x,y,u,pc

* X Device Record
* Y Interface Record
* U Memory Pointer
HubDisconnect
                    pshs      d,y
                    lda       USBDeviceId,x
* Find entry in hub table
                    ldb       #USBMaxHubs
                    leay      USBHubTable,u
loop0@              cmpa      USBHubDeviceId,y
                    beq       foundhub@
                    decb
                    beq       error@              did not find entry in table
                    leay      USBHubRecLength,y
                    bra       loop0@
foundhub@
* Call DetachDevice for every port
                    ldb       USBHubNumPorts,y
loop1@              lbsr      DetachDevice        ignore success or error
                    decb
                    bne       loop1@
* Remove entry from hub table
                    ldb       #USBHubRecLength
                    clra
loop2@              sta       ,y+
                    decb
                    bne       loop2@
                    bra       finish@
error@              comb
finish@             puls      y,d,pc

eom                 equ       *
                    end
