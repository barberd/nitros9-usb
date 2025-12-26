* Static vars go here

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
                    fdb       $4000               wLength (64 bytes)

ConfigDesc          fcb       $80                 bmRequestType
                    fcb       $06                 bRequest
                    fdb       $0002               wValue
                    fdb       $0000               wIndex
*These 2 bytes would normally be included but is replaced by routines
*                fdb     $0800                wLength (08 bytes)

* initialize ch376
CardInit
                    pshs      u,y,x,d
                    pshs      cc
                    orcc      #IntMasks           mask interrupts while we set up everything
* Check if device exists first
                    lda       #CH376_RESET_ALL
                    sta       CH376_CMDREG
                    lbsr      Delay               Datasheet says wait 40ms. Delay is 50ms.
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
                    lda       #$FF
                    sta       USBDeviceIdCache    Invalidate Device Id
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
                    bcs       InitErr@
* Now set chip to USB Host Mode
                    lda       #CH376_SET_USB_MODE
                    sta       CH376_CMDREG
                    lda       #$05                set host mode, no SOF package
                    sta       CH376_DATAREG
                    bsr       Delay
                    ldb       CH376_DATAREG
                    cmpb      #CH376_CMD_RET_SUCCESS
                    bne       InitErrRemoveHubDriver@
                    puls      cc
                    tfr       cc,a
                    anda      #IntMasks
                    beq       noirq@
* Interrupts are off, so check flag register manually.
* This is necessary while the system is still booting to check for
* a device connection.
                    lda       CH376_FLAGREG
                    bmi       noirq@
                    lbsr      IrqHandler
                    lda       $FF22               reset any latched CART interrupt
                    clr       USBIntStatus
noirq@              clra                          ensure success
                    bra       InitEx@
InitErrRemoveHubDriver@
                    leax      HubProbe,pcr
                    lbsr      DeregisterDriver
InitErr@            puls      cc
                    comb
InitEx@             puls      d,x,y,u,pc

Delay               pshs      x
                    ldx       #988*3              (5) (4) 6809 @ .89
w@                  leax      -1,x                (4+) (4+)
                    exg       a,a                 (8) (5)
                    bne       w@                  (3) (3)
                    puls      x,pc                (5) (4+)

* This routine waits for an interrupt.
* Return Carry Clear, Status in A
WaitIrqResult
                    pshs      x,u
                    lda       USBIntStatus        if already got status, return
                    bne       finish@
                    tfr       cc,a
                    anda      #IntMasks
                    beq       multitasking@
* If interrupts are off, check flag register manually.
manualintloop@      lda       CH376_FLAGREG
                    bmi       manualintloop@
                    lbsr      IrqHandler
                    lda       $FF22               reset any latched CART interrupt
                    lda       USBIntStatus
                    bra       finish@
multitasking@
loop@               lda       USBIntStatus        sleep/loop until interrupt received by handler
                    bne       finish@
                    bra       loop@
finish@             clr       USBIntStatus
                    puls      u,x,pc

*
* IRQ handler,
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
                    sta       USBIntStatus
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
                    lbsr      Delay
                    ldb       CH376_DATAREG
                    ldd       #0
                    lbsr      DetachDevice
                    clr       USBPortConnected
                    rts

* Set directly connected device to device id 0 and return
DirectPortReset
                    pshs      x,d,cc
                    orcc      #IntMasks           mask interrupts while we reset usb bus
                    lda       #CH376_SET_USB_MODE
                    sta       CH376_CMDREG
                    ldb       #$07
                    stb       CH376_DATAREG
                    lbsr      Delay
                    ldb       CH376_DATAREG
                    cmpb      #CH376_CMD_RET_SUCCESS
                    bne       error@
                    ldb       #CH376_GET_STATUS   clear any interrupts generated
                    stb       CH376_CMDREG
                    ldb       CH376_DATAREG
* According to USB 2.0 Spec, section 7.1.7.5 on page 153, host controllers should
* reset for at least 50ms. Do it twice just to be sure.
                    lbsr      Delay
                    lbsr      Delay
                    sta       CH376_CMDREG
                    ldb       #$06                set host mode, with SOF package
                    stb       CH376_DATAREG
                    lbsr      Delay
                    ldb       CH376_DATAREG
                    cmpb      #CH376_CMD_RET_SUCCESS
                    bne       error@
                    lda       #CH376_GET_STATUS   clear any interrupts generated
                    sta       CH376_CMDREG
                    lda       CH376_DATAREG
                    lda       $FF22               clear latched interrupts
                    clrb
                    bra       finish@
error@              comb
finish@             puls      cc,d,x,pc

* DirectConnect comes in from the IRQ Handler
* Also since this is a connect, and a ch376 only has one port, this has to be
* the only device in play right now...so don't worry about getting a USB Lock
DirectConnect
                    tst       USBPortConnected
* Error because we got a second connection event before a disconnect
                    bne       error@
                    lbsr      DirectPortReset
                    bcs       error@
                    inc       USBPortConnected
                    ldd       #0
                    lbra      AttachDevice
error@              comb
                    rts

* X struct of
*   Device Match Table Ptr
*   U Memory Location of device driver
*   Probe Address
*   Disconnect Address
* This function is normally called externally, so does not
* have the U pointer to the data area coming in
RegisterDriver
                    pshs      d,x,y,u
                    tfr       x,y
                    ldx       #USBDriverTable
                    ldb       #USBMaxDrivers
loop@               pshs      b
                    ldd       USBDriverDevMatchPtr,x
                    cmpd      #0
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
                    ldy       #USBInterfaceTable
                    ldb       #USBMaxInterfaces
loop1@              pshs      b                   store counter
                    tst       USBInterfaceDeviceId,y is this record populated?
                    beq       skipentry@
                    ldd       USBInterfaceDriverRecord,y check driver record
                    bne       skipentry@          this one is already claimed
                    lda       USBInterfaceDeviceId,y
                    lbsr      GetDeviceRec
                    pshs      y                   save interface record
                    tfr       x,y                 put device record into y
                    ldx       #Heap
getconfig@          bsr       GetConfigDescriptor fill allocated buffer with conf desc
                    puls      y
                    bcs       error@
go@
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
skipprobe@
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
                    ldx       #USBDeviceTable
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
                    ldy       #USBDriverTable
                    ldb       #USBMaxDrivers
loop0@              cmpx      USBDriverProbeFuncPtr,y
                    beq       found@
                    decb
                    beq       error@              got to end of table, error
                    leay      USBDriverRecLength,y
                    bra       loop0@
* Found device, now look for entries in Interface Table
found@              ldu       #USBInterfaceTable
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
findfidoloop@       ldy       #USBDeviceTable
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
                    ldy       #USBDeviceTable
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
                    lbsr      SetDevice
                    lda       #CH376_SET_ADDRESS
                    sta       CH376_CMDREG
                    lda       USBDeviceId,y       load device id in record
                    sta       CH376_DATAREG       #set earlier assigned ID
                    lbsr      WaitIrqResult
                    cmpa      #CH376_USB_INT_SUCCESS
                    lbne      error@
* Now, get first 64 bytes to get bMaxPacketSize0
                    lda       #64
                    sta       USBDeviceMaxPacketSize,y store 64 byte min temporarily because Windows and Linux do it this way so least likely to run into issues with a non-standard device
                    leax      DeviceDesc,pcr
                    leas      -69,s
                    stx       USBCTS.SetupPktPtr,s
                    leax      5,s
                    stx       USBCTS.BufferPtr,s
                    lda       USBDeviceId,y
                    sta       USBCTS.DeviceId,s
                    leax      ,s
                    lbsr      ControlTransfer
                    bcc       goodxfer0@
                    leas      69,s
                    lbra      error@
goodxfer0@          ldb       5+USBDDBMaxPacketSize0,s load bMaxPacketSize0 here
                    leas      69,s
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
                    lda       USBDeviceId,y
                    ldb       5+USBDDIManufacturer,s
                    lbsr      GetString
                    ldb       5+USBDDIProduct,s
                    lbsr      GetString
                    ldb       5+USBDDISerialNumber,s
                    lbsr      GetString
                    leas      31,s
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
goodxfer2@
                    ldd       5+USBCDWTotalLength,s get wTotalLength
                    exg       a,b
                    std       USBDeviceConfigurationWLength,y store in record
                    leas      22,s
                    ldx       #Heap
                    lbsr      GetConfigDescriptor
                    lbcs      error@
* Set Configuration 1
* All devices are always put in configuration 1, even devices without drivers
* This should probably be fixed, but will complicate logic since drivers
* attach to interfaces, not devices. So all interfaces for a device would
* have to be checked to make sure a driver wasn't attached for any of them.
* Then matching code would need to be put into RegisterDriver as well.
                    lda       USBDeviceId,y
                    lbsr      SetDevice
                    ldb       #1
                    lbsr      SetConfiguration
                    bcc       goodconfig@
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
                    ldy       #USBInterfaceTable
                    ldb       #USBMaxInterfaces
finditloop@         lda       USBInterfaceDeviceId,y
                    beq       foundit@            empty slot
                    leay      USBInterfaceRecLength,y
                    decb
                    bne       finditloop@
* Did not find free slot
                    leas      3,s                 clean up stored b and DeviceTable record Pointer
                    bra       error@              then error
foundit@            lda       [1,s]               device id on bus
                    sta       USBInterfaceDeviceId,y
                    ldd       #0                  initially set to no driver
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
                    ldx       #USBDeviceTable
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
                    ldy       #USBInterfaceTable
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
                    lbsr      SetDevice
                    lda       #CH376_SET_CONFIG
                    sta       CH376_CMDREG
                    stb       CH376_DATAREG
                    lbsr      WaitIrqResult
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
                    lda       USBInterfaceDeviceId,y
                    lbsr      GetDeviceRec
                    tfr       x,u                 populate U with Device record ptr
                    ldx       #USBDriverTable     must have memory area pointer intact
                    ldb       #USBMaxDrivers
loop@
* X is driver entry
* U is Device Record
                    pshs      b
                    ldy       USBDriverDevMatchPtr,x this is location of devmatch
                    lbeq      next@               if this slot empty, move to next
                    ldd       USBDeviceVendorId,u Vendor ID
                    anda      USBDevMatchVendorIdMask,y
                    andb      USBDevMatchVendorIdMask+1,y
                    cmpd      USBDevMatchVendorId,y
                    bne       next@
                    ldd       USBDeviceProductId,u Product ID
                    anda      USBDevMatchProductIdMask,y
                    andb      USBDevMatchProductIdMask+1,y
                    cmpd      USBDevMatchProductId,y
                    bne       next@
                    ldd       USBDeviceClass,u    DeviceClass and SubClass
                    anda      USBDevMatchDeviceClassMask,y
                    andb      USBDevMatchDeviceSubClassMask,y
                    cmpd      USBDevMatchDeviceClass,y
                    bne       next@
                    lda       USBDeviceProtocol,u DeviceProtocol
                    anda      USBDevMatchDeviceProtocolMask,y
                    cmpa      USBDevMatchDeviceProtocol,y
                    bne       next@
* Done with device matching, now move on to interface matching
                    ldu       5,s                 load original 'y' interface record
                    ldd       USBInterfaceClass,u InterfaceClass and SubClass
                    anda      USBDevMatchInterfaceClassMask,y
                    andb      USBDevMatchInterfaceSubClassMask,y
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
* This is a shortcut for a ControlTransfer with a Endpoint Request SETUP Packet
* for a ClearFeature with ENDPOINT_HALT (see USB 2.0 Spec pages 252-256)
* This also resets the data toggle bit to 0 even if not stalled.
* Input: DeviceId in A, EndpointId in B (bit 7 high if input endpoint)
* Output: Carry clear if success, set if error
ClearStall
                    pshs      d
                    lbsr      SetDevice
                    lda       #CH376_CLR_STALL
                    sta       CH376_CMDREG
                    stb       CH376_DATAREG
                    lbsr      WaitIrqResult
                    cmpa      #CH376_USB_INT_SUCCESS
                    bne       error@
                    clrb
                    bra       finish@
error@              comb
finish@             puls      d,pc

SetDevice           cmpa      USBDeviceIdCache
                    beq       finish@
                    pshs      a
                    lda       #CH376_SET_USB_ADDR
                    sta       CH376_CMDREG
                    puls      a
                    sta       CH376_DATAREG
                    sta       USBDeviceIdCache
finish@             rts

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
                    bsr       SetDevice
                    lda       USBITS.NakFlag,x
                    cmpa      USBNakRetryCache    if same as last time, skip change
                    beq       skipnak@
                    ldb       #CH376_SET_RETRY
                    stb       CH376_CMDREG
                    ldb       #$25
                    stb       CH376_DATAREG
                    sta       USBNakRetryCache
                    tsta
                    bne       returnnak@
                    lda       #$8F                default from datasheet
                    fcb       $21                 Skip next instruction (BRN)
returnnak@
                    clra
                    sta       CH376_DATAREG
skipnak@
                    ldb       #CH376_ISSUE_TKN_X
                    stb       CH376_CMDREG
                    ldb       USBITS.DataFlag,x   pull DATA0 or DATA1 flag for this endp
                    stb       CH376_DATAREG
                    lda       ,s                  grab transaction attribute off stack
                    sta       CH376_DATAREG
                    lbsr      WaitIrqResult
                    cmpa      #CH376_USB_INT_SUCCESS
                    lbne      error@
                    ldb       USBITS.DataFlag,x
                    eorb      #$80                flip and store flag for next time
                    stb       USBITS.DataFlag,x
                    lda       #CH376_RD_USB_DATA0
                    sta       CH376_CMDREG
                    clra
                    ldb       CH376_DATAREG       D now contains num bytes to read
                    bne       readbuf@            non-zero, so go read it
                    bra       wrapup@
readbuf@            pshs      d
* check against buffer size
* if too big, just error out entirely
                    cmpy      ,s
                    blt       sizeerr@
loop0@              lda       CH376_DATAREG
                    sta       ,u+
                    leay      -1,y
                    decb
                    bne       loop0@
donepkt@
                    puls      d
                    cmpy      #0                  if buffer is full, then exit
                    beq       wrapup@
                    cmpb      USBITS.MaxPacketSize,x see if this was a full packet
                    lbeq      bigloop@            if so then loop for more to read
wrapup@             ldd       USBITS.BufferLength,x
                    pshs      y
                    subd      ,s++
                    bra       finish@
sizeerr@
                    leas      2,s                 get rid of size to read on stack
                    lda       #E$BufSiz           give error code
error@
                    comb
                    tfr       a,b                 return interrupt received in B
finish@             leas      1,s                 get rid of transaction attribute
                    puls      u,y,pc

* Make sure default Nak behavior is set.
* Only used for ControlTransfer and OutTransfer.
* Custom behavior passed in for InTransfer.
SetDefaultNak
                    pshs      u,a
                    tst       USBNakRetryCache    if nak is not default, change to default
                    beq       skipnak@
                    lda       #CH376_SET_RETRY
                    sta       CH376_CMDREG
                    lda       #$25
                    sta       CH376_DATAREG
                    lda       #$8F                default from datasheet
                    sta       CH376_DATAREG
                    clr       USBNakRetryCache
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
                    lbsr      SetDevice
                    bsr       SetDefaultNak
                    lda       #CH376_WR_HOST_DATA
                    sta       CH376_CMDREG
* test against packet size
                    clra
                    ldb       USBOTS.MaxPacketSize,x
                    pshs      d
                    cmpy      ,s++
                    bge       doxfer@
                    tfr       y,d                 in case y is less than a packet size
doxfer@
                    stb       CH376_DATAREG
                    tstb
                    beq       donedata@
loop@               lda       ,u+
                    sta       CH376_DATAREG
                    leay      -1,y
                    decb
                    bne       loop@
donedata@
                    ldb       #CH376_ISSUE_TKN_X
                    stb       CH376_CMDREG
                    ldb       USBOTS.DataFlag,x   pull DATA0 or DATA1 flag for this endp
                    stb       CH376_DATAREG
                    lda       ,s
                    sta       CH376_DATAREG
                    lbsr      WaitIrqResult
                    cmpa      #CH376_USB_INT_SUCCESS
                    bne       error@
                    ldb       USBOTS.DataFlag,x
                    eorb      #$40                flip and store flag for next time
                    stb       USBOTS.DataFlag,x
                    cmpy      #0                  if buffer is done, then exit
                    bne       bigloop@
wrapup@             ldd       USBOTS.BufferLength,x
                    pshs      y
                    subd      ,s++
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
                    lbsr      SetDevice
                    lbsr      SetDefaultNak
                    lda       #CH376_WR_HOST_DATA
                    sta       CH376_CMDREG
                    ldu       USBCTS.SetupPktPtr,x
*                                                126/101 (6809/6309) cycles
                    ldb       #8                  2
                    stb       CH376_DATAREG       4/3
loop0@              lda       ,u+                 6/5 \
                    sta       CH376_DATAREG       4/3  \  loop 8 times
                    decb                          2/1  /  for 120/96 cycles
                    bne       loop0@              3   /
                    lda       #CH376_ISSUE_TKN_X
                    sta       CH376_CMDREG
                    clrb
                    stb       CH376_DATAREG       set to DATA0
                    ldb       #CH376_DEF_USB_PID_SETUP SETUP pid is 1101. endpoint is 0.
                    stb       CH376_DATAREG
                    lbsr      WaitIrqResult
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
                    ldd       #0
                    std       USBITS.BufferPtr,s
                    std       USBITS.BufferLength,s
                    lda       USBCTS.DeviceId,x
                    clrb                          endpoint is 0
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
                    clrb                          endpoint is 0
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
                    ldd       #0
                    std       USBOTS.BufferPtr,s
                    std       USBOTS.BufferLength,s
                    lda       USBCTS.DeviceId,x
                    clrb                          endpoint is 0
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
                    ldy       #USBHubTable
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
* Should probably add a check here that allocating this won't overflow
* the stack, but most hubs only take 1 or 2 bytes as each port is one bit
                    ldd       #0
                    subd      USBHubWMaxPacketSize,y make room on stack
                    leas      d,s                 wMaxPacketSize
retry@              leax      ,s
                    leas      -9,s                make room on stack
                    stx       USBITS.BufferPtr,s  pointer to buffer
                    ldd       USBHubWMaxPacketSize,y
                    std       USBITS.BufferLength,s length of buffer
                    lda       USBHubDeviceId,y
                    ldb       USBHubStatusEndpoint,y
                    std       USBITS.DeviceId,s   usb device id+endpoint id
                    ldd       USBHubWMaxPacketSize,y
                    stb       USBITS.MaxPacketSize,s maxpacket
* The hub will send a NAK if there is no change, so
* instruct the chip to not do a retry, and instead
* bubble up the NAK. Thus, this procedure will return
* with carry set if no change in ports.
* Note this differs from most usage of a set carry (in which it represents an error).
* Carry set from InTransfer with return of $2A return just means no change
* so exit cleanly in this scenario.
                    lda       #1
                    sta       USBITS.NakFlag,s    set to allow NAK
                    tfr       s,x
                    lda       USBHubDataFlag,y
                    sta       USBITS.DataFlag,s   DATA0/DATA1 flag
                    lbsr      InTransfer
                    bcc       goodxfer@
                    leas      9,s
                    cmpb      #$2A                Nak means no change
                    beq       wrapup@
* Try to recover from a DATA0/DATA1 sync error.
                    cmpb      #$2B                Handle DATA1 sync errors
                    beq       syncerror@
                    cmpb      #$23                Handle DATA0 sync errors
                    beq       syncerror@
                    tfr       d,x
                    ldd       USBHubWMaxPacketSize,y restore stack
                    leas      d,s
                    tfr       x,d
                    comb
                    bra       finish@
syncerror@          lda       USBHubDeviceId,y
                    ldb       USBHubStatusEndpoint,y
                    lbsr      ClearStall
                    lbsr      Delay
                    bra       retry@
goodxfer@           lda       USBITS.DataFlag,s   store DATA0/DATA1 flag
                    sta       USBHubDataFlag,y
                    leas      9,s
* Parse bitmap here
                    tfr       s,x                 set x to top of stack
                    lda       ,x+                 load first byte
                    rora                          Ignore Hub power status
                    ldb       #1                  start with port 1
loop0@              rora                          next bit to carry
                    bcc       notthisport@        carry clear means no change
                    lbsr      HubProcessPortChange process port if changed
notthisport@        incb                          go to next bit
                    bitb      #$07                if at a byte boundary
                    bne       notbyteboundary@
                    lda       ,x+                 then load next byte
notbyteboundary@
                    cmpb      USBHubNumPorts,y    compare to number of ports
                    ble       loop0@              keep going for all ports
                    bra       retry@              loop until got NAK
wrapup@             ldd       USBHubWMaxPacketSize,y restore stack
                    leas      d,s
                    clra
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
                    lda       #$03                SET FEATURE Table 9-4
                    bra       mergeclear@
* A is feature selector (USB 2.0 specification table 11-17 on page 421)
* B is port
* Y is hub table record
* Return is carry clear/set if success/error
ClearPortFeature
                    pshs      d,x
                    lda       #$01                CLEAR FEATURE Table 9-4
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
                    ldd       #0
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
*  A,B is destroyed
*  Carry Set if error
HubResetPort        pshs      y
                    lda       #$04                PORT_FEAT_RESET
                    lbsr      SetPortFeature
                    bcs       error@
                    leas      -4,s
                    leax      ,s                  x is 4 bytes result
retry@
                    lbsr      HubGetPortStatus
                    bcc       goodxfer@
                    leas      4,s
                    bra       error@
goodxfer@
                    lda       #$10                $10 is port_reset
                    bita      ,s
                    bne       retry@              loop until port reset done
                    leas      4,s
                    lda       #20                 table 11-17 page 422 usb_20.pdf
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
                    lda       #$01
                    bita      2,x
                    lbeq      noportconnectionchange@
* Ok, we have a connection change, so now we need to wait for debounce.
* This means wait up to 1500ms, checking every
* Tick (16.6ms), waiting for 100ms (6 ticks) of consistent results before
* acting on it.
* This is found in section 7.1.7.3 of the USB 2.0 spec (page 178).
* First, set up our counters on the stack
                    lda       #30                 wait up to 1500 ms (30*50ms)
                    pshs      a
                    leas      -1,s
starttest@          lda       #6                  wait for six 'no change' results
                    sta       ,s
* Then Clear Port Feature
                    lda       #$10                16  C_PORT_CONNECTION
                    lbsr      ClearPortFeature
                    bcs       debounceerror@
* Then wait 50ms to see if there is a change
looptest@           lbsr      Delay
* Then reload status into X buffer
                    lbsr      HubGetPortStatus
* If port status has NOT changed, then decrement ,s. If 0 then we had enough
* reports with no change so continue with connection or disconnection event.
* If it has changed, go back to start of 100ms test.
                    lda       #$01
                    bita      2,x
                    bne       starttest@
                    dec       ,s
                    beq       debouncegood@       got to 100ms, keep going
                    dec       1,s                 error out if timed out
                    bne       looptest@           go wait and test again
debounceerror@      leas      2,s
                    bra       error@
debouncegood@       leas      2,s
* Find out if connected or not
                    bita      ,x
                    lbeq      portdisconnected@
* New Hub Connection here
                    lbsr      HubResetPort
                    bcs       error@
                    lda       USBHubDeviceId,y
                    lbsr      AttachDevice
                    bcs       error@
                    bra       doneportconnchange@
portdisconnected@
* Old Device Disconnected here
                    lda       USBHubDeviceId,y    load hub device no
                    lbsr      DetachDevice
                    bcs       error@
doneportconnchange@
noportconnectionchange@
                    lda       #$02                C_PORT_ENABLE
                    bita      2,x
                    beq       noportenable@
* This is just saying if the port is enabled or not
* so don't handle connection/disconnection events here
* that is handled with C_PORT_CONNECTION instead
                    lda       #17
                    lbsr      ClearPortFeature
                    bcs       error@
noportenable@
                    lda       #$04                C_PORT_SUSPEND
                    bita      2,x
                    beq       noportsuspend@
* Just clearing the status, not doing anything else
* as support for suspending devices is not (yet) implemented in this
* driver so this should never trigger anyway
                    lda       #18                 C_PORT_SUSPEND
                    lbsr      ClearPortFeature
                    bcs       error@
noportsuspend@      lda       #$08                C_PORT_OVER_CURRENT
                    bita      2,x
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
                    ldu       #USBHubTable
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
                    sta       USBHubStatusEndpoint,u
                    ldd       USBEDWMaxPacketSize,x wMaxPacketSize from endpoint desc
                    exg       a,b                 make big endian
                    anda      #$07                strip off microframe data (9.6.6 on page 273 of usb 2.0 spec)
                    cmpd      #64
                    ble       goodsize@
                    ldd       #64                 CH376 max 64 byte buffer
goodsize@           std       USBHubWMaxPacketSize,u and store
* Get port count and store into Hub Table
* See 11.24.2 on page 420 of USB 2.0 Spec
                    leas      -21,S
                    lda       #$A0
                    sta       5,S                 bRequestType
                    lda       #6
                    sta       6,S                 bRequest
                    ldd       #$0029
                    std       7,S                 wValue
                    ldd       #0
                    std       9,S                 wIndex
                    lda       #$08
                    std       11,S                wLength
                    leax      5,S
                    stx       USBCTS.SetupPktPtr,S
                    leax      13,S
                    stx       USBCTS.BufferPtr,S
                    lda       USBInterfaceDeviceId,y
                    sta       USBCTS.DeviceId,S
                    tfr       s,x
                    lbsr      ControlTransfer
                    bcc       goodxfer@
                    leas      21,s
                    bra       xfrerror@
goodxfer@
* See 11.23.21 Hub Descriptor on page 417 of USB 2.0 Spec
* for offsets for bNbrPorts and bPwrOn2PwrGod
                    ldb       13+2,s              port count is at this location
                    stb       USBHubNumPorts,u    store port count into record
* Turn power on all ports
                    lda       #$08                POWER_FEAT_POWER
poweronloop@
                    lbsr      SetPortFeature
                    decb
                    bne       poweronloop@
* bPwrOn2PwrGood is the time in 2ms intervals until power is good on a port
* We're going to use Delay, which is 50ms, so we need the ceiling of this
* number divided by 25. We'll approximate this through multiply by 41, take
* the top byte, shift down 2 bits...which is 41/1024, which is very close
* to 1/25 but instead on a binary boundary allowing us to divide by shifting.
* Finally we increment as a ceiling function.
* This all works out to bPwrOn2PwrGood*FLOOR(41/1024)+1.
* It rounds up unnecessarily if exactly a multiple of 50ms,
* leading to an extra 50ms wait, but close enough for our purposes.
* Also always do a minimum of 100ms (2 Delays).
                    lda       13+5,s              bPwrOn2PwrGood
                    ldb       #41
                    mul
                    lsra
                    lsra
                    inca
                    cmpa      #2
                    bge       powerondelay@
                    lda       #2
powerondelay@       lbsr      Delay
                    deca
                    bne       powerondelay@
                    leas      21,s
* Now we poll the ports for our new hub
* This might recurse down into another new hub. Fun!
                    tfr       u,y                 move hub entry to y (throw away Y)
                    ldu       6,s                 restore USBManMem pointer
                    lbsr      PollHubPorts
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
                    ldy       #USBHubTable
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

