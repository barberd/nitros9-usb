********************************************************************
* llusbmsd - Low-level USB Mass Storage Device (encapsulated SCSI) driver
*
* $Id$
*
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*          2025/01/15  Don Barber
* New USB Mass Storage Device, forked with major changes from llscsi.asm
*
                    nam       llusbmsd
                    ttl       Low-level USB Mass Storage Device driver

                  IFP1
                    use       defsfile
                    use       rbsuper.d
                    use       scsi.d
                    use       usb.d
                  ENDC

tylg                set       Sbrtn+Objct
atrv                set       ReEnt+rev
rev                 set       0

NUMTRIES            equ       2


                    org       0
* USB Mass Storage Device Table Entries
DT.USBDeviceId      rmb       1
DT.USBEndpointIn    rmb       1
DT.USBEndpointOut   rmb       1
DT.USBMaxPacketSizeIn rmb       1
DT.USBMaxPacketSizeOut rmb       1
DT.USBDataFlag      rmb       1
DT.USBInterfaceNum  rmb       1
USBMSDDevSize       equ       .
USBMSDMaxDevices    equ       2
* Note: if you need more than 4 mass storage devices, this won't fit in the
* 64 bytes left by rbsuper as currently written. You'll need to refactor
* the code to request more memory with F$SRqMem in ll_init to store the table.

                    org       V.LLMem

* USB Bulk Transport Command Block CBW
V.dCBWSignature     rmb       4
V.dCBWTag           rmb       4
V.dCBWDataTransferLength rmb       4
V.bmCBWFlags        rmb       1
V.bCBWLUN           rmb       1
V.bCBWCBLength      rmb       1
* Low-level driver static memory area
* SCSI Command Packet
* SCSI packet length is 16 bytes
V.SCSICMD           rmb       1
V.SCSILUN           rmb       1
V.SCSIPrm0          rmb       1
V.SCSIPrm1          rmb       1
V.SCSIPrm2          rmb       1
V.SCSIPrm3          rmb       1
V.SCSIPrm4          rmb       1
V.SCSIPrm5          rmb       1
V.SCSIPrm6          rmb       1
V.SCSIPrm7          rmb       1
                    rmb       6
SCSIPkLn            equ       .-V.SCSICMD
V.Retries           rmb       1                   SCSI command retry counter
V.OS9Err            rmb       1                   (0 = return OS-9 error code, 1 = return SCSI error code)
V.TfrBuf            rmb       2                   transfer buffer pointer
* The sharp-eyed might notice that llscsi.asm had a buffer here for
* use in DSize and SCSIErr. That functionality was refactored to use the stack
* instead so there would be room for multiple USB mass storage devices here.
USBMSDDeviceTable   rmb       USBMSDDevSize*USBMSDMaxDevices

                  IFGT    .-V.LLMem-64
* rbsuper only leaves 64 bytes for low level drivers, so this spot needs
* to be <= 64
                    error     Memory              too big for rbsuper
                  ENDC

* Offsets for Command Status Wrapper from USB Mass Storage Device Bulk Transfer Spec
                    org       0
USBMSDDCSWSignature rmb       4
USBMSDDCSWTag       rmb       4
USBMSDDCSWDataResidue rmb       4
USBMSDBCSWStatus    rmb       1

                    mod       eom,name,tylg,atrv,start,0

name                fcs       /llusbmsd/
usbmanname          fcs       /usbman/

* Note to future self: we are trying to get total bytes, so we need to
* calculate sectors * bytes/sector. So for 256 bytes, multiply by $100,
* for 512 bytes, multiply by $200, and so on. Since the end result needs to be
* in little endian, instead just multiply by $1, $2, etc, and check to
* make sure the high byte is still 0. If the high byte is not 0, this means
* we overflowed what we can handle in 16 bits, and need to return an error.
SECMULTTBL          fcb       256/256
                    fcb       512/256
                    fcb       1024/256
                    fcb       2048/256

MSDDevMatch         fdb       $0000               0 VendorId
                    fdb       $0000               2 Mask
                    fdb       $0000               4 ProductId
                    fdb       $0000               6 Mask
                    fcb       $00                 8 DeviceClass
                    fcb       $00                 DeviceSubClass
                    fcb       $00                 10 ClassMask
                    fcb       $00                 SubClassMask
                    fcb       $00                 12 DeviceProtocol
                    fcb       $00                 13 Mask
                    fcb       $08                 14 InterfaceClass       Mass Storage
                    fcb       $06                 InterfaceSubClass    SCSI
                    fcb       $FF                 16 ClassMask
                    fcb       $FF                 SubClassMask
                    fcb       $50                 18 InterfaceProtocol    Bulk-Only
                    fcb       $FF                 19 Mask

start               bra       ll_init
                    nop
                    lbra      ll_read
                    lbra      ll_write
                    lbra      ll_getstat
                    lbra      ll_setstat
*         lbra  ll_term

* ll_term
*
* Entry:
*    Y  = address of device descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
*
* Note: This routine is called ONCE: for the last device
* IT IS NOT CALLED PER DEVICE!
*
ll_term
                    pshs      a,x,y
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    leax      MSDProbe,pcr
                    jsr       USBDeregisterDriver,y
                    puls      y,x,a,pc

* ll_init
*
* Entry:
*    Y  = address of device descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
* Note: This routine is called ONCE: for the first device
* IT IS NOT CALLED PER DEVICE!
*
ll_init
* Clear USB MSD device table
                    leay      USBMSDDeviceTable,u
                    ldx       #USBMSDDevSize*USBMSDMaxDevices
                    clra
clrloop@            sta       ,y+
                    leax      -1,x
                    bne       clrloop@
* Start up USBMan if not started yet
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
                    bcs       InitEx
usbmanready@
* Set up SCSI packet structure
                    ldd       #$5553
                    std       V.dCBWSignature,u
                    ldd       #$4243              signature in little endian
                    std       V.dCBWSignature+2,u
                    ldd       #0
                    std       V.dCBWDataTransferLength+2,u
                    std       V.dCBWTag,u
                    std       V.dCBWTag+2,u
                    stb       V.bCBWLUN,u
* Then register with USBMan
                    leas      -8,s
                    leax      MSDDevMatch,pcr
                    stx       ,s
                    stu       2,s
                    leax      MSDProbe,pcr
                    stx       4,s
                    leax      MSDDisconnect,pcr
                    stx       6,s
                    tfr       s,x
                    jsr       USBRegisterDriver,y
                    leas      8,s
InitEx              rts

* Note: this driver does not support devices with multiple LUNs in
* a single USB Device
* U Memory Location
* Y Interface table entry
* X Interface entry in memory
* Returns Carry Clear if accept
MSDProbe
                    pshs      d,x,y,u
* Find empty entry in MSD device table
                    leau      USBMSDDeviceTable,u
                    ldb       #USBMSDMaxDevices
findloop@           tst       ,u
                    beq       found@
                    leau      USBMSDDevSize,u
                    decb
                    bne       findloop@
                    bra       error@              no clear entry found
found@
* Store into Device Entry
                    lda       USBInterfaceDeviceId,y
                    sta       DT.USBDeviceId,u    device id
                    lda       USBInterfaceNum,y
                    sta       DT.USBInterfaceNum,u interfacenum
                    clr       DT.USBDataFlag,u    data0/data1 flag
                    lda       USBIDBNumEndpoints,x
                    sta       ,-s
* Loop over descriptor, stopping at endpoints.
loop1@              lda       USBDescriptorType,x
                    cmpa      #$05
                    bne       next@
foundendpoint@      lda       USBEDBEndpointAddress,x
                    bpl       endpout@
* record EndpointIn here
                    anda      #$7F
                    sta       DT.USBEndpointIn,u  store endpoint in table
                    bsr       FindMaxPacket
                    stb       DT.USBMaxPacketSizeIn,u store MaxPacketSizeIn
                    bra       merge@
* record EndpointOut here
endpout@            sta       DT.USBEndpointOut,u store endpoint in table
                    bsr       FindMaxPacket
                    stb       DT.USBMaxPacketSizeOut,u store MaxPacketSizeOut
merge@              dec       ,s
next@               ldb       USBDescriptorLength,x
                    leax      b,x
                    tst       ,s
                    bne       loop1@
                    leas      1,s
* Reset device
                    lbra      MSDResetInit
error@              comb
                    puls      u,x,y,d,pc

FindMaxPacket
                    ldd       USBEDWMaxPacketSize,x
                    exg       a,b
                    cmpd      #64
                    ble       nochangeOut@
                    ldb       #64
nochangeOut@        rts

* X is pointer to the USB Device Table Entry
* Y is pointer to the USB Interface Table Entry
* U is memory location for this driver
MSDDisconnect       pshs      d,u
                    lda       ,x
                    leau      USBMSDDeviceTable,u
                    ldb       #USBMSDMaxDevices
findloop@           cmpa      DT.USBDeviceId,u
                    beq       founddrive@
                    leau      USBMSDDevSize,u
                    decb
                    bne       findloop@
                    bra       error@              did not find entry
founddrive@         ldb       #USBMSDDevSize
                    clra
loop1@              sta       ,u+
                    decb
                    bne       loop1@
                    bra       finish@
error@              comb
finish@             puls      u,d,pc

* ll_getstat
*
* Entry:
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
ll_getstat
                    ldx       PD.RGS,y
                    lda       R$B,x
                    cmpa      #SS.DSize
                    beq       SSDSize
                    ldb       #E$UnkSvc
                    coma
                    rts

* SS.DSize - Return size information about a device
*
* Entry: B = SS.DSize, U=Device Memory Area
* Exit:  Carry = 1. error with code in B
*        Carry = 0:
*          IF B = 0
*            A = Sector Size (1 = 256, 2 = 512, 4 = 1024, 8 = 2048)
*            X = Number of Sectors (bits 31-16)
*            Y = Number of Sectors (Bits 15-0)
*          IF B != 0
*            A = Sector Size (1 = 256, 2 = 512, 4 = 1024, 8 = 2048)
*            X = Number of Logical Cylinders
*            B = Number of Logical Sides
*            Y = Number of Logical Sectors/Track
*
SSDSize
                    pshs      u,y
                    bsr       DSize
                    bcs       ex@
                    ldu       ,s                  get original Y path desc into U
                    ldu       PD.RGS,u
                    std       R$D,u
                    stx       R$X,u
                    sty       R$Y,u
                    clrb
ex@                 puls      y,u,pc

* DSize - Get SCSI disk size
* Entry: U=Device Memory Area
* Exit:  Carry = 1. error with code in B
*        Carry = 0:
*          IF B = 0
*            A = Sector Size (1 = 256, 2 = 512, 4 = 1024, 8 = 2048)
*            X = Number of Sectors (bits 31-16)
*            Y = Number of Sectors (Bits 15-0)
*          IF B != 0
*            A = Sector Size (1 = 256, 2 = 512, 4 = 1024, 8 = 2048)
*            X = Number of Logical Cylinders
*            B = Number of Logical Sides
*            Y = Number of Logical Sectors/Track
DSize               lbsr      SCSIPrep            do SCSI prep stuff
                    lda       #S$RCAP
                    sta       V.SCSICMD,u
                    lda       #$80
                    sta       V.bmCBWFlags,u
                    lda       #16
                    sta       V.bCBWCBLength,u
                    leas      -8,s
                    sts       V.TfrBuf,u
                    ldd       #$0800
                    std       V.dCBWDataTransferLength,u
                    lbsr      BOTXfer
                    bcs       ex@
                    ldd       2,s                 get bits 15-0 of block count
                    addd      #$0001              add 1 to count
                    std       2,s                 resave
                    pshs      cc
                    ldd       1,s
                    puls      cc
                    bcc       b@
                    ldd       ,s                  get bits 31-16
                    addd      #$0001              add 1
                    std       ,s                  resave
b@                  ldx       ,s
                    ldy       2,s
                    clrb
                    bra       finish@
ex@                 ldb       E$Read
finish@             leas      8,s
                    rts

* ll_setstat
*
* Entry:
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
ll_setstat
                    ldx       PD.RGS,y
                    lda       R$B,x
                    cmpa      #SS.SQD
                    bne       n@
                    bra       StopUnit
n@                  clrb
ex                  rts

* Entry:
*    R$B = SS.SQD
StopUnit
                    lbsr      SCSIPrep            do SCSI prep stuff
                    lda       #S$UNIT
                    sta       V.SCSICMD,u
                    clr       V.SCSIPrm2,u        we want to STOP unit
                    lda       #16
                    sta       V.bCBWCBLength,u
                    ldd       #0
                    std       V.dCBWDataTransferLength,u
                    lbra      BOTXfer

* ll_read
*
* Entry:
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Static memory referenced:
*    V.CchPSpot     = address of spot in cache where physical sector data will go
*    sectsize       = sector size (0=256,1=512,2=1024,3=2048)
*    V.SectCnt      = sectors to read
*    V.PhysSect = physical sector to start read from
ll_read
                    bsr       SCSIPrep            do SCSI prep stuff
                    bsr       MakeRead            make read command packet
                    lda       #$80
                    sta       V.bmCBWFlags,u
                    bsr       ll_rw
                    bcs       tfrerror@
                    bsr       BOTXfer
                    bcc       return@
tfrerror@           comb
                    ldb       #E$Read
return@             rts

* ll_write
*
* Entry:
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Static memory referenced:
*    V.CchPSpot     = address of spot in cache where physical sector data is
*    sectsize       = sector size (0=256,1=512,2=1024,3=2048)
*    V.SectCnt      = sectors to read
*    V.PhysSect     = physical sector to start write to
ll_write
                    bsr       SCSIPrep            do SCSI prep stuff
                    bsr       MakeWrite           make write command packet
                    clr       V.bmCBWFlags,u
                    bsr       ll_rw
                    bcs       tfrerror@
                    bsr       BOTXfer
                    bcc       return@
tfrerror@           comb
                    ldb       #E$Write
return@             rts

ll_rw               ldx       V.CchPSpot,u
                    stx       V.TfrBuf,u
                    lda       #16
                    sta       V.bCBWCBLength,u
* Calculate buffer size to transfer
                    lda       V.SectSize,u
                    leax      SECMULTTBL,pcr
                    lda       a,x
                    ldb       V.SectCnt,u
                    mul
                    tsta
* If anything is in A, this means 64k or more bytes to be transfered on machine
* that can only address 64k at a time.
* Clearly something went wrong for us to have that problem.
                    bne       error@
* remember this is stored little endian
                    std       V.dCBWDataTransferLength,u
                    bra       finish@
error@              comb
finish@             rts

* Make Read/Write Packet
* Entry:
*    A = SCSI command
*    V.PhysSect = 3 byte physical sector to read/write
MakeWrite           lda       #S$WRITEX
                    fcb       $8C                 skip next two bytes
MakeRead            lda       #S$READEX
MakeRW              sta       V.SCSICMD,u         put passed SCSI command
                    lda       V.SectCnt,u         'V.SectCnt' physical blocks
* Make SCSI Read/Write Extended 10 byte CDB here
                    sta       V.SCSIPrm6,u
                    lda       V.PhysSect,u
                    sta       V.SCSIPrm1,u
                    ldd       V.PhysSect+1,u
                    std       V.SCSIPrm2,u
                    rts

* Prep for SCSI transfer
* Preserves all regs
* Entry: Y = path descriptor, U=Device Memory Area
SCSIPrep            pshs      x,d
                    leax      V.SCSICMD,u
                    ldb       #SCSIPkLn
l@                  clr       ,x+
                    decb
                    bne       l@
                    lda       #NUMTRIES           get retry count
                    sta       V.Retries,u         and reset retry counter
                    clr       V.OS9Err,u          we want real SCSI errors returned
                    puls      x,d,pc

* Bulk Only Transfer as described in the USB Mass Storage Device spec
BOTXfer
*
* Entry:
*    Y  = address of Path Descriptor
*    U  = address of device memory area
*
* Static memory referenced:
*    V.TfrBuf,u      = address of transfer/receive buffer
                    pshs      y                   Store Path Descriptor on Stack
* Set up pointer to MSD Table Entry calculated from PD.DRV,y
* This pointer is populated in X most of the time
* But since X is used to pass structures to USBMan,
* it'll get moved on/off the stack and traded with U
                    lda       PD.DRV,y
                    ldb       #USBMSDDevSize
                    mul
                    leax      USBMSDDeviceTable,u
                    leax      d,x
                    lda       DT.USBDeviceId,x
                    lbeq      error@
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    inc       V.dCBWTag,u
                    pshs      u
                    exg       u,x
                    leas      -8,s
                    leax      V.dCBWSignature,x
                    stx       USBOTS.BufferPtr,s
                    ldx       #31
                    stx       USBOTS.BufferLength,s
                    ldb       DT.USBEndpointOut,u
                    std       USBOTS.DeviceId,s
                    lda       DT.USBDataFlag,u
                    sta       USBOTS.DataFlag,s
                    lda       DT.USBMaxPacketSizeOut,u
                    sta       USBOTS.MaxPacketSize,s
                    tfr       s,x
                    jsr       USBOutTransfer,y
                    pshs      cc
                    lda       7,s
                    sta       DT.USBDataFlag,u    store DATA0/DATA1 flag
                    puls      cc
                    leas      8,s
                    tfr       u,x
                    puls      u
                    lbcs      deverror@
                    ldd       V.dCBWDataTransferLength,u
                    beq       csw@                no transfer so skip to csw
                    leas      -9,s
                    exg       a,b                 little to big endian
                    std       USBOTS.BufferLength,s
                    ldd       V.TfrBuf,u
                    std       USBOTS.BufferPtr,s
                    lda       DT.USBDeviceId,x
                    sta       USBOTS.DeviceId,s
                    lda       DT.USBDataFlag,x
                    sta       USBOTS.DataFlag,s
                    tst       V.bmCBWFlags,u
                    bmi       dev2host@
* Do Host-to-Device (OUT) write
                    lda       DT.USBEndpointOut,x
                    sta       USBOTS.EndpointId,s
                    lda       DT.USBMaxPacketSizeOut,x
                    sta       USBOTS.MaxPacketSize,s
                    pshs      x
                    leax      2,s
                    jsr       USBOutTransfer,y
                    pshs      cc
                    lda       3+USBOTS.DataFlag,s
                    ldx       1,s
                    sta       DT.USBDataFlag,x    store DATA0/DATA1 flag
                    puls      cc
                    leas      11,s
                    bcc       csw@
                    lbra      deverror@
dev2host@
* Do Device-to-Host (IN) read
                    lda       DT.USBEndpointIn,x
                    sta       USBITS.EndpointId,s
                    lda       DT.USBMaxPacketSizeIn,x
                    sta       USBITS.MaxPacketSize,s
                    clr       USBITS.NakFlag,s    set standard NAK behavior
                    pshs      x
                    leax      2,s
                    jsr       USBInTransfer,y
                    pshs      cc
                    lda       3+USBITS.DataFlag,s
                    ldx       1,s
                    sta       DT.USBDataFlag,x    store DATA0/DATA1 flag
                    puls      cc
                    leas      11,s
                    lbcs      deverror@
csw@                pshs      x
                    leas      -22,s               9 bytes plus CSW status buf
                    ldd       #$000D
                    std       USBITS.BufferLength,s
                    ldd       DT.USBDeviceId,x
                    std       USBITS.DeviceId,s
                    lda       DT.USBDataFlag,x
                    sta       USBITS.DataFlag,s
                    lda       DT.USBMaxPacketSizeOut,x
                    sta       USBITS.MaxPacketSize,s
                    clr       USBITS.NakFlag,s    default NAK behavior
                    leax      9,s
                    stx       USBITS.BufferPtr,s
                    lda       #1                  set up 1 retry for STALL
                    sta       ,-s
redocsw@            leax      1,s
                    jsr       USBInTransfer,y
                    pshs      cc
                    lda       2+USBITS.DataFlag,s
                    ldx       24,s
                    sta       DT.USBDataFlag,x    store DATA0/DATA1 flag
                    puls      cc
                    bcc       goodcsw@            if no error, keep going
                    cmpb      #$2E                check if error was a stall
                    bne       cswerrorstack@      if not, go to reset recovery
                    tst       ,s                  if stall, check if first or second
                    beq       cswerrorstack@      if second, go to reset recovery
                    dec       ,s                  if first, decrement counter
* clear the endpoint in
                    ldd       DT.USBDeviceId,x    then clear stall
                    orb       #$80
                    jsr       USBClearStall,y
                    bra       redocsw@            and re-request CSW
goodcsw@
                    leas      1,s                 clean up stack from STALL retry counter
* check CSW for error here
* First check for proper signature
                    ldd       9+USBMSDDCSWSignature,s
                    cmpd      #$5553
                    bne       cswerror@
                    ldd       9+USBMSDDCSWSignature+2,s
                    cmpd      #$4253
                    bne       cswerror@
* Then check CSWStatus
                    lda       9+USBMSDBCSWStatus,s
                    beq       nostatuserror@
                    leas      24,s                clean up stack for error handling
                    bra       deverror@
nostatuserror@
* next check for matching CBWTag and CSWTag
                    ldd       V.dCBWTag,u
                    ldd       V.dCBWTag+2,u
                    ldd       9+USBMSDDCSWTag,s
                    cmpd      V.dCBWTag,u
                    bne       cswerror@
                    ldd       9+USBMSDDCSWTag+2,s
                    cmpd      V.dCBWTag+2,u
                    bne       cswerror@
* next check dCSWDataResidue, should be 0
                    ldd       9+USBMSDDCSWDataResidue,s
                    bne       cswerror@
                    ldd       9+USBMSDDCSWDataResidue+2,s
                    bne       cswerror@
* if here, then all CSW checks passed
                    leas      24,s
                    clrb
                    bra       finish@
cswerrorstack@      leas      1,s                 clean up stack from CSW stall counter
cswerror@           leas      24,s
deverror@           ldy       ,s                  Restore Y to path device pointer
                    bsr       MSDResetRecovery    Issue device reset and clear endpoint stalls
                    lda       V.Retries,u
                    deca
                    sta       V.Retries,u
                    beq       error@
                    puls      y
                    lbra      BOTXfer             Loop for retries
error@              comb
finish@             puls      y,pc

* As described in usbmassbulk_10.pdf as part of the USB Mass Storage Device spec
* found in section 5.3.4 on page 16.
* Input
*    Y    Pointer to Path Descriptor
*    U    Pointer to Driver Memory
MSDResetRecovery
                    pshs      d,x,y,u
                    lda       PD.DRV,y
                    ldb       #USBMSDDevSize
                    mul
                    leau      USBMSDDeviceTable,u
                    leau      d,u
MSDResetInit
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
* Issue a device reset on the interface num
* as described in USB Bulk-Only Mass Storage Device spec
                    leas      -13,s
                    ldd       #0
                    std       11,s
                    std       7,s
                    stb       10,s
                    lda       DT.USBInterfaceNum,u
                    sta       9,s
                    ldd       #$21FF
                    std       5,s
                    leax      5,s
                    stx       USBCTS.SetupPktPtr,s
                    lda       DT.USBDeviceId,u
                    sta       USBCTS.DeviceId,s
                    tfr       s,x
                    clr       ,-s                 try this up to 256 times
retrybulkreset@
                    jsr       USBControlTransfer,y
                    bcc       donebulkreset@
                    dec       ,s
                    bne       retrybulkreset@
donebulkreset@
                    leas      14,s
                    ldd       DT.USBDeviceId,u
                    orb       #$80                make bit 7 high for in endpoint
                    jsr       USBClearStall,y     clear endpoint in
                    ldb       DT.USBEndpointOut,u
                    jsr       USBClearStall,y     clear endpoint out
                    clr       DT.USBDataFlag,u    data0/data1 flag
                    puls      u,y,x,d,pc

                    emod
eom                 equ       *
                    end

