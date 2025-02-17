* msdinit
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
msdinit
* Set up SCSI packet structure
                    ldd       #$5553
                    std       V.dCBWSignature
                    ldd       #$4243              signature in little endian
                    std       V.dCBWSignature+2
                    ldd       #$0002
* remember this is stored little endian
                    std       V.dCBWDataTransferLength
                    ldd       #0
                    std       V.dCBWDataTransferLength+2
                    std       V.dCBWTag
                    std       V.dCBWTag+2
                    stb       V.bCBWLUN
                    lda       #1
                    sta       V.SCSIPrm6          write one sector
                    lda       #$28
                    sta       V.SCSICMD           put READ(10) SCSI Command
                    lda       #$80
                    sta       V.bmCBWFlags
                    lda       #16
                    sta       V.bCBWCBLength
* Then register with USBMan
                    leas      -8,s
                    leax      MSDDevMatch,pcr
                    stx       ,s
                    stu       2,s
                    leax      MSDProbe,pcr
                    stx       4,s
                    ldx       #0
                    stx       6,s
                    tfr       s,x
                    lbsr      RegisterDriver
                    leas      8,s
InitEx              rts

* Note: this driver does not support devices with multiple LUNs in
* a single USB Device
* U Memory Location
* Y Interface table entry
* X Interface entry in memory
* Returns Carry Clear if accept
MSDProbe
                    pshs      d,x,u
* Find empty entry in MSD device table
                    ldu       #USBMSDDeviceTable
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
* Record EndpointIn here
                    anda      #$7F
                    sta       DT.USBEndpointIn,u  store endpoint in table
                    bsr       FindMaxPacket
                    stb       DT.USBMaxPacketSizeIn,u store MaxPacketSizeIn
                    bra       merge@
* Record EndpointOut here
endpout@            sta       DT.USBEndpointOut,u store endpoint in table
                    bsr       FindMaxPacket
                    stb       DT.USBMaxPacketSizeOut,u store MaxPacketSizeOut
merge@              dec       ,s
next@               ldb       USBDescriptorLength,x
                    leax      b,x
                    tst       ,s
                    bne       loop1@
                    leas      1,s
                    bra       finish@
error@              comb
finish@             puls      u,x,d,pc

FindMaxPacket
                    ldd       USBEDWMaxPacketSize,x
                    exg       a,b
                    cmpd      #64
                    ble       nochangeOut@
                    ldb       #64
nochangeOut@        rts

* msdread
*
* Entry:
*    B  = MSB of the disk's LSN
*    X  = LSB of the disk's LSN
*    Y = buffer to read into
* Static memory referenced:
*    V.PhysSect = physical sector to start read from
msdread             pshs      x,d
                    stb       V.SCSIPrm1
                    stx       V.SCSIPrm2
                    sty       V.TfrBuf
                    lda       #3                  get retry count
                    sta       V.Retries           and reset retry counter
* Calculate buffer size to transfer
                    bsr       BOTXfer
                    bcc       return@
tfrerror@           comb
                    ldb       #E$Read
return@             puls      d,x,pc

* Bulk Only Transfer as described in the USB Mass Storage Device spec
BOTXfer
*
* Static memory referenced:
*    V.TfrBuf      = address of transfer/receive buffer
                    ldu       #USBMSDDeviceTable
                    lda       DT.USBDeviceId,u
                    lbeq      error@
                    inc       V.dCBWTag
                    leas      -8,s
                    ldx       #V.dCBWSignature
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
                    lbsr      OutTransfer
                    pshs      cc
                    lda       7,s
                    sta       DT.USBDataFlag,u    store DATA0/DATA1 flag
                    puls      cc
                    leas      8,s
                    lbcs      deverror@
                    ldd       V.dCBWDataTransferLength
                    beq       csw@                no transfer so skip to csw
                    leas      -9,s
                    exg       a,b                 little to big endian
                    std       USBOTS.BufferLength,s
                    ldd       V.TfrBuf
                    std       USBOTS.BufferPtr,s
                    lda       DT.USBDeviceId,u
                    sta       USBOTS.DeviceId,s
                    lda       DT.USBDataFlag,u
                    sta       USBOTS.DataFlag,s
                    tst       V.bmCBWFlags
                    bmi       dev2host@
* Do Host-to-Device (OUT) write
                    lda       DT.USBEndpointOut,u
                    sta       USBOTS.EndpointId,s
                    lda       DT.USBMaxPacketSizeOut,u
                    sta       USBOTS.MaxPacketSize,s
                    leax      ,s
                    lbsr      OutTransfer
                    pshs      cc
                    lda       1+USBOTS.DataFlag,s
                    sta       DT.USBDataFlag,u    store DATA0/DATA1 flag
                    puls      cc
                    leas      9,s
                    bcc       csw@
                    cmpb      #$2E                if stall
                    lbne      deverror@
                    lda       DT.USBDeviceId,u    then clear Out pipe
                    ldb       DT.USBEndpointOut,u
                    bra       merge@
dev2host@
* Do Device-to-Host (IN) read
                    lda       DT.USBEndpointIn,u
                    sta       USBITS.EndpointId,s
                    lda       DT.USBMaxPacketSizeIn,u
                    sta       USBITS.MaxPacketSize,s
                    clr       USBITS.NakFlag,s    set standard NAK behavior
                    leax      ,s
                    lbsr      InTransfer
                    pshs      cc
                    lda       1+USBITS.DataFlag,s
                    sta       DT.USBDataFlag,u    store DATA0/DATA1 flag
                    puls      cc
                    leas      9,s
                    bcc       csw@
                    cmpb      #$2E
                    lbne      deverror@
* On stall, clear bulk-in pipe
                    ldd       DT.USBDeviceId,x
                    orb       #$80
merge@              lbsr      ClearStall
                    lbra      deverror@
csw@                leas      -22,s               9 bytes plus CSW status buf
                    ldd       #$000D
                    std       USBITS.BufferLength,s
                    ldd       DT.USBDeviceId,u
                    std       USBITS.DeviceId,s
                    lda       DT.USBDataFlag,u
                    sta       USBITS.DataFlag,s
                    lda       DT.USBMaxPacketSizeOut,u
                    sta       USBITS.MaxPacketSize,s
                    clr       USBITS.NakFlag,s    default NAK behavior
                    leax      9,s
                    stx       USBITS.BufferPtr,s
                    lda       #1                  set up 1 retry for STALL
                    sta       ,-s
redocsw@            leax      1,s
                    lbsr      InTransfer
                    pshs      cc
                    lda       2+USBITS.DataFlag,s
                    sta       DT.USBDataFlag,u    store DATA0/DATA1 flag
                    puls      cc
                    bcc       goodcsw@            if no error, keep going
                    cmpb      #$2E                check if error was a stall
                    bne       cswerrorstack@      if not, go to reset recovery
                    tst       ,s                  if stall, check if first or second
                    beq       cswerrorstack@      if second, go to reset recovery
                    dec       ,s                  if first, decrement counter
* clear the endpoint in
                    ldd       DT.USBDeviceId,u    then clear stall
                    orb       #$80
                    lbsr      ClearStall
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
                    leas      22,s                clean up stack for error handling
                    bra       deverror@
nostatuserror@
* next check for matching CBWTag and CSWTag
                    ldd       V.dCBWTag
                    ldd       V.dCBWTag+2
                    ldd       9+USBMSDDCSWTag,s
                    cmpd      V.dCBWTag
                    bne       cswerror@
                    ldd       9+USBMSDDCSWTag+2,s
                    cmpd      V.dCBWTag+2
                    bne       cswerror@
* next check dCSWDataResidue, should be 0
                    ldd       9+USBMSDDCSWDataResidue,s
                    bne       cswerror@
                    ldd       9+USBMSDDCSWDataResidue+2,s
                    bne       cswerror@
* if here, then all CSW checks passed
                    leas      22,s
                    clrb
                    bra       finish@
cswerrorstack@      leas      1,s                 clean up stack from CSW stall counter
cswerror@           leas      22,s
deverror@           bsr       MSDResetRecovery    Issue device reset and clear endpoint stalls
                    dec       V.Retries
                    beq       error@
                    lbra      BOTXfer             Loop for retries
error@              comb
finish@             rts

* As described in usbmassbulk_10.pdf as part of the USB Mass Storage Device spec
* found in section 5.3.4 on page 16.
MSDResetRecovery
                    pshs      d,x,y,u
                    ldu       #USBMSDDeviceTable
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
                    lbsr      ControlTransfer
                    bcc       donebulkreset@
                    dec       ,s
                    bne       retrybulkreset@
donebulkreset@
                    leas      14,s
                    ldd       DT.USBDeviceId,u
                    orb       #$80                make bit 7 high for in endpoint
                    lbsr      ClearStall          clear endpoint in
                    ldb       DT.USBEndpointOut,u
                    lbsr      ClearStall          clear endpoint out
                    clr       DT.USBDataFlag,u    data0/data1 flag
                    puls      u,y,x,d,pc

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


