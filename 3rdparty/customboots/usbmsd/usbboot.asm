                    use       os9.d
                    use       usb.d
                    use       ch376.d

USBMemoryBase       equ       $1200

USBMaxDevices       equ       6
USBMaxDrivers       equ       2
USBMaxInterfaces    equ       16
USBMaxHubs          equ       3
USBMSDMaxDevices    equ       1

                    org       0
* USB Mass Storage Device Table Entry Offsets
DT.USBDeviceId      rmb       1
DT.USBEndpointIn    rmb       1
DT.USBEndpointOut   rmb       1
DT.USBMaxPacketSizeIn rmb       1
DT.USBMaxPacketSizeOut rmb       1
DT.USBDataFlag      rmb       1
DT.USBInterfaceNum  rmb       1
USBMSDDevSize       equ       .

                    org       0
* USB Hub Record offsets
USBHubDeviceId      rmb       1
USBHubNumPorts      rmb       1
USBHubStatusEndpoint rmb       1
USBHubWMaxPacketSize rmb       2
USBHubDataFlag      rmb       1
USBHubRecLength     equ       .

                    org       0
* Offsets for Command Status Wrapper from USB Mass Storage Device Bulk Transfer Spec
USBMSDDCSWSignature rmb       4
USBMSDDCSWTag       rmb       4
USBMSDDCSWDataResidue rmb       4
USBMSDBCSWStatus    rmb       1

                    org       USBMemoryBase
#USBEndpoints       rmb       2
USBIntStatus        rmb       1
USBPortConnected    rmb       1
USBDeviceTable      rmb       USBDeviceRecLength*USBMaxDevices
USBInterfaceTable   rmb       USBInterfaceRecLength*USBMaxInterfaces
USBDriverTable      rmb       USBDriverRecLength*USBMaxDrivers
USBHubTable         rmb       USBMaxHubs*USBHubRecLength
USBDeviceIdCache    rmb       1
USBNakRetryCache    rmb       1

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
V.TfrBuf            rmb       2                   transfer buffer pointer
USBMSDDeviceTable   rmb       USBMSDDevSize*USBMSDMaxDevices
USBMemSize          equ       .
Heap

                    ifndef    CODESTART
CODESTART           set       $3800
                  ENDC

                    org       CODESTART

start               lds       #$8000
                    orcc      #IntMasks
                    clr       $6F                 set output devnum to 0
* Clear out memory
                    ldu       #USBMemoryBase
                    ldx       #USBMemSize
                    clra
clrloop@            sta       ,u+
                    leax      -1,x
                    bne       clrloop@
                    leax      CardInitStr,pcr
                    lbsr      StrOut
                    lbsr      CardInit
                    bcc       goodinit@
                    leax      InitErrorStr,pcr
                    lbsr      StrOut
                    lbra      Crash
goodinit@
                    lbsr      msdinit
                    bcc       getstarted@
                    leax      MSDInitErrStr,pcr
                    lbsr      StrOut
                    lbra      Crash
getstarted@
                    leax      DoneInitStr,pcr
                    lbsr      StrOut
mainloop@
                    tst       USBPortConnected    Check if something is connected
                    bne       skipconnect@        If it is, check devices
needdevice@         leax      WaitDeviceStr,pcr
                    lbsr      StrOut
                    lbsr      WaitIrqResult       if not, wait for connect
                    bra       mainloop@
skipconnect@
                    tst       USBMSDDeviceTable   check if mass storage device is connect
                    bne       readtrack           if so, go read it
                    tst       USBHubTable         Check if a hub has been registered
                    beq       skiphub@            if not, skip poll
                    leax      PollHubStr,pcr
                    lbsr      StrOut
                    lbsr      PollHubs            if so, then poll them
                    lbsr      Delay
                    bra       mainloop@
skiphub@            leax      NoBootDevStr,pcr
                    bsr       StrOut
                    bra       needdevice@

readtrack
                    leax      FoundDeviceStr,pcr
                    bsr       StrOut
                    leax      LoadBTStr,pcr
                    bsr       StrOut

* load track 34 equivalent (starts at $26400)
* Move the $1200 bytes there into $2600-$37FF
* On a 512-byte sector device, this is 9 blocks starting at
* block $132 (decimal 306)
                    lda       #9
                    ldy       #$2600
                    clrb
                    ldx       #$132
loop@               lbsr      msdread
                    bcs       ReadError
                    leay      512,y
                    leax      1,x
                    deca
                    bne       loop@

* Check if the loaded boot record is good
                    ldd       $2600
                    cmpd      #$4F53              /OS/
                    beq       goodbootrec@
                    leax      BadBootRecStr,pcr
                    bsr       StrOut
                    bra       Crash
goodbootrec@

* Now do a MSD Reset Recovery to reset all the data flags
* so F$Boot can make a safe assumption on the data flag state
                    lbsr      MSDResetRecovery

                    ldx       #USBMSDDeviceTable
                    ldd       DT.USBEndpointIn,x
                    asla                          Shift both up into the top nibble for later ANDing with the PID
                    asla
                    asla
                    asla
                    aslb
                    aslb
                    aslb
                    aslb
                    ora       #CH376_DEF_USB_PID_IN
                    orb       #CH376_DEF_USB_PID_OUT
* Now check if the L2 module is in the place we expect
* If so, poke into place.
                    ldx       $28fb
                    cmpx      #$4C32              /L2/
                    bne       l1go@
* Update the endpoints for loading from the F$Boot module
* IMPORTANT: Module CRC checking MUST be OFF in the init module
* as poking this data into that module will invalidate the
* checksum!
                    std       $28fb
                    #std      USBEndpoints
                    bra       boot@
l1go@
* Otherwise, poke into just before the boot record for L1 module.
                    std       $25fe
* Now go boot!
boot@               leax      BootingStr,pcr
                    bsr       StrOut
* and jump to $2602 to start the rel module
                    lbsr      Delay
                    lbsr      Delay
                    jmp       $2602

ReadError           leax      ReadErrStr,pcr
                    bsr       StrOut
* Fall through to Crash

Crash               lda       #'*
                    jsr       [$A002]
                    tfr       b,a
                    jsr       [$A002]
self@               bra       self@

* output string with last character high bit set
StrOut              pshs      x,d
loop@               lda       ,x+
                    pshs      cc
                    anda      #$7F
                    jsr       [$A002]
                    puls      cc
                    bpl       loop@
                    lda       #$0D
                    jsr       [$A002]
                    puls      x,d,pc

CardInitStr         fcs       "INITIALIZING CARD..."
DoneInitStr         fcs       "DONE INITIALIZING."
WaitDeviceStr       fcs       "WAITING FOR DEVICE PLUGIN."
InitErrorStr        fcs       "ERROR INITIALIZING CARD."
MSDInitErrStr       fcs       "ERROR LOADING DEVICE DRIVER."
ReadErrStr          fcs       "ERROR READING DEVICE."
NoBootDevStr        fcs       "NO BOOT DEVICE FOUND."
BadBootRecStr       fcs       "NO BOOT RECORD FOUND."
FoundDeviceStr      fcs       "FOUND DEVICE."
PollHubStr          fcs       "POLLING HUBS..."
LoadBTStr           fcs       "LOADING BOOT TRACK..."
BootingStr          fcs       "BOOTING..."

                    include   usb.asm
                    include   usbmsd.asm
end
                    end       start

