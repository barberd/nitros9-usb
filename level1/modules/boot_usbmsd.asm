********************************************************************
* Boot - NitrOS-9 Boot from USB Mass Storage
* Provides HWInit, HWTerm, HWRead which are called by code in
* "use"d boot_common.asm
*
* This module assumes that the USB card has been initialized,
* any needed USB Hubs enumerated and ports enabled, the mass storage device
* configured with the CH376 chip set to communicate with the 
* USB Device ID.
* The bootstrapper needs to somehow tell this module the endpoints to 
* communicate, as the code to load the interface descriptors is quite lengthy
* and would not fit in this module.
* Level 1:
* The endpoints are loaded into memory locations $25FE (endpoint in) and
* $25FF (endpoint out).
* Level 2:
* The bootstrap program sets endpoint constants in memory at offsets:
* EndpointIn $1CB EndpointOut $1CC.
* On a standard coco level2 boot this module is loaded starting at $2730 so
* these bytes end up being at 0x28fb and 0x28fc, respectively.
* (rel moves the module to $ee30 during the boot process)
* Modifying the module in memory like this requires that CRC checksums 
* to NOT be enabled in the init module.
*
* This module also assumes the mass storage device:
*   Can handle 64 bytes transfered in a USB packet.
*   Uses 512 byte sectors (most anything except CDROMS).
* These assumptions shorten all this code enough to 
* allow the bootfile (also with rel and krn) 
* to fit into $1200 bytes. This in turn allows a 
* fairly standard os9 two-stage boot, working around the need for 
* a different boot sequence such as those used for the f256 and cocoboot.
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   1      2025/02/02  Don Barber
* Created.
*

                    nam       Boot
                    ttl       USB Mass Storage Boot Modules

                    ifp1
                    use       defsfile
                    use       usb.d
                    use       os9.d
                    use       ch376.d
                    endc

* Common booter-required defines
LSN24BIT            equ       1
FLOPPY              equ       0

                   ifgt       Level-1
EndpointIn          equ       Address
EndpointOut         equ       Address+1
                   else
EndpointIn          equ       $25FE
EndpointOut         equ       $25FF
                   endc

* NOTE: these are U-stack offsets, not DP
seglist             rmb       2                   pointer to segment list
blockloc            rmb       2                   pointer to memory requested
blockimg            rmb       2                   duplicate of the above
bootsize            rmb       2                   size in bytes
LSN0Ptr             rmb       2                   In memory LSN0 pointer
* USB Bulk Transport Command Block CBW
V.dCBWSignature          RMB  4
V.dCBWTag                RMB  4
V.dCBWDataTransferLength RMB  4
V.bmCBWFlags             RMB  1
V.bCBWLUN                RMB  1
V.bCBWCBLength           RMB  1
* Low-level driver static memory area
* SCSI Command Packet
* SCSI packet length is 10 bytes
V.SCSICMD      RMB       1
V.SCSILUN      RMB       1
V.SCSIPrm0     RMB       1
V.SCSIPrm1     RMB       1
V.SCSIPrm2     RMB       1
V.SCSIPrm3     RMB       1
V.SCSIPrm4     RMB       1
V.SCSIPrm5     RMB       1
V.SCSIPrm6     RMB       1
V.SCSIPrm7     RMB       1
               RMB       6
CmdPktLen      EQU       .-V.dCBWSignature
PhysSect       RMB       3
DataFlag       RMB       1
TfrBufAddr     RMB       2
localsize           equ       .-V.dCBWSignature
size                equ       .

tylg                set       Systm+Objct
atrv                set       ReEnt+rev
rev                 set       $00
edition             set       0

                    mod       eom,name,tylg,atrv,start,size

name                fcs       /Boot/
                    fcb       edition


*--------------------------------------------------------------------------
* HWInit - Initialize the device
*
*    Entry:
*       Y  = hardware address
*
*    Exit:
*       Carry Clear = OK, Set = Error
*       B  = error (Carry Set)
*
HWInit              * Set up USB and SCSI packet structure
                   ifeq   Level-1
                    lda   #'I
                    sta   $FF68
                   endc
                    clra
                    ldx   #localsize
                    leay  V.dCBWSignature,u
clrloop@            sta   ,y+
                    leax  -1,x
                    bne   clrloop@
                    ldd   #$5553
                    std   V.dCBWSignature,u
                    ldd   #$4243 
                    std   V.dCBWSignature+2,u
                    ldb   #$02                    512 in USB little endian
                    stb   V.dCBWDataTransferLength+1,u
                    lda   #$28                SCSI Read(10) command
                    sta   V.SCSICMD,u         put SCSI Read Command
                    lda   #1                  One Block
                    sta   V.SCSIPrm6,u
                    lda   #$80
                    sta   V.bmCBWFlags,u
                    lda   #16
                    sta   V.bCBWCBLength,u
                    sta   V.SCSIPrm1,u        Invalidate Cache
                    tfr   u,x                 save u
                    ldd   #512
                    os9   F$SRqMem
                    exg   x,u                 restore u
                    stx   TfrBufAddr,u
                    rts

*--------------------------------------------------------------------------
* HWTerm - Terminate the device
*
*    Entry:
*       Y  = hardware address
*
*    Exit:
*       Carry Clear = OK, Set = Error
*       B = error (Carry Set)
*
HWTerm              pshs   u 
                    ldu    TfrBufAddr,u
                    beq    finish@
                    ldd    #512
                    os9    F$SRtMem
finish@             puls   u,pc


***************************************************************************
                    use       boot_common.asm
***************************************************************************

*
* HWRead - Read a 256 byte sector from the device
*
*    Entry:
*       Y  = hardware address
*       B  = bits 23-16 of LSN
*       X  = bits 15-0  of LSN
*       blockloc,u = where to load the 256 byte sector
*
*    Exit:
*       X  = ptr to data (i.e. ptr in blockloc,u)
*       Carry Clear = OK, Set = Error
*
HWRead              * Convert from logical to physical first
                    lsrb
                    pshs      b
                    tfr       x,d
                    rora
                    rorb
                    tfr       d,x
                    puls      b
                    * Store 'odd' bit just rotated off on stack
                    pshs      cc
                    cmpb      V.SCSIPrm1,u
                    bne       fillcache@
                    cmpx      V.SCSIPrm2,u
                    beq       skipread@
fillcache@          stb       V.SCSIPrm1,u        ; high byte LSN
                    stx       V.SCSIPrm2,u        ; low word LSN
                    inc       V.dCBWTag,u
                    * Issue usb/scsi command
                    leay      V.dCBWSignature,u
                    ldx       #CmdPktLen
                    bsr       TfrOut
                    * Read result into buffer
                    ldy       TfrBufAddr,u
                    ldx       #$0200                512 bytes
                    bsr       TfrIn
                    * Now read in the Command Status Wrapper (CSW)
                    * No room in bootloader to check results for validity
                    leas      -13,s
                    tfr       s,y
                    ldx       #$000D              CSW is 13 bytes
                    bsr       TfrIn
                    ldb       12,s                USBMSDBCSWStatus
                    leas      13,s
                   IFGT       Level-1
                    lbne      Crash
                   ELSE 
                    lbne      StrCrash
                   ENDC
                    * use saved 'odd' bit on which half to transfer.
skipread@           ldx       blockloc,u          get address of buffer to fill
                    ldy       TfrBufAddr,u
                    puls      cc
                    bcc       startread@
                    leay      256,y               if odd, jump 256 bytes forward
startread@          pshs      x,u
                    ldb       #256/2 (reading words)
rdLoop              ldu       ,y++
                    stu       ,x++                store to buffer
                    decb                          decrement word counter
                    bne       rdLoop              loop until done
                    puls      x,u                 restore X and U
rdExit              rts


* Input: X number of bytes to write. Y is pointer to buffer.
* This assumes never have to send more than 64 bytes at a time.
* This is a safe assumption for read-only F$Boot purposes as the largest
* thing should be the command block wrapper (31 bytes)
TfrOut              lda       #CH376_WR_HOST_DATA 
                    sta       CH376_CMDREG
                    tfr       x,d
skip@               stb       CH376_DATAREG
loop@               lda       ,y+
                    sta       CH376_DATAREG
                    decb
                    bne       loop@
                    ldb       #CH376_ISSUE_TKN_X
                    stb       CH376_CMDREG
                    ldb       DataFlag,u
                    stb       CH376_DATAREG
                    eorb      #$40
                    stb       DataFlag,u
                   ifgt       Level-1
                    ldb       EndpointOut,pcr
                   else
                    ldb       EndpointOut
                    stb       $FF68
                   endc
                    stb       CH376_DATAREG
                    bsr       WaitInt
                    rts   

* Input: X number of bytes to read. Y is pointer to buffer.
TfrIn               ldb       #CH376_ISSUE_TKN_X
                    stb       CH376_CMDREG
                    ldb       DataFlag,u
                    stb       CH376_DATAREG
                    eorb      #$80
                    stb       DataFlag,u
                   ifgt       Level-1
                    ldb       EndpointIn,pcr
                   else
                    ldb       EndpointIn
                    stb       $FF68
                   endc
                    stb       CH376_DATAREG
                    bsr       WaitInt
skip@               lda       #CH376_RD_USB_DATA0
                    sta       CH376_CMDREG
                    clra
                    ldb       CH376_DATAREG
loop@               lda       CH376_DATAREG 
                    sta       ,y+
                    leax      -1,x
                    decb
                    bne       loop@
                    cmpx      #0
                    bne       TfrIn
                    rts   

WaitInt             ldb       CH376_FLAGREG
                    bmi       WaitInt
                    ldb       #CH376_GET_STATUS
                    stb       CH376_CMDREG
                    ldb       CH376_DATAREG
                    cmpb      #CH376_USB_INT_SUCCESS
                    bne       Crash
                    ldb       $FF22              clear PIA interrupt
                    rts


                   IFEQ      Level-1
StrCrash            leas     1,s
                   ENDC
Crash
                   IFGT      Level-1
                    jmp       <D.Crash
                   ELSE
                    comb
                    rts
                   ENDC
*--------------------------------------------------------------------------

                    ifgt      Level-1
* L2 kernel file is composed of rel, boot, krn. The size of each of these
* is controlled with filler, so that (after relocation):
* rel  starts at $ED00 and is $130 bytes in size
* boot starts at $EE30 and is $1D0 bytes in size
* krn  starts at $F000 and ends at $FEFF (there is no 'emod' at the end
*      of krn and so there are no module-end boilerplate bytes)
*
* Filler to get to a total size of $1D0. 3, 2, represent bytes after
* the filler: the end boilerplate for the module and fdb.
Filler              fill      $39,$1D0-3-2-*
                    endc

* This is set to ascii L2 for the bootloader to check if its safe
* to overwrite here. If not, the bootloader pokes into $25FE instead,
* which is used in L1, but not in L2.
Address             fdb       $4C32               /L2/
                    emod
eom                 equ       *
                    end
