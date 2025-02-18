********************************************************************
* usbprinter.asm - USB Printer Driver
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*  0/0     2025/02/17  Don Barber

                    nam       usbprinter
                    ttl       USB Printer Driver

                    ifp1
                    use       defsfile
                    use       usb.d
                    endc

tylg                set       Drivr+Objct
atrv                set       ReEnt+Rev
rev                 set       $00
edition             set       0

* USB Printer Device Table Entries
                    org       0
P.USBDeviceID       rmb       1
P.USBEndpointIn     rmb       1
P.USBEndpointOut    rmb       1
P.USBMaxPacketSizeIn rmb      1
P.USBMaxPacketSizeOut rmb     1
P.USBDataFlag       rmb       1
P.USBInterfaceNum   rmb       1
USBPrinterDevSize   equ       .

* Device memory area: offset from U
                    org       V.SCF               V.SCF: free memory for driver to use
V.PAR               rmb       1                   1=space, 2=mark parity
V.BIT               rmb       1                   0=7, 1=8 bits
V.STP               rmb       1                   0=1 stop bit, 1=2 stop bits
V.COM               rmb       2                   Com Status baud/parity (=Y from SS.Comst Set/GetStt
V.COL               rmb       1                   columns
V.ROW               rmb       1                   rows
V.WAIT              rmb       2                   wait count for baud rate?
V.TRY               rmb       2                   number of re-tries if printer is busy
V.RTRY              rmb       1                   low nibble of parity=high byte of number of retries
V.BUFF              rmb       $80                 room for 128 blocked processes
USBPrinterDevTable  rmb       USBPrinterDevSize
size                equ       .

                    mod       eom,name,tylg,atrv,start,size

                    fcb       READ.+WRITE.

name                fcs       /usbprinter/
                    fcb       edition             
usbmanname          fcs       /usbman/            

USBPrinterDevMatch  fdb       $0000               0 VendorId
                    fdb       $0000               2 Mask
                    fdb       $0000               4 ProductId
                    fdb       $0000               6 Mask
                    fcb       $00                 8 DeviceClass
                    fcb       $00                 DeviceSubClass
                    fcb       $00                 10 ClassMask
                    fcb       $00                 SubClassMask
                    fcb       $00                 12 DeviceProtocol
                    fcb       $00                 13 Mask
                    fcb       $07                 14 InterfaceClass       Printer Base Class
                    fcb       $01                 InterfaceSubClass       Printers
                    fcb       $FF                 16 ClassMask
                    fcb       $FF                 SubClassMask
                    fcb       $01                 18 InterfaceProtocol    Unidirectional
                    fcb       $FF                 19 Mask

start               lbra      Init
                    lbra      Read
                    lbra      Write
                    lbra      GetStt
                    lbra      SetStt

* Term
*
* Entry:
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
Term                equ       *
                    clrb
                    rts

* Init
*
* Entry:
*    Y  = address of device descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
Init                pshs      u
                    leau      USBPrinterDevTable,u
                    ldx       #USBPrinterDevSize
                    clra
clrloop@            sta       ,u+
                    leax      -1,x
                    bne       clrloop@
                    puls      u
                    ldd       <IT.COL,y           get number of columns/rows
                    std       <V.COL,u            save it in statics
                    ldd       <IT.PAR,y           parity and baud rate
                    lbsr      L0138               setup parity/baud in device memory
* clear out buffer
                    leax      V.BUFF,u            room for 128 blocked processes
                    ldb       #128
I010                clr       ,x+                 we're more concerned with room
                    decb                          than with speed, so we don't use TFM
                    bne       I010
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
                    leax      USBPrinterDevMatch,pcr
                    stx       ,s
                    stu       2,s
                    leax      USBPrinterProbe,pcr
                    stx       4,s
                    leax      USBPrinterDisconnect,pcr
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
USBPrinterProbe     pshs      d,x,u
* Find empty entry in Printer device table
                    leau      USBPrinterDevTable,u
                    tst       ,u
                    bne       error@             Already have a printer
* Store into Device Entry
                    lda       USBInterfaceDeviceId,y
                    sta       P.USBDeviceId,u    device id
                    lda       USBInterfaceNum,y
                    sta       P.USBInterfaceNum,u interfacenum
                    clr       P.USBDataFlag,u    data0/data1 flag
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
                    sta       P.USBEndpointIn,u  store endpoint in table
                    bsr       FindMaxPacket
                    stb       P.USBMaxPacketSizeIn,u store MaxPacketSizeIn
                    bra       merge@
* record EndpointOut here
endpout@            sta       P.USBEndpointOut,u store endpoint in table
                    bsr       FindMaxPacket
                    stb       P.USBMaxPacketSizeOut,u store MaxPacketSizeOut
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

* X is pointer to the USB Device Table Entry
* Y is pointer to the USB Interface Table Entry
* U is memory location for this driver
USBPrinterDisconnect pshs      d,u
                    lda       ,x
                    leau      USBPrinterDevTable,u
                    cmpa      P.USBDeviceId,u
                    bne       error@              did not find entry
                    ldb       #USBPrinterDevSize
                    clra
loop1@              sta       ,u+
                    decb
                    bne       loop1@
                    bra       finish@
error@              comb
finish@             puls      u,d,pc


* Write
*
* Entry:
*    A  = character to write
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
Write               equ       *
                    leax      V.BUFF,u            point to the buffer
                    ldb       V.BUSY,u            get my process number
                    tst       ,x                  get allowed process number
                    bne       W010                if not zero, else
                    stb       ,x                  I'm the only one allowed to use it

W010                cmpb      ,x                  am I allowed to use /p?
                    beq       WriteChar           if yes, go write a character

***************************************************************
* WARNING: If more than 128 processes try to use the printer,
* this will CRASH BADLY.  Since Level II on the Coco is limited
* to 32 processes, I don't think it's a problem.
***************************************************************

W020                tst       ,x+                 if not, find the first non-zero entry
                    bne       W020
                    stb       -1,x                and save my process number at the first zero
                    pshs      a
                    lda       V.BUFF,u            process that's allowed to use /p
                    sta       V.BUSY,u            make it the busy one
                    ldb       #S$Wake             wake it up
                    os9       F$Send              send a signal to wake it
                    ifne      H6309
                    tfr       0,x
                    else
                    ldx       #$0000
                    endc
                    os9       F$Sleep             and go to sleep forever
                    puls      a                   restore character to be sent, and continue
*write character in A to printer
WriteChar         tst         P.USBDeviceId,u     error if no printer found yet
                  beq         error@
                  IFGT    Level-1
                    ldy       <D.USBManSubAddr
                  ELSE
                    ldy       >D.USBManSubAddr
                  ENDC
                    leau      USBPrinterDevTable,u
* Idea: check if USBEndpointIn is populated, and if so
* then issue a GET_PORT_STATUS for the printer to get current status
* and error out if not ready.
                    pshs      a
                    tfr       s,x
                    leas      -8,s
                    stx       USBOTS.BufferPtr,s
                    ldx       #1
                    stx       USBOTS.BufferLength,s
                    lda       P.USBDeviceId,u
                    ldb       P.USBEndpointOut,u
                    std       USBOTS.DeviceId,s
                    lda       P.USBDataFlag,u
                    sta       USBOTS.DataFlag,s
                    lda       P.USBMaxPacketSizeOut,u
                    sta       USBOTS.MaxPacketSize,s
                    tfr       s,x
                    jsr       USBOutTransfer,y
                    pshs      cc
                    lda       1+USBOTS.DataFlag,s
                    sta       P.USBDataFlag,u
                    puls      cc
                    leas      9,s
                    bcc       finish@
error@              ldb       #E$NotRdy
finish@             rts

* GetStat
*
* Entry:
*    A  = function code
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
GetStt              cmpa      #SS.EOF             end of file?
                    bne       L0112
                    clrb                          if so, exit with no error
                    rts

L0112               ldx       PD.RGS,y
                    cmpa      #SS.ScSiz
                    beq       L0123
                    cmpa      #SS.ComSt
                    bne       L0173
                    ldd       <V.COM,u            get Com status
                    std       R$Y,x
                    clrb
                    rts

* get screen size GetStt
L0123               clra
                    ldb       <V.COL,u
                    std       R$X,x
                    ldb       <V.ROW,u
                    std       R$Y,x
                    clrb
                    rts

* SetStat
*
* Entry:
*    A  = function code
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
SetStt              cmpa      #SS.ComSt
                    bne       Close               if not, check if it's a close
                    ldx       PD.RGS,y
                    ldd       R$Y,x
* A = Parity byte
* B = baud rate
L0138               std       <V.COM,u            save parity, baud rate in com status
                    ifne      H6309
                    clrd
                    else
                    clra
                    clrb
                    endc
                    std       <V.PAR,u
                    sta       <V.STP,u
                    ldd       <V.COM,u
                    tstb
                    bpl       L014C
                    inc       <V.STP,u            do 2 stop bits
L014C               bitb      #$40                make sure the bit is zero
                    bne       Read
                    bitb      #$20                0=8, 1=7 bits
                    beq       L0157
                    inc       <V.BIT,u
L0157               bita      #$20
                    beq       L0169               if=0, no parity
                    bita      #$80
                    beq       Read                if high bit set (only for ACIA devices), error out
                    inc       <V.PAR,u            parity
                    bita      #$40
                    bne       L0169               1=space,
                    inc       <V.PAR,u            2=mark parity
L0169               anda      #$0F
                    sta       <V.RTRY,u
                    rts

Read                comb
                    ldb       #E$BMode
                    rts

L0173               comb
                    ldb       #E$UnkSVc
                    rts

Close               cmpa      #SS.Close           close the device?
                    bne       L0173
                    leax      V.BUFF,u            point to blocked process buffer

C010                lda       1,x                 get next process number
                    sta       ,x+
                    bne       C010                do until we have a zero byte

                    lda       V.BUFF,u            get the first process in the queue
                    beq       C020                if none left
                    ldb       #S$Wake             wake up signal
                    os9       F$Send              re-start the blocked process

C020                clrb
                    rts

                    emod
eom                 equ       *
                    end
