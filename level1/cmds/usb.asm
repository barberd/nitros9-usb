********************************************************************
* Usb - Control USB Subsystem
*
* Right now, this just calls F$USBPollHubs system call, telling the USB Manager
* to poll the hubs for added or removed devices.
*
* Conceptually, future versions might list current devices, registered drivers, etc.
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   0      2025/02/2   Don Barber

                    nam       usb
                    ttl       USB Subsystem Controller

                  IFP1
                    use       defsfile
                  ENDC

tylg                set       Prgrm+Objct
atrv                set       ReEnt+rev
rev                 set       $00
edition             set       0

                    mod       eom,name,tylg,atrv,start,size

                    org       0
size                equ       .

name                fcs       /Usb/
                    fcb       edition

start               os9       F$USBPollHubs
                    bcs       Exit
                    clrb
Exit                os9       F$Exit

                    emod
eom                 equ       *
                    end

