          IFNE      USB.D-1
USB.D   SET       1
********************************************************************
* usb.d - USB Definitions File
*
* $Id$
*
* Ed.    Comments                        Who            YY/MM/DD
* ------------------------------------------------------------------
*   1    Started                         Don Barber     25/01/17

         nam   usb.d
         ttl   USB Definitions File

* Offsets for module jump table
                         org   $0
USBInit                  rmb   3
USBRegisterDriver        rmb   3
USBDeregisterDriver      rmb   3
USBInTransfer            rmb   3
USBOutTransfer           rmb   3
USBControlTransfer       rmb   3
USBClearStall            rmb   3
USBSetConfiguration      rmb   3
USBResetDevice           rmb   3
USBPollHubs              rmb   3

* Offsets for Device Table
                              org   $0
USBDeviceId                   rmb   1
USBDeviceHub                  rmb   1
USBDevicePort                 rmb   1
USBDeviceMaxPacketSize        rmb   1
USBDeviceVendorId             rmb   2
USBDeviceProductId            rmb   2
USBDeviceClass                rmb   1
USBDeviceSubClass             rmb   1
USBDeviceProtocol             rmb   1
USBDeviceConfigurationWLength rmb   2
USBDeviceRecLength       equ   .

* Offsets for Interface Table
                              org   $0
USBInterfaceDeviceId          rmb   1
USBInterfaceNum               rmb   1
USBInterfaceDriverRecord      rmb   2
USBInterfaceClass             rmb   1
USBInterfaceSubClass          rmb   1
USBInterfaceProtocol          rmb   1
USBInterfaceRecLength         equ .

* Offsets for Driver Table
                              org   $0
USBDriverDevMatchPtr          rmb   2
USBDriverMemoryPtr            rmb   2
USBDriverProbeFuncPtr         rmb   2
USBDriverDisconnectFuncPtr    rmb   2
USBDriverRecLength            equ   .

* Offsets for Device Matching Table
                                  org   $0
USBDevMatchVendorId               rmb 2
USBDevMatchVendorIdMask           rmb 2
USBDevMatchProductId              rmb 2
USBDevMatchProductIdMask          rmb 2
USBDevMatchDeviceClass            rmb 1
USBDevMatchDeviceSubClass         rmb 1
USBDevMatchDeviceClassMask        rmb 1
USBDevMatchDeviceSubClassMask     rmb 1
USBDevMatchDeviceProtocol         rmb 1
USBDevMatchDeviceProtocolMask     rmb 1
USBDevMatchInterfaceClass         rmb 1
USBDevMatchInterfaceSubClass      rmb 1
USBDevMatchInterfaceClassMask     rmb 1
USBDevMatchInterfaceSubClassMask  rmb 1
USBDevMatchInterfaceProtocol      rmb 1
USBDevMatchInterfaceProtocolMask  rmb 1

* Offsets common to all USB Descriptors
                              org   $0
USBDescriptorLength           rmb   1
USBDescriptorType             rmb   1

* Offsets for USB Device Descriptor (from USB spec)
	                      org   $2
USBDDBcdUSB                   rmb   2
USBDDBDeviceClass             rmb   1
USBDDBDeviceSubClass          rmb   1
USBDDBDeviceProtocol          rmb   1
USBDDBMaxPacketSize0          rmb   1
USBDDIdVendor                 rmb   2
USBDDIdProduct                rmb   2
USBDDBcdDevice                rmb   2
USBDDIManufacturer            rmb   1
USBDDIProduct                 rmb   1
USBDDISerialNumber            rmb   1
USBDDBNumConfigurations       rmb   1

* Offsets for USB Configuration Descriptor (from USB spec)
                              org   $2
USBCDWTotalLength             rmb   2
USBCDBNumInterfaces           rmb   1
USBCDBConfigurationValue      rmb   1
USBCDIConfiguration           rmb   1
USBCDBmAttributes             rmb   1
USBCDBMaxPower                rmb   1

* Offsets for USB Interface Descriptor, type $04 (from USB spec)
                              org   $2
USBIDBInterfaceNumber         rmb   1
USBIDBAlternateSetting        rmb   1
USBIDBNumEndpoints            rmb   1
USBIDBInterfaceClass          rmb   1
USBIDBInterfaceSubClass       rmb   1
USBIDBInterfaceProtocol       rmb   1
USBIDIInterface               rmb   1

* Offsets for USB Endpoint Descriptor, type $05 (from USB spec)
                              org   $2
USBEDBEndpointAddress         rmb   1
USBEDBmAttributes             rmb   1
USBEDWMaxPacketSize           rmb   2
USBEDBInterval                rmb   1

* Offsets for InTransfer Struct
                              org   $0
USBITS.BufferPtr              rmb   2
USBITS.BufferLength           rmb   2
USBITS.DeviceId               rmb   1
USBITS.EndpointId             rmb   1
USBITS.DataFlag               rmb   1
USBITS.MaxPacketSize          rmb   1
USBITS.NakFlag                rmb   1

* Offsets for OutTransfer Struct
                              org   $0
USBOTS.BufferPtr              rmb   2
USBOTS.BufferLength           rmb   2
USBOTS.DeviceId               rmb   1
USBOTS.EndpointId             rmb   1
USBOTS.DataFlag               rmb   1
USBOTS.MaxPacketSize          rmb   1

* Offsets for ControlTransfer Struct
                              org   $0
USBCTS.SetupPktPtr            rmb   2
USBCTS.BufferPtr              rmb   2
USBCTS.DeviceId               rmb   1

                ENDC



