include $(NITROS9DIR)/rules.mak


all:	usbboot.bin usbboot.dsk usbboot.rom

usbboot.bin: usbboot.asm
	$(ASBIN) $(ASOUT)$@ $<

usbboot.rom: usbboot.asm
	$(ASROM) $(ASOUT)$@ $< -DCODESTART=49152

usbboot.dsk: usbboot.bin
	decb dskini $@
	decb copy -2 -b usbboot.bin $@,USBBOOT.BIN
	decb copy -0 -a -t AUTOEXEC.BAS $@,AUTOEXEC.BAS

clean:	
	$(RM) usbboot.bin usbboot.dsk usbboot.rom
