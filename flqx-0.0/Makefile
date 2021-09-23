# -s,Strip all symbols
# -x,Discard all local symbols
# -M,Print map file on standard output
# -m,Set emulation
# -Ttext,Set address of .text section
# --oformat,Specify target of output file. flq:just keep code
LDFLAGS16 =-s -x -Ttext 0 --oformat=binary
LDFLAGS32 =-s -x -M -m elf_i386 -Ttext 0 --oformat=binary 

Image: boot.s
	# for 16
	as boot.s -o boot.o
	ld $(LDFLAGS16) boot.o -o boot
	# for 32
	as --32 head.s -o head.o
	ld $(LDFLAGS32) head.o -o head > System.map
	# create floppy
	dd if=/dev/zero of=floppy.img bs=512 count=2880
	# make bootimg
	dd if=boot of=floppy.img
	dd bs=512 if=head of=floppy.img seek=1
	sync

disk: Image
	dd bs=8192 if=Image of=/dev/fd0
	sync;sync;sync

clean:
	rm -f Image System.map core boot head *.o system
