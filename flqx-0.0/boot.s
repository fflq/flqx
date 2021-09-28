#flqx v0.0 boot.s
#利用bios中断把内核代码(head.s)加载到内存0x10000处，
#然后再移动到0处(因为加载用到了0处的bios中断,加载完才能覆盖)
#最后进入保护模式,并跳转到0(head代码处)开始继续运行
.code16


# 引导扇区(本程序)被BIOS自动加载到内存0x7c00处
BOOT_SEG 	= 0x07c0	
# 内核代码head.s先被加载到10000h,后移到0处
OS_LOAD_SEG	= 0x1000	
OS_SEG	= 0x0	
# 离0最近使用中地址是0x7c00,即31KB,即62扇区,需故避免覆盖,OS_SECTORS<62
# 该数限定了内核代码的长度,这里20扇区是10KB
OS_SECTORS	= 20	


.section .text
.global _start
_start:

## 欢迎信息
welcome:
	movb	$0xe, %ah
	movb	$'F', %al
	int		$0x10


## 初始所有段寄存器为0,先设置下
init:
	# *如下报错，Error: operand type mismatch for `ljmp'
	#ljmp	$BOOT_SEG, go
	# *如下并没有修改cs值,仍为0,导致最终跳到f000:fff0而不是head.s的0处执行
	#jmp	go
	#[0x000000007c52] 0000:0000000000007c52 (unk. ctxt): jmpf 0x0008:0000          ; ea00000800
	#[0x0000fffffff0] f000:fff0 (unk. ctxt): jmpf 0xf000:e05b          ; ea5be000f0

	# 1.跳转到0x7c0:go,是为了设置cs
	ljmp	$BOOT_SEG, $go
go: 
	# 2.用cs值设置ds,ss=0x7c0
	movw	%cs, %ax
	movw	$BOOT_SEG, %ax
	movw	%ax, %ds
	movw	%ax, %ss
	# 设置临时栈指针sp,sp值不断减小,其值需大于程序末端并留有一定空间
	# 0x400到0有1KB大小,而0x7c00往上0.5KB是此boot.s代码,故栈是0x7d00往下0.5KB
	movw	$0x400, %sp


## 使用bios中断int0x13+ah2,加载内核代码到内存0x10000处
load_oscode:
	# 设置磁盘位置:dl驱动器号,dh磁头号,(cl[7:6]+ch)10位磁道号,cl[5:0]扇区号(从1记)
	# 驱动器号0,磁头号0,磁道号0,扇区号2(从1记,即第二个扇区)
	movw	$0, %dx
	movw	$2, %cx
	# 设置es:bx=0x1000:0为读入缓冲区位置
	movw	$OS_LOAD_SEG, %ax
	movw	%ax, %es
	xorw	%bx, %bx
	# ah=2读扇区功能号,al读扇区数
	movb	$2, %ah
	movb	$OS_SECTORS, %al
	int		$0x13
	# int13-2失败时置位cf
	jnc move_oscode
die:
	jmp die


## 把内核代码从0x10000移到0,共移动OS_SECTORS*0.5KB 
move_oscode:
	cli
	# 设置移动源目的ds:si=(0x1000:0)->es:di(0:0)
	movw	$OS_LOAD_SEG, %ax
	movw	%ax, %ds
	movw	$OS_SEG, %ax
	movw	%ax, %es
	xorw	%si, %si
	xorw	%di, %di
	# movw每次移动2B,一个扇区256次,OS_SECTORS个扇区,故256*OS_SECTORSa次,or(2^8*OS_SECTORS)
	movw	$OS_SECTORS, %cx
	shl		$8, %cx
	rep	movsw 


## 加载IDT和GDT基地址寄存器IDTR和GDTR
set_ldtr_gdtr:
	# 让ds重新指向当前0x7c0
	movw	%cs, %ax
	movw	%ax, %ds
	# 加载IDTR,GDTR.6字节操作数:2字节表长度,4字节线性基地址
	lidt	idt_48
	lgdt	gdt_48


## 设置控制寄存器CR0(即机器状态字),进入保护模式.
set_cr0:
	# 在CR0中设置保护模式标志PE(位0)
	movw	$1, %ax
	lmsw	%ax
	# 跳转至段选择符指定段,偏移0
	# 注意此时段值已是段选择符,该段的线性基地址0
	# gdt段描述符项占8B,故8对应gdt表第2个段描述符,第1个不用
	ljmp	$8, $0


## 全局描述符GDT.包含3个段描述符,第1个不用,另2个是代码和数据段描述符,段描述符项占8字节
gdt:
	# 段描述符0不用
	.quad	0

	# 段描述符1,,8
	# 8MB - 段限长值(从0)=2047(2048*4KB=8MB)
	.word	0x07ff
	# 段基地址0
	.word	0
	# 代码段,可读/执行.9a对应:1(P存在),00(DPL特权级),1(S非系统段),,1010(TYPE代码段可读/执行)
	.word	0x9a00
	# 段属性颗粒度=4KB,80386
	.word	0x00c0

	# 段描述符2,,16
	.word	0x07ff
	.word	0
	# 数据段,可读写.92对应:1(P存在),00(DPL特权级),1(S非系统段),,0010(TYPE数据段可读写)
	.word	0x9200
	.word	0x00c0


## LIDT和LGDT的6字节操作数
idt_48:
	# IDT表长度0
	.word	0
	# IDT表的线性基地址0
	.word	0, 0

gdt_48:
	# GDT表长度是2048B,可容纳256个描述符项
	.word	0x7ff
	# GDT表的线性基地址在上面gdt位置,即0x7c0:gdt
	.word	0x7c00+gdt, 0


## 引导扇区有效标志.必须处于引导扇区最后2字节处
# or .=_start+510
.org 510
	#.byte 0x55, 0xaa,因为x86是小端,故word时如下
	.word 	0xaa55



