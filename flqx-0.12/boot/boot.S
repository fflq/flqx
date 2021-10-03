/*
   boot.S
   -
   boot.s被ROM BIOS加载到0x7c00(31KB)处执行.
   将自己移动到0x90000(576KB)处,并跳转到那执行.
   使用bios中断将setup.s加载到自己后面0x90200(576.5KB)处(0x90000后一个扇区刚好).
   将system加载到0x10000(64KB)处.
   -
   内核长度限制512KB,因为起始从0x10000到boot的0x90000前,共0x80000=512KB.
 */


// 该头文件定义了一些常数符号
#include <linux/config.h>

// boot代码初始加载所在段
OLD_BOOTSEG  = 0x07c0			
// 移动boot到这,避开系统模块
BOOTSEG  = DEF_INITSEG			

// setup程序从0x90200处开始
SETUPSEG = DEF_SETUPSEG			
// setup.s占用的扇区数
SETUP_LEN = 4				

// SYS_SIZE是要加载的内核长度,单位节16B.直接用于段加减
// 默认值0x3000即0x30000字节(196KB),最大取0x1000
SYS_SIZE = DEF_SYSSIZE
// 扇区数,SYS_LEN = SYS_SIZE*16/512 = SYS_SIZE/32
SYS_LEN	= SYS_SIZE/32
// system模块加载到0x10000(64KB)处
SYSSEG   = DEF_SYSSEG			
// system模块停止加载的段地址
ENDSEG   = SYSSEG + SYSSIZE		

/*
   ROOT_DEV和SWAP_DEV由build程序写入.
   Linus设置设备号0x0306表示,指定根文件系统设备是第2个硬盘的第1个分区.
   而0x0301(即0x01,0x03)就是第1个硬盘的第1个分区.
   这是linux老式硬盘设备号命名方式,linux0.95后使用与现在同的命名方法了.
   *设备号 = 主设备号*256 + 次设备号 (即 dev_no = (major<<8) + minor)
   *主设备号: 1-内存, 2-磁盘, 3-硬盘, 4-ttyx, 5-tty, 6-并行口, 7-非命名管道
   ! 0x300 - /dev/hd0 - 代表整个第1个硬盘
   ! 0x301 - /dev/hd1 - 第1个盘的第1个分区
   ! -
   ! 0x304 - /dev/hd4 - 第1个盘的第4个分区
   ! 0x305 - /dev/hd5 - 代表整个第2个硬盘
   ! 0x306 - /dev/hd6 - 第2个盘的第1个分区
   ! -
   ! 0x309 - /dev/hd9 - 第2个盘的第4个分区
*/
ROOT_DEV = 0
SWAP_DEV = 0


.code16
.section .text
.global bootsector_start
bootsector_start: 

// 此时在0x7c00
move_boot:
	movw	$OLD_BOOTSEG, %ax
	movw	%ax, %ds
	movw	$BOOTSEG, %ax
	movw	%ax, %es
	sub		$si, %si
	sub		$di, %di
	// 将boot程序从当前ds:si=0x7c0:0移到es:di=0x9000:0,共256字(512B,一扇区)
	rep		movw
	
	ljmp	$BOOTSEG, $init


/* 此时已在0x90000处
   ds = es = ss = 0x90000(当前基地址)
   sp = 0x200(boot一个扇区) + 4*0x200(setup四个扇区) + 堆栈大小 - 12(参数表长度)
   sp = 0xff00 - 12 = 0xfef4
   *驱动器参数表
   *对于多扇区读数超过默认磁盘参数表中最大扇区数时,很多bios不能识别.
   *故在内存中重建新的驱动器参数表.位置就在sp起往上,故sp栈底上要留12B建表.
   而我现在是2021没必要考虑,故
   sp = (1+4)*0x200 + 堆栈大小 = 0xa00+ = 0xffff
 */
init:
	movw	%cs, %ax
	movw	%ax, %ds
	movw	%ax, %es
	movw	%ax, %ss
	movw	$0xffff, %ax
	movw	%ax, %sp


// 用bios的int13将setup从第2扇区读4个扇区.0x9000:0x200
// 若出错则显示磁盘上出错扇区位置,并复位驱动器重试,没有退路.
/*
! INT 0x13读扇区使用调用参数设置如下:
! ah = 0x02 - 读磁盘扇区到内存, al = 需要读出的扇区数量
! ch = 磁道(柱面)号的低8位, cl = 开始扇区(位0-5),磁道号高2位(位6-7)
! dh = 磁头号, dl = 驱动器号(如果是硬盘则位7要置位)
! es:bx 指向数据缓冲区
! 如果出错则CF标志置位,ah中是出错码.
*/
load_setup:
	movw	$0x0200+SETUP_LEN, %ax
	movw	$0x0200, %bx
	movw	$2, %cx
	xorw	%dx, %dx
	call	read_it


/*
  利用BIOS int0x13功能8取磁盘驱动器的参数.实际取每磁道扇区数spt.并保存在spt处
  取磁盘驱动器参数int0x13调用格式和返回信息如下:
  ! ah = 0x08  dl = 驱动器号(如果是硬盘则要置位7为1)
  ! 返回信息:
  ! 如果出错则CF置位,并且ah = 状态码.
  ! ah = 0, al = 0, bl = 驱动器类型(AT/PS2)
  ! ch = 最大磁道号的低8位, cl = 每磁道最大扇区数(位0-5),最大磁道号高2位(位6-7)
  ! dh = 最大磁头数, dl = 驱动器数量
  ! es:di - 软驱磁盘参数表。
*/
get_spt:
	// 因为下面es会改变
	pushw	%es

	xorb	%dl, %dl
	movb	$0x8, %ah
	int	0x13

	// 假设spt不超过256,故只看低8位的ch
	movw	%cx, spt
	popw	%es



/*
   利用BIOS int0x10功能0x03和0x13来显示信息"'Loading...'+\r+\n",共12B
   ! *BIOS中断0x10功能号ah = 0x03,读光标位置.
   ! 输入:bh = 页号
   ! 返回:ch = 扫描开始线 cl = 扫描结束线 dh = 行号(0x00顶端) dl = 列号(0x00最左边)。
   ! *BIOS中断0x10功能号ah = 0x13,显示字符串.
   ! 输入:al = 放置光标的方式及规定属性. 0x01-表示使用bl中的属性值,光标停在字符串结尾处.
   ! bh = 显示页面号:bl = 字符属性:dh = 行号 dl = 列号. cx = 显示的字符串字符数.
   ! es:bp 此寄存器对指向要显示的字符串起始位置处.
*/
print_load_msg:
	// 返回光标位置在dx
	movb	$0x3, %ah
	xorb	%bh, %bh
	int 0x10

	// 9个字符,,页号0属性7(普通),,写字符串并移动光标到串结尾处
	mov		$12, %cx
	movw	0x7, %bx
	movw	$load_msg, %bp
	movw	$1301
	int	0x10



// 将system模块加载到0x10000(64KB)
load_system:
	movw	$SYSSEG, %ax
	movw	%ax, %es
	movw	$0x0200+SYS_LEN, %ax
	xorw	%bx, %bx
	movw	$2+SETUP_LEN, %cx
	xorw	%dx, %dx
	// es为输入参数
	call	read_it
	// 关闭驱动器马达,这样就可以知道驱动器的状态了
	call	kill_motor
	call	print_nl
	



/*
  检查要使用哪个根文件系统设备
  如果已经指定了设备(!=0),就直接使用给定的设备.
  否则就需要根据BIOS报告的spt来确定到底使用/dev/PS0(2,28),还是/dev/at0(2,8).
  *上面一行中两个设备文件的含义说明如下：
  在Linux中软驱的主设备号是2(参见第43行的注释),次设备号 = type*4 + nr.
  其中nr为0-3分别对应软驱A-D,type是软驱的类型(2 1.2MB或7 1.44MB等).
  因为7*4 + 0 = 28,所以/dev/PS0(2,28)指的是1.44MB A驱动器,其设备号是0x021c
  同理/dev/at0(2,8)指的是1.2MB A驱动器,其设备号是0x0208
  *下面root_dev定义在引导扇区509,510字节处,指根文件系统所在设备号.
  0x0306指第2个硬盘第1个分区.默认为0x0306是因为当时Linus.这个值自定.
  当编译内核时,你可以在Makefile文件中另行指定你自己的值,build会设置.
*/
check_root_dev:
	// 取508,509字节处根设备号判断是否被定义
	movw	root_dev, %ax 
	orw		%ax, %ax
	jne	root_dev_defined

	//如果spt=15则说明是1.2MB的驱动器;如果18则1.44MB软驱.因为是可引导驱动器,所以肯定是A驱.
	movw	spt, %bx
	// /dev/ps0,1.2MB
	movw	$0x0208, %ax
	cmpw	$15, %bx
	je	root_defined
	// /dev/PS0,1.44MB
	movw	$0x0208, %ax
	cmpw	$18, %bx
	je	root_defined

	// 如果都不一样,则死循环死机
root_dev_undefined:
	jmp	root_dev_undefined

root_dev_defined:
	// 将检查后的设备号保存到root_dev
	movw	%ax, root_dev


// 跳转到setup,0x9020:0
	ljmp	SETUPSEG, $0




// ************函数**********************************

/* 
  linux0.12读system包含:尽可能读整条磁道,进度信息,出错信息,复位重试等.
  me为了简单,还是如读setup
*/

// 默认0头0磁道读: al-读扇区数, cl-开始扇区的位0-5, es:bx读到位置
read_it:
	xorw	%dx, %dx
	xorb	%ch, %ch
	//movw	$2+SETUP_LEN, %cx
	movw	$0x0200, %bx
	//movw	$0x0200+SYS_LEN, %ax
	mowb	$0x2, %ah

	pusha

	int	0x13
	jnc	ok_load_setup

	// 屏幕光标回车
	pushw	%ax
	call	print_nl
	// ss:bp指向欲显示的字
	movw	%sp, %bp
	call	print_hex
	popw	%ax

	// 复位磁盘控制器重试
	xorb	%dl, %dl
	xorb	%ah, %ah
	int	0x13

	popa
	jmp	read_it

ok_read_it:
	ret








! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUP_LEN	! sectors read of current track
head:	.word 0			! current head
track:	.word 0			! current track

read_system:
	mov ax,es
	test ax,#0x0fff
die:	jne die			! es must be at 64kB boundary
	xor bx,bx		! bx is starting address within segment
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?
	jb ok1_read
	ret
ok1_read:
	seg cs
	mov ax,sectors
	sub ax,sread
	mov cx,ax
	shl cx,#9
	add cx,bx
	jnc ok2_read
	je ok2_read
	xor ax,ax
	sub ax,bx
	shr ax,#9
ok2_read:
	call read_track
	mov cx,ax
	add ax,sread
	seg cs
	cmp ax,sectors
	jne ok3_read
	mov ax,#1
	sub ax,head
	jne ok4_read
	inc track
ok4_read:
	mov head,ax
	xor ax,ax
ok3_read:
	mov sread,ax
	shl cx,#9
	add bx,cx
	jnc rp_read
	mov ax,es
	add ah,#0x10
	mov es,ax
	xor bx,bx
	jmp rp_read

read_track:
	pusha
	pusha	
	mov	ax, #0xe2e 	! loading... message 2e = .
	mov	bx, #7
 	int	0x10
	popa		

	mov dx,track
	mov cx,sread
	inc cx
	mov ch,dl
	mov dx,head
	mov dh,dl
	and dx,#0x0100
	mov ah,#2
	
	push	dx				! save for error dump
	push	cx
	push	bx
	push	ax

	int 0x13
	jc bad_rt
	add	sp, #8   	
	popa
	ret

bad_rt:	push	ax				! save error code
	call	print_all			! ah = error, al = read
	
	
	xor ah,ah
	xor dl,dl
	int 0x13
	

	add	sp, #10
	popa	
	jmp read_track

/*
 *	print_all is for debugging purposes.  
 *	It will print out all of the registers.  The assumption is that this is
 *	called from a routine, with a stack frame like
 *	dx 
 *	cx
 *	bx
 *	ax
 *	error
 *	ret <- sp
 *
*/
 
print_all:
	mov	cx, #5		! error code + 4 registers
	mov	bp, sp	

print_loop:
	push	cx		! save count left
	call	print_nl	! nl for readability
	jae	no_reg		! see if register name is needed
	
	mov	ax, #0xe05 + 0x41 - 1
	sub	al, cl
	int	0x10

	mov	al, #0x58 	! X
	int	0x10

	mov	al, #0x3a 	! :
	int	0x10

no_reg:
	add	bp, #2		! next register
	call	print_hex	! print it
	pop	cx
	loop	print_loop
	ret

print_nl:
	mov	ax, #0xe0d	! CR
	int	0x10
	mov	al, #0xa	! LF
	int 	0x10
	ret

/*
 *	print_hex is for debugging purposes, and prints the word
 *	pointed to by ss:bp in hexadecmial.
*/

print_hex:
	mov	cx, #4		! 4 hex digits
	mov	dx, (bp)	! load word into dx
print_digit:
	rol	dx, #4		! rotate so that lowest 4 bits are used
	mov	ah, #0xe	
	mov	al, dl		! mask off so we have only next nibble
	and	al, #0xf
	add	al, #0x30	! convert to 0 based digit, '0'
	cmp	al, #0x39	! check for overflow
	jbe	good_digit
	add	al, #0x41 - 0x30 - 0xa 	! 'A' - '0' - 0xa

good_digit:
	int	0x10
	loop	print_digit
	ret


/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
kill_motor:
	push dx
	mov dx,#0x3f2
	xor al, al
	outb
	pop dx
	ret

sectors:
	.word 0

msg1:
	.byte 13,10
	.ascii "Loading"

.org 506
swap_dev:
	.word SWAP_DEV
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55

.text
endtext:
.data
enddata:
.bss
endbss:

