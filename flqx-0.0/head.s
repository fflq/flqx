# flqx v0.0 head.s
# 包含32位保护模式初始化设置代码，时钟中断代码，系统调用中断代码和两个任务的代码
# 在初始化完成后，移动到任务0开始执行，并在时钟中断控制下进行任务0和任务1间切换
.code32

# 定时器初始计数值，即每隔10ms发送一次中断请求
#LATCH = 11931
# 默认最大间隔55ms
LATCH = 65535
# 屏幕显示内存的段选择符
SCRN_SEL = 0x18
# 任务0的TSS段选择符，LDT段选择符
TASK0_TSS_SEL = 0x20
TASK0_LDT_SEL = 0x28
# 任务1的TSS段选择符，LDT段选择符
TASK1_TSS_SEL = 0x30
TASK1_LDT_SEL = 0x38

.section .text
.global _start
_start:
	
## 欢迎信息,此时无法调用BIOS中断
welcome:

## 加载数据段寄存器DS，SS，ESP。所有段的线性基地址都是0
init:
	# 0x10即16是GDT中数据段选择符
	movl	$0x10, %eax
	mov		%ax, %ds
	# LSS reg32, mem16:32  Load SS:reg32 with a 48-bit far pointer from memory
	# 这里init_stack: .long init_stack .word 0x10
	# att操作数相反，即是，ss=0x10，esp=init_stack
	lss		init_stack, %esp


## 在新的位置重新设置IDT和GDT
	# 把256个中断门都填默认处理程序的描述符
	call	setup_idt
	call	setup_gdt
	# 改变gdt后要重新加载所有段R
	movl	$0x10, %eax
	mov		%ax, %ds
	mov		%ax, %es
	mov		%ax, %fs
	mov		%ax, %gs
	lss		init_stack, %esp


## 设置8253定时芯片，把计数器通道0设置成每隔10ms向中断控制器发送一个中断请求信号
# 计数器工作原理：它有一个输入频率，在PC上是1193180HZ。在每一个时钟周期(CLK cycle)，计数器值会减1，当减到0时，就会触发一个输出。
# 由于计数器是16位的，所以最大值是65535，因此，默认的时钟中断的发生频率就是1193180/65536约等于18.2HZ，即最大间隔55ms
# 如果想让系统每10ms产生一次中断，也就是让输出频率为100HZ，那么需要为计数器赋值为1193180/100约等于11931.
# 1. 8253有3个16位的counter(0,1,2)，其中counter0用作IRQ0
# 2. PC输入频率为1193180Hz =>每次(count-1)需1/1193180s，则若想设置10ms中断则counter0应置为1193180/100
# 3. counter配置：(0,1,2)置位端口分别为端口(40h,41h,42h)，16位. 端口43h配置8253模式控制寄存器，用以设置相关属性，8位
	# 8253芯片控制字寄存器的写端口
	movw	$0x43, %dx
	# 控制字：设置通道0工作在方式3，计数初值采用二进制
	movb	$0x36, %al
	outb	%al, %dx
	
	# 8253芯片通道0的写端口
	movw	$0x40, %dx
	# 初始计数值设置为LATCH(1193180/100)，即频率100HZ
	movl	$LATCH, %eax
	outb	%al, %dx
	movb	%ah, %al
	outb	%al, %dx

	
## 在IDT表第8和128(0x80)项分别设置定时中断门描述符和系统调用陷阱门描述符(区别于中断门是不关中断)
	# 设置通用中断处理程序，指向gdt第二项
	# 中断程序属内核，即eah是内核代码段选择符0x0008
	movl	$0x80000, %eax
	# 设置定时中断门描述符。取定时中断处理程序地址
	movw	$timer_interrupt, %ax
	# 中断门类型TYPE是14(注意16位是6,32位是14)，特权级0或硬件使用
	movl	$0x8e00, %edx
	# bios时钟中断向量号是8，新系统仍用8这个数字
	movl	$0x08, %ecx
	# esi保存idt中8号时钟中断描述符的起始地址
	lea		idt(,%ecx,8), %esi
	# 描述符15..0是偏移值15..0为timer_interrupt，描述符31..16是段选择符为8(gdt的第二项)
	movl	%eax, (%esi)
	# 描述符47..32是属性为0x8e00，描述符61..48是偏移值31..16为0
	# 8e=1(P),00(DPL),0,,1110(TYPE中断门)
	movl	%edx, 4(%esi)

	# 设置系统调用陷阱门描述符
	# 取系统调用处理程序地址
	movw	$system_interrupt, %ax
	# 陷阱门类型TYPE是15，特权级3的程序可执行
	# ef=1(P),11(DPL),0,,1111(TYPE陷阱门)
	movl	$0xef00, %edx
	# 系统调用的向量号是0x80即128
	movl	$0x80, %ecx
	# esi保存idt中128号时钟中断描述符的起始地址
	lea		idt(,%ecx,8), %esi
	movl	%eax, (%esi)
	movl	%edx, 4(%esi)

## test
	#误少了个$，调试半天
	#movb	'L', %al
	movb	$'L', %al
	movl	$0xff, %ecx
1:	
#call	write_char
	int	$0x80
	loop	1b
#die:jmp die




## 为移动到任务0执行来操作堆栈内容，在堆栈中人工建立返回时的场景
iret_to_task0:
	# 复位标志寄存器EFLAGS中的嵌套任务标志
	pushfl
	# 把32位EFLAGS的第15位置0，
	andl	$0xffffbfff, (%esp)
	popfl

	# 把任务0的TSS段选择符加载到任务寄存器TR
	movl	$TASK0_TSS_SEL, %eax
	ltr		%ax
	# 把任务0的LDT段选择符加载到LDTR
	movl	$TASK0_LDT_SEL, %eax
	# TR和LDTR只需人工加载一次，以后CPU会自动处理
	lldt	%ax
	# 把当前任务号0保存到current变量中
	movl	$0, current

	# 中断是在move_code时cli关的
	sti

	# 把任务0当前局部空间数据段(堆栈段)选择符入栈
	pushl	$0x17
	# 把堆栈指针入栈(也可直接把esp入栈)
	pushl	$init_stack
	pushfl
	# 把当前局部空间代码段选择符入栈
	pushl	$0x0f
	# 把代码指针入栈，可能指偏移和上面的基地址
	pushl	$task0
	# 执行中断返回指令，从而切换到特权级3的任务0中执行
	iret




#### 函数 ##################################################

## 设置GDT和IDT中描述符项的子程序
# 使用6字节操作数lgdt_opcode设置GDT表位置和长度
setup_gdt:
	lgdt	lgdt_opcode
	ret


## 这段代码暂时设置IDT表中所有256个中断门描述符都为同一个默认值，均使用默认中断处理过程ignore_int
## 设置具体方法：先在eax和edx分别设置好，默认中断门描述符的0-3字节和4-7字节，然后循环往IDT表填
setup_idt:
	# 设置方法与设置定时中断门描述符的方法一样
	# 中断门描述符0-3字节(段选择符,偏移值15-0)=eax=(8,ignore_int)
	lea		ignore_int, %edx	
	# 选择符为0x8
	movl	$80000, %eax
	movw	%dx, %ax

	# 中断门类型，特权级为0
	# 中断门描述符4-7字节(偏移值31-16,属性)=edx=(0,0x8e00)
	# 属性0x8e00，8e=1(P),00(DPL),0,,1110(TYPE中断门)
	movl	$0x8e00, %edx

	lea		idt, %edi
	# 循环设置所有256个门描述符项
	mov		$256, %ecx
rep_idt:
	movl	%eax, (%edi)
	movl	%edx, 4(%edi)
	addl	$8, %edi
	dec		%ecx
	jne		rep_idt

	# 最后用6字节操作数加载IDTR寄存器
	lidt	lidt_opcode
	ret


## 显示字符子程序。取当前光标位置并把AL中字符显示在屏幕，整屏可显示80*25个字符
write_char:
	push	%gs
	pushl	%ebx

	# 让gs指向显示内存段0xb8000
	mov		$SCRN_SEL, %ebx
	mov		%bx, %gs
	# 从变量scr_loc(0-2000)中取目前字符显示位置
	movl	scr_loc, %ebx
	# 因为屏幕每个字符还有个属性字节，因为实际位置对应的偏移地址要乘2
	shl		$1, %ebx
	movb	%al, %gs:(%ebx)

	# 除2恢复scr_loc计数，然后加1对应到下一次。如此而不是+2是为了方便计算边界
	shr		$1, %ebx
	incl	%ebx
	# 若新计数大于2000，则复位成0从头开始
	cmpl	$2000, %ebx
	jb		1f
	# 新计数复位成0
	movl	$0, %ebx
1:
	# 保存新计数到全局变量src_loc
	movl	%ebx, scr_loc

	popl	%ebx
	pop		%gs
	ret


## 3个中断处理程序：默认终端，定时中断，系统调用中断
## 默认终端处理程序，在屏幕上显示个C
.align 4
ignore_int:
	push	%ds
	pushl	%eax

	# 先让ds指向内核数据段，因为中断程序属于内核
	movl	$0x10, %eax
	mov		%ax, %ds
	movl	$'C', %eax
	call	write_char

	popl	%eax
	pop		%ds
	iret


## 定时中断处理程序，主要执行任务切换操作
.align 4
timer_interrupt:
	push	%ds
	pushl	%eax

	# 先让ds指向内核数据段
	movl	$0x10, %eax
	mov		%ax, %ds

	# 立刻允许其他硬件终端，即向8259A发送EOI命令
	movb	$0x20, %al
	outb	%al, $0x20

	# 判断当前任务号，01相转
	movl	$1, %eax
	cmpl	%eax, current
	je to_task0
to_task1:
	# 存入current，跳转到任务1，偏移值无用但要写
	movl	$1, current
	ljmp	$TASK1_TSS_SEL, $0
	jmp		end
to_task0:
	movl	$0, current
	ljmp	$TASK0_TSS_SEL, $0
end:
	popl	%eax
	pop		%ds
	iret


## 系统调用中断int 0x80处理程序
.align 4
system_interrupt:
	push	%ds
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx

	# 先让ds指向内核数据段
	movl	$0x10, %edx
	mov		%dx, %ds

	# 显示al中字符
	call	write_char

	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	pop		%ds
	iret




#### 数据 #####################################

# 当前任务号
current: .long 0
# 屏幕当前显示位位，左上到右下
scr_loc: .long 0


.align 4
lidt_opcode:
	# 加载到IDTR的6字节操作数：表长度和基地址
	.word 256*8-1
	.long idt
lgdt_opcode:
	.word (end_gdt-gdt)-1
	.long gdt


.align 8
# IDT共256个门描述符，每个8字节，共占用2KB
idt:	.fill 256,8,0

# GDT，第一个不用
gdt:	.quad 0x0
		# 内核代码段描述符，选择符0x8
		.quad 0x00c09a00000007ff
		# 内核数据段描述符，选择符0x10
		.quad 0x00c09200000007ff
		# 显示内存段描述符，选择符0x18
		.quad 0x00c0920b80000002
		
		# TASK0_TSS_SEL段描述符，0x20
		.word 0x68, tss0, 0xe900, 0
		# TASK0_LDT_SEL段描述符，0x28
		.word 0x40, ldt0, 0xe200, 0
		# TASK1_TSS_SEL段描述符，0x20
		.word 0x68, tss1, 0xe900, 0
		# TASK1_LDT_SEL段描述符，0x28
		.word 0x40, ldt1, 0xe200, 0
end_gdt:


end_stack:
	# 初始内核堆栈空间
	.fill 128,4,0
# 刚进入保护模式时用于加载ss:esp堆栈指针值
init_stack:
	# 堆栈段偏移位置
	.long init_stack
	# 堆栈段和内核数据段
	.word 0x10


# 任务0的LDT表段中的局部段描述符
.align 8
ldt0:	.quad 0
		# 局部代码段描述符，对应选择符0x0f
		.quad 0x00c0fa00000003ff
		# 局部数据段描述符，选择符0x17
		.quad 0x00c0f200000003ff

# 任务0的TSS段内容，其中标号等字段在任务切换时不会改变
tss0:	
	# back link
	.long 0
	# esp0, ss0
	.long krn_stack0, 0x10
	# esp1, ss1, esp2, ss2, cr3
	.long 0, 0, 0, 0, 0
	# eip, elfags, eax, ecx, edx
	.long 0, 0, 0, 0, 0
	# ebx, esp, ebp, esi, edi
	.long 0, 0, 0, 0, 0
	# es, cs, ss, ds, fs, gs
	.long 0, 0, 0, 0, 0, 0
	# ldt, trace bitmap
	.long TASK0_LDT_SEL, 0x80000000

	# 任务0的内核栈空间
	.fill 128,4,0
krn_stack0:
	

	
# 任务1的LDT表段中的局部段描述符
.align 8
ldt1:	.quad 0
		# 局部代码段描述符，对应选择符0x0f
		.quad 0x00c0fa00000003ff
		# 局部数据段描述符，选择符0x17
		.quad 0x00c0f200000003ff

# 任务0的TSS段内容，其中标号等字段在任务切换时不会改变
tss1:	
	# back link
	.long 0
	# esp0, ss0
	.long krn_stack1, 0x10
	# esp1, ss1, esp2, ss2, cr3
	.long 0, 0, 0, 0, 0
	# eip, elfags, eax, ecx, edx
	.long task1, 0x200, 0, 0, 0
	# ebx, esp, ebp, esi, edi
	.long 0, usr_stack1, 0, 0, 0
	# es, cs, ss, ds, fs, gs
	.long 0x17, 0xf, 0x17, 0x17, 0x17, 0x17
	# ldt, trace bitmap
	.long TASK1_LDT_SEL, 0x80000000

	# 任务1的内核栈空间，其用户栈直接使用初始栈空间
	.fill 128,4,0
krn_stack1:

	.fill 128,4,0
usr_stack1:

	

## 任务0和1的程序，循环显示AB
task0:
	# 先让ds指向任务的局部数据段
	movl	$0x17, %eax
	# 当任务没有使用局部数据时，这两句可省略
	movw	%ax, %ds
	
	movb	$'A', %al
	int		$0x80
	# 用write_char会在mov %bx, %gs时出错，可能是段访问权限问题吧，难怪用int
	#call	write_char

	# 延时
	movl	$0xffff, %ecx
1:	loop	1b
	# 重复执行直到任务被切换
	jmp		task0


task1:
	movl	$0x17, %eax
	movw	%ax, %ds
	
	movb	$'B', %al
	int		$0x80

	movl	$0xffff, %ecx
1:	loop	1b
	jmp		task1










