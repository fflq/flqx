# flqx v0.0 head.s
.code32

.section .text
.global _start
_start:
	
## 欢迎信息,此时无法调用BIOS中断
welcome:
	movb	$0xe, %ah
	movb	$'L', %al
	int	$0x10
	movb	$'Q', %al
	int	$0x10


