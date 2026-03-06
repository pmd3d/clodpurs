	.globl fib

	.text
fib:
	pushq %rbp
	movq %rsp, %rbp
	movl %edi, %n.2
	cmpl $1, %n.2
	movl $0, %tmp.5
	setle %tmp.5
	cmpl $0, %tmp.5
	je .Lif_end.4
	movl %n.2, %eax

	movq %rbp, %rsp
	popq %rbp
	ret
.Lif_end.4:
	movl %n.2, %tmp.7
	subl $1, %tmp.7
	movl %tmp.7, %edi
	call fib
	movl %eax, %tmp.6
	movl %n.2, %tmp.9
	subl $2, %tmp.9
	movl %tmp.9, %edi
	call fib
	movl %eax, %tmp.8
	movl %tmp.6, %tmp.10
	addl %tmp.8, %tmp.10
	movl %tmp.10, %eax

	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax

	movq %rbp, %rsp
	popq %rbp
	ret
	.globl main

	.text
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $7, %edi
	call fib
	movl %eax, %tmp.11
	movl %tmp.11, 0(%p.3)
	movl $8, %edi
	call fib
	movl %eax, %tmp.12
	movl %tmp.12, 4(%p.3)
	movl 0(%p.3), %tmp.14
	movl %tmp.14, %eax
	cdq
	idivl $10
	movl %eax, %tmp.15
	movl $48, %tmp.16
	addl %tmp.15, %tmp.16
	movl %tmp.16, %edi
	call putchar@PLT
	movl %eax, %tmp.13
	movl 0(%p.3), %tmp.18
	movl %tmp.18, %eax
	cdq
	idivl $10
	movl %edx, %tmp.19
	movl $48, %tmp.20
	addl %tmp.19, %tmp.20
	movl %tmp.20, %edi
	call putchar@PLT
	movl %eax, %tmp.17
	movl $32, %edi
	call putchar@PLT
	movl %eax, %tmp.21
	movl 4(%p.3), %tmp.23
	movl %tmp.23, %eax
	cdq
	idivl $10
	movl %eax, %tmp.24
	movl $48, %tmp.25
	addl %tmp.24, %tmp.25
	movl %tmp.25, %edi
	call putchar@PLT
	movl %eax, %tmp.22
	movl 4(%p.3), %tmp.27
	movl %tmp.27, %eax
	cdq
	idivl $10
	movl %edx, %tmp.28
	movl $48, %tmp.29
	addl %tmp.28, %tmp.29
	movl %tmp.29, %edi
	call putchar@PLT
	movl %eax, %tmp.26
	movl $10, %edi
	call putchar@PLT
	movl %eax, %tmp.30
	movl $0, %eax

	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax

	movq %rbp, %rsp
	popq %rbp
	ret
	.section .note.GNU-stack,"",@progbits
