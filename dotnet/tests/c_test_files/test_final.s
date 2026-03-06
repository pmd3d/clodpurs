	.globl fib

	.text
fib:
	pushq %rbp
	movq %rsp, %rbp
	subq $0, %rsp
	pushq %rbx
	pushq %r12
	movl %edi, %ebx
	cmpl $1, %ebx
	movl $0, %r12d
	setle %r12b
	cmpl $0, %r12d
	je .Lif_end.4
	movl %ebx, %eax
	popq %r12
	popq %rbx

	movq %rbp, %rsp
	popq %rbp
	ret
.Lif_end.4:
	movl %ebx, %edi
	subl $1, %edi
	call fib
	movl %eax, %r12d
	subl $2, %ebx
	movl %ebx, %edi
	call fib
	addl %eax, %r12d
	movl %r12d, %eax
	popq %r12
	popq %rbx

	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax
	popq %r12
	popq %rbx

	movq %rbp, %rsp
	popq %rbp
	ret
	.globl main

	.text
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movl $7, %edi
	call fib
	movl %eax, -8(%rbp)
	movl $8, %edi
	call fib
	movl %eax, -4(%rbp)
	movl -8(%rbp), %eax
	cdq
	movl $10, %r10d
	idivl %r10d
	movl $48, %edi
	addl %eax, %edi
	call putchar@PLT
	movl -8(%rbp), %eax
	cdq
	movl $10, %r10d
	idivl %r10d
	movl $48, %edi
	addl %edx, %edi
	call putchar@PLT
	movl $32, %edi
	call putchar@PLT
	movl -4(%rbp), %eax
	cdq
	movl $10, %r10d
	idivl %r10d
	movl $48, %edi
	addl %eax, %edi
	call putchar@PLT
	movl -4(%rbp), %eax
	cdq
	movl $10, %r10d
	idivl %r10d
	movl $48, %edi
	addl %edx, %edi
	call putchar@PLT
	movl $10, %edi
	call putchar@PLT
	movl $0, %eax

	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax

	movq %rbp, %rsp
	popq %rbp
	ret
	.section .note.GNU-stack,"",@progbits
