	.file	"spawnedrw.c"
	.text
	.globl	bar
	.align	16, 0x90
	.type	bar,@function
bar:                                    # @bar
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp4:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__cilkrts_ivar_read
	movl	%eax, %ecx
	movl	%ecx, -12(%rbp)
	cmpl	$39, -12(%rbp)
	je	.LBB0_2
# BB#1:
	leaq	.L.str, %rdi
	movl	$39, %edx
	movl	-12(%rbp), %esi
	movb	$0, %al
	callq	printf
	movl	%eax, -16(%rbp)         # 4-byte Spill
	callq	abort
.LBB0_2:
	leaq	.L.str1, %rdi
	movl	-12(%rbp), %esi
	movb	$0, %al
	callq	printf
	movl	%eax, -20(%rbp)         # 4-byte Spill
	addq	$32, %rsp
	popq	%rbp
	ret
.Ltmp5:
	.size	bar, .Ltmp5-bar
	.cfi_endproc

	.globl	fun
	.align	16, 0x90
	.type	fun,@function
fun:                                    # @fun
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp9:
	.cfi_def_cfa_offset 16
.Ltmp10:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp11:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$168, %rsp
.Ltmp12:
	.cfi_offset %rbx, -56
.Ltmp13:
	.cfi_offset %r12, -48
.Ltmp14:
	.cfi_offset %r13, -40
.Ltmp15:
	.cfi_offset %r14, -32
.Ltmp16:
	.cfi_offset %r15, -24
	leaq	-136(%rbp), %rdi
	callq	__cilk_parent_prologue
	leaq	-144(%rbp), %rdi
	callq	__cilkrts_ivar_clear
	leaq	.L.str2, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -172(%rbp)        # 4-byte Spill
# BB#1:
	leaq	-136(%rbp), %rax
	movq	%rax, %rcx
	addq	$72, %rcx
	movq	%rax, %rdx
	addq	$76, %rdx
	#APP
	stmxcsr (%rcx)
	fnstcw (%rdx)
	#NO_APP
	addq	$32, %rax
	movq	%rbp, %rcx
	movq	%rcx, -104(%rbp)
	movq	%rsp, %rcx
	movq	%rcx, -88(%rbp)
	movq	$.LBB1_13, 8(%rax)
	#EH_SjLj_Setup	.LBB1_13
# BB#11:
	xorl	%eax, %eax
	movl	%eax, -176(%rbp)        # 4-byte Spill
.LBB1_12:
	movl	-176(%rbp), %eax        # 4-byte Reload
	cmpl	$0, %eax
	jne	.LBB1_3
# BB#2:
	leaq	-152(%rbp), %rdi
	leaq	-144(%rbp), %rax
	movq	%rax, -152(%rbp)
	callq	__cilk_spawn_helper
.LBB1_3:
	leaq	.L.str3, %rdi
	movb	$0, %al
	callq	printf
	leaq	-144(%rbp), %rdi
	movabsq	$39, %rsi
	movl	%eax, -180(%rbp)        # 4-byte Spill
	callq	__cilkrts_ivar_write
	leaq	.L.str4, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -184(%rbp)        # 4-byte Spill
# BB#4:
	leaq	-136(%rbp), %rax
	movq	%rax, %rcx
	addq	$72, %rcx
	movq	%rax, %rdx
	addq	$76, %rdx
	#APP
	stmxcsr (%rcx)
	fnstcw (%rdx)
	#NO_APP
	addq	$32, %rax
	movq	%rbp, %rcx
	movq	%rcx, -104(%rbp)
	movq	%rsp, %rcx
	movq	%rcx, -88(%rbp)
	movq	$.LBB1_16, 8(%rax)
	#EH_SjLj_Setup	.LBB1_16
# BB#14:
	xorl	%eax, %eax
	movl	%eax, -188(%rbp)        # 4-byte Spill
.LBB1_15:
	movl	-188(%rbp), %eax        # 4-byte Reload
	cmpl	$0, %eax
	jne	.LBB1_6
# BB#5:
	leaq	-160(%rbp), %rdi
	leaq	-144(%rbp), %rax
	movq	%rax, -160(%rbp)
	callq	__cilk_spawn_helper5
.LBB1_6:
	leaq	.L.str6, %rdi
	movq	%rdi, -200(%rbp)        # 8-byte Spill
	callq	__cilkrts_get_tls_worker
	movq	%rax, -168(%rbp)
	movq	-168(%rbp), %rax
	movl	40(%rax), %esi
	movq	-200(%rbp), %rdi        # 8-byte Reload
	movb	$0, %al
	callq	printf
	movl	-136(%rbp), %esi
	andl	$2, %esi
	cmpl	$0, %esi
	movl	%eax, -204(%rbp)        # 4-byte Spill
	je	.LBB1_10
# BB#7:                                 # %cilk.sync.savestate.i
	leaq	-136(%rbp), %rax
	movq	-120(%rbp), %rcx
	movq	96(%rcx), %rdx
	movq	104(%rcx), %rcx
	movq	%rcx, -48(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rax, %rcx
	addq	$72, %rcx
	movq	%rax, %rdx
	addq	$76, %rdx
	#APP
	stmxcsr (%rcx)
	fnstcw (%rdx)
	#NO_APP
	addq	$32, %rax
	movq	%rbp, %rcx
	movq	%rcx, -104(%rbp)
	movq	%rsp, %rcx
	movq	%rcx, -88(%rbp)
	movq	$.LBB1_19, 8(%rax)
	#EH_SjLj_Setup	.LBB1_19
# BB#17:                                # %cilk.sync.savestate.i
	xorl	%eax, %eax
	movl	%eax, -208(%rbp)        # 4-byte Spill
.LBB1_18:                               # %cilk.sync.savestate.i
	movl	-208(%rbp), %eax        # 4-byte Reload
	cmpl	$0, %eax
	jne	.LBB1_9
# BB#8:                                 # %cilk.sync.runtimecall.i
	leaq	-136(%rbp), %rdi
	callq	__cilkrts_sync
	jmp	.LBB1_10
.LBB1_9:                                # %cilk.sync.excepting.i
	jmp	.LBB1_10
.LBB1_10:                               # %__cilk_sync.exit
	leaq	-136(%rbp), %rdi
	movq	-120(%rbp), %rax
	movq	96(%rax), %rcx
	addq	$1, %rcx
	movq	%rcx, 96(%rax)
	callq	__cilk_parent_epilogue
	addq	$168, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
.LBB1_13:
	movl	$1, %eax
	movl	%eax, -176(%rbp)        # 4-byte Spill
	jmp	.LBB1_12
.LBB1_16:
	movl	$1, %eax
	movl	%eax, -188(%rbp)        # 4-byte Spill
	jmp	.LBB1_15
.LBB1_19:                               # %cilk.sync.savestate.i
	movl	$1, %eax
	movl	%eax, -208(%rbp)        # 4-byte Spill
	jmp	.LBB1_18
.Ltmp17:
	.size	fun, .Ltmp17-fun
	.cfi_endproc

	.align	16, 0x90
	.type	__cilk_parent_prologue,@function
__cilk_parent_prologue:                 # @__cilk_parent_prologue
# BB#0:                                 # %entry
	pushq	%rbp
	movq	%rsp, %rbp
	callq	__cilkrts_enter_frame_1
	popq	%rbp
	ret
.Ltmp18:
	.size	__cilk_parent_prologue, .Ltmp18-__cilk_parent_prologue

	.align	16, 0x90
	.type	__cilk_spawn_helper,@function
__cilk_spawn_helper:                    # @__cilk_spawn_helper
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp21:
	.cfi_def_cfa_offset 16
.Ltmp22:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp23:
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	leaq	-104(%rbp), %rax
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	%rdi, -112(%rbp)        # 8-byte Spill
	movq	%rax, %rdi
	callq	__cilk_reset_worker
	leaq	-104(%rbp), %rdi
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, -120(%rbp)        # 8-byte Spill
	callq	__cilk_helper_prologue
	movq	-120(%rbp), %rdi        # 8-byte Reload
	callq	__cilkrts_ivar_read
	leaq	-104(%rbp), %rdi
	movq	%rax, -128(%rbp)        # 8-byte Spill
	callq	__cilk_helper_epilogue
	addq	$128, %rsp
	popq	%rbp
	ret
.Ltmp24:
	.size	__cilk_spawn_helper, .Ltmp24-__cilk_spawn_helper
	.cfi_endproc

	.align	16, 0x90
	.type	__cilk_reset_worker,@function
__cilk_reset_worker:                    # @__cilk_reset_worker
# BB#0:                                 # %entry
	pushq	%rbp
	movq	%rsp, %rbp
	movq	$0, 16(%rdi)
	popq	%rbp
	ret
.Ltmp25:
	.size	__cilk_reset_worker, .Ltmp25-__cilk_reset_worker

	.align	16, 0x90
	.type	__cilk_helper_prologue,@function
__cilk_helper_prologue:                 # @__cilk_helper_prologue
# BB#0:                                 # %entry
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)          # 8-byte Spill
	callq	__cilkrts_enter_frame_fast_1
	movq	-8(%rbp), %rdi          # 8-byte Reload
	callq	__cilkrts_detach
	addq	$16, %rsp
	popq	%rbp
	ret
.Ltmp26:
	.size	__cilk_helper_prologue, .Ltmp26-__cilk_helper_prologue

	.align	16, 0x90
	.type	__cilkrts_detach,@function
__cilkrts_detach:                       # @__cilkrts_detach
# BB#0:                                 # %entry
	pushq	%rbp
	movq	%rsp, %rbp
	movq	16(%rdi), %rax
	movq	(%rax), %rcx
	movq	96(%rax), %rdx
	movq	104(%rax), %rsi
	movq	%rsi, 88(%rdi)
	movq	%rdx, 80(%rdi)
	movq	96(%rax), %rdx
	movq	104(%rax), %rsi
	movq	8(%rdi), %r8
	movq	%rsi, 88(%r8)
	movq	%rdx, 80(%r8)
	movq	$0, 96(%rax)
	movq	%rdi, %rdx
	addq	$80, %rdx
	movq	%rdx, 104(%rax)
	movq	8(%rdi), %rdx
	movq	%rdx, (%rcx)
	addq	$8, %rcx
	movq	%rcx, (%rax)
	movl	(%rdi), %r9d
	orl	$4, %r9d
	movl	%r9d, (%rdi)
	popq	%rbp
	ret
.Ltmp27:
	.size	__cilkrts_detach, .Ltmp27-__cilkrts_detach

	.align	16, 0x90
	.type	__cilk_helper_epilogue,@function
__cilk_helper_epilogue:                 # @__cilk_helper_epilogue
# BB#0:                                 # %entry
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	cmpq	$0, 16(%rdi)
	movq	%rdi, -8(%rbp)          # 8-byte Spill
	je	.LBB9_2
# BB#1:                                 # %body
	movq	-8(%rbp), %rdi          # 8-byte Reload
	callq	__cilkrts_pop_frame
	movq	-8(%rbp), %rdi          # 8-byte Reload
	callq	__cilkrts_leave_frame
.LBB9_2:                                # %exit
	addq	$16, %rsp
	popq	%rbp
	ret
.Ltmp28:
	.size	__cilk_helper_epilogue, .Ltmp28-__cilk_helper_epilogue

	.align	16, 0x90
	.type	__cilkrts_pop_frame,@function
__cilkrts_pop_frame:                    # @__cilkrts_pop_frame
# BB#0:                                 # %entry
	pushq	%rbp
	movq	%rsp, %rbp
	movq	8(%rdi), %rax
	movq	16(%rdi), %rcx
	movq	%rax, 72(%rcx)
	movq	$0, 8(%rdi)
	popq	%rbp
	ret
.Ltmp29:
	.size	__cilkrts_pop_frame, .Ltmp29-__cilkrts_pop_frame

	.align	16, 0x90
	.type	__cilk_spawn_helper5,@function
__cilk_spawn_helper5:                   # @__cilk_spawn_helper5
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp32:
	.cfi_def_cfa_offset 16
.Ltmp33:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp34:
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	leaq	-104(%rbp), %rax
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	%rdi, -112(%rbp)        # 8-byte Spill
	movq	%rax, %rdi
	callq	__cilk_reset_worker
	leaq	-104(%rbp), %rdi
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, -120(%rbp)        # 8-byte Spill
	callq	__cilk_helper_prologue
	movq	-120(%rbp), %rdi        # 8-byte Reload
	callq	bar
	leaq	-104(%rbp), %rdi
	callq	__cilk_helper_epilogue
	addq	$128, %rsp
	popq	%rbp
	ret
.Ltmp35:
	.size	__cilk_spawn_helper5, .Ltmp35-__cilk_spawn_helper5
	.cfi_endproc

	.align	16, 0x90
	.type	__cilk_parent_epilogue,@function
__cilk_parent_epilogue:                 # @__cilk_parent_epilogue
# BB#0:                                 # %entry
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)          # 8-byte Spill
	callq	__cilkrts_pop_frame
	movq	-8(%rbp), %rdi          # 8-byte Reload
	cmpl	$16777216, (%rdi)       # imm = 0x1000000
	je	.LBB12_2
# BB#1:
	movq	-8(%rbp), %rdi          # 8-byte Reload
	callq	__cilkrts_leave_frame
.LBB12_2:                               # %exit
	addq	$16, %rsp
	popq	%rbp
	ret
.Ltmp36:
	.size	__cilk_parent_epilogue, .Ltmp36-__cilk_parent_epilogue

	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp40:
	.cfi_def_cfa_offset 16
.Ltmp41:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp42:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$168, %rsp
.Ltmp43:
	.cfi_offset %rbx, -56
.Ltmp44:
	.cfi_offset %r12, -48
.Ltmp45:
	.cfi_offset %r13, -40
.Ltmp46:
	.cfi_offset %r14, -32
.Ltmp47:
	.cfi_offset %r15, -24
	leaq	-144(%rbp), %rax
	movl	%edi, -176(%rbp)        # 4-byte Spill
	movq	%rax, %rdi
	movq	%rsi, -184(%rbp)        # 8-byte Spill
	callq	__cilk_parent_prologue
	movl	$0, -44(%rbp)
	movl	-176(%rbp), %ecx        # 4-byte Reload
	movl	%ecx, -148(%rbp)
	movq	-184(%rbp), %rax        # 8-byte Reload
	movq	%rax, -160(%rbp)
# BB#1:
	leaq	-144(%rbp), %rax
	movq	%rax, %rcx
	addq	$72, %rcx
	movq	%rax, %rdx
	addq	$76, %rdx
	#APP
	stmxcsr (%rcx)
	fnstcw (%rdx)
	#NO_APP
	addq	$32, %rax
	movq	%rbp, %rcx
	movq	%rcx, -112(%rbp)
	movq	%rsp, %rcx
	movq	%rcx, -96(%rbp)
	movq	$.LBB13_10, 8(%rax)
	#EH_SjLj_Setup	.LBB13_10
# BB#8:
	xorl	%eax, %eax
	movl	%eax, -188(%rbp)        # 4-byte Spill
.LBB13_9:
	movl	-188(%rbp), %eax        # 4-byte Reload
	cmpl	$0, %eax
	jne	.LBB13_3
# BB#2:
	leaq	-168(%rbp), %rdi
	callq	__cilk_spawn_helper7
.LBB13_3:
	leaq	.L.str8, %rdi
	movb	$0, %al
	callq	printf
	movl	$0, -44(%rbp)
	movl	$1, -172(%rbp)
	movl	-144(%rbp), %ecx
	andl	$2, %ecx
	cmpl	$0, %ecx
	movl	%eax, -192(%rbp)        # 4-byte Spill
	je	.LBB13_7
# BB#4:                                 # %cilk.sync.savestate.i
	leaq	-144(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	96(%rcx), %rdx
	movq	104(%rcx), %rcx
	movq	%rcx, -56(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rax, %rcx
	addq	$72, %rcx
	movq	%rax, %rdx
	addq	$76, %rdx
	#APP
	stmxcsr (%rcx)
	fnstcw (%rdx)
	#NO_APP
	addq	$32, %rax
	movq	%rbp, %rcx
	movq	%rcx, -112(%rbp)
	movq	%rsp, %rcx
	movq	%rcx, -96(%rbp)
	movq	$.LBB13_13, 8(%rax)
	#EH_SjLj_Setup	.LBB13_13
# BB#11:                                # %cilk.sync.savestate.i
	xorl	%eax, %eax
	movl	%eax, -196(%rbp)        # 4-byte Spill
.LBB13_12:                              # %cilk.sync.savestate.i
	movl	-196(%rbp), %eax        # 4-byte Reload
	cmpl	$0, %eax
	jne	.LBB13_6
# BB#5:                                 # %cilk.sync.runtimecall.i
	leaq	-144(%rbp), %rdi
	callq	__cilkrts_sync
	jmp	.LBB13_7
.LBB13_6:                               # %cilk.sync.excepting.i
	jmp	.LBB13_7
.LBB13_7:                               # %__cilk_sync.exit
	leaq	-144(%rbp), %rdi
	movq	-128(%rbp), %rax
	movq	96(%rax), %rcx
	addq	$1, %rcx
	movq	%rcx, 96(%rax)
	callq	__cilk_parent_epilogue
	movl	-44(%rbp), %eax
	addq	$168, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
.LBB13_10:
	movl	$1, %eax
	movl	%eax, -188(%rbp)        # 4-byte Spill
	jmp	.LBB13_9
.LBB13_13:                              # %cilk.sync.savestate.i
	movl	$1, %eax
	movl	%eax, -196(%rbp)        # 4-byte Spill
	jmp	.LBB13_12
.Ltmp48:
	.size	main, .Ltmp48-main
	.cfi_endproc

	.align	16, 0x90
	.type	__cilk_spawn_helper7,@function
__cilk_spawn_helper7:                   # @__cilk_spawn_helper7
	.cfi_startproc
# BB#0:
	pushq	%rbp
.Ltmp51:
	.cfi_def_cfa_offset 16
.Ltmp52:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp53:
	.cfi_def_cfa_register %rbp
	subq	$112, %rsp
	leaq	-104(%rbp), %rax
	movq	%rdi, -8(%rbp)
	movq	%rax, %rdi
	callq	__cilk_reset_worker
	leaq	-104(%rbp), %rdi
	callq	__cilk_helper_prologue
	callq	fun
	leaq	-104(%rbp), %rdi
	callq	__cilk_helper_epilogue
	addq	$112, %rsp
	popq	%rbp
	ret
.Ltmp54:
	.size	__cilk_spawn_helper7, .Ltmp54-__cilk_spawn_helper7
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	 "TEST ERROR - BAD VALUE, %d, EXPECTED %d - ABORTING!\n"
	.size	.L.str, 53

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	 "reading i in bar: %d\n"
	.size	.L.str1, 22

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	 "read before write -slow path\n"
	.size	.L.str2, 30

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	 "write ivar\n"
	.size	.L.str3, 12

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	 "read after write -fast path\n"
	.size	.L.str4, 29

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	 "returning from fun...this will pop the frame (w=%d)\n"
	.size	.L.str6, 53

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	 "test complete\n"
	.size	.L.str8, 15


	.section	".note.GNU-stack","",@progbits
