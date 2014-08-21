; ModuleID = 'spawnedrw.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%__cilkrts_stack_frame = type { i32, i32, %__cilkrts_stack_frame*, %__cilkrts_worker*, i8*, [5 x i8*], i32, i16, i16, %__cilkrts_pedigree }
%__cilkrts_worker = type { %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, i32, i8*, i8*, i8*, %__cilkrts_stack_frame*, %__cilkrts_stack_frame**, i8*, %__cilkrts_pedigree }
%__cilkrts_pedigree = type { i64, %__cilkrts_pedigree* }
%struct.anon = type { i64* }
%struct.anon.0 = type { i64* }
%struct.__cilkrts_worker = type { %struct.__cilkrts_stack_frame**, %struct.__cilkrts_stack_frame**, %struct.__cilkrts_stack_frame**, %struct.__cilkrts_stack_frame**, %struct.__cilkrts_stack_frame**, i32, %struct.global_state_t*, %struct.local_state*, %struct.cilkred_map*, %struct.__cilkrts_stack_frame*, i8*, %struct.__cilkrts_worker_sysdep_state* }
%struct.__cilkrts_stack_frame = type { i32, i32, %struct.__cilkrts_stack_frame*, %struct.__cilkrts_worker*, i8*, [5 x i8*] }
%struct.global_state_t = type opaque
%struct.local_state = type opaque
%struct.cilkred_map = type opaque
%struct.__cilkrts_worker_sysdep_state = type opaque
%struct.anon.1 = type {}

@.str = private unnamed_addr constant [53 x i8] c"TEST ERROR - BAD VALUE, %d, EXPECTED %d - ABORTING!\0A\00", align 1
@.str1 = private unnamed_addr constant [22 x i8] c"reading i in bar: %d\0A\00", align 1
@.str2 = private unnamed_addr constant [30 x i8] c"read before write -slow path\0A\00", align 1
@.str3 = private unnamed_addr constant [12 x i8] c"write ivar\0A\00", align 1
@.str4 = private unnamed_addr constant [29 x i8] c"read after write -fast path\0A\00", align 1
@.str6 = private unnamed_addr constant [53 x i8] c"returning from fun...this will pop the frame (w=%d)\0A\00", align 1
@.str8 = private unnamed_addr constant [15 x i8] c"test complete\0A\00", align 1

; Function Attrs: nounwind uwtable
define void @bar(i64* %iv) #0 {
  %1 = alloca i64*, align 8
  %val = alloca i32, align 4
  store i64* %iv, i64** %1, align 8
  %2 = load i64** %1, align 8
  %3 = call i64 @__cilkrts_ivar_read(i64* %2)
  %4 = trunc i64 %3 to i32
  store i32 %4, i32* %val, align 4
  %5 = load i32* %val, align 4
  %6 = icmp ne i32 %5, 39
  br i1 %6, label %7, label %10

; <label>:7                                       ; preds = %0
  %8 = load i32* %val, align 4
  %9 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([53 x i8]* @.str, i32 0, i32 0), i32 %8, i32 39)
  call void @abort() #7
  unreachable

; <label>:10                                      ; preds = %0
  %11 = load i32* %val, align 4
  %12 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([22 x i8]* @.str1, i32 0, i32 0), i32 %11)
  ret void
}

declare i64 @__cilkrts_ivar_read(i64*) #1

declare i32 @printf(i8*, ...) #1

; Function Attrs: noreturn nounwind
declare void @abort() #2

; Function Attrs: nounwind uwtable
define void @fun() #0 {
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  call void @__cilk_parent_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %iv = alloca i64, align 8
  %1 = alloca %struct.anon, align 8
  %2 = alloca %struct.anon.0, align 8
  %w = alloca %struct.__cilkrts_worker*, align 8
  call void @__cilkrts_ivar_clear(i64* %iv)
  %3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([30 x i8]* @.str2, i32 0, i32 0))
  br label %4

; <label>:4                                       ; preds = %0
  %5 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %6 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %5, i16* %6)
  %7 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %8 = call i8* @llvm.frameaddress(i32 0)
  %9 = getelementptr inbounds [5 x i8*]* %7, i32 0, i32 0
  store i8* %8, i8** %9
  %10 = call i8* @llvm.stacksave()
  %11 = getelementptr inbounds [5 x i8*]* %7, i32 0, i32 2
  store i8* %10, i8** %11
  %12 = bitcast [5 x i8*]* %7 to i8*
  %13 = call i32 @llvm.eh.sjlj.setjmp(i8* %12) #8
  %14 = icmp eq i32 %13, 0
  br i1 %14, label %15, label %17

; <label>:15                                      ; preds = %4
  %16 = getelementptr inbounds %struct.anon* %1, i32 0, i32 0
  store i64* %iv, i64** %16, align 8
  call void @__cilk_spawn_helper(%struct.anon* %1)
  br label %17

; <label>:17                                      ; preds = %15, %4
  %18 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str3, i32 0, i32 0))
  call void @__cilkrts_ivar_write(i64* %iv, i64 39)
  %19 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([29 x i8]* @.str4, i32 0, i32 0))
  br label %20

; <label>:20                                      ; preds = %17
  %21 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %22 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %21, i16* %22)
  %23 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %24 = call i8* @llvm.frameaddress(i32 0)
  %25 = getelementptr inbounds [5 x i8*]* %23, i32 0, i32 0
  store i8* %24, i8** %25
  %26 = call i8* @llvm.stacksave()
  %27 = getelementptr inbounds [5 x i8*]* %23, i32 0, i32 2
  store i8* %26, i8** %27
  %28 = bitcast [5 x i8*]* %23 to i8*
  %29 = call i32 @llvm.eh.sjlj.setjmp(i8* %28) #8
  %30 = icmp eq i32 %29, 0
  br i1 %30, label %31, label %33

; <label>:31                                      ; preds = %20
  %32 = getelementptr inbounds %struct.anon.0* %2, i32 0, i32 0
  store i64* %iv, i64** %32, align 8
  call void @__cilk_spawn_helper5(%struct.anon.0* %2)
  br label %33

; <label>:33                                      ; preds = %31, %20
  %34 = call %struct.__cilkrts_worker* bitcast (%__cilkrts_worker* ()* @__cilkrts_get_tls_worker to %struct.__cilkrts_worker* ()*)()
  store %struct.__cilkrts_worker* %34, %struct.__cilkrts_worker** %w, align 8
  %35 = load %struct.__cilkrts_worker** %w, align 8
  %36 = getelementptr inbounds %struct.__cilkrts_worker* %35, i32 0, i32 5
  %37 = load i32* %36, align 4
  %38 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([53 x i8]* @.str6, i32 0, i32 0), i32 %37)
  %39 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 0
  %40 = load i32* %39
  %41 = and i32 %40, 2
  %42 = icmp eq i32 %41, 0
  br i1 %42, label %__cilk_sync.exit, label %cilk.sync.savestate.i

cilk.sync.savestate.i:                            ; preds = %33
  %43 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %44 = load %__cilkrts_worker** %43
  %45 = getelementptr inbounds %__cilkrts_worker* %44, i32 0, i32 12
  %46 = load %__cilkrts_pedigree* %45
  %47 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 9
  store %__cilkrts_pedigree %46, %__cilkrts_pedigree* %47
  %48 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %49 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %48, i16* %49)
  %50 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %51 = call i8* @llvm.frameaddress(i32 0)
  %52 = getelementptr inbounds [5 x i8*]* %50, i32 0, i32 0
  store i8* %51, i8** %52
  %53 = call i8* @llvm.stacksave()
  %54 = getelementptr inbounds [5 x i8*]* %50, i32 0, i32 2
  store i8* %53, i8** %54
  %55 = bitcast [5 x i8*]* %50 to i8*
  %56 = call i32 @llvm.eh.sjlj.setjmp(i8* %55) #8
  %57 = icmp eq i32 %56, 0
  br i1 %57, label %cilk.sync.runtimecall.i, label %cilk.sync.excepting.i

cilk.sync.runtimecall.i:                          ; preds = %cilk.sync.savestate.i
  call void @__cilkrts_sync(%__cilkrts_stack_frame* %__cilkrts_sf)
  br label %__cilk_sync.exit

cilk.sync.excepting.i:                            ; preds = %cilk.sync.savestate.i
  br label %__cilk_sync.exit

__cilk_sync.exit:                                 ; preds = %33, %cilk.sync.runtimecall.i, %cilk.sync.excepting.i
  %58 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %59 = load %__cilkrts_worker** %58
  %60 = getelementptr inbounds %__cilkrts_worker* %59, i32 0, i32 12
  %61 = getelementptr inbounds %__cilkrts_pedigree* %60, i32 0, i32 0
  %62 = load i64* %61
  %63 = add i64 %62, 1
  store i64 %63, i64* %61
  call void @__cilk_parent_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

; Function Attrs: inlinehint nounwind
define internal void @__cilk_parent_prologue(%__cilkrts_stack_frame*) #3 {
entry:
  call void @__cilkrts_enter_frame_1(%__cilkrts_stack_frame* %0)
  ret void
}

; Function Attrs: inlinehint nounwind
define available_externally void @__cilkrts_enter_frame_1(%__cilkrts_stack_frame*) #3 {
  %2 = call %__cilkrts_worker* @__cilkrts_get_tls_worker()
  %3 = icmp eq %__cilkrts_worker* %2, null
  br i1 %3, label %4, label %7

; <label>:4                                       ; preds = %1
  %5 = call %__cilkrts_worker* @__cilkrts_bind_thread_1()
  %6 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 0
  store i32 16777344, i32* %6
  br label %9

; <label>:7                                       ; preds = %1
  %8 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 0
  store i32 16777216, i32* %8
  br label %9

; <label>:9                                       ; preds = %7, %4
  %10 = phi %__cilkrts_worker* [ %5, %4 ], [ %2, %7 ]
  %11 = getelementptr inbounds %__cilkrts_worker* %10, i32 0, i32 9
  %12 = load %__cilkrts_stack_frame** %11
  %13 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 2
  store %__cilkrts_stack_frame* %12, %__cilkrts_stack_frame** %13
  %14 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 3
  store %__cilkrts_worker* %10, %__cilkrts_worker** %14
  %15 = getelementptr inbounds %__cilkrts_worker* %10, i32 0, i32 9
  store %__cilkrts_stack_frame* %0, %__cilkrts_stack_frame** %15
  ret void
}

declare %__cilkrts_worker* @__cilkrts_get_tls_worker()

declare %__cilkrts_worker* @__cilkrts_bind_thread_1()

declare void @__cilkrts_ivar_clear(i64*) #1

; Function Attrs: nounwind readnone
declare i8* @llvm.frameaddress(i32) #4

; Function Attrs: nounwind
declare i8* @llvm.stacksave() #5

; Function Attrs: nounwind
declare i32 @llvm.eh.sjlj.setjmp(i8*) #5

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper(%struct.anon* %__context) #6 {
  %1 = alloca %struct.anon*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon* %__context, %struct.anon** %1, align 8
  %2 = load %struct.anon** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %3 = getelementptr inbounds %struct.anon* %2, i32 0, i32 0
  %4 = load i64** %3, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %5 = call i64 @__cilkrts_ivar_read(i64* %4)
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

; Function Attrs: inlinehint nounwind
define internal void @__cilk_reset_worker(%__cilkrts_stack_frame*) #3 {
entry:
  %1 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 3
  store %__cilkrts_worker* null, %__cilkrts_worker** %1
  ret void
}

; Function Attrs: inlinehint nounwind
define internal void @__cilk_helper_prologue(%__cilkrts_stack_frame*) #3 {
entry:
  call void @__cilkrts_enter_frame_fast_1(%__cilkrts_stack_frame* %0)
  call void @__cilkrts_detach(%__cilkrts_stack_frame* %0)
  ret void
}

; Function Attrs: inlinehint nounwind
define available_externally void @__cilkrts_enter_frame_fast_1(%__cilkrts_stack_frame*) #3 {
  %2 = call %__cilkrts_worker* @__cilkrts_get_tls_worker()
  %3 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 0
  store i32 16777216, i32* %3
  %4 = getelementptr inbounds %__cilkrts_worker* %2, i32 0, i32 9
  %5 = load %__cilkrts_stack_frame** %4
  %6 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 2
  store %__cilkrts_stack_frame* %5, %__cilkrts_stack_frame** %6
  %7 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 3
  store %__cilkrts_worker* %2, %__cilkrts_worker** %7
  %8 = getelementptr inbounds %__cilkrts_worker* %2, i32 0, i32 9
  store %__cilkrts_stack_frame* %0, %__cilkrts_stack_frame** %8
  ret void
}

; Function Attrs: inlinehint nounwind
define internal void @__cilkrts_detach(%__cilkrts_stack_frame*) #3 {
entry:
  %1 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 3
  %2 = load %__cilkrts_worker** %1
  %3 = getelementptr inbounds %__cilkrts_worker* %2, i32 0, i32 0
  %4 = load %__cilkrts_stack_frame*** %3
  %5 = getelementptr inbounds %__cilkrts_worker* %2, i32 0, i32 12
  %6 = load %__cilkrts_pedigree* %5
  %7 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 9
  store %__cilkrts_pedigree %6, %__cilkrts_pedigree* %7
  %8 = getelementptr inbounds %__cilkrts_worker* %2, i32 0, i32 12
  %9 = load %__cilkrts_pedigree* %8
  %10 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 2
  %11 = load %__cilkrts_stack_frame** %10
  %12 = getelementptr inbounds %__cilkrts_stack_frame* %11, i32 0, i32 9
  store %__cilkrts_pedigree %9, %__cilkrts_pedigree* %12
  %13 = getelementptr inbounds %__cilkrts_worker* %2, i32 0, i32 12
  %14 = getelementptr inbounds %__cilkrts_pedigree* %13, i32 0, i32 0
  store i64 0, i64* %14
  %15 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 9
  %16 = getelementptr inbounds %__cilkrts_worker* %2, i32 0, i32 12
  %17 = getelementptr inbounds %__cilkrts_pedigree* %16, i32 0, i32 1
  store %__cilkrts_pedigree* %15, %__cilkrts_pedigree** %17
  %18 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 2
  %19 = load %__cilkrts_stack_frame** %18
  store %__cilkrts_stack_frame* %19, %__cilkrts_stack_frame** %4
  %20 = getelementptr %__cilkrts_stack_frame** %4, i32 1
  %21 = getelementptr inbounds %__cilkrts_worker* %2, i32 0, i32 0
  store %__cilkrts_stack_frame** %20, %__cilkrts_stack_frame*** %21
  %22 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 0
  %23 = load i32* %22
  %24 = or i32 %23, 4
  %25 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 0
  store i32 %24, i32* %25
  ret void
}

; Function Attrs: inlinehint nounwind
define internal void @__cilk_helper_epilogue(%__cilkrts_stack_frame*) #3 {
entry:
  %1 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 3
  %2 = load %__cilkrts_worker** %1
  %3 = icmp ne %__cilkrts_worker* %2, null
  br i1 %3, label %body, label %exit

body:                                             ; preds = %entry
  call void @__cilkrts_pop_frame(%__cilkrts_stack_frame* %0)
  call void @__cilkrts_leave_frame(%__cilkrts_stack_frame* %0)
  br label %exit

exit:                                             ; preds = %body, %entry
  ret void
}

; Function Attrs: inlinehint nounwind
define internal void @__cilkrts_pop_frame(%__cilkrts_stack_frame*) #3 {
entry:
  %1 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 2
  %2 = load %__cilkrts_stack_frame** %1
  %3 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 3
  %4 = load %__cilkrts_worker** %3
  %5 = getelementptr inbounds %__cilkrts_worker* %4, i32 0, i32 9
  store %__cilkrts_stack_frame* %2, %__cilkrts_stack_frame** %5
  %6 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 2
  store %__cilkrts_stack_frame* null, %__cilkrts_stack_frame** %6
  ret void
}

declare void @__cilkrts_leave_frame(%__cilkrts_stack_frame*)

declare void @__cilkrts_ivar_write(i64*, i64) #1

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper5(%struct.anon.0* %__context) #6 {
  %1 = alloca %struct.anon.0*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.0* %__context, %struct.anon.0** %1, align 8
  %2 = load %struct.anon.0** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %3 = getelementptr inbounds %struct.anon.0* %2, i32 0, i32 0
  %4 = load i64** %3, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @bar(i64* %4)
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

declare void @__cilkrts_sync(%__cilkrts_stack_frame*)

; Function Attrs: inlinehint nounwind
define internal void @__cilk_parent_epilogue(%__cilkrts_stack_frame*) #3 {
entry:
  call void @__cilkrts_pop_frame(%__cilkrts_stack_frame* %0)
  %1 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 0
  %2 = load i32* %1
  %3 = icmp ne i32 %2, 16777216
  br i1 %3, label %4, label %exit

; <label>:4                                       ; preds = %entry
  call void @__cilkrts_leave_frame(%__cilkrts_stack_frame* %0)
  br label %exit

exit:                                             ; preds = %4, %entry
  ret void
}

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  call void @__cilk_parent_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %4 = alloca %struct.anon.1, align 1
  %5 = alloca i32
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  br label %6

; <label>:6                                       ; preds = %0
  %7 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %8 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %7, i16* %8)
  %9 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %10 = call i8* @llvm.frameaddress(i32 0)
  %11 = getelementptr inbounds [5 x i8*]* %9, i32 0, i32 0
  store i8* %10, i8** %11
  %12 = call i8* @llvm.stacksave()
  %13 = getelementptr inbounds [5 x i8*]* %9, i32 0, i32 2
  store i8* %12, i8** %13
  %14 = bitcast [5 x i8*]* %9 to i8*
  %15 = call i32 @llvm.eh.sjlj.setjmp(i8* %14) #8
  %16 = icmp eq i32 %15, 0
  br i1 %16, label %17, label %18

; <label>:17                                      ; preds = %6
  call void @__cilk_spawn_helper7(%struct.anon.1* %4)
  br label %18

; <label>:18                                      ; preds = %17, %6
  %19 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str8, i32 0, i32 0))
  store i32 0, i32* %1
  store i32 1, i32* %5
  %20 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 0
  %21 = load i32* %20
  %22 = and i32 %21, 2
  %23 = icmp eq i32 %22, 0
  br i1 %23, label %__cilk_sync.exit, label %cilk.sync.savestate.i

cilk.sync.savestate.i:                            ; preds = %18
  %24 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %25 = load %__cilkrts_worker** %24
  %26 = getelementptr inbounds %__cilkrts_worker* %25, i32 0, i32 12
  %27 = load %__cilkrts_pedigree* %26
  %28 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 9
  store %__cilkrts_pedigree %27, %__cilkrts_pedigree* %28
  %29 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %30 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %29, i16* %30)
  %31 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %32 = call i8* @llvm.frameaddress(i32 0)
  %33 = getelementptr inbounds [5 x i8*]* %31, i32 0, i32 0
  store i8* %32, i8** %33
  %34 = call i8* @llvm.stacksave()
  %35 = getelementptr inbounds [5 x i8*]* %31, i32 0, i32 2
  store i8* %34, i8** %35
  %36 = bitcast [5 x i8*]* %31 to i8*
  %37 = call i32 @llvm.eh.sjlj.setjmp(i8* %36) #8
  %38 = icmp eq i32 %37, 0
  br i1 %38, label %cilk.sync.runtimecall.i, label %cilk.sync.excepting.i

cilk.sync.runtimecall.i:                          ; preds = %cilk.sync.savestate.i
  call void @__cilkrts_sync(%__cilkrts_stack_frame* %__cilkrts_sf)
  br label %__cilk_sync.exit

cilk.sync.excepting.i:                            ; preds = %cilk.sync.savestate.i
  br label %__cilk_sync.exit

__cilk_sync.exit:                                 ; preds = %18, %cilk.sync.runtimecall.i, %cilk.sync.excepting.i
  %39 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %40 = load %__cilkrts_worker** %39
  %41 = getelementptr inbounds %__cilkrts_worker* %40, i32 0, i32 12
  %42 = getelementptr inbounds %__cilkrts_pedigree* %41, i32 0, i32 0
  %43 = load i64* %42
  %44 = add i64 %43, 1
  store i64 %44, i64* %42
  call void @__cilk_parent_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %45 = load i32* %1
  ret i32 %45
}

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper7(%struct.anon.1* %__context) #6 {
  %1 = alloca %struct.anon.1*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.1* %__context, %struct.anon.1** %1, align 8
  %2 = load %struct.anon.1** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @fun()
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noreturn nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { inlinehint nounwind }
attributes #4 = { nounwind readnone }
attributes #5 = { nounwind }
attributes #6 = { noinline nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { noreturn nounwind }
attributes #8 = { returns_twice }

!cilk.spawn = !{!0, !1, !2}
!cilk.sync = !{!3}

!0 = metadata !{void (%struct.anon*)* @__cilk_spawn_helper}
!1 = metadata !{void (%struct.anon.0*)* @__cilk_spawn_helper5}
!2 = metadata !{void (%struct.anon.1*)* @__cilk_spawn_helper7}
!3 = metadata !{null}
