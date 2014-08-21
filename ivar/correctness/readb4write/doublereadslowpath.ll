; ModuleID = 'doublereadslowpath.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%__cilkrts_stack_frame = type { i32, i32, %__cilkrts_stack_frame*, %__cilkrts_worker*, i8*, [5 x i8*], i32, i16, i16, %__cilkrts_pedigree }
%__cilkrts_worker = type { %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, i32, i8*, i8*, i8*, %__cilkrts_stack_frame*, %__cilkrts_stack_frame**, i8*, %__cilkrts_pedigree }
%__cilkrts_pedigree = type { i64, %__cilkrts_pedigree* }
%struct.anon = type { i64* }
%struct.anon.0 = type { i64* }
%struct.anon.1 = type { i64* }

@.str = private unnamed_addr constant [53 x i8] c"TEST ERROR - BAD VALUE, %d, EXPECTED %d - ABORTING!\0A\00", align 1
@.str1 = private unnamed_addr constant [22 x i8] c"reading i in bar: %d\0A\00", align 1
@.str2 = private unnamed_addr constant [34 x i8] c"1st read before write -slow path\0A\00", align 1
@.str3 = private unnamed_addr constant [34 x i8] c"2nd read before write -slow path\0A\00", align 1
@.str5 = private unnamed_addr constant [12 x i8] c"write ivar\0A\00", align 1
@.str6 = private unnamed_addr constant [29 x i8] c"read after write -fast path\0A\00", align 1
@.str8 = private unnamed_addr constant [46 x i8] c"returning from fun...this will pop the frame\0A\00", align 1
@.str9 = private unnamed_addr constant [15 x i8] c"test complete\0A\00", align 1

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
  %3 = alloca %struct.anon.1, align 8
  call void @__cilkrts_ivar_clear(i64* %iv)
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([34 x i8]* @.str2, i32 0, i32 0))
  br label %5

; <label>:5                                       ; preds = %0
  %6 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %7 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %6, i16* %7)
  %8 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %9 = call i8* @llvm.frameaddress(i32 0)
  %10 = getelementptr inbounds [5 x i8*]* %8, i32 0, i32 0
  store i8* %9, i8** %10
  %11 = call i8* @llvm.stacksave()
  %12 = getelementptr inbounds [5 x i8*]* %8, i32 0, i32 2
  store i8* %11, i8** %12
  %13 = bitcast [5 x i8*]* %8 to i8*
  %14 = call i32 @llvm.eh.sjlj.setjmp(i8* %13) #8
  %15 = icmp eq i32 %14, 0
  br i1 %15, label %16, label %18

; <label>:16                                      ; preds = %5
  %17 = getelementptr inbounds %struct.anon* %1, i32 0, i32 0
  store i64* %iv, i64** %17, align 8
  call void @__cilk_spawn_helper(%struct.anon* %1)
  br label %18

; <label>:18                                      ; preds = %16, %5
  %19 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([34 x i8]* @.str3, i32 0, i32 0))
  br label %20

; <label>:20                                      ; preds = %18
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
  call void @__cilk_spawn_helper4(%struct.anon.0* %2)
  br label %33

; <label>:33                                      ; preds = %31, %20
  %34 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([12 x i8]* @.str5, i32 0, i32 0))
  call void @__cilkrts_ivar_write(i64* %iv, i64 39)
  %35 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([29 x i8]* @.str6, i32 0, i32 0))
  br label %36

; <label>:36                                      ; preds = %33
  %37 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %38 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %37, i16* %38)
  %39 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %40 = call i8* @llvm.frameaddress(i32 0)
  %41 = getelementptr inbounds [5 x i8*]* %39, i32 0, i32 0
  store i8* %40, i8** %41
  %42 = call i8* @llvm.stacksave()
  %43 = getelementptr inbounds [5 x i8*]* %39, i32 0, i32 2
  store i8* %42, i8** %43
  %44 = bitcast [5 x i8*]* %39 to i8*
  %45 = call i32 @llvm.eh.sjlj.setjmp(i8* %44) #8
  %46 = icmp eq i32 %45, 0
  br i1 %46, label %47, label %49

; <label>:47                                      ; preds = %36
  %48 = getelementptr inbounds %struct.anon.1* %3, i32 0, i32 0
  store i64* %iv, i64** %48, align 8
  call void @__cilk_spawn_helper7(%struct.anon.1* %3)
  br label %49

; <label>:49                                      ; preds = %47, %36
  %50 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([46 x i8]* @.str8, i32 0, i32 0))
  %51 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 0
  %52 = load i32* %51
  %53 = and i32 %52, 2
  %54 = icmp eq i32 %53, 0
  br i1 %54, label %__cilk_sync.exit, label %cilk.sync.savestate.i

cilk.sync.savestate.i:                            ; preds = %49
  %55 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %56 = load %__cilkrts_worker** %55
  %57 = getelementptr inbounds %__cilkrts_worker* %56, i32 0, i32 12
  %58 = load %__cilkrts_pedigree* %57
  %59 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 9
  store %__cilkrts_pedigree %58, %__cilkrts_pedigree* %59
  %60 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %61 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %60, i16* %61)
  %62 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %63 = call i8* @llvm.frameaddress(i32 0)
  %64 = getelementptr inbounds [5 x i8*]* %62, i32 0, i32 0
  store i8* %63, i8** %64
  %65 = call i8* @llvm.stacksave()
  %66 = getelementptr inbounds [5 x i8*]* %62, i32 0, i32 2
  store i8* %65, i8** %66
  %67 = bitcast [5 x i8*]* %62 to i8*
  %68 = call i32 @llvm.eh.sjlj.setjmp(i8* %67) #8
  %69 = icmp eq i32 %68, 0
  br i1 %69, label %cilk.sync.runtimecall.i, label %cilk.sync.excepting.i

cilk.sync.runtimecall.i:                          ; preds = %cilk.sync.savestate.i
  call void @__cilkrts_sync(%__cilkrts_stack_frame* %__cilkrts_sf)
  br label %__cilk_sync.exit

cilk.sync.excepting.i:                            ; preds = %cilk.sync.savestate.i
  br label %__cilk_sync.exit

__cilk_sync.exit:                                 ; preds = %49, %cilk.sync.runtimecall.i, %cilk.sync.excepting.i
  %70 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %71 = load %__cilkrts_worker** %70
  %72 = getelementptr inbounds %__cilkrts_worker* %71, i32 0, i32 12
  %73 = getelementptr inbounds %__cilkrts_pedigree* %72, i32 0, i32 0
  %74 = load i64* %73
  %75 = add i64 %74, 1
  store i64 %75, i64* %73
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

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper4(%struct.anon.0* %__context) #6 {
  %1 = alloca %struct.anon.0*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.0* %__context, %struct.anon.0** %1, align 8
  %2 = load %struct.anon.0** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %3 = getelementptr inbounds %struct.anon.0* %2, i32 0, i32 0
  %4 = load i64** %3, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %5 = call i64 @__cilkrts_ivar_read(i64* %4)
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

declare void @__cilkrts_ivar_write(i64*, i64) #1

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper7(%struct.anon.1* %__context) #6 {
  %1 = alloca %struct.anon.1*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.1* %__context, %struct.anon.1** %1, align 8
  %2 = load %struct.anon.1** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %3 = getelementptr inbounds %struct.anon.1* %2, i32 0, i32 0
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
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  call void @fun()
  %4 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([15 x i8]* @.str9, i32 0, i32 0))
  ret i32 0
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
!1 = metadata !{void (%struct.anon.0*)* @__cilk_spawn_helper4}
!2 = metadata !{void (%struct.anon.1*)* @__cilk_spawn_helper7}
!3 = metadata !{null}
