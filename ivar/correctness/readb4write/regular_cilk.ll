; ModuleID = 'regular_cilk.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%__cilkrts_stack_frame = type { i32, i32, %__cilkrts_stack_frame*, %__cilkrts_worker*, i8*, [5 x i8*], i32, i16, i16, %__cilkrts_pedigree }
%__cilkrts_worker = type { %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, i32, i8*, i8*, i8*, %__cilkrts_stack_frame*, %__cilkrts_stack_frame**, i8*, %__cilkrts_pedigree }
%__cilkrts_pedigree = type { i64, %__cilkrts_pedigree* }
%struct.anon = type {}

@.str = private unnamed_addr constant [27 x i8] c"var read successfully: %d\0A\00", align 1

; Function Attrs: nounwind uwtable
define void @fun() #0 {
  %i = alloca i32, align 4
  store i32 39, i32* %i, align 4
  %1 = load i32* %i, align 4
  %2 = icmp ne i32 %1, 39
  br i1 %2, label %3, label %4

; <label>:3                                       ; preds = %0
  call void @abort() #7
  unreachable

; <label>:4                                       ; preds = %0
  %5 = load i32* %i, align 4
  %6 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([27 x i8]* @.str, i32 0, i32 0), i32 %5)
  ret void
}

; Function Attrs: noreturn nounwind
declare void @abort() #1

declare i32 @printf(i8*, ...) #2

; Function Attrs: nounwind uwtable
define i32 @main(i32 %argc, i8** %argv) #0 {
  %1 = alloca i32, align 4
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  call void @__cilk_parent_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %2 = alloca i32, align 4
  %3 = alloca i8**, align 8
  %4 = alloca %struct.anon, align 1
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
  call void @__cilk_spawn_helper(%struct.anon* %4)
  br label %18

; <label>:18                                      ; preds = %17, %6
  %19 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 0
  %20 = load i32* %19
  %21 = and i32 %20, 2
  %22 = icmp eq i32 %21, 0
  br i1 %22, label %__cilk_sync.exit, label %cilk.sync.savestate.i

cilk.sync.savestate.i:                            ; preds = %18
  %23 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %24 = load %__cilkrts_worker** %23
  %25 = getelementptr inbounds %__cilkrts_worker* %24, i32 0, i32 12
  %26 = load %__cilkrts_pedigree* %25
  %27 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 9
  store %__cilkrts_pedigree %26, %__cilkrts_pedigree* %27
  %28 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %29 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %28, i16* %29)
  %30 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %31 = call i8* @llvm.frameaddress(i32 0)
  %32 = getelementptr inbounds [5 x i8*]* %30, i32 0, i32 0
  store i8* %31, i8** %32
  %33 = call i8* @llvm.stacksave()
  %34 = getelementptr inbounds [5 x i8*]* %30, i32 0, i32 2
  store i8* %33, i8** %34
  %35 = bitcast [5 x i8*]* %30 to i8*
  %36 = call i32 @llvm.eh.sjlj.setjmp(i8* %35) #8
  %37 = icmp eq i32 %36, 0
  br i1 %37, label %cilk.sync.runtimecall.i, label %cilk.sync.excepting.i

cilk.sync.runtimecall.i:                          ; preds = %cilk.sync.savestate.i
  call void @__cilkrts_sync(%__cilkrts_stack_frame* %__cilkrts_sf)
  br label %__cilk_sync.exit

cilk.sync.excepting.i:                            ; preds = %cilk.sync.savestate.i
  br label %__cilk_sync.exit

__cilk_sync.exit:                                 ; preds = %18, %cilk.sync.runtimecall.i, %cilk.sync.excepting.i
  %38 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %39 = load %__cilkrts_worker** %38
  %40 = getelementptr inbounds %__cilkrts_worker* %39, i32 0, i32 12
  %41 = getelementptr inbounds %__cilkrts_pedigree* %40, i32 0, i32 0
  %42 = load i64* %41
  %43 = add i64 %42, 1
  store i64 %43, i64* %41
  store i32 0, i32* %1
  store i32 1, i32* %5
  %44 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 0
  %45 = load i32* %44
  %46 = and i32 %45, 2
  %47 = icmp eq i32 %46, 0
  br i1 %47, label %__cilk_sync.exit4, label %cilk.sync.savestate.i1

cilk.sync.savestate.i1:                           ; preds = %__cilk_sync.exit
  %48 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %49 = load %__cilkrts_worker** %48
  %50 = getelementptr inbounds %__cilkrts_worker* %49, i32 0, i32 12
  %51 = load %__cilkrts_pedigree* %50
  %52 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 9
  store %__cilkrts_pedigree %51, %__cilkrts_pedigree* %52
  %53 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %54 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %53, i16* %54)
  %55 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %56 = call i8* @llvm.frameaddress(i32 0)
  %57 = getelementptr inbounds [5 x i8*]* %55, i32 0, i32 0
  store i8* %56, i8** %57
  %58 = call i8* @llvm.stacksave()
  %59 = getelementptr inbounds [5 x i8*]* %55, i32 0, i32 2
  store i8* %58, i8** %59
  %60 = bitcast [5 x i8*]* %55 to i8*
  %61 = call i32 @llvm.eh.sjlj.setjmp(i8* %60) #8
  %62 = icmp eq i32 %61, 0
  br i1 %62, label %cilk.sync.runtimecall.i2, label %cilk.sync.excepting.i3

cilk.sync.runtimecall.i2:                         ; preds = %cilk.sync.savestate.i1
  call void @__cilkrts_sync(%__cilkrts_stack_frame* %__cilkrts_sf)
  br label %__cilk_sync.exit4

cilk.sync.excepting.i3:                           ; preds = %cilk.sync.savestate.i1
  br label %__cilk_sync.exit4

__cilk_sync.exit4:                                ; preds = %__cilk_sync.exit, %cilk.sync.runtimecall.i2, %cilk.sync.excepting.i3
  %63 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %64 = load %__cilkrts_worker** %63
  %65 = getelementptr inbounds %__cilkrts_worker* %64, i32 0, i32 12
  %66 = getelementptr inbounds %__cilkrts_pedigree* %65, i32 0, i32 0
  %67 = load i64* %66
  %68 = add i64 %67, 1
  store i64 %68, i64* %66
  call void @__cilk_parent_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %69 = load i32* %1
  ret i32 %69
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
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @fun()
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

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { noreturn nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { inlinehint nounwind }
attributes #4 = { nounwind readnone }
attributes #5 = { nounwind }
attributes #6 = { noinline nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { noreturn nounwind }
attributes #8 = { returns_twice }

!cilk.spawn = !{!0}
!cilk.sync = !{!1}

!0 = metadata !{void (%struct.anon*)* @__cilk_spawn_helper}
!1 = metadata !{null}
