; ModuleID = 'delayed_writer.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%struct.timespec = type { i64, i64 }
%__cilkrts_stack_frame = type { i32, i32, %__cilkrts_stack_frame*, %__cilkrts_worker*, i8*, [5 x i8*], i32, i16, i16, %__cilkrts_pedigree }
%__cilkrts_worker = type { %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, %__cilkrts_stack_frame**, i32, i8*, i8*, i8*, %__cilkrts_stack_frame*, %__cilkrts_stack_frame**, i8*, %__cilkrts_pedigree }
%__cilkrts_pedigree = type { i64, %__cilkrts_pedigree* }
%struct.anon = type { i64* }
%struct.anon.0 = type { i64* }
%struct.anon.1 = type { i64* }
%struct.anon.2 = type { i64* }
%struct.anon.3 = type { i64* }
%struct.anon.4 = type { i64* }
%struct.__cilkrts_worker = type { %struct.__cilkrts_stack_frame**, %struct.__cilkrts_stack_frame**, %struct.__cilkrts_stack_frame**, %struct.__cilkrts_stack_frame**, %struct.__cilkrts_stack_frame**, i32, %struct.global_state_t*, %struct.local_state*, %struct.cilkred_map*, %struct.__cilkrts_stack_frame*, i8*, %struct.__cilkrts_worker_sysdep_state* }
%struct.__cilkrts_stack_frame = type { i32, i32, %struct.__cilkrts_stack_frame*, %struct.__cilkrts_worker*, i8*, [5 x i8*] }
%struct.global_state_t = type opaque
%struct.local_state = type opaque
%struct.cilkred_map = type opaque
%struct.__cilkrts_worker_sysdep_state = type opaque
%struct.anon.5 = type {}

@.str = private unnamed_addr constant [25 x i8] c"iv1* %p iv2* %p iv3* %p\0A\00", align 1
@.str1 = private unnamed_addr constant [14 x i8] c"in fun sf %p\0A\00", align 1
@.str7 = private unnamed_addr constant [30 x i8] c"sum of ivars is correct! (6)\0A\00", align 1
@.str8 = private unnamed_addr constant [14 x i8] c"sum wrong!! \0A\00", align 1
@.str9 = private unnamed_addr constant [17 x i8] c"above fun sf %p\0A\00", align 1
@.str11 = private unnamed_addr constant [17 x i8] c"below fun sf %p\0A\00", align 1
@.str12 = private unnamed_addr constant [27 x i8] c"<<<<<<<<<<<<<<< below fun\0A\00", align 1

; Function Attrs: nounwind uwtable
define void @__cilkrts_msleep(i64 %millis) #0 {
  %1 = alloca i64, align 8
  %time = alloca %struct.timespec, align 8
  store i64 %millis, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = udiv i64 %2, 1000
  %4 = getelementptr inbounds %struct.timespec* %time, i32 0, i32 0
  store i64 %3, i64* %4, align 8
  %5 = load i64* %1, align 8
  %6 = urem i64 %5, 1000
  %7 = mul i64 %6, 1000000
  %8 = getelementptr inbounds %struct.timespec* %time, i32 0, i32 1
  store i64 %7, i64* %8, align 8
  %9 = call i32 @nanosleep(%struct.timespec* %time, %struct.timespec* null)
  ret void
}

declare i32 @nanosleep(%struct.timespec*, %struct.timespec*) #1

; Function Attrs: nounwind uwtable
define void @__cilkrts_usleep(i64 %micros) #0 {
  %1 = alloca i64, align 8
  %time = alloca %struct.timespec, align 8
  store i64 %micros, i64* %1, align 8
  %2 = load i64* %1, align 8
  %3 = udiv i64 %2, 1000000
  %4 = getelementptr inbounds %struct.timespec* %time, i32 0, i32 0
  store i64 %3, i64* %4, align 8
  %5 = load i64* %1, align 8
  %6 = urem i64 %5, 1000
  %7 = mul i64 %6, 1000000
  %8 = getelementptr inbounds %struct.timespec* %time, i32 0, i32 1
  store i64 %7, i64* %8, align 8
  %9 = call i32 @nanosleep(%struct.timespec* %time, %struct.timespec* null)
  ret void
}

; Function Attrs: nounwind uwtable
define void @fun() #0 {
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  call void @__cilk_parent_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %iv1 = alloca i64, align 8
  %iv2 = alloca i64, align 8
  %iv3 = alloca i64, align 8
  %i1 = alloca i32, align 4
  %1 = alloca %struct.anon, align 8
  %i2 = alloca i32, align 4
  %2 = alloca %struct.anon.0, align 8
  %i3 = alloca i32, align 4
  %3 = alloca %struct.anon.1, align 8
  %4 = alloca %struct.anon.2, align 8
  %5 = alloca %struct.anon.3, align 8
  %6 = alloca %struct.anon.4, align 8
  %7 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([25 x i8]* @.str, i32 0, i32 0), i64* %iv1, i64* %iv2, i64* %iv3)
  %8 = call %struct.__cilkrts_worker* bitcast (%__cilkrts_worker* ()* @__cilkrts_get_tls_worker to %struct.__cilkrts_worker* ()*)()
  %9 = getelementptr inbounds %struct.__cilkrts_worker* %8, i32 0, i32 9
  %10 = load %struct.__cilkrts_stack_frame** %9, align 8
  %11 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([14 x i8]* @.str1, i32 0, i32 0), %struct.__cilkrts_stack_frame* %10)
  call void @__cilkrts_ivar_clear(i64* %iv1)
  call void @__cilkrts_ivar_clear(i64* %iv2)
  call void @__cilkrts_ivar_clear(i64* %iv3)
  br label %12

; <label>:12                                      ; preds = %0
  %13 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %14 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %13, i16* %14)
  %15 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %16 = call i8* @llvm.frameaddress(i32 0)
  %17 = getelementptr inbounds [5 x i8*]* %15, i32 0, i32 0
  store i8* %16, i8** %17
  %18 = call i8* @llvm.stacksave()
  %19 = getelementptr inbounds [5 x i8*]* %15, i32 0, i32 2
  store i8* %18, i8** %19
  %20 = bitcast [5 x i8*]* %15 to i8*
  %21 = call i32 @llvm.eh.sjlj.setjmp(i8* %20) #6
  %22 = icmp eq i32 %21, 0
  br i1 %22, label %23, label %25

; <label>:23                                      ; preds = %12
  %24 = getelementptr inbounds %struct.anon* %1, i32 0, i32 0
  store i64* %iv1, i64** %24, align 8
  call void @__cilk_spawn_helper(%struct.anon* %1, i32* %i1)
  br label %25

; <label>:25                                      ; preds = %23, %12
  call void @__cilkrts_usleep(i64 950000)
  br label %26

; <label>:26                                      ; preds = %25
  %27 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %28 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %27, i16* %28)
  %29 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %30 = call i8* @llvm.frameaddress(i32 0)
  %31 = getelementptr inbounds [5 x i8*]* %29, i32 0, i32 0
  store i8* %30, i8** %31
  %32 = call i8* @llvm.stacksave()
  %33 = getelementptr inbounds [5 x i8*]* %29, i32 0, i32 2
  store i8* %32, i8** %33
  %34 = bitcast [5 x i8*]* %29 to i8*
  %35 = call i32 @llvm.eh.sjlj.setjmp(i8* %34) #6
  %36 = icmp eq i32 %35, 0
  br i1 %36, label %37, label %39

; <label>:37                                      ; preds = %26
  %38 = getelementptr inbounds %struct.anon.0* %2, i32 0, i32 0
  store i64* %iv2, i64** %38, align 8
  call void @__cilk_spawn_helper2(%struct.anon.0* %2, i32* %i2)
  br label %39

; <label>:39                                      ; preds = %37, %26
  call void @__cilkrts_usleep(i64 950000)
  br label %40

; <label>:40                                      ; preds = %39
  %41 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %42 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %41, i16* %42)
  %43 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %44 = call i8* @llvm.frameaddress(i32 0)
  %45 = getelementptr inbounds [5 x i8*]* %43, i32 0, i32 0
  store i8* %44, i8** %45
  %46 = call i8* @llvm.stacksave()
  %47 = getelementptr inbounds [5 x i8*]* %43, i32 0, i32 2
  store i8* %46, i8** %47
  %48 = bitcast [5 x i8*]* %43 to i8*
  %49 = call i32 @llvm.eh.sjlj.setjmp(i8* %48) #6
  %50 = icmp eq i32 %49, 0
  br i1 %50, label %51, label %53

; <label>:51                                      ; preds = %40
  %52 = getelementptr inbounds %struct.anon.1* %3, i32 0, i32 0
  store i64* %iv3, i64** %52, align 8
  call void @__cilk_spawn_helper3(%struct.anon.1* %3, i32* %i3)
  br label %53

; <label>:53                                      ; preds = %51, %40
  br label %54

; <label>:54                                      ; preds = %53
  %55 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %56 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %55, i16* %56)
  %57 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %58 = call i8* @llvm.frameaddress(i32 0)
  %59 = getelementptr inbounds [5 x i8*]* %57, i32 0, i32 0
  store i8* %58, i8** %59
  %60 = call i8* @llvm.stacksave()
  %61 = getelementptr inbounds [5 x i8*]* %57, i32 0, i32 2
  store i8* %60, i8** %61
  %62 = bitcast [5 x i8*]* %57 to i8*
  %63 = call i32 @llvm.eh.sjlj.setjmp(i8* %62) #6
  %64 = icmp eq i32 %63, 0
  br i1 %64, label %65, label %67

; <label>:65                                      ; preds = %54
  %66 = getelementptr inbounds %struct.anon.2* %4, i32 0, i32 0
  store i64* %iv1, i64** %66, align 8
  call void @__cilk_spawn_helper4(%struct.anon.2* %4)
  br label %67

; <label>:67                                      ; preds = %65, %54
  br label %68

; <label>:68                                      ; preds = %67
  %69 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %70 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %69, i16* %70)
  %71 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %72 = call i8* @llvm.frameaddress(i32 0)
  %73 = getelementptr inbounds [5 x i8*]* %71, i32 0, i32 0
  store i8* %72, i8** %73
  %74 = call i8* @llvm.stacksave()
  %75 = getelementptr inbounds [5 x i8*]* %71, i32 0, i32 2
  store i8* %74, i8** %75
  %76 = bitcast [5 x i8*]* %71 to i8*
  %77 = call i32 @llvm.eh.sjlj.setjmp(i8* %76) #6
  %78 = icmp eq i32 %77, 0
  br i1 %78, label %79, label %81

; <label>:79                                      ; preds = %68
  %80 = getelementptr inbounds %struct.anon.3* %5, i32 0, i32 0
  store i64* %iv2, i64** %80, align 8
  call void @__cilk_spawn_helper5(%struct.anon.3* %5)
  br label %81

; <label>:81                                      ; preds = %79, %68
  br label %82

; <label>:82                                      ; preds = %81
  %83 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %84 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %83, i16* %84)
  %85 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %86 = call i8* @llvm.frameaddress(i32 0)
  %87 = getelementptr inbounds [5 x i8*]* %85, i32 0, i32 0
  store i8* %86, i8** %87
  %88 = call i8* @llvm.stacksave()
  %89 = getelementptr inbounds [5 x i8*]* %85, i32 0, i32 2
  store i8* %88, i8** %89
  %90 = bitcast [5 x i8*]* %85 to i8*
  %91 = call i32 @llvm.eh.sjlj.setjmp(i8* %90) #6
  %92 = icmp eq i32 %91, 0
  br i1 %92, label %93, label %95

; <label>:93                                      ; preds = %82
  %94 = getelementptr inbounds %struct.anon.4* %6, i32 0, i32 0
  store i64* %iv3, i64** %94, align 8
  call void @__cilk_spawn_helper6(%struct.anon.4* %6)
  br label %95

; <label>:95                                      ; preds = %93, %82
  %96 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 0
  %97 = load i32* %96
  %98 = and i32 %97, 2
  %99 = icmp eq i32 %98, 0
  br i1 %99, label %__cilk_sync.exit, label %cilk.sync.savestate.i

cilk.sync.savestate.i:                            ; preds = %95
  %100 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %101 = load %__cilkrts_worker** %100
  %102 = getelementptr inbounds %__cilkrts_worker* %101, i32 0, i32 12
  %103 = load %__cilkrts_pedigree* %102
  %104 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 9
  store %__cilkrts_pedigree %103, %__cilkrts_pedigree* %104
  %105 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %106 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %105, i16* %106)
  %107 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %108 = call i8* @llvm.frameaddress(i32 0)
  %109 = getelementptr inbounds [5 x i8*]* %107, i32 0, i32 0
  store i8* %108, i8** %109
  %110 = call i8* @llvm.stacksave()
  %111 = getelementptr inbounds [5 x i8*]* %107, i32 0, i32 2
  store i8* %110, i8** %111
  %112 = bitcast [5 x i8*]* %107 to i8*
  %113 = call i32 @llvm.eh.sjlj.setjmp(i8* %112) #6
  %114 = icmp eq i32 %113, 0
  br i1 %114, label %cilk.sync.runtimecall.i, label %cilk.sync.excepting.i

cilk.sync.runtimecall.i:                          ; preds = %cilk.sync.savestate.i
  call void @__cilkrts_sync(%__cilkrts_stack_frame* %__cilkrts_sf)
  br label %__cilk_sync.exit

cilk.sync.excepting.i:                            ; preds = %cilk.sync.savestate.i
  br label %__cilk_sync.exit

__cilk_sync.exit:                                 ; preds = %95, %cilk.sync.runtimecall.i, %cilk.sync.excepting.i
  %115 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %116 = load %__cilkrts_worker** %115
  %117 = getelementptr inbounds %__cilkrts_worker* %116, i32 0, i32 12
  %118 = getelementptr inbounds %__cilkrts_pedigree* %117, i32 0, i32 0
  %119 = load i64* %118
  %120 = add i64 %119, 1
  store i64 %120, i64* %118
  %121 = load i32* %i1, align 4
  %122 = load i32* %i2, align 4
  %123 = add nsw i32 %121, %122
  %124 = load i32* %i3, align 4
  %125 = add nsw i32 %123, %124
  %126 = icmp eq i32 6, %125
  br i1 %126, label %127, label %129

; <label>:127                                     ; preds = %__cilk_sync.exit
  %128 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([30 x i8]* @.str7, i32 0, i32 0))
  br label %131

; <label>:129                                     ; preds = %__cilk_sync.exit
  %130 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([14 x i8]* @.str8, i32 0, i32 0))
  br label %131

; <label>:131                                     ; preds = %129, %127
  %132 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 0
  %133 = load i32* %132
  %134 = and i32 %133, 2
  %135 = icmp eq i32 %134, 0
  br i1 %135, label %__cilk_sync.exit4, label %cilk.sync.savestate.i1

cilk.sync.savestate.i1:                           ; preds = %131
  %136 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %137 = load %__cilkrts_worker** %136
  %138 = getelementptr inbounds %__cilkrts_worker* %137, i32 0, i32 12
  %139 = load %__cilkrts_pedigree* %138
  %140 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 9
  store %__cilkrts_pedigree %139, %__cilkrts_pedigree* %140
  %141 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %142 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %141, i16* %142)
  %143 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %144 = call i8* @llvm.frameaddress(i32 0)
  %145 = getelementptr inbounds [5 x i8*]* %143, i32 0, i32 0
  store i8* %144, i8** %145
  %146 = call i8* @llvm.stacksave()
  %147 = getelementptr inbounds [5 x i8*]* %143, i32 0, i32 2
  store i8* %146, i8** %147
  %148 = bitcast [5 x i8*]* %143 to i8*
  %149 = call i32 @llvm.eh.sjlj.setjmp(i8* %148) #6
  %150 = icmp eq i32 %149, 0
  br i1 %150, label %cilk.sync.runtimecall.i2, label %cilk.sync.excepting.i3

cilk.sync.runtimecall.i2:                         ; preds = %cilk.sync.savestate.i1
  call void @__cilkrts_sync(%__cilkrts_stack_frame* %__cilkrts_sf)
  br label %__cilk_sync.exit4

cilk.sync.excepting.i3:                           ; preds = %cilk.sync.savestate.i1
  br label %__cilk_sync.exit4

__cilk_sync.exit4:                                ; preds = %131, %cilk.sync.runtimecall.i2, %cilk.sync.excepting.i3
  %151 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %152 = load %__cilkrts_worker** %151
  %153 = getelementptr inbounds %__cilkrts_worker* %152, i32 0, i32 12
  %154 = getelementptr inbounds %__cilkrts_pedigree* %153, i32 0, i32 0
  %155 = load i64* %154
  %156 = add i64 %155, 1
  store i64 %156, i64* %154
  call void @__cilk_parent_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

; Function Attrs: inlinehint nounwind
define internal void @__cilk_parent_prologue(%__cilkrts_stack_frame*) #2 {
entry:
  call void @__cilkrts_enter_frame_1(%__cilkrts_stack_frame* %0)
  ret void
}

; Function Attrs: inlinehint nounwind
define available_externally void @__cilkrts_enter_frame_1(%__cilkrts_stack_frame*) #2 {
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

declare i32 @printf(i8*, ...) #1

declare void @__cilkrts_ivar_clear(i64*) #1

; Function Attrs: nounwind readnone
declare i8* @llvm.frameaddress(i32) #3

; Function Attrs: nounwind
declare i8* @llvm.stacksave() #4

; Function Attrs: nounwind
declare i32 @llvm.eh.sjlj.setjmp(i8*) #4

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper(%struct.anon* %__context, i32*) #5 {
  %2 = alloca %struct.anon*, align 8
  %3 = alloca i32*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon* %__context, %struct.anon** %2, align 8
  store i32* %0, i32** %3, align 8
  %4 = load %struct.anon** %2
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %5 = getelementptr inbounds %struct.anon* %4, i32 0, i32 0
  %6 = load i64** %5, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %7 = call i64 @__cilkrts_ivar_read(i64* %6)
  %8 = trunc i64 %7 to i32
  store i32 %8, i32* %0, align 4
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

; Function Attrs: inlinehint nounwind
define internal void @__cilk_reset_worker(%__cilkrts_stack_frame*) #2 {
entry:
  %1 = getelementptr inbounds %__cilkrts_stack_frame* %0, i32 0, i32 3
  store %__cilkrts_worker* null, %__cilkrts_worker** %1
  ret void
}

declare i64 @__cilkrts_ivar_read(i64*) #1

; Function Attrs: inlinehint nounwind
define internal void @__cilk_helper_prologue(%__cilkrts_stack_frame*) #2 {
entry:
  call void @__cilkrts_enter_frame_fast_1(%__cilkrts_stack_frame* %0)
  call void @__cilkrts_detach(%__cilkrts_stack_frame* %0)
  ret void
}

; Function Attrs: inlinehint nounwind
define available_externally void @__cilkrts_enter_frame_fast_1(%__cilkrts_stack_frame*) #2 {
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
define internal void @__cilkrts_detach(%__cilkrts_stack_frame*) #2 {
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
define internal void @__cilk_helper_epilogue(%__cilkrts_stack_frame*) #2 {
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
define internal void @__cilkrts_pop_frame(%__cilkrts_stack_frame*) #2 {
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
define internal void @__cilk_spawn_helper2(%struct.anon.0* %__context, i32*) #5 {
  %2 = alloca %struct.anon.0*, align 8
  %3 = alloca i32*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.0* %__context, %struct.anon.0** %2, align 8
  store i32* %0, i32** %3, align 8
  %4 = load %struct.anon.0** %2
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %5 = getelementptr inbounds %struct.anon.0* %4, i32 0, i32 0
  %6 = load i64** %5, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %7 = call i64 @__cilkrts_ivar_read(i64* %6)
  %8 = trunc i64 %7 to i32
  store i32 %8, i32* %0, align 4
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper3(%struct.anon.1* %__context, i32*) #5 {
  %2 = alloca %struct.anon.1*, align 8
  %3 = alloca i32*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.1* %__context, %struct.anon.1** %2, align 8
  store i32* %0, i32** %3, align 8
  %4 = load %struct.anon.1** %2
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %5 = getelementptr inbounds %struct.anon.1* %4, i32 0, i32 0
  %6 = load i64** %5, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %7 = call i64 @__cilkrts_ivar_read(i64* %6)
  %8 = trunc i64 %7 to i32
  store i32 %8, i32* %0, align 4
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper4(%struct.anon.2* %__context) #5 {
  %1 = alloca %struct.anon.2*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.2* %__context, %struct.anon.2** %1, align 8
  %2 = load %struct.anon.2** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %3 = getelementptr inbounds %struct.anon.2* %2, i32 0, i32 0
  %4 = load i64** %3, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @__cilkrts_ivar_write(i64* %4, i64 1)
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

declare void @__cilkrts_ivar_write(i64*, i64) #1

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper5(%struct.anon.3* %__context) #5 {
  %1 = alloca %struct.anon.3*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.3* %__context, %struct.anon.3** %1, align 8
  %2 = load %struct.anon.3** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %3 = getelementptr inbounds %struct.anon.3* %2, i32 0, i32 0
  %4 = load i64** %3, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @__cilkrts_ivar_write(i64* %4, i64 2)
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper6(%struct.anon.4* %__context) #5 {
  %1 = alloca %struct.anon.4*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.4* %__context, %struct.anon.4** %1, align 8
  %2 = load %struct.anon.4** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  %3 = getelementptr inbounds %struct.anon.4* %2, i32 0, i32 0
  %4 = load i64** %3, align 8
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @__cilkrts_ivar_write(i64* %4, i64 3)
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

declare void @__cilkrts_sync(%__cilkrts_stack_frame*)

; Function Attrs: inlinehint nounwind
define internal void @__cilk_parent_epilogue(%__cilkrts_stack_frame*) #2 {
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
  %4 = alloca %struct.anon.5, align 1
  store i32 0, i32* %1
  store i32 %argc, i32* %2, align 4
  store i8** %argv, i8*** %3, align 8
  %5 = call %struct.__cilkrts_worker* bitcast (%__cilkrts_worker* ()* @__cilkrts_get_tls_worker to %struct.__cilkrts_worker* ()*)()
  %6 = getelementptr inbounds %struct.__cilkrts_worker* %5, i32 0, i32 9
  %7 = load %struct.__cilkrts_stack_frame** %6, align 8
  %8 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([17 x i8]* @.str9, i32 0, i32 0), %struct.__cilkrts_stack_frame* %7)
  br label %9

; <label>:9                                       ; preds = %0
  %10 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %11 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %10, i16* %11)
  %12 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %13 = call i8* @llvm.frameaddress(i32 0)
  %14 = getelementptr inbounds [5 x i8*]* %12, i32 0, i32 0
  store i8* %13, i8** %14
  %15 = call i8* @llvm.stacksave()
  %16 = getelementptr inbounds [5 x i8*]* %12, i32 0, i32 2
  store i8* %15, i8** %16
  %17 = bitcast [5 x i8*]* %12 to i8*
  %18 = call i32 @llvm.eh.sjlj.setjmp(i8* %17) #6
  %19 = icmp eq i32 %18, 0
  br i1 %19, label %20, label %21

; <label>:20                                      ; preds = %9
  call void @__cilk_spawn_helper10(%struct.anon.5* %4)
  br label %21

; <label>:21                                      ; preds = %20, %9
  %22 = call %struct.__cilkrts_worker* bitcast (%__cilkrts_worker* ()* @__cilkrts_get_tls_worker to %struct.__cilkrts_worker* ()*)()
  %23 = getelementptr inbounds %struct.__cilkrts_worker* %22, i32 0, i32 9
  %24 = load %struct.__cilkrts_stack_frame** %23, align 8
  %25 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([17 x i8]* @.str11, i32 0, i32 0), %struct.__cilkrts_stack_frame* %24)
  %26 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([27 x i8]* @.str12, i32 0, i32 0))
  %27 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 0
  %28 = load i32* %27
  %29 = and i32 %28, 2
  %30 = icmp eq i32 %29, 0
  br i1 %30, label %__cilk_sync.exit, label %cilk.sync.savestate.i

cilk.sync.savestate.i:                            ; preds = %21
  %31 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %32 = load %__cilkrts_worker** %31
  %33 = getelementptr inbounds %__cilkrts_worker* %32, i32 0, i32 12
  %34 = load %__cilkrts_pedigree* %33
  %35 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 9
  store %__cilkrts_pedigree %34, %__cilkrts_pedigree* %35
  %36 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 6
  %37 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 7
  call void asm sideeffect "stmxcsr $0\0A\09fnstcw $1", "*m,*m,~{dirflag},~{fpsr},~{flags}"(i32* %36, i16* %37)
  %38 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 5
  %39 = call i8* @llvm.frameaddress(i32 0)
  %40 = getelementptr inbounds [5 x i8*]* %38, i32 0, i32 0
  store i8* %39, i8** %40
  %41 = call i8* @llvm.stacksave()
  %42 = getelementptr inbounds [5 x i8*]* %38, i32 0, i32 2
  store i8* %41, i8** %42
  %43 = bitcast [5 x i8*]* %38 to i8*
  %44 = call i32 @llvm.eh.sjlj.setjmp(i8* %43) #6
  %45 = icmp eq i32 %44, 0
  br i1 %45, label %cilk.sync.runtimecall.i, label %cilk.sync.excepting.i

cilk.sync.runtimecall.i:                          ; preds = %cilk.sync.savestate.i
  call void @__cilkrts_sync(%__cilkrts_stack_frame* %__cilkrts_sf)
  br label %__cilk_sync.exit

cilk.sync.excepting.i:                            ; preds = %cilk.sync.savestate.i
  br label %__cilk_sync.exit

__cilk_sync.exit:                                 ; preds = %21, %cilk.sync.runtimecall.i, %cilk.sync.excepting.i
  %46 = getelementptr inbounds %__cilkrts_stack_frame* %__cilkrts_sf, i32 0, i32 3
  %47 = load %__cilkrts_worker** %46
  %48 = getelementptr inbounds %__cilkrts_worker* %47, i32 0, i32 12
  %49 = getelementptr inbounds %__cilkrts_pedigree* %48, i32 0, i32 0
  %50 = load i64* %49
  %51 = add i64 %50, 1
  store i64 %51, i64* %49
  call void @__cilk_parent_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  %52 = load i32* %1
  ret i32 %52
}

; Function Attrs: noinline nounwind uwtable
define internal void @__cilk_spawn_helper10(%struct.anon.5* %__context) #5 {
  %1 = alloca %struct.anon.5*, align 8
  %__cilkrts_sf = alloca %__cilkrts_stack_frame
  store %struct.anon.5* %__context, %struct.anon.5** %1, align 8
  %2 = load %struct.anon.5** %1
  call void @__cilk_reset_worker(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @__cilk_helper_prologue(%__cilkrts_stack_frame* %__cilkrts_sf)
  call void @fun()
  call void @__cilk_helper_epilogue(%__cilkrts_stack_frame* %__cilkrts_sf)
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { inlinehint nounwind }
attributes #3 = { nounwind readnone }
attributes #4 = { nounwind }
attributes #5 = { noinline nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { returns_twice }

!cilk.spawn = !{!0, !1, !2, !3, !4, !5, !6}
!cilk.sync = !{!7}

!0 = metadata !{void (%struct.anon*, i32*)* @__cilk_spawn_helper}
!1 = metadata !{void (%struct.anon.0*, i32*)* @__cilk_spawn_helper2}
!2 = metadata !{void (%struct.anon.1*, i32*)* @__cilk_spawn_helper3}
!3 = metadata !{void (%struct.anon.2*)* @__cilk_spawn_helper4}
!4 = metadata !{void (%struct.anon.3*)* @__cilk_spawn_helper5}
!5 = metadata !{void (%struct.anon.4*)* @__cilk_spawn_helper6}
!6 = metadata !{void (%struct.anon.5*)* @__cilk_spawn_helper10}
!7 = metadata !{null}
