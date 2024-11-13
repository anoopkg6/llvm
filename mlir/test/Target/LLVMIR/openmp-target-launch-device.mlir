// RUN: mlir-translate -mlir-to-llvmir %s | FileCheck %s

// CHECK:      @[[EXEC_MODE1:.*]] = weak protected constant i8 1
// CHECK:      @llvm.compiler.used{{.*}} = appending global [1 x ptr] [ptr @[[EXEC_MODE1]]], section "llvm.metadata"
// CHECK:      @[[KERNEL1_ENV:.*_kernel_environment]] = weak_odr protected constant %struct.KernelEnvironmentTy {
// CHECK-SAME: %struct.ConfigurationEnvironmentTy { i8 1, i8 1, i8 [[EXEC_MODE1:1]], i32 [[MIN_THREADS1:1]], i32 [[MAX_THREADS1:10]], i32 [[MIN_TEAMS1:1]], i32 [[MAX_TEAMS1:-1]], i32 0, i32 0 },
// CHECK-SAME: ptr @{{.*}}, ptr @{{.*}} }

// CHECK:      @[[EXEC_MODE2:.*]] = weak protected constant i8 1
// CHECK:      @llvm.compiler.used{{.*}} = appending global [1 x ptr] [ptr @[[EXEC_MODE2]]], section "llvm.metadata"
// CHECK:      @[[KERNEL2_ENV:.*_kernel_environment]] = weak_odr protected constant %struct.KernelEnvironmentTy {
// CHECK-SAME: %struct.ConfigurationEnvironmentTy { i8 1, i8 1, i8 [[EXEC_MODE2:1]], i32 [[MIN_THREADS2:1]], i32 [[MAX_THREADS2:50]], i32 [[MIN_TEAMS2:50]], i32 [[MAX_TEAMS2:50]], i32 0, i32 0 },
// CHECK-SAME: ptr @{{.*}}, ptr @{{.*}} }

module attributes {llvm.target_triple = "amdgcn-amd-amdhsa", omp.is_target_device = true, omp.is_gpu = true} {
  llvm.func @main(%num_teams : i32) {
    // CHECK: define weak_odr protected amdgpu_kernel void @__omp_offloading_{{.*}}_main_l{{[0-9]+}}(ptr %[[KERNEL_ARGS:.*]], i64 %[[NUM_TEAMS_ARG:.*]]) #[[ATTRS1:[0-9]+]]
    // CHECK: %{{.*}} = call i32 @__kmpc_target_init(ptr @[[KERNEL1_ENV]], ptr %[[KERNEL_ARGS]])
    %target_threads = llvm.mlir.constant(20) : i32
    %teams_threads = llvm.mlir.constant(10) : i32
    omp.target thread_limit(%target_threads : i32)
               host_eval(%num_teams -> %arg_teams, %teams_threads -> %arg_teams_threads : i32, i32) {
      omp.teams num_teams(to %arg_teams : i32) thread_limit(%arg_teams_threads : i32) {
        omp.terminator
      }
      omp.terminator
    }

    // CHECK: define weak_odr protected amdgpu_kernel void @__omp_offloading_{{.*}}_main_l{{[0-9]+}}(ptr %[[KERNEL_ARGS:.*]]) #[[ATTRS2:[0-9]+]]
    // CHECK: %{{.*}} = call i32 @__kmpc_target_init(ptr @[[KERNEL2_ENV]], ptr %[[KERNEL_ARGS]])
    %const_val = llvm.mlir.constant(50) : i32
    omp.target thread_limit(%const_val : i32) host_eval(%const_val -> %arg_const_val : i32) {
      omp.teams num_teams(to %arg_const_val : i32) {
        omp.terminator
      }
      omp.terminator
    }
    llvm.return
  }
}

// CHECK: attributes #[[ATTRS1]] = { "amdgpu-flat-work-group-size"="[[MIN_THREADS1]],[[MAX_THREADS1]]" "omp_target_thread_limit"="[[MAX_THREADS1]]" }
// CHECK: attributes #[[ATTRS2]] = { "amdgpu-flat-work-group-size"="[[MIN_THREADS2]],[[MAX_THREADS2]]" "amdgpu-max-num-workgroups"="[[MIN_TEAMS2]],1,1" "omp_target_num_teams"="[[MIN_TEAMS2]]" "omp_target_thread_limit"="[[MIN_TEAMS2]]" }
