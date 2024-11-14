! The "thread_limit" clause was added to the "target" construct in OpenMP 5.1.
! RUN: %flang_fc1 -emit-hlfir -fopenmp -fopenmp-version=51 %s -o - | FileCheck %s
! RUN: %flang_fc1 -emit-hlfir -fopenmp -fopenmp-version=51 -fopenmp-is-target-device %s -o - | FileCheck %s

! CHECK-LABEL: func.func @_QPteams
subroutine teams()
  ! CHECK: omp.target
  ! CHECK-SAME: host_eval(%{{.*}} -> %[[NUM_TEAMS:.*]], %{{.*}} -> %[[THREAD_LIMIT:.*]] : i32, i32)
  !$omp target

  ! CHECK: omp.teams
  ! CHECK-SAME: num_teams( to %[[NUM_TEAMS]] : i32) thread_limit(%[[THREAD_LIMIT]] : i32)
  !$omp teams num_teams(1) thread_limit(2)
  call foo()
  !$omp end teams

  !$omp end target

  ! CHECK: omp.teams
  ! CHECK-SAME: num_teams({{.*}}) thread_limit({{.*}}) {
  !$omp teams num_teams(1) thread_limit(2)
  call foo()
  !$omp end teams
end subroutine teams

! CHECK-LABEL: func.func @_QPdistribute_parallel_do
subroutine distribute_parallel_do()
  ! CHECK: omp.target
  ! CHECK-SAME: host_eval(%{{.*}} -> %[[LB:.*]], %{{.*}} -> %[[UB:.*]], %{{.*}} -> %[[STEP:.*]], %{{.*}} -> %[[NUM_THREADS:.*]] : i32, i32, i32, i32)
  
  ! CHECK: omp.teams
  !$omp target teams

  ! CHECK: omp.parallel
  ! CHECK-SAME: num_threads(%[[NUM_THREADS]] : i32)

  ! CHECK: omp.distribute
  ! CHECK-NEXT: omp.wsloop
  ! CHECK-NEXT: omp.loop_nest
  ! CHECK-SAME: (%{{.*}}) : i32 = (%[[LB]]) to (%[[UB]]) inclusive step (%[[STEP]])
  !$omp distribute parallel do num_threads(1)
  do i=1,10
    call foo()
  end do
  !$omp end distribute parallel do
  !$omp end target teams

  ! CHECK: omp.target
  ! CHECK-NOT: host_eval({{.*}})
  ! CHECK-SAME: {
  ! CHECK: omp.teams
  !$omp target teams
  call foo() !< Prevents this from being SPMD.

  ! CHECK: omp.parallel
  ! CHECK-SAME: num_threads({{.*}})
  ! CHECK: omp.distribute
  ! CHECK-NEXT: omp.wsloop
  !$omp distribute parallel do num_threads(1)
  do i=1,10
    call foo()
  end do
  !$omp end distribute parallel do
  !$omp end target teams

  ! CHECK: omp.teams
  !$omp teams

  ! CHECK: omp.parallel
  ! CHECK-SAME: num_threads({{.*}})
  ! CHECK: omp.distribute
  ! CHECK-NEXT: omp.wsloop
  !$omp distribute parallel do num_threads(1)
  do i=1,10
    call foo()
  end do
  !$omp end distribute parallel do
  !$omp end teams
end subroutine distribute_parallel_do

! CHECK-LABEL: func.func @_QPdistribute_parallel_do_simd
subroutine distribute_parallel_do_simd()
  ! CHECK: omp.target
  ! CHECK-SAME: host_eval(%{{.*}} -> %[[LB:.*]], %{{.*}} -> %[[UB:.*]], %{{.*}} -> %[[STEP:.*]], %{{.*}} -> %[[NUM_THREADS:.*]] : i32, i32, i32, i32)

  ! CHECK: omp.teams
  !$omp target teams

  ! CHECK: omp.parallel
  ! CHECK-SAME: num_threads(%[[NUM_THREADS]] : i32)

  ! CHECK: omp.distribute
  ! CHECK-NEXT: omp.wsloop
  ! CHECK-NEXT: omp.simd
  ! CHECK-NEXT: omp.loop_nest

  ! CHECK-SAME: (%{{.*}}) : i32 = (%[[LB]]) to (%[[UB]]) inclusive step (%[[STEP]])
  !$omp distribute parallel do simd num_threads(1)
  do i=1,10
    call foo()
  end do
  !$omp end distribute parallel do simd
  !$omp end target teams

  ! CHECK: omp.target
  ! CHECK-NOT: host_eval({{.*}})
  ! CHECK-SAME: {
  ! CHECK: omp.teams
  !$omp target teams
  call foo() !< Prevents this from being SPMD.

  ! CHECK: omp.parallel
  ! CHECK-SAME: num_threads({{.*}})
  ! CHECK: omp.distribute
  ! CHECK-NEXT: omp.wsloop
  ! CHECK-NEXT: omp.simd
  !$omp distribute parallel do simd num_threads(1)
  do i=1,10
    call foo()
  end do
  !$omp end distribute parallel do simd
  !$omp end target teams

  ! CHECK: omp.teams
  !$omp teams

  ! CHECK: omp.parallel
  ! CHECK-SAME: num_threads({{.*}})
  ! CHECK: omp.distribute
  ! CHECK-NEXT: omp.wsloop
  ! CHECK-NEXT: omp.simd
  !$omp distribute parallel do simd num_threads(1)
  do i=1,10
    call foo()
  end do
  !$omp end distribute parallel do simd
  !$omp end teams
end subroutine distribute_parallel_do_simd
