!RUN: %flang_fc1 -emit-hlfir -fopenmp %s -o - | FileCheck %s

subroutine target_teams_loop
    implicit none
    integer :: x, i

    !$omp target teams loop
    do i = 0, 10
      x = x + i
    end do
end subroutine target_teams_loop

!CHECK-LABEL: func.func @_QPtarget_teams_loop
!CHECK:         omp.target map_entries(
!CHECK-SAME:      %{{.*}} -> %[[I_ARG:[^[:space:]]+]],
!CHECK-SAME:      %{{.*}} -> %[[X_ARG:[^[:space:]]+]] : {{.*}}) {

!CHECK:           %[[I_DECL:.*]]:2 = hlfir.declare %[[I_ARG]]
!CHECK:           %[[X_DECL:.*]]:2 = hlfir.declare %[[X_ARG]]

!CHECK:           omp.teams {

! TODO we probably need to move the `loop_nest` bounds ops from the `teams`
! region to the `parallel` region to avoid making these values `shared`. We can
! find the backward slices of these bounds that are within the `teams` region
! and move these slices to the `parallel` op.
!CHECK:             %[[LB:.*]] = arith.constant 0 : i32
!CHECK:             %[[UB:.*]] = arith.constant 10 : i32
!CHECK:             %[[STEP:.*]] = arith.constant 1 : i32

!CHECK:             omp.parallel private(@{{.*}} %[[I_DECL]]#0 
!CHECK-SAME:          -> %[[I_PRIV_ARG:[^[:space:]]+]] : !fir.ref<i32>) {
!CHECK:               omp.distribute {
!CHECK:                 omp.wsloop {

!CHECK:                   omp.loop_nest (%{{.*}}) : i32 = 
!CHECK-SAME:                (%[[LB]]) to (%[[UB]]) inclusive step (%[[STEP]]) {
!CHECK:                     %[[I_PRIV_DECL:.*]]:2 = hlfir.declare %[[I_PRIV_ARG]]
!CHECK:                     fir.store %{{.*}} to %[[I_PRIV_DECL]]#1 : !fir.ref<i32>
!CHECK:                   }
!CHECK:                 }
!CHECK:               }
!CHECK:             }
!CHECK:           }
!CHECK:         }
