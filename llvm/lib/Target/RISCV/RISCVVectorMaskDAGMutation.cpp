//===- RISCVVectorMaskDAGMutation.cpp - RISCV Vector Mask DAGMutation -----===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// A schedule mutation that add a dependency between masks producing
// instructions and masked instructions, so that we will not extend the live
// interval of mask register.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/RISCVBaseInfo.h"
#include "MCTargetDesc/RISCVMCTargetDesc.h"
#include "RISCVRegisterInfo.h"
#include "RISCVTargetMachine.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/ScheduleDAGInstrs.h"
#include "llvm/CodeGen/ScheduleDAGMutation.h"
#include "llvm/TargetParser/RISCVTargetParser.h"

#define DEBUG_TYPE "machine-scheduler"

namespace llvm {

static inline bool isVectorMaskProducer(const MachineInstr *MI) {
  switch (RISCV::getRVVMCOpcode(MI->getOpcode())) {
  // Vector Mask Instructions
  case RISCV::VMAND_MM:
  case RISCV::VMNAND_MM:
  case RISCV::VMANDN_MM:
  case RISCV::VMXOR_MM:
  case RISCV::VMOR_MM:
  case RISCV::VMNOR_MM:
  case RISCV::VMORN_MM:
  case RISCV::VMXNOR_MM:
  case RISCV::VMSBF_M:
  case RISCV::VMSIF_M:
  case RISCV::VMSOF_M:
  // Vector Integer Add-with-Carry / Subtract-with-Borrow Instructions
  case RISCV::VMADC_VV:
  case RISCV::VMADC_VX:
  case RISCV::VMADC_VI:
  case RISCV::VMADC_VVM:
  case RISCV::VMADC_VXM:
  case RISCV::VMADC_VIM:
  case RISCV::VMSBC_VV:
  case RISCV::VMSBC_VX:
  case RISCV::VMSBC_VVM:
  case RISCV::VMSBC_VXM:
  // Vector Integer Compare Instructions
  case RISCV::VMSEQ_VV:
  case RISCV::VMSEQ_VX:
  case RISCV::VMSEQ_VI:
  case RISCV::VMSNE_VV:
  case RISCV::VMSNE_VX:
  case RISCV::VMSNE_VI:
  case RISCV::VMSLT_VV:
  case RISCV::VMSLT_VX:
  case RISCV::VMSLTU_VV:
  case RISCV::VMSLTU_VX:
  case RISCV::VMSLE_VV:
  case RISCV::VMSLE_VX:
  case RISCV::VMSLE_VI:
  case RISCV::VMSLEU_VV:
  case RISCV::VMSLEU_VX:
  case RISCV::VMSLEU_VI:
  case RISCV::VMSGTU_VX:
  case RISCV::VMSGTU_VI:
  case RISCV::VMSGT_VX:
  case RISCV::VMSGT_VI:
  // Vector Floating-Point Compare Instructions
  case RISCV::VMFEQ_VV:
  case RISCV::VMFEQ_VF:
  case RISCV::VMFNE_VV:
  case RISCV::VMFNE_VF:
  case RISCV::VMFLT_VV:
  case RISCV::VMFLT_VF:
  case RISCV::VMFLE_VV:
  case RISCV::VMFLE_VF:
  case RISCV::VMFGT_VF:
  case RISCV::VMFGE_VF:
    return true;
  }
  return false;
}

class RISCVVectorMaskDAGMutation : public ScheduleDAGMutation {
private:
  const TargetRegisterInfo *TRI;

public:
  RISCVVectorMaskDAGMutation(const TargetRegisterInfo *TRI) : TRI(TRI) {}

  void apply(ScheduleDAGInstrs *DAG) override {
    SUnit *NearestUseV0SU = nullptr;
    for (SUnit &SU : DAG->SUnits) {
      const MachineInstr *MI = SU.getInstr();
      if (MI->findRegisterUseOperand(RISCV::V0, TRI))
        NearestUseV0SU = &SU;

      if (NearestUseV0SU && NearestUseV0SU != &SU && isVectorMaskProducer(MI) &&
          // For LMUL=8 cases, there will be more possibilities to spill.
          // FIXME: We should use RegPressureTracker to do fine-grained
          // controls.
          RISCVII::getLMul(MI->getDesc().TSFlags) != RISCVII::LMUL_8)
        DAG->addEdge(&SU, SDep(NearestUseV0SU, SDep::Artificial));
    }
  }
};

std::unique_ptr<ScheduleDAGMutation>
createRISCVVectorMaskDAGMutation(const TargetRegisterInfo *TRI) {
  return std::make_unique<RISCVVectorMaskDAGMutation>(TRI);
}

} // namespace llvm
