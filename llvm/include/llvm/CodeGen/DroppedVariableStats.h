///===- DroppedVariableStats.h - Opt Diagnostics -*- C++ -*----------------===//
///
/// Part of the LLVM Project, under the Apache License v2.0 with LLVM
/// Exceptions. See https://llvm.org/LICENSE.txt for license information.
/// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
///
///===---------------------------------------------------------------------===//
/// \file
/// Dropped Variable Statistics for Debug Information. Reports any number
/// of #dbg_value that get dropped due to an optimization pass.
///
///===---------------------------------------------------------------------===//

#ifndef LLVM_CODEGEN_DROPPEDVARIABLESTATS_H
#define LLVM_CODEGEN_DROPPEDVARIABLESTATS_H

#include "llvm/CodeGen/MachinePassManager.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassInstrumentation.h"

namespace llvm {

/// A unique key that represents a #dbg_value.
using VarID =
    std::tuple<const DIScope *, const DIScope *, const DILocalVariable *>;

/// A base class to collect and print dropped debug information variable
/// statistics.
class DroppedVariableStats {
public:
  DroppedVariableStats(bool DroppedVarStatsEnabled)
      : DroppedVariableStatsEnabled(DroppedVarStatsEnabled) {
    if (DroppedVarStatsEnabled)
      llvm::outs()
          << "Pass Level, Pass Name, Num of Dropped Variables, Func or "
             "Module Name\n";
  };

  virtual ~DroppedVariableStats() = default;

  // We intend this to be unique per-compilation, thus no copies.
  DroppedVariableStats(const DroppedVariableStats &) = delete;
  void operator=(const DroppedVariableStats &) = delete;

  bool getPassDroppedVariables() { return PassDroppedVariables; }

protected:
  void setup() {
    DebugVariablesStack.push_back(
        {DenseMap<const Function *, DebugVariables>()});
    InlinedAts.push_back(
        {DenseMap<StringRef, DenseMap<VarID, DILocation *>>()});
    return;
  }

  void cleanup() {
    DebugVariablesStack.pop_back();
    InlinedAts.pop_back();
    return;
  }

  bool PassDroppedVariables = false;
  bool DroppedVariableStatsEnabled = false;

  struct DebugVariables {
    /// DenseSet of VarIDs before an optimization pass has run.
    DenseSet<VarID> DebugVariablesBefore;
    /// DenseSet of VarIDs after an optimization pass has run.
    DenseSet<VarID> DebugVariablesAfter;
  };

  /// A stack of a DenseMap, that maps DebugVariables for every pass to an
  /// llvm::Function. A stack is used because an optimization pass can call
  /// other passes.
  SmallVector<DenseMap<const Function *, DebugVariables>> DebugVariablesStack;

  /// A DenseSet tracking whether a scope was visited before.
  DenseSet<const DIScope *> VisitedScope;
  /// A stack of DenseMaps, which map the name of an llvm::Function to a
  /// DenseMap of VarIDs and their inlinedAt locations before an optimization
  /// pass has run.
  SmallVector<DenseMap<StringRef, DenseMap<VarID, DILocation *>>> InlinedAts;
  /// Remove a dropped #dbg_value VarID from all Sets in the
  /// DroppedVariablesBefore stack.
  void removeVarFromAllSets(VarID Var, const Function *F) {
    // Do not remove Var from the last element, it will be popped from the
    // stack.
    for (auto &DebugVariablesMap : llvm::drop_end(DebugVariablesStack))
      DebugVariablesMap[F].DebugVariablesBefore.erase(Var);
  }
  /// Return true if \p Scope is the same as \p DbgValScope or a child scope of
  /// \p DbgValScope, return false otherwise.
  bool isScopeChildOfOrEqualTo(const DIScope *Scope,
                               const DIScope *DbgValScope);
  /// Return true if \p InlinedAt is the same as \p DbgValInlinedAt or part of
  /// the InlinedAt chain, return false otherwise.
  bool isInlinedAtChildOfOrEqualTo(const DILocation *InlinedAt,
                                   const DILocation *DbgValInlinedAt);

  /// Calculate the number of dropped variables in an llvm::Function or
  /// llvm::MachineFunction and print the relevant information to stdout.
  void calculateDroppedStatsAndPrint(DebugVariables &DbgVariables,
                                     StringRef FuncName, StringRef PassID,
                                     StringRef FuncOrModName,
                                     StringRef PassLevel);

  /// Check if a \p Var has been dropped or is a false positive.
  bool checkDroppedStatus(DILocation *DbgLoc, const DIScope *Scope,
                          const DIScope *DbgValScope,
                          DenseMap<VarID, DILocation *> &InlinedAtsMap,
                          VarID Var);
  /// Run code to populate relevant data structures over an llvm::Function or
  /// llvm::MachineFunction.
  void run(DebugVariables &DbgVariables, StringRef FuncName, bool Before);
  /// Populate the VarIDSet and InlinedAtMap with the relevant information
  /// needed for before and after pass analysis to determine dropped variable
  /// status.
  void populateVarIDSetAndInlinedMap(
      const DILocalVariable *DbgVar, DebugLoc DbgLoc, DenseSet<VarID> &VarIDSet,
      DenseMap<StringRef, DenseMap<VarID, DILocation *>> &InlinedAtsMap,
      StringRef FuncName, bool Before);
  /// Visit every VarID in the \p DebugVariablesBeforeSet and check if it may
  /// have been dropped by an optimization pass.
  virtual void
  visitEveryDebugVariable(unsigned &DroppedCount,
                          DenseSet<VarID> &DebugVariablesBeforeSet,
                          DenseSet<VarID> &DebugVariablesAfterSet,
                          DenseMap<VarID, DILocation *> &InlinedAtsMap) = 0;
  /// Visit every debug intrinsic in an llvm::Function or llvm::MachineFunction
  /// to populate relevant data structures to determine dropped variable status.
  virtual void visitEveryDebugIntrinsic(
      DenseSet<VarID> &VarIDSet,
      DenseMap<StringRef, DenseMap<VarID, DILocation *>> &InlinedAtsMap,
      StringRef FuncName, bool Before) = 0;
};

/// A class to collect and print dropped debug information due to LLVM IR
/// optimization passes. After every LLVM IR pass is run, it will print how many
/// #dbg_values were dropped due to that pass.
class DroppedVariableStatsIR : public DroppedVariableStats {
public:
  DroppedVariableStatsIR(bool DroppedVarStatsEnabled)
      : llvm::DroppedVariableStats(DroppedVarStatsEnabled) {}

  virtual ~DroppedVariableStatsIR() = default;

  void runBeforePass(Any IR) {
    setup();
    if (const auto *M = unwrapIR<Module>(IR))
      return this->runOnModule(M, true);
    if (const auto *F = unwrapIR<Function>(IR))
      return this->runOnFunction(F, true);
  }

  void runAfterPass(StringRef P, Any IR) {
    if (const auto *M = unwrapIR<Module>(IR))
      runAfterPassModule(P, M);
    else if (const auto *F = unwrapIR<Function>(IR))
      runAfterPassFunction(P, F);
    return cleanup();
  }

  void registerCallbacks(PassInstrumentationCallbacks &PIC);

private:
  const Function *Func;

  void runAfterPassFunction(StringRef PassID, const Function *F) {
    runOnFunction(F, false);
    calculateDroppedVarStatsOnFunction(F, PassID, F->getName().str(),
                                       "Function");
  }

  void runAfterPassModule(StringRef PassID, const Module *M) {
    runOnModule(M, false);
    calculateDroppedVarStatsOnModule(M, PassID, M->getName().str(), "Module");
  }
  /// Populate DebugVariablesBefore, DebugVariablesAfter, InlinedAts before or
  /// after a pass has run to facilitate dropped variable calculation for an
  /// llvm::Function.
  void runOnFunction(const Function *F, bool Before);
  /// Iterate over all Instructions in a Function and report any dropped debug
  /// information.
  void calculateDroppedVarStatsOnFunction(const Function *F, StringRef PassID,
                                          StringRef FuncOrModName,
                                          StringRef PassLevel);
  /// Populate DebugVariablesBefore, DebugVariablesAfter, InlinedAts before or
  /// after a pass has run to facilitate dropped variable calculation for an
  /// llvm::Module. Calls runOnFunction on every Function in the Module.
  void runOnModule(const Module *M, bool Before);
  /// Iterate over all Functions in a Module and report any dropped debug
  /// information. Will call calculateDroppedVarStatsOnFunction on every
  /// Function.
  void calculateDroppedVarStatsOnModule(const Module *M, StringRef PassID,
                                        StringRef FuncOrModName,
                                        StringRef PassLevel);
  /// Override base class method to run on an llvm::Function specifically.
  virtual void visitEveryDebugVariable(
      unsigned &DroppedCount, DenseSet<VarID> &DebugVariablesBeforeSet,
      DenseSet<VarID> &DebugVariablesAfterSet,
      DenseMap<VarID, DILocation *> &InlinedAtsMap) override;
  /// Override base class method to run on #dbg_values specifically.
  virtual void visitEveryDebugIntrinsic(
      DenseSet<VarID> &VarIDSet,
      DenseMap<StringRef, DenseMap<VarID, DILocation *>> &InlinedAtsMap,
      StringRef FuncName, bool Before) override;

  template <typename IRUnitT> static const IRUnitT *unwrapIR(Any IR) {
    const IRUnitT **IRPtr = llvm::any_cast<const IRUnitT *>(&IR);
    return IRPtr ? *IRPtr : nullptr;
  }
};

} // namespace llvm

#endif
