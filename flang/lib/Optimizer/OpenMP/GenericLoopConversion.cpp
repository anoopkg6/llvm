//===- GenericLoopConversion.cpp ------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "flang/Common/OpenMP-utils.h"
#include "flang/Optimizer/Dialect/FIRDialect.h"
#include "flang/Optimizer/HLFIR/HLFIRDialect.h"
#include "flang/Optimizer/OpenMP/Passes.h"
#include "flang/Semantics/symbol.h"

#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Math/IR/Math.h"
#include "mlir/Dialect/OpenMP/OpenMPDialect.h"
#include "mlir/IR/IRMapping.h"
#include "mlir/Transforms/DialectConversion.h"

#include <memory>

namespace flangomp {
#define GEN_PASS_DEF_GENERICLOOPCONVERSIONPASS
#include "flang/Optimizer/OpenMP/Passes.h.inc"
} // namespace flangomp

namespace {

class GenericLoopConversionPattern
    : public mlir::OpConversionPattern<mlir::omp::LoopOp> {
public:
  enum class GenericLoopCombinedInfo {
    None,
    TargetTeamsLoop,
    TargetParallelLoop
  };

  using mlir::OpConversionPattern<mlir::omp::LoopOp>::OpConversionPattern;

  GenericLoopConversionPattern(mlir::MLIRContext *context)
      : OpConversionPattern(context) {}

  mlir::LogicalResult
  matchAndRewrite(mlir::omp::LoopOp loopOp, OpAdaptor adaptor,
                  mlir::ConversionPatternRewriter &rewriter) const override {
    assert(isLoopConversionSupported(loopOp));

    rewriteToDistributeParallelDo(loopOp, rewriter);
    rewriter.eraseOp(loopOp);
    return mlir::success();
  }

  static GenericLoopCombinedInfo
  findGenericLoopCombineInfo(mlir::omp::LoopOp loopOp) {
    mlir::Operation *parentOp = loopOp->getParentOp();
    GenericLoopCombinedInfo result = GenericLoopCombinedInfo::None;

    if (auto teamsOp = mlir::dyn_cast_if_present<mlir::omp::TeamsOp>(parentOp))
      if (mlir::isa<mlir::omp::TargetOp>(teamsOp->getParentOp()))
        result = GenericLoopCombinedInfo::TargetTeamsLoop;

    if (auto parallelOp =
            mlir::dyn_cast_if_present<mlir::omp::ParallelOp>(parentOp))
      if (mlir::isa<mlir::omp::TargetOp>(parallelOp->getParentOp()))
        result = GenericLoopCombinedInfo::TargetParallelLoop;

    return result;
  }

  static bool isLoopConversionSupported(mlir::omp::LoopOp loopOp) {
    GenericLoopCombinedInfo combinedInfo = findGenericLoopCombineInfo(loopOp);

    // TODO Support standalone `loop` ops and other forms of combined `loop` op
    // nests.
    if (combinedInfo != GenericLoopCombinedInfo::TargetTeamsLoop)
      return false;

    // TODO Support other clauses.
    if (loopOp.getBindKind() || loopOp.getOrder() ||
        !loopOp.getReductionVars().empty())
      return false;

    // TODO For `target teams loop`, check similar constrains to what is checked
    // by `TeamsLoopChecker` in SemaOpenMP.cpp.
    return true;
  }

  void rewriteToDistributeParallelDo(
      mlir::omp::LoopOp loopOp,
      mlir::ConversionPatternRewriter &rewriter) const {
    mlir::omp::ParallelOperands parallelClauseOps;
    parallelClauseOps.privateVars = loopOp.getPrivateVars();

    if (loopOp.getPrivateSyms())
      parallelClauseOps.privateSyms = llvm::SmallVector<mlir::Attribute>(
          loopOp.getPrivateSyms()->getAsRange<mlir::Attribute>());

    Fortran::openmp::common::EntryBlockArgs parallelArgs;
    parallelArgs.priv.vars = parallelClauseOps.privateVars;

    auto parallelOp = rewriter.create<mlir::omp::ParallelOp>(loopOp.getLoc(),
                                                             parallelClauseOps);
    mlir::Block *parallelBlock =
        genEntryBlock(rewriter, parallelArgs, parallelOp.getRegion());
    parallelOp.setComposite(true);
    rewriter.setInsertionPoint(
        rewriter.create<mlir::omp::TerminatorOp>(loopOp.getLoc()));

    mlir::omp::DistributeOperands distributeClauseOps;
    auto distributeOp = rewriter.create<mlir::omp::DistributeOp>(
        loopOp.getLoc(), distributeClauseOps);
    distributeOp.setComposite(true);
    rewriter.createBlock(&distributeOp.getRegion());

    mlir::omp::WsloopOperands wsloopClauseOps;
    auto wsloopOp =
        rewriter.create<mlir::omp::WsloopOp>(loopOp.getLoc(), wsloopClauseOps);
    wsloopOp.setComposite(true);
    rewriter.createBlock(&wsloopOp.getRegion());

    mlir::IRMapping mapper;
    mlir::Block &loopBlock = *loopOp.getRegion().begin();

    for (auto [loopOpArg, parallelOpArg] : llvm::zip_equal(
             loopBlock.getArguments(), parallelBlock->getArguments()))
      mapper.map(loopOpArg, parallelOpArg);

    rewriter.clone(*loopOp.begin(), mapper);
  }
};

class GenericLoopConversionPass
    : public flangomp::impl::GenericLoopConversionPassBase<
          GenericLoopConversionPass> {
public:
  GenericLoopConversionPass() = default;

  void runOnOperation() override {
    mlir::func::FuncOp func = getOperation();

    if (func.isDeclaration()) {
      return;
    }

    mlir::MLIRContext *context = &getContext();
    mlir::RewritePatternSet patterns(context);
    patterns.insert<GenericLoopConversionPattern>(context);
    mlir::ConversionTarget target(*context);
    target.markUnknownOpDynamicallyLegal(
        [](mlir::Operation *) { return true; });
    target.addDynamicallyLegalOp<mlir::omp::LoopOp>(
        [](mlir::omp::LoopOp loopOp) {
          return !GenericLoopConversionPattern::isLoopConversionSupported(
              loopOp);
        });

    if (mlir::failed(mlir::applyFullConversion(getOperation(), target,
                                               std::move(patterns)))) {
      mlir::emitError(mlir::UnknownLoc::get(context),
                      "error in converting `omp.loop` op");
      signalPassFailure();
    }
  }
};
} // namespace
