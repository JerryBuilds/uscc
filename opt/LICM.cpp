//
//  LICM.cpp
//  uscc
//
//  Implements basic loop invariant code motion
//
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------
#include "Passes.h"
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/ValueTracking.h>
#pragma clang diagnostic pop

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool LICM::runOnLoop(llvm::Loop *L, llvm::LPPassManager &LPM)
{
	mChanged = false;
	
	// PA5: Implement
    
    // Initialization
    mChanged = false;
    mCurrLoop = L; // Save the current loop
    mLoopInfo = &getAnalysis<LoopInfo>(); // Grab the loop info
    mDomTree = &getAnalysis<DominatorTreeWrapperPass>().getDomTree(); // Grab the dominator tree
    
    // start
    hoistPreOrder(mDomTree->getNode(mCurrLoop->getHeader()));
	
	return mChanged;
}

void LICM::getAnalysisUsage(AnalysisUsage &Info) const
{
	// PA5: Implement
    
    // LICM does not modify the CFG
    Info.setPreservesCFG();
    // Execute after dead blocks have been removed
    Info.addRequired<DeadBlocks>();
    // Use the built-in Dominator tree and loop info passes
    Info.addRequired<DominatorTreeWrapperPass>();
    Info.addRequired<LoopInfo>();
}

bool LICM::isSafeToHoistInstr(Instruction* instr)
{
    if (mCurrLoop->hasLoopInvariantOperands(instr)
        && isSafeToSpeculativelyExecute(instr)
        && (isa<BinaryOperator>(instr)
            || isa<CastInst>(instr)
            || isa<SelectInst>(instr)
            || isa<GetElementPtrInst>(instr)
            || isa<CmpInst>(instr))) {
        return true;
    }
    return false;
}

void LICM::hoistInstr(llvm::Instruction* instr)
{
    instr->moveBefore(mCurrLoop->getLoopPreheader()->getTerminator());
    mChanged = true;
}

void LICM::hoistPreOrder(llvm::DomTreeNode* dtn)
{
    // get basic block associated with DomTreeNode
    BasicBlock* block = dtn->getBlock();
    
    // if block is in current loop
    if (mLoopInfo->getLoopFor(block))
    {
        // loop through each instruction in block
        BasicBlock::iterator instrIter = block->begin();
        while (instrIter != block->end())
        {
            llvm::Instruction* currentInstr = instrIter;
            ++instrIter; // to avoid getting stuck in infinite loop
                    // this step ensures the iterator doesn't point at preheader block
            if (isSafeToHoistInstr(currentInstr))
            {
                hoistInstr(currentInstr);
            }
        }
    }
    
    // loop through children of DomTreeNode recursively
    std::vector<DomTreeNodeBase<llvm::BasicBlock>*> dtn_children = dtn->getChildren();
    for (DomTreeNodeBase<llvm::BasicBlock>* dtnb : dtn_children)
    {
        hoistPreOrder(dtnb);
    }
}
	
} // opt
} // uscc

char uscc::opt::LICM::ID = 0;
