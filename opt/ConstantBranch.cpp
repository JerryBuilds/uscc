//
//  ContantBranch.cpp
//  uscc
//
//  Implements Constant Branch Folding opt pass.
//  This converts conditional branches on constants to
//  unconditional branches.
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
#pragma clang diagnostic pop
#include <set>

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool ConstantBranch::runOnFunction(Function& F)
{
	bool changed = false;
	
	// PA5: Implement
    
    // Make set that contains instructions to remove
    std::set<BranchInst*> removeSet;
    
    // Loop through each basic block
    Function::iterator blockIter = F.begin();
    while (blockIter != F.end())
    {
        // Loop through each instruction in block
        BasicBlock::iterator instrIter = blockIter->begin();
        while (instrIter != blockIter->end())
        {
            // Is this a BranchInst
            if (BranchInst* brInst = dyn_cast<BranchInst>(instrIter))
            {
                // Is it conditional, and is its condition a Constant?
                if (brInst->isConditional() && (isa<ConstantInt>(brInst->getCondition())))
                {
                    // Add to remove set
                    removeSet.insert(brInst);
                }
            }
            
            ++instrIter;
        }
        
        ++blockIter;
    }
    
    // Loop through each branch instruction to be removed
    if (removeSet.size() > 0)
    {
        changed = true;
        for (std::set<BranchInst*>::iterator brIter = removeSet.begin(); brIter != removeSet.end(); ++brIter)
        {
            // if branch's condition is true
            if (dyn_cast<ConstantInt>((*brIter)->getCondition())->isOne())
            {
                // Create new unconditional branch to left successor
                BranchInst::Create((*brIter)->getSuccessor(0), (*brIter)->getParent());
                
                // Notify right successor
                (*brIter)->getSuccessor(1)->removePredecessor((*brIter)->getParent());
            }
            // if branch's condition is false
            else
            {
                // Create new unconditional branch to right successor
                BranchInst::Create((*brIter)->getSuccessor(1), (*brIter)->getParent());
                
                // Notify left successor
                (*brIter)->getSuccessor(0)->removePredecessor((*brIter)->getParent());
            }
            // Erase old branch instruction
            (*brIter)->eraseFromParent();
        }
    }
    
	
	return changed;
}

void ConstantBranch::getAnalysisUsage(AnalysisUsage& Info) const
{
	// PA5: Implement
    
    Info.addRequired<ConstantOps>();
}
	
} // opt
} // uscc

char uscc::opt::ConstantBranch::ID = 0;
