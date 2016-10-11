//
//  DeadBlocks.cpp
//  uscc
//
//  Implements Dead Block Removal optimization pass.
//  This removes blocks from the CFG which are unreachable.
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
#include <llvm/IR/CFG.h>
#include <llvm/ADT/DepthFirstIterator.h>
#pragma clang diagnostic pop
#include <set>

using namespace llvm;

namespace uscc
{
namespace opt
{
	
bool DeadBlocks::runOnFunction(Function& F)
{
	bool changed = false;
	
    // PA5: Implement
    
    // Make a set that contains blocks that are not dead
    std::set<BasicBlock*> visitedSet;
    
    // Make a set that contains unreachable sets (dead blocks)
    std::set<BasicBlock*> unreachableSet;
    
    // Perform DFS to tag all visited blocks
    std::deque<BasicBlock*> stack;
    stack.push_front(&F.getEntryBlock());
    while (!stack.empty())
    {
        BasicBlock* currentBlock = stack.front();
        stack.pop_front();
        for (succ_iterator succ_it = succ_begin(currentBlock); succ_it != succ_end(currentBlock); ++succ_it)
        {
            if (visitedSet.find(*succ_it) == visitedSet.end())
            {
                stack.push_front(*succ_it);
            }
        }
        visitedSet.insert(currentBlock);
    }
    
    // Loop through each block
    Function::iterator blockIter = F.begin();
    while (blockIter != F.end())
    {
        // Is this block unreachable?
        if (visitedSet.find(blockIter) == visitedSet.end())
        {
            // Add to unreachable set
            unreachableSet.insert(blockIter);
        }
        
        ++blockIter;
    }
    
    // Loop through each unreachable block
    for (BasicBlock* deadblock : unreachableSet)
    {
        // Loop through each successor
        for (succ_iterator succ_it = succ_begin(deadblock); succ_it != succ_end(deadblock); ++succ_it)
        {
            // Tell successor to remove the deadblock as predecessor
            succ_it->removePredecessor(deadblock);
        }
        // Remove deadblock
        deadblock->removeFromParent();
    }
	
	return changed;
}
	
void DeadBlocks::getAnalysisUsage(AnalysisUsage& Info) const
{
	// PA5: Implement
    
    Info.addRequired<ConstantBranch>();
}

} // opt
} // uscc

char uscc::opt::DeadBlocks::ID = 0;
