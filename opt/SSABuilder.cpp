//
//  SSABuilder.cpp
//  uscc
//
//  Implements SSABuilder class
//  
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------

#include "SSABuilder.h"
#include "../parse/Symbols.h"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#pragma clang diagnostic pop

#include <list>

using namespace uscc::opt;
using namespace uscc::parse;
using namespace llvm;

// Called when a new function is started to clear out all the data
void SSABuilder::reset()
{
	// PA4: Implement
    
    for (auto & val : mVarDefs) {
        delete val.second;
    }
    mVarDefs.clear();
    
    for (auto & val : mIncompletePhis) {
        delete val.second;
    }
    mIncompletePhis.clear();
    
    mSealedBlocks.clear();
}

// For a specific variable in a specific basic block, write its value
void SSABuilder::writeVariable(Identifier* var, BasicBlock* block, Value* value)
{
	// PA4: Implement

    SubMap * myVarMap = mVarDefs[block];
    (*myVarMap)[var] = value;
}

// Read the value assigned to the variable in the requested basic block
// Will recursively search predecessor blocks if it was not written in this block
Value* SSABuilder::readVariable(Identifier* var, BasicBlock* block)
{
	// PA4: Implement
    
    SubMap * myVarMap = mVarDefs[block];
    
    // check if variable is in current block
    if (myVarMap->find(var) != myVarMap->end()) {
        return myVarMap->at(var);
    }
	
	return readVariableRecursive(var, block);
}

// This is called to add a new block to the maps
void SSABuilder::addBlock(BasicBlock* block, bool isSealed /* = false */)
{
	// PA4: Implement
    
    mVarDefs.insert({block, new SubMap});
    mIncompletePhis.insert({block, new SubPHI});
    
    if (isSealed) {
        sealBlock(block);
    }
}

// This is called when a block is "sealed" which means it will not have any
// further predecessors added. It will complete any PHI nodes (if necessary)
void SSABuilder::sealBlock(llvm::BasicBlock* block)
{
	// PA4: Implement
    
    SubPHI* mySubPhi = mIncompletePhis[block];
    for (auto it = mySubPhi->begin(), E = mySubPhi->end(); it != E; ++it) {
        addPhiOperands(it->first, it->second);
    }
    mSealedBlocks.insert(block);
}

// Recursively search predecessor blocks for a variable
Value* SSABuilder::readVariableRecursive(Identifier* var, BasicBlock* block)
{
	Value* retVal = nullptr;
	
	// PA4: Implement
    
    // count predecessors
    int predCount = 0;
    for (pred_iterator PI = pred_begin(block), E = pred_end(block); PI != E; ++PI) {
        ++predCount;
    }
    
    // 3 Cases
    
    // Case 1: Block is not sealed
    if (mSealedBlocks.find(block) == mSealedBlocks.end()) {
        llvm::PHINode * phi;
        if (block->empty()) {
            phi = PHINode::Create(var->llvmType(), 0, var->getName(), block);
        }
        else {
            phi = PHINode::Create(var->llvmType(), 0, var->getName(), &block->front());
        }
        mIncompletePhis[block]->insert({var, phi});
        retVal = phi;
    }
    
    // Case 2: Block is sealed, and has only 1 predecessor
    else if (mSealedBlocks.find(block) != mSealedBlocks.end()
             && predCount == 1) {
        retVal = readVariable(var, *pred_begin(block));
    }
    
    // Case 3: Block is sealed, and has multiple predecessors
    else {
        llvm::PHINode * phi;
        if (block->empty()) {
            phi = PHINode::Create(var->llvmType(), predCount, var->getName(), block);
        }
        else {
            phi = PHINode::Create(var->llvmType(), predCount, var->getName(), &block->front());
        }
        retVal = phi;
        writeVariable(var, block, retVal);
        retVal = addPhiOperands(var, phi);
    }
    
    writeVariable(var, block, retVal);
    
	return retVal;
}

// Adds phi operands based on predecessors of the containing block
Value* SSABuilder::addPhiOperands(Identifier* var, PHINode* phi)
{
	// PA4: Implement
	
    for (pred_iterator PI = pred_begin(phi->getParent()), E = pred_end(phi->getParent()); PI != E; ++PI) {
        phi->addIncoming(readVariable(var, *PI), *PI);
    }
    
	return tryRemoveTrivialPhi(phi);
}

// Removes trivial phi nodes
Value* SSABuilder::tryRemoveTrivialPhi(llvm::PHINode* phi)
{
	Value* same = nullptr;
	
	// PA4: Implement
    
    for (int i=0; i < phi->getNumIncomingValues(); ++i) {
        llvm::Value* op = phi->getIncomingValue(i);
        if (op == same || op == phi) {
            continue; // unique value or self-reference
        }
        if (same != nullptr) {
            return phi; // phi merges at least two values: not trivial
        }
        same = op;
    }
    
    if (same == nullptr) {
        same = UndefValue::get(phi->getType()); // phi is unreachable or in start block
    }
    
    phi->replaceAllUsesWith(same); // reroute all uses of phi to same
    
    // remove from variable map
    for (auto MI = mVarDefs.begin(), E = mVarDefs.end(); MI != E; ++MI) {
        SubMap* mySubMap = MI->second;
        for (auto MI2 = mySubMap->begin(), E2 = mySubMap->end(); MI2 != E2; ++MI2) {
            if (MI2->second == phi) {
                MI2->second = same;
            }
        }
    }
    
    phi->eraseFromParent(); // remove phi
    
    // try to recursively remove all phi users, which might have become trivial
    for (llvm::AShrOperator::use_iterator UI = phi->use_begin(), E = phi->use_end(); UI != E; ++UI) {
        if (UI->getUser() != phi) {
            tryRemoveTrivialPhi(cast<llvm::PHINode>(UI->getUser()));
        }
    }
	
	return same;
}
