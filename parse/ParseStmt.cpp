//
//  ParseStmt.cpp
//  uscc
//
//  Implements all of the recursive descent parsing
//  functions for statement grammar rules.
//
//---------------------------------------------------------
//  Copyright (c) 2014, Sanjay Madhav
//  All rights reserved.
//
//  This file is distributed under the BSD license.
//  See LICENSE.TXT for details.
//---------------------------------------------------------

#include "Parse.h"
#include "Symbols.h"

using namespace uscc::parse;
using namespace uscc::scan;

using std::shared_ptr;
using std::make_shared;

shared_ptr<ASTDecl> Parser::parseDecl()
{
	shared_ptr<ASTDecl> retVal;
	// A decl MUST start with int or char
	if (peekIsOneOf({Token::Key_int, Token::Key_char}))
	{
		Type declType = Type::Void;
		if (peekToken() == Token::Key_int)
		{
			declType = Type::Int;
		}
		else
		{
			declType = Type::Char;
		}
		
		consumeToken();
		
		// Set this to @@variable for now. We'll later change it
		// assuming we parse the identifier properly
		Identifier* ident = mSymbols.getIdentifier("@@variable");
		
		// Now we MUST get an identifier so go into a try
		try
		{
			if (peekToken() != Token::Identifier)
			{
				throw ParseExceptMsg("Type must be followed by identifier");
			}
            
            // check for redeclarations
            if (mSymbols.isDeclaredInScope(getTokenTxt())) { // if attempted redeclaration
                std::string err("Invalid redeclaration of identifier '");
                err += getTokenTxt();
                err += "'";
                reportSemantError(err);
            }
            else {
                ident = mSymbols.createIdentifier(getTokenTxt());
            }
			
			
			consumeToken();
			
			// Is this an array declaration?
			if (peekAndConsume(Token::LBracket))
			{
				shared_ptr<ASTConstantExpr> constExpr;
				if (declType == Type::Int)
				{
					declType = Type::IntArray;
					
					// int arrays must have a constant size defined,
					// because USC doesn't support initializer lists
					constExpr = parseConstantFactor();
					if (!constExpr)
					{
						reportSemantError("Int arrays must have a defined constant size");
					}
					
					if (constExpr)
					{
						int count = constExpr->getValue();
						if (count <= 0 || count > 65536)
						{
							reportSemantError("Arrays must have a min of 1 and a max of 65536 elements");
						}
						ident->setArrayCount(count);
					}
					else
					{
						ident->setArrayCount(0);
					}
				}
				else
				{
					declType = Type::CharArray;
					
					// For character, we support both constant size or
					// implict size if it's assigned to a constant string
					constExpr = parseConstantFactor();
					if (constExpr)
					{
						int count = constExpr->getValue();
						if (count <= 0 || count > 65536)
						{
							reportSemantError("Arrays must have a min of 1 and a max of 65536 elements");
						}
						ident->setArrayCount(count);
					}
					else
					{
						// We'll determine this later in the parse
						ident->setArrayCount(0);
					}
				}
				
				matchToken(Token::RBracket);
			}
			
			ident->setType(declType);
			
			shared_ptr<ASTExpr> assignExpr;
			
            int col = mColNumber;
            
			// Optionally, this decl may have an assignment
			if (peekAndConsume(Token::Assign))
			{
				// We don't allow assignment for int arrays
				if (declType == Type::IntArray)
				{
					reportSemantError("USC does not allow assignment of int array declarations");
				}
				
				assignExpr = parseExpr();
				if (!assignExpr)
				{
					throw ParseExceptMsg("Invalid expression after = in declaration");
				}
				
				// PA2: Type checks
                
                if (ident->getType() == Type::Char && assignExpr->getType() == Type::Int) {
                    assignExpr = intToChar(assignExpr);
                }
                else if (ident->getType() != assignExpr->getType()) {
                    std::string err("Cannot assign an expression of type ");
                    err += getTypeText(assignExpr->getType());
                    err += " to ";
                    err += getTypeText(ident->getType());
                    reportSemantError(err, col);
                }
				
				// If this is a character array, we need to do extra checks
				if (ident->getType() == Type::CharArray)
				{
					ASTStringExpr* strExpr = dynamic_cast<ASTStringExpr*>(assignExpr.get());
					if (strExpr != nullptr)
					{
						// If we have a declared size, we need to make sure
						// there's enough room to fit the requested string.
						// Otherwise, we need to set our size
						if (ident->getArrayCount() == 0)
						{
							ident->setArrayCount(strExpr->getLength() + 1);
						}
						else if (ident->getArrayCount() < (strExpr->getLength() + 1))
						{
							reportSemantError("Declared array cannot fit string");
						}
					}
				}
			}
			else if (ident->getType() == Type::CharArray && ident->getArrayCount() == 0)
			{
				reportSemantError("char array must have declared size if there's no assignment");
			}
			
			matchToken(Token::SemiColon);
			
			retVal = make_shared<ASTDecl>(*ident, assignExpr);
		}
		catch (ParseExcept& e)
		{
			reportError(e);
			
			// Skip all the tokens until the next semi-colon
			consumeUntil(Token::SemiColon);
			
			if (peekToken() == Token::EndOfFile)
			{
				throw EOFExcept();
			}
			
			// Grab this semi-colon, also
			consumeToken();
			
			// Put in a decl here with the bogus identifier
			// "@@error". This is so the parse will continue to the
			// next decl, if there is one.
			retVal = make_shared<ASTDecl>(*(ident));
		}
	}
	
	return retVal;
}

shared_ptr<ASTStmt> Parser::parseStmt()
{
	shared_ptr<ASTStmt> retVal;
	try
	{
		// NOTE: AssignStmt HAS to go before ExprStmt!!
		// Read comments in AssignStmt for why.
		if ((retVal = parseCompoundStmt(false)))
			;
		else if ((retVal = parseAssignStmt()))
			;
        else if ((retVal = parseReturnStmt()))
            ;
        else if ((retVal = parseWhileStmt()))
            ;
        else if ((retVal = parseExprStmt()))
            ;
        else if ((retVal = parseNullStmt()))
            ;
        else if ((retVal = parseIfStmt()))
            ;
		// PA1: Add additional cases
		
		else if (peekIsOneOf({Token::Key_int, Token::Key_char}))
		{
			throw ParseExceptMsg("Declarations are only allowed at the beginning of a scope block");
		}
	}
	catch (ParseExcept& e)
	{
		reportError(e);
		
		// Skip all the tokens until the next semi-colon
		consumeUntil(Token::SemiColon);
		
		if (peekToken() == Token::EndOfFile)
		{
			throw EOFExcept();
		}
		
		// Grab this semi-colon, also
		consumeToken();
		
		// Put in a null statement here
		// so we can try to continue.
		retVal = make_shared<ASTNullStmt>();
	}
	
	return retVal;
}

// If the compound statement is a function body, then the symbol table scope
// change will happen at a higher level, so it shouldn't happen in
// parseCompoundStmt.
shared_ptr<ASTCompoundStmt> Parser::parseCompoundStmt(const bool isFuncBody)
{
	shared_ptr<ASTCompoundStmt> retVal;
	
	// PA1: Implement
    
    // Syntax:
    // CompoundStmt --> { Declarations Statements }
    
    // if CompoundStmt didn't come from a function, create a new scope
    // otherwise, don't because a function has already created a new scope
    SymbolTable::ScopeTable* table;
    if (!isFuncBody) {
        table = mSymbols.enterScope();
    }
    
    // Parse left brace
    if (peekToken() == Token::LBrace) {
        
        consumeToken();
        
        retVal = make_shared<ASTCompoundStmt>();
    
        // Parse declarations
        // if the next token is a VarType, then it's a declaration
        shared_ptr<ASTDecl> decl = parseDecl();
        while (decl) {
            retVal->addDecl(decl);
            decl = parseDecl();
        }
        
        // Parse statements
        shared_ptr<ASTStmt> prev_stmt, curr_stmt = parseStmt();
        while (curr_stmt) {
            prev_stmt = curr_stmt;
            retVal->addStmt(prev_stmt);
            curr_stmt = parseStmt();
        }
        
        // PA2 Semantic Check
        if (isFuncBody) { // in function
            // if last stmt is NOT returnStmt and is NOT void
            std::shared_ptr<ASTReturnStmt> isReturn = std::dynamic_pointer_cast<ASTReturnStmt>(prev_stmt); // RTTI
            if (!isReturn && mCurrReturnType != Type::Void) {
                std::string err("USC requires non-void functions to end with a return");
                reportSemantError(err);
            }
            else if (!isReturn && mCurrReturnType == Type::Void) { // create return stmt to help build IR
                std::shared_ptr<ASTReturnStmt> voidReturnStmt = std::make_shared<ASTReturnStmt>(nullptr);
                retVal->addStmt(voidReturnStmt);
            }
        }
        
        // Parse right brace
        matchToken(Token::RBrace);
        
        
    }
    
    // exit scope if necessary
    if (!isFuncBody) {
        mSymbols.exitScope();
    }
    
	
	return retVal;
}

shared_ptr<ASTStmt> Parser::parseAssignStmt()
{
	shared_ptr<ASTStmt> retVal;
	shared_ptr<ASTArraySub> arraySub;
	
	if (peekToken() == Token::Identifier)
	{
		Identifier* ident = getVariable(getTokenTxt());
		
		consumeToken();
		
		// Now let's see if this is an array subscript
		if (peekAndConsume(Token::LBracket))
		{
			try
			{
				shared_ptr<ASTExpr> expr = parseExpr();
				if (!expr)
				{
					throw ParseExceptMsg("Valid expression required inside [ ].");
				}
				
				arraySub = make_shared<ASTArraySub>(*ident, expr);
			}
			catch (ParseExcept& e)
			{
				// If this expr is bad, consume until RBracket
				reportError(e);
				consumeUntil(Token::RBracket);
				if (peekToken() == Token::EndOfFile)
				{
					throw EOFExcept();
				}
			}
			
			matchToken(Token::RBracket);
		}
		
		// Just because we got an identifier DOES NOT necessarily mean
		// this is an assign statement.
		// This is because there is a common left prefix between
		// AssignStmt and an ExprStmt with statements like:
		// id ;
		// id [ Expr ] ;
		// id ( FuncCallArgs ) ;
		
		// So... We see if the next token is a =. If it is, then this is
		// an AssignStmt. Otherwise, we set the "unused" variables
		// so parseFactor will later find it and be able to match
		int col = mColNumber;
		if (peekAndConsume(Token::Assign))
		{
			shared_ptr<ASTExpr> expr = parseExpr();
			
			if (!expr)
			{
				throw ParseExceptMsg("= must be followed by an expression");
			}
			
			// If we matched an array, we want to make an array assign stmt
			if (arraySub)
			{
				// Make sure the type of this expression matches the declared type
				Type subType;
				if (arraySub->getType() == Type::IntArray)
				{
					subType = Type::Int;
				}
				else
				{
					subType = Type::Char;
				}
				if (mCheckSemant && subType != expr->getType())
				{
					// We can do a conversion if it's from int to char
					if (subType == Type::Char &&
						expr->getType() == Type::Int)
					{
						expr = intToChar(expr);
					}
					else
					{
						std::string err("Cannot assign an expression of type ");
						err += getTypeText(expr->getType());
						err += " to ";
						err += getTypeText(subType);
						reportSemantError(err, col);
					}
				}
				retVal = make_shared<ASTAssignArrayStmt>(arraySub, expr);
			}
			else
			{
				// PA2: Check for semantic errors
                
                if (ident->getType() == Type::Char && expr->getType() == Type::Int) {
                    expr = intToChar(expr);
                }
                else if (ident->getType() == Type::CharArray || ident->getType() == Type::IntArray) {
                    std::string err("Reassignment of arrays is not allowed");
                    reportSemantError(err, col);
                }
                else if (ident->getType() != expr->getType()) {
                    std::string err("Cannot assign an expression of type ");
                    err += getTypeText(expr->getType());
                    err += " to ";
                    err += getTypeText(ident->getType());
                    reportSemantError(err, col);
                }
				
				retVal = make_shared<ASTAssignStmt>(*ident, expr);
			}
			
			matchToken(Token::SemiColon);
		}
		else
		{
			// We either have an unused array, or an unused ident
			if (arraySub)
			{
				mUnusedArray = arraySub;
			}
			else
			{
				mUnusedIdent = ident;
			}
		}
	}
	
	return retVal;
}

shared_ptr<ASTIfStmt> Parser::parseIfStmt()
{
	shared_ptr<ASTIfStmt> retVal;
	
	// PA1: Implement
    
    // Syntax:
    // IfStmt --> if ( Expr ) Stmt
    //         |  if ( Expr ) Stmt else Stmt
    
    shared_ptr<ASTExpr> expr;
    shared_ptr<ASTStmt> thenStmt;
    shared_ptr<ASTStmt> elseStmt = nullptr;
    
    // if
    if (peekToken() == Token::Key_if) {
        consumeToken();
        
        // (
        if (peekToken() == Token::LParen) {
            consumeToken();
            
            // expr
            expr = parseExpr();
            if (!expr) {
                throw ParseExceptMsg("Invalid condition for if statement");
            }
            
            // )
            matchToken(Token::RParen);
        }
        else {
            throw TokenMismatch(Token::LParen, peekToken(), getTokenTxt());
        }
        
        // stmt
        thenStmt = parseStmt();
        
        // else
        if (peekToken() == Token::Key_else) {
            consumeToken();
            
            // stmt
            elseStmt = parseStmt();
        }
        
        retVal = make_shared<ASTIfStmt>(expr, thenStmt, elseStmt);
    }
	
	return retVal;
}

shared_ptr<ASTWhileStmt> Parser::parseWhileStmt()
{
	shared_ptr<ASTWhileStmt> retVal;
	
	// PA1: Implement
    
    // Syntax:
    // WhileStmt --> while ( Expr ) Stmt
    
    shared_ptr<ASTExpr> expr;
    shared_ptr<ASTStmt> stmt;
    
    // while
    if (peekToken() == Token::Key_while) {
        consumeToken();
        
        // (
        if (peekToken() == Token::LParen) {
            consumeToken();
            
            // Expr
            expr = parseExpr();
            
            if (expr) {
                
                // )
                if (peekToken() == Token::RParen) {
                    consumeToken();
                    
                    // Stmt
                    stmt = parseStmt();
                    
                    if (stmt) {
                        retVal = make_shared<ASTWhileStmt>(expr, stmt);
                    }
                }
            }
            else {
                throw ParseExceptMsg("Invalid condition for while statement");
            }
            
        }
        
    }
	
	return retVal;
}

shared_ptr<ASTReturnStmt> Parser::parseReturnStmt()
{
	shared_ptr<ASTReturnStmt> retVal;
	
	// PA1: Implement
    
    // Syntax:
    // ReturnStmt --> return;
    //             |  return Expr ;

    if (peekToken() == Token::Key_return) {
        consumeToken();
        
        int col = mColNumber;
        
        // optional expression
        shared_ptr<ASTExpr> expr = parseExpr();
        
        // PA2 Semantic Check
        if (expr && mCurrReturnType == Type::Char && expr->getType() == Type::Int) {
            expr = intToChar(expr);
        }
        else if (expr && mCurrReturnType != expr->getType()) {
            std::string err("Expected type ");
            err += getTypeText(mCurrReturnType);
            err += " in return statement";
            reportSemantError(err, col);
        }
        else if (!expr && mCurrReturnType != Type::Void) {
            std::string err("Invalid empty return in non-void function");
            reportSemantError(err, col);
        }
        
        if (expr) {
            retVal = make_shared<ASTReturnStmt>(expr);
        }
        else {
            retVal = make_shared<ASTReturnStmt>(nullptr);
        }
        
        matchToken(Token::SemiColon);
    }
    
	return retVal;
}

shared_ptr<ASTExprStmt> Parser::parseExprStmt()
{
	shared_ptr<ASTExprStmt> retVal;
	
	// PA1: Implement
    
    // Syntax:
    // Stmt --> Expr ;
    //       |  (and other options)
    
    shared_ptr<ASTExpr> expr = parseExpr();
    
    if (expr) {
        if (peekToken() == Token::SemiColon) {
            consumeToken();
            retVal = make_shared<ASTExprStmt>(expr);
        }
    }
	
	return retVal;
}

shared_ptr<ASTNullStmt> Parser::parseNullStmt()
{
	shared_ptr<ASTNullStmt> retVal;
	
	// PA1: Implement
    
    // Syntax:
    // Stmt --> ;
    //       |  (and other options)
    
    if (peekToken() == Token::SemiColon) {
        consumeToken();
        retVal = make_shared<ASTNullStmt>();
    }
	
	return retVal;
}
