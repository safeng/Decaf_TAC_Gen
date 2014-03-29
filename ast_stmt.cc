/**** ast_stmt.cc - Statement nodes **********************************/

#include <cstring>

#include "ast_stmt.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "ast_expr.h"
#include "scope.h"
#include "errors.h"
#include "codegen.h"

void Program::Check() {
    nodeScope = new Scope();
    decls->DeclareAll(nodeScope);
    decls->CheckAll();
}

void Program::Emit() {
    bool main_found = false;
    for (int i = 0; !main_found && i < decls->NumElements(); i++) {
        Decl *d = decls->Nth(i)->GetId()->GetDeclRelativeToBase();
        if (d->IsFnDecl() && strcmp(d->GetName(), "main") == 0) {
            main_found = true;
        }
    }
    if (!main_found) {
        ReportError::NoMainFound();
    } else {
        CodeGenerator *tca = new CodeGenerator();
        CodeGen(tca, NULL);
        tca->DoFinalCodeGen();
    }
}

void Program::CodeGen(CodeGenerator *tca, int *var_num)
{
    /*
    for (int i = 0; i < decls->NumElements(); i++) {
        Decl *d = decls->Nth(i)->GetId()->GetDeclRelativeToBase();
        if (d->IsVarDecl()) {
            d->CodeGen(tca, NULL);
        }
    }
    */
    for (int i = 0; i < decls->NumElements(); i++) {
        Decl *d = decls->Nth(i)->GetId()->GetDeclRelativeToBase();
        if (d->IsFnDecl()) {
            d->CodeGen(tca, NULL);
        }
    }
}

StmtBlock::StmtBlock(List<VarDecl*> *d, List<Stmt*> *s) {
    Assert(d != NULL && s != NULL);
    (decls=d)->SetParentAll(this);
    (stmts=s)->SetParentAll(this);
}
void StmtBlock::Check() {
    nodeScope = new Scope();
    decls->DeclareAll(nodeScope);
    decls->CheckAll();
    stmts->CheckAll();
}

ConditionalStmt::ConditionalStmt(Expr *t, Stmt *b) {
    Assert(t != NULL && b != NULL);
    (test=t)->SetParent(this);
    (body=b)->SetParent(this);
}

void ConditionalStmt::Check() {
    if (!test->CheckAndComputeResultType()->IsCompatibleWith(Type::boolType))
        ReportError::TestNotBoolean(test);
    body->Check();
}

ForStmt::ForStmt(Expr *i, Expr *t, Expr *s, Stmt *b): LoopStmt(t, b) {
    Assert(i != NULL && t != NULL && s != NULL && b != NULL);
    (init=i)->SetParent(this);
    (step=s)->SetParent(this);
}
void ForStmt::Check() {
    init->Check();
    step->Check();
    ConditionalStmt::Check();
}

IfStmt::IfStmt(Expr *t, Stmt *tb, Stmt *eb): ConditionalStmt(t, tb) {
    Assert(t != NULL && tb != NULL); // else can be NULL
    elseBody = eb;
    if (elseBody) elseBody->SetParent(this);
}
void IfStmt::Check() {
    ConditionalStmt::Check();
    if (elseBody) elseBody->Check();
}


void BreakStmt::Check() {
    if (!FindSpecificParent<LoopStmt>())
        ReportError::BreakOutsideLoop(this);
}

ReturnStmt::ReturnStmt(yyltype loc, Expr *e) : Stmt(loc) {
    Assert(e != NULL);
    (expr=e)->SetParent(this);
}
void ReturnStmt::Check() {
    Type *got = expr->CheckAndComputeResultType();
    Type *expected =  FindSpecificParent<FnDecl>()->GetReturnType();
    if (!got->IsCompatibleWith(expected))
        ReportError::ReturnMismatch(this, got, expected);
}

PrintStmt::PrintStmt(List<Expr*> *a) {
    Assert(a != NULL);
    (args=a)->SetParentAll(this);
}
void PrintStmt::Check() {
    for (int i = 0; i < args->NumElements();i++) {
        Type *t = args->Nth(i)->CheckAndComputeResultType();
        if (t->IsEquivalentTo(Type::errorType)) continue;
        if (!(t->IsEquivalentTo(Type::intType) || t->IsEquivalentTo(Type::stringType) || t->IsEquivalentTo(Type::boolType)))
            ReportError::PrintArgMismatch(args->Nth(i),i + 1, t);
    }
}
