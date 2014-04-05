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
        CodeGenerator *tac = new CodeGenerator();
        CodeGen(tac, NULL);
        tac->DoFinalCodeGen();
    }
}

Location *Program::CodeGen(CodeGenerator *tac, int *var_num)
{ 
    varLocation = new Hashtable<Location*>();
    // global variable
    for (int i = 0; i < decls->NumElements(); i++) {
        Decl *d = decls->Nth(i)->GetId()->GetDeclRelativeToBase();
        if (d->IsVarDecl()) {
            const char *vname = d->GetName();
            varLocation->Enter(vname, tac->GenGlobVar(vname));
        }
    }
    // class
    for(int i = 0; i < decls->NumElements(); ++i) {
        Decl *d = decls->Nth(i)->GetId()->GetDeclRelativeToBase();
        if (d->IsClassDecl()) {
            d->CodeGen(tac, NULL);
        }
    }
    // global function
    for (int i = 0; i < decls->NumElements(); i++) {
        Decl *d = decls->Nth(i)->GetId()->GetDeclRelativeToBase();
        if (d->IsFnDecl()) {
            d->CodeGen(tac, NULL);
        }
    }

    return NULL;
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

void StmtBlock::PrepareVarLocation(int *var_num){
    varLocation = new Hashtable<Location*>();
    // Place all local variables into varLocation table
    for(int i = 0; i < decls->NumElements(); ++i){
        const char *vname = decls->Nth(i)->GetName();
        Location * loc = new Location(fpRelative,
                                      CodeGenerator::OffsetToFirstLocal -
                                      CodeGenerator::VarSize*(*var_num), vname);
        varLocation->Enter(vname, loc);
        (*var_num)++;
    }
}

Location * StmtBlock::CodeGen(CodeGenerator *tac, int *var_num){
    PrepareVarLocation(var_num);
    for(int i = 0; i < stmts->NumElements(); ++i){
        stmts->Nth(i)->CodeGen(tac, var_num);
    }
    return NULL;
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

Location *ForStmt::CodeGen(CodeGenerator *tac, int *var_num){
    char * test_label = tac->NewLabel();
    char * end_label = tac->NewLabel();
    this->end_label = end_label;
    Location * tmp_i = init->CodeGen(tac, var_num); // initialization
    tac->GenLabel(test_label);
    Location * tmp_test = test->CodeGen(tac, var_num);
    tac->GenIfZ(tmp_test, end_label);
    body->CodeGen(tac, var_num);
    step->CodeGen(tac, var_num);
    tac->GenGoto(test_label);
    tac->GenLabel(end_label);

    return NULL;
}

Location *WhileStmt::CodeGen(CodeGenerator *tac, int *var_num){
    char * entry_label = tac->NewLabel();
    char * end_label = tac->NewLabel();
    this->end_label = end_label;
    tac->GenLabel(entry_label);
    Location * tmp_test = test->CodeGen(tac, var_num);
    tac->GenIfZ(tmp_test, end_label);
    body->CodeGen(tac, var_num);
    tac->GenGoto(entry_label);
    tac->GenLabel(end_label);

    return NULL;
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

Location *IfStmt::CodeGen(CodeGenerator *tac, int *var_num){
    Location * tmp_test = test->CodeGen(tac, var_num); // temp var for testing
    char * else_label = tac->NewLabel();
    tac->GenIfZ(tmp_test, else_label); // Go to else
    body->CodeGen(tac, var_num); // if body
    char * end_label = NULL;
    if(elseBody != NULL){
        end_label = tac->NewLabel();
        tac->GenGoto(end_label);
    }
    tac->GenLabel(else_label); // else label
    if(elseBody != NULL){
        elseBody->CodeGen(tac, var_num);
        tac->GenLabel(end_label);
    }
    return NULL;
}


void BreakStmt::Check() {
    if (!FindSpecificParent<LoopStmt>())
        ReportError::BreakOutsideLoop(this);
}

Location *BreakStmt::CodeGen(CodeGenerator *tac, int *var_num){
    LoopStmt * inner_loop = FindSpecificParent<LoopStmt>(); // specify template
    char * end_label = inner_loop->GetEndLabel();
    Assert(end_label != NULL);
    tac->GenGoto(end_label);
    return NULL;
}

ReturnStmt::ReturnStmt(yyltype loc, Expr *e) : Stmt(loc) {
    Assert(e != NULL);
    (expr=e)->SetParent(this);
}

Location *ReturnStmt::CodeGen(CodeGenerator *tac, int *var_num){
    Location *tmp_return = expr->CodeGen(tac, var_num);
    tac->GenReturn(tmp_return);
    return NULL;
}

void ReturnStmt::Check() {
    Type *got = expr->CheckAndComputeResultType();
    Type *expected = FindSpecificParent<FnDecl>()->GetReturnType();
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

Location *PrintStmt::CodeGen(CodeGenerator *tac, int *var_num){
    for(int i = 0; i < args->NumElements(); i++){
        Type *t = args->Nth(i)->CheckAndComputeResultType();
        Location *loc = args->Nth(i)->CodeGen(tac, var_num);
        if(t->IsEquivalentTo(Type::intType)){
            tac->GenBuiltInCall(var_num, PrintInt, loc);
        } else if(t->IsEquivalentTo(Type::stringType)){
            tac->GenBuiltInCall(var_num, PrintString, loc);
        } else{ // bool
            tac->GenBuiltInCall(var_num, PrintBool, loc);
        }
    }
    return NULL;
}
