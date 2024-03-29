/**** ast_decl.cc - Declaration nodes ********************************/

#include "ast_decl.h"
#include "ast_type.h"
#include "ast_stmt.h"
#include "scope.h"
#include "errors.h"
#include "codegen.h"
#include "tac.h"

#include <cstring>

Decl::Decl(Identifier *n) : Node(*n->GetLocation()) {
    Assert(n != NULL);
    (id=n)->SetParent(this);
}

bool Decl::ConflictsWithPrevious(Decl *prev) {
    if (prev == NULL || prev == this)
        return false;
    ReportError::DeclConflict(this, prev);
    return true;
}

VarDecl::VarDecl(Identifier *n, Type *t) : Decl(n) {
    Assert(n != NULL && t != NULL);
    (type=t)->SetParent(this);
}

void VarDecl::Check() {
    type->Check();
    if (type->IsError()) type = Type::errorType;
}

bool VarDecl::IsIvarDecl()
{
    return dynamic_cast<ClassDecl*>(parent) != NULL;
}

ClassDecl::ClassDecl(Identifier *n, NamedType *ex, List<Decl*> *m) : Decl(n) {
    // extends can be NULL, impl & mem may be empty lists but cannot be NULL
    Assert(n != NULL && m != NULL);
    extends = ex;
    if (extends) extends->SetParent(this);
    (members=m)->SetParentAll(this);
    cType = new NamedType(n);
    cType->SetParent(this);
    cType->SetDeclForType(this);
    classLayout = NULL;
    fnLayout = NULL;
    fnTable = NULL;
}

void ClassDecl::Check() {
    ClassDecl *ext = extends ? dynamic_cast<ClassDecl*>(parent->FindDecl(extends->GetId())) : NULL;
    if (extends && !ext) {
        ReportError::IdentifierNotDeclared(extends->GetId(), LookingForClass);
        extends = NULL;
    }
    PrepareScope();
    members->CheckAll();
}

// This is not done very cleanly. I should sit down and sort this out. Right now
// I was using the copy-in strategy from the old compiler, but I think the link to
// parent may be the better way now.
Scope *ClassDecl::PrepareScope()
{
    if (nodeScope) return nodeScope;
    nodeScope = new Scope();
    if (extends) {
        ClassDecl *ext = dynamic_cast<ClassDecl*>(parent->FindDecl(extends->GetId()));
        if (ext) nodeScope->CopyFromScope(ext->PrepareScope(), this);
    }
    members->DeclareAll(nodeScope);
    return nodeScope;
}

int ClassDecl::GetFnDeclIdx(FnDecl *fn_decl)
{
    PrepareFnLayout();
    for(int i = 0; i < fnLayout->NumElements(); ++i)
    {
        if(strcmp(fnLayout->Nth(i)->GetName(), fn_decl->GetName()) == 0)
            return i;
    }
    return -1;
}

void ClassDecl::PrepareClassLayout()
{
    if(classLayout) return;
    classLayout = new Hashtable<int>(); 
    int offset = fieldOffset;
    if(extends) {
        ClassDecl *ext = static_cast<ClassDecl*>(parent->FindDecl(extends->GetId()));
        ext->PrepareClassLayout();
        *classLayout = *ext->GetClassLayout();
        offset += classLayout->NumEntries()*CodeGenerator::VarSize;
    }
    for(int i = 0; i < members->NumElements(); ++i){
        if(members->Nth(i)->IsVarDecl()){
           classLayout->Enter(members->Nth(i)->GetName(), offset);
           offset += CodeGenerator::VarSize;
        }
    }
}


void ClassDecl::PrepareFnLayout()
{
    if(fnLayout) return;
    fnLayout = new List<FnDecl *>();
    fnTable = new List<const char *>();
    if(extends) {
        ClassDecl *ext = static_cast<ClassDecl*>(parent->FindDecl(extends->GetId()));
        ext->PrepareFnLayout();
        *fnLayout = *ext->GetFnLayout();
        *fnTable = *ext->GetFnTable();
    }
    int fnLayout_size = fnLayout->NumElements();
    for(int i = 0; i < members->NumElements(); ++i) {
        if(members->Nth(i)->IsFnDecl()){
            FnDecl * fn_decl = static_cast<FnDecl* >(members->Nth(i));
            const char *fn_label = fn_decl->GetLabel();
            int idx = -1;
            for(int j = 0; j<fnLayout_size; ++j) {
                if(strcmp(fn_decl->GetName(), fnLayout->Nth(j)->GetName()) == 0){
                    idx = j;
                    break;
                }
            }
            if(idx == -1){ // new function
                fnLayout->Append(fn_decl);
                fnTable->Append(fn_label);
            }else { // update function label as derived class
                fnTable->Update(idx, fn_label);
            }
        }
    }
}

Location *ClassDecl::CodeGen(CodeGenerator *tac, int *var_num)
{
    PrepareClassLayout();
    // Gen code for all member functions
    for(int i = 0; i < members->NumElements(); ++i){
        if(members->Nth(i)->IsFnDecl()){
            members->Nth(i)->CodeGen(tac, var_num);
        }
    }
    PrepareFnLayout();
    tac->GenVTable(this->GetName(), fnTable);
    return NULL;
}

bool ClassDecl::IsCompatibleWith(Type *other) {
    if (cType->IsEquivalentTo(other)) return true;
    return (extends && extends->IsCompatibleWith(other));
}

/*** FnDecl **********************************************************/

FnDecl::FnDecl(Identifier *n, Type *r, List<VarDecl*> *d) : Decl(n)
{
    Assert(n != NULL && r!= NULL && d != NULL);
    (returnType=r)->SetParent(this);
    (formals=d)->SetParentAll(this);
    body = NULL;
    label = NULL;
}

void FnDecl::SetFunctionBody(Stmt *b)
{
    (body=b)->SetParent(this);
    if (IsMain()) {
        label = strdup("main");
    } else {
        label = CodeGenerator::NewFuncLabel();
    }
}

void FnDecl::Check()
{
    Assert(parent != NULL);
    nodeScope = new Scope();
    formals->DeclareAll(nodeScope);
    CheckPrototype();
    if (body)
        body->Check();
}

void FnDecl::CheckPrototype()
{
    returnType->Check();
    if (returnType->IsError()) returnType = Type::errorType;
    formals->CheckAll();
}

bool FnDecl::ConflictsWithPrevious(Decl *prev)
{
    if (prev == NULL || prev == this) {
        return false;
    } else if (IsMethodDecl() && prev->IsMethodDecl() &&
        parent != prev->GetParent()) {
        if (!MatchesPrototype(dynamic_cast<FnDecl*>(prev))) {
            ReportError::OverrideMismatch(this);
            return true;
        } else {
            return false;
        }
    } else {
        ReportError::DeclConflict(this, prev);
        return true;
    }
}

bool FnDecl::MatchesPrototype(FnDecl *other)
{
    CheckPrototype();
    other->CheckPrototype();
    if (returnType != Type::errorType && other->returnType != Type::errorType &&
        !returnType->IsEquivalentTo(other->returnType)) return false;
    if (formals->NumElements() != other->formals->NumElements())
        return false;
    for (int i = 0; i < formals->NumElements(); i++) {
        Type* t1 = formals->Nth(i)->GetDeclaredType();
        Type* t2 = other->formals->Nth(i)->GetDeclaredType();
        if (t1 != Type::errorType && t2 != Type::errorType && !t1->IsEquivalentTo(t2))
            return false;
    }
    return true;
}

void FnDecl::PrepareVarLocation()
{
    varLocation = new Hashtable<Location*>();
    int this_offset = 0;
    if(IsMethodDecl()){ // add 'this'
        Location *this_loc = new Location(fpRelative,
                                     CodeGenerator::OffsetToFirstParam,
                                     "this");
        varLocation->Enter("this", this_loc);
        this_offset = 1;
    }

    for (int i = 0; i < formals->NumElements(); i++) {
        const char *vname = formals->Nth(i)->GetName();
        Location *loc = new Location(fpRelative,
                                     CodeGenerator::OffsetToFirstParam +
                                     CodeGenerator::VarSize * (i + this_offset),
                                     vname);
        varLocation->Enter(vname, loc);
    }
}

Location *FnDecl::CodeGen(CodeGenerator *tac, int *var_num)
{
    int sub_var_num = 0;
    PrepareVarLocation();
    tac->GenLabel(label);
    BeginFunc *begin_func = tac->GenBeginFunc();
    body->CodeGen(tac, &sub_var_num);
    tac->GenEndFunc();
    begin_func->SetFrameSize(sub_var_num * CodeGenerator::VarSize);
    return NULL;
}
