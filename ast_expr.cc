/**** ast_expr.cc - Expression nodes *********************************/

#include <cstring>

#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "errors.h"

StringConstant::StringConstant(yyltype loc, const char *val) : Expr(loc) {
    Assert(val != NULL);
    value = strdup(val);
}

Type *StringConstant::CheckAndComputeResultType() {
    return Type::stringType;
}

Type *NullConstant::CheckAndComputeResultType() {
    return Type::nullType;
}

Operator::Operator(yyltype loc, const char *tok) : Node(loc) {
    Assert(tok != NULL);
    strncpy(tokenString, tok, sizeof(tokenString));
}
CompoundExpr::CompoundExpr(Expr *l, Operator *o, Expr *r)
    : Expr(Join(l->GetLocation(), r->GetLocation())) {
        Assert(l != NULL && o != NULL && r != NULL);
        (op=o)->SetParent(this);
        (left=l)->SetParent(this);
        (right=r)->SetParent(this);
    }

CompoundExpr::CompoundExpr(Operator *o, Expr *r)
    : Expr(Join(o->GetLocation(), r->GetLocation())) {
        Assert(o != NULL && r != NULL);
        left = NULL;
        (op=o)->SetParent(this);
        (right=r)->SetParent(this);
    }
void CompoundExpr::ReportErrorForIncompatibleOperands(Type *lhs, Type *rhs) {
    if (!lhs) { //unary op
        ReportError::IncompatibleOperand(op, rhs);
    } else { // binary op
        ReportError::IncompatibleOperands(op, lhs, rhs);
    }
}

bool CompoundExpr::CanDoArithmetic(Type *lhs, Type *rhs) {
    if (lhs && lhs != Type::errorType && rhs != Type::errorType)
        return rhs->IsNumeric() && rhs->IsEquivalentTo(lhs);
    if (!lhs || lhs == Type::errorType)
        return rhs->IsNumeric() || rhs == Type::errorType;
    return rhs != Type::errorType || lhs->IsNumeric();
}


Type *GetResultType(Type *lhs, Type *rhs) {
    Type *lesser = rhs;
    if (lhs) lesser = lesser->LesserType(lhs);
    if (!lesser || !lesser->IsNumeric())
        return Type::errorType;
    return lesser;
}

Type*ArithmeticExpr::CheckAndComputeResultType() {
    Type *lType = left?left->CheckAndComputeResultType():NULL, *rType = right->CheckAndComputeResultType();
    if (!CanDoArithmetic(lType, rType))
        ReportErrorForIncompatibleOperands(lType, rType);
    return GetResultType(lType, rType);
}

Type* RelationalExpr::CheckAndComputeResultType() {
    Type*lhs = left->CheckAndComputeResultType(), *rhs = right->CheckAndComputeResultType();
    if (!CanDoArithmetic(lhs, rhs))
        ReportErrorForIncompatibleOperands(lhs, rhs);
    return Type::boolType;
}

Type* EqualityExpr::CheckAndComputeResultType() {
    Type*lhs = left->CheckAndComputeResultType(), *rhs = right->CheckAndComputeResultType();
    if (!lhs->IsCompatibleWith(rhs) && !rhs->IsCompatibleWith(lhs))
        ReportErrorForIncompatibleOperands(lhs, rhs);
    return Type::boolType;
}

Type* LogicalExpr::CheckAndComputeResultType() {
    Type *lhs = left ?left->CheckAndComputeResultType() :NULL, *rhs = right->CheckAndComputeResultType();
    if ((lhs && !lhs->IsCompatibleWith(Type::boolType)) ||
        (!rhs->IsCompatibleWith(Type::boolType)))
        ReportErrorForIncompatibleOperands(lhs, rhs);
    return Type::boolType;
}

Type * AssignExpr::CheckAndComputeResultType() {
    Type *lhs = left->CheckAndComputeResultType(), *rhs = right->CheckAndComputeResultType();
    if (!rhs->IsCompatibleWith(lhs)) {
        ReportErrorForIncompatibleOperands(lhs, rhs);
        return Type::errorType;
    }
    return lhs;
}
Type* This::CheckAndComputeResultType() {
    if (!enclosingClass) enclosingClass = FindSpecificParent<ClassDecl>();
    if (!enclosingClass)
        ReportError::ThisOutsideClassScope(this);
    if (!enclosingClass) return Type::errorType;
    return enclosingClass->GetDeclaredType();
}



ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc) {
    (base=b)->SetParent(this);
    (subscript=s)->SetParent(this);
}
Type *ArrayAccess::CheckAndComputeResultType() {
    Type *baseT = base->CheckAndComputeResultType();
    if ((baseT != Type::errorType) && !baseT->IsArrayType())
        ReportError::BracketsOnNonArray(base);
    if (!subscript->CheckAndComputeResultType()->IsCompatibleWith(Type::intType))
        ReportError::SubscriptNotInteger(subscript);
    return baseT->IsArrayType() ? dynamic_cast<ArrayType*>(baseT)->GetArrayElemType() : Type::errorType;
}


FieldAccess::FieldAccess(Expr *b, Identifier *f)
    : LValue(b? Join(b->GetLocation(), f->GetLocation()) : *f->GetLocation()) {
        Assert(f != NULL); // b can be be NULL (just means no explicit base)
        base = b;
        if (base) base->SetParent(this);
        (field=f)->SetParent(this);
    }


Type* FieldAccess::CheckAndComputeResultType() {
    Type *baseType = base ? base->CheckAndComputeResultType() : NULL;
    Decl *ivar = field->GetDeclRelativeToBase(baseType);
    if (ivar && ivar->IsIvarDecl() && !base) { // add implicit "this"
        base = new This(*field->GetLocation());
        base->SetParent(this);
        baseType = base->CheckAndComputeResultType();
    }
    if (base) {
        if (baseType == Type::errorType)
            return Type::errorType;
        else if (!ivar || !ivar->IsVarDecl()) {
            ReportError::FieldNotFoundInBase(field, baseType);
            return Type::errorType;
        } else {
            ClassDecl *enclosingClass = FindSpecificParent<ClassDecl>(); // check cur scope for compatibility
            Type *withinClass = (enclosingClass? enclosingClass->GetDeclaredType() : NULL);
            if (ivar && (!withinClass|| !withinClass->IsCompatibleWith(baseType))) {
                ReportError::InaccessibleField(field, baseType);
                return Type::errorType;
            }
        }
    } else if (!ivar || !ivar->IsVarDecl()) {
        ReportError::IdentifierNotDeclared(field, LookingForVariable);
        return Type::errorType;
    }
    return ivar ? (dynamic_cast<VarDecl *>(ivar))->GetDeclaredType() : Type::errorType;
}


Call::Call(yyltype loc, Expr *b, Identifier *f, List<Expr*> *a) : Expr(loc)  {
    Assert(f != NULL && a != NULL); // b can be be NULL (just means no explicit base)
    base = b;
    if (base) base->SetParent(this);
    (field=f)->SetParent(this);
    (actuals=a)->SetParentAll(this);
}
// special-case code for length() on arrays... sigh.
Type* Call::CheckAndComputeResultType() {
    Type *baseType = base ? base->CheckAndComputeResultType() : NULL;
    FnDecl *fd = dynamic_cast<FnDecl *>(field->GetDeclRelativeToBase(baseType));
    if (fd && fd->IsMethodDecl() && !base) { // add implicit "this"
        base = new This(*field->GetLocation());
        base->SetParent(this);
        baseType = base->CheckAndComputeResultType();
    }
    List<Type*> aTypes;
    for (int i = 0; i < actuals->NumElements(); i++)
        aTypes.Append(actuals->Nth(i)->CheckAndComputeResultType());
    // jdz cascade, above loop checks actuals before function confirmed.
    // what about excess actuals? what if function doesn't exist at all?
    if (baseType && baseType->IsArrayType() && strcmp(field->GetName(), "length") == 0) {
        if (actuals->NumElements() != 0)
            ReportError::NumArgsMismatch(field, 0, actuals->NumElements());
        return Type::intType;
    }
    if (baseType == Type::errorType) {
        return Type::errorType;
    }
    if (baseType && !fd) { // had receiver, but no field in receiver (not class, wrong name, etc.)
        ReportError::FieldNotFoundInBase(field, baseType);
        return Type::errorType;
    } else if (!fd) { // no base, bad function
        ReportError::IdentifierNotDeclared(field, LookingForFunction);
        return Type::errorType;
    }

    List<VarDecl*> *formals = fd->GetFormals();
    if (formals->NumElements() != actuals->NumElements()) {
        ReportError::NumArgsMismatch(field, formals->NumElements(), actuals->NumElements());
    }
    for (int i = 0; i < formals->NumElements(); i++) {
        if (i >= actuals->NumElements()) break;
        Type *at = aTypes.Nth(i);
        if (!at->IsCompatibleWith(formals->Nth(i)->GetDeclaredType()))
            ReportError::ArgMismatch(actuals->Nth(i), i+1, at,
                                     formals->Nth(i)->GetDeclaredType());
    }
    return fd->GetReturnType();
}


NewExpr::NewExpr(yyltype loc, NamedType *c) : Expr(loc) {
    Assert(c != NULL);
    (cType=c)->SetParent(this);
}

Type* NewExpr::CheckAndComputeResultType() {
    if (!cType->IsClass()) {
        ReportError::IdentifierNotDeclared(cType->GetId(), LookingForClass);
        return Type::errorType;
    }
    return cType;
}

NewArrayExpr::NewArrayExpr(yyltype loc, Expr *sz, Type *et) : Expr(loc) {
    Assert(sz != NULL && et != NULL);
    (size=sz)->SetParent(this);
    (elemType=et)->SetParent(this);
}
Type *NewArrayExpr::CheckAndComputeResultType() {
    Type *st = size->CheckAndComputeResultType();
    if (!st->IsCompatibleWith(Type::intType))
        ReportError::NewArraySizeNotInteger(size);
    elemType->Check();
    if (elemType->IsError())
        return Type::errorType;
    yyltype none;
    return new ArrayType(none, elemType);
}

Type *ReadIntegerExpr::CheckAndComputeResultType() { return Type::intType; }
Type *ReadLineExpr::CheckAndComputeResultType() { return Type::stringType; }
