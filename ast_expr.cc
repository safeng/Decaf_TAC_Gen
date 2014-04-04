/**** ast_expr.cc - Expression nodes *********************************/

#include <cstring>

#include "ast_expr.h"
#include "ast_type.h"
#include "ast_decl.h"
#include "errors.h"

/*** class expr ******************************************************/

Expr::Expr(yyltype loc) : Stmt(loc)
{
}

Expr::Expr() : Stmt()
{
}

/*** class int_const *************************************************/

IntConstant::IntConstant(yyltype loc, int val) : Expr(loc)
{
    value = val;
}

/*** class bool_const ************************************************/

BoolConstant::BoolConstant(yyltype loc, bool val) : Expr(loc)
{
    value = val;
}

/*** class str_const *************************************************/

StringConstant::StringConstant(yyltype loc, const char *val)
    : Expr(loc)
{
    value = strdup(val);
}

/*** class null_const ************************************************/

NullConstant::NullConstant(yyltype loc) : Expr(loc)
{
}

/*** class opr *******************************************************/

Operator::Operator(yyltype loc, const char *tok) : Node(loc)
{
    strncpy(tokenString, tok, sizeof(tokenString));
}

CompoundExpr::CompoundExpr(Expr *l, Operator *o, Expr *r)
    : Expr(Join(l->GetLocation(), r->GetLocation()))
{
    (op=o)->SetParent(this);
    (left=l)->SetParent(this);
    (right=r)->SetParent(this);
}

CompoundExpr::CompoundExpr(Operator *o, Expr *r)
    : Expr(Join(o->GetLocation(), r->GetLocation()))
{
    left = NULL;
    (op=o)->SetParent(this);
    (right=r)->SetParent(this);
}

void CompoundExpr::ReportErrorForIncompatibleOperands(Type *lhs,
                                                      Type *rhs)
{
    if (lhs == NULL) {
        ReportError::IncompatibleOperand(op, rhs);
    } else {
        ReportError::IncompatibleOperands(op, lhs, rhs);
    }
}

bool CompoundExpr::CanDoArithmetic(Type *lhs, Type *rhs)
{
    if (lhs && lhs != Type::errorType && rhs != Type::errorType) {
        return rhs->IsNumeric() && rhs->IsEquivalentTo(lhs);
    } else if (!lhs || lhs == Type::errorType) {
        return rhs->IsNumeric() || rhs == Type::errorType;
    } else {
        return rhs != Type::errorType || lhs->IsNumeric();
    }
}

/*** class arith_expr ************************************************/

ArithmeticExpr::ArithmeticExpr(Expr *lhs, Operator *op, Expr *rhs)
    : CompoundExpr(lhs,op,rhs)
{
}

ArithmeticExpr::ArithmeticExpr(Operator *op, Expr *rhs)
    : CompoundExpr(op,rhs)
{
}

Type *GetResultType(Type *lhs, Type *rhs)
{
    Type *lesser = rhs;
    if (lhs != NULL) {
        lesser = lesser->LesserType(lhs);
    }
    if (lesser == NULL || !lesser->IsNumeric()) {
        return Type::errorType;
    } else {
        return lesser;
    }
}

Type *ArithmeticExpr::CheckAndComputeResultType()
{
    Type *lType = NULL;
    Type *rType = right->CheckAndComputeResultType();
    if (left != NULL) {
        lType = left->CheckAndComputeResultType();
    }
    if (!CanDoArithmetic(lType, rType)) {
        ReportErrorForIncompatibleOperands(lType, rType);
    }
    return GetResultType(lType, rType);
}

Location* ArithmeticExpr::CodeGen(CodeGenerator *tac, int *nvar)
{
    Location *right_loc = right->CodeGen(tac, nvar);
    Location *left_loc = NULL;
    if (left == NULL) {
        left_loc = tac->GenLoadConstant(nvar, 0);
    } else {
        left_loc = left->CodeGen(tac, nvar);
    }
    return tac->GenBinaryOp(nvar, op->str(), left_loc, right_loc);
}

/*** class rel_expr **************************************************/

RelationalExpr::RelationalExpr(Expr *lhs, Operator *op, Expr *rhs)
    : CompoundExpr(lhs,op,rhs)
{
}

Type *RelationalExpr::CheckAndComputeResultType()
{
    Type *lhs = left->CheckAndComputeResultType();
    Type *rhs = right->CheckAndComputeResultType();
    if (!CanDoArithmetic(lhs, rhs))
        ReportErrorForIncompatibleOperands(lhs, rhs);
    return Type::boolType;
}

Location* RelationalExpr::CodeGen(CodeGenerator *tac, int *nvar)
{
    Location *left_loc = left->CodeGen(tac, nvar);
    Location *right_loc = right->CodeGen(tac, nvar);
    if (strcmp(op->str(), "<") == 0) {
        return tac->GenBinaryOp(nvar, "<", left_loc, right_loc);
    } else if (strcmp(op->str(), ">") == 0) {
        return tac->GenBinaryOp(nvar, "<", right_loc, left_loc);
    } else if (strcmp(op->str(), "<=") == 0) {
        Location *neg_loc = tac->GenBinaryOp(nvar, "<", right_loc,
                                             left_loc);
        Location *const0 = tac->GenLoadConstant(nvar, 0);
        return tac->GenBinaryOp(nvar, "==", neg_loc, const0);
    } else {
        Location *neg_loc = tac->GenBinaryOp(nvar, "<", left_loc,
                                             right_loc);
        Location *const0 = tac->GenLoadConstant(nvar, 0);
        return tac->GenBinaryOp(nvar, "==", neg_loc, const0);
    }
}

/*** class eq_expr ***************************************************/

EqualityExpr::EqualityExpr(Expr *lhs, Operator *op, Expr *rhs)
    : CompoundExpr(lhs, op, rhs)
{
}

Type* EqualityExpr::CheckAndComputeResultType()
{
    Type *lhs = left->CheckAndComputeResultType();
    Type *rhs = right->CheckAndComputeResultType();
    if (!lhs->IsCompatibleWith(rhs) && !rhs->IsCompatibleWith(lhs)) {
        ReportErrorForIncompatibleOperands(lhs, rhs);
    }
    return Type::boolType;
}

Location* EqualityExpr::CodeGen(CodeGenerator *tac, int *nvar)
{
    Type *left_type = left->CheckAndComputeResultType();
    Location *left_loc = left->CodeGen(tac, nvar);
    Location *right_loc = right->CodeGen(tac, nvar);
    Location *eqv_loc;
    if (left_type->IsEquivalentTo(Type::stringType)) {
        eqv_loc = tac->GenBuiltInCall(nvar, StringEqual, left_loc,
                                      right_loc);
    } else {
        eqv_loc = tac->GenBinaryOp(nvar, "==", left_loc, right_loc);
    }
    if (op->str()[0] == '=') {
        return eqv_loc;
    } else {
        Location *const0 = tac->GenLoadConstant(nvar, 0);
        return tac->GenBinaryOp(nvar, "==", eqv_loc, const0);
    }
}

/*** class log_expr **************************************************/

LogicalExpr::LogicalExpr(Expr *lhs, Operator *op, Expr *rhs)
    : CompoundExpr(lhs,op,rhs)
{
}

LogicalExpr::LogicalExpr(Operator *op, Expr *rhs)
    : CompoundExpr(op,rhs)
{
}

Type *LogicalExpr::CheckAndComputeResultType()
{
    Type *rhs = right->CheckAndComputeResultType();
    Type *lhs = NULL;
    if (left != NULL) {
        lhs = left->CheckAndComputeResultType();
    }
    if ((lhs && !lhs->IsCompatibleWith(Type::boolType)) ||
        (!rhs->IsCompatibleWith(Type::boolType)))
        ReportErrorForIncompatibleOperands(lhs, rhs);
    return Type::boolType;
}

Location* LogicalExpr::CodeGen(CodeGenerator *tac, int *nvar)
{
    Location *right_loc = right->CodeGen(tac, nvar);
    if (left == NULL) {
        Location *const0 = tac->GenLoadConstant(nvar, 0);
        return tac->GenBinaryOp(nvar, "==", right_loc, const0);
    } else {
        Location *left_loc = left->CodeGen(tac, nvar);
        return tac->GenBinaryOp(nvar, op->str(), left_loc, right_loc);
    }
}

/*** class assign_expr ***********************************************/

AssignExpr::AssignExpr(Expr *lhs, Operator *op, Expr *rhs)
    : CompoundExpr(lhs,op,rhs)
{
}

Type *AssignExpr::CheckAndComputeResultType()
{
    Type *lhs = left->CheckAndComputeResultType();
    Type *rhs = right->CheckAndComputeResultType();
    if (!rhs->IsCompatibleWith(lhs)) {
        ReportErrorForIncompatibleOperands(lhs, rhs);
        return Type::errorType;
    }
    return lhs;
}

Location* AssignExpr::CodeGen(CodeGenerator *tac, int *nvar)
{
    Location *left_loc = left->CodeGen(tac, nvar);
    Location *right_loc = right->CodeGen(tac, nvar);
    tac->GenAssign(left_loc, right_loc);
    return left_loc;
}

/*** class this_obj **************************************************/

This::This(yyltype loc) : Expr(loc)
{
    enclosingClass = NULL;
}

Type* This::CheckAndComputeResultType()
{
    if (!enclosingClass) {
        enclosingClass = FindSpecificParent<ClassDecl>();
    }
    if (!enclosingClass) {
        ReportError::ThisOutsideClassScope(this);
        return Type::errorType;
    } else {
        return enclosingClass->GetDeclaredType();
    }
}

Location* This::CodeGen(CodeGenerator *tac, int *nvar)
{
    return FindLocation("this");
}

/*** class array_acc *************************************************/

ArrayAccess::ArrayAccess(yyltype loc, Expr *b, Expr *s) : LValue(loc)
{
    base = b;
    base->SetParent(this);
    subscript = s;
    subscript->SetParent(this);
}

Type *ArrayAccess::CheckAndComputeResultType()
{
    Type *baseT = base->CheckAndComputeResultType();
    if ((baseT != Type::errorType) && !baseT->IsArrayType()) {
        ReportError::BracketsOnNonArray(base);
    }
    if (!subscript->CheckAndComputeResultType()->IsCompatibleWith(Type::intType))
        ReportError::SubscriptNotInteger(subscript);
    if (baseT->IsArrayType()) {
        return dynamic_cast<ArrayType*>(baseT)->GetArrayElemType();
    } else {
        return Type::errorType;
    }
}

FieldAccess::FieldAccess(Expr *b, Identifier *f)
    : LValue(b? Join(b->GetLocation(), f->GetLocation()) : *f->GetLocation())
{
    Assert(f != NULL); // b can be be NULL (just means no explicit base)
    base = b;
    if (base) base->SetParent(this);
    (field=f)->SetParent(this);
}


Type* FieldAccess::CheckAndComputeResultType()
{
    Type *baseType;
    if (base != NULL) {
        baseType = base->CheckAndComputeResultType();
    } else {
        baseType = NULL;
    }
    Decl *ivar = field->GetDeclRelativeToBase(baseType);
    if (ivar && ivar->IsIvarDecl() && !base) {
        base = new This(*field->GetLocation());
        base->SetParent(this);
        baseType = base->CheckAndComputeResultType();
    }
    if (base != NULL) {
        if (baseType == Type::errorType) {
            return Type::errorType;
        } else if (ivar == NULL || !ivar->IsVarDecl()) {
            ReportError::FieldNotFoundInBase(field, baseType);
            return Type::errorType;
        } else {
            ClassDecl *enclosingClass = FindSpecificParent<ClassDecl>();
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

Location *FieldAccess::CodeGen(CodeGenerator *tac, int *nvar)
{
    if (base != NULL) {
        Location *base_loc = base->CodeGen(tac, nvar);
        return NULL; // TODO: Add class support.
    } else {
        Decl *ivar = field->GetDeclRelativeToBase(NULL);
        return FindLocation(ivar->GetName());
    }
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

Location *Call::CodeGen(CodeGenerator *tac, int *nvar)
{
    if (base == NULL) {
        FnDecl *fd = static_cast<FnDecl*>(field->GetDeclRelativeToBase(NULL));
        for (int i = actuals->NumElements() - 1; i >= 0; i--) {
            Location *arg_loc = actuals->Nth(i)->CodeGen(tac, nvar);
            tac->GenPushParam(arg_loc);
        }
        bool is_void = fd->GetReturnType() == Type::voidType;
        Location *res_loc = tac->GenLCall(nvar, fd->GetLabel(),
                                          !is_void);
        tac->GenPopParams(actuals->NumElements() *
                          CodeGenerator::VarSize);
        return res_loc;
    }
    return NULL; // TODO: Add support for classes and arrays.
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

NewArrayExpr::NewArrayExpr(yyltype loc, Expr *sz, Type *et) : Expr(loc)
{
    Assert(sz != NULL && et != NULL);
    (size=sz)->SetParent(this);
    (elemType=et)->SetParent(this);
}

Type *NewArrayExpr::CheckAndComputeResultType()
{
    Type *st = size->CheckAndComputeResultType();
    if (!st->IsCompatibleWith(Type::intType))
        ReportError::NewArraySizeNotInteger(size);
    elemType->Check();
    if (elemType->IsError())
        return Type::errorType;
    yyltype none;
    return new ArrayType(none, elemType);
}

/*** Read classes *****************************************************
 *
 *  Define ReadInteger expression node class read_int and ReadLine exp-
 *  ression node class read_line                                     */

Location *ReadIntegerExpr::CodeGen(CodeGenerator *tac, int *nvar)
{
    return tac->GenBuiltInCall(nvar, ReadInteger);
}

Location *ReadLineExpr::CodeGen(CodeGenerator *tac, int *nvar)
{
    return tac->GenBuiltInCall(nvar, ReadLine);
}
