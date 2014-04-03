/**** ast_expr.h - Expression nodes **********************************/

#ifndef _H_ast_expr
#define _H_ast_expr

#include "ast.h"
#include "ast_stmt.h"
#include "ast_type.h"
#include "list.h"

class ClassDecl;
class NamedType;
class Type;

class Expr : public Stmt
{
    public:
        Expr(yyltype loc);
        Expr();

        virtual Type* CheckAndComputeResultType() = 0;

        void Check();
};

class EmptyExpr : public Expr
{
    public:
        Type* CheckAndComputeResultType();
};

/*** class int_const *************************************************/

class IntConstant : public Expr
{
    protected:
        int value;

    public:
        IntConstant(yyltype loc, int val);

        Type *CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tca, int *nvar);
};

inline Type *IntConstant::CheckAndComputeResultType()
{
    return Type::intType;
}

inline Location* IntConstant::CodeGen(CodeGenerator *tca, int *nvar)
{
    return tca->GenLoadConstant(nvar, value);
}

/*** class bool_const ************************************************/

class BoolConstant : public Expr
{
    protected:
        bool value;

    public:
        BoolConstant(yyltype loc, bool val);

        Type *CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tca, int *nvar);
};

inline Type *BoolConstant::CheckAndComputeResultType()
{
    return Type::boolType;
}

inline Location* BoolConstant::CodeGen(CodeGenerator *tca, int *nvar)
{
    return tca->GenLoadConstant(nvar, value ? 1 : 0);
}

/*** class str_const *************************************************/

class StringConstant : public Expr
{
    protected:
        char *value;

    public:
        StringConstant(yyltype loc, const char *val);

        Type *CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tca, int *nvar);
};

inline Type *StringConstant::CheckAndComputeResultType()
{
    return Type::stringType;
}

inline Location* StringConstant::CodeGen(CodeGenerator *tca, int *nvar)
{
    return tca->GenLoadConstant(nvar, value);
}

/*** class null_const ************************************************/

class NullConstant: public Expr
{
    public:
        NullConstant(yyltype loc);

        Type *CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tca, int *nvar);
};

inline Type *NullConstant::CheckAndComputeResultType()
{
    return Type::nullType;
}

inline Location* NullConstant::CodeGen(CodeGenerator *tca, int *nvar)
{
    return tca->GenLoadConstant(nvar, 0);
}

class Operator : public Node
{
    protected:
        char tokenString[4];

    public:
        Operator(yyltype loc, const char *tok);
        friend std::ostream& operator<<(std::ostream& out, Operator *o) { return out << o->tokenString; }
        const char *str() { return tokenString; }
};

class CompoundExpr : public Expr
{
    protected:
        Operator *op;
        Expr *left, *right; // left will be NULL if unary

    public:
        CompoundExpr(Expr *lhs, Operator *op, Expr *rhs); // for binary
        CompoundExpr(Operator *op, Expr *rhs);             // for unary
        void ReportErrorForIncompatibleOperands(Type *lhs, Type *rhs);
        bool CanDoArithmetic(Type *lhs, Type *rhs);
};

class ArithmeticExpr : public CompoundExpr
{
    public:
        ArithmeticExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
        ArithmeticExpr(Operator *op, Expr *rhs) : CompoundExpr(op,rhs) {}
        Type* CheckAndComputeResultType();
};

class RelationalExpr : public CompoundExpr
{
    public:
        RelationalExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
        Type* CheckAndComputeResultType();
};

class EqualityExpr : public CompoundExpr
{
    public:
        EqualityExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
        const char *GetPrintNameForNode() { return "EqualityExpr"; }
        Type* CheckAndComputeResultType();
};

class LogicalExpr : public CompoundExpr
{
    public:
        LogicalExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
        LogicalExpr(Operator *op, Expr *rhs) : CompoundExpr(op,rhs) {}
        const char *GetPrintNameForNode() { return "LogicalExpr"; }
        Type* CheckAndComputeResultType();
};

class AssignExpr : public CompoundExpr
{
    public:
        AssignExpr(Expr *lhs, Operator *op, Expr *rhs) : CompoundExpr(lhs,op,rhs) {}
        const char *GetPrintNameForNode() { return "AssignExpr"; }
        Type* CheckAndComputeResultType();
};

class LValue : public Expr
{
    public:
        LValue(yyltype loc) : Expr(loc) {}
};

class This : public Expr
{
    protected:
        ClassDecl *enclosingClass;

    public:
        This(yyltype loc) : Expr(loc), enclosingClass(NULL)  {}
        Type* CheckAndComputeResultType();
};

class ArrayAccess : public LValue
{
    protected:
        Expr *base, *subscript;

    public:
        ArrayAccess(yyltype loc, Expr *base, Expr *subscript);
        Type *CheckAndComputeResultType();
};

/* Note that field access is used both for qualified names
 * base.field and just field without qualification. We don't
 * know for sure whether there is an implicit "this." in
 * front until later on, so we use one node type for either
 * and sort it out later. */
class FieldAccess : public LValue
{
    protected:
        Expr *base;	// will be NULL if no explicit base
        Identifier *field;

    public:
        FieldAccess(Expr *base, Identifier *field); //ok to pass NULL base
        Type* CheckAndComputeResultType();
};

/* Like field access, call is used both for qualified base.field()
 * and unqualified field().  We won't figure out until later
 * whether we need implicit "this." so we use one node type for either
 * and sort it out later. */
class Call : public Expr
{
    protected:
        Expr *base;	// will be NULL if no explicit base
        Identifier *field;
        List<Expr*> *actuals;

    public:
        Call(yyltype loc, Expr *base, Identifier *field, List<Expr*> *args);
        Decl *GetFnDecl();
        Type *CheckAndComputeResultType();
};

class NewExpr : public Expr
{
    protected:
        NamedType *cType;

    public:
        NewExpr(yyltype loc, NamedType *clsType);
        Type* CheckAndComputeResultType();
};

class NewArrayExpr : public Expr
{
    protected:
        Expr *size;
        Type *elemType;

    public:
        NewArrayExpr(yyltype loc, Expr *sizeExpr, Type *elemType);
        Type* CheckAndComputeResultType();
};

class ReadIntegerExpr : public Expr
{
    public:
        ReadIntegerExpr(yyltype loc) : Expr(loc) {}
        Type *CheckAndComputeResultType();
};

class ReadLineExpr : public Expr
{
    public:
        ReadLineExpr(yyltype loc) : Expr (loc) {}
        Type *CheckAndComputeResultType();
};

/*** Expr ************************************************************/

inline Expr::Expr(yyltype loc) : Stmt(loc)
{
}

inline Expr::Expr() : Stmt()
{
}

inline void Expr::Check()
{
    CheckAndComputeResultType();
}

/*** EmptyExpr *******************************************************/

inline Type *EmptyExpr::CheckAndComputeResultType()
{
    return Type::voidType;
}

#endif
