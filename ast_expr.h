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

/*** class expr ******************************************************/

class Expr : public Stmt
{
    public:
        Expr(yyltype loc);
        Expr();

        virtual Type* CheckAndComputeResultType() = 0;

        void Check();
};

inline void Expr::Check()
{
    CheckAndComputeResultType();
}

/*** class empty_expr ************************************************/

class EmptyExpr : public Expr
{
    public:
        Type* CheckAndComputeResultType();
};

inline Type *EmptyExpr::CheckAndComputeResultType()
{
    return Type::voidType;
}

/*** class int_const *************************************************/

class IntConstant : public Expr
{
    protected:
        int value;

    public:
        IntConstant(yyltype loc, int val);

        Type *CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

inline Type *IntConstant::CheckAndComputeResultType()
{
    return Type::intType;
}

inline Location* IntConstant::CodeGen(CodeGenerator *tac, int *nvar)
{
    return tac->GenLoadConstant(nvar, value);
}

/*** class bool_const ************************************************/

class BoolConstant : public Expr
{
    protected:
        bool value;

    public:
        BoolConstant(yyltype loc, bool val);

        Type *CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

inline Type *BoolConstant::CheckAndComputeResultType()
{
    return Type::boolType;
}

inline Location* BoolConstant::CodeGen(CodeGenerator *tac, int *nvar)
{
    return tac->GenLoadConstant(nvar, value ? 1 : 0);
}

/*** class str_const *************************************************/

class StringConstant : public Expr
{
    protected:
        char *value;

    public:
        StringConstant(yyltype loc, const char *val);

        Type *CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

inline Type *StringConstant::CheckAndComputeResultType()
{
    return Type::stringType;
}

inline Location* StringConstant::CodeGen(CodeGenerator *tac, int *nvar)
{
    return tac->GenLoadConstant(nvar, value);
}

/*** class null_const ************************************************/

class NullConstant: public Expr
{
    public:
        NullConstant(yyltype loc);

        Type *CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

inline Type *NullConstant::CheckAndComputeResultType()
{
    return Type::nullType;
}

inline Location* NullConstant::CodeGen(CodeGenerator *tac, int *nvar)
{
    return tac->GenLoadConstant(nvar, 0);
}

/*** class operat ****************************************************/

class Operator : public Node
{
    protected:
        char tokenString[4];

    public:
        Operator(yyltype loc, const char *tok);
        friend std::ostream& operator<<(std::ostream& out,
                                        Operator *o);
        const char *str();
};

inline std::ostream& operator<<(std::ostream& out, Operator *o)
{
    return out << o->tokenString;
}

inline const char *Operator::str()
{
    return tokenString;
}

/*** class compound_expr *********************************************/

class CompoundExpr : public Expr
{
    protected:
        Operator *op;
        Expr *left, *right;

    public:
        CompoundExpr(Expr *lhs, Operator *op, Expr *rhs);
        CompoundExpr(Operator *op, Expr *rhs);
        void ReportErrorForIncompatibleOperands(Type *lhs, Type *rhs);
        bool CanDoArithmetic(Type *lhs, Type *rhs);
};

/*** class arith_expr ************************************************/

class ArithmeticExpr : public CompoundExpr
{
    public:
        ArithmeticExpr(Expr *lhs, Operator *op, Expr *rhs);
        ArithmeticExpr(Operator *op, Expr *rhs);

        Type* CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

/*** class rel_expr **************************************************/

class RelationalExpr : public CompoundExpr
{
    public:
        RelationalExpr(Expr *lhs, Operator *op, Expr *rhs);

        Type* CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

/*** class eq_expr ***************************************************/

class EqualityExpr : public CompoundExpr
{
    public:
        EqualityExpr(Expr *lhs, Operator *op, Expr *rhs);

        Type* CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

/*** class log_expr **************************************************/

class LogicalExpr : public CompoundExpr
{
    public:
        LogicalExpr(Expr *lhs, Operator *op, Expr *rhs);
        LogicalExpr(Operator *op, Expr *rhs);

        Type* CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

/*** class assign_expr ***********************************************/

class AssignExpr : public CompoundExpr
{
    public:
        AssignExpr(Expr *lhs, Operator *op, Expr *rhs);

        Type* CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

class LValue : public Expr
{
    public:
        LValue(yyltype loc) : Expr(loc) {}
};

/*** class this_obj **************************************************/

class This : public Expr
{
    protected:
        ClassDecl *enclosingClass;

    public:
        This(yyltype loc);

        Type* CheckAndComputeResultType();
        Location* CodeGen(CodeGenerator *tac, int *nvar);
};

/*** class array_acc *************************************************/

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

#endif
