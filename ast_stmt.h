/**** ast_stmt.h - Statement nodes ***********************************/

#ifndef _H_ast_stmt
#define _H_ast_stmt

#include "list.h"
#include "ast.h"

class Decl;
class VarDecl;
class Expr;

class Program : public Node
{
    protected:
        List<Decl*> *decls;

    public:
        Program(List<Decl*> *declList);

        void Check();
        void Emit();
        Location * CodeGen(CodeGenerator *tca, int *var_num);
};

class Stmt : public Node
{
    public:
        Stmt() : Node() {}
        Stmt(yyltype loc) : Node(loc) {}
};

class StmtBlock : public Stmt
{
    protected:
        List<VarDecl*> *decls;
        List<Stmt*> *stmts;

    public:
        StmtBlock(List<VarDecl*> *variableDeclarations, List<Stmt*> *statements);
        void Check();
        void PrepareVarLocation();
        Location * CodeGen(CodeGenerator *tac, int *var_num);
};


class ConditionalStmt : public Stmt
{
    protected:
        Expr *test;
        Stmt *body;

    public:
        ConditionalStmt(Expr *testExpr, Stmt *body);
        void Check();
};

class LoopStmt : public ConditionalStmt
{
    protected:
        char * end_label; // mark the end of the loop

    public:
        LoopStmt(Expr *testExpr, Stmt *body)
            : ConditionalStmt(testExpr, body) { end_label = NULL; }
        char * GetEndLabel() { return end_label; }
};

class ForStmt : public LoopStmt
{
    protected:
        Expr *init, *step;

    public:
        ForStmt(Expr *init, Expr *test, Expr *step, Stmt *body);
        void Check();
        Location * CodeGen(CodeGenerator *tac, int *var_num);
};

class WhileStmt : public LoopStmt
{
    public:
        WhileStmt(Expr *test, Stmt *body) : LoopStmt(test, body) {}
        Location * CodeGen(CodeGenerator *tac, int *var_num);
};

class IfStmt : public ConditionalStmt
{
    protected:
        Stmt *elseBody;

    public:
        IfStmt(Expr *test, Stmt *thenBody, Stmt *elseBody);
        void Check();
        Location * CodeGen(CodeGenerator *tac, int *var_num);
};

class BreakStmt : public Stmt
{
    public:
        BreakStmt(yyltype loc) : Stmt(loc) {}
        void Check();
        Location * CodeGen(CodeGenerator *tac, int *var_num);
};

class ReturnStmt : public Stmt
{
    protected:
        Expr *expr;

    public:
        ReturnStmt(yyltype loc, Expr *expr);
        void Check();
        Location * CodeGen(CodeGenerator *tac, int *var_num);
};

class PrintStmt : public Stmt
{
    protected:
        List<Expr*> *args;

    public:
        PrintStmt(List<Expr*> *arguments);
        void Check();
        Location * CodeGen(CodeGenerator *tac, int *var_num);
};

/*** Program *********************************************************/

inline Program::Program(List<Decl*> *d)
{
    Assert(d != NULL);
    (decls=d)->SetParentAll(this);
}


#endif
