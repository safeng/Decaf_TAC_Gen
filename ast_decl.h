/**** ast_decl.h - Declaration nodes *********************************/

#ifndef _H_ast_decl
#define _H_ast_decl

#include "ast.h"
#include "ast_type.h"
#include "ast_stmt.h"
#include "list.h"

class Identifier;
class Stmt;
class FnDecl;
class CodeGenerator;

class Decl : public Node
{
    protected:
        Identifier *id;

    public:
        Decl(Identifier *name);
        friend std::ostream& operator<<(std::ostream& out, Decl *d) { return out << d->id; }
        Identifier *GetId() { return id; }
        const char *GetName() { return id->GetName(); }

        virtual bool ConflictsWithPrevious(Decl *prev);

        virtual bool IsVarDecl() { return false; } // jdz: could use typeid/dynamic_cast for these
        virtual bool IsClassDecl() { return false; }
        virtual bool IsFnDecl() { return false; }
        virtual bool IsMethodDecl() { return false; }
        virtual bool IsIvarDecl() { return false; }
};

class VarDecl : public Decl
{
    protected:
        Type *type;

    public:
        VarDecl(Identifier *name, Type *type);
        void Check();
        Type *GetDeclaredType() { return type; }
        bool IsVarDecl() { return true; }
        bool IsIvarDecl();
};

class ClassDecl : public Decl
{
    protected:
        List<Decl*> *members;
        NamedType *extends;
        NamedType *cType;
        Hashtable<int> *classLayout;
        List<const char *> *fnTable; // function table to generate virtual table
        List<FnDecl *> *fnLayout;
        static const int fieldOffset = 4;

    public:
        ClassDecl(Identifier *name, NamedType *extends, List<Decl*> *members);
        void Check();
        bool IsClassDecl() { return true; }
        Scope *PrepareScope();
        void PrepareClassLayout();
        void PrepareFnLayout();
        bool IsCompatibleWith(Type *type);
        Type *GetDeclaredType() { return cType; } //  used by "this"
        const char *GetClassName() { return id->GetName(); }
        Hashtable<int> *GetClassLayout() { return classLayout; }
        List<FnDecl *> *GetFnLayout() { return fnLayout; }
        int GetFnDeclIdx(FnDecl *fn_decl);
        List<const char *> *GetFnTable() { return fnTable; }
        Location* CodeGen(CodeGenerator *tac, int *var_num);
};

/*** FnDecl **********************************************************/

class FnDecl : public Decl
{
    protected:
        List<VarDecl*> *formals;
        Type *returnType;
        Stmt *body;
        char *label;

    public:
        FnDecl(Identifier *name, Type *returnType, List<VarDecl*> *formals);

        bool IsFnDecl();
        bool IsMethodDecl();
        bool IsMain();
        void SetFunctionBody(Stmt *b);
        Type *GetReturnType();
        List<VarDecl*> *GetFormals();
        const char *GetLabel();

        void Check();
        void CheckPrototype();
        bool ConflictsWithPrevious(Decl *prev);
        bool MatchesPrototype(FnDecl *other);
        void PrepareVarLocation();
        Location* CodeGen(CodeGenerator *tac, int *var_num);
};

inline bool FnDecl::IsFnDecl()
{
    return true;
}

inline bool FnDecl::IsMethodDecl()
{
    return dynamic_cast<ClassDecl*>(parent) != NULL;
}

inline bool FnDecl::IsMain()
{
    return !IsMethodDecl() && strcmp(GetName(), "main") == 0;
}

inline Type *FnDecl::GetReturnType()
{
    return returnType;
}

inline List<VarDecl*> *FnDecl::GetFormals()
{
    return formals;
}

inline const char *FnDecl::GetLabel()
{
    return label;
}

#endif
