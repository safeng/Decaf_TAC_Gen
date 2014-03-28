/**** ast_decl.h - Declaration nodes *********************************/

#ifndef _H_ast_decl
#define _H_ast_decl

#include "ast.h"
#include "ast_type.h"
#include "list.h"

class Identifier;
class Stmt;
class FnDecl;

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

  public:
    ClassDecl(Identifier *name, NamedType *extends, List<Decl*> *members);
    void Check();
    bool IsClassDecl() { return true; }
    Scope *PrepareScope();
    bool IsCompatibleWith(Type *type);
    Type *GetDeclaredType() { return cType; } //  used by "this"
    const char *GetClassName() { return id->GetName(); }
};

class FnDecl : public Decl
{
  protected:
    List<VarDecl*> *formals;
    Type *returnType;
    Stmt *body;

  public:
    FnDecl(Identifier *name, Type *returnType, List<VarDecl*> *formals);
    void SetFunctionBody(Stmt *b);
    void Check();
    void CheckPrototype();
    bool IsFnDecl() { return true; }
    bool IsMethodDecl();
    bool ConflictsWithPrevious(Decl *prev);
    bool MatchesPrototype(FnDecl *other);
    Type *GetReturnType() {return returnType; }
    List<VarDecl*> *GetFormals() { return formals; }
};

#endif
