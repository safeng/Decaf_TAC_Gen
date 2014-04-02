/**** ast.h - Abstract Node, Identifier and Error ********************/

#ifndef _H_ast
#define _H_ast

#include <stdlib.h>   // for NULL
#include "location.h"
#include "codegen.h"
#include <iostream>

class CodeGenerator;
class Scope;
class Decl;
class Identifier;
class Type;

class Node
{
    protected:
        yyltype *location;
        Node *parent;
        Scope *nodeScope;
        Hashtable<Location*> *varLocation;

    public:
        Node(yyltype loc);
        Node();

        yyltype *GetLocation()   { return location; }
        void SetParent(Node *p)  { parent = p; }
        Node *GetParent()        { return parent; }
        virtual void Check() {} // not abstract, since some nodes have nothing to do

        typedef enum { kShallow, kDeep } lookup;
        virtual Decl *FindDecl(Identifier *id, lookup l = kDeep);
        virtual Scope *PrepareScope() { return NULL; }
        virtual void PrepareVarLocation() {}
        virtual Location* CodeGen(CodeGenerator *tca, int *var_num) {}
        Location* FindLocation(Identifier * id, lookup l = kDeep);
        template <class Specific> Specific *FindSpecificParent() {
            Node *p = parent;
            while (p) {
                if (Specific *s = dynamic_cast<Specific*>(p)) return s;
                p = p->parent;
            }
            return NULL;
        }
};


class Identifier : public Node
{
    protected:
        char *name;
        Decl *cached;

    public:
        Identifier(yyltype loc, const char *name);
        friend std::ostream& operator<<(std::ostream& out, Identifier *id) { return out << id->name; }
        const char *GetName() { return name; }
        Decl *GetDeclRelativeToBase(Type *base = NULL);
};


// This node class is designed to represent a portion of the tree that
// encountered syntax errors during parsing. The partial completed tree
// is discarded along with the states being popped, and an instance of
// the Error class can stand in as the placeholder in the parse tree
// when your parser can continue after an error.
class Error : public Node
{
    public:
        Error() : Node() {}
};

#endif
