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

/*** class node ******************************************************/

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
        virtual Location* CodeGen(CodeGenerator *tca, int *var_num);
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

inline Location* Node::CodeGen(CodeGenerator *tca, int *var_num)
{
    return NULL;
}

/*** class identifier ************************************************/

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


/*** class errornode **************************************************
 *
 *  A placeholder class that is used for implementing error recovery in
 *  syntax analysis.                                                 */

class Error : public Node
{
    public:
        Error();
};

#endif
