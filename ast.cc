/**** ast.cc - Abstract Node, Identifier and Error *******************/

#include "ast.h"
#include "ast_type.h"
#include "ast_decl.h"
#include <string.h> // strdup
#include <stdio.h>  // printf
#include "errors.h"
#include "scope.h"

Node::Node(yyltype loc) {
    location = new yyltype(loc);
    parent = NULL;
    nodeScope = NULL;
    varLocation = NULL;
}

Node::Node() {
    location = NULL;
    parent = NULL;
    nodeScope = NULL;
    varLocation = NULL;
}

Decl *Node::FindDecl(Identifier *idToFind, lookup l) {
    Decl *mine;
    if (!nodeScope) PrepareScope();
    if (nodeScope && (mine = nodeScope->Lookup(idToFind)))
        return mine;
    if (l == kDeep && parent)
        return parent->FindDecl(idToFind, l);
    return NULL;
}

Location *Node::FindLocation(const char *query, lookup l)
{
    Location *loc;
    if (varLocation != NULL) {
        PrepareVarLocation();
    }
    if (varLocation != NULL && (loc = varLocation->Lookup(query))) {
        return loc;
    } else if (l == kDeep && parent != NULL) {
        return parent->FindLocation(query, l);
    } else {
        return NULL;
    }
}

Identifier::Identifier(yyltype loc, const char *n) : Node(loc) {
    name = strdup(n);
    cached = NULL;
}

Decl *Identifier::GetDeclRelativeToBase(Type *baseType)
{
    if (!cached) {
        if (!baseType)
            cached = FindDecl(this);
        else if (!baseType->IsNamedType())
            return NULL; // only classes are aggregates
        else {
            Decl *cd = dynamic_cast<NamedType*>(baseType)->GetDeclForType();
            cached = (cd ? cd->FindDecl(this, kShallow) : NULL);
        }
    }
    return cached;
}

/*** class errornode *************************************************/

Error::Error() : Node()
{
}
