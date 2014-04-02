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

Location *Node::FindLocation(Identifier *id, lookup l){
    Location *loc;
    if(!varLocation) PrepareVarLocation();
    if(varLocation && (loc = varLocation->Lookup(id->GetName())))
        return loc;
    if(l == kDeep && parent)
        return parent->FindLocation(id, l);
    return NULL;
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
