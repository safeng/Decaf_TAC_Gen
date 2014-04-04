/* File: scope.cc
 * --------------
 * Each Scope object tracks its own hashtable and
 * may have additional information about the particulars for this
 * scope (class, fn, global, etc.)
 */

#include "scope.h"
#include "ast_decl.h"

Scope::Scope()
{
    table = new Hashtable<Decl*>;
}

Iterator<Decl*> Scope::GetIterator()
{
    return table->GetIterator();
}

Decl *Scope::Lookup(Identifier *id)
{
    return table->Lookup(id->GetName());
}

bool Scope::Declare(Decl *decl)
{
    Decl *prev = table->Lookup(decl->GetName());
    PrintDebug("scope", "Line %d declaring %s (prev? %p)\n", decl->GetLocation()->first_line, decl->GetName(), prev);
    if (prev && decl->ConflictsWithPrevious(prev)) // throw away second, keep first
        return false;
    table->Enter(decl->GetName(), decl);
    return true;
}

void Scope::CopyFromScope(Scope *other, ClassDecl *addTo)
{
    Iterator<Decl*> iter = other->table->GetIterator();
    Decl *decl;
    while ((decl = iter.GetNextValue()) != NULL) {
        table->Enter(decl->GetName(), decl);
    }
}

