/**** ast_type.h - Type nodes ****************************************/

#ifndef _H_ast_type
#define _H_ast_type

#include "ast.h"
#include "list.h"
#include <iostream>


class Type : public Node
{
    protected:
        char *typeName;

    public :
        static Type *intType, *doubleType, *boolType, *voidType,
                    *nullType, *stringType, *errorType;

        Type(yyltype loc) : Node(loc) {}
        Type(const char *str);

        virtual void PrintToStream(std::ostream& out) { out << typeName; }
        friend std::ostream& operator<<(std::ostream& out, Type *t) { t->PrintToStream(out); return out; }
        virtual bool IsEquivalentTo(Type *other) { return this == other; }
        virtual bool IsArrayType() { return false; }
        virtual bool IsNamedType() { return false; }
        virtual bool IsCompatibleWith(Type *other);
        virtual bool IsNumeric() { return this == Type::intType; }
        virtual bool IsError() { return false;}
        virtual Type *LesserType(Type *other);
};

class NamedType : public Type
{
    protected:
        Identifier *id;
        Decl *declForType; // either class or inteface
        bool isError;

    public:
        NamedType(Identifier *i);

        void PrintToStream(std::ostream& out) { out << id; }
        void Check();
        Decl *GetDeclForType();
        void SetDeclForType(Decl *decl);
        bool IsClass();
        Identifier *GetId() { return id; }
        bool IsEquivalentTo(Type *other);
        bool IsNamedType() { return true; }
        bool IsCompatibleWith(Type *other);
        bool IsError() { return isError;}
};

class ArrayType : public Type
{
    protected:
        Type *elemType;

    public:
        ArrayType(yyltype loc, Type *elemType);

        void PrintToStream(std::ostream& out) { out << elemType << "[]"; }
        void Check();
        bool IsEquivalentTo(Type *other);
        bool IsArrayType() { return true; }
        bool IsError() { return elemType->IsError(); }
        Type *GetArrayElemType() { return elemType; }
};

#endif
