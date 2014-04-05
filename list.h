/**** list.h - List class ********************************************/

#ifndef _H_list
#define _H_list

#include <deque>
#include <algorithm>
#include <iterator>
#include "utility.h"  // for Assert()
#include "scope.h"

class Node;
class CodeGenerator;

template<class Element> class List {

    private:
        std::deque<Element> elems;

    public:
        // Create a new empty list
        List() {}
        // Copy a list
        List(const List<Element> &lst) : elems(lst.elems) {}

        // Clear the list
        void Clear() { elems.clear(); }

        // Returns count of elements currently in list
        int NumElements() const
        { return elems.size(); }

        // Returns element at index in list. Indexing is 0-based.
        // Raises an assert if index is out of range.
        Element Nth(int index) const
        { Assert(index >= 0 && index < NumElements());
            return elems[index]; }

        // Inserts element at index, shuffling over others
        // Raises assert if index out of range
        void InsertAt(const Element &elem, int index)
        { Assert(index >= 0 && index <= NumElements());
            elems.insert(elems.begin() + index, elem); }

        // Adds element to list end
        void Append(const Element &elem)
        { elems.push_back(elem); }

        // Adds all elements to list end
        void AppendAll(const List<Element> &lst)
        { for (int i = 0; i < lst.NumElements(); i++)
            Append(lst.Nth(i)); }

        // Removes element at index, shuffling down others
        // Raises assert if index out of range
        void RemoveAt(int index)
        { Assert(index >= 0 && index < NumElements());
            elems.erase(elems.begin() + index); }

        void Update(int idx, Element e)
        {
            elems[idx] = e;
        }

        // Removes all elements of a specific value
        void Remove(const Element &elem)
        { elems.erase(std::remove(elems.begin(), elems.end(), elem), elems.end()); }

        // Sort and remove repeated elements
        void Unique()
        { std::sort(elems.begin(), elems.end());
            elems.erase(std::unique(elems.begin(), elems.end()), elems.end()); }

        // These are some specific methods useful for lists of ast nodes
        // They will only work on lists of elements that respond to the
        // messages, but since C++ only instantiates the template if you use
        // you can still have Lists of ints, chars*, as long as you
        // don't try to SetParentAll on that list.
        void SetParentAll(Node *p)
        { for (int i = 0; i < NumElements(); i++)
            Nth(i)->SetParent(p); }
        void EmitAll(CodeGenerator *cg)
        { for (int i = 0; i < NumElements(); i++)
            Nth(i)->Emit(cg); }
        void DeclareAll(Scope *s)
        { for (int i = 0; i < NumElements(); i++)
            s->Declare(Nth(i)); }

        void CheckAll()
        { for (int i = 0; i < NumElements(); i++)
            Nth(i)->Check(); }

};

#endif
