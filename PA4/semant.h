#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <map>
#include <set>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
// using namespace std;

#define TRUE 1
#define FALSE 0


// typedef SymbolTable<Symbol, Symbol> ObjectEnvironment;
// typedef std::map<Symbol, Feature> MethodTable;
// typedef std::map<Symbol, MethodTable> MethodEnvironment;

// typedef std::vector<Class_> InheritancePath;

// typedef std::unordered_set<Symbol> SymbolSet;

class ClassTable;
typedef ClassTable *ClassTableP;
// typedef std::map<Symbol, Class_> ClassMap;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  std::map<Symbol, Class_> class_map;
public:

  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  ostream& semant_error(Class_ c, tree_node *t);

  // Added methods
  std::map<Symbol, Class_>* get_class_map();
  void add_class(Symbol s, Class_ c);
  Class_ get_class_by_symbol(Symbol s);
  bool is_in_table(Symbol s);
  std::vector<Class_> get_inheritance_path(Symbol s);
  bool check_inheritance(Symbol ancestor, Symbol descendant);
  Class_ find_least_common_ancestor(Symbol s1, Symbol s2);
  
};

#endif

