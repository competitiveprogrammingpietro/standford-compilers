/*
 *  The semantic analyser for the COOL language.
 *  09/04/2017 Pietro Paolini : general.2.pulsarpietro@spamgourmet.com
 *
 */
#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0
#define MAX_CHILDREN (128)

class ClassNode {
public:
  int                                 m_index;
  ClassNode *                         m_children[MAX_CHILDREN];
  const Class_                        m_value;

  ClassNode(Class_ node): m_index(0), m_value(node) { };
  void add_child(ClassNode * node);
  bool operator==(ClassNode &node);
  friend ostream& operator<<(ostream& stream, const ClassNode& node);
};

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int                         semant_errors;
  ostream&                    error_stream;
  Classes                     m_classes;
  ClassNode *                 m_root;
public:
  SymbolTable<Symbol, Symbol> m_map;
  void                   build_hierarchy();
  bool                   cycles();
  ClassNode *            find_node(Symbol item);
  Class_                 find_class(Symbol name);
  ClassNode *            add_node(Class_ class_);
  std::stack<Class_> object_path(Symbol class_);
  bool                   is_conformant(Symbol parent, Symbol child);
  friend ostream&        operator<<(ostream& stream, const ClassTable& obj);
  bool                   semant_analysis();
  bool                   is_base_class(Symbol name);
  Feature                find_method(Class_ class_, Symbol name);
  Symbol                 common_ancestor(Symbol one, Symbol two);
  ClassTable(Classes classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

};


#endif

