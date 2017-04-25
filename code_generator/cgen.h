/*
 *  The code generator for the COOL language.
 *  24/04/2017 Pietro Paolini : general.2.pulsarpietro@spamgourmet.com
 *
 */

#include <assert.h>
#include <stdio.h>
#include <ostream>
#include <list>
#include <utility>
#include <typeinfo>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
enum ArithmeticOperation {PLUS, MINUS, STAR, SLASH};
enum LocationType {STACK, FRAME_POINTER, CLASS_FIELD};

#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

#define ATTRIBUTES_OFFSET(i) (i+3)

/** 
 * This class wraps a name-location pair.
 * A name is a variable name and a location can be in the Frame Pointer
 * stack or class field. It's basically a binding.
 */
class NameLocation {
 public:
  Symbol       m_name;
  LocationType m_location;
 NameLocation(Symbol name, LocationType location): m_name(name), m_location(location) { }
};

/**
 * The class mirrors the state of the stack and it is used for name 
 * resolution, given a certain name emits the code to load it to the
 * accumulator or to store it into the stack.
 */
class NameResolver {
 private:
  CgenNode&  m_class;
  std::list<Symbol> m_list_fp;
  std::list<Symbol> m_list_sp;

 public:
 NameResolver(CgenNode& class_): m_class(class_) { }
  void add(Symbol name, LocationType type);
  void pop_sp(int n);
  void clear_fp();
  void clear_sp();
  int size_fp();
  int size_sp();
  std::pair<LocationType, int> name_to_address(Symbol name);
  void emit_load_name(Symbol name, ostream& s);
  void emit_store_name(Symbol name, ostream& s);
  CgenNode& get_class() { return m_class; }
};

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};

/**
 * The class wraps a pair made up by a method and the class where such method
 * belongs to.
 */
class Method {
 public:
 Method(Symbol class_, method_class method): m_class(class_), m_method(method) { }
 Symbol        m_class;
 method_class  m_method;
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

  // They include parent's methods and attributes and their main
  // purpose is to record their offset
  std::list<attr_class>      attributes;

  // The shortcoming of having a class for the method is that the class'name
  // will be stored across evey method belonging to the same class.
  // The name is a pointer to Symbol and it is not a big deal and the C++ version
  // this project works with does not include hashmaps  (TR1, C++11)
  std::list<Method>      methods;
  
public:
  CgenNode(Class_ c,
            Basicness bstatus,
	   CgenClassTableP class_table);
  
  void add_child(CgenNodeP child);
  List<CgenNode> *get_children() { return children; }
  void set_parentnd(CgenNodeP p);
  CgenNodeP get_parentnd() { return parentnd; }
  int basic() { return (basic_status == Basic); }
  friend std::ostream& operator<<(std::ostream& out, const CgenNode& c);
  void inherit(std::list<attr_class>& inh_attributes, std::list<Method>& inh_methods);
  void compute_features();
  void assign_index(int base);
  int method_index(Symbol name);
  method_class get_method(Symbol name);
  int attribute_index(Symbol name);
  void emit_classnameTab_entry(ostream& s);
  void emit_classnameDispTab(ostream& s);
  void emit_classnameProtoObj(ostream& s);
  void emit_classnameInit(ostream& s);
  void emit_objTab(ostream& s);
  void emit_classnameMethods(ostream& s);
  int index;
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

