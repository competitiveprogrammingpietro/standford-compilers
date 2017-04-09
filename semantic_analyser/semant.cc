/*
 *  The semantic analyser for the COOL language.
 *  09/04/2017 Pietro Paolini : general.2.pulsarpietro@spamgourmet.com
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <exception>
#include <stack>
#include <vector>
#include <set>
#include <symtab.h>
#include <typeinfo>
#include "semant.h"
#include "utilities.h"
#include "stringtab.h"

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_s lot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");

    // 
}

static ClassTable * TABLE;
static Symbol FILENAME;
static tree_node * TREE_NODE;

// Method added to class_class
Symbol class__class::get_parent() 
{
  return parent;
}

Symbol class__class::get_name() 
{
  return name;
}

Features class__class::get_features()
{
  return features;
}

// The function pushes on the symbol table all attributes belonging
// to the parent's up to the Obejct class.
void class__class::push_hierarchy_attributes_scope()
{
  if (semant_debug)
    std::cerr << __func__ 
	      << ":" 
	      << name 
	      << std::endl;
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feature =  features->nth(i);
    TABLE->m_map.addid(feature->get_name(), new Symbol(feature->get_type()));
    if (semant_debug)
      std::cerr << "\t pushing:(" 
		<< feature->get_name() 
		<< "," 
		<< feature->get_type() 
		<< ")"
		<< std::endl;
  }
  if (parent == No_class)
    return;

  // Parent's attributes
  TABLE->find_class(parent)->push_hierarchy_attributes_scope();
}
// End class_class

// Method class

// The list of expressions passed as parameters need to be checked
// against the formal method's parameters.
bool method_class::is_conformant(Expressions actual)
{
  int i;
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << ":" << name << std::endl;

  // Arithy mismatch
  if (actual->len() != formals->len()) {
    TABLE->semant_error() << "error:arithy mismatch, given " 
			  << actual->len()
			  << " given "
			  << formals->len()
			  << std::endl;
    return false;
  }

  for(i = actual->first(); actual->more(i); i = actual->next(i)) {
    actual->nth(i)->semantic_analysis();
    Symbol actual_type = actual->nth(i)->get_type();
    Symbol formal_type = formals->nth(i)->get_type();
    if (!TABLE->is_conformant(formal_type, actual_type)) {
      TABLE->semant_error(FILENAME, TREE_NODE) << " In call of method "
					  << this->name
					  << " type "
					  << actual_type 
					  << " of parameter "
					  << formals->nth(i)->get_name()
					  << " does not conform to declated type "
					  << formal_type
					  << std::endl;
      return false;
    }
  }
  return true;
}

// Semantic analysis methods
bool class__class::semantic_analysis()
{
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << ""<< name << std::endl;
  FILENAME = this->filename;

  // It is not allowed to extend basic classes
  bool inherit_correct = (parent != Bool) &&
    (parent != Str) &&
    (parent != Int);
  if (!inherit_correct) {
    TABLE->semant_error(FILENAME, this) 
      << "Class "
      << name 
      << " cannot inherit class "
      << parent
      << "."
      << std::endl;
    return false;
  }
  TABLE->m_map.enterscope();
  TABLE->m_map.addid(SELF_TYPE, new Symbol(name));
  
  // Add upwards's hierarchy attributes 
  TABLE->find_class(this->parent)->push_hierarchy_attributes_scope();

  // Add class's attributes
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feature =  features->nth(i);
    feature->semantic_analysis();
  }
  TABLE->m_map.exitscope();
  return true;
}

bool method_class::semantic_analysis()
{
  bool  _ret = true;
  std::set<Symbol> formal_names;

  if (semant_debug)
    std::cerr << typeid(*this).name() 
	      << "::" 
	      << __func__ 
	      << "-"
	      << name
	      << std::endl;
  
  
  TABLE->m_map.addid(name, new Symbol(return_type));
  TABLE->m_map.enterscope();

  // Check if formal return type exists
  if (!TABLE->find_class(return_type)) {
    TABLE->semant_error(FILENAME, this) 
      << "Undefined return type "
      << return_type
      << " in method "
      << name
      << "."
      << std::endl;
    _ret = false;
  }

  // Add all formal parameters to the scope
  for(int i = formals->first(); formals->more(i); i = formals->next(i)) {

    // Multiple definition of the same formal's name not allowed
    if (formal_names.find(formals->nth(i)->get_name()) != formal_names.end()) {
      TABLE->semant_error(FILENAME, this) 
	<< "Formal parameter "
	<< formals->nth(i)->get_name()
	<< "is multiply defined."
	<< std::endl;
    }
    formal_names.insert(formals->nth(i)->get_name());
    
    // It is now allowed to use SELF_TYPE as formal parameter - warning only
    // we do not stop scanning
    if (formals->nth(i)->get_type() == SELF_TYPE) {
      TABLE->semant_error(FILENAME, this) 
	<< "Formal parameter "
	<< formals->nth(i)->get_name()
	<< " cannot have type SELF_TYPE"
	<< std::endl;
    }
    _ret = formals->nth(i)->semantic_analysis();
  }

  // Add self and evaluate body
  TABLE->m_map.addid(self, new Symbol(SELF_TYPE));
  expr->semantic_analysis();
  bool conform = TABLE->is_conformant(return_type, expr->get_type());
  if (!conform) {
    TABLE->semant_error(FILENAME, this) 
      << "Undefined return type "
      << return_type
      << " in method "
      << name
      << "."
      << std::endl;
    _ret = false;
  }
  TABLE->m_map.exitscope();
  return _ret;
}

bool attr_class::semantic_analysis()
{
  if (semant_debug) {
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  }
  if (name == self) {
    TABLE->semant_error(FILENAME, this)
      << "'self' cannot be the name of an attribute."
      << std::endl;
    return false;
  }

  // The Cool language does not allow inherited attribute redefinition 
  if (TABLE->m_map.probe(name)) {
    TABLE->semant_error(FILENAME, this) 
      << "Attribute "
      << name
      << " is an attribute of an inherited class."
      << std::endl;
    return false;
  }

  /*
   * Check if the attribute's type exists by checking if it is a user-defined
   * type or a primite type - type error otherwise
   */
  if (!TABLE->is_base_class(type_decl) &&
      !TABLE->find_class(type_decl)) {
    std::cerr << "Unknown type " << type_decl << " for " << name  <<  std::endl;
    return false;
  }
  init->semantic_analysis();

  /* Check if the init expression matches the declaration's type - this check 
   * happens only if there is an  initialization for the attribute
   */
  if (!init->is_null_expr() && 
      !TABLE->is_conformant(type_decl, init->get_type())) {
      std::cerr << "Type declaration " 
		<< type_decl << " for " 
		<< name  
		<<  " does not match initialization expression " 
		<< init->get_type()
		<<  std::endl;
      return false;
  }
  if (semant_debug)
    std::cerr << "map.addid(" << name << "," << type_decl << ")" <<  std::endl;
  TABLE->m_map.addid(name, new Symbol(type_decl));
  return true;
}

bool formal_class::semantic_analysis()
{
  if (semant_debug) {
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
    std::cerr << "map.addid(" << name << "," << type_decl << ")" <<  std::endl;
  }
  TABLE->m_map.addid(name, new Symbol(type_decl));
  return true;
}

bool branch_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  return true;
}

bool assign_class::semantic_analysis()
{  
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  
  // Undeclared identifier
  if (!TABLE->m_map.lookup(name)) {
    TABLE->semant_error(FILENAME, this) 
      << " Undeclared identifier "
      << name
      << "."
      << std::endl;
    type = Object;
    return false;
  }

  if (!expr->semantic_analysis())
    return false;
  
  Symbol rh_type = expr->get_type();
  Symbol * lh_type = TABLE->m_map.lookup(name);
  if (!lh_type)
    return false;
  if (semant_debug)
    std::cerr << *lh_type << " <- " << rh_type << std::endl;
  if (!TABLE->is_conformant(*lh_type, rh_type)) {
    TABLE->semant_error(FILENAME, this) 
      << "Type "
      << rh_type
      << " of assigned expression does not conform to declared type "
      <<  *lh_type
      << " of identifier "
      << name 
      << "."
      << std::endl;
    //    std::cerr << "Mismatch in type " << std::endl;
    type = Object;
    return false;
  }
  TABLE->m_map.addid(name, new Symbol(rh_type));
  type = rh_type;
  return true;
}

bool static_dispatch_class::semantic_analysis()
{
 if (semant_debug)
   std::cerr << typeid(*this).name() << "::" << __func__ << ":" <<  name << std::endl;

 Symbol e0_type;
 Class_ class_;
 Feature method;

 // The e0 type MUST be conformant to the static type
 expr->semantic_analysis();
 e0_type = expr->get_type();
 if (!TABLE->is_conformant(type_name, e0_type)) {
   TABLE->semant_error(FILENAME, this) 
     << "Expression type "
     << e0_type
     << " does not conform to declared static dispatch type "
     << type_name
     << std::endl;
   type = Object;
   return false;
 }
 class_ = TABLE->find_class(type_name);
 
 // Object's type method is called upon is invalid
 if(!class_) {
   TABLE->semant_error() << " No valid type " << e0_type;
   return false;
 }
 method = TABLE->find_method(class_, name);
 
 // There is not method for such class
 if (!method) {
   TABLE->semant_error(FILENAME, this) << " There is no method " 
			 << name
			 << " for class "
			 << class_->get_name()
			 << std::endl;
   return false;
 }
 type = method->get_type();
 return true;
}

bool dispatch_class::semantic_analysis()
{
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << ":" <<  name << std::endl;
 
  // Find e0's type
  Symbol e0_type;
  Feature method;
  Class_ class_;
  expr->semantic_analysis();
  e0_type = expr->get_type();
  if (e0_type == SELF_TYPE)
    e0_type = *(TABLE->m_map.lookup(SELF_TYPE));
  class_ = TABLE->find_class(e0_type);

  // Object's type method is called upon is invalid
  if(!class_) {
    TABLE->semant_error() << " No valid type " << e0_type << std::endl;
    type = Object;
    return false;
  }
  method = TABLE->find_method(class_, name);
  // Method's name invalid for the given class's type
  if (!method) {
    TABLE->semant_error(FILENAME, this) 
      << "Dispatch to undefined method " 
      << name
      << "."
      << std::endl;
    type = Object;
    return false;
  }

  TREE_NODE = this;
  // For all  [ei..en] 0 < i < n type_actual(ei) MUST be equal to type_formal(ei) 
  if (!method->is_conformant(actual)) {
    type = Object;
    return false;
  }

  /* 
   *  This will result the IO test failing - but Cool manual and dispatch-test
   *  excpects SELF_TYPE to be 'risolved' in a real type'
   *  The IO test is likely to be broken.
  */
  type = (method->get_type() == SELF_TYPE) ? 
    class_->get_name() : method->get_type();
  return true;
}

bool cond_class::semantic_analysis()
{
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  
  pred->semantic_analysis();
  if (pred->get_type() != Bool) {
    TABLE->semant_error(FILENAME, this) << "predicate type different from boolean" << std::endl;
    return false;
  }
  then_exp->semantic_analysis();
  else_exp->semantic_analysis();
  type = TABLE->common_ancestor(then_exp->get_type(), else_exp->get_type());
  if (semant_debug) {
    std::cerr << typeid(*this).name() << "::" << __func__ 
	      << " Common ancestor:"
	      << type
	      << std::endl;
  }
  return true;
}

bool loop_class::semantic_analysis()
{
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  pred->semantic_analysis();
  if (pred->get_type() != Bool) {
    TABLE->semant_error(FILENAME, this) << "Loop condition does not have type Bool" << std::endl;
    type = Object;
    return false;
  }
  body->semantic_analysis();
  type = Object;
  return true;
}

bool typcase_class::semantic_analysis()
{
  Case case_;
  std::set<Symbol> branches_type;

  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  expr->semantic_analysis();

  // Compute the first branch
  case_ = cases->nth(0);
  TABLE->m_map.addid(case_->get_name(), new Symbol(case_->get_type()));
  case_->get_expr()->semantic_analysis();
  type = case_->get_expr()->get_type();
  branches_type.insert(type);

  for (int i = 1; cases->more(i); i = cases->next(i)) {
    case_ = cases->nth(i);
    TABLE->m_map.addid(case_->get_name(), new Symbol(case_->get_type()));
    case_->get_expr()->semantic_analysis();

    // It is not allowed to have multiple branches with identical type in a
    // case statement
    if (branches_type.find(case_->get_expr()->get_type()) != branches_type.end()) {
      TABLE->semant_error(FILENAME, this)
	<< "Duplicate branch "
	<< case_->get_expr()->get_type()
	<< " in case statement."
	<< std::endl;
      type = Object;
      return false;
    }
    branches_type.insert(case_->get_expr()->get_type());

    // Compute the common ancestor of the branches evaluated until now
    type = TABLE->common_ancestor(type, case_->get_expr()->get_type());
  }
  
  // Placeholder waiting for common ancestor algorithm
  return true;
}

bool block_class::semantic_analysis()
{
 bool ret;

 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
 TABLE->m_map.enterscope();
 for (int i = 0; body->more(i); i = body->next(i)) {
   ret = body->nth(i)->semantic_analysis();
   type =  body->nth(i)->get_type();
 }
 return ret;
}

bool let_class::semantic_analysis()
{
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;

  // No self binding
  if (identifier == self) {
    TABLE->semant_error(FILENAME, this) 
      << "'self' cannot be bound in a 'let' expression."
      << std::endl;
    type = type_decl;
    return false;
  }
  init->semantic_analysis();
  
  // If init expression is provided we must ensure its type conforms the identifier's type
  if (init->get_type() != No_type && !TABLE->is_conformant(type_decl, init->get_type())) {
    TABLE->semant_error(FILENAME, this) << " Let types not conformat" << std::endl;
      type = Object;
      return false;
  }
  TABLE->m_map.enterscope();
  TABLE->m_map.addid(identifier, new Symbol(type_decl));
  body->semantic_analysis();
  type = body->get_type();
  TABLE->m_map.exitscope();
  return true;
}

bool plus_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;

 e1->semantic_analysis();
 e2->semantic_analysis();
 if (e1->get_type() != Int || e1->get_type() != e2->get_type()) {
   TABLE->semant_error(FILENAME, this) 
     << "non Int arguments: "
     << e1->get_type()
     << " + "
     << e2->get_type()
     << std::endl;
   type = Object;
   return false;
 } 
 type = Int;
 return true;
}

bool sub_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;

 e1->semantic_analysis();
 e2->semantic_analysis();
 if (e1->get_type() != Int || e1->get_type() != e2->get_type()) {
   TABLE->semant_error(FILENAME, this) 
     << "non Int arguments: "
     << e1->get_type()
     << " + "
     << e2->get_type()
     << std::endl;
   type = Object;
   return false;
 } 
 type = Int;
 return true;
}

bool mul_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;

 e1->semantic_analysis();
 e2->semantic_analysis();
 if (e1->get_type() != Int || e1->get_type() != e2->get_type()) {
   TABLE->semant_error(FILENAME, this) 
     << "non Int arguments: "
     << e1->get_type()
     << " + "
     << e2->get_type()
     << std::endl;
   type = Object;
   return false;
 } 
 type = Int;
 return true;
}

bool divide_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;

 e1->semantic_analysis();
 e2->semantic_analysis();
 if (e1->get_type() != Int || e1->get_type() != e2->get_type()) {
  TABLE->semant_error(FILENAME, this) 
     << "non Int arguments: "
     << e1->get_type()
     << " + "
     << e2->get_type()
     << std::endl;
  type = Object;
  return false;
 } 
 type = Int;
 return true;
}

bool neg_class::semantic_analysis()
{
 if (semant_debug)
   std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;

 e1->semantic_analysis();

 // Expression type MUST be int
 if (e1->get_type() != Int) {
   std::cerr << "Type mismatch: " << e1->get_type() << std::endl;
   return false;
 } 
 type = Int;
 return true;
}

bool lt_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
 
 e1->semantic_analysis();
 e2->semantic_analysis();
 if (e1->get_type() != Int || e1->get_type() != e2->get_type()) {
   TABLE->semant_error() << "error: can't apply  < operator to type "
			 << e1->get_type()
			 << ","
			 << e2->get_type()
			 << std::endl;
   return false;
 }
 type = Bool;
 return true;
}

bool eq_class::semantic_analysis()
{
  if (semant_debug)
   std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  e1->semantic_analysis();
  e2->semantic_analysis();
  Symbol e1_type = e1->get_type();
  Symbol e2_type = e2->get_type();
  bool e1_basic_type = e1_type == Int || e1_type == Bool || e1_type == Str;
  bool e2_basic_type = e2_type == Int || e2_type == Bool || e2_type == Str;

  // If one of the two has type {Int, String, Bool} both neeed to have the same
  // type
  if (e1_basic_type || e2_basic_type) {
    if (e1_type != e2_type) {
      TABLE->semant_error(FILENAME, this) << " Illegal comparison with a basic type"  << std::endl;
      type = Object;
      return false;
    }
  }
  type = Bool;
  return true;
}

bool leq_class::semantic_analysis()
{
  if (semant_debug)
     std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  
 e1->semantic_analysis();
 e2->semantic_analysis();
 if (e1->get_type() != Int || e1->get_type() != e2->get_type()) {
   TABLE->semant_error() << "error: can't apply  <= operator to type "
			 << e1->get_type()
			 << ","
			 << e2->get_type()
			 << std::endl;
   return false;
 }
 type = Bool;
 return true;
}

bool comp_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
 e1->semantic_analysis();
 
 if (e1->get_type() != Bool) {
   TABLE->semant_error() << "error: can't apply the NOT operator to  "
			 << e1->get_type()
			 << std::endl;
   return false;
 }
 type = Bool;
 return true;
}

bool int_const_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  type = Int;
  return true;
}

bool bool_const_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  type = Bool;
  return true;
}

bool string_const_class::semantic_analysis()
{
 if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  type = Str;
  return true;
}

bool new__class::semantic_analysis()
{
  Symbol * new_type;
  
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  
  if (type_name == SELF_TYPE) {
    type = type_name;
    return true;
  }
  
  Class_ class_ = TABLE->find_class(type_name);
  if (!class_)   {
    std::cerr << "'new' used with undefined class " 
	      <<  type_name 
	      << "."
	      << std::endl;
    type = Object;
    return false;
  }
  type = idtable.lookup_string(type_name->get_string());
  return true;
}

bool isvoid_class::semantic_analysis()
{ 
  if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  e1->semantic_analysis();
  type = Bool;
  return true;
}

bool no_expr_class::semantic_analysis()
{ if (semant_debug)
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
  type = No_type;
  return true;
}

bool object_class::semantic_analysis()
{
  Symbol * type_obj;

  // Self case
  if (name == self) {
    type = SELF_TYPE;
    // type_obj = TABLE->m_map.lookup(SELF_TYPE);
    // type = *type_obj;
    return true;
  }
  type_obj = TABLE->m_map.lookup(name);
  if (!type_obj) {
    TABLE->semant_error(FILENAME, this) 
      << " Undeclared identifier "
      << name
      << "."
      << std::endl;
    type = Object;
    return false;
  }
  type = *type_obj;
  if (semant_debug) {
    std::cerr << typeid(*this).name() << "::" << __func__ << std::endl;
    std::cerr << name << ":" << *type_obj << std::endl;
  }
  return true;
}

// ClassNode
void ClassNode::add_child(ClassNode * node)
{
  this->m_children[m_index++] = node;
}

bool ClassNode::operator==(ClassNode &node)
{
  return this->m_value == node.m_value;
}

ostream& operator<<(ostream& stream, const ClassNode& node)
{
  const ClassNode *link;
  int i;

  i = 0;
  stream << "Node:" << node.m_value->get_name() << std::endl;
  if (node.m_index == 0)
    return stream;
  while(i < node.m_index) {
    link = node.m_children[i];
    stream << '\t' << link->m_value->get_name() << std::endl;
    i++;
  }
  return stream;
}
// End ClassNode

// ClassTable
ClassNode * ClassTable::add_node(Class_ class_)
{
  Class_ parent_class;
  ClassNode * node_class, * parent_node;
  
  // Defensive : do not add Object
  if (class_->get_name() == Object)
    return NULL;
  node_class = new ClassNode(class_);
  parent_class = find_class(class_->get_parent());
  if (semant_debug) {
    std::cout << class_->get_name()->get_string()
	      << "'s parent is "
	      << class_->get_parent()->get_string() 
	      << std::endl;
  }
  parent_node = find_node(parent_class->get_name());

  // If the parent hasn't been added to the graph yet add it 
  // with its hierarchy
  if (parent_node) {
    parent_node->add_child(node_class);
  } else {
    parent_node = add_node(parent_class);
    parent_node->add_child(node_class);
  }
  return node_class;
}

ClassNode * ClassTable::find_node(Symbol  item)
{
  std::stack<ClassNode *> Q;
  ClassNode * node;

  Q.push(m_root);
  while(!Q.empty()) {
    int i;

    node = Q.top();
    Q.pop();
    if (node->m_value->get_name() == item)
      return node;
    for (i = 0; i < node->m_index; i++) {
      ClassNode * tmp = node->m_children[i];
      Q.push(tmp);
    }
  }
  return NULL;
}

// Returns a stack of classes from the one given to 
// as argument till the base class Object
std::stack<Class_> ClassTable::object_path(Symbol class_)
{
  std::stack<Class_> Q;
  Class_ class_item;

  if (semant_debug) {
    std::cerr << __func__ 
	      <<  "("
	      << class_
	      << ")"
	      << std::endl;
  }

  class_item = find_class(class_);
  while(true) {
    Q.push(class_item);
    
    // Only Object has No_class as parent
    if (class_item->get_parent() == No_class)
      return Q;
    class_item = find_class(class_item->get_parent());
  }
  return Q;
}

bool ClassTable::is_conformant(Symbol parent, Symbol child)
{
 if (semant_debug) 
   std::cerr << __func__ 
	     << "(" 
	     << parent 
	     << "," 
	     << child 
	     << ")"
	     << std::endl;

 // Deal with the simplest cases 
 // 1. SELF_TYPE
 if (child == SELF_TYPE)
   child = *TABLE->m_map.lookup(SELF_TYPE);

 // 2. Same return type
 if (parent == SELF_TYPE)
   parent = *TABLE->m_map.lookup(SELF_TYPE);

 // 3. Both have the same type
 if (parent == child)
   return true;

 // 4. All classes inherit from Object
 if (parent == Object)
   return true;

 if (semant_debug) 
   std::cerr << __func__ 
	     << "(" 
	     << parent 
	     << "," 
	     << child 
	     << ")"
	     << std::endl;

 ClassNode * node;
 std::stack<ClassNode *> Q;  
 node = find_node(parent);
 if (!node)
   return false;
 Q.push(node);
 while(!Q.empty()) {
   char * node_name;
   int i;
   
   node = Q.top();
   Q.pop();
   if (node->m_value->get_name() == child)
     return true;
   for (i = 0; i < node->m_index; i++) {
     ClassNode * tmp = node->m_children[i];
     Q.push(tmp);
   }
 }
 return false;
}

bool ClassTable::semant_analysis()
{
  std::stack<ClassNode *> Q;
  ClassNode * node;
  bool result;

  result = true;
  Q.push(m_root);
  while(!Q.empty()) {
    int i;
    node = Q.top();
    Q.pop();

    for (i = 0; i < node->m_index; i++) {
      ClassNode * tmp = node->m_children[i];
      Q.push(tmp);
    }

    // Base classes aren't analysed
    if (is_base_class(node->m_value->get_name())) {
      if (semant_debug)
	std::cerr << " Skipping " << node->m_value->get_name() << std::endl;
      continue;
    }
    if (!node->m_value->semantic_analysis())
      result = false;
  }
  return result;
}

ostream& operator<<(ostream& stream, const ClassTable& obj)
{
  int i;
  std::stack<ClassNode *> Q;
  ClassNode * node;

  Q.push(obj.m_root);
  while(!Q.empty()) {
    node = Q.top();
    Q.pop();
    stream << *node << std::endl;
    for (i = 0; i < node->m_index; i++) {
      ClassNode * tmp = node->m_children[i];
      Q.push(tmp);
    }

  }
  return stream;
}


Class_ ClassTable::find_class(Symbol name)
{
  for (int i = m_classes->first(); m_classes->more(i); i = m_classes->next(i)) {
    if (name != m_classes->nth(i)->get_name())
      continue;
    return m_classes->nth(i);
  }
  return NULL;
}


// To find a cycles() in the graph is sufficient to check if
// the a node's parent's parent does not inherit from the node
// itself.
// This simplification is caused by the fact that there is no
//  multiple inheritance in COOL.
bool ClassTable::cycles()
{
  Class_ class_, parent;
  for (int i = m_classes->first(); m_classes->more(i); i = m_classes->next(i)) {
    class_ = m_classes->nth(i);

    if (class_->get_name() == Object)
      continue;
    
    parent = NULL;
    for (int i = m_classes->first(); m_classes->more(i); i = m_classes->next(i)) {
      if (class_->get_name() != m_classes->nth(i)->get_name())
	continue;
      parent = m_classes->nth(i);
      break;
    }
    
    // Class chain incomplete
    if (!parent) {
      cerr << "Class " << class_->get_name() << "'s parent " << class_->get_parent() << " not found" << std::endl;
      return true;
    }

    if (semant_debug) {
      std::cout << class_->get_name() << " inherits " << parent->get_name() << std::endl;
    }

    // Cycle  
    char * parent_parent =  parent->get_parent()->get_string();
    if (strcmp(class_->get_name()->get_string(), parent_parent) == 0) {
      std::cerr << "Cycle detected " << class_->get_name() << "<->" << class_->get_parent() << std::endl; 
      return true;
    }
  }
  return false;
}

bool ClassTable::is_base_class(Symbol name) 
{
  if (name == Object ||
      name == IO ||
      name == Int ||
      name == Bool ||
      name == Str)
    return true;
  return false;
}

// In COOL a method is unequivocally determined by its name
Feature ClassTable::find_method(Class_ class_, Symbol name)
{
  Features features = class_->get_features();
  for(int i = features->first(); features->more(i); i = features->next(i)) {
    Feature feature =  features->nth(i);
    if (!feature->is_method())
      continue;
    if (feature->get_name() != name)
      continue;

    // Better way to do it ?
    return  feature;
  }
  
  // Termination condition
  if (class_->get_parent() == No_class)
    return NULL;

  // Look up in the classes' hierarchy
  Class_ parent = TABLE->find_class(class_->get_parent());
  return TABLE->find_method(parent, name);
}

Symbol ClassTable::common_ancestor(Symbol one, Symbol two)
{
  if(semant_debug)  
    std::cerr << typeid(*this).name() 
	      << __func__ 
	      << "("
	      << one
	      << ","
	      << two
	      << ")"
	      << std::endl;

  // Simplest case
  if (one == two)
    return one;

  Class_ common_ancestor;
  std::stack<Class_> path_one;
  std::stack<Class_> path_two;

  path_one = object_path(one);
  path_two = object_path(two);

  /* The loop scans the classes' hierarchy
   * forwards and either updates the common
   * ancestor when they match or returns the
   * one computed otherwise
   */ 
  while (true) {
    if (path_one.empty() || path_two.empty())
      return common_ancestor->get_name();

    if (semant_debug) {
      std::cerr << path_one.top()->get_name()
		<< "<->"
		<< path_two.top()->get_name()
		<< std::endl;
    }
    
    if (path_one.top() == path_two.top())
      common_ancestor = path_one.top();
    if (path_one.top() != path_two.top())
      return common_ancestor->get_name();
    path_one.pop();
    path_two.pop();
  }
  // this shoud never be the case
  throw;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr)
{
  // The tree package uses these globals to annotate the classes built below.
  // curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");
  
  // The following demonstrates how to create dummy parse trees to
  // refer to basic Cool classes.  There's no need for method
  // bodies -- these are already built into the runtime system.
  
  // IMPORTANT: The results of the following expressions are
  // stored in local variables.  You will want to do something
  // with those variables at the end of this method to make this
  // code meaningful.
  
  // 
  // The Object class has no parent class. Its methods are
  //        abort() : Object    aborts the program
  //        type_name() : Str   returns a string representation of class name
  //        copy() : SELF_TYPE  returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  
  Class_ Object_class =
    class_(Object, 
	   No_class,
	   append_Features(
			   append_Features(
					   single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					   single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename);
  
  // 
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE       writes a string to the output
  //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
  //        in_string() : Str                 reads a string from the input
  //        in_int() : Int                      "   an int     "  "     "
  //
  Class_ IO_class = 
    class_(IO, 
	   Object,
	   append_Features(
			   append_Features(
					   append_Features(
							   single_Features(method(out_string, single_Formals(formal(arg, Str)),
										  SELF_TYPE, no_expr())),
							   single_Features(method(out_int, single_Formals(formal(arg, Int)),
										  SELF_TYPE, no_expr()))),
					   single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			   single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename);  
  
  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer. 
  //
  Class_ Int_class =
    class_(Int, 
	   Object,
	   single_Features(attr(val, prim_slot, no_expr())),
	   filename);
  
  //
  // Bool also has only the "val" slot.
  //
  Class_ Bool_class =
    class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
  
  //
  // The class Str has a number of slots and operations:
  //       val                                  the length of the string
  //       str_field                            the string itself
  //       length() : Int                       returns length of the string
  //       concat(arg: Str) : Str               performs string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring selection
  //       
  Class_ Str_class =
    class_(Str, 
	   Object,
	   append_Features(
			   append_Features(
					   append_Features(
							   append_Features(
									   single_Features(attr(val, Int, no_expr())),
									   single_Features(attr(str_field, prim_slot, no_expr()))),
							   single_Features(method(length, nil_Formals(), Int, no_expr()))),
					   single_Features(method(concat, 
								  single_Formals(formal(arg, Str)),
								  Str, 
								  no_expr()))),
			   single_Features(method(substr, 
						  append_Formals(single_Formals(formal(arg, Int)), 
								 single_Formals(formal(arg2, Int))),
						  Str, 
						  no_expr()))),
	   filename);
  m_classes = nil_Classes();
  m_classes = append_Classes(m_classes, classes);

  // 1. There cannot be redefinition of basic classes
  // 2. Class Main must be defined
  // 3. Class's name cannot be 'SELF_TYPE'
  bool main = false;
  Class_ class_item;
  for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
    Class_ duplicate;
    class_item = classes->nth(i);
    if (class_item->get_name() == SELF_TYPE) {
      semant_error(class_item)
	<< "Redefinition of basic class SELF_TYPE."
	<< std::endl;
    }
    
    // The same class can't be defined more than once
    for (int j = classes->first(); classes->more(j); j = classes->next(j)) {
      duplicate = classes->nth(j);
      if (duplicate == class_item)
	continue;
      if (duplicate->get_name() != class_item->get_name())
	continue;
      semant_error(class_item)
	<< "Class "
	<<  class_item->get_name()
	<< " was previously defined."
	<< std::endl;
    }

    // Main class found
    if (class_item->get_name() == Main)
      main = true;

    if (!is_base_class(class_item->get_name()))
      continue;

    // Basic type redefinition
    semant_error(class_item)
      << " Redefinition of basic class "
      << class_item->get_name()
      << std::endl;
  }
  
  if (!main) {
      semant_error(class_item)
      << "Class Main is not defined."
      << std::endl;
  }

  // Add base classes
  m_classes = append_Classes(m_classes, single_Classes(Object_class));
  m_classes = append_Classes(m_classes, single_Classes(Int_class));
  m_classes = append_Classes(m_classes, single_Classes(IO_class));
  m_classes = append_Classes(m_classes, single_Classes(Bool_class));
  m_classes = append_Classes(m_classes, single_Classes(Str_class));
  m_root = new ClassNode(Object_class);

  // Classes mush inherit from a defined class - this check must be done
  // after the base classes have been added to the classes' list
  for (int i = m_classes->first(); m_classes->more(i); i = m_classes->next(i)) {
    class_item = m_classes->nth(i);
    
    if (class_item->get_parent() == No_class)
      continue;
   
    if (find_class(class_item->get_parent()))
      continue;
    
    // Missing parent
    semant_error(class_item)
      << "Class "
      << class_item->get_name()
      << " inherits from an undefined class "
      << class_item->get_parent()
      << "."
      << std::endl;
  }
}

// Build hierarchy - there MUST NOT be cycles
void ClassTable::build_hierarchy()
{
  for (int i = m_classes->first(); m_classes->more(i); i = m_classes->next(i))
      this->add_node(m_classes->nth(i));
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
  error_stream << filename << ":" << t->get_line_number() << ": ";
  return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
  initialize_constants();
  //semant_debug = 1;
  TABLE  = new ClassTable(classes);

  // The constructor carries out some semantic analysis
  if (TABLE->errors()) {
    cerr << "Compilation halted due to static semantic errors" << endl;
    exit(1);
  }

  if (TABLE->cycles()) {
    TABLE->semant_error();
  } else { 
    //      std::cerr << *ClassHierarchy::Instance() << std::endl;
    TABLE->build_hierarchy();
    TABLE->semant_analysis();
    //idtable.print();
  }
  
  /* some semantic analysis code may go here */
  if (TABLE->errors()) {
    cerr << "Compilation halted due to static semantic errors" << endl;
    exit(1);
  }
}
