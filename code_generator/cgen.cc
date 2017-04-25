/*
 *  The code generator for the COOL language.
 *  24/04/2017 Pietro Paolini : general.2.pulsarpietro@spamgourmet.com
 *
 */

//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"


extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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
       val,
       parameter,
       temporary;
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
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
  parameter   = idtable.add_string("parameter");
  temporary   = idtable.add_string("temporary");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

// Global object for name resolution
CgenClassTable * g_codegen_classtable;
NameResolver *   g_name_resolver;
int              label_counter;

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  // Abort case

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgt(char *src1, char * src2, int label, ostream &s)
{
  s << BGT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}


//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD; emit_disptable_ref(Str, s);


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD;  emit_disptable_ref(Int, s);

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD; emit_disptable_ref(Bool, s);

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  Utility functions written by me.
//
//////////////////////////////////////////////////////////////////////////////
bool static is_base_type(Symbol type) 
{
  if (type == Int || type == Str || type == Bool)
    return true;
  return false;
}

void static initialize_default_value(Symbol type, ostream& s) 
{
  if (type == Str) 
       emit_load_string(ACC,stringtable.lookup_string(""),s);
  else if (type == Int)
       emit_load_int(ACC,inttable.lookup_string("0"),s);
  else if (type == Bool)
    emit_load_bool(ACC, falsebool, s);
  else
    emit_load_imm(ACC, 0, s); // Void
} 

static void perform_arithmetic_operation(ostream& s,
					 Expression e1,
					 Expression e2,
					 ArithmeticOperation op)
{
  // Shared code among arith. operation
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_push(ACC, s);
  g_name_resolver->add(temporary, STACK);
  e2->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  g_name_resolver->pop_sp(1);

  // ACC holds e2 while T1 holds e1
  switch(op) {
  case PLUS:
    emit_add(ACC, ACC, T1, s);
    break;
  case MINUS:
    emit_sub(ACC, T1, ACC, s);
    break;
  case STAR:
    emit_mul(ACC, ACC, T1, s);
    break;
  case SLASH:
    emit_div(ACC, T1, ACC, s);
    break;
  }

  // Save the actual integer in the stack
  // and proceed creating an Integer object
  // to be filled with such value
  emit_push(ACC, s);
  emit_load_address(ACC, INTNAME""PROTOBJ_SUFFIX, s);
  emit_jal(OBJECT_COPY, s);
  emit_jal(INTNAME""CLASSINIT_SUFFIX, s);
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  emit_store(T1, 3, ACC, s);
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{ 
  enterscope();
  if (cgen_debug) cout << "Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();
  
  // TODO: not ideal but I can't change the fact that the project
  // performs all the code generation in the constructor
  g_codegen_classtable = this;
  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i)) {
    CgenNode * item = new CgenNode(cs->nth(i),NotBasic,this);
    install_class(item);
  }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  CgenNode * root = probe(Object);

  // Assign classes' tag and assign them to the basic type
  root->assign_index(0);
  stringclasstag = probe(Str)->index;
  intclasstag =    probe(Int)->index;
  boolclasstag =   probe(Bool)->index;

  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

  //                 Add your code to emit
  //                   - prototype objects
  //                   - class_nameTab
  //                   - dispatch tables

  // Compute all features
  root->compute_features();

  // class_nameTab
  str << CLASSNAMETAB << LABEL;
  root->emit_classnameTab_entry(str);

  // classes' dispatch tab
  root->emit_classnameDispTab(str);
  
  // classes' protoobj
  root->emit_classnameProtoObj(str); 
  
  // classes' Obj tab
  str << CLASSOBJTAB << LABEL;
  root->emit_objTab(str);

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

  // Object Initializer
  root->emit_classnameInit(str);

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  root->emit_classnameMethods(str);
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////
CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus),
   index(-1)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

std::ostream& operator<<(std::ostream& out, const CgenNode& c)
{
  out << "CgenNode: " << c.name << "[" << c.index << "]" << std::endl;
  out << "\t Parent :" << c.parentnd->name << std::endl; 
  out << "\t Children:\n";
  for(List<CgenNode> *l = c.children; l; l = l->tl()) {
    out << "\t\t" << l->hd()->get_name() << " " << std::endl;
  }
  out << "\n\t Attributes : ";
  for(std::list<attr_class>::const_iterator it = c.attributes.begin();
      it != c.attributes.end();
      it++)
    out << "\n\t\t" << *it->name;
  out << "\n\t Methods :";
  for(std::list<Method>::const_iterator it = c.methods.begin();
      it != c.methods.end();
      it++)
    out << "\n\t\t" << it->m_class << "." << it->m_method.name;
  out << std::endl;
  return out;
}

void CgenNode::inherit(std::list<attr_class>& inh_attributes, std::list<Method>& inh_methods)
{
  this->attributes = std::list<attr_class>(inh_attributes);
  this->methods = std::list<Method>(inh_methods);
}
// Fill method's and attributes of the tree whose root is node this
void CgenNode::compute_features()
{
  Feature feature;
  if (cgen_debug)
    std::cerr << __func__ << ":" << this->name << std::endl;

  // Insert the feature at the end of the attributes and methods list
  // respectively 
  for (int i = this->features->first();
       this->features->more(i); 
       i = this->features->next(i)) {
    feature = this->features->nth(i);
    if (cgen_debug)
      std::cerr << "\tAdding " << feature->get_name() << std::endl;
    if (feature->is_method()) 
      this->methods.insert(this->methods.end(),
			   Method(name, (method_class &) *feature));
    else
      this->attributes.insert(this->attributes.end(), (attr_class &) *feature);
  }  

  // Compute children's features
  for(List<CgenNode> *l = children; l; l = l->tl()) {
    l->hd()->inherit(this->attributes, this->methods);
    l->hd()->compute_features();
  }
}

void CgenNode::assign_index(int base)
{
  this->index = base++;
  for(List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->assign_index(base++);
}

int CgenNode::attribute_index(Symbol attribute_name)
{
  int index = 0;
  for (std::list<attr_class>::iterator it = this->attributes.begin();
       it != this->attributes.end();
       it++, index++)
    {
      if (it->name == attribute_name)
	return index;
    }
  return -1;
}

int CgenNode::method_index(Symbol name)
{
  int index = 0;
  for (std::list<Method>::iterator it = this->methods.begin();
       it != this->methods.end();
       it++, index++)
    {
      if (it->m_method.name == name)
	return index;
    }
  return -1;
}

method_class CgenNode::get_method(Symbol name)
{
  int index = 0;
  for (std::list<Method>::iterator it = this->methods.begin();
       it != this->methods.end();
       it++, index++)
    {
      if (it->m_method.name == name)
	return it->m_method;
    }
  throw;
}

void CgenNode::emit_classnameTab_entry(ostream& s) 
{
 if (cgen_debug)
   std::cerr << __func__ << ":" << this->name << std::endl;

  // A bit tortuous - I haven't found a better way yet
 s << WORD;
 stringtable.lookup_string(this->name->get_string())->code_ref(s);
 s << std::endl;
  
 // Call on every children
 for(List<CgenNode> *l = children; l; l = l->tl())
   l->hd()->emit_classnameTab_entry(s);
}

void CgenNode::emit_classnameDispTab(ostream& s) 
{
  if (cgen_debug)
   std::cerr << __func__ << ":" << this->name << std::endl;
  
  s << this->name << DISPTAB_SUFFIX << LABEL;
  for (std::list<Method>::iterator it = this->methods.begin(); 
       it != this->methods.end();
       it++) 
    s << WORD << it->m_class << "." << it->m_method.name << endl;
  for(List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->emit_classnameDispTab(s);
}

void CgenNode::emit_classnameProtoObj(ostream& s) 
{
 if (cgen_debug)
   std::cerr << __func__ << ":" << this->name << std::endl;

  s << WORD << "-1" << endl;
  s << this->name << PROTOBJ_SUFFIX << LABEL;
  s << WORD << this->index << endl;
  
  // Class tag, dispatch pointer and size always included 
  s << WORD << ( 3 + this->attributes.size()) << endl;
  s << WORD << this->name << DISPTAB_SUFFIX << endl;
  for (std::list<attr_class>::iterator it = this->attributes.begin();
       it != this->attributes.end();
       it++)
    {
      Symbol type = it->type_decl;
      if (type == Str || type == Int || type == Bool)
	s << WORD;

      if (type == Str)
	stringtable.lookup_string("")->code_ref(s);
      else if (type == Int)
	inttable.lookup_string("0")->code_ref(s);
      else if (type == Bool)
	falsebool.code_ref(s);
      else
	s << WORD << "0";
      s << endl;
    }
  for(List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->emit_classnameProtoObj(s);
}

void CgenNode::emit_classnameInit(ostream& s) 
{
  int index;

  if (cgen_debug)
   std::cerr << __func__ << ":" << this->name << std::endl;
  
  // Method's name label
  s << this->name << CLASSINIT_SUFFIX << LABEL;
  
  // Build activation record (FP, Parameter(self), RA) (FP set ?)
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);

  // Save ACC into SELF
  emit_move(SELF, ACC, s);

  // Call parent's init if not Object
  if (this->name != Object) {
    char parent_init[128] = { 0 };
    strcpy(parent_init, this->parentnd->name->get_string());
    strcat(parent_init, CLASSINIT_SUFFIX);
    if (cgen_debug)
      std::cerr << __func__ << ":" << parent_init << std::endl;
    emit_jal(parent_init, s);    
  }

  // Initialize attributes
  index = 0;
  for (std::list<attr_class>::iterator it = this->attributes.begin();
       it != this->attributes.end();
       it++) {

    if (cgen_debug)
      std::cerr << __func__ << ": init attribute " << it->name << std::endl;
    
    // Initialize using the default initialization only if it is a basic type
    // and no initialization expression is provided. 
    // A null_expr() evaluation generates a void result which is what we want
    // for non basic types
    if (is_base_type(it->type_decl) && it->init->is_null_expr())
      initialize_default_value(it->type_decl, s);
    else
      it->init->code(s);
    
    // Store the computed value into the object
    emit_store(ACC, ATTRIBUTES_OFFSET(index), SELF, s);
    index++;
  }
  
  // Restore ACC from SELF 
  emit_move(ACC, SELF, s);

  // Restore and remove activation record
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  emit_return(s);
  for(List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->emit_classnameInit(s);
}

void CgenNode::emit_objTab(ostream& s)
{

  if (cgen_debug)
    std::cerr << __func__ << ":" << this->name << std::endl;

  // Proto object, init function
  s << WORD << this->name << PROTOBJ_SUFFIX << std::endl;
  s << WORD << this->name << CLASSINIT_SUFFIX << std::endl;
  
  for(List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->emit_objTab(s);
}

void CgenNode::emit_classnameMethods(ostream& s)
{
  int stack_space;
  if (cgen_debug)
    std::cerr << __func__ << ":" << this->name << std::endl;
  
  // We do not bother with memory leaks
  g_name_resolver = new NameResolver(*this);

  // Do not generate basic classes' methods
  if (basic_status == Basic) {
    if (cgen_debug)
      std::cerr << __func__ << ": skipping " << this->name << std::endl;
    for(List<CgenNode> *l = children; l; l = l->tl())
      l->hd()->emit_classnameMethods(s);
    return;
  }

  // For all methods in the class
  for (std::list<Method>::iterator it = this->methods.begin();
       it != this->methods.end();
       it++) {    
    stack_space = 0;

    // Skipping parent's method
    if (it->m_class != this->name) {
      if (cgen_debug)
	std::cerr << __func__ << ": skipping " 
		  <<  it->m_class << "." << it->m_method.name << std::endl;
      continue;
    }
    if (cgen_debug)
	std::cerr << __func__ << ": generating " 
		  <<  it->m_class << "." << it->m_method.name << std::endl;
    method_class method = it->m_method;
    
    // Generate method's label
    s << it->m_class << "." << method.name << LABEL;

    // Build activation record (FP, Parameter(self), RA)
    emit_addiu(SP, SP, -12, s);
    emit_store(FP, 3, SP, s);
    emit_store(SELF, 2, SP, s);
    emit_store(RA, 1, SP, s);
    emit_addiu(FP, SP, 4, s);
    stack_space += 3;
    
    // Push method's formals's name
    Formal formal;
    int i =  method.formals->first();
    for (; method.formals->more(i); i = method.formals->next(i)) {
      formal = method.formals->nth(i);
      g_name_resolver->add(formal->get_name(), FRAME_POINTER);
    }

    // Save ACC to self
    emit_move(SELF, ACC, s);

    // Evaluate body
    method.expr->code(s);

    // Clear stack from stack arguments and parameters passed by caller
    stack_space += g_name_resolver->size_sp();
    stack_space += g_name_resolver->size_fp();
    g_name_resolver->clear_fp();
    g_name_resolver->clear_sp();

    // Restore registers and remove activation record
    emit_load(FP, 3, SP, s);
    emit_load(SELF, 2, SP, s);
    emit_load(RA, 1, SP, s);
    emit_addiu(SP, SP, stack_space * 4, s);
    emit_return(s);
  }
  
  // Call on children
  for(List<CgenNode> *l = children; l; l = l->tl())
    l->hd()->emit_classnameMethods(s);
  return;
}
//******************************************************
//  Name Resolver class methods' implementation
//******************************************************

void NameResolver::add(Symbol name, LocationType type)
{
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" 
  	      << __func__ << "(" << name 
  	      << ", " << type << ")" << std::endl;
  switch(type) {
  case STACK:
    this->m_list_sp.insert(this->m_list_sp.begin(), name);
    break;
  case FRAME_POINTER:
    this->m_list_fp.insert(this->m_list_fp.begin(), name);
    break;
  default:
    throw;
  }
  return;
}

int NameResolver::size_sp()
{
  return m_list_sp.size();
}

void NameResolver::pop_sp(int n)
{
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" 
	      << __func__ << "(" 
	      << n << ")" << std::endl;
  std::list<Symbol>::iterator it = this->m_list_sp.begin();

  // It shouldn't be needed TODO: investigate
  if (n == 1) {
    this->m_list_sp.erase(it);
    return;
  }
  for (int i = 0; i < n; i++)
   it++;
  this->m_list_sp.erase(this->m_list_sp.begin(), it);
  return;
}

void NameResolver::clear_fp()
{
  this->m_list_fp.clear();
}

void NameResolver::clear_sp()
{
  this->m_list_sp.clear();
}

int NameResolver::size_fp()
{
  return this->m_list_fp.size();
}

std::pair<LocationType, int> NameResolver::name_to_address(Symbol name)
{
   if (cgen_debug)
    std::cerr << __func__ << ":" << name << std::endl;

  std::list<Symbol>::iterator it = this->m_list_sp.begin();
  std::list<Symbol>::iterator end = this->m_list_sp.end();
  int index;

  // Scan the stack first
  for (index = 1; it != end; it++, index++) {
    if (cgen_debug)
      std::cerr << __func__ << ":scan_stack " << *it << std::endl;

    if (*it != name)
      continue;
    
    if (cgen_debug) 
      std::cerr << __func__ << ":" << name << " was found on the stack at index " << index << std::endl;

    std::pair<LocationType, int> ret(STACK, index);
    return ret;
  }

  // Scan the frame pointer TODO: write private function to avoid copy and paste
  // NOTE: index starts from zero this time
  it = this->m_list_fp.begin();
  end = this->m_list_fp.end();
  for (index = 0; it != end; it++, index++) {
   if (cgen_debug)
     std::cerr << __func__ << ":scan_fp " << *it << std::endl;

    if (*it != name)
      continue;
    
    if (cgen_debug) 
      std::cerr << __func__ << ":" << name << " was found on the frame pointer at index " << index << std::endl;

    // [FP, SELF, RA] 
    index = index + 3;

    std::pair<LocationType, int> ret(FRAME_POINTER, index);
    return ret;
  }
  
  // Scan the class's fields - it requires to the self object to be stored in s0
  if ((index = this->m_class.attribute_index(name)) != -1) {
     if (cgen_debug)
       std::cerr << __func__ << ":class_field " << index << std::endl;

     // [Class tag, Object size, Dispatch pointer, Attr1, .., Attrn]
     index = ATTRIBUTES_OFFSET(index);
     if (cgen_debug) 
       std::cerr << __func__ << ":" << name << " was found among class's members at index " << index << std::endl;
     std::pair<LocationType, int> ret(CLASS_FIELD, index);
     return ret;
  }
  
  // Shouldn't happend - TODO: should be better handled as well
  if (cgen_debug) 
    std::cerr << __func__ << ":" << name << " was found anywhere !" << std::endl;
  throw;
}

void NameResolver::emit_load_name(Symbol name, ostream& s)
{
  if (cgen_debug)
    std::cerr << __func__  << std::endl;
  
  std::pair<LocationType, int> lookup = this->name_to_address(name);
  char * reg;
  switch (lookup.first) {
  case STACK:
    reg = SP;
    break;
  case FRAME_POINTER:
    reg = FP;
    break;
  case CLASS_FIELD:
    reg = SELF;
    break;
  default:
    throw;
  }

  // Load the requested name into the accumulator
  emit_load(ACC, lookup.second, reg, s);
}

void NameResolver::emit_store_name(Symbol name, ostream& s)
{
  if (cgen_debug)
    std::cerr << __func__  << std::endl;

 std::pair<LocationType, int> lookup = this->name_to_address(name);
  char * reg;
  switch (lookup.first) {
  case STACK:
    reg = SP;
    break;
  case FRAME_POINTER:
    reg = FP;
    break;
  case CLASS_FIELD:
    reg = SELF;
    break;
  default:
    throw;
  }
  // Store the accumulator value into the requested name
  emit_store(ACC, lookup.second, reg, s);
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" 
	      << __func__ << " " 
	      << this->name << std::endl;

  // a0 always hold the result of an evaluation
  this->expr->code(s);
  g_name_resolver->emit_store_name(this->name, s);
}

// Common code for dispatch
void static dispatch(bool static_dispatch,
		     Expression expr,
		     Symbol type_name,
		     Symbol name,
		     Expressions actual,
		     int line_number,
		     ostream& s) {

  int method_index, params, abort_branch, start_branch;
  CgenNode * class_;
  Formals method_formals;
  

  // Build the Abort case at the beginning of the generated code
  // filename is always string 0.
  abort_branch = label_counter++;
  start_branch = label_counter++;
  
  // We jump to the start of the method
  s << JAL; emit_label_ref(start_branch, s); s << endl;

  // Abort branch beginning
  emit_label_def(abort_branch, s);
  emit_load_imm(T1, line_number, s);
  s << LA << ACC << " " << STRCONST_PREFIX << 0 << endl;
  s << JAL << DISPATCH_ABORT << endl;


  // Start branch start
  emit_label_def(start_branch, s);
  params = 0;

  // Compute all parameters' expressions and save them to the stack
  for (int i = actual->first();
       actual->more(i);
       i = actual->next(i))
    { 
      params++;
      actual->nth(i)->code(s);
      g_name_resolver->add(parameter, STACK);
      emit_push(ACC, s);
    }
  expr->code(s);
  
  // Attempt to dispatch on a void object, jump to the abort branch
  emit_beqz(ACC, abort_branch, s);
  
  // Get the expression's type class
  if (static_dispatch) {
    class_ = g_codegen_classtable->probe(type_name);
  } else {
    if (expr->get_type() == SELF_TYPE)
      class_ =  (CgenNode *) &g_name_resolver->get_class();
    else
      class_ = g_codegen_classtable->probe(expr->get_type());
  }

  // The AST should be correct and this should never happen
  if (!class_) {
    if (cgen_debug)
      std::cerr << ":" << __func__  << " there is no type " 
		<< expr->get_type()
		<< std::endl;
    throw;
  }
  method_index = class_->method_index(name);
  if (method_index == -1) {
    if (cgen_debug)
      std::cerr << ":" << __func__ << " there is no method " 
		<< name
		<< " for class " 
		<< class_->name << std::endl;
    throw;
  }
  if (cgen_debug)
    std::cerr << ":" << __func__  << " method " 
	      << name
	      << " index " 
	      << method_index 
	      << std::endl;
  
  // Load the dispatch pointer for object where method is called upon
  emit_load(T1, 2, ACC, s);
  
  // Add the method's index, T1 now holds the method's address
  emit_load(T1, method_index, T1, s);
  emit_jalr(T1, s);
  g_name_resolver->pop_sp(params);
  return;
}
		     

void static_dispatch_class::code(ostream &s) {
    if (cgen_debug)
      std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
   dispatch(true,
	   this->expr,
	   this->type_name,
	   this->name,
	   this->actual,
	   this->line_number,
	   s);
}

//I'd love to simplify this
void dispatch_class::code(ostream &s) {
  int method_index, params, abort_branch, start_branch;
  CgenNode * class_;
  Formals method_formals;
  
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ 
	      << " generating dispatch to " << this->name
	      << std::endl;;
  dispatch(false,
	   this->expr,
	   NULL,
	   this->name,
	   this->actual,
	   this->line_number,
	   s);
}

void cond_class::code(ostream &s) {
  int else_branch;
  int end_branch;
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  else_branch = label_counter++;
  end_branch = label_counter++;
  this->pred->code(s);

  // We need to get to the third word where the actual boolean is stored
  emit_load(ACC, 3, ACC, s);
  emit_beqz(ACC, else_branch, s);
  this->then_exp->code(s);
  s << JAL; emit_label_ref(end_branch, s); s << std::endl;
  emit_label_def(else_branch, s);
  this->else_exp->code(s);
  emit_label_def(end_branch, s);
}

void loop_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  int guard = label_counter++;
  int end = label_counter++;

  emit_label_def(guard, s);
  this->pred->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_beqz(ACC, end, s);
  this->body->code(s);
  s << JAL; emit_label_ref(guard, s); s << std::endl;
  emit_label_def(end, s);
}

void typcase_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;

}

void block_class::code(ostream &s) {
  for (int i = body->first(); body->more(i); i = body->next(i)) {
    body->nth(i)->code(s);
  }
}

void let_class::code(ostream &s) {
   if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;

   if (cgen_debug)
     std::cerr << typeid(*this).name() << ":adding " << *this->identifier << std::endl;

   // The let statement comes with an expression only, evaluate it and 
   // add it to the name resolver after its evaluation.
   // Proceed with default initiliazition if none is provided for a basic type
   if (is_base_type(this->type_decl) && init->is_null_expr())
     initialize_default_value(this->type_decl, s);
   else
     init->code(s);
   g_name_resolver->add(this->identifier, STACK);
   emit_push(ACC, s);
   body->code(s);
  
  // Free SP
  g_name_resolver->pop_sp(1);
  emit_addiu(SP, SP, 4, s);
}


void plus_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  perform_arithmetic_operation(s, this->e1, this->e2,  PLUS);
  return;
}

void sub_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  perform_arithmetic_operation(s, this->e1, this->e2,  MINUS);
  return;
}

void mul_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  perform_arithmetic_operation(s, this->e1, this->e2,  STAR);
  return;
}

void divide_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  perform_arithmetic_operation(s, this->e1, this->e2,  SLASH);
  return;
}

void neg_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() 
	      << ":" << __func__ << " " << std::endl;

  // Load the actual value negate and store into the stack
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_neg(ACC, ACC, s);
  emit_push(ACC, s);

  // Create an Integer object to be filled with the negated value
  emit_load_address(ACC, INTNAME""PROTOBJ_SUFFIX, s);
  emit_jal(OBJECT_COPY, s);
  emit_jal(INTNAME""CLASSINIT_SUFFIX, s);
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  emit_store(T1, 3, ACC, s);
}

void lt_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() 
	      << ":" << __func__ << " " << std::endl;


 // Store assumption, false
 emit_load_bool(ACC, falsebool, s);
 emit_push(ACC, s);

 // Evaluate expressions
 // Load the actual number into the register
 e1->code(s);
 emit_load(ACC, 3, ACC, s);
 emit_push(ACC, s);
 e2->code(s);
 emit_load(ACC, 3, ACC, s);
 emit_load(T1, 1, SP, s);
 emit_addiu(SP, SP, 4, s);
 
 // STL sets ACC to one if T1 <= ACC
 s << SLT << ACC << " "  
   << T1 " "
   << ACC << std::endl;
 emit_beqz(ACC, label_counter, s);
 
 // True case, discard old value and replace with true
 emit_addiu(SP, SP, 4, s);
 emit_load_bool(ACC, truebool, s);
 emit_push(ACC, s);

 // Label
 emit_label_def(label_counter, s); 
 label_counter += 1;
 
 // Load into ACC whatever is on the top of the stack
 emit_load(ACC, 1, SP, s);
 emit_addiu(SP, SP, 4, s);
}

// One expression is equal to another if the point to the same
// address.If the two objects are of type String, Bool, or Int, their
// respective contents are compared.
void eq_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() 
	      << ":" << __func__ << " " << std::endl;
 int false_branch, end_branch, shift;

 false_branch = label_counter++;
 end_branch   = label_counter++;

 // Use the provided equality_test assembly routine
 e1->code(s);
 emit_move(T1, ACC, s);
 e2->code(s);
 emit_move(T2, ACC, s);
 emit_push(ACC, s);
 emit_jal(EQUALITY_TEST, s);  
 emit_load(T1, 1, SP, s);
 emit_addiu(SP, SP, 4, s);
 emit_bne(T1, ACC, false_branch, s);
 emit_load_bool(ACC, truebool, s);
 s << JAL; emit_label_ref(end_branch, s); s << std::endl;
 emit_label_def(false_branch, s);
 emit_load_bool(ACC, falsebool, s);
 emit_label_def(end_branch, s);
}

void leq_class::code(ostream &s) {
 if (cgen_debug)
    std::cerr << typeid(*this).name() 
	      << ":" << __func__ << " " << std::endl;
 int false_branch, end_branch, shift;

 false_branch = label_counter++;
 end_branch   = label_counter++;
 e1->code(s);
 emit_load(ACC, 3, ACC, s);
 emit_push(ACC, s);
 e2->code(s);
 emit_load(ACC, 3, ACC, s);
 emit_load(T1, 1, SP, s);
 emit_addiu(SP, SP, 4, s);
 emit_bgt(T1, ACC, false_branch, s);
 emit_load_bool(ACC, truebool, s);
 s << JAL; emit_label_ref(end_branch, s); s << std::endl;
 emit_label_def(false_branch, s);
 emit_load_bool(ACC, falsebool, s);
 emit_label_def(end_branch, s);
}

void comp_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() 
	      << ":" << __func__ << " " << std::endl;

  int true_branch = label_counter++;
  int end_branch = label_counter++;

  e1->code(s);
  emit_load_bool(T1, falsebool, s);
  emit_beq(ACC, T1, true_branch, s);

  // Not branched : the value it's true and false must be returned.
  emit_load_bool(ACC, falsebool, s);
  s << JAL; emit_label_ref(end_branch, s); s << std::endl;
  
  // True branch
  emit_label_def(true_branch, s);
  emit_load_bool(ACC, truebool, s);

  // End branch
  emit_label_def(end_branch, s);
}

void int_const_class::code(ostream& s)  
{
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;

  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {
  int proto_obj_index = 0;

  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  
  // Load the right protoObj in a register for the Object.copy function
  proto_obj_index = g_codegen_classtable->probe(this->type_name)->index;
  
  // Load the CLASSOBJTAB  into a register
  emit_load_address(T1, CLASSOBJTAB, s);

  // Load into the ACC the proto_obj address
  // and call the Object.copy functions
  emit_load(ACC,  (proto_obj_index * 2), T1, s);
  emit_jal(OBJECT_COPY, s);
  
  // Load the CLASSOBJTAB  into a register
  emit_load_address(T1, CLASSOBJTAB, s);

  // Load the init object function address into T1
  emit_load(T1,  (proto_obj_index * 2) + 1, T1, s);
     
  // Call the init upon the freshly allocated object
  emit_jalr(T1, s);
  
}

void isvoid_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;

  int end_branch = label_counter++;
  int true_branch = label_counter++;

  e1->code(s);
  emit_beqz(ACC, true_branch, s);
  emit_load_bool(ACC, falsebool, s);
  s << JAL; emit_label_ref(true_branch, s); s << std::endl;
  emit_label_def(true_branch, s);
  emit_load_bool(ACC, truebool, s);
  emit_label_def(end_branch, s);
}

void no_expr_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ << std::endl;
  
  // No expression : void
  emit_load_imm(ACC, 0, s);
  return;
}

void object_class::code(ostream &s) {
  if (cgen_debug)
    std::cerr << typeid(*this).name() << ":" << __func__ 
	      << " "  << this->name << std::endl;
  if (name != self)
    g_name_resolver->emit_load_name(name, s);
  else
   emit_move(ACC, SELF, s);
  
}


