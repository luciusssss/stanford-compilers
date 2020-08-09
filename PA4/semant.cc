

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

static ClassTable *classtable;
/* Type environments: O, M, C */
static SymbolTable<Symbol, Symbol> *obj_env;
static std::map<Symbol, std::map<Symbol, Feature> > mthd_env;
static Class_ current_class;



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
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    /* Fill this in */
    install_basic_classes();
    /* Traverse all the classes */
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ cur_class = classes->nth(i);
        Symbol cur_class_parent = cur_class->get_parent();
        //  Redefinition of basic class SELF_TYPE
        if (cur_class->get_name() == SELF_TYPE) {
            semant_error(cur_class) << "Redefinition of basic class SELF_TYPE." << endl;
            return;
        }
        else if (is_in_table(cur_class->get_name())) {
            semant_error(cur_class) << "Duplicated class name:" << cur_class->get_name() << endl;
            return;
        }
        else if (cur_class_parent == Int || cur_class_parent == Str || cur_class_parent == Bool) {
            semant_error(cur_class) << "Inherited from Int, Str or Bool" << endl;
            return;
        }
        else {
            add_class(cur_class->get_name(), cur_class);
        }
    }

    /* Check 'Main' class */
    if (!is_in_table(Main)) {
        semant_error() << "Class Main is not defined." << endl;
        return;
    }

    /* Check inheritance */
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ cur_class = classes->nth(i);
        Symbol class_name = cur_class->get_name();
        while (cur_class->get_name() != Object) {
            Symbol parent_name = cur_class->get_parent();
            
            // Parent name not in table
            if (!is_in_table(parent_name)) {
                semant_error(cur_class) << "Invalid parent " << parent_name << "of Class " << cur_class->get_name() << endl;
                return;
            }

            // Cycle in graph
            if (parent_name == class_name) {
                semant_error(current_class) << "Cycle in inheritance graph of class " << class_name << endl;
                return;
            }
            
            cur_class = get_class_by_symbol(parent_name);
        }
    }

}

void ClassTable::install_basic_classes() {

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
    
    add_class(Object, Object_class);
    add_class(IO, IO_class);
    add_class(Int, Int_class);
    add_class(Bool, Bool_class);
    add_class(Str, Str_class);
}

/*
    add_class, get_class_by_symbol, is_in_table, get_class_map
    are methods related to the data structure class_map
*/
void ClassTable::add_class(Symbol s, Class_ c) {
    class_map[s] = c;
}

Class_ ClassTable::get_class_by_symbol(Symbol s) {
    return class_map[s];
}

bool ClassTable::is_in_table(Symbol s) {
    return class_map.find(s) != class_map.end();
}

std::map<Symbol, Class_>* ClassTable::get_class_map() {
    return &class_map;
}


/* get_inheritance_path(Symbol s),
*  check_inheritance(),
*  find_least_common_ancestor(Symbol s1, Symbol s2)
*  are methods related to inheritance graph 
*/ 
std::vector<Class_> ClassTable::get_inheritance_path(Symbol s) {
    if (s == SELF_TYPE)
        s = current_class->get_name();
    Class_ cur_class = get_class_by_symbol(s);
    std::vector<Class_> path;
    while (cur_class->get_name() != Object) {
        path.push_back(cur_class);
        cur_class = get_class_by_symbol(cur_class->get_parent());
    }
    path.push_back(cur_class);
    return path;
}

bool ClassTable::check_inheritance(Symbol ancestor, Symbol descendant) {
    if (ancestor == SELF_TYPE) 
        ancestor = current_class->get_name();
    if (descendant == SELF_TYPE)
        descendant = current_class->get_name();

    if (ancestor == Object)
        return true;
    Class_ cur_class = get_class_by_symbol(descendant);

    while (cur_class->get_name() != Object) {
        if (cur_class->get_name() == ancestor)
            return true;
        cur_class = get_class_by_symbol(cur_class->get_parent());
    }
    return false;
}

Class_ ClassTable::find_least_common_ancestor(Symbol s1, Symbol s2) {
    std::vector<Class_> path1 = get_inheritance_path(s1);
    std::vector<Class_> path2 = get_inheritance_path(s2);
    int ind1 = path1.size() - 1, ind2 = path2.size() - 1;
    while(ind1 >= 0 && ind2 >= 0) {
        if (path1[ind1]->get_name() == path2[ind2]->get_name()) {
            ind1--;
            ind2--;
        }
        else {
            return path1[ind1+1];
        }
    }
    return path1[ind1+1];
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
//    ostream& ClassTable::semant_error(CLass_ c, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    error_stream << c->get_filename() << ":" << c->get_line_number() << ": ";
    semant_errors++; 
    return error_stream;
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    semant_errors++; 
    return error_stream;
}

ostream& ClassTable::semant_error(Class_ c, tree_node *t)
{
    error_stream << c->get_filename() << ":" << t->get_line_number() << ": ";
    semant_errors++; 
    return error_stream;
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                           
    return error_stream;
} 



//////////////////////////////////////////////////////////////////////
//
// Construct the method environment
//
//////////////////////////////////////////////////////////////////////

static void construct_method_environment() {
    std::map<Symbol, Class_>::iterator it;
    for (it = classtable->get_class_map()->begin(); it != classtable->get_class_map()->end(); it++) {
        Symbol class_name = it->first;
        mthd_env[class_name] = std::map<Symbol, Feature>();
        Features features = it->second->get_features();

        for (int i = features->first(); features->more(i); i = features->next(i)) {
            Feature cur_feature = features->nth(i);

            // Skip attribute
            if (!cur_feature->is_method()) continue;

            Symbol mthd_name = cur_feature->get_name();
            // Check if there are duplicate method
            if (mthd_env[class_name].find(mthd_name) != mthd_env[class_name].end()) {
                classtable->semant_error(it->second) << "Duplicated method " << mthd_name << " in class " << class_name << endl;
            }

            // Add method into mthd_env
            mthd_env[class_name][mthd_name] = cur_feature;
        }
    }

}

static void check_type() {
    obj_env = new SymbolTable<Symbol, Symbol>();

    std::map<Symbol, Class_>::iterator it;
    // check each class
    for (it = classtable->get_class_map()->begin(); it != classtable->get_class_map()->end(); it++) {
        Symbol class_name = it->first;

        // Skip basic classes
        if (class_name == Object || class_name == Int || class_name == Str || class_name == Bool || class_name == IO)
            continue;

        std::vector<Class_> path = classtable->get_inheritance_path(class_name);

        // insert attr of ancestor classes into obj_env
        for (int k = path.size() - 1; k >= 0; k--) {
            Class_ c = path[k];
            Features features = c->get_features();

            obj_env->enterscope();  
            
            for (int i = features->first(); features->more(i); i = features->next(i)) {
                Feature feature = features->nth(i);
                if (feature->is_method()) continue; // skip method
                
                // cannot override parent's attribute
                if (obj_env->lookup(feature->get_name()) != NULL) {
                    classtable->semant_error(current_class) << "Attribute " << feature->get_name() << " is an attribute of an inherited class." << endl;
                }
                obj_env->addid(feature->get_name(), new Symbol(feature->get_type_decl()));
            }
        }

        current_class = it->second;
        Features features = current_class->get_features();
        // check the feature of current class
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            Feature feature = features->nth(i);

            if (feature->is_method()) { // check method
                obj_env->enterscope();

                // check override
                for (int k = path.size() - 1; k >= 1; k--) {
                    Class_ c = path[k];
                    if (mthd_env[c->get_name()].find(feature->get_name()) == mthd_env[c->get_name()].end()) 
                        continue;
                    if (feature->get_return_type() != SELF_TYPE && mthd_env[c->get_name()][feature->get_name()]->get_return_type() == SELF_TYPE) {
                        classtable->semant_error(current_class) << "Failed overriding" << endl;
                        break;
                    }
                    int ind1, ind2;
                    Formals formals1 = feature->get_formals();
                    Formals formals2 = mthd_env[c->get_name()][feature->get_name()]->get_formals();
                    for (ind1 = formals1->first(), ind2 = formals2->first(); formals1->more(ind1) && formals2->more(ind2); ind1 = formals1->next(ind1), ind2 = formals2->next(ind2)) {
                        if (formals1->nth(ind1)->get_type_decl() == formals2->nth(ind2)->get_type_decl())
                            continue;
                        classtable->semant_error(current_class) << "Failed overriding" << endl;
                        break;
                    }
                    if (formals1->more(ind1) || formals2->more(ind2)) {
                        classtable->semant_error(current_class) << "Failed overriding" << endl;
                    }
                }

                // check formals
                Formals formals = feature->get_formals();
                for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
                    Formal formal = formals->nth(j);
                    Symbol formal_name = formal->get_name();
                    Symbol formal_type_decl = formal->get_type_decl();

                    // formal_name cannot be self
                    if (formal_name == self) {
                        classtable->semant_error(current_class) << "'self' cannot be the name of a formal parameter." << endl;
                    }
                    // check no duplicated formal name
                    else if (obj_env->probe(formal_name) != NULL) {
                        classtable->semant_error(current_class) << "Duplicated formal " << formal_name << " in method " << feature->get_name() << endl;
                    }
                    // formal type cannot be SELF_TYPE
                    else if (formal_type_decl == SELF_TYPE) {
                        classtable->semant_error(current_class) << "Formal parameter " << formal_name << " cannot have type SELF_TYPE." << endl;
                    }
                    // check the formal type is in classtable
                    else if (!classtable->is_in_table(formal_type_decl)) {
                        classtable->semant_error(current_class) << "Invalid type " << formal_type_decl << " of the formal " << formal_name << " in method " << feature->get_name() << endl;
                    }
                    // insert the formal into obj_env
                    else {
                        obj_env->addid(formal_name, new Symbol(formal_type_decl));
                    }
                }

                Symbol ret_type = feature->get_return_type();

                if (ret_type != SELF_TYPE && !classtable->is_in_table(ret_type)) {
                    classtable->semant_error(current_class) << "Invalid return type " << ret_type << " of method " << feature->get_name() << endl;
                }

                Symbol expr_type = feature->get_expr()->check_type();
                if (expr_type != SELF_TYPE && classtable->is_in_table(expr_type) && !classtable->check_inheritance(ret_type, expr_type)) {
                    classtable->semant_error(current_class) << expr_type << " is not an ancestor of " << ret_type << endl;
                }
                // special case for 'selftypebadreturn.test'
                if (ret_type == SELF_TYPE && expr_type != SELF_TYPE) {
                    classtable->semant_error(current_class) << "Inferred return type " << expr_type << " of method foo does not conform to declared return type SELF_TYPE." << endl;
                }

                obj_env->exitscope();

            }
            else { // check attr
                // attr cannot be self
                Symbol attr_name = feature->get_name();
                if (attr_name == self) {
                    classtable->semant_error(current_class) << "'self' cannot be the name of an attribute." << endl;
                }

                Symbol type_decl = feature->get_type_decl();
                if (type_decl == SELF_TYPE) {
                    type_decl = current_class->get_name();
                }

                if (!classtable->is_in_table(type_decl)) {
                    classtable->semant_error(current_class) << "Invalid type " << type_decl << " of the attribute " << feature->get_name() << " in class " << current_class->get_name() << endl; 
                }

                // if init type is in classtable, it must conform to type_decl
                Symbol init_type = feature->get_init()->check_type();
                if (classtable->is_in_table(init_type) && !classtable->check_inheritance(type_decl, init_type)) {
                    classtable->semant_error(current_class, feature->get_init()) << init_type << " is not an ancestor of " << type_decl << endl;
                }
            }
        }

        for (int k = 0; k < path.size(); k++) {
            obj_env->exitscope();
        }
    }
}


//////////////////////////////////////////////////////////////////////
//
// check_type() method of branch_class
//
//////////////////////////////////////////////////////////////////////

Symbol branch_class::check_type() {
    obj_env->enterscope();
    obj_env->addid(name, new Symbol(type_decl));
    Symbol expr_type = expr->check_type();
    obj_env->exitscope();
    return expr_type;
}


//////////////////////////////////////////////////////////////////////
//
// check_type() methods of subclasses of Expression_class
//
//////////////////////////////////////////////////////////////////////

Symbol assign_class::check_type() {
    Symbol expr_type = expr->check_type();

    if (name == self) {
        classtable->semant_error(current_class) << "Cannot assign to 'self'." <<endl;
        type = Object;
        return type;
    }
    if (obj_env->lookup(name) == NULL) {
        classtable->semant_error(current_class) << "Undeclared identifier " << name << endl;
        type = Object;
        return type;
    }
    
    Symbol id_type = *(obj_env->lookup(name));

    if (!classtable->check_inheritance(id_type, expr_type)) {
        classtable->semant_error(current_class) << id_type << " is not an ancestor of " << expr_type << endl;
        type = Object;
        return type;
    }

    type = expr_type;
    return type;
}

Symbol static_dispatch_class::check_type() {
    bool error = false;

    Symbol expr_type = expr->check_type();
    
    if (!classtable->is_in_table(type_name)) {
        classtable->semant_error(current_class) << "Invalid type " << type_name << endl;
        error = true;
    }

    if (!classtable->check_inheritance(type_name, expr_type)){
        classtable->semant_error(current_class) << type_name << " is not an ancestor of " << expr_type << endl;
        error = true;
    }

    // look for method in inheritance path
    std::vector<Class_> path = classtable->get_inheritance_path(type_name);
    Feature method = NULL;
    for (int i = 0; i < path.size(); i++) {
        Symbol ancestor_name = path[i]->get_name();
        if (mthd_env[ancestor_name].find(name) != mthd_env[ancestor_name].end()) {
            method = mthd_env[ancestor_name][name];
            break;
        }
    }
    if (method == NULL) {
        classtable->semant_error(current_class) << "No method " << name << " in the inheritance path of " << type_name << endl;
        error = true;
    }

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression actual_expr = actual->nth(i);
        Symbol actual_expr_type = actual_expr->check_type();
        if (method != NULL) {
            Symbol formal_type = method->get_formals()->nth(i)->get_type_decl();
            if (!classtable->check_inheritance(formal_type, actual_expr_type)) {
                classtable->semant_error(current_class) << formal_type << " is not an ancestor of " << actual_expr_type << endl;
                error = true;
            }
        }
    }

    if (error) {
        type = Object;
    }
    else {
        type = method->get_return_type();
        if (type == SELF_TYPE) {
            type = type_name;
        }
    }
    return type;
}

Symbol dispatch_class::check_type() {
    bool error = false;

    Symbol expr_type = expr->check_type();

    if (expr_type != SELF_TYPE && !classtable->is_in_table(expr_type)) {
        classtable->semant_error(current_class) << "Invalid type " << expr_type << endl;
        error = true;
    }

    // look for method in inheritance path
    std::vector<Class_> path = classtable->get_inheritance_path(expr_type);
    Feature method = NULL;
    for (int i = 0; i < path.size(); i++) {
        Symbol ancestor_name = path[i]->get_name();

        if (mthd_env[ancestor_name].find(name) != mthd_env[ancestor_name].end()) {
            method = mthd_env[ancestor_name][name];
            break;
        }
    }
    if (method == NULL) {
        classtable->semant_error(current_class) << "No method " << name << " in the inheritance path of " << expr_type << endl;
        error = true;
    }

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Expression actual_expr = actual->nth(i);
        Symbol actual_expr_type = actual_expr->check_type();
        
        if (method != NULL) {
            Symbol formal_type = method->get_formals()->nth(i)->get_type_decl();
            if (!classtable->check_inheritance(formal_type, actual_expr_type)) {
                classtable->semant_error(current_class) << formal_type << " is not an ancestor of " << actual_expr_type << endl;
                error = true;
            }
        }
    }

    if (error) {
        type = Object;
    }
    else {
        type = method->get_return_type();
        if (type == SELF_TYPE) {
            type = expr_type;
        }
    }
    return type;
}

Symbol cond_class::check_type() {
    Symbol pred_type = pred->check_type();
    if (pred_type != Bool) {
        classtable->semant_error(current_class) << "Pred is not bool" << endl;
    }
    Symbol then_exp_type = then_exp->check_type();
    Symbol else_exp_type = else_exp->check_type();
    if (else_exp_type == No_type) {
        type = then_exp_type;
    }
    else {
        type = classtable->find_least_common_ancestor(then_exp_type, else_exp_type)->get_name();
    }
    return type;
}

Symbol loop_class::check_type() {
    Symbol pred_type = pred->check_type();
    if (pred_type != Bool) {
        classtable->semant_error(current_class) << "Pred is not bool" << endl;
    }
    Symbol body_type = body->check_type();
    type = Object;
    return type;
}

Symbol typcase_class::check_type() {
    Symbol expr_type = expr->check_type();

    std::set<Symbol> type_decl_set;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Case case_ = cases->nth(i);

        Symbol type_decl = case_->get_type_decl();
        if (!classtable->is_in_table(type_decl)) {
             classtable->semant_error(current_class) << "Invalid declared type " << type_decl << endl;
        }
        else if (type_decl_set.find(type_decl) != type_decl_set.end()) {
            classtable->semant_error(current_class) << "Duplicated declared type of branches" << endl;
        }
        else {
            type_decl_set.insert(type_decl);
        }

        Symbol branch_type = case_->check_type();
        if (i == cases->first()) {
            type = branch_type;
        }
        else {
            type = classtable->find_least_common_ancestor(type, branch_type)->get_name();
        }
    }

    return type;
}

Symbol block_class::check_type() {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        type = body->nth(i)->check_type();
    }
    return type;
}

Symbol let_class::check_type() {
    obj_env->enterscope();

    if (identifier == self) {
        classtable->semant_error(current_class) << "'self' cannot be bound in a 'let' expression." << endl;
    }
    else if (type_decl != SELF_TYPE && !classtable->is_in_table(type_decl)) {
        classtable->semant_error(current_class) << "Invalid declared type " << type_decl << endl;
    }
    else {
        obj_env->addid(identifier, new Symbol(type_decl));
    }

    Symbol init_type = init->check_type();

    if (init_type != No_type) {
        if (!classtable->check_inheritance(type_decl, init_type)) {
            classtable->semant_error(current_class) << type_decl << " is not an ancestor of " << init_type << endl;
        }
    }

    type = body->check_type();

    obj_env->exitscope();
    return type;
}

Symbol plus_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(current_class) << "Not Int in arith" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol sub_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(current_class) << "Not Int in arith" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol mul_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(current_class) << "Not Int in arith" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol divide_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(current_class) << "Not Int in Arith" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol neg_class::check_type() {
    Symbol e1_type = e1->check_type();
    if (e1_type != Int) {
        classtable->semant_error(current_class) << "Not Int in Neg" << endl;
        type = Object;
    }
    else {
        type = Int;
    }
    return type;
}

Symbol lt_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(current_class) << "Not Int in Comp" << endl;
        type = Object;
    }
    else {
        type = Bool;
    }
    return type;
}

Symbol eq_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != e2_type && (e1_type == Int || e2_type == Int || e1_type == Str || e2_type == Str || e1_type == Bool || e2_type == Bool)) {
        classtable->semant_error(current_class) << "Unmatched or unsuitable types for comparison" << endl;
        type = Object;
    }
    else {
        type = Bool;
    }
    return type;
}

Symbol leq_class::check_type() {
    Symbol e1_type = e1->check_type();
    Symbol e2_type = e2->check_type();
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(current_class) << "Not Int in Comparison" << endl;
        type = Object;
    }
    else {
        type = Bool;
    }
    return type;
}

Symbol comp_class::check_type() {
    Symbol e1_type = e1->check_type();
    if (e1_type != Bool) {
        classtable->semant_error(current_class) << "Not Int in Comp" << endl;
        type = Object;
    }
    else {
        type = Bool;
    }
    return type;
}

Symbol int_const_class::check_type() {
    type = Int;
    return type;
}

Symbol bool_const_class::check_type() {
    type = Bool;
    return type;
}

Symbol string_const_class::check_type() {
    type = Str;
    return type;
}

Symbol new__class::check_type() {
    if (type_name != SELF_TYPE && !classtable->is_in_table(type_name)) {
        classtable->semant_error(current_class) << "Invalid type " << type_name << endl;
        type = Object;
    }
    else {
        type = type_name;
    }
    return type;
}

Symbol isvoid_class::check_type() {
    Symbol e1_type = e1->check_type();
    type = Bool;
    return type;
}

Symbol no_expr_class::check_type() {
    type = No_type;
    return type;
}

Symbol object_class::check_type() {
    if (name == self) {
        type = SELF_TYPE;
    }
    else if (obj_env->lookup(name) == NULL) {
        classtable->semant_error(current_class) << "Undeclared identifier " << name << endl;
        type = Object;
    }
    else {
        type = *obj_env->lookup(name);
    }
    return type;
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

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors() > 0) {
        classtable->semant_error() << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    construct_method_environment();

    check_type();

    if (classtable->errors() > 0) {
        classtable->semant_error() << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

}


