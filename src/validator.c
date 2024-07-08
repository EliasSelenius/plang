
static void validate_node(Parser* parser, NodeRef p);
static Datatype _validate_expr_base(Parser* parser, NodeRef p, Datatype expected_type);
static Datatype validate_expr_expect_type(Parser* parser, NodeRef p, Datatype expected_type) { return p.expr->datatype = _validate_expr_base(parser, p, expected_type); }
static Datatype validate_expr(Parser* parser, NodeRef p) { return validate_expr_expect_type(parser, p, type_invalid); }



static bool typeAssignable(Datatype toType, Datatype fromType) {
    toType = dealiasType(toType);
    fromType = dealiasType(fromType);

    if (fromType.kind == Typekind_Invalid) return true;
    if (toType.kind == Typekind_Invalid) return true;

    if (typeEquals(toType, fromType)) return true;

    if (toType.numPointers != fromType.numPointers) return false;
    u32 numPointers = toType.numPointers;

    if (numPointers) { // if we are a pointer of any degree

        if (toType.kind == Typekind_void || fromType.kind == Typekind_void) return true;

        if (toType.kind == Typekind_Procedure && fromType.kind == Typekind_Procedure) {
            // if (procSignatureAssignable(toType.procedure, fromType.procedure)) return true;
            return true; // TODO: ...
        }

        return false;
    } // if we are a value-type:


    // Ambiguous cases
    if (fromType.kind == Typekind_AmbiguousInteger) switch (toType.kind) {
        case Typekind_uint8:
        case Typekind_uint16:
        case Typekind_uint32:
        case Typekind_uint64:
        case Typekind_int8:
        case Typekind_int16:
        case Typekind_int32:
        case Typekind_int64:
        case Typekind_AmbiguousDecimal:
        case Typekind_float32:
        case Typekind_float64: return true;
        default: return false;
    } else if (fromType.kind == Typekind_AmbiguousDecimal) switch (toType.kind) {
        case Typekind_float32:
        case Typekind_float64: return true;
        default: return false;
    }

    if (isIntegralType(fromType)) {

        if (toType.kind == Typekind_float32 ||
            toType.kind == Typekind_float64 ||
            toType.kind == Typekind_AmbiguousDecimal) return true;

        if (isIntegralType(toType))
            if (rangein(getNumericDomain(fromType), getNumericDomain(toType)))
                return true;

        return false;
    }

    if (fromType.kind == Typekind_float32 && toType.kind == Typekind_float64) return true;


    if (toType.kind == Typekind_Array) {
        if (fromType.kind == Typekind_Dynamic_Array
         || fromType.kind == Typekind_Fixed_Array
         || fromType.kind == Typekind_Array) {
            return typeAssignable(toType.array_typenode->array.element_type->solvedstate, fromType.array_typenode->array.element_type->solvedstate);
        }
    }

    return false;
}


static char* get_basic_type_string(Datatype type) {
    switch (type.kind) {
        case Typekind_Invalid:          return "Invalid-Type";
        case Typekind_AmbiguousInteger: return "Ambiguous-Integer";
        case Typekind_AmbiguousDecimal: return "Ambiguous-Decimal";

        case Typekind_uint8:            return "uint8";
        case Typekind_uint16:           return "uint16";
        case Typekind_uint32:           return "uint32";
        case Typekind_uint64:           return "uint64";
        case Typekind_int8:             return "int8";
        case Typekind_int16:            return "int16";
        case Typekind_int32:            return "int32";
        case Typekind_int64:            return "int64";
        case Typekind_float32:          return "float32";
        case Typekind_float64:          return "float64";
        case Typekind_void:             return "void";
        case Typekind_char:             return "char";
        case Typekind_string:           return "string";

        case Typekind_Struct:           return get_string(type.stru->name);
        case Typekind_Enum:             return get_string(type._enum->name);
        case Typekind_Typedef:          return get_string(type.type_def->name);

        case Typekind_Procedure:        return null;
        case Typekind_Array:            return null;
        case Typekind_Fixed_Array:      return null;
        case Typekind_Dynamic_Array:    return null;
    }
}

static char* construct_type_string(Datatype type, StringBuilder* sb) {

    Type* typenode = type.array_typenode; // same as proc_ptr_typenode

    switch (type.kind) {
        case Typekind_Procedure: {
            construct_type_string(typenode->procedure.return_type->solvedstate, sb);
            sbAppend(sb, "(");
            Type* arg = typenode->procedure.first_argument;
            if (arg) {
                do {
                    construct_type_string(arg->solvedstate, sb);
                    sbAppend(sb, ", ");
                    arg = arg->next;
                } while (arg);
                sb->length -= 2; // trim of trailing comma: ", "
            }
            sbAppend(sb, ")");

            type.numPointers--; // proc pointers have an implicit asterix
        } break;


        case Typekind_Array:         construct_type_string(typenode->array.element_type->solvedstate, sb); sbAppend(sb, "[]"); break;
        case Typekind_Fixed_Array:   construct_type_string(typenode->array.element_type->solvedstate, sb);
                                     sbAppend(sb, "[0]");//  sbAppend(sb, numberToString()) // TODO: append array size
                                     break;
        case Typekind_Dynamic_Array: construct_type_string(typenode->array.element_type->solvedstate, sb); sbAppend(sb, "[..]"); break;

        default:
            char* typename = get_basic_type_string(type);
            sbAppend(sb, typename);
            break;
    }

    u32 np = type.numPointers;
    while (np-- != 0) sbAppendChar(sb, '*');

    return sb->content;
}

static char* get_temp_type_name(Datatype type) {
    return construct_type_string(type, temp_builder());
}


static void assertAssignability(Parser* parser, Datatype dst_type, Datatype src_type, NodeRef ref) {
    if (typeAssignable(dst_type, src_type)) return;

    error_node(parser, ref, "Type missmatch. \"%s\" is not assignable to \"%s\".", get_temp_type_name(src_type), get_temp_type_name(dst_type));
}

static Datatype typeofStatement(Statement* sta) {
    if (!sta) return type_invalid;
    NodeRef p = (NodeRef)sta;
    switch (p.node->kind) {
        case Node_Declaration: return p.Declaration->type->solvedstate;
        case Node_Constant:    return p.Constant->expr.expr->datatype;
        case Node_Argument:    return p.Argument->type->solvedstate;
        case Node_Procedure:   return p.Procedure->type_node->solvedstate;
        case Node_EnumEntry:   return (Datatype) { .kind = Typekind_Enum, ._enum = p.EnumEntry->_enum };
        case Node_ForStmt:     return p.ForStmt->index_type ? p.ForStmt->index_type->solvedstate : default_for_loop_numeric_type;
        default: return type_invalid;
    }
}

static Procedure* searchMatchingOverload(Procedure* proc, ProcCallExpression* call) {
    u32 argslen = call->args ? list_length(call->args) : 0;

    Procedure* starting_proc = proc;
    do {
        if (argslen != (proc->arguments ? list_length(proc->arguments) : 0)) goto next;

        for (u32 i = 0; i < argslen; i++)
            if (!typeAssignable(proc->arguments[i].type->solvedstate, call->args[i].expr->datatype)) goto next;

        return proc;
        next:
        proc = proc->next_overload;
    } while (proc && (proc != starting_proc));

    return null;
}


/*

foo()
foo.bar()

void bar(with Foo foo) {

}

Foo foo;
foo.bar();

*/

static Procedure* get_possibly_overloaded_proc(NodeRef p) {
    switch (p.node->kind) {
        case Node_Variable: if (p.Variable->ref.node->kind == Node_Procedure) return p.Variable->ref.Procedure; else return null;

        // TODO: when contextual-inclusion on arguments are implemented, then this expression may return a Procedure that could be overloaded
        // case Node_Deref: {
        //     DerefExpression* deref = (DerefExpression*)expr;
        //     if (deref->expr) 
        //     return null;
        // }

        default: return null;
    }
}

static Datatype validateProcCall(Parser* parser, ProcCallExpression* call) {

    // validate passed arguments
    u32 argslength = call->args ? list_length(call->args) : 0;
    for (u32 i = 0; i < argslength; i++) validate_expr(parser, call->args[i]);

    validate_expr(parser, call->proc_expr);


    // builtin procedures
    if (call->proc_expr.node->kind == Node_Variable) {
        VariableExpression* var = call->proc_expr.Variable;

        if (var->name == builtin_string_print) return type_void;

        if (var->name == builtin_string_add) {
            if (argslength == 2) {
                Datatype first_arg_type = call->args[0].expr->datatype;
                if (first_arg_type.kind == Typekind_Dynamic_Array) {
                    Datatype element_type = first_arg_type.array_typenode->array.element_type->solvedstate;
                    if (typeAssignable(call->args[1].expr->datatype, element_type)) return type_void;
                }
            }
        }
    }


    // Special case for overloaded procedures
    Procedure* proc = get_possibly_overloaded_proc(call->proc_expr);
    if (proc) {
        Procedure* correct_overload = searchMatchingOverload(proc, call);
        if (correct_overload) {
            call->proc = correct_overload;
            return correct_overload->return_type->solvedstate;
        }

        error_node(parser, (NodeRef)call, "No valid overload found for \"%s\"", get_string(proc->name));
        return type_invalid;
    }


    Datatype type = dealiasType(call->proc_expr.expr->datatype); // TODO: what if it is a double pointer? Then this shouldnt work.
    if (type.kind == Typekind_Invalid) return type;

    if (type.kind != Typekind_Procedure) {
        error_node(parser, (NodeRef)call, "Invalid procedurecall. This expression is not a procedure.");
        return type_invalid;
    }

    // TODO: typecheck arguments
    // return type.procedure->return_type;
    return type.proc_ptr_typenode->procedure.return_type->solvedstate;
}

// static void assignment(Datatype toType, Expression* expr) {
// }

static void validate_declaration(Parser* parser, Declaration* decl) {
    if (decl->expr.node) {
        Datatype type = validate_expr_expect_type(parser, decl->expr, decl->type->solvedstate);
        if (type.kind == Typekind_Invalid) return;

        if (decl->type->kind == Node_Type_MustInfer) decl->type->solvedstate = resolveTypeAmbiguity(type);
        else assertAssignability(parser, decl->type->solvedstate, type, decl->expr);
    }
}

static void validateStruct(Parser* parser, Struct* stru) {
    u32 fieldLen = list_length(stru->fields);
    for (u32 i = 0; i < fieldLen; i++) {
        Declaration* field = &stru->fields[i];

        // TODO: field may be an alias
        if (field->type->solvedstate.kind == Typekind_Struct && field->type->solvedstate.numPointers == 0) {
            if (stru == field->type->solvedstate.stru) {
                error_node(parser, (NodeRef)field, "Struct \"%s\" self reference by value.", get_string(stru->name));
            }
        }
    }
}

static void validateEnum(Parser* parser, Enum* en) {
    u64 entry_value = 0;

    foreach (entry, en->entries) {
        if (entry->expr.node) validate_expr(parser, entry->expr); // TODO: evaluate expression value
        entry->value = entry_value++;
    }
}


static void validate_scope(Parser* parser, Scope* scope) {
    foreach (refp, scope->statements) validate_node(parser, *refp);
}

static void validate_procedure(Parser* parser, Procedure* proc) {
    if (!node_is_null(proc->sub_node)) {
        Procedure* otherproc = parser->procedure; // in case this is a local procedure
        parser->procedure = proc;
        validate_node(parser, proc->sub_node);
        parser->procedure = otherproc;
    }

    if (proc->return_type->kind == Node_Type_MustInfer) proc->return_type->solvedstate = type_void;
}

static Datatype validate_deref_expr(Parser* parser, DerefExpression* deref, Datatype expected_type) {
    NodeRef inner_expr = deref->expr;
    Identifier member_name = deref->name;

    // implicit derefing:
    if (node_is_null(inner_expr)) {
        if (expected_type.kind == Typekind_Enum && expected_type.numPointers == 0) {
            Enum* en = expected_type._enum;
            if (getEnumEntry(en, member_name)) return expected_type;
            error_node(parser, (NodeRef)deref, "Invalid enum member in implicit dereferencing \".%s\".", get_string(member_name));
            return type_invalid;
        }

        error_node(parser, (NodeRef)deref, "Cannot infer implicit dereferencing \".%s\".", get_string(member_name));
        return type_invalid;
    }

    Datatype inner_expr_type = validate_expr(parser, inner_expr);

    // TODO: we could remove this if we consolidated get_datatype_from_statement() with typeofStatement()
    // then validate_expr() would handle this case for us,
    // and out switch statement down there could do the checks to see if member_name
    // actually is a member of the enum type.
    if (inner_expr.node->kind == Node_Variable) {
        if (inner_expr.Variable->ref.node->kind == Node_Enum) return (Datatype) { Typekind_Enum, ._enum = inner_expr.Variable->ref.Enum };
    }

    if (inner_expr_type.numPointers > 1) {
        error_node(parser, (NodeRef)deref, "Attempted to dereference a %dth-degree pointer.", inner_expr_type.numPointers);
        return type_invalid;
    }

    switch (inner_expr_type.kind) {
        case Typekind_Struct: {
            Statement* member = getMember(inner_expr_type.stru, member_name);
            if (member) return typeofStatement(member);

            error_node(parser, (NodeRef)deref, "Member \"%s\" does not exist in struct \"%s\".", get_string(member_name), get_string(inner_expr_type.stru->name));
            return type_invalid;
        }

        case Typekind_string: {
            if (member_name == builtin_string_length) return type_uint32;
            if (member_name == builtin_string_chars) return type_charPointer;
        } break;

        case Typekind_Dynamic_Array:
            if (member_name == builtin_string_capacity) return type_uint32; // TODO: hardcoded type of capacity field
        case Typekind_Array:
        case Typekind_Fixed_Array: {
            if (member_name == builtin_string_length) return type_uint32; // TODO: hardcoded type of length field
        } break;
        default: break;
    }

    error_node(parser, (NodeRef)deref, "Invalid dereferencing. \"%s\" is not a member of %s", get_string(member_name), get_temp_type_name(inner_expr_type));
    return type_invalid;
}

/* TODOs on validation checks and error messages:
    - valid expression type in if/while/switch/ternary_expr
    - is compile-time value in case labels and possibly const declarations
    - is on correct context (like continue/break/default must be inside a loop/switch)
    - ambigious dereference caused by contextual inclusion
    - error on use of uninitialized variable (very important)
    - propegate expected type in compound literals (for arrays and structs)
*/

static Datatype type_ptr(Datatype type, i32 nump) { type.numPointers += nump; return type; }


static Datatype _validate_expr_base(Parser* parser, NodeRef p, Datatype expected_type) {
switch (p.node->kind) {

    case Node_Less:
    case Node_Greater:
    case Node_LessEquals:
    case Node_GreaterEquals:
    case Node_Equals:
    case Node_NotEquals:
    case Node_BooleanAnd:
    case Node_BooleanOr:
        validate_expr_expect_type(parser, p.Binary->left,  expected_type);
        validate_expr_expect_type(parser, p.Binary->right, expected_type);
        return type_bool;

    case Node_Plus:
    case Node_Minus:
    case Node_Mul:
    case Node_Div:
    case Node_Mod:
    case Node_Bitwise_And:
    case Node_Bitwise_Or:
    case Node_Bitwise_Xor:
    case Node_Bitwise_Lshift:
    case Node_Bitwise_Rshift:
    {
        Datatype type_l = validate_expr_expect_type(parser, p.Binary->left,  expected_type);
        Datatype type_r = validate_expr_expect_type(parser, p.Binary->right, expected_type);

        if (type_l.kind == Typekind_Invalid || type_r.kind == Typekind_Invalid) return type_invalid;

        foreach (item, parser->codebase->operators) {
            Procedure* op = *item;
            if (p.node->kind == op->operator)
            if (typeAssignable(op->arguments[0].type->solvedstate, type_l)
             && typeAssignable(op->arguments[1].type->solvedstate, type_r)) {
                p.Binary->operator_overload = op;
                return op->return_type->solvedstate;
            }
        }

        if (type_l.numPointers) return type_l; // to enable pointer arithmetic

        if (typeAssignable(type_l, type_r)) return type_l;
        if (typeAssignable(type_r, type_l)) return type_r;

        error_node(parser, p, "Invalid binary expression. operator %s (%s, %s)", get_node_symbol(p.node->kind).symbol, get_temp_type_name(type_l), get_temp_type_name(type_r));
        return type_invalid;
    }

    // TODO: make a list of operators, and search for the existence of an operator given the left and rigth type, this will also facilitate operator overloads

    // TODO: Node_Unary_Negate: can type be negated?
    // TODO: Node_Unary_BitwiseNot: can everything be bit flipped like this?
    // TODO: can type be incremented/decremented?
    // TODO: Node_Unary_Not: is type a boolean type?
    // TODO: Node_Unary_Not: return boolean
    case Node_Unary_PreIncrement:
    case Node_Unary_PostIncrement:
    case Node_Unary_PreDecrement:
    case Node_Unary_PostDecrement:
    case Node_Unary_Not:
    case Node_Unary_BitwiseNot:
    case Node_Unary_Negate:
        return validate_expr_expect_type(parser, p.Unary->inner_expr, expected_type);

    // TODO: is unary->expr an expression we can take the address of?
    // TODO: is unary->expr an expression we can take the value of?
    case Node_Unary_AddressOf: return type_ptr(validate_expr(parser, p.Unary->inner_expr), 1); // TODO: we could have an expected type here..
    case Node_Unary_ValueOf:   return type_ptr(validate_expr(parser, p.Unary->inner_expr), -1);

    case Node_Literal_Integer: return type_ambiguousInteger;
    case Node_Literal_Decimal: return type_ambiguousDecimal;
    case Node_Literal_Char:    return type_char;
    case Node_Literal_String:  return type_charPointer; // TODO: string literals should be Typekind_string
    case Node_Literal_True:    return type_bool;
    case Node_Literal_False:   return type_bool;
    case Node_Literal_Null:    return type_voidPointer;

    case Node_Variable: return typeofStatement(p.Variable->ref.stmt);
    case Node_Alloc: {
        if (p.Alloc->size_expr.node) validate_expr(parser, p.Alloc->size_expr); // TODO: is this a valid integer expression?
        return type_ptr(p.Alloc->type->solvedstate, 1);
    }

    case Node_Ternary:
        validate_expr(parser, p.Ternary->condition);
        validate_expr_expect_type(parser, p.Ternary->then_expr, expected_type);
        validate_expr_expect_type(parser, p.Ternary->else_expr, expected_type);
        // TODO: are the then and else types compatible with expected_type? if expexted_type is not applicable: are they compatible with eachother?
        return p.Ternary->then_expr.expr->datatype; // TODO: determine what type actually needs to be returned here

    case Node_ProcCall: return validateProcCall(parser, p.ProcCall); // TODO: use expected type here to resolve overload with return type
    case Node_Deref: return validate_deref_expr(parser, p.Deref, expected_type);

    case Node_Indexing: {
        Datatype type = validate_expr(parser, p.Indexing->indexed);
        validate_expr(parser, p.Indexing->index); // TODO: is index a valid integer expression?
        if (type.numPointers) return type_ptr(type, -1);
        if ((type.kind == Typekind_Array) ||
            (type.kind == Typekind_Fixed_Array) ||
            (type.kind == Typekind_Dynamic_Array)) return type.array_typenode->array.element_type->solvedstate;
        error_node(parser, p, "Attempted to index an expression that is not indexable.");
        return type_invalid;
    }

    case Node_Cast: validate_expr_expect_type(parser, p.Cast->expr, p.Cast->new_type->solvedstate); return p.Cast->new_type->solvedstate; // TODO: is this a valid casting operation
    case Node_Sizeof: return type_ambiguousInteger;
    case Node_Parenthesized: return validate_expr_expect_type(parser, p.Parenthesized->inner_expr, expected_type);
    case Node_Compound: {
        Datatype inner_expected_type = type_invalid; // TODO: propegate expected type for structs aswell
        if (expected_type.kind == Typekind_Array) inner_expected_type = expected_type.array_typenode->array.element_type->solvedstate;
        if (p.Compound->elements) { foreach (el, p.Compound->elements) validate_expr_expect_type(parser, el->expr, inner_expected_type); }
        return expected_type;
    }

    default:
        // this should never happen
        error_node(parser, p, "Invalid node type. This node is not an expression.");
        return type_invalid;
}}

static void validate_node(Parser* parser, NodeRef p) {
switch (p.node->kind) {

    case Node_Declaration: validate_declaration(parser, p.Declaration); return;
    case Node_Constant: validate_expr(parser, p.Constant->expr); return;
    case Node_Typedef: return;
                        // TODO: init_typenode_for_proc should not be here
    case Node_Procedure: init_typenode_for_proc(p.Procedure); validate_procedure(parser, p.Procedure); return;
    case Node_Argument: return;
    case Node_Struct: validateStruct(parser, p.Struct); return;
    case Node_Enum: validateEnum(parser, p.Enum); return;
    case Node_EnumEntry: return;
    case Node_Assignment: {
        Datatype dst = validate_expr(parser, p.Assignment->dst_expr);
        Datatype src = validate_expr_expect_type(parser, p.Assignment->src_expr, dst);
        // TODO: this only works for normal assignment, but not for += -= etc
        //       make it so this uses operator list
        assertAssignability(parser, dst, src, p.Assignment->src_expr);
    } return;


    case Node_Scope: validate_scope(parser, p.Scope); return;
    case Node_IfStmt:
        validate_expr(parser, p.IfStmt->condition);
        if (p.IfStmt->then_statement.node) validate_node(parser, p.IfStmt->then_statement);
        if (p.IfStmt->else_statement.node) validate_node(parser, p.IfStmt->else_statement);
        return;
    case Node_WhileStmt: validate_expr(parser, p.WhileStmt->condition); if (p.WhileStmt->statement.node) validate_node(parser, p.WhileStmt->statement); return;

    case Node_ForStmt: {
        if (p.ForStmt->iterator_assignment.node) { // TODO: this is doing the job of an assignment, abstract away...
            Datatype it_type = validate_expr_expect_type(parser, p.ForStmt->iterator_assignment, p.ForStmt->index_type->solvedstate);
            if (p.ForStmt->index_type->kind == Node_Type_MustInfer) p.ForStmt->index_type->solvedstate = it_type;
            else assertAssignability(parser, p.ForStmt->index_type->solvedstate, it_type, p.ForStmt->iterator_assignment);
        }
        validate_expr(parser, p.ForStmt->min_expr);
        if (p.ForStmt->max_expr.node) validate_expr(parser, p.ForStmt->max_expr);
        if (p.ForStmt->statement.node) validate_node(parser, p.ForStmt->statement);
    } return;

    case Node_SwitchStmt: validate_expr(parser, p.SwitchStmt->expr); validate_scope(parser, p.SwitchStmt->scope); return;
    case Node_ContinueStmt: return;
    case Node_BreakStmt: return;
    case Node_ReturnStmt: { // TODO: this is kinda similar to an 'assignment' (see TODO in case Node_ForStmt) consider abstracting away
        Datatype type = type_void;
        if (p.ReturnStmt->expr.node) type = validate_expr_expect_type(parser, p.ReturnStmt->expr, parser->procedure->return_type->solvedstate);
        if (type.kind == Typekind_Invalid) return;
        if (parser->procedure->return_type->kind == Node_Type_MustInfer) parser->procedure->return_type->solvedstate = resolveTypeAmbiguity(type);
        else if (!typeAssignable(parser->procedure->return_type->solvedstate, type)) error_node(parser, p, "Return type missmatch in function \"%s\".", get_string(parser->procedure->name));
    } return;

    case Node_GotoStmt: return;
    case Node_LabelStmt: return;

    case Node_CaseLabelStmt:
        if (p.CaseLabelStmt->switch_statement) validate_expr_expect_type(parser, p.CaseLabelStmt->expr, p.CaseLabelStmt->switch_statement->expr.expr->datatype);
        else error_node(parser, p, "Case label not declared inside switch."); return;

    case Node_DefaultLabelStmt: return;

    case Node_Type_MustInfer: return;
    case Node_Type_Basic: return;
    case Node_Type_Procedure: return;
    case Node_Type_Array: return;
    case Node_Type_Fixed_Array: return;
    case Node_Type_Dynamic_Array: return;

    default: validate_expr(parser, p); return;
}}

static void validate(Parser* parser, Codebase* cb) {

    // validate structs
    u32 struLen = list_length(cb->structs);
    for (u32 i = 0; i < struLen; i++) {
        validateStruct(parser, cb->structs[i]);
    }

    foreach (en, cb->enums) {
        validateEnum(parser, *en);
    }

    foreach (decl, cb->global_vars) {
        validate_declaration(parser, *decl);
    }

    foreach (con, cb->global_consts) {
        validate_expr(parser, (*con)->expr);
    }

    parser->procedure = null;
    foreach (proc, cb->procedures) {
        validate_procedure(parser, *proc);
    }
}