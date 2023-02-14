import * as PromptSync from "prompt-sync";

const prompt: PromptSync.Prompt = PromptSync({sigint:true});

import { is_boolean, set_head, is_pair, list_ref, apply_in_underlying_javascript, pair, stringify, is_null, error, math_abs, math_PI, math_E, display, map, accumulate, length, parse, append, head, list, tail } from 'sicp';

type Environment = List<Frame>;
type Frame = Pair<List<Symbol>, List<Value>>;

type TaggedList = Pair<string, any>;
type TaggedRecord = { tag: string };

type TaggedListComponent = TaggedListStatement | TaggedListExpression; 

type TaggedListStatement = TaggedListBlock | TaggedListSequence | TaggedListDeclaration | TaggedListAssignment | TaggedListReturnStatement;
type TaggedListBlock = ["block", [List<TaggedListStatement>, null]];
type TaggedListSequence = ["sequence", [List<TaggedListStatement>, null]];
type TaggedListDeclaration = TaggedListConstant | TaggedListFunction | TaggedListVariable;
type TaggedListFunction = ["function_declaration", [Name, [List<Name>, [TaggedListComponent, null]]]];
type TaggedListConstant = ["constant_declaration", [Name, [TaggedListExpression, null]]];
type TaggedListVariable= ["variable_declaration", [Name, [TaggedListExpression, null]]];
type TaggedListAssignment = ["assignment", [Name, [TaggedListExpression, null]]];
type TaggedListReturnStatement = ["return_statement", [TaggedListExpression, null]];

type TaggedListExpression = TaggedListConditional | TaggedListLambda | TaggedListName | TaggedListLiteral | TaggedListApplication | TaggedListOperatorCombination;
type TaggedListConditional = ["conditional_expression", [TaggedListExpression, [TaggedListComponent, [TaggedListComponent, null]]]];
type TaggedListLambda = ["lambda_expression", [List<Name>, [TaggedListComponent, null]]];
type TaggedListName = ["name", [Symbol, null]];
type TaggedListLiteral = ["literal", [Value, null]];
type TaggedListApplication = ["application", [Expression, [List<TaggedListExpression>, null]]];
type TaggedListOperatorCombination = TaggedListUnary | TaggedListBinary;
type TaggedListUnary = ["unary_operator_combination", [Operator, List<TaggedListExpression>]];
type TaggedListBinary= ["binary_operator_combination", [Operator, List<TaggedListExpression>]];

type Component = Statement | Expression; 

type Statement = Block | Sequence | Declaration | Assignment | ReturnStatement;
type Block = { tag: "block", body: Component };
type Sequence = { tag: "sequence", statements: List<Statement> };
type Declaration = Constant | Function | Variable;
type Function = { tag: "function_declaration", name: Name, parameters: List<Name>, body: Component };
type Constant = { tag: "constant_declaration", name: Name, initialiser: Expression };
type Variable = { tag: "variable_declaration", name: Name, initialiser: Expression };
type Assignment = { tag: "assignment", name: Name, right_hand_side: Expression };
type ReturnStatement = { tag: "return_statement", return_expression: Expression };

type Expression = Conditional | Lambda | Name | Literal | Application | OperatorCombination;
type Conditional = { tag: conditional_tags, predicate: Expression, consequent: Component, alternative: Component};
type conditional_tags = "conditional_expression" | "conditional_statement";
type Lambda = { tag: "lambda_expression", parameters: List<Name>, body: Component };
type Name = { tag: "name", symbol: Symbol };
type Literal = { tag: "literal", value: Value };
type Application = { tag: "application", function_expression: Expression, arguments: List<Expression> };
type OperatorCombination = Unary | Binary;
type Operator = "+" | "-" | "*" | "/";
type Unary = { tag: "unary_operator_combination", operator: Operator, operand: Expression };
type Binary= { tag: "binary_operator_combination", operator: Operator, left: Expression, right: Expression };

type Value = string | number | boolean | undefined | null | ReturnValue | CompoundFunction | Primitive;

type ReturnValue = ["return_value", [Value, null]];
type CompoundFunction = ["compound_function", [List<Name>, [Component, [Environment, null]]]];
type Primitive = ["primitive", [(..._: any[]) => any, null]];

type Symbol = string;

function tagged_list_to_record(component: TaggedListComponent): Component {
    function is_statement(c: TaggedList): c is TaggedListExpression {
        const tag = head(c);
        return tag === "block" || tag === "sequence" || 
            tag === "function_declaration" || tag === "constant_declaration" || tag === "assignment" || 
            tag === "return_statement";
    }
    function is_expression(c: TaggedList): c is TaggedListExpression {
        const tag = head(c);
        return tag === "conditional_expression" || tag === "lambda_expression" || 
            tag === "name" || tag === "literal" || tag === "application" || 
            tag === "unary_operator_combination" || tag === "binary_operator_combination";
    }
    function is_tagged_list(component: any, the_tag: string): boolean {
        return is_pair(component) && head(component) === the_tag;
    }
    function is_literal(component: TaggedListComponent): component is TaggedListLiteral {
        return is_tagged_list(component, "literal");
    }
    function is_name(component: TaggedListComponent): component is TaggedListName {
        return is_tagged_list(component, "name");
    }
    function is_assignment(component: TaggedListComponent): component is TaggedListAssignment {
        return is_tagged_list(component, "assignment");
    }
    function is_declaration(component: TaggedListComponent): component is TaggedListDeclaration {
        return is_tagged_list(component, "constant_declaration") ||
            is_tagged_list(component, "variable_declaration") ||
            is_tagged_list(component, "function_declaration");
    }
    function is_lambda_expression(component: TaggedListComponent): component is TaggedListLambda {
        return is_tagged_list(component, "lambda_expression");
    }
    function is_function_declaration(component: TaggedListComponent): component is TaggedListFunction {
        return is_tagged_list(component, "function_declaration");
    }
    function is_return_statement(component: TaggedListComponent): component is TaggedListReturnStatement {
        return is_tagged_list(component, "return_statement");
    }
    function is_conditional(component: TaggedListComponent): component is TaggedListConditional {
        return is_tagged_list(component, "conditional_expression") ||
            is_tagged_list(component, "conditional_statement");
    }
    function is_sequence(stmt: TaggedListComponent): stmt is TaggedListSequence {
        return is_tagged_list(stmt, "sequence");
    }
    function is_block(component: TaggedListComponent): component is TaggedListBlock {
        return is_tagged_list(component, "block");
    }
    function is_operator_combination(component: TaggedListComponent): component is TaggedListOperatorCombination {
        return is_unary_operator_combination(component) ||
            is_binary_operator_combination(component);
    }
    function is_unary_operator_combination(component: TaggedListComponent): component is TaggedListUnary {
        return is_tagged_list(component, "unary_operator_combination");
    }
    function is_binary_operator_combination(component: TaggedListComponent): component is TaggedListBinary {
        return is_tagged_list(component, "binary_operator_combination");
    }
    function is_application(component: TaggedListComponent): component is TaggedListApplication {
        return is_tagged_list(component, "application");
    }

    // Transformers
    function transform_name(name: TaggedListName): Name {
        return { tag: "name", symbol: head(tail(name)) };
    }
    function transform_literal(literal: TaggedListLiteral): Literal {
        return { tag: "literal", value: head(tail(literal)) };
    }
    function transform_application(app: TaggedListApplication): Application {
        return { tag: "application", function_expression: transform_expression(head(tail(app))), arguments: map(tagged_list_to_record, head(tail(tail(app)))) };
    }
    function transform_operator_combination(op: TaggedListOperatorCombination): OperatorCombination {
        const operator: Operator = head(tail(op));
        const operands: List<Expression> = map(transform_expression, tail(tail(op)));
        return head(op) === "unary_operator_combination"
            ? { tag: "unary_operator_combination", operator: operator, operand: list_ref(operands, 0) }
            : { tag: "binary_operator_combination", operator: operator, left: list_ref(operands, 0), right: list_ref(operands, 1) };
    }
    function transform_conditional(cond: TaggedListConditional): Conditional {
        return { tag: head(cond), predicate: transform_expression(list_ref(cond, 1)), consequent: transform_component(list_ref(cond, 2)), alternative: transform_component(list_ref(cond, 3)) };
    }
    function transform_lambda(lam: TaggedListLambda): Lambda {
        return { tag: "lambda_expression", parameters: map(transform_name, list_ref(lam, 1)), body: transform_component(list_ref(lam, 2)) };
    }
    function transform_sequence(seq: TaggedListSequence): Sequence {
        return { tag: "sequence", statements: map(transform_component, list_ref(seq, 1)) };
    }
    function transform_block(block: TaggedListBlock): Block {
        return { tag: "block", body: map(transform_statement, list_ref(block, 0)) };
    }
    function transform_return_statement(ret: TaggedListReturnStatement): ReturnStatement {
        return { tag: "return_statement", return_expression: transform_expression(list_ref(ret, 1)) };
    }
    function transform_function_declaration(fun: TaggedListFunction): Function {
        return { tag: "function_declaration", name: transform_name(list_ref(fun, 1)), parameters: map(transform_name, list_ref(fun, 2)), body: transform_component(list_ref(fun, 3)) };
    }
    function transform_declaration(decl: TaggedListDeclaration): Declaration {
        return { tag: "constant_declaration", name: transform_name(list_ref(decl, 1)), initialiser: transform_expression(list_ref(decl, 2)) };
    }
    function transform_assignment(assg: TaggedListAssignment): Assignment {
        return { tag: "assignment", name: transform_name(list_ref(assg, 1)), right_hand_side: transform_expression(list_ref(assg, 2)) };
    }

    function transform_component(component: TaggedListComponent): Component {
        return is_expression(component)
            ? transform_expression(component)
            : is_statement(component)
            ? transform_statement(component)
            : error(component, "unknown syntax -- record transformation");
    }

    function transform_statement(exp: TaggedListStatement): Statement {
        return is_sequence(exp)
            ? transform_sequence(exp)
            : is_block(exp)
            ? transform_block(exp)
            : is_return_statement(exp)
            ? transform_return_statement(exp)
            : is_function_declaration(exp)
            ? transform_function_declaration(exp)
            : is_declaration(exp)
            ? transform_declaration(exp)
            : is_assignment(exp)
            ? transform_assignment(exp)
            : error(exp, "Not a statement!");
    }
    
    function transform_expression(exp: TaggedListExpression): Expression {
        return is_literal(exp)
            ? transform_literal(exp)
            : is_name(exp)
            ? transform_name(exp)
            : is_application(exp)
            ? transform_application(exp)
            : is_operator_combination(exp)
            ? transform_operator_combination(exp)
            : is_conditional(exp)
            ? transform_conditional(exp)
            : is_lambda_expression(exp)
            ? transform_lambda(exp)
            : error(exp, "Not an expression!");
    }

    return transform_component(component);
}

function incremental_transform(component: TaggedListComponent): Component {
    function is_statement(c: TaggedList): c is TaggedListExpression {
        const tag = head(c);
        return tag === "block" || tag === "sequence" || 
            tag === "function_declaration" || tag === "constant_declaration" || tag === "assignment" || 
            tag === "return_statement";
    }
    function is_expression(c: TaggedList): c is TaggedListExpression {
        const tag = head(c);
        return tag === "conditional_expression" || tag === "lambda_expression" || 
            tag === "name" || tag === "literal" || tag === "application" || 
            tag === "unary_operator_combination" || tag === "binary_operator_combination";
    }
    function is_tagged_list(component: any, the_tag: string): boolean {
        return is_pair(component) && head(component) === the_tag;
    }
    function is_literal(component: TaggedListComponent): component is TaggedListLiteral {
        return is_tagged_list(component, "literal");
    }
    function is_name(component: TaggedListComponent): component is TaggedListName {
        return is_tagged_list(component, "name");
    }
    function is_assignment(component: TaggedListComponent): component is TaggedListAssignment {
        return is_tagged_list(component, "assignment");
    }
    function is_declaration(component: TaggedListComponent): component is TaggedListDeclaration {
        return is_tagged_list(component, "constant_declaration") ||
            is_tagged_list(component, "variable_declaration") ||
            is_tagged_list(component, "function_declaration");
    }
    function is_lambda_expression(component: TaggedListComponent): component is TaggedListLambda {
        return is_tagged_list(component, "lambda_expression");
    }
    function is_function_declaration(component: TaggedListComponent): component is TaggedListFunction {
        return is_tagged_list(component, "function_declaration");
    }
    function is_return_statement(component: TaggedListComponent): component is TaggedListReturnStatement {
        return is_tagged_list(component, "return_statement");
    }
    function is_conditional(component: TaggedListComponent): component is TaggedListConditional {
        return is_tagged_list(component, "conditional_expression") ||
            is_tagged_list(component, "conditional_statement");
    }
    function is_sequence(stmt: TaggedListComponent): stmt is TaggedListSequence {
        return is_tagged_list(stmt, "sequence");
    }
    function is_block(component: TaggedListComponent): component is TaggedListBlock {
        return is_tagged_list(component, "block");
    }
    function is_operator_combination(component: TaggedListComponent): component is TaggedListOperatorCombination {
        return is_unary_operator_combination(component) ||
            is_binary_operator_combination(component);
    }
    function is_unary_operator_combination(component: TaggedListComponent): component is TaggedListUnary {
        return is_tagged_list(component, "unary_operator_combination");
    }
    function is_binary_operator_combination(component: TaggedListComponent): component is TaggedListBinary {
        return is_tagged_list(component, "binary_operator_combination");
    }
    function is_application(component: TaggedListComponent): component is TaggedListApplication {
        return is_tagged_list(component, "application");
    }

    // Transformers
    function transform_name(name: TaggedListName): Name {
        // return name as unknown as Name;
        return { tag: "name", symbol: head(tail(name)) };
    }
    function transform_literal(literal: TaggedListLiteral): Literal {
        return literal as unknown as Literal;
        // return { tag: "literal", value: head(tail(literal)) };
    }
    function transform_application(app: TaggedListApplication): Application {
        return list(head(app), transform_expression(head(tail(app))), map(incremental_transform, head(tail(tail(app)))));
        // return { tag: "application", function_expression: transform_expression(head(tail(app))), arguments: map(incremental_transform, head(tail(tail(app)))) };
    }
    function transform_operator_combination(op: TaggedListOperatorCombination): OperatorCombination {
        return append(list(head(op), head(tail(op))), map(transform_component, tail(tail(op))));
        // const operator: Operator = head(tail(op));
        // const operands: List<Expression> = map(transform_expression, tail(tail(op)));
        // return head(op) === "unary_operator_combination"
            // ? { tag: "unary_operator_combination", operator: operator, operand: list_ref(operands, 0) }
            // : { tag: "binary_operator_combination", operator: operator, left: list_ref(operands, 0), right: list_ref(operands, 1) };
    }
    function transform_conditional(cond: TaggedListConditional): Conditional {
        return list(head(cond), transform_expression(list_ref(cond, 1)), transform_component(list_ref(cond, 2)), transform_component(list_ref(cond, 3)));
        // return { tag: head(cond), predicate: transform_expression(list_ref(cond, 1)), consequent: transform_component(list_ref(cond, 2)), alternative: transform_component(list_ref(cond, 3)) };
    }
    function transform_lambda(lam: TaggedListLambda): Lambda {
        return list(head(lam), map(transform_component, list_ref(lam, 1)), transform_component(list_ref(lam, 2)));
        // return { tag: "lambda_expression", parameters: map(transform_name, list_ref(lam, 1)), body: transform_component(list_ref(lam, 2)) };
    }
    function transform_sequence(seq: TaggedListSequence): Sequence {
        return list(head(seq), map(transform_component, list_ref(seq, 1)));
        // return { tag: "sequence", statements: map(transform_component, list_ref(seq, 1)) };
    }
    function transform_block(block: TaggedListBlock): Block {
        return list(head(block), map(transform_component, list_ref(block, 1)));
        // return { tag: "block", body: map(transform_statement, list_ref(block, 1)) };
    }
    function transform_return_statement(ret: TaggedListReturnStatement): ReturnStatement {
        return list(head(ret), transform_expression(list_ref(ret, 1)));
        // return { tag: "return_statement", return_expression: transform_expression(list_ref(ret, 1)) };
    }
    function transform_function_declaration(fun: TaggedListFunction): Function {
        return list("function_declaration", transform_name(list_ref(fun, 1)), map(transform_name, list_ref(fun, 2)), transform_component(list_ref(fun, 3)));
        // return { tag: "function_declaration", name: transform_name(list_ref(fun, 1)), parameters: map(transform_name, list_ref(fun, 2)), body: transform_component(list_ref(fun, 3)) };
    }
    function transform_declaration(decl: TaggedListDeclaration): Declaration {
        return list("constant_declaration", transform_name(list_ref(decl, 1)), transform_expression(list_ref(decl, 2)));
        // return { tag: "constant_declaration", name: transform_name(list_ref(decl, 1)), initialiser: transform_expression(list_ref(decl, 2)) };
    }
    function transform_assignment(assg: TaggedListAssignment): Assignment {
        return list("assignment", transform_name(list_ref(assg, 1)), transform_expression(list_ref(assg, 2)));
        // return { tag: "assignment", name: transform_name(list_ref(assg, 1)), right_hand_side: transform_expression(list_ref(assg, 2)) };
    }

    function transform_component(component: TaggedListComponent): Component {
        return is_expression(component)
            ? transform_expression(component)
            : is_statement(component)
            ? transform_statement(component)
            : error(component, "unknown syntax -- record transformation");
    }

    function transform_statement(exp: TaggedListStatement): Statement {
        return is_sequence(exp)
            ? transform_sequence(exp)
            : is_block(exp)
            ? transform_block(exp)
            : is_return_statement(exp)
            ? transform_return_statement(exp)
            : is_function_declaration(exp)
            ? transform_function_declaration(exp)
            : is_declaration(exp)
            ? transform_declaration(exp)
            : is_assignment(exp)
            ? transform_assignment(exp)
            : error(exp, "Not a statement!");
    }
    
    function transform_expression(exp: TaggedListExpression): Expression {
        return is_literal(exp)
            ? transform_literal(exp)
            : is_name(exp)
            ? transform_name(exp)
            : is_application(exp)
            ? transform_application(exp)
            : is_operator_combination(exp)
            ? transform_operator_combination(exp)
            : is_conditional(exp)
            ? transform_conditional(exp)
            : is_lambda_expression(exp)
            ? transform_lambda(exp)
            : error(exp, "Not an expression!");
    }

    return transform_component(component);
}


// SICP JS 3.1.4

// functions from SICP JS 4.1.1

function evaluate(component: Component, env: Environment): Value {
    return is_literal(component)
           ? literal_value(component)
           : is_name(component)
           ? lookup_symbol_value(symbol_of_name(component), env)
           : is_application(component)
           ? apply(evaluate(function_expression(component), env),
                   list_of_values(arg_expressions(component), env))
           : is_operator_combination(component)
           ? evaluate(operator_combination_to_application(component),
                      env)
           : is_conditional(component)
           ? eval_conditional(component, env)
           : is_lambda_expression(component)
           ? make_function(lambda_parameter_symbols(component),
                           lambda_body(component), env)
           : is_sequence(component)
           ? eval_sequence(sequence_statements(component), env)
           : is_block(component)
           ? eval_block(component, env)
           : is_return_statement(component)
           ? eval_return_statement(component, env)
           : is_function_declaration(component)
           ? evaluate(function_decl_to_constant_decl(component), env)
           : is_declaration(component)
           ? eval_declaration(component, env)
           : is_assignment(component)
           ? eval_assignment(component, env)
           : error(component, "unknown syntax -- evaluate");
}

function apply(fun: Value, args: List<Value>) {
    if (is_primitive_function(fun)) {
        return apply_primitive_function(fun, args);
    } else if (is_compound_function(fun)) {
        const result = evaluate(function_body(fun),
                                extend_environment(
                                    function_parameters(fun),
                                    args,
                                    function_environment(fun)));
        return is_return_value(result)
               ? return_value_content(result)
               : undefined;
    } else {
        error(fun, "unknown function type -- apply");
    }
}

function list_of_values(exps: List<Expression>, env: Environment) {
    return map((arg: Expression) => evaluate(arg, env), exps);
}

function eval_conditional(component: Conditional, env: Environment): Value {
    return is_truthy(evaluate(conditional_predicate(component), env))
           ? evaluate(conditional_consequent(component), env)
           : evaluate(conditional_alternative(component), env);
}

function eval_sequence(stmts: List<Statement>, env: Environment): Value {
    if (is_empty_sequence(stmts)) {
        return undefined;
    } else if (is_last_statement(stmts)) {
        return evaluate(first_statement(stmts), env);
    } else {
        const first_stmt_value = 
            evaluate(first_statement(stmts), env);
        if (is_return_value(first_stmt_value)) {
            return first_stmt_value;
        } else {
            return eval_sequence(rest_statements(stmts), env);
        }
    }
}

function scan_out_declarations(component: Component): List<Symbol> {
    return is_sequence(component)
           ? accumulate(append,
                        null,
                        map(scan_out_declarations,
                            sequence_statements(component)))
           : is_declaration(component)
           ? list(declaration_symbol(component))
           : null;
}

function eval_block(component: Block, env: Environment): Value {
    const body = block_body(component);
    const locals = scan_out_declarations(body);
    const unassigneds = list_of_unassigned(locals);
    return evaluate(body, extend_environment(locals,
                                             unassigneds, 
                                             env));
}
function list_of_unassigned(symbols: List<Symbol>): List<string> {
    return map((_:any) => "*unassigned*", symbols);
}

function eval_return_statement(component: ReturnStatement, env: Environment): Value {
    return make_return_value(evaluate(return_expression(component),
                                      env));
}

function eval_assignment(component: Assignment, env: Environment): Value {
    const value = evaluate(assignment_value_expression(component),
                           env);
    assign_symbol_value(assignment_symbol(component), value, env);
    return value;
}

function eval_declaration(component: Declaration, env: Environment): void {
    assign_symbol_value(
        declaration_symbol(component), 
        evaluate(declaration_value_expression(component), env),
        env);
    return undefined;
}

// functions from SICP JS 4.1.2

function is_tagged_list(component: any, the_tag: string): boolean {
    return is_pair(component) && head(component) === the_tag;
}

function is_literal(component: Component): component is Literal {
    return is_tagged_list(component, "literal");
}
function literal_value(component: Literal): Value {
    return head(tail(component));
}

function make_literal(value: Value): Literal {
    return list("literal", value);
}

function is_name(component: Component): component is Name {
    return is_tagged_list(component, "name");
}

function make_name(symbol: Symbol): Name {
    return list("name", symbol);
}

function symbol_of_name(component: Name): Symbol {
    return head(tail(component));
}

function is_assignment(component: Component): component is Assignment {
    return is_tagged_list(component, "assignment");
}
function assignment_symbol(component: Assignment): Symbol {
    return head(tail(head(tail(component))));
}
function assignment_value_expression(component: Assignment): Expression {
    return head(tail(tail(component)));
}

function is_declaration(component: Component): component is Declaration {
    return is_tagged_list(component, "constant_declaration") ||
           is_tagged_list(component, "variable_declaration") ||
           is_tagged_list(component, "function_declaration");
}

function declaration_symbol(component: Declaration): Symbol {
    return symbol_of_name(list_ref(component, 1));
}
function declaration_value_expression(component: Declaration): Expression {
    return list_ref(component, 2);
}

function make_constant_declaration(name: Name, value_expression: Expression): Constant {
    return list("constant_declaration", name, value_expression);
}

function is_lambda_expression(component: Component): component is Lambda {
    return is_tagged_list(component, "lambda_expression");
}
function lambda_parameter_symbols(component: Lambda): List<Symbol> {
    return map(symbol_of_name, head(tail(component)));
}
function lambda_body(component: Lambda): Component {
    return head(tail(tail(component)));
}

function make_lambda_expression(parameters: List<Name>, body: Component): Lambda {
    return list("lambda_expression", parameters, body);
}

function is_function_declaration(component: Component): component is Function {
    return is_tagged_list(component, "function_declaration");
}
function function_declaration_name(component: Function): Name {
    // return list_ref(component, 1);
    return head(tail(component));
}
function function_declaration_parameters(component: Function): List<Name> {
    // return list_ref(component, 2);
    return head(tail(tail(component)));
}
function function_declaration_body(component: Function): Component {
    return list_ref(component, 3);
}
function function_decl_to_constant_decl(component: Function): Constant {
    return make_constant_declaration(
               function_declaration_name(component),
               make_lambda_expression(
                   function_declaration_parameters(component),
                   function_declaration_body(component)));
}

function is_return_statement(component: Component): component is ReturnStatement {
   return is_tagged_list(component, "return_statement");
}
function return_expression(component: ReturnStatement) {
   return head(tail(component));
}

function is_conditional(component: Component): component is Conditional {
    return is_tagged_list(component, "conditional_expression") ||
           is_tagged_list(component, "conditional_statement");
}
function conditional_predicate(component: Conditional): Expression {
   return list_ref(component, 1);
}
function conditional_consequent(component: Conditional): Component {
   return list_ref(component, 2);
}
function conditional_alternative(component: Conditional): Component {
   return list_ref(component, 3);
}

function is_sequence(stmt: Component): stmt is Sequence {
   return is_tagged_list(stmt, "sequence");
}
function sequence_statements(stmt: Sequence): List<Statement> { 
   return head(tail(stmt));
}
function first_statement(stmts: List<Statement>): Statement {
   return head(stmts);
}
function rest_statements(stmts: List<Statement>): List<Statement> {
   return tail(stmts);
}
function is_empty_sequence(stmts: List<Statement>): boolean {
   return is_null(stmts);
}
function is_last_statement(stmts: List<Statement>): boolean {
   return is_null(tail(stmts));
}

function is_block(component: Component): component is Block {
    return is_tagged_list(component, "block");
}
function block_body(component: Block): Component {
    return component.body;
}

function make_block(statement: Statement): Block {
    return list("block", statement);
}

function is_operator_combination(component: Component): component is OperatorCombination {
    return is_unary_operator_combination(component) ||
           is_binary_operator_combination(component);
}
function is_unary_operator_combination(component: Component): component is Unary {
    return is_tagged_list(component, "unary_operator_combination");
}
function is_binary_operator_combination(component: Component): component is Binary {
    return is_tagged_list(component, "binary_operator_combination");
}
function operator_symbol(component: OperatorCombination): Operator {
    return head(tail(component));
}
function first_operand(component: OperatorCombination): Expression {
    return head(tail(tail(component)));
}
function second_operand(component: Binary): Expression {
    return head(tail(tail(tail(component))));
}

function make_application(function_expression: Name, argument_expressions: List<Expression>): Application {
    return list("application",
                function_expression, argument_expressions);
}

function operator_combination_to_application(component: OperatorCombination): Application {
    const operator = operator_symbol(component);
    return is_unary_operator_combination(component)
           ? make_application(make_name(operator),
                              list(first_operand(component)))
           : make_application(make_name(operator),
                              list(first_operand(component),
                                   second_operand(component)));
}

function is_application(component: Component): component is Application {
   return is_tagged_list(component, "application");
}
function function_expression(component: Application): Name {
   return head(tail(component));
}
function arg_expressions(component: Application): List<Expression> {
   return head(tail(tail(component)));
}

// functions from SICP JS 4.1.3

function is_truthy(x: any): boolean {
    return is_boolean(x) 
           ? x
           : error(x, "boolean expected, received");
}
function is_falsy(x: any): boolean { return ! is_truthy(x); }

function make_function(parameters: List<Symbol>, body: Component, env: Environment): CompoundFunction {
    return list("compound_function", parameters, body, env);
}
function is_compound_function(f: Value): f is CompoundFunction {
    return is_tagged_list(f, "compound_function");
}
function function_parameters(f: CompoundFunction): List<Symbol> { 
    return head(tail(f));
}

function function_body(f: CompoundFunction): Component { 
    return head(tail(tail(f)));
}

function function_environment(f: CompoundFunction): Environment { 
    return head(tail(tail(tail(f))));
}

function make_return_value(content: Value): ReturnValue {
    return list("return_value", content);
}
function is_return_value(value: Value): value is ReturnValue {
    return is_tagged_list(value, "return_value");
}
function return_value_content(value: ReturnValue): Value {
    return head(tail(value));
}

function enclosing_environment(env: Environment): Environment { return tail(env); }

function first_frame(env: Environment): Frame{ return head(env); }

const the_empty_environment: Environment = null;

function make_frame(symbols: List<Symbol>, values: List<Value>): Frame { return pair(symbols, values); }

function frame_symbols(frame: Frame): List<Symbol> { return head(frame); }

function frame_values(frame: Frame): List<Value> { return tail(frame); }

function extend_environment(symbols: List<Symbol>, vals: List<Value>, base_env: Environment): Environment {
    return length(symbols) === length(vals)
           ? pair(make_frame(symbols, vals), base_env)
           : length(symbols) < length(vals)
           ? error("too many arguments supplied: " + 
                   stringify(symbols) + ", " + 
                   stringify(vals))
           : error("too few arguments supplied: " + 
                   stringify(symbols) + ", " + 
                   stringify(vals));
}

function lookup_symbol_value(symbol: Symbol, env: Environment): Value {
    function env_loop(env: Environment): Value {
        function scan(symbols: List<Symbol>, vals: List<Value>): Value {
            return is_null(symbols)
                   ? env_loop(enclosing_environment(env))
                   : symbol === head(symbols)
                   ? head(vals)
                   : scan(tail(symbols), tail(vals));
        }
        if (env === the_empty_environment) {
            error(symbol, "unbound name");
        } else {
            const frame = first_frame(env);
            return scan(frame_symbols(frame), frame_values(frame));
        }
    }
    return env_loop(env);
}

function assign_symbol_value(symbol: Symbol, val: Value, env: Environment): void {
    function env_loop(env: Environment): void {
        function scan(symbols: List<Symbol>, vals: List<Value>): void {
            return is_null(symbols)
                   ? env_loop(enclosing_environment(env))
                   : symbol === head(symbols)
                   ? set_head(vals, val)
                   : scan(tail(symbols), tail(vals));
        } 
        if (env === the_empty_environment) {
            error(symbol, "unbound name -- assignment");
        } else {
            const frame = first_frame(env);
            return scan(frame_symbols(frame), frame_values(frame));
        }
    }
    return env_loop(env);
}

// functions from SICP JS 4.1.4

function is_primitive_function(fun: Value): fun is Primitive {
    return is_tagged_list(fun, "primitive");
}

function primitive_implementation(fun: Primitive): (..._: any[]) => any { return head(tail(fun)); }

const primitive_functions = list(
       list("head",    head             ),
       list("tail",    tail             ),
       list("pair",    pair             ),
       list("list",    list             ),
       list("is_null", is_null          ),
       list("display", display          ),
       list("error",   error            ),
       list("math_abs",math_abs         ),
       list("+",       (x: any, y: any) => x + y  ),
       list("-",       (x: any, y: any) => x - y  ),
       list("-unary",  (x: any)         =>   - x  ),
       list("*",       (x: any, y: any) => x * y  ),
       list("/",       (x: any, y: any) => x / y  ),
       list("%",       (x: any, y: any) => x % y  ),
       list("===",     (x: any, y: any) => x === y),
       list("!==",     (x: any, y: any) => x !== y),
       list("<",       (x: any, y: any) => x <   y),
       list("<=",      (x: any, y: any) => x <=  y),
       list(">",       (x: any, y: any) => x >   y),
       list(">=",      (x: any, y: any) => x >=  y),
       list("!",       (x: any)         =>   !   x)
       );
const primitive_function_symbols =
    map(head, primitive_functions);
const primitive_function_objects =
    map((fun: [string, [Primitive, null]]) => list("primitive", head(tail(fun))),
        primitive_functions);

const primitive_constants = list(list("undefined", undefined),
                                 list("Infinity",  Infinity),
                                 list("math_PI",   math_PI),
                                 list("math_E",    math_E),
                                 list("NaN",       NaN)
                                );
const primitive_constant_symbols =
    map((c: List<any>) => head(c), primitive_constants);
const primitive_constant_values =
    map((c: List<any>) => head(tail(c)), primitive_constants);

function apply_primitive_function(fun: Primitive, arglist: List<Value>) {
    return apply_in_underlying_javascript(
               primitive_implementation(fun), arglist);
}

function setup_environment(): Environment {
    return extend_environment(append(primitive_function_symbols,
                                     primitive_constant_symbols),
                              append(primitive_function_objects, 
                                     primitive_constant_values),
                              the_empty_environment);
}

const the_global_environment = setup_environment();

function to_string(object: any): string {
    return is_compound_function(object)
           ? "<compound-function>"
           : is_primitive_function(object)
           ? "<primitive-function>"
           : is_pair(object)
           ? "[" + to_string(head(object)) + ", "
                 + to_string(tail(object)) + "]"
           : stringify(object);
}

function user_print(prompt_string: string, object: any): void {
    display("----------------------------",
            prompt_string + "\n" + to_string(object) + "\n");
}

function user_read(prompt_string: string): string | null {
    display(prompt_string);
    return prompt("");
}

const input_prompt = "\nM-evaluate input:\n";
const output_prompt = "\nM-evaluate value:\n";

function driver_loop(env: Environment, history: string): void {
    const input = user_read(history);
    if (is_null(input)) {
        display("", history + "\n--- session end ---\n");
    } else {
        const program = parse(input);
        const locals = scan_out_declarations(program);
        const unassigneds = list_of_unassigned(locals);
        const program_env = extend_environment(
                                locals, unassigneds, env);
        const output = evaluate(program, program_env);
        const new_history = history +
              input_prompt +
              input +
              output_prompt +
              to_string(output);
        return driver_loop(program_env, new_history);
    }
}

"metacircular evaluator loaded";

driver_loop(the_global_environment, "--- session start ---");
