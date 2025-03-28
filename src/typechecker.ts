import { error } from "console";
import type {
    DeclFun,
    Expr,
    Pattern,
    PatternVar,
    PatternVariant,
    Program,
    Type,
    TypeBool,
    TypeFun,
    TypeList,
    TypeNat,
    TypeRecord,
    TypeRef,
    TypeTuple,
    TypeUnit,
    TypeVariant,
    TypeBottom,
    TypeSum,
    TypeVar,
} from './ast';

enum Errors {
    // Coding Exercise 1
    /** unexpected type specified for a parameter of an anonymous function */
    UNEXPECTED_TYPE_FOR_PARAMETER = 'ERROR_UNEXPECTED_TYPE_FOR_PARAMETER',
    /** type of an expression does not match an expected type (known from larger context) */
    UNEXPECTED_TYPE_FOR_EXPRESSION = 'ERROR_UNEXPECTED_TYPE_FOR_EXPRESSION',
    /** unexpected anonymous function where an expression of a non-function type is expected */
    UNEXPECTED_LAMBDA = 'ERROR_UNEXPECTED_LAMBDA',
    /** unexpected expression where a function is expected */
    NOT_A_FUNCTION = 'ERROR_NOT_A_FUNCTION',
    /** undefined variable in a an expression */
    UNDEFINED_VARIABLE = 'ERROR_UNDEFINED_VARIABLE',
    /** a program is missing main function */
    MISSING_MAIN = 'ERROR_MISSING_MAIN',

    // Coding Exercise 2
    /** record is missing one or more of the expected fields */
    MISSING_RECORD_FIELDS = 'ERROR_MISSING_RECORD_FIELDS',
    /** record has one or more unexpected fields */
    UNEXPECTED_RECORD_FIELDS = 'ERROR_UNEXPECTED_RECORD_FIELDS',
    /** unexpected record where an expression of a non-record type is expected */
    UNEXPECTED_RECORD = 'ERROR_UNEXPECTED_RECORD',
    /** unexpected expression where a record is expected */
    NOT_A_RECORD = 'ERROR_NOT_A_RECORD',
    /** access to a field that is not present in the record */
    UNEXPECTED_FIELD_ACCESS = 'ERROR_UNEXPECTED_FIELD_ACCESS',
    /** unexpected tuple/pair where an expression of a non-tuple type is expected */
    UNEXPECTED_TUPLE = 'ERROR_UNEXPECTED_TUPLE',
    /** unexpected expression where a tuple/pair is expected */
    NOT_A_TUPLE = 'ERROR_NOT_A_TUPLE',
    TUPLE_INDEX_OUT_OF_BOUNDS = 'ERROR_TUPLE_INDEX_OUT_OF_BOUNDS',

    // Coding Exercise 3
    /* when it is impossible to typecheck an expression of a sum type because the other half of the type is unknown */
    AMBIGUOUS_SUM_TYPE = 'ERROR_AMBIGUOUS_SUM_TYPE',
    /* when it is impossible to typecheck an expression of a list type because the type of its elements is unknown */
    AMBIGUOUS_LIST_TYPE = 'ERROR_AMBIGUOUS_LIST_TYPE',
    /* when match-expression does not have any patterns */
    ILLEGAL_EMPTY_MATCHING = 'ERROR_ILLEGAL_EMPTY_MATCHING',
    /* when match-expression does not have all necessary patterns (inl and inr for sum types and at least the empty list pattern and cons-pattern for lists) */
    NONEXHAUSTIVE_MATCH_PATTERNS = 'ERROR_NONEXHAUSTIVE_MATCH_PATTERNS',
    /* when an expression of a non-list type appears as the argument to one of the builtin list functions (List::head, List::tail, or List::isempty) */
    NOT_A_LIST = 'ERROR_NOT_A_LIST',
    /* when a list (List or ConsList) is encountered instead of an expression of expected non-list type */
    UNEXPECTED_LIST = 'ERROR_UNEXPECTED_LIST',
    /* when an injection into a sum type (inl or inr) is encountered instead of an expression of expected non-sum type */
    UNEXPECTED_INJECTION = 'ERROR_UNEXPECTED_INJECTION',
    /* when a pattern in a match-expression does not correspond to the type of matched expression */
    UNEXPECTED_PATTERN_FOR_TYPE = 'ERROR_UNEXPECTED_PATTERN_FOR_TYPE',

    // Coding Exercise 4
    /** when type of a variant (Variant) cannot be inferred (needs a type ascription) */
    AMBIGUOUS_VARIANT_TYPE = 'ERROR_AMBIGUOUS_VARIANT_TYPE',
    /** when a variant (Variant) is encountered where an expression of a non-variant type is expected */
    UNEXPECTED_VARIANT = 'ERROR_UNEXPECTED_VARIANT',
    /** when a variant (Variant) contains a label that does not match any of the labels in the expected variant type */
    UNEXPECTED_VARIANT_LABEL = 'ERROR_UNEXPECTED_VARIANT_LABEL',

    // Coding Exercise 5
    /** if an exception mechanism (Throw, TryCatch) is used without a globally declared exception type */
    EXCEPTION_TYPE_NOT_DECLARED = 'ERROR_EXCEPTION_TYPE_NOT_DECLARED',
    /** if the type of throw-expression cannot be inferred (not the type of of the exception, but the type of throw-expression itself!) */
    AMBIGUOUS_THROW_TYPE = 'ERROR_AMBIGUOUS_THROW_TYPE',
    /** if a bare memory address is found without an expected type for it */
    AMBIGUOUS_REFERENCE_TYPE = 'ERROR_AMBIGUOUS_REFERENCE_TYPE',
    /** if a panic expression is found without an expected type for it */
    AMBIGUOUS_PANIC_TYPE = 'ERROR_AMBIGUOUS_PANIC_TYPE',
    /** when to assign to or dereference an expression that does not have a reference type */
    NOT_A_REFERENCE = 'ERROR_NOT_A_REFERENCE',
    /** if a memory address literal is found when not expecting a reference */
    UNEXPECTED_MEMORY_ADDRESS = 'ERROR_UNEXPECTED_MEMORY_ADDRESS',
    UNEXPECTED_TUPLE_LENGTH = 'ERROR_UNEXPECTED_TUPLE_LENGTH'
}

interface Context {
    symbolTable: Map<string, Type>;
    exceptionType: Type | null;
    hasMain: boolean;

}

function generateEmptyContext(): Context {
    return {
        symbolTable: new Map(),
        exceptionType: null,
        hasMain: false,

    };
}

function copyContextWithSymbol(
    ctx: Context,
    name: string,
    type: Type
): Context {
    const newSymbols = new Map(ctx.symbolTable);
    newSymbols.set(name, type);
    return {
        ...ctx,
        symbolTable: newSymbols,
    };
}

const TYPE_NAT: TypeNat = { type: 'TypeNat' };
const TYPE_BOOL: TypeBool = { type: 'TypeBool' };
const TYPE_UNIT: TypeUnit = { type: 'TypeUnit' };
const TYPE_BOT: TypeBottom = { type: 'TypeBottom' };
const TYPE_LIST = (t: Type): TypeList => ({ type: 'TypeList', elementType: t });
const TYPE_FUN = (param: Type, ret: Type): TypeFun => ({
    type: 'TypeFun',
    parametersTypes: [param],
    returnType: ret,
});
const TYPE_REF = (type: Type): TypeRef => ({
    type: 'TypeRef',
    referredType: type,
});
const TYPE_SUM = (left: Type, right: Type): TypeSum => ({
    type: 'TypeSum',
    left,
    right,
});
export function typecheckProgram(ast: Program) {
    const ctx = generateEmptyContext();
    try {
        for (const decl of ast.declarations) {
            switch (decl.type) {
                case "DeclFun": {
                    ctx.symbolTable.set(decl.name, {
                        type: "TypeFun",
                        parametersTypes: [decl.parameters[0].paramType],
                        // TODO: make sure a return type actually exists
                        returnType: decl.returnType!,
                    });
                    typecheckFunctionDecl(decl, ctx);
                    break;
                }
                case "DeclExceptionType": {
                    ctx.exceptionType = decl.exceptionType;
                    break;
                }
                default:
                    throw new Error("Unknown declaration type");
            }
        }

        if (!ctx.hasMain) {
            throw new Error(Errors.MISSING_MAIN);
        }
        console.log("Everything typechecks!");
    }
    catch (error) {
        console.error("An unexpected error:", error);
        process.exit(1);
    }
}

function typecheckFunctionDecl(decl: DeclFun, ctx: Context) {
    const { name, parameters, returnValue, returnType } = decl;
    console.log(`Checking the function "${name}"...`);
    if (name === "main") {
        ctx.hasMain = true;
    }
    const param = parameters[0];
    const newContext = copyContextWithSymbol(ctx, param.name, param.paramType);
    const bodyType = typecheckExpr(returnValue, returnType!, newContext);
    verifyTypesMatch(bodyType, returnType!);
}

function typecheckExpr(
    expr: Expr,
    expectedType: Type | null,
    ctx: Context
): Type {
    console.log('Checking expression', expr);
    console.log('expected type:', expectedType);
    switch (expr.type) {
        case "Var": {
            const { symbolTable } = ctx;
            if (!symbolTable.has(expr.name)) {
                console.log(expr.name);
                throw new Error(Errors.UNDEFINED_VARIABLE);
            }
            return symbolTable.get(expr.name)!;
        }
        case "ConstBool": {
            return TYPE_BOOL;
        }
        case "If": {
            // check condition
            const condType = typecheckExpr(expr.condition, TYPE_BOOL, ctx);
            verifyTypesMatch(condType, TYPE_BOOL);
            // check both sides 
            const thenType = typecheckExpr(expr.thenExpr, expectedType, ctx);
            const elseType = typecheckExpr(expr.elseExpr, expectedType, ctx);
            // make sure both sides have the same type
            console.log("typesss", thenType);
            console.log("type2", elseType);
            verifyTypesMatch(thenType, elseType);
            return thenType;
        }
        case "ConstInt": {
            return TYPE_NAT;
        }
        case "Succ": {
            //check the passed expr is nat
            const exprType = typecheckExpr(expr.expr, TYPE_NAT, ctx);
            // make sure it's nat
            verifyTypesMatch(TYPE_NAT, exprType);
            // succ(nat) -> nat 
            return TYPE_NAT;
        }
        case "NatIsZero": {
            //check the passed expr is nat
            const exprType = typecheckExpr(expr.expr, TYPE_NAT, ctx);
            verifyTypesMatch(exprType, TYPE_NAT);
            return TYPE_BOOL;
        }
        case "NatRec": {
            const { n, initial, step } = expr;
            // n must be NAT
            const nType = typecheckExpr(n, TYPE_NAT, ctx);
            verifyTypesMatch(nType, TYPE_NAT);

            const initialType = typecheckExpr(initial, expectedType, ctx);
            if (initialType == null) {
                throw new Error("Couldn't infer a type for Nat::rec initial value");
            }
            const stepExpectedType: TypeFun = {
                type: "TypeFun",
                parametersTypes: [TYPE_NAT],
                returnType: {
                    type: "TypeFun",
                    parametersTypes: [initialType],
                    returnType: initialType,
                },
            };
            const stepActualType = typecheckExpr(step, stepExpectedType, ctx);
            verifyTypesMatch(stepExpectedType, stepActualType);
            return initialType;
        }
        case "Abstraction": {
            if (expectedType && expectedType.type !== "TypeFun") {
                throw new Error(Errors.UNEXPECTED_LAMBDA);
            }
            const { parameters, returnValue } = expr;
            // TODO: support multiple parameters
            const param = parameters[0];
            const paramExpected = expectedType?.parametersTypes[0];
            if (paramExpected) {
                try {
                    verifyTypesMatch(paramExpected, param.paramType);
                } catch {
                    throw new Error(Errors.UNEXPECTED_TYPE_FOR_PARAMETER);
                }
            }
            const newContext = copyContextWithSymbol(
                ctx,
                param.name,
                param.paramType
            );
            const bodyType = typecheckExpr(
                returnValue,
                expectedType?.returnType ?? null,
                newContext
            );
            return {
                type: "TypeFun",
                parametersTypes: [param.paramType],
                returnType: bodyType,
            };
        }
        // come here later
        case "Application": {
            const { function: func, arguments: args } = expr;
            const funcType = typecheckExpr(func, null, ctx);
            if (funcType.type !== "TypeFun") {
                throw new Error(Errors.NOT_A_FUNCTION);
            }
            const argType = typecheckExpr(args[0], funcType.parametersTypes[0], ctx);
            // TODO: support multiple paramaters
            verifyTypesMatch(funcType.parametersTypes[0], argType);
            if (expectedType) {
                verifyTypesMatch(funcType.returnType, expectedType);
            }
            return (funcType as TypeFun).returnType;
        }
        case 'Sequence': {
            const { expr1, expr2 } = expr;
            const expr1Type = typecheckExpr(expr1, TYPE_UNIT, ctx);
            verifyTypesMatch(TYPE_UNIT, expr1Type);
            return typecheckExpr(expr2, expectedType, ctx);
        }

        case "Unit": {
            return TYPE_UNIT;
        }
        case "Tuple": {
            if (expectedType && expectedType.type !== "TypeTuple") {
                throw new Error(Errors.UNEXPECTED_TUPLE);
            }
            // only pairs
            const exprs = expr.exprs;
            const expectedTypes = expectedType?.types || [];
            if (expectedType && exprs.length !== expectedTypes.length) {
                // Handle tuples with different lengths if necessary
                throw new Error(Errors.UNEXPECTED_TYPE_FOR_EXPRESSION);
            }
            const types = exprs.map((expr, index) =>
                typecheckExpr(expr, expectedTypes[index] ?? null, ctx)
            );
            return {
                type: "TypeTuple",
                types: types,
            };
        }
        case "DotTuple": {
            const { expr: tuple, index } = expr;
            const tupleType = typecheckExpr(tuple, null, ctx);
            if (tupleType.type !== "TypeTuple") {
                throw new Error(Errors.NOT_A_TUPLE);
            }
            if (index < 1 || index > tupleType.types.length) {
                throw new Error(Errors.TUPLE_INDEX_OUT_OF_BOUNDS);
            }
            // Stella pair index is 1-based
            return tupleType.types[index - 1];
        }
        case "Record": {
            if (expectedType && expectedType.type !== "TypeRecord") {
                throw new Error(Errors.UNEXPECTED_RECORD);
            }
            const { bindings } = expr;

            return {
                type: "TypeRecord",
                fieldTypes: bindings.map((b, i) => ({
                    type: "RecordFieldType" as const,
                    label: b.name,
                    fieldType: typecheckExpr(
                        b.expr,
                        (expectedType as TypeRecord | null)?.fieldTypes[i] ?? null,
                        ctx
                    ),
                })),
            };
        }
        case "DotRecord": {
            const { expr: record, label } = expr;
            const recordType = typecheckExpr(record, null, ctx);
            if (recordType.type !== "TypeRecord") {
                throw new Error(Errors.NOT_A_RECORD);
            }
            const fieldType = recordType.fieldTypes.find(
                (field) => field.label === label
            );
            if (fieldType == null) {
                throw new Error(Errors.UNEXPECTED_FIELD_ACCESS);
            }
            return fieldType.fieldType;
        }
        case "Let": {
            const newContext = { ...ctx };
            const { body, patternBindings } = expr;
            for (const { pattern, rhs } of patternBindings) {
                // Only PatternVar is supported for now
                if (pattern.type !== "PatternVar") {
                    throw new Error("Only PatternVar is supported for now");
                }
                const { name } = pattern as PatternVar;
                const type = typecheckExpr(rhs, null, newContext);
                newContext.symbolTable.set(name, type);
            }
            return typecheckExpr(body, expectedType, newContext);
        }

        case "TypeAscription": {
            const { expr: expression, ascribedType } = expr;
            const inferredType = typecheckExpr(expression, ascribedType, ctx);
            verifyTypesMatch(ascribedType, inferredType);
            return ascribedType;
        }
        case "Inl": {
            if (!expectedType) {
                throw new Error(Errors.AMBIGUOUS_SUM_TYPE);
            }
            if (expectedType.type !== "TypeSum") {
                throw new Error(Errors.UNEXPECTED_INJECTION);
            }
            const { expr: expression } = expr;
            const actualType = typecheckExpr(
                expression,
                expectedType?.left ?? null,
                ctx
            );
            verifyTypesMatch(expectedType.left, actualType);
            return expectedType;
        }
        case "Inr": {
            if (!expectedType) {
                throw new Error(Errors.AMBIGUOUS_SUM_TYPE);
            }
            if (expectedType.type !== "TypeSum") {
                throw new Error(Errors.UNEXPECTED_INJECTION);
            }
            const { expr: expression } = expr;
            const actualType = typecheckExpr(
                expression,
                expectedType?.right ?? null,
                ctx
            );
            verifyTypesMatch(expectedType.right, actualType);
            return expectedType;

        }
        case "Match": {
            const { cases, expr: expression } = expr;
            const exprType = typecheckExpr(expression, null, ctx);
            if (cases.length === 0) {
                throw new Error(Errors.ILLEGAL_EMPTY_MATCHING);
            }
            let caseBodyExpectedType: Type | null = expectedType;
            for (const matchCase of cases) {
                console.log("pattern:", matchCase.pattern);
                const extendedCtx = checkPattern(matchCase.pattern, exprType, ctx);
                const inferredType = typecheckExpr(
                    matchCase.expr,
                    expectedType,
                    extendedCtx
                );
                if (caseBodyExpectedType != null) {
                    verifyTypesMatch(caseBodyExpectedType, inferredType);
                } else {
                    caseBodyExpectedType = inferredType;
                }
            }
            if (
                !isExhaustive(
                    exprType,
                    cases.map((case_) => case_.pattern)
                )
            ) {
                throw new Error(Errors.NONEXHAUSTIVE_MATCH_PATTERNS);
            }
            return caseBodyExpectedType!;
        }
        case "List": {
            if (expectedType && expectedType.type !== "TypeList") {
                throw new Error(Errors.UNEXPECTED_LIST);
            }
            const { exprs } = expr;
            if (exprs.length === 0 && expectedType == null) {
                throw new Error(Errors.AMBIGUOUS_LIST_TYPE);
            }
            let elementExpectedType: Type | null = expectedType?.elementType!;
            // console.log("list type", elementExpectedType);
            for (const element of exprs) {
                const inferredType = typecheckExpr(
                    element,
                    expectedType?.elementType ?? null,
                    ctx
                );
                if (elementExpectedType) {
                    verifyTypesMatch(elementExpectedType, inferredType);
                } else {
                    elementExpectedType = inferredType;
                }
            }
            return TYPE_LIST(elementExpectedType);
        }
        case "Cons": {
            const { head, tail } = expr;
            if (expectedType && expectedType.type !== "TypeList") {
                throw new Error(Errors.UNEXPECTED_LIST);
            }
            const headType = typecheckExpr(
                head,
                expectedType?.elementType ?? null,
                ctx
            );
            const tailType = typecheckExpr(tail, expectedType, ctx);
            if (tailType.type !== "TypeList") {
                throw new Error(Errors.NOT_A_LIST);
            }
            verifyTypesMatch(headType, tailType.elementType);
            return tailType;
        }
        case "ListHead": {
            const { expr: expression } = expr;
            const exprType = typecheckExpr(
                expression,
                expectedType && TYPE_LIST(expectedType),
                ctx
            );
            if (exprType.type !== "TypeList") {
                throw new Error(Errors.NOT_A_LIST);
            }
            return exprType.elementType;
        }
        case "ListTail": {
            const { expr: expression } = expr;
            const exprType = typecheckExpr(
                expression,
                expectedType && TYPE_LIST(expectedType),
                ctx
            );
            if (exprType.type !== "TypeList") {
                throw new Error(Errors.NOT_A_LIST);
            }
            return exprType;
        }
        case "ListIsEmpty": {
            const { expr: expression } = expr;
            const exprType = typecheckExpr(
                expression,
                expectedType && TYPE_LIST(expectedType),
                ctx
            );
            if (exprType.type !== "TypeList") {
                throw new Error(Errors.NOT_A_LIST);
            }
            return TYPE_BOOL;
        }
        case "Variant": {
            const { label, expr: value } = expr;
            console.log("variant", label, value);
            let fieldExpectedType: Type | null = null;
            if (expectedType) {
                if (expectedType.type !== 'TypeVariant') {
                    throw new Error(Errors.UNEXPECTED_VARIANT);
                }
                const field = expectedType.fieldTypes.find(
                    (field) => field.label === label
                );
                if (field == undefined) {
                    throw new Error(Errors.UNEXPECTED_VARIANT_LABEL);
                }
                fieldExpectedType = field.fieldType!;
            }
            const fieldType = typecheckExpr(value!, fieldExpectedType, ctx);
            if (fieldExpectedType) {
                verifyTypesMatch(fieldExpectedType, fieldType);
            }
            return (
                expectedType ?? {
                    type: 'TypeVariant',
                    fieldTypes: [{ type: 'VariantFieldType', label, fieldType }],
                });
        }
        case "Fix": {
            const { expr: func } = expr;
            const expected = expectedType && TYPE_FUN(expectedType, expectedType);
            const actualType = typecheckExpr(func, expected, ctx);
            if (actualType.type !== "TypeFun") {
                throw new Error(Errors.NOT_A_FUNCTION);
            }
            verifyTypesMatch(actualType.parametersTypes[0], actualType.returnType);
            return actualType.returnType;
        }

        case 'ConstMemory': {
            if (!expectedType) {
                throw new Error(Errors.AMBIGUOUS_REFERENCE_TYPE);
            }
            if (expectedType.type !== 'TypeRef') {
                throw new Error(Errors.UNEXPECTED_MEMORY_ADDRESS);
            }
            return expectedType;
        }
        case 'Reference': {
            const { expr: initialValue } = expr;
            if (expectedType && expectedType.type === 'TypeRef') {
                expectedType = expectedType.referredType;
            }
            const exprType = typecheckExpr(initialValue, expectedType, ctx);
            return TYPE_REF(exprType);
        }
        case 'Dereference': {
            const { expr: reference } = expr;
            const refType = typecheckExpr(
                reference,
                expectedType && TYPE_REF(expectedType),
                ctx
            );
            if (refType.type !== 'TypeRef') {
                throw new Error(Errors.NOT_A_REFERENCE);
            }
            return refType.referredType;
        }
        case 'Assignment': {
            const { lhs, rhs } = expr;
            const lhsType = typecheckExpr(lhs, null, ctx);
            if (lhsType.type !== 'TypeRef') {
                throw new Error(Errors.NOT_A_REFERENCE);
            }
            const rhsType = typecheckExpr(rhs, lhsType.referredType, ctx);
            verifyTypesMatch(rhsType, lhsType.referredType);
            return TYPE_UNIT;
        }
        case "Reference": {
            if (expectedType && expectedType?.type === "TypeRef") {
                expectedType = expectedType.referredType;
            }
            const exprType = typecheckExpr(expr.expr, expectedType, ctx);
            const refType: Type = { type: "TypeRef", referredType: exprType }
            return refType
        }
        case "Dereference": {

            if (!expectedType) {
                throw new Error(Errors.AMBIGUOUS_REFERENCE_TYPE);
            }
            const exprType = typecheckExpr(expr.expr, {
                type: "TypeRef",
                referredType: expectedType,
            }, ctx);


            if (exprType.type !== "TypeRef") {
                throw new Error(Errors.NOT_A_REFERENCE);
            }
            return exprType.referredType;
        }
        case 'Panic': {
            if (!expectedType) {
                return TYPE_BOT;
            }
            return expectedType;
        }
        case 'Throw': {
            if (ctx.exceptionType == null) {
                throw new Error(Errors.EXCEPTION_TYPE_NOT_DECLARED);
            }
            if (!expectedType) {
                return TYPE_BOT;
            }
            const { expr: thrownValue } = expr;
            const valueType = typecheckExpr(thrownValue, ctx.exceptionType, ctx);
            verifyTypesMatch(ctx.exceptionType, valueType);
            return expectedType;
        }
        case 'TryWith': {
            const { tryExpr, fallbackExpr } = expr;
            const tryType = typecheckExpr(tryExpr, expectedType, ctx);
            const fallbackType = typecheckExpr(fallbackExpr, expectedType, ctx);
            verifyTypesMatch(tryType, fallbackType);
            return tryType;
        }
        case 'TryCatch': {
            const { tryExpr, pattern, fallbackExpr } = expr;
            const tryType = typecheckExpr(tryExpr, expectedType, ctx);
            if (ctx.exceptionType == null) {
                throw new Error(Errors.EXCEPTION_TYPE_NOT_DECLARED);
            }
            const fallbackExprCtx = checkPattern(pattern, ctx.exceptionType, ctx);
            const fallbackType = typecheckExpr(
                fallbackExpr,
                expectedType,
                fallbackExprCtx
            );
            verifyTypesMatch(tryType, fallbackType);
            return tryType;
        }
        default:
            throw new Error("Unknown expression type");
    }
}

function checkPattern(pattern: Pattern, type: Type, ctx: Context): Context {
    console.log(pattern, type);
    switch (pattern.type) {
        // Without nested patterns, all the recursive `checkPattern` calls should lead to PatternVar
        case "PatternVar": {
            return copyContextWithSymbol(ctx, pattern.name, type);
        }
        case "PatternInl": {
            if (type.type !== "TypeSum") {
                throw new Error(Errors.UNEXPECTED_PATTERN_FOR_TYPE);
            }
            return checkPattern(pattern.pattern, type.left, ctx);
        }
        case "PatternInr": {
            if (type.type !== "TypeSum") {
                throw new Error(Errors.UNEXPECTED_PATTERN_FOR_TYPE);
            }
            return checkPattern(pattern.pattern, type.right, ctx);
        }
        case "PatternVariant": {
            if (type.type !== "TypeVariant") {
                throw new Error(Errors.UNEXPECTED_PATTERN_FOR_TYPE);
            }
            const { label, pattern: innerPattern } = pattern;
            const { fieldTypes } = type;
            const field = fieldTypes.find((field) => field.label === label);
            if (field == undefined) {
                throw new Error(Errors.UNEXPECTED_PATTERN_FOR_TYPE);
            }
            // #nullary-variant-labels is bonus, so we'll consider fieldType and inner pattern as non-nullable
            return checkPattern(innerPattern!, field.fieldType!, ctx);
        }
        default:
            throw new Error("Unimplemented");
    }
}

function isExhaustive(type: Type, patterns: Pattern[]): boolean {
    const types = patterns.map((pattern) => pattern.type);
    if (types.some((type) => type === "PatternVar")) return true;
    switch (type.type) {
        case "TypeSum": {
            return types.includes("PatternInl") && types.includes("PatternInr");
        }
        case "TypeVariant": {
            const { fieldTypes } = type;
            const usedPatternLabels = (patterns as PatternVariant[]).map(
                (pattern) => pattern.label
            );
            for (const { label } of fieldTypes) {
                if (!usedPatternLabels.includes(label)) {
                    return false;
                }
            }
            return true;
        }
        default:
            return false;
    }
}

function verifyTypesMatch(expected: Type, actual: Type) {
    if (expected.type === 'TypeTop') {
        return true;
    }
    if (actual.type === 'TypeBottom') {
        return true;
    }
    // tuples
    if (expected.type === "TypeTuple" && actual.type !== "TypeTuple") {
        throw new Error(Errors.NOT_A_TUPLE);
    }
    if (expected.type !== "TypeTuple" && actual.type === "TypeTuple") {
        throw new Error(Errors.UNEXPECTED_TUPLE);
    }


    // Record
    if (expected.type !== "TypeRecord" && actual.type === "TypeRecord") {
        throw new Error(Errors.UNEXPECTED_RECORD);
    }

    if (expected.type === "TypeRecord" && actual.type !== "TypeRecord") {
        throw new Error(Errors.NOT_A_RECORD);
    }
    // fun
    if (expected.type === "TypeFun" && actual.type !== "TypeFun") {
        throw new Error(Errors.NOT_A_FUNCTION);
    }

    if (expected.type !== "TypeFun" && actual.type === "TypeFun") {
        throw new Error(Errors.UNEXPECTED_LAMBDA);
    }

    // list
    if (expected.type !== "TypeList" && actual.type === "TypeList") {
        throw new Error(Errors.UNEXPECTED_LIST);
    }

    if (expected.type === "TypeList" && actual.type !== "TypeList") {
        throw new Error(Errors.NOT_A_LIST);
    }

    if (expected.type !== actual.type) {
        throw new Error(Errors.UNEXPECTED_TYPE_FOR_EXPRESSION);
    }
    console.log('Matching types', expected, 'and', actual);
    if (expected.type === actual.type) {
        // Some types still require additional checks for complex internal structure
        switch (expected.type) {
            case 'TypeVar': {
                return expected.name === (actual as TypeVar).name;
            }
            case "TypeFun": {
                // TODO: handle multiple parameters
                verifyTypesMatch(
                    (actual as TypeFun).parametersTypes[0], // super
                    expected.parametersTypes[0]
                );
                verifyTypesMatch(expected.returnType, (actual as TypeFun).returnType);
                return true;
            }
            case "TypeTuple": {
                const expectedTypes = expected.types;
                const actualTypes = (actual as TypeTuple).types;
                if (expectedTypes.length !== actualTypes.length) {
                    throw new Error(Errors.UNEXPECTED_TUPLE_LENGTH);
                }
                for (let i = 0; i < expectedTypes.length; i++) {
                    verifyTypesMatch(expectedTypes[i], actualTypes[i]);
                }

                return true;
            }
            case "TypeRecord": {
                const actualFields = (actual as TypeRecord).fieldTypes;
                for (const { label, fieldType } of expected.fieldTypes) {
                    const actualField = actualFields.find((f) => f.label === label);
                    if (actualField == null) {
                        // Expected a field but did not find it
                        throw new Error(Errors.MISSING_RECORD_FIELDS);
                    }
                    verifyTypesMatch(fieldType, actualField.fieldType);
                }
                if (
                    actualFields.some(
                        (field) =>
                            !expected.fieldTypes.some(
                                (exField) => exField.label === field.label
                            )
                    )
                ) {
                    // There is an actual field that was not expected
                    throw new Error(Errors.UNEXPECTED_RECORD_FIELDS);
                }
                return true;
            }
            case "TypeList": {
                verifyTypesMatch(
                    expected.elementType,
                    (actual as TypeList).elementType
                );
                return true;
            }
            case 'TypeVariant': {
                const actualFields = (actual as TypeVariant).fieldTypes;
                // Less fields are ok, more are not
                for (const { label, fieldType } of actualFields) {
                    const expectedField = expected.fieldTypes.find(
                        (f) => f.label === label
                    );
                    if (expectedField == null) {
                        // unexpected variant
                        throw new Error(Errors.UNEXPECTED_VARIANT_LABEL);
                    }
                    verifyTypesMatch(expectedField.fieldType!, fieldType!);
                }
                return true;
            } case 'TypeRef': {
                verifyTypesMatch(
                    expected.referredType,
                    (actual as TypeRef).referredType
                );
                verifyTypesMatch(
                    (actual as TypeRef).referredType,
                    expected.referredType
                );
                return true;
            }
            case 'TypeSum': {
                verifyTypesMatch(expected.left, (actual as TypeSum).left);
                verifyTypesMatch(expected.right, (actual as TypeSum).right);
            }
            default:
                return true;
        }
    }
    return true;
}
