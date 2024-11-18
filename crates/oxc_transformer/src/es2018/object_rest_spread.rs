//! ES2018 object spread transformation.
//!
//! This plugin transforms rest properties for object destructuring assignment and spread properties for object literals.
//!
//! > This plugin is included in `preset-env`, in ES2018
//!
//! ## Example
//!
//! Input:
//! ```js
//! var x = { a: 1, b: 2 };
//! var y = { ...x, c: 3 };
//! ```
//!
//! Output:
//! ```js
//! var x = { a: 1, b: 2 };
//! var y = _objectSpread({}, x, { c: 3 });
//! ```
//!
//! ## Implementation
//!
//! Implementation based on [@babel/plugin-transform-object-rest-spread](https://babeljs.io/docs/babel-plugin-transform-object-rest-spread).
//!
//! ## References:
//! * Babel plugin implementation: <https://github.com/babel/babel/tree/v7.26.2/packages/babel-plugin-transform-object-rest-spread>
//! * Object rest/spread TC39 proposal: <https://github.com/tc39/proposal-object-rest-spread>

use std::mem;

use serde::Deserialize;

use oxc_allocator::{CloneIn, GetAddress, Vec as ArenaVec};
use oxc_ast::{ast::*, NONE};
use oxc_diagnostics::OxcDiagnostic;
use oxc_ecmascript::{BoundNames, ToJsString};
use oxc_semantic::{IsGlobalReference, ReferenceFlags, ScopeFlags, ScopeId, SymbolFlags};
use oxc_span::{GetSpan, SPAN};
use oxc_traverse::{Ancestor, MaybeBoundIdentifier, Traverse, TraverseCtx};

use crate::{common::helper_loader::Helper, TransformCtx};

#[derive(Debug, Default, Clone, Copy, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct ObjectRestSpreadOptions {
    pub loose: bool,

    pub use_built_ins: bool,
}

pub struct ObjectRestSpread<'a, 'ctx> {
    ctx: &'ctx TransformCtx<'a>,

    excluded_variabled_declarators: Vec<VariableDeclarator<'a>>,
}

impl<'a, 'ctx> ObjectRestSpread<'a, 'ctx> {
    pub fn new(options: ObjectRestSpreadOptions, ctx: &'ctx TransformCtx<'a>) -> Self {
        if options.loose {
            ctx.error(OxcDiagnostic::error(
                "Option `loose` is not implemented for object-rest-spread.",
            ));
        }
        if options.use_built_ins {
            ctx.error(OxcDiagnostic::error(
                "Option `useBuiltIns` is not implemented for object-rest-spread.",
            ));
        }
        if ctx.assumptions.object_rest_no_symbols {
            ctx.error(OxcDiagnostic::error(
                "Compiler assumption `objectRestNoSymbols` is not implemented for object-rest-spread.",
            ));
        }
        if ctx.assumptions.ignore_function_length {
            ctx.error(OxcDiagnostic::error(
                "Compiler assumption `ignoreFunctionLength` is not implemented for object-rest-spread.",
            ));
        }
        Self { ctx, excluded_variabled_declarators: vec![] }
    }
}

impl<'a, 'ctx> Traverse<'a> for ObjectRestSpread<'a, 'ctx> {
    fn exit_program(&mut self, _node: &mut Program<'a>, ctx: &mut TraverseCtx<'a>) {
        if !self.excluded_variabled_declarators.is_empty() {
            let declarators = ctx.ast.vec_from_iter(self.excluded_variabled_declarators.drain(..));
            let kind = VariableDeclarationKind::Const;
            let declaration = ctx.ast.alloc_variable_declaration(SPAN, kind, declarators, false);
            let statement = Statement::VariableDeclaration(declaration);
            self.ctx.top_level_statements.insert_statement(statement);
        }
    }

    fn enter_expression(&mut self, expr: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        self.transform_object_expression(expr, ctx);
        self.transform_assignment_expression(expr, ctx);
    }

    fn enter_arrow_function_expression(
        &mut self,
        arrow: &mut ArrowFunctionExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        Self::transform_arrow(arrow, ctx);
    }

    fn enter_function(&mut self, func: &mut Function<'a>, ctx: &mut TraverseCtx<'a>) {
        Self::transform_function(func, ctx);
    }

    // Includes `for (var {...x} = 1;;);`
    fn enter_variable_declaration(
        &mut self,
        decl: &mut VariableDeclaration<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        self.transform_variable_declaration(decl, ctx);
    }

    fn enter_catch_clause(&mut self, clause: &mut CatchClause<'a>, ctx: &mut TraverseCtx<'a>) {
        Self::transform_catch_clause(clause, ctx);
    }

    fn enter_for_in_statement(&mut self, stmt: &mut ForInStatement<'a>, ctx: &mut TraverseCtx<'a>) {
        let scope_id = stmt.scope_id();
        self.transform_for_statement_left(scope_id, &mut stmt.left, &mut stmt.body, ctx);
    }

    fn enter_for_of_statement(&mut self, stmt: &mut ForOfStatement<'a>, ctx: &mut TraverseCtx<'a>) {
        let scope_id = stmt.scope_id();
        self.transform_for_statement_left(scope_id, &mut stmt.left, &mut stmt.body, ctx);
    }
}

impl<'a, 'ctx> ObjectRestSpread<'a, 'ctx> {
    // Transform `({ x, ..y } = foo)`.
    // Transform `([{ x, ..y }] = foo)`.
    fn transform_assignment_expression(
        &mut self,
        expr: &mut Expression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let Expression::AssignmentExpression(assign_expr) = expr else {
            return;
        };
        // Allow `{...x} = {}` and `[{...x}] = []`.
        if !Self::has_nested_target_rest(&assign_expr.left) {
            return;
        };

        if !matches!(&assign_expr.left, AssignmentTarget::ObjectAssignmentTarget(t) if t.rest.is_some())
        {
            self.walk_and_replace_nested_object_target(expr, ctx);
            return;
        }

        let kind = VariableDeclarationKind::Var;
        let symbol_flags = kind_to_symbol_flags(kind);
        let scope_id = ctx.current_scope_id();
        let mut reference_builder =
            ReferenceBuilder::new(&mut assign_expr.right, symbol_flags, scope_id, true, ctx);
        let mut builder = DeclsBuilder::new(kind, symbol_flags, scope_id, ctx);

        if let Some(id) = reference_builder.binding.take() {
            builder.decls.push(ctx.ast.variable_declarator(SPAN, builder.kind, id, None, false));
        }

        let data = Self::walk_assignment_target(&mut assign_expr.left, &mut builder, ctx);

        // Insert `var _foo` before this statement.
        if !builder.decls.is_empty() {
            for node in ctx.ancestors() {
                if let Ancestor::ExpressionStatementExpression(decl) = node {
                    let kind = VariableDeclarationKind::Var;
                    let declaration =
                        ctx.ast.alloc_variable_declaration(SPAN, kind, builder.decls, false);
                    let statement = Statement::VariableDeclaration(declaration);
                    self.ctx.statement_injector.insert_before(&decl.address(), statement);
                    break;
                }
            }
        }

        // Make an sequence expression.
        let mut expressions = ctx.ast.vec();
        let op = AssignmentOperator::Assign;

        // Insert `_foo = rhs`
        if let Some(expr) = reference_builder.expr.take() {
            expressions.push(ctx.ast.expression_assignment(
                SPAN,
                op,
                reference_builder.maybe_bound_identifier.create_read_write_target(ctx),
                expr,
            ));
        }

        // Insert `{} = _foo`
        expressions.push(ctx.ast.expression_assignment(
            SPAN,
            op,
            ctx.ast.move_assignment_target(&mut assign_expr.left),
            reference_builder.create_read_expression(ctx),
        ));

        // Insert all `rest = _extends({}, (_objectDestructuringEmpty(_foo), _foo))`
        for datum in data {
            let (lhs, rhs) = datum.get_lhs_rhs(
                &mut reference_builder,
                &mut self.excluded_variabled_declarators,
                self.ctx,
                ctx,
            );
            if let BindingPatternOrAssignmentTarget::AssignmentTarget(lhs) = lhs {
                if let AssignmentTarget::AssignmentTargetIdentifier(ident) = &lhs {
                    ctx.symbols_mut()
                        .get_reference_mut(ident.reference_id())
                        .flags_mut()
                        .insert(ReferenceFlags::Read);
                }
                expressions.push(ctx.ast.expression_assignment(SPAN, op, lhs, rhs));
            }
        }

        // Insert final read `_foo`.
        expressions.push(reference_builder.create_read_expression(ctx));

        *expr = ctx.ast.expression_sequence(assign_expr.span, expressions);
    }

    fn walk_assignment_target(
        target: &mut AssignmentTarget<'a>,
        builder: &mut DeclsBuilder<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Vec<Datum<'a>> {
        match target {
            AssignmentTarget::ObjectAssignmentTarget(t) => {
                let mut data = vec![];
                for prop in t.properties.iter_mut() {
                    if let AssignmentTargetProperty::AssignmentTargetPropertyProperty(p) = prop {
                        data.extend(match &mut p.binding {
                            AssignmentTargetMaybeDefault::AssignmentTargetWithDefault(t) => {
                                Self::walk_assignment_target(&mut t.binding, builder, ctx)
                            }
                            _ => Self::walk_assignment_target(
                                p.binding.to_assignment_target_mut(),
                                builder,
                                ctx,
                            ),
                        })
                    }
                }
                if let Some(datum) = Self::transform_object_assignment_target(t, builder, ctx) {
                    data.push(datum);
                }
                data
            }
            _ => vec![],
        }
    }

    fn transform_object_assignment_target(
        object_assignment_target: &mut ObjectAssignmentTarget<'a>,
        builder: &mut DeclsBuilder<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Datum<'a>> {
        let rest = object_assignment_target.rest.take()?;
        let mut all_primitives = true;
        let keys =
            ctx.ast.vec_from_iter(object_assignment_target.properties.iter_mut().flat_map(|e| {
                match e {
                    AssignmentTargetProperty::AssignmentTargetPropertyIdentifier(ident) => {
                        let name = ident.binding.name.clone();
                        let expr = ctx.ast.expression_string_literal(SPAN, name);
                        Some(ArrayExpressionElement::from(expr))
                    }
                    AssignmentTargetProperty::AssignmentTargetPropertyProperty(p) => {
                        Self::transform_property_key(&mut p.name, builder, &mut all_primitives, ctx)
                    }
                }
            }));
        Some(Datum {
            lhs: BindingPatternOrAssignmentTarget::AssignmentTarget(rest.target),
            path: vec![],
            keys,
            has_no_properties: object_assignment_target.is_empty(),
            all_primitives,
        })
    }

    fn has_nested_target_rest(target: &AssignmentTarget<'a>) -> bool {
        match target {
            AssignmentTarget::ObjectAssignmentTarget(t) => {
                t.rest.is_some()
                    || t.properties.iter().any(|p| match p {
                        AssignmentTargetProperty::AssignmentTargetPropertyIdentifier(_) => false,
                        AssignmentTargetProperty::AssignmentTargetPropertyProperty(p) => {
                            match &p.binding {
                                AssignmentTargetMaybeDefault::AssignmentTargetWithDefault(t) => {
                                    Self::has_nested_target_rest(&t.binding)
                                }
                                _ => Self::has_nested_target_rest(p.binding.to_assignment_target()),
                            }
                        }
                    })
            }
            AssignmentTarget::ArrayAssignmentTarget(t) => {
                t.elements.iter().flatten().any(|e| match e {
                    AssignmentTargetMaybeDefault::AssignmentTargetWithDefault(t) => {
                        Self::has_nested_target_rest(&t.binding)
                    }
                    _ => Self::has_nested_target_rest(e.to_assignment_target()),
                }) || t.rest.as_ref().is_some_and(|r| Self::has_nested_target_rest(&r.target))
            }
            _ => false,
        }
    }

    fn walk_and_replace_nested_object_target(
        &mut self,
        expr: &mut Expression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let Expression::AssignmentExpression(assign_expr) = expr else {
            return;
        };
        let mut decls = vec![];
        let mut exprs = vec![];
        Self::resursive_walk_assignment_target(&mut assign_expr.left, &mut decls, &mut exprs, ctx);
        for node in ctx.ancestors() {
            if let Ancestor::ExpressionStatementExpression(decl) = node {
                let kind = VariableDeclarationKind::Var;
                let declaration = ctx.ast.alloc_variable_declaration(
                    SPAN,
                    kind,
                    ctx.ast.vec_from_iter(decls),
                    false,
                );
                let statement = Statement::VariableDeclaration(declaration);
                self.ctx.statement_injector.insert_before(&decl.address(), statement);
                break;
            }
        }
        let mut expressions = ctx.ast.vec1(ctx.ast.move_expression(expr));
        expressions.extend(exprs);
        *expr = ctx.ast.expression_sequence(SPAN, expressions);
    }

    fn resursive_walk_assignment_target(
        pat: &mut AssignmentTarget<'a>,
        decls: &mut Vec<VariableDeclarator<'a>>,
        exprs: &mut Vec<Expression<'a>>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        match pat {
            AssignmentTarget::ArrayAssignmentTarget(t) => {
                for e in t.elements.iter_mut().flatten() {
                    Self::recursive_walk_assignment_target_maybe_default(e, decls, exprs, ctx)
                }
            }
            AssignmentTarget::ObjectAssignmentTarget(t) => {
                for p in t.properties.iter_mut() {
                    if let AssignmentTargetProperty::AssignmentTargetPropertyProperty(e) = p {
                        Self::recursive_walk_assignment_target_maybe_default(
                            &mut e.binding,
                            decls,
                            exprs,
                            ctx,
                        )
                    }
                }
                if !t.rest.is_some() {
                    return;
                }
                let scope_id = ctx.scoping.current_scope_id();
                let flags = SymbolFlags::FunctionScopedVariable;
                let bound_identifier = ctx.generate_uid("ref", scope_id, flags);
                let id = bound_identifier.create_binding_pattern(ctx);
                let kind = VariableDeclarationKind::Var;
                decls.push(ctx.ast.variable_declarator(SPAN, kind, id, None, false));
                exprs.push(ctx.ast.expression_assignment(
                    SPAN,
                    AssignmentOperator::Assign,
                    ctx.ast.move_assignment_target(pat),
                    bound_identifier.create_read_expression(ctx),
                ));
                *pat = bound_identifier.create_spanned_write_target(SPAN, ctx);
            }
            _ => {}
        }
    }

    fn recursive_walk_assignment_target_maybe_default(
        target: &mut AssignmentTargetMaybeDefault<'a>,
        decls: &mut Vec<VariableDeclarator<'a>>,
        exprs: &mut Vec<Expression<'a>>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        match target {
            AssignmentTargetMaybeDefault::AssignmentTargetWithDefault(d) => {
                Self::resursive_walk_assignment_target(&mut d.binding, decls, exprs, ctx)
            }
            _ => Self::resursive_walk_assignment_target(
                target.to_assignment_target_mut(),
                decls,
                exprs,
                ctx,
            ),
        }
    }
}

// Transform `({ x, ..y })`.
impl<'a, 'ctx> ObjectRestSpread<'a, 'ctx> {
    fn transform_object_expression(&self, expr: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        let Expression::ObjectExpression(obj_expr) = expr else {
            return;
        };

        if obj_expr.properties.iter().all(|prop| !prop.is_spread()) {
            return;
        }

        let mut call_expr: Option<CallExpression<'a>> = None;
        let mut props = vec![];

        for prop in obj_expr.properties.drain(..) {
            if let ObjectPropertyKind::SpreadProperty(spread_prop) = prop {
                self.make_object_spread(&mut call_expr, &mut props, ctx);
                let arg = ctx.ast.move_expression(&mut spread_prop.unbox().argument);
                call_expr.as_mut().unwrap().arguments.push(Argument::from(arg));
            } else {
                props.push(prop);
            }
        }

        if !props.is_empty() {
            self.make_object_spread(&mut call_expr, &mut props, ctx);
        }

        *expr = Expression::CallExpression(ctx.ast.alloc(call_expr.unwrap()));
    }

    fn make_object_spread(
        &self,
        expr: &mut Option<CallExpression<'a>>,
        props: &mut Vec<ObjectPropertyKind<'a>>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let had_props = !props.is_empty();
        let obj = ctx.ast.expression_object(SPAN, ctx.ast.vec_from_iter(props.drain(..)), None);
        let new_expr = if let Some(call_expr) = expr.take() {
            let callee = self.ctx.helper_load(Helper::ObjectSpread2, ctx);
            let arg = Expression::CallExpression(ctx.ast.alloc(call_expr));
            let mut arguments = ctx.ast.vec1(Argument::from(arg));
            if had_props {
                let empty_object = ctx.ast.expression_object(SPAN, ctx.ast.vec(), None);
                arguments.push(Argument::from(empty_object));
                arguments.push(Argument::from(obj));
            }
            ctx.ast.call_expression(SPAN, callee, NONE, arguments, false)
        } else {
            let callee = self.ctx.helper_load(Helper::ObjectSpread2, ctx);
            let arguments = ctx.ast.vec1(Argument::from(obj));
            ctx.ast.call_expression(SPAN, callee, NONE, arguments, false)
        };
        expr.replace(new_expr);
    }
}

impl<'a, 'ctx> ObjectRestSpread<'a, 'ctx> {
    // Transform `function foo({...x}) {}`.
    fn transform_function(func: &mut Function<'a>, ctx: &mut TraverseCtx<'a>) {
        let scope_id = func.scope_id();
        let Some(body) = func.body.as_mut() else { return };
        for param in func.params.items.iter_mut() {
            if Self::has_nested_object_rest(&param.pattern) {
                Self::replace_rest_element(
                    VariableDeclarationKind::Var,
                    &mut param.pattern,
                    &mut body.statements,
                    scope_id,
                    ctx,
                );
            }
        }
    }

    // Transform `(...x) => {}`.
    fn transform_arrow(arrow: &mut ArrowFunctionExpression<'a>, ctx: &mut TraverseCtx<'a>) {
        let scope_id = arrow.scope_id();
        let mut replaced = false;
        for param in arrow.params.items.iter_mut() {
            if Self::has_nested_object_rest(&param.pattern) {
                Self::replace_rest_element(
                    VariableDeclarationKind::Var,
                    &mut param.pattern,
                    &mut arrow.body.statements,
                    scope_id,
                    ctx,
                );
                replaced = true;
            }
        }
        if replaced && arrow.expression {
            arrow.expression = false;
        }
    }

    // Transform `try {} catch (...x) {}`.
    fn transform_catch_clause(clause: &mut CatchClause<'a>, ctx: &mut TraverseCtx<'a>) {
        let Some(param) = &mut clause.param else { return };
        if Self::has_nested_object_rest(&param.pattern) {
            let scope_id = clause.body.scope_id();
            // Remove `SymbolFlags::CatchVariable`.
            param.pattern.bound_names(&mut |ident| {
                ctx.symbols_mut()
                    .get_flags_mut(ident.symbol_id())
                    .remove(SymbolFlags::CatchVariable);
            });
            Self::replace_rest_element(
                VariableDeclarationKind::Var,
                &mut param.pattern,
                &mut clause.body.body,
                scope_id,
                ctx,
            );
            // Add `SymbolFlags::CatchVariable`.
            param.pattern.bound_names(&mut |ident| {
                ctx.symbols_mut()
                    .get_flags_mut(ident.symbol_id())
                    .insert(SymbolFlags::CatchVariable);
            });
        }
    }

    // Transform `for (var { ...x };;)`.
    fn transform_variable_declaration_for_x_statement(
        &mut self,
        decl: &mut VariableDeclaration<'a>,
        body: &mut Statement<'a>,
        scope_id: ScopeId,
        ctx: &mut TraverseCtx<'a>,
    ) {
        for declarator in decl.declarations.iter_mut() {
            if Self::has_nested_object_rest(&declarator.id) {
                let new_scope_id = Self::try_replace_statement_with_block(body, scope_id, ctx);
                let Statement::BlockStatement(block) = body else {
                    unreachable!();
                };
                let mut bound_names = vec![];
                declarator.id.bound_names(&mut |ident| bound_names.push(ident.clone()));
                Self::replace_rest_element(
                    declarator.kind,
                    &mut declarator.id,
                    &mut block.body,
                    scope_id,
                    ctx,
                );
                // Move the bindings from the for init scope to scope of the loop body.
                for ident in bound_names {
                    ctx.symbols_mut().set_scope_id(ident.symbol_id(), new_scope_id);
                    ctx.scopes_mut().move_binding(scope_id, new_scope_id, ident.name.into());
                }
            }
        }
    }

    // If the assignment target contains an object rest,
    // create a reference and move the assignment target to the block body.
    // `for ({...x} in []);` `for ({...x} of []);`
    // `for ([{...x}] in []);` `for ([{...x}] of []);`
    fn transform_for_statement_left(
        &mut self,
        scope_id: ScopeId,
        left: &mut ForStatementLeft<'a>,
        body: &mut Statement<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        if let ForStatementLeft::VariableDeclaration(decl) = left {
            self.transform_variable_declaration_for_x_statement(decl, body, scope_id, ctx);
            return;
        }
        if !Self::has_nested_target_rest(left.to_assignment_target()) {
            return;
        }
        let target = left.to_assignment_target_mut();
        let assign_left = ctx.ast.move_assignment_target(target);
        let flags = SymbolFlags::FunctionScopedVariable;
        let bound_identifier = ctx.generate_uid("ref", scope_id, flags);
        let id = bound_identifier.create_binding_pattern(ctx);
        let kind = VariableDeclarationKind::Var;
        let declarations = ctx.ast.vec1(ctx.ast.variable_declarator(SPAN, kind, id, None, false));
        let decl = ctx.ast.alloc_variable_declaration(SPAN, kind, declarations, false);
        *left = ForStatementLeft::VariableDeclaration(decl);
        Self::try_replace_statement_with_block(body, scope_id, ctx);
        let Statement::BlockStatement(block) = body else {
            unreachable!();
        };
        let operator = AssignmentOperator::Assign;
        let right = bound_identifier.create_read_expression(ctx);
        let expr = ctx.ast.expression_assignment(SPAN, operator, assign_left, right);
        let stmt = ctx.ast.statement_expression(SPAN, expr);
        block.body.insert(0, stmt);
    }

    fn try_replace_statement_with_block(
        stmt: &mut Statement<'a>,
        parent_scope_id: ScopeId,
        ctx: &mut TraverseCtx<'a>,
    ) -> ScopeId {
        if let Statement::BlockStatement(block) = stmt {
            return block.scope_id();
        }
        let scope_id = ctx.create_child_scope(parent_scope_id, ScopeFlags::empty());
        let (span, stmts) = if let Statement::EmptyStatement(empty_stmt) = stmt {
            (empty_stmt.span, ctx.ast.vec())
        } else {
            let span = stmt.span();
            (span, ctx.ast.vec1(ctx.ast.move_statement(stmt)))
        };
        *stmt = Statement::BlockStatement(
            ctx.ast.alloc_block_statement_with_scope_id(span, stmts, scope_id),
        );
        scope_id
    }

    /// Recursively check for object rest.
    fn has_nested_object_rest(pat: &BindingPattern<'a>) -> bool {
        match &pat.kind {
            BindingPatternKind::ObjectPattern(pat) => {
                pat.rest.is_some()
                    || pat.properties.iter().any(|p| Self::has_nested_object_rest(&p.value))
            }
            BindingPatternKind::ArrayPattern(pat) => {
                pat.elements
                    .iter()
                    .any(|e| e.as_ref().is_some_and(|p| Self::has_nested_object_rest(p)))
                    || pat.rest.as_ref().is_some_and(|e| Self::has_nested_object_rest(&e.argument))
            }
            BindingPatternKind::AssignmentPattern(pat) => Self::has_nested_object_rest(&pat.left),
            BindingPatternKind::BindingIdentifier(_) => false,
        }
    }

    /// Move the binding to the body if it contains an object rest.
    /// The object pattern will be transform by `transform_object_pattern` afterwards.
    fn replace_rest_element(
        kind: VariableDeclarationKind,
        pattern: &mut BindingPattern<'a>,
        body: &mut ArenaVec<'a, Statement<'a>>,
        scope_id: ScopeId,
        ctx: &mut TraverseCtx<'a>,
    ) {
        match &mut pattern.kind {
            // Replace the object pattern, no need to walk the object properties.
            BindingPatternKind::ObjectPattern(_) => {
                Self::replace_object_pattern_and_insert_into_block_body(
                    kind, pattern, body, scope_id, ctx,
                );
            }
            BindingPatternKind::AssignmentPattern(pat) => {
                Self::replace_object_pattern_and_insert_into_block_body(
                    kind,
                    &mut pat.left,
                    body,
                    scope_id,
                    ctx,
                );
            }
            // Or replace all occurances of object pattern inside array pattern.
            BindingPatternKind::ArrayPattern(pat) => {
                for element in pat.elements.iter_mut().flatten() {
                    Self::replace_rest_element(kind, element, body, scope_id, ctx);
                }
                if let Some(element) = &mut pat.rest {
                    Self::replace_rest_element(kind, &mut element.argument, body, scope_id, ctx);
                }
            }
            BindingPatternKind::BindingIdentifier(_) => {}
        }
    }

    // Add `let {...x} = _ref` to body.
    fn replace_object_pattern_and_insert_into_block_body(
        kind: VariableDeclarationKind,
        pat: &mut BindingPattern<'a>,
        body: &mut ArenaVec<'a, Statement<'a>>,
        scope_id: ScopeId,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let decl = Self::create_temporary_reference_for_binding(kind, pat, scope_id, ctx);
        body.insert(0, Statement::VariableDeclaration(ctx.ast.alloc(decl)));
    }

    fn create_temporary_reference_for_binding(
        kind: VariableDeclarationKind,
        pat: &mut BindingPattern<'a>,
        scope_id: ScopeId,
        ctx: &mut TraverseCtx<'a>,
    ) -> VariableDeclaration<'a> {
        let bound_identifier = ctx.generate_uid("ref", scope_id, kind_to_symbol_flags(kind));
        let kind = VariableDeclarationKind::Let;
        let id = mem::replace(pat, bound_identifier.create_binding_pattern(ctx));
        let init = bound_identifier.create_read_expression(ctx);
        let declarations =
            ctx.ast.vec1(ctx.ast.variable_declarator(SPAN, kind, id, Some(init), false));
        let decl = ctx.ast.variable_declaration(SPAN, kind, declarations, false);
        decl.bound_names(&mut |ident| {
            *ctx.symbols_mut().get_flags_mut(ident.symbol_id()) = SymbolFlags::BlockScopedVariable;
        });
        decl
    }
}

// Transform `let { x, ..y } = foo`.
// Transform `let [{ x, ..y }] = foo`.
impl<'a, 'ctx> ObjectRestSpread<'a, 'ctx> {
    fn transform_variable_declaration(
        &mut self,
        decl: &mut VariableDeclaration<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let mut new_decls = vec![];
        for (i, variable_declarator) in decl.declarations.iter_mut().enumerate() {
            if variable_declarator.init.is_some()
                && Self::has_nested_object_rest(&variable_declarator.id)
            {
                let decls = self.transform_variable_declarator(variable_declarator, ctx);
                new_decls.push((i, decls));
            }
        }
        // TODO: performance can be improved by not shuffling the vec multiple times.
        for (i, decls) in new_decls {
            decl.declarations.splice(i..=i, decls);
        }
    }

    // The algorithm depth searches for object rest,
    // and then creates a ref for the object, subsequent visit will then transform this object rest.
    // Transforms:
    // * `var {...a} = foo` -> `var a = _extends({}, (_objectDestructuringEmpty(foo), foo));`
    // * `var [{x: {y: {...a}}}] = z` -> `var [{x: {y: _ref}}] = z, {...a} = _ref`
    fn transform_variable_declarator(
        &mut self,
        decl: &mut VariableDeclarator<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> ArenaVec<'a, VariableDeclarator<'a>> {
        // It is syntax error or inside for loop if missing initializer in destructuring pattern.
        let init = decl.init.as_mut().unwrap();

        // Use the scope of the identifier, scope is different for
        // `for (var {...x} = {};;);` and `for (let {...x} = {};;);`
        // TODO: improve this by getting the value only once.
        let mut scope_id = ctx.current_scope_id();
        let mut symbol_flags = kind_to_symbol_flags(decl.kind);
        let symbols = ctx.symbols();
        decl.id.bound_names(&mut |ident| {
            let symbol_id = ident.symbol_id();
            scope_id = symbols.get_scope_id(symbol_id);
            symbol_flags.insert(symbols.get_flags(symbol_id));
        });

        let mut builder = DeclsBuilder::new(decl.kind, symbol_flags, scope_id, ctx);

        let mut reference_builder = ReferenceBuilder::new(init, symbol_flags, scope_id, false, ctx);

        // Add `_foo = foo()`
        if let Some(id) = reference_builder.binding.take() {
            let decl = ctx.ast.variable_declarator(
                SPAN,
                builder.kind,
                id,
                Some(reference_builder.create_read_expression(ctx)),
                false,
            );
            builder.decls.push(decl);
        }

        let index = builder.decls.len();

        match &mut decl.id.kind {
            // Example: `let { x, ...rest } = foo();`.
            BindingPatternKind::ObjectPattern(pat) => {
                // Walk the properties that may contain a nested rest spread.

                if let Some(rest) = pat.rest.take() {
                    for p in pat.properties.iter_mut() {
                        self.recursive_walk_binding_pattern(&mut p.value, &mut builder, ctx);
                    }
                    // Transform the object pattern with a rest pattern.
                    let lhs =
                        BindingPatternOrAssignmentTarget::BindingPattern(rest.unbox().argument);
                    let mut all_primitives = true;
                    // Create the access keys.
                    // `let { a, b, ...c } = foo` -> `["a", "b"]`
                    let keys = ctx.ast.vec_from_iter(pat.properties.iter_mut().flat_map(
                        |binding_property| {
                            Self::transform_property_key(
                                &mut binding_property.key,
                                &mut builder,
                                &mut all_primitives,
                                ctx,
                            )
                        },
                    ));
                    // Remove the object pattern that became empty.
                    // object_pattern.properties.retain(|binding_property| match &binding_property.value.kind {
                    // BindingPatternKind::ObjectPattern(object_pattern) => {
                    // !object_pattern.properties.is_empty()
                    // }
                    // _ => true,
                    // });
                    let datum = Datum {
                        lhs,
                        path: vec![],
                        keys,
                        has_no_properties: pat.properties.is_empty(),
                        all_primitives,
                    };

                    // Add `{ x } = foo`.
                    if !pat.properties.is_empty() {
                        let binding_pattern_kind = ctx.ast.binding_pattern_kind_object_pattern(
                            pat.span,
                            ctx.ast.move_vec(&mut pat.properties),
                            NONE,
                        );
                        builder.decls.push(ctx.ast.variable_declarator(
                            decl.span,
                            decl.kind,
                            ctx.ast.binding_pattern(binding_pattern_kind, NONE, false),
                            Some(reference_builder.create_read_expression(ctx)),
                            false,
                        ));
                    };
                    // Add `rest = babelHelpers.extends({}, (babelHelpers.objectDestructuringEmpty(_foo), _foo))`.
                    // Or `rest = babelHelpers.objectWithoutProperties(_foo, ["x"])`.
                    let (lhs, rhs) = datum.get_lhs_rhs(
                        &mut reference_builder,
                        &mut self.excluded_variabled_declarators,
                        self.ctx,
                        ctx,
                    );
                    if let BindingPatternOrAssignmentTarget::BindingPattern(lhs) = lhs {
                        let decl = ctx.ast.variable_declarator(
                            lhs.span(),
                            decl.kind,
                            lhs,
                            Some(rhs),
                            false,
                        );
                        builder.decls.push(decl);
                    }
                }

                // let mut binding_pattern_kind =
                // ctx.ast.binding_pattern_kind_object_pattern(decl.span, ctx.ast.vec(), NONE);
                // mem::swap(&mut binding_pattern_kind, &mut decl.id.kind);
                // let decl = ctx.ast.variable_declarator(
                // decl.span,
                // decl.kind,
                // ctx.ast.binding_pattern(binding_pattern_kind, NONE, false),
                // Some(ctx.ast.move_expression(init)),
                // false,
                // );
                // builder.decls.insert(index, decl);
            }
            _ => {
                self.recursive_walk_binding_pattern(&mut decl.id, &mut builder, ctx);
            }
        }

        // Insert the original declarator by copying its data out.
        let mut binding_pattern_kind =
            ctx.ast.binding_pattern_kind_object_pattern(decl.span, ctx.ast.vec(), NONE);
        mem::swap(&mut binding_pattern_kind, &mut decl.id.kind);
        let decl = ctx.ast.variable_declarator(
            decl.span,
            decl.kind,
            ctx.ast.binding_pattern(binding_pattern_kind, NONE, false),
            Some(reference_builder.create_read_expression(ctx)),
            false,
        );
        builder.decls.insert(index, decl);

        builder.decls
    }

    fn recursive_walk_binding_pattern(
        &mut self,
        pat: &mut BindingPattern<'a>,
        builder: &mut DeclsBuilder<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        match &mut pat.kind {
            BindingPatternKind::BindingIdentifier(_) => {}
            BindingPatternKind::ArrayPattern(array_pat) => {
                for p in array_pat.elements.iter_mut().flatten() {
                    self.recursive_walk_binding_pattern(p, builder, ctx);
                }
            }
            BindingPatternKind::AssignmentPattern(assign_pat) => {
                self.recursive_walk_binding_pattern(&mut assign_pat.left, builder, ctx);
            }
            BindingPatternKind::ObjectPattern(p) => {
                for p in p.properties.iter_mut() {
                    self.recursive_walk_binding_pattern(&mut p.value, builder, ctx);
                }
                if p.rest.is_some() {
                    let bound_identifier =
                        ctx.generate_uid("ref", builder.scope_id, builder.symbol_flags);
                    let id = mem::replace(pat, bound_identifier.create_binding_pattern(ctx));
                    let init = bound_identifier.create_read_expression(ctx);
                    let mut decl =
                        ctx.ast.variable_declarator(SPAN, builder.kind, id, Some(init), false);
                    builder.decls.extend(self.transform_variable_declarator(&mut decl, ctx));
                    return;
                }
            }
        }
    }

    fn transform_property_key(
        key: &mut PropertyKey<'a>,
        builder: &mut DeclsBuilder<'a>,
        all_primitives: &mut bool,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<ArrayExpressionElement<'a>> {
        match key {
            // `let { a, ... rest }`
            PropertyKey::StaticIdentifier(ident) => {
                let name = ident.name.clone();
                let expr = ctx.ast.expression_string_literal(ident.span, name);
                Some(ArrayExpressionElement::from(expr))
            }
            // `let { 'a', ... rest }`
            // `let { ['a'], ... rest }`
            PropertyKey::StringLiteral(lit) => {
                let name = lit.value.clone();
                let expr = ctx.ast.expression_string_literal(lit.span, name.clone());
                Some(ArrayExpressionElement::from(expr))
            }
            // `let { [`a`], ... rest }`
            PropertyKey::TemplateLiteral(lit) if lit.is_no_substitution_template() => {
                let expr = Expression::TemplateLiteral(lit.clone_in(ctx.ast.allocator));
                Some(ArrayExpressionElement::from(expr))
            }
            PropertyKey::PrivateIdentifier(_) => {
                /* syntax error */
                None
            }
            key => {
                let Some(expr) = key.as_expression_mut() else { return None };
                // `let { [1], ... rest }`
                if expr.is_literal() {
                    let span = expr.span();
                    let s = expr.to_js_string().unwrap();
                    let expr = ctx.ast.expression_string_literal(span, s);
                    return Some(ArrayExpressionElement::from(expr));
                }
                *all_primitives = false;
                if let Expression::Identifier(ident) = expr {
                    if !ident.is_global_reference(ctx.symbols()) {
                        let expr = MaybeBoundIdentifier::from_identifier_reference(ident, ctx)
                            .create_read_expression(ctx);
                        return Some(ArrayExpressionElement::from(expr));
                    }
                }
                let bound_identifier =
                    ctx.generate_uid_based_on_node(expr, builder.scope_id, builder.symbol_flags);
                let p = bound_identifier.create_binding_pattern(ctx);
                let mut lhs = bound_identifier.create_read_expression(ctx);
                mem::swap(&mut lhs, expr);
                builder.decls.push(ctx.ast.variable_declarator(
                    SPAN,
                    builder.kind,
                    p,
                    Some(lhs),
                    false,
                ));
                Some(ArrayExpressionElement::from(bound_identifier.create_read_expression(ctx)))
            }
        }
    }
}

struct DeclsBuilder<'a> {
    decls: ArenaVec<'a, VariableDeclarator<'a>>,
    kind: VariableDeclarationKind,
    symbol_flags: SymbolFlags,
    scope_id: ScopeId,
}

impl<'a> DeclsBuilder<'a> {
    fn new(
        kind: VariableDeclarationKind,
        symbol_flags: SymbolFlags,
        scope_id: ScopeId,
        ctx: &mut TraverseCtx<'a>,
    ) -> Self {
        Self { decls: ctx.ast.vec(), kind, symbol_flags, scope_id }
    }
}

fn kind_to_symbol_flags(kind: VariableDeclarationKind) -> SymbolFlags {
    match kind {
        VariableDeclarationKind::Var => SymbolFlags::FunctionScopedVariable,
        VariableDeclarationKind::Let
        | VariableDeclarationKind::Using
        | VariableDeclarationKind::AwaitUsing => SymbolFlags::BlockScopedVariable,
        VariableDeclarationKind::Const => {
            SymbolFlags::BlockScopedVariable | SymbolFlags::ConstVariable
        }
    }
}

#[derive(Debug)]
enum BindingPatternOrAssignmentTarget<'a> {
    BindingPattern(BindingPattern<'a>),
    AssignmentTarget(AssignmentTarget<'a>),
}

#[derive(Debug)]
enum KeyPath<'a> {
    PropertyKey(PropertyKey<'a>),
    MaybeBoundIdentifier(MaybeBoundIdentifier<'a>),
}

/// Datum containing path to a object rest.
#[derive(Debug)]
struct Datum<'a> {
    lhs: BindingPatternOrAssignmentTarget<'a>,
    path: Vec<KeyPath<'a>>,
    keys: ArenaVec<'a, ArrayExpressionElement<'a>>,
    has_no_properties: bool,
    all_primitives: bool,
}

impl<'a> Datum<'a> {
    fn create_arg(
        &self,
        reference_builder: &mut ReferenceBuilder<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Expression<'a> {
        let mut arg = reference_builder.create_read_expression(ctx);
        // For nested case, `let { x: { [y]: { ...z } } } = foo`
        // Create rhs access `foo.x[y]`.
        for key_path in self.path.iter().rev() {
            match key_path {
                KeyPath::MaybeBoundIdentifier(ident) => {
                    let expr = ident.create_read_expression(ctx);
                    arg = Expression::ComputedMemberExpression(
                        ctx.ast.alloc_computed_member_expression(SPAN, arg, expr, false),
                    );
                }
                KeyPath::PropertyKey(property_key) => match property_key {
                    PropertyKey::StaticIdentifier(ident) => {
                        let property = ctx.ast.identifier_name(ident.span, ident.name.clone());
                        arg = Expression::StaticMemberExpression(
                            ctx.ast.alloc_static_member_expression(SPAN, arg, property, false),
                        );
                    }
                    PropertyKey::PrivateIdentifier(_) => continue, // syntax error
                    _ => {
                        let expr = property_key.clone_in(ctx.ast.allocator).into_expression();
                        arg = Expression::ComputedMemberExpression(
                            ctx.ast.alloc_computed_member_expression(SPAN, arg, expr, false),
                        );
                    }
                },
            }
            // PropertyKey::Identifier(ident) => {
            // let expr = MaybeBoundIdentifier::from_identifier_reference(ident, ctx)
            // .create_read_expression(ctx);
            // arg = Expression::ComputedMemberExpression(
            // ctx.ast.alloc_computed_member_expression(span, arg, expr, false),
            // );
            // }
            // PropertyKey::PrivateIdentifier(_) => continue, // syntax error
            // _ => {
            // let expr = property_key.clone_in(ctx.ast.allocator).into_expression();
            // arg = Expression::ComputedMemberExpression(
            // ctx.ast.alloc_computed_member_expression(span, arg, expr, false),
            // );
            // }
        }
        arg
    }

    fn get_lhs_rhs(
        self,
        reference_builder: &mut ReferenceBuilder<'a>,
        excluded_variabled_declarators: &mut Vec<VariableDeclarator<'a>>,
        transform_ctx: &TransformCtx<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> (BindingPatternOrAssignmentTarget<'a>, Expression<'a>) {
        let rhs = if self.has_no_properties {
            // The `ObjectDestructuringEmpty` function throws a type error when destructuring null.
            // `function _objectDestructuringEmpty(t) { if (null == t) throw new TypeError("Cannot destructure " + t); }`
            let mut arguments = ctx.ast.vec();
            // Add `{}`.
            arguments.push(Argument::ObjectExpression(ctx.ast.alloc_object_expression(
                SPAN,
                ctx.ast.vec(),
                None,
            )));
            // Add `(_objectDestructuringEmpty(b), b);`
            arguments.push(Argument::SequenceExpression(ctx.ast.alloc_sequence_expression(
                SPAN,
                {
                    let mut sequence = ctx.ast.vec();
                    sequence.push(transform_ctx.helper_call_expr(
                        Helper::ObjectDestructuringEmpty,
                        SPAN,
                        ctx.ast.vec1(Argument::from(self.create_arg(reference_builder, ctx))),
                        ctx,
                    ));
                    sequence.push(self.create_arg(reference_builder, ctx));
                    sequence
                },
            )));
            transform_ctx.helper_call_expr(Helper::Extends, SPAN, arguments, ctx)
        } else {
            // / `let { a, b, ...c } = z` -> _objectWithoutProperties(_z, ["a", "b"]);
            // / `_objectWithoutProperties(_z, ["a", "b"])`
            let mut arguments =
                ctx.ast.vec1(Argument::from(self.create_arg(reference_builder, ctx)));
            let key_expression = ctx.ast.expression_array(SPAN, self.keys, None);

            let key_expression = if self.all_primitives
                && ctx.scoping.current_scope_id() != ctx.scopes().root_scope_id()
            {
                // Move the key_expression to the root scope.
                let bound_identifier = ctx.generate_uid_in_root_scope(
                    "excluded",
                    SymbolFlags::BlockScopedVariable | SymbolFlags::ConstVariable,
                );
                let kind = VariableDeclarationKind::Const;
                let declarator = ctx.ast.variable_declarator(
                    SPAN,
                    kind,
                    bound_identifier.create_binding_pattern(ctx),
                    Some(key_expression),
                    false,
                );
                excluded_variabled_declarators.push(declarator);
                bound_identifier.create_read_expression(ctx)
            } else if !self.all_primitives {
                // map to `toPropertyKey` to handle the possible non-string values
                // `[_ref].map(babelHelpers.toPropertyKey))`
                let property = ctx.ast.identifier_name(SPAN, "map");
                let callee = Expression::StaticMemberExpression(
                    ctx.ast.alloc_static_member_expression(SPAN, key_expression, property, false),
                );
                let arguments = ctx
                    .ast
                    .vec1(Argument::from(transform_ctx.helper_load(Helper::ToPropertyKey, ctx)));
                ctx.ast.expression_call(SPAN, callee, NONE, arguments, false)
            } else {
                key_expression
            };
            arguments.push(Argument::from(key_expression));
            transform_ctx.helper_call_expr(Helper::ObjectWithoutProperties, SPAN, arguments, ctx)
        };
        (self.lhs, rhs)
    }
}

#[derive(Debug)]
struct ReferenceBuilder<'a> {
    expr: Option<Expression<'a>>,
    binding: Option<BindingPattern<'a>>,
    maybe_bound_identifier: MaybeBoundIdentifier<'a>,
}

impl<'a> ReferenceBuilder<'a> {
    fn new(
        expr: &mut Expression<'a>,
        symbol_flags: SymbolFlags,
        scope_id: ScopeId,
        force_create_binding: bool,
        ctx: &mut TraverseCtx<'a>,
    ) -> Self {
        let expr = ctx.ast.move_expression(expr);
        let binding;
        let maybe_bound_identifier;
        match &expr {
            Expression::Identifier(ident) if !force_create_binding => {
                binding = None;
                maybe_bound_identifier = MaybeBoundIdentifier::from_identifier_reference(ident, ctx)
            }
            expr => {
                let bound_identifier = ctx.generate_uid_based_on_node(expr, scope_id, symbol_flags);
                binding = Some(bound_identifier.create_binding_pattern(ctx));
                maybe_bound_identifier = bound_identifier.to_maybe_bound_identifier();
            }
        }
        Self { expr: Some(expr), binding, maybe_bound_identifier }
    }

    fn create_read_expression(&mut self, ctx: &mut TraverseCtx<'a>) -> Expression<'a> {
        self.expr.take().unwrap_or_else(|| self.maybe_bound_identifier.create_read_expression(ctx))
    }
}
