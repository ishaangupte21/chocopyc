/*
    This file provides method implementations for pretty printing AST nodes.
*/

#include "parse/ASTExprNode.h"

#include <print>

namespace chocopyc::Parse {
// Pretty printing for literal expression nodes.
auto ASTLiteralExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTLiteralExprNode");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "kind: {}", literal_names[static_cast<int>(kind)]);

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "offset: {}", offset);

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "size: {}", size);

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for name expression nodes.
auto ASTNameExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTNameExprNode");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "offset: {}", offset);

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "size: {}", size);

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for list expression nodes.
auto ASTListExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTListExprNode");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "[");

    for (auto &expr : contents) {
        expr->pretty_print(out, level + 1);
    }

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "]");

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for expressions enclosed in parentheses.
auto ASTParenExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTParenExprNode");

    // Print the enclosed expression
    expr->pretty_print(out, level + 1);

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for attribute reference expressions.
auto ASTAttributeRefExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTAttributeRefExprNode");

    // Print the LHS
    lhs->pretty_print(out, level + 1);

    // Print the RHS
    rhs->pretty_print(out, level + 1);

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for indexing expressions
auto ASTIndexingExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTIndexingExprNode");

    // Print the indexee
    indexee->pretty_print(out, level + 1);

    // Print the index
    index->pretty_print(out, level + 1);

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for function call expressions.
auto ASTFunctionCallExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTFunctionCallExpr");

    // Print the callee
    callee->pretty_print(out, level + 1);

    // Print the args
    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "[");

    for (auto &expr : args) {
        expr->pretty_print(out, level + 1);
    }

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "]");

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for unary operator expression.
auto ASTUnaryOpExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTUnaryOpExprNode");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "operator: {}", operator_names[static_cast<int>(op)]);

    // Print the expression
    expr->pretty_print(out, level + 1);

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for binary operator expression.
auto ASTBinaryOpExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTBinaryOpExprNode");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "operator: {}", operator_names[static_cast<int>(op)]);

    // Print the LHS
    lhs->pretty_print(out, level + 1);

    // Print the RHS
    rhs->pretty_print(out, level + 1);

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

// Pretty printing for ternary operator expression.
auto ASTTernaryExprNode::pretty_print(FILE *out, int level) -> void {
    // First, we must set the indentation level for the name.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "{{");

    for (int i = 0; i < (level + 1) * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "name: ASTTernaryExprNode");

    // Print the condition
    condition->pretty_print(out, level + 1);

    // Print the 'then' expr
    then_expr->pretty_print(out, level + 1);

    // Print the 'else' expr
    else_expr->pretty_print(out, level + 1);

    // First, we must print the closing curly brace.
    for (int i = 0; i < level * 4; ++i) {
        std::print(out, " ");
    }
    std::println(out, "}}");
}

} // namespace chocopyc::Parse