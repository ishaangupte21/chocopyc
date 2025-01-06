/*
    This file defines the AST nodes for Chocopy expressions.
*/

#ifndef CHOCOPYC_PARSE_ASTEXPRNODE_H
#define CHOCOPYC_PARSE_ASTEXPRNODE_H

#include "parse/ASTNode.h"

#include <vector>

namespace chocopyc::Parse {
struct ASTLiteralExprNode : public ASTNode {
// We need to use the macro trick here to provide printing names for literal
// types.
#define LITERAL_KINDS(F)                                                       \
    F(Int)                                                                     \
    F(Float)                                                                   \
    F(String)                                                                  \
    F(Boolean)

#define F(x) x,
    enum class LiteralKind { LITERAL_KINDS(F) };
#undef F

#define F(x) #x,
    static inline const char *literal_names[] = {LITERAL_KINDS(F)};
#undef F
#undef LITERAL_KINDS

    // This field holds the enum value that determines the type of this literal.
    LiteralKind kind;

    // If this is a boolean literal, we will store the value of the literal
    // here.
    bool boolean_literal_value;

    ASTLiteralExprNode(LiteralKind kind, size_t offset, int size)
        : ASTNode{offset, size}, kind{kind} {}

    ASTLiteralExprNode(bool boolean_literal_value, size_t offset, int size)
        : ASTNode{offset, size}, kind{LiteralKind::Boolean},
          boolean_literal_value{boolean_literal_value} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTNameExprNode : public ASTNode {
    // We don't have any special fields here.
    ASTNameExprNode(size_t offset, int size) : ASTNode{offset, size} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTListExprNode : public ASTNode {
    // This field represents the expressions that are actually in this list.
    std::vector<NodePtr> contents;

    ASTListExprNode(std::vector<NodePtr> contents, size_t offset, int size)
        : ASTNode{offset, size}, contents{std::move(contents)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTParenExprNode : public ASTNode {
    // This field represents the expression enclosed by the parentheses.
    NodePtr expr;

    ASTParenExprNode(NodePtr expr, size_t offset, int size)
        : ASTNode{offset, size}, expr{std::move(expr)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTAttributeRefExprNode : public ASTNode {
    // This field represents the left hand side of the operator. It is the
    // object that holds the field being referenced.
    NodePtr lhs;

    // This field represents the right hand side of the operator. It is the name
    // of the referenced field.
    std::unique_ptr<ASTNameExprNode> rhs;

    ASTAttributeRefExprNode(NodePtr lhs, std::unique_ptr<ASTNameExprNode> rhs,
                            size_t offset, int size)
        : ASTNode{offset, size}, lhs{std::move(lhs)}, rhs{std::move(rhs)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTIndexingExprNode : public ASTNode {
    // This field represents the expression that is being indexed into.
    NodePtr indexee;

    // This field represents the expression that makes the index.
    NodePtr index;

    ASTIndexingExprNode(NodePtr indexee, NodePtr index, size_t offset, int size)
        : ASTNode{offset, size}, indexee{std::move(indexee)},
          index{std::move(index)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTFunctionCallExpr : public ASTNode {
    // This field represents the expression that holds the method that is being
    // called.
    NodePtr callee;

    // This field represents the list of arguments given to the function.
    std::vector<NodePtr> args;

    ASTFunctionCallExpr(NodePtr callee, std::vector<NodePtr> args,
                        size_t offset, int size)
        : ASTNode{offset, size}, callee{std::move(callee)},
          args{std::move(args)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

} // namespace chocopyc::Parse

#endif