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

struct ASTFunctionCallExprNode : public ASTNode {
    // This field represents the expression that holds the method that is being
    // called.
    NodePtr callee;

    // This field represents the list of arguments given to the function.
    std::vector<NodePtr> args;

    ASTFunctionCallExprNode(NodePtr callee, std::vector<NodePtr> args,
                            size_t offset, int size)
        : ASTNode{offset, size}, callee{std::move(callee)},
          args{std::move(args)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTUnaryOpExprNode : public ASTNode {
// We need to use the macro trick here to get printable operator names.
#define UNARY_OPERATORS(F)                                                     \
    F(UnaryNot)                                                                \
    F(UnaryMinus)

#define F(x) x,
    enum class Operator { UNARY_OPERATORS(F) };
#undef F

#define F(x) #x,
    static inline const char *operator_names[] = {UNARY_OPERATORS(F)};
#undef F
#undef UNARY_OPERATORS

    // This field defines the operator that is appled on the expression.
    Operator op;

    // This field defines the expression that the operator is applied on.
    NodePtr expr;

    ASTUnaryOpExprNode(Operator op, NodePtr expr, size_t offset, int size)
        : ASTNode{offset, size}, op{op}, expr{std::move(expr)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTBinaryOpExprNode : public ASTNode {
// We need to use the macro trick here to get printable operator names.
#define BINARY_OPERATORS(F)                                                    \
    F(BinaryPlus)                                                              \
    F(BinaryMinus)                                                             \
    F(BinaryAsterisk)                                                          \
    F(BinarySlash)                                                             \
    F(BinarySlashSlash)                                                        \
    F(BinaryPercent)                                                           \
    F(BinaryEqualsEquals)                                                      \
    F(BinaryExclamationEquals)                                                 \
    F(BinaryLessEquals)                                                        \
    F(BinaryLess)                                                              \
    F(BinaryGreaterEquals)                                                     \
    F(BinaryGreater)                                                           \
    F(BinaryIs)                                                                \
    F(BinaryLogicalAnd)                                                        \
    F(BinaryLogicalOr)

#define F(x) x,
    enum class Operator { BINARY_OPERATORS(F) };
#undef F

#define F(x) #x,
    static inline const char *operator_names[] = {BINARY_OPERATORS(F)};
#undef F
#undef BINARY_OPERATORS

    // This field holds the operator for the expression.
    Operator op;

    // These fields hold the LHS and RHS of the expression.
    NodePtr lhs, rhs;

    ASTBinaryOpExprNode(Operator op, NodePtr lhs, NodePtr rhs, size_t offset,
                        int size)
        : ASTNode{offset, size}, op{op}, lhs{std::move(lhs)},
          rhs{std::move(rhs)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

} // namespace chocopyc::Parse

#endif