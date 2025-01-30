/*
    This file defines the AST nodes for Chocopy statements.
*/

#ifndef CHOCOPYC_PARSE_ASTSTMTNODE_H
#define CHOCOPYC_PARSE_ASTSTMTNODE_H

#include "parse/ASTExprNode.h"
#include "parse/ASTNode.h"

namespace chocopyc::Parse {
struct ASTPassStmtNode : public ASTNode {
    // This statement doesn't contain any data, so we don't need anything
    // besides the location info.
    ASTPassStmtNode(size_t offset, int size) : ASTNode{offset, size} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTReturnStmtNode : public ASTNode {
    // This field represents the expression to be returned from a function. It
    // will be null in the case of a void return type.
    NodePtr return_val;

    ASTReturnStmtNode(NodePtr return_val, size_t offset, int size)
        : ASTNode{offset, size}, return_val{std::move(return_val)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTAssignmentStmtNode : public ASTNode {
// We need to use the macro trick here to provide for target
// types.
#define TARGET_KINDS(F)                                                        \
    F(Name)                                                                    \
    F(AttributeRef)                                                            \
    F(IndexingExpr)

#define F(x) x,
    enum class TargetKind { TARGET_KINDS(F) };
#undef F

#define F(x) #x,
    static inline const char *target_names[] = {TARGET_KINDS(F)};
#undef F
#undef TARGET_KINDS

    // This field represents the target expression for this assignment.
    NodePtr target_expr;

    // This field represents the kind of expression the target is.
    TargetKind target_kind;

    // This field represents the RHS, which is the value to be assigned.
    NodePtr rhs;

    ASTAssignmentStmtNode(NodePtr target_expr, TargetKind target_kind,
                          NodePtr rhs, size_t offset, int size)
        : ASTNode{offset, size}, target_expr{std::move(target_expr)},
          target_kind{target_kind}, rhs{std::move(rhs)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTExprStmtNode : public ASTNode {
    // This field represents the expression created by this statement.
    NodePtr expr;

    ASTExprStmtNode(NodePtr expr, size_t offset, int size)
        : ASTNode{offset, size}, expr{std::move(expr)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTStmtBlockNode : public ASTNode {
    // This field represents the list of statements held by this block.
    std::vector<NodePtr> stmts;

    ASTStmtBlockNode(std::vector<NodePtr> stmts, size_t offset, int size)
        : ASTNode{offset, size}, stmts{std::move(stmts)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTElIfStmtNode : public ASTNode {
    // This field represents the condition for the 'elif' block.
    NodePtr condition;

    // This field represents the block that goes with this clause.
    NodePtr block;

    ASTElIfStmtNode(NodePtr condition, NodePtr block, size_t offset, int size)
        : ASTNode{offset, size}, condition{std::move(condition)},
          block{std::move(block)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTIfStmtNode : public ASTNode {
    // This field represents the condition of the initial 'if' clause.
    NodePtr condition;

    // This field represents the 'then' block.
    NodePtr then_block;

    // This field represents the list of all 'elif' blocks.
    std::vector<std::unique_ptr<ASTElIfStmtNode>> elif_blocks;

    // This field represents the 'else' block.
    NodePtr else_block;

    ASTIfStmtNode(NodePtr condition, NodePtr then_block,
                  std::vector<std::unique_ptr<ASTElIfStmtNode>> elif_blocks,
                  NodePtr else_block, size_t offset, int size)
        : ASTNode{offset, size}, condition{std::move(condition)},
          then_block{std::move(then_block)},
          elif_blocks{std::move(elif_blocks)},
          else_block{std::move(else_block)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTWhileStmtNode : public ASTNode {
    // This field represents the expression used for the condition.
    NodePtr condition;

    // This field represents the statement block.
    NodePtr block;

    ASTWhileStmtNode(NodePtr condition, NodePtr block, size_t offset, int size)
        : ASTNode{offset, size}, condition{std::move(condition)},
          block{std::move(block)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTForStmtNode : public ASTNode {
    // This field represents the name to be used on each iteration.
    std::unique_ptr<ASTNameExprNode> name;

    // This field represents the container to be iterated over.
    NodePtr container;

    // This field represents the stmt block of the loop.
    NodePtr block;

    ASTForStmtNode(std::unique_ptr<ASTNameExprNode> name, NodePtr container,
                   NodePtr block, size_t offset, int size)
        : ASTNode{offset, size}, name{std::move(name)},
          container{std::move(container)}, block{std::move(block)} {}

    auto pretty_print(FILE *out, int level) -> void override;
};

} // namespace chocopyc::Parse

#endif