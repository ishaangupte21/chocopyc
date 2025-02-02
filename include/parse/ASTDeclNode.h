/*
    This file defines AST Nodes for Chocopy declarations
*/

#ifndef CHOCOPYC_PARSE_ASTDECLNODE_H
#define CHOCOPYC_PARSE_ASTDECLNODE_H

#include "parse/ASTExprNode.h"
#include "parse/ASTNode.h"

namespace chocopyc::Parse {
struct ASTGlobalNameDeclNode : public ASTNode {
    // This field represents the name to be declared at a global scope.
    std::unique_ptr<ASTNameExprNode> name;

    ASTGlobalNameDeclNode(std::unique_ptr<ASTNameExprNode> name, size_t offset,
                          int size)
        : ASTNode{offset, size}, name{std::move(name)} {}

    virtual auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTNonlocalNameDeclNode : public ASTNode {
    // This field represents the name to be declared at a nonlocal scope.
    std::unique_ptr<ASTNameExprNode> name;

    ASTNonlocalNameDeclNode(std::unique_ptr<ASTNameExprNode> name,
                            size_t offset, int size)
        : ASTNode{offset, size}, name{std::move(name)} {}

    virtual auto pretty_print(FILE *out, int level) -> void override;
};

struct ASTTypeExprNode : public ASTNode {
    // This field represents the inner type of the type expression.
    // It will be either a name, or another array type.
    NodePtr inner_type;

    // This field tracks whether the given type expression represents an array
    // type.
    bool is_array_type;

    ASTTypeExprNode(NodePtr inner_type, bool is_array_type, size_t offset,
                    int size)
        : ASTNode{offset, size}, inner_type{std::move(inner_type)},
          is_array_type{is_array_type} {}

    virtual auto pretty_print(FILE *out, int level) -> void override;
};
} // namespace chocopyc::Parse

#endif