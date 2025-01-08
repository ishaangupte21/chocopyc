/*
    This file defines the AST nodes for Chocopy statements.
*/

#ifndef CHOCOPYC_PARSE_ASTSTMTNODE_H
#define CHOCOPYC_PARSE_ASTSTMTNODE_H

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
} // namespace chocopyc::Parse

#endif