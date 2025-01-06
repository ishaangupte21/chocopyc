/*
    This file defines the base AST Node interface for parsing Chocopy source
   code.
*/

#ifndef CHOCOPYC_PARSE_ASTNODE_H
#define CHOCOPYC_PARSE_ASTNODE_H

#include <cstdio>
#include <memory>

namespace chocopyc::Parse {
// This is the base class that all AST nodes will inherit from. All AST nodes
// are required to have source location information, as well as implement a
// pretty printing method. This will be an abstract class.
struct ASTNode {
    // This field represents the starting offset of this AST construct within
    // the source file.
    size_t offset;

    // This field represents the size of this AST construct.
    int size;

    ASTNode(size_t offset, int size) : offset{offset}, size{size} {}

    virtual ~ASTNode() {};

    // This method provides the end offset of a node. It is used for computing
    // sizes of other nodes.
    [[nodiscard]] auto end() -> size_t { return offset + size; }

    // Abstract method for pretty printing nodes.
    virtual auto pretty_print(FILE *out, int level) -> void = 0;
};

// Declaration of a pointer to an ASTNode to make things easier.
using NodePtr = std::unique_ptr<ASTNode>;
} // namespace chocopyc::Parse

#endif