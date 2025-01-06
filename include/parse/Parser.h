/*
    This file defines the interface for parsing Chocopy source code.
*/

#include "frontend/ErrorReporter.h"
#include "parse/ASTNode.h"
#include "parse/Lexer.h"

#ifndef CHOCOPYC_PARSE_PARSER_H
#define CHOCOPYC_PARSE_PARSER_H

namespace chocopyc::Parse {
// This object defines the parser that will be used for converting Chocopy
// source files into an Abstract Syntax Tree.
class Parser {
    // This field defines the lexer instance for the given source file.
    // We can guarantee that the lexer instance will outlive the parser, so a
    // reference is fine here.
    Lexer &lexer;

    // This is a reference to the source file instance for this parser. It will
    // be used for error reporting.
    const Source::SourceFile &src_file;

    // This field defines the token instance. To avoid creating several
    // short-lived objects, we will make a single token instance that will be
    // mutated by the lexer.
    Token tok;

    // For each parser method, we must have the capability to return both a
    // result and a boolean field that indicates whether an error has already
    // been reported. This allows us to print error messages at the point with
    // the most context and avoid printing multiple message for the same error.
    using ReturnType = std::expected<NodePtr, bool>;

    /* Parser subroutines */

    // This method will request the next token from the lexer.
    auto advance() -> void { lexer.lex_next_tok(tok); }

    // This method checks if the current token matches the expected token
    [[nodiscard]] auto expect(TokenKind expected) -> bool {
        return expected == tok.kind;
    }

    // This method handles error reporting.
    auto report_parser_error(const char *msg, size_t offset, int size) -> void {
        Frontend::ErrorReporter::report_error(msg, offset, size, src_file);
    }

    // This method parses the most basic form of expressions in Chocopy.
    [[nodiscard]] auto parse_chocopy_primary_expr() -> ReturnType;

    // This method parses Chocopy list literal expressions.
    [[nodiscard]] auto parse_chocopy_list_literal() -> ReturnType;

    // This method parses Chocopy expressions enclosed in parentheses.
    [[nodiscard]] auto parse_chocopy_paren_expr() -> ReturnType;

    // This method parses Chocopy expressions.
    [[nodiscard]] auto parse_chocopy_expr() -> ReturnType {
        return parse_chocopy_primary_expr();
    };

  public:
    Parser(Lexer &lexer, const Source::SourceFile &src_file)
        : lexer{lexer}, src_file{src_file} {}

    [[nodiscard]] auto parse_chocopy_compilation_unit() -> ReturnType {
        advance();
        return parse_chocopy_expr();
    }
};
} // namespace chocopyc::Parse

#endif