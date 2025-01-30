/*
    This file defines the Lexical Analyzer for chocopy.
*/

#ifndef CHOCOPYC_PARSE_LEXER_H
#define CHOCOPYC_PARSE_LEXER_H

#include "frontend/ErrorReporter.h"
#include "parse/Token.h"
#include "source/SourceFile.h"

#include <stack>
#include <unordered_map>

namespace chocopyc::Parse {
// This object is the lexical analyzer that will scan the chocopy source file
// for tokens.
class Lexer {
    // This field represents the instance of the source file information object.
    const Source::SourceFile &src_file;

    // This field represents the current pointer within the buffer.
    char *ptr;

    // This field represents the stack that will be used to track whitespace
    // values for indenting.
    std::stack<int> whitespace_stack;

    // This field represents a lookup table for all keywords.
    static std::unordered_map<std::string_view, TokenKind> keyword_table;

    /* Lexer subroutines */

    // This method reports lexical errors to the user through the ErrorReporter
    // interface.
    auto report_lexer_error(const char *msg, char *tok_start, int size)
        -> void {
        Frontend::ErrorReporter::report_error(
            msg, tok_start - src_file.get_start_ptr(), size, src_file);
    }

    // This method sets the information for the next token to be used by the
    // parser.
    auto make_token(Token &tok, TokenKind kind, char *tok_start, int size)
        -> void {
        tok.set_info(kind, tok_start - src_file.get_start_ptr(), size);
    }

    // This method checks whether the previous line in the input was empty.
    [[nodiscard]] auto was_last_line_empty(Token &tok) -> bool {
        return tok.kind == TokenKind::Newline ||
               tok.kind == TokenKind::Indent || tok.kind == TokenKind::Dedent;
    }

    // This method consumes all horizontal whitespace ahead of a given token and
    // returns the number of bytes consumed.
    [[nodiscard]] auto consume_horizontal_whitespace() -> int;

    // This method scans numeric literals from the source.
    auto lex_numeric_literal(Token &tok, char *tok_start) -> void;

    // This method scans floating point literals from the source.
    auto lex_floating_point_literal(Token &tok, char *tok_start) -> void;

    // This method scans floating point literals from the source.
    auto lex_string_literal(Token &tok, char *tok_start) -> void;

    // This method scans identifiers and keywords from the source.
    auto lex_identifier_or_keyword(Token &tok, char *tok_start) -> void;

  public:
    Lexer(const Source::SourceFile &src_file)
        : src_file{src_file}, ptr{src_file.get_start_ptr()} {
        // We want to push a default value of 0 onto the whitespacestack.
        whitespace_stack.push(0);
    }

    // This method will serve as the main lexer routine.
    auto lex_next_tok(Token &tok) -> void;

    // This method will expose the current indentation level.
    [[nodiscard]] auto get_indentation_level() -> int {
        return whitespace_stack.top();
    }
};
} // namespace chocopyc::Parse

#endif