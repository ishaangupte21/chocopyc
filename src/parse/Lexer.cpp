/*
    This file implements the Lexical Analyzer for Chocopy.
*/

#include "parse/Lexer.h"
#include "parse/Token.h"

namespace chocopyc::Parse {
// This is an array of the string representation of token names. This allows us
// to use the names when printing.
#define F(x) #x,
const char *token_names[] = {TOKEN_LIST(F)};
#undef F

// This hash table serves as a lookup table for all reserved keywords. In
// chocopy, all of Python's keywords are reserved, although they may not
// actually be used.
std::unordered_map<std::string_view, TokenKind> Lexer::keyword_table = {
    {"False", TokenKind::KeywordFalse},
    {"None", TokenKind::KeywordNone},
    {"True", TokenKind::KeywordTrue},
    {"and", TokenKind::KeywordAnd},
    {"as", TokenKind::KeywordAs},
    {"assert", TokenKind::KeywordAssert},
    {"async", TokenKind::KeywordAsync},
    {"await", TokenKind::KeywordAwait},
    {"break", TokenKind::KeywordBreak},
    {"class", TokenKind::KeywordClass},
    {"continue", TokenKind::KeywordContinue},
    {"def", TokenKind::KeywordDef},
    {"del", TokenKind::KeywordDel},
    {"elif", TokenKind::KeywordElse},
    {"else", TokenKind::KeywordElse},
    {"except", TokenKind::KeywordExcept},
    {"finally", TokenKind::KeywordFinally},
    {"for", TokenKind::KeywordFor},
    {"from", TokenKind::KeywordFrom},
    {"global", TokenKind::KeywordGlobal},
    {"if", TokenKind::KeywordIf},
    {"import", TokenKind::KeywordImport},
    {"in", TokenKind::KeywordIn},
    {"is", TokenKind::KeywordIs},
    {"lambda", TokenKind::KeywordLambda},
    {"nonlocal", TokenKind::KeywordNonlocal},
    {"not", TokenKind::KeywordNot},
    {"or", TokenKind::KeywordOr},
    {"pass", TokenKind::KeywordPass},
    {"raise", TokenKind::KeywordRaise},
    {"return", TokenKind::KeywordReturn},
    {"try", TokenKind::KeywordTry},
    {"while", TokenKind::KeywordWhile},
    {"with", TokenKind::KeywordWith},
    {"yield", TokenKind::KeywordYield},
};

// This is the main lexical analysis routine. It is essentially an
// implementation of a DFA and the goal is to mutate the given token instance
// rather than allocate memory for another short-lived token.
auto Lexer::lex_next_tok(Token &tok) -> void {
lexer_begin:
    char *tok_start = ptr;
    // The first thing that we want to do, regardless of state, is consume all
    // horizontal whitespace.
    int whitespace_size = consume_horizontal_whitespace();

    // If the previous token was a newline token, we can now check for the need
    // to insert indent and dedent tokens.
    if (tok.kind == TokenKind::Newline) {
        // If the amount of whitespace before the next token is greater than the
        // amount at the top of the stack, we can insert an indent token and
        // push the new value onto the top of the stack.
        if (whitespace_size > whitespace_stack.top()) {
            whitespace_stack.push(whitespace_size);

            make_token(tok, TokenKind::Indent, tok_start, whitespace_size);
            return;
        }

        // If the amount of whitespace before the next token is less than the
        // amount at the top of the stack, we can insert a dedent token and pop
        // from the top of the stack.
        if (whitespace_size < whitespace_stack.top()) {
            whitespace_stack.pop();

            make_token(tok, TokenKind::Dedent, tok_start, whitespace_size);
            return;
        }
    }

    // If we get here, it means that we have a normal token.
    tok_start = ptr;

    switch (*ptr) {
    case '\0': {
        // Here, we must check for the end of the file.
        if (ptr == src_file.get_end_ptr()) {
            make_token(tok, TokenKind::End, tok_start, 1);
            return;
        }

        // If we have not hit the end of the file but find a null character, we
        // can simply consume it and restart.
        ++ptr;
        goto lexer_begin;
    }

    // Now, we will handle newline characters. However, the chocopy standard
    // requires that empty lines only contain one newline character.
    case '\n': {
        ++ptr;

        if (!was_last_line_empty(tok)) {
            make_token(tok, TokenKind::Newline, tok_start, 1);
            return;
        }

        // If we don't want to return a newline token, we must restart the
        // lexer.
        goto lexer_begin;
    }

    case '\r': {
        // The chocopy standard allows for \r\n newline characters, so we must
        // check for those too.
        if (ptr[1] == '\n') {
            ptr += 2;

            if (!was_last_line_empty(tok)) {
                make_token(tok, TokenKind::Newline, tok_start, 2);
                return;
            }

            // If we don't want to return a newline token, we must restart the
            // lexer.
            goto lexer_begin;
        }

        ++ptr;
        if (!was_last_line_empty(tok)) {
            make_token(tok, TokenKind::Newline, tok_start, 1);
            return;
        }

        // If we don't want to return a newline token, we must restart the
        // lexer.
        goto lexer_begin;
    }

    // Now, we will handle delimiters.
    case '+': {
        make_token(tok, TokenKind::Plus, tok_start, 1);
        ++ptr;

        return;
    }

    case '-': {
        if (ptr[1] == '>') {
            make_token(tok, TokenKind::Arrow, tok_start, 2);
            ptr += 2;

            return;
        }

        make_token(tok, TokenKind::Minus, tok_start, 1);
        ++ptr;

        return;
    }

    case '*': {
        make_token(tok, TokenKind::Asterisk, tok_start, 1);
        ++ptr;

        return;
    }

    case '/': {
        if (ptr[1] == '/') {
            make_token(tok, TokenKind::SlashSlash, tok_start, 2);
            ptr += 2;

            return;
        }

        make_token(tok, TokenKind::Slash, tok_start, 1);
        ++ptr;

        return;
    }

    case '%': {
        make_token(tok, TokenKind::Percent, tok_start, 1);
        ++ptr;

        return;
    }

    case '(': {
        make_token(tok, TokenKind::LeftParen, tok_start, 1);
        ++ptr;

        return;
    }

    case ')': {
        make_token(tok, TokenKind::RightParen, tok_start, 1);
        ++ptr;

        return;
    }

    case '[': {
        make_token(tok, TokenKind::LeftSquare, tok_start, 1);
        ++ptr;

        return;
    }

    case ']': {
        make_token(tok, TokenKind::RightSquare, tok_start, 1);
        ++ptr;

        return;
    }

    case ',': {
        make_token(tok, TokenKind::Comma, tok_start, 1);
        ++ptr;

        return;
    }

    // With '.' tokens, we can potentially have a floating point number.
    case '.': {
        // We must check if the following character is a digit. If it is, we
        // know that we have a floating point literal.
        if (isdigit(ptr[1])) {
            lex_floating_point_literal(tok, tok_start);
            return;
        }

        make_token(tok, TokenKind::Dot, tok_start, 1);
        ++ptr;

        return;
    }

    case ':': {
        make_token(tok, TokenKind::Colon, tok_start, 1);
        ++ptr;

        return;
    }

    case '<': {
        if (ptr[1] == '=') {
            make_token(tok, TokenKind::LessEquals, tok_start, 2);
            ptr += 2;

            return;
        }

        make_token(tok, TokenKind::Less, tok_start, 1);
        ++ptr;

        return;
    }

    case '>': {
        if (ptr[1] == '=') {
            make_token(tok, TokenKind::GreaterEquals, tok_start, 2);
            ptr += 2;

            return;
        }

        make_token(tok, TokenKind::Greater, tok_start, 1);
        ++ptr;

        return;
    }

    case '=': {
        if (ptr[1] == '=') {
            make_token(tok, TokenKind::EqualsEquals, tok_start, 2);
            ptr += 2;

            return;
        }

        make_token(tok, TokenKind::Equals, tok_start, 1);
        ++ptr;

        return;
    }

    case '!': {
        // Since chocopy (or Python for that matter) does not use '!' as the not
        // operator, we need to inform the user.
        if (ptr[1] != '=') {
            report_lexer_error(
                "unknown operator '!'. Did you mean 'not' instead?", tok_start,
                1);

            make_token(tok, TokenKind::KeywordNot, tok_start, 1);
            ++ptr;
            return;
        }

        make_token(tok, TokenKind::ExclamationEquals, tok_start, 2);
        ptr += 2;

        return;
    }

    // Now, we will handle literals. The easiest case is a numeric literal.
    case '1' ... '9': {
        lex_numeric_literal(tok, tok_start);
        return;
    }

    case '0': {
        // '0' must be either by itself, or the start of a floating point
        // literal.
        if (ptr[1] == '.') {
            ++ptr;

            lex_floating_point_literal(tok, tok_start);
            return;
        }

        // If the following character is a digit, we will report an error and
        // treat this 0 as if it were by itself.
        if (isdigit(ptr[1])) {
            report_lexer_error("'0' cannot be followed by another digit.",
                               tok_start + 1, 1);
        }

        make_token(tok, TokenKind::IntLiteral, tok_start, 1);
        ++ptr;

        return;
    }

    // String literals.
    case '"': {
        lex_string_literal(tok, tok_start);
        return;
    }

    // Finally, we will handle identifiers. Unlike the chocopy spec, we will
    // use identifiers as class names, just like conventional languages. We
    // will also check for keywords here. Once we have scanned an
    // identifier, we will check if it is a keyword.
    case 'a' ... 'z':
    case 'A' ... 'Z':
    case '_': {
        lex_identifier_or_keyword(tok, tok_start);
        return;
    }

    default: {
        // For all other characters, we must report an error as they do not form
        // a valid token.
        report_lexer_error("illegal character in source file.", tok_start, 1);
        ++ptr;

        goto lexer_begin;
    }
    }
}

// The goal of this method is to consume all horizontal whitespace and return
// the size of that whitespace. Chocopy requires us to treat spaces and tabs as
// the same.
auto Lexer::consume_horizontal_whitespace() -> int {
    char *whitespace_start_ptr = ptr;
    while (*ptr == ' ' || *ptr == '\t') {
        ++ptr;
    }

    return ptr - whitespace_start_ptr;
}

// This method consumes digits that are part of an integer numeric literal.
// After the digits, if a '.' is found, the type of literal is assumed to be a
// floating point literal.
auto Lexer::lex_numeric_literal(Token &tok, char *tok_start) -> void {
    // Now, we need to consume all of the digits that we get.
    while (isdigit(*ptr)) {
        ++ptr;
    }

    // Once we have consume digits, we can potentially get a floating point
    // value.
    if (*ptr == '.') {
        lex_floating_point_literal(tok, tok_start);
        return;
    }

    // If we don't have a floating point number, we can simply make the token
    // and return it.
    make_token(tok, TokenKind::IntLiteral, tok_start, ptr - tok_start);
}

// This method handles floating point literals. Execution in this method begins
// once a floating point has been encountred. After consuming the floating
// point, we check for any digits as part of the fractional component.
auto Lexer::lex_floating_point_literal(Token &tok, char *tok_start) -> void {
    // First, we will consume the floating point.
    ++ptr;

    // Now, we can consume all digits.
    while (isdigit(*ptr)) {
        ++ptr;
    }

    // At the end, we can return the token.
    make_token(tok, TokenKind::FloatLiteral, tok_start, ptr - tok_start);
}

// This method handles string literals. String literals are enclosed in double
// quotes and consist of characters in the range [0x20, 0x7E] with escape
// sequences.
auto Lexer::lex_string_literal(Token &tok, char *tok_start) -> void {
    // First, we will consume the opening double quote.
    ++ptr;

    // Chocopy requires that all characters within a string be between 0x20 and
    // 0x7E. Therefore, we will consume all characters that fall within that
    // range. We also need to check for escape sequences.
    while (true) {
        switch (*ptr) {
        // If we get a double quote, we can end the string literal and
        // return the token.
        case '"': {
            // Since the double quote is part of the literal, we will
            // consume that character as well.
            ++ptr;
            make_token(tok, TokenKind::StringLiteral, tok_start,
                       ptr - tok_start);

            return;
        }

        case '\0': {
            // Here, we might have the end of the file. If we do hit the end of
            // the file, that is an error. However, if we are not at the end of
            // the file, a null character is not allowed anyways.
            if (ptr == src_file.get_end_ptr()) {
                report_lexer_error("expected '\"' to terminate string literal.",
                                   ptr, 1);

                make_token(tok, TokenKind::End, ptr, 1);
                return;
            }

            // Report the invalid character error and keep on scanning.
            report_lexer_error(
                "illegal character in string literal. Only characters in the "
                "range [0x20, 0x7E] are allowed.",
                ptr, 1);
            ++ptr;
            break;
        }

        case '\\': {
            // Here, we must handle escape sequences.
            // The allowed escape sequences are '\"', '\n', '\t', and '\\'.
            if (ptr[1] != '"' && ptr[1] != 'n' && ptr[1] != 't' &&
                ptr[1] != '\\') {
                report_lexer_error(
                    "invalid escape sequence. The only accepted escape "
                    "sequences are '\\\"', '\\n', '\\t', '\\\\'.",
                    ptr, 2);

                // For the purposes of error recovery, we will just consume the
                // backslash.
                ++ptr;
            } else {
                // If the escape sequence has been successfully matched, we will
                // consume both characters.
                ptr += 2;
            }

            break;
        }

        case '\x01' ... '\x19':
        case '\x7f': {
            // Report the invalid character error and keep on scanning.
            report_lexer_error(
                "illegal character in string literal. Only characters in the "
                "range [0x20, 0x7E] are allowed.",
                ptr, 1);
            ++ptr;
            break;
        }

        default: {
            // For all valid characters, consume and keep scanning.
            ++ptr;
        }
        }
    }
}

// This method handles identifiers and keywords. We consume all letters, digits,
// and underscores as part of the identifier. Once we have finished scanning, we
// check if that identifier is a reserved keyword.
auto Lexer::lex_identifier_or_keyword(Token &tok, char *tok_start) -> void {
    // We need to consume all digits, alphabet, and underscores.
    while (isalnum(*ptr) || *ptr == '_') {
        ++ptr;
    }

    // Now, we need to check if we have a keyword.
    size_t id_token_size = ptr - tok_start;
    std::string_view id_contents{tok_start, id_token_size};

    // If we have a keyword, we will set the token for that keyword.
    auto keyword_lookup = keyword_table.find(id_contents);
    if (keyword_lookup != keyword_table.end()) {
        make_token(tok, keyword_lookup->second, tok_start, id_token_size);
        return;
    }

    // Otherwise, if we don't have a keyword, we know that we have an
    // identifier.
    make_token(tok, TokenKind::Identifier, tok_start, id_token_size);
    return;
};
} // namespace chocopyc::Parse