/*
    This file defines the interface for lexical tokens in Chocopy.
*/

#ifndef CHOCOPYC_PARSE_TOKEN_H
#define CHOCOPYC_PARSE_TOKEN_H

#include <cstddef>

#define TOKEN_LIST(F)                                                          \
    F(End)                                                                     \
    F(Dummy)                                                                   \
    F(Indent)                                                                  \
    F(Dedent)                                                                  \
    F(Newline)                                                                 \
    F(Plus)                                                                    \
    F(Minus)                                                                   \
    F(Asterisk)                                                                \
    F(Slash)                                                                   \
    F(SlashSlash)                                                              \
    F(Percent)                                                                 \
    F(Less)                                                                    \
    F(LessEquals)                                                              \
    F(Greater)                                                                 \
    F(GreaterEquals)                                                           \
    F(EqualsEquals)                                                            \
    F(ExclamationEquals)                                                       \
    F(Equals)                                                                  \
    F(LeftParen)                                                               \
    F(RightParen)                                                              \
    F(LeftSquare)                                                              \
    F(RightSquare)                                                             \
    F(Comma)                                                                   \
    F(Colon)                                                                   \
    F(Dot)                                                                     \
    F(Arrow)                                                                   \
    F(IntLiteral)                                                              \
    F(FloatLiteral)                                                            \
    F(StringLiteral)                                                           \
    F(Identifier)                                                              \
    F(KeywordFalse)                                                            \
    F(KeywordNone)                                                             \
    F(KeywordTrue)                                                             \
    F(KeywordAnd)                                                              \
    F(KeywordAs)                                                               \
    F(KeywordAssert)                                                           \
    F(KeywordAsync)                                                            \
    F(KeywordAwait)                                                            \
    F(KeywordBreak)                                                            \
    F(KeywordClass)                                                            \
    F(KeywordContinue)                                                         \
    F(KeywordDef)                                                              \
    F(KeywordDel)                                                              \
    F(KeywordElif)                                                             \
    F(KeywordElse)                                                             \
    F(KeywordExcept)                                                           \
    F(KeywordFinally)                                                          \
    F(KeywordFor)                                                              \
    F(KeywordFrom)                                                             \
    F(KeywordGlobal)                                                           \
    F(KeywordIf)                                                               \
    F(KeywordImport)                                                           \
    F(KeywordIn)                                                               \
    F(KeywordIs)                                                               \
    F(KeywordLambda)                                                           \
    F(KeywordNonlocal)                                                         \
    F(KeywordNot)                                                              \
    F(KeywordOr)                                                               \
    F(KeywordPass)                                                             \
    F(KeywordRaise)                                                            \
    F(KeywordReturn)                                                           \
    F(KeywordTry)                                                              \
    F(KeywordWhile)                                                            \
    F(KeywordWith)                                                             \
    F(KeywordYield)

namespace chocopyc::Parse {

#define F(x) x,
enum class TokenKind { TOKEN_LIST(F) };
#undef F

// This object represents a token that will be configured by the lexer and
// consumed by the parser. In order to avoid creating several short-lived
// instances, we will pass a single reference from the parser to the lexer to
// mutate.
struct Token {
    // This field represents the type of the last token that was consumed. It is
    // initialized to be a dummy token.
    TokenKind kind = TokenKind::Dummy;

    // This field represents the offset of the starting character of this token
    // within the source file.
    size_t offset;

    // This field represents the length of this token.
    int size;

    auto set_info(TokenKind kind, size_t offset, int size) -> void {
        this->kind = kind;
        this->offset = offset;
        this->size = size;
    }

    // This method provides the end offset of a token. It is used for computing
    // sizes of AST nodes.
    [[nodiscard]] auto end() -> size_t { return offset + size; }

// This is an array of the string representation of token names. This allows
// us to use the names when printing.
#define F(x) #x,
    static inline const char *token_names[] = {TOKEN_LIST(F)};
#undef F
};
} // namespace chocopyc::Parse

#undef TOKEN_LIST
#endif