/*
    This file implements the interface for parsing Chocopy source code.
*/

#include "parse/Parser.h"
#include "parse/ASTExprNode.h"
#include "parse/Token.h"

namespace chocopyc::Parse {
// This method parses the most basic form of expressions within Chocopy. Here,
// we will handle literals (integer, float, boolean, string), identifiers, and
// lists. We will also check for postfix components such as attribute
// referencing and function calls.
auto Parser::parse_chocopy_primary_expr() -> Parser::ReturnType {
    // We first need to hold the main part of the expression. We will begin by
    // checking for literals.
    NodePtr primary_expr = nullptr;

    switch (tok.kind) {
    case TokenKind::IntLiteral: {
        primary_expr = std::make_unique<ASTLiteralExprNode>(
            ASTLiteralExprNode::LiteralKind::Int, tok.offset, tok.size);

        // Consume this integer literal.
        advance();
        break;
    }
    case TokenKind::FloatLiteral: {
        primary_expr = std::make_unique<ASTLiteralExprNode>(
            ASTLiteralExprNode::LiteralKind::Float, tok.offset, tok.size);

        // Consume this float literal.
        advance();
        break;
    }
    case TokenKind::StringLiteral: {
        primary_expr = std::make_unique<ASTLiteralExprNode>(
            ASTLiteralExprNode::LiteralKind::String, tok.offset, tok.size);

        // Consume this string literal.
        advance();
        break;
    }
    case TokenKind::KeywordTrue: {
        primary_expr =
            std::make_unique<ASTLiteralExprNode>(true, tok.offset, tok.size);

        // Consume this boolean literal.
        advance();
        break;
    }
    case TokenKind::KeywordFalse: {
        primary_expr =
            std::make_unique<ASTLiteralExprNode>(false, tok.offset, tok.size);

        // Consume this boolean literal.
        advance();
        break;
    }
    case TokenKind::Identifier: {
        primary_expr = std::make_unique<ASTNameExprNode>(tok.offset, tok.size);

        // Consume this identifier token.
        advance();
        break;
    }

    // Check for expressions enclosed in parentheses.
    case TokenKind::LeftParen: {
        auto paren_expr_expected = parse_chocopy_paren_expr();
        // If there is an error, we propagate that error up to the caller.
        if (!paren_expr_expected.has_value())
            return paren_expr_expected;

        primary_expr = std::move(paren_expr_expected.value());
        break;
    }

    // Now, we will check for list literals.
    case TokenKind::LeftSquare: {
        auto list_expr_expected = parse_chocopy_list_literal();
        // If there is an error, we propagate that error up to the caller.
        if (!list_expr_expected.has_value())
            return list_expr_expected;

        primary_expr = std::move(list_expr_expected.value());
        break;
    }

    // For all other tokens, we want to report an error, but the error field
    // will be false. This allows the creation of context-specific error
    // messages rather than just 'expected expression', and prevents multiple
    // messages from being generated for the same issue.
    default:
        return std::unexpected{false};
    }

    // Once we have parsed the main component of the expression, we can check
    // for any postfix components. This includes an indexing operator, an
    // attribute reference operator, and a call expression.
    while (true) {
        switch (tok.kind) {

        // A '.' represents an attribute referencing expression.
        case TokenKind::Dot: {
            // After the dot must come an identifier.
            // First, we will consume the dot.
            advance();

            if (!expect(TokenKind::Identifier)) {
                report_parser_error("expected identifier after '.' in "
                                    "attribute referencing expression.",
                                    tok.offset, tok.size);

                return std::unexpected{true};
            }

            // Now that we have an identifier, we can make a node for the name.
            auto name_expr =
                std::make_unique<ASTNameExprNode>(tok.offset, tok.size);

            // We can also make a node for the actual attribute reference
            // expression itself.
            size_t start = primary_expr->offset;
            int size = name_expr->end() - primary_expr->offset;

            primary_expr = std::make_unique<ASTAttributeRefExprNode>(
                std::move(primary_expr), std::move(name_expr), start, size);

            break;
        }

        // A '[' represents an indexing expression
        case TokenKind::LeftSquare: {
            // First, we will consume the left square.
            advance();

            // Now, there must be an expression representing the actual index.
            size_t expr_expected_offset = tok.offset;
            int expr_expected_size = tok.size;

            auto expr_expected = parse_chocopy_expr();
            if (!expr_expected.has_value()) {
                if (!expr_expected.value())
                    report_parser_error(
                        "expected expression for index after '['.",
                        expr_expected_offset, expr_expected_size);

                return std::unexpected{true};
            }

            // Now, we need a closing right square bracket.
            if (!expect(TokenKind::RightSquare)) {
                report_parser_error(
                    "expected ']' after index in indexing expression.",
                    tok.offset, tok.size);
                return std::unexpected{true};
            }

            // If we have the bracket, we must consume it and make the node.
            size_t start = primary_expr->offset;
            int size = tok.end() - primary_expr->offset;

            primary_expr = std::make_unique<ASTIndexingExprNode>(
                std::move(primary_expr), std::move(expr_expected.value()),
                start, size);

            break;
        }

        // A left parenthesis represents a call expression.
        case TokenKind::LeftParen: {
            // First, we will consume the left parenthesis.
            advance();

            std::vector<NodePtr> args;

            // First, we will check for no arguments.
            if (expect(TokenKind::RightParen)) {
                size_t start = primary_expr->offset;
                int size = tok.end() - start;

                return std::make_unique<ASTFunctionCallExpr>(
                    std::move(primary_expr), std::move(args), start, size);
            }

            // We must get at least one argument here.
            size_t first_arg_expected_start = tok.offset;
            int first_arg_expected_size = tok.size;

            auto first_arg_expected = parse_chocopy_expr();
            if (!first_arg_expected.has_value()) {
                if (!first_arg_expected.error()) {
                    report_parser_error(
                        "expected expression as argument in function call.",
                        first_arg_expected_start, first_arg_expected_size);
                }

                return std::unexpected{true};
            }

            args.push_back(std::move(first_arg_expected.value()));

            // Now, we need to check for commas and other arguments.
            while (expect(TokenKind::Comma)) {
                // Consume the comma.
                advance();

                // Now, we must have an expression as the next argument.
                size_t arg_expected_start = tok.offset;
                int arg_expected_size = tok.size;

                auto arg_expected = parse_chocopy_expr();
                if (!arg_expected.has_value()) {
                    if (!arg_expected.error()) {
                        report_parser_error(
                            "expected expression as argument after "
                            "',' in function call.",
                            arg_expected_start, arg_expected_size);
                    }

                    return std::unexpected{true};
                }

                args.push_back(std::move(arg_expected.value()));
            }

            // Now, we must have a right parenthesis.
            if (!expect(TokenKind::RightParen)) {
                report_parser_error("expected closing ')' in function call.",
                                    tok.offset, tok.size);
                return std::unexpected{true};
            }

            int size = tok.end() - primary_expr->offset;
            primary_expr = std::make_unique<ASTFunctionCallExpr>(
                std::move(primary_expr), std::move(args), primary_expr->offset,
                size);

            break;
        }

        // If there are no postfix components, we can just return the
        // expression.
        default:
            return primary_expr;
        }
    }
}

// This method parses chocopy list literals. We will begin with an opening
// square bracket, and consume all expressions until we find the right square
// bracket.
auto Parser::parse_chocopy_list_literal() -> Parser::ReturnType {
    // First, we will mark the offset of the square bracket and then consume the
    // square bracket.
    size_t lsquare_offset = tok.offset;
    advance();

    std::vector<NodePtr> contents;

    // If the token following immediately is a right square bracket, we can
    // return an empty list
    if (expect(TokenKind::RightSquare)) {
        // Here we must compute the size of the literal.
        int size = tok.end() - lsquare_offset;

        // Now, we can consume the square bracket and make the node.
        advance();

        return std::make_unique<ASTListExprNode>(std::move(contents),
                                                 lsquare_offset, size);
    }

    // Otherwise, we must have at least one expression. If an error occured, we
    // will send that error up to where it can be recovered from. If there is no
    // error and the expression was successfully parsed, we can add it to the
    // list.
    size_t first_expr_start_offset = tok.offset;
    int first_expr_start_size = tok.size;

    auto first_expr_expected = parse_chocopy_expr();
    if (!first_expr_expected.has_value()) {
        if (!first_expr_expected.error())
            report_parser_error(
                "expected expression after '[' in list literal.",
                first_expr_start_offset, first_expr_start_size);

        return std::unexpected{true};
    }

    contents.push_back(std::move(first_expr_expected.value()));

    // Now, while we have a comma, we can expect more expressions.
    while (expect(TokenKind::Comma)) {
        // Consume the comma.
        advance();

        // Now, we need an expression, and we will check for errors.
        size_t expr_start_offset = tok.offset;
        int expr_start_size = tok.size;

        auto expr_expected = parse_chocopy_expr();
        if (!expr_expected.has_value()) {
            if (!first_expr_expected.error())
                report_parser_error(
                    "expected expression after ',' in list literal.",
                    expr_start_offset, expr_start_size);

            return std::unexpected{true};
        }

        // If the expression was sucessfully parsed, we will add it to the list.
        contents.push_back(std::move(expr_expected.value()));
    }

    // At the end of the list, we must have a closing right square bracket.
    // Maybe later on, we can provide a better error message by checking if the
    // next token forms the start of an expression and if the user possibly
    // omitted a comma.
    if (!expect(TokenKind::RightSquare)) {
        report_parser_error("expected ']' at the end of a list literal.",
                            tok.offset, tok.size);
        return std::unexpected{true};
    }

    // If the square bracket is found, we can compute the length of the literal
    // and return the node.
    int size = tok.end() - lsquare_offset;
    advance();

    return std::make_unique<ASTListExprNode>(std::move(contents),
                                             lsquare_offset, size);
}

auto Parser::parse_chocopy_paren_expr() -> ReturnType {
    // First, we will mark the starting position and consume the left
    // parenthesis.
    size_t lparen_offset = tok.offset;
    advance();

    // Now, we need an expression that is enclosed within the parentheses.
    size_t expr_expected_start = tok.offset;
    int expr_expected_size = tok.size;

    auto expr_expected = parse_chocopy_expr();
    if (!expr_expected.has_value()) {
        if (!expr_expected.error())
            report_parser_error("expected expression after '('.",
                                expr_expected_start, expr_expected_size);

        return std::unexpected{true};
    }

    // Now, we need a closing right parenthesis.
    if (!expect(TokenKind::RightParen)) {
        report_parser_error("expected closing ')' after expression.",
                            tok.offset, tok.size);
        return std::unexpected{true};
    }

    // If we get the right parenthesis, we can store its location and consume
    // it.
    int size = tok.end() - lparen_offset;
    advance();

    return std::make_unique<ASTParenExprNode>(std::move(expr_expected.value()),
                                              lparen_offset, size);
}
} // namespace chocopyc::Parse