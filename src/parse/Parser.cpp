/*
    This file implements the interface for parsing Chocopy source code.
*/

#include "parse/Parser.h"
#include "parse/ASTExprNode.h"
#include "parse/Token.h"

namespace chocopyc::Parse {
// This method performs enum conversions between token types and AST binary
// operator types.
static auto get_chocopy_binary_operator(TokenKind kind)
    -> ASTBinaryOpExprNode::Operator {
    switch (kind) {
    case TokenKind::Plus:
        return ASTBinaryOpExprNode::Operator::BinaryPlus;
    case TokenKind::Minus:
        return ASTBinaryOpExprNode::Operator::BinaryMinus;
    case TokenKind::Asterisk:
        return ASTBinaryOpExprNode::Operator::BinaryAsterisk;
    case TokenKind::Slash:
        return ASTBinaryOpExprNode::Operator::BinarySlash;
    case TokenKind::SlashSlash:
        return ASTBinaryOpExprNode::Operator::BinarySlashSlash;
    case TokenKind::Percent:
        return ASTBinaryOpExprNode::Operator::BinaryPercent;
    case TokenKind::EqualsEquals:
        return ASTBinaryOpExprNode::Operator::BinaryEqualsEquals;
    case TokenKind::ExclamationEquals:
        return ASTBinaryOpExprNode::Operator::BinaryExclamationEquals;
    case TokenKind::LessEquals:
        return ASTBinaryOpExprNode::Operator::BinaryLessEquals;
    case TokenKind::Less:
        return ASTBinaryOpExprNode::Operator::BinaryLess;
    case TokenKind::Greater:
        return ASTBinaryOpExprNode::Operator::BinaryGreater;
    case TokenKind::GreaterEquals:
        return ASTBinaryOpExprNode::Operator::BinaryGreaterEquals;
    case TokenKind::KeywordIs:
        return ASTBinaryOpExprNode::Operator::BinaryIs;
    case TokenKind::KeywordAnd:
        return ASTBinaryOpExprNode::Operator::BinaryLogicalAnd;
    case TokenKind::KeywordOr:
        return ASTBinaryOpExprNode::Operator::BinaryLogicalOr;
    default:
        // This should not be reachable, but we will return a dummy value.
        return ASTBinaryOpExprNode::Operator::BinaryPercent;
    }
};

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

    // The last primary expression we will support is the unary negation
    // expression.
    case TokenKind::Minus: {
        // First, we must store the offset of the minus sign and then consume
        // it.
        size_t minus_op_offset = tok.offset;
        advance();

        // Now, we need an expression. THe Chocopy standard requires expressions
        // following a unary '-' operator to be a primary expression or binary
        // operator expression.
        size_t expr_expected_start = tok.offset;
        int expr_expected_size = tok.size;

        auto expr_expected = parse_chocopy_binary_op_expr();
        if (!expr_expected.has_value()) {
            if (!expr_expected.error())
                report_parser_error(
                    "expected expression after the unary operator '-'.",
                    expr_expected_start, expr_expected_size);

            return std::unexpected{true};
        }

        // If the expression was found, we can create the node.
        int size = expr_expected.value()->end() - minus_op_offset;

        primary_expr = std::make_unique<ASTUnaryOpExprNode>(
            ASTUnaryOpExprNode::Operator::UnaryMinus,
            std::move(expr_expected.value()), minus_op_offset, size);

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

                return std::make_unique<ASTFunctionCallExprNode>(
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
            primary_expr = std::make_unique<ASTFunctionCallExprNode>(
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

// This method parses Chocopy expressions that are enclosed in parentheses. For
// the purposes of error reporting, we have a separate AST Node for them.
// Otherwise, they will just be expanded into the parent tree.
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

// This method takes a reference to a token and returns its precedence if it is
// a binary operator. If it is not a valid binary operator, this method will
// return -1.
auto Parser::get_chocopy_binary_op_precedence(const Token &tok) -> int {
    switch (tok.kind) {
    // First, we will do all relational operators.
    case TokenKind::EqualsEquals:
    case TokenKind::ExclamationEquals:
    case TokenKind::Less:
    case TokenKind::Greater:
    case TokenKind::LessEquals:
    case TokenKind::GreaterEquals:
    case TokenKind::KeywordIs:
        return 1;

    // Next we will do addition operators.
    case TokenKind::Plus:
    case TokenKind::Minus:
        return 2;

    // Next, we have multiplication operators.
    case TokenKind::Asterisk:
    case TokenKind::Slash:
    case TokenKind::SlashSlash:
    case TokenKind::Percent:
        return 3;

    // For everything else, we will return -1 as it is not a valid binary
    // operator.
    default:
        return -1;
    }
}

// This method will parse binary expressions. These are equivalent to chocopy's
// 'cexpr' construct.
auto Parser::parse_chocopy_binary_op_expr() -> ReturnType {
    // First, we must have a primary expression as the LHS of the expression.
    // This is one of the few places where we won't report errors. We will just
    // pass the error up to the caller.
    auto lhs_expected = parse_chocopy_primary_expr();
    if (!lhs_expected.has_value())
        return lhs_expected;

    // If we do have an LHS, we can proceed to parse the RHS.
    return parse_chocopy_binary_op_expr_rhs(std::move(lhs_expected.value()));
}

// Here we will use operator precedence parsing to optimize the parsing of the
// RHS of expressions. The basic idea here is that all operators on the same
// level should be parsed continuously. If you reach an operator with a higher
// level, you make another recursive call to enforce the precedence. If you
// reach one with a lower level, you return out of the call.
auto Parser::parse_chocopy_binary_op_expr_rhs(NodePtr lhs, int precedence)
    -> ReturnType {
    while (true) {
        // First, we need to check if we have a binary operator that has a
        // precedence at least as much as the current precedence. If we don't we
        // will exit as that operator needs to be parsed at a lower level.
        int tok_precedence = get_chocopy_binary_op_precedence(tok);
        if (tok_precedence < precedence)
            return lhs;

        // Now that we know this is a valid binary operator, we can store it and
        // consume the token.
        auto binary_operator = tok.kind;
        advance();

        // After here, we must have an expression representing the RHS of the
        // binary expression.
        size_t rhs_expected_start = tok.offset;
        int rhs_expected_size = tok.size;

        auto rhs_expected = parse_chocopy_primary_expr();
        if (!rhs_expected.has_value()) {
            if (!rhs_expected.error())
                report_parser_error("expected expression on the right hand "
                                    "side of a binary operator.",
                                    rhs_expected_start, rhs_expected_size);

            return std::unexpected{true};
        }

        // Now that we have the RHS, we need to keep parsing while we have an
        // operator that has a higher precedence.
        int next_tok_precedence = get_chocopy_binary_op_precedence(tok);
        if (tok_precedence < next_tok_precedence) {
            size_t rhs_expected_start = tok.offset;
            int rhs_expected_size = tok.size;

            rhs_expected = parse_chocopy_binary_op_expr_rhs(
                std::move(rhs_expected.value()), tok_precedence + 1);

            if (!rhs_expected.has_value()) {
                if (!rhs_expected.error())
                    report_parser_error("expected expression on the right hand "
                                        "side of a binary operator.",
                                        rhs_expected_start, rhs_expected_size);

                return std::unexpected{true};
            }
        }

        // Once parsing is complete, we can combine the LHS and RHS.
        auto converted_op = get_chocopy_binary_operator(binary_operator);
        size_t start = lhs->offset;
        int size = rhs_expected.value()->end() - start;

        lhs = std::make_unique<ASTBinaryOpExprNode>(
            converted_op, std::move(lhs), std::move(rhs_expected.value()),
            start, size);
    }
}

auto Parser::parse_chocopy_unary_not_expr() -> ReturnType {
    // Here, we first need to check for the unary not operator.
    // If one is not present, we will pass the parsing on to binary expressions.
    if (!expect(TokenKind::KeywordNot))
        return parse_chocopy_binary_op_expr();

    // Otherwise, if the 'not' keyword is found, we will treat it as a prefix
    // operator.
    size_t not_op_start = tok.offset;
    advance();

    // Now, we need an expression.
    size_t expr_expected_start = tok.offset;
    int expr_expected_size = tok.size;

    auto expr_expected = parse_chocopy_binary_op_expr();
    if (!expr_expected.has_value()) {
        if (!expr_expected.error())
            report_parser_error(
                "expected expression after unary operator 'not'.",
                expr_expected_start, expr_expected_size);

        return std::unexpected{true};
    }

    // Now, we can create the node.
    int size = expr_expected.value()->end() - not_op_start;

    return std::make_unique<ASTUnaryOpExprNode>(
        ASTUnaryOpExprNode::Operator::UnaryNot,
        std::move(expr_expected.value()), expr_expected_start,
        expr_expected_size);
}

// This method parses Chocopy logical 'and' expressions. Although they are
// parsed separately due to precedence, they are ultimately still binary
// expressions. Therefore, we need to deal with the left recursion that arises
// here.
//
// The actual grammar for this part would be:
// logical_boolean_expr := logical_boolean_expr 'and' logical_boolean_expr
//
// However, to eliminate the left recursion. we will transform it into:
// logical_boolean_expr := unary_not_expr [ 'and' unary_not_expr]*
auto Parser::parse_chocopy_logical_and_expr() -> ReturnType {
    // First we need to get a unary not expression. If it isn't present, we will
    // just propagate the error to the caller.
    auto lhs = parse_chocopy_unary_not_expr();
    if (!lhs.has_value())
        return lhs;

    // Now, while we have a viable operator, we must keep parsing.
    while (expect(TokenKind::KeywordAnd)) {
        advance();

        // Now, we must have an RHS for our expression.
        size_t rhs_expected_start = tok.offset;
        int rhs_expected_size = tok.size;

        auto rhs_expected = parse_chocopy_unary_not_expr();
        if (!rhs_expected.has_value()) {
            if (!rhs_expected.error())
                report_parser_error("expected expression on the right hand "
                                    "side of the operator 'and'.",
                                    rhs_expected_start, rhs_expected_size);

            return std::unexpected{true};
        }

        // Now, we can create our node.
        size_t start = lhs.value()->offset;
        int size = rhs_expected.value()->end() - start;

        lhs = std::make_unique<ASTBinaryOpExprNode>(
            ASTBinaryOpExprNode::Operator::BinaryLogicalAnd,
            std::move(lhs.value()), std::move(rhs_expected.value()), start,
            size);
    }

    return lhs;
}

// This method parses Chocopy logical 'or' expressions. Although they are
// parsed separately due to precedence, they are ultimately still binary
// expressions. Therefore, we need to deal with the left recursion that arises
// here.
//
// The actual grammar for this part would be:
// logical_boolean_expr := logical_boolean_expr 'or' logical_boolean_expr
//
// However, to eliminate the left recursion. we will transform it into:
// logical_boolean_expr := logical_and_expr [ 'or' logical_and_expr]*
auto Parser::parse_chocopy_logical_or_expr() -> ReturnType {
    // First we need to get a logical and expression. If it isn't present, we
    // will just propagate the error to the caller.
    auto lhs = parse_chocopy_logical_and_expr();
    if (!lhs.has_value())
        return lhs;

    // Now, while we have a viable operator, we must keep parsing.
    while (expect(TokenKind::KeywordOr)) {
        advance();

        // Now, we must have an RHS for our expression.
        size_t rhs_expected_start = tok.offset;
        int rhs_expected_size = tok.size;

        auto rhs_expected = parse_chocopy_logical_and_expr();
        if (!rhs_expected.has_value()) {
            if (!rhs_expected.error())
                report_parser_error("expected expression on the right hand "
                                    "side of the operator 'or'.",
                                    rhs_expected_start, rhs_expected_size);

            return std::unexpected{true};
        }

        // Now, we can create our node.
        size_t start = lhs.value()->offset;
        int size = rhs_expected.value()->end() - start;

        lhs = std::make_unique<ASTBinaryOpExprNode>(
            ASTBinaryOpExprNode::Operator::BinaryLogicalOr,
            std::move(lhs.value()), std::move(rhs_expected.value()), start,
            size);
    }

    return lhs;
}

auto Parser::parse_chocopy_ternary_expr() -> ReturnType {
    // First, we need to parse the 'then' expression.
    // We do not need to report errors here as it is possible that this was
    // never part of a ternary expression.
    auto then_expr_expected = parse_chocopy_logical_or_expr();

    // Since we may not actually have a ternary expression, if the following
    // token is not 'if', we will also just return the result from the parsed
    // expression.
    if (!then_expr_expected.has_value() || !expect(TokenKind::KeywordIf))
        return then_expr_expected;

    // Now, we can consume the if keyword and check for the condition
    // expression.
    advance();

    size_t condition_expected_start = tok.offset;
    int condition_expected_size = tok.size;

    auto condition_expected = parse_chocopy_expr();
    if (!condition_expected.has_value()) {
        if (!condition_expected.error())
            report_parser_error(
                "expected expression after 'if' in ternary expression.",
                condition_expected_start, condition_expected_size);

        return std::unexpected{true};
    }

    // Now, we must have an 'else' token.
    if (!expect(TokenKind::KeywordElse)) {
        report_parser_error("expected 'else' after condition expression within "
                            "ternary expression.",
                            tok.offset, tok.size);

        return std::unexpected{true};
    }

    // Consume the 'else' component.
    advance();

    // Now, we need an else expression.
    size_t else_expr_expected_start = tok.offset;
    int else_expr_expected_size = tok.size;

    auto else_expr_expected = parse_chocopy_expr();
    if (!else_expr_expected.has_value()) {
        if (!else_expr_expected.error())
            report_parser_error(
                "expected expression after 'else' in ternary expression.",
                else_expr_expected_start, else_expr_expected_size);

        return std::unexpected{true};
    }

    // Now, we can construct the node.
    size_t start = then_expr_expected.value()->offset;
    int size = else_expr_expected.value()->end() - start;

    return std::make_unique<ASTTernaryExprNode>(
        std::move(condition_expected.value()),
        std::move(then_expr_expected.value()),
        std::move(else_expr_expected.value()), start, size);
}
} // namespace chocopyc::Parse