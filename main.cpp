#include "parse/Lexer.h"
#include "source/SourceFile.h"

#include <print>

int main(int argc, char *argv[]) {
    if (argc < 2) {
        std::println(stderr, "error: no source file given.");
        return 1;
    }

    // For now, we will treat argv[1] as the source file path.
    auto src_file_expected =
        chocopyc::Source::SourceFile::from_src_file(argv[1]);

    if (!src_file_expected.value()) {
        std::println(stderr, "error: {}", src_file_expected.error());
        return 1;
    }

    // Since we checked for errors, we can now take out its value.
    auto src_file = src_file_expected.value();

    chocopyc::Parse::Lexer lexer{src_file};
    chocopyc::Parse::Token tok;

    lexer.lex_next_tok(tok);
    while (tok.kind != chocopyc::Parse::TokenKind::End) {
        std::println("token: {}",
                     chocopyc::Parse::token_names[static_cast<int>(tok.kind)]);
        
        lexer.lex_next_tok(tok);
    }
}