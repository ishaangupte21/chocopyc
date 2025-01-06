/*
    This file implements the interface for reporting frontend errors to the
   user.
*/

#include "frontend/ErrorReporter.h"

#include <print>

namespace chocopyc::Frontend {

// This is the method responsible for reporting error messages to the user. For
// now, we will just print the line and column numbers and the message. Later
// on, we can possibly display to the user the exact point of failure within the
// source.
auto ErrorReporter::report_error(const char *msg, size_t offset, int size,
                                 const Source::SourceFile &src_file)
    -> void {
    // The first thing we need to do is get the line and column information for
    // this position within the source file.
    auto [line_no, col_no] = src_file.get_location(offset);

    // Now, we can print the error to the user.
    // We also want an extra newline character at the end to make it look nicer.
    std::println(stderr, "error: {} at line {}, column {}: {}\n",
                 src_file.get_src_file_path(), line_no, col_no, msg);
}
} // namespace chocopyc::Frontend