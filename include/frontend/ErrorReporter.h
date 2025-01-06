/*
    This file defines the interface for reporting frontend errors to the user.
*/

#ifndef CHOCOPYC_FRONTEND_ERRORREPORTER_H
#define CHOCOPYC_FRONTEND_ERRORREPORTER_H

#include "source/SourceFile.h"

namespace chocopyc::Frontend {
// This object serves as the interface for reporting frontend errors to the
// user. It is a singleton instance and all fields and methods are static.
class ErrorReporter {
    // This field represents a global state representing whether an error has
    // been found during the frontend phase.
    static inline bool has_seen_error = false;

    ErrorReporter() {}

  public:
    // This method reports errors to the user.
    static auto report_error(const char *msg, size_t offset, int size,
                             const Source::SourceFile &src_file) -> void;

    // This method returns whether an error has been detected.
    [[nodiscard]] static auto failed() -> bool { return has_seen_error; }
};
} // namespace chocopyc::Frontend

#endif