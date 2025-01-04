/*
    This file defines the source file object that will be used to provide line
   and column information.
*/

#ifndef CHOCOPYC_SOURCE_SOURCEFILE_H
#define CHOCOPYC_SOURCE_SOURCEFILE_H

#include <expected>
#include <memory>
#include <string>
#include <vector>

namespace chocopyc::Source {
// This is the object that will contain all source file metadata and serve as an
// interface for computing line and column information.
class SourceFile {
    // Type definition to simplify code relating to newline characters. An
    // 'instance' of a newline character will simply be a tuple consisting of
    // the offset within the source file and its size (either 1 or 2 bytes).
    using NewlineChar = std::pair<size_t, int>;

    // This is the path of the source file.
    std::string src_file_path;

    // This is the buffer to the contents of the source file.
    std::unique_ptr<char[]> src_file_buffer;

    // This is the size of the source file.
    size_t src_file_size;

    // This is the list of newline objects that will be used to compute line and
    // column information. Each entry to this list will consist of the position
    // of the newline character and its length.
    std::vector<NewlineChar> newline_chars;

    // This is the method that records the positions of all newline characters
    // in the file.
    [[nodiscard]] static auto
    locate_newline_chars(std::unique_ptr<char[]> &buffer, size_t src_file_size)
        -> std::vector<NewlineChar>;

  public:
    SourceFile(std::string src_file_path,
               std::unique_ptr<char[]> src_file_buffer, size_t src_file_size,
               std::vector<NewlineChar> newline_chars)
        : src_file_path{std::move(src_file_path)},
          src_file_buffer{std::move(src_file_buffer)},
          src_file_size{src_file_size},
          newline_chars{std::move(newline_chars)} {}

    // Since instances of this object will never be owned by a single phase of
    // the compilation process, it is important to ensure that it can only be
    // created as a reference counted pointer.
    [[nodiscard]] static auto from_src_file(const char *src_file_path)
        -> std::expected<std::shared_ptr<SourceFile>, std::string>;

    // This method returns the pointer to the start of the source file buffer.
    [[nodiscard]] auto get_start_ptr() -> char * {
        return src_file_buffer.get();
    }

    // This method returns the pointer to the end of the source file buffer. It
    // will contain a sentinel character.
    [[nodiscard]] auto get_end_ptr() -> char * {
        return get_start_ptr() + src_file_size - 1;
    }

    // This method will take in an offset within this source file and return the
    // line and column numbers of the given position.
    [[nodiscard]] auto get_location(size_t offset) -> std::pair<int, int>;

    // This method will return a reference to the source file's path.
    [[nodiscard]] auto get_src_file_path() -> std::string & {
        return src_file_path;
    }
};
} // namespace chocopyc::Source

#endif