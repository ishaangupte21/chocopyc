/*
    This file implements the source file object that will be used to provide
   line and column information.
*/

#include "source/SourceFile.h"

#include <algorithm>
#include <cerrno>
#include <cstdio>

namespace chocopyc::Source {
// This method willl take a source file path and return a new instance of
// SourceFile that consists of that file's buffered contents and metadata.
// We use the C++ 23 'std::expected' class to provide the ability to handle any
// possible errors.
auto SourceFile::from_src_file(const char *src_file_path)
    -> std::expected<SourceFile, const char *> {
    // First, we need to attempt to open the file.
    errno = 0;
    FILE *src_file = fopen(src_file_path, "r");
    if (!src_file)
        return std::unexpected{strerror(errno)};

    // Now, we must get the file size and read the file into the buffer.
    // Here, we will increment the source file size by 1 to allow for a sentinel
    // character.
    fseek(src_file, 0, SEEK_END);
    size_t src_file_size = ftell(src_file) + 1;
    fseek(src_file, 0, SEEK_SET);

    auto buffer = std::make_unique<char[]>(src_file_size);
    fread(buffer.get(), sizeof(char), src_file_size - 1, src_file);

    // Set the sentinel character at the end of the buffer.
    buffer[src_file_size - 1] = '\0';

    fclose(src_file);

    auto newline_chars = locate_newline_chars(buffer, src_file_size);

    // Now, we can return an instance of this object.
    return SourceFile{std::string{src_file_path}, std::move(buffer),
                      src_file_size, std::move(newline_chars)};
}

// The purpose of this method is to scan through the source file and locate the
// positions of newline characters. We will also store the length of these
// newline characters. This will allow us to better compute line and column
// information when reporting errors in the frontend.
auto SourceFile::locate_newline_chars(std::unique_ptr<char[]> &buffer,
                                      size_t src_file_size)
    -> std::vector<NewlineChar> {
    std::vector<NewlineChar> newline_chars;

    // We must make a pass over the source.
    char *start_ptr = buffer.get();
    char *fwd_ptr = start_ptr;
    char *end_ptr = start_ptr + src_file_size - 1;

    while (fwd_ptr != end_ptr) {
        // For each character in the buffer, we can check if it forms a newline
        // character. Chocopy supports three types of newline characters: '\n',
        // '\r', and '\r\n'.
        if (*fwd_ptr == '\n') {
            newline_chars.emplace_back(fwd_ptr - start_ptr, 1);
            ++fwd_ptr;
        } else if (*fwd_ptr == '\r') {
            // Check for '\r\n' newline characters
            if (fwd_ptr[1] == '\n') {
                newline_chars.emplace_back(fwd_ptr - start_ptr, 2);
                fwd_ptr += 2;
            } else {
                newline_chars.emplace_back(fwd_ptr - start_ptr, 1);
                ++fwd_ptr;
            }
        } else {
            ++fwd_ptr;
        }
    }

    return newline_chars;
}

// This method takes an offset to a position within in a source file and returns
// the line and column information associated with that position. It is mainly
// used for error reporting.
auto SourceFile::get_location(size_t offset) const -> std::pair<int, int> {
    // Here, we will first check if there is only one line. If there is only one
    // line, we know that the column lies within this line. Since columns are
    // 1-indexed, we need to add 1 to the offset.
    if (!newline_chars.size())
        return std::make_pair(1, offset + 1);

    // If there is more than one line, we need to locate the file in which this
    // current line resides. Since we know that the new-line characters will be
    // monotonically increasing by offset, we can use a binary search. We want
    // to find the first new-line character with an offset greater than the
    // offset we are looking for. The new-line character before that will be our
    // desired line.
    auto cmp = [](size_t offset, const std::pair<size_t, int> &newline_char) {
        return offset <= newline_char.first;
    };

    auto newline_greater_than = std::upper_bound(
        newline_chars.begin(), newline_chars.end(), offset, cmp);

    // If we get an end iterator, it means that the desired offset is in the
    // last line.
    if (newline_greater_than == newline_chars.end()) {
        int line_no = newline_chars.size() + 1;
        // To compute the column number, we must subtract the offset from the
        // start of the last new line character and add one.
        auto &[last_newline_pos, last_newline_size] = newline_chars.back();

        int col_no = offset - (last_newline_pos + last_newline_size) + 1;

        return std::make_pair(line_no, col_no);
    }

    // Here, we need to compute the line number. Here, the line number will be
    // the index of the first newline greater than the target offset, plus 1.
    int line_no = newline_greater_than - newline_chars.begin() + 1;

    // To compute the column number, we must subtract the offset from the
    // start of the last new line character and add one.
    auto &[last_newline_pos, last_newline_size] = *(newline_greater_than - 1);

    int col_no = offset - (last_newline_pos + last_newline_pos) + 1;

    return std::make_pair(line_no, col_no);
}
} // namespace chocopyc::Source