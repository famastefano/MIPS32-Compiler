#pragma once

#include <mips32/token.hpp>

#include <string>
#include <vector>

namespace mips32
{

/*
 * Divide a file into a sequence of tokens.
 * 
 * #!#!#! WARNING
 * This class owns the memory of the tokens
 * and their data. Do NOT free any pointers.
 * !#!#!#
 */
class Tokenizer
{
public:
  Tokenizer();

  /*
   * Loads the file into memory,
   * ready to be divided into tokens
   * 
   * `path` file path
   * 
   * returns `true`, if the task succeed
   *         `false` otherwise
   */
  bool tokenize( char *path ) noexcept;

  /*
   * Check if the file has more tokens to get.
   */
  bool has_token() noexcept;
  
  /*
   * Get the next token.
   * 
   * `tok` will hold the new token
   * 
   * returns `true`, `tok` has the new token
   *         `false`, no tokens are available,
   *                  `tok` is untouched
   */
  bool get( Token& tok ) noexcept;

  void rewind() noexcept;

private:
  void parse_new_tokens() noexcept;

  inline static constexpr int tok_limit{ 256 };

  std::string file;
  std::size_t file_pos = 0;

  std::vector<Token> tokens;
  std::size_t tok_pos = 0;
};

}