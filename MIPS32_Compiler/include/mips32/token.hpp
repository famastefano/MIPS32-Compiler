#pragma once

#include <cstddef>
#include <cstdint>

namespace mips32
{

struct Token
{
  enum
  {
    UNKNOWN = 0,

    LABEL_DECL,
    LABEL_REF,
    DIRECTIVE,
    INSTRUCTION,
    INT_REGISTER,
    FLOAT_REGISTER,
    COMMENT,
    NUMBER,
    STRING,
    COMMA,     // no data
    LEFT_PAR,  // no data
    RIGHT_PAR, // no data
    NEWLINE,   // no data
  } type = UNKNOWN;

  union
  {
    // register
    std::uint32_t reg;

    // numbers
    double num;

    // label decl/ref
    std::size_t hash;

    // directive, instruction
    int code;
    
    // non owning view
    // string or comment
    struct
    {
      char const *data;
      int size;
    } str;
  };
};

}