#include "tokenizer.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <fstream>
#include <sstream>
#include <string_view>

namespace mips32
{
  Token extract_token(std::string const& file, std::size_t& file_pos) noexcept;
  Token extract_directive( std::string const& file, std::size_t& file_pos ) noexcept;
  Token extract_register( std::string const& file, std::size_t& file_pos ) noexcept;
  Token extract_number( std::string const& file, std::size_t& file_pos ) noexcept;
  Token extract_comment( std::string const& file, std::size_t& file_pos ) noexcept;
  Token extract_string( std::string const& file, std::size_t& file_pos ) noexcept;
  Token extract_label_or_inst( std::string const& file, std::size_t& file_pos ) noexcept;

  void skip_whitespace( std::string const& file, std::size_t& file_pos ) noexcept;
  std::size_t advance( std::string const& file, std::size_t& file_pos ) noexcept;
  int instruction_offset( std::size_t hash ) noexcept;
}

bool mips32::Tokenizer::load( char * path ) noexcept
{
  std::ifstream in{ path };
  if ( !in.is_open() )
    return false;

  return load( in );
}

bool mips32::Tokenizer::load( std::istream & in ) noexcept
{
  std::stringstream ss;
  ss << in.rdbuf();

  file = std::move( ss.str() );

  tokens.clear();
  tokens.reserve( tok_limit );

  file_pos = 0;
  tok_pos = 0;

  return !file.empty();
}

bool mips32::Tokenizer::has_token() noexcept
{
  return file_pos < file.size() || tok_pos < tokens.size();
}

bool mips32::Tokenizer::get( Token & tok ) noexcept
{
  if ( !has_token() )
    return false;

  if ( tok_pos == tokens.size() )
    parse_new_tokens();
  
  tok = tokens[tok_pos++];

  return true;
}

void mips32::Tokenizer::rewind() noexcept
{
  file_pos = 0;
  tok_pos = 0;
  tokens.clear();
}

void mips32::Tokenizer::parse_new_tokens() noexcept
{
  if ( !has_token() )
    return;

  tokens.clear();
  tok_pos = 0;
  
  while ( has_token() && tokens.size() < tok_limit )
  {
    auto tok = extract_token(file, file_pos);
    tokens.emplace_back( tok );
  }
}

mips32::Token mips32::extract_token(std::string const& file, std::size_t& file_pos) noexcept
{
  skip_whitespace( file, file_pos );

  char c = file[file_pos];

  switch ( c )
  {
  case '.':
    return extract_directive( file, file_pos );
  case '$':
    return extract_register( file, file_pos );
  case '#':
    return extract_comment( file, file_pos );
  case '"':
    return extract_string( file, file_pos );
  case '(':
    ++file_pos;
    return Token{ Token::LEFT_PAR };
  case ')':
    ++file_pos;
    return Token{ Token::RIGHT_PAR };
  case '\n':
    ++file_pos;
    return Token{ Token::NEWLINE };
  case ',':
    ++file_pos;
    return Token{ Token::COMMA };
  }

  if ( isdigit( c ) || c == '+' || c == '-' )
    return extract_number( file, file_pos );
  else if ( isalpha( c ) || c == '_' )
    return extract_label_or_inst( file, file_pos );

  return Token{};
}

#define MIPS32_HASH(sv) (std::hash<std::string_view>{}(sv))

mips32::Token mips32::extract_directive( std::string const& file, std::size_t& file_pos ) noexcept
{
  using namespace std::string_view_literals;

  static const std::array<std::size_t, 14> dir_hashtable_lo
  {
    MIPS32_HASH( ".word"sv ),
    MIPS32_HASH( ".text"sv ),
    MIPS32_HASH( ".space"sv ),
    MIPS32_HASH( ".ktext"sv ),
    MIPS32_HASH( ".kdata"sv ),
    MIPS32_HASH( ".half"sv ),
    MIPS32_HASH( ".globl"sv ),
    MIPS32_HASH( ".float"sv ),
    MIPS32_HASH( ".double"sv ),
    MIPS32_HASH( ".data"sv ),
    MIPS32_HASH( ".byte"sv ),
    MIPS32_HASH( ".asciiz"sv ),
    MIPS32_HASH( ".ascii"sv ),
    MIPS32_HASH( ".align"sv ),

  };
  static const std::array<std::size_t, 14> dir_hashtable_hi
  {
    MIPS32_HASH( ".WORD"sv ),
    MIPS32_HASH( ".TEXT"sv ),
    MIPS32_HASH( ".SPACE"sv ),
    MIPS32_HASH( ".KTEXT"sv ),
    MIPS32_HASH( ".KDATA"sv ),
    MIPS32_HASH( ".HALF"sv ),
    MIPS32_HASH( ".GLOBL"sv ),
    MIPS32_HASH( ".FLOAT"sv ),
    MIPS32_HASH( ".DOUBLE"sv ),
    MIPS32_HASH( ".DATA"sv ),
    MIPS32_HASH( ".BYTE"sv ),
    MIPS32_HASH( ".ASCIIZ"sv ),
    MIPS32_HASH( ".ASCII"sv ),
    MIPS32_HASH( ".ALIGN"sv ),
  };

  ++file_pos; // advancing breaks on '.' and a directive starts with it
  auto old_pos = advance( file, file_pos ) - 1;

  Token tok;
  auto view = std::string_view( file.data() + old_pos, file_pos - old_pos );
  auto hash = MIPS32_HASH( view );

  auto d_lo_end = dir_hashtable_lo.cend();
  auto d_hi_end = dir_hashtable_hi.cend();

  auto d_lo = std::find( dir_hashtable_lo.cbegin(), d_lo_end, hash );
  auto d_hi = std::find( dir_hashtable_hi.cbegin(), d_hi_end, hash );

  if ( d_lo != d_lo_end )
  {
    tok.type = tok.DIRECTIVE;
    tok.code = d_lo_end - d_lo - 1;
  }
  else if ( d_hi != d_hi_end )
  {
    tok.type = tok.DIRECTIVE;
    tok.code = d_hi_end - d_hi - 1;
  }

  return tok;
}

mips32::Token mips32::extract_register( std::string const& file, std::size_t& file_pos ) noexcept
{
  using namespace std::string_view_literals;

  static const std::array<std::size_t, 32> integer_hashtable_as_num
  {
    MIPS32_HASH( "$31"sv ),MIPS32_HASH( "$30"sv ),MIPS32_HASH( "$29"sv ),MIPS32_HASH( "$28"sv ),
    MIPS32_HASH( "$27"sv ),MIPS32_HASH( "$26"sv ),MIPS32_HASH( "$25"sv ),MIPS32_HASH( "$24"sv ),
    MIPS32_HASH( "$23"sv ),MIPS32_HASH( "$22"sv ),MIPS32_HASH( "$21"sv ),MIPS32_HASH( "$20"sv ),
    MIPS32_HASH( "$19"sv ),MIPS32_HASH( "$18"sv ),MIPS32_HASH( "$17"sv ),MIPS32_HASH( "$16"sv ),
    MIPS32_HASH( "$15"sv ),MIPS32_HASH( "$14"sv ),MIPS32_HASH( "$13"sv ),MIPS32_HASH( "$12"sv ),
    MIPS32_HASH( "$11"sv ),MIPS32_HASH( "$10"sv ),MIPS32_HASH( "$9"sv ),MIPS32_HASH( "$8"sv ),
    MIPS32_HASH( "$7"sv ),MIPS32_HASH( "$6"sv ),MIPS32_HASH( "$5"sv ),MIPS32_HASH( "$4"sv ),
    MIPS32_HASH( "$3"sv ),MIPS32_HASH( "$2"sv ),MIPS32_HASH( "$1"sv ),MIPS32_HASH( "$0"sv ),
  };

  static const std::array<std::size_t, 32> integer_hashtable_as_string
  {
    MIPS32_HASH( "$ra"sv ),MIPS32_HASH( "$s8"sv ),MIPS32_HASH( "$sp"sv ),MIPS32_HASH( "$gp"sv ),
    MIPS32_HASH( "$k1"sv ),MIPS32_HASH( "$k0"sv ),MIPS32_HASH( "$t9"sv ),MIPS32_HASH( "$t8"sv ),
    MIPS32_HASH( "$s7"sv ),MIPS32_HASH( "$s6"sv ),MIPS32_HASH( "$s5"sv ),MIPS32_HASH( "$s4"sv ),
    MIPS32_HASH( "$s3"sv ),MIPS32_HASH( "$s2"sv ),MIPS32_HASH( "$s1"sv ),MIPS32_HASH( "$s0"sv ),
    MIPS32_HASH( "$t7"sv ),MIPS32_HASH( "$t6"sv ),MIPS32_HASH( "$t5"sv ),MIPS32_HASH( "$t4"sv ),
    MIPS32_HASH( "$t3"sv ),MIPS32_HASH( "$t2"sv ),MIPS32_HASH( "$t1"sv ),MIPS32_HASH( "$t0"sv ),
    MIPS32_HASH( "$a3"sv ),MIPS32_HASH( "$a2"sv ),MIPS32_HASH( "$a1"sv ),MIPS32_HASH( "$a0"sv ),
    MIPS32_HASH( "$v1"sv ),MIPS32_HASH( "$v0"sv ),MIPS32_HASH( "$at"sv ),MIPS32_HASH( "$zero"sv ),
  };

  static const std::array<std::size_t, 32> float_hashtable
  {
    MIPS32_HASH( "$f31"sv ),MIPS32_HASH( "$f30"sv ),MIPS32_HASH( "$f29"sv ),MIPS32_HASH( "$f28"sv ),
    MIPS32_HASH( "$f27"sv ),MIPS32_HASH( "$f26"sv ),MIPS32_HASH( "$f25"sv ),MIPS32_HASH( "$f24"sv ),
    MIPS32_HASH( "$f23"sv ),MIPS32_HASH( "$f22"sv ),MIPS32_HASH( "$f21"sv ),MIPS32_HASH( "$f20"sv ),
    MIPS32_HASH( "$f19"sv ),MIPS32_HASH( "$f18"sv ),MIPS32_HASH( "$f17"sv ),MIPS32_HASH( "$f16"sv ),
    MIPS32_HASH( "$f15"sv ),MIPS32_HASH( "$f14"sv ),MIPS32_HASH( "$f13"sv ),MIPS32_HASH( "$f12"sv ),
    MIPS32_HASH( "$f11"sv ),MIPS32_HASH( "$f10"sv ),MIPS32_HASH( "$f9"sv ),MIPS32_HASH( "$f8"sv ),
    MIPS32_HASH( "$f7"sv ),MIPS32_HASH( "$f6"sv ),MIPS32_HASH( "$f5"sv ),MIPS32_HASH( "$f4"sv ),
    MIPS32_HASH( "$f3"sv ),MIPS32_HASH( "$f2"sv ),MIPS32_HASH( "$f1"sv ),MIPS32_HASH( "$f0"sv ),
  };

  auto old_pos = advance( file, file_pos );

  auto view = std::string_view( file.data() + old_pos, file_pos - old_pos );
  auto hash = MIPS32_HASH( view );

  Token tok;

  auto n_end = integer_hashtable_as_num.cend();
  auto h_end = integer_hashtable_as_string.cend();
  auto f_end = float_hashtable.cend();

  auto n_reg = std::find( integer_hashtable_as_num.cbegin(), n_end, hash );
  auto h_reg = std::find( integer_hashtable_as_string.cbegin(), h_end, hash );
  auto f_reg = std::find( float_hashtable.cbegin(), f_end, hash );

  if ( n_reg != n_end )
  {
    tok.type = tok.INT_REGISTER;
    tok.reg = n_end - n_reg - 1;
  }
  else if ( h_reg != h_end )
  {
    tok.type = tok.INT_REGISTER;
    tok.reg = h_end - h_reg - 1;
  }
  else if ( f_reg != f_end )
  {
    tok.type = tok.FLOAT_REGISTER;
    tok.reg = f_end - f_reg - 1;
  }
  
  return tok;
}

mips32::Token mips32::extract_number( std::string const& file, std::size_t& file_pos ) noexcept
{
  auto old_pos = advance( file, file_pos );

  auto num_str = file.substr( old_pos, file_pos - old_pos );

  // Remove `'` from the number, as it is allowed as separator for us but not for the STL
  num_str.erase( std::remove( num_str.begin(), num_str.end(), '\'' ), num_str.end() );

  std::istringstream iss;
  iss.str( num_str );

  Token tok;

  if ( num_str.find( '.' ) != num_str.npos ) // possible floating point
  {
    if ( iss >> tok.f64 )
    {
      tok.type = tok.FLOAT;
      return tok;
    }
  }

  // Possible integer
  int pos = 0;
  if ( num_str[0] == '+' || num_str[0] == '-' )
  {
    if ( num_str.size() == 1 ) return tok;
    else pos = 1;
  }

  if ( num_str[pos] == '0' ) // Hex or Oct?
  {
    if ( num_str.size() == 1 + pos )
      tok.u32 = 0;
    else if ( tolower( num_str[pos + 1] ) == 'x' )
      iss >> std::hex >> tok.u32; // Base 16
    else
      iss >> std::oct >> tok.u32; // Base 8
  }
  else
    iss >> std::dec >> tok.u32;   // Base 10

  if ( iss )
    tok.type = tok.INTEGER;

  return tok;
}

mips32::Token mips32::extract_comment( std::string const& file, std::size_t& file_pos ) noexcept
{
  Token tok;
  tok.type = tok.COMMENT;
  tok.str = {};

  auto old_pos = file_pos;
  file_pos = file.find( '\n', file_pos );

  char const *start, *end;

  start = file.data() + old_pos;

  if ( file_pos != file.npos )
    end = start + file_pos;
  else
    end = file.data() + file.size();

  tok.str.data = start;
  tok.str.size = end - start;

  return tok;
}

mips32::Token mips32::extract_string( std::string const& file, std::size_t& file_pos ) noexcept
{
  auto new_pos = file_pos + 1;

  while ( true )
  {
    new_pos = file.find( '"', new_pos );

    if ( new_pos == file.npos )
      return Token{};

    if ( file[new_pos - 1] != '\\' ) // check for \" escape
      break;
    else
      ++new_pos;
  }

  Token tok;

  tok.type = tok.STRING;
  tok.str = {};
  tok.str.data = file.data() + file_pos;
  tok.str.size = file.data() + new_pos - tok.str.data;

  file_pos = new_pos + 1;

  return tok;
}

mips32::Token mips32::extract_label_or_inst( std::string const& file, std::size_t& file_pos ) noexcept
{
  auto old_pos = advance( file, file_pos );

  auto tok_str = file.substr( old_pos, file_pos - old_pos );

  std::size_t normal_hash, uppercase_hash;

  normal_hash = MIPS32_HASH( tok_str );

  for ( auto & c : tok_str )
    c = toupper( c );

  uppercase_hash = MIPS32_HASH( tok_str );

  Token tok;

  if ( tok_str.back() == ':' )
  {
    tok.hash = normal_hash;
    tok.type = tok.LABEL_DECL;
  }
  else if ( auto code = instruction_offset( uppercase_hash ); code != -1 )
  {
    tok.code = code;
    tok.type = tok.INSTRUCTION;
  }
  else
  {
    tok.hash = normal_hash;
    tok.type = tok.LABEL_REF;
  }

  return tok;
}

void mips32::skip_whitespace( std::string const & file, std::size_t & file_pos ) noexcept
{
  while ( file_pos < file.size() && ( file[file_pos] == ' ' || file[file_pos] == '\t' ) )
    ++file_pos;
}

std::size_t mips32::advance( std::string const & file, std::size_t& file_pos ) noexcept
{
  auto old_pos = file_pos;

  while ( file_pos != file.size() )
  {
    switch ( file[file_pos] )
    {
    case ':':
      ++file_pos; // we need it for the label declaration
    case '\n':
    case ',':
    case ' ':
    case '\t':
    case '(':
    case ')':
      goto _exit;
    }

    ++file_pos;
  }

_exit:
  return old_pos;
}

int mips32::instruction_offset( std::size_t hash ) noexcept
{
  static const std::array<std::size_t, 513> inst_hashtable
  {
    MIPS32_HASH( "ABS.S" ),
    MIPS32_HASH( "ABS.D" ),
    MIPS32_HASH( "ADD" ),
    MIPS32_HASH( "ADD.S" ),
    MIPS32_HASH( "ADD.D" ),
    MIPS32_HASH( "ADDI" ),
    MIPS32_HASH( "ADDIU" ),
    MIPS32_HASH( "ADDIUPC" ),
    MIPS32_HASH( "ADDU" ),
    MIPS32_HASH( "ALIGN" ),
    MIPS32_HASH( "ALNV.PS" ),
    MIPS32_HASH( "ALUIPC" ),
    MIPS32_HASH( "AND" ),
    MIPS32_HASH( "ANDI" ),
    MIPS32_HASH( "AUI" ),
    MIPS32_HASH( "AUIPC" ),
    MIPS32_HASH( "B" ),
    MIPS32_HASH( "BAL" ),
    MIPS32_HASH( "BALC" ),
    MIPS32_HASH( "BC" ),
    MIPS32_HASH( "BCEQZ" ),
    MIPS32_HASH( "BCNEZ" ),
    MIPS32_HASH( "BCF" ),
    MIPS32_HASH( "BCFL" ),
    MIPS32_HASH( "BCT" ),
    MIPS32_HASH( "BCTL" ),
    MIPS32_HASH( "BEQ" ),
    MIPS32_HASH( "BEQL" ),
    MIPS32_HASH( "BGEZ" ),
    MIPS32_HASH( "BGEZAL" ),
    MIPS32_HASH( "BLEZALC" ),
    MIPS32_HASH( "BGEZALC" ),
    MIPS32_HASH( "BGTZALC" ),
    MIPS32_HASH( "BLTZALC" ),
    MIPS32_HASH( "BEQZALC" ),
    MIPS32_HASH( "BNEZALC" ),
    MIPS32_HASH( "BGEZALL" ),
    MIPS32_HASH( "BEQC" ),
    MIPS32_HASH( "BNEC" ),
    MIPS32_HASH( "BLTC" ),
    MIPS32_HASH( "BGEC" ),
    MIPS32_HASH( "BLTUC" ),
    MIPS32_HASH( "BGEUC" ),
    MIPS32_HASH( "BGTC" ),
    MIPS32_HASH( "BLEC" ),
    MIPS32_HASH( "BGTUC" ),
    MIPS32_HASH( "BLEUC" ),
    MIPS32_HASH( "BLTZC" ),
    MIPS32_HASH( "BLEZC" ),
    MIPS32_HASH( "BGEZC" ),
    MIPS32_HASH( "BGTZC" ),
    MIPS32_HASH( "BEQZC" ),
    MIPS32_HASH( "BNEZC" ),
    MIPS32_HASH( "BGEZL" ),
    MIPS32_HASH( "BGTZ" ),
    MIPS32_HASH( "BGTZL" ),
    MIPS32_HASH( "BITSWAP" ),
    MIPS32_HASH( "BLEZ" ),
    MIPS32_HASH( "BLEZL" ),
    MIPS32_HASH( "BLTZ" ),
    MIPS32_HASH( "BLTZAL" ),
    MIPS32_HASH( "BLTZALL" ),
    MIPS32_HASH( "BLTZL" ),
    MIPS32_HASH( "BNE" ),
    MIPS32_HASH( "BNEL" ),
    MIPS32_HASH( "BOVC" ),
    MIPS32_HASH( "BNVC" ),
    MIPS32_HASH( "BREAK" ),
    MIPS32_HASH( "C.F.S" ),
    MIPS32_HASH( "C.F.D" ),
    MIPS32_HASH( "C.F.PS" ),
    MIPS32_HASH( "C.UN.S" ),
    MIPS32_HASH( "C.UN.D" ),
    MIPS32_HASH( "C.UN.PS" ),
    MIPS32_HASH( "C.EQ.S" ),
    MIPS32_HASH( "C.EQ.D" ),
    MIPS32_HASH( "C.EQ.PS" ),
    MIPS32_HASH( "C.UEQ.S" ),
    MIPS32_HASH( "C.UEQ.D" ),
    MIPS32_HASH( "C.UEQ.PS" ),
    MIPS32_HASH( "C.OLT.S" ),
    MIPS32_HASH( "C.OLT.D" ),
    MIPS32_HASH( "C.OLT.PS" ),
    MIPS32_HASH( "C.ULT.S" ),
    MIPS32_HASH( "C.ULT.D" ),
    MIPS32_HASH( "C.ULT.PS" ),
    MIPS32_HASH( "C.OLE.S" ),
    MIPS32_HASH( "C.OLE.D" ),
    MIPS32_HASH( "C.OLE.PS" ),
    MIPS32_HASH( "C.ULE.S" ),
    MIPS32_HASH( "C.ULE.D" ),
    MIPS32_HASH( "C.ULE.PS" ),
    MIPS32_HASH( "C.SF.S" ),
    MIPS32_HASH( "C.SF.D" ),
    MIPS32_HASH( "C.SF.PS" ),
    MIPS32_HASH( "C.NGLE.S" ),
    MIPS32_HASH( "C.NGLE.D" ),
    MIPS32_HASH( "C.NGLE.PS" ),
    MIPS32_HASH( "C.SEQ.S" ),
    MIPS32_HASH( "C.SEQ.D" ),
    MIPS32_HASH( "C.SEQ.PS" ),
    MIPS32_HASH( "C.NGL.S" ),
    MIPS32_HASH( "C.NGL.D" ),
    MIPS32_HASH( "C.NGL.PS" ),
    MIPS32_HASH( "C.LT.S" ),
    MIPS32_HASH( "C.LT.D" ),
    MIPS32_HASH( "C.LT.PS" ),
    MIPS32_HASH( "C.NGE.S" ),
    MIPS32_HASH( "C.NGE.D" ),
    MIPS32_HASH( "C.NGE.PS" ),
    MIPS32_HASH( "C.LE.S" ),
    MIPS32_HASH( "C.LE.D" ),
    MIPS32_HASH( "C.LE.PS" ),
    MIPS32_HASH( "C.NGT.S" ),
    MIPS32_HASH( "C.NGT.D" ),
    MIPS32_HASH( "C.NGT.PS" ),
    MIPS32_HASH( "C.T.S" ),
    MIPS32_HASH( "C.T.D" ),
    MIPS32_HASH( "C.T.PS" ),
    MIPS32_HASH( "C.OR.S" ),
    MIPS32_HASH( "C.OR.D" ),
    MIPS32_HASH( "C.OR.PS" ),
    MIPS32_HASH( "C.NEQ.S" ),
    MIPS32_HASH( "C.NEQ.D" ),
    MIPS32_HASH( "C.NEQ.PS" ),
    MIPS32_HASH( "C.OGL.S" ),
    MIPS32_HASH( "C.OGL.D" ),
    MIPS32_HASH( "C.OGL.PS" ),
    MIPS32_HASH( "C.UGE.S" ),
    MIPS32_HASH( "C.UGE.D" ),
    MIPS32_HASH( "C.UGE.PS" ),
    MIPS32_HASH( "C.OGE.S" ),
    MIPS32_HASH( "C.OGE.D" ),
    MIPS32_HASH( "C.OGE.PS" ),
    MIPS32_HASH( "C.UGT.S" ),
    MIPS32_HASH( "C.UGT.D" ),
    MIPS32_HASH( "C.UGT.PS" ),
    MIPS32_HASH( "C.OGT.S" ),
    MIPS32_HASH( "C.OGT.D" ),
    MIPS32_HASH( "C.OGT.PS" ),
    MIPS32_HASH( "C.ST.S" ),
    MIPS32_HASH( "C.ST.D" ),
    MIPS32_HASH( "C.ST.PS" ),
    MIPS32_HASH( "C.GLE.S" ),
    MIPS32_HASH( "C.GLE.D" ),
    MIPS32_HASH( "C.GLE.PS" ),
    MIPS32_HASH( "C.SNE.S" ),
    MIPS32_HASH( "C.SNE.D" ),
    MIPS32_HASH( "C.SNE.PS" ),
    MIPS32_HASH( "C.GL.S" ),
    MIPS32_HASH( "C.GL.D" ),
    MIPS32_HASH( "C.GL.PS" ),
    MIPS32_HASH( "C.NLT.S" ),
    MIPS32_HASH( "C.NLT.D" ),
    MIPS32_HASH( "C.NLT.PS" ),
    MIPS32_HASH( "C.GE.S" ),
    MIPS32_HASH( "C.GE.D" ),
    MIPS32_HASH( "C.GE.PS" ),
    MIPS32_HASH( "C.NLE.S" ),
    MIPS32_HASH( "C.NLE.D" ),
    MIPS32_HASH( "C.NLE.PS" ),
    MIPS32_HASH( "C.GT.S" ),
    MIPS32_HASH( "C.GT.D" ),
    MIPS32_HASH( "C.GT.PS" ),
    MIPS32_HASH( "CACHE" ),
    MIPS32_HASH( "CACHEE" ),
    MIPS32_HASH( "CEIL.L.S" ),
    MIPS32_HASH( "CEIL.L.D" ),
    MIPS32_HASH( "CEIL.W.S" ),
    MIPS32_HASH( "CEIL.W.D" ),
    MIPS32_HASH( "CFC" ),
    MIPS32_HASH( "CLASS.S" ),
    MIPS32_HASH( "CLASS.D" ),
    MIPS32_HASH( "CLO" ),
    MIPS32_HASH( "CLZ" ),
    MIPS32_HASH( "CMP.AF.S" ),
    MIPS32_HASH( "CMP.AF.D" ),
    MIPS32_HASH( "CMP.UN.S" ),
    MIPS32_HASH( "CMP.UN.D" ),
    MIPS32_HASH( "CMP.EQ.S" ),
    MIPS32_HASH( "CMP.EQ.D" ),
    MIPS32_HASH( "CMP.UEQ.S" ),
    MIPS32_HASH( "CMP.UEQ.D" ),
    MIPS32_HASH( "CMP.LT.S" ),
    MIPS32_HASH( "CMP.LT.D" ),
    MIPS32_HASH( "CMP.ULT.S" ),
    MIPS32_HASH( "CMP.ULT.D" ),
    MIPS32_HASH( "CMP.LE.S" ),
    MIPS32_HASH( "CMP.LE.D" ),
    MIPS32_HASH( "CMP.ULE.S" ),
    MIPS32_HASH( "CMP.ULE.D" ),
    MIPS32_HASH( "CMP.SAF.S" ),
    MIPS32_HASH( "CMP.SAF.D" ),
    MIPS32_HASH( "CMP.SUN.S" ),
    MIPS32_HASH( "CMP.SUN.D" ),
    MIPS32_HASH( "CMP.SEQ.S" ),
    MIPS32_HASH( "CMP.SEQ.D" ),
    MIPS32_HASH( "CMP.SUEQ.S" ),
    MIPS32_HASH( "CMP.SUEQ.D" ),
    MIPS32_HASH( "CMP.SLT.S" ),
    MIPS32_HASH( "CMP.SLT.D" ),
    MIPS32_HASH( "CMP.SULT.S" ),
    MIPS32_HASH( "CMP.SULT.D" ),
    MIPS32_HASH( "CMP.SLE.S" ),
    MIPS32_HASH( "CMP.SLE.D" ),
    MIPS32_HASH( "CMP.SULE.S" ),
    MIPS32_HASH( "CMP.SULE.D" ),
    MIPS32_HASH( "CMP.AT.S" ),
    MIPS32_HASH( "CMP.AT.D" ),
    MIPS32_HASH( "CMP.OR.S" ),
    MIPS32_HASH( "CMP.OR.D" ),
    MIPS32_HASH( "CMP.UNE.S" ),
    MIPS32_HASH( "CMP.UNE.D" ),
    MIPS32_HASH( "CMP.NE.S" ),
    MIPS32_HASH( "CMP.NE.D" ),
    MIPS32_HASH( "CMP.UGE.S" ),
    MIPS32_HASH( "CMP.UGE.D" ),
    MIPS32_HASH( "CMP.OGE.S" ),
    MIPS32_HASH( "CMP.OGE.D" ),
    MIPS32_HASH( "CMP.UGT.S" ),
    MIPS32_HASH( "CMP.UGT.D" ),
    MIPS32_HASH( "CMP.OGT.S" ),
    MIPS32_HASH( "CMP.OGT.D" ),
    MIPS32_HASH( "CMP.SAT.S" ),
    MIPS32_HASH( "CMP.SAT.D" ),
    MIPS32_HASH( "CMP.SOR.S" ),
    MIPS32_HASH( "CMP.SOR.D" ),
    MIPS32_HASH( "CMP.SUNE.S" ),
    MIPS32_HASH( "CMP.SUNE.D" ),
    MIPS32_HASH( "CMP.SNE.S" ),
    MIPS32_HASH( "CMP.SNE.D" ),
    MIPS32_HASH( "CMP.SUGE.S" ),
    MIPS32_HASH( "CMP.SUGE.D" ),
    MIPS32_HASH( "CMP.SOGE.S" ),
    MIPS32_HASH( "CMP.SOGE.D" ),
    MIPS32_HASH( "CMP.SUGT.S" ),
    MIPS32_HASH( "CMP.SUGT.D" ),
    MIPS32_HASH( "CMP.SOGT.S" ),
    MIPS32_HASH( "CMP.SOGT.D" ),
    MIPS32_HASH( "COP" ),
    MIPS32_HASH( "CRCB" ),
    MIPS32_HASH( "CRCH" ),
    MIPS32_HASH( "CRCW" ),
    MIPS32_HASH( "CRCCB" ),
    MIPS32_HASH( "CRCCH" ),
    MIPS32_HASH( "CRCCW" ),
    MIPS32_HASH( "CTC1" ),
    MIPS32_HASH( "CTC2" ),
    MIPS32_HASH( "CVT.D.S" ),
    MIPS32_HASH( "CVT.D.W" ),
    MIPS32_HASH( "CVT.D.L" ),
    MIPS32_HASH( "CVT.L.S" ),
    MIPS32_HASH( "CVT.L.D" ),
    MIPS32_HASH( "CVT.PS.S" ),
    MIPS32_HASH( "CVT.S.PL" ),
    MIPS32_HASH( "CVT.S.PU" ),
    MIPS32_HASH( "CVT.S.D" ),
    MIPS32_HASH( "CVT.S.W" ),
    MIPS32_HASH( "CVT.S.L" ),
    MIPS32_HASH( "CVT.W.S" ),
    MIPS32_HASH( "CVT.W.D" ),
    MIPS32_HASH( "DDIV" ),
    MIPS32_HASH( "DDIVU" ),
    MIPS32_HASH( "DERET" ),
    MIPS32_HASH( "DI" ),
    MIPS32_HASH( "DIV" ),
    MIPS32_HASH( "MOD" ),
    MIPS32_HASH( "DIVU" ),
    MIPS32_HASH( "MODU" ),
    MIPS32_HASH( "DIV.S" ),
    MIPS32_HASH( "DIV.D" ),
    MIPS32_HASH( "DVP" ),
    MIPS32_HASH( "EHB" ),
    MIPS32_HASH( "EI" ),
    MIPS32_HASH( "ERET" ),
    MIPS32_HASH( "ERETNC" ),
    MIPS32_HASH( "EVP" ),
    MIPS32_HASH( "EXT" ),
    MIPS32_HASH( "FLOOR.L.S" ),
    MIPS32_HASH( "FLOOR.L.D" ),
    MIPS32_HASH( "FLOOR.W.S" ),
    MIPS32_HASH( "FLOOR.W.D" ),
    MIPS32_HASH( "GINVI" ),
    MIPS32_HASH( "GINVT" ),
    MIPS32_HASH( "INS" ),
    MIPS32_HASH( "J" ),
    MIPS32_HASH( "JAL" ),
    MIPS32_HASH( "JALR" ),
    MIPS32_HASH( "JALR.HB" ),
    MIPS32_HASH( "JALX" ),
    MIPS32_HASH( "JIALC" ),
    MIPS32_HASH( "JIC" ),
    MIPS32_HASH( "JR" ),
    MIPS32_HASH( "JR.HB" ),
    MIPS32_HASH( "LB" ),
    MIPS32_HASH( "LBE" ),
    MIPS32_HASH( "LBU" ),
    MIPS32_HASH( "LBUE" ),
    MIPS32_HASH( "LDC" ),
    MIPS32_HASH( "LDXC1" ),
    MIPS32_HASH( "LH" ),
    MIPS32_HASH( "LHE" ),
    MIPS32_HASH( "LHU" ),
    MIPS32_HASH( "LHUE" ),
    MIPS32_HASH( "LL" ),
    MIPS32_HASH( "LLE" ),
    MIPS32_HASH( "LLWP" ),
    MIPS32_HASH( "LLWPE" ),
    MIPS32_HASH( "LSA" ),
    MIPS32_HASH( "LUI" ),
    MIPS32_HASH( "LUXC1" ),
    MIPS32_HASH( "LW" ),
    MIPS32_HASH( "LWC" ),
    MIPS32_HASH( "LWE" ),
    MIPS32_HASH( "LWL" ),
    MIPS32_HASH( "LWLE" ),
    MIPS32_HASH( "LWPC" ),
    MIPS32_HASH( "LWR" ),
    MIPS32_HASH( "LWRE" ),
    MIPS32_HASH( "LWXC1" ),
    MIPS32_HASH( "MADD" ),
    MIPS32_HASH( "MADD.S" ),
    MIPS32_HASH( "MADD.D" ),
    MIPS32_HASH( "MADD.PS" ),
    MIPS32_HASH( "MADDF.S" ),
    MIPS32_HASH( "MADDF.D" ),
    MIPS32_HASH( "MSUBF.S" ),
    MIPS32_HASH( "MSUBF.D" ),
    MIPS32_HASH( "MADDU" ),
    MIPS32_HASH( "MAX.S" ),
    MIPS32_HASH( "MAX.D" ),
    MIPS32_HASH( "MIN.S" ),
    MIPS32_HASH( "MIN.D" ),
    MIPS32_HASH( "MAXA.S" ),
    MIPS32_HASH( "MAXA.D" ),
    MIPS32_HASH( "MINA.S" ),
    MIPS32_HASH( "MINA.D" ),
    MIPS32_HASH( "MFC0" ),
    MIPS32_HASH( "MFC1" ),
    MIPS32_HASH( "MFC2" ),
    MIPS32_HASH( "MFHC0" ),
    MIPS32_HASH( "MFHC1" ),
    MIPS32_HASH( "MFHC2" ),
    MIPS32_HASH( "MFHI" ),
    MIPS32_HASH( "MFLO" ),
    MIPS32_HASH( "MOV.S" ),
    MIPS32_HASH( "MOV.D" ),
    MIPS32_HASH( "MOV.PS" ),
    MIPS32_HASH( "MOVF" ),
    MIPS32_HASH( "MOVF.S" ),
    MIPS32_HASH( "MOVF.D" ),
    MIPS32_HASH( "MOVF.PS" ),
    MIPS32_HASH( "MOVN" ),
    MIPS32_HASH( "MOVN.S" ),
    MIPS32_HASH( "MOVN.D" ),
    MIPS32_HASH( "MOVN.PS" ),
    MIPS32_HASH( "MOVT" ),
    MIPS32_HASH( "MOVT.S" ),
    MIPS32_HASH( "MOVT.D" ),
    MIPS32_HASH( "MOVT.PS" ),
    MIPS32_HASH( "MOVZ" ),
    MIPS32_HASH( "MOVZ.S" ),
    MIPS32_HASH( "MOVZ.D" ),
    MIPS32_HASH( "MOVZ.PS" ),
    MIPS32_HASH( "MSUB" ),
    MIPS32_HASH( "MSUB.S" ),
    MIPS32_HASH( "MSUB.D" ),
    MIPS32_HASH( "MSUB.PS" ),
    MIPS32_HASH( "MSUBU" ),
    MIPS32_HASH( "MTC0" ),
    MIPS32_HASH( "MTC1" ),
    MIPS32_HASH( "MTC2" ),
    MIPS32_HASH( "MTHC0" ),
    MIPS32_HASH( "MTHC1" ),
    MIPS32_HASH( "MTHC2" ),
    MIPS32_HASH( "MTHI" ),
    MIPS32_HASH( "MTLO" ),
    MIPS32_HASH( "MUL" ),
    MIPS32_HASH( "MUH" ),
    MIPS32_HASH( "MULU" ),
    MIPS32_HASH( "MUHU" ),
    MIPS32_HASH( "MUL.S" ),
    MIPS32_HASH( "MUL.D" ),
    MIPS32_HASH( "MUL.PS" ),
    MIPS32_HASH( "MULT" ),
    MIPS32_HASH( "MULTU" ),
    MIPS32_HASH( "NAL" ),
    MIPS32_HASH( "NEG.S" ),
    MIPS32_HASH( "NEG.D" ),
    MIPS32_HASH( "NEG.PS" ),
    MIPS32_HASH( "NMADD.S" ),
    MIPS32_HASH( "NMADD.D" ),
    MIPS32_HASH( "NMADD.PS" ),
    MIPS32_HASH( "NMSUB.S" ),
    MIPS32_HASH( "NMSUB.D" ),
    MIPS32_HASH( "NMSUB.PS" ),
    MIPS32_HASH( "NOP" ),
    MIPS32_HASH( "NOR" ),
    MIPS32_HASH( "OR" ),
    MIPS32_HASH( "ORI" ),
    MIPS32_HASH( "PAUSE" ),
    MIPS32_HASH( "PLL.PS" ),
    MIPS32_HASH( "PLU.PS" ),
    MIPS32_HASH( "PREF" ),
    MIPS32_HASH( "PREFE" ),
    MIPS32_HASH( "PREFX" ),
    MIPS32_HASH( "PUL.PS" ),
    MIPS32_HASH( "PUU.PS" ),
    MIPS32_HASH( "RDHWR" ),
    MIPS32_HASH( "RDPGPR" ),
    MIPS32_HASH( "RECIP.S" ),
    MIPS32_HASH( "RECIP.D" ),
    MIPS32_HASH( "RINT.S" ),
    MIPS32_HASH( "RINT.D" ),
    MIPS32_HASH( "ROTR" ),
    MIPS32_HASH( "ROTRV" ),
    MIPS32_HASH( "ROUND.L.S" ),
    MIPS32_HASH( "ROUND.L.D" ),
    MIPS32_HASH( "ROUND.W.S" ),
    MIPS32_HASH( "ROUND.W.D" ),
    MIPS32_HASH( "RSQRT.S" ),
    MIPS32_HASH( "RSQRT.D" ),
    MIPS32_HASH( "SB" ),
    MIPS32_HASH( "SBE" ),
    MIPS32_HASH( "SC" ),
    MIPS32_HASH( "SCE" ),
    MIPS32_HASH( "SCWP" ),
    MIPS32_HASH( "SCWPE" ),
    MIPS32_HASH( "SDBBP" ),
    MIPS32_HASH( "SDC" ),
    MIPS32_HASH( "SDXC" ),
    MIPS32_HASH( "SEB" ),
    MIPS32_HASH( "SEH" ),
    MIPS32_HASH( "SEL.S" ),
    MIPS32_HASH( "SEL.D" ),
    MIPS32_HASH( "SELEQZ" ),
    MIPS32_HASH( "SELNEZ" ),
    MIPS32_HASH( "SELEQZ.S" ),
    MIPS32_HASH( "SELEQZ.D" ),
    MIPS32_HASH( "SELNEQZ.S" ),
    MIPS32_HASH( "SELNEQZ.D" ),
    MIPS32_HASH( "SH" ),
    MIPS32_HASH( "SHE" ),
    MIPS32_HASH( "SIGRIE" ),
    MIPS32_HASH( "SLL" ),
    MIPS32_HASH( "SLLV" ),
    MIPS32_HASH( "SLT" ),
    MIPS32_HASH( "SLTI" ),
    MIPS32_HASH( "SLTIU" ),
    MIPS32_HASH( "SLTU" ),
    MIPS32_HASH( "SQRT.S" ),
    MIPS32_HASH( "SQRT.D" ),
    MIPS32_HASH( "SRA" ),
    MIPS32_HASH( "SRAV" ),
    MIPS32_HASH( "SRL" ),
    MIPS32_HASH( "SRLV" ),
    MIPS32_HASH( "SSNOP" ),
    MIPS32_HASH( "SUB" ),
    MIPS32_HASH( "SUB.S" ),
    MIPS32_HASH( "SUB.D" ),
    MIPS32_HASH( "SUB.PS" ),
    MIPS32_HASH( "SUBU" ),
    MIPS32_HASH( "SUXC" ),
    MIPS32_HASH( "SW" ),
    MIPS32_HASH( "SWC" ),
    MIPS32_HASH( "SWE" ),
    MIPS32_HASH( "SWL" ),
    MIPS32_HASH( "SWLE" ),
    MIPS32_HASH( "SWR" ),
    MIPS32_HASH( "SWRE" ),
    MIPS32_HASH( "SWXC" ),
    MIPS32_HASH( "SYNC" ),
    MIPS32_HASH( "SYNCI" ),
    MIPS32_HASH( "SYSCALL" ),
    MIPS32_HASH( "TEQ" ),
    MIPS32_HASH( "TEQI" ),
    MIPS32_HASH( "TGE" ),
    MIPS32_HASH( "TGEI" ),
    MIPS32_HASH( "TGEIU" ),
    MIPS32_HASH( "TGEU" ),
    MIPS32_HASH( "TLBINV" ),
    MIPS32_HASH( "TLBINVF" ),
    MIPS32_HASH( "TLBP" ),
    MIPS32_HASH( "TLBR" ),
    MIPS32_HASH( "TLBWI" ),
    MIPS32_HASH( "TLBWR" ),
    MIPS32_HASH( "TLT" ),
    MIPS32_HASH( "TLTI" ),
    MIPS32_HASH( "TLTIU" ),
    MIPS32_HASH( "TLTU" ),
    MIPS32_HASH( "TNE" ),
    MIPS32_HASH( "TNEI" ),
    MIPS32_HASH( "TRUNC.L.S" ),
    MIPS32_HASH( "TRUNC.L.D" ),
    MIPS32_HASH( "TRUNC.W.S" ),
    MIPS32_HASH( "TRUNC.W.D" ),
    MIPS32_HASH( "WAIT" ),
    MIPS32_HASH( "WRPGPR" ),
    MIPS32_HASH( "WSBH" ),
    MIPS32_HASH( "XOR" ),
    MIPS32_HASH( "XORI" ),
  };

  auto end = inst_hashtable.cend();
  auto inst = std::find( inst_hashtable.cbegin(), end, hash );

  if ( inst != end )
    return end - inst - 1;

  return -1;
}

#undef MIPS32_HASH