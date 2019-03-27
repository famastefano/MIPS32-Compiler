#include "stdafx.h"
#include "CppUnitTest.h"

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

#include "../MIPS32_Compiler/src/tokenizer.hpp"

#include <cstdio>
#include <fstream>

namespace Test
{		
	TEST_CLASS(TestTokenizer)
	{
	public:
    mips32::Tokenizer tokenizer;

    std::wostringstream DebugOut;

    TEST_METHOD( TokenizerLoad_FilePath )
    {
      std::ofstream out{ "TokenizerLoad_FilePath.tmp" };
      out << "_";
      out.close();

      bool r = tokenizer.load( "TokenizerLoad_FilePath.tmp" );
      Assert::IsTrue( r, L"load failed" );

      std::remove( "TokenizerLoad_FilePath.tmp" );
    }

    TEST_METHOD( TokenizerLoad_Stream )
    {
      std::istringstream stream;
      stream.str( "_" );

      bool r = tokenizer.load( stream );
      Assert::IsTrue( r, L"load failed" );
    }

    TEST_METHOD( TokenizerHasToken_NotLoadedFile )
    {
      Assert::IsFalse( tokenizer.has_token() );
    }

    TEST_METHOD( TokenizerGet_ExpectedType )
    {
      std::stringstream sample;
      sample.str(
        ".data 0x8000'0000\n"   // directive number newline
        ".globl main\n"         // directive label_ref newline
        "main:xor $0, $0, $0\n" // label_decl inst reg comma reg comma reg newline
        "\taddiu $1, $2, $3\n"  // inst reg comma reg comma reg newline
        "\tlw $0, 0($sp)\n"     // inst reg comma num left_par reg right_par newline
        "\tsw $s3, -142($ra)\n"  // inst reg comma num left_par reg right_par newline
        "\tsyscall\n"           // inst newline
      );

      tokenizer.load( sample );

      constexpr int tok_n{ 39 };
      
      mips32::Token tok;
      decltype( mips32::Token::type ) expected_type[tok_n] =
      {
        tok.DIRECTIVE, tok.INTEGER, tok.NEWLINE,
        tok.DIRECTIVE, tok.LABEL_REF, tok.NEWLINE,
        tok.LABEL_DECL, tok.INSTRUCTION, tok.INT_REGISTER, tok.COMMA, tok.INT_REGISTER, tok.COMMA, tok.INT_REGISTER, tok.NEWLINE,
        tok.INSTRUCTION, tok.INT_REGISTER, tok.COMMA, tok.INT_REGISTER, tok.COMMA, tok.INT_REGISTER, tok.NEWLINE,
        tok.INSTRUCTION, tok.INT_REGISTER, tok.COMMA, tok.INTEGER, tok.LEFT_PAR, tok.INT_REGISTER, tok.RIGHT_PAR, tok.NEWLINE,
        tok.INSTRUCTION, tok.INT_REGISTER, tok.COMMA, tok.INTEGER, tok.LEFT_PAR, tok.INT_REGISTER, tok.RIGHT_PAR, tok.NEWLINE,
        tok.INSTRUCTION, tok.NEWLINE
      };

      for ( int i = 0; i < tok_n; ++i )
      {
        DebugOut << L"Token #" << (i+1);
        
        tokenizer.get( tok );
        Assert::IsTrue( tok.type == expected_type[i], DebugOut.str().c_str() );

        DebugOut.str(L"");
      }
    }

    TEST_METHOD( TokenizerGet_Registers )
    {
      std::istringstream int_reg_as_num, int_reg_coded, float_reg;
      
      int_reg_as_num.str(
        "$0 $1 $2 $3 $4 $5 $6 $7 $8 $9 "
        "$10 $11 $12 $13 $14 $15 $16 $17 $18 $19 "
        "$20 $21 $22 $23 $24 $25 $26 $27 $28 $29 "
        "$30 $31"
      );

      int_reg_coded.str(
        "$zero $at $v0 $v1 "
        "$a0   $a1 $a2 $a3 "
        "$t0   $t1 $t2 $t3 "
        "$t4   $t5 $t6 $t7 "
        "$s0   $s1 $s2 $s3 "
        "$s4   $s5 $s6 $s7 "
        "$t8   $t9 $k0 $k1 "
        "$gp   $sp $s8 $ra"
      );

      float_reg.str(
        "$f0 $f1 $f2 $f3 $f4 $f5 $f6 $f7 $f8 $f9 "
        "$f10 $f11 $f12 $f13 $f14 $f15 $f16 $f17 $f18 $f19 "
        "$f20 $f21 $f22 $f23 $f24 $f25 $f26 $f27 $f28 $f29 "
        "$f30 $f31"
      );
      
      mips32::Token tok;

      // Integer Registers as numbers
      tokenizer.load( int_reg_as_num );
      for ( int i = 0; i < 32; ++i )
      {
        tokenizer.get( tok );

        DebugOut << L"int_reg_as_num #" << ( i + 1 ) << L" invalid type";
        Assert::IsTrue( tok.type == tok.INT_REGISTER, DebugOut.str().c_str() );
        DebugOut.str( L"" );

        DebugOut << L"int_reg_as_num #" << ( i + 1 ) << L" wrong number";
        Assert::IsTrue( tok.reg == std::uint32_t(i), DebugOut.str().c_str() );
        DebugOut.str( L"" );
      }

      // Integer Registers encoded
      tokenizer.load( int_reg_coded );
      for ( int i = 0; i < 32; ++i )
      {
        tokenizer.get( tok );

        DebugOut << L"int_reg_coded #" << ( i + 1 ) << L" invalid type";
        Assert::IsTrue( tok.type == tok.INT_REGISTER, DebugOut.str().c_str() );
        DebugOut.str( L"" );

        DebugOut << L"int_reg_coded #" << ( i + 1 ) << L" wrong number";
        Assert::IsTrue( tok.reg == std::uint32_t( i ), DebugOut.str().c_str() );
        DebugOut.str( L"" );
      }

      // Float Registers
      tokenizer.load( float_reg );
      for ( int i = 0; i < 32; ++i )
      {
        DebugOut << L"float_reg #" << ( i + 1 );

        tokenizer.get( tok );
        Assert::IsTrue( tok.type == tok.FLOAT_REGISTER, DebugOut.str().c_str() );
        Assert::IsTrue( tok.reg == std::uint32_t( i ), DebugOut.str().c_str() );

        DebugOut.str( L"" );
      }
    }
    
    TEST_METHOD_INITIALIZE( RewindTokenizer )
    {
      tokenizer.rewind();
      DebugOut.str(L"");
    }
	};
}