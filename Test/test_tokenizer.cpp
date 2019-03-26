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
    
    TEST_METHOD_INITIALIZE( RewindTokenizer )
    {
      tokenizer.rewind();
      DebugOut.str(L"");
    }
	};
}