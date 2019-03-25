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

    #define TEST_TOKENIZER_SAMPLE "test_tokenizer.mips32sample.txt"

    mips32::Tokenizer tokenizer;

    TEST_METHOD( TokenizerShallTokenizerSuccessfully )
    {
      bool r = tokenizer.tokenize( TEST_TOKENIZER_SAMPLE );
      Assert::IsTrue( r, L"tokenize should succeed" );
    }

    TEST_METHOD( TokenizerHasToken_JustLoadedFile )
    {
      tokenizer.tokenize( TEST_TOKENIZER_SAMPLE );
      Assert::IsTrue( tokenizer.has_token() );
    }

    TEST_METHOD( TokenizerHasToken_NotLoadedFile )
    {
      Assert::IsFalse( tokenizer.has_token() );
    }

    TEST_METHOD( TokenizerGet_ExpectedType )
    {
      tokenizer.tokenize( TEST_TOKENIZER_SAMPLE );
      mips32::Token tok;

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.NEWLINE, L"1st tok should be a NEWLINE" );

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.NEWLINE, L"2nd tok should be a NEWLINE" );

      // vector_out_of_range_1: .asciiz "Vector Size: "\n

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.LABEL_DECL, L"3rd tok should be a LABEL_DECL" );

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.DIRECTIVE, L"4th tok should be a DIRECTIVE" );

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.STRING, L"5th tok should be a STRING" );

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.NEWLINE, L"6th tok should be a NEWLINE" );

      // vector_out_of_range_2: .asciiz "Given Position: "\n

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.LABEL_DECL, L"7th tok should be a LABEL_DECL" );

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.DIRECTIVE, L"8th tok should be a DIRECTIVE" );

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.STRING, L"9th tok should be a STRING" );

      Assert::IsTrue( tokenizer.get( tok ), L"Get should always succeed" );
      Assert::IsTrue( tok.type == tok.NEWLINE, L"10th tok should be a NEWLINE" );
    }

    TEST_CLASS_INITIALIZE( InitializeClass )
    {
      std::string sample = R"(

      vector_out_of_range_1: .asciiz "Vector Size: "
      vector_out_of_range_2: .asciiz "Given Position: "
      
      
      .globl main
      .text
      
      ## Begin: main

      # Main function that initializes the program
      # and then performs operations chosen by the user
      main:
        addiu $sp,$sp,-8
        sw $s0, 0( $sp)
        sw $ra, 4($sp )
        sw $zero, 0($at)

        li $a0, 10        # vector size
        li $a1, 21   # vector element size 20 char + \0
        jal vector_constructor

        la $s0, choice

        main_while :
        li $v0, 4
        la $a0, newline
        syscall

        jal menu_print      # Print Menu
        jal menu_choose     # Save the chosen option
        lw $t0, ( $s0 )       # Load choice

        slt $t1, $t0, $zero # choice < 0
        sgt $t2, $t0, 5     # choice > 5
        or $t1, $t1, $t2    # choice < 0 || choice > 5
        bnez $t1, main_while)";

      std::ofstream out{ TEST_TOKENIZER_SAMPLE };
      out << sample;
      out.close();
    }

    TEST_CLASS_CLEANUP( CleanupClass )
    {
      std::remove( TEST_TOKENIZER_SAMPLE );
    }

    TEST_METHOD_INITIALIZE( RewindTokenizer )
    {
      tokenizer.rewind();
    }

    #undef TEST_TOKENIZER_SAMPLE
	};
}