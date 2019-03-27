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

    TEST_METHOD( TokenizerGet_Directives )
    {
      std::istringstream dir_lo, dir_hi;

      dir_lo.str(
        #pragma region dir_lo_region
        ".align "
        ".ascii "
        ".asciiz "
        ".byte "
        ".data "
        ".double "
        ".float "
        ".globl "
        ".half "
        ".kdata "
        ".ktext "
        ".space "
        ".text "
        ".word"
        #pragma endregion
      );

      dir_hi.str(
        #pragma region dir_hi_region
        ".ALIGN "
        ".ASCII "
        ".ASCIIZ "
        ".BYTE "
        ".DATA "
        ".DOUBLE "
        ".FLOAT "
        ".GLOBL "
        ".HALF "
        ".KDATA "
        ".KTEXT "
        ".SPACE "
        ".TEXT "
        ".WORD"
        #pragma endregion
      );

      mips32::Token tok;
      tokenizer.load( dir_lo );

      for ( int i = 0; i < 14; ++i )
      {
        tokenizer.get( tok );

        DebugOut << L"dir_lo #" << ( i + 1 ) << " invalid type";
        Assert::IsTrue( tok.type == tok.DIRECTIVE, DebugOut.str().c_str() );
        DebugOut.str( L"" );

        DebugOut << L"dir_lo #" << ( i + 1 ) << " invalid code";
        Assert::IsTrue( tok.code == i, DebugOut.str().c_str() );
        DebugOut.str( L"" );
      }

      tokenizer.load( dir_hi );

      for ( int i = 0; i < 14; ++i )
      {
        tokenizer.get( tok );

        DebugOut << L"dir_hi #" << ( i + 1 ) << " invalid type";
        Assert::IsTrue( tok.type == tok.DIRECTIVE, DebugOut.str().c_str() );
        DebugOut.str( L"" );

        DebugOut << L"dir_hi #" << ( i + 1 ) << " invalid code";
        Assert::IsTrue( tok.code == i, DebugOut.str().c_str() );
        DebugOut.str( L"" );
      }
    }

    TEST_METHOD( TokenizerGet_Instructions )
    {
      std::istringstream inst;
      inst.str(
        #pragma region inst_region
        "ABS.S "
        "ABS.D "
        "ADD "
        "ADD.S "
        "ADD.D "
        "ADDI "
        "ADDIU "
        "ADDIUPC "
        "ADDU "
        "ALIGN "
        "ALNV.PS "
        "ALUIPC "
        "AND "
        "ANDI "
        "AUI "
        "AUIPC "
        "B "
        "BAL "
        "BALC "
        "BC "
        "BCEQZ "
        "BCNEZ "
        "BCF "
        "BCFL "
        "BCT "
        "BCTL "
        "BEQ "
        "BEQL "
        "BGEZ "
        "BGEZAL "
        "BLEZALC "
        "BGEZALC "
        "BGTZALC "
        "BLTZALC "
        "BEQZALC "
        "BNEZALC "
        "BGEZALL "
        "BEQC "
        "BNEC "
        "BLTC "
        "BGEC "
        "BLTUC "
        "BGEUC "
        "BGTC "
        "BLEC "
        "BGTUC "
        "BLEUC "
        "BLTZC "
        "BLEZC "
        "BGEZC "
        "BGTZC "
        "BEQZC "
        "BNEZC "
        "BGEZL "
        "BGTZ "
        "BGTZL "
        "BITSWAP "
        "BLEZ "
        "BLEZL "
        "BLTZ "
        "BLTZAL "
        "BLTZALL "
        "BLTZL "
        "BNE "
        "BNEL "
        "BOVC "
        "BNVC "
        "BREAK "
        "C.F.S "
        "C.F.D "
        "C.F.PS "
        "C.UN.S "
        "C.UN.D "
        "C.UN.PS "
        "C.EQ.S "
        "C.EQ.D "
        "C.EQ.PS "
        "C.UEQ.S "
        "C.UEQ.D "
        "C.UEQ.PS "
        "C.OLT.S "
        "C.OLT.D "
        "C.OLT.PS "
        "C.ULT.S "
        "C.ULT.D "
        "C.ULT.PS "
        "C.OLE.S "
        "C.OLE.D "
        "C.OLE.PS "
        "C.ULE.S "
        "C.ULE.D "
        "C.ULE.PS "
        "C.SF.S "
        "C.SF.D "
        "C.SF.PS "
        "C.NGLE.S "
        "C.NGLE.D "
        "C.NGLE.PS "
        "C.SEQ.S "
        "C.SEQ.D "
        "C.SEQ.PS "
        "C.NGL.S "
        "C.NGL.D "
        "C.NGL.PS "
        "C.LT.S "
        "C.LT.D "
        "C.LT.PS "
        "C.NGE.S "
        "C.NGE.D "
        "C.NGE.PS "
        "C.LE.S "
        "C.LE.D "
        "C.LE.PS "
        "C.NGT.S "
        "C.NGT.D "
        "C.NGT.PS "
        "C.T.S "
        "C.T.D "
        "C.T.PS "
        "C.OR.S "
        "C.OR.D "
        "C.OR.PS "
        "C.NEQ.S "
        "C.NEQ.D "
        "C.NEQ.PS "
        "C.OGL.S "
        "C.OGL.D "
        "C.OGL.PS "
        "C.UGE.S "
        "C.UGE.D "
        "C.UGE.PS "
        "C.OGE.S "
        "C.OGE.D "
        "C.OGE.PS "
        "C.UGT.S "
        "C.UGT.D "
        "C.UGT.PS "
        "C.OGT.S "
        "C.OGT.D "
        "C.OGT.PS "
        "C.ST.S "
        "C.ST.D "
        "C.ST.PS "
        "C.GLE.S "
        "C.GLE.D "
        "C.GLE.PS "
        "C.SNE.S "
        "C.SNE.D "
        "C.SNE.PS "
        "C.GL.S "
        "C.GL.D "
        "C.GL.PS "
        "C.NLT.S "
        "C.NLT.D "
        "C.NLT.PS "
        "C.GE.S "
        "C.GE.D "
        "C.GE.PS "
        "C.NLE.S "
        "C.NLE.D "
        "C.NLE.PS "
        "C.GT.S "
        "C.GT.D "
        "C.GT.PS "
        "CACHE "
        "CACHEE "
        "CEIL.L.S "
        "CEIL.L.D "
        "CEIL.W.S "
        "CEIL.W.D "
        "CFC "
        "CLASS.S "
        "CLASS.D "
        "CLO "
        "CLZ "
        "CMP.AF.S "
        "CMP.AF.D "
        "CMP.UN.S "
        "CMP.UN.D "
        "CMP.EQ.S "
        "CMP.EQ.D "
        "CMP.UEQ.S "
        "CMP.UEQ.D "
        "CMP.LT.S "
        "CMP.LT.D "
        "CMP.ULT.S "
        "CMP.ULT.D "
        "CMP.LE.S "
        "CMP.LE.D "
        "CMP.ULE.S "
        "CMP.ULE.D "
        "CMP.SAF.S "
        "CMP.SAF.D "
        "CMP.SUN.S "
        "CMP.SUN.D "
        "CMP.SEQ.S "
        "CMP.SEQ.D "
        "CMP.SUEQ.S "
        "CMP.SUEQ.D "
        "CMP.SLT.S "
        "CMP.SLT.D "
        "CMP.SULT.S "
        "CMP.SULT.D "
        "CMP.SLE.S "
        "CMP.SLE.D "
        "CMP.SULE.S "
        "CMP.SULE.D "
        "CMP.AT.S "
        "CMP.AT.D "
        "CMP.OR.S "
        "CMP.OR.D "
        "CMP.UNE.S "
        "CMP.UNE.D "
        "CMP.NE.S "
        "CMP.NE.D "
        "CMP.UGE.S "
        "CMP.UGE.D "
        "CMP.OGE.S "
        "CMP.OGE.D "
        "CMP.UGT.S "
        "CMP.UGT.D "
        "CMP.OGT.S "
        "CMP.OGT.D "
        "CMP.SAT.S "
        "CMP.SAT.D "
        "CMP.SOR.S "
        "CMP.SOR.D "
        "CMP.SUNE.S "
        "CMP.SUNE.D "
        "CMP.SNE.S "
        "CMP.SNE.D "
        "CMP.SUGE.S "
        "CMP.SUGE.D "
        "CMP.SOGE.S "
        "CMP.SOGE.D "
        "CMP.SUGT.S "
        "CMP.SUGT.D "
        "CMP.SOGT.S "
        "CMP.SOGT.D "
        "COP "
        "CRCB "
        "CRCH "
        "CRCW "
        "CRCCB "
        "CRCCH "
        "CRCCW "
        "CTC1 "
        "CTC2 "
        "CVT.D.S "
        "CVT.D.W "
        "CVT.D.L "
        "CVT.L.S "
        "CVT.L.D "
        "CVT.PS.S "
        "CVT.S.PL "
        "CVT.S.PU "
        "CVT.S.D "
        "CVT.S.W "
        "CVT.S.L "
        "CVT.W.S "
        "CVT.W.D "
        "DDIV "
        "DDIVU "
        "DERET "
        "DI "
        "DIV "
        "MOD "
        "DIVU "
        "MODU "
        "DIV.S "
        "DIV.D "
        "DVP "
        "EHB "
        "EI "
        "ERET "
        "ERETNC "
        "EVP "
        "EXT "
        "FLOOR.L.S "
        "FLOOR.L.D "
        "FLOOR.W.S "
        "FLOOR.W.D "
        "GINVI "
        "GINVT "
        "INS "
        "J "
        "JAL "
        "JALR "
        "JALR.HB "
        "JALX "
        "JIALC "
        "JIC "
        "JR "
        "JR.HB "
        "LB "
        "LBE "
        "LBU "
        "LBUE "
        "LDC "
        "LDXC1 "
        "LH "
        "LHE "
        "LHU "
        "LHUE "
        "LL "
        "LLE "
        "LLWP "
        "LLWPE "
        "LSA "
        "LUI "
        "LUXC1 "
        "LW "
        "LWC "
        "LWE "
        "LWL "
        "LWLE "
        "LWPC "
        "LWR "
        "LWRE "
        "LWXC1 "
        "MADD "
        "MADD.S "
        "MADD.D "
        "MADD.PS "
        "MADDF.S "
        "MADDF.D "
        "MSUBF.S "
        "MSUBF.D "
        "MADDU "
        "MAX.S "
        "MAX.D "
        "MIN.S "
        "MIN.D "
        "MAXA.S "
        "MAXA.D "
        "MINA.S "
        "MINA.D "
        "MFC0 "
        "MFC1 "
        "MFC2 "
        "MFHC0 "
        "MFHC1 "
        "MFHC2 "
        "MFHI "
        "MFLO "
        "MOV.S "
        "MOV.D "
        "MOV.PS "
        "MOVF "
        "MOVF.S "
        "MOVF.D "
        "MOVF.PS "
        "MOVN "
        "MOVN.S "
        "MOVN.D "
        "MOVN.PS "
        "MOVT "
        "MOVT.S "
        "MOVT.D "
        "MOVT.PS "
        "MOVZ "
        "MOVZ.S "
        "MOVZ.D "
        "MOVZ.PS "
        "MSUB "
        "MSUB.S "
        "MSUB.D "
        "MSUB.PS "
        "MSUBU "
        "MTC0 "
        "MTC1 "
        "MTC2 "
        "MTHC0 "
        "MTHC1 "
        "MTHC2 "
        "MTHI "
        "MTLO "
        "MUL "
        "MUH "
        "MULU "
        "MUHU "
        "MUL.S "
        "MUL.D "
        "MUL.PS "
        "MULT "
        "MULTU "
        "NAL "
        "NEG.S "
        "NEG.D "
        "NEG.PS "
        "NMADD.S "
        "NMADD.D "
        "NMADD.PS "
        "NMSUB.S "
        "NMSUB.D "
        "NMSUB.PS "
        "NOP "
        "NOR "
        "OR "
        "ORI "
        "PAUSE "
        "PLL.PS "
        "PLU.PS "
        "PREF "
        "PREFE "
        "PREFX "
        "PUL.PS "
        "PUU.PS "
        "RDHWR "
        "RDPGPR "
        "RECIP.S "
        "RECIP.D "
        "RINT.S "
        "RINT.D "
        "ROTR "
        "ROTRV "
        "ROUND.L.S "
        "ROUND.L.D "
        "ROUND.W.S "
        "ROUND.W.D "
        "RSQRT.S "
        "RSQRT.D "
        "SB "
        "SBE "
        "SC "
        "SCE "
        "SCWP "
        "SCWPE "
        "SDBBP "
        "SDC "
        "SDXC "
        "SEB "
        "SEH "
        "SEL.S "
        "SEL.D "
        "SELEQZ "
        "SELNEZ "
        "SELEQZ.S "
        "SELEQZ.D "
        "SELNEQZ.S "
        "SELNEQZ.D "
        "SH "
        "SHE "
        "SIGRIE "
        "SLL "
        "SLLV "
        "SLT "
        "SLTI "
        "SLTIU "
        "SLTU "
        "SQRT.S "
        "SQRT.D "
        "SRA "
        "SRAV "
        "SRL "
        "SRLV "
        "SSNOP "
        "SUB "
        "SUB.S "
        "SUB.D "
        "SUB.PS "
        "SUBU "
        "SUXC "
        "SW "
        "SWC "
        "SWE "
        "SWL "
        "SWLE "
        "SWR "
        "SWRE "
        "SWXC "
        "SYNC "
        "SYNCI "
        "SYSCALL "
        "TEQ "
        "TEQI "
        "TGE "
        "TGEI "
        "TGEIU "
        "TGEU "
        "TLBINV "
        "TLBINVF "
        "TLBP "
        "TLBR "
        "TLBWI "
        "TLBWR "
        "TLT "
        "TLTI "
        "TLTIU "
        "TLTU "
        "TNE "
        "TNEI "
        "TRUNC.L.S "
        "TRUNC.L.D "
        "TRUNC.W.S "
        "TRUNC.W.D "
        "WAIT "
        "WRPGPR "
        "WSBH "
        "XOR "
        "XORI "
        #pragma endregion
      );
      
      mips32::Token tok;

      tokenizer.load( inst );

      for ( int i = 0; i < 501; ++i )
      {
        tokenizer.get( tok );
        
        DebugOut << "#" << i << " invalid type";
        Assert::IsTrue( tok.type == tok.INSTRUCTION, DebugOut.str().c_str() );
        DebugOut.str( L"" );

        DebugOut << "#" << i << " invalid code";
        Assert::IsTrue( tok.code != -1, DebugOut.str().c_str() );
        DebugOut.str( L"" );

        tok = mips32::Token{};
      }
    }
    
    TEST_METHOD_INITIALIZE( RewindTokenizer )
    {
      tokenizer.rewind();
      DebugOut.str(L"");
    }
	};
}