package com.example
package symbol.symboltable

import com.example.ast.ParserUtil


class MySuit extends munit.FunSuite:
  test("hello"):
    val source  = """
      |struct A {
      |  int x;
      |  struct B {
      |    int y;
      |    float zz;
      |  };
      |  B b;
      |  struct C { int z; };
      |  C c;
      |};
      |A a;
  
      |void f(float x)
      |{
      |  struct D {
      |    int i;
      |  };
      |  D d;
      |  d = d;
      |  d.z = a.b.y;
      |  d.i = x;
      |  a.b.zz;
      |}
      """.stripMargin.trim()
    val annotatedCode = SymbolTableApp.resolveSymbol.tupled(ParserUtil.parse(source))
    val expected = 
        """
        |struct A {
        |  int/*int*/ x;
        |  struct B {
        |    int/*int*/ y;
        |    float/*float*/ zz;
        |  };
        |  B/*B*/ b;
        |  struct C { int/*int*/ z; };
        |  C/*C*/ c;
        |};
        |A/*A*/ a;
  
        |void/*void*/ f()
        |{
        |  struct D {
        |    int/*int*/ i;
        |  };
        |  D/*D*/ d;
        |  d/*D*/ = d/*D*/;
        |  d.z/*unresolved*/ = a.b.y/*int*/;
        |  d.i/*int*/ = x/*float*/;
        |  a.b.zz/*float*/;
        |}
        """.stripMargin.trim()

    println(annotatedCode == expected)

