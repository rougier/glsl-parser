# -*- coding: utf-8 -*-
# -----------------------------------------------------------------------------
# Copyright (c) 2014, Nicolas P. Rougier
# Distributed under the (new) BSD License. See LICENSE.txt for more info.
# -----------------------------------------------------------------------------
from pyparsing import *

keywords  = ("attribute const uniform varying break continue do for while"
             "if else"
             "in out inout"
             "float int void bool true false"
             "lowp mediump highp precision invariant"
             "discard return"
             "mat2 mat3 mat4"
             "vec2 vec3 vec4 ivec2 ivec3 ivec4 bvec2 bvec3 bvec4 sampler2D samplerCube"
             "struct")
reserved  = ("asm"
             "class union enum typedef template this packed"
             "goto switch default"
             "inline noinline volatile public static extern external"
             "interface flat long short double half fixed unsigned superp"
             "input output"
             "hvec2 hvec3 hvec4 dvec2 dvec3 dvec4 fvec2 fvec3 fvec4 sampler1D sampler3D"
             "sampler1DShadow sampler2DShadow"
             "sampler2DRect sampler3DRect sampler2DRectShadow"
             "sizeof cast"
             "namespace using")
precision = "lowp mediump high"
storage   = "const uniform attribute varying"


# Tokens
# ----------------------------------
LPAREN     = Literal("(").suppress()
RPAREN     = Literal(")").suppress()
LBRACK     = Literal("[").suppress()
RBRACK     = Literal("]").suppress()
LBRACE     = Literal("{").suppress()
RBRACE     = Literal("}").suppress()
IDENTIFIER = Word(alphas + '_', alphanums + '_')
TYPE       = Word(alphas + '_', alphanums + "_")
END        = Literal(";").suppress()
INT        = Word(nums)
FLOAT      = Regex('[+-]?(((\d+\.\d*)|(\d*\.\d+))([eE][-+]?\d+)?)|(\d*[eE][+-]?\d+)')
STORAGE    = Regex('|'.join(storage.split(' ')))
PRECISION  = Regex('|'.join(precision.split(' ')))
STRUCT     = Literal("struct").suppress()


# ------------------------
def get_prototypes(code):
    """
    Get all function declarations

    Code example
    ------------

    mediump vec3 function_1(vec4);
    vec3 function_2(float a, float b);
    """

    PARAMETER = Group(Optional(PRECISION).setResultsName("precision") +
                      TYPE.setResultsName("type") +
                      Optional(IDENTIFIER).setResultsName("name"))
    PARAMETERS = delimitedList(PARAMETER).setResultsName("arg",listAllMatches=True)
    PROTOTYPE = (Optional(PRECISION).setResultsName("precision") +
                 TYPE.setResultsName("type") +
                 IDENTIFIER.setResultsName("name") +
                 LPAREN + Optional(PARAMETERS).setResultsName("args") + RPAREN +
                 END)
    PROTOTYPE.ignore(cStyleComment)

    for (token, start, end) in PROTOTYPE.scanString(code):
        print token.precision, token.type, token.name, '(',
        for arg in token.args:
            print arg.precision, arg.type, arg.name, ',',
        print ')'


# ------------------------
def get_functions(code):
    """
    Get all function definitions

    Code example
    ------------

    mediump vec3 compute_normal(vec4);
    """

    PARAMETER = Group(Optional(PRECISION).setResultsName("precision") +
                      TYPE.setResultsName("type") +
                      Optional(IDENTIFIER).setResultsName("name"))
    PARAMETERS = delimitedList(PARAMETER).setResultsName("arg",listAllMatches=True)
    FUNCTION = (Optional(PRECISION).setResultsName("precision") +
                TYPE.setResultsName("type") +
                IDENTIFIER.setResultsName("name") +
                LPAREN + Optional(PARAMETERS).setResultsName("args") + RPAREN +
                nestedExpr("{", "}").setParseAction(keepOriginalText).setResultsName("code"))
    FUNCTION.ignore(cStyleComment)

    for (token, start, end) in FUNCTION.scanString(code):
        print token.precision, token.type, token.name, '(',
        for arg in token.args:
            print arg.precision, arg.type, arg.name, ',',
        print ') { ... }'

        # print token.code
        # print code[start:end]


# ------------------------
def get_version(code):
    """
    Get shader version (if specified)

    Code example
    ------------

    #version 120
    """

    VERSION = (Literal("#") + Keyword("version")).suppress() + INT
    for (token, start, end) in VERSION.scanString(code):
        version = token[0]
        # print code[start:end]
    return version

# ------------------------
def get_declarations(code):
    """
    Get all declarations prefixed with a storage qualifier.

    Code example
    ------------

    uniform lowp vec4 fg_color = vec4(1),
                      bg_color = vec4(vec3(0),1);
    """

    # Callable expression
    EXPRESSION  = Forward()
    ARG         = Group(EXPRESSION) | IDENTIFIER | FLOAT | INT
    ARGS        = delimitedList(ARG)
    EXPRESSION << IDENTIFIER + Group(LPAREN + Optional(ARGS) + RPAREN)

    # Value
    VALUE = (EXPRESSION | Word(alphanums + "_()+-/*")).setParseAction(keepOriginalText)

    # Single declaration
    VARIABLE = (IDENTIFIER.setResultsName("name") +
                Optional(LBRACK +
                         (INT | IDENTIFIER).setResultsName("size")
                         + RBRACK) +
                Optional(Literal("=").suppress() + VALUE.setResultsName("value")) )

    # Several declarations at once
    DECLARATION = (STORAGE.setResultsName("storage") +
                   Optional(PRECISION).setResultsName("precision") +
                   TYPE.setResultsName("type") +
                   delimitedList(VARIABLE.setResultsName("variable",listAllMatches=True)) +
                   END)
    DECLARATION.ignore(cStyleComment)

    for (tokens, start, end) in DECLARATION.scanString(code):
        for token in tokens.variable:
            print tokens.storage, tokens.precision, tokens.type,
            print token.name, token.size


# ------------------------
def get_definitions(code):
    """
    Get all structure definitions and associated declarations.

    Code example
    ------------

    uniform struct Light {
        vec4 position;
        vec3 color;
    } light0, light1;
    """

    # Single declaration
    DECLARATION = Group(IDENTIFIER.setResultsName("name") +
                        Optional(LBRACK +
                                 (INT | IDENTIFIER).setResultsName("size") +
                                 RBRACK))
    # Several declarations at once
    DECLARATIONS = (Optional(PRECISION) +
                    TYPE +
                    delimitedList(DECLARATION) +
                    END)

    # Definition + declarations
    DEFINITION = ( STRUCT +
                   IDENTIFIER.setResultsName("name") +
                   LBRACE + OneOrMore(DECLARATIONS).setResultsName('content') + RBRACE +
                   Optional(delimitedList(DECLARATION.setResultsName("declarations",listAllMatches=True))) +
                   END )
    DEFINITION.ignore(cStyleComment)

    for (tokens, start, end) in DEFINITION.scanString(code):
        for token in tokens.declarations:
            print tokens.name, token.name
            # print tokens.content



# ----------------
def resolve(code):
    """
    Expand const and preprocessor definitions in order to get constant values.

    Return the transformed code
    """

    constants = {}

    DEFINITION = (Literal("#") + Literal("define") +
                  IDENTIFIER.setResultsName("name") +
                  restOfLine.setResultsName("value"))

    VALUE       = Word(alphanums + "_()+-/*")
    DECLARATION = (Literal("const") +
                   TYPE.setResultsName("type") +
                   IDENTIFIER.setResultsName("name") +
                   Literal("=") +
                   VALUE.setResultsName("value") +
                   Literal(";"))
    REFERENCE = Forward()

    def process_definition(s,l,t):
        value = REFERENCE.transformString(t.value)
        constants[t.name] = value
        REFERENCE << MatchFirst(map(Keyword, constants.keys()))
        return "#define " + t.name + " " + value

    def process_declaration(s,l,t):
        value = REFERENCE.transformString(t.value)
        constants[t.name] = value
        REFERENCE << MatchFirst(map(Keyword, constants.keys()))
        return "const " + t.type + " " + t.name + "=" + value + ";"

    def process_reference(s,l,t):
        return constants[t[0]]

    REFERENCE.setParseAction(process_reference)
    DEFINITION.setParseAction(process_definition)
    DECLARATION.setParseAction(process_declaration)
    EXPANDER = REFERENCE | DEFINITION | DECLARATION

    code = EXPANDER.transformString(code)
    for key,val in constants.items():
        constants[key] = eval(val)

    return code, constants


# -----------------------------------------------------------------------------
if __name__ == '__main__':

    code = """
#version 120

#define A (1)
const int B=(A+2);
#define C (B+3)
const int D=C+4;

uniform float array[D];

struct Point {
    vec4 position;
    float size;
};

uniform struct Light {
    vec4 position;
    vec3 color;
} light0, light1;

const float PI = 3.14159265358979323846264;
const float SQRT_2 = 1.4142135623730951;

uniform vec4 fg_color = vec4(1),
             bg_color = vec4(vec3(0),1);

mediump vec3 compute_normal(vec4 position, vec3 orientation);
vec3 /* */ compute_light(vec4, vec3, float intensity)
{
   vec3 hello;
   vec3 hello;
}

"""
code, _ = resolve(code)
print "GLSL version: %s\n" % get_version(code)

get_definitions(code)
get_declarations(code)
get_prototypes(code)
get_functions(code)

# code = """
# #if A
# #if B
# #if C
# #endif
# #endif
# #endif
# """

# IF = (Literal('#') + (Keyword('if') | Keyword('ifdef') | Keyword('ifndef')))
# ENDIF = (Literal('#') + Keyword('endif'))
# MACRO = (IF + restOfLine() +
#          SkipTo(ENDIF, include=True)).setParseAction(keepOriginalText)
# for (tokens, start, end) in MACRO.scanString(code):
#     print tokens
