import unittest
import sys, os

sys.path.append(os.path.dirname(os.path.realpath(__file__)) + '/../..')
from syncode.parsers import create_parser
from syncode.parsers.grammars.grammar import Grammar

pharo_grammar = Grammar('pharo')
inc_parser = create_parser(pharo_grammar, ignore_whitespace=True)

class TestPharoParser(unittest.TestCase):
    def check_parsing(self, code, should_pass=True, msg=""):
        inc_parser.reset()
        try:
            inc_parser.base_parser.parse(code)
            self.assertTrue(should_pass, f"Should NOT have parsed (Negative Case): {code} | {msg}")
        except Exception as e:
            if isinstance(e, AssertionError):
                raise
            self.assertFalse(should_pass, f"Failed to parse (Positive Case): {code}\nError: {e}\n{msg}")

def wrap_in_method(expr):
    """Wraps an expression in a valid method structure: testMethod ^ expr"""
    return f"testMethod ^ {expr}"

def wrap_stmts_in_method(stmts):
    """Wraps statements in a valid method structure: testMethod stmts"""
    return f"testMethod {stmts}"

POSITIVE_LITERALS = [
    # --- Integers ---
    ("int_0", "0"),
    ("int_1", "1"),
    ("int_pos", "123"),
    ("int_large", "9999999999"),
    ("int_neg", "-50"),
    # --- Floats ---
    ("float_simple", "1.0"),
    ("float_dec", "1.23"),
    ("float_zero", "0.0"),
    ("float_small", "0.1"),
    ("float_neg", "-1.5"),
    ("float_sci", "1.0e10"),
    ("float_sci_plus", "1.23e+5"),
    ("float_sci_neg", "1.2e-5"),
    ("float_scaled", "1.2s2"),
    ("double_precision_float", "1.2d0"),
    ("quad_precision_float", "1.2q0"),
    # --- Radix Numbers ---
    ("radix_16", "16rF"),
    ("radix_16_long", "16rFF"),
    ("radix_16_mixed", "16rFa"),
    ("radix_36_double_r", "36rr"),
    ("radix_2", "2r101"),
    ("radix_8", "8r77"),
    ("radix_10", "10r99"),
    ("radix_2_zero", "2r0"),
    ("radix_neg", "-16rA"),
    ("radix_float", "16rA.B"),
    ("radix_sci", "2r10e2"),
    ("radix_float", "16rA.C"),
    # --- Scaled Decimals ---
    ("scaled_decimal", "123s2"),
    ("trivial_scale", "123s"),
    ("scaled_decimal_neg", "-123.45s2"),
    ("scaled_float", "12.3s2"),
    ("scaled_radix", "16rA.Bs3"),
    ("scaled_radix_float", "16r101.As9"),
    # --- Strings ---
    ("string_simple", "'hello'"),
    ("string_empty", "''"),
    ("string_space", "' '"),
    ("string_quote", "'haven''t'"),
    ("string_special", "'!@#$%'"),
    ("string_multiline", "'Line1\nLine2'"),
    ("string_escapes", "'Str with ''quotes'' inside'"),
    # --- Characters ---
    ("char_a", "$a"),
    ("char_A", "$A"),
    ("char_digit", "$0"),
    ("char_dollar", "$$"),
    ("char_space", "$ "),
    ("char_sep", "$."),
    ("char_plus", "$+"),
    ("char_newline", "$n"),
    # --- Symbols ---
    ("sym_simple", "#foo"),
    ("sym_keyword", "#foo:bar:"),
    ("sym_quoted", "#'symbol with spaces'"),
    ("sym_quoted_op", "#'+'"),
    ("sym_empty_quoted", "#' '"),
    ("sym_complex_quote", "#'abc.def:ghi'"),
    ("sym_bin_slash", "#//"),
    ("sym_bin_arrow", "#->"),
    ("sym_bin_match", "#match:"),
    ("sym_quoted_weird", "#' foo:bar '"),
    ("sym_quoted_bin", "#'++'"),
    # --- Bare Binary Symbols ---
    ("sym_bin_plus", "#+"),
    ("sym_bin_minus", "#-"),
    ("sym_bin_comma", "#,"),
    ("sym_bin_eq", "#="),
    ("sym_bin_cmp", "#=="),
    ("sym_bin_mix", "#+*"),
    # --- Arrays ---
    ("array_int", "#(1 2 3)"),
    ("array_mixed", "#(1 'a' $b)"),
    ("array_empty", "#()"),
    ("array_nested", "#(1 #(2 3))"),
    ("array_nested_deep", "#((1 2) (3 4))"),
    ("array_builtins", "#(true false nil self super thisContext)"),
    ("array_sym", "#(#foo #bar)"),
    ("array_float", "#(1.2 1.5)"),
    ("array_messy", "#( 1 '2' $3 #four )"),
    ("array_mixed_nums", "#( 16rF 1.2e3 2r101s2 )"),
    ("array_symbols_no_hash", "#( foo + - * / = == )"),
    ("array_strings_nested", "#( 'str' 'string' ('nested' 'with' ('deep' 'deeper')) )"),
    ("array_byte_literal", "#( #[1 2] #[3 4] )"),
    ("array_byte_literal", "#( Object String Array )"),
    # --- Arrays with punctuation ---
    ("array_punct_dot", "#( . )"),
    ("array_punct_caret", "#( ^ )"),
    ("array_punct_assign", "#( := )"),
    ("array_punct_brackets", "#( [ ] { } )"),
    ("array_punct_mixed", "#( a . b ; c ^ d := )"),
    # --- Byte Arrays ---
    ("bytearray_simple", "#[1 2 3]"),
    ("bytearray_empty", "#[]"),
    ("bytearray_large", "#[255]"),
    ("bytearray_zeros", "#[0 0 0]"),
    # --- Built-ins ---
    ("builtin_true", "true"),
    ("builtin_false", "false"),
    ("builtin_nil", "nil"),
    ("builtin_self", "self"),
    ("builtin_super", "super"),
    ("builtin_thisContext", "thisContext")
]

NEGATIVE_LITERALS = [
    ("bad_float_dots", "1.2.3"),
    ("float_start_dot", ".5"),
    ("float_double_exp", "1e2e3"),
    ("double_scale", "123ss4"),
    ("invalid_scale", "123sA"),
    ("negative_scale", "123s-5"),
    ("bad_sym_dot", "#.symbol"),
    ("bad_sym_num", "#123"),
    ("bad_sym_colon", "#:"),
    ("bad_sym_colon_start", "#:abc"),
    ("string_unclosed", "'unclosed"),
    ("string_quote_mismatch", "'abc''"),
    ("char_empty", "$"),
    ("array_unclosed", "#("),
    ("array_unclosed", "#( 1 2"),
    ("bytearray_unclosed", "#[ 1 2"),
    ("bytearray_with_char", "#[ a ]"),
]

POSITIVE_EXPRESSIONS = [
    # --- Assignments ---
    ("assign_simple", "x := 1"),
    ("assign_chain", "x := y := 2"),
    ("assign_expr", "x := 1 + 2"),
    ("assign_lit_str", "var := 'string'"),
    ("assign_lit_sym", "var := #sym"),
    ("assign_space_char", "var := $ "),
    # --- Unary Messages ---
    ("msg_unary_1", "1 toString"),
    ("msg_unary_paren", "(x negated)"),
    ("msg_unary_class", "(Color red)"),
    ("msg_unary_chain", "(x y z)"),
    ("msg_unary_spaced", "(x   negated)"),
    # --- Binary Messages ---
    ("msg_binary_simple", "1 + 2"),
    ("msg_binary_negative", "-1 - -2"),
    ("msg_binary_mul", "3 * 4"),
    ("msg_binary_comma", "(a , b)"),
    ("msg_binary_cmp", "(x > y)"),
    ("msg_binary_prec", "1 + 2 * 3"),
    ("msg_binary_logic_or", "a | b"),
    ("msg_binary_logic_and", "a & b"),
    ("msg_binary_eq", "a = b"),
    ("msg_binary_neq", "a ~= b"),
    ("msg_binary_ident", "a == b"),
    # --- Keyword Messages ---
    ("msg_kw_simple", "x at: 1"),
    ("msg_kw_multi", "x at: 1 put: 2"),
    ("msg_kw_color", "Color r: 1 g: 0 b: 0"),
    ("msg_kw_block", "x ifTrue: [ 1 ] ifFalse: [ 2 ]"),
    ("msg_kw_long", "Window labeled: 'Title' request: 'Msg' initialAnswer: 'Yes'"),
    # --- Precedence ---
    ("msg_precedence_1", "(x foo + y bar)"),
    ("msg_precedence_2", "(x foo: y bar + z baz)"),
    # --- Cascades ---
    ("cascade_simple", "(x foo; bar; baz)"),
    ("cascade_kw", "(x at: 1 put: 2; at: 3 put: 4)"),
    ("cascade_nested", "(x + 1; * 2)"),
    ("cascade_multi_kw", "Console print: 'A'; print: 'B'; cr"),
    ("cascade_string", "'string' size; asString; size"),
    ("cascade_parens", "(x foo); bar"),
    # --- Dynamic Arrays ---
    ("dyn_array_simple", "{ 1 . 2 . 3 }"),
    ("dyn_array_expr", "{ 1 + 2 . 3 * 4 }"),
    ("dyn_array_empty", "{ }"),
    ("dyn_array_mixed", "{ 1 . 'a' . $b }"),
    ("dyn_array_trailing_dot", "{ 1. 2. }"),
    ("dyn_array_nested", "{ { 1 } . { 2 } }"),
    ("dyn_array_blocks", "{ [1] . [:x|x] }"),
    # --- Parentheses ---
    ("parens_nested", "((1))"),
    ("parens_expr", "(1 + 2)"),
    ("parens_negative_expr", "(-5 abs)"),
]

NEGATIVE_EXPRESSIONS = [
    ("expr_bad_assign", "x :="),
    ("expr_bad_assign_dot", "x := ."),
    ("expr_assign_to_lit", "1 := 2"),
    ("expr_assign_to_builtin", "self := 1"),
    ("expr_invalid_char", "x := $"),
    ("expr_bad_sign", "x := @"),
    ("expr_bad_binary", "x +"),
    ("expr_bad_kw", "x at:"),
    ("expr_bad_kw_arg", "x at: 1 put:"),
    ("expr_kw_break", "x at: 1 put : 2"),
    ("expr_unclosed_brace", "{ 1 . 2"),
    ("expr_unclosed_paren", "( 1 + 2"),
    ("expr_unclosed_block", "[ 1 + 2"),
    ("expr_bad_cascade_start", "; foo"),
    ("expr_double_op", "1 + + 2"),
    ("expr_bad_dot_place", ". 1"),
    ("expr_bad_return", "1 + ^ 2"),
    ("expr_cascade_no_receiver", "; msg"),
]

POSITIVE_BLOCKS = [
    ("blk_empty", "[]"),
    ("blk_space", "[ ]"),
    ("blk_ret_lit", "[ 1 ]"),
    ("blk_stmts", "[ 1. 2. ]"),
    ("blk_arg", "[ :x | x + 1 ]"),
    ("blk_args", "[ :a :b | a + b ]"),
    ("blk_args_stmt", "[ :a :b :c | a. b. c ]"),
    ("blk_temp", "[ | temp | temp := 1 ]"),
    ("blk_empty_temp", "[ | | 1 ]"),
    ("blk_arg_temp", "[ :x | | t | t := x. t ]"),
    ("blk_args_empty_temp", "[ :x :y | || x + y ]"),
    ("blk_complex_arg", "[ :x :y | | z | z := x + y. z * 2 ]"),
    ("blk_nested", "[ [ ] ]"),
    ("blk_nested_arg", "[ :x | [ :y | x + y ] ]"),
    ("blk_return", "[ ^ 1 ]"),
    ("blk_return_arg", "[ :x | ^ x ]"),
    ("blk_nested_ret", "[ [ ^ 1 ] ]"),
    ("blk_read_write_temp", "[ | x | x := 1. x ]"),
    ("blk_comment_in_args", "[ :x \"comment\" :y | x+y ]"),
]

NEGATIVE_BLOCKS = [
    ("blk_unclosed", "["), ("blk_unopened", "]"),
    ("blk_bad_arg_sep", "[ : | ]"),
    ("blk_bad_arg_name", "[ :1 | ]"),
    ("blk_missing_bar", "[ :x  x + 1 ]"),
    ("blk_missing_temp_bar", "[ | x ]"),
    ("blk_bad_temp_struct", "[ :x | | y ]"),
    ("blk_bad_nesting", "[ [ ]"),
]

POSITIVE_STRUCTURE = [
    ("struct_dot", "x := 1."),
    ("struct_dots", "x := 1. y := 2."),
    ("struct_no_dot", "x := 1. y := 2"),
    ("struct_ret", "x := 1. ^ 2"),
    ("struct_comment", "x := 1 \"comment\" ."),
    ("struct_comment_start", "\"start\" x := 1"),
    ("struct_comment_mid", "x := \"mid\" 1"),
    ("struct_newlines", "x\n:=\n1"),
    ("struct_tabs", "\tx\t:=\t1"),
]

POSITIVE_METHODS = [
    # --- Unary ---
    ("meth_unary", "unaryMethod ^ self"),
    ("meth_unary_simple", "simple ^ 1"),
    ("meth_unary_super", "initialize ^ super initialize"),
    ("meth_unary_empty", "nothing"),
    ("meth_unary_cmts", "commented \"comment\" ^ self"),
    # --- Binary ---
    ("meth_bin_plus", "+ other ^ self plus: other"),
    ("meth_bin_multiple_plus", "++++ other ^ self plus: other"),
    ("meth_bin_star", "* aNumber ^ self multiply: aNumber"),
    ("meth_bin_multiple_star", "**** aNumber ^ self multiply: aNumber"),
    ("meth_bin_comma", ", anObject ^ self append: anObject"),
    ("meth_bin_multiple_comma", ",,,, anObject ^ self append: anObject"),
    ("meth_bin_eq", "= other ^ self isIdentical: other"),
    ("meth_bin_multiple_eq", "==== other ^ self isIdentical: other"),
    ("meth_bin_leq", "< other ^ self isIdentical: other"),
    ("meth_bin_multiple_leq", "<<<< other ^ self isIdentical: other"),
    ("meth_bin_geq", "> other ^ self isIdentical: other"),
    ("meth_bin_multiple_geq", ">>>> other ^ self isIdentical: other"),
    ("meth_bin_and", "& other ^ self isIdentical: other"),
    ("meth_bin_multiple_and", "&&&& other ^ self isIdentical: other"),
    ("meth_bin_pipe", "| other ^ self isIdentical: other"),
    ("meth_bin_multiple_pipe", "|||| other ^ self isIdentical: other"),
    # --- Keyword ---
    ("meth_kw_at", "at: index ^ elements at: index"),
    ("meth_kw_at_put", "at: key put: value ^ self set: key to: value"),
    ("meth_kw_loop", "from: start to: end do: block ^ self iter"),
    ("meth_kw_long", "a: x b: y c: z ^ x + y + z"),
    ("meth_kw_no_args", "arg: x ^ x"),
    # --- Temporaries ---
    ("meth_temp", "foo | x | x := 1. ^ x"),
    ("meth_temp_multi", "bar | a b c | a := 1. b := 2. c := 3. ^ a+b+c"),
    ("meth_empty_temp_without_space", "bar || ^5"),
    ("meth_empty_temp_with_space", "bar | | ^5"),
    # --- Pragmas ---
    ("meth_pragma", "foo <primitive: 1> ^ self"),
    ("meth_pragmas", "foo <bar> <baz: 1> ^ 2"),
    ("meth_complex_pragma", "foo <primitive: 'name' module: 'mod' error: ec> ^ ec"),
    ("meth_pragma_temp", "foo <trace> | x | x := 1"),
    # --- Return Structure ---
    ("meth_ret_dot", "foo ^ self."),
]

NEGATIVE_METHODS = [
    ("meth_double_return", "unaryMethod ^ self. ^ super"),
    ("meth_dot_binary", ". other ^ self"),
    ("meth_lbracket_binary", "[ other ^ self"),
    ("meth_rbracket_binary", "] other ^ self"),
    ("meth_lbrace_binary", "{ other ^ self"),
    ("meth_rbrace_binary", "} other ^ self"),
    ("meth_lparen_binary", "( other ^ self"),
    ("meth_rparen_binary", ") other ^ self"),
    ("meth_missing_selector", "^ 1"),
    ("meth_bad_selector", "1plus2 ^ 3"),
    ("meth_builtin_arg", "at: true ^ 1"),
    ("meth_args_mismatch", "a: x b: ^ 1"),
    ("meth_pragma_after_stmts", "m x := 1. <foo> ^ x"),
]

POSITIVE_PRAGMAS = [
    ("pragma_unary", "m <foo> ^ 1"),
    ("pragma_kw_1", "m <foo: 1> ^ 1"),
    ("pragma_kw_2", "m <foo: 1 bar: 2> ^ 1"),
    ("pragma_bin_eq", "m < = 2 > ^ 1"),
    ("pragma_args_types", "m <foo: true bar: 'str' baz: #sym qux: $c> ^ 1"),
    ("pragma_multi", "m <p1> <p2: 1> ^ self"),
    ("pragma_negative", "m <id: -1> ^ 1"),
]

NEGATIVE_PRAGMAS = [
    ("pragma_no_angle", "m foo: 1 ^ 1"),
    ("pragma_unclosed", "m <foo: 1 ^ 1"),
    ("pragma_bad_arg", "m <foo 1> ^ 1"),
    ("pragma_misplaced", "<foo: 1> m ^ 1"),
]

def create_test_method(code, should_pass, name):
    def test_func(self):
        self.check_parsing(code, should_pass, msg=f"Case: {name}")
    return test_func

cases = [
    (POSITIVE_LITERALS, True, "literal_pos", wrap_in_method),
    (NEGATIVE_LITERALS, False, "literal_neg", wrap_in_method),
    (POSITIVE_EXPRESSIONS, True, "expr_pos", wrap_stmts_in_method),
    (NEGATIVE_EXPRESSIONS, False, "expr_neg", wrap_stmts_in_method),
    (POSITIVE_BLOCKS, True, "block_pos", wrap_in_method),
    (NEGATIVE_BLOCKS, False, "block_neg", wrap_in_method),
    (POSITIVE_STRUCTURE, True, "structure_pos", wrap_stmts_in_method),
    (POSITIVE_METHODS, True, "method_pos", lambda c: c),
    (NEGATIVE_METHODS, False, "method_neg", lambda c: c),
    (POSITIVE_PRAGMAS, True, "pragma_pos", lambda c: c),
    (NEGATIVE_PRAGMAS, False, "pragma_neg", lambda c: c),
]

for case_list, should_pass, prefix, wrapper in cases:
    for name, code in case_list:
        test_name = f"test_{prefix}_{name}"
        setattr(
            TestPharoParser,
            test_name,
            create_test_method(wrapper(code), should_pass, name)
        )

if __name__ == '__main__':
    unittest.main()
