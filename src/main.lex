structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
        "line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor MainLexFun(structure Tokens: Main_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
any=.;
ident=[A-Za-z]+;  

%%

\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());


"let" => (Tokens.LET(!pos,!pos));
"val" => (Tokens.VAL(!pos,!pos));
"rec" => (Tokens.REC(!pos,!pos));
"if" => (Tokens.IF(!pos,!pos));
"then" => (Tokens.THEN(!pos,!pos));
"else" => (Tokens.ELSE(!pos,!pos));
"fn" => (Tokens.FN(!pos,!pos));
"=>" => (Tokens.ARROW(!pos,!pos));
"=" => (Tokens.ASSIGN(!pos,!pos));
">" => (Tokens.GT(!pos,!pos));
"<" => (Tokens.LT(!pos,!pos));
"(" => (Tokens.OPAREN(!pos,!pos));
")" => (Tokens.CPAREN(!pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));

[a-zA-Z][a-zA-Z0-9]*  => (Tokens.ID(yytext,!pos,!pos));
{digit}+=> (Tokens.NUM (valOf (Int.fromString yytext), !pos, !pos));

{any} => (error ("Bad character "^yytext,!pos,!pos);
             (raise (Fail "LexError")));
