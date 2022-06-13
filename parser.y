%{
#include <iostream>
#include "symbols.h"
#include "lex.yy.cpp"
#include <string>

#define Trace(t) cout <<"Progress:\t"<<t<<endl;
void yyerror (string s);
List_Symbol_Table symbol_list;
vector<vector<Tuple_Identity>> check_arguments;
%}

/* Union is used to declare the datatype of the inputs given*/
%union{
	int int_dataType;
	double double_dataType;
	bool bool_dataType;
	string *string_dataType;
	Tuple_Identity* compound_dataType;
	int dataType;
}

/* token is used to declare what are the possible sent values from the scanner */
%token ADDEQ SUBEQ MULEQ DIVEQ MODEQ INC DEC AND OR EQ NEQ REQ NREQ LEQ GEQ NONNULL NULLABLE LAMBDA DD
%token BOOL BREAK CHAR CASE CLASS CONTINUE DECLARE DO ELSE EXIT FLOAT FOR FUN IF IN INT LOOP PRINT PRINTLN READ RETURN STRING VAL VAR VOID WHILE
/* the tokens below are declared with their own respective datatype, while the tokens above has no datatype */
%token <int_dataType> INT_CONST
%token <double_dataType> REAL_CONST
%token <bool_dataType> BOOL_CONST
%token <string_dataType> STR_CONST
%token <string_dataType> ID

%type <compound_dataType> constant_values expression call_function logical_expression relational_expression bool_expression calculation_expression variable_choice_variation constant_choice_variation
%type <dataType> data_type

/* For the expression precedence */
%left OR
%left AND 
%left '!'
%left '<' LEQ EQ GEQ '>' NEQ
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

%%

class_choice	: CLASS ID
				{
					if (symbol_list.lookup_top(*$2) != NULL)
					{
						yyerror("Identifier has been used");
					}
					
					Tuple_Identity *data = new Tuple_Identity(*$2, type_class, add_data_class, false);
						
					symbol_list.insert(*$2, *data);
					symbol_list.push();
				}
				'{' inside_class '}'
				{
					Trace("Reducing to Program");

					if(symbol_list.lookup_top("main") == NULL)
					{
						yyerror("Program has no main function");
					}
					symbol_list.dump();
					symbol_list.pop();
				}
				;

inside_class	: inside_class function_choice 
				| inside_class variable_choice 
				| inside_class constant_choice 
				| inside_class statement_choice 
				|
				;

function_choice	: FUN ID
				{
					if (symbol_list.lookup_top(*$2) != NULL)
					{
						yyerror("Identifier has been used");
					}
					
					Tuple_Identity *data = new Tuple_Identity(*$2, type_void, add_data_function, false);

					symbol_list.insert(*$2, *data);
					symbol_list.push();
				}
				function_variation '{' inside_function '}'
				{
					Trace("Declare function");

					Tuple_Identity *data = symbol_list.get_function();

					if(data->type != type_void && data->value.int_dataType == 0)
					{
						yyerror("Function has no return expression declared");
					}

					symbol_list.dump();
					symbol_list.pop();
				}
				;

function_variation	: '(' multiple_arguments ')' ':' data_type
					{
						symbol_list.return_type($5);
					}
					| '(' ')' ':' data_type
					{
						symbol_list.return_type($4);
					}
					|'(' multiple_arguments ')'
					{	

					}
					| '(' ')'
					{

					}
					;

multiple_arguments	: multiple_arguments ',' single_argument 
					| single_argument
					;

single_argument		: ID ':' data_type
					{
						if(symbol_list.lookup_top(*$1) != NULL)
						{
							yyerror("Identifier has been used");
						}
						
						Tuple_Identity *data = new Tuple_Identity(*$1, $3, add_data_constant_variable, true);

						symbol_list.insert(*$1, *data);
						symbol_list.insert_argument(*data);
					}
					;

inside_function		: inside_function variable_choice 
					| inside_function constant_choice
					| inside_function statement_choice
					|
					;

variable_choice		: VAR ID 
					{
						if(symbol_list.lookup_top(*$2) != NULL)
						{
							yyerror("Identifier has been used");
						}
					}
					variable_choice_variation
					{
						$4->s = *$2;
						symbol_list.insert(*$2, *$4);
					}
					;

variable_choice_variation	: ':' data_type '=' expression
							{
								Trace ("Declare variable with defined data type and value");

								if ($2 != $4->type)
								{
									yyerror("Identifier and expression has different data type");
								}
						
								Tuple_Identity *data = new Tuple_Identity("", $4->type, add_data_variable, true);
								data->value = $4->value;

								$$ = data;
							}
							| ':' data_type
							{
								Trace ("Declare variable with defined data type");

								Tuple_Identity *data = new Tuple_Identity("", $2, add_data_variable, false);
								
								$$ = data;
							}
							| '=' expression
							{
								Trace("Declare variable with defined value");

								Tuple_Identity *data = new Tuple_Identity("", $2->type, add_data_variable, true);
								data->value = $2->value;

								$$ = data;
							}
							|
							{
								Trace("Declare variable");

								Tuple_Identity *data = new Tuple_Identity("", type_integer, add_data_variable, false);
								
								$$ = data;
							}
							;

constant_choice		: VAL ID
					{
						if(symbol_list.lookup_top(*$2) != NULL)
						{
							yyerror("Identifier has been used");
						}
					}
					constant_choice_variation
					{
						$4->s = *$2;
						symbol_list.insert(*$2, *$4);
					}
					;

constant_choice_variation	: ':' data_type '=' expression
							{
								Trace ("Declare constant value with defined data type");

								if ($2 != $4->type)
								{
									yyerror("Identifier and expression has different data type");
								}
								else
								{
									if (check_constant(*$4) == false)
									{
										yyerror("Expression is not constant");
									}
								}
						
								Tuple_Identity *data = new Tuple_Identity("", $4->type, add_data_constant_variable, true);
								data->value = $4->value;

								$$ = data;
							}
							| '=' expression
							{
								Trace("Declare constant value without defined data type");

								if(check_constant(*$2) == false)
								{
									yyerror("Expression is not constant");
								}

								Tuple_Identity *data = new Tuple_Identity("", $2->type, add_data_constant_variable, true);
								data->value = $2->value;

								$$ = data;
							}
							;

statement_choice	: simple_statement
					| conditional_statement
					| loop_statement
					;

simple_statement	: call_function
					{
						Trace("Declare statement: Call function");
					}
					| ID
					{
						Tuple_Identity *data = symbol_list.lookup(*$1);
						if(data == NULL)
						{
							yyerror("Identifier not found");
						}
						if(data->additional_data_type != add_data_variable)
						{
							yyerror("Identifier is not a variable");
						}
					}
					'=' expression
					{
						Trace("Declare statement: Assign value to variable");

						Tuple_Identity *data = symbol_list.lookup(*$1);
						if (data->type != $4->type)
						{
							yyerror("Identifier and expression has different data type");
						}
					}
					| PRINT expression
					{
						/* print */
						Trace("Declare statement: Print");
					}
                    | PRINT '(' expression ')'
					{
						Trace("Declare statement: Print()");
					}
					| PRINTLN expression
					{
						/* println */
						Trace("Declare statement: Println");
					}
					| PRINTLN '(' expression ')'
					{
						Trace("Declare statement: Println()");
					}
					| RETURN expression
					{
						Trace("Declare statement: Return value");

						Tuple_Identity *data = symbol_list.get_function();
						if(data == NULL)
						{
							yyerror("Return out of function");
						}
						else
						{
							if(data->type != $2->type)
							{
								yyerror("Return expression data type doesn't match function return data type");
							}
						}
							
						data->value.int_dataType += 1;
					}
					| RETURN
					{
						Trace("Declare statement: Return NONE");

						Tuple_Identity *data = symbol_list.get_function();
						if(data == NULL)
						{
							yyerror("Return out of function");
						}
						else
						{
							if(data->type != type_void)
							{
								yyerror("Function need return value");
							}
						}
					}
					;

conditional_statement	: IF '(' bool_expression ')' 
						{
							symbol_list.push();
						}
						block_or_simple_conditional 
						{
							Trace("Declare statement: conditional statement");
							symbol_list.dump();
							symbol_list.pop();
						}
						else_choice
						{

						}
						;

else_choice				:ELSE 
						{
							symbol_list.push();
						}
						block_or_simple_conditional
						{
							Trace("Declare conditional: With ELSE");
							symbol_list.dump();
							symbol_list.pop();
						}
						| 
						{
							Trace("Declare conditional: without ELSE");
						}
						;

block_or_simple_conditional	: '{' inside_block_conditional '}'
							| simple_statement
							;

inside_block_conditional	: inside_block_conditional statement_choice 
							|
							;

loop_statement			: WHILE '(' bool_expression ')' 
						{
							symbol_list.push();
						}
						block_or_simple_loop
						{
							Trace("Declare statement: While loop");
							symbol_list.dump();
							symbol_list.pop();
						}
						| FOR '(' ID IN INT_CONST
						{
							Tuple_Identity *check = symbol_list.lookup_top(*$3);
							if(check != NULL)
							{
								if(check->type != type_integer || check->additional_data_type == add_data_constant_variable)
								{
									yyerror("Identifier type is not appropriate");
								}
								else
								{
									yyerror("Identifier has been used");
								}
							}
							
							symbol_list.push();
							Tuple_Identity *data = new Tuple_Identity(*$3, type_integer, add_data_variable, true);
							data->value.int_dataType = $5;
							symbol_list.insert(*$3, *data);
						}
						DD INT_CONST ')' block_or_simple_loop
						{
							Trace("Declare statement: For loop statement");
							
							symbol_list.dump();
							symbol_list.pop();
						}
						;

block_or_simple_loop	: '{' inside_block_loop '}'
						| simple_statement
						| BREAK
						| CONTINUE
						;

inside_block_loop		: inside_block_loop statement_choice 
						| inside_block_loop BREAK 
						| inside_block_loop CONTINUE 
						|
						;

call_function			: ID
						{
							Tuple_Identity *check = symbol_list.lookup(*$1);
							if(check == NULL)
							{
								yyerror("Identifier not found");
							}
							if(check->additional_data_type != add_data_function)
							{
								yyerror("Identifier is not a function");
							}
							check_arguments.push_back(vector<Tuple_Identity>());
						}
						'(' check_call_function_argument ')'
						{
							Tuple_Identity *check = symbol_list.lookup(*$1);
							vector<Tuple_Identity> para = check->value.array_dataType;
							if(para.size() != check_arguments[check_arguments.size()-1].size())
							{
								yyerror("Number of arguments not match");
							}

							for(int i = 0; i < para.size(); ++i)
		                    {
							    if(para[i].type != check_arguments[check_arguments.size() - 1][i].type) 
								{
									yyerror("Parameter and arguments have different data type");
								}
						    }

						    $$ = check;
						    check_arguments.pop_back();
						}
						;

check_call_function_argument	: comma_seperated_arguments
								|
								;

comma_seperated_arguments		: comma_seperated_arguments ',' call_function_parameter 
								| call_function_parameter
								;
	
call_function_parameter		: expression
							{
								check_arguments[check_arguments.size() - 1].push_back(*$1);
							}
							;

expression		: call_function
				{
					Trace("Function invocation");
					if($1->type == type_void)
					{
						yyerror("Function invocation has void return");
					}
				}
				| ID
				{
					Tuple_Identity *data = symbol_list.lookup(*$1);
					if(data == NULL)
					{
						yyerror("Identifier not found");
					}
					else
					{
						if(data->additional_data_type == add_data_class || data->additional_data_type == add_data_function)
						{
							yyerror("Identifier is a class or function type");
						}
					}

					$$ = data;
				}
				| '(' expression ')'
				{
					Trace("Expression in brackets");
					$$ = $2;
				}
				| calculation_expression
				| bool_expression
				| constant_values
				;

calculation_expression	: '-' expression %prec UMINUS
						{
							Trace("Negative expression");
							
							if($2->type != type_integer && $2->type != type_real)
							{
								yyerror("Expression is not integer nor double data type");
							}
														
							string output = "";
							if(check_constant(*$2) == true)
							{
								output += "sipush ";
								if($2->type == type_integer)
								{									
									output += to_string($2->value.int_dataType);
								}
								else if($2->type == type_real)
								{
									output += to_string($2->value.double_dataType);
								}
								cout<<output;
							}
							else if($2->s != "")
							{
								cout<<"Error HERE";
								if(symbol_list.lookup_top($2->s) == NULL)
								{
									output += "getstatic ";
									if($2->type == type_integer)
									{
										output += "int ";
									}
									else if($2->type == type_real)
									{
										output += "double ";
									}
									Tuple_Identity* check_class = symbol_list.get_class();
									output += check_class->s;
									output += ".";
									output += $2->s;
									cout<<output;
								}
								else
								{
									output += "iload ";
									output += to_string($2->idx);
								}
							}
							output += "\nineg";
							cout<< output;
							/*negativeint(output);*/

							Tuple_Identity *data = new Tuple_Identity("", $2->type, add_data_variable, false);
							$$ = data;

						}
						| expression '*' expression
						{
							Trace("Expression Multiplication");

							if(($1->type == type_integer || $1->type == type_real) && ($3->type == type_integer || $3->type == type_real))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real))
							{
								yyerror("Left expression is not integer nor double data type");
							}
							if(($3->type != type_integer && $3->type != type_real))
							{
								yyerror("Right expression is not integer nor double data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", $1->type, add_data_variable, false);
							$$ = data;
						}
						| expression '/' expression
						{
							Trace("Expression Division");

							if(($1->type == type_integer || $1->type == type_real) && ($3->type == type_integer || $3->type == type_real))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real))
							{
								yyerror("Left expression is not integer nor double data type");
							}
							if(($3->type != type_integer && $3->type != type_real))
							{
								yyerror("Right expression is not integer nor double data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", $1->type, add_data_variable, false);
							$$ = data;
						
						}
						| expression '+' expression
						{
							Trace("Expression Addition");

							if(($1->type == type_integer || $1->type == type_real) && ($3->type == type_integer || $3->type == type_real))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real))
							{
								yyerror("Left expression is not integer nor double data type");
							}
							if(($3->type != type_integer && $3->type != type_real))
							{
								yyerror("Right expression is not integer nor double data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", $1->type, add_data_variable, false);
							$$ = data;
						
						}
						| expression '-' expression
						{
							Trace("Expression Substraction");

							if(($1->type == type_integer || $1->type == type_real) && ($3->type == type_integer || $3->type == type_real))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real))
							{
								yyerror("Left expression is not integer nor double data type");
							}
							if(($3->type != type_integer && $3->type != type_real))
							{
								yyerror("Right expression is not integer nor double data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", $1->type, add_data_variable, false);
							$$ = data;
						
						}
						;

bool_expression		: relational_expression
					| logical_expression
					;

relational_expression	: expression '<' expression
						{
							Trace("Expression Less Than Comparison");

							if(($1->type == type_integer || $1->type == type_real) && ($3->type == type_integer || $3->type == type_real))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real))
							{
								yyerror("Left expression is not integer nor double data type");
							}
							if(($3->type != type_integer && $3->type != type_real))
							{
								yyerror("Right expression is not integer nor double data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						| expression LEQ expression
						{
							Trace("Expression Less Than or Equal to Comparison");

							if(($1->type == type_integer || $1->type == type_real) && ($3->type == type_integer || $3->type == type_real))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real))
							{
								yyerror("Left expression is not integer nor double data type");
							}
							if(($3->type != type_integer && $3->type != type_real))
							{
								yyerror("Right expression is not integer nor double data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						| expression '>' expression
						{
							Trace("Expression Greater Than Comparison");

							if(($1->type == type_integer || $1->type == type_real) && ($3->type == type_integer || $3->type == type_real))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real))
							{
								yyerror("Left expression is not integer nor double data type");
							}
							if(($3->type != type_integer && $3->type != type_real))
							{
								yyerror("Right expression is not integer nor double data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						| expression GEQ expression
						{
							Trace("Expression Greater Than or Equal to Comparison");

							if(($1->type == type_integer || $1->type == type_real) && ($3->type == type_integer || $3->type == type_real))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real))
							{
								yyerror("Left expression is not integer nor double data type");
							}
							if(($3->type != type_integer && $3->type != type_real))
							{
								yyerror("Right expression is not integer nor double data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						| expression EQ expression
						{
							Trace("Expression Equality Comparison");

							if(($1->type == type_integer || $1->type == type_real || $1->type == type_bool) && ($3->type == type_integer || $3->type == type_real || $3->type == type_bool))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real && $1->type != type_bool))
							{
								yyerror("Left expression is not integer, double, nor bool data type");
							}
							if(($3->type != type_integer && $3->type != type_real && $3->type != type_bool))
							{
								yyerror("Right expression is not integer, double, nor bool data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						| expression NEQ expression
						{
							Trace("Expression Not Equality Comparison");

							if(($1->type == type_integer || $1->type == type_real || $1->type == type_bool) && ($3->type == type_integer || $3->type == type_real || $3->type == type_bool))
							{
								if($1->type != $3->type)
								{
									yyerror("Both expression has different data type");
								}
							}
							if(($1->type != type_integer && $1->type != type_real && $1->type != type_bool))
							{
								yyerror("Left expression is not integer, double, nor bool data type");
							}
							if(($3->type != type_integer && $3->type != type_real && $3->type != type_bool))
							{
								yyerror("Right expression is not integer, double, nor bool data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						;

logical_expression		: '!' expression
						{
							Trace("Expression Negation");

							if($2->type != type_bool)
							{
								yyerror("Expression is not bool data type");
							}
							
							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						| expression AND expression
						{
							Trace("Expression AND Logic");

							if(($1->type != type_bool) || ($3->type != type_bool))
							{
								yyerror("Expression is not bool data type");
							}

							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						| expression OR expression
						{
							Trace("Expression OR Logic");

							if(($1->type != type_bool) || ($3->type != type_bool))
							{
								yyerror("Expression is not bool data type");
							}
							
							Tuple_Identity *data = new Tuple_Identity("", type_bool, add_data_variable, false);
							$$ = data;
						}
						;

constant_values			: INT_CONST
						{
							$$ = integer_constant($1);
						}
						| REAL_CONST
						{
							$$ = real_constant($1);
						}
						| BOOL_CONST
						{
							$$ = boolean_constant($1);
						}
						| STR_CONST
						{
							$$ = string_constant($1);
						}
						;

data_type	: INT
			{
				$$ = type_integer;
			}
			| FLOAT
			{
				$$ = type_real;
			}
			| BOOL
			{
				$$ = type_bool;
			}
			| STRING
			{
				$$ = type_string;
			}
			| VOID
			{
				$$ = type_void;
			}
			;

%%

void yyerror(string s) 
{
	cout<<"\033[1;31m"<<s<<"\033[0m"<<endl;
}

int main(int argc, char** argv) 
{
	if(argc != 2)
    {
        cout<<"Usage: sc filename"<<endl;
        exit(1);
    }
    yyin = fopen(argv[1], "r");

    if(yyparse()==1)
    {
        yyerror("Parsing error !");
    }
}
