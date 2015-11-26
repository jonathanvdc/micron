﻿namespace libmicron

open Flame
open Flame.Compiler
open libcontextfree

type Parser<'nt, 't> =
    't list -> Choice<ParseTree<'nt, 't>, 't list>

module Parser =
    // A list of nonterminal names.

    /// A nonterminal name for function application.
    let applyIdentifier = "apply"
    let parenIdentifier = "paren"
    let programIdentifier = "program"
    let moduleIdentifier = "module"
    let operatorIdentifier = "operator"
    /// A nonterminal name for identifiers. This is
    /// the Moon-Moon of the micron parser.
    let identifierIdentifier = "identifier"
    /// A nonterminal name for identifiers.
    let identifierListIdentifier = "identifier..."
    /// A nonterminal name for if-then-else expressions.
    let ifThenElseIdentifier = "if-then-else"
    /// A nonterminal name for let-expressions.
    let letIdentifier = "let"
    /// A nonterminal name for let-definitions.
    let letDefinitionIdentifier = "let-definition"
    /// A nonterminal name for let-definition lists.
    let letDefinitionListIdentifier = "let-definition..."
    /// A nonterminal name for infix specifiers (infixl(n)/infixr(n)).
    let infixSpecifierIdentifier = "infix-specifier"
    /// A nonterminal name for infix specifier keywords (infixl/infixr).
    let infixKeywordIdentifier = "infix-keyword"
    /// A nonterminal name for integer literals.
    let literalIntIdentifier = "literal-int"
    /// A nonterminal name for double literals.
    let literalDoubleIdentifier = "literal-double"

    // All expressions starting with a dollar sign exist
    // solely to group other nonterminals, or for precedence reasons.
    // They are listed below.

    let exprGroupIdentifier = "$expr"
    let applyGroupIdentifier = "$apply"
    let parenGroupIdentifier = "$paren"

    /// Tests if the given nonterminal name is really a group
    /// name. An internal node in a parse tree
    /// with a group name nonterminal as its label derives its 
    /// meaning entirely from its only child, which must itself
    /// be a production node.
    let isGroupIdentifier (ident : string) =
        ident.StartsWith("$")

    /// Replaces group production nodes by their child.
    ///
    /// For example, a tree such as 
    ///
    ///         $expr
    ///           |
    ///        $paren
    ///           | 
    ///       identifier
    ///           |
    ///           f
    ///
    /// is simplified to
    ///
    ///       identifier
    ///           |
    ///           f
    let rec stripGroups : ParseTree<string, Token> -> ParseTree<string, Token> = function
    | TerminalLeaf _ as node -> node
    | ProductionNode(head, [child]) when isGroupIdentifier head -> 
        stripGroups child
    | ProductionNode(head, items) ->
        ProductionNode(head, List.map stripGroups items)

    /// A set of production rules for expressions.
    let expressionRules : Set<ProductionRule<string, TokenType>> =
        let (-->) A β = ProductionRule(A, β)
        Set.ofList
            [
                // $expr -> $apply | if-then-else | let | operator
                exprGroupIdentifier --> [Nonterminal applyGroupIdentifier]
                exprGroupIdentifier --> [Nonterminal ifThenElseIdentifier]
                exprGroupIdentifier --> [Nonterminal letIdentifier]
                exprGroupIdentifier --> [Nonterminal operatorIdentifier]

                // $apply -> apply | $paren
                applyGroupIdentifier --> [Nonterminal applyIdentifier]
                applyGroupIdentifier --> [Nonterminal parenGroupIdentifier]
                // apply -> $apply $paren
                applyIdentifier --> [Nonterminal applyGroupIdentifier
                                     Nonterminal parenGroupIdentifier]

                // $paren -> paren | identifier | literal-int | literal-double
                parenGroupIdentifier --> [Nonterminal parenIdentifier]
                parenGroupIdentifier --> [Nonterminal identifierIdentifier]
                parenGroupIdentifier --> [Nonterminal literalIntIdentifier]
                parenGroupIdentifier --> [Nonterminal literalDoubleIdentifier]
                
                // paren -> <(> $expr <)>
                parenIdentifier --> [Terminal TokenType.LParen
                                     Nonterminal exprGroupIdentifier
                                     Terminal TokenType.RParen]

                // if-then-else -> <if> $expr <then> $expr <else> $expr
                ifThenElseIdentifier --> [Terminal TokenType.IfKeyword
                                          Nonterminal exprGroupIdentifier
                                          Terminal TokenType.ThenKeyword
                                          Nonterminal exprGroupIdentifier
                                          Terminal TokenType.ElseKeyword
                                          Nonterminal exprGroupIdentifier]

                // let -> let-definition <in> $expr
                letIdentifier --> [Nonterminal letDefinitionIdentifier
                                   Terminal TokenType.InKeyword
                                   Nonterminal exprGroupIdentifier]
                                               
                // let-definition -> <let> <identifier> identifier... <=> $expr
                letDefinitionIdentifier --> [Terminal TokenType.LetKeyword
                                             Terminal TokenType.Identifier
                                             Nonterminal identifierListIdentifier
                                             Terminal TokenType.Equals
                                             Nonterminal exprGroupIdentifier]

                // let-definition -> <let> <fixity> identifier <op> identifier <=> $expr
                letDefinitionIdentifier --> [Terminal TokenType.LetKeyword
                                             Nonterminal infixSpecifierIdentifier
                                             Nonterminal identifierIdentifier
                                             Terminal TokenType.OperatorToken
                                             Nonterminal identifierIdentifier
                                             Terminal TokenType.Equals
                                             Nonterminal exprGroupIdentifier]

                // infix-specifier -> infix-keyword <left-parenthesis> <literal-int> <right-parenthesis
                infixSpecifierIdentifier --> [Nonterminal infixKeywordIdentifier
                                              Terminal TokenType.LParen
                                              Terminal TokenType.Integer
                                              Terminal TokenType.RParen]

                // infix-keyword -> <infixl> | <infixr>
                infixKeywordIdentifier --> [Terminal TokenType.InfixlKeyword]
                infixKeywordIdentifier --> [Terminal TokenType.InfixrKeyword]

                // identifier... -> ε | <identifier> identifier...
                identifierListIdentifier --> []
                identifierListIdentifier --> [Terminal TokenType.Identifier
                                              Nonterminal identifierListIdentifier]

                // let-definition... -> ε | let-definition let-definition...
                letDefinitionListIdentifier --> []
                letDefinitionListIdentifier --> [Nonterminal letDefinitionIdentifier
                                                 Nonterminal letDefinitionListIdentifier]

                // identifier -> <identifier>
                identifierIdentifier --> [Terminal TokenType.Identifier]
                // literal-int -> <integer>
                literalIntIdentifier --> [Terminal TokenType.Integer]
                // literal-double -> <double>
                literalDoubleIdentifier --> [Terminal TokenType.Double]
                // operator -> $expr <op> $paren
                operatorIdentifier --> [Nonterminal parenGroupIdentifier
                                        Terminal TokenType.OperatorToken
                                        Nonterminal exprGroupIdentifier]
            ]

    /// A grammar for micron expressions.
    let expressionGrammar : ContextFreeGrammar<string, TokenType> =
        ContextFreeGrammar(expressionRules, exprGroupIdentifier)

    /// A grammar for micron programs.
    let programGrammar : ContextFreeGrammar<string, TokenType> =
        // TODO: create the top-level grammar for micron here.
        let rules = 
            let (-->) A β = ProductionRule(A, β)
            [
                // program -> ε | let-definition program | module
                programIdentifier --> []
                programIdentifier --> [Nonterminal letDefinitionIdentifier
                                       Nonterminal programIdentifier]
                programIdentifier --> [Nonterminal moduleIdentifier]

                // module -> <module> <identifier> let-definition...
                moduleIdentifier --> [Terminal TokenType.ModuleKeyword
                                      Terminal TokenType.Identifier
                                      Nonterminal letDefinitionListIdentifier]

                // etc
            ]

        let allRules = Set.ofList rules |> Set.union expressionRules
        ContextFreeGrammar(allRules, programIdentifier)

    /// Tries to create an LR(1) parser for the given grammar.
    let tryCreateParser : ContextFreeGrammar<string, TokenType> -> Result<Parser<string, Token>> =
        LRParser.createLR1 >> Result.map LRParser.toFunctionalParser
                           >> Result.map ((<|||) (LRParser.parse TokenHelpers.tokenType))

    /// Creates an LR(1) parser for the given grammar. If this
    /// cannot be done, an exception is thrown.
    let createParser : ContextFreeGrammar<string, TokenType> -> Parser<string, Token> =
        tryCreateParser >> Result.get

    // The fixity of an operator.
    type OpFixity = InfixLeft of int
                  | InfixRight of int

    // Extract only the precedence from the fixity.
    let precedence : OpFixity -> int = function | InfixLeft i -> i
                                                | InfixRight i -> i

    /// Flatten those ops boy
    let rec flattenOps : ParseTree<string, Token> -> (ParseTree<string, Token> * Token) list * ParseTree<string, Token> = function
        | ProductionNode("operator", [left; TerminalLeaf op; right]) -> 
            let lFlat, lSuffix = flattenOps left
            let rFlat, rSuffix = flattenOps right
            List.append lFlat ((lSuffix, op) :: rFlat), rSuffix
        | node -> [], node

    /// Reasosciaete the operators to obey the order of operators
    let reassociate (prec : string -> OpFixity) (tree : ParseTree<string, Token>) : ParseTree<string, Token> =

        let rec subReassociate : (ParseTree<string, Token> * Token) list * ParseTree<string, Token> -> ParseTree<string, Token> = function
            | [], suf -> suf
            | items, suf -> 
                let _, maxPrec = List.maxBy (fun (_, op) -> precedence (prec op.contents)) items
                let exprComp  (expr, op) = if op.contents = maxPrec.contents then Some expr else None
                let lTree, rTree = 
                    match prec maxPrec.contents with
                    | InfixLeft _ -> 
                        let ls, mid, rs = Option.get (ListHelpers.splitAtFirst exprComp items)
                        subReassociate (ls, mid), subReassociate (rs, suf)
                    | InfixRight _ ->
                        let ls, mid, rs = Option.get (ListHelpers.splitAtLast exprComp items)
                        subReassociate (ls, mid), subReassociate (rs, suf)

                ProductionNode("operator", [lTree; TerminalLeaf maxPrec; rTree])

        let flattened = flattenOps tree
        subReassociate flattened