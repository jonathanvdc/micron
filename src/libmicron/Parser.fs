namespace libmicron

open Flame
open Flame.Compiler
open libcontextfree

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

    let expressionGroupIdentifier = "$expr"
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
        Set.ofList
            [
                // $expr -> $apply | if-then-else | let | operator
                ProductionRule(expressionGroupIdentifier, [Nonterminal applyGroupIdentifier])
                ProductionRule(expressionGroupIdentifier, [Nonterminal ifThenElseIdentifier])
                ProductionRule(expressionGroupIdentifier, [Nonterminal letIdentifier])
                ProductionRule(expressionGroupIdentifier, [Nonterminal operatorIdentifier])

                // $apply -> apply | $paren
                ProductionRule(applyGroupIdentifier, [Nonterminal applyIdentifier])
                ProductionRule(applyGroupIdentifier, [Nonterminal parenGroupIdentifier])
                // apply -> $apply $paren
                ProductionRule(applyIdentifier, [Nonterminal applyGroupIdentifier
                                                 Nonterminal parenGroupIdentifier])

                // $paren -> paren | identifier | literal-int | literal-double
                ProductionRule(parenGroupIdentifier, [Nonterminal parenIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal identifierIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal literalIntIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal literalDoubleIdentifier])
                
                // paren -> <(> $expr <)>
                ProductionRule(parenIdentifier, [Terminal TokenType.LParen
                                                 Nonterminal expressionGroupIdentifier
                                                 Terminal TokenType.RParen])
                // if-then-else -> <if> $expr <then> $expr <else> $expr
                ProductionRule(ifThenElseIdentifier, [Terminal TokenType.IfKeyword
                                                      Nonterminal expressionGroupIdentifier
                                                      Terminal TokenType.ThenKeyword
                                                      Nonterminal expressionGroupIdentifier
                                                      Terminal TokenType.ElseKeyword
                                                      Nonterminal expressionGroupIdentifier])

                // let -> let-definition <in> $expr
                ProductionRule(letIdentifier, [Nonterminal letDefinitionIdentifier
                                               Terminal TokenType.InKeyword
                                               Nonterminal expressionGroupIdentifier])
                                               
                // let-definition -> <let> <identifier> identifier... <=> $expr
                ProductionRule(letDefinitionIdentifier, [Terminal TokenType.LetKeyword
                                                         Terminal TokenType.Identifier
                                                         Nonterminal identifierListIdentifier
                                                         Terminal TokenType.Equals
                                                         Nonterminal expressionGroupIdentifier])

                // let-definition -> <let> <fixity> identifier <op> identifier <=> $expr
                ProductionRule(letDefinitionIdentifier, [Terminal TokenType.LetKeyword
                                                         Nonterminal infixSpecifierIdentifier
                                                         Nonterminal identifierIdentifier
                                                         Terminal TokenType.OperatorToken
                                                         Nonterminal identifierIdentifier
                                                         Terminal TokenType.Equals
                                                         Nonterminal expressionGroupIdentifier])

                // infix-specifier -> infix-keyword <left-parenthesis> <literal-int> <right-parenthesis
                ProductionRule(infixSpecifierIdentifier, [Nonterminal infixKeywordIdentifier
                                                          Terminal TokenType.LParen
                                                          Terminal TokenType.Integer
                                                          Terminal TokenType.RParen])

                // infix-keyword -> <infixl> | <infixr>
                ProductionRule(infixKeywordIdentifier, [Terminal TokenType.InfixlKeyword])
                ProductionRule(infixKeywordIdentifier, [Terminal TokenType.InfixrKeyword])

                // identifier... -> epsilon | <identifier> identifier...
                ProductionRule(identifierListIdentifier, [])
                ProductionRule(identifierListIdentifier, [Terminal TokenType.Identifier
                                                          Nonterminal identifierListIdentifier])

                // let-definition... -> epsilon | let-definition let-definition...
                ProductionRule(letDefinitionListIdentifier, [])
                ProductionRule(letDefinitionListIdentifier, [Nonterminal letDefinitionIdentifier
                                                             Nonterminal letDefinitionListIdentifier])

                // identifier -> <identifier>
                ProductionRule(identifierIdentifier, [Terminal TokenType.Identifier])
                // literal-int -> <integer>
                ProductionRule(literalIntIdentifier, [Terminal TokenType.Integer])
                // literal-double -> <double>
                ProductionRule(literalDoubleIdentifier, [Terminal TokenType.Double])
                // operator -> $expr <op> $paren
                ProductionRule(operatorIdentifier, [Nonterminal parenGroupIdentifier; Terminal TokenType.OperatorToken; Nonterminal expressionGroupIdentifier])
            ]

    /// A grammar for micron expressions.
    let expressionGrammar : ContextFreeGrammar<string, TokenType> =
        ContextFreeGrammar(expressionRules, expressionGroupIdentifier)

    /// A grammar for micron programs.
    let programGrammar : ContextFreeGrammar<string, TokenType> =
        // TODO: create the top-level grammar for micron here.
        let rules = 
            [
                // program -> let-definition program | module | epsilon
                ProductionRule(programIdentifier, [])
                ProductionRule(programIdentifier, [Nonterminal letDefinitionIdentifier
                                                   Nonterminal programIdentifier])
                ProductionRule(programIdentifier, [Nonterminal moduleIdentifier])

                // module -> <module> <identifier> let-definition...
                ProductionRule(moduleIdentifier, [Terminal TokenType.ModuleKeyword
                                                  Terminal TokenType.Identifier
                                                  Nonterminal letDefinitionListIdentifier])

                // etc
            ]

        let allRules = Set.ofList rules |> Set.union expressionRules
        ContextFreeGrammar(allRules, programIdentifier)

    /// Tries to create an LR(1) parser for the given grammar.
    let tryCreateParser (grammar : ContextFreeGrammar<string, TokenType>) =
        LRParser.createLR1 grammar |> Result.map LRParser.toFunctionalParser
                                   |> Result.map ((<|||) (LRParser.parse TokenHelpers.tokenType))

    /// Creates an LR(1) parser for the given grammar. If this
    /// cannot be done, an exception is thrown.
    let createParser (grammar : ContextFreeGrammar<string, TokenType>) =
        match tryCreateParser grammar with
        | Success x -> x
        | Error e -> raise (System.InvalidOperationException(e))

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