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
    /// A nonterminal name for identifiers. This is
    /// the Moon-Moon of the micron parser.
    let identifierIdentifier = "identifier"
    /// A nonterminal name for if-then-else expressions.
    let ifThenElseIdentifier = "if-then-else"
    /// A nonterminal name for let-expressions.
    let letIdentifier = "let"
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
                // $expr -> $apply
                ProductionRule(expressionGroupIdentifier, [Nonterminal applyGroupIdentifier])

                // $apply -> apply | $paren
                ProductionRule(applyGroupIdentifier, [Nonterminal applyIdentifier])
                ProductionRule(applyGroupIdentifier, [Nonterminal parenGroupIdentifier])
                // apply -> $apply $expr
                ProductionRule(applyIdentifier, [Nonterminal applyGroupIdentifier; Nonterminal expressionGroupIdentifier])

                // $paren -> paren | if-then-else | let | identifier | literal-int | literal-double
                ProductionRule(parenGroupIdentifier, [Nonterminal parenIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal identifierIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal literalIntIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal literalDoubleIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal ifThenElseIdentifier])
                ProductionRule(parenGroupIdentifier, [Nonterminal letIdentifier])
                
                // paren -> <(> $expr <)>
                ProductionRule(parenIdentifier, [Terminal TokenType.LParen; Nonterminal expressionGroupIdentifier; Terminal TokenType.RParen])
                // if-then-else -> <if> $expr <then> $expr <else> $expr
                ProductionRule(ifThenElseIdentifier, [Terminal TokenType.IfKeyword; Nonterminal expressionGroupIdentifier; 
                                                      Terminal TokenType.ThenKeyword; Nonterminal expressionGroupIdentifier; 
                                                      Terminal TokenType.ElseKeyword; Nonterminal expressionGroupIdentifier])
                // TODO: implement let-expressions

                // identifier -> <identifier>
                ProductionRule(identifierIdentifier, [Terminal TokenType.Identifier])
                // literal-int -> <integer>
                ProductionRule(literalIntIdentifier, [Terminal TokenType.Integer])
                // literal-double -> <double>
                ProductionRule(literalDoubleIdentifier, [Terminal TokenType.Double])
            ]

    /// A grammar for micron expressions.
    let expressionGrammar : ContextFreeGrammar<string, TokenType> =
        ContextFreeGrammar(expressionRules, expressionGroupIdentifier)

    /// A grammar for micron programs.
    let programGrammar : ContextFreeGrammar<string, TokenType> =
        // TODO: create the top-level grammar for micron here.
        let rules = 
            [
                // program -> epsilon
                ProductionRule(programIdentifier, [])
                // program -> module
                ProductionRule(moduleIdentifier, [Nonterminal moduleIdentifier])
                // module -> <module> <identifier>
                // ProductionRule("module", [Terminal TokenType.ModuleKeyword; Terminal TokenType.Identifier])

                // etc
            ]

        let allRules = Set.ofList rules |> Set.union expressionRules
        ContextFreeGrammar(allRules, programIdentifier)

