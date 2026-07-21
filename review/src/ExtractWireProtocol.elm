module ExtractWireProtocol exposing (rule)

import Dict exposing (Dict)
import Elm.Docs as Docs
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Json.Encode as Encode
import ProtocolIR exposing (TypeDef, TypeKey)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)



-- CONTEXT


type alias ProjectContext =
    { types : Dict TypeKey TypeDef
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : List String
    , types : Dict TypeKey TypeDef
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { types = Dict.empty
    }



-- RULE


rule : Rule
rule =
    Rule.newProjectRuleSchema "ExtractWireProtocol" initialProjectContext
        |> Rule.withDependenciesProjectVisitor dependenciesVisitor
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor



-- DEPENDENCIES (Elm.Docs)


dependenciesVisitor : Dict String Dependency -> ProjectContext -> ( List (Error { useErrorForModule : () }), ProjectContext )
dependenciesVisitor deps context =
    let
        docsModules : List Docs.Module
        docsModules =
            deps
                |> Dict.values
                |> List.concatMap Dependency.modules

        indexed =
            List.foldl indexDocsModule context.types docsModules
    in
    ( [], { context | types = indexed } )


indexDocsModule : Docs.Module -> Dict TypeKey TypeDef -> Dict TypeKey TypeDef
indexDocsModule docsModule acc =
    let
        withUnions =
            List.foldl
                (\union dict ->
                    let
                        ( key, def ) =
                            ProtocolIR.fromDocsUnion docsModule.name union
                    in
                    Dict.insert key def dict
                )
                acc
                docsModule.unions

        withAliases =
            List.foldl
                (\alias_ dict ->
                    let
                        ( key, def ) =
                            ProtocolIR.fromDocsAlias docsModule.name alias_
                    in
                    Dict.insert key def dict
                )
                withUnions
                docsModule.aliases
    in
    withAliases



-- MODULE CONTEXT


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable moduleName _ ->
            { lookupTable = lookupTable
            , moduleName = moduleName
            , types = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleName


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { types = moduleContext.types }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts a b =
    { types = Dict.union a.types b.types }



-- DECLARATIONS


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationListVisitor declarations context =
    let
        newTypes =
            List.foldl (collectDeclaration context) context.types declarations
    in
    ( [], { context | types = newTypes } )


collectDeclaration : ModuleContext -> Node Declaration -> Dict TypeKey TypeDef -> Dict TypeKey TypeDef
collectDeclaration context node acc =
    -- Skip Evergreen versions so roots resolve to live Types.*
    if List.member "Evergreen" context.moduleName then
        acc

    else
        case Node.value node of
            Declaration.CustomTypeDeclaration type_ ->
                let
                    ( key, def ) =
                        ProtocolIR.fromSyntaxCustom context.lookupTable context.moduleName type_
                in
                Dict.insert key def acc

            Declaration.AliasDeclaration alias_ ->
                let
                    ( key, def ) =
                        ProtocolIR.fromSyntaxAlias context.lookupTable context.moduleName alias_
                in
                Dict.insert key def acc

            _ ->
                acc



-- EXTRACT


dataExtractor : ProjectContext -> Encode.Value
dataExtractor projectContext =
    case ProtocolIR.findRoots projectContext.types of
        Err err ->
            Encode.object
                [ ( "ok", Encode.bool False )
                , ( "error", Encode.string err )
                , ( "typeCount", Encode.int (Dict.size projectContext.types) )
                , ( "typeKeys"
                  , projectContext.types
                        |> Dict.keys
                        |> List.sort
                        |> Encode.list Encode.string
                  )
                ]

        Ok ( rootBe, rootFe ) ->
            let
                ( included, unresolved, forceExternal ) =
                    ProtocolIR.closeFromRoots projectContext.types rootBe rootFe

                emitted =
                    ProtocolIR.emitElm rootBe rootFe included unresolved forceExternal

                problemParts =
                    List.filter (not << String.isEmpty)
                        [ if List.isEmpty emitted.errors then
                            ""

                          else
                            String.join "; " emitted.errors
                        , if List.isEmpty emitted.unresolved then
                            ""

                          else
                            "Unresolved types: " ++ String.join ", " emitted.unresolved
                        ]

                ok =
                    List.isEmpty emitted.errors && List.isEmpty emitted.unresolved
            in
            Encode.object
                [ ( "ok", Encode.bool ok )
                , ( "error"
                  , if ok then
                        Encode.null

                    else
                        Encode.string (String.join " | " problemParts)
                  )
                , ( "elmSource", Encode.string emitted.elmSource )
                , ( "proofElmSource", Encode.string emitted.proofElmSource )
                , ( "rootToBackend", Encode.string rootBe )
                , ( "rootToFrontend", Encode.string rootFe )
                , ( "included", Encode.list Encode.string emitted.included )
                , ( "unresolved", Encode.list Encode.string emitted.unresolved )
                , ( "errors", Encode.list Encode.string emitted.errors )
                , ( "includedCount", Encode.int (List.length emitted.included) )
                , ( "indexCount", Encode.int (Dict.size projectContext.types) )
                ]