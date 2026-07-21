module ProtocolIR exposing
    ( Emitted
    , TypeDef
    , TypeKey
    , closeFromRoots
    , emitElm
    , findRoots
    , fromDocsAlias
    , fromDocsUnion
    , fromSyntaxAlias
    , fromSyntaxCustom
    )

{-| Intermediate representation and pure logic for wire protocol extraction.
-}

import Dict exposing (Dict)
import Elm.Docs as Docs
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type as SyntaxType
import Elm.Syntax.TypeAlias as SyntaxTypeAlias
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Type as ElmType
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Set exposing (Set)



-- KEYS


{-| Fully-qualified type key: "Auth.Common.ToBackend" or "Types.ToBackend".
-}
type alias TypeKey =
    String


makeKey : List String -> String -> TypeKey
makeKey moduleName typeName =
    case moduleName of
        [] ->
            typeName

        _ ->
            String.join "." moduleName ++ "." ++ typeName


parseKey : TypeKey -> ( List String, String )
parseKey key =
    case List.reverse (String.split "." key) of
        [] ->
            ( [], key )

        typeName :: revMod ->
            ( List.reverse revMod, typeName )



-- IR


type TypeAnn
    = Generic String
    | Unit
    | Typed TypeKey (List TypeAnn)
    | Tupled (List TypeAnn)
    | Record (List ( String, TypeAnn ))
    | ExtensibleRecord String (List ( String, TypeAnn ))
    | Function TypeAnn TypeAnn


type alias Constructor =
    { name : String
    , args : List TypeAnn
    }


type TypeDef
    = Custom { constructors : List Constructor, generics : List String }
    | Alias { body : TypeAnn, generics : List String }



-- KERNEL TYPES


{-| Modules whose types should be imported, not inlined.
-}
kernelModules : Set String
kernelModules =
    Set.fromList
        [ "Basics"
        , "Bitwise"
        , "Array"
        , "List"
        , "Dict"
        , "Set"
        , "Maybe"
        , "Result"
        , "String"
        , "Char"
        , "Tuple"
        , "Platform"
        , "Platform.Cmd"
        , "Platform.Sub"
        , "Process"
        , "Task"
        , "Json.Encode"
        , "Json.Decode"
        , "Bytes"
        , "Bytes.Encode"
        , "Bytes.Decode"
        , "Time"
        , "Url"
        , "Url.Builder"
        , "Http"
        , "File"
        , "File.Download"
        , "File.Select"
        , "Random"
        , "Regex"
        , "Parser"
        , "Debug"
        , "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        , "Browser"
        , "Browser.Dom"
        , "Browser.Events"
        , "Browser.Navigation"
        , "VirtualDom"
        ]


{-| Unqualified prelude types (from Basics / default imports).
-}
preludeTypes : Set String
preludeTypes =
    Set.fromList
        [ "Int"
        , "Float"
        , "Bool"
        , "Char"
        , "String"
        , "Never"
        , "Order"
        , "List"
        , "Maybe"
        , "Result"
        , "Dict"
        , "Set"
        , "Array"
        , "Cmd"
        , "Sub"
        ]


{-| Map Lamdera aliases and similar to wire-level kernels.
-}
kernelAliases : Dict TypeKey TypeKey
kernelAliases =
    Dict.fromList
        [ ( "Lamdera.SessionId", "String" )
        , ( "Lamdera.ClientId", "String" )
        , ( "Lamdera.WsError", "String" )
        ]


isKernelKey : TypeKey -> Bool
isKernelKey key =
    if Dict.member key kernelAliases then
        True

    else if Set.member key preludeTypes then
        True

    else
        let
            ( mod, name ) =
                parseKey key

            modStr =
                String.join "." mod
        in
        if List.isEmpty mod then
            Set.member name preludeTypes

        else if Set.member modStr kernelModules then
            True

        else if modStr == "Lamdera" then
            -- Treat remaining Lamdera types as opaque imports if any
            True

        else
            False


normalizeKey : TypeKey -> TypeKey
normalizeKey key =
    Dict.get key kernelAliases
        |> Maybe.withDefault key



-- SYNTAX → IR


fromSyntaxCustom : ModuleNameLookupTable -> List String -> SyntaxType.Type -> ( TypeKey, TypeDef )
fromSyntaxCustom lookup currentModule type_ =
    let
        name =
            Node.value type_.name

        key =
            makeKey currentModule name

        generics =
            List.map Node.value type_.generics

        constructors =
            List.map (syntaxConstructor lookup currentModule) type_.constructors
    in
    ( key, Custom { constructors = constructors, generics = generics } )


syntaxConstructor : ModuleNameLookupTable -> List String -> Node SyntaxType.ValueConstructor -> Constructor
syntaxConstructor lookup currentModule (Node _ ctor) =
    { name = Node.value ctor.name
    , args = List.map (\argNode -> fromSyntaxAnn lookup currentModule (Node.value argNode)) ctor.arguments
    }


fromSyntaxAlias : ModuleNameLookupTable -> List String -> SyntaxTypeAlias.TypeAlias -> ( TypeKey, TypeDef )
fromSyntaxAlias lookup currentModule alias_ =
    let
        name =
            Node.value alias_.name

        key =
            makeKey currentModule name

        generics =
            List.map Node.value alias_.generics

        body =
            fromSyntaxAnn lookup currentModule (Node.value alias_.typeAnnotation)
    in
    ( key, Alias { body = body, generics = generics } )


fromSyntaxAnn : ModuleNameLookupTable -> List String -> TypeAnnotation -> TypeAnn
fromSyntaxAnn lookup currentModule ann =
    case ann of
        TypeAnnotation.GenericType name ->
            Generic name

        TypeAnnotation.Unit ->
            Unit

        TypeAnnotation.Typed node args ->
            let
                ( writtenMod, typeName ) =
                    Node.value node

                resolvedMod =
                    case ModuleNameLookupTable.moduleNameFor lookup node of
                        Just [] ->
                            currentModule

                        Just mod ->
                            mod

                        Nothing ->
                            if List.isEmpty writtenMod then
                                currentModule

                            else
                                writtenMod

                argAnns =
                    List.map (\a -> fromSyntaxAnn lookup currentModule (Node.value a)) args

                key =
                    normalizeKey (makeKey resolvedMod typeName)
            in
            Typed key argAnns

        TypeAnnotation.Tupled nodes ->
            Tupled (List.map (\n -> fromSyntaxAnn lookup currentModule (Node.value n)) nodes)

        TypeAnnotation.Record fields ->
            Record (List.map (syntaxRecordField lookup currentModule) fields)

        TypeAnnotation.GenericRecord (Node _ ext) (Node _ fields) ->
            ExtensibleRecord ext (List.map (syntaxRecordField lookup currentModule) fields)

        TypeAnnotation.FunctionTypeAnnotation left right ->
            Function
                (fromSyntaxAnn lookup currentModule (Node.value left))
                (fromSyntaxAnn lookup currentModule (Node.value right))


syntaxRecordField : ModuleNameLookupTable -> List String -> Node TypeAnnotation.RecordField -> ( String, TypeAnn )
syntaxRecordField lookup currentModule (Node _ ( Node _ fieldName, Node _ fieldType )) =
    ( fieldName, fromSyntaxAnn lookup currentModule fieldType )



-- DOCS → IR


fromDocsUnion : String -> Docs.Union -> ( TypeKey, TypeDef )
fromDocsUnion moduleName union =
    let
        key =
            makeKey (String.split "." moduleName) union.name

        constructors =
            List.map
                (\( ctorName, args ) ->
                    { name = ctorName
                    , args = List.map fromDocsType args
                    }
                )
                union.tags
    in
    ( key, Custom { constructors = constructors, generics = union.args } )


fromDocsAlias : String -> Docs.Alias -> ( TypeKey, TypeDef )
fromDocsAlias moduleName alias_ =
    let
        key =
            makeKey (String.split "." moduleName) alias_.name
    in
    ( key, Alias { body = fromDocsType alias_.tipe, generics = alias_.args } )


fromDocsType : ElmType.Type -> TypeAnn
fromDocsType t =
    case t of
        ElmType.Var name ->
            Generic name

        ElmType.Lambda a b ->
            Function (fromDocsType a) (fromDocsType b)

        ElmType.Tuple [] ->
            Unit

        ElmType.Tuple items ->
            Tupled (List.map fromDocsType items)

        ElmType.Type name args ->
            Typed (normalizeKey (docsTypeNameToKey name)) (List.map fromDocsType args)

        ElmType.Record fields ext ->
            case ext of
                Just extName ->
                    ExtensibleRecord extName (List.map (\( n, ft ) -> ( n, fromDocsType ft )) fields)

                Nothing ->
                    Record (List.map (\( n, ft ) -> ( n, fromDocsType ft )) fields)


{-| Docs type names may be "Int", "Maybe.Maybe", "OAuth.AuthorizationError".
-}
docsTypeNameToKey : String -> TypeKey
docsTypeNameToKey name =
    case String.split "." name of
        [ single ] ->
            -- Unqualified: treat as prelude or bare name
            if Set.member single preludeTypes then
                single

            else
                single

        parts ->
            case List.reverse parts of
                typeName :: revMod ->
                    let
                        mod =
                            List.reverse revMod

                        modStr =
                            String.join "." mod
                    in
                    if Set.member modStr kernelModules || Set.member typeName preludeTypes then
                        if Set.member typeName preludeTypes && List.length mod <= 1 then
                            -- Maybe.Maybe, String.String, Result.Result → short kernel form
                            typeName

                        else if modStr == "Json.Encode" || modStr == "Json.Decode" then
                            makeKey mod typeName

                        else if Set.member modStr kernelModules then
                            -- Time.Posix, Url.Url → keep module-qualified for import
                            makeKey mod typeName

                        else
                            makeKey mod typeName

                    else
                        makeKey mod typeName

                [] ->
                    name



-- REFS + CLOSURE


typeAnnRefs : TypeAnn -> List TypeKey
typeAnnRefs ann =
    case ann of
        Generic _ ->
            []

        Unit ->
            []

        Typed key args ->
            key :: List.concatMap typeAnnRefs args

        Tupled items ->
            List.concatMap typeAnnRefs items

        Record fields ->
            List.concatMap (\( _, t ) -> typeAnnRefs t) fields

        ExtensibleRecord _ fields ->
            List.concatMap (\( _, t ) -> typeAnnRefs t) fields

        Function a b ->
            typeAnnRefs a ++ typeAnnRefs b


typeDefRefs : TypeDef -> List TypeKey
typeDefRefs def =
    case def of
        Custom { constructors } ->
            List.concatMap (\c -> List.concatMap typeAnnRefs c.args) constructors

        Alias { body } ->
            typeAnnRefs body


findRoots : Dict TypeKey TypeDef -> Result String ( TypeKey, TypeKey )
findRoots index =
    let
        toBackendCandidates =
            Dict.keys index
                |> List.filter (\k -> Tuple.second (parseKey k) == "ToBackend")
                |> List.filter (\k -> not (String.contains "Evergreen" k))

        toFrontendCandidates =
            Dict.keys index
                |> List.filter (\k -> Tuple.second (parseKey k) == "ToFrontend")
                |> List.filter (\k -> not (String.contains "Evergreen" k))
    in
    case ( toBackendCandidates, toFrontendCandidates ) of
        ( [ be ], [ fe ] ) ->
            Ok ( be, fe )

        ( [], _ ) ->
            Err "No ToBackend type found in project (looked for type named ToBackend, excluding Evergreen)."

        ( _, [] ) ->
            Err "No ToFrontend type found in project (looked for type named ToFrontend, excluding Evergreen)."

        ( bes, fes ) ->
            -- Prefer keys ending with exactly Types.ToBackend / Types.ToFrontend
            let
                preferTypes list =
                    case List.filter (\k -> String.endsWith ".Types.ToBackend" k || String.endsWith "Types.ToBackend" k || k == "Types.ToBackend" || String.endsWith ".Types.ToFrontend" k || k == "Types.ToFrontend") list of
                        [ one ] ->
                            Just one

                        _ ->
                            case List.filter (\k -> String.startsWith "Types." k) list of
                                [ one ] ->
                                    Just one

                                _ ->
                                    Nothing
            in
            case ( preferTypes bes, preferTypes fes ) of
                ( Just be, Just fe ) ->
                    Ok ( be, fe )

                _ ->
                    Err
                        ("Ambiguous roots. ToBackend candidates: "
                            ++ String.join ", " bes
                            ++ "; ToFrontend candidates: "
                            ++ String.join ", " fes
                        )


closeFromRoots : Dict TypeKey TypeDef -> TypeKey -> TypeKey -> ( Dict TypeKey TypeDef, List TypeKey )
closeFromRoots index rootBe rootFe =
    let
        step : List TypeKey -> Set TypeKey -> List TypeKey -> ( Set TypeKey, List TypeKey )
        step worklist included unresolved =
            case worklist of
                [] ->
                    ( included, unresolved )

                key0 :: rest ->
                    let
                        key =
                            normalizeKey key0
                    in
                    if Set.member key included then
                        step rest included unresolved

                    else if isKernelKey key then
                        step rest included unresolved

                    else
                        case Dict.get key index of
                            Nothing ->
                                step rest included (key :: unresolved)

                            Just def ->
                                let
                                    refs =
                                        typeDefRefs def
                                            |> List.map normalizeKey
                                            |> List.filter (\r -> not (isKernelKey r))
                                            |> List.filter (\r -> not (Set.member r included))
                                in
                                step (rest ++ refs) (Set.insert key included) unresolved

        ( includedSet, unresolvedList ) =
            step [ rootBe, rootFe ] Set.empty []

        includedDict =
            includedSet
                |> Set.toList
                |> List.filterMap
                    (\k ->
                        Dict.get k index
                            |> Maybe.map (\def -> ( k, def ))
                    )
                |> Dict.fromList
    in
    ( includedDict, List.sort (unique unresolvedList) )


unique : List comparable -> List comparable
unique list =
    Set.toList (Set.fromList list)



-- EMIT


type alias Emitted =
    { elmSource : String
    , proofElmSource : String
    , included : List TypeKey
    , unresolved : List TypeKey
    , rootToBackend : TypeKey
    , rootToFrontend : TypeKey
    , errors : List String
    }


{-| Opaque package types (docs with empty constructor lists) cannot be inlined
with wire-identical codecs. Treat as hard extract failures — no funky deps.
-}
isOpaqueCustom : TypeDef -> Bool
isOpaqueCustom def =
    case def of
        Custom { constructors } ->
            List.isEmpty constructors

        Alias _ ->
            False


emitNameFor : TypeKey -> TypeKey -> List String -> TypeKey -> String
emitNameFor rootBe rootFe rootModule key =
    if key == rootBe then
        "ToBackend"

    else if key == rootFe then
        "ToFrontend"

    else
        let
            ( mod, name ) =
                parseKey key
        in
        if mod == rootModule then
            name

        else if List.isEmpty mod then
            name

        else
            String.join "_" mod ++ "_" ++ name


{-| Project modules keep inlined ctors. Package modules (OAuth, Http, …) are
externalized when constructor names would collide in a single Elm module.
Wire3 tags depend on constructor names, so we never rename them.
-}
isLikelyPackageKey : List String -> TypeKey -> Bool
isLikelyPackageKey rootModule key =
    let
        ( mod, _ ) =
            parseKey key

        modStr =
            String.join "." mod

        packageRoots =
            Set.fromList
                [ "OAuth"
                , "Http"
                , "Json"
                , "Time"
                , "Url"
                , "Bytes"
                , "Parser"
                , "Regex"
                , "File"
                , "Browser"
                , "Html"
                , "Svg"
                , "VirtualDom"
                , "Process"
                , "Task"
                , "Platform"
                , "Array"
                , "Dict"
                , "Set"
                , "SeqDict"
                , "List"
                , "Maybe"
                , "Result"
                , "String"
                , "Char"
                , "Basics"
                , "Lamdera"
                ]
    in
    if List.isEmpty mod then
        False

    else if mod == rootModule then
        False

    else
        case List.head mod of
            Just head ->
                Set.member head packageRoots

            Nothing ->
                False


ctorOwners : Dict TypeKey TypeDef -> Dict String (List TypeKey)
ctorOwners included =
    included
        |> Dict.toList
        |> List.concatMap
            (\( key, def ) ->
                case def of
                    Custom { constructors } ->
                        List.map (\c -> ( c.name, key )) constructors

                    Alias _ ->
                        []
            )
        |> List.foldl
            (\( ctorName, key ) acc ->
                Dict.update ctorName
                    (\m ->
                        Just (key :: Maybe.withDefault [] m)
                    )
                    acc
            )
            Dict.empty


resolveConstructorCollisions :
    List String
    -> Dict TypeKey TypeDef
    -> ( Dict TypeKey TypeDef, Set TypeKey, List String )
resolveConstructorCollisions rootModule included =
    let
        owners =
            ctorOwners included

        collidingKeys : Set TypeKey
        collidingKeys =
            owners
                |> Dict.values
                |> List.filter (\ks -> List.length (unique ks) > 1)
                |> List.concat
                |> Set.fromList

        packageColliders =
            collidingKeys
                |> Set.filter (isLikelyPackageKey rootModule)

        emittable2 =
            Set.foldl Dict.remove included packageColliders

        owners2 =
            ctorOwners emittable2

        remainingErrors =
            owners2
                |> Dict.toList
                |> List.filter (\( _, ks ) -> List.length (unique ks) > 1)
                |> List.map
                    (\( ctorName, ks ) ->
                        "Constructor name collision in single module: "
                            ++ ctorName
                            ++ " used by "
                            ++ String.join ", " (unique ks)
                            ++ ". Cannot flatten without breaking Wire3 tags (all owners are project types)."
                    )
    in
    ( emittable2, packageColliders, remainingErrors )


externalImportLines : List TypeKey -> List String
externalImportLines keys =
    keys
        |> List.sort
        |> List.filterMap
            (\key ->
                let
                    ( mod, name ) =
                        parseKey key

                    modStr =
                        String.join "." mod
                in
                if List.isEmpty mod then
                    Nothing

                else
                    Just ("import " ++ modStr ++ " exposing (" ++ name ++ "(..))")
            )


emitElm : TypeKey -> TypeKey -> Dict TypeKey TypeDef -> List TypeKey -> Emitted
emitElm rootBe rootFe included unresolved =
    let
        rootModule =
            Tuple.first (parseKey rootBe)

        -- Opaque package types (empty docs constructors): import, do not redefine.
        opaqueKeys : Set TypeKey
        opaqueKeys =
            included
                |> Dict.toList
                |> List.filter (\( _, def ) -> isOpaqueCustom def)
                |> List.map Tuple.first
                |> Set.fromList

        -- Drop opaques from emit set; they will be imported as externalized.
        emittable : Dict TypeKey TypeDef
        emittable =
            Set.foldl Dict.remove included opaqueKeys

        -- If constructor names collide, externalize package-origin types so project
        -- types keep wire-correct constructor names in this module.
        ( emittable2, packageExternalized, remainingCollisionErrors ) =
            resolveConstructorCollisions rootModule emittable

        externalized : Set TypeKey
        externalized =
            Set.union opaqueKeys packageExternalized

        emitName : TypeKey -> String
        emitName key =
            if Set.member key externalized then
                Tuple.second (parseKey key)

            else
                emitNameFor rootBe rootFe rootModule key

        allErrors =
            remainingCollisionErrors

        finalUnresolved =
            unresolved

        kernelRefs : Set TypeKey
        kernelRefs =
            emittable2
                |> Dict.values
                |> List.concatMap typeDefRefs
                |> List.map normalizeKey
                |> List.filter (\k -> isKernelKey k || Set.member k externalized)
                |> Set.fromList
                |> (\s -> List.foldl Set.insert s (Set.toList externalized))

        imports =
            kernelImportLines (Set.toList kernelRefs)
                ++ externalImportLines (Set.toList externalized)

        sortedKeys =
            topologicalKeys rootBe rootFe emittable2

        typeBlocks =
            List.filterMap
                (\key ->
                    Dict.get key emittable2
                        |> Maybe.map (\def -> emitTypeDef emitName key def)
                )
                sortedKeys

        exposingList =
            sortedKeys
                |> List.map
                    (\key ->
                        case Dict.get key emittable2 of
                            Just (Custom _) ->
                                emitName key ++ "(..)"

                            _ ->
                                emitName key
                    )
                |> String.join ", "

        header =
            "module Protocol exposing\n    ( "
                ++ formatExposing exposingList
                ++ "\n    )\n\n{-| Auto-generated wire protocol types for ToBackend / ToFrontend.\n\nSingle-module freeze of the app wire surface for Wire3-compatible clients.\nGenerated by wire-extractor. Do not edit by hand.\n\nRoots:\n  - ToBackend  ← "
                ++ rootBe
                ++ "\n  - ToFrontend ← "
                ++ rootFe
                ++ "\n-}\n\n"

        importBlock =
            if List.isEmpty imports then
                ""

            else
                String.join "\n" imports ++ "\n\n"

        body =
            String.join "\n\n" typeBlocks ++ "\n"

        source =
            if List.isEmpty allErrors && List.isEmpty finalUnresolved then
                header ++ importBlock ++ body

            else
                -- Still emit best-effort source for debugging, but mark incomplete
                header
                    ++ "-- EXTRACT INCOMPLETE\n"
                    ++ String.concat (List.map (\e -> "-- ERROR: " ++ e ++ "\n") allErrors)
                    ++ String.concat (List.map (\u -> "-- UNRESOLVED: " ++ u ++ "\n") finalUnresolved)
                    ++ "\n"
                    ++ importBlock
                    ++ body

        proofSource =
            if List.isEmpty allErrors && List.isEmpty finalUnresolved then
                -- Use full `emittable` (incl. externalized package types) so samples can
                -- construct OAuth.* / package values; externalized controls qualifiers.
                emitProofElm rootBe rootFe emitName emittable externalized

            else
                "module ProtocolWireProof exposing (suite)\n\nimport Test exposing (Test, test, describe)\nimport Expect\n\nsuite : Test\nsuite =\n    test \"extract incomplete\" <| \\_ -> Expect.fail \"Protocol extract had errors\"\n"
    in
    { elmSource = source
    , proofElmSource = proofSource
    , included = sortedKeys
    , unresolved = finalUnresolved
    , rootToBackend = rootBe
    , rootToFrontend = rootFe
    , errors = allErrors
    }


{-| Generate ProtocolWireProof.elm: for every ToBackend/ToFrontend constructor,
build a minimal Protocol value and prove:

  protocol-encode → app-decode → app-encode → protocol-decode

with identical Wire3 byte lists (real Lamdera w3_* codecs).
-}
emitProofElm : TypeKey -> TypeKey -> (TypeKey -> String) -> Dict TypeKey TypeDef -> Set TypeKey -> String
emitProofElm rootBe rootFe emitName included externalized =
    let
        allRefs =
            included
                |> Dict.values
                |> List.concatMap typeDefRefs
                |> List.map normalizeKey
                |> (\refs -> refs ++ Set.toList externalized)

        needsUrl =
            List.any (\k -> k == "Url" || k == "Url.Url" || String.endsWith ".Url" k) allRefs

        needsDict =
            List.any (\k -> k == "Dict" || k == "Dict.Dict" || String.startsWith "Dict." k) allRefs

        needsSet =
            List.any (\k -> k == "Set" || k == "Set.Set") allRefs

        needsTime =
            List.any
                (\k ->
                    String.startsWith "Time." k
                        || k == "Posix"
                        || k == "Month"
                        || k == "Weekday"
                )
                allRefs

        needsArray =
            List.any (\k -> k == "Array" || k == "Array.Array") allRefs

        needsHttp =
            List.any (\k -> String.startsWith "Http." k || k == "Http.Error") allRefs

        needsBytes =
            List.any (\k -> String.startsWith "Bytes." k || k == "Bytes") allRefs

        needsEncode =
            List.any
                (\k ->
                    String.contains "Json.Encode" k
                        || String.contains "Json.Decode" k
                        || String.endsWith ".Value" k
                )
                allRefs

        needsSeqDict =
            List.any (\k -> String.contains "SeqDict" k) allRefs

        imports =
            String.join "\n" <|
                List.filter (not << String.isEmpty)
                    [ "import Expect exposing (Expectation)"
                    , "import Lamdera.Wire3 as Wire3"
                    , "import Protocol"
                    , "import Fuzz exposing (Fuzzer)"
                    , "import Test exposing (Test, describe, fuzz, test)"
                    , "import Types"
                    , if needsUrl then
                        "import Url exposing (Url)"

                      else
                        ""
                    , if needsDict then
                        "import Dict"

                      else
                        ""
                    , if needsSet then
                        "import Set"

                      else
                        ""
                    , if needsTime then
                        "import Time"

                      else
                        ""
                    , if needsArray then
                        "import Array"

                      else
                        ""
                    , if needsHttp then
                        "import Http"

                      else
                        ""
                    , if needsBytes then
                        "import Bytes"

                      else
                        ""
                    , if needsEncode then
                        "import Json.Encode as Encode"

                      else
                        ""
                    , if needsSeqDict then
                        "import SeqDict exposing (SeqDict)"

                      else
                        ""
                    ]
                ++ externalImportLines (Set.toList externalized)

        urlHelper =
            if needsUrl then
                """
exampleUrl : Url
exampleUrl =
    case Url.fromString "https://example.com/callback" of
        Just u ->
            u

        Nothing ->
            { protocol = Url.Https
            , host = "example.com"
            , port_ = Nothing
            , path = "/callback"
            , query = Nothing
            , fragment = Nothing
            }

"""

            else
                ""

        beSamples =
            sampleConstructors rootBe emitName included externalized "ToBackend"

        feSamples =
            sampleConstructors rootFe emitName included externalized "ToFrontend"

        beList =
            "allToBackend : List ( String, Protocol.ToBackend )\nallToBackend =\n    [ "
                ++ String.join "\n    , " beSamples
                ++ "\n    ]\n"

        feList =
            "allToFrontend : List ( String, Protocol.ToFrontend )\nallToFrontend =\n    [ "
                ++ String.join "\n    , " feSamples
                ++ "\n    ]\n"

        beFuzz =
            propertyFuzzers rootBe included externalized "ToBackend" "roundTripToBackend"

        feFuzz =
            propertyFuzzers rootFe included externalized "ToFrontend" "roundTripToFrontend"

        body =
            """
roundTripToBackend : Protocol.ToBackend -> Expectation
roundTripToBackend protocolVal =
    let
        protocolBytes =
            Wire3.bytesEncode (Protocol.w3_encode_ToBackend protocolVal)

        protocolList =
            Wire3.intListFromBytes protocolBytes
    in
    case Wire3.bytesDecode Types.w3_decode_ToBackend protocolBytes of
        Nothing ->
            Expect.fail
                ("App failed to decode protocol ToBackend bytes: "
                    ++ Debug.toString protocolList
                )

        Just appVal ->
            let
                appBytes =
                    Wire3.bytesEncode (Types.w3_encode_ToBackend appVal)

                appList =
                    Wire3.intListFromBytes appBytes
            in
            if protocolList /= appList then
                Expect.fail
                    ("ToBackend Wire3 byte mismatch\\n  protocol: "
                        ++ Debug.toString protocolList
                        ++ "\\n  app:      "
                        ++ Debug.toString appList
                    )

            else
                case Wire3.bytesDecode Protocol.w3_decode_ToBackend appBytes of
                    Nothing ->
                        Expect.fail "Protocol failed to decode app ToBackend bytes"

                    Just protocolVal2 ->
                        let
                            again =
                                Wire3.intListFromBytes
                                    (Wire3.bytesEncode (Protocol.w3_encode_ToBackend protocolVal2))
                        in
                        Expect.equal protocolList again


roundTripToFrontend : Protocol.ToFrontend -> Expectation
roundTripToFrontend protocolVal =
    let
        protocolBytes =
            Wire3.bytesEncode (Protocol.w3_encode_ToFrontend protocolVal)

        protocolList =
            Wire3.intListFromBytes protocolBytes
    in
    case Wire3.bytesDecode Types.w3_decode_ToFrontend protocolBytes of
        Nothing ->
            Expect.fail
                ("App failed to decode protocol ToFrontend bytes: "
                    ++ Debug.toString protocolList
                )

        Just appVal ->
            let
                appBytes =
                    Wire3.bytesEncode (Types.w3_encode_ToFrontend appVal)

                appList =
                    Wire3.intListFromBytes appBytes
            in
            if protocolList /= appList then
                Expect.fail
                    ("ToFrontend Wire3 byte mismatch\\n  protocol: "
                        ++ Debug.toString protocolList
                        ++ "\\n  app:      "
                        ++ Debug.toString appList
                    )

            else
                case Wire3.bytesDecode Protocol.w3_decode_ToFrontend appBytes of
                    Nothing ->
                        Expect.fail "Protocol failed to decode app ToFrontend bytes"

                    Just protocolVal2 ->
                        let
                            again =
                                Wire3.intListFromBytes
                                    (Wire3.bytesEncode (Protocol.w3_encode_ToFrontend protocolVal2))
                        in
                        Expect.equal protocolList again


suite : Test
suite =
    describe "Wire protocol identity (Protocol ↔ Types)"
        [ describe "ToBackend every constructor (minimal samples)"
            (List.map
                (\\( name, value ) ->
                    test ("ToBackend." ++ name) <|
                        \\_ -> roundTripToBackend value
                )
                allToBackend
            )
        , describe "ToFrontend every constructor (minimal samples)"
            (List.map
                (\\( name, value ) ->
                    test ("ToFrontend." ++ name) <|
                        \\_ -> roundTripToFrontend value
                )
                allToFrontend
            )
"""
                ++ beFuzz
                ++ feFuzz
                ++ "\n        ]\n"
    in
    "module ProtocolWireProof exposing (suite)\n\n{-| AUTO-GENERATED by wire-extractor. Do not edit.\n\nProperty + exhaustive Wire3 identity: Protocol ↔ Types for ToBackend/ToFrontend.\n-}\n\n"
        ++ imports
        ++ "\n"
        ++ urlHelper
        ++ "\n"
        ++ beList
        ++ "\n"
        ++ feList
        ++ body


sampleConstructors : TypeKey -> (TypeKey -> String) -> Dict TypeKey TypeDef -> Set TypeKey -> String -> List String
sampleConstructors rootKey emitName included externalized rootLabel =
    case Dict.get rootKey included of
        Just (Custom { constructors }) ->
            List.map
                (\ctor ->
                    let
                        expr =
                            minimalCtorExpr emitName included externalized rootKey ctor Set.empty
                    in
                    "( \"" ++ ctor.name ++ "\", " ++ expr ++ " )"
                )
                constructors

        _ ->
            [ "( \"MISSING\", Debug.todo \"no " ++ rootLabel ++ " constructors\" )" ]


{-| Property-based tests for constructors whose args are fully fuzzable kernels.
-}
propertyFuzzers : TypeKey -> Dict TypeKey TypeDef -> Set TypeKey -> String -> String -> String
propertyFuzzers rootKey included externalized rootLabel roundTripFn =
    case Dict.get rootKey included of
        Just (Custom { constructors }) ->
            let
                tests =
                    List.filterMap
                        (\ctor ->
                            case fuzzCtorExpr included externalized rootKey ctor of
                                Just fuzzerExpr ->
                                    Just
                                        ("fuzz ("
                                            ++ fuzzerExpr
                                            ++ ") \"property "
                                            ++ rootLabel
                                            ++ "."
                                            ++ ctor.name
                                            ++ "\" "
                                            ++ roundTripFn
                                        )

                                Nothing ->
                                    Nothing
                        )
                        constructors
            in
            if List.isEmpty tests then
                ""

            else
                "\n        , describe \""
                    ++ rootLabel
                    ++ " property (fuzzable constructors)\"\n            [ "
                    ++ String.join "\n            , " tests
                    ++ "\n            ]"

        _ ->
            ""


{-| Build `Fuzz.mapN Ctor f1 f2...` or `Fuzz.constant Ctor` when all args fuzzable.
-}
fuzzCtorExpr : Dict TypeKey TypeDef -> Set TypeKey -> TypeKey -> Constructor -> Maybe String
fuzzCtorExpr included externalized typeKey ctor =
    let
        head =
            ctorQualifier typeKey externalized ++ ctor.name
    in
    case ctor.args of
        [] ->
            Just ("Fuzz.constant " ++ head)

        args ->
            case List.map (fuzzAnnExpr included externalized) args of
                fuzzers ->
                    if List.any ((==) Nothing) fuzzers then
                        Nothing

                    else
                        let
                            fs =
                                List.filterMap identity fuzzers
                        in
                        case fs of
                            [ f1 ] ->
                                Just ("Fuzz.map " ++ head ++ " (" ++ f1 ++ ")")

                            [ f1, f2 ] ->
                                Just ("Fuzz.map2 " ++ head ++ " (" ++ f1 ++ ") (" ++ f2 ++ ")")

                            [ f1, f2, f3 ] ->
                                Just ("Fuzz.map3 " ++ head ++ " (" ++ f1 ++ ") (" ++ f2 ++ ") (" ++ f3 ++ ")")

                            [ f1, f2, f3, f4 ] ->
                                Just ("Fuzz.map4 " ++ head ++ " (" ++ f1 ++ ") (" ++ f2 ++ ") (" ++ f3 ++ ") (" ++ f4 ++ ")")

                            [ f1, f2, f3, f4, f5 ] ->
                                Just ("Fuzz.map5 " ++ head ++ " (" ++ f1 ++ ") (" ++ f2 ++ ") (" ++ f3 ++ ") (" ++ f4 ++ ") (" ++ f5 ++ ")")

                            _ ->
                                -- too many args for mapN — skip property for this ctor
                                Nothing


fuzzAnnExpr : Dict TypeKey TypeDef -> Set TypeKey -> TypeAnn -> Maybe String
fuzzAnnExpr included externalized ann =
    case ann of
        Generic _ ->
            Nothing

        Unit ->
            Just "Fuzz.constant ()"

        Typed key args ->
            fuzzTypedExpr included externalized (normalizeKey key) args

        Tupled items ->
            case List.map (fuzzAnnExpr included externalized) items of
                fuzzers ->
                    if List.any ((==) Nothing) fuzzers then
                        Nothing

                    else
                        let
                            fs =
                                List.filterMap identity fuzzers
                        in
                        case fs of
                            [ a, b ] ->
                                Just ("Fuzz.map2 (\\x y -> ( x, y )) (" ++ a ++ ") (" ++ b ++ ")")

                            [ a, b, c ] ->
                                Just ("Fuzz.map3 (\\x y z -> ( x, y, z )) (" ++ a ++ ") (" ++ b ++ ") (" ++ c ++ ")")

                            _ ->
                                Nothing

        Record fields ->
            -- only records of fuzzable kernels with 1-4 fields
            let
                fieldFuzz =
                    List.map (\( n, t ) -> ( n, fuzzAnnExpr included externalized t )) fields
            in
            if List.any (\( _, m ) -> m == Nothing) fieldFuzz then
                Nothing

            else
                case fieldFuzz of
                    [ ( n1, Just f1 ) ] ->
                        Just ("Fuzz.map (\\v1 -> { " ++ n1 ++ " = v1 }) (" ++ f1 ++ ")")

                    [ ( n1, Just f1 ), ( n2, Just f2 ) ] ->
                        Just
                            ("Fuzz.map2 (\\v1 v2 -> { "
                                ++ n1
                                ++ " = v1, "
                                ++ n2
                                ++ " = v2 }) ("
                                ++ f1
                                ++ ") ("
                                ++ f2
                                ++ ")"
                            )

                    [ ( n1, Just f1 ), ( n2, Just f2 ), ( n3, Just f3 ) ] ->
                        Just
                            ("Fuzz.map3 (\\v1 v2 v3 -> { "
                                ++ n1
                                ++ " = v1, "
                                ++ n2
                                ++ " = v2, "
                                ++ n3
                                ++ " = v3 }) ("
                                ++ f1
                                ++ ") ("
                                ++ f2
                                ++ ") ("
                                ++ f3
                                ++ ")"
                            )

                    _ ->
                        Nothing

        ExtensibleRecord _ _ ->
            Nothing

        Function _ _ ->
            Nothing


fuzzTypedExpr : Dict TypeKey TypeDef -> Set TypeKey -> TypeKey -> List TypeAnn -> Maybe String
fuzzTypedExpr included externalized key args =
    let
        ( mod, name ) =
            parseKey key

        modStr =
            String.join "." mod
    in
    if key == "String" || name == "String" then
        Just "Fuzz.string"

    else if key == "Int" || name == "Int" then
        Just "Fuzz.int"

    else if key == "Float" || name == "Float" then
        Just "Fuzz.niceFloat"

    else if key == "Bool" || name == "Bool" then
        Just "Fuzz.bool"

    else if key == "Char" || name == "Char" then
        Just "Fuzz.char"

    else if name == "List" || key == "List" || modStr == "List" then
        case args of
            [ inner ] ->
                fuzzAnnExpr included externalized inner
                    |> Maybe.map (\f -> "Fuzz.list (" ++ f ++ ")")

            _ ->
                Just "Fuzz.constant []"

    else if name == "Maybe" || key == "Maybe" || modStr == "Maybe" then
        case args of
            [ inner ] ->
                fuzzAnnExpr included externalized inner
                    |> Maybe.map (\f -> "Fuzz.maybe (" ++ f ++ ")")

            _ ->
                Just "Fuzz.constant Nothing"

    else if name == "Result" || key == "Result" || modStr == "Result" then
        case args of
            [ err, ok ] ->
                case ( fuzzAnnExpr included externalized err, fuzzAnnExpr included externalized ok ) of
                    ( Just fe, Just fo ) ->
                        Just
                            ("Fuzz.oneOf [ Fuzz.map Err ("
                                ++ fe
                                ++ "), Fuzz.map Ok ("
                                ++ fo
                                ++ ") ]"
                            )

                    _ ->
                        Nothing

            _ ->
                Nothing

    else if key == "Url" || key == "Url.Url" || (modStr == "Url" && name == "Url") then
        Just "Fuzz.constant exampleUrl"

    else if Set.member key externalized then
        Nothing

    else
        case Dict.get key included of
            Just (Alias { body }) ->
                fuzzAnnExpr included externalized body

            Just (Custom { constructors }) ->
                -- Only nullary-only enums are property-fuzzed as oneOf constants
                if List.all (\c -> List.isEmpty c.args) constructors then
                    let
                        constants =
                            List.map
                                (\c -> "Fuzz.constant (" ++ ctorQualifier key externalized ++ c.name ++ ")")
                                constructors
                    in
                    Just ("Fuzz.oneOf [ " ++ String.join ", " constants ++ " ]")

                else
                    Nothing

            Nothing ->
                Nothing


ctorQualifier : TypeKey -> Set TypeKey -> String
ctorQualifier typeKey externalized =
    if Set.member typeKey externalized then
        let
            ( mod, _ ) =
                parseKey typeKey
        in
        String.join "." mod ++ "."

    else
        "Protocol."


minimalCtorExpr : (TypeKey -> String) -> Dict TypeKey TypeDef -> Set TypeKey -> TypeKey -> Constructor -> Set TypeKey -> String
minimalCtorExpr emitName included externalized typeKey ctor visiting =
    let
        head =
            ctorQualifier typeKey externalized ++ ctor.name
    in
    case ctor.args of
        [] ->
            head

        args ->
            head
                ++ " "
                ++ String.join " "
                    (List.map
                        (\ann -> "(" ++ minimalAnnExpr emitName included externalized ann (Set.insert typeKey visiting) ++ ")")
                        args
                    )


minimalAnnExpr : (TypeKey -> String) -> Dict TypeKey TypeDef -> Set TypeKey -> TypeAnn -> Set TypeKey -> String
minimalAnnExpr emitName included externalized ann visiting =
    case ann of
        Generic _ ->
            -- should not appear on wire roots without instantiation
            "()"

        Unit ->
            "()"

        Typed key args ->
            minimalTypedExpr emitName included externalized (normalizeKey key) args visiting

        Tupled items ->
            "("
                ++ String.join ", "
                    (List.map (\a -> minimalAnnExpr emitName included externalized a visiting) items)
                ++ ")"

        Record fields ->
            if List.isEmpty fields then
                "{}"

            else
                "{ "
                    ++ String.join ", "
                        (List.map
                            (\( n, t ) ->
                                n ++ " = " ++ minimalAnnExpr emitName included externalized t visiting
                            )
                            fields
                        )
                    ++ " }"

        ExtensibleRecord _ fields ->
            -- treat as closed record for samples
            minimalAnnExpr emitName included externalized (Record fields) visiting

        Function _ _ ->
            "(\\_ -> ())"


minimalTypedExpr : (TypeKey -> String) -> Dict TypeKey TypeDef -> Set TypeKey -> TypeKey -> List TypeAnn -> Set TypeKey -> String
minimalTypedExpr emitName included externalized key args visiting =
    let
        ( mod, name ) =
            parseKey key

        modStr =
            String.join "." mod
    in
    -- Kernels / specials
    if key == "String" || name == "String" then
        "\"\""

    else if key == "Int" || name == "Int" then
        "0"

    else if key == "Float" || name == "Float" then
        "0"

    else if key == "Bool" || name == "Bool" then
        "False"

    else if key == "Char" || name == "Char" then
        "'x'"

    else if name == "List" || key == "List" || modStr == "List" then
        "[]"

    else if name == "Maybe" || key == "Maybe" || modStr == "Maybe" then
        "Nothing"

    else if name == "Result" || key == "Result" || modStr == "Result" then
        case args of
            [ errAnn, _ ] ->
                "Err (" ++ minimalAnnExpr emitName included externalized errAnn visiting ++ ")"

            _ ->
                "Err \"\""

    else if name == "Dict" || modStr == "Dict" then
        "Dict.empty"

    else if name == "Set" || modStr == "Set" then
        "Set.empty"

    else if name == "Array" || modStr == "Array" then
        "Array.empty"

    else if name == "SeqDict" || modStr == "SeqDict" then
        "SeqDict.empty"

    else if key == "Url" || key == "Url.Url" || (modStr == "Url" && name == "Url") then
        "exampleUrl"

    else if key == "Time.Posix" || (modStr == "Time" && name == "Posix") then
        "Time.millisToPosix 0"

    else if key == "Time.Month" || (modStr == "Time" && name == "Month") then
        "Time.Jan"

    else if key == "Time.Weekday" || (modStr == "Time" && name == "Weekday") then
        "Time.Mon"

    else if key == "Time.Zone" || (modStr == "Time" && name == "Zone") then
        "Time.utc"

    else if key == "Time.Era" || (modStr == "Time" && name == "Era") then
        "Time.CE"

    else if key == "Http.Error" || (modStr == "Http" && name == "Error") then
        "Http.BadUrl \"\""

    else if key == "Bytes.Bytes" || (modStr == "Bytes" && name == "Bytes") then
        "Bytes.fromList []"

    else if (modStr == "Json.Encode" || modStr == "Json.Decode") && name == "Value" then
        "Encode.null"

    else if key == "Order" || name == "Order" then
        "EQ"

    else if isKernelKey key then
        -- Fallback for other kernels
        case name of
            "Value" ->
                "Encode.null"

            _ ->
                "Debug.todo \"minimal kernel " ++ key ++ "\""

    else if Set.member key visiting then
        -- Break recursion: only nullary constructors are safe
        case Dict.get key included of
            Just (Custom { constructors }) ->
                case List.filter (\c -> List.isEmpty c.args) constructors of
                    c :: _ ->
                        ctorQualifier key externalized ++ c.name

                    [] ->
                        "Debug.todo \"recursive type without nullary ctor: "
                            ++ key
                            ++ "\""

            _ ->
                "Debug.todo \"recursive " ++ key ++ "\""

    else
        case Dict.get key included of
            Just (Alias { body }) ->
                minimalAnnExpr emitName included externalized body visiting

            Just (Custom { constructors }) ->
                -- Prefer nullary ctor, else first ctor
                let
                    chosen =
                        case List.filter (\c -> List.isEmpty c.args) constructors of
                            c :: _ ->
                                c

                            [] ->
                                case constructors of
                                    c :: _ ->
                                        c

                                    [] ->
                                        { name = "EMPTY", args = [] }
                in
                if chosen.name == "EMPTY" then
                    "Debug.todo \"no constructors for " ++ key ++ "\""

                else
                    minimalCtorExpr emitName included externalized key chosen visiting

            Nothing ->
                if Set.member key externalized then
                    -- Type not in included dict but externalized — try docs already merged into included
                    "Debug.todo \"externalized missing def " ++ key ++ "\""

                else
                    "Debug.todo \"unknown type " ++ key ++ "\""


formatExposing : String -> String
formatExposing list =
    -- Keep one long exposing for simplicity; pretty enough
    String.join "\n    , " (String.split ", " list)


kernelImportLines : List TypeKey -> List String
kernelImportLines keys =
    let
        neededModules : Set String
        neededModules =
            keys
                |> List.filterMap
                    (\key ->
                        let
                            ( mod, name ) =
                                parseKey key
                        in
                        case mod of
                            [] ->
                                -- prelude – no import needed for Int/String/etc. except Dict/Set/Array
                                if Set.member name (Set.fromList [ "Dict", "Set", "Array" ]) then
                                    Just name

                                else
                                    Nothing

                            _ ->
                                let
                                    modStr =
                                        String.join "." mod
                                in
                                if Set.member modStr (Set.fromList [ "Basics", "List", "Maybe", "Result", "String", "Char", "Tuple", "Bitwise", "Debug", "Platform" ]) then
                                    Nothing

                                else if modStr == "Platform.Cmd" || modStr == "Platform.Sub" then
                                    Nothing

                                else
                                    Just modStr
                    )
                |> Set.fromList

        importFor mod =
            case mod of
                "Dict" ->
                    "import Dict exposing (Dict)"

                "Set" ->
                    "import Set exposing (Set)"

                "Array" ->
                    "import Array exposing (Array)"

                "Time" ->
                    "import Time exposing (Posix)"

                "Url" ->
                    "import Url exposing (Url)"

                "Http" ->
                    "import Http"

                "Json.Encode" ->
                    "import Json.Encode as Encode"

                "Json.Decode" ->
                    "import Json.Decode as Decode"

                "Bytes" ->
                    "import Bytes exposing (Bytes)"

                "Lamdera" ->
                    "import Lamdera"

                other ->
                    "import " ++ other
    in
    neededModules
        |> Set.toList
        |> List.sort
        |> List.map importFor


emitTypeDef : (TypeKey -> String) -> TypeKey -> TypeDef -> String
emitTypeDef emitName key def =
    let
        name =
            emitName key

        genericsStr generics =
            case generics of
                [] ->
                    ""

                gs ->
                    " " ++ String.join " " gs
    in
    case def of
        Custom { constructors, generics } ->
            case constructors of
                [] ->
                    "type " ++ name ++ genericsStr generics ++ "\n    = -- no constructors"

                first :: rest ->
                    "type "
                        ++ name
                        ++ genericsStr generics
                        ++ "\n    = "
                        ++ emitConstructor emitName first
                        ++ String.concat
                            (List.map
                                (\c -> "\n    | " ++ emitConstructor emitName c)
                                rest
                            )

        Alias { body, generics } ->
            "type alias "
                ++ name
                ++ genericsStr generics
                ++ " =\n    "
                ++ emitAnn emitName body


emitConstructor : (TypeKey -> String) -> Constructor -> String
emitConstructor emitName ctor =
    case ctor.args of
        [] ->
            ctor.name

        args ->
            ctor.name ++ " " ++ String.join " " (List.map (emitAnnParens emitName) args)


emitAnnParens : (TypeKey -> String) -> TypeAnn -> String
emitAnnParens emitName ann =
    case ann of
        Typed _ args ->
            if List.isEmpty args then
                emitAnn emitName ann

            else
                "(" ++ emitAnn emitName ann ++ ")"

        Tupled _ ->
            emitAnn emitName ann

        Record _ ->
            emitAnn emitName ann

        ExtensibleRecord _ _ ->
            "(" ++ emitAnn emitName ann ++ ")"

        Function _ _ ->
            "(" ++ emitAnn emitName ann ++ ")"

        _ ->
            emitAnn emitName ann


emitAnn : (TypeKey -> String) -> TypeAnn -> String
emitAnn emitName ann =
    case ann of
        Generic name ->
            name

        Unit ->
            "()"

        Typed key args ->
            let
                head =
                    emitTypedHead emitName key

                argStrs =
                    List.map (emitAnnParens emitName) args
            in
            String.join " " (head :: argStrs)

        Tupled items ->
            "(" ++ String.join ", " (List.map (emitAnn emitName) items) ++ ")"

        Record fields ->
            if List.isEmpty fields then
                "{}"

            else
                "{ "
                    ++ String.join ", " (List.map (\( n, t ) -> n ++ " : " ++ emitAnn emitName t) fields)
                    ++ " }"

        ExtensibleRecord ext fields ->
            "{ "
                ++ ext
                ++ " | "
                ++ String.join ", " (List.map (\( n, t ) -> n ++ " : " ++ emitAnn emitName t) fields)
                ++ " }"

        Function a b ->
            emitAnnParens emitName a ++ " -> " ++ emitAnn emitName b


emitTypedHead : (TypeKey -> String) -> TypeKey -> String
emitTypedHead emitName key =
    if isKernelKey key then
        kernelEmitName key

    else
        emitName key


kernelEmitName : TypeKey -> String
kernelEmitName key =
    let
        key2 =
            normalizeKey key

        ( mod, name ) =
            parseKey key2

        modStr =
            String.join "." mod
    in
    case ( modStr, name ) of
        ( "", n ) ->
            n

        -- Default-imported / bare prelude modules: always short name
        ( "Basics", n ) ->
            n

        ( "List", _ ) ->
            "List"

        ( "Maybe", _ ) ->
            "Maybe"

        ( "Result", _ ) ->
            "Result"

        ( "String", _ ) ->
            "String"

        ( "Char", _ ) ->
            "Char"

        ( "Tuple", n ) ->
            n

        ( "Platform.Cmd", _ ) ->
            "Cmd"

        ( "Platform.Sub", _ ) ->
            "Sub"

        ( "Time", "Posix" ) ->
            "Posix"

        ( "Url", "Url" ) ->
            "Url"

        ( "Json.Encode", "Value" ) ->
            "Encode.Value"

        ( "Json.Decode", "Value" ) ->
            "Decode.Value"

        ( "Bytes", "Bytes" ) ->
            "Bytes"

        ( "Dict", _ ) ->
            "Dict"

        ( "Set", _ ) ->
            "Set"

        ( "Array", _ ) ->
            "Array"

        ( "Lamdera", n ) ->
            case Dict.get key2 kernelAliases of
                Just "String" ->
                    "String"

                _ ->
                    "Lamdera." ++ n

        ( m, n ) ->
            if Set.member m kernelModules then
                -- Prefer Module.Name for non-prelude kernels (Http.Error, etc.)
                if n == lastSegment m then
                    n

                else
                    m ++ "." ++ n

            else
                n


lastSegment : String -> String
lastSegment s =
    String.split "." s
        |> List.reverse
        |> List.head
        |> Maybe.withDefault s

{-| Supporting types first (alpha), then ToBackend, then ToFrontend.
Only the actual root keys are treated as roots (not Auth.Common.ToBackend).
-}
topologicalKeys : TypeKey -> TypeKey -> Dict TypeKey TypeDef -> List TypeKey
topologicalKeys rootBe rootFe included =
    let
        keys =
            Dict.keys included |> List.sort

        rest =
            List.filter (\k -> k /= rootBe && k /= rootFe) keys
    in
    rest ++ [ rootBe, rootFe ]
