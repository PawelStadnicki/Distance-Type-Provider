module DistanceTypeProviderImplementation

open System
open FSharp.Core.CompilerServices
open FSharp.Data.Runtime
open ProviderImplementation.ProvidedTypes
open MyRuntime

[<TypeProvider>]
type DistanceTypeProvider (config : TypeProviderConfig) as this =

    inherit TypeProviderForNamespaces (config, assemblyReplacementMap=[("DistanceTypeProvider.DesignTime", "DistanceTypeProvider.Runtime")])
    
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "DistanceProvider"

    let fromFile = ProvidedTypeDefinition(asm, ns, "GeojsonFile", Some(typeof<obj>))
    do fromFile.AddXmlDoc("Creates types from a static parameter pointing to a geojson file location.")

    let fromFile2 = ProvidedTypeDefinition(asm, ns, "GeojsonFiles", Some(typeof<obj>))
    do fromFile2.AddXmlDoc("Creates types from a static parameter pointing to a geojson file content.")

    let fromContent = ProvidedTypeDefinition(asm, ns, "GeojsonContent", Some(typeof<obj>))
    do fromContent.AddXmlDoc("Creates types from a static parameter pointing to 2 geojson files.")

    let fromContent2 = ProvidedTypeDefinition(asm, ns, "GeojsonContents", Some(typeof<obj>))
    do fromContent2.AddXmlDoc("Creates types from a static parameter pointing to 2 geojson contents.")

    let upperFirst( w:string) =
        w.Substring(0,1).ToUpper() + w.Substring(1).ToLower()

    let exists (value:string) = String.IsNullOrWhiteSpace value |> not

    let fromTo (entry:ProviderEntry) = 
        let source = entry.Source |> Parser.parseGeojson entry.SourceAttrName
        let target =
            if entry.Target.IsSome then entry.Target.Value |> Parser.parseGeojson  entry.TargetAttrName
            else source

        let from = ProvidedTypeDefinition(asm, ns, "From", Some(typeof<obj>), hideObjectMethods = true)
        from.AddXmlDoc("Provides names of all source features, from whom you can get spatial or exact distance")
        from.AddMembersDelayed(fun () -> 
            
            source
            |> List.ofArray 
            |> List.map (fun (place:BaseTypes.Data)  -> 

                let {
                        BaseTypes.Data.Location = (lon,lat); 
                        BaseTypes.Data.Name = name
                    } = place

                let tp = ProvidedTypeDefinition(asm, ns, name, Some(typeof<obj>), hideObjectMethods = true)

                tp.AddMemberDelayed ( fun () -> 
                    let details = 
                        ProvidedProperty("Details",typeof<BaseTypes.Data>, isStatic = true, getterCode = fun _ ->        
                            <@@ BaseTypes.Data.New((lon,lat),name) @@>
                        )
                    details.AddXmlDoc("Get name and position of the place")
                    details
                )
            
                tp.AddMemberDelayed ( fun () -> 
                    let dto = 
                        ProvidedMethod(
                            "DistanceTo", 
                            [
                                ProvidedParameter("lon", typeof<float>)
                                ProvidedParameter("lat", typeof<float>)
      
                            ], 
                            typeof<float>, 
                            isStatic=true, 
                            invokeCode = 
                                function
                                | [arg1;arg2] ->
                                    let (c,d) = place.Location
                                    <@@
                                        let  a = %%(arg1) : float
                                        let  b = %%(arg2) : float
                                        BaseTypes.Data.ToDistance((a,b),(c,d)) 
                                    @@> 
                                | other -> failwith (sprintf "DistanceTo expects 2 args but found %d" other.Length)
                         )
                    dto.AddXmlDoc("Calculate spatial distance to exact place")
                    dto
                )

                let facilities = ProvidedTypeDefinition(asm, ns, "To", Some(typeof<obj>),hideObjectMethods = true)
                facilities.AddXmlDocDelayed(fun () -> sprintf "All  names from the target file")

                let fs = target

                facilities.AddMembersDelayed ( fun () ->        
                    fs  
                    |> List.ofArray
                    |> List.map ( fun f -> 
               
                        ProvidedProperty(f.Name,typeof<float>, isStatic = true, getterCode = fun _ -> 
                            let (x,y) = f.Location
                    
                            let d = BaseTypes.Data.ToDistance(place.Location, (x, y))
                            <@@ d @@>
                        )
                    )
                )
                tp.AddMember facilities

                if(entry.MapBoxKey.IsSome) then
                    tp.AddMembersDelayed(
                        fun () -> 
                        ["driving";"walking";"cycling"]
                        |> List.map (fun profile ->
                            let directions = ProvidedTypeDefinition(asm, ns, $"{upperFirst profile}To", Some(typeof<obj>),hideObjectMethods = true)
                            directions.AddXmlDocDelayed(fun () -> sprintf "Obtains Mapbox direction-matrix with %s profile" profile)
                            directions.AddMembersDelayed ( fun () ->        
                                    fs  
                                    |> List.ofArray
                                    |> List.map ( fun f -> 
                                        let cords = sprintf "%s;%s" f.Position place.Position
                                        let key = entry.MapBoxKey.Value
                                        let p = (upperFirst(profile))
                                        ProvidedProperty(f.Name,typeof<Mapbox.NiceDirectionResponse>, isStatic = true, getterCode  = fun ph ->
       
                                            <@@ Mapbox.getDirections key p cords @@>
                                        )
                                    )
                            )
                            directions
                        )  
                    )
                tp
            ) 
        )

        match entry.MapBoxKey with
        | None -> [from]
        | Some key -> 
            let directions = ProvidedTypeDefinition(asm, ns, "Directions", Some(typeof<obj>), hideObjectMethods = true)

            directions.AddMembersDelayed( fun() ->
            
                ["driving";"walking";"cycling"] 
                |> List.map ( fun profile ->
                    ProvidedMethod(
                        $"{upperFirst profile}To", 
                        [
                            ProvidedParameter("data", typeof<BaseTypes.Data list>)
                        ], 
                        typeof<Mapbox.NiceDirectionResponse>, 
                        isStatic=true, 
                        invokeCode = 
                            function
                            | [arg1] ->

    
                                let p = (upperFirst(profile))
                                <@@
                                    let data = %%(arg1) : BaseTypes.Data list
                                    let cords = data |> List.map (fun x -> x.Position) |> List.toArray 
                                    let raw = System.String.Join(";", cords)
                                    Mapbox.getDirections key p raw 
                                @@> 
                            | other -> failwith (sprintf "DistanceTo expects 2 args but found %d" other.Length)
                        )
                )
            
            )
            [from;directions]

    let createType (tyName : string) (entry: ProviderEntry) = 

        let ptd = ProvidedTypeDefinition(asm, ns, tyName, Some(typeof<BaseTypes.PositionHolder>),hideObjectMethods = true) 

        ptd.AddMembersDelayed ( fun () -> fromTo entry)

        let ctor1 = 
            ProvidedConstructor(
                [ProvidedParameter("pos", typeof<float*float>)],
                invokeCode =
                    function
                    | [p] -> <@@ BaseTypes.PositionHolder.New(%%p) @@>
                    | other -> failwith "never"
            )         

        do ptd.AddMember ctor1

        ptd.AddMembersDelayed ( fun () ->     
            entry.Source 
            |> Parser.parseGeojson entry.SourceAttrName
            |> Array.toList 
            |> List.map ( fun d ->
                ProvidedProperty(d.Name,typeof<float>, isStatic = false, getterCode  = fun ph ->
                    let lon,lat = d.Location
                    <@@ BaseTypes.Data.ToDistance(((%%ph.[0]): BaseTypes.PositionHolder).Pos, (lon, lat)) @@>
                )
            )
        )
        ptd

    do fromFile.DefineStaticParameters(
        [
            ProvidedStaticParameter("source", typeof<string>)
            ProvidedStaticParameter("mapboxKey", typeof<string>, "")
            ProvidedStaticParameter("sourceAttrName", typeof<string>, "name")
        ], 
        fun tyName args ->
            match args with
            | [| :? string as file |] -> 
                {
                    Source = File file
                    Target = None
                    MapBoxKey = None
                    SourceAttrName = None
                    TargetAttrName = None
                }
                |> createType tyName 

            | [| :? string as file ;:? string as key; :? string as sourceAttrName |] -> 

                {
                    Source = File file
                    Target = None
                    MapBoxKey = if exists key then Some key else None
                    SourceAttrName = if exists sourceAttrName then Some sourceAttrName else  None
                    TargetAttrName = None
                }
                |> createType tyName 

            | other -> failwith "To many parameters for this type provider"
    )
    do fromFile2.DefineStaticParameters(
        [
            ProvidedStaticParameter("source", typeof<string>)
            ProvidedStaticParameter("target", typeof<string>)
            ProvidedStaticParameter("mapboxKey", typeof<string>, "")
            ProvidedStaticParameter("sourceAttrName", typeof<string>, "name")
            ProvidedStaticParameter("targetAttrName", typeof<string>, "name")
        ], 
        fun tyName args ->
            match args with
            | [| :? string as file1; :? string as file2;:? string as key; :? string as sourceAttrName; :? string as targetAttrName |] -> 
                {
                        Source = File file1
                        Target = Some (File file2)
                        MapBoxKey = if exists key then Some key else None
                        SourceAttrName = if exists sourceAttrName then Some sourceAttrName else  None
                        TargetAttrName = if exists targetAttrName then Some targetAttrName else  None
                }
                |> createType tyName 
      
            | other -> failwith "To many parameters for this type provider"
    )
    do fromContent.DefineStaticParameters(
        [
            ProvidedStaticParameter("source", typeof<string>)
            ProvidedStaticParameter("mapboxKey", typeof<string>, "")
            ProvidedStaticParameter("sourceAttrName", typeof<string>, "name")
        ], 
        fun tyName args ->
            match args with
            | [| :? string as file |] -> 
                {
                    Source = Raw file
                    Target = None
                    MapBoxKey = None
                    SourceAttrName = None
                    TargetAttrName = None
                }
                |> createType tyName 

            | [| :? string as file ;:? string as key; :? string as sourceAttrName |] -> 

                {
                    Source = Raw file
                    Target = None
                    MapBoxKey = if exists key then Some key else None
                    SourceAttrName = if exists sourceAttrName then Some sourceAttrName else  None
                    TargetAttrName = None
                }
                |> createType tyName 

            | other -> failwith "To many parameters for this type provider"
    )
    do fromContent2.DefineStaticParameters(
        [
            ProvidedStaticParameter("source", typeof<string>)
            ProvidedStaticParameter("target", typeof<string>)
            ProvidedStaticParameter("mapboxKey", typeof<string>, "")
            ProvidedStaticParameter("sourceAttrName", typeof<string>, "name")
            ProvidedStaticParameter("targetAttrName", typeof<string>, "name")
        ], 
        fun tyName args ->
            match args with
            | [| :? string as file1; :? string as file2;:? string as key; :? string as sourceAttrName; :? string as targetAttrName |] -> 
                {
                        Source = Raw file1
                        Target = Some (Raw file2)
                        MapBoxKey = if exists key then Some key else None
                        SourceAttrName = if exists sourceAttrName then Some sourceAttrName else  None
                        TargetAttrName = if exists targetAttrName then Some targetAttrName else  None
                }
                |> createType tyName 
      
            | other -> failwith "To many parameters for this type provider"
    )

    do this.AddNamespace(ns, [fromFile;fromFile2;fromContent; fromContent2])