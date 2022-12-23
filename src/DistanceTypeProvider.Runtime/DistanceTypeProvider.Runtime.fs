namespace MyRuntime

open Microsoft.FSharp.Core
open type System.Math
open System.Linq
open System.IO
open NetTopologySuite.IO
open NetTopologySuite.Features
open System.Net.Http
open Thoth.Json.Net
open FSharp.Data.UnitSystems.SI.UnitSymbols

type GeojsonSource = 
    | File of path:string
    | Raw of content:string
    member this.content =
        match this with
        | File path -> File.ReadAllText path
        | Raw c -> c

type ProviderEntry = 
    {
        Source : GeojsonSource
        Target : GeojsonSource option
        MapBoxKey : string option
        SourceAttrName : string option
        TargetAttrName : string option
    }

module Spatial =
    let calc  (at: float[]) (bt:float[]) =
        let r = 6371.0; // km
        let dLat = (bt.[1] - at.[1]) * PI / 180.0
        let dLon = (bt.[0] - at.[0]) * PI / 180.0
        let lat1 = at.[1] * PI / 180.0
        let lat2 = bt.[1] * PI / 180.0
        
        let a = Sin(dLat/2.0) * Sin(dLat/2.0) +
                Sin(dLon/2.0) * Sin(dLon/2.0) * Cos(lat1) * Cos(lat2)
        let c = 2.0 * Atan2(Sqrt(a), Sqrt(1.0-a))
        r * c * 1000.

module BaseTypes =
    type PositionHolder=
        {
            Pos: float*float
        }
        static member New(lon, lat) =
            { Pos = lon,lat}

    type Data =
        {
            Name : string
            Location : float*float
        }
        member t.Position =
            let (a,b) = t.Location
            sprintf "%f,%f" a b

        static member New((lon, lat),name) =
            { Location = (lon,lat); Name = name}

        static member ToDistance( (lon1,lat1), (lon2, lat2)) = Spatial.calc [|lon1;lat1|] [|lon2;lat2|]


module Json = 
    let decodeResult x =
        match x with
        | Error er -> 
            failwith er
        | Ok props -> props

    let decode<'t> x = Decode.Auto.fromString<'t>(x) |> decodeResult

module FeatureReader = 

    let readCollection (content:string) =
        let reader = GeoJsonReader()
        reader.Read<FeatureCollection>(content).ToArray()

module Parser = 
    open BaseTypes

    let feature filePath (properties:'p[]) =
     
        filePath 
        |> File.ReadAllText
        |> FeatureReader.readCollection
        |> Array.ofSeq
        |> Array.zip properties 
        |> Array.mapi ( fun i (p,f) -> 
            {|
                Index = i
                Data = p
                Feature = f 
                Position = f.Geometry.Coordinate
            |}
        )

    let removeWhitespacesAndDiacritics (x:string) = 
        x.Replace("'",System.String.Empty)

    let parseGeojson (attrName : string option)(source : GeojsonSource) =

        let name =
            match attrName with
            | None -> "name"
            | Some value -> value

        let content =
            match source with
            | File path -> System.IO.File.ReadAllText(path)
            | Raw content -> content

        content 
        |> FeatureReader.readCollection 
        |> Array.map (fun x -> 
            {
                Name = string (x.Attributes.[name]) |>  removeWhitespacesAndDiacritics
                Location = (x.Geometry.Centroid.Coordinate.X, x.Geometry.Centroid.Coordinate.Y)
            } 
         )

module Mapbox =
    type DirectionResponse =
        {
            code : string
            distances: (float[][]) option

            // if a place is not drivable in a reasonable time, the whole array or part of it will be undefined
            durations: (float option [])[] option
        }
    type NiceDirectionResponse =
        {
            Distances: float[]
            Durations: float[]
        }
        static member From (res:DirectionResponse) =
            match res.code with
            |"Ok" -> 
                {
                    Distances = res.distances.Value.[0]
                    Durations = res.durations.Value.[0] |> Array.map (fun x -> x.Value)
                }
            | status -> failwith (sprintf "Mapbox returned failure code %s" status)
        member t.Distance: float<m> = t.Distances.[0] |> LanguagePrimitives.FloatWithMeasure
        member t.Duration: float<s> = t.Durations.[0] |> LanguagePrimitives.FloatWithMeasure

    let getDirections (key : string) (profile:string) (cords:string) =
        async {
            let count = cords.Split(';').Length  
            let client = new HttpClient()
            let! response = client.GetAsync($"https://api.mapbox.com/directions-matrix/v1/mapbox/{profile.ToLower()}/{cords}?sources=0&destinations=all&annotations=distance,duration&access_token={key}") |> Async.AwaitTask
            response.EnsureSuccessStatusCode() |> ignore
            let! responseBody = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            let res = responseBody |> Json.decode<DirectionResponse>
            let res2 =  NiceDirectionResponse.From res 
            let res3 = 
                if res2.Distances.[0] = 0. && res2.Durations.[0] = 0. then 
                    {res2 with Distances = res2.Distances |> Array.skip 1; Durations = res2.Durations |> Array.skip 1}
                else res2
            return res3
        } |> Async.RunSynchronously
        
[<assembly:CompilerServices.TypeProviderAssembly("DistanceTypeProvider.DesignTime.dll")>]
do ()