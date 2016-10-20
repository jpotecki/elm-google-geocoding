module Geocoding
    exposing
        ( send
        , requestForAddress
        , requestForComponents
        , withLanguage
        , withBounds
        , withRegion
        , withComponent
        , withAddress
        , reverseRequestForLatLng
        , reverseRequestForPlaceId
        , reverseWithLanguage
        , withResultTypes
        , withLocationTypes
        , sendReverseRequest
        , GeocodingResult
        , Status
        , Viewport
        , ApiKey
        , Component(..)
        , LocationType(..)
        , ComponentType(..)
        , Response
        , requestUrl
        , reverseRequestUrl
        )

{-| This library is an interface to Google's geocoding service

https://developers.google.com/maps/documentation/geocoding/intro

It provides a pipline friendly, builder-like API, and ADTs that map as closely as possible to the Google API

You can start building a request one of two ways:

    Geocoding.requestForAddress apiKey "77 Battery St."
    Geocoding.requestForComponents
      [
        ("Spain", Geocoding.CountryComponent)
      ]

or for reverse geocoding:
    Geocoding.reverseRequestForLatLng apiKey ( 37.8489277, -122.4031502 )
    Geocoding.reverseRequestForPlaceId apiKey "ChIJrTLr-GyuEmsRBfy61i59si0"

Once you've built your request, calling send will return a Task, which you Perform to generate your own msg types

# Types
@docs GeocodingResult, Status, Viewport, ApiKey, Component, LocationType, ComponentType, Response

# Building a request
@docs requestForAddress, requestForComponents, withAddress, withComponent, withLanguage, withRegion, withBounds

# Building a reverse geocoding request
@docs reverseRequestForLatLng

# Sending a request
@docs send, sendReverseRequest

# Inspecting a request
@docs requestUrl, reverseRequestUrl
-}

import Dict exposing (Dict)
import Http
import String
import Task exposing (Task)
import Json.Decode.Pipeline
    exposing
        ( decode
        , required
        , optional
        , nullable
        )
import Json.Decode
    exposing
        ( list
        , int
        , string
        , succeed
        , fail
        , float
        , Decoder
        , (:=)
        )


-- Types


{-| response status and a list of results (list will be empty if status is other than OK)
https://developers.google.com/maps/documentation/geocoding/intro#GeocodingResponses
-}
type alias Response =
    { status : Status
    , results : List GeocodingResult
    }


{-| mapping of Google API response statuses
https://developers.google.com/maps/documentation/geocoding/intro#StatusCodes
-}
type Status
    = GeocodingOk
    | ZeroResults
    | OverQueryLimit
    | RequestDenied
    | InvalidRequest
    | UnknownError


{-| an individual result
https://developers.google.com/maps/documentation/geocoding/intro#Results
-}
type alias GeocodingResult =
    { addressComponents : List AddressComponent
    , formattedAddress : String
    , geometry : Geometry
    , types : List ComponentType
    , placeId : String
    }


{-| a component of an address
   https://developers.google.com/maps/documentation/geocoding/intro#Results
-}
type alias AddressComponent =
    { longName : Maybe String
    , shortName : Maybe String
    , types : List ComponentType
    }


{-| the latitude and longitude of the location, location type and recommended viewport
   https://developers.google.com/maps/documentation/geocoding/intro#Results
-}
type alias Geometry =
    { location : Location
    , locationType : LocationType
    , viewport : Viewport
    }


{-| address component types
   https://developers.google.com/maps/documentation/geocoding/intro#Types
-}
type ComponentType
    = StreetAddress
    | Route
    | Intersection
    | Political
    | Country
    | AdministrativeAreaLevel1
    | AdministrativeAreaLevel2
    | AdministrativeAreaLevel3
    | AdministrativeAreaLevel4
    | AdministrativeAreaLevel5
    | ColloquialArea
    | Locality
    | Sublocality
    | SublocalityLevel1
    | SublocalityLevel2
    | SublocalityLevel3
    | SublocalityLevel4
    | SublocalityLevel5
    | Neighborhood
    | Premise
    | Subpremise
    | PostalCode
    | NaturalFeature
    | Airport
    | Park
    | PostBox
    | StreetNumber
    | Floor
    | Room
    | Establishment
    | PointOfInterest
    | Parking
    | PostalTown
    | BusStation
    | TrainStation
    | TransitStation
    | PostalCodeSuffix
    | OtherComponent


type alias Location =
    { latitude : Float
    , longitude : Float
    }


{-| a bounding box
   https://developers.google.com/maps/documentation/geocoding/intro#Viewports
-}
type alias Viewport =
    { northeast : Location
    , southwest : Location
    }


{-| additional data about a location
   https://developers.google.com/maps/documentation/geocoding/intro#Result
-}
type LocationType
    = Rooftop
    | RangeInterpolated
    | GeometricCenter
    | Approximate
    | OtherLocationType



-- request types


type RequestInfo
    = Address String
    | Components (Dict String Component)
    | AddressAndComponents String (Dict String Component)


type alias GeocodingRequest =
    { requestInfo : RequestInfo
    , bounds : Maybe Viewport
    , language : Maybe String
    , region : Maybe String
    , apiKey : ApiKey
    }


type ReverseRequestInfo
    = LatLng ( Float, Float )
    | PlaceId String


type alias ReverseGeocodingRequest =
    { requestInfo : ReverseRequestInfo
    , language : Maybe String
    , resultType : Maybe (List ComponentType)
    , locationType : Maybe (List LocationType)
    , apiKey : ApiKey
    }


{-| components for request filtering
   https://developers.google.com/maps/documentation/geocoding/intro#ComponentFiltering
-}
type Component
    = RouteComponent
    | LocalityComponent
    | AdministrativeAreaComponent
    | PostalCodeComponent
    | CountryComponent


{-| alias for a Google API key
-}
type alias ApiKey =
    String



-- Decoders


statusDecoder : Decoder Status
statusDecoder =
    string `Json.Decode.andThen` mapStatus


mapStatus : String -> Decoder Status
mapStatus str =
    case str of
        "OK" ->
            succeed GeocodingOk

        "ZERO_RESULTS" ->
            succeed ZeroResults

        "OVER_QUERY_LIMIT" ->
            succeed OverQueryLimit

        "REQUEST_DENIED" ->
            succeed RequestDenied

        "INVALID_REQUEST" ->
            succeed InvalidRequest

        "UNKNOWN_ERROR" ->
            succeed UnknownError

        _ ->
            fail "really, really, unknown error"


responseDecoder : Decoder Response
responseDecoder =
    decode Response
        |> required "status" statusDecoder
        |> required "results" resultListDecoder


resultListDecoder : Decoder (List GeocodingResult)
resultListDecoder =
    list resultDecoder


resultDecoder : Decoder GeocodingResult
resultDecoder =
    decode GeocodingResult
        |> required "address_components" addressComponentListDecoder
        |> required "formatted_address" string
        |> required "geometry" geometryDecoder
        |> required "types" typeListDecoder
        |> required "place_id" string


addressComponentListDecoder : Decoder (List AddressComponent)
addressComponentListDecoder =
    list addressComponentDecoder


addressComponentDecoder : Decoder AddressComponent
addressComponentDecoder =
    decode AddressComponent
        |> optional "long_name" (nullable string) Nothing
        |> optional "short_name" (nullable string) Nothing
        |> required "types" typeListDecoder


locationDecoder : Decoder Location
locationDecoder =
    decode Location
        |> required "lat" float
        |> required "lng" float


viewportDecoder : Decoder Viewport
viewportDecoder =
    decode Viewport
        |> required "northeast" locationDecoder
        |> required "southwest" locationDecoder


locationTypeDecoder : Decoder LocationType
locationTypeDecoder =
    string `Json.Decode.andThen` mapLocationType


geometryDecoder : Decoder Geometry
geometryDecoder =
    decode Geometry
        |> required "location" locationDecoder
        |> required "location_type" locationTypeDecoder
        |> required "viewport" viewportDecoder


typeListDecoder : Decoder (List ComponentType)
typeListDecoder =
    list typeDecoder


typeDecoder : Decoder ComponentType
typeDecoder =
    string `Json.Decode.andThen` mapComponentType


mapLocationType : String -> Decoder LocationType
mapLocationType =
    succeed << stringToLocationType


mapComponentType : String -> Decoder ComponentType
mapComponentType =
    succeed << stringToComponentType



-- URL building


geocodingUrl : String
geocodingUrl =
    "https://maps.googleapis.com/maps/api/geocode/json"


{-| for inspecting the request URL for testing purposes
-}
requestUrl : GeocodingRequest -> String
requestUrl =
    Http.url geocodingUrl << toParameters


{-| for inspecting the request URL for testing purposes
-}
reverseRequestUrl : ReverseGeocodingRequest -> String
reverseRequestUrl =
    Http.url geocodingUrl << toReverseRequestParameters



-- query formatting


componentToString : Component -> String
componentToString c =
    case c of
        RouteComponent ->
            "route"

        LocalityComponent ->
            "locality"

        AdministrativeAreaComponent ->
            "administrative_area"

        PostalCodeComponent ->
            "postal_code"

        CountryComponent ->
            "country"


locationToString : Location -> String
locationToString l =
    String.join "," [ toString l.latitude, toString l.longitude ]


viewportToString : Viewport -> String
viewportToString v =
    String.join "|" [ locationToString v.southwest, locationToString v.northeast ]


componentsToString : Dict String Component -> String
componentsToString components =
    String.join ("|")
        <| Dict.foldr (\k v acc -> acc ++ [ componentToString v ++ ":" ++ k ]) [] components


requestInfoParameter : RequestInfo -> List ( String, String )
requestInfoParameter info =
    case info of
        Address a ->
            [ ( "address", a ) ]

        Components c ->
            [ ( "components", componentsToString c ) ]

        AddressAndComponents a c ->
            [ ( "address", a ), ( "components", componentsToString c ) ]


reverseRequestInfoParameter : ReverseRequestInfo -> ( String, String )
reverseRequestInfoParameter info =
    case info of
        LatLng ( lat, lng ) ->
            ( "latlng", [ lat, lng ] |> List.map toString |> String.join "," )

        PlaceId id ->
            ( "place_id", id )


singleton : a -> List a
singleton x =
    [ x ]


toParameters : GeocodingRequest -> List ( String, String )
toParameters req =
    List.concat
        [ [ ( "key", req.apiKey ) ]
        , requestInfoParameter req.requestInfo
        , Maybe.map (singleton << (,) "bounds" << viewportToString) req.bounds |> Maybe.withDefault []
        , Maybe.map (singleton << (,) "language") req.language |> Maybe.withDefault []
        , Maybe.map (singleton << (,) "region") req.region |> Maybe.withDefault []
        ]


toReverseRequestParameters : ReverseGeocodingRequest -> List ( String, String )
toReverseRequestParameters req =
    List.concat
        [ [ ( "key", req.apiKey ) ]
        , [ reverseRequestInfoParameter req.requestInfo ]
        , Maybe.map (singleton << (,) "language") req.language |> Maybe.withDefault []
        , Maybe.map (singleton << (,) "result_type" << String.join "|" << List.map componentTypeToString) req.resultType |> Maybe.withDefault []
        , Maybe.map (singleton << (,) "location_type" << String.join "|" << List.map locationTypeToString) req.locationType |> Maybe.withDefault []
        ]


{-| transform a request into a Task

    Geocoding.requestForAddress apiKey "77 Battery St"
      |> Geocoding.send
      |> Task.perform MyFailureMsg MySuccessMsg
-}
send : GeocodingRequest -> Task Http.Error Response
send req =
    Http.get responseDecoder <| requestUrl req


{-| Build a request for an address

    Geocoding.requestForAddress apiKey "77 Battery St"
-}
requestForAddress : ApiKey -> String -> GeocodingRequest
requestForAddress key address =
    GeocodingRequest (Address address) Nothing Nothing Nothing key


{-| Build a request for a list of component filters

    Geocoding.requestForComponents apiKey
      [
        ("Spain", Geocoding.CountryComponent)
      , ("Toledo", Geocoding.AdministrativeAreaComponent)
      ]
-}
requestForComponents : ApiKey -> List ( String, Component ) -> GeocodingRequest
requestForComponents key components =
    GeocodingRequest (Components <| Dict.fromList components) Nothing Nothing Nothing key


{-| Specify the language for the request

    Geocoding.requestForAddress apiKey "77 Battery St"
      |> Geocoding.withLanguage("FR")
-}
withLanguage : String -> GeocodingRequest -> GeocodingRequest
withLanguage lang { requestInfo, bounds, language, region, apiKey } =
    GeocodingRequest requestInfo bounds (Just lang) region apiKey


{-| Specify a viewport bias for the request

    Geocoding.requestForAddress apiKey "Belmont"
      |> Geocoding.withBounds (41, -74) (42, -70)
-}
withBounds : ( Float, Float ) -> ( Float, Float ) -> GeocodingRequest -> GeocodingRequest
withBounds ( swLat, swLng ) ( neLat, neLng ) { requestInfo, bounds, language, region, apiKey } =
    let
        viewport =
            Viewport (Location neLat neLng) (Location swLat swLng)
    in
        GeocodingRequest requestInfo (Just viewport) language region apiKey


{-| specify region biasing for request

    Geocoding.requestForAddress apiKey "Toledo"
      |> Geocoding.withRegion "ES"
-}
withRegion : String -> GeocodingRequest -> GeocodingRequest
withRegion reg { requestInfo, bounds, language, region, apiKey } =
    GeocodingRequest requestInfo bounds language (Just reg) apiKey


{-| add a component filter to a request (can be called more than once for a request)

    Geocoding.requestForAddress apiKey "Toledo"
      |> Geocoding.withComponent ("Spain", Geocoding.CountryComponent)
-}
withComponent : ( String, Component ) -> GeocodingRequest -> GeocodingRequest
withComponent comp { requestInfo, bounds, language, region, apiKey } =
    let
        info =
            requestInfo |> addComponent comp
    in
        GeocodingRequest info bounds language region apiKey


{-| set the address to a request. If called more than once, the later call overwrites the earlier

    Geocoding.requestForComponents apiKey
      [
        ("Spain", Geocoding.CountryComponent)
      , ("Toledo", Geocoding.AdministrativeAreaComponent)
      ]
        |> Geocoding.withAddress "Toledo"
-}
withAddress : String -> GeocodingRequest -> GeocodingRequest
withAddress address { requestInfo, bounds, language, region, apiKey } =
    let
        info =
            requestInfo |> addAddress address
    in
        GeocodingRequest info bounds language region apiKey



-- Reverse geocoding request builders


{-|
  transform a reverse geocoding request into a Task

    Geocoding.requestForLatLng apiKey (37.8489277,-122.4031502)
      |> Geocoding.sendReverseRequest
      |> Task.perform MyFailureMsg MySuccessMsg

-}
sendReverseRequest : ReverseGeocodingRequest -> Task Http.Error Response
sendReverseRequest req =
    Http.get responseDecoder <| reverseRequestUrl req


{-| Build a reverse geocoding request for an location

    Geocoding.reverseRequestForLatLng apiKey (37.8489277,-122.4031502)
-}
reverseRequestForLatLng : ApiKey -> ( Float, Float ) -> ReverseGeocodingRequest
reverseRequestForLatLng key latLng =
    ReverseGeocodingRequest (LatLng latLng) Nothing Nothing Nothing key


{-| Build a reverse geocoding request for Google place_id

    Geocoding.reverseRequestForLatLng apiKey "ChIJrTLr-GyuEmsRBfy61i59si0"
-}
reverseRequestForPlaceId : ApiKey -> String -> ReverseGeocodingRequest
reverseRequestForPlaceId key placeId =
    ReverseGeocodingRequest (PlaceId placeId) Nothing Nothing Nothing key


{-| Set the language for the request

  Geocoding.reverseRequestForLatLng apiKey "ChIJrTLr-GyuEmsRBfy61i59si0"
    |> Geocoding.reverseWithLanguage("FR")
-}
reverseWithLanguage : String -> ReverseGeocodingRequest -> ReverseGeocodingRequest
reverseWithLanguage lang { requestInfo, language, resultType, locationType, apiKey } =
    ReverseGeocodingRequest requestInfo (Just lang) resultType locationType apiKey


{-| Set the result type(s) for the request

  Geocoding.reverseRequestForLatLng apiKey (37.8489277,-122.4031502)
    |> Geocoding.withResultTypes [Country, PostalCode]
-}
withResultTypes : List ComponentType -> ReverseGeocodingRequest -> ReverseGeocodingRequest
withResultTypes resultTypes { requestInfo, language, resultType, locationType, apiKey } =
    ReverseGeocodingRequest requestInfo language (Just resultTypes) locationType apiKey


{-| Set the location type filters for the request

  Geocoding.reverseRequestForLatLng apiKey (37.8489277,-122.4031502)
    |> Geocoding.withLocationTypes [Approximate]

-}
withLocationTypes : List LocationType -> ReverseGeocodingRequest -> ReverseGeocodingRequest
withLocationTypes locationTypes { requestInfo, language, resultType, locationType, apiKey } =
    ReverseGeocodingRequest requestInfo language resultType (Just locationTypes) apiKey



-- request building helpers


addAddress : String -> RequestInfo -> RequestInfo
addAddress address req =
    case req of
        Address old ->
            Address address

        Components dict ->
            AddressAndComponents address dict

        AddressAndComponents old dict ->
            AddressAndComponents address dict


addComponent : ( String, Component ) -> RequestInfo -> RequestInfo
addComponent ( val, comp ) req =
    case req of
        Address address ->
            AddressAndComponents address <| Dict.fromList [ ( val, comp ) ]

        Components dict ->
            Components <| Dict.insert val comp dict

        AddressAndComponents address dict ->
            AddressAndComponents address <| Dict.insert val comp dict


componentTypeList : List ( String, ComponentType )
componentTypeList =
    [ ( "street_address", StreetAddress )
    , ( "route", Route )
    , ( "intersection", Intersection )
    , ( "political", Political )
    , ( "country", Country )
    , ( "administrative_area_level_1", AdministrativeAreaLevel1 )
    , ( "administrative_area_level_2", AdministrativeAreaLevel2 )
    , ( "administrative_area_level_3", AdministrativeAreaLevel3 )
    , ( "administrative_area_level_4", AdministrativeAreaLevel4 )
    , ( "administrative_area_level_5", AdministrativeAreaLevel5 )
    , ( "colloquial_area", ColloquialArea )
    , ( "locality", Locality )
    , ( "sublocality", Sublocality )
    , ( "sublocality_level_1", SublocalityLevel1 )
    , ( "sublocality_level_2", SublocalityLevel2 )
    , ( "sublocality_level_3", SublocalityLevel3 )
    , ( "sublocality_level_4", SublocalityLevel4 )
    , ( "sublocality_level_5", SublocalityLevel5 )
    , ( "neighborhood", Neighborhood )
    , ( "premise", Premise )
    , ( "subpremise", Subpremise )
    , ( "postal_code", PostalCode )
    , ( "natural_feature", NaturalFeature )
    , ( "airport", Airport )
    , ( "park", Park )
    , ( "post_box", PostBox )
    , ( "street_number", StreetNumber )
    , ( "floor", Floor )
    , ( "room", Room )
    , ( "establishment", Establishment )
    , ( "point_of_interest", PointOfInterest )
    , ( "parking", Parking )
    , ( "postal_town", PostalTown )
    , ( "bus_station", BusStation )
    , ( "train_station", TrainStation )
    , ( "transit_station", TransitStation )
    , ( "postal_code_suffix", PostalCodeSuffix )
    ]


componentTypeMap : Dict String ComponentType
componentTypeMap =
    Dict.fromList componentTypeList


stringToComponentType : String -> ComponentType
stringToComponentType str =
    Dict.get str componentTypeMap
        |> Maybe.withDefault OtherComponent


componentTypeToString : ComponentType -> String
componentTypeToString component =
    componentTypeList
        |> List.filter (\( s, c ) -> component == c)
        |> List.head
        |> Maybe.map fst
        |> Maybe.withDefault ""


locationTypeList : List ( String, LocationType )
locationTypeList =
    [ ( "ROOFTOP", Rooftop )
    , ( "RANGE_INTERPOLATED", RangeInterpolated )
    , ( "GEOMETRIC_CENTER", GeometricCenter )
    , ( "APPROXIMATE", Approximate )
    ]


locationTypeMap : Dict String LocationType
locationTypeMap =
    Dict.fromList locationTypeList


stringToLocationType : String -> LocationType
stringToLocationType str =
    Dict.get str locationTypeMap
        |> Maybe.withDefault OtherLocationType


locationTypeToString : LocationType -> String
locationTypeToString locationType =
    locationTypeList
        |> List.filter (\( s, loc ) -> locationType == loc)
        |> List.head
        |> Maybe.map fst
        |> Maybe.withDefault ""
