module Tests exposing (..)

import Test exposing (..)
import Expect
import Geocoding as G


apiKey : String
apiKey =
    "ABCD"


all : Test
all =
    describe "A Test Suite"
        [ testInitializeWithAddressOnly
        , testInitializeWithComponentsOnly
        , testWithAddress
        , testWithComponents
        , testWithRegion
        , testWithLanguage
        , testWithBounds
        , testReverseForLatLng
        , testReverseForPlaceId
        , testReverseWithLanguage
        , testReverseWithResultTypes
        , testReverseWithLocationTypes
        ]


testInitializeWithAddressOnly : Test
testInitializeWithAddressOnly =
    let
        request =
            G.requestForAddress apiKey "77 Battery St."

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=77+Battery+St."
    in
        test "initializeWithAddressOnly" <|
            \() -> Expect.equal (G.requestUrl request) expected


testInitializeWithComponentsOnly : Test
testInitializeWithComponentsOnly =
    let
        request =
            G.requestForComponents apiKey [ ( "Spain", G.CountryComponent ) ]

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&components=country%3ASpain"
    in
        test "initializeWithComponentsOnly" <|
            \() -> Expect.equal (G.requestUrl request) expected


testWithAddress : Test
testWithAddress =
    let
        request =
            G.requestForComponents apiKey [ ( "Spain", G.CountryComponent ) ]
                |> G.withAddress ("Toledo")

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Toledo&components=country%3ASpain"
    in
        test "withAddress" <|
            \() -> Expect.equal (G.requestUrl request) expected


testWithComponents : Test
testWithComponents =
    let
        request =
            G.requestForAddress apiKey "Toledo"
                |> G.withComponent ( "Spain", G.CountryComponent )
                |> G.withComponent ( "Toledo", G.AdministrativeAreaComponent )

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Toledo&components=administrative_area%3AToledo%7Ccountry%3ASpain"
    in
        test "withComponents" <|
            \() -> Expect.equal (G.requestUrl request) expected


testWithRegion : Test
testWithRegion =
    let
        request =
            G.requestForAddress apiKey "Toledo"
                |> G.withRegion "ES"

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Toledo&region=ES"
    in
        test "withRegion" <|
            \() -> Expect.equal (G.requestUrl request) expected


testWithLanguage : Test
testWithLanguage =
    let
        request =
            G.requestForAddress apiKey "Toledo"
                |> G.withLanguage "ES"

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Toledo&language=ES"
    in
        test "withLanguage" <|
            \() -> Expect.equal (G.requestUrl request) expected


testWithBounds : Test
testWithBounds =
    let
        request =
            G.requestForAddress apiKey "Belmont"
                |> G.withBounds ( 41, -74 ) ( 42, -70 )

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Belmont&bounds=41%2C-74%7C42%2C-70"
    in
        test "withBounds" <|
            \() -> Expect.equal (G.requestUrl request) expected


testReverseForLatLng : Test
testReverseForLatLng =
    let
        request =
            G.reverseRequestForLatLng apiKey ( 37.8489277, -122.4031502 )

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&latlng=37.8489277%2C-122.4031502"
    in
        test "reverseForLatLng" <|
            \() -> Expect.equal (G.reverseRequestUrl request) expected


testReverseForPlaceId : Test
testReverseForPlaceId =
    let
        request =
            G.reverseRequestForPlaceId apiKey "ChIJIQBpAG2ahYAR_6128GcTUEo"

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&place_id=ChIJIQBpAG2ahYAR_6128GcTUEo"
    in
        test "reverseForPlaceId" <|
            \() -> Expect.equal (G.reverseRequestUrl request) expected


testReverseWithLanguage : Test
testReverseWithLanguage =
    let
        request =
            G.reverseRequestForLatLng apiKey ( 37.8489277, -122.4031502 )
                |> G.reverseWithLanguage "FR"

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&latlng=37.8489277%2C-122.4031502&language=FR"
    in
        test "reverseWithLanguage" <|
            \() -> Expect.equal (G.reverseRequestUrl request) expected


testReverseWithResultTypes : Test
testReverseWithResultTypes =
    let
        request =
            G.reverseRequestForLatLng apiKey ( 37.8489277, -122.4031502 )
                |> G.withResultTypes [ G.StreetAddress, G.Country ]

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&latlng=37.8489277%2C-122.4031502&result_type=street_address%7Ccountry"
    in
        test "reverseWithResultTypes" <|
            \() -> Expect.equal (G.reverseRequestUrl request) expected


testReverseWithLocationTypes : Test
testReverseWithLocationTypes =
    let
        request =
            G.reverseRequestForLatLng apiKey ( 37.8489277, -122.4031502 )
                |> G.withLocationTypes [ G.Rooftop, G.Approximate ]

        expected =
            "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&latlng=37.8489277%2C-122.4031502&location_type=ROOFTOP%7CAPPROXIMATE"
    in
        test "reverseWithLocationTypes" <|
            \() -> Expect.equal (G.reverseRequestUrl request) expected
