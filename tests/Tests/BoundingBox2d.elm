module Tests.BoundingBox2d exposing (poissonDiskSampling)

import Array
import BoundingBox2d
import BoundingBox2d.Extra as BoundingBox2d
import DelaunayTriangulation2d
import Expect
import Length
import LineSegment2d
import Point2d
import Quantity
import Random
import Test exposing (Test)
import Test.Random as Test
import Triangle2d


poissonDiskSampling : Test
poissonDiskSampling =
    Test.describe "poissonDiskSamples"
        [ Test.check2 "Produces within a reasonable bound of the desired count"
            Random.independentSeed
            (Random.int 20 100)
            (\seed count ->
                let
                    bbox =
                        BoundingBox2d.from (Point2d.meters 5 5) (Point2d.meters 10 10)
                in
                Random.step (BoundingBox2d.poissonDiskSamples count bbox) seed
                    |> Tuple.first
                    |> List.length
                    |> Expect.all
                        [ Expect.atLeast (floor (toFloat count * 0.91))
                        , Expect.atMost (ceiling (toFloat count * 1.5))
                        ]
            )
        , Test.check2 "points are reasonably far apart"
            Random.independentSeed
            (Random.int 20 50)
            (\seed count ->
                let
                    bbox =
                        BoundingBox2d.from (Point2d.meters 0 0) (Point2d.meters 10 10)

                    minDist =
                        Length.meters (sqrt (100 / (toFloat count * 1.5)))
                in
                case
                    Random.step (BoundingBox2d.poissonDiskSamples count bbox) seed
                        |> Tuple.first
                        |> Array.fromList
                        |> DelaunayTriangulation2d.fromPoints
                of
                    Err (DelaunayTriangulation2d.CoincidentVertices v1 v2) ->
                        Expect.fail <| "poissonDiskSamples produces coincident points " ++ Debug.toString v1 ++ " and " ++ Debug.toString v2

                    Ok triangulation ->
                        let
                            delaunayEdges =
                                triangulation
                                    |> DelaunayTriangulation2d.triangles
                                    |> List.concatMap
                                        (\triangle ->
                                            let
                                                ( e1, e2, e3 ) =
                                                    Triangle2d.edges triangle
                                            in
                                            [ e1, e2, e3 ]
                                        )

                            isAtLeastMinDistLong edge =
                                LineSegment2d.length edge |> Quantity.greaterThanOrEqualTo minDist
                        in
                        if List.all isAtLeastMinDistLong delaunayEdges then
                            Expect.pass

                        else
                            Expect.fail "all points are at least minDist distant from each other"
            )
        ]
