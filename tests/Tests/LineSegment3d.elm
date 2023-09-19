module Tests.LineSegment3d exposing (nearestPoints)

import Expect
import LineSegment3d
import LineSegment3d.Extra
import Point3d
import Test exposing (Test)


nearestPoints : Test
nearestPoints =
    Test.describe "nearestPoints"
        [ Test.test "Parallel lines but it's okay because they aren't adjacent" <|
            \_ ->
                LineSegment3d.Extra.nearestPoints
                    (LineSegment3d.from (Point3d.meters 0 0 0) (Point3d.meters 10 0 0))
                    (LineSegment3d.from (Point3d.meters 11 0 0) (Point3d.meters 20 0 0))
                    |> Expect.equal
                        (Just
                            { nearestOnFirstLine = Point3d.meters 10 0 0
                            , nearestOnSecondLine = Point3d.meters 11 0 0
                            }
                        )
        , Test.test "Example case" <|
            \_ ->
                LineSegment3d.Extra.nearestPoints
                    (LineSegment3d.from (Point3d.meters 10 0 5) (Point3d.meters -2 3 0))
                    (LineSegment3d.from (Point3d.meters 11 -2 1) (Point3d.meters 20 5 1))
                    |> Expect.equal
                        (Just
                            { nearestOnFirstLine = Point3d.meters 10 0 5
                            , nearestOnSecondLine = Point3d.meters 11.346153846153847 -1.7307692307692308 1
                            }
                        )
        ]
