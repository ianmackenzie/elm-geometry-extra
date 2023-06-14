module Tests.QuadraticSpline2d exposing (nearestPoint)

import Expect
import Geometry.Expect as Expect
import Geometry.Random as Random
import Point2d
import QuadraticSpline2d
import QuadraticSpline2d.Extra as QuadraticSpline2d
import Test exposing (Test)
import Test.Random as Test
import Vector2d


nearestPoint : Test
nearestPoint =
    Test.check2 "Nearest point is found correctly"
        Random.quadraticSpline2d
        Random.point2d
        (\spline point ->
            let
                t =
                    QuadraticSpline2d.nearestPoint spline point

                pointOnSpline =
                    QuadraticSpline2d.pointOn spline t

                displacement =
                    Vector2d.from pointOnSpline point
            in
            case ( QuadraticSpline2d.nondegenerate spline, Vector2d.direction displacement ) of
                ( Ok nondegenerateSpline, Just direction ) ->
                    let
                        tangentDirection =
                            QuadraticSpline2d.tangentDirection nondegenerateSpline t
                    in
                    direction |> Expect.direction2dPerpendicularTo tangentDirection

                _ ->
                    Expect.pass
        )
