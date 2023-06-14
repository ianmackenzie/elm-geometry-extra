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
    Test.check3 "Nearest point is found correctly"
        Random.quadraticSpline2d
        Random.point2d
        Random.parameterValue
        (\spline point randomParameterValue ->
            let
                computedParameterValue =
                    QuadraticSpline2d.nearestPoint spline point

                computedPoint =
                    QuadraticSpline2d.pointOn spline computedParameterValue

                randomPoint =
                    QuadraticSpline2d.pointOn spline randomParameterValue

                distanceToComputedPoint =
                    Point2d.distanceFrom point computedPoint

                distanceToRandomPoint =
                    Point2d.distanceFrom point randomPoint
            in
            -- The distance to our computed 'nearest point' on the spline should definitely not be
            -- greater than the distance to some other random point on the spline!
            distanceToComputedPoint |> Expect.quantityAtMost distanceToRandomPoint
        )
