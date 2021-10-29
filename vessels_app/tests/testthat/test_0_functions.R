library(testthat)

test_that('Testing functions', {
      # Degrees to Radians
      expect_equal(to_radians(45), 45 * pi / 180)
      expect_equal(to_radians(10), 10 * pi / 180)
      expect_error(to_radians("str"))
      expect_error(to_radians("100.50"))
      
      # Calculate Distance
      expect_equal(round(haversine(51, 10, 50, 18),0), 576345)
      expect_error(to_radians("51", "10", "50", "18"))
      
      expect_equal(round(to_km(75)), 139)
      expect_error(to_km("51"))
      
      expect_equal(calculate_distance(60, 1), 111120)
      expect_error(calculate_distance(60))
})
