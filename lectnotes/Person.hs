module Person where

data Person
  = Student
      { firstName :: String
      , lastName :: String
      , id :: String
      , major :: String
      , year :: Int
      , courses_enrolled :: [(String, (Int, Int))]
      }
  | Teacher
      { firstName :: String
      , lastName :: String
      , dept :: String
      , courses_teaching :: [(Int, Int)]
      }

------------------------------------------------------------------------------

getNames :: [Person] -> String -> (Int, Int) -> [(String, String)]
getNames students dept course =[ (lastName,firstName) |(Student firstName lastName _ _ _ courses_enrolled) <- students, (dept,course) `elem` courses_enrolled ]


studentsOfTeacher_ :: [Person] -> Person -> [((Int, Int), [(String, String)])]
studentsOfTeacher_ students Teacher{dept = d, courses_teaching = ct} = [(course, getNames students d course) | course <- ct]
studentsOfTeacher_ students Student{} = []

studentsOfTeacher :: Person -> [((Int, Int), [(String, String)])]
studentsOfTeacher = studentsOfTeacher_ allStudents

------------------------------------------------------------------------------

professorChugh :: Person
professorChugh =
  Teacher "Ravi" "Chugh" "CMSC" [(16100,1)]

professorKurtz :: Person
professorKurtz =
  Teacher "Stuart" "Kurtz" "CMSC" [(16100,2), (28000,1)]

allStudents :: [Person]
allStudents =
  [ Student "A" "Student" "********" "CMSC" 1 [("CMSC", (15100,1))]
  , Student "B" "Student" "********" "CMSC" 1 [("CMSC", (16100,1))]
  , Student "C" "Student" "********" "CMSC" 2 [("CMSC", (16100,2))]
  , Student "D" "Student" "********" "MATH" 2 [("CMSC", (28000,1))]
  , Student "E" "Student" "********" "MATH" 3 [("CMSC", (28000,1))]
  , Student "F" "Student" "********" "ARTV" 3 [("CMSC", (12100,1))]
  , Student "STEAM" "Student" "********" "ARTV" 4
      [("CMSC", (16100,1)), ("ARTV", (22500,1)), ("ARTV", (22502,1))]
  ]

