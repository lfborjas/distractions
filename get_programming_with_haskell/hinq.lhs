Capstone project for Unit 5: working with type in a context

> import Control.Monad -- necessary for functions like `guard`
> import Control.Applicative -- necessary for Alternative

This unit introduces Functors (source of the `fmap` operator: <$>), which can take a function
that applies to values outside of contexts, a value in a context, and returns another value in a context:

fmap  :: Functor f => (a -> b) -> f a -> f b
(<$>) :: Functor f => (a -> b) -> f a -> f b

Coincidentally, `map` is the `fmap` of the List type, which makes it a functor (i.e. it can take a function
that transforms between individual types and a list in type a and produce a list in type b; which is to say:
it takes a function for single elements and can put them in the context of a list.

Then, it introduces Applicative. In this case, it can take a function in a context and a value in that context and
produce a value in that context still. This is useful for partial application, where you have a function in a context
(vs. a single value), want to apply a value in the context to it, and stay in the context:

(<*>) :: Applicative f :: f (a -> b) -> f a -> f b -- this operator is called `app`
pure  :: Applicative f :: a -> f a

`pure` puts a value in a context

With applicatives, one can chain applications, which is to say, apply functions of more than one argument
and stay in the context (Functors aren't enough, since they operate on a single-argument function)

e.g.:

(*) <$> Just 6 <*> Just 7 -- returns: Just 42

which reads as: apply multiplication in a Maybe context, and produce a function in a Maybe context that can take an
argument in Maybe, and stay in Maybe.

`pure` can remove the need to wrap that `6`in a Maybe, by doing that work for us:

pure (6+) <*> Just 5

which is equivalent to

(6+) <$> Just 5

that is, in both cases we're putting the 6 in a Maybe context, and then staying in it by either applying
`app` to a function that's been put in a Maybe context; or by applying `fmap` to a non-context function
and contextualizing it to receive a Maybe value.

An interesting thing is that List is Applicative too, and `(<*>)` allows us to do nondeterministic
computation by applying each element of a list to each element of another list.

And then we end up in the context of wanting to chain a value in a context and a function
that doesn't expect that value in a context, but which produces a value in a context.

This is where a Monad comes in with the `bind` operator

(>>=) :: Monad m => m a -> (a -> m b) -> m b

which is useful in contexts that take, e.g., a non-Maybe type but produce one, and one seeks
to chain them (e.g. when having Map.Map instances that relate to one another)

studentGrades name = lookupRoom name >>= lookupID >>= lookupGrades

Imagining these type signatures

type ID = Int
studentGrades :: String -> Maybe [Int]
lookupRoom    :: String -> Maybe ID
lookupRoom    :: ID     -> Maybe ID
lookupGrades  :: ID     -> Maybe [Int]

Which means that e.g. the first chaining would be:

(String -> Maybe ID) -> (ID -> Maybe ID)

which is exactly what `bind` makes possible

Monad also has a `>>` operator that takes a monad in a type and a monad in another and returns the second (throws away the first)
and a `return` operator that's the same as `pure`: it takes a value and "wraps" it in the Monad.

The unit also introduces do notation, which stands in for applications of the Monad operators. These are equivalent:

helloN :: IO ()
hellon = askForName >>
         getLine >>=
         (\name -> return (nameStatement name)) >>= putStrLn

and:

helloN :: IO ()
helloN = do
  askForName
  name <- getLine
  let statement = nameStatement 
  putStrLn statement

Where:

askForName :: IO ()
nameStatement :: String -> String
getLine :: IO string
putStrlLn :: String -> IO ()

(Notice the trick of using a lambda to "wrap" a function that doesn't take a Monad, apply it and then return a monadic value)

In essence, in do notation, "assignment" with `<-` allows us to treat a monadic value as if it wasn't monadic, whereas `let`
is normal assignment.

Finally, it shows that list comprehensions are further syntactic sugar; and that using do notation illustrates
how we can write the exact same code for different monads like Maybe, IO and List: the bind operator is in charge of "making sense" of it.

In this capstone project, we put all these things together implementing a LINQ-like query language for lists.


> data Name = Name
>             { firstName :: String
>             , lastName  :: String
>             }
> instance Show Name where
>   show (Name first last) = mconcat [first, " ", last]
>
> data GradeLevel = Freshman
>                 | Sophomore
>                 | Junior
>                 | Senior deriving (Eq, Ord, Enum, Show)
>
> data Student = Student
>                { studentId :: Int
>                , gradeLevel :: GradeLevel
>                , studentName :: Name
>                } deriving Show

Which gives us our "schema", notice how we're deriving typeclasses to aid with comparison, sorting, enumeration and display!

> students :: [Student]
> students = [(Student 1 Senior $ Name "Nena" "Alpaca")
>            ,(Student 2 Junior $ Name "Charlie" "Alpaca")
>            ,(Student 3 Freshman $ Name "Guy" "Debord")
>            ,(Student 4 Senior $ Name "Jean" "Baudrillard")
>            ,(Student 5 Sophomore $ Name "Gilles" "Deleuze")
>            ,(Student 6 Junior $ Name "Wislawa" "Szymborszka")
>            ]

The book doesn't seem to use the `$` syntactic sugar as liberally, but it's neat to de-parenthesize (we'd have
to wrap the Name constructions in parens otherwise)

We want to implement three functions: select, where and join, with the following signatures:

select :: (a -> b) -> [a] -> [b] -- take a list of entries and get a list of properties
where  :: (a-> Bool) -> [a] -> [a] -- take a list of entries and filter it down
join   :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) [(a, b)] -- take two lists, and functions that extract join properties

1. Implementing Select
======================

Looking at the signature, we know that what we want is just `fmap`. To make this applicable to any context,
we'll use do-notation, which works because List is an instance of Monad, and do-notation allows us to act on
individual elements of the list:


The list-focused type signature would've been:
_select :: (a -> b) -> [a] -> [b]

But we want it to work for all monads, so we do:

> _select :: Monad m => (a -> b) -> m a -> m b
> _select prop vals = do
>   val <- vals
>   return (prop val)

Using record-generated methods and composition, we get this elegant fmap-like function:

λ> _select (firstName . studentName) students
["Nena","Charlie","Guy","Jean","Gilles","Wislawa"]

λ> _select gradeLevel students
[Senior,Junior,Freshman,Senior,Sophomore,Junior]

λ> _select (\x -> (studentName x, gradeLevel x)) students
[(Nena Alpaca,Senior),(Charlie Alpaca,Junior),(Guy Debord,Freshman),(Jean Baudrillard,Senior),(Gilles Deleuze,Sophomore),(Wislawa Szymborszka,Junior)]

Notice that even though it _looks_ like fmap, it isn't really the same simply because the type signature doesn't follow that of fmap. Refactoring will happen later.


2. Implementing Where
=====================

The list-only signature for _where would be:
 _where :: (a -> Bool) -> [a] -> [a]

However, if we want this to work for all monads, we just need to generalize it. Notice that
we add a further constraint that the Monad needs to also be an instance of Alternative,
since `guard` works on those (an Alternative is required to define an empty element.)
-- both List and Maybe are members of Alternative, and IO isn't.

> _where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
> _where test vals = do
>   val <- vals
>   guard (test val)
>   return val

Which acts pretty much like filter, but already has an inkling of going beyond lists.

> startsWith :: Char -> String -> Bool
> startsWith char string = char == (head string)


λ> _where (startsWith 'J' . firstName) $ _select studentName students
[Jean Baudrillard]

3. More schema
==============

Before implementing `_join`, we need more relations:

> data Teacher = Teacher
>   { teacherId :: Int
>   , teacherName :: Name
>   } deriving Show
>
> teachers :: [Teacher]
> teachers = [(Teacher 100 $ Name "Simone" "De Beauvoir")
>            ,(Teacher 200 $ Name "Susan"  "Sontag")
>            ]
>
> data Course = Course
>   { courseId :: Int
>   , courseTitle :: String
>   , teacher :: Int
>   } deriving Show
>
> courses :: [Course]
> courses = [ Course 100 "Women's Studies" 100
>           , Course 200 "Photography" 200
>           ]

Notice that a join (inner join) is, essentially, a filtered-down cross-join (cartesian product)
where properties match. That's why the type signature requires two functions that will find properties of the
same type to match on, one for each list. In SQL, these would be the two colums on an `on` clause

Given the above description, the list-focused signature for _join would be:

_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]

But generalizing to monads, it looks like:

> _join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
> _join data1 data2 prop1 prop2 = do
>   d1 <- data1
>   d2 <- data2
>   let dpairs = (d1, d2)
>   guard $ (prop1 (fst dpairs)) == (prop2 (snd dpairs))
>   return dpairs

Which allows us to do the following:

λ> _join teachers courses teacherId teacher
[(Teacher {teacherId = 100, teacherName = Simone De Beauvoir},Course {courseId = 100, courseTitle = "Women's Studies", teacher = 100}),(Teacher {teacherId = 200, teacherName = Susan Sontag},Course {courseId = 200, courseTitle = "Photography", teacher = 200})]

4. Syntactic sugar
==================

With what we have so far, querying looks like this:

λ> _joinData = (_join teachers courses teacherId teacher)
λ> _whereResult = _where ((== "Photography") . courseTitle . snd) _joinData
λ> _selectResult = _select (teacherName . fst) _whereResult
λ> show _selectResult
"[Susan Sontag]"

We want something a bit more sugary, something like:

(_select $ teacherName . fst)
(_join teachers courses teacherId teacher)
(_where $ (== "Photography") . courseTitle . snd)

A way of implementing this is a relatively simple function that rearranges things as one would expect:

> _hinq selectQuery joinQuery whereQuery =
>   (\joinData ->
>      (\whereResult ->
>         selectQuery whereResult)
>   (whereQuery joinData)
>   ) joinQuery

Which gives us things like:

> finalResult :: [Name]
> finalResult = _hinq
>               (_select $ teacherName . fst)
>               (_join teachers courses teacherId teacher)
>               (_where $ (== "Photography") . courseTitle . snd)

Which indeed returns [Susan Sontag]

An annoyance this creates is when we, for example, don't need a where clause; like when asking
for the name for all teachers in that result set:

> teacherFirstNames :: [String]
> teacherFirstNames = _hinq
>                     (_select firstName)
>                     finalResult
>                     (_where (\_ -> True))

λ> teacherFirstNames
["Susan"]

To resolve this, we'll create a type for queries that can have either a select and join, or select, join and where.

5. Taking queries to the next level
===================================

So far, our functions operate on lists only. But it's not a far jump to make them work on all monads. In fact,
it's as easy as modifying the type signatures! (see the definitions above for the list-only signatures, commented out.)

Notice that this is why we used do notation instead of list-specific functions like map or filter (or list comprehension: )
with no other work than generalizing to monads in the type signatures, the functions already work for any Monad!

With that, we can now define our type for queries:

> data HINQ m a b = HINQ  (m a -> m b) (m a) (m a -> m a) -- or, select, join and where
>                 | HINQ' (m a -> m b) (m a)

Where:
m = the type of Monad
a = the type of the data
b = the type of the result
Remembering that:
_select :: Monad m => (a -> b) -> m a -> m b
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a

Notice that the types as parameters to HINQ are the _return_ types of the functions we send it.



> runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
> runHINQ (HINQ  sClause jClause wClause) = _hinq sClause jClause wClause
> runHINQ (HINQ' sClause jClause)         = _hinq sClause jClause $ _where (\_ -> True)

With that defined, we can run queries as follows:

> query1 :: HINQ [] (Teacher, Course) Name -- that is, we want Names out of Teacher, Course tuples in the List monad
> query1 = HINQ
>          (_select $ teacherName . fst)
>          (_join teachers courses teacherId teacher)
>          (_where $ (== "Photography") . courseTitle . snd)
> query2 :: HINQ [] Teacher Name -- give me Names out of Teachers in a List
> query2 = HINQ'
>          (_select teacherName)
>          teachers
> query3 :: HINQ [] Teacher String
> query3 = HINQ'
>          (_select $ firstName . teacherName)
>          teachers

Which is not evaluated, yet, unless we actually run it:

λ> runHINQ query1
[Susan Sontag]

λ> runHINQ query2
[Simone De Beauvoir,Susan Sontag]

λ> runHINQ query3
["Simone","Susan"]

Which mimics the expected behavior of ORMs like ActiveRecord or LINQ in .NET.

6. Working with Maybe
=====================

Note that so far we've worked with Lists, but due to how this is built, it can just as well work with Maybe values!

> possibleTeacher :: Maybe Teacher
> possibleTeacher = Just $ head teachers
>
> possibleCourse :: Maybe Course
> possibleCourse = Just $ head courses
>

Which means we can still do relational queries between these entities _even_ if there's actually no match:

> maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
> maybeQuery1 = HINQ
>               (_select $ teacherName .fst)
>               (_join possibleTeacher possibleCourse teacherId teacher)
>               (_where  $ (== "Women's Studies") . courseTitle . snd)

λ> runHINQ maybeQuery1
Just Simone De Beauvoir

And it works for cases where there's no match (or one of the values is Nothing),
since we rely on Maybe to use `guard` properly:

> maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
> maybeQuery2 = HINQ
>               (_select $ teacherName .fst)
>               (_join possibleTeacher possibleCourse teacherId teacher)
>               (_where  $ (== "French") . courseTitle . snd)

λ> runHINQ maybeQuery2
Nothing

7. Multiple Joins
=================

> data Enrollment = Enrollment
>   { student :: Int
>   , course  :: Int
>   } deriving Show
>
> enrollments :: [Enrollment]
> enrollments = [(Enrollment 1 100)
>               ,(Enrollment 2 100)
>               ,(Enrollment 2 200)
>               ,(Enrollment 3 100)
>               ,(Enrollment 4 200)
>               ,(Enrollment 5 100)
>               ,(Enrollment 6 200)
>               ]

Now we can attempt fancier queries such as getting all the names + the courses they're enrolled in:

> studentEnrollmentsQ = HINQ'
>                       (_select (\(st, en) -> (studentName st, course en)))
>                       (_join students enrollments studentId student)
> studentEnrollments :: [(Name, Int)] -- name and course id
> studentEnrollments =  runHINQ studentEnrollmentsQ

Note that in this case we let Haskell infer the type of our query--we can get that from GHC if curious:

λ> :t studentEnrollmentsQ
studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Name, Int)

λ> studentEnrollments
[(Nena Alpaca,100),(Charlie Alpaca,100),(Charlie Alpaca,200),(Guy Debord,100),(Jean Baudrillard,200),(Gilles Deleuze,100),(Wislawa Szymborszka,200)]

> photographyStudentsQ = HINQ
>                        (_select $ fst . fst)
>                        (_join studentEnrollments courses snd courseId)
>                        (_where $ (== "Photography") . courseTitle . snd)
> photographyStudents :: [Name]
> photographyStudents = runHINQ photographyStudentsQ

And the type signature explains that weird `fst . fst` we had to compose:

λ> photographyStudents
[Charlie Alpaca,Jean Baudrillard,Wislawa Szymborszka]
λ> :t photographyStudentsQ
photographyStudentsQ :: HINQ [] ((Name, Int), Course) Name

And of course, it's easy to generalize to helper functions:

> getEnrollments :: String -> [Name]
> getEnrollments courseName = runHINQ courseQuery
>   where  courseQuery = HINQ
>                        (_select $ fst . fst)
>                        (_join studentEnrollments courses snd courseId)
>                        (_where $ (== courseName) . courseTitle . snd)

λ> getEnrollments "Photography"
[Charlie Alpaca,Jean Baudrillard,Wislawa Szymborszka]
λ> getEnrollments "Women's Studies"
[Nena Alpaca,Charlie Alpaca,Guy Debord,Gilles Deleuze]
