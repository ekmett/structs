{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Struct.TH (makeStruct) where

import           Control.Monad (when, zipWithM)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Either (partitionEithers)
import           Data.Primitive
import           Data.Struct
import           Data.Struct.Internal (Dict(Dict), initializeUnboxedField, st)
import           Data.List (groupBy, nub)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (VarStrictType)

#ifdef HLINT
{-# ANN module "HLint: ignore Use ." #-}
#endif

data StructRep = StructRep
  { srState       :: Name
  , srName        :: Name
  , srTyVars      :: [TyVarBndr]
#if MIN_VERSION_template_haskell(2,12,0)
  , srDerived     :: [DerivClause]
#else
  , srDerived     :: Cxt
#endif
  , srCxt         :: Cxt
  , srConstructor :: Name
  , srMembers     :: [Member]
  } deriving Show

data Member = Member
  { _memberRep :: Representation
  , memberName :: Name
  , _memberType :: Type
  }
  deriving Show

data Representation = BoxedField | UnboxedField | Slot
  deriving Show

-- | Generate allocators, slots, fields, unboxed fields, Eq instances,
-- and Struct instances for the given "data types".
--
-- Inputs are expected to be "data types" parameterized by a state
-- type. Strict fields are considered to be slots, Non-strict fields
-- are considered to be boxed types, Unpacked fields are considered
-- to be unboxed primitives.
--
-- The data type should use record syntax and have a single constructor.
-- The field names will be used to generate slot, field, and unboxedField
-- values of the same name.
--
-- An allocator for the struct is generated by prefixing "alloc" to the
-- data type name.
makeStruct :: DecsQ -> DecsQ
makeStruct dsq =
  do ds   <- dsq
     (passthrough, reps) <- partitionEithers <$> traverse computeRep ds
     ds's <- traverse (generateCode passthrough) reps
     return (passthrough ++ concat ds's)

mkAllocName :: StructRep -> Name
mkAllocName rep = mkName ("alloc" ++ nameBase (srName rep))

mkInitName :: StructRep -> Name
mkInitName rep = mkName ("new" ++ nameBase (srName rep))

------------------------------------------------------------------------
-- Input validation
------------------------------------------------------------------------

computeRep :: Dec -> Q (Either Dec StructRep)
computeRep (DataD c n vs _ cs ds) =
  do state <- validateStateType vs
     (conname, confields) <- validateContructor cs
     members <- traverse (validateMember state) confields

     return $ Right StructRep
       { srState = state
       , srName  = n
       , srTyVars = vs
       , srConstructor = conname
       , srMembers = members
       , srDerived = ds
       , srCxt = c
       }
computeRep d = return (Left d)

-- | Check that only a single data constructor was provided and
-- that it was a record constructor.
validateContructor :: [Con] -> Q (Name,[VarStrictType])
validateContructor [RecC name fields] = return (name,fields)
validateContructor [_] = fail "Expected a record constructor"
validateContructor xs = fail ("Expected 1 constructor, got " ++ show (length xs))

-- A struct type's final type variable should be suitable for
-- use as the ('PrimState' m) argument.
validateStateType :: [TyVarBndr] -> Q Name
validateStateType xs =
  do when (null xs) (fail "state type expected but no type variables found")
     case last xs of
       PlainTV n -> return n
       KindedTV n k
         | k == starK -> return n
         | otherwise  -> fail "state type should have kind *"


-- | Figure out which record fields are Slots and which are
-- Fields. Slots will have types ending in the state type
validateMember :: Name -> VarStrictType -> Q Member
validateMember s (fieldname,Bang NoSourceUnpackedness NoSourceStrictness,fieldtype) =
  do when (occurs s fieldtype)
       (fail ("state type may not occur in field `" ++ nameBase fieldname ++ "`"))
     return (Member BoxedField fieldname fieldtype)
validateMember s (fieldname,Bang NoSourceUnpackedness SourceStrict,fieldtype) =
  do f <- unapplyType fieldtype s
     when (occurs s f)
       (fail ("state type may only occur in final position in slot `" ++ nameBase fieldname ++ "`"))
     return (Member Slot fieldname f)
validateMember s (fieldname,Bang SourceUnpack SourceStrict,fieldtype) =
  do when (occurs s fieldtype)
       (fail ("state type may not occur in unpacked field `" ++ nameBase fieldname ++ "`"))
     return (Member UnboxedField fieldname fieldtype)
validateMember _ _ = fail "validateMember: can't unpack nonstrict fields"

unapplyType :: Type -> Name -> Q Type
unapplyType (AppT f (VarT x)) y | x == y = return f
unapplyType t n =
  fail $ "Unable to match state type of slot: " ++ show t ++ " | expected: " ++ nameBase n

------------------------------------------------------------------------
-- Code generation
------------------------------------------------------------------------

generateCode :: [Dec] -> StructRep -> DecsQ
generateCode ds rep = concat <$> sequence
  [ generateDataType rep
  , generateStructInstance rep
  , generateMembers rep
  , generateNew rep
  , generateAlloc rep
  , generateRoles ds rep
  ]

-- Generates: newtype TyCon a b c s = DataCon (Object s)
generateDataType :: StructRep -> DecsQ
generateDataType rep = sequence
  [ newtypeD (return (srCxt rep)) (srName rep) (srTyVars rep)
      Nothing
      (normalC
         (srConstructor rep)
         [ bangType
             (bang noSourceUnpackedness noSourceStrictness)
             [t| Object $(varT (srState rep)) |]
         ])
#if MIN_VERSION_template_haskell(2,12,0)
      (map return (srDerived rep))
#else
      (return (srDerived rep))
#endif
  ]

generateRoles :: [Dec] -> StructRep -> DecsQ
generateRoles ds rep
  | hasRoleAnnotation = return []
  | otherwise = sequence [ roleAnnotD (srName rep) (computeRoles rep) ]

  where
  hasRoleAnnotation = any isTargetRoleAnnot ds

  isTargetRoleAnnot (RoleAnnotD n _) = n == srName rep
  isTargetRoleAnnot _ = False

-- Currently all roles are set to nominal. A more general solution
-- should be able to infer some representional/phantom roles. To do
-- this for arbitrary types we'll need a way to query the roles of
-- existing type constructors to infer the correct roles.
computeRoles :: StructRep -> [Role]
computeRoles = map (const NominalR) . srTyVars

-- | Type of the object not applied to a state type. This
-- should have kind * -> *
repType1 :: StructRep -> TypeQ
repType1 rep = repTypeHelper (srName rep) (init (srTyVars rep))

-- | Type of the object as originally declared, fully applied.
repType :: StructRep -> TypeQ
repType rep = repTypeHelper (srName rep) (srTyVars rep)

repTypeHelper :: Name -> [TyVarBndr] -> TypeQ
repTypeHelper c vs = foldl appT (conT c) (tyVarBndrT <$> vs)

-- Construct a 'TypeQ' from a 'TyVarBndr'
tyVarBndrT :: TyVarBndr -> TypeQ
tyVarBndrT (PlainTV  n  ) = varT n
tyVarBndrT (KindedTV n k) = sigT (varT n) k

generateStructInstance :: StructRep -> DecsQ
generateStructInstance rep =
  [d| instance Struct $(repType1 rep) where struct = Dict
      instance Eq     $(repType  rep) where (==)   = eqStruct
    |]

-- generates: allocDataCon = alloc <n>
generateAlloc :: StructRep -> DecsQ
generateAlloc rep =
  do mName <- newName "m"
     let m = varT mName
         n = length (groupBy isNeighbor (srMembers rep))
         allocName = mkAllocName rep

     simpleDefinition rep allocName
       (forallT [PlainTV mName] (cxt [])
          [t| PrimMonad $m => $m ( $(repType1 rep) (PrimState $m) ) |])
       [| alloc n |]


-- generates:
-- newDataCon a .. = do this <- alloc <n>; set field1 this a; ...; return this
generateNew :: StructRep -> DecsQ
generateNew rep =
  do this <- newName "this"
     let ms = groupBy isNeighbor (srMembers rep)

         addName m = do n <- newName (nameBase (memberName m))
                        return (n,m)

     msWithArgs <- traverse (traverse addName) ms

     let name = mkInitName rep
         body = doE
                -- allocate struct
              $ bindS (varP this) (varE (mkAllocName rep))

                -- initialize each member
              : (noBindS <$> zipWith (assignN (varE this)) [0..] msWithArgs)

                -- return initialized struct
             ++ [ noBindS [| return $(varE this) |] ]

     sequence
       [ sigD name (newStructType rep)
       , funD name [ clause (varP . fst <$> concat msWithArgs)
                            (normalB [| st $body |] ) [] ]
       ]


assignN :: ExpQ -> Int -> [(Name,Member)] -> ExpQ

assignN this _ [(arg,Member BoxedField n _)] =
  [| setField $(varE n) $this $(varE arg) |]

assignN this _ [(arg,Member Slot n _)] =
  [| set      $(varE n) $this $(varE arg)|]

assignN this i us =
  do let n = length us
     mba <- newName "mba"
     let arg0 = fst (head us)
     doE $ bindS (varP mba) [| initializeUnboxedField i n (sizeOf $(varE arg0)) $this |]
         : [ noBindS [| writeByteArray $(varE mba) j $(varE arg) |]
           | (j,(arg,_)) <- zip [0 :: Int ..] us ]

-- | The type of the struct initializer is complicated enough to
-- pull it out here.
-- generates:
-- PrimMonad m => field1 -> field2 -> ... -> m (TyName a b ... (PrimState m))
newStructType :: StructRep -> TypeQ
newStructType rep =
  do mName <- newName "m"
     let m = varT mName
         s = [t| PrimState $m |]
         obj = repType1 rep

         buildType (Member BoxedField   _ t) = return t
         buildType (Member UnboxedField _ t) = return t
         buildType (Member Slot         _ f) = [t| $(return f) $s |]

         r = foldr (-->)
               [t| $m ($obj $s) |]
               (buildType <$> srMembers rep)

         primPreds = primPred <$> nub [ t | Member UnboxedField _ (VarT t) <- srMembers rep ]

     forallRepT rep $ forallT [PlainTV mName] (cxt primPreds)
       [t| PrimMonad $m => $r |]

-- generates a slot, field, or unboxedField definition per member
generateMembers :: StructRep -> DecsQ
generateMembers rep
  = concat <$>
    zipWithM
      (generateMember1 rep)
      [0..]
      (groupBy isNeighbor (srMembers rep))

isNeighbor :: Member -> Member -> Bool
isNeighbor (Member UnboxedField _ t) (Member UnboxedField _ u) = t == u
isNeighbor _ _ = False

------------------------------------------------------------------------

generateMember1 :: StructRep -> Int -> [Member] -> DecsQ

-- generates: fieldname = field <n>
generateMember1 rep n [Member BoxedField fieldname fieldtype] =
  simpleDefinition rep fieldname
    [t| Field $(repType1 rep) $(return fieldtype) |]
    [| field n |]

-- generates: slotname = slot <n>
generateMember1 rep n [Member Slot slotname slottype] =
  simpleDefinition rep slotname
    [t| Slot $(repType1 rep) $(return slottype) |]
    [| slot n |]

-- It the first type patterns didn't hit then we expect a list
-- of unboxed fields due to the call to groupBy in generateMembers
-- generates: fieldname = unboxedField <n> <i>
generateMember1 rep n us =
  concat <$> sequence
    [ simpleDefinition rep fieldname
          (addPrimCxt fieldtype
             [t| Field $(repType1 rep) $(return fieldtype) |])
          [| unboxedField n i |]

    | (i,Member UnboxedField fieldname fieldtype) <- zip [0 :: Int ..] us
    ]
  where
  addPrimCxt (VarT t) = forallT [] (cxt [primPred t])
  addPrimCxt _        = id

-- Generate code for definitions without arguments, with type variables
-- quantified over those in the struct rep, including an inline pragma
simpleDefinition :: StructRep -> Name -> TypeQ -> ExpQ -> DecsQ
simpleDefinition rep name typ def =
  sequence
    [ sigD name (forallRepT rep typ)
    , simpleValD name def
    , pragInlD name Inline FunLike AllPhases
    ]

------------------------------------------------------------------------

-- Simple use of 'valD' bind an expression to a name
simpleValD :: Name -> ExpQ -> DecQ
simpleValD var val = valD (varP var) (normalB val) []

-- Quantifies over all of the type variables in a struct data type
-- except the state variable which is likely to be ('PrimState' s)
forallRepT :: StructRep -> TypeQ -> TypeQ
forallRepT rep = forallT (init (srTyVars rep)) (cxt [])

(-->) :: TypeQ -> TypeQ -> TypeQ
f --> x = arrowT `appT` f `appT` x

primPred :: Name -> PredQ
primPred t = [t| Prim $(varT t) |]

occurs :: Name -> Type -> Bool
occurs n (AppT f x) = occurs n f || occurs n x
occurs n (VarT m) = n == m
occurs n (ForallT _ _ t) = occurs n t -- all names are fresh in quoted code, see below
occurs n (SigT t _) = occurs n t
occurs _ _ = False

-- Prelude Language.Haskell.TH> runQ (stringE . show =<< [t| forall a. a -> (forall a. a) |])
-- LitE (StringL "ForallT [PlainTV a_0] [] (AppT (AppT ArrowT (VarT a_0)) (ForallT [PlainTV a_1] [] (VarT a_1)))")
